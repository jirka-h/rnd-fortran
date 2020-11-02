! ==========================================================================
!
! Basic VMC/DMC code for educational purposes.
! This file: system-specific subroutines for H2 molecule
!
! Copyright (C) 2014 Jindrich Kolorenc
!
! The software is released under MIT/X11 license.
!
! ==========================================================================

module qmc_input
  use types_const, only: dp, missing
  implicit none
  private

  public :: sys_dim
  public :: t_walker, t_sys
  public :: init_sys
  public :: EL_drift, psiT2

  ! logically, this should go to t_sys, but I really want to keep static arrays
  ! in t_walker, which enforces this to be a global constant
  integer, parameter :: sys_dim=6       ! dimension of walker

  type t_walker
     real(dp), dimension(sys_dim) :: r  ! position
     real(dp), dimension(sys_dim) :: vD ! drift velocity
     real(dp) :: EL                     ! local energy
     real(dp) :: psiT                   ! trial wave function
     real(dp) :: psiTsq                 ! square of trial wavefunction
     real(dp) :: wt=1.0_dp              ! DMC weight
     integer :: sgn=1                   ! sign of trial wavefunction at r
     integer :: age=0                   ! how many times rejected by det. bal.
  end type t_walker

  type t_sys
     character(11) :: id="H2 molecule"  ! identification of the system
     real(dp) :: R=1.4_dp               ! proton-proton distance
     real(dp) :: b=4.0_dp               ! denominator in the ee-cusp term
  end type t_sys

  interface EL_drift
     !module procedure EL_drift_HeitlerLondon
     module procedure EL_drift_HeitlerLondonJastrow
  end interface EL_drift

  interface psiT2
     !module procedure psiT2_HeitlerLondon
     module procedure psiT2_HeitlerLondonJastrow
  end interface psiT2

contains

  subroutine init_sys(sys,words)
    ! {{{ initialize the system and wave-function parameters
    use cfparser, only: t_words, readsection, readvalue, clear
    type(t_sys), intent(inout) :: sys
    type(t_words), intent(in) :: words
    type(t_words) :: syswords

    if ( .not.readsection(words,syswords,"system") ) then
       print *, "Missing section 'system' in the input file."
       stop
    end if

    if ( .not.readvalue(syswords,sys%R,"R") ) call missing("R")
    call clear(syswords)

    write(unit=*,fmt='(1x,a)') "system:"
    write(unit=*,fmt='(3x,a)') sys%id
    write(unit=*,fmt='(3x,a,f8.4)') "proton-proton distance [R]:", sys%R
    write(unit=*,fmt=*)
    ! }}}
  end subroutine init_sys

  pure subroutine EL_drift_HeitlerLondon(sys,wlkr)
    ! {{{ local energy, drift velocity, wave-function squared
    type(t_sys), intent(in) :: sys
    type(t_walker), intent(inout) :: wlkr
    real(dp), dimension(3) :: r1, r2
    real(dp) :: rho1, r1a, r1b, rho2, r2a, r2b, r12
    real(dp) :: phiA1B2, phiA2B1, psi
    r1=wlkr%r(1:3)
    r2=wlkr%r(4:6)
    rho1=r1(1)**2+r1(2)**2
    r1a=sqrt(rho1+(r1(3)-sys%R)**2)
    r1b=sqrt(rho1+r1(3)**2)
    rho2=r2(1)**2+r2(2)**2
    r2a=sqrt(rho2+(r2(3)-sys%R)**2)
    r2b=sqrt(rho2+r2(3)**2)
    r12=sqrt(sum((r1-r2)**2))
    phiA1B2=exp(-r1a-r2b)
    phiA2B1=exp(-r2a-r1b)
    psi=phiA1B2+phiA2B1
    wlkr%psiT=psi
    wlkr%psiTsq=psi**2
    wlkr%EL=-1.0_dp-((1.0_dp/r1b+1.0_dp/r2a)*phiA1B2 &
         +(1.0_dp/r1a+1.0_dp/r2b)*phiA2B1)/psi+1.0_dp/r12+1.0_dp/sys%R
    wlkr%vD(1:2)=-phiA1B2*r1(1:2)/r1a-phiA2B1*r1(1:2)/r1b
    wlkr%vD(3)=-phiA1B2*(r1(3)-sys%R)/r1a-phiA2B1*r1(3)/r1b
    wlkr%vD(4:5)=-phiA1B2*r2(1:2)/r2b-phiA2B1*r2(1:2)/r2a
    wlkr%vD(6)=-phiA1B2*r2(3)/r2b-phiA2B1*(r2(3)-sys%R)/r2a
    wlkr%vD=wlkr%vD/psi
    wlkr%sgn=1
    ! }}}
  end subroutine EL_drift_HeitlerLondon

  pure subroutine psiT2_HeitlerLondon(sys,wlkr)
    ! {{{ trial wave-function squared
    type(t_sys), intent(in) :: sys
    type(t_walker), intent(inout) :: wlkr
    real(dp), dimension(3) :: r1, r2
    real(dp) :: rho1, r1a, r1b, rho2, r2a, r2b
    real(dp) :: phiA1B2, phiA2B1, psi
    r1=wlkr%r(1:3)
    r2=wlkr%r(4:6)
    rho1=r1(1)**2+r1(2)**2
    r1a=sqrt(rho1+(r1(3)-sys%R)**2)
    r1b=sqrt(rho1+r1(3)**2)
    rho2=r2(1)**2+r2(2)**2
    r2a=sqrt(rho2+(r2(3)-sys%R)**2)
    r2b=sqrt(rho2+r2(3)**2)
    phiA1B2=exp(-r1a-r2b)
    phiA2B1=exp(-r2a-r1b)
    psi=phiA1B2+phiA2B1
    wlkr%psiT=psi
    wlkr%psiTsq=psi**2
    wlkr%sgn=1
    ! }}}
  end subroutine psiT2_HeitlerLondon


  pure subroutine EL_drift_HeitlerLondonJastrow(sys,wlkr)
    ! {{{ local energy, drift velocity, wave-function squared
    type(t_sys), intent(in) :: sys
    type(t_walker), intent(inout) :: wlkr
    type(t_walker) :: wlkrHL
    real(dp), dimension(3) :: r12, grad1J
    real(dp) :: r12l, f, J, lap1J
    wlkrHL=wlkr
    call EL_drift_HeitlerLondon(sys,wlkrHL)
    r12=wlkr%r(1:3)-wlkr%r(4:6)
    r12l=sqrt(sum(r12**2))
    f=1.0_dp/(2.0_dp+sys%b*r12l)
    J=r12l*f
    grad1J=2.0_dp*r12/r12l*f**2
    !lap1J=-4.0_dp*sys%b*f**3
    lap1J=8.0_dp*f**3/r12l
    wlkr%psiT=exp(J)*wlkrHL%psiT
    wlkr%psiTsq=wlkr%psiT**2
    wlkr%EL=wlkrHL%EL - sum(grad1J**2) - lap1J &
         - sum(grad1J*(wlkrHL%vD(1:3)-wlkrHL%vD(4:6)))
    wlkr%vD(1:3)=wlkrHL%vd(1:3)+grad1J
    wlkr%vD(4:6)=wlkrHL%vd(4:6)-grad1J
    wlkr%sgn=wlkrHL%sgn
    ! }}}
  end subroutine EL_drift_HeitlerLondonJastrow

  pure subroutine psiT2_HeitlerLondonJastrow(sys,wlkr)
    ! {{{ trial wave-function squared
    type(t_sys), intent(in) :: sys
    type(t_walker), intent(inout) :: wlkr
    type(t_walker) :: wlkrHL
    real(dp), dimension(3) :: r12
    real(dp) :: r12l, f, J
    wlkrHL=wlkr
    call EL_drift_HeitlerLondon(sys,wlkrHL)
    r12=wlkr%r(1:3)-wlkr%r(4:6)
    r12l=sqrt(sum(r12**2))
    f=1.0_dp/(2.0_dp+sys%b*r12l)
    J=r12l*f
    wlkr%psiT=exp(J)*wlkrHL%psiT
    wlkr%psiTsq=wlkr%psiT**2
    wlkr%sgn=wlkrHL%sgn
    ! }}}
  end subroutine psiT2_HeitlerLondonJastrow


  pure subroutine EL_drift_HeitlerLondonJastrow_nogood(sys,wlkr)
    ! {{{ local energy, drift velocity, wave-function squared
    !     the Jastrow factor takes care only of the e-e cusp but there
    !     are still some e-nucleus divergencies
    type(t_sys), intent(in) :: sys
    type(t_walker), intent(inout) :: wlkr
    real(dp), dimension(3) :: r1, r2
    real(dp), dimension(3) :: r120, r1a0, r1b0, r2a0, r2b0
    real(dp) :: rho1, r1a, r1b, rho2, r2a, r2b, r12
    real(dp) :: phiA, phiB, psi, f
    r1=wlkr%r(1:3)
    r2=wlkr%r(4:6)
    !
    r1a0=r1
    r1b0=r1
    r1a0(3)=r1(3)-sys%R
    rho1=r1(1)**2+r1(2)**2
    r1a=sqrt(rho1+r1a0(3)**2)
    r1b=sqrt(rho1+r1(3)**2)
    r1a0=r1a0/r1a
    r1b0=r1b0/r1b
    !
    r2a0=r2
    r2b0=r2
    r2a0(3)=r2(3)-sys%R
    rho2=r2(1)**2+r2(2)**2
    r2a=sqrt(rho2+r2a0(3)**2)
    r2b=sqrt(rho2+r2(3)**2)
    r2a0=r2a0/r2a
    r2b0=r2b0/r2b
    !
    r120=r1-r2
    r12=sqrt(sum(r120**2))
    r120=r120/r12
    f=1.0_dp/(2.0_dp+r12)
    !
    phiA=exp(-r1a-r2b) * (1.0_dp+r12/2.0_dp)
    phiB=exp(-r2a-r1b) * (1.0_dp+r12/2.0_dp)
    psi=phiA+phiB
    wlkr%psiTsq=psi**2
    wlkr%sgn=1
    !
    wlkr%EL=-1.0_dp + ( &
         ( f*(sum(r120*(r1a0-r2b0))+1) - 1.0_dp/r1b - 1.0_dp/r2a )*phiA + &
         ( f*(sum(r120*(r1b0-r2a0))+1) - 1.0_dp/r1a - 1.0_dp/r2b )*phiB &
         )/psi + 1.0_dp/sys%R
    wlkr%vD(1:3)= ( f*r120 - r1a0)*phiA + ( f*r120 - r1b0)*phiB
    wlkr%vD(4:6)= (-f*r120 - r2b0)*phiA + (-f*r120 - r2a0)*phiB
    wlkr%vD=wlkr%vD/psi
    ! }}}
  end subroutine EL_drift_HeitlerLondonJastrow_nogood

  pure subroutine psiT2_HeitlerLondonJastrow_nogood(sys,wlkr)
    ! {{{ trial wave-function squared
    type(t_sys), intent(in) :: sys
    type(t_walker), intent(inout) :: wlkr
    real(dp), dimension(3) :: r1, r2
    real(dp) :: rho1, r1a, r1b, rho2, r2a, r2b, r12
    real(dp) :: phiA1B2, phiA2B1, psi
    r1=wlkr%r(1:3)
    r2=wlkr%r(4:6)
    rho1=r1(1)**2+r1(2)**2
    r1a=sqrt(rho1+(r1(3)-sys%R)**2)
    r1b=sqrt(rho1+r1(3)**2)
    rho2=r2(1)**2+r2(2)**2
    r2a=sqrt(rho2+(r2(3)-sys%R)**2)
    r2b=sqrt(rho2+r2(3)**2)
    r12=sqrt(sum((r1-r2)**2))
    phiA1B2=exp(-r1a-r2b)
    phiA2B1=exp(-r2a-r1b)
    psi=( phiA1B2+phiA2B1 ) * (1.0_dp+r12/2.0_dp)
    wlkr%psiTsq=psi**2
    wlkr%sgn=1
    ! }}}
  end subroutine psiT2_HeitlerLondonJastrow_nogood

end module qmc_input


! Local variables:
! folded-file: t
! End:
