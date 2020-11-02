! https://groups.google.com/g/comp.lang.fortran/c/XzFrWNPoUOg?pli=1
! To use asynchronous IO, link with -pthread
! asynchronous IO breaks the byte order at the output!
! gfortran -O2 -Wall -Wextra -o test-rnd-run test-rnd-run.f90 test-rnd.f90
! Runtime checks: gfortran -O2 -Wall -Wextra -fsanitize=undefined -o test-rnd-run test-rnd-run.f90 test-rnd.f90
! ./test-rnd-run >(head -c 12 | xxd -b -g 0 -c 3 1>&2) | head -20
! ./test-rnd-run >(head -c 12 | xxd -b -g 0 -c 3 1>&2) >/dev/null
! ./test-rnd-run >(head -c1M | md5sum)
! timeout 10 ./test-rnd-run >(pv >/dev/null)
! time ./test-rnd-run >(PractRand-RNG_test stdin -tlmax 4G -multithreaded)

program test_rnd_run
  use types_const, only: dp, missing
  use test_rnd
  implicit none

  integer                         :: num_args
  character(len=1024)             :: filename
  integer, parameter              :: size=4096
  integer(kind=4)                 :: j
  integer(kind=1), dimension(3*size) :: d
  type(t_rnd_state), dimension(1) :: s
  real(dp)                        :: x
  integer(kind=4)                 :: i

  num_args = command_argument_count()

  if (num_args /= 1) then
   print *, "Expecting one argument"
   stop 1
  end if
  call get_command_argument(1, filename)
  ! asynchronous='yes' breaks the order!
  open(file=filename, unit=8, access="stream",form="unformatted", action='write',asynchronous='no')
  call rnd_init(s)

  do
    do i=1,size
      call ran1(s(1),x)
      j = INT(x*16777216,4)
      d(3*i-2) = INT(IBITS(j, 16, 8),1)
      d(3*i-1) = INT(IBITS(j, 8, 8),1)
      d(3*i)   = INT(IBITS(j, 0, 8),1)
!     Same implementation using integer arithmetic - it's slower      
!      d(k)   = INT(j/65536,1)
!      d(k+1) = INT(MODULO(j/256,256),1)
!      d(k+2) = INT(MODULO(j,256),1)
!     Same implementation (up to endianness) using 32 bit integers
!     Creates 3x 32-bit integers from 4x 24-bit integers
!     Same performance as with IBITS and working with integer(kind=1)
!     Byte order in output stream is governed by endianness
!     This can be fixed with open(file=filename, unit=8, access="stream",form="unformatted", action='write', convert='big_endian')
!     But convert='big_endian' has negative performance impact
!      do k=1,4
!        call ran1(s(1),x(k))      
!      end do
!      j = INT(x*16777216,4)
!      d(3*i-2) = IOR ( ISHFT(j(1), 8) , ISHFT(j(2), -16) )
!      d(3*i-1) = IOR ( ISHFT(j(2), 16), ISHFT(j(3), -8)  )
!      d(3*i)   = IOR ( ISHFT(j(3), 24), j(4)             )

!      print *, "j:", j
!      print *, "j, MSB:", j/65536, d(k)
!      print *, "j, mid:", MODULO(j/256,256), d(k+1)
!      print *, "j, LSB:", MODULO(j,256), d(k+2)

    end do
    write(8,asynchronous='no') d(1:3*size)
 end do

 close(8)
end program test_rnd_run
