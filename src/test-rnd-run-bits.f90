! gfortran -O2 -Wall -Wextra -o test-rnd-run-bits test-rnd-run-bits.f90 test-rnd.f90
! Runtime checks: gfortran -O2 -Wall -Wextra -fsanitize=undefined -o test-rnd-run-bits test-rnd-run-bits.f90 test-rnd.f90
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
  character(len=1024)             :: filename, bits_string
  integer, parameter              :: size=4096
  integer(kind=4), dimension(:), allocatable :: j
  integer(kind=1), dimension(:), allocatable :: d
  integer(kind=1)                 :: carry
  integer(kind=4)                 :: carry_bits, top_bit, bits
  type(t_rnd_state), dimension(1) :: s
  real(dp)                        :: x
  integer(kind=4)                 :: i, target_bits, target_bytes, source_words, error, k, source

  num_args = command_argument_count()

  if (num_args /= 2) then
   print *, "Expecting two arguments"
   stop 1
  end if
  call get_command_argument(1, bits_string)
  read(bits_string,*)bits
  if (bits > 32 .or. bits < 1) then
   print *, "bits has to be in range 1 - 32"
   stop 1
  end if

  call get_command_argument(2, filename)
  ! asynchronous='yes' breaks the order!
  open(file=filename, unit=8, access="stream",form="unformatted", action='write')
  call rnd_init(s)

  target_bits = lcm(8,bits)
  target_bytes = target_bits/8
  source_words = target_bits/bits


  allocate( d(target_bytes*size), j(source_words), stat=error)
  if ( error /= 0 ) then
      print *, "Cannot allocate arrays."
      stop
  end if

  do
    do i=1,size
      
      do k=1, source_words
        call ran1(s(1),x)
        j(k) = INT(x*16777216,4)
      end do

      source=1
      top_bit=bits

      do k=1, target_bytes
        if(top_bit>7) then
          top_bit = top_bit - 8
          d(target_bytes*(i-1)+k) = INT(IBITS(j(source), top_bit, 8),1)
        else
          carry =  INT(ISHFT(IBITS(j(source), 0, top_bit),8-top_bit),1)
          carry_bits = top_bit
          do while (carry_bits<8)
            source = source + 1
            top_bit = bits
            if(top_bit>=8-carry_bits) then
              top_bit = top_bit + carry_bits -8
              d(target_bytes*(i-1)+k) = IOR(carry,INT(IBITS(j(source), top_bit, 8-carry_bits),1))
              exit
            else
              carry = IOR(carry, INT(ISHFT(IBITS(j(source), 0, top_bit),8-carry_bits-top_bit),1))
              source = source + 1
              top_bit = bits
              carry_bits = carry_bits + top_bit
            end if
          end do  
        end if
      end do
      
    end do
    write(8) d(1:target_bytes*size)
 end do

 close(8)

contains

  integer (kind=4) function lcm(a,b)
    implicit none
    integer (kind=4), intent(in) :: a,b
    lcm = a*b / gcd(a,b)
  end function lcm

  integer (kind=4) function gcd(a,b)
    implicit none
    integer (kind=4), intent(in) :: a,b
    integer (kind=4) :: aa,bb,t
    aa = a
    bb = b
    do while (bb/=0)
      t = bb
      bb = mod(aa,bb)
      aa = t
    end do
    gcd = abs(aa)
  end function gcd

end program test_rnd_run
