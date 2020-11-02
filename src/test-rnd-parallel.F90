! gfortran -O2 -Wall -Wextra -o test-rnd-parallel test-rnd-parallel.F90 test-rnd.f90 -fopenmp
! Runtime checks: gfortran -O2 -Wall -Wextra -fsanitize=undefined -o test-rnd-parallel test-rnd-parallel.F90 test-rnd.f90 -fopenmp
! ./test-rnd-run >(head -c 12 | xxd -b -g 0 -c 3 1>&2) | head -20
! ./test-rnd-run >(head -c 12 | xxd -b -g 0 -c 3 1>&2) >/dev/null
! ./test-rnd-run >(head -c1M | md5sum)
! timeout 10 ./test-rnd-run >(pv >/dev/null)
! time ./test-rnd-run >(PractRand-RNG_test stdin -tlmax 4G -multithreaded)

program test_rnd_run
  use omp_lib
  use types_const, only: dp, missing
  use test_rnd
  implicit none

  integer                         :: num_args
  character(len=1024)             :: filename
!  integer, parameter              :: k4b=selected_int_kind(9)
  integer, parameter              :: size=4*1024*1024
  integer(kind=4)                 :: j
  integer(kind=1), dimension(:), allocatable :: d
  type(t_rnd_state), dimension(:), allocatable :: s
  real(dp)                        :: x
  integer(kind=4)                 :: i, k, error, chunks, threads, tid

  num_args = command_argument_count()

  if (num_args /= 1) then
   print *, "Expecting one argument"
   stop 1
  end if
  call get_command_argument(1, filename)

  ! the return value of omp_get_max_chunks() is controlled by
  !   export OMP_NUM_THREADS=n
  threads=omp_get_max_threads()
  print *, "Threads: ", threads

  ! chunks has to be >= threads 
  !chunks = 4
  if (threads>1) then
    chunks = threads + 1
  else
    chunks = 1
  end if  

  print *, "Chunks: ", chunks

  allocate( s(chunks), d(3*size*chunks), stat=error)
  if ( error /= 0 ) then
      print *, "Cannot allocate arrays."
      stop
  end if

  ! asynchronous='yes' breaks the order!
  open(file=filename, unit=8, access="stream",form="unformatted", action='write', asynchronous='no')

  call rnd_init(s)

  !$omp parallel default(none) private(k,x,j,i,tid) shared(s,d,chunks)
  do
    !$omp do ordered
    do k=1, chunks
      do i=(k-1)*size+1,k*size
        call ran1(s(k),x)
        j = INT(x*16777216,4)
        d(3*i-2) = INT(IBITS(j, 16, 8),1)
        d(3*i-1) = INT(IBITS(j, 8, 8),1)
        d(3*i)   = INT(IBITS(j, 0, 8),1)
      end do
      !$omp ordered
      !print *,k, "chunk", d(3*(k-1)*size+1:3*k*size)
      !print *,"k",k,"thread_id",omp_get_thread_num()
      write(8,asynchronous="no") d(3*(k-1)*size+1:3*k*size)
      !$omp end ordered
    end do
    !omp barrier
    !omp single
    !write(8,asynchronous="yes") d(1:3*size*chunks)
    !do k=1, chunks
    !  print *,k, "mmunk", d(3*(k-1)*size+1:3*k*size)
    !end do
    !omp end single
  end do
  !$omp end parallel


 close(8)
end program test_rnd_run
