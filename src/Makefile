shell=/bin/bash
FFLAGS      = -O2 -Wall -Wextra
#FFLAGS     = -g -O2 -Wall -Wextra -fsanitize=undefined

all: test-rnd-run test-rnd-run-bits test-rnd-parallel

test-speed: all
	bash -c "./test-rnd-run 4 /dev/null"
	bash -c "./test-rnd-run-bits 24 4 /dev/null"
	bash -c "OMP_NUM_THREADS=3 /usr/bin/time ./test-rnd-parallel 4 /dev/null"

test-speed_pv: all
	bash -c "./test-rnd-run 4 >(pv >/dev/null)"
	bash -c "./test-rnd-run-bits 24 4 >(pv >/dev/null)"
	bash -c "OMP_NUM_THREADS=3 /usr/bin/time ./test-rnd-parallel 4 >(pv >/dev/null)"

test-sha256sum: all
	bash -c "./test-rnd-run 0 >(head -c1G | sha256sum -b)          || true"
	bash -c "./test-rnd-run-bits 24 0 >(head -c1G | sha256sum -b)          || true"
	bash -c "OMP_NUM_THREADS=1 ./test-rnd-parallel 0 >(head -c1G | sha256sum -b)          || true"

test-rnd-run: test-rnd-run.f90 lib
	gfortran $(FFLAGS) -o test-rnd-run test-rnd-run.f90 types_const.o test-rnd.o -lrt

test-rnd-run-bits: test-rnd-run-bits.f90 lib
	gfortran $(FFLAGS) -o test-rnd-run-bits test-rnd-run-bits.f90 types_const.o test-rnd.o -lrt

test-rnd-parallel: test-rnd-parallel.F90 lib
	gfortran $(FFLAGS) -o test-rnd-parallel test-rnd-parallel.F90 types_const.o test-rnd.o -fopenmp -lrt

lib:  types_const.f90 test-rnd.f90
	gfortran $(FFLAGS) -c types_const.f90
	gfortran $(FFLAGS) -c test-rnd.f90

clean:
	rm -f test-rnd-run test-rnd-run-bits test-rnd-parallel
	rm -f test-rnd.o types_const.o
	rm -f test_rnd.mod types_const.mod


