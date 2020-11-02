PLATFORM := default
include ../make/$(PLATFORM).mk

OBJ := cfparser.o list_words.o
MOD := cfparser.mod list_words.mod

all: lib demo

lib: libcfparser.a

demo: lib
	test ! -d demo || ( cd demo && make )

libcfparser.a: $(OBJ)
	ar rcs $@ $(OBJ)

cfparser.o: list_words.o
list_words.o: list_template.F90.inc

%.o: %.F90
	$(FC) $(FFLAGS) -c $< -o $@

%.o: %.f90
	$(FC) $(FFLAGS) -c $< -o $@

clean:
	rm -f $(OBJ)
	test ! -d demo || ( cd demo && make clean )

mrproper: clean
	rm -f $(MOD) libcfparser.a
	test ! -d demo || ( cd demo && make mrproper )

