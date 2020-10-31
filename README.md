# rnd-fortran
Lagged Fibonacci generator combined with two Marsaglia shift sequences as described in [NR F90, chapter B7, page 1149](http://ckw.phys.ncku.edu.tw/public/pub/Notes/Computers/Lectures/Resources/NumericalRecipe/F90/www.library.cornell.edu/nr/bookf90pdf/chap7f9.pdf)

This work is based on [VMC/DMC code](https://www.fzu.cz/~kolorenc/correlations/00600_QMC_code/man.pdf) by [Dr. Jindrich Kolorenc.](https://www.fzu.cz/~kolorenc/) 

RNG is tested by PractRand(https://sourceforge.net/projects/pracrand/)

RNG generates single precision floating point values or 24-bits integers. The period of this generator is about 8.5×10^37
