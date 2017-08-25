 ifort -c -w95 -O3 -I/opt/mpich/intel/include  headers.f90 
 ifort -c -w95 -O3 -I/opt/mpich/intel/include  *.f90 
 ifort -c -w95 -O3 -I/opt/mpich/intel/include  *.f
 mpif77 -O3 -o crunch_it *.o -lmpich
 rm -rf *.o








