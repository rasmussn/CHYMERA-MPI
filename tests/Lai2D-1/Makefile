MPI_PATH = /usr/local/bin

CC = $(MPI_PATH)/mpicc
FC = $(MPI_PATH)/mpif90

OPT_FLAG = -g

CFLAGS  = $(OPT_FLAG)
FFLAGS  = $(OPT_FLAG)

OBJS = params.o Lai2D_1.o

%.o : %.c
	$(CC) -c $< $(CFLAGS)

%.o : %.f90
	$(FC) -c $< $(FFLAGS)

%.o : %.F90
	$(FC) -c $< $(FFLAGS)


all : $(OBJS) poisson2D

poisson2D : $(OBJS) poisson2D.o
	$(FC) -o $@ $(LDFLAGS) poisson2D.o $(OBJS) $(LIBS)

poisson_c : $(OBJS) poisson_c.o
	$(CC) -o $@ $(LDFLAGS) poisson_c.o $(OBJS) $(LIBS)

poisson-multigrid : $(OBJS) poisson-multigrid.o
	$(FC) -o $@ $(LDFLAGS) poisson-multigrid.o $(OBJS) $(LIBS)

multigrid.o : multigrid.f90
	$(FC) -c $< $(FFLAGS)

jacobi.o : jacobi.f90

check : poisson
	rm -f *.vti
	mpirun -np 2 poisson

clean :
	rm -rf $(OBJS) *.vti *.mod poisson
