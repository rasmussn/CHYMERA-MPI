Module Params
!=============================================================================
! Name        : Params.f90
! Author(s)   : Craig Rasmussen
! Version     :
! Copyright   : University of Oregon, 2014
! Description : Parameters for solving Poisson's equation in two dimensions (phi,r)
!
!=============================================================================
Implicit None

Integer, parameter :: NDIMS  =   2
Integer, parameter :: M      =  16
Integer, parameter :: N      =  16
Integer, parameter :: H      =   1

Real, parameter :: PI = 3.141592653589793

Real, parameter :: DR  = 2.0/(2.0*M + 1.0)
Real, parameter :: DTH = 2.0*PI/N

!... dependent variables
!    -------------------
Real, allocatable :: Th(:), R(:)

End Module Params
