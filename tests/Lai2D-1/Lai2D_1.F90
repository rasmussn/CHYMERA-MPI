Module Lai2D_1
!=============================================================================
! Name        : Lai2D_1.F90
! Author(s)   : Craig Rasmussen
! Version     :
! Copyright   : University of Oregon, 2014
! Description : Solves the Poisson equation in two dimensions (th,r)
!
! Method :
!
!   Uses method of Lai, "A simple compact fourth-order Poisson solver on
!   "polar geometry", JCP, 182, 337-345, 2002.
!
!=============================================================================
Implicit None

Contains

Subroutine Initialize(U, F)
!=============================================================================
! Initialize the dependent variables
!
! Problem :
!
!   From Lai (2002) test function 1 :
!
!      u(x,y) = 3 e^{x+y} (x - x^2) (y - y^2) + 5
!
!   initially let's try: u(th,r) = r^2 cos(th) + 5
!
!   f(th,r) = 3 cos(th)
!
!=============================================================================
Use Params, only : M, N, DR, DTH, R, TH
Implicit None
Real, intent(OUT), allocatable :: U(:,:), F(:,:)

Integer :: i, j

!-----------------------------------------------------------------------------

!... Allocate independent and dependent variables
!    --------------------------------------------
Allocate(Th(0:N-1), R(0:M+1))
Allocate(U(0:N-1,0:M+1), F(0:N-1,0:M+1))

!... Initialize arrays and set boundary conditions
!    ---------------------------------------------

do i = 0, M+1
   R(i) = (i - 0.5)*DR
end do

do j = 0, N-1
   Th(j) = j*DTH
end do

do i = 1, M+1
   F(:,i) = 3.0*cos(Th)
end do

U = 0.0
U(:,M+1) = cos(Th) + 5.0   ! outer boundary condition

End Subroutine Initialize

End Module Lai2D_1
