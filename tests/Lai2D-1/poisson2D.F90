Program Poisson2D
!=============================================================================
! Name        : poisson2D.F90
! Author(s)   : Craig Rasmussen
! Version     :
! Copyright   : University of Oregon, 2014
! Description : Solves the Poisson equation in two dimensions (phi,r)
!
! Method :
!
!   Uses method of Lai, "A simple compact fourth-order Poisson solver on
!   "polar geometry", JCP, 182, 337-345, 2002.
!
!=============================================================================
Use Params,  only : M, N, H, R, TH
Use Lai2D_1, only : Initialize

Implicit None

Real, allocatable :: U(:,:), F(:,:)

Integer :: i, j

!-----------------------------------------------------------------------------

!... Initialize arrays and set boundary conditions
!    ---------------------------------------------

Call Initialize(U, F)

print *, Th
print *, R

#ifdef NOT_YET
call error_allocate(M, N, H)

!Call VTK_Output (M, N, H, T1, 0, 'T', aContext)
!Call VTK_Output (M, N, H, T2, 1, 'T', aContext)

Call VTK_Output (M, N, H, Err, 0, 'E', aContext)

Print*, "I am Alive"

!... Iterate solution
!    ________________
do i = 2, nsteps, 2
   Call Iterate (M, N, H, T1, T2);
   Call Parallel_HaloArray_Exchange (ha2, aTopology)
   Call Iterate (M, N, H, T2, T1);
   Call Parallel_HaloArray_Exchange (ha1, aTopology)
   Call calc_error (m, n, h, T2)
   Call VTK_Output (M, N, 0, Err, i  , 'E', aContext)
   !Call VTK_Output (M, N, H, T2, i  , 'T', aContext)
   !Call VTK_Output (M, N, H, T1, i+1, 'T', aContext)
end do

Call Parallel_HaloArray_Deallocate ( ha1, T1 )
Call Parallel_HaloArray_Deallocate ( ha2, T2 )

Call Parallel_End (aContext)
#endif

End Program Poisson2D
