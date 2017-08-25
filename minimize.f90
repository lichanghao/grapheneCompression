SUBROUTINE minimize(mesh0,mat1,x0,eta,x ,x0_BC,mdofBC,mdofOP,ndofBC,n, &
            shapef,weight,ngauss,F0,J0,forces,f_loc,f_ext,f,g,W_dens, &
            EPS0,nW_hat,crit,E_out,vdw1,W,NWORK,Prec,GNORM)

USE data_mesh
USE data_tensor22
USE data_vector2
USE data_mat
USE data_vdw
implicit REAL(8) (a-h,o-z)
implicit INTEGER*4 (i-n)
PARAMETER(MSAVE=5)
TYPE(mesh) :: mesh0
TYPE(material) mat1
TYPE(vdw_data):: vdw1
TYPE(tensor22) :: F0(mesh0%numele)
TYPE(vector2) :: eta(ngauss,(mesh0%numele))
REAL(8) :: x0(3*(mesh0%numnods+mesh0%nedge)), forces(3*(mesh0%numnods+mesh0%nedge))
REAL(8) :: W_dens(mesh0%numele), J0(mesh0%numele), f_ext(3*(mesh0%numnods))
REAL(8) :: f_loc(3*(mesh0%numnods+mesh0%nedge))
!!$REAL(8), ALLOCATABLE :: W(:),Prec(:)
include 'mpif.h' !MPI
REAL(8) :: W(NWORK),Prec(N)
REAL(8) shapef(ngauss,12,6), weight(ngauss), E_out(4)
DIMENSION x(n),x0_BC(ndofBC), g(n),& 
          mdofBC(ndofBC),mdofOP(n)
REAL(8) :: F,EPS0,EPS,XTOL,GTOL,T1,T2,STPMIN,STPMAX
INTEGER IPRINT(2),IFLAG,ICALL,N,M,MP,LP,J
LOGICAL DIAGCO
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
REAL(8) :: bondennod(mesh0%numnods+mesh0%nedge),nobdennod(mesh0%numnods+mesh0%nedge)
REAL(8) :: W_dens1(mesh0%numele)
REAL(8) :: vdw11(mesh0%numele)
REAL(8) :: totnod(mesh0%numnods+mesh0%nedge)
INTEGER(4) :: neigh11(3)
character*30 filename
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
EXTERNAL LB2
COMMON /LB3/MP,LP,GTOL,STPMIN,STPMAX
data one/1.0D+0/

!!$NWORK=N*(2*MSAVE +1)+2*MSAVE
!!$ALLOCATE(W(NWORK),Prec(N),STAT=istat)
!!$if (istat/=0) STOP '**** Not enough memory **** minimize'

N_EVALMAX=20000
EPS=EPS0
if (EPS.gt.1) then
  N_EVALMAX=int(EPS)
  EPS=1.d-8
endif
IPRINT(1)= 1
IPRINT(2)= 0
GNORM = 1.0

dx1=maxval(x0(1:3*mesh0%numnods:3))-minval(x0(1:3*mesh0%numnods:3))
dx2=maxval(x0(2:3*mesh0%numnods:3))-minval(x0(2:3*mesh0%numnods:3))
dx3=maxval(x0(3:3*mesh0%numnods:3))-minval(x0(3:3*mesh0%numnods:3))
XNORM0=dsqrt(dx1**2+dx2**2+dx3**2)

!     We do not wish to provide the diagonal matrices Hk0, and 
!     therefore set DIAGCO to FALSE.
DIAGCO= .FALSE.
XTOL= 1.0D-16
ICALL=0
IFLAG=0

20 CONTINUE

call long(x0,x,x0_BC,mdofBC,mdofOP,ndofBC,n,mesh0)
call energy(mesh0,x0,eta,mat1,shapef,weight,ngauss,F0,J0, &
            W_dens,f,forces,f_loc,f_ext,nW_hat,crit,E_out,vdw1)
call short(forces,g,mdofOP,ndofBC,n)

!!$write(*,*)"Initial guass is",X
!!$write(*,*)"Initial energy is",F
!!$write(*,*)"Initial force1  is",forces
!!$write(*,*)"Initial force2  is",g

CALL LBFGS(N,MSAVE,X,F,G,DIAGCO,Prec,IPRINT,EPS,XTOL,W,IFLAG,XNORM0,GNORM)
IF(IFLAG.LE.0) GO TO 50
ICALL=ICALL + 1

!    IF(ICALL.GT.N_EVALMAX) GO TO 50
IF(GNORM .LT. EPS .or. ICALL.GT.N_EVALMAX) GOTO 50
    GO TO 20
50  CONTINUE

!!!!!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
    bondennod=0.0
    nobdennod=0.0
    totnod=0.0
    call MPI_ALLREDUCE(W_dens(1),W_dens1(1),mesh0%numele,MPI_DOUBLE_PRECISION,MPI_SUM,MPI_COMM_WORLD,ierr)
    call MPI_ALLREDUCE(vdw1%W(1),vdw11(1),mesh0%numele,MPI_DOUBLE_PRECISION,MPI_SUM,MPI_COMM_WORLD,ierr)

    do i=1,mesh0%numele

       if(.not.(any(mesh0%elem_ghost.eq.i)))then


          neigh11(:)=mesh0%connect(i)%vertices(:)
          bondennod(neigh11(1))=bondennod(neigh11(1))+ W_dens1(i)*J0(i)/6
          nobdennod(neigh11(1))=nobdennod(neigh11(1))+ vdw11(i)/3
          bondennod(neigh11(2))=bondennod(neigh11(2))+ W_dens1(i)*J0(i)/6
          nobdennod(neigh11(2))=nobdennod(neigh11(2))+ vdw11(i)/3
          bondennod(neigh11(3))=bondennod(neigh11(3))+ W_dens1(i)*J0(i)/6
          nobdennod(neigh11(3))=nobdennod(neigh11(3))+ vdw11(i)/3
       endif
    enddo
    totnod(:)=bondennod(:)+nobdennod(:)


    !output
    filename='Eofnode000'
    write(filename(8:10),'(i3.3)') mesh0%nstep
    open(40,FILE=filename)

    jcount=1
    do i = 1,mesh0%numnods+mesh0%nedge

       WRITE(40,1000)jcount,bondennod(i),nobdennod(i),totnod(i)
       jcount = jcount + 1
    enddo

1000 format(i8,3e12.5)

    close(40)

    mesh0%nstep=mesh0%nstep+1
!!!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


END SUBROUTINE minimize
