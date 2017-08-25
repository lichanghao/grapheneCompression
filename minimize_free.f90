SUBROUTINE minimize_free(mesh0,mat1,x0,eta,x0_BC,mdofBC,mdofOP,ndofBC,ndofOP, &
            N,shapef,weight,ngauss,F0,J0,g,f_loc,f_ext,f,W_dens, &
            EPS0,nW_hat,crit,E_out,vdw1,GNORM)
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
REAL(8) :: x0(3*(mesh0%numnods+mesh0%nedge)), g(3*(mesh0%numnods+mesh0%nedge))
REAL(8) :: W_dens(mesh0%numele), J0(mesh0%numele), f_ext(3*(mesh0%numnods))
REAL(8) :: f_loc(3*(mesh0%numnods+mesh0%nedge))
REAL(8), ALLOCATABLE :: W(:),Prec(:)
!!REAL(8) :: W(NWORK),Prec(N)
REAL(8) shapef(ngauss,12,6), weight(ngauss), E_out(4)
DIMENSION x0_short(ndofOP),x0_BC(ndofBC), &
          mdofBC(ndofBC),mdofOP(ndofOP)
REAL(8) :: F,EPS0,EPS,XTOL,GTOL,T1,T2,STPMIN,STPMAX
INTEGER IPRINT(2),IFLAG,ICALL,N,M,MP,LP,J
LOGICAL DIAGCO
EXTERNAL LB2
COMMON /LB3/MP,LP,GTOL,STPMIN,STPMAX
data one/1.0D+0/

NWORK=N*(2*MSAVE +1)+2*MSAVE
ALLOCATE(W(NWORK),Prec(N),STAT=istat)
if (istat/=0) STOP '**** Not enough memory **** minimize'

N_EVALMAX=10000
EPS=EPS0
if (EPS.gt.1) then
  N_EVALMAX=int(EPS)
  EPS=1.d-8
endif
GNORM = 1.0


IPRINT(1)= 1
IPRINT(2)= 0


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

call energy(mesh0,x0,eta,mat1,shapef,weight,ngauss,F0,J0, &
            W_dens,f,g,f_loc,f_ext,nW_hat,crit,E_out,vdw1)

CALL LBFGS(N,MSAVE,X0,F,G,DIAGCO,Prec,IPRINT,EPS,XTOL,W,IFLAG,XNORM0,GNORM)
IF(IFLAG.LE.0) GO TO 50
ICALL=ICALL + 1
!   We allow at most 2000 evaluations of F and G
!    IF(ICALL.GT.N_EVALMAX) GO TO 50
IF(GNORM .lt. EPS .or. ICALL .GT.N_EVALMAX) GOTO 50

    GO TO 20
50  CONTINUE

!!$DEALLOCATE(W,Prec)

  END SUBROUTINE minimize_free
