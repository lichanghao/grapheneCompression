!***************************************************************************
! Here are the potentials
SUBROUTINE MM3(mat1,pe,W,dW)
USE data_mat
implicit REAL(8) (a-h,o-z)
implicit INTEGER*4 (i-n)
TYPE(material) mat1
DIMENSION :: pe(6), Vs(2), Va(2), dW(6)

W=0.d0
do i=1,3
  call Vsmm3(pe(i),mat1,Vs)
  W=W+Vs(1)
  dW(i)=Vs(2)
  call Vamm3(pe(i+3),mat1,Va)
  W=W+2.d0*Va(1)
  dW(3+i)=2.d0*Va(2)
end do
W=W/mat1%s0
dW=dW/mat1%s0
END SUBROUTINE MM3
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
SUBROUTINE Vsmm3(a,mat1,Vs)
USE data_mat
implicit REAL(8) (a-h,o-z)
implicit INTEGER*4 (i-n)
TYPE(material), INTENT(IN) :: mat1
REAL(8), INTENT(IN) :: a
REAL(8), INTENT(OUT) :: Vs(2)
parameter (t1=-25.5d0,t2=148.75d0)

 
da=(a-mat1%A0)
da2=da*da

Vs(1)=da2*(1.d0 + t1*da + t2*da2)
Vs(2)=(2.d0 + 3.d0*t1*da + 4.d0*t2*da2)*da

Vs=Vs*mat1%Vs(1)*0.5d0

END SUBROUTINE Vsmm3
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
SUBROUTINE Vamm3(ang,mat1,Va)
USE data_mat
implicit REAL(8) (a-h,o-z)
implicit INTEGER*4 (i-n)
TYPE(material), INTENT(IN) :: mat1
REAL(8), INTENT(IN) :: ang
REAL(8), INTENT(OUT) :: Va(2)
parameter (ppi=3.141592653589793238D0)
PARAMETER (ang0=2.d0*ppi/3.d0)
!PARAMETER (ang0=122.7d0*ppi/180.d0)
parameter (a1=-0.014d0*180.d0/ppi,   a2=5.6d-5*(180.d0/ppi)**2)
parameter (a3=-7.d-7*(180.d0/ppi)**3,a4=9.d-10*(180.d0/ppi)**4)

t1=(ang-ang0)
t2=t1*t1
t3=t2*t1
t4=t2*t2

Va(1)=t2*(1.d0 + a1*t1 + a2*t2 + a3*t3 + a4*t4)
Va(2)=t1*(2.d0 + 3.d0*a1*t1 + 4.d0*a2*t2 + 5.d0*a3*t3 + 6.d0*a4*t4)

Va=Va*mat1%Va(1)*0.5d0

END SUBROUTINE Vamm3


!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
! NOW STARTS THE INNER PART
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

SUBROUTINE Inner_MM3(mat1,pe,dpedeta,ddpedeta,W,dWdeta,ddWdeta,dW)
USE data_mat
USE data_vector3
USE data_vector2
implicit REAL(8) (a-h,o-z)
implicit INTEGER*4 (i-n)
DIMENSION :: pe(6),Vs(3), Va(3),dWdeta(2),ddWdeta(3),aux1(3)
! 1 is 1,1, 2 is 2,2 and 3 is both 1,2 and 2,1
TYPE(vector2) :: dpedeta(6)
TYPE(vector3) :: ddpedeta(6)
TYPE(material) mat1

W=0.d0
dWdeta=[0.,0.]
ddWdeta=[0.,0.,0.]
do i=1,3
  call Vsmm3_bis(pe(i),mat1,Vs)
  W=W+Vs(1)
  dWdeta=dWdeta+Vs(2)*dpedeta(i)%val
  aux1=[(dpedeta(i)%val(1))**2,(dpedeta(i)%val(2))**2,dpedeta(i)%val(1)*dpedeta(i)%val(2)]
  ddWdeta=ddWdeta+Vs(3)*aux1+Vs(2)*ddpedeta(i)%val
  call Vamm3_bis(pe(i+3),mat1,Va)
  W=W+2.d0*Va(1)
  dWdeta=dWdeta+2.d0*Va(2)*dpedeta(3+i)%val
  aux1=[(dpedeta(3+i)%val(1))**2,(dpedeta(3+i)%val(2))**2,dpedeta(3+i)%val(1)*dpedeta(3+i)%val(2)]
  ddWdeta=ddWdeta+2.d0*Va(3)*aux1+2.d0*Va(2)*ddpedeta(3+i)%val
end do
W=W/mat1%s0
dWdeta=dWdeta/mat1%s0
ddWdeta=ddWdeta/mat1%s0

END SUBROUTINE Inner_MM3

!***************************************************************************
! Here are the potentials
SUBROUTINE Vsmm3_bis(a,mat1,Vs)
USE data_mat
implicit REAL(8) (a-h,o-z)
implicit INTEGER*4 (i-n)
TYPE(material), INTENT(IN) :: mat1
REAL(8), INTENT(IN) :: a
REAL(8), INTENT(OUT) :: Vs(3)
parameter (t1=-25.5d0,t2=148.75d0)


da=(a-mat1%A0)
da2=da*da

Vs(1)=da2*(1.d0 + t1*da + t2*da2)
Vs(2)=(2.d0 + 3.d0*t1*da + 4.d0*t2*da2)*da
Vs(3)=2.d0 + 6.d0*t1*da + 12.d0*t2*da2


Vs=Vs*mat1%Vs(1)*0.5d0

END SUBROUTINE Vsmm3_bis
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
SUBROUTINE Vamm3_bis(ang,mat1,Va)
USE data_mat
implicit REAL(8) (a-h,o-z)
implicit INTEGER*4 (i-n)
TYPE(material), INTENT(IN) :: mat1
REAL(8), INTENT(IN) :: ang
REAL(8), INTENT(OUT) :: Va(3)
parameter (ppi=3.141592653589793238D0)
PARAMETER (ang0=2.d0*ppi/3.d0)
!PARAMETER (ang0=122.7d0*ppi/180.d0)
parameter (a1=-0.014d0*180.d0/ppi,   a2=5.6d-5*(180.d0/ppi)**2)
parameter (a3=-7.d-7*(180.d0/ppi)**3,a4=9.d-10*(180.d0/ppi)**4)

t1=(ang-ang0)
t2=t1*t1
t3=t2*t1
t4=t2*t2

Va(1)=t2*(1.d0 + a1*t1 + a2*t2 + a3*t3 + a4*t4)
Va(2)=t1*(2.d0 + 3.d0*a1*t1 +  4.d0*a2*t2 +  5.d0*a3*t3 +  6.d0*a4*t4)
Va(3)=    2.d0 + 6.d0*a1*t1 + 12.d0*a2*t2 + 20.d0*a3*t3 + 30.d0*a4*t4

Va=Va*mat1%Va(1)*0.5d0

END SUBROUTINE Vamm3_bis


