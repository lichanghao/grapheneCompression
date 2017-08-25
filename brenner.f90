!***************************************************************************
! Here are the potentials
SUBROUTINE Brenner(mat1,pe,W,dW)
USE data_mat
implicit REAL(8) (a-h,o-z) 
implicit INTEGER*4 (i-n)
TYPE(material) mat1
! Vr , Va and Ga, 1 is function, 2 is first derivative
DIMENSION :: pe(6),  dW(6), Vr(2), Va(2), Ga(2,3), iperm(2,3)
data iperm(1,1:3) /2, 3, 1/
data iperm(2,1:3) /3, 1, 2/

if (maxval(pe(1:3)).gt.0.17) write(*,*) ' Brenner Potential implemented for moderate defs.'

W=0.
dW=0.
call Gangular(pe(4:6),mat1,Ga)

do ibond=1,3
  Fang=1.d0/dsqrt(1.d0+Ga(1,iperm(1,ibond))+Ga(1,iperm(2,ibond)))
  call Vrepulsv(pe(ibond),mat1,Vr)
  call Vattract(pe(ibond),mat1,Va)
  W=W+Vr(1)-Fang*Va(1)
  dW(ibond)=Vr(2)-Fang*Va(2)
  dW(3+iperm(1,ibond))=dW(3+iperm(1,ibond))+Va(1)*(Fang**3)*Ga(2,iperm(1,ibond))/2.d0
  dW(3+iperm(2,ibond))=dW(3+iperm(2,ibond))+Va(1)*(Fang**3)*Ga(2,iperm(2,ibond))/2.d0
end do

W=W/mat1%s0
dW=dW/mat1%s0
END SUBROUTINE Brenner

!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
SUBROUTINE Vrepulsv(a,mat1,Vr)
USE data_mat
implicit REAL(8) (a-h,o-z)
implicit INTEGER*4 (i-n)
TYPE(material), INTENT(IN) :: mat1
REAL(8), INTENT(IN) :: a
REAL(8), INTENT(OUT) :: Vr(2)

aux=dsqrt(2.d0*mat1%Vs(3))*mat1%Vs(2)
ex=exp(-aux*(a-mat1%A1))
Vr(1)=mat1%Vs(1)/(mat1%Vs(3)-1.d0)*ex
Vr(2)=-Vr(1)*aux
END SUBROUTINE Vrepulsv
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
SUBROUTINE Vattract(a,mat1,Va)
USE data_mat
implicit REAL(8) (a-h,o-z)
implicit INTEGER*4 (i-n)
TYPE(material), INTENT(IN) :: mat1
REAL(8), INTENT(IN) :: a
REAL(8), INTENT(OUT) :: Va(2)

aux=dsqrt(2.d0/mat1%Vs(3))*mat1%Vs(2)
ex=exp(-aux*(a-mat1%A1))
Va(1)=mat1%Vs(1)*mat1%Vs(3)/(mat1%Vs(3)-1.d0)*ex
Va(2)=-Va(1)*aux
END SUBROUTINE Vattract

!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
SUBROUTINE Gangular(theta,mat1,Ga)
USE data_mat
implicit REAL(8) (a-h,o-z)
implicit INTEGER*4 (i-n)
TYPE(material), INTENT(IN) :: mat1
DIMENSION :: theta(3)
DIMENSION :: Ga(2,3)

do i=1,3
  aux1=1.d0+dcos(theta(i))
  aux2=(mat1%Va(3)+aux1*aux1)
  Ga(1,i)=1.d0+(mat1%Va(2)/mat1%Va(3))-mat1%Va(2)/aux2
  Ga(2,i)=-2.d0/aux2/aux2*mat1%Va(2)*aux1*dsin(theta(i))
end do
Ga=Ga*mat1%Va(1)

END SUBROUTINE Gangular

!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
! NOW STARTS THE INNER PART
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

SUBROUTINE Inner_Brenner(mat1,pe,dpedeta,ddpedeta,W,dWdeta,ddWdeta,dW)
!SUBROUTINE Inner_Brenner(mat1,pe,W,dW,ddW)
USE data_mat
USE data_vector3
USE data_vector2
implicit REAL(8) (a-h,o-z)
implicit INTEGER*4 (i-n)
DIMENSION :: pe(6),Vr(3), Va(3),dWdeta(2),ddWdeta(3),aux1(3)
DIMENSION :: Ga(3,3), iperm(2,3), dW(6), ddW(6,6)
! 1 is 1,1, 2 is 2,2 and 3 is both 1,2 and 2,1
TYPE(vector2) :: dpedeta(6),aux(6)
TYPE(vector3) :: ddpedeta(6)
TYPE(material) mat1
data iperm(1,1:3) /2, 3, 1/
data iperm(2,1:3) /3, 1, 2/

W=0.
dW=0.d0
ddW=0.d0
call Gang_bis(pe(4:6),mat1,Ga)

do ibond=1,3
  Fang=1.d0/dsqrt(1.d0+Ga(1,iperm(1,ibond))+Ga(1,iperm(2,ibond)))
  call Vrep_bis(pe(ibond),mat1,Vr)
  call Vatt_bis(pe(ibond),mat1,Va)
  W=W+Vr(1)-Fang*Va(1)
  dW(ibond)=Vr(2)-Fang*Va(2)
  dW(3+iperm(1,ibond))=dW(3+iperm(1,ibond))+Va(1)*(Fang**3)*Ga(2,iperm(1,ibond))/2.d0
  dW(3+iperm(2,ibond))=dW(3+iperm(2,ibond))+Va(1)*(Fang**3)*Ga(2,iperm(2,ibond))/2.d0
  ddW(ibond,ibond)=Vr(3)-Fang*Va(3)
  ddW(3+iperm(1,ibond),3+iperm(1,ibond))=ddW(3+iperm(1,ibond),3+iperm(1,ibond))+ &
        Ga(3,iperm(1,ibond))/2.d0*Va(1)*(Fang**3) &
	   -3.d0/4.d0*((Ga(2,iperm(1,ibond)))**2)*Va(1)*(Fang**5)
  ddW(3+iperm(2,ibond),3+iperm(2,ibond))=ddW(3+iperm(2,ibond),3+iperm(2,ibond))+ &
        Ga(3,iperm(2,ibond))/2.d0*Va(1)*(Fang**3) &
	   -3.d0/4.d0*((Ga(2,iperm(2,ibond)))**2)*Va(1)*(Fang**5)
  do kk=1,2
    ddW(ibond,3+iperm(kk,ibond))=Va(2)*(Fang**3)*Ga(2,iperm(kk,ibond))/2.d0
  end do
  ddW(3+iperm(1,ibond),3+iperm(2,ibond))=-3./4.*Va(1)*(Fang**5)* &
             Ga(2,iperm(1,ibond))*Ga(2,iperm(2,ibond))
end do

!fill in lower part of symmetric matrix
ddW(4,6)=ddW(6,4)
ddW(5,4)=ddW(4,5)
ddW(6,5)=ddW(5,6)
do i=4,6
  do j=1,3
    ddW(i,j)=ddW(j,i)
  end do
end do
W=W/mat1%s0
dW=dW/mat1%s0
ddW=ddW/mat1%s0

do i=1,6
  aux(i)%val=[0.d0,0.d0]
  do j=1,6
    aux(i)%val=aux(i)%val + ddW(i,j)*dpedeta(j)%val
  end do
end do

dWdeta=[0.,0.]
ddWdeta=[0.,0.,0.]
do i=1,6
  dWdeta=dWdeta+dW(i)*dpedeta(i)%val
  aux1=[aux(i)%val(1)*dpedeta(i)%val(1), &
        aux(i)%val(2)*dpedeta(i)%val(2), &
	   (aux(i)%val(1)*dpedeta(i)%val(2) + aux(i)%val(2)*dpedeta(i)%val(1))/2.]
  ddWdeta=ddWdeta+aux1+dW(i)*ddpedeta(i)%val
end do

END SUBROUTINE Inner_Brenner


!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
SUBROUTINE Vrep_bis(a,mat1,Vr)
USE data_mat
implicit REAL(8) (a-h,o-z)
implicit INTEGER*4 (i-n)
TYPE(material), INTENT(IN) :: mat1
REAL(8), INTENT(IN) :: a
REAL(8), INTENT(OUT) :: Vr(3)

aux=dsqrt(2.d0*mat1%Vs(3))*mat1%Vs(2)
ex=exp(-aux*(a-mat1%A1))
Vr(1)=mat1%Vs(1)/(mat1%Vs(3)-1.d0)*ex
Vr(2)=-Vr(1)*aux
Vr(3)= Vr(1)*aux*aux
END SUBROUTINE Vrep_bis
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
SUBROUTINE Vatt_bis(a,mat1,Va)
USE data_mat
implicit REAL(8) (a-h,o-z)
implicit INTEGER*4 (i-n)
TYPE(material), INTENT(IN) :: mat1
REAL(8), INTENT(IN) :: a
REAL(8), INTENT(OUT) :: Va(3)

aux=dsqrt(2.d0/mat1%Vs(3))*mat1%Vs(2)
ex=exp(-aux*(a-mat1%A1))
Va(1)=mat1%Vs(1)*mat1%Vs(3)/(mat1%Vs(3)-1.d0)*ex
Va(2)=-Va(1)*aux
Va(3)= Va(1)*aux*aux

END SUBROUTINE Vatt_bis
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
SUBROUTINE Gang_bis(theta,mat1,Ga)
USE data_mat
implicit REAL(8) (a-h,o-z)
implicit INTEGER*4 (i-n)
TYPE(material), INTENT(IN) :: mat1
DIMENSION :: theta(3)
DIMENSION :: Ga(3,3)

do i=1,3
  aux0=dcos(theta(i))
  aux1=1.d0+aux0
  aux2=(mat1%Va(3)+aux1*aux1)
  Ga(1,i)=1.d0+(mat1%Va(2)/mat1%Va(3))-(mat1%Va(2)/aux2)
  Ga(2,i)=-2.d0/aux2/aux2*mat1%Va(2)*aux1*dsin(theta(i))
  Ga(3,i)= 2.d0/aux2/aux2/aux2*mat1%Va(2)* &
          ((1.d0-aux0-2.d0*aux0*aux0)*aux2 - 4.*(aux1*dsin(theta(i)))**2)
end do
Ga=Ga*mat1%Va(1)

END SUBROUTINE Gang_bis
