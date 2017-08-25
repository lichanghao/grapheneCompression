
!***************************************************************************
!***************************************************************************
!***************************************************************************
! This routine computes the hyperelastic potential as well as the inner stress
! This variant is to be used in the inner relaxation

SUBROUTINE Hyper_pot_inner(C_elem,curvppal,vppal,mat1,eta,W,dWdeta,ddWdeta,dW)
USE data_mat
USE data_vector3
USE data_vector2
implicit REAL(8) (a-h,o-z)
implicit INTEGER*4 (i-n)
DIMENSION :: C_elem(3), curvppal(2), vppal(2,2),a_def(3,3),pe(6), iperm(2,3)
DIMENSION :: ttemp(2), ttemp1(2), ttemp2(2)
DIMENSION :: dpdeta(2), dqdeta(2), A_norm(3), Ei(3,2), eta(2),dWdeta(2), dW(6)
! 1 is 1,1, 2 is 2,2 and 3 is both 1,2 and 2,1
DIMENSION :: ddWdeta(3), dpdeta2(3), dqdeta2(3), aux1(3), aux2(3), aux3(3), xaux(2)
TYPE(vector2) :: dadeta(3,3), dpedeta(6)
TYPE(vector3) :: ddadeta(3,3), ddpedeta(6)
TYPE(material) mat1
data iperm(1,1:3) /2, 3, 1/
data iperm(2,1:3) /3, 1, 2/


! update the bond vectors with the inner displacements
do ibond=1,3
  Ei(ibond,1)=mat1%A0*mat1%E(ibond,1)
  Ei(ibond,2)=mat1%A0*mat1%E(ibond,2)
  Ei(ibond,1)=Ei(ibond,1) + eta(1)
  Ei(ibond,2)=Ei(ibond,2) + eta(2)
  A_norm(ibond)=sqrt(Ei(ibond,1)*Ei(ibond,1)+Ei(ibond,2)*Ei(ibond,2))
  Ei(ibond,1)=Ei(ibond,1)/A_norm(ibond)
  Ei(ibond,2)=Ei(ibond,2)/A_norm(ibond)
end do

ttemp1(1)=C_elem(1)*vppal(1,1)+C_elem(3)*vppal(1,2)
ttemp1(2)=C_elem(3)*vppal(1,1)+C_elem(2)*vppal(1,2)
ttemp2(1)=C_elem(1)*vppal(2,1)+C_elem(3)*vppal(2,2)
ttemp2(2)=C_elem(3)*vppal(2,1)+C_elem(2)*vppal(2,2)

!temp1=vppal(1,1)*ttemp1(1)+vppal(1,2)*ttemp1(2)
!temp2=vppal(2,1)*ttemp2(1)+vppal(2,2)*ttemp2(2)
dpdeta=(ttemp1)!/sqrt(temp1)
dqdeta=(ttemp2)!/sqrt(temp2)

!now the tensor products
dpdeta2(1)=dpdeta(1)*dpdeta(1)
dpdeta2(2)=dpdeta(2)*dpdeta(2)
dpdeta2(3)=dpdeta(1)*dpdeta(2)
dqdeta2(1)=dqdeta(1)*dqdeta(1)
dqdeta2(2)=dqdeta(2)*dqdeta(2)
dqdeta2(3)=dqdeta(1)*dqdeta(2)



do i=1,3
  ! Compute p and q
  ! ===============
  ttemp(1)=C_elem(1)*Ei(i,1)+C_elem(3)*Ei(i,2)
  ttemp(2)=C_elem(3)*Ei(i,1)+C_elem(2)*Ei(i,2)

  temp3=vppal(1,1)*ttemp(1)+vppal(1,2)*ttemp(2)
  temp4=vppal(2,1)*ttemp(1)+vppal(2,2)*ttemp(2)
  p=A_norm(i)*temp3!/sqrt(temp1)
  q=A_norm(i)*temp4!/sqrt(temp2)


  ! Compute the vector a
  ! ====================
  f1=sinxx(curvppal(1)*p)
  f2=sinxx(curvppal(2)*q)
  f12=sinxx(curvppal(1)*p/2.)
  f22=sinxx(curvppal(2)*q/2.)

  a_def(i,1)=p*f1
  a_def(i,2)=q*f2
  a_def(i,3)=curvppal(1)*p*p/2.d0*f12**2 + curvppal(2)*q*q/2.d0*f22**2

  ! Derivatives wrt eta
  ! -------------------
  g1=dsinxx(curvppal(1)*p)
  g2=dsinxx(curvppal(2)*q)
  g12=dsinxx(curvppal(1)*p/2.)
  g22=dsinxx(curvppal(2)*q/2.)

  dadeta(i,1)%val=(f1+curvppal(1)*p*g1)*dpdeta
  dadeta(i,2)%val=(f2+curvppal(2)*q*g2)*dqdeta
  xx1=f12+curvppal(1)*p/(2.d0)*g12
  xx2=f22+curvppal(2)*q/(2.d0)*g22
  yy1=curvppal(1)*p*f12
  yy2=curvppal(2)*q*f22
  dadeta(i,3)%val=yy1*(xx1)*dpdeta &
                 +yy2*(xx2)*dqdeta


  ! Second Derivatives wrt eta
  ! --------------------------
  h1=ddsinxx(curvppal(1)*p)
  h2=ddsinxx(curvppal(2)*q)
  h12=ddsinxx(curvppal(1)*p/2.)
  h22=ddsinxx(curvppal(2)*q/2.)

  ddadeta(i,1)%val=curvppal(1)*(2.d0*g1+curvppal(1)*p*h1)*dpdeta2
  ddadeta(i,2)%val=curvppal(2)*(2.d0*g2+curvppal(2)*q*h2)*dqdeta2
  ddadeta(i,3)%val=curvppal(1)*(xx1**2+yy1*(g12+curvppal(1)*p/4.d0*h12))*dpdeta2 &
                  +curvppal(2)*(xx2**2+yy2*(g22+curvppal(2)*q/4.d0*h22))*dqdeta2



  ! Compute the norm of a
  ! =====================
  pe(i)=sqrt(a_def(i,1)*a_def(i,1)+a_def(i,2)*a_def(i,2)+a_def(i,3)*a_def(i,3))
  dpedeta(i)%val=(dadeta(i,1)%val*a_def(i,1)+dadeta(i,2)%val*a_def(i,2)+dadeta(i,3)%val*a_def(i,3))/pe(i)
  ! this is the second derivative wrt eta of the scalar product, not the norm
  aux1=[(dadeta(i,1)%val(1))**2+(dadeta(i,2)%val(1))**2+(dadeta(i,3)%val(1))**2, &
        (dadeta(i,1)%val(2))**2+(dadeta(i,2)%val(2))**2+(dadeta(i,3)%val(2))**2, &
		 dadeta(i,1)%val(1)*dadeta(i,1)%val(2)+dadeta(i,2)%val(1)*dadeta(i,2)%val(2)+ &
		 dadeta(i,3)%val(1)*dadeta(i,3)%val(2)]
  aux1=(a_def(i,1)*ddadeta(i,1)%val+a_def(i,2)*ddadeta(i,2)%val+a_def(i,3)*ddadeta(i,3)%val+ &
             aux1)
  ! this is the tensor product of the derivative of the norm wrt eta twice 
  aux2=[dpedeta(i)%val(1)*dpedeta(i)%val(1), &
        dpedeta(i)%val(2)*dpedeta(i)%val(2), &
		dpedeta(i)%val(1)*dpedeta(i)%val(2)]
  ! the second derivative of the norm wrt to eta:
  ddpedeta(i)%val=(aux1-aux2)/pe(i)
end do

! Compute the scalar products
! ===========================
do k=1,3
  i=iperm(1,k)
  j=iperm(2,k)
  ! temp6 is the dot product and temp7 the cosine
  temp6=a_def(i,1)*a_def(j,1)+a_def(i,2)*a_def(j,2)+a_def(i,3)*a_def(j,3)
  temp7= temp6/pe(i)/pe(j)
  pe(3+k)=dacos(temp7)


  ! first derivative of angle theta wrt eta
  fact=(-1.d0)/sin(pe(3+k))
  ! ttemp is the first derivative of the cosine wrt eta
  ttemp=dadeta(i,1)%val*a_def(j,1)+dadeta(i,2)%val*a_def(j,2)+dadeta(i,3)%val*a_def(j,3) &
       +dadeta(j,1)%val*a_def(i,1)+dadeta(j,2)%val*a_def(i,2)+dadeta(j,3)%val*a_def(i,3)
  ttemp=(ttemp-temp6*(dpedeta(i)%val/pe(i)+dpedeta(j)%val/pe(j)))/pe(i)/pe(j)
  dpedeta(3+k)%val=fact*ttemp


  ! second derivative wrt eta
  ! this is the second derivative of the scalar product
  aux1=0.
  do icomp=1,3
    aux1=aux1+[2.d0*dadeta(i,icomp)%val(1)*dadeta(j,icomp)%val(1), &
	           2.d0*dadeta(i,icomp)%val(2)*dadeta(j,icomp)%val(2), &
               dadeta(i,icomp)%val(1)*dadeta(j,icomp)%val(2)+dadeta(i,icomp)%val(2)*dadeta(j,icomp)%val(1)]
  end do
  aux1=a_def(j,1)*ddadeta(i,1)%val+a_def(j,2)*ddadeta(i,2)%val+a_def(j,3)*ddadeta(i,3)%val+ &
       a_def(i,1)*ddadeta(j,1)%val+a_def(i,2)*ddadeta(j,2)%val+a_def(i,3)*ddadeta(j,3)%val+ aux1
  ! this is the second derivative of the cosine wrt eta
  xaux=pe(j)*dpedeta(i)%val+pe(i)*dpedeta(j)%val
  aux2=[2.d0*ttemp(1)*xaux(1),2.d0*ttemp(2)*xaux(2),ttemp(1)*xaux(2)+ttemp(2)*xaux(1)]
  aux2=1.d0/pe(i)/pe(j)*(aux1-aux2-temp7*(pe(j)*ddpedeta(i)%val+pe(i)*ddpedeta(j)%val+ &
       [2.d0*dpedeta(i)%val(1)*dpedeta(j)%val(1),2.d0*dpedeta(i)%val(2)*dpedeta(j)%val(2), &
	    dpedeta(i)%val(1)*dpedeta(j)%val(2)+dpedeta(i)%val(2)*dpedeta(j)%val(1)]))
  ! this is the tensor product of the derivative of the angle wrt eta twice 
  aux3=[dpedeta(3+k)%val(1)*dpedeta(3+k)%val(1), &
        dpedeta(3+k)%val(2)*dpedeta(3+k)%val(2), &
		dpedeta(3+k)%val(1)*dpedeta(3+k)%val(2)]
  ! finally, the second derivative of the angle wrt eta
  ddpedeta(3+k)%val=(-1.d0)/sin(pe(3+k))*(temp7*aux3+aux2)
end do

if (mat1%nCode_Pot.eq.1) then
  call Inner_Morse(mat1,pe,dpedeta,ddpedeta,W,dWdeta,ddWdeta,dW)
else if (mat1%nCode_Pot.eq.2) then
  call Inner_Brenner(mat1,pe,dpedeta,ddpedeta,W,dWdeta,ddWdeta,dW)
else if (mat1%nCode_Pot.eq.22) then
  call Inner_Brenner2(mat1,pe,dpedeta,ddpedeta,W,dWdeta,ddWdeta,dW)
else if (mat1%nCode_Pot.eq.3) then
  call Inner_MM3(mat1,pe,dpedeta,ddpedeta,W,dWdeta,ddWdeta,dW)
else
  STOP 'Atomistic description not implemented'
endif


END SUBROUTINE Hyper_pot_inner


