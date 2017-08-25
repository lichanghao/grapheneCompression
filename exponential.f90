!***************************************************************************
!***************************************************************************
!***************************************************************************
! This routine computes the deformed bonds as well as their scalar products
SUBROUTINE def_bonds(C_elem,curvppal,vppal,dcurvppaldC,dcurvppaldk, &
                     dvppaldC,dvppaldk,A_norm,Ei,pe,dpedC,dpedk)
USE data_mat
USE data_vector3
implicit REAL(8) (a-h,o-z)
implicit INTEGER*4 (i-n)
DIMENSION :: C_elem(3), curvppal(2), vppal(2,2),a_def(3,3),pe(6), iperm(2,3)
DIMENSION :: dtemp11(3)
DIMENSION :: dtemp22(3), dtemp3(3), dtemp4(3), ttemp(2)
DIMENSION :: dpdC(3), dqdC(3), dpdk(3), dqdk(3), A_norm(3), Ei(3,2)
TYPE(vector3) :: dcurvppaldC(2), dcurvppaldk(2), dvppaldC(2,2), dvppaldk(2,2)
TYPE(vector3) :: dadC(3,3), dpedC(6), dadk(3,3), dpedk(6)
data iperm(1,1:3) /2, 3, 1/
data iperm(2,1:3) /3, 1, 2/


do i=1,3
  ! Compute p and q
  ! ===============
  ttemp(1)=C_elem(1)*Ei(i,1)+C_elem(3)*Ei(i,2)
  ttemp(2)=C_elem(3)*Ei(i,1)+C_elem(2)*Ei(i,2)

  temp3=vppal(1,1)*ttemp(1)+vppal(1,2)*ttemp(2)
  temp4=vppal(2,1)*ttemp(1)+vppal(2,2)*ttemp(2)
  p=A_norm(i)*temp3
  q=A_norm(i)*temp4

  ! Derivatives wrt C
  ! -----------------
  dtemp3=dvppaldC(1,1)%val*ttemp(1)+dvppaldC(1,2)%val*ttemp(2) + &
         [vppal(1,1)*Ei(i,1),vppal(1,2)*Ei(i,2),vppal(1,1)*Ei(i,2)+vppal(1,2)*Ei(i,1)]
  dtemp4=dvppaldC(2,1)%val*ttemp(1)+dvppaldC(2,2)%val*ttemp(2) + &
         [vppal(2,1)*Ei(i,1),vppal(2,2)*Ei(i,2),vppal(2,1)*Ei(i,2)+vppal(2,2)*Ei(i,1)]

  dpdC=dtemp3*A_norm(i)
  dqdC=dtemp4*A_norm(i)

  ! Derivatives wrt k0
  ! ------------------
  dtemp3= dvppaldk(1,1)%val*ttemp(1)+dvppaldk(1,2)%val*ttemp(2)
  dtemp4= dvppaldk(2,1)%val*ttemp(1)+dvppaldk(2,2)%val*ttemp(2)

  dpdk=dtemp3*A_norm(i)
  dqdk=dtemp4*A_norm(i)

  ! Compute the vector a
  ! ====================
  f1=sinxx(curvppal(1)*p)
  f2=sinxx(curvppal(2)*q)
  f12=sinxx(curvppal(1)*p/2.)
  f22=sinxx(curvppal(2)*q/2.)

  a_def(i,1)=p*f1
  a_def(i,2)=q*f2
  a_def(i,3)=curvppal(1)*p*p/2.d0*f12**2 + curvppal(2)*q*q/2.d0*f22**2

  ! Derivatives wrt C
  ! -----------------
  !now variables used as auxiliary
  dtemp11=p*dcurvppaldC(1)%val+curvppal(1)*dpdC
  dtemp22=q*dcurvppaldC(2)%val+curvppal(2)*dqdC
  dtemp3=p*(p*dcurvppaldC(1)%val+2.*curvppal(1)*dpdC)
  dtemp4=q*(q*dcurvppaldC(2)%val+2.*curvppal(2)*dqdC)

  g1=dsinxx(curvppal(1)*p)
  g2=dsinxx(curvppal(2)*q)
  g12=dsinxx(curvppal(1)*p/2.)
  g22=dsinxx(curvppal(2)*q/2.)

  dadC(i,1)%val=dpdC*f1+p*g1*dtemp11
  dadC(i,2)%val=dqdC*f2+q*g2*dtemp22
  dadC(i,3)%val=(dtemp3*f12*f12+curvppal(1)*p*p*f12*g12*dtemp11 + &
             dtemp4*f22*f22+curvppal(2)*q*q*f22*g22*dtemp22)/2.

  ! Derivatives wrt k0
  ! ------------------
  dtemp11=p*dcurvppaldk(1)%val+curvppal(1)*dpdk
  dtemp22=q*dcurvppaldk(2)%val+curvppal(2)*dqdk
  dtemp3=p*(p*dcurvppaldk(1)%val+2.*curvppal(1)*dpdk)
  dtemp4=q*(q*dcurvppaldk(2)%val+2.*curvppal(2)*dqdk)

  dadk(i,1)%val=dpdk*f1+p*g1*dtemp11
  dadk(i,2)%val=dqdk*f2+q*g2*dtemp22
  dadk(i,3)%val=(dtemp3*f12*f12+curvppal(1)*p*p*f12*g12*dtemp11 + &
             dtemp4*f22*f22+curvppal(2)*q*q*f22*g22*dtemp22)/2.

  ! Compute the norm of a
  ! =====================
  pe(i)=sqrt(a_def(i,1)*a_def(i,1)+a_def(i,2)*a_def(i,2)+a_def(i,3)*a_def(i,3))
  dpedC(i)%val=(dadC(i,1)%val*a_def(i,1)+dadC(i,2)%val*a_def(i,2)+dadC(i,3)%val*a_def(i,3))/pe(i)
  dpedk(i)%val=(dadk(i,1)%val*a_def(i,1)+dadk(i,2)%val*a_def(i,2)+dadk(i,3)%val*a_def(i,3))/pe(i)
end do

do k=1,3
  i=iperm(1,k)
  j=iperm(2,k)
  temp6=a_def(i,1)*a_def(j,1)+a_def(i,2)*a_def(j,2)+a_def(i,3)*a_def(j,3)
  pe(3+k)= temp6/pe(i)/pe(j)
  pe(3+k)=acos(pe(3+k))

  dtemp11=dadC(i,1)%val*a_def(j,1)+dadC(i,2)%val*a_def(j,2)+dadC(i,3)%val*a_def(j,3) + &
          dadC(j,1)%val*a_def(i,1)+dadC(j,2)%val*a_def(i,2)+dadC(j,3)%val*a_def(i,3)
  fact=(-1.)/sin(pe(3+k))/pe(i)/pe(j)
  dpedC(3+k)%val=fact*(dtemp11-temp6*(dpedC(i)%val/pe(i)+dpedC(j)%val/pe(j)))

  dtemp11=dadk(i,1)%val*a_def(j,1)+dadk(i,2)%val*a_def(j,2)+dadk(i,3)%val*a_def(j,3) + &
          dadk(j,1)%val*a_def(i,1)+dadk(j,2)%val*a_def(i,2)+dadk(j,3)%val*a_def(i,3)
  dpedk(3+k)%val=fact*(dtemp11-temp6*(dpedk(i)%val/pe(i)+dpedk(j)%val/pe(j)))
end do

END SUBROUTINE def_bonds
!***************************************************************************
!***************************************************************************
! This routine computes the deformed bonds as well as their scalar products
SUBROUTINE def_bonds_(C_elem,curvppal,vppal,A_norm,Ei,pe)
USE data_mat
USE data_vector3
implicit REAL(8) (a-h,o-z)
implicit INTEGER*4 (i-n)
DIMENSION :: C_elem(3), curvppal(2), vppal(2,2),a_def(3,3),pe(6), iperm(2,3)
DIMENSION :: ttemp(2)
DIMENSION :: A_norm(3), Ei(3,2)
data iperm(1,1:3) /2, 3, 1/
data iperm(2,1:3) /3, 1, 2/


do i=1,3
  ! Compute p and q
  ! ===============
  ttemp(1)=C_elem(1)*Ei(i,1)+C_elem(3)*Ei(i,2)
  ttemp(2)=C_elem(3)*Ei(i,1)+C_elem(2)*Ei(i,2)

  temp3=vppal(1,1)*ttemp(1)+vppal(1,2)*ttemp(2)
  temp4=vppal(2,1)*ttemp(1)+vppal(2,2)*ttemp(2)
  p=A_norm(i)*temp3
  q=A_norm(i)*temp4

  ! Compute the vector a
  ! ====================
  f1=sinxx(curvppal(1)*p)
  f2=sinxx(curvppal(2)*q)
  f12=sinxx(curvppal(1)*p/2.)
  f22=sinxx(curvppal(2)*q/2.)

  a_def(i,1)=p*f1
  a_def(i,2)=q*f2
  a_def(i,3)=curvppal(1)*p*p/2.d0*f12**2 + curvppal(2)*q*q/2.d0*f22**2

  ! Compute the norm of a
  ! =====================
  pe(i)=sqrt(a_def(i,1)*a_def(i,1)+a_def(i,2)*a_def(i,2)+a_def(i,3)*a_def(i,3))
end do

do k=1,3
  i=iperm(1,k)
  j=iperm(2,k)
  temp6=a_def(i,1)*a_def(j,1)+a_def(i,2)*a_def(j,2)+a_def(i,3)*a_def(j,3)
  pe(3+k)= temp6/pe(i)/pe(j)
  pe(3+k)=acos(pe(3+k))
end do

END SUBROUTINE def_bonds_
