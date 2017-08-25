!***************************************************************************
!***************************************************************************
!***************************************************************************
! This routine finds the principal curvatures as well as the principal
! direction in the undeformed body Euclidean coordinate system
SUBROUTINE principal(C_elem,curv0_elem,curvppal,vppal, &
                     dcurvppaldC,dcurvppaldk,dvppaldC,dvppaldk,flag_num_diff)
USE data_vector3
implicit REAL(8) (a-h,o-z)
implicit INTEGER*4 (i-n)
REAL(8) :: C_elem(3), curv0_elem(3)
REAL(8) :: curvppal(2), vppal(2,2)
TYPE(vector3) :: dcurvppaldC(2), dcurvppaldk(2), dvppaldC(2,2), dvppaldk(2,2)
DIMENSION :: temp1(3), temp2(3), iperm(2)
DIMENSION :: C1vppal(2,2), C2vppal(2,2)
data iperm(1:2) /2, 1/
LOGICAL :: flag_num_diff

! First, principal curvatures
! ===========================

detC=C_elem(1)*C_elem(2)-C_elem(3)*C_elem(3)
detk0=curv0_elem(1)*curv0_elem(2)-curv0_elem(3)*curv0_elem(3)
alpha=(C_elem(1)*curv0_elem(2)+C_elem(2)*curv0_elem(1)- &
       2.d0*C_elem(3)*curv0_elem(3))
xmean=alpha/2.d0/detC
gauss=detk0/detC
beta=sqrt(xmean*xmean-gauss)
curvppal(1)=xmean+beta
curvppal(2)=xmean-beta


!write(*,*) !'hola'

! Second, principal directions
! ============================
if (abs(beta).lt.1.d-6) then
!*******************************************************************************
!************  CASE 1: k1=k2, this case is dificult and exceptional  ***********
!*******************************************************************************
  ! The surface is a plane or a sphere, any C-orthogonal directions will do
  ! Make sure these directions always work!!!!
  flag_num_diff=.TRUE.
 ! write(*,*) ' REPEATED Ppal Curv '

  ! First, pick any two C-orthogonal directions, and normalize them
  vppal(1,1)= C_elem(1)
  vppal(1,2)= C_elem(3)
  vppal(2,1)=-C_elem(3)*(C_elem(1)+C_elem(2))
  vppal(2,2)= C_elem(1)*C_elem(1)+C_elem(3)*C_elem(3)

  do ipp=1,2
    fkk=sqrt(C_elem(1)*vppal(ipp,1)*vppal(ipp,1) + &
        2.d0*C_elem(3)*vppal(ipp,1)*vppal(ipp,2) + &
             C_elem(2)*vppal(ipp,2)*vppal(ipp,2))
    vppal(ipp,:)=vppal(ipp,:)/fkk
  enddo

  dcurvppaldk(1)%val=0.d0
  dcurvppaldC(1)%val=0.d0
  dcurvppaldk(2)%val=0.d0
  dcurvppaldC(2)%val=0.d0
  dvppaldk(1,1)%val=0.d0
  dvppaldC(1,1)%val=0.d0
  dvppaldk(1,2)%val=0.d0
  dvppaldC(1,2)%val=0.d0
  dvppaldk(2,2)%val=0.d0
  dvppaldC(2,2)%val=0.d0
  dvppaldk(2,1)%val=0.d0
  dvppaldC(2,1)%val=0.d0
else

!*******************************************************************************
!************  CASE 2: k1.ne.k2, this case is simple and common  ***************
!*******************************************************************************
  flag_num_diff=.FALSE.
  ! First ppal direction
  C1vppal(1,1)=-curv0_elem(3)+curvppal(1)*C_elem(3)
  C1vppal(1,2)= curv0_elem(1)-curvppal(1)*C_elem(1)
  C2vppal(1,1)=-curv0_elem(2)+curvppal(1)*C_elem(2)
  C2vppal(1,2)= curv0_elem(3)-curvppal(1)*C_elem(3)
  test1=maxval(abs(C1vppal(1,:)))
  test2=maxval(abs(C2vppal(1,:)))
  if (test1>test2) then
    vppal(1,:)=C1vppal(1,:)
  else 
    vppal(1,:)=C2vppal(1,:)
  end if
  ! Second ppal direction
  C1vppal(2,1)=-curv0_elem(3)+curvppal(2)*C_elem(3)
  C1vppal(2,2)= curv0_elem(1)-curvppal(2)*C_elem(1)
  C2vppal(2,1)=-curv0_elem(2)+curvppal(2)*C_elem(2)
  C2vppal(2,2)= curv0_elem(3)-curvppal(2)*C_elem(3)
  test3=maxval(abs(C1vppal(2,:)))
  test4=maxval(abs(C2vppal(2,:)))
  if (test3>test4) then
    vppal(2,:)=C1vppal(2,:)
  else
    vppal(2,:)=C2vppal(2,:)
  end if
  ! Pick Largest ppal direction, then pick its orthogonal
  if (max(test1,test2)>max(test3,test4)) then
    ! the first rules
    vppal(2,1)=-C_elem(3)*vppal(1,1)-C_elem(2)*vppal(1,2)
    vppal(2,2)= C_elem(1)*vppal(1,1)+C_elem(3)*vppal(1,2)
  else
    ! the second rules
    vppal(1,1)=-C_elem(3)*vppal(2,1)-C_elem(2)*vppal(2,2)
    vppal(1,2)= C_elem(1)*vppal(2,1)+C_elem(3)*vppal(2,2)
  end if
  ! Normalize
  do ipp=1,2
    fkk=sqrt(     C_elem(1)*vppal(ipp,1)*vppal(ipp,1) + &
        2.d0*C_elem(3)*vppal(ipp,1)*vppal(ipp,2) + &
             C_elem(2)*vppal(ipp,2)*vppal(ipp,2))
    vppal(ipp,:)=vppal(ipp,:)/fkk
  enddo

! Derivatives of ppal curvatures and ppal directions
! ==================================================
  temp1=[vppal(1,1)*vppal(2,1), &
         vppal(1,2)*vppal(2,2), &
         vppal(1,1)*vppal(2,2)+vppal(2,1)*vppal(1,2)]
  do ipp=1,2
    temp2=[vppal(ipp,1)*vppal(ipp,1), &
           vppal(ipp,2)*vppal(ipp,2), 2.d0*vppal(ipp,1)*vppal(ipp,2)]
    dcurvppaldk(ipp)%val=1.d0*temp2
    dcurvppaldC(ipp)%val=-curvppal(ipp)*dcurvppaldk(ipp)%val

    jpp=iperm(ipp)
    dvppaldk(ipp,1)%val=temp1*vppal(jpp,1)/((curvppal(ipp))-(curvppal(jpp)))
    dvppaldk(ipp,2)%val=temp1*vppal(jpp,2)/((curvppal(ipp))-(curvppal(jpp)))

    dvppaldC(ipp,1)%val= -curvppal(ipp)*dvppaldk(ipp,1)%val -.5d0*vppal(ipp,1)*temp2
    dvppaldC(ipp,2)%val= -curvppal(ipp)*dvppaldk(ipp,2)%val -.5d0*vppal(ipp,2)*temp2
  enddo

end if


END SUBROUTINE principal
!***************************************************************************
! This routine finds the principal curvatures as well as the principal
! direction in the undeformed body Euclidean coordinate system
SUBROUTINE principal_(C_elem,curv0_elem,curvppal,vppal,flag_num_diff)
USE data_vector3
implicit REAL(8) (a-h,o-z)
implicit INTEGER*4 (i-n)
REAL(8), INTENT(IN) :: C_elem(3), curv0_elem(3)
REAL(8), INTENT(OUT) :: curvppal(2), vppal(2,2)
DIMENSION :: C1vppal(2,2), C2vppal(2,2)
TYPE(vector3) :: Phi(2,2)
LOGICAL :: flag_num_diff


! First, principal curvatures
! ===========================

detC=C_elem(1)*C_elem(2)-C_elem(3)*C_elem(3)
detk0=curv0_elem(1)*curv0_elem(2)-curv0_elem(3)*curv0_elem(3)
alpha=(C_elem(1)*curv0_elem(2)+C_elem(2)*curv0_elem(1)- &
       2.d0*C_elem(3)*curv0_elem(3))
xmean=alpha/2.d0/detC
gauss=detk0/detC
beta=sqrt(xmean*xmean-gauss)
curvppal(1)=xmean+beta
curvppal(2)=xmean-beta


!write(*,*) !'hola'

! Second, principal directions
! ============================
if (flag_num_diff) then
!  write(*,*) ' REPEATED Ppal Curv_ '
  ! First, pick any two C-orthogonal directions, and normalize them
  vppal(1,1)= C_elem(1)
  vppal(1,2)= C_elem(3)
  vppal(2,1)=-C_elem(3)*(C_elem(1)+C_elem(2))
  vppal(2,2)= C_elem(1)*C_elem(1)+C_elem(3)*C_elem(3)
  do ipp=1,2
    fkk=sqrt(C_elem(1)*vppal(ipp,1)*vppal(ipp,1) + &
        2.d0*C_elem(3)*vppal(ipp,1)*vppal(ipp,2) + &
             C_elem(2)*vppal(ipp,2)*vppal(ipp,2))
    vppal(ipp,:)=vppal(ipp,:)/fkk
  enddo
else
!*******************************************************************************
!************  CASE 2: k1.ne.k2, this case is simple and common  ***************
!*******************************************************************************
  ! First ppal direction
  C1vppal(1,1)=-curv0_elem(3)+curvppal(1)*C_elem(3)
  C1vppal(1,2)= curv0_elem(1)-curvppal(1)*C_elem(1)
  C2vppal(1,1)=-curv0_elem(2)+curvppal(1)*C_elem(2)
  C2vppal(1,2)= curv0_elem(3)-curvppal(1)*C_elem(3)
  test1=maxval(abs(C1vppal(1,:)))
  test2=maxval(abs(C2vppal(1,:)))
  if (test1>test2) then
    vppal(1,:)=C1vppal(1,:)
  else 
    vppal(1,:)=C2vppal(1,:)
  end if
  ! Second ppal direction
  C1vppal(2,1)=-curv0_elem(3)+curvppal(2)*C_elem(3)
  C1vppal(2,2)= curv0_elem(1)-curvppal(2)*C_elem(1)
  C2vppal(2,1)=-curv0_elem(2)+curvppal(2)*C_elem(2)
  C2vppal(2,2)= curv0_elem(3)-curvppal(2)*C_elem(3)
  test3=maxval(abs(C1vppal(2,:)))
  test4=maxval(abs(C2vppal(2,:)))
  if (test3>test4) then
    vppal(2,:)=C1vppal(2,:)
  else
    vppal(2,:)=C2vppal(2,:)
  end if
  ! Pick Largest ppal direction, then pick its orthogonal
  if (max(test1,test2)>max(test3,test4)) then
    ! the first rules
    vppal(2,1)=-C_elem(3)*vppal(1,1)-C_elem(2)*vppal(1,2)
    vppal(2,2)= C_elem(1)*vppal(1,1)+C_elem(3)*vppal(1,2)
  else
    ! the second rules
    vppal(1,1)=-C_elem(3)*vppal(2,1)-C_elem(2)*vppal(2,2)
    vppal(1,2)= C_elem(1)*vppal(2,1)+C_elem(3)*vppal(2,2)
  end if
  ! Normalize
  do ipp=1,2
    fkk=sqrt(     C_elem(1)*vppal(ipp,1)*vppal(ipp,1) + &
        2.d0*C_elem(3)*vppal(ipp,1)*vppal(ipp,2) + &
             C_elem(2)*vppal(ipp,2)*vppal(ipp,2))
    vppal(ipp,:)=vppal(ipp,:)/fkk
  enddo
end if

END SUBROUTINE principal_
