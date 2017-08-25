
!***************************************************************************
!***************************************************************************
!***************************************************************************

SUBROUTINE metric(xneigh,DN,F0,C_elem,dC,xnor_elem,dnorm)
USE data_vector3
implicit REAL(8) (a-h,o-z)
implicit INTEGER*4 (i-n)
TYPE(vector3) :: dC(12,3)
TYPE(vector3) :: dnorm(12,3), dg(12,3)
REAL(8) :: g_elem(3), C_elem(3)
REAL(8) :: xnor_elem(3), DN(12,2), xneigh(12,3), g_convect(2,3)
DIMENSION :: neigh(12), F0(2,2), dJ(12,3), temp(12,3)
DIMENSION :: temp1(3,12,3)

C_elem=0.d0
xnor_elem=0.d0

! Cartesian coordinates in R^3 of the convected basis vectors
do i=1,2
  do j=1,3
    g_convect(i,j)=0.d0
    do k=1,12
       g_convect(i,j)=g_convect(i,j)+DN(k,i)*xneigh(k,j)
    enddo
  enddo
enddo


! Covariant components of the metric tensor
g_elem(1)=g_convect(1,1)**2+g_convect(1,2)**2+g_convect(1,3)**2
g_elem(2)=g_convect(2,1)**2+g_convect(2,2)**2+g_convect(2,3)**2
g_elem(3)=g_convect(1,1)*g_convect(2,1)+g_convect(1,2)*g_convect(2,2) &
           +g_convect(1,3)*g_convect(2,3)
! Pull-back of the metric, Cartesian components of Green Def Tensor in R^2
C_elem(1)=g_elem(1)*F0(1,1)*F0(1,1) + & 
     2.d0*g_elem(3)*F0(1,1)*F0(2,1) + g_elem(2)* F0(2,1)*F0(2,1) 
C_elem(3)=g_elem(1)*F0(1,1)*F0(1,2) + g_elem(3)*(F0(1,1)*F0(2,2)+F0(1,2)*F0(2,1)) + &
          g_elem(2)* F0(2,1)*F0(2,2)
C_elem(2)=g_elem(1)*F0(1,2)*F0(1,2) + &
     2.d0*g_elem(3)*F0(1,2)*F0(2,2) + g_elem(2)* F0(2,2)*F0(2,2)

xnor_elem(1)=g_convect(1,2)*g_convect(2,3)-g_convect(1,3)*g_convect(2,2)
xnor_elem(2)=g_convect(1,3)*g_convect(2,1)-g_convect(1,1)*g_convect(2,3)
xnor_elem(3)=g_convect(1,1)*g_convect(2,2)-g_convect(1,2)*g_convect(2,1)

xnorm=sqrt(xnor_elem(1)**2+xnor_elem(2)**2+xnor_elem(3)**2)
xnor_elem=xnor_elem/xnorm


temp1=0.d0
! temp is (NI,2 g_1 - NI,1 g_2)
do idof=1,3
  temp(:,idof)=DN(:,2)*g_convect(1,idof) - DN(:,1)*g_convect(2,idof)
enddo
! temp1(a,:,:) is (i_a x temp)
temp1(1,:,2)=-temp(:,3)
temp1(1,:,3)= temp(:,2)
temp1(2,:,1)= temp(:,3)
temp1(2,:,3)=-temp(:,1)
temp1(3,:,1)=-temp(:,2)
temp1(3,:,2)= temp(:,1)


do idof=1,3
  dg(:,idof)%val(1)=2.d0*DN(:,1)*g_convect(1,idof)
  dg(:,idof)%val(2)=2.d0*DN(:,2)*g_convect(2,idof)
  dg(:,idof)%val(3)=DN(:,1)*g_convect(2,idof)+DN(:,2)*g_convect(1,idof)
  dJ(:,idof)=( (DN(:,1)*g_elem(2) - DN(:,2)*g_elem(3))*g_convect(1,idof)  &
            -  (DN(:,1)*g_elem(3) - DN(:,2)*g_elem(1))*g_convect(2,idof) )/xnorm
  do ia=1,3
    dnorm(:,idof)%val(ia)= (temp1(ia,:,idof) - xnor_elem(ia)*dJ(:,idof) )/xnorm
  enddo

enddo

dC(:,:)%val(1)=dg(:,:)%val(1)*F0(1,1)*F0(1,1) + & 
    2.d0*dg(:,:)%val(3)*F0(1,1)*F0(2,1) + dg(:,:)%val(2)* F0(2,1)*F0(2,1) 
dC(:,:)%val(3)=dg(:,:)%val(1)*F0(1,1)*F0(1,2) + dg(:,:)%val(3)*(F0(1,1)*F0(2,2)+F0(1,2)*F0(2,1)) + &
         dg(:,:)%val(2)* F0(2,1)*F0(2,2)
dC(:,:)%val(2)=dg(:,:)%val(1)*F0(1,2)*F0(1,2) + &
    2.d0*dg(:,:)%val(3)*F0(1,2)*F0(2,2) + dg(:,:)%val(2)* F0(2,2)*F0(2,2)


END SUBROUTINE metric

!***************************************************************************
!***************************************************************************
!***************************************************************************

SUBROUTINE curv(xneigh,DDN,F0,xnor_elem,dnorm,curv0_elem,dcurv)
USE data_tensor22
USE data_vector3
implicit REAL(8) (a-h,o-z)
implicit INTEGER*4 (i-n)
TYPE(vector3) :: dcurv(12,3), dk(12,3)
TYPE(vector3) :: dnorm(12,3)
REAL(8) xnor_elem(3), DDN(12,3), xneigh(12,3), aux(3,3), F0(2,2)
DIMENSION neigh(12), curv0_aux(3), curv0_elem(3)

curv0_elem=0.d0
curv0_aux=0.d0

! aux(1,:) is g_1,1, aux(2,:) is g_2,2 and aux(3,:) is g_1,2
do i=1,3
  do j=1,3
    aux(i,j)=0.d0
	do k=1,12
	  aux(i,j)=aux(i,j)+DDN(k,i)*xneigh(k,j)
	enddo
  enddo
enddo


! Covariant components of curvature tensor
curv0_aux(1)=xnor_elem(1)*aux(1,1)+xnor_elem(2)*aux(1,2)+xnor_elem(3)*aux(1,3)
curv0_aux(2)=xnor_elem(1)*aux(2,1)+xnor_elem(2)*aux(2,2)+xnor_elem(3)*aux(2,3)
curv0_aux(3)=xnor_elem(1)*aux(3,1)+xnor_elem(2)*aux(3,2)+xnor_elem(3)*aux(3,3)
! Pull-back of curvature tensor
curv0_elem(1)=curv0_aux(1)*F0(1,1)*F0(1,1) + & 
         2.d0*curv0_aux(3)*F0(1,1)*F0(2,1) + curv0_aux(2)* F0(2,1)*F0(2,1) 
curv0_elem(3)=curv0_aux(1)*F0(1,1)*F0(1,2) + curv0_aux(3)*(F0(1,1)*F0(2,2)+F0(1,2)*F0(2,1)) + &
              curv0_aux(2)* F0(2,1)*F0(2,2)
curv0_elem(2)=curv0_aux(1)*F0(1,2)*F0(1,2) + &
         2.d0*curv0_aux(3)*F0(1,2)*F0(2,2) + curv0_aux(2)* F0(2,2)*F0(2,2)


do idof=1,3
  dk(:,idof)%val(1)= ( aux(1,1)*dnorm(:,idof)%val(1) + aux(1,2)*dnorm(:,idof)%val(2) + &
                       aux(1,3)*dnorm(:,idof)%val(3) ) + DDN(:,1)*xnor_elem(idof)
  dk(:,idof)%val(2)= ( aux(2,1)*dnorm(:,idof)%val(1) + aux(2,2)*dnorm(:,idof)%val(2) + &
                       aux(2,3)*dnorm(:,idof)%val(3) ) + DDN(:,2)*xnor_elem(idof)
  dk(:,idof)%val(3)= ( aux(3,1)*dnorm(:,idof)%val(1) + aux(3,2)*dnorm(:,idof)%val(2) + &
                       aux(3,3)*dnorm(:,idof)%val(3) ) + DDN(:,3)*xnor_elem(idof)

enddo


dcurv(:,:)%val(1)=dk(:,:)%val(1)*F0(1,1)*F0(1,1) + & 
    2.d0*dk(:,:)%val(3)*F0(1,1)*F0(2,1) + dk(:,:)%val(2)* F0(2,1)*F0(2,1) 
dcurv(:,:)%val(3)=dk(:,:)%val(1)*F0(1,1)*F0(1,2) + dk(:,:)%val(3)*(F0(1,1)*F0(2,2)+F0(1,2)*F0(2,1)) + &
         dk(:,:)%val(2)* F0(2,1)*F0(2,2)
dcurv(:,:)%val(2)=dk(:,:)%val(1)*F0(1,2)*F0(1,2) + &
    2.d0*dk(:,:)%val(3)*F0(1,2)*F0(2,2) + dk(:,:)%val(2)* F0(2,2)*F0(2,2)


END SUBROUTINE curv

