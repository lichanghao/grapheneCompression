SUBROUTINE ener_elem(inner_fail,xneigh,ngauss,shapef,F0_el,ielem,numel,nW_hat, &
                     mat1,crit,weight,f_elem,W_elem,eta)
USE data_vector2
USE data_vector3
USE data_mat
implicit REAL(8) (a-h,o-z)
implicit INTEGER*4 (i-n)
TYPE(vector2) :: eta(ngauss,numel)
REAL(8) :: F0_el(2,2)
TYPE(vector3) :: dC(12,3), dcurv(12,3)
TYPE(vector3) :: dcurvppaldC(2), dcurvppaldk(2), dvppaldC(2,2), dvppaldk(2,2)
TYPE(vector3) :: dnorm(12,3), dpedC(6), dpedk(6)
TYPE(material) mat1
REAL(8) shapef(ngauss,12,6), weight(ngauss), f_elem(12,3)
DIMENSION :: xneigh(12,3), C_elem(3), xnor_elem(3)
DIMENSION :: curv0_elem(3), curvppal(2), vppal(2,2), pe(6), dW(6), dW_(6)
DIMENSION :: S_n(3), S_m(3), S_h(2), A_norm(3), Ei(3,2), eta_gauss(2)
LOGICAL :: flag_num_diff
DIMENSION :: C_elem_(3), curv0_elem_(3), curvppal_(2), vppal_(2,2)
PARAMETER(h=1.d-8)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
DIMENSION ::pe_(6)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

f_elem=0.d0
W_elem=0.d0
do igauss=1,ngauss
  call metric(xneigh,shapef(igauss,:,2:3),F0_el,C_elem,dC,xnor_elem,dnorm)
  call curv(xneigh,shapef(igauss,:,4:6),F0_el,xnor_elem,dnorm,curv0_elem,dcurv)
  call principal(C_elem,curv0_elem,curvppal,vppal, &
                 dcurvppaldC,dcurvppaldk,dvppaldC,dvppaldk,flag_num_diff)
! Constitutive inner relaxation
  if (nW_hat.eq.1) then
    eta_gauss=eta(igauss,ielem)%val
    maxn=25
    call newton_inner(C_elem,curvppal,vppal,mat1,eta_gauss,W,dW,crit,numiter,maxn)

    if (numiter.ge.maxn) then
       inner_fail=inner_fail+1
    else
       eta(igauss,ielem)%val=eta_gauss
    end if
  end if
! update the bond vectors with the inner displacements
  do ibond=1,3
    Ei(ibond,:)=mat1%A0*mat1%E(ibond,:)
    Ei(ibond,:)=Ei(ibond,:) + eta(igauss,ielem)%val(:)
    A_norm(ibond)=sqrt(Ei(ibond,1)*Ei(ibond,1)+Ei(ibond,2)*Ei(ibond,2))
    Ei(ibond,:)=Ei(ibond,:)/A_norm(ibond)
  end do
  if (flag_num_diff) then
       ! Stresses obtained by numerical differentiation
       ! because k1=k2
!!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ Xu comment this out
 !      write(*,*)  ' NUMERICAL DIFFERENTIATION', ielem,igauss
 !      write(77,*) ' NUMERICAL DIFFERENTIATION', ielem,igauss
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
       ! Exponential map
       call def_bonds_(C_elem,curvppal,vppal,A_norm,Ei,pe)
       call Hyper_Pot(mat1,pe,W,dW)
 
       ! Half of second Piola-Kichhoff stress
       do i=1,3
         C_elem_=C_elem
         curv0_elem_=curv0_elem
         C_elem_(i)=C_elem_(i)+h
         call principal_(C_elem_,curv0_elem_,curvppal_,vppal_,flag_num_diff)
         call def_bonds_(C_elem_,curvppal_,vppal_,A_norm,Ei,pe_)
!         write(*,*)"checkkkkkkkkkkkkkkkkkkk",pe_(1),pe_(2),pe_(3)        
         call Hyper_Pot(mat1,pe_,W_,dW_)
!         write(*,*)"come hereeeeeeeeeeeeeeeee",W_,W
         S_n(i)=(W_-W)/h
!         write(*,*)"writeout in the middle",i,W_,W
       enddo
       ! Bending stress
       do i=1,3
         C_elem_=C_elem
         curv0_elem_=curv0_elem
         curv0_elem_(i)=curv0_elem_(i)+h
         call principal_(C_elem_,curv0_elem_,curvppal_,vppal_,flag_num_diff)
         call def_bonds_(C_elem_,curvppal_,vppal_,A_norm,Ei,pe_)
         call Hyper_Pot(mat1,pe_,W_,dW_)
         S_m(i)=(W_-W)/h
       enddo
  else
       ! Stresses obtained analytically
       ! Exponential map
       call def_bonds(C_elem,curvppal,vppal,dcurvppaldC,dcurvppaldk, &
                      dvppaldC,dvppaldk,A_norm,Ei,pe,dpedC,dpedk)
       if (numiter.ge.maxn) then
         ! Needs only to be computed if inner has not converged, 
         ! otherwise, it has already been computed in newton_inner 
         call Hyper_Pot(mat1,pe,W,dW)
       endif
       call Stresses(dW,dpedC,dpedk,S_n,S_m)
  endif
  do ij=1,3
    f_elem=f_elem + (S_n(ij)*dC(:,:)%val(ij) + S_m(ij)*dcurv(:,:)%val(ij))*weight(igauss)
  end do

!!$!!!!!!!!!!!!!!!!!!!!!!!!!
!!$write(*,*)"s_n",s_n(1),s_n(2),s_n(3)
!!$write(*,*)"W_ ",W_
!!$write(*,*)"W",W
!!$aaaa=(W_-W)/h
!!$write(*,*)"aaaaa",h,aaaa
!!$!!!!!!!!!!!!!!!!!!!!!!!!!!
! Global Assignment
  W_elem=W_elem+W*weight(igauss)
end do

END SUBROUTINE ener_elem



