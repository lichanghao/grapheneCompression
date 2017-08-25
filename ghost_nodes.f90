!*********************************************************************************
!*********************************************************************************

SUBROUTINE ghost_nodes(meshh,xx)
USE data_mesh
implicit REAL(8) (a-h,o-z)
implicit INTEGER*4 (i-n)
TYPE(mesh) :: meshh
REAL(8) ::  xx(3*(meshh%numnods+meshh%nedge)), xaux(3)
!REAL(8) :: a(3), b(3), c(3), d(3)
!dimension  ivert(3), iperm(2,3)
!data iperm(1,1:3) /2, 3, 1/
!data iperm(2,1:3) /3, 1, 2/


do ijk=1,meshh%nedge
  i1=meshh%nghost_tab(ijk,1)
  i2=meshh%nghost_tab(ijk,2)
  i3=meshh%nghost_tab(ijk,3)
  xaux=xx(3*i1-2:3*i1)+xx(3*i2-2:3*i2)-xx(3*i3-2:3*i3)    ! parallelogram
  xx(3*(meshh%numnods+ijk)-2:3*(meshh%numnods+ijk))=xaux  ! position of ghost
end do


END SUBROUTINE ghost_nodes

!*********************************************************************************
!*********************************************************************************
!*********************************************************************************
