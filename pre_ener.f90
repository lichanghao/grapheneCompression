SUBROUTINE short(x0,x0_short,mdofOP,ndofBC,ndofOP)
implicit REAL(8) (a-h,o-z)
implicit INTEGER*4 (i-n)
DIMENSION mdofOP(ndofOP)
REAL(8) x0(ndofBC+ndofOP), x0_short(ndofOP)

!write(*,*) ndofBC,ndofOP

x0_short=x0(mdofOP(:))

END SUBROUTINE short

!****************************************************************************
!****************************************************************************
!****************************************************************************

SUBROUTINE long(x0,x0_short,x0_BC,mdofBC,mdofOP,ndofBC,ndofOP,mesh0)
USE data_mesh
implicit REAL(8) (a-h,o-z)
implicit INTEGER*4 (i-n)
TYPE(mesh) :: mesh0
DIMENSION x0(3*(mesh0%numnods+mesh0%nedge)), x0_short(ndofOP),x0_BC(ndofBC), & 
          mdofBC(ndofBC),mdofOP(ndofOP)

x0(mdofOP(:))=x0_short(:)
x0(mdofBC(:))=x0_BC(:)
!call ghost_nodes(mesh0,x0)

END SUBROUTINE long

!****************************************************************************
!****************************************************************************
!****************************************************************************
!****************************************************************************

SUBROUTINE force_ghost(forces,mesh0)
USE data_mesh
implicit REAL(8) (a-h,o-z)
implicit INTEGER*4 (i-n)
TYPE(mesh) :: mesh0
DIMENSION forces(3*(mesh0%numnods+mesh0%nedge))

do ighost=1,mesh0%nedge 
  i=mesh0%numnods+ighost
  i1=mesh0%nghost_tab(ighost,1)
  i2=mesh0%nghost_tab(ighost,2)
  i3=mesh0%nghost_tab(ighost,3)
  forces(3*i1-2:3*i1)=forces(3*i1-2:3*i1)+forces(3*i-2:3*i)
  forces(3*i2-2:3*i2)=forces(3*i2-2:3*i2)+forces(3*i-2:3*i)
  forces(3*i3-2:3*i3)=forces(3*i3-2:3*i3)-forces(3*i-2:3*i)
enddo


END SUBROUTINE force_ghost

!****************************************************************************



