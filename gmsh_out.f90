SUBROUTINE gmsh_out(xx,meshh,name,ntab)
USE data_mesh
implicit REAL(8) (a-h,o-z)
implicit INTEGER*4 (i-n)
CHARACTER(80), INTENT(IN) :: name
TYPE(mesh) :: meshh
REAL(8) ::  xx(3*(meshh%numnods))
INTEGER(4) :: ntab(meshh%numnods)
!!$INTEGER(4), ALLOCATABLE :: ntab(:)

open(unit=99,file=name,status='unknown')

!!$ALLOCATE(ntab(meshh%numnods),STAT=istat)
!!$if (istat/=0) STOP '**** Not enough memory **** gmsh_out'
ntab=0

! renumber
if (meshh%nelem_ghost.gt.0) then
  ii=0
  do inode=1,meshh%numnods
    if (all(inode.ne.meshh%node_ghost(:))) then
      ii=ii+1
      ntab(inode)=ii
    endif
  end do
else
  do inode=1,meshh%numnods
      ntab(inode)=inode
  end do
endif


! NODES
write(99,'(A)') '$NOD'
write(99,'(I10)') meshh%numnods-meshh%nnode_ghost
do inode=1,meshh%numnods
    if (all(inode.ne.meshh%node_ghost(:))) then
      inoden=ntab(inode)
      ix=inode*3-2
      iy=inode*3-1
      iz=inode*3
      write(99,'(I10,3E15.6)') inoden,xx(ix),xx(iy),xx(iz)
    endif
end do
write(99,'(A)') '$ENDNOD'

! ELEMENTS
write(99,'(A)') '$ELM'
write(99,'(I10)') meshh%numele-meshh%nelem_ghost

ii=0
do ielem=1,meshh%numele
  if (meshh%nelem_ghost.gt.0) then
    if (all(ielem.ne.meshh%elem_ghost(:))) then
      ii=ii+1
      write(99,'(8I10)') ii,2,100,6,3,ntab(meshh%connect(ielem)%vertices(1)), &
                                      ntab(meshh%connect(ielem)%vertices(2)), &
                                      ntab(meshh%connect(ielem)%vertices(3))
    endif
  else
    ii=ii+1
    write(99,'(8I10)') ii,2,100,6,3,meshh%connect(ielem)%vertices
  endif
end do
write(99,'(A)') '$ENDELM'
write(99,'(I10)') meshh%nedge

!!$DEALLOCATE(ntab)

close(99)


END SUBROUTINE gmsh_out

!************************************************************************
!************************************************************************
!************************************************************************

SUBROUTINE gmsh_out_field(x0,mesh0,field,nam1,nam2)
USE data_mesh
implicit REAL(8) (a-h,o-z)
implicit INTEGER*4 (i-n)
CHARACTER(*), INTENT(IN) :: nam1, nam2
TYPE(mesh) :: mesh0
REAL(8) ::  x0(3*(mesh0%numnods)), field(mesh0%numele)
dimension :: ivert(3), xx1(3), xx2(3), xx3(3)


open(unit=88,file=nam1,status='unknown')

  write(88,'(A)') '$PostFormat'
  write(88,'(A)') '1.19 0 8'
  write(88,'(A)') '$EndPostFormat'
  write(88,*) 
  write(88,'(A)') '$View'
  write(88,'(2A)') nam2,', 1'
  write(88,*) 0,0,0
  write(88,*) 0,0,0
  write(88,*) mesh0%numele-mesh0%nelem_ghost, 0, 0
  write(88,*) 0,0,0
  write(88,*) 0


do ielem=1,mesh0%numele
  if (all(ielem.ne.mesh0%elem_ghost(:))) then

  ivert = mesh0%connect(ielem)%vertices
  xx1=x0(3*ivert(1)-2:3*ivert(1))
  xx2=x0(3*ivert(2)-2:3*ivert(2))
  xx3=x0(3*ivert(3)-2:3*ivert(3))
  write(88,'(3E15.6)') xx1(3), xx2(3), xx3(3)
  write(88,'(3E15.6)') xx1(2), xx2(2), xx3(2)
  write(88,'(3E15.6)') xx1(1), xx2(1), xx3(1)
  write(88,'(3E15.6)') field(ielem), field(ielem), field(ielem)
  endif
end do
write(88,'(A)') '$endView'

close(88)


END SUBROUTINE gmsh_out_field

