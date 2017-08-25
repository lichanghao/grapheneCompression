subroutine ensight_out(time,x0,meshh,elem,&
          nfem_nbor,fem_nborlist,nlayer,ngroup,nlateral,nelem,ntag)

USE data_mesh
implicit REAL(8) (a-h,o-z)
implicit INTEGER*4 (i-n)
TYPE(mesh) :: meshh
INTEGER(4) :: elem(3,meshh%numele),fem_nborlist(meshh%numnods,0:6)
INTEGER(4) :: neigh(3),nfem_nbor,ntemp(3)
real(8) :: time,x0(3*(meshh%numnods+meshh%nedge))
INTEGER(4) :: ngroup(nlayer),ntag(meshh%numnods)
INTEGER(4) :: nlateral(nlayer),nelem(nlayer)

!  Common variables
logical:: ensinit
integer:: enscount,ENSMAX
real(8)::  enstlist
parameter (ENSMAX=500)
common/ensight/ ensinit,enscount,enstlist(ENSMAX)

!  Local variables
integer(4):: i,i3,icount,jcount,kcount,j,ij
character*30 filename

!  Initialize, if necessary
numnods = meshh%numnods
numele  = meshh%numele

if (.not.ensinit) then
   ensinit = .true.
   enscount = 0
   do i = 1,ENSMAX
      enstlist(i) = 0.0
   end do
end if

!  Open file for output
filename="ens_movie.geo000"
write(filename(14:16),'(i3.3)') enscount
OPEN(30,FILE=filename)
WRITE(30,*) "Particle geometry file"
WRITE(30,*) "for atomistic data "
WRITE(30,*) "node id given"
WRITE(30,*) "element id given"
WRITE(30,*) "coordinates"
WRITE(30,'(i8)') numnods
      
! write out FEM node positions
! FEM nodes: icount --- jcount
jcount = 1
do i = 1, numnods
   i3=3*i-3
   WRITE(30,1000)jcount, x0(i3+1),x0(i3+2),x0(i3+3)
   jcount = jcount + 1
enddo

1000 format(i8,3e12.5)

! part for FEM nodes

do ig = 1, nlayer ! number of shells

   WRITE(30,501) ig
   WRITE(30,502) ig
   WRITE(30,*) "point"
 
   WRITE(30,'(i8)') ngroup(ig)


   do i = 1, numnods
      if(ntag(i) .eq. ig)then
         WRITE(30,'(2i8)') i,i
      endif
   end do
   
enddo


! sum finite elements
do ilateral = 1, nlayer
   WRITE(30,501) nlayer + ilateral
   WRITE(30,*)'Structural Model'
   WRITE(30,*)'bar2'
   WRITE(30,'(i8)') nlateral(ilateral)
   
   icount = 0
   DO i = 1,numnods
      if(ntag(i).eq.ilateral)then
         nnbors = fem_nborlist(i,0)
         DO ij = 1,nnbors
            j = fem_nborlist(i,ij)
            icount = icount+1
            WRITE(30,'(3i8)')icount, i, j
         ENDDO
      endif
   ENDDO
ENDDO


do ielem = 1,nlayer

   WRITE(30,501) 2*nlayer + ielem
   WRITE(30,*)'Structural Model'
   WRITE(30,*)'tria3'
   WRITE(30,'(i8)') nelem(ielem)
   icount = 0
   do i = 1, numele
      if(ntag(elem(1,i)).eq.ielem .and.ntag(elem(2,i)).eq.ielem .and. ntag(elem(3,i)).eq.ielem )then
         icount = icount + 1
         WRITE(30,'(4i8)') icount, elem(1,i),elem(2,i),elem(3,i)
      endif
   enddo

enddo

501 format("part",i4)
502 format("group",i1)
       

!  Update
enscount = enscount+1
enstlist(enscount) = time
CLOSE(30)

end subroutine ensight_out


subroutine ensight_summary
  implicit none
  
  !  Common variables
  logical ensinit
  integer enscount,ENSMAX
  real*8  enstlist
  parameter (ENSMAX=100)
  common/ensight/ ensinit,enscount,enstlist(ENSMAX)

  !  Local variables
  character*30 filename
  integer i,outcount
  
! Open file for output
  filename="ens_movie.case"
  OPEN(30,FILE=filename)
  WRITE(30,*) "FORMAT"
  WRITE(30,*) "type: ensight"
  WRITE(30,*)
  WRITE(30,*) "GEOMETRY"
  WRITE(30,*) "model:   1   ens_movie.geo***"
  WRITE(30,*) 
  WRITE(30,*) "TIME"
  WRITE(30,*) "time set:               1"
  WRITE(30,*) "number of steps:       ",enscount
  WRITE(30,*) "filename start number:  0"
  WRITE(30,*) "filename increment:     1"
  WRITE(30,*) "time values: "
  outcount = 0
  do while (outcount.lt.enscount)
     WRITE(30,'(4e12.5)') &       
          (enstlist(i),i=outcount+1,min(outcount+4,enscount))
     outcount = outcount+4
  end do
  
  CLOSE(30)

end subroutine ensight_summary



subroutine partition_group(meshh,x00,elem,fem_nborlist,nlayer,ngroup,nlateral,nelem,ntag)

USE data_mesh
implicit REAL(8) (a-h,o-z)
implicit INTEGER*4 (i-n)
TYPE(mesh) :: meshh

INTEGER(4) :: elem(3,meshh%numele),fem_nborlist(meshh%numnods,0:6)
INTEGER(4) :: neigh(3),nfem_nbor,ntemp(3)
REAL(8) :: x00(3*(meshh%numnods+meshh%nedge))
PARAMETER (r0 = 0.34)

INTEGER(4) :: ngroup(nlayer),ntag(meshh%numnods)
INTEGER(4) :: nlateral(nlayer),nelem(nlayer)

write(*,*) 'we are in partition'
do ig = 1, nlayer
   ngroup(ig) = 0
   nlateral(ig) = 0
   nelem(ig) = 0
enddo


do i = 1, meshh%numnods
   ri = sqrt(x00(3*i-1)*x00(3*i-1)+x00(3*i)*x00(3*i))+0.1d0
!   kk = int(ri/r0)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!only for 1 layer graphene
    kk=1
  ! if(kk.gt.10)stop 'increase the size'
   ngroup(kk) = ngroup(kk) + 1
   ntag(i) = kk
   
   nnbors = fem_nborlist(i,0)

   DO ij = 1,nnbors
      j = fem_nborlist(i,ij)
      nlateral(kk) = nlateral(kk) + 1
   ENDDO
ENDDO

write(*,*)'check'
do i = 1, meshh%numele
   nd = elem(1,i)
   kk = ntag(nd)
   nelem(kk) = nelem(kk) + 1
enddo


END subroutine partition_group
