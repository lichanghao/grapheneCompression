SUBROUTINE ensight_wrap_fem(meshh,elem,fem_nborlist,nfem_nbor)
USE data_mesh
implicit REAL(8) (a-h,o-z)
implicit INTEGER*4 (i-n)
TYPE(mesh) :: meshh
INTEGER(4) :: elem(3,meshh%numele),fem_nborlist(meshh%numnods,0:6)
INTEGER(4) :: neigh(3),nfem_nbor,ntemp(3)
LOGICAL    :: exist(3)

nfem_nbor = 0
fem_nborlist = 0

do imesh = 1, meshh%numele
   elem(:,imesh) = meshh%connect(imesh)%vertices	
enddo

do imesh = 1, meshh%numele
	neigh(:) = meshh%connect(imesh)%vertices

	jmin = neigh(1)
	jmid = neigh(2)
	jmax = neigh(3)
	do i = 1, 3
	  if(neigh(i) .lt. jmin) jmin = neigh(i)
      if(neigh(i) .gt. jmax) jmax = neigh(i)
    enddo
	
	do i = 1,3
	  if(neigh(i) .ne. jmin .and. neigh(i) .ne.jmax) jmid = neigh(i)
	enddo

	exist = .false.

!  add neighbor list (jmin,jmid),(jmin,jmax),(jmid,jmax)

   do i = 1, 6
	  j = fem_nborlist(jmin,i)
	  k = fem_nborlist(jmid,i)
	  if(j.eq. jmid) exist(1) = .true.
	  if(j.eq. jmax) exist(2) = .true.
	  if(k.eq. jmax) exist(3) = .true.
   enddo

   if( exist(1) .eq. .false.) then
      fem_nborlist(jmin,0) = fem_nborlist(jmin,0) + 1
      fem_nborlist(jmin,fem_nborlist(jmin,0))= jmid
   endif
   
   if( exist(2) .eq. .false.) then
      fem_nborlist(jmin,0) = fem_nborlist(jmin,0) + 1
      fem_nborlist(jmin,fem_nborlist(jmin,0)) = jmax
   endif
   
   if( exist(3) .eq. .false.) then
      fem_nborlist(jmid,0) = fem_nborlist(jmid,0) + 1
      fem_nborlist(jmid,fem_nborlist(jmid,0)) = jmax
   endif
enddo

do i = 1, meshh%numnods
	  nfem_nbor = nfem_nbor + fem_nborlist(i,0)	  
enddo

END SUBROUTINE ensight_wrap_fem


