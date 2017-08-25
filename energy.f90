SUBROUTINE energy(mesh0,x0,eta,mat1,shapef,weight,ngauss,F0,J0, &
                  W_dens,E_tot,forces,f_loc,f_ext,nW_hat,crit, &
                  E_out,vdw1)
! E_out 1 the physical total energy, once the ghost elements are removed
! E_out 2 the internal energy (without ghost elements)
! E_out 3 the vd Waals energy (without ghost elements)
! E_out 4 the external energy
USE data_mesh
USE data_tensor22
USE data_vector2
USE data_vector3
USE data_mat
USE data_vdw
implicit REAL(8) (a-h,o-z)
implicit INTEGER*4 (i-n)
include 'mpif.h' !MPI
TYPE(mesh) :: mesh0
TYPE(material) :: mat1
TYPE(vdw_data):: vdw1
TYPE(tensor22) :: F0(mesh0%numele)
TYPE(vector2) :: eta(ngauss,(mesh0%numele))
REAL(8) :: x0(3*(mesh0%numnods+mesh0%nedge)), forces(3*(mesh0%numnods+mesh0%nedge))
REAL(8) :: W_dens(mesh0%numele), J0(mesh0%numele), f_ext(3*(mesh0%numnods))
REAL(8) :: f_loc(3*(mesh0%numnods+mesh0%nedge))
REAL(8) shapef(ngauss,12,6), weight(ngauss), f_elem(12,3)
DIMENSION :: xneigh(12,3), neigh(12), E_out(4), E_aux(4), E_aux_tot(4)


forces=0.d0
f_loc=0.d0
W_dens=0.d0
vdw1%W=0.d0

call MPI_COMM_RANK( MPI_COMM_WORLD, id, ierr )
call MPI_COMM_SIZE( MPI_COMM_WORLD, nprocs, ierr )

call ghost_nodes(mesh0,x0)


inner_fail=0

if(vdw1%nvdw.eq.1) then
  !write(*,*) ' We locate gauss points!!!'
  call xlocate_gauss(vdw1,mesh0,x0) ! locate the gauss points
  !write(*,*) ' end locate gauss points!!!'
  if (int(vdw1%neval/vdw1%meval)*vdw1%meval.eq.vdw1%neval) then
     !write(*,*) ' WE UPDATE NEIGHBORS!!!'
!!!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!     call find_in_range(vdw1,mesh0,x0,J0(1)) 
!!!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
     !write(*,*) ' End WE UPDATE NEIGHBORS!!!'
  endif
  vdw1%neval=vdw1%neval+1
endif
!write(*,*) ' ACTUALITY!!!. near: ', maxval(vdw1%near(:,0)), &
!                        ' neigh: ', maxval(vdw1%neigh(:,0)), &
!                          ' bin: ', maxval(vdw1%bin(:,:,:,0))

do ielem=1+id,mesh0%numele,nprocs
    neigh=mesh0%connect(ielem)%neigh_vert
    xneigh(:,1)=x0(3*neigh(:)-2)
    xneigh(:,2)=x0(3*neigh(:)-1)
    xneigh(:,3)=x0(3*neigh(:))

    call  ener_elem(inner_fail,xneigh,ngauss,shapef,F0(ielem)%val, &
                     ielem,(mesh0%numele),nW_hat, &
                     mat1,crit,weight,f_elem,W_elem,eta)
!     write(*,*)"meshhhhhhhhhh",mesh0%x_indent,mesh0%y_indent,mesh0%z_indent
    if(vdw1%nvdw.eq.1) then
       call indent_elem(mat1%s0,mesh0,ielem,neigh,vdw1,J0,W_vdw,f_loc)
       vdw1%W(ielem)=W_vdw
    endif

!    write(*,*)"energy",f_elem
    
    W_dens(ielem)=W_elem
    f_elem=f_elem*J0(ielem)/2.d0
    do i=1,12
      f_loc(3*neigh(i)-2:3*neigh(i))=f_loc(3*neigh(i)-2:3*neigh(i))+f_elem(i,:)
    enddo
end do


!!$do ielem = 1,mesh0%numele
!!$write(*,*)'ielem =',ielem,' W_elem=',W_dens(ielem),"J0=",J0(ielem)
!!$enddo

!write(*,*)'55555555555555555555555555555555555555555555555555555'
!write(*,*) ' hola # 1'
call MPI_REDUCE(f_loc(1),forces(1),3*(mesh0%numnods+mesh0%nedge),MPI_DOUBLE_PRECISION, &
                MPI_SUM,0,MPI_COMM_WORLD,ierr)
!write(*,*) ' hola # 2'
if (id.eq.0) call force_ghost(forces,mesh0)
call MPI_BCAST(forces(1),3*mesh0%numnods,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
!write(*,*) ' hola # 3'
!write(*,*)'55555555555555555555555555555555555555555555555555555'
E_tot=0.d0
E_int=0.d0
E_ext=0.d0
E_vdw=0.d0
! Energy of all elements
if(vdw1%nvdw.eq.1) then
  do i=1+id,mesh0%numele,nprocs
    E_int=E_int+W_dens(i)*J0(i)
    E_vdw=E_vdw+vdw1%W(i)
  enddo
else
  do i=1+id,mesh0%numele,nprocs
    E_int=E_int+W_dens(i)*J0(i)
  enddo
endif
E_int=E_int/2.d0
!write(*,*) ' hola # 4'

! The external energy is computed serially
if (maxval(abs(f_ext(:))).gt.1.d-7) then
 forces=forces-f_ext
 do i=1,3*mesh0%numnods
   E_ext=E_ext+x0(i)*f_ext(i)
 enddo
endif
!write(*,*) ' hola # 5'

! Postpro energies (performed serially)
E_int_r=0.d0
E_vdw_r=0.d0
!!$E_int_r_tot=0.d0
!!$E_vdw_r_tot=0.d0
if (mesh0%nelem_ghost.gt.0) then
  if(vdw1%nvdw.eq.1) then
      do i=1,mesh0%nelem_ghost
           j=mesh0%elem_ghost(i)
             E_int_r=E_int_r+W_dens(j)*J0(j)
             E_vdw_r=E_vdw_r+vdw1%W(j)
      enddo
   else
      do i=1,mesh0%nelem_ghost
           j=mesh0%elem_ghost(i)
             E_int_r=E_int_r+W_dens(j)*J0(j)
      enddo
   endif
  E_int_r=E_int_r/2.
endif
!write(*,*) ' hola # 6'

E_aux(1)=E_int
E_aux(2)=E_vdw
E_aux(3)=E_int_r
E_aux(4)=E_vdw_r
E_aux_tot=0.d0
call MPI_ALLREDUCE(E_aux,E_aux_tot,4,MPI_DOUBLE_PRECISION,MPI_SUM,MPI_COMM_WORLD,ierr)
E_int_tot=E_aux_tot(1)
E_vdw_tot=E_aux_tot(2)
E_int_r_tot=E_aux_tot(3)
E_vdw_r_tot=E_aux_tot(4)

E_tot=E_int_tot+E_vdw_tot-E_ext                            ! total

E_out(1)=E_int_tot-E_int_r_tot-E_ext+E_vdw_tot-E_vdw_r_tot ! total reduced
E_out(2)=E_int_tot-E_int_r_tot                             ! internal reduced
E_out(3)=E_vdw_tot-E_vdw_r_tot                             ! van der waals reduced
E_out(4)=E_ext                                             ! external
!write(*,*) ' hola # 7'

! Give warning
if (inner_fail.gt.0) then
  continue
  write(*,*) 'Constitutive Inner relaxation failed: ', inner_fail, '  times.'
end if
!write(*,*) ' hola # 8'


END SUBROUTINE energy


!***************************************************************************
!***************************************************************************
!***************************************************************************
! This routine computes the strain energy density
SUBROUTINE Hyper_Pot(mat1,pe,W,dW)
USE data_mat
implicit REAL(8) (a-h,o-z)
implicit INTEGER*4 (i-n)
TYPE(material) mat1
DIMENSION :: pe(6), Vs(2), Va(2), dW(6)

if (mat1%nCode_Pot.eq.1) then
  call Morse(mat1,pe,W,dW)
else if (mat1%nCode_Pot.eq.2) then
  call Brenner(mat1,pe,W,dW)
else if (mat1%nCode_Pot.eq.22) then
  call Brenner2(mat1,pe,W,dW)
else if (mat1%nCode_Pot.eq.3) then
  call MM3(mat1,pe,W,dW)
else
  STOP 'Atomistic description not implemented'
endif
END SUBROUTINE Hyper_Pot


!***************************************************************************
! Here are the stresses
SUBROUTINE Stresses(dW,dpedC,dpedk,S_n,S_m)
USE data_vector3
implicit REAL(8) (a-h,o-z)
implicit INTEGER*4 (i-n)
TYPE(vector3) :: dpedC(6), dpedk(6)
DIMENSION :: dW(6), S_n(3), S_m(3)

S_n=0.
S_m=0.
do i=1,6
  S_n=S_n+dW(i)*dpedC(i)%val
  S_m=S_m+dW(i)*dpedk(i)%val
end do

END SUBROUTINE Stresses


