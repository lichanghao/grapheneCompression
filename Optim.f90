program OPTIM
USE data_mesh
USE data_BC
USE data_tensor22
USE data_mat
USE data_vdw
USE data_vector2
USE data_vector3
implicit REAL(8) (a-h,o-z)
implicit INTEGER*4 (i-n)
PARAMETER(MSAVE=5)
PARAMETER(nlayer=1)
include 'mpif.h' !MPI
TYPE(mesh) :: mesh0
TYPE(material) :: mat1
TYPE(vdw_data):: vdw1
TYPE(BC_data):: BCs
TYPE(tensor22), ALLOCATABLE :: F0(:), temp(:)
TYPE(vector2), ALLOCATABLE :: eta(:,:)
REAL(8), ALLOCATABLE :: x0(:),x00(:),J0(:), W_dens(:),  forces(:), f_loc(:)
REAL(8), ALLOCATABLE :: shapef(:,:,:), weight(:), f_ext(:)
REAL(8), ALLOCATABLE :: x0_BC(:),x0_short(:), f_short(:)
REAL(8), ALLOCATABLE :: W(:),Prec(:)
INTEGER(4), ALLOCATABLE :: elem(:,:),fem_nborlist(:,:)
INTEGER(4), ALLOCATABLE :: ntab(:)
REAL(8) :: crit(2), E_out(4), vmax(3), vmix(3)

INTEGER(4), ALLOCATABLE :: ntag(:)
INTEGER(4) :: ngroup(nlayer)
INTEGER(4) :: nlateral(nlayer),nelem(nlayer)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
mesh0%nstep=0
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
write(*,*)'enter the main stream'
call MPI_INIT(ierr) !MPI
call MPI_COMM_SIZE(MPI_COMM_WORLD,NPROCS,ierr)
call MPI_COMM_RANK(MPI_COMM_WORLD,ID,ierr)

write(*,*)'get  over the MPI'
IF(ID.EQ.0) THEN
  write(6,*) 'Numero de procesadores: ',nprocs
  write(6,*) 'Soy el procesador: ',ID
  
  !@@@@@@@@@@@ OPEN FILES @@@@@@@@@@@@@@@@@@@@@@@@
  open(unit=77,file='output.dat',status='unknown')   ! debugging file
  open(unit=78,file='energy.dat',status='unknown')   ! energy output file
  open(unit=90,file='nano_dims.dat',status='old')
  open(unit=91,file='nano_general.dat',status='old')
  open(unit=92,file='nano_BCs.dat',status='old')
  open(unit=94,file='nano_Mesh.dat',status='old')
  open(unit=95,file='nano_zero.dat',status='old')
  open(unit=96,file='nano_config.dat',status='old')
  open(unit=97,file='nano_final_config.dat',status='unknown')
  !@@@@@@@@@@@ END OPEN FILES @@@@@@@@@@@@@@@@@@@@@@@@


  !@@@@@@@@@@@ READ DIMENSIONS @@@@@@@@@@@@@@@@@@@@@@@@
  call read_dim(mesh0%numele,mesh0%numnods,mesh0%nedge,mesh0%nelem_ghost, &
       mesh0%nnode_ghost,ngauss,BCs%nnodBC,BCs%ndofBC,BCs%ndofOP, &
       vdw1%nvdw,vdw1%ngauss_vdw,vdw1%ng_tot,nneigh,ninrange,vdw1%indentation)
  !@@@@@@@@@@@ END READ DIMENSIONS @@@@@@@@@@@@@@@@@@@@@@@@
  if ((nneigh.lt.0).and.(vdw1%nvdw.eq.1)) then
    open(unit=56,file='nano_tub_loc.dat',status='old')
    read(56,*) ntubes
  endif
ENDIF
!!!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
   mesh0%nstep=0
!!!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
write(*,*)mesh0%nedge


write(*,*)'finishing reading dimensions'

! Broadcast the dimensions
call  MPI_BCAST(mesh0%numele,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
call  MPI_BCAST(mesh0%numnods,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
call  MPI_BCAST(mesh0%nedge,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
call  MPI_BCAST(mesh0%nelem_ghost,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
call  MPI_BCAST(mesh0%nnode_ghost,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
call  MPI_BCAST(ngauss,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
call  MPI_BCAST(BCs%nnodBC,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
call  MPI_BCAST(BCs%ndofBC,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
call  MPI_BCAST(BCs%ndofOP,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
call  MPI_BCAST(vdw1%nvdw,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)


if (vdw1%nvdw.eq.1) then
  call  MPI_BCAST(vdw1%ngauss_vdw,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
  call  MPI_BCAST(vdw1%ng_tot,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
  call  MPI_BCAST(nneigh,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
  call  MPI_BCAST(ninrange,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
  vdw1%ninrange=ninrange
  if (nneigh.lt.0) then
    call  MPI_BCAST(ntubes,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
    vdw1%ntubes=ntubes
  endif
endif

!@@@@@@@@@@@ MEMORY ALLOCATION @@@@@@@@@@@@@@@@@@@@@@@@
write(*,*) ' General memory allocation '
! This is for minimize routine
NWORK=BCs%ndofOP*(2*MSAVE +1)+2*MSAVE
ALLOCATE(W(NWORK),Prec(BCs%ndofOP),STAT=istat)
if (istat/=0) STOP '**** Not enough memory **** minimize'
ALLOCATE(x0(3*(mesh0%numnods+mesh0%nedge)),x00(3*(mesh0%numnods+mesh0%nedge)),&
    mesh0%connect(mesh0%numele), &
    ntag(mesh0%numnods),J0(mesh0%numele), & 
     !   mesh0%ntable(mesh0%numnods,maxneigh_vert+1), &
     mesh0%nghost_tab(mesh0%nedge,3), &
     F0(mesh0%numele),temp(mesh0%numele), &
     eta(ngauss,(mesh0%numele)),STAT=istat)
if (istat/=0) STOP '**** Not enough memory ****'
ALLOCATE(ntab(mesh0%numnods),STAT=istat)
if (istat/=0) STOP '**** Not enough memory **** gmsh_out'
ALLOCATE(W_dens(mesh0%numele), &
     forces(3*(mesh0%numnods+mesh0%nedge)), &
     f_loc(3*(mesh0%numnods+mesh0%nedge)),f_ext(3*mesh0%numnods), &
     shapef(ngauss,12,6),weight(ngauss),STAT=istat)
if (istat/=0) STOP '**** Not enough memory ****'
if (vdw1%nvdw.eq.1) then
   ALLOCATE(vdw1%shapef(vdw1%ngauss_vdw,12),vdw1%weight(vdw1%ngauss_vdw),STAT=istat)
   if (istat/=0) STOP '**** Not enough memory ****'
   vdw1%nneigh=nneigh
   if (nneigh.gt.0) then
     ALLOCATE(vdw1%near(vdw1%ng_tot,0:nneigh),STAT=istat)
     if (istat/=0) STOP '**** Not enough memory ****'
   else ! Straightforward method for near, for very large problems
     ALLOCATE(vdw1%near(1,0:1),STAT=istat)
     if (istat/=0) STOP '**** Not enough memory ****'
     ALLOCATE(vdw1%ntub_pos(0:ntubes),STAT=istat)
     if (istat/=0) STOP '**** Not enough memory ****'
   endif
   ALLOCATE(vdw1%x(vdw1%ng_tot,3), &
        vdw1%W(mesh0%numele), &
        vdw1%gbin(vdw1%ng_tot,3), &
        vdw1%neigh(vdw1%ng_tot,0:ninrange),STAT=istat)
   if (istat/=0) STOP '**** Not enough memory ****'
   vdw1%neval=0
!!$  ndata=(int(mesh0%numele/nprocs)+1)*vdw1%ngauss_vdw
!!$  ALLOCATE(vdw1%xpack( ndata, 3 ),STAT=istat)
!!$  if (istat/=0) STOP '**** Not enough memory ****'
endif

ALLOCATE(BCs%mdofBC(BCs%ndofBC),BCs%mdofOP(BCs%ndofOP), &
     x0_BC(BCs%ndofBC),x0_short(BCs%ndofOP), &
     BCs%mnodBC(BCs%nnodBC,2),f_short(BCs%ndofOP),STAT=istat)
if (istat/=0) STOP '**** Not enough memory ****'
if (mesh0%nnode_ghost.gt.0) then
   ALLOCATE(mesh0%elem_ghost(mesh0%nelem_ghost),mesh0%node_ghost(mesh0%nnode_ghost),STAT=istat)
   if (istat/=0) STOP '**** Not enough memory ****'
else
   ALLOCATE(mesh0%elem_ghost(1),mesh0%node_ghost(1),STAT=istat)
   if (istat/=0) STOP '**** Not enough memory ****'
endif
write(*,*) ' End of general memory allocation '
!@@@@@@@@@@@ END MEMORY ALLOCATION @@@@@@@@@@@@@@@@@@@@@@@@


IF(ID.EQ.0) THEN
   write(*,*) ' Read data '
   !@@@@@@@@@@@ READ PROBLEM DATA @@@@@@@@@@@@@@@@@@@@@@@@
   call read_general(ylength,mat1,nW_hat,crit,imperfect,fact_imp)
   call read_mesh(mesh0)
   call read_zero(mesh0%numele,F0,J0)
   call read_BCs(BCs)
   call read_vdw(vdw1,nneigh)
   call read_config(mesh0%numnods,mesh0%numele,ngauss,x0,eta)
   !@@@@@@@@@@@ END READ PROBLEM DATA @@@@@@@@@@@@@@@@@@@@@@@@
ENDIF

! General
write(*,*) ' Bcast data '
call  MPI_BCAST(ylength,1,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
call  MPI_BCAST(nW_hat,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
call  MPI_BCAST(crit(1),2,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
call  MPI_BCAST(imperfect,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
call  MPI_BCAST(fact_imp,1,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
call  MPI_BCAST(mat1%A0,1,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
call  MPI_BCAST(mat1%A1,1,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
call  MPI_BCAST(mat1%Vs,3,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
call  MPI_BCAST(mat1%Va,3,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
call  MPI_BCAST(mat1%E(:,1),3,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
call  MPI_BCAST(mat1%E(:,2),3,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
call  MPI_BCAST(mat1%s0,1,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
call  MPI_BCAST(mat1%nCode_Pot,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
! Mesh
do ielem=1,mesh0%numele
  call  MPI_BCAST(mesh0%connect(ielem)%vertices(1),3, &
                 MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
  call  MPI_BCAST(mesh0%connect(ielem)%num_neigh_elem,1, &
                 MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
  call  MPI_BCAST(mesh0%connect(ielem)%num_neigh_vert,1, &
                 MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
  call  MPI_BCAST(mesh0%connect(ielem)%neigh_elem(1),12, &
                 MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
  call  MPI_BCAST(mesh0%connect(ielem)%neigh_vert(1),12, &
                 MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
  call  MPI_BCAST(mesh0%connect(ielem)%code_bc(1),3, &
                 MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
enddo
call  MPI_BCAST(mesh0%nghost_tab(1,1),mesh0%nedge,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
call  MPI_BCAST(mesh0%nghost_tab(1,2),mesh0%nedge,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
call  MPI_BCAST(mesh0%nghost_tab(1,3),mesh0%nedge,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
call  MPI_BCAST(mesh0%elem_ghost(1),mesh0%nelem_ghost,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
call  MPI_BCAST(mesh0%node_ghost(1),mesh0%nnode_ghost,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
! Zero
call  MPI_BCAST(J0(1),mesh0%numele,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
do ielem=1,mesh0%numele
  call  MPI_BCAST(F0(ielem)%val(1,1),4,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
enddo
!BCs
call  MPI_BCAST(BCs%nloadstep,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
call  MPI_BCAST(BCs%nCodeLoad,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
call  MPI_BCAST(BCs%mdofBC(1),BCs%ndofBC,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
call  MPI_BCAST(BCs%mdofOP(1),BCs%ndofOP,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
call  MPI_BCAST(BCs%mnodBC(1,1),2*BCs%nnodBC,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
call  MPI_BCAST(BCs%rotation,9,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
call  MPI_BCAST(BCs%xc,3,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
call  MPI_BCAST(BCs%value,1,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
! vdw
if (vdw1%nvdw.eq.1) then
  close(56)
  call  MPI_BCAST(vdw1%neval,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
  call  MPI_BCAST(vdw1%meval,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
  call  MPI_BCAST(vdw1%r_cut,7,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
  call  MPI_BCAST(vdw1%shapef(1,1),12*vdw1%ngauss_vdw,MPI_DOUBLE_PRECISION, &
                  0,MPI_COMM_WORLD,ierr)
  call  MPI_BCAST(vdw1%weight(1),vdw1%ngauss_vdw,MPI_DOUBLE_PRECISION, &
                  0,MPI_COMM_WORLD,ierr)
  if (nneigh.gt.0) then
    call  MPI_BCAST(vdw1%near(1,0),vdw1%ng_tot*(nneigh+1),MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
  else
    call  MPI_BCAST(vdw1%ntub_pos(0),vdw1%ntubes+1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
  endif
endif

if(vdw1%indentation.eq.1) then
    call  MPI_BCAST(vdw1%mdent_eval,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierr)
    call  MPI_BCAST(vdw1%denter_a,1,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
    call  MPI_BCAST(vdw1%denter_r,1,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
    call  MPI_BCAST(vdw1%denter_x,3,MPI_DOUBLE_PRECISION,0,MPI_COMM_WORLD,ierr)
endif

! config
call  MPI_BCAST(x0(1),3*mesh0%numnods,MPI_DOUBLE_PRECISION, &
                0,MPI_COMM_WORLD,ierr)
call  MPI_BCAST(eta(1,1)%val(1),mesh0%numele*ngauss*2,MPI_DOUBLE_PRECISION, &
                0,MPI_COMM_WORLD,ierr)
write(*,*) ' end Bcast data '



!@@@@@@@@@@@ XTRA VDW ALLOCATIONS @@@@@@@@@@@@@@@@@@@@@@@@
if (vdw1%nvdw.eq.1) then
   write(*,*) ' XTRA VDW ALLOCATION '
   ! We allocate variables of binning. If allocation is insufficient, then it is
   ! enlarged on the fly
   vmax(1)=maxval(x0(1:3*mesh0%numnods:3))
   vmix(1)=minval(x0(1:3*mesh0%numnods:3))
   vmax(2)=maxval(x0(2:3*mesh0%numnods:3))
   vmix(2)=minval(x0(2:3*mesh0%numnods:3))
   vmax(3)=maxval(x0(3:3*mesh0%numnods:3))
   vmix(3)=minval(x0(3:3*mesh0%numnods:3))
   rc=vdw1%r_cut
   fact=1.05d0
   vdw1%nx(:)=max(int((vmax(:)-vmix(:))/(fact*rc)),1)
   vdw1%nxdim(:)=(vdw1%nx(:)+1)
   ALLOCATE(vdw1%binx(3,0:maxval(vdw1%nxdim(:))),STAT=istat)
   if (istat/=0) STOP '**** Not enough memory ****'
   ng=mesh0%numele*vdw1%ngauss_vdw
   ng_per_bin=int(sqrt(2.)*rc*rc*vdw1%ngauss_vdw/(J0(1)/2.)*1.3 *3) * 1.3
   ALLOCATE(vdw1%bin(vdw1%nxdim(1),vdw1%nxdim(2),vdw1%nxdim(3), &
            0:ng_per_bin),STAT=istat)
!write(*,*) ' DIMENSIONING. near: ', nneigh,' neigh: ', ninrange,' bin: ',  ng_per_bin
   write(*,*) ' End XTRA VDW ALLOCATION '

endif
!@@@@@@@@@@@ END XTRA VDW ALLOCATIONS @@@@@@@@@@@@@@@@@@@@@@@@

!@@@@@@@@@@@ DEAL WITH ENSIGHT @@@@@@@@@@@@@@@@@@@@@@@@
x00(:) = x0(:)
ALLOCATE(elem(3,mesh0%numele),fem_nborlist(mesh0%numnods,0:6),STAT=istat)
if(ID.eq.0)then
   call ensight_wrap_fem(mesh0,elem,fem_nborlist,nfem_nbor)
   call partition_group(mesh0,x00,elem,fem_nborlist,nlayer,ngroup,nlateral,nelem,ntag)
   call ensight_out(0.d0,x0,mesh0,elem,nfem_nbor,fem_nborlist,&
        nlayer,ngroup,nlateral,nelem,ntag)
   DEALLOCATE(x00)
   !@@@@@@@@@@@ END ENSIGHT @@@@@@@@@@@@@@@@@@@@@@@@
endif


!@@@@@@@@@@@ INITIALIZATIONS @@@@@@@@@@@@@@@@@@@@@@@@
! Set external force to zero
f_ext=0.d0
! Compute shape functions and its derivatives at Gauss points
call gauss(ngauss,shapef,weight)
! Initialize BC's
x0_BC=x0(BCs%mdofBC(:))
!@@@@@@@@@@@ END INITIALIZATIONS @@@@@@@@@@@@@@@@@@@@@@@@

!@@@@@@@@@@@ INITIAL ENERGY @@@@@@@@@@@@@@@@@@@@@@@@

!!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
mesh0%x_indent=BCs%xc(1)
mesh0%y_indent=BCs%xc(2)
mesh0%z_indent=BCs%xc(3)+442.0
 
!write(*,*)mesh0%x_indent,mesh0%y_indent,mesh0%z_indent

!!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

write(*,*) ' Compute initial energy '
!write(*,*)'222222222222222222222222222222222222222222222222222222222222222222'
call energy(mesh0,x0,eta,mat1,shapef,weight,ngauss, &
        F0,J0,W_dens,E_tot_ini,forces,f_loc,f_ext,nW_hat,crit(2), &
        E_out,vdw1)

!call ensight_summary
!stop
!write(*,*)  '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
IF(ID.EQ.0) THEN
   write(*,*) ' INITIAL ENERGY: ', E_out(:)
   write(77,*) ' Initial energy    :',  E_out(1)
   write(78,'(e14.5,4e16.8)') 0., E_out(1), E_out(2), E_out(3), E_out(4) !this goes to energy file
   close(78)
   !Output of the Initial mesh
   call gmsh_out(x0,mesh0,'meshini.msh',ntab)
   if (nprocs.eq.1) call gmsh_out_field(x0,mesh0, W_dens,'Wdenini.msh','Wdenini')
   !@@@@@@@@@@@ END INITIAL ENERGY @@@@@@@@@@@@@@@@@@@@@@@@
   write(*,*) '**** INCREMENTAL PROCESS ****'
ENDIF

!add ensight
!ALLOCATE(elem(3,mesh0%numele),fem_nborlist(mesh0%numnods,0:6),STAT=istat)
!call ensight_wrap_fem(mesh0,elem,fem_nborlist,nfem_nbor)
!call ensight_out(0.d0,x0,mesh0,elem,nfem_nbor,fem_nborlist,BCs)

if (BCs%nCodeLoad.le.-69) then
   !@@@@@@@@@@@ DYNAMICAL RELAXATION OF THE NANOTUBE @@@@@@@@@@@@@@@@@@@@@@@
   CALL dyna_relax(mesh0,mat1,x0,x0_BC,eta, &
                shapef,weight,ngauss,F0,J0,forces, f_loc, f_ext,  &
		W_dens,crit, &
		nW_hat,imperfect,fact_imp, &
		ylength,vdw1,ID,nprocs,E_out,BCs,ntab)
   !@@@@@@@@@@@ END DYNAMICAL RELAXATION OF THE NANOTUBE @@@@@@@@@@@@@@@@@@@@@@@
else
   !@@@@@@@@@@@ INCREMENTAL LOADING OF THE NANOTUBE @@@@@@@@@@@@@@@@@@@@@@@
   CALL pasapas(mesh0,mat1,x0,eta,BCs,x0_short,x0_BC, &
                shapef,weight,ngauss,F0,J0,forces,f_loc,f_ext,  &
		f_short,W_dens,crit, &
		nW_hat,imperfect,fact_imp, &
		ylength,vdw1,ID,nprocs,E_out,ntab,W,NWORK,Prec,&
                elem,nfem_nbor,fem_nborlist,nlayer,ngroup,nlateral,nelem,ntag)
   !@@@@@@@@@@@ END INCREMENTAL LOADING OF THE NANOTUBE @@@@@@@@@@@@@@@@@@@@@@@
endif

call ensight_summary

IF(ID.EQ.0) THEN
   !@@@@@@@@@@@ OUTPUT FINAL CONFIGURATION @@@@@@@@@@@@@@@@@@@@@@@
   write(*,*) ' FINAL ENERGY: ',E_out(:)
   call write_config(mesh0%numnods,mesh0%numele,ngauss,x0,eta)
   close(77)
!   close(78)
   close(90)
   close(91)
   close(92)
   close(94)
   close(95)
   close(96)
   close(97)
ENDIF



!@@@@@@@@@@@ FINALIZE SMOOTHLY @@@@@@@@@@@@@@@@@@@@@@@
DEALLOCATE(W,Prec)
DEALLOCATE(BCs%mdofBC,BCs%mdofOP,BCs%mnodBC,x0_BC,x0_short,f_short)
DEALLOCATE(x0,mesh0%connect, &
     !           mesh0%ntable, &
mesh0%nghost_tab,F0,J0,eta,W_dens,  forces,f_loc,f_ext,shapef)
DEALLOCATE(ntab)
if (vdw1%nvdw.eq.1) then
   DEALLOCATE(vdw1%shapef,vdw1%weight,vdw1%near)
   DEALLOCATE(vdw1%x,vdw1%W,vdw1%bin,vdw1%gbin,vdw1%neigh)
endif
DEALLOCATE(mesh0%elem_ghost,mesh0%node_ghost)
!@@@@@@@@@@@ END FINALIZE SMOOTHLY @@@@@@@@@@@@@@@@@@@@@@@
call MPI_FINALIZE() !MPI

end program OPTIM

