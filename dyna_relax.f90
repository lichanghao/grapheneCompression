SUBROUTINE dyna_relax(mesh0,mat1,x0,x0_BC,eta, &
                shapef,weight,ngauss,F0,J0,forces, f_loc, f_ext,  &
				W_dens,crit, &
				nW_hat,imperfect,fact_imp, &
				ylength,vdw1,ID,nprocs,E_out,BCs,ntab)
USE data_mesh
USE data_tensor22
USE data_vector2
USE data_mat
USE data_vdw
USE data_BC
implicit REAL(8) (a-h,o-z)
implicit INTEGER*4 (i-n)
TYPE(mesh) :: mesh0
TYPE(material) :: mat1
TYPE(vdw_data):: vdw1
TYPE(BC_data):: BCs
TYPE(tensor22) :: F0(mesh0%numele)
TYPE(vector2) :: eta(ngauss,(mesh0%numele))
REAL(8) :: x0(3*(mesh0%numnods+mesh0%nedge)), forces(3*(mesh0%numnods+mesh0%nedge))
REAL(8) :: W_dens(mesh0%numele), J0(mesh0%numele), f_ext(3*(mesh0%numnods))
REAL(8) :: f_loc(3*(mesh0%numnods+mesh0%nedge))
INTEGER(4) :: ntab(mesh0%numnods)
REAL(8) :: shapef(ngauss,12,6), weight(ngauss)
REAL(8), ALLOCATABLE :: mass(:), acc(:), vel(:)
DIMENSION x0_BC(BCs%ndofBC)
REAL(8) crit(2), E_out(4)
CHARACTER(80) name1, name2, name3
PARAMETER (pi0=3.141592653589793238d0)


ntsteps=BCs%nloadstep
nout=max(int(ntsteps/500),1)

! Define mass
ALLOCATE(mass(mesh0%numnods),acc(3*mesh0%numnods),vel(3*mesh0%numnods),STAT=istat)
if (istat/=0) STOP '**** Not enough memory ****'
call make_mass(mass,mesh0,mat1,J0,dt)
alpha=4.d0*pi0/10.d0       *0.05d0 ! the separated number is factor w.r.t. critical

write(*,*) ' TIME STEP ',dt,nout,maxval(mass(:))

! Initialize
vdw1%neval=0
if (BCs%nCodeLoad.eq.-70) then
   call get_force(BCs,x0_BC,f_ext,mesh0%numnods,0)
endif
call energy(mesh0,x0,eta,mat1,shapef,weight,ngauss, &
        F0,J0,W_dens,E_tot_ini,forces,f_loc,f_ext,nW_hat,crit(2), &
        E_out,vdw1)

! f_loc will play the role of the current (as opposed to -1) velocity, forces of curr accel
f_loc=0.d0
do i=1,mesh0%numnods
  forces(3*i-2)=-forces(3*i-2)/mass(i) ! compute initial acceleration
  forces(3*i-1)=-forces(3*i-1)/mass(i) ! compute initial acceleration
  forces(3*i)  =-forces(3*i)  /mass(i) ! compute initial acceleration
enddo
vel(1:3*mesh0%numnods)=f_loc(1:3*mesh0%numnods)
acc(1:3*mesh0%numnods)=forces(1:3*mesh0%numnods)
vel(BCs%mdofBC(:))=0.d0
acc(BCs%mdofBC(:))=0.d0

!#############################################################################
do istep=1,ntsteps
   if (istep.gt.20000) alpha=0.d0
   ! update the force
   if (BCs%nCodeLoad.eq.-70) then
     call get_force(BCs,x0_BC,f_ext,mesh0%numnods,istep)
   endif
   ! time integration
   x0(1:3*mesh0%numnods)= x0(1:3*mesh0%numnods) + dt*vel(1:3*mesh0%numnods) &
                                                + dt*dt/2.d0*acc(1:3*mesh0%numnods)
   ! Boundary conditions in an explicit scheme
   x0(BCs%mdofBC(:))=x0_BC
   call energy(mesh0,x0,eta,mat1,shapef,weight,ngauss, &
           F0,J0,W_dens,E_tot_ini,forces,f_loc,f_ext,nW_hat,crit(2), &
           E_out,vdw1)
   ! compute acceleration with damping
   do i=1,mesh0%numnods
     forces(3*i-2)=-forces(3*i-2)/mass(i) - alpha*(acc(3*i-2)*dt/2.d0 + vel(3*i-2))  
     forces(3*i-1)=-forces(3*i-1)/mass(i) - alpha*(acc(3*i-1)*dt/2.d0 + vel(3*i-1))  
     forces(3*i)  =-forces(3*i)  /mass(i) - alpha*(acc(3*i)  *dt/2.d0 + vel(3*i)  )  
   enddo
   forces(BCs%mdofBC(:))=0.d0
   ! compute velocity
   f_loc(1:3*mesh0%numnods)= vel(1:3*mesh0%numnods) + &
                              dt/2.d0*(acc(1:3*mesh0%numnods)+forces(1:3*mesh0%numnods))
   f_loc(BCs%mdofBC(:))=0.d0
   ! Update
   vel(1:3*mesh0%numnods)=f_loc(1:3*mesh0%numnods)
   acc(1:3*mesh0%numnods)=forces(1:3*mesh0%numnods)

   ! Compute the Kinetic energy:
   E_Kinetic=0.d0   
   do ikk=1,mesh0%numnods
     E_Kinetic=E_Kinetic+mass(ikk)*(vel(3*ikk-2)*vel(3*ikk-2) + &
        vel(3*ikk-1)*vel(3*ikk-1) + vel(3*ikk  )*vel(3*ikk  ) )
   enddo
   E_Kinetic=.5d0*E_Kinetic

  ! OUTPUT
  IF ((ID.EQ.0)) then
  write(78,'(5e16.8)') istep*dt, &
                 E_out(1), E_out(2), E_Kinetic, x0(53*3-1)  !this goes to energy file
  IF ((mod(istep,nout).eq.0)) THEN
    write(*,*)   ' TIME Step         :', istep, '  Max Acc: ', maxval(acc(:))
    write(77,*)  ' TIME Step         :', istep, '  Max Acc: ', maxval(acc(:))
    write(*,*)   ' Current energy:', E_out(1)
    write(77,*)  ' Current energy:', E_out(1)
!    write(78,'(5e15.6)') istep*dt, &
!                   E_out(1), E_out(2), E_out(2), E_out(4) !this goes to energy file

    name1=''
    name2=''
    if (istep/nout<10) then
      write(name1,'(3i1)') 0,0,istep/nout
    else if (istep/nout<100) then
      write(name1,'(i1,i2)') 0,istep/nout
    else if (istep/nout<1000) then
      write(name1,'(i3)') istep/nout
    else
      STOP ' *** A problem with file output names'
    end if
    read(name1,'(A3)') name2

    name3=''
    name3='mesh'//name2(1:3)//'.msh'
    l1=len_trim(name3)
    write(*,*) name3(1:l1)
    call gmsh_out(x0,mesh0,name3(1:l1),ntab)
  
    name3=''
    name3='Wden'//name2(1:3)//'.msh'
    l1=len_trim(name3)
    write(*,*) name3(1:l1)
    name1=''
    name1='Wden'//name2(1:3)
    l2=len_trim(name1)
    if (nprocs.eq.1) call gmsh_out_field(x0,mesh0, W_dens,name3(1:l1),name1(1:l2))
  ENDIF
  ENDIF
end do

DEALLOCATE(mass,acc,vel)

END SUBROUTINE dyna_relax

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

SUBROUTINE make_mass(mass,mesh0,mat1,J0,dt)
USE data_mesh
USE data_tensor22
USE data_vector2
USE data_mat
USE data_vdw
USE data_BC
implicit REAL(8) (a-h,o-z)
implicit INTEGER*4 (i-n)
TYPE(mesh) :: mesh0
TYPE(material) :: mat1
REAL(8) ::  J0(mesh0%numele), mass((mesh0%numnods))
INTEGER(4) :: vert(3)

xmass_C=19.925d-3
! Lumped mass matrix
mass=0.d0
do ielem=1,mesh0%numele
  vert(:)=mesh0%connect(ielem)%vertices(:)
  xmass_el=J0(ielem)/2.d0/(mat1%s0/2.d0)*xmass_C
  mass(vert(1))=mass(vert(1)) + xmass_el/3
  mass(vert(2))=mass(vert(2)) + xmass_el/3
  mass(vert(3))=mass(vert(3)) + xmass_el/3
enddo

! Celerity, assuming E=1.TPa and t=0.34nm
Ce = sqrt(1.d3/2.d0/xmass_C*mat1%s0*0.34)
dt=1.d0*sqrt(J0(1)) / Ce                      / 1.d0

END SUBROUTINE make_mass

!###########################################################################

SUBROUTINE get_force(BCs,x0_BC,f_ext,numno,istep)
USE data_BC
implicit REAL(8) (a-h,o-z)
implicit INTEGER*4 (i-n)
TYPE(BC_data):: BCs
DIMENSION :: x0_BC(BCs%ndofBC)
REAL(8) :: xb(3), temp(3), f_ext(3*numno)
PARAMETER (pi0=3.141592653589793238d0)

nfreq=(40+5*int(istep/2000))*100

  do inod=1,BCs%nnodBC
    if (BCs%mnodBC(inod,2).eq.2) then
      f_ext(3*BCs%mnodBC(inod,1)-2:3*BCs%mnodBC(inod,1))= &
              BCs%rotation(1,1:3)*BCs%nloadstep * dsin(istep*2.d0*pi0/nfreq)  
!             f_ext(3*BCs%mnodBC(inod,1)-2:3*BCs%mnodBC(inod,1)) + BCs%rotation(1,1:3)
    endif
  enddo
if (istep.gt.20000) f_ext=0.d0


END SUBROUTINE get_force
