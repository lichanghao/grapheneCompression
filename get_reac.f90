SUBROUTINE get_reac(mesh0,forces,nCodeLoad,ylength, &
     mdofBC,ndofBC,reaction1,reaction2,x0,BCs)
  USE data_mesh
  USE data_BC
  implicit REAL(8) (a-h,o-z)
  implicit INTEGER*4 (i-n)
  TYPE(mesh) :: mesh0
  TYPE(BC_data):: BCs
  DIMENSION forces(3*(mesh0%numnods)),mdofBC(ndofBC)
  DIMENSION x0(3*(mesh0%numnods))
  reaction1=0.d0
  reaction2=0.d0

  if (nCodeLoad.eq.2) then
!!$   do i=1,ndofBC/6
!!$     reaction1=reaction1 + forces(mdofBC(3*i))*(x0(mdofBC(3*i-1))-BCs%xc(2)) &
!!$  -forces(mdofBC(3*i-1))*(x0(mdofBC(3*i))-BCs%xc(3))
!!$   !  reaction2=reaction2 + forces(mdofBC(ndofBC/2+3*i-2))
!!$     reaction2=reaction2 +  forces(mdofBC(ndofBC/2+3*i))*(x0(mdofBC(ndofBC/2+3*i-1)) &
!!$-BCs%xc(2))-forces(mdofBC(ndofBC/2+3*i-1))*(x0(mdofBC(ndofBC/2+3*i))-BCs%xc(3))
!!$   end do


     do i=1,BCs%nnodBC
        if (BCs%mnodBC(i,2).eq.1) then
           reaction1=reaction1 + forces(mdofBC(3*i))*(x0(mdofBC(3*i-1))-BCs%xc(2)) &
                -forces(mdofBC(3*i-1))*(x0(mdofBC(3*i))-BCs%xc(3))
        else
           reaction2=reaction2 +  forces(mdofBC(3*i))*(x0(mdofBC(3*i-1)) &
                -BCs%xc(2))-forces(mdofBC(3*i-1))*(x0(mdofBC(3*i))-BCs%xc(3)) 
        end if
     enddo

!!!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

  elseif (nCodeLoad.eq.3) then
     do i=1,BCs%nnodBC
        if (BCs%mnodBC(i,2).eq.1) then
           reaction1=reaction1 + forces(mdofBC(3*i))
        else
           reaction2=reaction2 +  forces(mdofBC(3*i))
        end if
     enddo


!!!!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  else if (nCodeLoad.eq.4) then
     do i=1,(ndofBC-3)/2
        reaction1=reaction1 + forces(mdofBC(i))
        reaction2=reaction2 + forces(mdofBC((ndofBC-3)/2+i))
     end do

  else if (nCodeLoad.eq.20) then
     write(*,*)'we are writing the reaction forces'

  else
     STOP 'Code of loading not implemented' 
  end if

  !reaction1=reaction1/ylength
  !reaction2=reaction2/ylength

END SUBROUTINE get_reac
