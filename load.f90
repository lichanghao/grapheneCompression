!*********************************************************************88
!*********************************************************************88
! Increment boundary condition
SUBROUTINE load_doit(BCs,x0, &
                   f_ext,numno,xlength,denter_x,iload,mesh0,x0_BC)
USE data_BC
USE data_mesh
implicit REAL(8) (a-h,o-z)
implicit INTEGER*4 (i-n)
TYPE(BC_data):: BCs
TYPE(mesh) :: mesh0
DIMENSION :: x0_BC(BCs%ndofBC)
REAL(8) :: xb(3), temp(3), f_ext(3*numno),denter_x(3),anglevalue
REAL(8) :: x0(3*(mesh0%numnods+mesh0%nedge))
PARAMETER (pi0=3.141592653589793238d0)

if ((BCs%nCodeLoad.eq.0)) then
  continue
else if ((BCs%nCodeLoad.eq.1).or.(BCs%nCodeLoad.eq.2)) then
      ! Twisting

   !BCs%value=BCs%value*pi0/180.

   do inod=1,mesh0%numnods
      if (x0(inod*3-2).gt.BCs%xc(1))then
         anglevalue=(x0(inod*3-2)-BCs%xc(1))*2*BCs%value*pi0/xlength/180
!write(*,*)'@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@',BCs%value,anglevalue,x
!write(*,*) '###########################', anglevalue
         BCs%rotation(1,1:3)=[1.d0,0.d0,0.d0]
         BCs%rotation(2,1:3)=[0.d0, cos(anglevalue/BCs%nloadstep),sin(anglevalue/BCs%nloadstep)]
         BCs%rotation(3,1:3)=[0.d0,-sin(anglevalue/BCs%nloadstep),cos(anglevalue/BCs%nloadstep)]

         xb=x0(3*inod-2:3*inod)
         temp(1)=BCs%rotation(1,1)*(xb(1)-BCs%xc(1))+BCs%rotation(1,2)*(xb(2)-BCs%xc(2))+ &
              BCs%rotation(1,3)*(xb(3)-BCs%xc(3))
         temp(2)=BCs%rotation(2,1)*(xb(1)-BCs%xc(1))+BCs%rotation(2,2)*(xb(2)-BCs%xc(2))+ &
              BCs%rotation(2,3)*(xb(3)-BCs%xc(3))
         temp(3)=BCs%rotation(3,1)*(xb(1)-BCs%xc(1))+BCs%rotation(3,2)*(xb(2)-BCs%xc(2))+ &
              BCs%rotation(3,3)*(xb(3)-BCs%xc(3))
         x0(3*inod-2:3*inod)=temp+BCs%xc
      endif

      if (x0(inod*3-2).lt.BCs%xc(1))then

         anglevalue=(BCs%xc(1)-x0(inod*3-2))*2*BCs%value*pi0/xlength/180


         BCs%rotation(1,1:3)=[1.d0,0.d0,0.d0]
         BCs%rotation(2,1:3)=[0.d0, cos(anglevalue/BCs%nloadstep),sin(anglevalue/BCs%nloadstep)]
         BCs%rotation(3,1:3)=[0.d0,-sin(anglevalue/BCs%nloadstep),cos(anglevalue/BCs%nloadstep)]

         xb=x0(3*inod-2:3*inod)
         temp(1)=BCs%rotation(1,1)*(xb(1)-BCs%xc(1))+BCs%rotation(2,1)*(xb(2)-BCs%xc(2))+ &
              BCs%rotation(3,1)*(xb(3)-BCs%xc(3))
         temp(2)=BCs%rotation(1,2)*(xb(1)-BCs%xc(1))+BCs%rotation(2,2)*(xb(2)-BCs%xc(2))+ &
              BCs%rotation(3,2)*(xb(3)-BCs%xc(3))
         temp(3)=BCs%rotation(1,3)*(xb(1)-BCs%xc(1))+BCs%rotation(2,3)*(xb(2)-BCs%xc(2))+ &
              BCs%rotation(3,3)*(xb(3)-BCs%xc(3))
         x0(3*inod-2:3*inod)=temp+BCs%xc
      endif
   enddo

x0_BC(:) = x0(BCs%mdofBC(:))



else if (BCs%nCodeLoad.eq.10) then
   do inod=1,BCs%nnodBC
      if (BCs%mnodBC(inod,2).eq.1) then
         ! look for position of these dofs in mdof
         do idof=1,BCs%ndofBC
            if (BCs%mdofBC(idof).eq.3*BCs%mnodBC(inod,1)-2) then
           jdof=idof
           exit
         endif
      enddo
      xb=x0_BC(jdof:jdof+2)
      temp(1)=BCs%rotation(1,1)*(xb(1)-BCs%xc(1))+BCs%rotation(1,2)*(xb(2)-BCs%xc(2))+ &
              BCs%rotation(1,3)*(xb(3)-BCs%xc(3))
      temp(2)=BCs%rotation(2,1)*(xb(1)-BCs%xc(1))+BCs%rotation(2,2)*(xb(2)-BCs%xc(2))+ &
              BCs%rotation(2,3)*(xb(3)-BCs%xc(3))
      temp(3)=BCs%rotation(3,1)*(xb(1)-BCs%xc(1))+BCs%rotation(3,2)*(xb(2)-BCs%xc(2))+ &
              BCs%rotation(3,3)*(xb(3)-BCs%xc(3))
      x0_BC(jdof:jdof+2)=temp+BCs%xc
    endif
    if (BCs%mnodBC(inod,2).eq.2) then
      ! look for position of these dofs in mdof
      do idof=1,BCs%ndofBC
         if (BCs%mdofBC(idof).eq.3*BCs%mnodBC(inod,1)-2) then
           jdof=idof
           exit
         endif
      enddo
      xb=x0_BC(jdof:jdof+2)
      temp(1)=BCs%rotation(1,1)*(xb(1)-BCs%xc(1))+BCs%rotation(2,1)*(xb(2)-BCs%xc(2))+ &
              BCs%rotation(3,1)*(xb(3)-BCs%xc(3))
      temp(2)=BCs%rotation(1,2)*(xb(1)-BCs%xc(1))+BCs%rotation(2,2)*(xb(2)-BCs%xc(2))+ &
              BCs%rotation(3,2)*(xb(3)-BCs%xc(3))
      temp(3)=BCs%rotation(1,3)*(xb(1)-BCs%xc(1))+BCs%rotation(2,3)*(xb(2)-BCs%xc(2))+ &
              BCs%rotation(3,3)*(xb(3)-BCs%xc(3))
      x0_BC(jdof:jdof+2)=temp+BCs%xc
    endif
  enddo
else if ((BCs%nCodeLoad.eq.13)) then
  do inod=1,BCs%nnodBC
    if (BCs%mnodBC(inod,2).eq.1) then
      xb=x0_BC(3*inod-2:3*inod)
      temp(1)=BCs%rotation(1,1)*(xb(1)-BCs%xc(1))+BCs%rotation(1,2)*(xb(2)-BCs%xc(2))+ &
              BCs%rotation(1,3)*(xb(3)-BCs%xc(3))
      temp(2)=BCs%rotation(2,1)*(xb(1)-BCs%xc(1))+BCs%rotation(2,2)*(xb(2)-BCs%xc(2))+ &
              BCs%rotation(2,3)*(xb(3)-BCs%xc(3))
      temp(3)=0.d0*(xb(1)-BCs%xc(1))+0.d0*(xb(2)-BCs%xc(2))+ &
              1.d0*(xb(3)-BCs%xc(3))
      x0_BC(3*inod-2:3*inod)=temp+BCs%xc
      x0_BC(3*inod-2:3*inod)=x0_BC(3*inod-2:3*inod) + BCs%rotation(3,1:3)
    else
      xb=x0_BC(3*inod-2:3*inod)
      temp(1)=BCs%rotation(1,1)*(xb(1)-BCs%xc(1))+BCs%rotation(2,1)*(xb(2)-BCs%xc(2))+ &
              0.d0*(xb(3)-BCs%xc(3))
      temp(2)=BCs%rotation(1,2)*(xb(1)-BCs%xc(1))+BCs%rotation(2,2)*(xb(2)-BCs%xc(2))+ &
              0.d0*(xb(3)-BCs%xc(3))
      temp(3)=BCs%rotation(1,3)*(xb(1)-BCs%xc(1))+BCs%rotation(2,3)*(xb(2)-BCs%xc(2))+ &
              1.d0*(xb(3)-BCs%xc(3))
      x0_BC(3*inod-2:3*inod)=temp+BCs%xc
      x0_BC(3*inod-2:3*inod)=x0_BC(3*inod-2:3*inod) - BCs%rotation(3,1:3)
    endif
  enddo
else if (BCs%nCodeLoad.eq.11) then
  dang=BCs%value/BCs%nloadstep*pi0/180.d0
  ang1= iload   *dang
  ang2=(iload-1)*dang
  DL=-1.d0*xlength*(sinxx(ang1)-sinxx(ang2))
  do inod=1,BCs%nnodBC
    if (BCs%mnodBC(inod,2).eq.1) then
      xb=x0_BC(3*inod-2:3*inod)
      temp(1)=BCs%rotation(1,1)*(xb(1)-BCs%xc(1))+BCs%rotation(1,2)*(xb(2)-BCs%xc(2))+ &
              BCs%rotation(1,3)*(xb(3)-BCs%xc(3))
      temp(2)=BCs%rotation(2,1)*(xb(1)-BCs%xc(1))+BCs%rotation(2,2)*(xb(2)-BCs%xc(2))+ &
              BCs%rotation(2,3)*(xb(3)-BCs%xc(3))
      temp(3)=BCs%rotation(3,1)*(xb(1)-BCs%xc(1))+BCs%rotation(3,2)*(xb(2)-BCs%xc(2))+ &
              BCs%rotation(3,3)*(xb(3)-BCs%xc(3))
      x0_BC(3*inod-2:3*inod)=temp+BCs%xc
      x0_BC(3*inod-2)=x0_BC(3*inod-2) + DL/2.d0
    else
      xb=x0_BC(3*inod-2:3*inod)
      temp(1)=BCs%rotation(1,1)*(xb(1)-BCs%xc(1))+BCs%rotation(2,1)*(xb(2)-BCs%xc(2))+ &
              BCs%rotation(3,1)*(xb(3)-BCs%xc(3))
      temp(2)=BCs%rotation(1,2)*(xb(1)-BCs%xc(1))+BCs%rotation(2,2)*(xb(2)-BCs%xc(2))+ &
              BCs%rotation(3,2)*(xb(3)-BCs%xc(3))
      temp(3)=BCs%rotation(1,3)*(xb(1)-BCs%xc(1))+BCs%rotation(2,3)*(xb(2)-BCs%xc(2))+ &
              BCs%rotation(3,3)*(xb(3)-BCs%xc(3))
      x0_BC(3*inod-2:3*inod)=temp+BCs%xc
      x0_BC(3*inod-2)=x0_BC(3*inod-2) - DL/2.d0
    endif
  enddo
else if (BCs%nCodeLoad.eq.3) then
   if(iload.gt.-10)then
   do inod=1,mesh0%numnods
      if (x0(inod*3-1).gt.BCs%xc(2))then
         anglevalue=(x0(inod*3-1)-BCs%xc(2))*2*BCs%value/xlength/BCs%nloadstep
         !write(*,*)'@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@',BCs%value,anglevalue,x
         !write(*,*) '###########################', anglevalue
         BCs%rotation(1,1:3)=[0.d0,anglevalue,0.d0]
         x0(3*inod-2:3*inod)=x0(3*inod-2:3*inod)-BCs%rotation(1,1:3)
      endif

      if (x0(inod*3-1).lt.BCs%xc(2))then

         anglevalue=(BCs%xc(2)-x0(inod*3-1))*2*BCs%value/xlength/BCs%nloadstep

         BCs%rotation(1,1:3)=[0.d0,anglevalue,0.d0]
         x0(3*inod-2:3*inod)=x0(3*inod-2:3*inod)+BCs%rotation(1,1:3)
      endif
   enddo

   else
   do inod=1,mesh0%numnods
      if (x0(inod*3-2).gt.BCs%xc(1))then
         anglevalue=(x0(inod*3-2)-BCs%xc(1))*2*BCs%value/xlength/BCs%nloadstep
         !write(*,*)'@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@',BCs%value,anglevalue,x
         !write(*,*) '###########################', anglevalue
         BCs%rotation(1,1:3)=[anglevalue,0.d0,0.d0]
         x0(3*inod-2:3*inod)=x0(3*inod-2:3*inod)-BCs%rotation(1,1:3)
      endif

      if (x0(inod*3-2).lt.BCs%xc(1))then

         anglevalue=(BCs%xc(1)-x0(inod*3-2))*2*BCs%value/xlength/BCs%nloadstep

         BCs%rotation(1,1:3)=[anglevalue,0.d0,0.d0]
         x0(3*inod-2:3*inod)=x0(3*inod-2:3*inod)+BCs%rotation(1,1:3)
      endif
   enddo
 
   endif



x0_BC(:) = x0(BCs%mdofBC(:))


!!$
!!$  do inod=1,BCs%nnodBC
!!$    if (BCs%mnodBC(inod,2).eq.1) then
!!$      x0_BC(3*inod-2)=x0_BC(3*inod-2) + 1.0
!!$    else if (BCs%mnodBC(inod,2).eq.2) then
!!$      x0_BC(3*inod-2)=x0_BC(3*inod-2) - 1.0
!!$    else if (BCs%mnodBC(inod,2).eq.3) then
!!$      x0_BC(3*inod-1)=x0_BC(3*inod-1) + 1.0
!!$    else if (BCs%mnodBC(inod,2).eq.4) then
!!$      x0_BC(3*inod-1)=x0_BC(3*inod-1) - 1.0
!!$    else if (BCs%mnodBC(inod,2).eq.5) then
!!$      x0_BC(3*inod-2)=x0_BC(3*inod-2) + 1.0
!!$      x0_BC(3*inod-1)=x0_BC(3*inod-1) - 1.0
!!$    else if (BCs%mnodBC(inod,2).eq.6) then
!!$      x0_BC(3*inod-2)=x0_BC(3*inod-2) - 1.0
!!$      x0_BC(3*inod-1)=x0_BC(3*inod-1) - 1.0
!!$    else if (BCs%mnodBC(inod,2).eq.7) then
!!$      x0_BC(3*inod-2)=x0_BC(3*inod-2) - 1.0
!!$      x0_BC(3*inod-1)=x0_BC(3*inod-1) + 1.0
!!$    else if (BCs%mnodBC(inod,2).eq.8) then
!!$      x0_BC(3*inod-2)=x0_BC(3*inod-2) + 1.0
!!$      x0_BC(3*inod-1)=x0_BC(3*inod-1) + 1.0
!!$    end if
!!$  enddo
else if ((BCs%nCodeLoad.eq.4)) then
  do inod=1,BCs%nnodBC
    if (BCs%mnodBC(inod,2).eq.1) then
      x0_BC(inod)=x0_BC(inod) + BCs%rotation(1,1)
    else
      x0_BC(inod)=x0_BC(inod) - BCs%rotation(1,1)
    end if
  enddo
else if ((BCs%nCodeLoad.eq.7)) then
  do inod=1,BCs%nnodBC
    if (BCs%mnodBC(inod,2).eq.1) then
      x0_BC(3*inod-2:3*inod)=x0_BC(3*inod-2:3*inod) + BCs%rotation(1,1:3)
    else
      x0_BC(3*inod-2:3*inod)=x0_BC(3*inod-2:3*inod) - BCs%rotation(1,1:3)
    end if
  enddo
else if ((BCs%nCodeLoad.eq.5)) then
  do inod=1,BCs%nnodBC
    if (BCs%mnodBC(inod,2).eq.2) then
      f_ext(3*BCs%mnodBC(inod,1)-2:3*BCs%mnodBC(inod,1))= &
             f_ext(3*BCs%mnodBC(inod,1)-2:3*BCs%mnodBC(inod,1)) + BCs%rotation(1,1:3)
    endif
  enddo
else if ((BCs%nCodeLoad.eq.20)) then ! indentation
   denter_x(1) = denter_x(1) - 0.1d0
   denter_x(2:3) = [0.0,0.0]
   write(*,*) 'denter_x(1)=', denter_x(1)
else if((BCs%nCodeLoad.eq.21)) then ! totally free
   continue
else if ((BCs%nCodeLoad.eq.6)) then
  do inod=1,BCs%nnodBC
    if (BCs%mnodBC(inod,2).eq.1) then
      x0_BC(inod)=x0_BC(inod)+BCs%rotation(1,2)
    else if (BCs%mnodBC(inod,2).eq.2) then
      x0_BC(inod)=x0_BC(inod)-BCs%rotation(1,2)
    end if
  enddo
else if ((BCs%nCodeLoad.eq.66)) then


  N_=BCs%nloadstep+1

  do inod=1,BCs%nnodBC

    if (BCs%mnodBC(inod,2).eq.1) then
       icdof=BCs%mdofBC(inod)
       Y_i_=x0_BC(inod)
       Y_0 = (N_*Y_i_-(iload-1)*0.34d0/2.d0)/(1.d0*(N_-iload+1))
       Y_i = Y_0 - (Y_0-0.34d0/2.d0)/N_*(iload)
       x0_BC(inod)=Y_i
    endif
    if (BCs%mnodBC(inod,2).eq.2) then
       icdof=BCs%mdofBC(inod)
       Y_i_=-x0_BC(inod)
       Y_0 = (N_*Y_i_-(iload-1)*0.34d0/2.d0)/(1.d0*(N_-iload+1))
       Y_i = Y_0 - (Y_0-0.34d0/2.d0)/N_*(iload)
       x0_BC(inod)=-Y_i

    endif

  enddo


else if ((BCs%nCodeLoad.eq.8)) then
  do inod=1,BCs%nnodBC
    if (BCs%mnodBC(inod,2).eq.1) then
      f_ext(3*BCs%mnodBC(inod,1)-2:3*BCs%mnodBC(inod,1))= &
             f_ext(3*BCs%mnodBC(inod,1)-2:3*BCs%mnodBC(inod,1)) + BCs%rotation(1,1:3)
    endif
    if (BCs%mnodBC(inod,2).eq.2) then
      f_ext(3*BCs%mnodBC(inod,1)-2)= &
             f_ext(3*BCs%mnodBC(inod,1)-2) - BCs%rotation(1,1)
      f_ext(3*BCs%mnodBC(inod,1)-1)= &
             f_ext(3*BCs%mnodBC(inod,1)-1) + BCs%rotation(1,2)
      f_ext(3*BCs%mnodBC(inod,1)  )= &
             f_ext(3*BCs%mnodBC(inod,1)  ) + BCs%rotation(1,3)
    endif
  enddo
else
  STOP ' Loading option not implemented'
end if


END SUBROUTINE load_doit




