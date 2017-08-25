SUBROUTINE gauss(ngauss,shapef,weight)
implicit REAL(8) (a-h,o-z)
implicit INTEGER*4 (i-n)
REAL(8) shapef(ngauss,12,6), weight(ngauss)

if (ngauss==1) then
  call BSpline(shapef(1,:,1),1./3.,1./3.)
  call DBSpline(shapef(1,:,2:3),1./3.,1./3.)
  call DDBSpline(shapef(1,:,4:6),1./3.,1./3.)
  weight(1)=1.d0
else if (ngauss==2) then

pos1=1.d0/6.d0
pos2=2.d0/3.d0

  call BSpline(shapef(1,:,1),pos1,pos2)
  call DBSpline(shapef(1,:,2:3),pos1,pos2)
  call DDBSpline(shapef(1,:,4:6),pos1,pos2)

  call BSpline(shapef(2,:,1),pos2,pos1)
  call DBSpline(shapef(2,:,2:3),pos2,pos1)
  call DDBSpline(shapef(2,:,4:6),pos2,pos1)

  weight=[1.d0/2.d0,1.d0/2.d0]

else if (ngauss==3) then

pos1=1.d0/6.d0
pos2=2.d0/3.d0


  call BSpline(shapef(1,:,1),pos1,pos1)
  call DBSpline(shapef(1,:,2:3),pos1,pos1)
  call DDBSpline(shapef(1,:,4:6),pos1,pos1)

  call BSpline(shapef(2,:,1),pos2,pos1)
  call DBSpline(shapef(2,:,2:3),pos2,pos1)
  call DDBSpline(shapef(2,:,4:6),pos2,pos1)

  call BSpline(shapef(3,:,1),pos1,pos2)
  call DBSpline(shapef(3,:,2:3),pos1,pos2)
  call DDBSpline(shapef(3,:,4:6),pos1,pos2)

!  call BSpline(shapef(1,:,1),1./2.,0.)
!  call DBSpline(shapef(1,:,2:3),1./2.,0.)
!  call DDBSpline(shapef(1,:,4:6),1./2.,0.)

!  call BSpline(shapef(2,:,1),0.,1./2.)
!  call DBSpline(shapef(2,:,2:3),0.,1./2.)
!  call DDBSpline(shapef(2,:,4:6),0.,1./2.)

!  call BSpline(shapef(3,:,1),1./2.,1./2.)
!  call DBSpline(shapef(3,:,2:3),1./2.,1./2.)
!  call DDBSpline(shapef(3,:,4:6),1./2.,1./2.)

  weight=[1.d0/3.d0,1.d0/3.d0,1.d0/3.d0]
 else
  STOP ' Number of Gauss points not implemented'
end if

END SUBROUTINE gauss 

