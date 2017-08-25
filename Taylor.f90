PURE REAL(8) FUNCTION sinxx(x)
implicit REAL(8) (a-h,o-z)
implicit INTEGER*4 (i-n)
REAL(8), INTENT(IN) :: x

!if (abs(x)<1.d-6) then
!  t1 = x**2
!  t3 = t1**2
!  t7 = t3**2
!  sinxx = 1.D0-t1/6.D0+t3/120.D0-t3*t1/5040.D0+t7/362880.D0

      t2 = x**2
      t4 = t2**2
      t8 = t4**2
   sinxx = 1.D0-t2/6.D0+t4/120.D0-t4*t2/5040.D0+t8/362880.D0-t8*t2/39916800.D0

!else
!  sinxx=dsin(x)/x
!end if

END FUNCTION sinxx


PURE REAL(8) FUNCTION dsinxx(x)
implicit REAL(8) (a-h,o-z)
implicit INTEGER*4 (i-n)
REAL(8), INTENT(IN) :: x

!if (abs(x)<1.d-6) then
!  t2 = x**2
!  t3 = t2*x
!  t5 = t2**2
!  dsinxx = -x/3.D0+t3/30.D0-t5*x/840.D0+t5*t3/45360.D0

      t4 = x**2
      t5 = t4*x
      t7 = t4**2
      t12 = t7**2
   dsinxx = -x/3.D0+t5/30.D0-t7*x/840.D0+t7*t5/45360.D0-t12*x/3991680.D0

!else
!  dsinxx=(dcos(x)-dsin(x)/x)/x
!end if


END FUNCTION dsinxx

PURE REAL(8) FUNCTION ddsinxx(x)
implicit REAL(8) (a-h,o-z)
implicit INTEGER*4 (i-n)
REAL(8), INTENT(IN) :: x

!if (abs(x)<1.d-6) then
!  t1 = x**2      
!  t3 = t1**2
!  t7 = t3**2
!  ddsinxx = -1.D0/3.D0+t1/10.D0-t3/168.D0+t3*t1/6480.D0-t7/443520.D0

      t4 = x**2
      t6 = t4**2
      t10 = t6**2
   ddsinxx = -1.D0/3.D0+t4/10.D0-t6/168.D0+t6*t4/6480.D0-t10/443520.D0 &
             +t10*t4/47174400.D0

!else
!  ddsinxx=-(dsin(x)+2.d0*(dcos(x)-dsin(x)/x)/x)/x
!end if


END FUNCTION ddsinxx







