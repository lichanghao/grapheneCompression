SUBROUTINE newton_inner(C_elem,curvppal,vppal,mat1,x,W,dWdp,crit,n,maxn)
USE data_mat
implicit REAL(8) (a-h,o-z)
implicit INTEGER*4 (i-n)
TYPE(material) mat1
REAL(8) C_elem(3), curvppal(2), vppal(2,2), dWdp(6)
DIMENSION :: x(2), dx(2),dW(2),ddW(3)


n=0
call Hyper_pot_inner(C_elem,curvppal,vppal,mat1,x,W,dW,ddW,dWdp)
test=crit*(1.d0+dabs(W))

do while ((dsqrt(dW(1)*dW(1)+dW(2)*dW(2)).gt.test).and.(n<maxn))
!do while ((maxval(dabs(dW)).gt.test).and.(n<maxn))
  n=n+1
  det=ddW(1)*ddW(2)-ddW(3)*ddW(3)
  dx(1)=(dW(2)*ddW(3)-dW(1)*ddW(2))/det
  dx(2)=(dW(1)*ddW(3)-dW(2)*ddW(1))/det
  x=x+dx

  if (dsqrt(x(1)**2+x(2)**2).gt.0.2*mat1%A0) then
    n=maxn
    exit
  end if

  call Hyper_pot_inner(C_elem,curvppal,vppal,mat1,x,W,dW,ddW,dWdp)
  test=crit*(1.d0+dabs(W))
end do


! We got to a maximum
if (det.lt.0.d0) then
! write(*,*) ' INNER WARNING 3'
 n=maxn
end if

END SUBROUTINE newton_inner
