!***************************************************************************
! Here are the potentials
SUBROUTINE Brenner2(mat1,pe,W,dW)
USE data_mat
implicit REAL(8) (a-h,o-z)
implicit INTEGER*4 (i-n)
TYPE(material) mat1
! Vr , Va and Ga, 1 is function, 2 is first derivative
DIMENSION :: pe(6),  dW(6), Vr(2), Va(2), Ga(2,3), iperm(2,3)
data iperm(1,1:3) /2, 3, 1/
data iperm(2,1:3) /3, 1, 2/

if (maxval(pe(1:3)).gt.0.17) write(*,*) ' Brenner Potential 2 implemented for moderate defs.'

W=0.
dW=0.
call Gangular2(pe(4:6),Ga)

do ibond=1,3
  Fang=1.d0/dsqrt(1.d0+Ga(1,iperm(1,ibond))+Ga(1,iperm(2,ibond)))
  call Vrepulsv2(pe(ibond),Vr)
  call Vattract2(pe(ibond),Va)
  W=W+Vr(1)-Fang*Va(1)
  dW(ibond)=Vr(2)-Fang*Va(2)
  dW(3+iperm(1,ibond))=dW(3+iperm(1,ibond))+Va(1)*(Fang**3)*Ga(2,iperm(1,ibond))/2.d0
  dW(3+iperm(2,ibond))=dW(3+iperm(2,ibond))+Va(1)*(Fang**3)*Ga(2,iperm(2,ibond))/2.d0
end do

W=W/mat1%s0
dW=dW/mat1%s0
END SUBROUTINE Brenner2

!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
SUBROUTINE Vrepulsv2(a,Vr)
implicit REAL(8) (a-h,o-z)
implicit INTEGER*4 (i-n)
REAL(8), INTENT(IN) :: a
REAL(8), INTENT(OUT) :: Vr(2)
PARAMETER (Q_par=0.03134602960833d0,A_par=1.754951652511304d3,alpha_par=47.465390606595d0)
!PARAMETER (Q_par=-0.030202639138d0,A_par=1.592511339809484d3,alpha_par=44.3353425736d0)

aux1=1.d0+Q_par/a
aux2=A_par*exp(-alpha_par*a)

Vr(1)= aux1*aux2
Vr(2)=-aux2*(Q_par/a/a+alpha_par*aux1)

END SUBROUTINE Vrepulsv2
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
SUBROUTINE Vattract2(a,Va)
implicit REAL(8) (a-h,o-z)
implicit INTEGER*4 (i-n)
REAL(8), INTENT(IN) :: a
REAL(8), INTENT(OUT) :: Va(2)
REAL(8) :: B_par(3), beta_par(3)
data B_par(1:3)  /1.984903756490406d3, 2.81460945880185d0, 4.92107577361796d0/
data beta_par(1:3) /47.204523127d0, 14.332132499d0, 13.826912506d0/
!data B_par(1:3)  /0.43460221973329d3, 1.0579560223603662d1, 4.829005919247588d0/
!data beta_par(1:3) /36.7942973600d0, 17.8128093260d0, 29.6342372285d0/
Va=0.d0
Va=0.d0

do i=1,3
  aux1=B_par(i) * exp(-beta_par(i)*a)
  Va(1)=Va(1) + aux1
  Va(2)=Va(2) - aux1 * beta_par(i)
enddo

END SUBROUTINE Vattract2
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
SUBROUTINE Gangular2(theta,Ga)
implicit REAL(8) (a-h,o-z)
implicit INTEGER*4 (i-n)
DIMENSION :: theta(3)
DIMENSION :: Ga(2,3)
DIMENSION :: dat1(6),dat2(6), G_spline(2), dat(6)
! We have two intervals, [c1,c2]=[cos pi, cos 2pi/3] and [c2,c3]=[cos 2pi/3, cos 0.6082pi]
!   beyond which the potential is not implemented
! The model parameters (first and second derivatives) have been scaled 
!   for a quintic spline between 0 and 1
! The order is 1,2: function at borns of interval, 3,4: first der., 5,6: second der.
!data (dat1(1)=-0.001d0,dat1(2)=0.0528d0,dat1(3)=0.052d0,dat1(4)=0.085d0,dat1(5)=0.d0,dat1(6)=0.0925d0)
!data (dat2(1)=0.0528d0,dat2(2)=0.09733d0,dat2(3)=0.02831996387327d0,dat2(4)=0.06663520911358d0 &
!       ,dat2(5)=0.01026808065397d0,dat2(6)=0.05494810728343d0)
data dat1(1:6)  /-0.001d0, 0.0528d0, 0.104d0, 0.17d0, 0.d0, 0.37d0/
data dat2(1:6)  /0.0528d0, 0.09733d0, 0.17d0, 0.4d0, 0.37d0, 1.98d0/
data c1, c2, c3 /-1.d0, -.5d0, -.33341197721605d0/

dcos1=c2-c1
dcos2=c3-c2


do i=1,3
  aux1=dcos(theta(i))
  if ((aux1.gt.c1).and.(aux1.le.c2)) then
    dat(:)=dat1(:)
    dat(3:6)=dat(3:6)*dcos1
    dat(5:6)=dat(5:6)*dcos1
    aux2=(aux1-c1)/dcos1
	call spline(dat,aux2,G_spline)
    Ga(1,i)=G_spline(1)
    ! G_spline(2) is the derivative of G_spline(1) wrt xi\in[0,1]
    Ga(2,i)=G_spline(2) /dcos1 * (-1.d0*dsin(theta(i))) 
  else  if ((aux1.gt.c2).and.(aux1.lt.c3)) then
    dat(:)=dat2(:)
    dat(3:6)=dat(3:6)*dcos2
    dat(5:6)=dat(5:6)*dcos2
    aux2=(aux1-c2)/dcos2
	call spline(dat,aux2,G_spline)
    Ga(1,i)=G_spline(1)
    Ga(2,i)=G_spline(2) /dcos2 * (-1.d0*dsin(theta(i))) 
  else  if ((aux1.gt.c3).and.(aux1.le.1.d0)) then
    x=aux1
    x2=x*x
    x3=x*x2
    x4=x2*x2
    x5=x4*x
    Ga(1,i)=-.04005399574455538497890162d0*x5 + 1.272040452823819270894289d0*x4 &
  	        -.5596772082312575809128441d0*x3  - .4330817380737560058603059d0*x2 &
		   +.4889164892257497008577588d0*x  + .2718560000000000000000001d0
    Ga(2,i)=-5.d0*.04005399574455538497890162d0*x4 + 4.d0*1.272040452823819270894289d0*x3 &
	       -3.d0*.5596772082312575809128441d0*x2 - 2.d0*.4330817380737560058603059d0*x  &
		   +.4889164892257497008577588d0
    Ga(2,i)=Ga(2,i) * (-1.d0*dsin(theta(i))) 
  else
    write(*,*) 'kk1 ',theta(i)*180.d0/3.14159
    STOP ' Angle out of range of BRENNER 2'
  endif
end do
END SUBROUTINE Gangular2

SUBROUTINE spline(dat,x,G)
implicit REAL(8) (a-h,o-z)
implicit INTEGER*4 (i-n)
REAL*8 dat(6), G(2)
REAL*8 x
REAL*8 funcs(6), dfuncs(6)
integer*4 i

x2=x*x
x3=x*x2
x4=x2*x2
x5=x4*x

funcs(1) = -6.D0*x5+15.D0*x4-10.D0*x3+1.D0
funcs(2) = 6.D0*x5-15.D0*x4+10.D0*x3
funcs(3) = -3.D0*x5+8.D0*x4-6.D0*x3+x
funcs(4) = -3.D0*x5+7.D0*x4-4.D0*x3
funcs(5) = -x5/2.D0+3.D0/2.D0*x4-3.D0/2.D0*x3+x2/2.D0
funcs(6) = x5/2.D0-x4+x3/2.D0

dfuncs(1) = -30.D0*x4+60.D0*x3-30.D0*x2
dfuncs(2) = 30.D0*x4-60.D0*x3+30.D0*x2
dfuncs(3) = -15.D0*x4+32.D0*x3-18.D0*x2+1.D0
dfuncs(4) = -15.D0*x4+28.D0*x3-12.D0*x2
dfuncs(5) = -5.D0/2.D0*x4+6.D0*x3-9.D0/2.D0*x2+x
dfuncs(6) = 5.D0/2.D0*x4-4.D0*x3+3.D0/2.D0*x2
G = 0.D0
do i = 1,6
   G(1) = G(1) +  funcs(i)*dat(i)
   G(2) = G(2) + dfuncs(i)*dat(i)
enddo
END SUBROUTINE spline


!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
! NOW STARTS THE INNER PART
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

SUBROUTINE Inner_Brenner2(mat1,pe,dpedeta,ddpedeta,W,dWdeta,ddWdeta,dW)
USE data_mat
USE data_vector3
USE data_vector2
implicit REAL(8) (a-h,o-z)
implicit INTEGER*4 (i-n)
DIMENSION :: pe(6),Vr(3), Va(3),dWdeta(2),ddWdeta(3),aux1(3)
DIMENSION :: Ga(3,3), iperm(2,3), dW(6), ddW(6,6)
! 1 is 1,1, 2 is 2,2 and 3 is both 1,2 and 2,1
TYPE(vector2) :: dpedeta(6),aux(6)
TYPE(vector3) :: ddpedeta(6)
TYPE(material) mat1
data iperm(1,1:3) /2, 3, 1/
data iperm(2,1:3) /3, 1, 2/

W=0.
dW=0.d0
ddW=0.d0
call Gang_bis2(pe(4:6),Ga)

do ibond=1,3
  Fang=1/dsqrt(1.d0+Ga(1,iperm(1,ibond))+Ga(1,iperm(2,ibond)))
  call Vrep_bis2(pe(ibond),Vr)
  call Vatt_bis2(pe(ibond),Va)
  W=W+Vr(1)-Fang*Va(1)
  dW(ibond)=Vr(2)-Fang*Va(2)
  dW(3+iperm(1,ibond))=dW(3+iperm(1,ibond))+Va(1)*(Fang**3)*Ga(2,iperm(1,ibond))/2.d0
  dW(3+iperm(2,ibond))=dW(3+iperm(2,ibond))+Va(1)*(Fang**3)*Ga(2,iperm(2,ibond))/2.d0

  ddW(ibond,ibond)=Vr(3)-Fang*Va(3)
  ddW(3+iperm(1,ibond),3+iperm(1,ibond))=ddW(3+iperm(1,ibond),3+iperm(1,ibond))+ &
        Ga(3,iperm(1,ibond))/2.d0*Va(1)*(Fang**3) &
	   -3./4.*((Ga(2,iperm(1,ibond)))**2)*Va(1)*(Fang**5)
  ddW(3+iperm(2,ibond),3+iperm(2,ibond))=ddW(3+iperm(2,ibond),3+iperm(2,ibond))+ &
        Ga(3,iperm(2,ibond))/2.d0*Va(1)*(Fang**3) &
	   -3./4.*((Ga(2,iperm(2,ibond)))**2)*Va(1)*(Fang**5)
  do kk=1,2
    ddW(ibond,3+iperm(kk,ibond))=Va(2)*(Fang**3)*Ga(2,iperm(kk,ibond))/2.d0
  end do
  ddW(3+iperm(1,ibond),3+iperm(2,ibond))=-3./4.*Va(1)*(Fang**5)* &
             Ga(2,iperm(1,ibond))*Ga(2,iperm(2,ibond))

end do

!fill in lower part of symmetric matrix except for one term
ddW(4,6)=ddW(6,4)
do i=2,6
  do j=1,i-1
    ddW(i,j)=ddW(j,i)
  end do
end do
W=W/mat1%s0
dW=dW/mat1%s0
ddW=ddW/mat1%s0

do i=1,6
  aux(i)%val=[0.,0.]
  do j=1,6
    aux(i)%val=aux(i)%val + ddW(i,j)*dpedeta(j)%val
  end do
end do

dWdeta=[0.,0.]
ddWdeta=[0.,0.,0.]
do i=1,6
  dWdeta=dWdeta+dW(i)*dpedeta(i)%val
  aux1=[aux(i)%val(1)*dpedeta(i)%val(1), &
        aux(i)%val(2)*dpedeta(i)%val(2), &
	   (aux(i)%val(1)*dpedeta(i)%val(2) + aux(i)%val(2)*dpedeta(i)%val(1))/2.]
  ddWdeta=ddWdeta+aux1+dW(i)*ddpedeta(i)%val
end do

END SUBROUTINE Inner_Brenner2


!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
SUBROUTINE Vrep_bis2(a,Vr)
implicit REAL(8) (a-h,o-z)
implicit INTEGER*4 (i-n)
REAL(8), INTENT(IN) :: a
REAL(8), INTENT(OUT) :: Vr(3)
PARAMETER (Q_par=0.03134602960833d0,A_par=1.754951652511304d3,alpha_par=47.465390606595d0)
!PARAMETER (Q_par=-0.030202639138d0,A_par=1.592511339809484d3,alpha_par=44.3353425736d0)


aux1=1.d0+Q_par/a
aux2=A_par*exp(-alpha_par*a)

Vr(1)= aux1*aux2
aux3=(Q_par/a/a+alpha_par*aux1)
Vr(2)=-aux2*aux3
Vr(3)=aux2*(alpha_par*aux3 + Q_par/a/a*(2.d0/a+alpha_par))

END SUBROUTINE Vrep_bis2
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
SUBROUTINE Vatt_bis2(a,Va)
implicit REAL(8) (a-h,o-z)
implicit INTEGER*4 (i-n)
REAL(8), INTENT(IN) :: a
REAL(8), INTENT(OUT) :: Va(3)
REAL(8) :: B_par(3), beta_par(3)
data B_par(1:3)  /1.984903756490406d3, 2.81460945880185d0, 4.92107577361796d0/
data beta_par(1:3) /47.204523127d0, 14.332132499d0, 13.826912506d0/
!data B_par(1:3)  /0.43460221973329d3, 1.0579560223603662d1, 4.829005919247588d0/
!data beta_par(1:3) /36.7942973600d0, 17.8128093260d0, 29.6342372285d0/


Va=0.d0

do i=1,3
  aux1=B_par(i) * exp(-beta_par(i)*a)
  Va(1)=Va(1) + aux1
  Va(2)=Va(2) - aux1 * beta_par(i)
  Va(3)=Va(3) + aux1 * beta_par(i) * beta_par(i)
enddo

END SUBROUTINE Vatt_bis2
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
SUBROUTINE Gang_bis2(theta,Ga)
implicit REAL(8) (a-h,o-z)
implicit INTEGER*4 (i-n)
DIMENSION :: theta(3)
DIMENSION :: Ga(3,3)
DIMENSION :: dat1(6),dat2(6), G_spline(3), dat(6)
! We have two intervals, [c1,c2]=[cos pi, cos 2pi/3] and [c2,c3]=[cos 2pi/3, cos 0.6082pi]
!   beyond which the potential is not implemented
! The model parameters (first and second derivatives) have been scaled 
!   for a quintic spline between 0 and 1
! The order is 1,2: function at borns of interval, 3,4: first der., 5,6: second der.
!data (dat1(1)=-0.001d0,dat1(2)=0.0528d0,dat1(3)=0.052d0,dat1(4)=0.085d0,dat1(5)=0.d0,dat1(6)=0.0925d0)
!data (dat2(1)=0.0528d0,dat2(2)=0.09733d0,dat2(3)=0.02831996387327d0,dat2(4)=0.06663520911358d0 &
!       ,dat2(5)=0.01026808065397d0,dat2(6)=0.05494810728343d0)
data dat1(1:6)  /-0.001d0, 0.0528d0, 0.104d0, 0.17d0, 0.d0, 0.37d0/
data dat2(1:6)  /0.0528d0, 0.09733d0, 0.17d0, 0.4d0, 0.37d0, 1.98d0/
data c1, c2, c3 /-1.d0, -.5d0, -.33341197721605d0/

dcos1=c2-c1
dcos2=c3-c2

do i=1,3
  aux1=dcos(theta(i))
  if ((aux1.gt.c1).and.(aux1.le.c2)) then
    aux2=(aux1-c1)/dcos1
    dat(:)=dat1(:)
    dat(3:6)=dat(3:6)*dcos1
    dat(5:6)=dat(5:6)*dcos1
	call spline_bis(dat,aux2,G_spline)
    Ga(1,i)=G_spline(1)
    ! G_spline(2) is the derivative of G_spline(1) wrt xi\in[0,1]
    fact=(-1.d0*dsin(theta(i))) /dcos1
    Ga(2,i)=G_spline(2) * fact
!    Ga(3,i)=G_spline(3) * fact * fact + G_spline(2) * (-1.d0*dcos(theta(i))) /dcos1
    Ga(3,i)=G_spline(3) * fact * fact + G_spline(2) * (-1.d0*aux1) /dcos1
  else  if ((aux1.gt.c2).and.(aux1.lt.c3)) then
    aux2=(aux1-c2)/dcos2
    dat(:)=dat2(:)
    dat(3:6)=dat(3:6)*dcos2
    dat(5:6)=dat(5:6)*dcos2
	call spline_bis(dat,aux2,G_spline)
    Ga(1,i)=G_spline(1)
	fact=(-1.d0*dsin(theta(i))) /dcos2
    Ga(2,i)=G_spline(2) * fact
    Ga(3,i)=G_spline(3) * fact * fact + G_spline(2) * (-1.d0*aux1) /dcos2
  else  if ((aux1.gt.c3).and.(aux1.le.1.d0)) then
    x=aux1
    x2=x*x
    x3=x*x2
    x4=x2*x2
    x5=x4*x
    Ga(1,i)=-.04005399574455538497890162d0*x5 + 1.272040452823819270894289d0*x4 &
  	        -.5596772082312575809128441d0*x3  - .4330817380737560058603059d0*x2 &
		   +.4889164892257497008577588d0*x  + .2718560000000000000000001d0
    temp1=-5.d0*.04005399574455538497890162d0*x4 + 4.d0*1.272040452823819270894289d0*x3 &
	       -3.d0*.5596772082312575809128441d0*x2 - 2.d0*.4330817380737560058603059d0*x  &
		   +.4889164892257497008577588d0
    temp2=-20.d0*.04005399574455538497890162d0*x3 + 12.d0*1.272040452823819270894289d0*x2 &
	       -6.d0*.5596772082312575809128441d0*x - 2.d0*.4330817380737560058603059d0

    fact=(-1.d0*dsin(theta(i))) 
    Ga(2,i)=temp1 * fact
    Ga(3,i)=temp2 * fact*fact + temp1 * (-1.d0*dcos(theta(i)))
  else
    write(*,*) theta(i)*180/3.14159
    STOP ' Angle out of range of BRENNER 2'
  endif
end do
END SUBROUTINE Gang_bis2

SUBROUTINE spline_bis(dat,x,G)
implicit REAL(8) (a-h,o-z)
implicit INTEGER*4 (i-n)
REAL*8 dat(6), G(3)
REAL*8 funcs(6), dfuncs(6), ddfuncs(6)
integer*4 i

x2=x*x
x3=x*x2
x4=x2*x2
x5=x4*x

funcs(1) = -6.D0*x5+15.D0*x4-10.D0*x3+1.D0
funcs(2) = 6.D0*x5-15.D0*x4+10.D0*x3
funcs(3) = -3.D0*x5+8.D0*x4-6.D0*x3+x
funcs(4) = -3.D0*x5+7.D0*x4-4.D0*x3
funcs(5) = -x5/2.D0+3.D0/2.D0*x4-3.D0/2.D0*x3+x2/2.D0
funcs(6) = x5/2.D0-x4+x3/2.D0

dfuncs(1) = -30.D0*x4+60.D0*x3-30.D0*x2
dfuncs(2) = 30.D0*x4-60.D0*x3+30.D0*x2
dfuncs(3) = -15.D0*x4+32.D0*x3-18.D0*x2+1.D0
dfuncs(4) = -15.D0*x4+28.D0*x3-12.D0*x2
dfuncs(5) = -5.D0/2.D0*x4+6.D0*x3-9.D0/2.D0*x2+x
dfuncs(6) = 5.D0/2.D0*x4-4.D0*x3+3.D0/2.D0*x2

ddfuncs(1) = -120.D0*x3+180.D0*x2-60.D0*x
ddfuncs(2) = 120.D0*x3-180.D0*x2+60.D0*x
ddfuncs(3) = -60.D0*x3+96.D0*x2-36.D0*x
ddfuncs(4) = -60.D0*x3+84.D0*x2-24.D0*x
ddfuncs(5) = -10.D0*x3+18.D0*x2-9.D0*x+1.D0
ddfuncs(6) = 10.D0*x3-12.D0*x2+3.D0*x


G = 0.D0
do i = 1,6
   G(1) = G(1) +  funcs(i)*dat(i)
   G(2) = G(2) + dfuncs(i)*dat(i)
   G(3) = G(3) +ddfuncs(i)*dat(i)
enddo
END SUBROUTINE spline_bis
