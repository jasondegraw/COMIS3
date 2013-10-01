C+*********************************************************** comv-fe8.f
C@tno jcp 1997Jul03_15:50:37 new file comv-fe8.f
C@tno jcp 1997Nov12_11:55:00 reconstructed comment that fell off position 80
C this file is for the catmull interpolation of fan datapoints
C
C for a certain Pressure x the procedure search gets 4 appropriate points
C (x4,y4)
C these points are fed into catmull that delivers the smoothed curve point
C (px,py)
C this is done by an interpolator t
C the curve px are within x4(2) and x4(3) as t moves from 0 to 1
C
C  at the beginning of the data: search returns x4(1) y4(1) as an extended point
C  at the end       of the data: search returns x4(4) y4(4) as an extended point
C outside the range of x data, a linear inter/extrapolation is done on these
C extra points
C
C
C Catmull moves both x and y a bit. That means that the x-value returned by
C Catmull is not the value you wanted.
C (a small loop  and call to RTsafe fixes that.
C
C at the proper point (t that delivers the wanted x)
C we can calculate dx/dt and dy/dt and so: dy/dx


Ch**********************************************************************
C
        SUBROUTINE FanCat(Fma,DFma,Dp,Dat)
C pass parameter #        1   2    3  4
C**********************************************************************
C
C Purpose: This is the routine that interpolates the fan data points
C          It is called from comv-feq.f SUBROUTINE Fan( ) if the flag Dat(2)=3
C
C
C Module:  tno jcp 1997July03
C Changes:

C Pass parameters:
C IO # Name    unit name  description
C O  1 Fma     (kg/s)	  mass flow through the link
C O  2 DFma    (kg/s/Pa)  derivative d Fma/d Dp
C I  3 Dp      (Pa)	  pressure difference accross the link
C I  4 DAT(*)  multi	  data about the duct
C
C Contents of the array Dat(*):
C Dat(1)=2 means this link is a fan
C Dat(2)= Flag 1:use C0..C5 3:use datapoints and catmul interpolation
C Dat(3)= highest power in the polynomial, n
C Dat(4)= airdensity belonging to the fandata which gave these coefficients
C Dat(5)= fanspeed belonging to the fandata which gave these coefficients
C Dat(6)= Cm if the fan is off
C Dat(7)= Expn if the fan is off
C Dat(8)= Pmin range where the polynomial is valid
C Dat(9)= Pmax	,,    ,,
C Dat(10)= slope for extrapolation
C Dat(11)= intercept for extrapolation
C Dat(12)= C0 polynomial coefficient
C Dat(13)= C1......
C Dat(12+Dat(3))=Cn

C@tno jcp 1997Jul03_14:40:22
C Dat(17)=C5
C Dat(18)=Number of data points say 4 here
C Dat(19)=P1
C Dat(20)=Q1
C Dat(21)=P2
C Dat(22)=Q2
C Dat(23)=P3
C Dat(24)=Q3
C Dat(25)=P4
C Dat(26)=Q4
C Dat(27)=Filter coef pol1
C Dat(28)=Filter coef pol2
C Dat(29)=Filter coef pol3
C Dat(30)=Filter coef pol4
C Dat(31)=Filter coef pol5
C
Ch**********************************************************************
        IMPLICIT NONE
        include 'comv-uni.inc'


        REAL      Dat(*), DFma
C@empa aw 1997sep18 RTSAFE
        DOUBLE PRECISION Dp, Fma, RTSAFE
C@lbl bvs 1997Nov19 declare type for catmul
	REAL Catmul
        real x4(4),y4(4),m,x3,eps
        double precision x0,t,dum,dxdt,dydt
        integer npoints

        npoints=dat(18)

c        write(cof,*) 'FanCat npoints=',npoints
        If (nPoints .gt. 1) Then
c          write(cof,*) 'FanCat dp =',dp
c          write(cof,*) 'FanCat dat(19)=',dat(19)
c          write(cof,*) 'FanCat dat(last)=',dat(19+(npoints-1)*2)
          x0 = dp
          eps=1.E-8*x0
C@tno jcp 1997Nov12_12:00:27 eps > 0 (6 added lines)
          If (eps.lt. 0.D00) then
            eps=-eps
          end if
          If (eps.eq. 0.D00) then
            eps=1.E-8
          end if


c          write(cof,*) 'FanCat dp=x0 =',x0
          Call search(x0, x4, y4,DAT,npoints)
c          write(cof,*) 'FanCat search y4=',y4(1),y4(2),y4(3),y4(4)

          If ((dp .ge. Dat(19)).and.
     &       ( dp .lt. Dat(19+(nPoints-1)*2))) Then


            if (Abs(x0-x4(2)).le.eps) then
             t=0.0D0
            else if (Abs(x0-x4(3)).le.eps) then
             t=1.0D0
            else
C@empa aw 1997sep18 third argument must be a DOUBLE
             t = RTSAFE(0.0D0, 1.0D0, DBLE(eps*1.D-3),x4,x0)
            end if
c            write(cof,*) 'FanCat RTsafe t=',t
c            if (1.eq.0) then
            x3 = catmul(t, x4)
c            write(cof,*) 'FanCat catmul(t) x3=',x3
            Call funcd(t, dum, dxdt,x4,0.0D0)
c            write(cof,*) 'FanCat catmul(t) x=',dum
c            write(cof,*) 'FanCat catmul(t) dx/dt=',dxdt
            Call funcd(t, dum, dydt,y4,0.0D0)
            fma=dum
c            write(cof,*) 'FanCat catmul(t) y=',fma
c            write(cof,*) 'FanCat catmul(t) dy/dt=',dydt

            If (dxdt.ne. 0) Then
              dfma = dydt / dxdt
c              write(cof,*) 'FanCat catmul(t) dfma=',dfma
            Else
c              write(cof,*) 'Catmull FA dfma, dy/dx=infinite'
            End If
c debug if 1=0
c            end if
          else
c pressure outside the catmull rom interpolation
            If (dp .lt. Dat(19)) then
c below the first data point use x4(1)..x4(2)
              m=(y4(2)-y4(1))/(x4(2)-x4(1))
c              write(cof,*) 'linear part below pmin'
c              write(cof,*) 'Catmull FA x1',x4(1)
c              write(cof,*) 'Catmull FA x2',x4(2)
c              write(cof,*) 'Catmull FA y1',y4(1)
c              write(cof,*) 'Catmull FA y2',y4(2)
c              write(cof,*) 'Catmull FA m',m
c              write(cof,*) 'Catmull FA m',m
c              write(cof,*) 'Catmull FA m',m
              fma=y4(1)+m*(dp-x4(1))
c              write(cof,*) 'Catmull FA fma',fma
              dfma=m
            else
c above the last data point use x4(3)..x4(4)
              m=(y4(4)-y4(3))/(x4(4)-x4(3))
              fma=y4(4)+m*(dp-x4(4))
              dfma=m

            end if
          End If
        End If
        RETURN
	END


Ch**********************************************************************
C
        SUBROUTINE search(x0, x4, y4, DAT,npoints)
C pass parameter #        1   2    3  4
C**********************************************************************
C
C Purpose: Search returns 4 successive points from the fan data points
C          pressure x0 will be in the interval x4(2) x4(3)
C
C Module:  tno jcp 1997July03
C Changes:

C Pass parameters:
C IO # Name    unit name  description
C I  1 x0      (Pa)       pressure across the link
C O  2 x4(4)   (Pa)       4 datapoints pressuredifference
C O  3 y4(4)   (kg/s)     4 datapoints flowrate
C I  4 DAT     (Pa,kg/s,..)  fan data
C I  4 npoints (-)        number of datapoints
Ch**********************************************************************
        IMPLICIT NONE
        include 'comv-uni.inc'
        double precision x0
        REAL   X4(*),Y4(*),Dat(*)
        integer npoints

        integer i,j,k,correct1,correct4
C@lbl bvs 1997Dec11 declare type for catmul
	REAL Catmul

        j = 0
10      continue
          j = j + 1
        if ((j.LE.npoints).and.(x0 .ge. DAT(19+(j-1)*2))) goto 10

        j = j - 1
c        write(cof,*) 'j=',j

        If (j .lt. 1) Then
          j = 1
        end if

c        write(cof,*) 'j=',j

        If (nPoints.eq.1) Then
          do 11  i = 1,4
            x4(i) = DAT(19)
            y4(i) = DAT(20)
11        continue
        Else
          correct1 = 0
          correct4 = 0
          do 20 i = 1,4
            k = j - 2 + i
            If (k .lt. 1) Then
C point before the first data, move to the left
              k = 1
              correct1 = 1
            Else
              If (k .gt. nPoints) Then
C point beyond the last datapoint, move to the right
                k = nPoints
                correct4 = 1
              end if
            End If

            x4(i) = dat(19+(k-1)*2)
            y4(i) = dat(20+(k-1)*2)
c            write(cof,*)'x4(',i,')=',x4(i)
20        continue
C Next i

          If (correct1 .eq.1) Then
C correct point 1
            x4(1) = 2.0 * x4(1) - catmul(0.15D0, x4)
c            write(cof,*)'correct x4(',1,')=',x4(1)
            y4(1) = 2.0 * y4(1) - catmul(0.15D0, y4)
          End If

          If (correct4.eq.1) Then
C correct point 4
            x4(4) = 2.0 * x4(4) - catmul(0.85D0, x4)
c            write(cof,*)'correct x4(',4,')=',x4(4)
            y4(4) = 2.0 * y4(4) - catmul(0.85D0, y4)
          End If
        End If
        RETURN
        End

Ch**********************************************************************
C
C@empa aw 1997sep18 REAL
CC        Double precision FUNCTION Catmul(t,x)
	REAL FUNCTION Catmul(t,x)

C pass parameter #        1   2
C**********************************************************************
C
C Purpose: return an interpolated value from X(1..4)
C          t=0 then Catmul=X(2)
C          t=1 then Catmul=X(3)
C
C Module:  tno jcp 1997July03
C Changes:

C Pass parameters:
C IO # Name    unit name  description
C I  1 t       (-)        0...1
C O  2 x(4)    (Pa,kg/s)  4 datapoints pressuredifference or flowrate
Ch**********************************************************************
        IMPLICIT NONE
       double precision t
       Real X(*)
       double precision a,b,c,d
       a = (-X(1) + 3.0D0 * X(2) - 3.0D0 * X(3) + X(4))
       b = (2.0D0 * X(1) - 5.0D0 * X(2) + 4.0D0 * X(3) - X(4))
       c = (-X(1) + X(3))
       d = 2.0D0 * X(2)

       catmul = 0.5D0 * (((t * a+ b)*t + c)*t + d)
       RETURN
       End



Ch**********************************************************************
C
        SUBROUTINE funcd(t, f, f1,x4,x0)
C pass parameter #        1   2
C**********************************************************************
C
C Purpose: similar to catmull but calculate f1=dx/dt too
C          f is the interpolated value
C
C Module:  tno jcp 1997July03
C Changes:

C Pass parameters:
C IO # Name    unit name  description
C I  1 t       (-)        0...1
C O  2 f                  value of the interpolation in x4
C O  3 f1                 dx/dt
C I  4 x(4)    (Pa)       4 datapoints pressuredifference or flowrate
C I  5 x0      (Pa) x0 that has to be found by RTsafe,if call for dy/dt pass 0.0
Ch**********************************************************************
        IMPLICIT NONE
       Double precision t,f,f1,x0
       Real X4(*)
       double precision a,b,c,d
       a = (-x4(1) + 3 * x4(2) - 3 * x4(3) + x4(4))
       b = (2 * x4(1) - 5 * x4(2) + 4 * x4(3) - x4(4))
       c = (-x4(1) + x4(3))
       d = 2 * x4(2)

       f = 0.5D0 * (((t * a + b)*t + c)*t + d) - x0
       f1 = 0.5D0 * ((3 * a * t + 2 * b )*t + c)
       RETURN
       End



Ch**********************************************************************
      Double Precision FUNCTION RTSAFE(X1,X2,XACC,X4,x0)
Ch**********************************************************************
        IMPLICIT NONE
      Double precision x1,x2,xacc,x0
      real x4(*)
      double precision f,fl,fh,df,xl,xh,swap,dxold,dx,temp
C@lbl bvs 1997Dec11 declare maxit and j
      integer maxit,j
C copied from numerical recipies
      PARAMETER (MAXIT=100)

        INCLUDE 'comv-uni.inc'

      CALL FUNCD(X1,FL,DF,x4,x0)
      CALL FUNCD(X2,FH,DF,x4,x0)
c      if (1.eq.0) then

      IF(FL*FH.GE.0.0) then
C@tno jcp 1997Nov12_12:03:41 mistyped RTSafe
        write(cof,*) 'FAn RTSafe root must be bracketed'
      end if

      IF(FL.LT.0.0)THEN
        XL=X1
        XH=X2
      ELSE
        XH=X1
        XL=X2
        SWAP=FL
        FL=FH
        FH=SWAP
      ENDIF

      RTSAFE=0.5D0*(X1+X2)
      DXOLD=ABS(X2-X1)
      DX=DXOLD
      CALL FUNCD(RTSAFE,F,DF,x4,x0)
      DO 11 J=1,MAXIT
        IF(((RTSAFE-XH)*DF-F)*((RTSAFE-XL)*DF-F).GE.0.
     *      .OR. ABS(2.0*F).GT.ABS(DXOLD*DF) ) THEN
          DXOLD=DX
          DX=0.5*(XH-XL)
          RTSAFE=XL+DX
          IF(XL.EQ.RTSAFE)RETURN
        ELSE
          DXOLD=DX
          DX=F/DF
          TEMP=RTSAFE
          RTSAFE=RTSAFE-DX
          IF(TEMP.EQ.RTSAFE)RETURN
        ENDIF
        IF(ABS(DX).LT.XACC) RETURN
        CALL FUNCD(RTSAFE,F,DF,x4,x0)
        IF(F.LT.0.0) THEN
          XL=RTSAFE
          FL=F
        ELSE
          XH=RTSAFE
          FH=F
        ENDIF
11    CONTINUE
c      if (1.eq.0) then
c      end if
      write(cof,*) 'FAn RTSAFE exceeding maximum iterations'
      RETURN

      END
