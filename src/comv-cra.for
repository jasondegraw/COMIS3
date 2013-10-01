C+*********************************************************** comv-cra.f
Ch***********************************************************************
C
      SUBROUTINE CRACK(Fma,DFma,Tl,Dp,linInit,Cm,Expn,Diflim,Lstyp,
     &  Lstat,TempL1,TempL2,RhoL1,RhoL2,NormCrRho,
     &  NormCrMu,Crlen,Wthick,Uval)
C
C************************************************************************
C Purpose: Crack flow equation routine for cracks as well as closed
C   windows.This part has been adapted to Liu Mingsheng's routine.
C   See COMIS Fundamentals
C
C Module:  empa vd 1991Sep13
C Changes:      vd 1992Feb13
C@lbl dml 1999nov19 Change integer variable initFlg to logical linInit.
C@NBI PGS 2000Jul17 - Note that Fma must have a viable value before calling
C@NBI                 this routine (can be zero), because its initial value
C@NBI                 is an input argument for routine CRTEMPC at the
C@NBI                 beginning of this routine.
C
C Pass parameters:
C IO  Name      Units       Description
C IO  Fma       kg/s        Mass flow through the link
C  O  DFma      kg/s/Pa     Derivative d Fma/d Dp
C  O  Tl        C           Mean air temperature in the link
C I   Dp        Pa          Pressure difference accross the link
C I   linInit   -           Linear initialisation flag
C I   Cm        kg/sec@1pa  Crack flow coefficient
C I   Expn      -           Crack flow exponent
C I   Lstyp     -           Crack Type (See Comis Fundamentals)
C                            1: Double frame window
C                            2: Single frame window
C                            3: Wall crack
C I   Lstat     -           Link status
C I   TempL1    C           Link temperature from
C I   TempL2    C           Link temperature to
C I   NormCrRho kg/m^3      Crack normrho
C I   NormCrMu              Crack normmu
C I   Crlen     m           Crack length
C I   Wthick    m           Thickness of the wall
C I   Uval      W/m^2/K     Heat transfer coefficient of the wall
C
Ch**********************************************************************

      IMPLICIT NONE
C vent2 added for output use in Vent2Out
      real FvVeloc
      COMMON /vent2/ FvVeloc

C     ! Input arguments.
      real DFma,Tl,Cm,Expn,Diflim
      real TempL1,TempL2,RhoL1,RhoL2
      real NormCrRho,NormCrMu,Crlen,Wthick,Uval
      double precision Dp, Fma
      integer Lstyp,Lstat
      logical linInit

C     ! Local variables.
      real Ctl
      double precision flow
C-----
      CALL CRTEMPC(Ctl,Tl,Lstyp,Lstat,TempL1,TempL2,
     &  RhoL1,RhoL2,NormCrRho,NormCrMu,Crlen,
     &  Wthick,Uval,Fma,Expn)

      if( linInit ) then
C        !linear initialization
         Fma=CtL*Cm*dp
         DFma=CtL*Cm
      else
C        ! powerlaw
         if (ABS(dp).GT.DifLim) then
           if (Expn .LE. 0.5) then
             if (dp.GT.0.0D0) then
               Fma=CtL*Cm*SQRT(ABS(Dp))
             else
               Fma=-CtL*Cm*SQRT(ABS(Dp))
             endif
             DFma=Fma*0.5/Dp
           else
             if (dp.GT.0.0D0) then
               Fma=CtL*Cm*(ABS(Dp))**Expn
             else
               Fma=-CtL*Cm*(ABS(Dp))**Expn
             endif
             DFma=Fma*Expn/Dp
           endif
         else
C        ! the flow at DifLim is:
           if (Expn .LE. 0.5) then
             flow=CtL*Cm*sqrt(DifLim)
           else
             flow=CtL*Cm*(DifLim)**Expn
           endif
           if (dp.NE.0.0) then
             Fma=flow*dp/DifLim
           else
             Fma=0.0
           endif
           DFma=flow/DifLim
         endif
      endif

      if (Cm .gt. 0.0) then
C        ! Velocity= Fva/Nett area=Fma/Rho/(Cm/( (2**N)* sqrt(1.2) ) )
         FvVeloc=Fma*(2**ExpN)*SQRT(1.2)/(FvVeloc*Cm)
      endif

      END

Ch***********************************************************************
      SUBROUTINE CRTEMPC(Ctl,Tl,Lstyp,Lstat,TempL1,TempL2,
     &  RhoL1,RhoL2,NormCrRho,NormCrMu,Crlen,Wthick,Uval,
     &  Fma,Expn)

C***********************************************************************
C
C       Purpose:        This SUBROUTINE calculates the temperature corection
C             factor for crack flow. The numbers in the comment
C             columns are referenced to the eqations in the
C             COMIS Fundamentals.
C
C       Module:         empa aw 1991dec5
C
C       Changes:
C
C       Limits:         Only suitable to cracks.
C
C Pass parameters:
C IO  Name      units       Description
C  O  Ctl       -           Correction factor
C  O  Tl        C           Mean air temperature in the link
C I   Lstyp     -           Type of component structure
C I   Lstat     -           Link status
C I   TempL1    C           Link temperature from
C I   TempL1    C           Link temperature to
C I   NormCrRho kg/m^3      Crack normrho
C I   NormCrMu              Crack normmu
C I   Crlen     m           Crack length
C I   Wthick    m           Thickness of the wall
C I   Uval      W/m^2/K     Heat transfer coefficient of the wall
C I   Fma       kg/h        Mass flow rate
C I   Expn                  Exponent of powerlaw
C
Ch***********************************************************************

      IMPLICIT NONE
      INCLUDE 'comv-phy.inc'

C vent2 added for output use in Vent2Out
      real FvVeloc
      common /vent2/ FvVeloc

C     ! Input arguments.
      real Ctl,Tl,TempL1,TempL2,RhoL1,RhoL2
      real NormCrRho,NormCrMu,Crlen,Wthick,Uval,expn
      double precision  Fma
      integer Lstyp,Lstat

C     ! Local variables.
      real fmaloc,ti,to,dt,fh,twi,two,tw0,twX,tl0
      real tl1,tl2,tl3,rho,mu,alpwali,alpwalo
C     ! fmaloc      local variable for massflow
C     ! ti  C  inside airtemperature
C     ! to  C  outside airtemperature
C     ! dt  K   temperature difference between inside and outside air
C     ! twi C  inside walltemperature
C     ! two C  outside walltemperature
C     ! tw0 C  walltemperature at the enterance of the air
C     ! twX C  walltemperature at the outlet of the air
C     ! tl0 C  airtemperature at the enterance
C     ! rho kg/m^3  rho of the leakage air
C     ! mu      kinetic viscosity of the leakage air
C     ! fh,tl1,tl2,tl3  div. intermediate results
C     !   Local variables for material properties (perhaps they should be
C     ! parameters):
      real alpha
C     ! cv      J/kg/K  heat capacity of air
C     ! ki      W/m^2/K heat transfer coefficient of the prime pane
C     ! ko      W/m^2/K heat transfer coefficient of the storme pane
C     ! alpwini W/m^2/K inside convective heat exchange coeff. of the window
C     ! alpwino W/m^2/K outside convective heat exchange coeff. of the window
C     ! alpwali W/m^2/K inside convective heat exchange coeff. of the wall
C     ! alpwala W/m^2/K outside convective heat exchange coeff. of the wall
C     ! alpha   W/m^2/K convective heat exchange coefficient in the crack
C     ! Default values:  (some are just assumptions)
C@tno jcp 1996Jul24_13:40:15 CpAir used
CC    parameter(cv=1005)
      parameter(alpwali=8,alpwalo=23,alpha=10)
      real muf

      fmaloc=Fma
      if ((Lstyp.EQ.0).OR.(Wthick.EQ.0.0))then
         Tl=(TempL1+TempL2)/2
         Ctl=1.0
         GOTO 100
      endif

      if (fma.eq.0.0) then
         Tl=(TempL1+TempL2)/2
         GOTO 100
      endif

      if ((Lstat.eq.3).or.(Lstat.eq.6)) then
C        ! From is a zone that means it is inside
         ti=TempL1
         to=TempL2
C        ! As infiltration (outside to inside) is defined as positive
C        ! flow, the sign of fmaloc must be the oposite.
         fmaloc=-fmaloc
      else if ((Lstat.eq.1).or.(Lstat.eq.2)) then
C        ! To is zone that means it is inside
         ti=TempL2
         to=TempL1
      else
C        ! No formula for crack temperature is defined yet for this case
C        ! I take the warmer zone as inside.
         if (TempL2.gt.TempL1) then
           ti=TempL2
           to=TempL1
         else
           ti=TempL1
           to=TempL2
C          ! As infiltration (outside to inside) is defined as
C          ! positive flow, the sign of fma must be the oposite.
           fmaloc=-fmaloc
         endif
      endif

      dt=ti-to
      if (dt.eq.0) then
         Tl=ti
      else
         GOTO(10,20,30),Lstyp
10       CONTINUE
C        ! for double window
C        ! The equations for double pane windows in the fundamentals
C        ! don't seem to be proved enough. Here they are overjumped and
C        ! tl is set to the average of TempL1 and TempL2.
         Tl=(TempL1+TempL2)/2
         GOTO 100
20       CONTINUE
C        ! For single window and door
         if (fmaloc.GT.0) then
C          ! Infiltration (eqn 37):
           Tl=to+0.1363*dt
         else
C          ! Exfiltration (eqn 38):
           Tl=ti-0.3636*dt
         endif
         GOTO 100
30       CONTINUE
C        ! for wall
         two=to+Uval*dt/alpwalo
         twi=ti-Uval*dt/alpwali
         if (fmaloc.GT.0.0) then
C          ! Infiltration:
           tl0=to
           tw0=two
           twX=twi
         else
C          ! Exfiltration:
           tl0=ti
           tw0=twi
           twX=two
         endif
C@tno jcp 1996Jul24_13:40:38 Cpair used
CC       fh=abs(fma)/Crlen*cv/alpha/Wthick
         fh=abs(fma)/Crlen*CpAir/alpha/Wthick
         tl1=fh*((tl0-tw0)+fh*(twX-tw0))
         tl2=tl1*exp(-1/fh)
         tl3=(twX-tw0)*(0.5-fh)
C        ! (eqn 50):
         Tl=tw0+tl1-tl2+tl3
      endif

100   CONTINUE

C     ! Calculate Ctl.
      if (Fma.gt.0)then
         Rho=RhoL1*(TempL1+Tzero)/(Tl+Tzero)
      else
         Rho=RhoL2*(TempL2+Tzero)/(Tl+Tzero)
      endif
      mu=muf(Tl)
C     ! (eqn 32.2):
      ctl=(NormCrRho/rho)**(expn-1)*(NormCrMu/mu)**(2*expn-1)

C     ! Assign FvVeloc with rho
      FvVeloc=rho

      RETURN
      END
