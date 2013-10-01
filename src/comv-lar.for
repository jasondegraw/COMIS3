C+*********************************************************** comv-lar.f
C@empa aw 1994oct26 New Large Vertical Opening routines: VerticOp, PresProfile
C@                  All old Large Opening routines canceled.
Ch***********************************************************************
      SUBROUTINE VerticOp(fma12,fma21,dp1fma12,dp1fma21,
     &          DpProfNew,RhoProfF,RhoProfT,CFact,Dat,SLinkDat,
     &          DifLim)
C***********************************************************************
C
C       THIS ROUTINE IS CLOSELY RELATED TO THE ROUTINE FMAPROFCAL !
C
C Purpose:      This routine calculates the massflow and its derivative
C               through a large opening in both flow directions. As input
C               the density profiles RhoProfF/T are required aswell as the
C               effective pressure difference profile DpProfNew, which is the
C               sum of the stack pressure difference profile DpProf and the
C               difference of the actual pressures at reference height. The
C               profiles are calculated in the routine PresProfile.
C               The massflow and its derivative are calculated for each
C               interval representing a step of the pressure difference
C               profile. The total flow and derivative are obtained by
C               summation over the whole opening.
C               The calculation is split into different cases representing
C               different situations of the opening:
C               - closed opening (opening factor = 0): summation of top and
C                 bottom crack (crack length = lwmax) plus "integration" over
C                 a vertically distributed crack of length (2*lhmax+lextra).
C               - type 1: normal rectangular opening: "integration" over NrInt
C                 openings with width actlw and height actlh/NrInt
C               - type 2: horizontally pivoted window: flow direction assumed
C                 strictly perpendicular to the plane of the opening
C                 -> "integration" over normal rectangular openings at top
C                 and bottom of LO plus a rectangular opening in series with two
C                 triangular openings in the middle of the LO (most general
C                 situation). The geometry is defined by the input parameters
C                 actlw(=lwmax), actlh, axisheight, opening angle.
C               Assuming the massflow perpendicular to the opening plane in all
C               cases the ownheightfactor has no influence on the massflow.
C
C
C Module:
C
C Changes:
C@epma aw 1995may19 New definition for opening factors for LVO type 2:
C@                  opening angle = 90 degrees --> opening factor = 1.0
C@NBI PGS 1999Aug10 - Had to change name of variable 'Interval' to 'DeltaZ',
C@NBI                 to avoid confusion with the variable 'Interval' 
C@NBI                 (i.e. schedule deltaT) in the INCLUDE file.
C@empa aw 2005may31 LVO type 3: Alternative geometry for horizontal pivoting axis window
C@empa aw 2005may31 according to proposal from PGS.
C@empa aw 2005may31 LVO types 4 and 5: Triangles with vertex at the top (5) and at the bottom (4) 
C
C Pass Parameters:
C
C IO Name             Unit    Description
C  O fma12            kg/s    massflow in direction "from-to"
C  O fma21            kg/s    massflow in direction "to-from"
C  O dp1fma12         kg/s/Pa derivative d fma12 / d Dp
C  O dp1fma21         Kg/s/Pa derivative d fma21 / d Dp
C  I DpProfNew(NrInt+2) Pa     Differential pressure profile for Large
C                             Openings, taking into account fixed pressures
C                             and actual zone pressures at reference height
C  I RhoProfF(NrInt+2) Kg/m3   Density profile in FROM zone
C  I RhoProfT(NrInt+2) Kg/m3   Density profile in TO zone
C@epma aw 1995may19 New definition for opening factors for LVO type 2:
C@                  opening angle = 90 degrees --> opening factor = 1.0
C  I Cfact            -       Actual opening factor
C  I Dat(*)           multi   Characteristic parameters for the opening
C  I SLinkDat(*)      multi   Actual parameters for the opening
C  I DifLim          Pa       Limit for the pressure difference where
C                             laminarization takes place
C
C Local variables (many extracted from Dat and SLinkDat):
C
C    ActCD     [-]          Actual value for discharge coefficient.
C    ActLh     [m]          Actual height of opening.
C    ActLw     [m]          Actual width of opening.
C    alpha     [-]          Open fraction, in the range 0.0 to 1.0.
C    Area      [m²]         A or Cd·A for actual window opening size.
C    Axishght  [m]          Axis height of horizontal pivot window.
C    Cs        [kg/s/Pa**n] Massflow coefficient for crack in case of
C                           closed opening.
C    deltaZ    [-]          Interval (distance) between two evaluation points.
C    DpZeroOffset           Actual limit for the pressure difference
C                           where laminarization takes place.
C    dZ42      [m]          The part of a detlaZ strip that is blocked off by window.
C    evalHght  [m]          Height of evaluation plane of pressure and
C                           density profile (midheight of a deltaZ strip).
C    Expn      [-]          Mass flow exponent for crack in case of
C                           closed opening.
C    fmasum,dfmasum         Massflow through actual interval and derivative.
C    h2,h4     [m]          Only for LVO2: Height from bottom frame to
C                           bottom and top of open window.
C    Lextra    [m]          Extra crack length for closed opening.
C    o2,o4     [m]          Only for LVO2: Narrowest opening width at bottom
C                           and top of window: Perpendicular distance from
C                           glass to horizontal window frame.
C    p2,p4     [m]          Only for LVO2: horizontal width of the botton and
C                           top open trianges at each side of the open window.
C    Prefact   [-]          Mass flow coefficient.
C    rholink   [kg/m³]      Density of air in the link.
C    Type      [-]          Opening type.

Ch***********************************************************************

        IMPLICIT NONE
        INCLUDE 'comv-par.inc'

C Passparameters:

        DOUBLE PRECISION  fma12,fma21
        DOUBLE PRECISION  dp1fma12,dp1fma21
        DOUBLE PRECISION  DpProfNew(NrInt+2)
        REAL RhoProfF(NrInt+2),RhoProfT(NrInt+2)
        REAL Cfact,Dat(*),SLinkDat(*)
        REAL DifLim

C Local variables:

        REAL ActLh,ActLw,Lextra,Axishght,ActCD,Cs,expn,Type
        REAL deltaZ,fmasum,dfmasum,Prefact,EvalHghts(NrInt+2)
        REAL h2,h4,alpha,rholink,c1,c2
C@tno jcp 1996Jul24_14:13:53 area added
        REAL DpZeroOffset,area

C@empa aw 2005may31 new local variables
        REAL l2,l4,o2,o4,p2,p4,sinAlpha,cosAlpha,dZ42,evalhght
        DOUBLE PRECISION  fma12o,fma21o
        DOUBLE PRECISION  dp1fma12o,dp1fma21o


        INTEGER i
C@tno jcp 1996Jun12_11:53:26 smuggled in FvVeloc in Large openings
        REAL FvVeloc
        common /vent2/ FvVeloc

C Extracting variables from Dat and SLinkDat
        ActLw=SLinkDat(2)
        ActLh=Slinkdat(3)
        ActCD=Slinkdat(4)
        Cs=Dat(3)
        Expn=Dat(4)
        Type=Dat(5)
C@empa aw 2005jun01 types 3,4,5
        IF ((Type.EQ.1).OR.(Type.EQ.4).OR.(Type.EQ.5)) THEN
           Lextra=Dat(8)
           AxisHght=0
        ELSE IF ((Type.EQ.2).OR.(Type.EQ.3)) THEN
           Lextra=0
           AxisHght=Dat(8)
        ENDIF


C Initialization:

        deltaZ=ActLh/NrInt
        fma12=0
        fma21=0
        dp1fma12=0
        dp1fma21=0


C Closed LO

        IF (Cfact.EQ.0) THEN

        DpZeroOffset=DifLim

C       bottom crack
C@empa aw 2005may31 bottom and top crack only for types 1 to 3
        IF (TYPE.LE.3)THEN
          IF (DpProfNew(1).GT.0) THEN
            IF (Abs(DpProfNew(1)).LE.DpZeroOffset) THEN
              dfmasum=Cs*ActLw*DpZeroOffset**expn/DpZeroOffset
              fmasum=DpProfNew(1)*dfmasum
            ELSE
              fmasum=Cs*ActLw*(DpProfNew(1))**expn
              dfmasum=fmasum*expn/DpProfNew(1)
            ENDIF
            fma12=fma12+fmasum
            dp1fma12=dp1fma12+dfmasum
          ELSE
            IF (Abs(DpProfNew(1)).LE.DpZeroOffset) THEN
              dfmasum=-Cs*ActLw*DpZeroOffset**expn/DpZeroOffset
              fmasum=DpProfNew(1)*dfmasum
            ELSE
              fmasum=Cs*ActLw*(-DpProfNew(1))**expn
              dfmasum=fmasum*expn/DpProfNew(1)
            ENDIF
            fma21=fma21+fmasum
            dp1fma21=dp1fma21+dfmasum
          ENDIF

C       top crack
          IF (DpProfNew(NrInt+2).GT.0) THEN
            IF (Abs(DpProfNew(NrInt+2)).LE.DpZeroOffset) THEN
              dfmasum=Cs*ActLw*DpZeroOffset**expn/DpZeroOffset
              fmasum=DpProfNew(NrInt+2)*dfmasum
            ELSE
              fmasum=Cs*ActLw*(DpProfNew(NrInt+2))**expn
              dfmasum=fmasum*expn/DpProfNew(NrInt+2)
            ENDIF
            fma12=fma12+fmasum
            dp1fma12=dp1fma12+dfmasum
          ELSE
            IF (Abs(DpProfNew(NrInt+2)).LE.DpZeroOffset) THEN
              dfmasum=-Cs*ActLw*DpZeroOffset**expn/DpZeroOffset
              fmasum=DpProfNew(NrInt+2)*dfmasum
            ELSE
              fmasum=Cs*ActLw*(-DpProfNew(NrInt+2))**expn
              dfmasum=fmasum*expn/DpProfNew(NrInt+2)
            ENDIF
            fma21=fma21+fmasum
            dp1fma21=dp1fma21+dfmasum
          ENDIF
        ENDIF

C       side and extra cracks
C@empa aw 2005may31 The total cracklength is only in lextra for Type 4 and 5. 
C@empa aw 2005may31 If not defined, a length of 1 is assumed 
        IF (TYPE.LE.3)THEN
          Prefact=deltaZ*(2+lextra/ActLh)*Cs
        ELSEIF ((TYPE.EQ.4).OR.(TYPE.EQ.5))THEN
          IF (lextra.lt.1e-10)lextra=1 
            Prefact=deltaZ*(lextra/ActLh)*Cs
        ENDIF
          DO 100 i=2,NrInt+1
            IF (DpProfNew(i).GT.0) THEN
              IF (Abs(DpProfNew(i)).LE.DpZeroOffset) THEN
                dfmasum=Prefact*DpZeroOffset**expn/DpZeroOffset
                fmasum=DpProfNew(i)*dfmasum
              ELSE
                fmasum=Prefact*(DpProfNew(i))**expn
                dfmasum=fmasum*expn/DpProfNew(i)
              ENDIF
              fma12=fma12+fmasum
              dp1fma12=dp1fma12+dfmasum

            ELSE
              IF (Abs(DpProfNew(i)).LE.DpZeroOffset) THEN
                dfmasum=-Prefact*DpZeroOffset**expn/DpZeroOffset
                fmasum=DpProfNew(i)*dfmasum
              ELSE
                fmasum=Prefact*(-DpProfNew(i))**expn
                dfmasum=fmasum*expn/DpProfNew(i)
              ENDIF
              fma21=fma21+fmasum
              dp1fma21=dp1fma21+dfmasum

            ENDIF
 100      CONTINUE

        ENDIF

C@empa aw 2005may31 calculate rectangle opening also as reference 
C@empa aw 2005may31 (totally opened window) for type 3  
C Open LO, type 1 and as reference for type 3

        IF ((Cfact.NE.0).AND.((type.EQ.1).OR.(type.EQ.3)))  THEN

          DpZeroOffset=DifLim*1E-3

          Prefact=ActLw*ActCd*deltaZ*SQRT(2.0)

          DO 200 i=2,NrInt+1
            IF (DpProfNew(i).GT.0) THEN
              IF (Abs(DpProfNew(i)).LE.DpZeroOffset) THEN
                dfmasum=SQRT(RhoProfF(i)*DpZeroOffset)/DpZeroOffset
                fmasum=DpProfNew(i)*dfmasum
              ELSE
                fmasum=SQRT(RhoProfF(i)*DpProfNew(i))
                dfmasum=0.5*fmasum/DpProfNew(i)
              ENDIF
              fma12=fma12+fmasum
              dp1fma12=dp1fma12+dfmasum
            ELSE
              IF (Abs(DpProfNew(i)).LE.DpZeroOffset) THEN
                dfmasum=-SQRT(RhoProfT(i)*DpZeroOffset)/DpZeroOffset
                fmasum=DpProfNew(i)*dfmasum
              ELSE
                fmasum=SQRT(-RhoProfT(i)*DpProfNew(i))
                dfmasum=0.5*fmasum/DpProfNew(i)
              ENDIF
              fma21=fma21+fmasum
              dp1fma21=dp1fma21+dfmasum
            ENDIF

 200      CONTINUE

          fma12=prefact*fma12
          fma21=prefact*fma21
          dp1fma12=prefact*dp1fma12
          dp1fma21=prefact*dp1fma21

        ENDIF


C Open LO, type 2

        IF ((Cfact.NE.0).AND.(type.EQ.2))  THEN

C       Initialization
          DpZeroOffset=DifLim*1E-3
C New definition for opening factors for LVO type 2:
C opening angle = 90 degrees --> opening factor = 1.0
          alpha=Cfact*3.14159/2
          h2=Axishght*(1-COS(alpha))
          h4=Axishght+(ActLh-Axishght)*COS(alpha)
          EvalHghts(1)=0
          EvalHghts(NrInt+2)=ActLh
C New definition for opening factors for LVO type 2:
C opening angle = 90 degrees --> opening factor = 1.0
          IF (Cfact.EQ.1.0) THEN
            h2=Axishght
            h4=Axishght
          ENDIF

          DO 300 i=2,NrInt+1
            EvalHghts(i)=deltaZ*(i-1.5)
 300      CONTINUE

C       Calculation of massflow and its derivative
          DO 400 i=2,NrInt+1

            IF (DpProfNew(i).GT.0) THEN
              rholink=RhoProfF(i)
            ELSE
              rholink=RhoProfT(i)
            ENDIF

            IF ((EvalHghts(i).LE.h2).OR.(EvalHghts(i).GE.h4)) THEN
C       rectangular opening on top and bottom of LO
              IF (Abs(DpProfNew(i)).LE.DpZeroOffset) THEN
                dfmasum=ActCd*ActLw*deltaZ*SQRT(2.0*rholink*
     &          DpZeroOffset)/DpZeroOffset*DSIGN(1d0,DpProfNew(i))
                fmasum=DpProfNew(i)*dfmasum
              ELSE
                fmasum=ActCd*ActLw*deltaZ*SQRT(2.0*rholink*
     &          ABS(DpProfNew(i)))
                dfmasum=0.5*fmasum/DpProfNew(i)
              ENDIF
            ELSE
C       triangular opening at the side of LO
              c1=ActCd*ActLw*deltaZ*SQRT(2.0*rholink)
              c2=2*ActCd*ABS((Axishght-Evalhghts(i)))*TAN(alpha)*
     &        deltaZ*SQRT(2.0*rholink)
              IF ((c1.NE.0).AND.(c2.NE.0)) THEN
                IF (Abs(DpProfNew(i)).LE.DpZeroOffset) THEN
                  dfmasum=SQRT(DpZeroOffset/(1/c1/c1+1/c2/c2))/
     &            DpZeroOffset*DSIGN(1d0,DpProfNew(i))
                  fmasum=DpProfNew(i)*dfmasum
                ELSE
                  fmasum=SQRT(ABS(DpProfNew(i))/(1/c1/c1+1/c2/c2))
                  dfmasum=0.5*fmasum/DpProfNew(i)
                ENDIF
              ELSE
                fmasum=0
                dfmasum=0
              ENDIF
            ENDIF

            IF (DpProfNew(i).GT.0) THEN
              fma12=fma12+fmasum
              dp1fma12=dp1fma12+dfmasum

            ELSE
              fma21=fma21+fmasum
              dp1fma21=dp1fma21+dfmasum

            ENDIF

 400      CONTINUE
          ENDIF
C@empa aw 2005may30  Type 3 with geometry from PGS and type 4 and 5 triangles
          IF ((Cfact.NE.0).AND.(type.GE.3))  THEN
            fma21o=fma21
            fma12o=fma12
            dp1fma21o=dp1fma21
            dp1fma12o=dp1fma12
              fma12=0
              fma21=0
              dp1fma12=0
              dp1fma21=0

C           Initialization
            DpZeroOffset=DifLim*1E-3
            alpha=Cfact*1.5707963
            cosAlpha=COS(alpha)
            sinAlpha=SQRT(MAX(0.0,1.0-cosAlpha*cosAlpha))

500         CONTINUE
            o2=axisHght*sinAlpha
            L2=axisHght*cosAlpha
            o4=(actlH-axisHght)*sinAlpha
            L4=(actlH-axisHght)*cosAlpha
C@empa aw 2005jun01 I compare the total flows not the areas of the totally and partly opened 
C@empa aw 2005jun01 window (see below), because the area of the partly opened window might be bigger
C@empa aw 2005jun01 but the flow is still smaller than in the totally opend window
CC            IF (o2+o4+(o2*L2+o4*L4)/actlW.GT.actlH+REALMin) THEN
C             The window is almost (but not totally) openned. Nevetheless,
C             the actual rectanular window frame itself (actlH*actlW) is
C             the smallest flow area:
CC              cosAlpha=0.0
CC              sinAlpha=1.0
CC              GOTO 500
CC            ENDIF

            h2=o2*sinAlpha
            h4=actlH-o4*sinAlpha
            p2=o2*cosAlpha
            p4=o4*cosAlpha

C           Calculation of mass flow and its derivative
            DO i=2,NrInt+1
              EvalHght=deltaZ*(i-1.5)

              IF (DpProfNew(i).GT.0) THEN
                rholink=RhoProfF(i)
              ELSE
                rholink=RhoProfT(i)
              ENDIF

C             Calculate opening area at top and bottom, for strip "evalHght"
              IF (TYPE.EQ.3)THEN

              dZ42=MIN(h4,evalHght+deltaZ*0.5)
     &            -MAX(h2,evalHght-deltaZ*0.5)
              IF (dZ42.LT.deltaZ-1.0E-7) THEN
                area=(deltaZ-MAX(0.0,dZ42))/sinAlpha*actlW
              ELSE
                area=0.0
              ENDIF

C             Calculate opening area in side triangles, for strip "evalHght"
              IF (evalHght.LE.h2) THEN
               IF(h2.GT.0.0) area=area+deltaZ*2.0*p2*evalHght/h2
              ELSEIF (evalHght.LE.axisHght) THEN
               IF(axisHght-h2.GT.0.0)
     &        area=area+deltaZ*2.0*p2*(axisHght-evalHght)/(axisHght-h2)
              ELSEIF (evalHght.LE.h4) THEN
               IF(h4-axisHght.GT.0.0)
     &         area=area+deltaZ*2.0*p4*(evalHght-axisHght)/(h4-axisHght)
              ELSE
               IF(actlH-axisHght.GT.0.0)
     &         area=area+deltaZ*2.0*p4*(actlH-evalHght)/(actlH-h4)
              ENDIF
C	      type 4: Triangle with vertex at the bottom
              ELSEIF (TYPE.EQ.4)THEN
               area = evalHght*ActLw/ActLh*deltaZ  
C	      type 5: Triangle with vertex at the top
              ELSEIF (TYPE.EQ.5)THEN
               area = (ActLh-evalHght)*ActLw/ActLh*deltaZ  
              ENDIF
              

              IF (Abs(DpProfNew(i)).LE.DpZeroOffset) THEN
C               laminar flow
                dfmasum=ActCd*area*SQRT(2.0*rholink*
     &          DpZeroOffset)/DpZeroOffset*DSIGN(1d0,DpProfNew(i))
                fmasum=DpProfNew(i)*dfmasum
              ELSE
C               turbulent flow
                fmasum=ActCd*area*SQRT(2.0*rholink*ABS(DpProfNew(i)))
                dfmasum=0.5*fmasum/DpProfNew(i)
              ENDIF

              IF (DpProfNew(i).GT.0) THEN
                fma12=fma12+fmasum
                dp1fma12=dp1fma12+dfmasum
              ELSE
                fma21=fma21+fmasum
                dp1fma21=dp1fma21+dfmasum
              ENDIF
            ENDDO
            IF (type.eq.3.and.((fma12+fma21).GT.(fma21o+fma12o)))THEN 
C            take the flows from the totally opened window as they are smaller              
             fma12=fma12o
             fma21=fma21o
             dp1fma12=dp1fma12o
             dp1fma21=dp1fma21o
            ENDIF

          ENDIF

C@tno jcp 1996Jun12_11:54:16 just calculate some velocity in the large opening
C fro output (draft analysis or so)
        area=ActLh*ActLw*actCD
        if (area.gt.(cs+REALMin)) then
          if (area.gt.REALMin) then
            FvVeloc=(fma21+fma12)/area
          else
            FvVeloc=0
          end if
        else
C here the average velocity over the full area, may blow half in half out.
C velocity= Fva/Nett area=Fma/Rho/(Cm/( (2**N)* sqrt(1.2) ) )
          if (cs.gt.0.0) then

C get the average Rho for this closed window
            DO 401 i=2,NrInt+1
              rholink=0
              IF (DpProfNew(i).GT.0) THEN
                rholink=RhoProfF(i)
              ELSE
                rholink=RhoProfT(i)
              ENDIF
              rholink=rholink/nrInt
C@tno jcp 1996Jun14_14:07:34 something is wrong with rholink in LO if Closed
              rholink=1.2
401         CONTINUE
c            write(*,*) 'rholink c,n=',rholink,cs,expn
c            write(*,*) 'fma1 2 =',fma12,fma21


            FvVeloc=(Fma21+fma12)*(2**ExpN)*SQRT(1.2)/
     &      (rhoLink*Cs)
          else
            FvVeloc=0.0
          end if
        end if

        RETURN
        END



Ch***********************************************************************
      SUBROUTINE PresProfile(DpProf,RhoProfF,RhoProfT,G,DpF,DpT,
     &          BetaF,BetaT,RhoStF,
     &          RhoStT,LayPtr,LayDat,From,To,Zl,ActLh,OwnHeightFactor)
C***********************************************************************
C
C Purpose:      This routine calculates for a large opening profiles
C               of stack pressure difference and densities in the zones
C               linked by the LO. The profiles are obtained in the following
C               way:    - the opening is divided into NrInt vertical intervals
C                       - the stack pressure difference and densities in From-
C                         and To-zone are calculated at the centre of each
C                         interval as well as at the top and bottom of the LO
C                       - these values are stored in the (NrInt+2)-dimensional
C                         arrays DpProf, RhoProfF, RhoProfT.
C               The calculation of stack pressure and density in the two zones
C               is based on the arrays DpF/T, RhoStF/T, BetaF/T. These arrays
C               are calculated in the COMIS routine Lclimb. They contain the
C               values of stack pressure and density at the startheight of the
C               opening and at startheights of all layers lying inside the
C               opening, and the density gradients across the layers.
C               The effective startheight zl(1/2) in the From/To zone and the
C               effective length actLh of the LO take into account the
C               startheightfactor, heightfactor and ownheightfactor. Thus for
C               slanted windows the range of the profiles is the vertical
C               projection of the actual opening.
C
C Module:
C
C Changes:
C@NBI PGS 1999Aug10 - Had to change name of variable 'Interval' to 'DeltaZ'
C
C Pass Parameters:
C
C IO Name             Unit    Description
C  O DpProf(NrInt+2)     Pa      Differential pressure profile for Large
C                             Openings
C  O RhoProfF(NrInt+2)   Kg/m3   Density profile in FROM zone
C  O RhoProfT(NrInt+2)   Kg/m3   Density profile in TO zone
C  I 1 G              N/kg    gravitation field strength
C  I DpF(MaxNrLay+1)    Pa      Stack pressures at start heights of Layers
C                             in the FROM zone (starting at linkheight)
C  I DpT(MaxNrLay+1)    Pa      Stack pressures at start heights of Layers
C                             in the TO zone (starting at linkheight)
C  I BetaF(MaxNrLay+1)  Kg/m3/m Density gradients in the FROM zone
C                             (starting at linkheight)
C  I BetaT(MaxNrLay+1)  Kg/m3/m Density gradients in the TO zone
C                             (starting at linkheight)
C  I RhoStF(MaxNrLay+1) Kg/m3   Density at the start heights of Layers in
C                             the FROM zone( starting at linkheight)
C  I RhoStT(MaxNrLay+1) Kg/m3   Density at the start heights of Layers in
C                             the TO zone( starting at linkheight)
C  I LayPtr(2,Maxz)   -       Pointer zone to start and stop element in LayDat
C  I LayDat(MaxLay)Multi      Serial: LhL,BetaT, BetaXFct, BtaCFct
C  I From           -         Number of FROM zone
C  I To             -         Number of TO zone
C  I Zl(2)          m         Linkheight in FROM and TO zone
C  I ActLh          m         Actual height of opening
C  I OwnHeightFactor    -     Cosine of deviation angle of the opening plane
C                             from the vertical direction
Ch***********************************************************************

        IMPLICIT NONE
        INCLUDE 'comv-par.inc'


C Passparameters:

        REAL RhoProfF(NrInt+2),RhoProfT(NrInt+2)
C@empa aw 1997sep12 DpF, DPt DOUBLE PRECISION
CC        REAL G,DpF(MaxNrLay+1),DpT(MaxNrLay+1)
        REAL G
        DOUBLE PRECISION DpF(MaxNrLay+1),DpT(MaxNrLay+1)
        REAL BetaF(MaxNrLay+1),BetaT(MaxNrLay+1)
        REAL RhoStF(MaxNrLay+1),RhoStT(MaxNrLay+1)
        REAL ActLh,LayDat(MaxLay),Zl(2),OwnHeightFactor
        INTEGER LayPtr(2,0:MaxZ),From,To
C@empa aw 1997sep12 DpProf as DOUBLE PRECISION
        DOUBLE PRECISION DpProf(NrInt+2)

C Local variables:

C zF, zT:       Startheights of layers in FROM-, TO-zone
C zStF,zStT:    Startheights of layers within the LO, starting with
C               the actual startheight of the LO. The values in the arrays
C               DpF, DpT, BetaF, BetaT, RhoStF, RhoStT are calculated at
C               these heights.
C hghtsF,hghtsT:Heights of evaluation points for pressure and density profiles
C delzF,delzT:  Interval between actual evaluation point and startheight
C               of actual layer in FROM-, TO-zone
C deltaZ:       Interval (distance) between two evaluation points
C AnzLayF,AnzLayT:Number of layers in FROM-, TO-zone
C lF,lT:        Actual index for DpF/T, BetaF/T, RhoStF/T, zStF/T

        REAL zF(MaxNrLay),zT(MaxNrLay)
        REAL zStF(MaxNrLay+2),zStT(MaxNrLay+2)
C@empa aw 1998jun26 REALS for checks
        REAL hghtsFR, hghtsTR 
C@empa aw 1997sep12 DOUBLE PRECISION
        DOUBLE PRECISION hghtsF(NrInt+2),hghtsT(NrInt+2)
        DOUBLE PRECISION deltaZ,delzF,delzT
        INTEGER AnzLayF,AnzLayT,lF,lT,n,i,k


C Initialization

        delzF=0
        delzT=0
        deltaZ=ActLh*OwnHeightFactor/NrInt

        DO 400 n=1,NrInt
          hghtsF(n+1)=Zl(1)+deltaZ*(n-0.5)
          hghtsT(n+1)=Zl(2)+deltaZ*(n-0.5)
 400    CONTINUE
        hghtsF(1)=Zl(1)
        hghtsT(1)=Zl(2)
        hghtsF(NrInt+2)=Zl(1)+ActLh*OwnHeightFactor
        hghtsT(NrInt+2)=Zl(2)+ActLh*OwnHeightFactor


        lF=1
        lT=1
        AnzLayF=(LayPtr(2,From)-LayPtr(1,From)+1)/9
        AnzLayT=(LayPtr(2,To)-LayPtr(1,To)+1)/9

        IF (AnzLayF.GT.0) THEN
          DO 500 n=1,AnzLayF
            zF(n)=LayDat(LayPtr(1,From)+(n-1)*9)
 500      CONTINUE
        ENDIF

        IF (AnzLayT.GT.0) THEN
          DO 600 n=1,AnzLayT
            zT(n)=LayDat(LayPtr(1,To)+(n-1)*9)
 600      CONTINUE
        ENDIF

        zStF(1)=Zl(1)
        i=2
        k=1
 700    CONTINUE
          IF (k.GT.AnzLayF) GOTO 710
          IF (zF(k).GT.zStF(1)) GOTO 710
          k=k+1
          GOTO 700
 710    CONTINUE
 720    CONTINUE
          IF (k.GT.AnzLayF) GOTO 730
          IF (zF(k).GT.hghtsF(NrInt)) GOTO 730
          zStF(i)=zF(k)
          i=i+1
          k=k+1
          GOTO 720
 730    CONTINUE
        zStF(i)=Zl(1)+ActLh*OwnHeightFactor

        zStT(1)=Zl(2)
        i=2
        k=1
 800    CONTINUE
          IF (k.GT.AnzLayT) GOTO 810
          IF (zT(k).GT.zStT(1)) GOTO 810
          k=k+1
          GOTO 800
 810    CONTINUE
 820    CONTINUE
          IF (k.GT.AnzLayT) GOTO 830
          IF (zT(k).GT.hghtsT(NrInt)) GOTO 830
          zStT(i)=zT(k)
          i=i+1
          k=k+1
          GOTO 820
 830    CONTINUE
        zStT(i)=Zl(2)+ActLh*OwnHeightFactor


C Calculation of DpProf, RhoProfF, RhoProfT

        DO 900 i=1,NrInt+2
C@empa aw 1998jun26 take reals for the checks 
        hghtsFR=hghtsF(i)
        hghtsTR=hghtsT(i)
 910      CONTINUE
CC          IF (hghtsF(i).GT.zStF(lF+1)) THEN
            IF (hghtsFR.GT.zStF(lF+1)) THEN
C@lbl bvs 1998May26 was no check for end of array !
C@empa aw 1999apr21 Yes, but the bound will be reached already with MaxNrLay+1 
CC            IF (lF .ge. MaxNrLay+2) GOTO 920
              IF (lF .ge. MaxNrLay+1) GOTO 920
              lF=lF+1
            ENDIF
CC          IF (hghtsF(i).LE.zStF(lF+1)) GOTO 920
            IF (hghtsFR.LE.zStF(lF+1)) GOTO 920
          GOTO 910
 920      CONTINUE

 930      CONTINUE
CC          IF (hghtsT(i).GT.zStT(lT+1)) THEN
            IF (hghtsTR.GT.zStT(lT+1)) THEN
C@lbl bvs 1998May26 was no check for end of array !
C@empa aw 1999apr21 Yes, but the bound will be reached already with MaxNrLay+1 
CC            IF (lT .ge. MaxNrLay+2) GOTO 940
              IF (lT .ge. MaxNrLay+1) GOTO 940
              lT=lT+1
            ENDIF
CC          IF (hghtsT(i).LE.zStT(lT+1)) GOTO 940
            IF (hghtsTR.LE.zStT(lT+1)) GOTO 940
          GOTO 930
 940      CONTINUE

          delzF=hghtsF(i)-zStF(lF)
          delzT=hghtsT(i)-zStT(lT)

          RhoProfF(i)=RhoStF(lF)+BetaF(lF)*delzF
          RhoProfT(i)=RhoStT(lT)+BetaT(lT)*delzT

          DpProf(i)=DpF(lF)-DpT(lT)
     &             -G*(RhoStF(lF)*delzF+BetaF(lF)*delzF**2/2)
     &             +G*(RhoStT(lT)*delzT+BetaT(lT)*delzT**2/2)

 900    CONTINUE

      END

Ch***********************************************************************
        SUBROUTINE ssvent(H   ,B        ,Si,C1,C2,hce,
     &                  kw  ,aw ,Time,T1 ,Tw0,
     &                  T2  ,Twt,Fmas1   ,Q,ErFl            )
C pass parameter # =    1    2   3    4    5  6
C                       7    8   9   10    11
C                       12   13  14   15
C***********************************************************************
C Purpose:      ssvent determines single sided ventilation
C               taking heat transfer with the walls into account.
C               Byproducts are the heat loss and the inside air
C               temperature after the opening of the window or door.
C
C Module:       unknown
C Version       No 1.1,august 30 ,1989
C Changes:
C changed F1 into F1_
C@lbl bs  1991jun04   changed F1_ into F10 to avoid syntax error message
C@lbl     during VAX compilation
C Author:       Leso/EPFL, Koos van der Maas
C Limits:
C
C Pass parameters
C IO  #   Name  Unit            description
C I   1   H     [m]             Opening height
C I   2   B     [m]             Opening width
C I   3   Si    [m2]            room-total wall surface area
C I   4   C1    [m]             Discharge coefficient
C I   5   C2    [m]             Fraction of Si actIF in heat transfer
C I   6
C I   7   kw    [W/m-K]         Thermal conductivity of the wall
C I   8   aw    [m2/s]          Thermal dIFfusivity of the wall
C I   9   TIME  [hrs]           hours after opening window
C I  10   T1    [degrC]         outside air temperature
C I  11   Tw0   [degrC]         initial wall temperature
C I       ZoNr  [-]             sequence number of the SSV zone
C  O 12   T2    [degrC]         inside air temperature
C  O 13   Twt   [degrC]         wall temperature at time t
C  O 14   Fmas1 [kg/s]          mass flow rate out of the room
C  O 15   Q     [W]             heat loss from room
C  O 16   ErFl  [-]             Error flag
C
CC Example:
C       Program COMIS4
C       DOUBLE PRECISION H,B,Si,C1,C2,hce,kw,aw,Time,T1,Tw0,T2,Twt,Fmas1,Q
C       DATA        H /1.5/,B/0.8/,Si/60/,C1/0.6/,C2/1.0/,hce/4.0/
C     &             kw/0.76/,aw/5.35E-7/,Time/0.0/,T1/0.0 /,Tw0/20.0/,
C     &             T2/0/,Twt/0/,Fmas1/0/,Q/0/
C       Call ssvent (H   ,B  ,Si,C1,C2,hce,
C     &              kw  ,aw ,Time,T1 ,Tw0,
C     &              T2  ,Twt,Fmas1   ,Q         )
C       Print*,'Prog: T1,T2,Twt,Fmas1,Q = ',T1,T2,Twt,Fmas1,Q
C       END
CC Result1: T2=10.54747,Twt=20,Fmas1=0.215085,Q=2268.607
Ch***********************************************************************
C

        IMPLICIT NONE
        REAL H,B,Si,C1,C2,hce
        REAL kw,aw,Time,T1,Tw0
        REAL T2,Twt,Fmas1,Q
C CRT=number of output unit
C Iter=loop counter
C Imax=maximum iterations
C StopIt=condition to leave loop
C V=ventilation rate of room (m3/s)
C Tav=average absolute temperature of the two isothermal zones
C vmax= maximum air velocity due to stack effect (m/s)
C F10=function to be minimized
C DF1dT2=derivitave of F10 with respect to T2
C Dzero=Newtons correction in zero search
C T2new=T2-Dzero
C Par0-Par5 parameters for the algebra
        INTEGER Iter,Imax
        LOGICAL StopIt,ErFl
        REAL V,Tav
        REAL F10,DF1dT2,Dzero,T2new
        REAL PAR0, PAR1, PAR2, PAR3, PAR4, PAR5
C ga= gravity acceleration
C T0abs = absolute temperature at 0 degrC
C C1=discharge coefficient of opening 0.6
C hce=convective heat transfer coefficient (6W/m2*K)
C Cp=air specIFiC heat (J/kg-K)
C Rho=mean air density (kg/m3)
C ErrorT=convergence criterion for Newton
        REAL GA,T0abs,Cp,Rho,hrsec,pi,ErrorT
        PARAMETER (GA=9.81)
        PARAMETER (T0abs=273.15)
        PARAMETER (Cp=1000.0)
        PARAMETER (Rho=1.268)
        PARAMETER (hrsec=3600.0)
        PARAMETER (pi=3.14159265359)
C set output device
        PARAMETER (Imax=20)
C set the end of iteration condition
        PARAMETER (ErrorT=1.0E-5)
        DATA Iter/0/
        DATA Dzero/0.0/
C initial value of Variable
        T2=Tw0
        Iter=0
        ErFl=.FALSE.

C-------------------------
C     BEGIN ITERATION LOOP
C-------------------------

50      CONTINUE
        Iter=Iter+1
        Tav=(T1+T2)/2 + T0abs
C initialise inside air temperature: T2 = initial wall temperature
        IF (T1.LT.Tw0) THEN
C
C bernoulli expression for the maximum velocity, the neutral level Hn is
C at midheight, the velocity profile is symmetriC with respect to Hn=H/2.
C (1) vmax=C1*SQRT(GA*H*(T2-T1)/Tav)
                Par0=C1*SQRT(GA*H/Tav)
C     vmax=Par0* SQRT(T2-T1)
C (6) V=W*H*vmax/3
                Par1=B*H*Par0/3
C     V=Par1*SQRT(T2-T1)
C (8) Q=Cp*Rho*V*(T2-T1)
                Par2=Cp*Rho*Par1
C     Q=Par2*(T2-T1)**1.5
C (9) Twt-T2=Q/(C2*Si*hce)
                Par3=1/(C2*Si*hce)
C (11) Twt=Tw0-Q*2*SQRT(aw*Time*hrsec/pi)/(C2*Si*kw)
                Par4= 2*SQRT(aw*Time*hrsec/pi)/(C2*Si*kw)
C     Twt=Tw0-Par4*Q
C (9) vary T2 and find zero of F(T2)= Twt-T2-Par3*Q
C function = Tw0-T2-(Par3+Par4)*Q
                Par5=Par2*(Par3+Par4)
C
C FIND ZERO BY ITERATION USING NEWTONS METHOD
C          (Variable T2)
C function F1_ = Tw0-T2-Par5*(T2-T1)**1.5
C derivative for Newton: dF1/dT2=-(1+1.5*Par5*SQRT(T2-T1))
                F10=Tw0-T2-Par5*(T2-T1)**1.5
                DF1dT2=-(1+1.5*Par5*SQRT(T2-T1))
                Dzero = F10/DF1dT2
                T2new=T2-Dzero
                StopIt = ((ABS(T2-T2new).LT.ErrorT )
     &            .OR. (Iter  .GT.Imax      ))
                T2=T2new
C               Print*,'Iter,Dzero,T2',Iter,Dzero,T2
                IF(.NOT. StopIt) GOTO 50

C--------------
C      END LOOP
C--------------

C output parameters:
                V=Par1*SQRT(T2-T1)
                Fmas1=Rho*V
                Q=Par2*(T2-T1)**1.5
                Twt=Tw0-Par4*Q
        ELSE
C               WRITE(CRT,*) 'Error: T1 (outside)> Twall'
                ErFl=.TRUE.
        ENDIF
C
        END


Ch***********************************************************************
      SUBROUTINE CheckSSVInp
C***********************************************************************
C
C Purpose:  Prepares and checks the input data for single sided ventilation
C
C Module:   empa aw 1994nov17
C
C Changes:
C
C Pass Parameters:
Ch***********************************************************************

        IMPLICIT NONE
       INCLUDE 'comv-inp.inc'
       INTEGER  i,j,LS,pZo,pLi,LStr1,LStr2,LStr3,key,lstr
       REAL     zLi
        INTEGER LenStr

       DO 100 i=1,Nzt
         pLi=0
         CALL LookNam(Nz,ZTNa(i),ZoTree,ZoTreeN,pZo,ZoLarg,
     &   ZoSmal,key)
         IF (key.EQ.1) THEN
           LStr=LENSTR(ZTNa(i))
           CALL INERR('&-NET-ZT: zone name: '// ZTNa(i)(1:LStr)//
     &     ' does not exist in &-NET-ZONes !',' ',.FALSE.,2)
         ELSE
           pZTZo(i)=pZo
C          calculate zone wall surface, thermal diffusity and initialize
C          the open flag
           Sz(i)=2*(HZ(pZo)*DZ(pZo)+HZ(pZo)*WZ(pZo)+DZ(pZo)*Wz(pZo))
           IF (Sz(i).EQ.0.) THEN
             LStr1=LenStr(ZoNa(pZo))
             CALL INERR ('&-NET-ZONE: '//ZoNa(pZo)(1:LStr1)//
     &       ': H/D/W of the zone must be given for single sided '//
     &       'ventilation','model !',.false.,2)
           ENDIF
           IF (layptr(1,pzo).NE.0)THEN
             LStr1=LenStr(ZoNa(pZo))
             CALL INERR ('&-NET-ZT: Zone: '//ZoNa(pZo)(1:LStr1)//
     &       ': No layers may be defined in a single sided '//
     &       'ventilated zone !',' ',.false.,2)
           ENDIF
           if ((CapW(i)*RhoW(i)).ne.0) then
             aw(i)=LamdaW(i)/RhoW(i)/CapW(i)
           endif
           SSVFl(i)=.FALSE.
           DO 200 j=1, Nl
C            Check whether we have a window to outside
             LS=Lstat(j)
             IF (LDat(pLiLDat(j)).EQ.9.)THEN
               IF (LS.EQ.1.OR.LS.EQ.2)THEN
                 IF (FromTo(2,j).EQ.pZo) THEN
C                  From is outside, To is SSV zone
                   IF (LDat(pLiLDat(j)+4).NE.1) THEN
                     LStr1=LenStr(ZoNa(pZo))
                     LStr2=LenStr(LiTyNa(j))
                     LStr3=LenStr(LiNa(j))
                     CALL INERR('&-NET-LINKS: '//LiNa(j)(LStr3:1)//
     &               ': Opening *'//LiTyNa(j)(1:LStr2)//
     &               ' to single sided vent. '//
     &               'zone '//ZoNa(pZo)(1:LStr1)//' is not '//
     &               'LVO type 1 !',
     &               'Calculation of temperature drop with:'//
     &               '(Open area = Lhmax x Lwmax)!',.false.,1)
                   ENDIF
                   IF (pLi.EQ.0) THEN
                     pLi=j
                     zLi=Zl(2,pLi)
                   ELSE
                     LStr1=LenStr(ZoNa(pZo))
                     LStr2=LenStr(LiNa(pLi))
                     LStr3=LenStr(LiNa(j))
                     CALL INERR('&-NET-LINKS: '//
     &               LiNa(pLi)(1:LStr2)//' and '//LiNa(j)(1:LStr3)//
     &               ':','Only one large opening is allowded to '//
     &               'single sided ventilation zone '//
     &               ZoNa(pZo)(1:LStr1)//' !',.false.,2)
                   ENDIF
                 ENDIF
               ELSE IF (LS.EQ.3.OR.LS.EQ.6)THEN
C@empa aw 1999oct26 check with zonepointer pZo not with i
CC                 IF (FromTo(1,j).EQ.i)THEN
                 IF (FromTo(1,j).EQ.pZo)THEN
C                  To is outside, From is SSV zone
                   IF (LDat(pLiLDat(j)+4).NE.1) THEN
                     LStr1=LenStr(ZoNa(pZo))
                     LStr2=LenStr(LiTyNa(j))
                     LStr3=LenStr(LiNa(j))
                     CALL INERR('&-NET-LINKS: '//LiNa(j)(LStr3:1)//
     &               ': Opening *'//LiTyNa(j)(1:LStr2)//
     &               ' to single sided vent. '//
     &               'zone '//ZoNa(pZo)(1:LStr1)//' is not '//
     &               'LVO type 1 !',
     &               'Calculation of temperature drop with:'//
     &               '(Open area = Lhmax x Lwmax)!',.false.,1)
                   ENDIF
                   IF (pLi.EQ.0) THEN
                     pLi=j
                     zLi=Zl(1,pLi)
                   ELSE
                     LStr1=LenStr(ZoNa(pZo))
                     LStr2=LenStr(LiNa(pLi))
                     LStr3=LenStr(LiNa(j))
                     CALL INERR('&-NET-LINKS: '//
     &               LiNa(pLi)(1:LStr2)//' and '//LiNa(j)(1:LStr3)//
     &               ':','Only one large opening is allowded to '//
     &               'single sided ventilation zone '//
     &               ZoNa(pZo)(1:LStr1)//' !',.false.,2)
                   ENDIF
                 ENDIF
               ENDIF
             ENDIF
200        CONTINUE
           IF (pLi.NE.0.0) THEN
             pZTLi(i)=pLi
             IF((zLi+LDat(pLiLDat(pLi)+6)).GT.HZ(pZo))THEN
               LStr1=LenStr(ZoNa(pZo))
               LStr2=LenStr(LiNa(pLi))
               CALL INERR ('&-NET-ZONE: '//ZoNa(pZo)(1:Lstr1)//
     &         ';    &-NET-LINKS: '//LiNa(pLi)(1:LStr2)//
     &         ';    &-NET-AIR: *'//LiTyNa(pLi),
     &         'Upper level of window is above '//
     &         'ceiling level of the room!',.false.,2)
             ENDIF
           ENDIF
         ENDIF
100    CONTINUE
       END

Ch***********************************************************************
      SUBROUTINE PrecSSV(time)
C***********************************************************************
C
C Purpose:  Prepares and checks in every timestep the actual input parameters
C           for the single sided ventilation routine. Calls the single sided
C           ventilation routine
C
C Module:   empa aw 1994nov11
C
C Changes:
C
C Pass Parameters:
C I  time  [days]      Time of the actual timestep
Ch***********************************************************************

        IMPLICIT NONE
       INCLUDE 'comv-inp.inc'
       DOUBLE PRECISION time

        INTEGER LenStr
       INTEGER i,pLi,pZo,LStr1,LStr2
       REAL TzSSV(MaxZT),TzSav(MaxZT),Tw0(MaxZT),opentime
       REAL ActLh,ActLW,ActCd,UpWiLev,C2,Twt,QH
       REAL Penetr,fmas1
       REAL hce
C@empa aw 2001oct22 change t0 from real to double
       DOUBLE PRECISION t0(MaxZT)
       LOGICAL ErFl
       SAVE TzSSV,TzSav,Tw0,t0

        DO 100 i=1,Nzt
          pZo=pZTZo(i)
          pLi=pZTLi(i)
          IF (pLi.NE.0)THEN
C           We have a window to a SSV-zone
            IF (MF(pLi).NE.0.0) THEN
C             The window is open
              IF (.NOT.SSVFl(i))THEN
C               The window has been opened just in this timstep
                SSVFl(i)=.TRUE.
                t0(i)=time
                Tw0(i)=Tz(pZo)
C               Save the initial zone temperature set point.
                TzSav(i)=Tz(pZo)
              ENDIF
              IF (TzSSV(i).NE.Tz(pZo))THEN
C               Zone temperature set point has been changed by a schedule
C               during open time. Save new set point.
                TzSav(i)=Tz(pZo)
              ENDIF
C@empa aw 1999oct26 time is double so use REAL() function
              openTime=time-t0(i)
CC              openTime=REAL(time)-t0(i)
              opentime=opentime*24
              Penetr=SQRT(aw(i)*opentime*3600)
              IF (Penetr.GT.EW(i)) THEN
                LStr1=LenStr(LiNa(pLi))
                LStr2=LenStr(ZoNa(i))
                CALL ERROR2('The opening at link '//LiNa(pLi)(1:LStr1)//
     &          ' has been opened too long for the single sided '//
     &          'vent.',
     &          ' model. The calculated temperature drop in zone '//
     &          ZoNa(i)(1:LStr2)//' may not be correct!',2)
              ENDIF
C       Heat transfere coefficient:  6 W/m2K
C       (see: "Some aspects of gravity driven air flow through
C              large openings in buildings" K.v.d Maas, C.A.Roulet, J.A.Hertig)
              hce=6

              ActLw=LinkDat(pLinkDat(pLi)+1)
              ActLh=LinkDat(pLinkDat(pLi)+2)
              ActCd=LinkDat(pLinkDat(pLi)+3)
C@empa aw 1999oct25 check with zone pointer pZo not with i
CC              IF (FromTo(1,pLi).EQ.i)THEN
              IF (FromTo(1,pLi).EQ.pZo)THEN
                UpWiLev=Zll(1,pLi)+ActLh
CC              ELSE IF (FromTo(2,pLi).EQ.i)THEN
              ELSE IF (FromTo(2,pLi).EQ.pZo)THEN
                UpWiLev=Zll(2,pLi)+ActLh
              ELSE
C@empa aw 1999oct25 Give a correct error message"! It shouldn't be possible to get here.
C                   Otherwise there must be a bug in the code somewhere else.  
CC                STOP
                LStr1=LenStr(ZoNa(pZo))
                CALL ERROR2('Single sided ventilation in zone '//
     &          ZoNa(pZo)(1:LStr1)//': The zone sequence number '//
     &          'cannot be','found in the link From-To list.  '//
     &          'SOURCE CODE BUG! mail to: andreas.weber@empa.ch',3)
                Tz(pZo)=Tw0(i)
              ENDIF
              IF (UpWiLev.EQ.Hz(pZo))THEN
                 C2=1
              ELSE
                 C2=(Dz(pZo)*Wz(pZo)+UpWiLev*2*(Dz(pZo)+Wz(pZo)))/Sz(i)
              ENDIF
C             calculate zone temperature after opentime
              CALL SSVent (ActLh,ActLw,Sz(i),ActCd,C2,hce,
     &        LamdaW(i),aw(i),OpenTime,Tout,Tw0(i),
     &        TzSSV(i),Twt,Fmas1,QH,ErFl)
              Tz(pZo)=TzSSV(i)
              IF (ErFl) THEN
                LStr1=LenStr(ZoNa(pZo))
                CALL ERROR2('Single sided ventilation in zone '//
     &          ZoNa(pZo)(1:LStr1)//': Outside temperature is higher '//
     &          'than','initial zone temperature. Zone temperature '//
     &          'has been left at initial value!',2)
                Tz(pZo)=Tw0(i)
              ENDIF

            ELSE IF (SSVFl(i)) THEN
C             Window has been closed just in this timestep
C             Set zone temperature to saved zone temperature setpoint
              Tz(pZo)=TzSav(i)
              SSVFl(i)=.FALSE.
            ENDIF
          ENDIF
100     CONTINUE
        END

Ch***********************************************************************
      SUBROUTINE CheckSSVOutp
C***********************************************************************
C
C Purpose:  Checks the flows in a single sided ventilated zone.
C           The routine writes a message, how much of the total flow does
C           not go through the opening link.
C
C Module:   empa aw 1994nov28
C
C Changes:
C
C Pass Parameters:
Ch***********************************************************************

        IMPLICIT NONE
       INCLUDE 'comv-inp.inc'

       INTEGER i,pLi,pZo,LStr1,LStr2,LStr3,AFI
       REAL MinFl,AF
       CHARACTER AStr*30
        INTEGER LenStr

        DO 100 i=1,Nzt
          pZo=pZTZo(i)
          pLi=pZTLi(i)
          IF (pLi.NE.0)THEN
C           We have a window to a SSV-zone
            IF (MF(pLi).NE.0.0) THEN
C             The window is open
              MinFl=MIN(FV2(1,pLi),FV2(2,pLi))
              IF(FT(pZo)*0.99.GT.MinFl) THEN
                AF=100-MinFl/FT(pZo)*100
                AFI=NInt(AF)
                CALL IntDis(AFI,AStr,LStr3)
                LStr1=LenStr(ZoNa(pZo))
                LStr2=LenStr(LiNa(pLi))
                CALL ERROR2(AStr(1:LStr3)//'% of the total flow in '//
     &          ' the single sided vent. zone '//ZoNa(pZo)(1:LStr1)//
     &          ' does not flow through','the large opening'//
     &          ' link '//LiNa(pLi)(1:LStr2)//' .',2)
              ENDIF
            ENDIF
          ENDIF
100     CONTINUE
        END
