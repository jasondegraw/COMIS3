C+*********************************************************** comv-flo.f
Ch***********************************************************************

	SUBROUTINE Cross(NotFound,X1,C,CA,NA,DifLim)

C************************************************************************
C
C     Purpose: Determines the cross points between a polynomial and
C              a expomential function use the Newton method with 
C              under-relaxation coefficients.
C
C Module :
C Version: @empa aw 1995july05
C Change :
C Limit  :
C
C Pass parameters:
C
C IO  #  Name     unit    description
C
C O   1  NotFound  -      Flag wether cross point has been found 
C IO  2  X1       pa      I: guess for cross point 
C                         O: cross point between the two curve
C
C I   3  C(0:20)          coefficients C0,C1,...,C(n-1)
C I   4  CA       m3/s    flow coefficient C of power law
C I   5  NA       -       flow coefficient n of power law
C I   6  Diflim   Pa      below this pressure the flow is laminar
Ch********************************************************************
C
      IMPLICIT NONE
	REAL ER,NA,CA,EM,C(0:20),al,Yd,X1,DifLim
	DOUBLE PRECISION X,XX,Y
	INTEGER K,NI
        LOGICAL NotFound

C initialisation of EM and NI
	EM=1E-6
C NI: Number of iterations
	NI=100
C al : Start with relaxation = 0.6
        al=0.6
C start with the guess for XX
        X=X1
        NotFound=.false.
10	CONTINUE
          K=1
20	  CONTINUE
	    CALL FONX(CA,NA,X,C,DifLim,Y,Yd)
            IF (YD.NE.0.) THEN
	       XX=X-Y/YD
            ELSE
C              This point is a maximum or a mimimum. Try near this point.
               XX=X+10*ER
            ENDIF
	    ER=ABS(XX-X)
	    IF (ER.GE.EM) THEN
	       X=al*X+(1-al)*XX
               K=K+1
	    ENDIF
          IF ((ER.GE.EM).AND.(K.LE.NI)) GOTO 20
          al=al+0.1
C       Not converged, try with new relaxation
        IF ((K.GE.NI).AND.(al.LT.1.0)) GOTO 10
        IF (K.GE.NI) THEN
C           No cross point found
            NotFound=.true.
        ELSE
           X1=XX
        ENDIF                
C        write(6,*) 'al = ',al-0.1, 'K = ', K-1
	RETURN
	END
C
Ch*******************************************************************

	 SUBROUTINE FONX(CA,NA,X,C,DifLim,Y,Yd)

C********************************************************************
C
C      Purpose: Calculates the non linear function solved after with
C               the Newton method.
C
C Module :
C Change :
C Limit  :
C
C Pass parameters:
C
C IO  #   Name    unit    description
C
C I   1   CA      m3/s    flow coefficient C of power law
C I   2   NA       -      flow coefficient n of power law
C I   3   X        Pa     the variable, in the general case
C                         the pressure difference accross the link
C I   4   C(0:20)  -      coefficients C0,C1,...,C(n-1)
C I   5   Diflim   Pa     below this pressure the flow is laminar
C O   6   Y       m3/s    the result of the function
C O   7   Yd      m3/s/Pa derivative of the function
C
Ch*******************************************************************
C
      IMPLICIT NONE
	REAL NA,C(0:20),CA,Yd,DifLim
	DOUBLE PRECISION X,Y,flow
C@lbl bvs 1997Oct20 added I
	INTEGER I,J
	Y=C(0)
        Yd=0
	IF (X.NE.0.0D0) THEN
	  DO 10 I=1,5
	     Y=Y+C(I)*X**(I)
	     Yd=Yd+I*C(I)*X**(I-1)
  10      CONTINUE
        ELSE
          Yd=C(1)
	ENDIF
	IF (X.LT.0.0D0) THEN
	  J=-1
	ELSE
	  J=1
	ENDIF
	IF (ABS(X).GT.DifLim) THEN
	  Y=Y-CA*J*ABS(X)**NA
	  Yd=Yd-(CA*NA)/(J*ABS(X)**ABS(NA-1))
        ELSE
          flow=CA*DifLim**NA
          Y=Y-flow*X/DifLim
          Yd=Yd-flow/DifLim
	ENDIF
	RETURN
	END

Ch*******************************************************************

C@lbl bvs 1997Jun9 InitFlg removed from common - now arguments to FLCON1-4
CC	 SUBROUTINE FlCon1(Dat,Dp,Mf1,DifLim,RhoL1,RhoL2,Fma,DFma)
	SUBROUTINE FlCon1(Dat,Dp,Mf1,DifLim,RhoL1,RhoL2,Fma,DFma,
     &   linInit)

C@lbl dml 1999nov19 Change integer variable initFlg to logical linInit.

Ch*******************************************************************

      IMPLICIT NONE
        REAL Dat(0:9),Mf1,DFma,DifLim,RhoL1,RhoL2,rho,SetP
        REAL K,CsF,CsL,ExpF,ExpL,DfmaF,DFmaL

C@ lbl bvs 1996Nov1 flow never used
CC        DOUBLE PRECISION Dp,Fma,Dpa,flow,FlowF,FlowL
        DOUBLE PRECISION Dp,Fma,Dpa,FlowF,FlowL
C@lbl bvs 1997Jul28 declare linInit
        logical linInit

        IF(ABS(Dp).GT.DifLim) THEN
          Dpa=ABS(Dp)
        ELSE
          Dpa=DifLim
        ENDIF

        IF (Dp.GE.0.0) THEN
            rho=RhoL1
        ELSE
            rho=RhoL2
        ENDIF
C       Mass flow with temperature compensation: fundamental eqn. 32
        K=(Dat(1)/rho)**(Dat(3)-1)        
        CsF=Dat(2)
        ExpF=Dat(3)
        SetP=Dat(4)
        CsL=Dat(5)
        ExpL=Dat(6)
        IF ( linInit ) THEN
C          linear initialization:
           Fma=(CsF+CsL)*Dpa
           DFma=CsF+CsL
        ELSE 
C          range 1:
           FlowF=CsF*Dpa**ExpF
           FlowL=CsL*Dpa**ExpL
           DFmaF=FlowF*ExpF/Dpa
C@lbl bvs 1997Jun9 Exp should be ExpL
CC           DFmaL=FlowL*Exp/Dpa
           DFmaL=FlowL*ExpL/Dpa
           Fma=FlowF+FlowL
           DFma=DFmaF+DfmaL
        ENDIF
        IF (Fma.GE.SetP) THEN
          IF (FlowL.LE.SetP)THEN
C          range 2: Flow constant
           Fma=SetP
           Dfma=0
          ELSE
C          flowcontroller closed: only leakage
           Fma=FlowL
           DFma=DFmaL
          ENDIF
        ENDIF 

        IF (ABS(Dp).LE.DifLim) THEN
C         below DifLim:
          Dfma=Fma/Dpa
          Fma=ABS(Dp)*Dfma
        ENDIF


        IF (Dp.LT.0) THEN
           Fma=-Fma*Mf1*K
        ELSE
           Fma=Fma*Mf1*K
        ENDIF
        DFma=DFma*Mf1*K
        END


Ch*******************************************************************

C@lbl bvs 1997Jun9 InitFlg removed from common - now arguments to FLCON1-4
CC	 SUBROUTINE FlCon2(Dat,Dp,Mf1,DifLim,RhoL1,RhoL2,Fma,DFma)
	SUBROUTINE FlCon2(Dat,Dp,Mf1,DifLim,RhoL1,RhoL2,Fma,DFma,
     &   linInit)

C@lbl dml 1999nov19 Change integer variable initFlg to logical linInit.

Ch*******************************************************************

      IMPLICIT NONE
        REAL Dat(0:9),Mf1,DFma,DifLim,RhoL1,RhoL2,rho
        REAL K,CsF,CsL,ExpF,ExpL,DfmaF,DFmaL,SetP

C@ lbl bvs 1996Nov1 flow never used
CC        DOUBLE PRECISION Dp,Fma,Dpa,flow,FlowF,FlowL
        DOUBLE PRECISION Dp,Fma,Dpa,FlowF,FlowL
        logical linInit

        IF(ABS(Dp).GT.DifLim) THEN
          Dpa=ABS(Dp)
        ELSE
          Dpa=DifLim
        ENDIF

        IF (Dp.GE.0.0) THEN
            rho=RhoL1
            SetP=Dat(4)
        ELSE
            rho=RhoL2
            SetP=-Dat(5)
        ENDIF
C       Mass flow with temperature compensation: fundamental eqn. 32
        K=(Dat(1)/rho)**(Dat(3)-1)        
        CsF=Dat(2)
        ExpF=Dat(3)


        CsL=Dat(6)
        ExpL=Dat(7)
        IF ( linInit ) THEN
C          linear initialization:
           Fma=(CsF+CsL)*Dpa
           DFma=CsF+CsL
        ELSE 
C          range 1:
           FlowF=CsF*Dpa**ExpF
           FlowL=CsL*Dpa**ExpL
           DFmaF=FlowF*ExpF/Dpa
C@lbl bvs 1997Jun9 Exp should be ExpL
CC           DFmaL=FlowL*Exp/Dpa
           DFmaL=FlowL*ExpL/Dpa
           Fma=FlowF+FlowL
           DFma=DFmaF+DfmaL
        ENDIF

        IF (Fma.GE.SetP) THEN
          IF (FlowL.LE.SetP)THEN
C          range 2: Flow constant
           Fma=SetP
           Dfma=0
          ELSE
C          flowcontroller closed: only leakage
           Fma=FlowL
           DFma=DFmaL
          ENDIF
        ENDIF 

        IF (ABS(Dp).LE.DifLim) THEN
C         below DifLim:
          Dfma=Fma/Dpa
          Fma=ABS(Dp)*Dfma
        ENDIF


        IF (Dp.LT.0) THEN
           Fma=-Fma*Mf1*K
        ELSE
           Fma=Fma*Mf1*K
        ENDIF
        DFma=DFma*Mf1*K
        END


Ch*******************************************************************

C@lbl bvs 1997Jun9 InitFlg removed from common - now arguments to FLCON1-4
CC	SUBROUTINE FlCon3(Dat,Dp,Mf1,DifLim,RhoL1,RhoL2,Fma,DFma)
      SUBROUTINE FlCon3(Dat,Dp,Mf1,DifLim,RhoL1,RhoL2,Fma,DFma,
     &   linInit)

C@lbl dml 1999nov19 Change integer variable initFlg to logical linInit.

Ch*******************************************************************

      IMPLICIT NONE
        REAL Dat(0:23),Mf1,DFma,DifLim,RhoL1,RhoL2,rho
        REAL K,Cq
C@ lbl bvs 1996Nov1 flow never used
CC        DOUBLE PRECISION Dp,Fma,Dpa,flow
        DOUBLE PRECISION Dp,Fma,Dpa
        logical linInit

        IF(ABS(Dp).GT.DifLim) THEN
          Dpa=ABS(Dp)
        ELSE
          Dpa=DifLim
        ENDIF

        IF (Dp.GE.0.0) THEN
            rho=RhoL1
        ELSE
            rho=RhoL2
        ENDIF
C       Mass flow with temperature compensation: fundamental eqn. 32
        K=(Dat(1)/rho)**(Dat(3)-1)         
        Cq=Dat(2)
        IF ( linInit ) THEN
C          linear initialization:
           Fma=Cq*Dpa
           DFma=Cq
        ELSE IF (Dpa.LT.Dat(4)) THEN
C          range 1:

           Fma=Cq*Dpa**Dat(3)
           DFma=Fma*Dat(3)/Dpa

        ELSE IF (Dpa.LT.Dat(11)) THEN
C          range 2:
C@ lbl bvs 1997jun6 first two args to FONX are real
CC           CALL FONX(0,0,Dpa,Dat(5),DifLim,Fma,Dfma)
           CALL FONX(0.0,0.0,Dpa,Dat(5),DifLim,Fma,Dfma)

        ELSE 
C          range 3:
C@ lbl bvs 1997jun6 first two args to FONX are real
CC           CALL FONX(0,0,Dpa,Dat(12),DifLim,Fma,Dfma)
           CALL FONX(0.0,0.0,Dpa,Dat(12),DifLim,Fma,Dfma)
           IF ((Dat(18).NE.0.0).AND.(Dpa.GT.Dat(18))) THEN
C             above range 3:
              Fma=Dpa*Dat(19)
              DFma=Dat(19)
           ENDIF
        ENDIF

        IF (ABS(Dp).LE.DifLim) THEN
C         below DifLim:
          Dfma=Fma/Dpa
          Fma=ABS(Dp)*Dfma
        ENDIF

        IF (Dp.LT.0) THEN
           Fma=-Fma*Mf1*K
        ELSE
           Fma=Fma*Mf1*K
        ENDIF
        DFma=DFma*Mf1*K
        END


Ch*******************************************************************

C@lbl bvs 1997Jun9 InitFlg removed from common - now arguments to FLCON1-4
CC    SUBROUTINE FlCon4(Dat,Dp,Mf1,DifLim,RhoL1,RhoL2,Fma,DFma)
      SUBROUTINE FlCon4(Dat,Dp,Mf1,DifLim,RhoL1,RhoL2,Fma,DFma,
     &   linInit)

C@lbl dml 1999nov19 Change integer variable initFlg to logical linInit.

Ch*******************************************************************

      IMPLICIT NONE
        REAL Dat(0:38),Mf1,DFma,DifLim,RhoL1,RhoL2,rho
        REAL K,Cq
C@ lbl bvs 1996Nov1 flow never used
CC        DOUBLE PRECISION Dp,Fma,Dpa,flow
        DOUBLE PRECISION Dp,Fma,Dpa
        logical linInit

        IF(ABS(Dp).GT.DifLim) THEN
          Dpa=ABS(Dp)
        ELSE
          Dpa=DifLim
        ENDIF

        IF (Dp.GE.0.0) THEN
            rho=RhoL1
        ELSE
            rho=RhoL2
        ENDIF
C       Mass flow with temperature compensation: fundamental eqn. 32
        K=(Dat(1)/rho)**(Dat(3)-1)         
        Cq=Dat(2)
        IF ( linInit ) THEN
C          linear initialization:
           Fma=Cq*Dpa
           DFma=Cq
        ELSE IF (Dp.GT.0.0) THEN
C          positive flow:
           IF (Dpa.LT.Dat(4)) THEN
C             range 1:
              Fma=Cq*Dpa**Dat(3)
              DFma=Fma*Dat(3)/Dpa

           ELSE IF (Dpa.LT.Dat(11)) THEN
C             range 2:
C@ lbl bvs 1997jun6 first two args to FONX are real
CC              CALL FONX(0,0,Dpa,Dat(5),DifLim,Fma,Dfma)
              CALL FONX(0.0,0.0,Dpa,Dat(5),DifLim,Fma,Dfma)

           ELSE 
C             range 3:
C@ lbl bvs 1997jun6 first two args to FONX are real
CC              CALL FONX(0,0,Dpa,Dat(12),DifLim,Fma,Dfma)
              CALL FONX(0.0,0.0,Dpa,Dat(12),DifLim,Fma,Dfma)
              IF ((Dat(18).NE.0.0).AND.(Dpa.GT.Dat(18))) THEN
C                above range 3:
                 Fma=Dpa*Dat(19)
                 DFma=Dat(19)
              ENDIF

           ENDIF
        ELSE 
C          negative flow : 
           IF (Dpa.LT.Dat(20)) THEN
C             range 1:
              Fma=Cq*Dpa**Dat(3)
              DFma=Fma*Dat(3)/Dpa

           ELSE IF (Dpa.LT.Dat(27)) THEN
C             range 2:
C@ lbl bvs 1997jun6 first two args to FONX are real
CC              CALL FONX(0,0,Dpa,Dat(21),DifLim,Fma,Dfma)
              CALL FONX(0.0,0.0,Dpa,Dat(21),DifLim,Fma,Dfma)

           ELSE 
C             range 3:
C@ lbl bvs 1997jun6 first two args to FONX are real
CC              CALL FONX(0,0,Dpa,Dat(28),DifLim,Fma,Dfma)
              CALL FONX(0.0,0.0,Dpa,Dat(28),DifLim,Fma,Dfma)
              IF ((Dat(34).NE.0.0).AND.(Dpa.GT.Dat(34))) THEN
C                above range 3:
                 Fma=Dpa*Dat(35)
                 DFma=Dat(35)
              ENDIF
           ENDIF
        ENDIF

        IF (ABS(Dp).LE.DifLim) THEN
C         below DifLim:
          Dfma=Fma/Dpa
          Fma=ABS(Dp)*Dfma
        ENDIF

        IF (Dp.LT.0) THEN
           Fma=-Fma*Mf1*K
        ELSE
           Fma=Fma*Mf1*K
        ENDIF
        DFma=DFma*Mf1*K
        END


Ch**********************************************************************
C@NBI PGS 2003Apr27 - New component type: Thermostatic vent
      SUBROUTINE TV_flow(Dat,Dp,Mf1,DifLim,Temp1,Rho1,Rho2,Fma,DFma
     &                  ,linInit,NormCrRho)
Ch**********************************************************************

        IMPLICIT NONE

C       ! Passed arguments
        REAL Mf1,DFma,DifLim,Rho1,Rho2,Temp1,NormCrRho,Dat(0:10)
        DOUBLE PRECISION Dp,Fma
        logical linInit

C       ! Internal variables
        REAL K,Cm,Expn,rho,f
        DOUBLE PRECISION Dpa
C-----
        IF (Dp.GE.0.0D0) THEN
          rho=Rho1
        ELSE
          rho=Rho2
        ENDIF
C       ! Correction factor 'K' for actual air density
        K=(NormCrRho/rho)**(Expn-1.0)

        DPa=MAX(ABS(Dp),DBLE(DifLim))

C       ! Linear interpolation function 'f', across two linear ranges,
C       ! limited between 0 (closed) and 1 (open).
C       ! Presently the "FROM-zone" temperature is used for interpolation.
        IF (Temp1.LE.Dat(4)) THEN
C         ! Low temperature range (CLOSED to INTERMEDIATE)
          IF(Dat(4).EQ.Dat(1))THEN
            f=0.0
          ELSE
            f=MIN(1.0,MAX(0.0,(Temp1-Dat(1))/(Dat(4)-Dat(1))))
          ENDIF
          Cm=(f*(Dat(5)-Dat(2))+Dat(2))*Mf1*K
          Expn=f*(Dat(6)-Dat(3))+Dat(3)
        ELSE
C         ! High temperature range (INTERMEDIATE to FULLY OPEN)
         IF(Dat(7).EQ.Dat(4))THEN
            f=1.0
          ELSE
            f=MIN(1.0,MAX(0.0,(Temp1-Dat(4))/(Dat(7)-Dat(4))))
          ENDIF
          Cm=(f*(Dat(8)-Dat(5))+Dat(5))*Mf1*K
          Expn=f*(Dat(9)-Dat(6))+Dat(6)
        ENDIF

        IF (linInit) THEN
C         ! linear initialization
          Fma=DBLE(Cm)*Dpa
          DFma=Cm
        ELSE
C         ! normal calculation step
          Fma=DBLE(Cm)*Dpa**DBLE(Expn)
          DFma=Fma*Expn/Dpa
          IF (ABS(Dp).LE.DifLim) THEN
C           ! laminar flow assumed
            DFma=Fma/Dpa
            Fma=ABS(Dp)*DBLE(DFma)
          ENDIF
        ENDIF

        IF (Dp.LT.0.0D0) THEN
          IF (Dat(10).EQ.1.0) THEN
C           ! One-way flow : Closed
            Fma=0.0D0
            DFma=0.0
          ELSE
            Fma=-Fma
          ENDIF
        ENDIF
        RETURN
      END

