C+*********************************************************** comv-rex.f
Ch***********************************************************************
        SUBROUTINE RELAX(Nz, Nl, PZOLD, PzNew,
C                        1   2     4     5
C@tno jcp 1996Apr07_15:40:05 Ltyp deleted
     &  DifLim,DpL,FromTo,Lstat,Mf,Mp,pLiLDat,Ldat,RhoL,
C         6     7     8     9   10 11  12    13   14   15
C@tno jcp 1996Apr07_15:46:50 ctl removed
     &  SQRRhoL,MuL,corr,FmFact,DMZ,linInit,RelaxD,RelaxO,RSqrtDff)
C        16     17  18    19

C***********************************************************************
C Purpose: RELAX is a module for the calculation of relaxation
C          coefficient(s) for the non-linear solver.
C          Perform line seek to minimize function defined by 'QSUM'.
C          Start at 'PZOLD(*)=PzNew(*)' and move in direction 'CORR(*)'.
C          Deliver result in 'PzNew(*)'.
C
C Module : #4.4.6.1, TG V, MKH/June 20, 1989
C Changes: none
C jcp 1990dec28 linInit added in the call and in the call of QSUM1
C WrongL added to flag Lambda out of range and then write
C FI(1) FI(2) FI(3) the integral function of flow.
C extended limits of Lambda.
C added check if curve has minimum or maximum
C@lbl dml 1999nov19 Change integer variable initFlg to logical linInit.
C
C Limits : N/A
C
C Pass parameters:
C
C IO # Name       unit              description
C I  1 Nz         [-]               Number of zones
C I  2 Nl         [-]               Number of links
C IO 4 PZOLD      [Pa]              Old pressure for each zone
C IO 5 PzNew      [Pa]              New pressure for each zone
C I  6 DifLim     [-]
C I  7 Dpl        [Pa]
C I  8 FromTo     [-]
C I  9 Lstat      [-]
C I 10 Mf         [-]
C I 11 Mp
C I 12 pLiLDat
C I 13 LDat
C I 14 Ltyp
C I 15 RhoL
C I 16 SQRRhoL
C I 17 MuL
C O 18 Corr
C I 19 FmFact             Massflow  Display multiplication to userunits
C      DMZ        [kg/s]  changes of the Mass per Zone, (ie temperature)
C I 22 linInit     -      Linear initialization flag (turns n to 1)
C O 23 RelaxD      -      chosen relaxation coef returned to write
C I 24 RSqrtDff           Reciprocal of the squarroot of the Darcy
C                                 friction factor

Ch***********************************************************************

        IMPLICIT NONE
        INCLUDE 'comv-uni.inc'
        include 'comv-par.inc'
C
C PASSED
C
        INTEGER Nz, Nl, FromTo(2,maxl),
     &  Lstat(maxl),pLiLDat(maxl)
        REAL Mf(maxl),Mp(maxl),Ldat(maxd),RhoL(2,maxl),SQRRhoL(2,maxl)
        REAL Mul(2,maxl),diflim
C@empa aw 1997sep18 DpL as DOUBLE PRECISION
C@empa aw 2002dec04 DpL enlarged from DpL(2,MaxL) to DpL(3,MaxL) to have a space
C@empa aw 2002dec04 for the final pressure differnce
        DOUBLE PRECISION DpL(3,maxl)
        REAL FmFact
        REAL DMZ(maxz)
        REAL RSqrtDff(maxl)
        DOUBLE PRECISION PZOLD(maxz), PzNew(maxz),corr(maxz)
        logical linInit
        DOUBLE PRECISION RelaxD
        REAL RelaxO
        REAL D1,D2,D3,F
C
C LOCAL
C
        DOUBLE PRECISION DAB, DBC, FI(3), THETA, X(4)
        REAL fmb,fi2(3),totflow
        INTEGER WrongL

C
C
C DETERMINE THREE POINTS FOR THE INTEGRATED FUNCTION
C
        WrongL=0
        X(1) = 0.D0
        totflow=0.0

        CALL QSUM1(FI(1),X(1), .TRUE.,Nl,
C@tno jcp 1996Apr07_15:41:14 Ltyp deleted
     &          DifLim,FromTo,Lstat,Mf,Mp,pLiLDat,Ldat,RhoL,
     &          SQRRhoL,MuL,PzNew,DpL,Nz,pzold,corr,fmb,totflow,
C@tno jcp 1996Apr07_15:47:12 ctl removed
     &          FmFact,DMZ,linInit,RSqrtDff)

        FI(1) = 0.D0
        fi2(1)=fmb
C
        totflow=0.0
        X(2) = 0.5D0

        CALL QSUM1(FI(2),X(2), .FALSE.,Nl,
C@tno jcp 1996Apr07_15:41:14 Ltyp deleted
     &          DifLim,FromTo,Lstat,Mf,Mp,pLiLDat,Ldat,RhoL,
     &          SQRRhoL,MuL,PzNew,DpL,Nz,Pzold,corr,fmb,totflow,
C@tno jcp 1996Apr07_15:47:12 ctl removed
     &          FmFact,DMZ,linInit,RSqrtDff)

        fi2(2)=fmb-fi2(1)
C
        totflow=0.0
        X(3) = 1.D0

        CALL QSUM1(FI(3),X(3), .FALSE.,Nl,
C@tno jcp 1996Apr07_15:41:14 Ltyp deleted
     &          DifLim,FromTo,Lstat,Mf,Mp,pLiLDat,Ldat,RhoL,
     &          SQRRhoL,MuL,PzNew,DpL,Nz,pzold,corr,fmb,totflow,
C@tno jcp 1996Apr07_15:47:12 ctl removed
     &          FmFact,DMZ,linInit,RSqrtDff)

        fi2(3)=fmb-fi2(1)
C       fi2(1)=0
C check if curve has minimum or maximum
        IF (2.0D0*FI(2) .GT. FI(3)) THEN
C                                                   you will find a maximum now
          X(4)=1.0D0
        ELSE
C
C DETERMINE MINIMUM POINT OF FITTED POLYNOMIAL
C
          DAB = (FI(2) - FI(1)) / (X(2) - X(1))
          DBC = (FI(3) - FI(2)) / (X(3) - X(2))
C
          IF (DAB.NE.DBC) THEN
            THETA = DAB / (DAB - DBC)
            X(4) = 0.5D0 * (
     &      (X(1) + X(2)) * (1.D0 - THETA) +
     &      (X(2) + X(3)) * THETA)
          ELSE
            WRITE(CRT,*) 'DAB=DBC I take Lambda= 1.0'
            X(4)=1.D0
          ENDIF
        ENDIF
C in fact we want to check the range of FmaI too, IF it is larger than
C 10E10 THEN take X(4)=1

        RelaxO=X(4)
        X(4)=MIN(1.D0,(MAX(0.01D0,X(4))))
        RelaxD=X(4)

        IF ((test.ge.1 .AND. secho.ge.2) .or. WrongL.eq.1) THEN
C line changed to result user units  i.e. kg/h
          f=FmFact
          d1=f*FI(1)
          d2=f*FI(2)
          d3=f*FI(3)

          WRITE(CRT,*) 'FI=', d1,d2,d3
        ENDIF
C
C DETERMINE NEW PRESSURE VECTOR
C
        CALL NEWPR (Nz, PzNew, PZOLD, CORR, X(4))
C
        RETURN
        END
C
C
C
Ch***********************************************************************
        SUBROUTINE QSUM1(Qsum,SCALAR, SETREF, Nl,
C                         1     2      3        4
C@tno jcp 1996Apr07_15:39:13 Ltyp deleted
     &  DifLim,FromTo,Lstat,Mf,Mp,pLiLDat,Ldat,RhoL,
C        5      6      7    8   9    10   11   12
     &  SQRRhoL,MuL,PzNew,DpL,Nz,pzold,corr,fmb,totflow,
C         13    14   15   16  17  18    19   20    21
C@tno jcp 1996Apr07_15:47:52 ctl removed
     &  FmFact,DMZ,linInit,RSqrtDff)
C         22    23  24     25
*
C***********************************************************************
C Purpose: CALCULATE A SUM TO MINIMIZE,
C          THE GRADIENT OF THE SUM REPRESENTS THE FLOW BALANCE
C          VECTOR. SET REFERENCE 'SQRef' IF 'SETREF'.
C Module :
C Changes:linInit added to the call 1990dec28 jcp and in the call of FEQN2
C@lbl bvs 1995Jun DDiflim changed to XDiflim (conflict with Default Diflim)
C added COMMON block for SQRef
C DDifLim DOUBLE PRECISION
C to avoid warnings parameters just used
C@empa aw 1991July11 RSqrtDff added
C@empa aw 1993may26 COMMON /SQR/ canceled because it was used only in this
C@empa              subroutine, which doesn't make any sense (no transfer of
C@empa              the values to any other subroutine, not even between the
C@empa              successive calls of this routine).
C@empa              To do the latter, I save SQRef and Lamflg.
C@lbl dml 1999nov19 Change integer variable initFlg to logical linInit.
C Limits :
C
C       DMZ        [kg/s]         changes of the Mass per Zone, (ie temperature)
C       CtL(2,MaxL)  -            correction for Cm , pos and neg flow
Ch***********************************************************************

        IMPLICIT NONE
        INCLUDE 'comv-uni.inc'
        include 'comv-par.inc'
C
C PASSED
C
        INTEGER Nl,FromTo(2,maxl),Lstat(maxl),pLiLDat(maxl),Nz
        logical linInit
        REAL Mf(maxl),Mp(maxl),Ldat(maxd),RhoL(2,maxl),SQRRhoL(2,maxl)
        REAL MuL(2,maxl),diflim,
     &  fmb,totflow,fmfact
C@empa aw 1997sep18 DpL as DOUBLE PRECISION
C@empa aw 2002dec04 DpL enlarged from DpL(2,MaxL) to DpL(3,MaxL) to have a space
C@empa aw 2002dec04 for the final pressure differnce
        DOUBLE PRECISION DpL(3,MaxL)
        REAL DMZ(maxz)
        REAL RSqrtDff(maxl)

        DOUBLE PRECISION Qsum,SCALAR,PzNew(maxz),pzold(maxz),corr(maxz)
        LOGICAL SETREF
C
C LOCAL
C
        DOUBLE PRECISION SQRef
        INTEGER LamFlg(maxl)
        SAVE SQRef,Lamflg
        INTEGER I,START,From,To
        DOUBLE PRECISION Dp,FmaI,Fma,
     &  SQ, PNEXT(maxz)
        DOUBLE PRECISION XDifLim
        REAL fmba(maxz)
        INTEGER L

        Pznew(1)=Pznew(1)

C incoming flows are counted positive in FMB . When DMZ is positive in time,
C more air has to enter the zone to balance that. So in that case FMB has to
C start negative.
        DO 1 I=1,Nz
        fmba(i)=-DMZ(i)
1       CONTINUE
        XDifLim=DifLim
C
C
C CALCULATE CURRENT PRESSURE VECTOR (SCALAR 0.0, 0.5, AND 1.0)
C
        CALL NEWPR (Nz, PNEXT,PzOld, CORR, SCALAR)
C
        SQ = 0.D0
        IF (SETREF) THEN
         DO 2 I=1,nl
          LamFlg(I)=0
2        CONTINUE
        ENDIF
C
        DO 100 I = 1, NL
C
        FROM=FromTo(1,I)
        TO=FromTo(2,I)

        L=Lstat(I)
        IF (L.EQ.0) THEN
        Dp=PNext(FROM)-PNext(TO)+DpL(1,I)
        ELSE IF (L.EQ.3 .OR. L.EQ.6) THEN
        Dp=PNext(FROM)+DpL(1,I)
        ELSE IF (L.LT.3 ) THEN
        Dp=-PNext(TO)+DpL(1,I)
        ENDIF

C DpL(1,I) is only valid for positive Dp, IF Dp<0 THEN, correct

        IF (Dp .LT. 0.0) THEN
         Dp=Dp-DpL(1,I)+DpL(2,I)
        IF (Dp .GT. 0.0) THEN

C we have a chimney on a cold house and hot weather outside. Probably we
C will get stratIFication here. None of the thermal pressure differences
C in the link makes sense. This is a stable situation.
C A warm house and cold weather could give a bi-stable flow in the duct.
C Once backdraft occurs the chimney will cool further giving more backdraft.
C Once warm air from the house flows out, it heats up the duct and gives a
C more stable outflow. This fenomenon cannot be simulated accurately by the
C used method, that first assumes positive flow. A difference can occur when
C modelling ducts to outside positive ("From" inside "To" outside) in comparison
C with ducts to the inside positive ("From" outside "To" inside).

        dp=dp+.5*DpL(1,I)-.5*DpL(2,I)

        ENDIF
        ENDIF

C Start points at the first data element of this link in LDat(*)

        Start=pLiLDat(I)

C
C SET 'DIFRef' IF SCALAR EQUAL TO ZERO
C
        IF (SETREF .AND. ABS(dp).LT.XDifLim) THEN
            LamFlg(I) = 1
        ENDIF

C
C DETERMINE THE CURRENT VALUE, SQ, OF THE INTEGRAL FUNCTION
C

        CALL FEQN2(FmaI,Fma,Dp,LDat(Start),
     &          Mf(I),Mp(I),RhoL(1,I),RhoL(2,i),
     &          sqrRhoL(1,I),sqrRhoL(2,I),
     &          Mul(1,I),Mul(2,I),LamFlg(I),DifLim,fmfact,
     &          linInit,RSqrtDff(I))

        L=Lstat(I)
        IF (L.EQ.0) THEN
        FMBa(FROM)=FMBa(FROM)-Fma
        FMBa(TO)=FMBa(TO)+Fma

        ELSE IF (L.EQ.3 .OR. L.eq.6) THEN
        FMBa(FROM)=FMBa(FROM)-Fma

        ELSE IF (L.LT.3) THEN
        FMBa(TO)=FMBa(TO)+Fma
        ENDIF

        totflow=totflow+ABS(fma)

        IF (test.ge.1 .AND. secho.ge.4) THEN
          WRITE (CRT,1001) 'I',I,'Dp',Dp,'FmaI',FmaI*fmfact,
     &          'lamflg',Lamflg(i),'fma=',Fma
1001      FORMAT(1X, A2, I4, A3, E10.3, A5, E10.3, A7, I2, A4, E10.3)
        ENDIF
        SQ=SQ+FmaI
100     CONTINUE
C
C SET REFERENCE VALUE SQRef OR SUBSTRACT REFERENCE VALUE
C
        IF (SETREF) THEN
          SQRef = SQ
        ELSE
          SQ = SQ - SQRef
        ENDIF
C
C RETURN CURRENT VALUE OF INTEGRAL FUNCTION
C
        QSUM = SQ
C
C determine the total flowbalance error
        fmb=0.0
        DO 200 I=1,Nz
        fmb=fmb+ABS(fmba(I))
200     CONTINUE
        RETURN
        END


Ch***********************************************************************
        SUBROUTINE FEQN2(FmaI,Fma,Dp,DAT,Mf1,Mp1,
C pass parameter # =      1    2   3  4  5    6
     &RhoL1,RhoL2,sqrRho1,sqrRho2,
C      7      8     9       10
     &Mul1,Mul2,LamFlg,DifLim,FmFact,linInit,RSqrtDff)
C     11    12    13     14     15     16       17      18

C***********************************************************************
C Purpose: FEQN2 calculates FmaI  for different types of links.
C
C Module : 4.2.x, TG IV, hcp/ jun 19, 1989
C Changes: aw/ 22.10.1990  crack, powerlaw:
C@empa     aw/ 22.10.1990  crack, powerlaw:
C@empa      - ExpN is set to 0.5 if ExpN is less or equal to 0.5.
C@empa      - Mf1 is taken into account in the calculation of cm. That means
C@empa        it has not to appear in the calculation of Fma.
C@empa      - Massflow Fma has also to be calculated if Dp < DifLim.
C@empa      AW/ 7.11.90 Fan:
C@empa      - NFO is equal to the multiplication factor Mf1.
C@empa      - Parameters in the call of FAN2 corrected.
C 1990dec28 jcp linInit added to the call
C added to match CONVFAN     INTEGER RETURN1,Key
C Absolute value of Dp is used more than once
C this caused the error with the solver0 at negative
C wrong elements of Dat for RhoI and NfI
C@empa aw 1991july09 Parameter RSqrtDff added
C@empa aw 1991july09 call DWC2 added
C@empa aw 1993jun09 error return from CONVFAN and Key canceled
C@lbl dml 1999nov19 Change integer variable initFlg to logical linInit.
C
C Limits : only for crackflow; not all equations are in here.
C
C Pass parameters:
C
C IO # Name    unit       description
C O  1 FmaI    (kg/s*Pa)  integral of the mass flow through the link
C I  2 Dp      (Pa)       pressure difference accross the link
C I  3 DAT(*)  multi      data about flow coefficients
C I  4 Mf1     -          the output is multiplied with Mf1
C I  5 Mp1     -          the input pressure Dp is first multiplied with Mp1
C I  6 RhoL1   kg/m3      the air density for positive flow
C I  7 RhoL2   kg/m3      the air density for negative flow
C I  8 SQRRho1            SQRT of RhoL positive flow
C I  9 SQRRho2            SQRT of RhoL negative flow
C I  0 Mul1    Pa.s       the air viscosity for positive and negative flow
C I  1 Mul2    Pa.s       the air viscosity for positive and negative flow
C I  2 LamFlg  -          =1 for links linearized at relax=0
C I  3 DifLim Pa          pressure limit to linearize crackflow
C      FmFact
C      CtL1    -          correction for Cm , pos flow
C      CtL2    -          correction for Cm , neg flow
C I    linInit -          Linear initialization flag sets Expn to 1.
C I    RSqrtDff -         1/sqrt (Darcy friction factor)
C
C The general form of the DAT(*) array is:
C ---------------------------------------
C DAT(1)=pointer used for selection of the proper formulas for the link
C     1= crack       2= fan          3= straight duct      4=duct fitting
C     5= flowcontroller ideal     symmetric
C     6= flowcontroller ideal     non-symmetric
C     7= flowcontroller non-ideal symmetric
C     8= flowcontroller non-ideal non-symmetric
C     9= large opening
C    10= test data (measured curve) Fma=f(Dp)
C    11= Airnet powerlaw element according Walton in AIRNET
C   (12= Duct A,B,C              according Walton in AIRNET ) ??
C    14= Thermostatic vent (TV)
C
C DAT(2)= first coefficient for calculating Fma from Dp
C DAT(3)= second ,,, and so on.
C
C Note that in the CALL of FEQN the DAT(*) array is rotated so DAT(1) is REALLY
C the first data belonging to this link
C
C The maximum number of coefficients per link may vary from type to type.
C This has to be defined in input and this routine.
C    A simple crack may have 5 coefficients
C    A fan probably     5 to 15
C    A duct            10 to 15
C    A flow controller up to 30
C    A measured curve        ?
C    A large opening    5 to 10
C
C (Geometrical) Subelements maybe connected to a link. Those could have an
C influence on the flow.
C
C example:
C call
Ch***********************************************************************

        IMPLICIT NONE
        INCLUDE 'comv-uni.inc'
        INCLUDE 'comv-par.inc'

C At the moment there are a lot of variables declared
C       which are not used. These variables will be used after implementing
C       the ducts into the program. So, this comment should be deleted after
C       the ducts are implemented into the program.

C@empa aw 1995nov28 none of the following is used
CC        REAL CA,NA,C(0:20)
CC        DOUBLE PRECISION XX1,XX3,XX5

        DOUBLE PRECISION FmaI,Dp,Fma
        REAL Dat(*),Mf1,Mp1
        REAL RhoL1,sqrRho1,Mul1
        REAL RhoL2,sqrRho2,Mul2
        REAL Fmal,DifLim,Fmfact
        REAL CtL1,Ctl2
        REAL RSqrtDff

        REAL PCor,Rho,NFO
        INTEGER lamflg
        logical linInit
        DOUBLE PRECISION dCm,dCs,dExpn,dL,xDifLim,dMf1,dCtl1,dCtl2,ADp
        REAL Fratio,DFma,fma1,fma2


        ADp=DABS(Dp)

C 1=crack  This part has to be replaced by Liu's routine
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  1=crack  %%%%%%%%%%%%%%
        IF (Dat(1)  .eq.1) THEN

C powerlaw

C crack Dat(1)=1
C Dat(2)=Cs
C Dat(3)=expN
C Dat(4)=length

        dCs=dat(2)
        dExpn=Dat(3)
        dL=Dat(4)
        dMf1=Mf1
C Ctl1,Ctl2 had no value. For the moment=1.0 !!!!
        Ctl1=1.0
        Ctl2=1.0

        dCtl1=Ctl1
        dCtl2=Ctl2
        xDifLim=DifLim

        dCm=dMf1/dL*dCs
        dExpn=dat(3)


        IF (LamFlg.NE.1) THEN
C                                                               not laminar
          IF (dExpN .LE. 0.5) THEN
C                                               n=0.5   SQRT
            IF (dp .GT. 0.0D0) THEN
              Fma=dCtL1*dCm*SQRT(Dp)
            ELSE
              Fma=-dCtL2*dCm*SQRT(ADp)
            ENDIF
C Note that FmaI is always positive
             FmaI=Fma*Dp/1.5
          ELSE
C                                               n<>0.5  **n
            IF (dp .GT. 0.0D0) THEN
              Fma=dCtL1*dcm*(Dp)**dExpn
            ELSE
              Fma=-dCtL2*dcm*(ADp)**dExpn
            ENDIF
            FmaI=Fma*Dp/(dExpN+1.0D0)
          ENDIF

        ELSE
C                                                               laminar
C calculate a 'laminar value' for dCm
          IF (dExpN .LE. 0.5) THEN
            dCm=SQRT(xDifLim)/xDifLim*dCm
          ELSE
            dCm=xDifLim**dExpN/xDifLim*dCm
          ENDIF

          If (Dp .GT. 0.0D0) THEN
            Fma=dCtl1*dCm*Dp
          ELSE
            Fma=dCtl2*dCm*Dp
          ENDIF

          FmaI=Fma*Dp/2.0D0

        ENDIF

        CALL fmaTofma12(fma,fma1,fma2)

C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  2=fan    %%%%%%%%%%%%%%
        ELSE IF(Dat(1)  .eq.2) THEN
        IF (Mp1.GT.0) THEN
C assume that the fan is blowing harder than its possible opposite
C pressure
          Rho=RhoL1
        ELSE
          Rho=RhoL2
        ENDIF

        NFO=Mf1
        CALL ConvFan(PCor,1.0,Dat(4),Rho,Dat(5),NFO,
     &        1.0,Fratio)
          CALL FAN2(FmaI,Fma,DFma,Dp,DAT,Fratio,Mp1*PCor)
        IF (fma*Mp1.LT.0) THEN
          IF (Mp1.LE.0) THEN
            Rho=RhoL1
          ELSE
            Rho=RhoL2
          ENDIF

          CALL ConvFan(PCor,1.0,Dat(4),Rho,Dat(5),NFO,
     &        1.0,Fratio)

          CALL fan2(FmaI,Fma,DFma,Dp,DAT,Fratio,Mp1*PCor)
        ENDIF

C Depending on input unit 9, the flow through the fan is calculated
C in m3/s or m3/h. Since in the solver subroutines we are using m3/s
C for the calulation, we have to convert the flow into m3/s. Therefore
C the subroutine convfa is introduced.

C Split the flow through the link into two flows.
        CALL fmaTofma12(fma,fma1,fma2)


C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  3=duct   %%%%%%%%%%%%%%
        ELSE IF(Dat(1)  .eq.3) THEN
             CALL DWC2(FmaI,Fma,Dp,Dat,Rhol1,Rhol2,Mul1,Mul2,linInit,
     &       RSqrtDff)


C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  4=duct fitting %%%%%%%%

C       ELSE IF(Dat(1)  .eq.4) THEN

C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  5=flow controller %%%%%
C                                                     ideal symmetric
C@empa aw 1995nov28 All routines for flowcontrollers are canceled here.
C@ If in the future once it will be decided to keep solver 0 (optimum relax),
C@ then these routines have to be rewritten according to the corresponding
C@ new routines in COMV-FLO called in FEQN.

        ELSE IF(Dat(1)  .eq.5) THEN
CC        CALL FLCON1I(DAT,NA,CA,
CC     &        Fma,SNGL(FmaI),Dp,Mf1,Mp1)

C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  6=flow controller %%%%%
C                                                     ideal nonsymmetric
        ELSE IF(Dat(1)  .eq.6) THEN
CC        CALL FLCON2I(DAT,NA,CA,
CC     &        Fma,SNGL(FmaI),Dp,Mf1,Mp1)

C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  2=flow controller %%%%%
C                                                     nonideal symmetric
        ELSE IF(Dat(1)  .eq.7) THEN
CC        CALL FLCON3I(DAT,
CC     &        C,NA,CA,Fma,SNGL(FmaI),Dp,XX1,Mf1,Mp1)

C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  8=flow controller %%%%%%
C                                                     nonideal nonsymmetric
        ELSE IF(Dat(1)  .eq.8) THEN
CC        CALL FLCON4I(DAT,
CC     &        C,NA,CA,Fma,SNGL(FmaI),Dp,XX3,XX5,Mf1,Mp1)

C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  9=large opening %%%%%%%

C       ELSE IF(Dat(1)  .eq.9) THEN

C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 10=test data     %%%%%%%

C       ELSE IF(Dat(1)  .eq.10) THEN

C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 11=walton powerlaw %%%%%
C 11= George Waltons prl element
        ELSE IF(Dat(1)  .eq.11) THEN

C Dat(2)=initialization coefficient
C Dat(3)=laminar flow coefficient
C Dat(4)= Cm
C Dat(5)= ExpN

        IF ( linInit ) THEN
          IF (dp.GT.0.0) THEN
            DFma=Mf1*dat(2)*RhoL1/MuL1
          ELSE
            DFma=Mf1*dat(2)*RhoL2/MuL2
          ENDIF
          Fma=+DFma*dp
        ELSE

C no initialization

          IF (dp.GT.0.0) THEN

C prl positive

            Fma=Mf1*Dat(4)*sqrRho1*Dp**Dat(5)

C linear DFma

            DFma=Mf1*dat(3)*RhoL1/MuL1
          ELSE

C prl negative

            Fma=-Mf1*Dat(4)*sqrRho2*(-Dp)**Dat(5)

C linear DFma

            DFma=Mf1*dat(3)*RhoL2/MuL2
          ENDIF

C linear Fmal

          Fmal=+DFma*dp

          IF (ABS(Fmal).LE.ABS(Fma)) THEN

C we are in the laminar region

            Fma=Fmal
          ELSE

C we are in the prl region and dp cannot be zero

            DFma=Fma*Dat(5)/Dp

          ENDIF
        ENDIF

C       WRITE(CRT,*) 'dp,Fma,DFma=',dp,Fma,DFma
C Duct with A,B,C
C     ELSE IF(Dat(1)  .eq.11) THEN
C     ELSE IF(Dat(1)  .eq.12) THEN
C     ELSE IF(Dat(1)  .eq.13) THEN


        ENDIF
        IF (test.ge.1 .AND. oecho.ge.4) THEN
        WRITE(CRT,1001) 'Fma=',Fma*fmfact,' DFma=',DFma
        ENDIF
        RETURN

1001    FORMAT (1X,A4,E10.3,A6,E10.3)
        END


Ch***********************************************************************
         SUBROUTINE Fan2(FmaI,Fma,DFma,Dp,Dat,Mf1,Mp1)
C pass parameter # =  1   2    3  4   5   6
C***********************************************************************
C Purpose: fan2 calculates the flowrate FmaI through a fan at a given Dp
C          Keep in mind that fandata is given in the first quadrant,
C          but in a link the curve will appear in the third quadrant. Because
C          the linkpressure is defined opposite to the fan pressure rise.
C          The fan data is given in m3/s to obtain the polynomial. So infact
C          the polynomial RETURNs the Fva. In Mf1 we put the conversion factor
C          for both: NFO/NFI times Fma/Fva. Therefore the returned value is
C          Fma at the fanspeed NFO.
C
C          It has a polynomial approximation from the pressures Pmin=Dat(8)
C          to Pmax=Dat(9) (Pa) . The form is:
C                C0         C1            C2                  Cpower
C          Fva= Dat(12)+DpF*Dat(13)+DpF**2*Dat(14),...DpF**power*Dat(power+12)
C          where the value of power is in Dat(3) and DpF=-Dp*Mp1
C
C          Outside the range Dat(8) to Dat(9) Pa the Fma is calculated from:
C               slope    intercept
C          Fva= Dat(10)*DpF+Dat(11)   ... kg/s
C
C          The Fma and DpF has to be corrected according to the routine Rhofan.
C          The conditions of the fandata at which the coefficients are calcu
C          lated are in Dat(5)=NFI and Dat(4)=RhoI
C          A practical maximum highest power seems to be 6 for fan curves.
C A summary of the contents of Dat(*):
C Dat(1)=2 means this link is a fan
C Dat(2)= Flag is needed for COMIN and not for COMIS
C Dat(3)= highest power in the polynomial, n
C Dat(4)= airdensity belonging to the fandata which gave these coefficients
C Dat(5)= fanspeed belonging to the fandata which gave these coefficients
C Dat(6)= Cm if the fan is off
C Dat(7)= Expn if the fan is off
C Dat(8)= Pmin range where the polynomial is valid
C Dat(9)= Pmax  ,,    ,,
C Dat(10)= slope for extrapolation
C Dat(11)= intercept for extrapolation
C Dat(12)= C0 polynomial coefficient
C Dat(13)= C1......
C Dat(12+Dat(3))=Cn
C
C Module : 4.2.3, TG IV, hcp/february 14,1989
C Version: 20. march 1991
C Changes:
C inserted fanspeed and Rho in Dat(6) and Dat(7)
C@empa      AW/31.10.90 Order of the elements in Dat according to the order
C@empa      of the elemnts in LDat in the call.
C@empa      AW/1.11.90, integral of the mass flow
C@empa      AW/1.11.90 Brackets inserted in the equation for Fma outside the
C@empa      normal operation range (slope, intercept)
C line taken from FAN in COMV-FEQ.FOR
C endif moved down
C Dat(j+13 must be Dat(j+12)
C Mp1 added
C also here is need for a Mp1 correction
C I think Sign(Dpf) is wrong here, but Mp1 correction
C Limits :check the quality of the polynomial approximation.
C         as the curve is made as Fma=f(dP) there can be no contra flexture,
C         so IF you have a fan curve that has a local minimum (Pfan=f(Fma)),
C         then draw the maximum pressure rise just higher than the highest
C         point in the curve and keep it monotone descending for increasing
C         Fma.
C
C Pass parameters:
C
C IO # Name    unit        description
C O  1 FmaI    (kg)        Integral of mass flow throu the fan
C O  1 Fma     (kg/s)      mass flowrate
C O  2 DFma    (kg/s/Pa)   derivative (d Fma)/(d Dp)
C I  3 Dp      (Pa)        pressure difference of the link
C I  4 Dat(*)  multi       array with coefficients for this fan
C I  5 Mf1     -           multiplication factor for the fan output flowrate
C I  6 Mp1     -           multiplication factor for the fan input pressure DpF.
C              To scale the fan in this link to one with a higher pressure rise,
C              Mp1 should be smaller than 1. THEN the same curve is used.
C
C example:
C call
Ch***********************************************************************

        IMPLICIT NONE
        REAL DFma,Dat(*),Mf1,Mp1
        DOUBLE PRECISION Dp,Fma,DpF,FmaI

        INTEGER j

C The pressure in the fancurve is always defined
C opposite to the definition of Dp. Therefore we
C use DpF=-....


        DpF=-Mp1*Dp

        IF ((Dpf.LT.dat(8)).or.(Dpf.GT.dat(9)))THEN

          Fma=Mf1*(Dat(10)*DpF+Dat(11))

C the - sign is to convert from the fancurves df/dp to the df/dp of the linkflow

          DFma=-Dat(10)*Mf1*Mp1
          FmaI=-Mf1*(Dat(10)/2*DpF**2+Dat(11)*DpF)/Mp1

        ELSE

          Fma=0.0
          DO 100 j=Dat(3),1,-1
            Fma=(Fma+Dat(j+12))*DpF
100       CONTINUE
C now add C0
          Fma=Mf1*(Fma+Dat(12))

C DFma=   C1*SIGN(1,DpF)      +2*C2*DpF .....

          DFma=0.0
          DO 110 j=Dat(3),2,-1
            DFma=(DFma+j*Dat(j+12))*DpF
110       CONTINUE

C the - sign is to convert from the fancurves df/dp to the df/dp of the linkflow

          DFma=-Mf1*(DFma+Dat(13))*Mp1

C Calculation of FmaI in the normal operation range inserted
C FmaI = C0*DpF+C1/2*DpF**2+C2/3*DpF**3....Cpower/(power+1)*DpF**(power+1)

          FmaI=0.0
          DO 120 j=Dat(3),0,-1
            FmaI=(FmaI+Dat(j+12)/(j+1))*DpF
120       CONTINUE
          FmaI=-Mf1*FmaI/Mp1

        ENDIF

        RETURN
        END
C**********************************************************************
        SUBROUTINE DWC2(FmaI,Fma,Dp,Dat,RhoL1,RhoL2,MuL1,MuL2,linInit,
C pass parameter #       1   2    3  4   5     6     7    8    9
     &  RSqrtDff)
C       10
C**********************************************************************
C
C Purpose: This is the routine from Walton for the dwc element.
C          Darcy-Weisbach pipe/duct model using Colebrook equation for the
C          turbulent friction factor.
C
C Module:  empa aw 1991July04
C
C Pass parameters:
C IO # Name    unit name  description
C                   (Walton)
C O  1 FmaI    (kg/s*Pa)  integral of the massflow throu the link
C O  2 Fma     (kg/s)     mass flow through the link
C I  3 Dp      (Pa)       pressure difference accross the link
C I  4 DAT(*)  multi      data about the duct
C I  5 RhoL1   kg/m3      the air density for positive flow
C I  6 RhoL2   kg/m3      the air density for negative flow
C I  7 Mul1    Pa.s       the air viscosity for positive and negative flow
C I  8 Mul2    Pa.s       the air viscosity for positive and negative flow
C I  9 linInit   -        linear initialiszation flag
C I 10 RSqrtDff       g   1/sqrt(Darcy friction factor)
C
C Contents of the array Dat(*):
C
C      Dat(1)=3 -         means this is a staight duct
C      Dat(2)   m         Diam1
C      Dat(3)   m         Diam2 (round duct if only Diam1 is given)
C      Dat(4)   mm   e    roughness dimension
C      Dat(5)   m    L    Lduct length of the duct
C      Dat(6)   -    C    Zeta of the DS and the DF given in Dat(7)
C      Dat(7)   -         Type duct fitting
C      Dat(8)   -         Parameter 1 of the fitting
C      Dat(9)   -         Parameter 2 of the fitting
C      Dat(10)  m    D    hydraulic diameter
C      Dat(11)  m2   A    cross sectional area
C      Dat(12)  -    k    laminar friction loss coefficient
C      Dat(13)  -    K    laminar dynamic loss coefficient
C      Dat(14)  -         laminar initialization coefficient
C      Dat(15)  -    L/D  relative length,
C      Dat(16)  -    a    1.14 - 0.868589*ln(e/D)
C
C  Local:
C@lbl rw 1992may13 RE is not used at the moment
C     RE      - Reynolds number.
C     FL      - friction factor for laminar flow.
C     FT      - friction factor for turbulent flow.
C@lbl dml 1999nov19 Change integer variable initFlg to logical linInit.
C
Ch**********************************************************************

        IMPLICIT NONE
        logical  linInit
        REAL     Dat(*), DFma
        REAL     RhoL1, Rhol2, MuL1, MuL2
        REAL     RSqrtDff
        REAL     A0, A1, A2, B, C, D, EPS, S2, CDM, FL, FT, FTT
        DOUBLE PRECISION Dp, Fma, FmaI
C
C     CODE  ************************************************************
C
        DATA C,EPS / 0.868589, 0.001 /
        IF (RSqrtDff.EQ.0.0) THEN
C                              Initialisation of RSqrtDff
        RSqrtDff=Dat(16)
        ENDIF

        IF( linInit ) THEN
C                              Initialization by linear relation.
        IF(Dp.GE.0.0) THEN
          DFma = (2.*RhoL1*Dat(11)*Dat(10))/
     &    (MuL1*Dat(14)*Dat(15)*Dat(6))
        ELSE
          DFma = (2.*RhoL2*Dat(11)*Dat(10))/
     &    (MuL2*Dat(14)*Dat(15)*Dat(6))
        END IF
        Fma = DFma*Dp
        FmaI= Fma*Dp/2.0D0
        ELSE
C                              Standard calculation.
        IF(Dp.GE.0.0) THEN
C                                Flow in positive direction.
C                                  Laminar flow. Dat(13)=0
          IF(Dat(13).GE.0.001) THEN
            A2 = Dat(13)/(2.*RhoL1*Dat(11)*Dat(11))
            A1 = (MuL1*Dat(12)*Dat(15))/(2.*RhoL1*Dat(11)*Dat(10))
            A0 = -Dp
            CDM = SQRT(A1*A1-4.*A2*A0)
            FL = (CDM-A1)/(2.*A2)
            CDM = 1.0/CDM
          ELSE
            CDM = (2.*RhoL1*Dat(11)*Dat(10))/(MuL1*Dat(12)*Dat(15))
            FL = CDM*Dp
          END IF

C                                            Turbulent flow!
           IF(Dp.NE.0) THEN
            S2 = SQRT(2.*RhoL1*Dp)*Dat(11)
            FTT = S2 / SQRT(Dat(15)/RSqrtDff**2+Dat(6))
10            CONTINUE
              FT = FTT
              B = (9.3*MuL1*Dat(11))/(FT*Dat(4)/1000)
              D = 1.0 + RSqrtDff*B
C  Quit iteration loop
              IF (D.LE.0.0)  GOTO 11
              RSqrtDff = RSqrtDff-(RSqrtDff- Dat(16)+LOG(D))/(1.0+C*B/D)
              FTT = S2 / SQRT(Dat(15)/RSqrtDff**2+Dat(6))
              IF(ABS(FTT-FT)/FTT .GE. EPS) GO TO 10
            FT = FTT
            GOTO 12
11          CONTINUE
            FT = 0.0
12          CONTINUE
           ELSE
            FT = 0.0
           ENDIF
        ELSE
C                                Flow in negative direction.
C                                  Laminar flow. Dat(13)!=0
          IF(Dat(13).GE.0.001) THEN
            A2 = Dat(13)/(2.*RhoL2*Dat(11)*Dat(11))
            A1 = (MuL2*Dat(12)*Dat(15))/(2.*RhoL2*Dat(11)*Dat(10))
            A0 = Dp
            CDM = SQRT(A1*A1-4.*A2*A0)
            FL = -(CDM-A1)/(2.*A2)
            CDM = 1.0/CDM
          ELSE
            CDM = (2.*RhoL2*Dat(11)*Dat(10))/(MuL2*Dat(12)*Dat(15))
            FL = CDM*Dp
          END IF

C                                                Turbulent flow!
           IF(Dp.NE.0.0) THEN
            S2 = SQRT(-2.*RhoL2*Dp)*Dat(11)
            FTT = S2 / SQRT(Dat(15)/RSqrtDff**2+Dat(6))
20            CONTINUE
              FT = FTT
              B = (9.3*MuL2*Dat(11))/(FT*Dat(4)/1000)
              D = 1.0 + RSqrtDff*B
C     Quit iteration loop
              IF (D.LE.0.0)  GOTO 21
              RSqrtDff=RSqrtDff-(RSqrtDff-Dat(16)+C*LOG(D))/(1.0+C*B/D)
              FTT = S2 / SQRT(Dat(15)/RSqrtDff**2+Dat(6))
              IF(ABS(FTT-FT)/FTT .GE. EPS) GO TO 20
            FT = -FTT
            GOTO 22
21          CONTINUE
            FT = 0.0
22          CONTINUE
           ELSE
            FT = 0.0
           ENDIF
        END IF

C                                Select laminar or turbulent flow.

        IF((ABS(FL).LE.ABS(FT)).OR.(ABS(FT).EQ.0.0)) THEN
          Fma = FL
          DFma = CDM
          FmaI=Fma*Dp/2.0D0
        ELSE
          Fma = FT
          DFma = 0.5*FT/Dp
          FmaI=Fma*Dp/1.5D0
        END IF
        END IF
C
        RETURN
        END
