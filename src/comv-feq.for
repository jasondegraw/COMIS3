C+*********************************************************** comv-feq.f
Ch***********************************************************************
      SUBROUTINE FEQN(Fma,DFma,Dfma1,Dfma2,Dp,DAT,Mf1,Mp1,
     & RhoL1,RhoL2,sqrRho1,sqrRho2,
     & Mul1,Mul2,linInit,Fma1,Fma2,DifLim,FmFact,RSqrtDff,
     & SLinkDat,
     & Tl, Lstat, TempL1, TempL2, normCrRho, normCrMu,
C@tno jcp 1996Jun13_23:18:26 for PS Vmet and RhoOut
     & DpProfNew,RhoProfF,RhoProfT,ILink,FVnew,iRFlist,Vmet,RhoOut)
C***********************************************************************
C Purpose: FEQN calculates Fma and DFma for different types of links.
C
C Module : 4.2, TG IV, hcp/ feb 18, 1989
C Version: COMV1-4.LARG EMPA  VD 20.Jan 92
C
C           CAUTION : THIS ROUTINE IS UNDER DEVELOPMENT for
C                     large opening integration
C
C Changes:
C powerlaw element George Walton added as type 11
C numerical derivative if dp=0
C fma1,fma2 added for large openings
C@empa        AW/ oct 1990:  crack, powerlaw: Fma=CtL*Cm*SQRT(ABS(Dp))
C@empa               and          DFma=Fma*0.5/Dp
C@empa      VD/ 25.oct 90  Numbering of linktyp pointer Dat(1) adjusted
C@empa                     according user guide draft.
C@empa               if Dat(3) is less than or equal to 0.5.
C@empa        AW/ 6.nov.90   Fan:-NFO=Mf1
C@empa                NFO  fan rotational speed under consideration)
C@empa                Mf1  multiplication factor (from linkdata)
C@empa                   -Parameterlist in the call of FAN corrected
C@empa                according to the outputparameters of CONVFAN.
C@empa aw 1991july09 Parameter RSqrtDff added
C@empa aw 1991july09 call DWC added
C@lbl hef 1991jul24  if(test.ge.1) write ...
C@empa vd 1992jan20  Integration of Window type
C@empa aw 1992jan23 parameters for crack temp. coefficient added
C@empa aw 1992apr21 the actual crack length in m is just Mf1
C@lbl rw  1992jul24 added iunit (is used in the fan section to convert
C     the volume flow through the fan into m3/s)
C@empa aw 1992dec10 fma1lo,fma2lo,dfma1,dfma2
C@empa aw 1992dec10 fma1lo, fma2lo are DOUBLE PRECISIONs fma1,fma2 REALs
C@empa aw 1993jun09 error return from CONVFAN and Key canceled
C@empa aw 1995nov28 Parameterlist changed for all flow controller routines
C@tno jcp 1996Jun13_23:18:26 for PS Vmet and RhoOut
C@tno jcp 1996Jul25_09:17:24 linktype 4=duct fitting, inerr missed arg
C@lbl bvs 1996Nov1 XX1 not used
C@lbl bvs 1997Jun6 first call to FLCON4 should have been commented out
C@lbl bvs 1997Jun9 InitFlg removed from common - now arguments to FLCON1-4
C@tno jcp 1997Jul03_12:45:52 explanation of DAT(*) for the fan
C@lbl bvs 1997Nov11 CA, NA, C not used
C@lbl bvs 1997Nov11 XX3, XX5 not used
C@lbl dml 1999nov19 Change integer variable initFlg to logical linInit.
C@lbl dml 1999nov24 Clean up indentation to reflect logical structure.
C   In the course of doing this, found that the case dat(1).eq.13
C   was handled using "if" rather than using "else if" construct.
C   Therefore it nested logically as an if-construct within the case
C   dat(1).eq.12. Changed to "else if", and eliminated extraneous
C..."endif".
C@lbl dml 1999nov24 Combine two calls of fmaTofma12() in code for
C   link type 2=fan.
C@lbl dml 1999nov24 Remove obsolete variable DbFma.
C@NBI PGS 2000Jul17 - Note that Fma must have a viable value before calling
C@NBI                 this routine (can be zero), because its initial value
C@NBI                 is an input/output argument for routine CRACK at the
C@NBI                 beginning of this routine.
C
C Limits : not all equations are in here.
C
C Pass parameters:
C
C IO  # Name      Units   Description
C IO  1 Fma       (kg/s)  mass flow through the link
C  O  2 DFma      (kg/s/Pa)  derivative d Fma/d Dp
C I   3 Dp        (Pa)    pressure difference accross the link
C I   4 DAT(*)    multi   data about flow coefficients
C I   5 Mf1       -       the output is multiplied with Mf1
C I   6 Mp1       -       the input pressure Dp is first multiplied with Mp1
C I   7 RhoL1     kg/m3   the air density for positive flow
C I   8 RhoL2     kg/m3   the air density for negative flow
C I   9 SQRRho1           SQRT of RhoL positive flow
C I   0 SQRRho2           SQRT of RhoL negative flow
C I   1 Mul1      Pa.s    the air viscosity for positive and negative flow
C I   2 Mul2      Pa.s    the air viscosity for positive and negative flow
C I  13 linInit   -       linear initialization flag
C  O 14 fma1      -       ALL openings fma1
C  O 15 fma2      -       ALL openings fma2
C I  16 DifLim    Pa      limit for linearizing crackflow (Not for Waltons plr)
C I     RSqrtDff          Reciprocal of the squarroot of the Darcy friction
C                         factor
C I  18 BetaL1,2  degC/m  Temperature gradient
C I  20 SLinkDat  multi   Subarray of rotated array LinkDat
C  O    Tl        'C      Mean temperature of the air in the link
C I     Lstat     -       Link status:    Lstat From     To
C                         0 zone    zone
C                         1  ext    zone
C                         2 spec    zone
C                         3 zone     ext
C                         4  ext     ext
C                         5 spec     ext
C                         6 zone    spec
C                         7  ext    spec
C                         8 spec    spec
C I     TempL1    'C      Air temperature in "FROM" zone at link height
C I     TempL2    'C      Air temperature in "TO" zone at link height
C I     NormCrRrho Kg/m^3 airdensity at crack test conditions
C I     normCrMu  Pa.s    dynamic air viscosity at crack test conditions
C I     ilink     -       link number of the current link (used at RF)
C I     FVnew     kg/s    all flowrates of links 1..iLink note that from iLink
C                         up to NL the array has old flowrates!
C I     iRFlist   -       for RF links it has the linknumber of the Reference L
C
C
C The general form of the DAT(*) array is:
C ---------------------------------------
C DAT(1)=pointer used for selection of the proper formulas for the link
C     1= crack         2= fan      3= straight duct      4=duct fitting
C     5= flowcontroller ideal   symmetric
C     6= flowcontroller ideal   non-symmetric
C     7= flowcontroller non-ideal symmetric
C     8= flowcontroller non-ideal non-symmetric
C     9= large opening
C    10= test data (measured curve) Fma=f(Dp)
C    11= Airnet powerlaw element according Walton in AIRNET
C   (12= Duct A,B,C        according Walton in AIRNET ) ??
C    12= RF Related Flow component
C@NBI PGS 2003Apr27 - New component type: Thermostatic vent
C    14= TV, Thermostatic-controlled vent
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
C    A fan probably   5 to 15
C    A duct          10 to 15
C    A flow controller up to 30
C    A measured curve      ?
C    A large opening  5 to 10
C
C (Geometrical) Subelements maybe connected to a link. Those could have an
C influence on the flow.
C
C example:
C call
Ch***********************************************************************

      IMPLICIT NONE
      include 'comv-uni.inc'
      include 'comv-par.inc'

C@tno jcp 1996Jun12_10:07:26 common vent2 is to print Velocity in vent2out
      real FvVeloc
      common /vent2/ FvVeloc

CC    REAL CA,NA,C(0:20)
      real DFma,Dat(*),Mf1,Mp1
      real RhoL1,sqrRho1,Mul1
      real RhoL2,sqrRho2,Mul2
      logical linInit
      real Fmal,Fma1,Fma2,DifLim,fmfact,expn,pi,fnpi
      real RSqrtDff
      real SLinkDat(maxd)

CC    double precision Dp,Fma,DbFma,XX1,XX3,XX5
CC    double precision Dp,Fma,DbFma,XX3,XX5
CC    double precision Dp,Fma,DbFma
      double precision Dp,Fma

      real Cm
      real PCor,Rho,NFO
      real Fratio
C     ! counter for TD,RF datapair and the element number in Dat(*)
      integer iTDPair,iRFPair,IElement

C     ! Pass parameters:
      real Tl,TempL1,TempL2
      real normCrRho,normCrMu
      integer Lstyp,Lstat,Ilink,iRFlist(MaxL)
C     ! FVnew is used for RF to look at the RefFlow
      real FVNew(maxL)
      real Vmet,RhoOut

C     ! Local Parameters:
      real Crlen,Wthick,Uval

C     ! Local parameters for Large openings:
      double precision fma1lo,fma2lo
      double precision dfma1,dfma2
      double precision DpProfNew(NrInt+2)
      real RhoProfF(NrInt+2)
      real RhoProfT(NrInt+2)
      real Q1,Q2,P1,P2,RQ1,RQ2
      real RefFlow

C@tno jcp 1996Jun05_16:32:44 added variables for PS, 3 lines
      real Agrille,Dduct,D2duct,LambDuct,ZetaCowl,CpCowl
      real L1duct,Aduct,Dhduct,detduct,A1duct,B1duct,C1duct,RhoDuct
      double precision DpDuct
C-----

C@tno jcp 1996Jun14_14:13:13 reset FvVeloc in FEQN, Routines might not set it
      FvVeloc=0.0

      if( test.ge.1 .and. secho.ge.7 ) then
         WRITE(CRT,*) 'I am in FEQN'
         WRITE(CRT,*) 'Link number',iLink
         WRITE(CRT,*) 'Fma',Fma
         WRITE(CRT,*) 'DFma',DFma
         WRITE(CRT,*) 'dp=',dp
         WRITE(CRT,*) 'Dat(1)',dat(1)
         WRITE(CRT,*) 'Dat(2)',dat(2)
         WRITE(CRT,*) 'Dat(3)',dat(3)
         WRITE(CRT,*) 'Dat(4)',dat(4)
         WRITE(CRT,*) 'Dat(5)',dat(5)
         WRITE(CRT,*) 'Dat(6)',dat(6)
         WRITE(CRT,*) 'Mf1',Mf1
         WRITE(CRT,*) 'Mp1',Mp1
         WRITE(CRT,*) 'RhoL1',RhoL1
         WRITE(CRT,*) 'RhoL2',RhoL2
         WRITE(CRT,*) 'sqrRho1',sqrRho1
         WRITE(CRT,*) 'sqrRho2',sqrRho2
         WRITE(CRT,*) 'MuL1',MuL1
         WRITE(CRT,*) 'MuL2',MuL2
         WRITE(CRT,*) 'linInit',linInit
      endif

      fma1=0.0
      fma2=0.0

C     !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 1= crack
      if( Dat(1) .eq. 1 ) then
C        ! 1=Crack.
C        ! Dat(1)=Linktyp =1 Crack
C        ! Dat(2)=Cs
C        ! Dat(3)=expN
C        ! Dat(4)=length of the crack in Dat(2) (cannot be 0)
C        ! Dat(5)=wall thickness
C        ! Dat(6)=U value of the wall
C        ! Cm=Mf1/length*Cs
         Cm=Mf1/Dat(4)*Dat(2)
         Expn=Dat(3)
         Crlen=Mf1
         Wthick=Dat(5)
         Uval=Dat(6)
C        ! Lstyp=3 for wall crack
         Lstyp=3
         CALL CRACK(Fma,DFma,Tl,Dp,linInit,Cm,Expn,Diflim,Lstyp,
     &     Lstat,TempL1,TempL2,RhoL1,RhoL2,
     &     normCrRho,normCrMu,Crlen,Wthick,Uval)
C        ! Split the flow through the link into two flows.
         CALL fmaTofma12(fma,fma1,fma2)

C     !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 2= fan
      else if( Dat(1) .eq. 2 ) then
C        ! 2=Fan.
C        ! Dat( 1)=type=2=fan
C        ! Dat( 2)=flag 1=C0..C5 2=use data points
C        ! Dat( 3)=exp polynom
C        ! Dat( 4)=RhoI
C        ! Dat( 5)=NfI  rpm, (normalised)fan speed
C        ! Dat( 6)=Cm   if fan is off (<1% of speed)
C        ! Dat( 7)=ExpN if fan is off (<1% of speed)
C        ! Dat( 8)=Pmin of polynomial part
C        ! Dat( 9)=Pmax of polynomial part
C        ! Dat(10)=slope     of linear part
C        ! Dat(11)=intercept of linear part
C        ! Dat(12)=C0
C        ! Dat(13)=C1
C        ! Dat(14)=C2
C        ! Dat(15)=C3
C        ! Dat(16)=C4
C        ! Dat(17)=C5
C        ! Dat(18)=Number of data points say 4 here
C        ! Dat(19)=P1
C        ! Dat(20)=Q1
C        ! Dat(21)=P2
C        ! Dat(22)=Q2
C        ! Dat(23)=P3
C        ! Dat(24)=Q3
C        ! Dat(25)=P4
C        ! Dat(26)=Q4
C        ! Dat(27)=Filter coef pol1
C        ! Dat(28)=Filter coef pol2
C        ! Dat(29)=Filter coef pol3
C        ! Dat(30)=Filter coef pol4
C        ! Dat(31)=Filter coef pol5
C        !   If the fan speed is near zero, the fan acts like a crack.
C        ! Assume the fan acts like a crack, if the fraction of
C        ! (real fan speed / test fan speed ) is less than 0.01.
         if( (mf1/dat(5)) .lt. 0.01 ) then
C          ! Fan acts like a crack.
           cm=dat(6)
           Expn=Dat(7)
           Crlen=1.
           Wthick=0.0
           Uval=0.3
C          ! Lstyp=3 for wall crack
           Lstyp=3
           CALL CRACK(Fma,DFma,Tl,Dp,linInit,Cm,Expn,Diflim,Lstyp,
     &       Lstat,TempL1,TempL2,RhoL1,RhoL2,
     &       normCrRho, normCrMu,Crlen,Wthick,Uval)
CCC        ! Split the flow through the link into two flows.
CC         CALL fmaTofma12(fma,fma1,fma2)
         else
C          ! Fan acts like a fan, i.e., not like a crack.
           if( Mp1.gt.0 ) then
C            !   Assume the fan is blowing harder than its possible
C            ! opposing pressure.
             Rho=RhoL1
           else
             Rho=RhoL2
           endif
           NFO=Mf1
           CALL ConvFan(PCor,1.0,Dat(4),Rho,Dat(5),NFO,
     &       1.0,Fratio)
           if( (test.ge.1).and.(pecho.ge.2) ) then
             write(COF,*) 'FRatio=',FRatio
           endif
           CALL Fan(Fma,DFma,Dp,DAT,Fratio,Mp1*PCor)
           if( (test.ge.1).and.(pecho.ge.2) ) then
             write(COF,*)'Fan Dp=',dp,'Fma=',Fma,'DFma=',DFma
           endif
           if( fma*Mp1.LT.0 ) then
C          !   The fan is reversed by the contra pressure. Do the
C          ! calculation again for the opposite RhoL
             if( Mp1.le.0 ) then
               Rho=RhoL1
             else
               Rho=RhoL2
             endif
             CALL ConvFan(PCor,1.0,Dat(4),Rho,Dat(5),NFO,1.0,Fratio)
             CALL Fan (Fma,DFma,Dp,DAT,Fratio,Mp1*PCor)
             if( (test.ge.1).and.(pecho.ge.2) ) then
               write(COF,*)'Fan Dp=',dp,'Fma=',Fma,'DFma=',DFma
             endif
           endif
CC         DbFma = DFma
CC         DFma = DbFma
CCC        ! Split the flow through the link into two flows.
CC         CALL fmaTofma12(fma,fma1,fma2)
         endif
C        ! Split the flow through the link into two flows.
         CALL fmaTofma12(fma,fma1,fma2)

C     !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 3= duct
      else if( Dat(1).eq. 3 ) then
C        ! 3=duct
C@empa aw 2001sep19 mf1 added
         CALL DWC(Fma,DFma,Dp,Dat,Rhol1,Rhol2,Mul1,Mul2,linInit,
     &     RSqrtDff,mf1)
C        ! Split the flow through the link into two flows.
         CALL fmaTofma12(fma,fma1,fma2)

C     !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 4= duct fitting
      else if( Dat(1).eq. 4 ) then
C        ! 4=duct fitting
CC       call error('Duct fittings are not available yet!',3)
         call inerr('Duct fittings are not available yet!',
     &     ' ',.true.,3)

C     !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 5= flow controller
      else if( Dat(1).eq. 5 ) then
C        ! 5=ideal symmetric
CC       CALL FLCON1(Dat,NA,CA,Fma,DFma,Dp,Mf1,Mp1)
CC       CALL FlCon1(Dat,Dp,Mf1,DifLim,RhoL1,RhoL2,Fma,DFma)
         CALL FlCon1(Dat,Dp,Mf1,DifLim,RhoL1,RhoL2,Fma,DFma,linInit)
C        ! Split the flow through the link into two flows.
         CALL fmaTofma12(fma,fma1,fma2)

C     !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 6= flow controller
      else if( Dat(1).eq. 6 ) then
C        ! 6=ideal nonsymmetric
CC       CALL FLCON2(Dat,NA,CA,Fma,DFma,Dp,Mf1,Mp1)
CC       CALL FlCon2(Dat,Dp,Mf1,DifLim,RhoL1,RhoL2,Fma,DFma)
         CALL FlCon2(Dat,Dp,Mf1,DifLim,RhoL1,RhoL2,Fma,DFma,linInit)
C        ! Split the flow through the link into two flows.
         CALL fmaTofma12(fma,fma1,fma2)

C     !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 7= flow controller
      else if( Dat(1).eq. 7 ) then
C        ! 7=nonideal symmetric
CC       CALL FLCON3(Dat,C,NA,CA,Fma,DFma,Dp,XX1,Mf1,Mp1)
CC       CALL FlCon3(Dat,Dp,Mf1,DifLim,RhoL1,RhoL2,Fma,DFma)
         CALL FlCon3(Dat,Dp,Mf1,DifLim,RhoL1,RhoL2,Fma,DFma,linInit)
C        ! Split the flow through the link into two flows.
         CALL fmaTofma12(fma,fma1,fma2)

C     !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 8= flow controller
      else if( Dat(1).eq. 8 ) then
C        ! 8=nonideal nonsymmetric
CC       CALL FLCON4(Dat,C,NA,CA,Fma,DFma,Dp,XX3,XX5,Mf1,Mp1)
CC       CALL FlCon4(Dat,Dp,Mf1,DifLim,RhoL1,RhoL2,Fma,DFma)
         CALL FlCon4(Dat,Dp,Mf1,DifLim,RhoL1,RhoL2,Fma,DFma,linInit)
C        ! Split the flow through the link into two flows.
         CALL fmaTofma12(fma,fma1,fma2)

C     !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 9= large opening
      else if( Dat(1).eq. 9 ) then
C        ! 9=large opening
         CALL VerticOp(fma1Lo,fma2Lo,dfma1,dfma2,
     &     DpProfNew,RhoProfF,RhoProfT,Mf1,Dat,SLinkDat,DifLim)
C        ! Net flow and derivative Fma,dFma
         Fma=  Fma1Lo-Fma2Lo
         dFma= dFma1-dFma2
C        ! convert Fma1Lo,Fma2Lo (double precision) to Fma1, Fma2 (real)
         Fma1=Fma1Lo
         Fma2=Fma2Lo
CC       write(cof,*) 'fma12 ',fma1,fma2
CC       write(cof,*) 'Dpprofnew',dpprofnew
C@tno jcp 1996Jun11_16:44:45 fix temperature of the link TL for large opening
         if( fma1+fma2.ne.0 ) then
           TL=(TempL1*Fma1+TempL2*Fma2)/(Fma1+Fma2)
         else
           TL=(TempL1+TempL2)/2
         endif
C        ! Note flow already split into fma1,fma2. Don't call fmaTofma12.

C     !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 10= test data
      else if( Dat(1).eq. 10 ) then
C        ! Test Data Dat(1)=10.
C@lbl dml 2000mar24 Renumber Dat(*) to reflect useage of current input
C   file definition. Input subroutine, INTD, does not actually store the
C   flow/mass flag, and the input file does not specify the flag, so
C   remove it from the code. Instead, correct calculated flow with rhoI.
C        ! Dat(1)=10
C        ! Dat(2)=Npair
CC       ! Dat(3)=Flag 1=Fva, 2=Fma  NO LONGER USED
C        ! Dat(3)=rhoI
C        ! Dat(4)=p1, Dat(5)=q1
C        ! Dat(6)=p2, Dat(7)=q2 if Npair>1
C        ! Pressure data for point i is in Dat(i*2+2), i=(1..Npair)
C        ! Flow     data for point i is in Dat(i*2+3)
         if( Dat(2).eq.1 ) then
C          ! Constant flow rate.
C@empa aw 2001may09 Bugfix from dml: Data for constant flow resides in Dat(5) not in Dat(6)
CC           FMA=Dat(6)*MF1
           FMA=Dat(5)*MF1
           Dfma=0.0
         else
C          ! Npair>1. Interpolate between the closest datapoints. Assume
C          ! the data pairs are sorted in ascending pressures. Note that
C          ! the pressure across the link is the pressure TO-FROM. Thus
C          ! a crack has data like p=-10 q=-0.05 p=0 q=0 p=10 q=0.05.
C          ! Also, a fan has its curve in the 2nd quadrant, like:
C          ! p=-100 q=0 p=-50 q=0.05 p=0 q=0.07 this fan blows in the
C          ! TO-zone and sucks in the FROM-zone
C          ! Next loop increments ITDpair until P1<dp<P2 or P1 and P2
C          ! are either end points in the data pairs.
C          ! P1,Q1 is one datapair, P2,Q2 is the following pair
C          ! (P=pressure Q=flow)
C          !
CC         ! Note this was original loop:
C@tno jcp 1997Jun03_15:09:22 loop rewritten
CC         ITDpair=1
CC500      if( ITDpair .LT. (Dat(2)) ) then
CCC          !loop to increase iTDpair until P1<DP<P2
CC           ITDpair=ITDpair+1
CC           write(cof,*) 'ITD pair=',ITDpair
CC           IElement=ITDpair*2+3
CC           P2=Dat(IElement)
CC           write(cof,*) 'element=',Ielement,'p2=',p2
CC           if( DP.GE.P2 ) then
CCC            ! exit this loop
CC             GOTO 510
CC           endif
CC         else
CCC          ! loop one more time
CC           GOTO 500
CC         endif
CC         ! Note this was rewritten loop:
C@lbl dml 2000mar24 The rewritten loop, below, does not correctly handle
C   the case when the input dP is less than the first pressure stored in
C   Dat(*). When this happens, the loop gets P2, but fails to get P1.
CC         ITDpair=1
CC         P2=dat(5)
CC500      if( ITDpair .LT. (Dat(2)) ) then
CC           if( dp.gt.p2 ) then
CCC            ! loop to increase iTDpair until P1<DP<P2
CC             ITDpair=ITDpair+1
CC             IElement=ITDpair*2+3
CC             P2=Dat(IElement)
CC           endif
CC           if( DP.LT.P2 ) then
CCC            ! exit this loop as dp is in the interval p1-p2
CC             GOTO 510
CC           endif
CC           GOTO 500
CC         endif
CC510      if( Dat(2).gt.1 ) Then
CC           P1=Dat(IElement-2)
CC           Q1=Dat(IElement-1)
CC           Q2=Dat(IElement+1)
CC         else
CC           P1=dat(5)
CC           Q1=dat(6)
CC           P2=P1
CC           Q2=Q1
CC         endif
C@tno jcp 1997Jun03_15:09:22 end of rewritten loop
C@lbl dml 2000mar24 New version of loop.
C          !   Loop to pick up ordered pressures P1<dP<P2, or else
C          ! dP<P1<P2 where P1 is first TD pressure, or else
C          ! P1<P2<dP where P2 is last TD pressure.
C          !   Note know nPair>1.
           ITDpair=2
           P1 = Dat(4)
           P2 = Dat(6)
500        if( dP .lt. P2 .or. ITDpair .ge. Dat(2) ) then
C            ! Either have bracketed, or P2 is at last data pair.
             goto 510
           endif
C          ! Pick up next data pair as P2.
           ITDpair = ITDpair+1
           P1 = P2
           P2 = Dat(ITDpair*2+2)
           goto 500
510        continue
C@lbl dml 2000mar24 End of rewritten loop
C          !   Here, have bracketing P1 and P2, and ITDpair gives index
C          ! of pair 2. Thus ITDpair-1 gives index of pair 1.
           Q1 = Dat(ITDpair*2+1)
           Q2 = Dat(ITDpair*2+3)
           if( (P2-P1) .ne. 0.0 ) then
C            ! Linear interpolation of Fma from the pressures P1 and P2
C            ! to dp
             Fma=(Q1*(P2-dp)+Q2*(dp-P1))/(P2-P1)*MF1
             Dfma=(Q2-Q1)/(P2-P1)*MF1
           else
             WRITE(CRT,*) 'ERROR. Routine FEQ found wrong TD-data:'
             WRITE(CRT,*) 'No pressure difference between datapair',
     &       ITDPair,' (p=',P1,') and',ITDPair+1,' (p=',P2,')'
           endif
         endif
C        ! Here, know Fma and Dfma for TD component.
C@lbl dml 2000mar24 Remove reference to volume/mass flow flag in Dat(3).
CC       if( dat(3).eq.1 ) then
CCC        ! data in volume flow
CC         if( FMA .LT. 0.0 ) then
CC           FMA=FMA*RhoL2
CC         else
CC           FMA=FMA*RhoL1
CC         endif
CC       else
CCC        ! data is in mass flow (no correction necessary)
CC       endif
CCC      ! Split the flow through the link into two flows.
C@lbl dml 2000mar24. Instead of that flag, use rhoI, per aw @empa.
C        ! Correct with Rho/RhoI. Note rhoI in Dat(3).
         if( Fma .lt. 0.0 ) then
           Rho=RhoL2
         else
           Rho=RhoL1
         endif
         Fma = Fma*Rho/Dat(3)
         Dfma = Dfma*Rho/Dat(3)
C        ! Split the flow through the link into two flows.
         CALL fmaTofma12(fma,fma1,fma2)

C     !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 11= walton powerlaw
      else if( Dat(1).eq. 11 ) then
C        ! 11=Walton powerlaw
C        ! Dat(2)=  initialization coefficient
C        ! Dat(3)=  laminar flow coefficient
C        ! Dat(4)=  turbulent flow coefficient
C        ! Dat(5)=  ExpN
C        ! Note that for this element the coefficients have other units
C        ! as coefficients for element type no 1 crack
         if( linInit ) then
           if( dp.GT.0.0 ) then
             DFma=Mf1*dat(2)*RhoL1/MuL1
           else
             DFma=Mf1*dat(2)*RhoL2/MuL2
           endif
           Fma=+DFma*dp
         else
C          ! no initialization
           if( dp.GT.0.0 ) then
C            ! prl positive
             Fma=Mf1*Dat(4)*sqrRho1*Dp**Dat(5)
C            ! linear DFma
             DFma=Mf1*dat(3)*RhoL1/MuL1
           else
C            ! prl negative
             Fma=-Mf1*Dat(4)*sqrRho2*(-Dp)**Dat(5)
C            ! linear DFma
             DFma=Mf1*dat(3)*RhoL2/MuL2
           endif
C          ! linear Fmal
           Fmal=+DFma*dp
           if( ABS(Fmal).LE.ABS(Fma) ) then
C            ! we are in the laminar region
             Fma=Fmal
           else
C            ! we are in the prl region and dp cannot be zero
             DFma=Fma*Dat(5)/Dp
           endif
         endif
C        ! Split the flow through the link into two flows.
         CALL fmaTofma12(fma,fma1,fma2)

C     !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 12= RF Related Flow
      else if( Dat(1).eq. 12 ) then
C        ! 12=RF Related Flow Dat(1)
C        ! Dat(2)=Npair
C        ! Dat(3)=Flag 1=Fva, 2=Fma
C        ! Dat(4)=rhoI
C        ! Dat(5)=RQ1 (independent; flow rate through the RefLink)
C        ! Dat(6)=q1  (dependent  ; flowrate that goes through RF at RQ1 through RefLink)
C        ! Dat(7)=RQ2 if Npair>1 (last element (q) in Ldat is Npair*2+3)
C        ! Dat(8)=q2
C        ! pressure data for point N is in Dat(N*2+3)  N=(1..Npair)
C        ! flow     data for point N is in Dat(N*2+4)
C        !   Get the flowrate through the reference link (indicated by
C        ! iRFlist(Ilink)). The link iRFlist(iLink) must appear before
C        ! the current iLink, otherwise the flowrate is unknown and
C        ! contains an old value. This has been checked at PreRF.
         RefFLow=FVNew(iRFlist(iLink))
CC          write(CRT,*)'FVNew(1)=',FVNew(1)
CC          write(CRT,*)'FVNew(2)=',FVNew(2)
CC          write(CRT,*)'FVNew(3)=',FVNew(3)
CC          write(CRT,*)'FVNew(4)=',FVNew(4)
CC          write(CRT,*)'iRFlist(1)=',iRFlist(1)
CC          write(CRT,*)'iRFlist(2)=',iRFlist(2)
CC          write(CRT,*)'iRFlist(3)=',iRFlist(3)
CC          write(CRT,*)'iRFlist(4)=',iRFlist(4)
CC          write(CRT,*)'RefLink=',iRFlist(ilink)
CC          write(CRT,*)'RefFlow=',RefFlow
C        ! RefFlow has the same function as DP for the TD component,
c        ! it is the independent variable. By interpolation in the
C        ! Input datapointsthe flow through this RF link is the result.
         if( Dat(2).EQ.1 ) then
C          ! constant flow rate
           FMA=Dat(6)*MF1
           Dfma=0.0
         else
C          ! else Npair>1
C          ! interpolation between the closest datapoints. Assume the
C          ! data pairs are in sorted order: independent flowrates
C          ! go up. (independent flowrates are the flowrates through
C          ! the RefLink)
           iRFpair=1
C          ! Next loop increments iRFpair until RQ1<RefFlow<RQ2 or RQ1
C          ! and RQ2 are either endpoints in the data pairs
C          ! RQ1,Q1 is one datapair, RQ2,Q2 is the following pair
C          ! (RQ=refflow Q=RFflow)
C          ! loop to increase iRFpair until QR1<RefFlow<QR2
1500       if( iRFpair .LT. (Dat(2)) ) then
C            ! there is still another data pair
             iRFpair=iRFpair+1
             IElement=iRFpair*2+3
             RQ2=Dat(IElement)
             if( RefFlow.GE.RQ2 ) then
C              ! go one more time (if enough data)
               GOTO 1500
             endif
           else
C            ! exit this loop
             GOTO 1510
           endif
1510       RQ1=Dat(IElement-2)
           Q1=Dat(IElement-1)
           Q2=Dat(IElement+1)
CC         write(crt,*) 'RF datapair=',iRFpair
CC         write(crt,*) 'Rq1=',rq1
CC         write(crt,*) 'Rq2=',rq2
CC         write(crt,*) ' q1=', q1
CC         write(crt,*) ' q2=', q2
           if( (RQ2-RQ1) .NE. 0.0 ) then
C            ! linear interpolation of Fma from the pressures RQ1 and
C            ! RQ2 to RefFlow
             Fma=(Q1*(RQ2-RefFlow)+Q2*(RefFlow-RQ1))/(RQ2-RQ1)
CC           Dfma=(Q2-Q1)/(RQ2-RQ1)
C            ! the derivative between the connected nodes has no
C            ! relation with the derivative of this component
C            ! try to set it to zero
             DFMA=0
           else
             WRITE(CRT,*) 'ERROR. Routine FEQ found wrong RF-data:'
             WRITE(CRT,*) 'No Flow rate difference between datapair',
     &         iRFPair,' (q=',RQ1,') and',iRFPair+1,' (q=',RQ2,')'
           endif
         endif
C        ! endif (dat(2) .eq. 1)
         if( dat(3).eq.1 ) then
C          ! data is in volume flow
C@tno jcp 1996Feb08 have to think about the meaning of this
           if( FMA .LT. 0.0 ) then
             FMA=FMA*RhoL2
             dFMA=dFMA*RhoL2
           else
             FMA=FMA*RhoL1
             dFMA=dFMA*RhoL1
           endif
CC         write(crt,*) 'converted to mass flow:'
CC         write(crt,*) ' fma=', fma
CC         write(crt,*) 'dfma=',dfma
         else
C          ! data is in mass flow (no correction necessary)
         endif
C        ! Split the flow through the link into two flows.
         CALL fmaTofma12(fma,fma1,fma2)

C     !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 13= Passive Stack PS
CC    if( Dat(1) .eq. 13 ) then
      else if( Dat(1) .eq. 13 ) then
C        ! 13=passive stack ps
C        ! The passive stack is a link from a zone in a building to
C        ! outside (roof) from inside to outside it has the following
C        ! elements:
C        ! - a grille or opening, visible in the room, mounted on the
C        ! duct. The opening is Agrille (m2) and leads to a pressure
C        ! loss of 1/2*rho*(fva/Agrille)^2
C        ! - a duct follows.
C        ! * this duct is round if you input one diameter (2nd
C        ! diameter=0) it is rectangular if you input two diameters
C        ! * the length of the duct (m) is input. The length can be
C        ! adjusted by giving an Act. factor at &-NET-LINKs
C        ! * the friction loss factor (Lambda) is input (if a negative
C        ! value is input, I think to interpret it as Eps-wall roughness
C        ! to call the Reynolds dependent calculation of Lambda)
C        ! * if there are extra zeta's in the duct put them in a
C        ! decreased inlet grille
C        ! - a cowl (hood) at the top of the duct outside.
C        ! * the cowl has a zeta for the flow between the duct and
C        ! outside
C        ! the cowl has an extra Cp value that might be 0...-1 and
C        ! is added to the outside pressure.
C        !   The defined equation:
C        ! dp=rho/2*((qv/Agrille)^2+(qv/Aduct)^2*(Lam*L/D+Zeta))
C        !   + Qv/q1*SQRT(rhoout/2*Vmet^2*(-Cp cowl))
C        ! The last term is an extra pressure loss due to the fact that
C        ! the lower pressure created by the cowls's outside is made
C        ! more positive by the outflowing ventilation air.
C        ! q1 is the flow through the cowl only at 1Pa:
C        ! q1=Aduct*SQRT(2*1/(rho*zeta))
C        ! REAL Agrille,Dduct,Lduct,LambDuct,ZetaCowl,CpCowl
C        ! REAL L1duct,Aduct,detduct,A1duct,B1duct,C1duct,rhoduct
C        ! Dat(1)=Linktyp =13 Passive stack
C        ! Dat(2)=Agrille
C        ! Dat(3)=D duct
C        ! Dat(4)=D2duct
C        ! Dat(5)=L duct
C        ! Dat(6)=Lambda friction coefficient duct
C        ! Dat(7)=zeta (pressure loss factor ) of cowl
C        ! Dat(8)=extra cp of cowl  (normallly an under pressure)
C        ! actual length =Mf1*Lduct, Act Value multiplies length
         L1duct  =Mf1*Dat(4)
         Agrille =Dat(2)
         LambDuct=Dat(6)
         dduct   =Dat(3)
         d2duct  =Dat(4)
         ZetaCowl=Dat(7)
         CpCowl  =Dat(8)
         if( d2duct.eq.0 ) then
C          ! round
           PI=FnPI()
           Aduct=PI/4*dduct*dduct
           DhDuct=dduct
         else
C          ! rectangular
           Aduct=dduct*d2duct
           DhDuct=4*Aduct/(2*dduct+2*d2duct)
         endif
C@tno jcp 1996Jun05_16:43:43 vmet or vbuilding or what?
C        ! Dpduct will increase id Dp>0 and Dat(8)<0 (suction)
         DpDuct=Dp-rhoOut/2*vmet*vmet*CpCowl
         if( dpduct.gt.0 ) then
           c1duct=-dpduct
           rhoDuct=rhol1
         else
           c1duct=+dpduct
           rhoDuct=rhol2
         endif
         A1duct=rhoDuct/2*(1/(Agrille*Agrille)+
     &      1/(Aduct*Aduct)*(LambDuct*L1duct/DhDuct+ZetaCowl))
C@tno jcp 1996Jun05_16:55:31 rho met or rho out?
C@tno jcp 1996Jun05_16:56:38 only for negative extra Cp's
         B1duct=SQRT(rhoOut/2*vmet*vmet*(-CpCowl))/
     &     (Aduct*SQRT(2/(rhoDuct*zetacowl)))
         detDuct=B1duct*B1duct-4*A1duct*C1duct
         if( detduct.ge.0.0 ) then
           fma=(-b1duct+SQRT(detduct))/2/a1duct
           if( dpduct.lt.0 ) then
             fma=-fma
           endif
         else
           call error('determinant of PS duct zero',2)
           fma=0
         endif
CC       endif ! Extraneous endif associated with "if dat(1).eq.13"
C        ! Split the flow through the link into two flows.
         CALL fmaTofma12(fma,fma1,fma2)

C@NBI PGS 2003Apr27 - New component type: Thermostatic vent
C     !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 1= crack
      ELSEIF(Dat(1).EQ.14) THEN
         CALL TV_flow(Dat,Dp,Mf1,DifLim,TempL1,RhoL1,RhoL2,Fma,DFma
     &               ,linInit,NormCrRho)
C        ! Split the flow through the link into two flows.
         CALL fmaTofma12(fma,fma1,fma2)

C     !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% end
      endif
C     ! end processing of set of AFCtypes selected by dat(1)

      if( test.ge.1 .AND. secho.ge.6 ) then
         WRITE(CRT,1001) 'Fma=',Fma*FmFact,' DFma=',DFma
      endif

      RETURN
1001  FORMAT (1X,A4,E10.3,A6,E10.3)
      END
C
Ch***********************************************************************

      SUBROUTINE Fan(Fma,DFma,Dp,Dat,Mf1,Mp1)
C pass parameter # =  1   2    3  4   5   6
C***********************************************************************
C Purpose: fan calculates the flowrate through a fan at a given Dp

C          Keep in mind that fandata is given in the first quadrant,
C          but in a link the curve will appear in the third quadrant. Because
C          the linkpressure is defined opposite to the fan pressure rise.
C          The fan data is given in m3/s to obtain the polynomial. So infact
C          the polynomial RETURNs the Fva. In Mf1 we put the conversion factor
C          for both: NFO/NFI times Fma/Fva. Therefore the RETURNed value is
C          Fma at the fanspeed NFO.
C
C          It has a polynomial approximation from the pressures Pmin=Dat(8)
C          to Pmax=Dat(9) (Pa) . The form is:
C                C0         C1            C2                  Cpower
C          Fva= Dat(12)+DpF*Dat(13)+DpF**2*Dat(14),...
C               ..DpF**power*Dat(power+12)
C          where the value of power is in Dat(3) and DpF=-Dp*Mp1
C
C          Outside the range Dat(8) to Dat(9) Pa the Fma is calculated from:
C               slope    intercept
C          Fva= Dat(10)*DpF+Dat(11)   ... kg/s
C
C          The Fma and DpF has to be corrected according to the routine Rhofan.
C          The conditions of the fandata at which the coefficients are
C          calculated are in Dat(5)=NFI and Dat(4)=RhoI
C          A practical maximum highest power seems to be 6 for fan curves.
C A summary of the contents of Dat(*):
C Dat(1)=2 means this link is a fan
C@tno jcp 1997Jul28_10:58:37 fan flag definition changed 1+2=coef 3=datapoints
CC few obsolete lines deleted here
CC Dat(2)= Flag to say : 1=use polynomial 2= use fan data points
C Dat( 2)=flag 1=use C0..C5 2=use C0..C5 3=use data points
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
C the first fan data is Dat(19)=dP Dat(20)=Fma
C the last  fan data is Dat(19+(npoints-1)*2)=dP Dat(20+(npoints-1)*2)=Fma

C Module : 4.2.3, TG IV, hcp/february 14,1989
C Version: 1997 jul 04
C Changes:
C inserted fanspeed and Rho in Dat(6) and Dat(7)
C@empa      1.11.90/AW : Order of the elements in Dat according to the order
C@empa      of the elements in LDat in the call.
C correction for Mp1 added
C the sign of DpF there?
C@tno jcp 1997Jul04_11:46:04
C inserted catmull-rom interpolation of fan data points

C Limits :check the quality of the polynomial approximation.
C         as the curve is made as Fma=f(dP) there can be no contra flexture,
C         so if you have a fan curve that has a local minimum (Pfan=f(Fma)),
C         then draw the maximum pressure rise just higher than the highest
C         point in the curve and keep in monotone descending for increasing
C         Fma.
C
C Pass parameters:
C
C IO # Name    unit        description
C O  1 Fma     (kg/s)      mass flowrate
C O  2 DFma    (kg/s/Pa)   derivative (d Fma)/(d Dp)
C I  3 Dp      (Pa)        pressure difference of the link
C I  4 Dat(*)  multi       array with coefficients for this fan
C I  5 Mf1     -           multiplication factor for the fan output flowrate
C I  6 Mp1     -           multiplication factor for the fan input pressure DpF.
C              To scale the fan of this link to one with a higher pressure rise,
C              Mp1 should be smaller than 1. then the same curve is used.
C
C example:
C call
Ch***********************************************************************

      IMPLICIT NONE
      include 'comv-uni.inc'

      real DFma,Dat(*),Mf1,Mp1
      double precision Dp,Fma,dpf

      integer j

C The pressure in the fancurve is always defined
C opposite to the definition of Dp. Therefore we
C use DpF=-....

        DpF=-Mp1*Dp

C@tno jcp 1997Jul28_11:03:41 fan flag comment line changed
C@tno jcp 1997Jul04_11:48:05 if flag=1+2(C0..C5), =3 fan data points (2 lines)
c        write(cof,*)'Dat(2),flag=',dat(2)
C@tno jcp 1997Jul28_11:03:56 fan flag ifstatement line changed
        if( Dat(2).lt. 2.5 ) then
c        write(cof,*)'use polynom'
C
C ------Flag = 1 or 2: FAN Use Polynom C0..C5
C

C       if( (Dpf-dat(8))*(dpf-dat(9)).GT.0.0 ) then
        if( (Dpf.LT.dat(8)).or.(Dpf.GT.dat(9)) ) then
C@tno temp
c         WRITE(COF,*) 'fan out range, dp,dpf,mp1,mf1=',dp,dpf,mp1,mf1


C the pressure is outside the normal operation range
C slope      intercept

          Fma=Mf1*(Dat(10)*DpF+Dat(11))

C the - sign is to convert from the fancurves df/dp to the df/dp of the linkflow

          DFma=-Dat(10)*Mf1*Mp1

        else
C@tno temp
C         write(cof,*) 'fan2 in range, dpf,mp1,mf1=',dpf,mp1,mf1

C Power=Dat(3)
C C0=DAT(12),C1=Dat(13) Cpower=Dat(power+12)
C Fma=C0+C1*DpF**1+C2*DpF**2...Cpower*DpF**power

          Fma=0.0
          DO 100 j=Dat(3),1,-1

c            Write(cof,*) 'FAN: j=',j
c            Write(cof,*) 'Fma=(Fma+Dat(j+12))*DpF=(',
c     &       Fma,'+',Dat(j+12),')*',DpF

            Fma=(Fma+Dat(j+12))*DpF

c            Write(cof,*) 'Fma=',fma

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

        endif
C@tno jcp 1997Jul04_11:48:05 else part for Catmull-rom fandata interpolation (14
        else
C
C ------Flag = 3: FAN Use fan data points
C
C the first data is Dat(19)=dP Dat(20)=Fma
C the last data is Dat(19+(npoints-1)*2)=dP Dat(20+(npoints-1)*2)=Fma

Cnote to use DpF here , not Dp. FanCAt is in comv-fe8.f
c          write(cof,*) 'FA cat dpf=',dpf
          CALL FanCat(Fma,DFma,DpF,Dat)
C@tno jcp 1997Nov12_11:49:24 The fma is not converted for Mf1 (Fratio,
C including rho and speed) yet (1 added line)
          Fma=Mf1*fma


C the - sign is to convert from the fancurves df/dp to the df/dp of the linkflow

C@tno jcp 1997Nov12_10:39:59   Dat(13) here? Dat(13) might have no value.
CC          DFma=-Mf1*(DFma+Dat(13))*Mp1
          DFma=-Mf1*(DFma)*Mp1
          if( (test.ge.1).and.(pecho.ge.2)) then
             WRITE(COF,*) 'fan catmull, dp,dpf,mp1,mf1,fma,dfma=',
     &          dp,dpf,mp1,mf1,fma,dfma
          endif

        endif
C@tno jcp 1997Jul04_11:48:05 endif flag=1(C0..C5), =3 fan data points

        RETURN
        END




Ch**********************************************************************
C
        SUBROUTINE DWC(Fma,DFma,Dp,Dat,RhoL1,RhoL2,MuL1,MuL2,linInit,
C pass parameter #     1   2    3  4   5     6     7    8    9
     &  RSqrtDff,mf1)
C       10
C**********************************************************************
C
C Purpose: This is the routine from Walton for the dwc element.
C          Darcy-Weisbach pipe/duct model using Colebrook equation for the
C          turbulent friction factor.
C
C Module:  empa aw 1991July04
C Changes:
C Dfma is not right. now
C                                   FMA(DP*(1-EPS2)) - FMA(DP)
C                            Dfma=  --------------------------
C                                          (Dp*EPS2)
C@empa aw 1993jan05 LOG(D) has to be multiplied by C
C@empa aw 1993jan07 quit iteration loop if RSqrtDff is to small
C@empa aw 1993nov02 zeta introduced
C@empa aw 1993nov02 if zeta=0 set zeta=0.001 for linear initialisation
C@lbl dml 1999nov19 Change integer variable initFlg to logical linInit.
C@empa aw 2001sep07 ZETA for reverse flow direction
C@empa aw 2001sep17 mf1 factor for dynamic pressure loss coefficient ZETA

C
C Pass parameters:
C IO # Name      Units     Description
C  O 1 Fma       (kg/s)    mass flow through the link
C  O 2 DFma      (kg/s/Pa) derivative d Fma/d Dp
C I  3 Dp        (Pa)      pressure difference accross the link
C I  4 DAT(*)    multi     data about the duct
C I  5 RhoL1     kg/m3     the air density for positive flow
C I  6 RhoL2     kg/m3     the air density for negative flow
C I  7 Mul1      Pa.s      the air viscosity for positive and negative flow
C I  8 Mul2      Pa.s      the air viscosity for positive and negative flow
C I  9 linInit   -         linear initialiszation flag
C I 10 RSqrtDffg g         1/sqrt(Darcy friction factor)
C I 11 mf1       -         factor for dynamic pressure loss coefficient ZETA
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
C@empa aw 2001sep07
C I    Dat(17) -    ZETA of the duct for positive flow (FROM to TO)
C I    Dat(18) -    ZETA of the duct for negative flow (TO to FROM)
C O    Dat(19) -    ZETA of the fitting for positive flow (FROM to TO)
C O    Dat(20) -    ZETA of the fitting for negative flow (TO to FROM)
C
C  Local:
C@lbl rw 1992may13 RE is not used anywhere
C     RE      - Reynolds number.
C     FL      - friction factor for laminar flow.
C     FT      - friction factor for turbulent flow.
C     zeta    - dynamic loss coefficient
Ch**********************************************************************

      IMPLICIT NONE
      integer  III
      logical linInit
      real      Dat(*), DFma
      real      RhoL1, Rhol2, MuL1, MuL2
      real      RSqrtDff,mf1
      real      A0, A1, A2, B, C, D, EPS, S2, CDM, FL, FT, FTT
      real    zeta,fma3
      double precision Dp, Fma, SpareDp, EPS2
C
C     CODE  ************************************************************
C
        DATA C,EPS / 0.868589, 0.001 /
        eps2=5.0D-004
        SpareDP=DP

C@empa aw 2001sep07
      if (Dp.GE.0.0) then
         zeta=dat(17)*mf1+dat(19)
      else
         zeta=dat(18)*mf1+dat(20)
      endif
        if( zeta.le.0.001 ) then
           zeta=0.001
        endif

C loop to calculate FMA3 and then FMA
        DO 100 III=1,2
        if( III.EQ.1 ) then
          Dp=Dp*(1.0D00 + EPS2)
          if( dp.eq.0.0D00 ) then
            dp=dp+eps2
          endif
        else
          Fma3=fma
          Dp=SpareDp
        endif

        if( RSqrtDff.EQ.0.0 ) then
C                              Initialisation of RSqrtDff
          RSqrtDff=Dat(16)
        endif
        if( linInit ) then
C                              Initialization by linear relation.
C@empa aw 2001sep07
CC        zeta=dat(6)

          if( Dp.GE.0.0 ) then
            DFma = (2.*RhoL1*Dat(11)*Dat(10))/
     &          (MuL1*Dat(14)*Dat(15)*zeta)
          else
            DFma = (2.*RhoL2*Dat(11)*Dat(10))/
     &          (MuL2*Dat(14)*Dat(15)*zeta)
          endif
          Fma = DFma*Dp
        else
C                              Standard calculation.
          if( Dp.GE.0.0 ) then
C                                Flow in positive direction.
C                                  Laminar flow. Dat(13)=0
            if( Dat(13).GE.0.001 ) then
              A2 = Dat(13)/(2.*RhoL1*Dat(11)*Dat(11))
              A1 = (MuL1*Dat(12)*Dat(15))/(2.*RhoL1*Dat(11)*Dat(10))
              A0 = -Dp
              CDM = SQRT(A1*A1-4.*A2*A0)
              FL = (CDM-A1)/(2.*A2)
              CDM = 1.0/CDM
            else
              CDM = (2.*RhoL1*Dat(11)*Dat(10))/(MuL1*Dat(12)*Dat(15))
              FL = CDM*Dp
            endif

C                                            Turbulent flow!
            if( Dp.NE.0 ) then
              S2 = SQRT(2.*RhoL1*Dp)*Dat(11)
C@empa aw 2001sep07
CC            FTT = S2 / SQRT(Dat(15)/RSqrtDff**2+Dat(6))
              FTT = S2 / SQRT(Dat(15)/RSqrtDff**2+zeta)
10            CONTINUE
              FT = FTT
              B = (9.3*MuL1*Dat(11))/(FT*Dat(4)/1000)
              D = 1.0 + RSqrtDff*B
C  Quit iteration loop
              if( D.LE.0.0 )  GOTO 11
              RSqrtDff=RSqrtDff-(RSqrtDff-Dat(16)+C*LOG(D))/(1.0+C*B/D)
C quit iteration loop if RSqrtDff is to small
              if( abs(RSqrtDff).LE.1.e-14 ) GOTO 11
C@empa aw 2001sep07
CC            FTT = S2 / SQRT(Dat(15)/RSqrtDff**2+Dat(6))
              FTT = S2 / SQRT(Dat(15)/RSqrtDff**2+zeta)
              if( ABS(FTT-FT)/FTT .GE. EPS ) GO TO 10
              FT = FTT
              GOTO 12
11            CONTINUE
              FT = 0.0
12            CONTINUE
            else
              FT = 0.0
            endif
          else
C                                Flow in negative direction.
C                                  Laminar flow. Dat(13)!=0
            if( Dat(13).GE.0.001 ) then
              A2 = Dat(13)/(2.*RhoL2*Dat(11)*Dat(11))
              A1 = (MuL2*Dat(12)*Dat(15))/(2.*RhoL2*Dat(11)*Dat(10))
              A0 = Dp
              CDM = SQRT(A1*A1-4.*A2*A0)
              FL = -(CDM-A1)/(2.*A2)
              CDM = 1.0/CDM
            else
              CDM = (2.*RhoL2*Dat(11)*Dat(10))/(MuL2*Dat(12)*Dat(15))
              FL = CDM*Dp
            endif

C                                                Turbulent flow!
            if( Dp.NE.0.0 ) then
              S2 = SQRT(-2.*RhoL2*Dp)*Dat(11)
C@empa aw 2001sep07
CC            FTT = S2 / SQRT(Dat(15)/RSqrtDff**2+Dat(6))
              FTT = S2 / SQRT(Dat(15)/RSqrtDff**2+zeta)
20            CONTINUE
              FT = FTT
              B = (9.3*MuL2*Dat(11))/(FT*Dat(4)/1000)
              D = 1.0 + RSqrtDff*B
C     Quit iteration loop
              if( D.LE.0.0 )  GOTO 21
              RSqrtDff=RSqrtDff-(RSqrtDff-Dat(16)+C*LOG(D))/(1.0+C*B/D)
C quit iteration loop if RSqrtDff is to small
              if( abs(RSqrtDff).LE.1.e-14 ) GOTO 21
C@empa aw 2001sep07
CC            FTT = S2 / SQRT(Dat(15)/RSqrtDff**2+Dat(6))
              FTT = S2 / SQRT(Dat(15)/RSqrtDff**2+zeta)
              if( ABS(FTT-FT)/FTT .GE. EPS ) GO TO 20
              FT = -FTT
              GOTO 22
21            CONTINUE
              FT = 0.0
22            CONTINUE
            else
              FT = 0.0
            endif
          endif
C                                Select laminar or turbulent flow.

          if( (ABS(FL).LE.ABS(FT)).OR.(ABS(FT).EQ.0.0) ) then
            Fma = FL
          else
            Fma = FT
          endif
        endif
C end of condition else (Initflag ...)


C end of the loop to calculate FMA3 and then FMA

100     CONTINUE

C final calculation of Dfma
        if( dp.eq.0.0D000 ) then
          Dfma=(FMA3-FMA)/(EPS2)
        else
          Dfma=(FMA3-FMA)/(DP*EPS2)
        endif

        RETURN
        END


C
Ch***********************************************************************
        SUBROUTINE POWLAW(PA,QA,CA,CN)
C pass parameter number   1  2  3  4
C***********************************************************************
C Purpose: this routine calculates the flow coefficients C and n from
C          the data set of two points.
C
C Module : 4.***, TG IV, hxy/May 24, 1989
C Change :
C Limit  :
C
C Pass parameters:
C
C IO # Name     unit      description
C
C I  1 PA(*)    (Pa)      pressure in the performance curve A
C I  2 QA(*)    (m3/s)    volume flow rate in the performance curve A
C O  3 CA       (m3/s)    flow coefficient C of power law
C O  4 CN       (-)       flow coefficient n of power law
C
Ch***********************************************************************

      IMPLICIT NONE
C@tno jcp 1996Jul23_16:26:25 include for REALMin
      include 'comv-par.inc'
C
      real PA(*),QA(*),CA,CN
      real X1,X2,Y1,Y2,A

      if( PA(1).eq.0.0 .and. QA(1).eq.0.0 ) then
         CALL ERROR2('In powlaw P(1) and Q(1) are both 0.0 ',
     &     ' Comis replaced them both by REALMin (1E-37)',0)
         Pa(1)=REALMin
         Qa(1)=REALMin
      endif

      if( PA(2).eq.0.0 .and. QA(2).eq.0.0 ) then
         CALL ERROR2('In powlaw P(2) and Q(2) are both 0.0 ',
     &     ' Comis replaced them both by REALMin (1E-37)',0)
         Pa(2)=REALMin
         Qa(2)=REALMin
      endif

      if( PA(1).eq.0.0.OR.PA(2).eq.0.0.OR.QA(1).eq.0.0.OR.
     &   QA(2).eq.0.0 ) then
         CALL ERROR('In powlaw you have pressure or flow rates'//
     &     ' that are zero !',3)
      endif

      if( PA(1).eq.PA(2) ) then
         CALL ERROR('In powlaw you have pressures P(1)=P(2)'//
     &     ' they should be different !',3)
      endif

      X1=LOG(PA(1))
      X2=LOG(PA(2))
      Y1=LOG(QA(1))
      Y2=LOG(QA(2))
      CN=(Y1-Y2)/(X1-X2)
      A=Y1-CN*X1
      CA=EXP(A)

      RETURN
      END


C@NBI PGS 2000Jul20 - This subroutine wasn't used any more, so commented out
CCCh***********************************************************************
CC      SUBROUTINE LEASTSQ(N,NDATA,X,Y,COEF)
CCC pass parameter # =   1 2     3    4 5
CCC***********************************************************************
CCC Purpose: Leastsq calculates the polynomial approx of X(*),Y(*)
CCC        Y=C0 + C1*x + C2*x**2 + C(n-1)*x**(n-1)
CCC
CCC Module : 0.3, TG , hcp/feb 15 , 1989
CCC Changes: mm dd yy
CCC Limits : highest power should be lower than the number of data points.
CCC        highest power is 20
CCC
CCC Pass parameters:
CCC
CCC IO # Name    unit            description
CCC I  1 N       -               highest power+1
CCC I  2 Ndata   -               number of datapoints in X(*),Y(*)
CCC O  3 Coef(0:20)              coefficients C0,C1,...C(n-1)
CCC I  4 X(*)                    x data
CCC I  5 Y(*)                    y data
CCC
CCC    This routine uses the routines SUM (summs the X and Y powers in matrix A)
CCC                                 SOLVELS (solves the matrix A)
CCC                                   SWAP    (swaps two REALs)
CCC example:
CCC call LEASTSQ(3,3,NDATA,COEF,X,Y)
CCC     DATA X/0,25,50,97*0./
CCC     DATA Y/.1,.075,.0,97*0./
CCC     should RETURN COEF(0)=.1 COEF(1)=1.16415e-09 COEF(2)=-4.e-05
CCC hcp.
CCCh***********************************************************************
CC
CC      IMPLICIT NONE
CC      integer N,NDATA
CC      real A(21,22),COEF(0:20),X(*),Y(*)
CC
CC      integer I,J
CCC                            reset A(21,22) and Coef(0:20) to 0
CC      DO 10 I=1,N
CC       DO 10 J=1,N+2
CC      A(I,J)=0.
CC10    CONTINUE
CC      DO 20 I=0,N-1
CC       COEF(I)=0.
CC20    CONTINUE
CCC                            sum the X squares sums and XY sums in A(*)
CC      DO 30 I=1,NDATA
CCC@NBI PGS 1999May04 - Changed name; SUM is inrinsic function in FORTRAN 90
CCCC    CALL SUM(N,X(I),Y(I),A)
CC      CALL SUM_XY(N,X(I),Y(I),A)
CC30    CONTINUE
CC      A(1,1)=NDATA
CCC                            fill out the whole A(*) with the first column
CCC                            and the last row
CC      DO 50 I=N,2,-1
CC       DO 40 J=1,N-1
CC      A(I-1,J+1)=A(I,J)
CC40    CONTINUE
CC50    CONTINUE
CC        CALL SOLVELS(N,A,COEF)
CC      RETURN
CC      END


Ch***********************************************************************
C@NBI PGS 1999May04 - Changed name; SUM is inrinsic function in FORTRAN 90
CC    SUBROUTINE SUM(N,X,Y,A)
      SUBROUTINE SUM_XY(N,X,Y,A)
C pass parameter # =   1 2 3 4
C***********************************************************************
C Purpose: Sum sums the X powers and Y*X powers in the
C                                matrix A for the least squares method
C                                of a polynomial
C
C Module : 0.3.3, TG IV, hcp/feb 15, 1989
C Changes: mm dd yy
C Limits :
C
C Pass parameters:
C
C IO # Name    unit   description
C I  1 N       (-)    number of equations
C I  2 X(*)    (-)    X data
C I  3 Y(*)    (-)    Y data
C O  4 A(21,22) -     coefficient matrix and B vector in col N+1
C
C example:
C call
Ch***********************************************************************

      IMPLICIT NONE
      integer N
      real x,y
      real A(21,22)
      integer I
C                             calculate the sum of the X powers in the first col
        DO 10 I=2,N
         A(I,1)=A(I,1)+X**(I-1)
10      CONTINUE
C                             calculate the sum of the X powers in the last row
        DO 20 I=2,N
         A(N,I)=A(N,I)+X**(I+N-2)
20      CONTINUE
C                              if X=0   0**0 might give an error, so this first
C                              element is calculated outside the following loop
        A(1,N+1)=A(1,N+1)+Y
C                             calculate the sum of the Y*X powers in the N+1 col
        DO 30 I=2,N
         A(I,N+1)=A(I,N+1)+Y*X**(I-1)
30      CONTINUE
        RETURN
        END


C
Ch***********************************************************************
        SUBROUTINE SOLVELS(N,A,COEF)
C pass parameter # =     1 2  3
C***********************************************************************
C Purpose: SolveLS solves the set of linear equations A*Coef=0
C
C Module : 0.3.1, TG, hcp/feb 15, 1989
C Changes: mm dd yy
C Limits :
C
C Pass parameters:
C
C IO # Name    unit  description
C I  1 N       -     number of equations
C I  2 A(21,22)-     coefficient matrix and result vector B is in the
C                                                 N+1th col of A
C O  3 Coef(0:20)    solution
C
C example:
C call
Ch***********************************************************************

      IMPLICIT NONE
      integer N
      real A(21,22),COEF(0:20)
      real PIV,SUM,RESERVE
      integer PIVROW,K,I,J
C                                        start reducing the rows
        DO 100 I=1,N-1
         PIV=0
C                                        look for the maximum pivot in a row
        DO 10 K=I,N
         RESERVE=ABS(A(K,I))
         if( RESERVE.GT.PIV ) then
          PIV=RESERVE
          PIVROW=K
         endif
10      CONTINUE
C                                        check if pivot is zero
        if( PIV.EQ.0 ) GOTO 1000
C                                       if I is not the pivot row then swap rows
        if( I.NE.PIVROW ) then
         DO 4 J=I,N+1
        CALL SWAP(A(I,J),A(PIVROW,J))
4       CONTINUE
        endif
        PIV=A(I,I)
C                                       normalise the pivot row
        DO 5 J=I+1,N+1
         A(I,J)=A(I,J)/PIV
5       CONTINUE
C                                       reduce all rows above the pivot row
        DO 6 K=I+1,N
         DO 6 J=I+1,N+1
        A(K,J)=A(K,J)-A(K,I)*A(I,J)
6       CONTINUE
100     CONTINUE
C                                       check if the last pivot=0(determinant 0)
        if( A(N,N).EQ.0 ) GOTO 1000
        COEF(N-1)=A(N,N+1)/A(N,N)
C                                       back solving of the reduced matrix
        DO 200 I=N-1,1,-1
         SUM=0
C@tno jcp 1996Mar05_12:12:22 should have been i+1
CC         DO 150 J=N,I,-1
         DO 150 J=N,I+1,-1
          SUM=SUM+A(I,J)*COEF(J-1)
150      CONTINUE
         COEF(I-1)=A(I,N+1)-SUM
200     CONTINUE
        RETURN

C@tno jcp 1996Jul23_18:16:32 Determinant=0 severe enough!
1000    CALL ERROR('Routine SolveLS: DETERMINANT IS ZERO',2)
        RETURN
        END

Ch***********************************************************************
        SUBROUTINE SWAP(FIG1,FIG2)
C pass parameter # =    1   2
C***********************************************************************
C Purpose: Swap changes two REALs
C
C Module : 0.3.2, TG IV, hcp/feb 15 , 1989
C Changes: mm dd yy
C Limits :
C
C Pass parameters:
C
C IO # Name    unit              description
C IO 1 Fig1    -       first REAL replaced by last REAL
C IO 2 Fig2    -       last  REAL replaced by first REAL
C
C example:
C call
Ch***********************************************************************

      IMPLICIT NONE
      real FIG1,FIG2
      real DUMMY
        DUMMY=FIG1
        FIG1=FIG2
        FIG2=DUMMY
        RETURN
        END


Ch***********************************************************************
        SUBROUTINE SWAPi(i1,i2)
C pass parameter # =    1   2
C***********************************************************************
C Purpose: Swap interchanges two integers
C called from dupltm
C
C@tno jcp 1996Jun17_18:30:48
C Changes: mm dd yy
C Limits :
C
C Pass parameters:
C
C IO # Name    unit              description
C IO 1 i1    -       first i1 replaced by last i2
C IO 2 i2    -       last  i2 replaced by first i1
C
C example:
C call
Ch***********************************************************************

      IMPLICIT NONE
      integer i1,i2
      integer iDUMMY
        iDUMMY=i1
        i1=i2
        i2=iDUMMY
        RETURN
        END


Ch***********************************************************************
      SUBROUTINE fmaTofma12(fma,fma1,fma2)
C************************************************************************
C
C     written by lbl rw
C     Purpose: This subroutine splits the mass flow fma (through a link) into the
C         two flows fma1 and fma2.
C
C     The flow fma1 is the flow, which is going though the link in the direction the
C     link is defined.
C     The flow fma2 is the flow, which is going in the opposite direction.
C     fma1 and fma2 are always positive.
Ch***********************************************************************

      IMPLICIT NONE
      double precision fma
      real fma1,fma2

      if( fma.gt.0.0 ) then
         fma1=fma
         fma2=0.0
      else
         fma1=0.0
         fma2=-fma
      endif
      return
      end


C@tno jcp 1996Mar05_12:11:41 made from SolveLS for use in Poltrans at Steady Sta
Ch***********************************************************************
C@tno jcp 1997Nov11_14:22:15 errorflag added as last parameter
        SUBROUTINE SOLVELS2(N,A,B,X,ierrflg)
C                           | | | |
C                           | | | solution
C                           | | vector
C                           | matrix
C                           number of equations
C pass parameter # =     1 2  3
C***********************************************************************
C Purpose: SolveLS2 solves the set of linear equations A*X=B
C
C Module : 0.3.1, TG, hcp/feb 15, 1989
C Changes: mm dd yy
C Limits :
C
C Pass parameters:
C
C IO # Name    unit  description
C I  1 N       -     number of equations
C I  2 A(maxz,maxz)- coefficient matrix
C I  2 B(maxz)-      result vector B
C O  3 X(maxz)       solution
C
C example:
C call
Ch***********************************************************************

      IMPLICIT NONE
      integer N
        include 'comv-par.inc'
      real A(maxz,maxz),B(maxz),X(Maxz)
      real PIV,SUM,RESERVE
C@tno jcp 1997Nov11_14:24:36 errorflag added
      integer ierrflg

      integer PIVROW,K,I,J
C@tno jcp 1997Nov11_14:25:22 BRIAN do you want me to set ierrflg here to 0?
        ierrflg=0
C                                        start reducing the rows
        DO 100 I=1,N-1
          PIV=0
C                                        look for the maximum pivot in a row
          DO 10 K=I,N
            RESERVE=ABS(A(K,I))
            if( RESERVE.GT.PIV ) then
              PIV=RESERVE
              PIVROW=K
            endif
10        CONTINUE
C                                        check if pivot is zero
          if( PIV.EQ.0 ) GOTO 1000
C                                        if I is not the pivot row then swap row
          if( I.NE.PIVROW ) then
            DO 4 J=I,N
              CALL SWAP(A(I,J),A(PIVROW,J))
4           CONTINUE
            CALL SWAP(B(I),B(PIVROW))
          endif
          PIV=A(I,I)
C                                       normalise the pivot row
          DO 5 J=I+1,N
            A(I,J)=A(I,J)/PIV
5         CONTINUE
          B(I)=B(I)/PIV
C                                       reduce all rows above the pivot row
          DO 7 K=I+1,N
            DO 6 J=I+1,N
              A(K,J)=A(K,J)-A(K,I)*A(I,J)
6           CONTINUE
            B(K)=B(K)-A(K,I)*B(I)
7         CONTINUE
100     CONTINUE
C                                       check if the last pivot=0(determinant 0)
        if( A(N,N).EQ.0 ) GOTO 1000
        X(N)=B(N)/A(N,N)
C                                       back solving of the reduced matrix
        DO 200 I=N-1,1,-1
          SUM=0
          DO 150 J=N,I+1,-1
            SUM=SUM+A(I,J)*X(J)
150       CONTINUE
          X(I)=B(I)-SUM
200     CONTINUE

        RETURN

C@tno jcp 1996Jul23_18:16:32 Determinant=0 severe enough!
1000    CALL ERROR('Routine SolveLS2: DETERMINANT IS ZERO',2)
C@tno jcp 1997Nov11_14:21:48
        ierrflg=1

        RETURN
        END

Ch***********************************************************************
