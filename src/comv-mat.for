C+*********************************************************** comv-mat.f
Ch***********************************************************************
	SUBROUTINE DPLinkI(DP,DpProfNew,I,Ltyp,From,To,
C@NBI PGS 1999May06 - Bugfix.  See "Changes" below
CC   & PzNew,DpL,LSTAT1,DpJ1,DpProf,ProfPtr1)
     & PzNew,DpL,LSTAT1,DpJ1,DpProf,ProfPtr1,Profstart)
C***********************************************************************
C Purpose: DP1 calculates the pressure difference and the DpProfNew for a link
C and is called from MatVec and the output routines
C
C Changes:
C@NBI PGS 1999May06 - Eh? This routine is called only from Vent2Out.
C@NBI                 Wouldn't it be good to cut down on code duplication
C@NBI                 by calling it from other routines too?
C@NBI               - Bugfix.  It now returns ProfStart as argument

C Pass parameters:
C IO   Name	      Unit     Description
C I    ProfPtr1   -        Should be = Profptr(I).  Only ever used here
C                          for large openings.  It obviates the need
C                          to pass the entire Profptr array as argument
C O    ProfStart  -        Array pointer
Ch***********************************************************************

        IMPLICIT NONE
	INCLUDE 'comv-uni.inc'
	include 'comv-par.inc'

C Definition pass parameters

        INTEGER LSTAT1
        DOUBLE PRECISION PzNew(maxz)
c@empa aw 1997sep18 DOUBLE Dpl
C@empa aw 2002dec04 DpL enlarged from DpL(2,MaxL) to DpL(3,MaxL) to have a space
C@empa aw 2002dec04 for the final pressure differnce

        DOUBLE PRECISION  DpL(3,maxl)
        REAL Dpj1


C@empa aw 1997sep12 DpProf DOUBLE PRECISION
CC      REAL DpProf(MaxLo*(NrInt+2))
        DOUBLE PRECISION DpProf(MaxLo*(NrInt+2))

        INTEGER ProfPtr1

C Definition local parameters

        INTEGER FROM,TO,I
        REAL DDpL,SI,Dpm
        DOUBLE PRECISION Dp
        INTEGER Ltyp
        INTEGER j,n,ProfStart
        DOUBLE PRECISION    DpProfNew(NrInt+2)

        ProfStart=1

C Lstat is the link status:
C Lstat From	 To
C  0	zone	zone
C  1	 ext	zone
C  2	spec	zone
C  3	zone	 ext
C  4	 ext	 ext
C  5	spec	 ext
C  6	zone	spec
C  7	 ext	spec
C  8	spec	spec
C

	IF (LSTAT1 .EQ. 0) THEN
	  Dp=PzNew(FROM)-PzNew(TO)+DpL(1,I)+DpJ1
	ELSE IF (LSTAT1.EQ.3 .or. LSTAT1.EQ.6) THEN
	  Dp=PzNew(FROM)+DpL(1,I)+DpJ1
	ELSE IF (LSTAT1.EQ.1 .or. LSTAT1.EQ.2) THEN
	  Dp=-PzNew(TO)+DpL(1,I)+DpJ1
	ELSE
	  Dp=DpL(1,I)
	  IF (Dp.LT.0.0) THEN
	    Dp=DpL(2,I)
	  ENDIF
	ENDIF

C DpL(1,I) is only valid for positive Dp, IF Dp<0 THEN, correct

C@empa aw 1993jul01 The change from DpL(1,I) to DpL(2,I) when Dp=0 should not
C@		jump at one point. This could cause convergence problems,
C@		when the solution is near Dp=0.
C@		This is a trial to solve this problem. In the cases I have
C@		testet yet it was succesful. But maybe we should have a more
C@		physical solution.(See H.Phaff Non-convergence of COMIS.
C@		Non horizontal links with buoyancy).
C@		I made an interpolation between DpL(1,I)and DpL(2,I),
C@		when Dpm=Dp+(DpL(1,I)+DpL(2,I)/2) is between +-DDpL.
C@		The gradient for the interpolation is 0.5.

C we have a chimney on a cold house and hot weather outside. Probably we
C will get stratIFication here. None of the thermal pressure differences
C in the link makes sense. This is a stable situation.
C A warm house and cold weather could give a bi-stable flow in the duct.
C Once backdraft occurs the chimney will cool further giving more backdraft.
C Once warm air from the house flows out, it heats up the duct and gives a
C more stable outflow. This phenomenon cannot be simulated accurately by the
C method used, which first assumes positive flow. A difference can occur when
C modelling ducts to outside positive ("From" inside "To" outside) in comparison
C with ducts to the inside positive ("From" outside "To" inside).


          DDpL=(DpL(1,I)-DpL(2,I))
          SI=SIGN(1.,DDpL)
          DDpL=ABS(DDpL)
C make the interpolation range symmetric for from to direction
	  Dpm=(DpL(1,I)+DpL(2,I))/2+Dp-DpL(1,I)
	  IF (ABS(Dpm).LT.DDpL) THEN
	      Dp=Dpm+Dpm*0.5*SI
	  ELSE IF (Dpm.LE.(-DDpL)) THEN
  	      Dp=Dp-DpL(1,I)+DpL(2,I)
	  ENDIF

C this part here is a thought only, we still have to code this
C IF   we are close to the solution
C  - AND the problem indicates that we want to have an accurate simulation
C  - AND the difference between DpL(1,I) and DpL(2,I) is large
C      (Indicates sensitivity for temperature of the crack)
C  - AND the Fma differs signIFicantly From the last
C      value used to calculate the link temperature
C THEN CALL CFT to get a closer value for the temperature of the link.
C  - AND CALL PSTACK1 for calculating the buoyancy of this link
C  - AND make a new addition of Pwind and Pstack in DpL
C  - AND update RhoL.


C update pressure difference profile for large openings with actual pressure
C difference between zones
        IF (Ltyp.EQ.9) THEN
          ProfStart=ProfPtr1
	  IF (LSTAT1 .EQ. 0) THEN
            DO 30 n=1,NrInt+2
              j=n-1+ProfStart
	      DpProfNew(n)=PzNew(FROM)-PzNew(TO)+DpJ1+DpProf(j)
30          CONTINUE
	  ELSE IF (LSTAT1.EQ.3 .or. LSTAT1.EQ.6) THEN
            DO 40 n=1,NrInt+2
              j=n-1+ProfStart
 	      DpProfNew(n)=PzNew(FROM)+DpJ1+DpProf(j)
40          CONTINUE
	  ELSE IF (LSTAT1.EQ.1 .or. LSTAT1.EQ.2) THEN
            DO 50 n=1,NrInt+2
              j=n-1+ProfStart
	      DpProfNew(n)=-PzNew(TO)+DpJ1+DpProf(j)
50          CONTINUE
          ENDIF
	ENDIF

	RETURN
	END

C end rtn DP1


Ch***********************************************************************
      SUBROUTINE MATVEC(Nz,Nl,JAC,FMB,
C                       1  2  3  4
C@lbl dml 1999oct27     & FMBOLD,PzNew,FVNEW,FVOLD,FV2,FT,DpL,FromTo,
     & PzNew,FVNEW,FVOLD,FV2,FT,DpL,FromTo,
     & LSTAT,Mf,Mp,pLiLDat,LDat,RhoL,
     & SQRRhoL,MuL,linInit,DifLim,FmFact,DMZ,RSqrtDff,
     & Njunc,JuncNr,DpJ,LinkDat,pLinkDat,Tl,TempL,NormCrRho,
     & NormCrMu,DpProf,RhoProfF,RhoProfT,ProfPtr,iRFlist,
C@tno jcp 1996Jun13_23:21:03 for PS added Vmet and RhouOut
C@NBI PGS 1999Aug19 - Added argument 'FVOldDefined'.  Read why below
     & Vmet,RhoOut,FVOldDefined)
C***********************************************************************
C Purpose: MATVEC sets up the Jacobian matrix and the flow balance vector
C
C Module : 4.4.3, TG V, hcp/february 15, 1989
C Changes: mkh/february 16, 1989, Structure
C    hcp/mar 1, 1989, Lstat sequence, LSTYP description
C    may 26 3-way flows
C    Jac=0.D0 and Dp .GT. 0.D0 (D added)
C@lbl bvs 1994May26 Dpint not used
C to avoid warnings parameters used
C@empa aw 1991july09 RSqrtDff added
C@empa aw 1991july29 call Tjunc and addition of DpJ to Dp
C@empa aw 1991july29 Njunc,JuncNr,DpJ added
C@empa aw 1991aug16  Parameterlist of TJUNC: Nl added
C@empa vd 1991sep13  Call to FEQ : changed parameter list, Fma also as input
C@empa vd 1991jan21  linkdat, plinkdat, call to FEQN changed
C@empa aw 1992jan28  pass parameters: Tl, TempL,NormCrRho,NormCrMu added
C@empa aw 1992jan28  passparameters: Ctl and Lstyp canceled
C@empa aw 1992apr15 save Fma in FVold
C@empa aw 1993jan25 new locals: DFma1,DFma2,Ltyp
C@empa aw 1993jan25 dfma1, dfma2 as parameter in the call of feqn
C@empa aw 1993jan25 for totflow take two way flow into account
C@empa aw 1993jan25 divide into two cases: one way and two way flow
C@empa aw 1993jul01 new locals: Dpint DDpL SI
C@empa aw 1993jul01 Interpolation between DpL(1,I) and DpL(2,I)
C@empa aw 1993sep13 made the interpolation range symmetric for from to direction
C@lbl dml 1999oct27 Clean up indentation to reflect logical structure.
C@NBI PGS 1999Aug19 - Bugfix. FVOLD wasn't saved!! Crashed with my F90 compiler
C@NBI                 Problem solved by defining FVOLD in subroutine 'NetSlv'
C@NBI                 and as new argument in call from 'NetSlv' to 'StrSlv' 
C@NBI               - Added new argument 'FVOldDefined'
C@lbl dml 1999oct27 Do not pass in FMBOld, which has no computational
C   role in this routine (note this requires change in solver also).
C   Remove copy of FMBNew into FMBOld. Rename FMBNew to FMB.
C@lbl dml 1999oct28 Zero JAC by columns, not rows (Fortran stores
C   arrays by columns, so moving down columns more efficient). To
C   improve cache hits, separate zeroing of matrix, initialization
C   of FMB, and zeroing of FT. Change array indices from K,L to more
C   standard i,j. Remove integer K.
C@lbl dml 1999oct28 Copy assignment L=LSTAT(i) to top of loop over
C   links. Replace non-assignment references to LSTAT(i) throughout
C   loop.
C@lbl dml 1999nov19 Change integer variable initFlg to logical linInit.
C
C Limits :
C
C Pass parameters:
C
C IO # Name         unit     description
C I  1 Nz            -       Actual number of Zones
C I  2 Nl            -       Actual number of Links
C O  3 JAC(MAXZ,*)  kg/s/Pa  Jacobian matrix
C O  4 FMB(*)       kg/s     New mass flow balance per Zone
C O xx FMBOLD(*)    kg/s     Old mass flow balance per Zone
C I  5 PzNew(*)     Pa       New pressure for each zone
C O  6 FVNEW(*)     kg/s     New mass flow for each link
C I  7 FVOLD(*)     kg/s     Old mass flow for each link
C    8 FV2(*)       kg/m3    2-way flows
C O  9 FT(*)        kg/s     Total flow into each zone
C I 10 DpL(2,*)     Pa       Extra  pressures per link
C I 11 FromTo(2,*)   -       Nodenumbers that are connected by this link
C I 12 LSTAT(*)      -       Link status=0  wind wind
C                               1  zone zone
C                               2  zone wind
C                               3  wind zone
C I 13 Mf(*)         -       Multiplication for mass flow
C I 14 Mp(*)         -       Multiplication for pressure
C I 15 pLiLDat(*)    -       Link Leakage pointer to start element in Ldat
C I 16 LDat(*)      multi    Link Data coefficients
C I xx LSTYP(*)      -       Link structure type (Link Temp calculation)
C O 17 RhoL(*)      kg/m3    Air density per link
C O 18 SQRRhoL(*)   Skg/m3   SQR Air density per link
C O 19 Mul(*)                Viscosity per link
C O 20 linInit       -       true=initialize with flow exp=1
C                            false=normal iteration
C I 21 DifLim       Pa       limit for linearization of crackflow
C I 22 FmFact        -       factor to multiply kg/s into user units
C   23 DMZ          kg/s     changes of the Mass per Zone, (ie temperature)
C   xx CtL(2,MaxL)   -       correction for Cm , pos and neg flow
C I 24 RSqrtDff              Reciprocal of the squarroot of the Darcy friction
C                               factor
C I 25 Njunc         -       Number of T-junctions
C I 26 JuncNr(MaxZ,5)-       Array with T-junction data
C O 27 DpJ          Pa       Extra pressure from T-junction per link
C I 28 LinkDat       multi   Array with additional data for individual links
C I 29 pLinkDat      -       pointer array for LinkDat
C O 30 Tl(MaxL)     0C       Mean temperatures of the air in the links
C I 31 TempL(2,MaxL) 0C      Air temperature in from and to zone at link
C                               height
C I 32 NormCrRrho    Kg/m^3  airdensity at crack testconditions
C I 33 NormCrMu      Pa.s    dynamic air viscosity at crack testconditions
C I 34 DpProf        Pa      Array with differential pressure profile for
C                            Large Openings
C I 35 RhoProfF      Kg/m3   Array with density profile in FROM zone
C I 36 RhoProfT      Kg/m3   Array with density profile in TO zone
C I 37 ProfPtr        -      Pointer from link number to the start of the
C I                          corresponding profile (differential pressure
C                            and density)
C I 38 iRFlist(maxL)  -      List of reference links for RF component
C   39 Vmet
C   40 RhoOut
C
C example:
Ch***********************************************************************

        IMPLICIT NONE
      include 'comv-uni.inc'
      include 'comv-par.inc'

C Definition pass parameters
      INTEGER Nz,Nl,FromTo(2,maxl),LSTAT(maxl),pLiLDat(maxl)
      logical linInit
      INTEGER Njunc, JuncNr(MaxZ,5)
      DOUBLE PRECISION JAC(MAXZ,MAXZ),FMB(maxz),PzNew(maxz)
C@empa aw 1997sep18 DpL as DOUBLE PRECISION
C@empa aw 2002dec04 DpL enlarged from DpL(2,MaxL) to DpL(3,MaxL) to have a space
C@empa aw 2002dec04 for the final pressure differnce
      DOUBLE PRECISION DpL(3,MaxL)
CC    REAL FMBOLD(maxz)
      REAL FVNEW(maxl),FVOLD(maxl),FV2(2,maxl),FT(maxz),
     &   Mf(maxl),Mp(maxl),LDat(maxd),RhoL(2,maxl),
     &   SQRRhoL(2,maxl),MuL(2,maxl),DifLim,FmFact
      REAL DMZ(maxz)
      REAL RSqrtDff(maxl)
      REAL Dpj(maxl)
      REAL LinkDat(maxd)
      INTEGER pLinkDat(maxl)
      REAL Tl(maxl),TempL(2,maxl),NormCrRho, NormCrMu
C@empa aw 1997sep12 DpProf DOUBLE PRECISION
CC        REAL DpProf(MaxLo*(NrInt+2))
      DOUBLE PRECISION DpProf(MaxLo*(NrInt+2))
      REAL RhoProfF(MaxLo*(NrInt+2))
      REAL RhoProfT(MaxLo*(NrInt+2))
C@tno jcp 1996Jun13_23:21:03 for PS added Vmet and RhouOut
      REAL Vmet,RhoOut
      INTEGER ProfPtr(MaxL),iRFlist(maxL)

C     ! Local parameters.
CC    INTEGER FROM,TO,Start,Start1,i,K,L,key
      INTEGER FROM,TO,Start,Start1,i,L,key
      REAL DFma,fma1,fma2
      REAL totflow
      REAL DDpL,SI,Dpm
      DOUBLE PRECISION Dp,Fma
      DOUBLE PRECISION DFma1,DFma2
      INTEGER Ltyp
      INTEGER j,n,ProfStart
      DOUBLE PRECISION DpProfNew(NrInt+2)
C@NBI PGS 1999Aug19 - Extracted FVOldDefined from INCLUDE file 'comv-par.inc'
C@NBI                 Now a subroutine argument sent down from sub. 'NetSlv'
        LOGICAL FVOldDefined

C@tno jcp 1996Apr07_15:22:32 1 line out
CC    FVold(1)=Fvold(1)

      totflow=0.0

C     ! Initialize JAC, FMB, and FT.
CC    DO 20 K=1,Nz
CC      DO 10 L=1,Nz
CC        JAC(K,L)=0.D0
CC10    CONTINUE
CC      FMBOLD(K)=FMB(K)
CCC       ! Incoming flows are counted positive in FMB . When DMZ is
CCC       ! positive in time, more air has to enter the zone to balance
CCC       ! that. So in that case FMB has to start negative.
CC        FMB(K) = -DMZ(K)
CC        FT(K) = 0.0
CC20    CONTINUE
      do 11 j=1, nZ
        do 10 i=1, nZ
          JAC(i,j) = 0.0D0
10      continue
11    continue
      do 12 i=1, nZ
C       ! Incoming flows are counted positive in FMB . When DMZ is
C       ! positive in time, more air has to enter the zone to balance
C       ! that. So in that case FMB has to start negative.
        FMB(i) = -DMZ(i)
12    continue
      do 13 i=1, nZ
        FT(i) = 0.0
13    continue

      IF( .not.linInit ) THEN
        IF (Njunc .GT.0) THEN
          CALL TJunc(Nl,PZNew,FVNew,FromTo,RhoL,LDat,pLiLDat,
     &      Njunc,JuncNr,DpJ,KEY)
        ENDIF
      ENDIF

C     ! Determine the from and to node.
      DO 200 i=1,Nl
        ProfStart=1
        FROM=FromTo(1,i)
        TO=FromTo(2,i)
        L = LSTAT(i)
C       ! Lstat is the link status:
C       ! Lstat  From  To
C       !   0   zone   zone
C       !   1   ext    zone
C       !   2   spec   zone
C       !   3   zone   ext
C       !   4   ext    ext
C       !   5   spec   ext
C       !   6   zone   spec
C       !   7   ext    spec
C       !   8   spec   spec
        if( L .eq. 0 ) then
          Dp=PzNew(FROM)-PzNew(TO)+DpL(1,i)+DpJ(i)
        else if( L.eq.3 .or. L.eq.6 ) then
          Dp=PzNew(FROM)+DpL(1,i)+DpJ(i)
        else if( L.eq.1 .or. L.eq.2 ) then
          Dp=-PzNew(TO)+DpL(1,i)+DpJ(i)
        else
C         ! This link is not in the network, it goes from a known to a
C         ! known pressure. Skip it here, but calculate the flow, when
C         ! presenting the output.
          goto 200
        endif

C       ! DpL(1,i) is only valid for positive Dp, IF Dp<0 THEN, correct.
C@empa aw 1993jul01 The change from DpL(1,i) to DpL(2,i) when Dp=0
CC      ! should not jump at one point. This could cause convergence
CC      ! problems, when the solution is near Dp=0.
CC      !   This is a trial to solve this problem. In the cases I have
CC      ! tested yet it was successful. But maybe we should have a more
CC      ! physical solution.(See H.Phaff Non-convergence of COMIS.
CC      ! Non horizontal links with buoyancy).
CC      !   I made an interpolation between DpL(1,i)and DpL(2,i),
CC      ! when Dpm=Dp+(DpL(1,i)+DpL(2,i)/2) is between +-DDpL.
CC      !   The gradient for the interpolation is 0.5.
CC      !   We have a chimney on a cold house and hot weather outside.
CC      ! Probably we will get stratification here. None of the thermal
CC      ! pressure differences in the link makes sense. This is a stable
CC      ! situation. A warm house and cold weather could give a
CC      ! bi-stable flow in the duct. Once backdraft occurs the chimney
CC      ! will cool further giving more backdraft. Once warm air from
CC      ! the house flows out, it heats up the duct and gives a more
CC      ! stable outflow. This phenomenon cannot be simulated accurately
CC      ! by the method used, which first assumes positive flow. A
CC      ! difference can occur when modeling ducts to outside positive
CC      ! ("From" inside "To" outside) in comparison with ducts to the
CC      ! inside positive ("From" outside "To" inside).
        DDpL=(DpL(1,i)-DpL(2,i))
        SI=SIGN(1.,DDpL)
        DDpL=ABS(DDpL)

C       ! Make the interpolation range symmetric for from to direction.
        Dpm = (DpL(1,i)+DpL(2,i))/2 + Dp - DpL(1,i)
        IF (ABS(Dpm).LT.DDpL) THEN
          Dp=Dpm+Dpm*0.5*SI
        ELSE IF (Dpm.LE.(-DDpL)) THEN
          Dp=Dp-DpL(1,i)+DpL(2,i)
        ENDIF
C@empa aw 2002dec04 Why not to save the presure difference. 
C@empa aw 2002dec04 We need it anyway at least once more for the output 
        DpL(3,i)=Dp

CC      ! This part here is a thought only, we still have to code this.
CC      ! IF we are close to the solution
CC      ! - AND the problem indicates that we want to have an accurate
CC      ! simulation
CC      ! - AND the difference between DpL(1,i) and DpL(2,i) is large
CC      ! (Indicates sensitivity for temperature of the crack)
CC      ! - AND the Fma differs significantly From the last value used
CC      ! to calculate the link temperature
CC      ! THEN CALL CFT to get a closer value for the temperature of
CC      ! the link.
CC      ! - AND CALL PSTACK1 for calculating the buoyancy of this link
CC      ! - AND make a new addition of Pwind and Pstack in DpL
CC      ! - AND update RhoL.

C       ! Start points tp the first data element of this link in LDat(*)
C       ! Start1 points to the first data element of this link in LinkDat(*)
        Start = pLiLDat(i)
        Start1 = pLinkDat(i)

C       ! Update pressure difference profile for large openings with
C       ! actual pressure difference between zones.
        IF (LDat(Start).EQ.9) THEN
          ProfStart=ProfPtr(i)
          if( L .eq. 0 ) then
            DO 30 n=1,NrInt+2
              j=n-1+ProfStart
              DpProfNew(n)=PzNew(FROM)-PzNew(TO)+DpJ(i)+DpProf(j)
30          CONTINUE
          else if( L.eq.3 .or. L.eq.6 ) then
            DO 40 n=1,NrInt+2
              j=n-1+ProfStart
              DpProfNew(n)=PzNew(FROM)+DpJ(i)+DpProf(j)
40          CONTINUE
          else if( L.eq.1 .or. L.eq.2 ) then
            DO 50 n=1,NrInt+2
              j=n-1+ProfStart
              DpProfNew(n)=-PzNew(TO)+DpJ(i)+DpProf(j)
50          CONTINUE
          endif
        ENDIF

C       ! Output if necessary.
        IF (test.ge.1 .AND. secho.ge.7) THEN
          WRITE(CRT,*)'dp',dp
          WRITE(CRT,*)'LDat(Start)',LDat(Start)
          WRITE(CRT,*)'LDat(Start+1)',LDat(Start+1)
          WRITE(CRT,*)'LDat(Start+2)',LDat(Start+2)
          WRITE(CRT,*)'LDat(Start+3)',LDat(Start+3)
          WRITE(CRT,*)'LDat(Start+4)',LDat(Start+4)
          WRITE(CRT,*)'Mf(i)',Mf(i)
          WRITE(CRT,*)'Mp(i)',Mp(i)
          WRITE(CRT,*)'RhoL(1,i)',RhoL(1,i)
          WRITE(CRT,*)'RhoL(2,i)',RhoL(2,i)
          WRITE(CRT,*)'SQRRhoL(1,i)',SQRRhoL(1,i)
          WRITE(CRT,*)'SQRRhoL(2,i)',SQRRhoL(2,i)
          WRITE(CRT,*)'MuL(1,i)',MuL(1,i)
          WRITE(CRT,*)'MuL(2,i)',MuL(2,i)
          WRITE(CRT,*)'linInit',linInit
        ENDIF

C       ! Calculate Fma and DFma.
C       ! Set Fma=FVold(i) as input for crack temperature calculation.
C@tno jcp 1996Apr07_15:21:49 FVold doesn't have a value the first time!
CC      ! FVoldDefined is smuggled in comv-par.inc.
C@NBI PGS 1999Aug19 - FVOldDefined removed from comv-par.inc; it isn't 
C@NBI                 a PARAMETER!
        if (FVoldDefined) then
          Fma=FVold(i)
        else
          Fma=0.0
        end if
        CALL FEQN(Fma,DFma,dfma1,dfma2,Dp,LDat(Start),
     &    Mf(i),Mp(i),RhoL(1,i),RhoL(2,i),
     &    sqrRhoL(1,i),sqrRhoL(2,i),
     &    Mul(1,i),Mul(2,i),linInit,fma1,fma2,
     &    DifLim,FmFact,
     &    RSqrtDff(i),LinkDat(Start1),
     &    Tl(i),Lstat(i),TempL(1,i),TempL(2,i),NormCrRho,NormCrMu,
     &    DpProfNew,RhoProfF(ProfStart),
C@tno jcp 1996Jun13_23:18:26 for PS Vmet and RhoOut
     &    RhoProfT(ProfStart),i,FVnew,iRFlist,Vmet,Rhoout)

C       ! fma is the sum of fma1,fma2. Save these 2 flows in FV2(*)
        FV2(1,i)=fma1
        FV2(2,i)=fma2
C       ! For totflow take two way flow into account.
        totflow=totflow+ABS(fma1)+abs(fma2)
        IF (test.ge.1 .AND. oecho.ge.3) THEN
          WRITE(CRT,1001) 'F=',Fma,'DF',DFma,'dp',dp,'i',i,
     &    'fr',From,'to',To
1001      FORMAT (1X,3(1X,A,E10.3),3(1X,A,I4))
        ENDIF

C       ! Note the difference in LDat(Start) and Mf(i),Mp(i).
C       ! LDat(Start) passes the whole array from the element Start.
C       ! (This saves calculation in FEQN)
C       ! Mf(i),Mp(i) each pass only one element.

C       ! Update the flow vector.
        FVNEW(i) = Fma
        Fvold(i) = Fma
C@NBI PGS 1999May06 - Bugfix. FVoldDefined must set after end of 1st link loop
CC        FvOlddefined=.TRUE.

C       ! TG V uses -FMB in the solver so we changed sign.
C       ! The convention for the sign of the FMB is:
C       ! in the From zone -Fma is added in FMB
C       ! in the  To  zone  Fma is added in FMB
C       ! A positive FMB means more air has to leave the zone to balance,
C       ! a negative FMB means more air has to enter the zone to balance.
C       ! Inwards flow into a zone is positive

C       !   Divide into two cases: one way and two way flow.
C       ! If  the link is a large opening and if it is the only link,
C       ! we fill up the jacobian matrix with dfma1 and dfma2 to prevent
C       ! singular matrix. In all other cases we take the net dfma
C       ! because that gives fewer convergence problems.
        L=Lstat(i)
        Ltyp=ldat(start)
        IF(.NOT.((Nl.EQ.1).AND.(Ltyp.EQ.9))) THEN
C         ! One way flow.
          IF (L .EQ. 0) THEN
C           ! One way, case 0: From and To are zones.
            FMB(FROM)=FMB(FROM)-Fma
            JAC(FROM,FROM)=JAC(FROM,FROM)+DFma
            FMB(TO)=FMB(TO)+Fma
            JAC(TO,TO)=JAC(TO,TO)+DFma
C           ! Update FT the total flow into each zone.
            FT(TO)=FT(TO)+Fma1+Fma2
            FT(FROM)=FT(FROM)+Fma1+Fma2
C           ! Here we only update the lower triangle
C           ! IF both have to be there, remove this IF-statement.
            IF (FROM .GT. TO) THEN
              JAC(FROM,TO)=JAC(FROM,TO)-DFma
            ELSE
              JAC(TO,FROM)=JAC(TO,FROM)-DFma
            ENDIF

          ELSE IF (L.EQ.3 .OR. L.eq.6) THEN
C           ! One way, case 2: From is a zone.
            FMB(FROM)=FMB(FROM)-Fma
            JAC(FROM,FROM)=JAC(FROM,FROM)+DFma
C           ! Update FT the total flow into each zone.
            FT(FROM)=FT(FROM)+Fma1+Fma2

          ELSE IF (L.EQ.1 .OR. L.EQ.2) THEN
C           ! One way, case 3: To is a zone.
            FMB(TO)=FMB(TO)+Fma
            JAC(TO,TO)=JAC(TO,TO)+DFma
C           ! Update FT the total flow into each zone.
            FT(TO)=FT(TO)+Fma1+Fma2
          ENDIF
        ! End one-way flow.

        ELSE
C         ! Two way flow (just one link which is a large opening
C         ! Ltyp = 9) in foward direction.
          IF (L .EQ. 0) THEN
C           ! Two way, case 0: From and To are zones.
            FMB(FROM)=FMB(FROM)-Fma1
            FMB(TO)=FMB(TO)+Fma1
            JAC(FROM,FROM)=JAC(FROM,FROM)+DFma1
            JAC(TO,TO)=JAC(TO,TO)+DFma1
C           ! Here we only update the lower triangle.
C           ! If both have to be there, remove this if-statement.
            IF (FROM .GT. TO) THEN
              JAC(FROM,TO)=JAC(FROM,TO)-DFma1
            ELSE
              JAC(TO,FROM)=JAC(TO,FROM)-DFma1
            ENDIF
C           ! In backward direction.
            FMB(FROM)=FMB(FROM)+Fma2
            FMB(TO)=FMB(TO)-Fma2
            JAC(FROM,FROM)=JAC(FROM,FROM)-DFma2
            JAC(TO,TO)=JAC(TO,TO)-DFma2
C           ! Here we only update the lower triangle.
C           ! If both have to be there, remove this if-statement.
            IF (FROM .LT. TO) THEN
              JAC(FROM,TO)=JAC(FROM,TO)+DFma2
            ELSE
              JAC(TO,FROM)=JAC(TO,FROM)+DFma2
            ENDIF
C           ! Update FT the total flow into each zone.
            FT(TO)=FT(TO)+Fma1+Fma2
            FT(FROM)=FT(FROM)+Fma1+Fma2

          ELSE IF ((L.EQ.3) .OR. (L.eq.6)) THEN
C           ! Two way, case 2: From is a zone.
C           ! In foward direction.
            FMB(FROM)=FMB(FROM)-Fma1
            JAC(FROM,FROM)=JAC(FROM,FROM)+DFma1
C           ! In backward direction.
            JAC(FROM,FROM)=JAC(FROM,FROM)-DFma2
            FMB(FROM)=FMB(FROM)+Fma2
C           ! Update FT the total flow into each zone.
            FT(FROM)=FT(FROM)+Fma1+Fma2

          ELSE IF (L.EQ.1 .OR. L.EQ.2) THEN
C           ! Two way, case 3: To is a zone
C           ! In foward direction.
            FMB(TO)=FMB(TO)+Fma1
            JAC(TO,TO)=JAC(TO,TO)+DFma1
C           ! In backward direction.
            FMB(TO)=FMB(TO)-Fma2
            JAC(TO,TO)=JAC(TO,TO)-DFMA2
C           ! Update FT the total flow into each zone.
            FT(TO)=FT(TO)+Fma1+Fma2
          ENDIF
        ! End two-way flow.

        ENDIF

200   CONTINUE

C@NBI PGS 1999May06 - FVoldDefined=.TRUE. moved here from above
        FvOlddefined=.TRUE.
      IF (test.ge.1 .AND. secho.ge.8) THEN
        WRITE (CRT,*) 'totflow=',totflow
      ENDIF

C     ! We summed in- and out-going flows in FT, to have the incoming
C     ! flow. We have to divide by 2. This is a pity, it takes time
C     ! you know?
      DO 300 i=1,NZ
        FT(i)=FT(i)/2
300   CONTINUE

C     write (CRT,*) 'JAC'
C     DO 400 i=1,Nz
C       DO 400 j=1,Nz
C         write (CRT,*)  JAC(i,j)
C400  CONTINUE

      RETURN
      END
