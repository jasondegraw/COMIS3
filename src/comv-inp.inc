C+********************************************************* comv-inp.inc
C***********************************************************************
C
C This is an INCLUDE file for COMIS
C
C***********************************************************************
C
C Changes:
C@lbl bvs 1995Jun    All references to CGO changed to COS.  CGO output
C		     removed (Lotus 123 file).  Only COS files (table
C		     output with separators specified) written now.
C@empa vd 1992feb2   Completely re-edited version
C                    A detailed description of the parameters is given
C                    in the COMVEN Programmer's Guide
C
C In addition to the reordering, the following changes have been made:
C 1) Common block for multischedule separated into /MULTI1 and /MULTI2
C 2) REAL LDatSub(10) deleted because not used in the code
C 3) BuAngle renamed to AngBui as named in the input reading routines
C 4) Nunits deleted because not used as global variable
C 5) Parameter MaxCGOk renamed to MaxCOSk and moved to com-par.inc
C 6) Common lastitem... named to Common/CHECHCIF/lastitem..
C
C@empa vd 1992feb02 end
C@empa aw 1993feb08 Parameter MaxDir introduced
C@empa aw 1993feb09 Zll (Zll= Zl + Actstart height)introduced
C@empa aw 1993mar09 Cold introduced
C@empa aw 1993mar11 COutLine introduced
C@empa vd 1993may12 FErrIn introduced
C@empa aw 1994mar10 COSs, COSLun, OCase added
C@empa aw 1994mar10 OutLine,IOutline,COutline with MCOSCol dimensioned
C@empa aw 1994oct25 Dimension of LayPtr changed
C@empa aw 1994nov17 Hz,Dz,Wz introduced
C@empa aw 1994nov17 NET-ZT added
C@empa aw 1994nov22 TmZ,CRelmZ,XhmZ introduced
C@empa aw 1994nov22 LayPrec,pZoLP introduced
C@empa hf 1995jan04 OUnitFkt,AgeConv,EConv added,
C@NBI PGS 1999Aug10 - timestep 'Interval' added
C@lbl dml 1999nov18 Replace solver control parameters noInit and useOpz
C   with stp1Init, stp2Init, and rhoSync. Replace default values DnoInit
C   and DuseOpz with Dstp1Init, Dstp2Init, and DrhoSync. Add description
C   of solver control parameters.
C@lbl dml 1999nov19 Rename variable Newton in COMMON block /PRCONT/
C   to nNewt.
C@NBI PGS 2000Jul18 - Changed file extensons .cho to .coh, .cos to .cos,
C@NBI                 and .uof to .cou, so that all output file extensions
C@NBI                 start with .co ("o" for output) and all input files
C@NBI                 should have extension .ci? ("i" for input)
C@NBI PGS 2000Oct08 - Just did some overdue tidying, no syntax changes


C Max parameters
C-----------------------------------------------------------------------
C All max parameters are defined in comv-par.inc

        include 'comv-par.inc'


C Version
C-----------------------------------------------------------------------
C@empa aw 2001feb05 CopyrightStr added
        CHARACTER*78 VersionStr,CopyrightStr
        COMMON/Versio/ VersionStr,CopyrightStr


C General settings
C-----------------------------------------------------------------------
C@tno jcp 1996Jun26_21:22:18 added general settings of comis

        INTEGER UseMonthName,WCRT
        COMMON/General/ UseMonthName,WCRT


C Keywords & names
C-----------------------------------------------------------------------
C added KeysU keywords for user units

        CHARACTER Keys*(MKeys),aName*(MAName),KeysU*(Ukeys)
        INTEGER nName,pName
        COMMON/CKeysNam/ Keys,aName,KeysU
        COMMON/IKeysNam/ nName,pName

C@tno jcp 1996Mar14_12:06:13 add NdKey for use in inSIMU (has been a local varia
CC      INTEGER pKeyRec(mkeys/10+1),pDAF
C@empa aw 2000nov28 pdaf1 added
        INTEGER pKeyRec(mkeys/10+1),pDAF,NDkey,pdaf1
CC      COMMON/KeyNam2/ pKeyRec,pDAF
        COMMON/KeyNam2/ pKeyRec,pDAF,NdKey,pdaf1

        INTEGER pKeNaNr(mkeys/10+1),pKeNaEn(mkeys/10+1),pNamRec(maname)
        COMMON/DAFACCESS/pKeNaNr,pKeNaEn,pNamRec
C@tno jcp 1996May30_11:29:01 HeaderNr reintroduced(used by inCpVal and inSchMete
        INTEGER HeaderNr
        COMMON/HEADER/HeaderNr

C@empa aw 1999dec08 Flag for the first call of InSimu
        LOGICAL FirstInSimu
        COMMON/FlgKeyW/FirstInSimu


C User unit conversion
C-----------------------------------------------------------------------

C replaced by Conv(15,16) Conv(unit,line) contains
C per line values for an quantity (as mass flow rate)
C concentration has been placed on line 16
C Now there is place for 15 different units per quantity
C (concentration uses 13 now)
C temperature uses line 3 (shift coefficient) and 7 (scale factor)

        REAL Conv(ConvEnt,NoUnit),ifact(NoUnit),ofact(NoUnit)

C                   user input * ifact => COMIS internal SI unit
C       COMIS internal SI unit * ofact => user output
C       See comv-par.inc

C       index 1 = AIRL(eakage)                   UnitCm
C             2 = FLOW (vent. flowrates)         UnitFma
C             3 = PRES(sure)                     UnitP
C             4 = TEMP(erature)                  UnitTmul
C             5 = HUMI(dity)                     UnitXh
C             6 = SOUR(ce)                       UnitPSou
C             7 = SINK                           UnitPSin
C             8 = CONC(entration of pollutants)  UnitPConc
C             9 = CFLO(w rate of fans)           UnitFva
C            10 = VELO(city of wind)             UnitW
C            11 = ACH  (air change rate)         UnitRate
C            12 = MEAN (age of air)              UnitAge
C            13 = ENER(rgy)                      UnitE
C            14 = PROF(ile, wind profile type)   UnitProf
C If quantities have to be added then insert them here--------------<
C and move UnitToff, UnitPol and UnitFvFlag up
C            15 = temperature offset             UnitToff
C            16 = Flag=1 :fan volume flow rate   UnitFvFlag
C                 Flag=0 :fan mass flow rate
C            17 = concentration pollutant 1      UnitPolCo + 1
C            18 = concentration pollutant 2      UnitPolCo + 2
C            19 = concentration pollutant 3      UnitPolCo + 3
C            ...
C         16 +n = concentration pollutant n      UnitPolCo + n
C   MaxC +16 +n = source strength, pollutant n   UnitPolSo + n
C 2*MaxC +16 +n = sink strength, pollutant n     UnitPolSi + n

        COMMON/USRuni1/Conv,ifact,ofact

C Flags volume(=1) or mass (=0) units
C added flags that signal changed concentration
C units and delays unit conversion until POLDES is read
C@NBI PGS 2000Oct09 - FlgIConc and FlgOConc now redundant, so removed
C       INTEGER VM(15)
CC      INTEGER VM(15),FlgIConc,FlgOConc,FlgPolDes
        INTEGER VM(15),FlgPolDes

CC      COMMON/USRuni3/ VM,FlgIConc,FlgOConc,FlgPolDes
        COMMON/USRuni3/ VM,FlgPolDes

        CHARACTER*(MUstrL) UnitStr(MUstr)
        CHARACTER*(MUUstrL) IUnit(NoUnit),OUnit(NoUnit)

        COMMON/USRUNI2/Unitstr,IUnit,OUnit


C Problem description
C-----------------------------------------------------------------------
 
C@empa aw 2005oct06 longer file names
        CHARACTER CIFs*160,COFs*160,COUs*160,ProName*800,VerName*160
        CHARACTER COSs*160
        COMMON/CIFPROB/CIFs,COFs,COUs,ProName,VerName,COSs


C Solver control parameters
C-----------------------------------------------------------------------

C  relaxV    Relaxation coefficient for solvers that don't choose own.
C  epsFA     Absolute convergence tolerance. Sum of flows into zone less
C            than this are deemed small.
C  epsFR     Relative convergence tolerance. Fraction of
C            (sum flows/sum abs flows) less than this are deemed small.
C  epsCJ     Stagnation tolerance. Any change in pressures greater than
C            this is deemed significant.
C  difLim    Max pressure drop where flow considered laminar. Not really
C            a solver control parameter-- controls element models.
C  nNewt     Number of Newton-Raphson iterations to take before switching
C            to relaxation routine, for solvers that don't define own
C            behavior.
C  slvSel    Solver algorithm to use.
C            1=Newton-Raphson, using relaxV.
C            3=Walton Steffensen.
C            5=Walton switch relaxV to 0.5, 0.75, and 1.0.
C            6=Trust region. Ignores relaxV, epsCJ, nNewt.
C  mIter     Maximum number of iterations (Jacobian factorizations) to
C            permit the solver to try.
C  nIter     Actual number of iterations taken for the current time step.
C  stp1Init  Method for initializing the first time step.
C            0=zero pressures
C            1=take one iteration on system with linear flow models
C  stp2Init  Method for initializing subsequent time steps.
C            0=zero pressures.
C            1=take one iteration on system with linear flow models.
C            2=retain pressures from previous time step.
C  rhoSync   Rule for updating zone densities within a time step.
C            false=fix density using initial zone pressure.
C            true=adjust density as zone pressure guess changes.
C
C Be careful - /PRCONT/ is also defined in comv-ma2.f and comv-mac.f
        REAL RelaxV,EpsFA,EpsFR,EpsCJ,DifLim
CC      INTEGER nNewt,useOpz,noInit,slvSel,mIter,nIter
        INTEGER nNewt,slvSel,mIter,nIter,stp1Init,stp2Init
        LOGICAL rhoSync
        COMMON/PRCONT/ relaxV,epsFA,epsFR,epsCJ,nNewt,difLim,
CC   &  useOpz,noInit,slvSel,mIter,nIter
     &  slvSel,mIter,nIter,stp1Init,stp2Init,rhoSync


C Air flow components input data
C-----------------------------------------------------------------------

        INTEGER Ltyp,Ntyp,nUsrAfc,pUsrAfc(MaxTyp),
     &  AFCNr(MaxTyp),UsTreeN(MTyT),SiStart(MaxTyp)

        COMMON/AFC1/Ltyp,Ntyp,nUsrAfc,pUsrAfc,AFCNr,UsTreeN,SiStart

        CHARACTER AFCName*500,UsrAfc(MaxTyp)*20,
     &  UsTree(MTyT)*20,UsLarg*20,UsSmal*20

        COMMON/AFC2/AFCName,UsrAfc,UsTree,UsLarg,UsSmal


C Air flow component data
C-----------------------------------------------------------------------

        REAL Ldat(MaxD)
        INTEGER MLDat,PtrL,ptrL0,npair

        COMMON/AFC3/Ldat,MLdat,PtrL,ptrL0,npair


C Transition Data
C-----------------------------------------------------------------------

        REAL ReLam,ReTurb
        COMMON/TRANSIT/ReLam,ReTurb


C Zone Input Data
C-----------------------------------------------------------------------

        REAL Tz(MaxZ),Zz(MaxZ),Vz(MaxZ),XhZ(MaxZ)
        REAL RhoZ(MaxZ),RhoDrZ(MaxZ),Muz(MaxZ)
        REAL Hz(Maxz),Dz(MaxZ),Wz(MaxZ)
        REAL TmZ(MaxZ),CRelmZ(MaxZ),XhmZ(MaxZ)
        INTEGER Nz,ZoNr(MaxZ),ZoTreeN(MZoT)

        COMMON/NETZON1/Tz,Zz,Vz,XhZ,RhoZ,RhoDrZ,Muz,
     &                 Nz,ZoNr,ZoTreeN,Hz,Dz,Wz,TmZ,CRelmZ,XhmZ

        CHARACTER ZoNa(MaxZ)*20,ZoNaU(MaxZ)*20,
     &  SchZ(MaxZ)*50,
     &	Zotree(MZoT)*20,ZoLarg*20,ZoSmal*20

        COMMON/NETZON2/ZoNa,ZoNaU,SchZ,ZoTree,ZoLarg,ZoSmal


C Zone Layers
C-----------------------------------------------------------------------

        REAL LayDat(MaxLay),LayPrec(3*(MaxNrLay+MaxZ))
        INTEGER Nzl,LayPtr(2,0:MaxZ),LayPtr1,MLayDat,pZoLP(2,MaxZ)
        COMMON/NETZL1/Nzl,LayDat,LayPtr,LayPtr1,MLayDat,LayPrec,pZoLP

        CHARACTER ZLNa(MaxZ)*20
        COMMON/NETZL2/ZLNa


C Zone Pollutants
C-----------------------------------------------------------------------

C extended the range for Concentrations
C now the negative elements are directly the outside concentrations at the cp
C points. Take care that all arrays passed via calls of routines should have
C the same dimension within that routine (i.e. in Poltr1)
C@empa aw 2000dec18 rkz(MaxC,MaxZ) first order decay per pollutant and zone added
C@empa aw 2000dec18 Osource(MaxSP) deleted
        REAL C(MaxC,-MaxW:MaxZ+MaxWC),Cold(MaxC,MaxZ),
C@tno jcp 1996Apr24_16:48:29 Sum of Occupant generated Pollutants per zone
     &  C1(MaxC),Source(MaxC,MaxZ),OccSource(MaxC,MaxZ),
     &  Sink(MaxC,MaxZ)
        REAL rkz(MaxC,MaxZ)
        REAL OSink(MaxC,MaxZ),OrSource(MaxC,MaxZ)
        INTEGER Nzp,Nconc,pSource
        COMMON/NETZP1/C,Cold,C1,Source,Sink,OccSource,
     &  OSink,OrSource,Nzp,Nconc,pSource,rkz

        CHARACTER ZPNa(MaxZ)*20,Cname(MaxC)*30
        COMMON/NETZP2/ZPNa,Cname


C Zone Thermal Properties
C-----------------------------------------------------------------------

        REAL LamdaW(MaxZT),RhoW(MaxZT),CapW(MaxZT),EW(MaxZT),Sz(MaxZT)
        REAL aw(MaxZT)
        INTEGER Nzt,pZTZo(MaxZT),pZTLi(MaxZT)
        LOGICAL SSVFl(MaxZT)
        COMMON/NETZT1/LamdaW,RhoW,CapW,EW,Sz,aw,Nzt,pZTZo,PZTLi,SSVFl

        CHARACTER ZTNa(MaxZT)*20
        COMMON/NETZT2/ZTNa


C External nodes
C-----------------------------------------------------------------------

        REAL OuCF(MaxW),ExtConc(MaxC,MaxW)
        INTEGER Nwind,ExNr(MaxW),ExTreeN(MExT)
        COMMON/NETEXT1/OuCF,ExtConc,Nwind,ExNr,ExTreeN

        CHARACTER ExNa(MaxW)*20,ExTree(MExT)*20,ExLarg*20,ExSmal*20
        COMMON/NETEXT2/ExNa,ExTree,ExLarg,ExSmal


C Facade elements and Cp-values
C-----------------------------------------------------------------------

        REAL CP(MaxCp),Cp1(maxW),CpDir(MaxDir)
        INTEGER Nf,NCpDir,FeNr(MaxF),FeTreeN(MFeT),Facade(MaxF)
        COMMON/CPVALUES1/Cp,Cp1,CpDir,Nf,NCpDir,FeNr,FeTreeN,Facade

        CHARACTER CpName*160,FacadeNa(MaxF)*20,FeNa(MaxF)*20,
     &  FeTree(MFeT)*20,FeLarg*20,FeSmal*20
        COMMON/CPVALUES2/CpName,FacadeNa,FeNa,FeTree,FeLarg,FeSmal


C Link input data and other data related to the individual links
C-----------------------------------------------------------------------
        REAL Zl(2,MaxL),Hfl(MaxL),Mf(MaxL),Mp(MaxL),P3D(MaxL),
     &	FiltL(MaxC,MaxL),Pspec(MaxS),DuAngle(MaxL)
        INTEGER Nl,Nspec,FromTo(2,MaxL),Lstat(MaxL),
     &  LiNr(MaxL),LitreeN(MLiT),TyNr(MaxL),
C to avoid calculation and search of Filter data
C pLiLDatF(MaxL) added. First filterdata=LDat(pLilDatF(iLink))
     &  pLiLdat(MaxL),NJunc,JuncNr(MaxZ,5),pLiLDatF(MaxL),iRFlist(MaxL)

        COMMON/NETLIN1/ Zl,HfL,Mf,Mp,P3D,FiltL,Pspec,DuAngle,
     &  Nl,Nspec,FromTo,Lstat,LiNr,LiTreeN,TyNr,pLiLDat,
     &  Njunc,JuncNr,pLiLDatF,iRFlist

C@tno jcp 1996May09_8:19:10 increased the link name lengt to 20 char
CC        CHARACTER LiNa(MaxL)*5,LiTyNa(MaxL)*10,Sol(MaxL)*5,
C@tno jcp 1996Jun17_12:48:55 schedule name 5 char is too short for Wa:A27
CC        CHARACTER LiNa(MaxL)*20,LiTyNa(MaxL)*10,Sol(MaxL)*5,
        CHARACTER LiNa(MaxL)*20,LiTyNa(MaxL)*10,Sol(MaxL)*10,
     &  FromToS(2,MaxL)*20,LiTree(MLiT)*20,
     &	LiSmal*20,LiLarg*20,TySmal*20,TyLarg*20,RefLink(MaxL)*5

        COMMON/NETLIN2/ LiNa,LiTyNa,SoL,FromToS,LiTree,
     &	LiSmal,LiLarg,TySmal,TyLarg,RefLink

C if =1 then Use Isso 17 (duct coefficients)
        INTEGER UseIsso17

        COMMON/NETLIN3/ UseIsso17


C Link data calculated in PRECAL
C-----------------------------------------------------------------------
C@empa aw 1997sep18 DpL DOUBLE PRECISION
CC        REAL DpL(2,MaxL),DpSt(2,MaxL)
C@empa aw 2002dec04 DpL enlarged from DpL(2,MaxL) to DpL(3,MaxL) to have a space
C@empa aw 2002dec04 for the final pressure differnce
        DOUBLE PRECISION DpL(3,MaxL),DpSt(2,MaxL)
C@empa aw 2000feb21 LinkDat(MaxDL)
CC        REAL    LinkDat(MaxD),BetaL(2,MaxL),Tl(MaxL),TempL(2,MaxL),
        REAL    LinkDat(MaxDL),BetaL(2,MaxL),Tl(MaxL),TempL(2,MaxL),
     &	rhol(2,MaxL),RhoDrL(2,MaxL),SQRRhoL(2,MaxL),
     &  MuL(2,MaxL),XhL(2,MaxL),CL(2,MaxL),CtL(2,MaxL),
     &  Zll(2,MaxL),
     &  RhoProfF(MaxLo*(NrInt+2)),RhoProfT(MaxLo*(NrInt+2))
        INTEGER pLinkDat(MaxL),ProfPtr(Maxl)

C@tno jcp 1996Apr07 Dpprof is used in NetSlv as DpProf(MaxLo*(NrInt+2))
C also for RhiProfF and T
        DOUBLE PRECISION DpProf(MaxLo*(NrInt+2))
        COMMON/LINK1/DpL,DpSt,LinkDat,BetaL,Tl,TempL,rhol,RhoDrL,
     &  SQRRhoL,MuL,XhL,CL,CtL,pLinkdat,Zll,DpProf,ProfPtr,
     &  RhoProfF,RhoProfT


C Building reference height for Cp values
C-----------------------------------------------------------------------

        REAL Zref
        COMMON/Cp     /Zref


C Building location and environment
C-----------------------------------------------------------------------

        REAL Zentr,AngBui,Lat,Lon,Wexp(24),EnvDir(24),AlphMet
        INTEGER MEnvDir
        COMMON/ENV/Zentr,AngBui,Lat,Lon,Wexp,EnvDir,MEnvDir,AlphMet


C Meteo Input data
C-----------------------------------------------------------------------

        REAL ZVmet,Zmet,Vmet,Dmet,Tmet,XhMet,PbMet
        COMMON/MET1/ZVmet,Zmet,Vmet,Dmet,Tmet,XhMet,PbMet

C@NBI PGS 2001Apr20 - Length of Meteo filename had to be extended;
C@NBI                 the other schedule files actually have 160 characters.
CC      CHARACTER MetTime*25,Metname*160,metfile*40,metopt*5
        CHARACTER MetTime*25,Metname*160,metfile*80,metopt*5
        COMMON/MET2/MetTime,Metname,metfile,metopt


C Outside conditions
C-----------------------------------------------------------------------

        REAL G,PbZ,RhoOut,Tout,Xhout,Cout(MaxC)
        COMMON/ENVOUT/G,PbZ,RhoOut,Tout,Xhout,Cout


C Pollutants description
C-----------------------------------------------------------------------

C@tno jcp 1996Apr07_11:11:04 limit concentration and Fictive sourcestrength adde
C@lbl bvs 1999Jun01 moved rk(maxc) here and added code in comv-inp.inc so
C   users can use the decay term for pollutants
C@ lbl bvs 1999Jun24 added POLSTEP1, POL1TIME, POLSTEP2 so user can calculate
C	      pollutants even when there are no ventilation events.
C	      Note that pollutant calculations are done after each ventilation
C	      event in addition to this interval.
C	   Polstep1 is the initial interval (seconds) *until* pol1time, after
C	   which polstep2 is the interval until the end of the simulation

        REAL MM(MaxC),Diffusion(MaxC),PolLimCon(MaxC),
     &  PolQzon(MaxC,0:3),PolQocc(MaxC),rk(MaxC)
        INTEGER polnr,nconcPolDes,Polstep1,Pol1time,Polstep2,Polstep
        COMMON/POLLUT1/MM,Diffusion,polnr,nconcPolDes,PolLimCon,
     &  PolQzon,PolQocc,rk,Polstep1,Pol1time,Polstep2,Polstep

C@tno jcp 1996Jul16_18:36:59 for SCH-POL
        CHARACTER SchPolName*160
        COMMON/POLLUT2/SchPolName


C Pollutant fictive source
C-----------------------------------------------------------------------

C@empa aw 2000feb01
        REAL      FSo(MaxFSo)
        INTEGER   FSPolNr(MaxFSo),FSDep(MaxFSo)
        CHARACTER FSPolN(MaxFSo)*30

        COMMON/POLFICT/FSo,FSPolNr,FSDep
        COMMON/POLFICT2/FSPolN


C Occupants
C-----------------------------------------------------------------------

        REAL OccAge(MaxO),OccL(MaxO),OccM(MaxO),
C@tno jcp 1996Apr11_11:38:00 LOccAct=Last Occupant Activity (previous interval)
C@tno jcp 1996Apr18_12:55:47 added the base source strength of a pollutant
C generated by an occupant (input at OCCUpant description as continuation line)
     &  OccAct(MaxO,MaxZ),LOccAct(MaxO,MaxZ),OOccAct(MaxO),
     &  OccSmo(MaxO),OccPol(MaxO,MaxC)
        COMMON/OCCUPAN1/OccAge,OccL,OccM,OccAct,LOccAct,
     &  OOccAct,OccSmo,OccPol

C@tno jcp 1996Apr04_23:47:29 track the maximum Occupant number used (to check)
C MaxOccNrH=for Histograms D=for occupant description
C@tno jcp 1996Apr07_10:36:51 added Number of occupants in a zone (like
C OccAct(*))
C@tno jcp 1996Apr11_11:38:00 LOccNum=Last Occupant Number (previous interval)
        INTEGER MaxOCCNrH,MaxOCCNrD,OccNum(MaxO,MaxZ),
C@tno jcp 1996Apr24_13:48:55 OccPolNr added to store the number of the matching
     &   LOccNum(MaxO,MaxZ),OccPolNr(MaxO,MaxC)
        COMMON/OCCUPAN2/MaxOccNrH,MaxOCCNrD,OccNum,LOccNum,OccPolNr

C@tno jcp 1996Apr05_16:17:48 occName added
C@tno jcp 1996Apr18_12:54:29 OccPolN added this is the name of pollutant of
C the source(s) by an occupant
        CHARACTER OccSex(MaxO)*20,OccName(MaxO)*20,
     &  OccPolN(MaxO,MaxC)*10
        COMMON/OCCUPAN3/OccSex,OccName,OccPolN

        LOGICAL OccAsSource
        COMMON/OCCUPAN4/OccAsSource

C@lbl bvs 1994Jun7 removed 3D-building variables - only used in COMIN



C@empa aw 2001may28
C@empa aw 2002aug12 NTInp,TInp,TInpTreeN,NLS,LSP,TInpNr,SOSP,SiSP,NSoS,NSiS,
C@empa aw 2002aug12 TInpNa,TInpNaU,TInptree,TInpLarg,TInpSmal canceled
C@empa aw 2002aug12 TRSoIS,TRSiIS changed to REAL
C References to BUI Varibles
C-----------------------------------------------------------------------
CC        INTEGER NTInp,TInp(MaxTInp),TInpTreeN(MTIT),NLS,LSP(MaxTInp,2)
CC        INTEGER TInpNr(MaxTInp)
CC        INTEGER NSoS,NSiS,TRSoIS(MaxC,MaxZ),TRSiIS(MaxC,MaxZ)
CC        INTEGER SOSP(MaxTInp,3),SiSP(MaxTInp,3),NPC
CC        REAL    TASch(MaxTInp/2),XhASch(MaxTInp/2),TRLiIS(MaxL)
CC
CC
CC        COMMON/TRNINP/NTInp,TInp,TInpTreeN,NLS,LSP,TInpNr,TRLiIS,
CC     &  NSoS,NSiS,TRSoIS,TRSiIS,SOSP,SiSP,TASch,XhASch,NPC
CC
CC        CHARACTER TInpNa(MaxTInp)*20,TInpNaU(MTIT)*20,
CC     &  TInptree(MTIT)*20,TInpLarg*20,TInpSmal*20
CC
CC        COMMON/TRNINP2/TInpNa,TInpNaU,TInptree,TInpLarg,TInpSmal

        INTEGER NPC
		REAL    TASch(MaxTInp/2),XhASch(MaxTInp/2),TRLiIS(MaxL)
		REAL    TRSoIS(MaxC,MaxZ),TRSiIS(MaxC,MaxZ)


        COMMON/TRNINP/TRLiIS,TRSoIS,TRSiIS,TASch,XhASch,NPC
  





C Reference values for crack parameters
C-----------------------------------------------------------------------

        REAL NormCRt, NormCrRho, NormCrMu,NormCrPb,NormCrXh
        COMMON/NORMCR/NormCRt,NormCrRho, NormCrMu,NormCrPb,NormCrXh


C Calpre (can't call it Precal because there is a subroutine by that name)
C-----------------------------------------------------------------------

C@lbl bvs 1995Sep15 added wprofu which is the current value of iUnit(UnitProf)
C		when the ENV-WINd values are read in
        REAL Alpha,VeRef,pwind(MaxW)
        character*10 wprofu
        COMMON/CALPRE/Alpha,VeRef,pwind,wprofu


C Post Calculation
C-----------------------------------------------------------------------

        REAL    Q(0:MaxZ,0:MaxZ),Qm(0:MaxZ,0:MaxZ),Qold(0:Maxz,0:Maxz)
C@tno jcp 1996May23_16:16:47 add a matrix to split external in the separate
C external nodes.
C FmMat(1,2) will contain the positive value of the frow from zone 1 to zone 2
        REAL FmMat(MaxZ+MaxW,MaxZ+MaxW)

        REAL    TauR(MaxZ),TauB, ACB
        LOGICAL CalcFM
C@tno jcp 1996May23_16:20:10 FmMat
CC        COMMON/CALPOS/Q,Qm,Qold,TauR,TauB,ACB,CalcFM
        COMMON/CALPOS/Q,Qm,Qold,TauR,TauB,ACB,CalcFM,FmMat


C Zone ventilation data
C-----------------------------------------------------------------------

        DOUBLE PRECISION Pz(MaxZ),FmB(MaxZ)
        REAL Ft(MaxZ),DMZ(MaxZ)
C@lbl bvs 1997Jun9 InitFlg removed from common - now arguments to FLCON1-4
C@lbl dml 1999nov19 Note changed initFlg from integer to logical linInit.
CC        INTEGER InitFlg
        COMMON/DZONVENT/Pz,FmB
        COMMON/RZONVENT/Ft,DMZ
C@lbl bvs 1997Jun9 InitFlg removed from common - now arguments to FLCON1-4
CC       COMMON/IZONVENT/InitFlg


C Link ventilation data
C-----------------------------------------------------------------------

C@empa aw 2000mar15 FVold, FVoldDefined added
        REAL Fv2(2,MaxL),FVnew(MaxL),RSqrtDff(MaxL),DpJ(MaxL)
		REAL FVOLD(maxl)
        INTEGER LamFlg(MaxL)
		LOGICAL FVoldDefined
        COMMON/LINKVENT/Fv2,FvNew,RSqrtDff,DpJ,Lamflg,FVold,FVoldDefined


C Spreadsheet file output
C-----------------------------------------------------------------------
        INTEGER COSLun(MaxCOSk)
        COMMON /COSLuns/COSLun

        character*3 KeyW(MaxCOSk)
        COMMON /KeyWords1/KeyW

        INTEGER NumKeyW
        INTEGER KWUnitNr(MaxCOSk)
        COMMON /KeyWords2/NumKeyW,KWUnitNr

        INTEGER COSline(MaxCOSk)
        INTEGER TabLin,OCase
        COMMON /COSlns/COSline, TabLin, OCase

        character*160 COSRange(MaxCOSk)
        COMMON /COSRanges/ COSRange

        character*8 SepStr
        COMMON /SepString/ SepStr

        DOUBLE PRECISION OutLine(MCOSCol)
        INTEGER IOutLine(MCOSCol)
        INTEGER OutIDX
        COMMON /OutLn/ OutLine,IOutLine, OutIDX

        CHARACTER*15 COutline(MCOSCol)
        COMMON /OutLn2/ COutLine

C@tno jcp 1996Mar28_13:33:27 Histograms
C storage space for used 1..50 histograms and definition of 1..20 histogram type
C@empa aw 2000feb08 HistoX redimensioned
CC        REAL HistoAr(MHisAr),HistoX(MHisTy,10)
        REAL HistoAr(MHisAr),HistoX(MHisTy,3)
        COMMON /Histo1/ HistoAr,HistoX

C HistUsed(*,1)=startposition in HistoAr(*) per used 1..50 histograms
C HistUsed(*,2)=Histogramtype ->(HistoX)    ,,   ,,
C HistUsed(*,3)=PolN ->(PolLimCon,PolFicSou),,   ,,
C HistStart is a counter that runs up for every used histogram while in &-PR-SIM
C NHisto contains the maximum histogram type number defined at &-HISTO
        INTEGER HistUsed(MHisUs,3),HistStart,Nhisto
        COMMON /Histo2/ HistUsed,HistStart,Nhisto

C name per used 1..50 histograms
        CHARACTER*40 HistUsedN(MHisUs)
        COMMON /Histo3/ HistUsedN


C Arrays for OutputOption 'T' are mean values; 'H'=histogram
C@tno jcp 1996Mar29_16:11:47 practically the whole -T option copied for -H
        DOUBLE PRECISION SumDT, OldT
        DOUBLE PRECISION OldH
C@empa aw 2000feb09 MaxOOH2
CC        REAL ROOptT(2,MaxOOT),ROOptH(MaxOOH)
        REAL ROOptT(2,MaxOOT),ROOptH(MaxOOH2)
        INTEGER IOOptT(3,MaxOOT), KeyWTCnt
C@empa aw 2000feb09 IOOptH(3,MaxOOH) added (pointer to start in ROOptH per Histogram)
CC        INTEGER IOOptH(2,MaxOOH), KeyWHCnt
        INTEGER IOOptH(3,MaxOOH), KeyWHCnt
C@tno jcp 1996Mar29_14:55:03 IsHopt,HistActive added
        LOGICAL IsTOpt,IsHopt,HistActive,HistConc

        COMMON/OutOptT/ SumDT, OldT, ROOptT, IOOptT, KeyWTCnt, IsTOpt
        COMMON/OutOptH/ OldH, ROoptH, IOOptH, KeyWHCnt, IsHopt,
     &        HistActive,HistConc


C Output options
C-----------------------------------------------------------------------

C@tno jcp 1996Apr15_12:01:49 11 output options now, made it 15
C@tno jcp 1996Jul11_11:36:57 InvMatrix flags the need for calculating the invers
        INTEGER outputoptions(15), COUunf,InvMatrix

        COMMON/PROUT/ outputoptions,COUunf,InvMatrix

        character*160 OnScreen,TotalInf

        COMMON/PROUTS/ OnScreen,TotalInf


C Schedule time processing
C-----------------------------------------------------------------------

        INTEGER JdStart,JdStop,IYYY1,IYYY2,MM1,MM2,ID1,ID2,H1,H2,M1,M2,
C@NBI PGS 1999Aug10 - timestep 'Interval' added
     &	S1,S2,SEC1,SEC2, Switch, Interval,
     &  JdSched,IYYY3,MM3,ID3,H3,M3,S3,SEC3,
C@tno jcp 1996Apr25_13:55:15 jd hist start added
     &  JdHist ,IYYY4,MM4,ID4,H4,M4,S4,SEC4,
C@tno jcp 1996Jul24_13:12:46 NsecSim2..4 added
     &  NSecSim2,NSecSim3,NSecSim4,
C@tno jcp 1996May02_15:06:49 Simulen, iPolH2O, useMetH2O, EmptyMeteo added
     &  bufsec, compday, notime, SiMuLen, iPolH2O,
     &  useMetH2O, EmptyMeteo,
C@tno jcp 1996Jul02_22:12:20 prev values to keep track of the event just
C before or at simulation start time  PrvSec is the time in seconds after Start;
     &  PrvSchTyp,PrvSchNa,PrvSec,LastBjDay,CYCLICmeteo


        COMMON/SCHTIM/ JdStart, JdStop, IYYY1, IYYY2, MM1,MM2,ID1,ID2,
C@NBI PGS 1999Aug10 - timestep 'Interval' added
     &	H1,H2,M1,M2,S1,S2,SEC1,SEC2, Switch, Interval,
     &  JdSched,IYYY3,MM3,ID3,H3,M3,S3,SEC3,
C@tno jcp 1996Apr25_13:55:15 jd hist start added
     &  JdHist ,IYYY4,MM4,ID4,H4,M4,S4,SEC4,
C@tno jcp 1996Jun28_14:01:22 NSecSim2..4 added (stop, schedule,histogram Nsec
C since simulation start)
     &  NSecSim2,NSecSim3,NSecSim4,
C@tno jcp 1996May02_15:06:49 Simulen, iPolH2O, useMetH2O added
     &  bufsec, compday, notime, SiMuLen, iPolH2O,
     &  useMetH2O, EmptyMeteo,
     &  PrvSchTyp,PrvSchNa,PrvSec,LastBjDay,CYCLICmeteo

C@tno jcp 1996Apr26_23:02:14 MetTime Start/Stop added to time commons
C@tno jcp 1996Jun27_22:22:25 repline 80->160 long
        CHARACTER RepLine*160, cont*5, keep*5,buftim*30,
     &  MetTimeStart*25,
C@tno jcp 1996Jul03_13:20:32 tmstart added
     &  MetTimeStop*25,PrvLine*160,tmstart*40
        COMMON/SCHTIM2/ RepLine, cont, keep, buftim,MetTimeStart,
     &  MetTimeStop,prvLine,tmstart


C Multischedule
C-----------------------------------------------------------------------

        CHARACTER multifile*160,multischednames*(Maxline)
        COMMON/MULTI1/multifile,multischednames
        INTEGER multischedtype(MaxCol),multischedtime,multitimestep
        REAL OFanSp(MaxL)
        COMMON/MULT2/multischedtype,multischedtime,multitimestep,OFansp

C buf=buffer containing upto mbuf lines that will be written to DAF
        CHARACTER buf(mBuf)*160
        COMMON/MULT3/buf


C Equations for Scheduled Window openings
C-----------------------------------------------------------------------

C@tno jcp 1996Jun14_15:10:15 new for EQN-WIN

c name of the schedule that uses the equation
        CHARACTER EqnSchName(MaxEqnWin)*10
        COMMON/EQNWIN1/EqnSchName
c total number of equations for windows defined/used and id's for variables
c and operators
        INTEGER EqnCount,EqnVar(MaxEqnWin,3),EqnOpr(MaxEqnWin,2)
        COMMON/EQNWIN2/EqnCount,EqnVar,EqnOpr
c coeficients C0 C1 C2 and number of the variable for the three terms
        REAL EqnCoef(MaxEqnWin,12)
        COMMON/EQNWIN3/EqnCoef


C Checking CIF-file
C-----------------------------------------------------------------------

        CHARACTER*40 LastItem
C@tno jcp 1996Jun27_22:21:45 lines10 80->10 long
        CHARACTER lines10(10)*160,variab*40
CC        INTEGER plines10,clines10,FInpErr
C@tno jcp 1996Jul08_17:14:00 nCifLine holds the line number of the Cif file
        INTEGER plines10,clines10,FInpErr,FlgDefault,nCifLine
C@tno jcp 1996Apr22_15:57:47 Default flag true is last read returned a default
C@tno jcp 1996Jun12_14:29:21 make FlgDefault an INTEGER
CC        LOGICAL FlgEnd,Keep10,FlgDefault
        LOGICAL FlgEnd,Keep10
        COMMON/CCHECKCIF/LastItem,lines10,variab
        COMMON/ICHECKCIF/FlgEnd
C@tno jcp 1996Jun12_14:30:27 FlgDefault is now and INTEGER
        COMMON/lst10l/plines10,clines10,FInpErr,FlgDefault,
     &    Keep10,nCifLine


C COMMON for default values
C-----------------------------------------------------------------------
C Note: put DMetName and DMetTime last since they are strings (padding)

C ODxxx default values means Originals as the
C Dxxx defaults will be changed if input unit conversions are used
C in principle Default=ODefault/ifact(nr) as during input the way will be
C InputVariable=Default  and  then
C SI=InputVariable*ifact(nr) where nr is the number 1..11 of the quantity

        REAL    ODepsFA, DEpsFA, DEpsFR, ODEpsCJ, DEpsCJ
        REAL    ODDiflim, DDifLim, DZref, DZentr
        REAL    DAngBui, DLat, DLon, DZVmet, DZmet, DAlphMet
        REAL    DEnvDir, DWExp, DVmet
        REAL    DDmet, DTmet, ODTmet, DXhMet, ODXhMet, DPbMet, ODPbMet
        REAL    DNormCrT, DNormCrPb,DNormCrXh
        REAL    ODNormCrT, ODNormCrPb, ODNormCrXh
        REAL    DReLam, DReTurb
        REAL    DTz, ODTz, DZz, DVz, DXhz, ODXhz, DMM, DDiffusion

        COMMON /RDEFVALS/ ODepsFA, DEpsFA, DEpsFR, ODEpsCJ, DEpsCJ,
     &		ODDiflim, DDifLim,
     &		DZref, DZentr,
     &		DAngBui, DLat, DLon, DZVmet, DZmet, DAlphMet,
     &		DEnvDir, DWExp, DVmet,
     &		DDmet, DTmet, ODTmet, DXhMet, ODXhMet, DPbMet, ODPbMet,
     &		DNormCrT, ODNormCrT,
     &          DNormCrPb, ODNormCrPb, DNormCrXh, ODNormCrXh,
     &		DReLam, DReTurb,
     &          DTz, ODTz, DZz, DVz, DXhz, ODXhz, DMM, DDiffusion

CC      INTEGER DnNewt,DuseOpz,DnoInit,DslvSel,DmIter

        INTEGER DnNewt,DslvSel,DmIter,Dstp1Init,Dstp2Init
CC      COMMON /IDEFVALS/ DnNewt,DuseOpz,DnoInit,DslvSel,DmIter
        COMMON /IDEFVALS/ DnNewt,DslvSel,DmIter,Dstp1Init,Dstp2Init

        LOGICAL DrhoSync
        COMMON /LDEFVALS/ DrhoSync

        CHARACTER DMetName*160, DMetTime*25, DSchPolName*160

        COMMON /CDEFVALS/ DMetName, DMetTime, DSchPolName


C User input data section
C-----------------------------------------------------------------------

        REAL USERDAT(10,10)
        COMMON /USERINP/ USERDAT


C WALL-MAterial and TYpe absorption Data
C-----------------------------------------------------------------------
C WDat(MaxWD)    =Wall absorption Data (Materials and Types)
C MWDat          =Maximum number of elements used in WDat
C PtrW           =pointer into Wdat
C ptrW0          =previous pointer into WDat
C ptrWN          =pointer into WName
C ptrWL          =pointer into WalLin (linking of walls)
C NWL            =Number of Wall-types in Links
C WNind(MaxWN)   =Index from WALl(MAterial and TYpe)Names to the position of
C                 the data in WDat
C WalLin(MaxWL)  =Index into WDat, From,TO,Orientation,AreaFactor
C
C WName(MaxWN)   =Names of WALl-MAterials or WALl-TYpes
C FromToW(2,MaxL)=Names of the From To nodes under &-WAL-LINking in the input
C                 This array is similar to FromToS and will be used to look up
C                 zone/node names after all has been read and place their
C                 sequence number in WalLin(*). Here for the maximum MaxL (=
C                 Max number of Air Flow Network Links) has been used.
C NConcWaTy      =least number of adsorption data lines (one line per pollutant)
C                 found for any WallType (NConcWaTy>NConc and NConc<NConcPolDes)
C NetWaLinF      =Flag that will be 1 if data under &-WAL-LINk is found (used
C                 to test NConCWaTy against NConc)
C-----

        REAL WDat(MaxWD)
        INTEGER MWDat,ptrW,ptrW0,ptrWN,ptrWL,NWL,WNind(MaxWN),
     &  WalLin(MaxWL),NConcWaTy,NetWaLinF

        COMMON/WAL1/Wdat,
     &   MWdat,PtrW,ptrW0,PtrWN,ptrWL,NWL,WNind,WalLin,
     &   NConcWaTy,NEtWaLinF


        CHARACTER*20 WName(MaxWN), FromToW(2,MaxL)

        COMMON/WAL2/WName,FromToW


C Error counters
C-----------------------------------------------------------------------
C@tno jcp 1996Apr05_16:33:08 counters for the errors added
C	CerCount(0) = NOTE
C	CerCount(1) = WARNING
C	CerCount(2) = SEVERE
C	CerCount(3) = FATAL (program stops)

        INTEGER CERCount(0:3)
        COMMON /CERCnt/ CERCount
