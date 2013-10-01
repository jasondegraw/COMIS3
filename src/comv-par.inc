C+**********************************************************comv-par.inc
C    CONSTANTS FOR COMIS
C***********************************************************************
C Changes:
C@lbl bvs 1995Jun MaxCGO... changed to MaxCOS...
C@empa aw 1994mar10 MCGOCol added
C@empa aw 1994sep20 MaxLO,NrInt added
C@empa aw 1994nov17 MaxZT added
C@empa hf 1994nov30 MaxCGOk changed to 38 and MaxOOT added
C@empa hf 1994nov30 MaxFUnitNr, StartFUnitForCGO added
C@empa hf 1995jan04 Unitxy added
C@empa aw 1995mar20 Parameter constants from COMV-INP.INC put into COMV-PAR.INC
C@empa aw 1995dec18 UnitFvFlag added
C@tno jcp 1996Mar28_13:26:48 MaxHis
C@empa aw 2000feb01 MaxFSo added
C@NBI PGS 2000Jul18 - Changed file extensons .cho to .coh, .cso to .cos,
C@NBI                 and .uof to .cou, so that all output file extensions
C@NBI                 start with .co ("o" for output) and all input files
C@NBI                 should have extension .ci? ("i" for input)
C@NBI PGS 2000Oct08 - Can now define different I/O units for each pollutant,
C@NBI                 for SINK, SOURce and CONCentration.  So "UnitPol"
C@NBI                 renamed to "UnitPolCo", and new PARAMETERS
C@NBI                 "UnitPolSi" and "UnitPolSo" added, for SINK and
C@NBI                 SOURce respectively.
C
C
C***********************************************************************
C    MAX PARAMETERS:
C***********************************************************************
C
C We have now three classes of max Parameters:
C
C 1. Independent max parameters
C    Numbers of input elements such as zones, links, linktypes etc.
C    This kind is thought to be changeable to adjust memory space to the
C    problem.
C
C 2. dependent max parameters evaluated by programmer:
C    The dependence is given in a comment and has to be evaluated and changed
C    by hand if the corresponding Independent max parameter has changed.
C    These values will be checked in the subroutine arraycheck. If they are
C    to small, an error message will occur in the first run after compilation.
C
C 3. dependent max parameters evaluated by compiler:
C    The dependence is given here as an arithmetic expression, so the value
C    will be evaluated at compile time and has not to be changed by hand.
C    If you have Memory space problems, it is possible to reduce some of
C    these values. For instance not every linktype needs 30 elements in Ldat
C    or not every zone has occupant source. But in that case you have no
C    guarantee that the arrays which are dimensioned with this parameter
C    will not owerflow.
C
C***********************************************************************
C 1. Independent max parameters
C***********************************************************************
C
C MaxTyp= maximum number of types of links
C MaxL  = maximum number of links
C MaxZ  = maximum number of zones
C MaxW  = maximum number of windpressure points
C MaxF  = maximum number of Facade elements
C MaxWMa= maximum number of Wall Materials
C MaxWTy= maximum number of Wall Types
C MaxWLr= maximum (average) number of Wall layers in a Wall Type
C         MaxWMa, MaxWTy.MaxWLr are combined in the max size of WDat,
C         as both Materials and Types are stored in the same array, this is the
C         actual limit and not MaxWMa or MaxWTy individually
C MaxWN = maximum number of names of Wall-Materials and WallTypes
C MaxWL = maximum number of Links of wall-layers to zones and/or to outside
C MaxWC = maximum number of Wall Concentrations  (in the 2 or 3 nodes per
C          wall layer)

C MaxS     = maximum number of special pressures
C MaxDir   = maximum number of winddirections
C MaxNrLay = maximum number of layers for gradients
C MaxC     = maximum number of pollutants
C MaxO     = maximum number of occupants
C MaxFSo   = maximum number of fictive sources
C@empa aw 2001may28
C MaxTInp  = maximum number of TRNSYS Inputs (BUI Variables)

C Maxline  = Maximum linelength for multi-schedule file
C MaxCol   = Maximum number of schedules in a multi-schedule file
C       (equals number of columns most of the time)
C MaxCOSk  = Maximum number of keywords for Output Options
C MaxOOT   = Maximum number of OutputOptions 'XY-T'
C MaxOOH   = Maximum number of OutputOptions 'XY-H'
C MCOSCol  = Maximum number of columns in a COS file
C MHisAr   = Maximum space in HistoAr(*)
C MHisTy   = Maximum number of Histogram types 1..20
C MHisUs   = Maximum number of used Histograms 1..50
C MKeys    = maximum stringlength of Keys (String which contains all keywords,
C            which can appear in an input file) (fixed value)
C MaxLO    = Maximum number of Large Openings
C NrInt    = Number of integration intervals for Large Openings
C MaxZT    = Maximum number of zones with thermal properties for
C            single sided ventilation
C MaxFUnitNr=Maximum numbers of file-units. This number is
C            different from Compiler to Compiler.
C            Example   - VAX-VMS Fortran77 :  99 (120 possible)
C                      - Watcom 77/32 9.5  : 999
C Mbuf     = Maximum number of lines in the buffer for multischedule file DAF

C UKeys   = maximum stringlength of KeysU (String which contains all subset
C           keywords, which can appear in an input file after &-UNITS) (fixed)
C MUUstrL = maximum User Unit length of a line in IUNIT(*) and OUNIT(*)

C           UnitStr replaces FmUnit,PUnit,TUnit etc and defines all other
C           possible units. One line is one quantity in the sequence as explaine
C           in FilUnit and the Programmers Guide.

C MUstr   = maximum number of lines in UnitStr*MUstrL(*)
C MUstrL  = maximum length of a line in UnitStr(*)

C ConvEnt = maximum number of entities(lines) in Conv(unit,quantity)
C NoUnit  = maximum number of units (columns) in Conv(unit,quantity). Also
C       is the maximum User Unit number of lines in the string
C       array IUNIT(*) and OUNIT(*)

        INTEGER MaxTyp,MaxL,MaxZ,MaxW,MaxF,MaxWMa,MaxWTy,MaxWLr
        INTEGER MaxWd,MaxWn,MaxWl,MaxWc
        INTEGER MaxS,MaxDir,MaxC,MaxO,MaxFSo
C@empa aw 2001may28 MaxTInp
        INTEGER MaxTInp
        INTEGER Maxline,MaxCol,MaxCOSk,MCOSCol,MKeys,Ukeys
C@tno jcp 1996Mar28_13:32:49 size of histograms added
        INTEGER MHisAr,MHisTy,MHisUs
        INTEGER MUUstrL, MUstr, MUstrL
        INTEGER ConvEnt
C@tno jcp 1996Mar29_16:06:42 maxOOH added
        INTEGER MaxNrLay,MaxLo,MaxZT,MaxOOT,MaxOOH,MaxFUnitNr,mBuf

        INTEGER MaxEqnWin
        PARAMETER (MaxTyp=200)
C@empa aw 2000feb21 MaxL reduced
CC        PARAMETER (MaxL=1500)
C@empa aw 2003nov10 Konrath: 2000 zones 4000 links
CC        PARAMETER (MaxL=600)
CC        PARAMETER (MaxZ=200)
        PARAMETER (MaxL=4000)
        PARAMETER (MaxZ=2000)
        PARAMETER (MaxC=5)
C jcp 1994Sep20 MaxW MaxF 1500?? so much ? for Who?
C CP is MaxDir*MaxW (50*1500)=75000 cp's. Has anybody such a file?
C and that for only 200 zones?
C@empa aw 2004aug09 MaxW and MaxF increased for P. Vogel 
CC        PARAMETER (MaxW=200)
CC        PARAMETER (MaxF=200)
        PARAMETER (MaxW=500)
        PARAMETER (MaxF=500)
        PARAMETER (MaxWMa=30)
        PARAMETER (MaxWTy=100)
        PARAMETER (MaxWLr=10)
        PARAMETER (MaxWD=MaxWma*(5+MaxC*7)+MaxWTy*(2+2*MaxWLr))
        PARAMETER (MaxWN=200)
        PARAMETER (MaxWL=200)
        PARAMETER (MaxWC=1200)

        PARAMETER (MaxS=50)
        PARAMETER (MaxDir=50)
        PARAMETER (MaxNrLay=50)
        PARAMETER (MaxO=5)
        PARAMETER (MaxFSo=5)
C@empa aw 2001may28 MaxTInp
        PARAMETER (MaxTInp=100)
        PARAMETER (Maxline=200)
        PARAMETER (MaxCol=10)
C@empa aw 2000feb01
C@empa aw 2000apr03
C@NBI PGS 2000Aug01 - MaxCOSk set to 54 (keyword 'EF' removed)
C@empa aw 2002dec04 - MaxCOSk set to 55 (keyword 'PL' added)
C@empa aw 2005aug29 - MaxCOSk set to 56 (keyword 'DL' added)    

CC        PARAMETER (MaxCOSk=50)
        PARAMETER (MaxCOSk=56)

        PARAMETER (MCOSCol=32)
C@tno jcp 1996Mar28_13:32:27 Histogram space added
        PARAMETER (MHisAr=5000)
        PARAMETER (MHisTy=20)
C@tno jcp 1996Mar29_16:07:36 possibly MHisUs is the same as MaxOOH
        PARAMETER (MHisUs=50)
C@tno jcp 1996Jun14_16:11:11 max for EqnWin
        PARAMETER (MaxEqnWin=10)

C@empa aw 2004jun23 MaxLo increased  
CC        PARAMETER (MaxLO=100)
        PARAMETER (MaxLO=200)
        PARAMETER (MaxZT=20)
        PARAMETER (MaxOOT=100)
        PARAMETER (MaxOOH=50)
        PARAMETER (MaxFUnitNr=120)
        PARAMETER (mBuf=200)

        PARAMETER (MKeys=600)
C the sub set for keywords under Keys=UNITS
        PARAMETER (UKeys=200)
C added for user units handling
        PARAMETER (MUUstrL=20)
        PARAMETER (MUstr=20)
        PARAMETER (MUstrL=160)

        PARAMETER (ConvEnt=16)

C***********************************************************************
C 2. dependent Max parameters evaluated by programmer:
C***********************************************************************


C MtyT = maximum number of elements in the binary tree for AFCtypenames
C MLiT = maximum number of elements in the binary tree for linknames
C MZoT = maximum number of elements in the binary tree for zonenames
C MExT = maximum number of elements in the binary tree for externalnode names
C MFeT = maximum number of elements in the binary tree for facade element names
C@empa aw 2001may28 
C@empa aw 2002aug12 canceled 
C MTIT = maximum number of elements in the binary tree for TRRNSYS Input names

CC        INTEGER MTyT,MLiT,MZoT,MExT,MFeT,MTIT
        INTEGER MTyT,MLiT,MZoT,MExT,MFeT

C numbers need to be the next 2**n-1 above MaxTyp,MaxL,MaxZ,MaxW,MaxF
        PARAMETER (MTyT=255)
C@empa aw 2003nov10 Konrath: 2000 zones 4000 links
CC        PARAMETER (MLiT=2047)
CC        PARAMETER (MZoT=255)
        PARAMETER (MLiT=4095)
        PARAMETER (MZoT=2047)
C@empa aw 2003feb28 MExT and MFeT adjusted acording MaxW and MaxF
C@empa aw 2004aug09 MExT and MFeT increased acording MaxW and MaxF for P. Vogel 
CC        PARAMETER (MExT=255)
CC        PARAMETER (MFeT=255)
        PARAMETER (MExT=511)
        PARAMETER (MFeT=511)
CC       PARAMETER (MTIT=127)


C***********************************************************************
C 3. dependent Max parameters evaluated by compiler:
C***********************************************************************

C MaxSP = maximum number of elements in the Osource array which has 5 REALs per
C     zone in which as source an occupant or combination has been input
C MaName= maximum stringlength of aName (collection of all user names appearing
C         in CIF
C MaxCp = maximum number of Cp*Number of directions in the input
C MaxD  = maximum number of elements in Ldat array (= data for each linktyp)
C MaxDl = maximum number of elements in Linkdat array (= data for individual
C MaxLay = maximum number of elements in LayDat array (= data for layers)
C         link)
C@empa aw 2000feb09 MaxOOH2 added
C MaxOOH2= maximum number of elements in ROOptH
        INTEGER MaxSP,MaName,MaxCp,MaxD,MaxDl,MaxLay,MaxOOH2

        PARAMETER (MaxSP=MaxZ*5)
        PARAMETER (MaName=1500)
C       In fact MaName depends on MaxTyp and the maximum number of
C       schedule names, but we have no max parameter for that yet.
C       Note if you change MaName, old TMS files cannot be used anymore
        PARAMETER (MaxCp=MaxW*MaxDir)
        PARAMETER (MaxD=MaxTyp*30)
        PARAMETER (MaxDl=MaxL*10)
        PARAMETER (MaxLay=MaxNrLay*9)
        PARAMETER (MaxOOH2=MaxOOH*50)
C
C
C***********************************************************************
C       OTHER CONSTANTS:
C***********************************************************************
C
C StartFUnitForCOS = Start of file-unit number for COS files:
C                    24(TMM=23) < StartFUnitForCOS < MaxFUnitNr
C
C NrInt            = Number of integration intervals for Large Openings
C
C Parameters to name the Line Types
C TCOM      = comment line
C TKEY      = keyword line
C THEAD     = header line
C TNAME     = name line
C TERR      = error line
C TDATA     = data line
C
C Numbers for input and output units
C UnitCm    = Unit number for air leakage:               1
C UnitFma   = Unit number for ventilation massflow:      2
C UnitP     = Unit number for ventilation pressure:      3
C UnitTmul  = Unit number for temperature multiplier:    4
C UnitXh    = Unit number for humidity:                  5
C UnitPSou  = Unit number for pollutant source:          6
C UnitPSin  = Unit number for pollutant sink:            7
C UnitPConc = Unit number for pollutant concentration:   8
C UnitFva   = Unit number for flow through fan:          9
C UnitW     = Unit number for wind velocity:            10
C UnitRate  = Unit number for air change rate:          11
C UnitAge   = Unit number for mean age:                 12
C UnitE     = Unit number for energy:                   13
C UnitProf  = Unit number for wind profile:             14
C ! If quantities have to be added then insert them here--------------<
C ! and move UnitToff, UnitPol?? and UnitFvFlag up
C UnitToff  = Unit number for temperature offset:       15
C UnitFvFlag    = Flag=1 :fan flow rate as volume flow: 16
C                 Flag=0 :fan flow rate as mass flow
C UnitPolCo = Unit number for pollutant concentration:  17 - 1
C UnitPolSo = Unit number for pollutant source:         17 - 1  + MaxC
C UnitPolSi = Unit number for pollutant sink:           17 - 1  + MaxC*2
C NoUnit    = number of units                           UnitPolSo+MaxC*3

        INTEGER StartFUnitForCOS
        INTEGER NrInt
        INTEGER TCOM,TKEY,THEAD,TNAME,TERR,TDATA,Tcont
        INTEGER UnitCm,UnitFma, UnitP, UnitTmul, UnitToff, UnitXh
        INTEGER UnitPSou, UnitPSin, UnitPConc
C@NBI PGS 2000Oct08 - Can now define different I/O units for each pollutant.
CC      INTEGER UnitFva, UnitW, UnitRate, UnitAge, UnitE, UnitPol
        INTEGER UnitFva, UnitW, UnitRate, UnitAge, UnitE, UnitPolCo
     &  ,UnitPolSo ,UnitPolSi
        INTEGER UnitProf
C@empa aw 1995dec18 UnitFvFlag added
        INTEGER UnitFvFlag
        INTEGER NoUnit
        INTEGER IntMax
        REAL REALMax,REALMin

        PARAMETER (StartFUnitForCOS=30)
C@empa aw 2005apr27 Nrint increased
CC        PARAMETER (NrInt=20)
        PARAMETER (NrInt=100)


C Input parser types

        PARAMETER (TCOM=1)
        PARAMETER (TKEY=2)
        PARAMETER (THEAD=3)
        PARAMETER (TNAME=4)
        PARAMETER (TERR=5)
        PARAMETER (TDATA=6)
C@tno jcp 1996Apr08_10:20:50 introduced a continuation line type as Tcont=7
        PARAMETER (TCont=7)

C Unit positions in conversion arrays
C *** The lines in the SET file must be in the same order ***

C Air leakage                      sequence in set Input 1  Output 1
        PARAMETER (UnitCm=1)
C Massflow (ventilation flowrates) sequence in set Input 2  Output 2
        PARAMETER (UnitFma=2)
C Pressure                         sequence in set Input 3  Output 3
        PARAMETER (UnitP=3)
C Temperature multiplier           sequence in set Input 4  Output 4
        PARAMETER (UnitTmul=4)
C Humidity                         sequence in set Input 5  Output 5
        PARAMETER (UnitXh=5)
C Pollutant source                 sequence in set Input 6  Output 6
        PARAMETER (UnitPSou=6)
C Pollutant sink                   sequence in set Input 7  Output 7
        PARAMETER (UnitPSin=7)
C Pollutant concentration          sequence in set Input 8  Output 8
        PARAMETER (UnitPConc=8)
C Fan flow rate                    sequence in set Input 9  Output 9
        PARAMETER (UnitFva=9)
C Wind speed                       sequence in set Input 10 Output 10
        PARAMETER (UnitW=10)
C Air change rate  OUTPUT ONLY     sequence in set Input  - Output 11
        PARAMETER (UnitRate=11)
C Mean age of air  OUTPUT ONLY     sequence in set Input  - Output 12
        PARAMETER (UnitAge=12)
C Energy           OUTPUT ONLY     sequence in set Input  - Output 13
        PARAMETER (UnitE=13)
C Wind profile, only strings, the conversion itself is done by routine Wind
C                  INPUT ONLY      sequence in set Input 11 Output -
        PARAMETER (UnitProf=14)

C Temperature offset
        PARAMETER (UnitToff=15)

C@empa aw 1995dec18 UnitFvFlag added, UnitPol?? moved one position
        PARAMETER (UnitFvFlag=16)

C UnitPolCo+1 to UnitPolCo+MaxC are for the constants for the MaxC pollutants
C@empa aw 1995dec18 change UnitPolCo+1 from 16 to 17
C@NBI PGS 2000Oct08 - Can now define different I/O units for each pollutant.
CC      PARAMETER (UnitPol=17)
        PARAMETER (UnitPolCo=17-1)
        PARAMETER (UnitPolSo=17-1+MaxC)
        PARAMETER (UnitPolSi=17-1+MaxC*2)

C Number of units
CC      PARAMETER (NoUnit=UnitPol+MaxC-1)
        PARAMETER (NoUnit=UnitPolCo+MaxC*3)

C value for minimum and maximum (a bit smaller so it fits positive and negative)
        PARAMETER (IntMax=2147438647)
        PARAMETER (REALMax=0.1E+37)
        PARAMETER (REALMin=1E-37)
C@NBI PGS 1999Aug19 - What's FVoldDefined doing here!?  It isn't a PARAMETER,
C@NBI                 so I've put it elsewhere in the code
CCC Need a flag in MatVec to tell if FVold has been assigned
CC        LOGICAL FVoldDefined
CC        COMMON/FVolddef/FVoldDefined
