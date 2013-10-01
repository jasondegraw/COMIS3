CWK
!DEC$ATTRIBUTES DLLEXPORT :: TYPE157

C+*********************************************************** comv-trn.for	
	SUBROUTINE TYPE157(TIME,XIN,OUT,T,DTDT,PAR,INFO,ICNTRL, *)
C**********************************************************************
CWK/end
C
CTC        This is the version of COMIS which is a subroutine type 
CTC        of the Transient Systems Analysis Program TRNSYS, 
CTC        to be used in conjunction with TYPE 56 Multizone Building.
CTC        This version is called COMIS-TRNSYS.
C
CTC        All code lines from the original COMIS version which are not used
CTC        in COMIS-TRNSYS are commented out with "CT". Comments concerning those 
CTC        obsolet code lines are marked with "CCT". Comments concerning 
CTC        COMIS-TRNSYS only, are marked with "CTC"  
C+*********************************************************** comv-mai.f
CT	PROGRAM COMIS
C**********************************************************************
C
C The program's version number is given in "VersionStr" which is defined
C in the INITIALIZATION section below.
C
C The name was changed from COMVEN to COMIS in March 1997 by a consensus
C of the IEA ECBCS Annex 23 members.
C The history of this program, and a description of its functionality,
C is given in the COMIS 3.1 User's Guide, EMPA, Switzerland, 2000
C
C A comprehensive introduction to this source code is given in the
C COMVEN Programmer's Guide, EMPA 175, Duebendorf, Switzerland, 1995,
C available on-line at ftp://epb1.lbl.gov/PROGUIDE/
C Changes to this official version must be marked according to the
C standards specified in the COMVEN Programmer's Guide
C
C The prototype COMIS program was written by the COMIS group in 1989:
C
C Name                     Organisation
C ----------------------   ----------------------
C Francis Allard           Centre de Thermique, Lyon, France
C Viktor  Dorer            EMPA, Duebendorf, Switzerland
C Helmut E. Feustel        LBL, Berkeley, USA
C Eduardo A. Rodriguez     Garcia ESII, Sevilla, Spain
C Mario Grosso             Dipartimento di Scienze e Tecniche per i Processi
C                          di Insediamento Politecnico, Torino, Italy
C Magnus Herrlin           RIT, Stockholm, Sweden
C Liu Mingsheng            Harbin Architectural and Civil Engineering
C                          Institute, Heating Research Laboratory ,Harbin,
C                          Peoples Republic of China
C Hans J.C. Phaff          TNO, Delft, The Netherlands
C Yasuo Utsumi             Miagy Nat College of Techn, Natori, Japan
C Hiroshi Yoshino          Tohoku University, Sendai, Japan
C Koos van der Maas        EPFL LESO, Lausanne, Switzerland
C
C Special help has been given by:
C
C All the AIVC Steering group members.
C George N. Walton         NIST, Gaithersburg, MD USA
C Claude-Alain Roulet      EPFL LESO, Lausanne, Switzerland
C Jean-Marie Fuerbringer   EPFL LESO, Lausanne, Switzerland
C Peter Hartmann           EMPA Duebendorf, Switzerland
C Brian V. Smith           LBL, Berkeley, USA
C Darryl Dickerhoff        LBL, Berkeley, USA
C Andreas Weber            EMPA, Duebendorf, Switzerland
C
C Recent COMIS versions have been developed by:
C
C Werner Keilholz          LBL, Berkeley, USA
C Andreas Klingler         LBL, Berkeley, USA
C Brian Smith              LBL, Berkeley, USA
C Bettina Stracke          LBL, Berkeley, USA
C David Lorenzetti         LBL, Berkeley, USA
C Viktor Dorer             EMPA 175, Duebendorf, Switzerland
C Andreas Weber            EMPA 175, Duebendorf, Switzerland
C Frederik Huck            EMPA 175, Duebendorf, Switzerland
C Hans J.C. Phaff          TNO, Delft, The Netherlands
C Peter G. Schild          NBI, Oslo, Norway
C
C***********************************************************************
C Changes to MAIN:
C@empa aw 1994nov29 Call ConvDate and HeadOut before Precal, that error
C                   messages from Precal can be attached to the correct
C                   timestep in the COF
C@tno jcp 1996May01_11:59:25 might take a long time before a first message
C appear on screen: so put one up here
C@tno jcp 1996May01_12:02:18 This initialization might take some time: Put
C something onscreen
C@tno jcp 1996Mar14_12:07:48 Ndkey to include file inp.inc
C@tno jcp 1996Mar29_14:56:20 output of histogram added
C@tno jcp 1996Apr05_16:46:23 for the last error message added variables
C@tno jcp 1996Apr06_10:33:31 call new routine for this in comv-ini.f
C@tno jcp 1996Apr11_12:11:23 'had to keep the previous Occupant Activity and
C Occupant numbers (might be possible to store them at the histograms)
C@tno jcp 1996Apr11_13:20:19 store the concentrations and flowrates at the
C beginning of the interval for use in the histograms (if any)
C@tno jcp 1996Apr15_12:45:48 DTstring now 31 long
C@tno jcp 1996Apr15_14:59:35 explain why there will follow more schedule output
C@tno jcp 1996Apr25_16:02:46 need TLotus in Pollutant for calling HistCalc
C@tno jcp 1996Apr26_23:42:33 give a linefeed above 'the output starts here'
C@tno jcp 1996Apr30_10:35:49 outputoptions(12)=1  Steady state all time steps
C@tno jcp 1996Apr30_12:52:49 debug output TMS and DAF
C@tno jcp 1996May01_11:57:42 flag only true after first time step used for the f
C@tno jcp 1996May01_11:58:52 FirstStep
C@tno jcp 1996May02_15:58:19 for date conversion 3 integers added
C@tno jcp 1996May02_15:03:24 total length of the simulation in seconds
C for use in OnScreen progress
C@NBI PGS 1999May06 - Moved up from later in routine
CC                    Useful to initialize TmStep before any error routines
C@tno jcp 1996May20_13:43:29 need TLotus to call PreRho from StrSlv
C@tno jcp 1996May23_10:59:15 new check for the characteristic of fans
C@tno jcp 1996May28_12:18:25 'time' in Lotus format renamed to TLotus in whole p
C@tno jcp 1996May28_12:23:51 TimeString renamed to DTstring (Date-Time string)
C in the whole program
C@tno jcp 1996May31_16:45:24 outputoptions(2)=1 must be >0
C@tno jcp 1996May31_16:49:49 adjust Nconc to the desired number of pollutants at
C@tno jcp 1996Jun04_17:46:24 limit to the defined number of pollutants
C@tno jcp 1996Jun04_17:48:47 str2 added
C@tno jcp 1996Jun11_12:32:01 temp loop to prnt all output options
C@NBI PGS 2000Jul19 -        ...temp loop deleted
C@tno jcp 1996Jun11_15:43:20 reset FlgEnd here?
C@tno jcp 1996Jun17_15:52:03 variab added
C@tno jcp 1996Jun18_12:27:34 DTstring added for better output
C@tno jcp 1996Jun21_16:25:42 call preH2OPol in main
C@tno jcp 1996Jun27_22:20:32 DAF reclength 100->160
C@tno jcp 1996Jun27_16:04:47 allow F: and f: to signal a file
C@tno jcp 1996Jul02_09:48:07 count the number of timesteps
C@tno jcp 1996Jul02_09:46:57 tmstep added to count the number of timesteps
C@tno jcp 1996Jul03_16:18:58 I get the feeling that MetINIT can be moved
C freely some where to before the timeloop close to Call tmsmain
C@tno jcp 1996Jul03_16:21:29 this part came out of the routine timestep, but as
C more sorting in DuplTM was needed after MetINIT I try to place it here
C@tno jcp 1996Jul04_17:05:08 make sure DuplTM writes the Array to TMS
C@tno jcp 1996Jul04_17:05:44 for the call of DuplTM
C@tno jcp 1996Jul04_07:39:46 for METINIT the filename of the reopened TMS file
C@tno jcp 1996Jul08_12:23:23 check TMS for a schedule with more than one event
C at the same time
C@tno jcp 1996Jul08_17:14:46 nCifLine the line number is reset after opening cif
C@tno jcp 1996Jul08_17:16:09 increment nCifLine the line number in Cif
C@tno jcp 1996Jul24_12:43:01 NetSlv:Ounit never used
C@empa aw 1996sep15 FirstStep added
C@lbl bvs 1997Jul16 SepStr must itself have quotes
C@lbl bvs 1997Jul28 InitFlg must be declared
C@lbl bvs 1997Jul28 InitFlg must be initialized
C@empa aw 1997sep08 OK, but we have to initialize it with 0. It will be set to
C@empa              1 if (noinit.eq.0) lateron in netslv.
C@empa aw 1997sep15 moved FistStep=.false. down
C@lbl bvs 1997Dec8 no need to delete old error file, just
C@lbl              open with status='UNKNOWN'
C@NBI PGS 1999May05 - Had to reinstate DELETE because old error files
C@NBI                 persisted when a new run is completely error-free
C@lbl bvs 1999Jan06 Switch must be initialized
C@NBI PGS 1999May06 - "SepStr" changed to Tab delimiter
C@NBI PGS 1999May06 - Bugfix
C@NBI PGS 1999May06 - TmStep=0 Moved up to start of routine
C@NBI PGS 1999May07 - dum no longer used
C@NBI PGS 1999May07 - Bugfix.  This originally rewound and overwrote all but
C@NBI                 the first line of the CER file... A bit daft!
C@NBI                 Message() is now changed to write to CER only if messages
C@lbl bvs 1999Jun24 added for pollutant loop
C@NBI PGS 2000Jul16 - Omit trailing banks from VersionStr
C@NBI PGS 1999Aug05 - line has been moved here from routine 'HeadOut',
C@NBI PGS 1999Aug06 - Made output more understandable
C@NBI PGS 1999Aug10 - 'interval' is now in the INCLUDE file.  I know this isn't
C@NBI                 a tidy solution, but it is the best way I can think of
C@NBI                 making it available to calculate ventilation heat loss.
C@NBI PGS 1999Aug11 - Optional check for condensation risk
C@NBI PGS 1999Aug13 - Introduced FORMAT label 1002
C@NBI PGS 1999Aug13 - Redundant IF(CRT.NE.0) statement.  CRT is = 6 here
C@NBI PGS 1999Aug13 - According to the COMIS 3.0 User's Guide, the settings
C@NBI                 should be under the &-COMIS keyword ?
C@NBI PGS 1999Aug13 - Common header FORMAT (label 1002)
C@NBI PGS 1999Aug14 - It is not really useful to define default filenames
C@NBI                 here; it is likely that default.cif won't exist, and so
C@NBI                 COMIS will just crash as it tries to open it.  The User's
C@NBI                 Guide doesn't mention what these default.cif/cof
C@NBI                 filenames should be.  Isn't it better to give a
C@NBI                 constructive warning message that no input or no output
C@NBI                 file is defined (i.e. not defined by .SET file or
C@NBI                 command-line options).  See message in subrou. 'RdCmdLn'
C@NBI PGS 1999Aug14 - These checks now done in subroutine 'RdCmdLn'
C@lbl dml 1999nov19 Remove variable initFlg, which was only passed
C   in to NetSlv(), where its value was set without regard to the value
C   assigned here. Note later changed from integer to logical.
C@lbl dml 1999nov19 Remove variables veRef, ofact(UnitP), and TLotus
C   from call of sbr NetSlv, in order to match its new definition.
C@lbl dml 1999nov22 Replace solver control parameters noInit and useOpz
C   with stp1Init, stp2Init, and rhoSync. Add integer stpInit to pass
C   proper initialization value in to network solver.
C@empa aw 1999nov23 check whether all names are unique
C@lbl dml 1999nov29 Merge code from call of NetSlv() into the main
C   program, so that call solver directly.
C@lbl dml 1999nov29 Add stpInit to call of PreCal(), to match new
C   definition.
C@lbl dml 1999dec01 Add call to solver #6.
C@lbl dml 1999dec03 Add implicit none.
C@empa aw 1999dec13 Unfortunatly not all of the checks report with InErr, which the
C@empa              flag FInpErr sets to 1. Now check with CerCount(2) (number of
C@empa              SEVERE errors).
C@empa aw 2000jan04 Check zone IDs in occupant schedules
C@empa aw 2000jan05 Error message if *.CIF not exist
C@empa aw 2000jan17 Close *.CSO files before the call off HistOut,
C                   then we can use the same unit numbers
C@empa aw 2000feb01 preFicSPol
C@empa aw 2000mar15 FVold, FVoldDefined added
C@empa aw 2000apr25 If we have multischedule or meteo schedule file, NextEvent could be
C@empa aw           CompDay or MultiSchedTime
C@NBI PGS 2000Jul17 - But it could be command-line error too, so message modified
C@NBI PGS 2000Jul19 - Tidied up code; no syntax change
C@NBI PGS 2000Jul19 - "UseCRT" and "Test" weren't initialized, but now are
C@NBI PGS 2000Jul19 - Simplified command-line syntax to COMIS [ifile] [ofile],
C@NBI                 Read about it in routine RdCmdLn (COMV-UNX.FOR)
C@NBI PGS 2000Oct09 - FlgIConc and FlgOConc now redundant, so removed
C@NBI PGS 2003Mar16 - "ICindex" now used so declatation added.
C
CTC The adaption of comven for TRNSYS has been made by:
CTC
CTC Viktor Dorer 		      EMPA, Duebendorf, Switzerland
CTC Andreas Weber		      EMPA, Duebendorf, Switzerland
CWK 
CWK and updated for TRNSYS 16 by 
CWK Werner Keilholz         CSTB Sophia Antipolis, France
CTC
CTC
CTC
CTC ------ Pass parameter description --------------------------------------
CTC
CTC  Name          Unit   Name  Description     
CTC  -----------------------------------------------------------------------
CTC  TIME            h          Simulation time since the simulation start
CTC
CTC  PARAMETERS: PAR(Nr)
CTC  Nr            Unit   Name  Description
CTC  ------------------------------------------------------------------------
CTC  1               -    NRZ   Number of ROOM zones !!
CTC  2               -    NLS   Number of links with schedules
CTC  3               -          Link nr. of first link with schedule
CTC  4               -          Link nr. of second link with schedule
CTC  NLS+2           -          Link nr. of NLSth link with schedule
CTC  NLS+3           -    NSO   Number of source schedules
CTC  NLS+4           -  Pnr.Znr Pollutant nr. and zone nr. of first source schedule
CTC      ...
CTC  NLS+NSO+3       -  Pnr.ZNr Pollutant nr. and zone nr. of last source schedule
CTC  NLS+NSO+4       -    NSI   Number of sink schedules
CTC  NLS+NSO+5       -  Pnr.Znr Pollutant nr. and zone nr. of first sink schedule
CTC      ...
CTC  NLS+NSO+NSI+4
CTC  NLS+NSO+NSI+5   -    NO    Number of outputs
CTC  NLS+NSO+NSI+6   -  OI1.OI2 Output Index one and two of the first output
CTC      ...
CTC  NLS+NSO+NSI+NO+5 - OI1.OI2 Output Index one and two of the last output
CTC
CTC  INPUTS: XIN(Nr)
CTC  Nr            Unit   Name  Description
CTC  ------------------------------------------------------------------------
CTC  1             m/sec  V     Wind speed 
CTC  2             deg    Dir   Wind Direction
CTC  3             deg C  T-a   Outdoor air temperature 
CTC  4             kg/kg  Hum-a Absolute humidity of outdoor air   
CTC  5             Pa     P-a   Absolute outdoor pressure             
CTC  6             kg/kg  C1-a  Outdoor concentration of pollutant nr 1 
CTC  7             kg/kg  C2-a  Outdoor concentration of pollutant nr 2 
CTC  8             kg/kg  C3-a  Outdoor concentration of pollutant nr 3 
CTC  9             kg/kg  C4-a  Outdoor concentration of pollutant nr 4 
CTC  10            kg/kg  C5-a  Outdoor concentration of pollutant nr 5 
CTC  11            deg C  Tz(1) Room temperature zone 1 
CTC      ...
CTC  NRZ+10        deg C  Tz(Nrz)
CTC  NRZ+11         -     Mf    Actual multiplication factor for first link with schedule
CTC      ...
CTC  NRZ+NLS+10     -     Mf    Actual multipl. factor for last link with schedule
CTC                             Multiplication factor is the actual value of:
CTC                             - Window opening factor or
CTC                             - Fan speed real or normalized
CTC                             Note:   A negative input value for Mf means:
CTC                                     The initial value given in CIF will not change.
CTC  NRZ+NLS+11     -           Actual factor for first pollutant source with schedule  
CTC        ....                 
CTC  NRZ+NLS+NSO+10 -           Actual factor for last pollutant source with schedule 
CTC  NRZ+NLS+NSO+11 -           Actual factor for last pollutant sink with schedule 
CTC        ....
CTC  NRZ+NLS+NSO+NSI+10         Actual factor for last pollutant sink with schedule 
CTC
CTC  OUTPUTS: OUT(Nr)
CTC  Nr                   Description             
CTC  ---------------------------------------------------------------------------
CTC  1                    First output according the first OI1.OI2
CTC  ...
CTC  NO                   NO'th output according the NO'th OI1.OI2
CTC
CTC  Mode         Unit  Name   Output
CTC  ---------------------------------------------------------------------------
CTC  OI1 > 0      kg/h  Fma    Interzonal flow from zone OI1 zo zone OI2    
CTC  OI1 = -1     ach/h Inf    Infiltration into zone OI2
CTC  OI1 = -2     ach/h VIn    Ventilation supply into zone OI2 
CTC  OI1 = -3     ach/h Exf    Exfiltration from zone OI2        
CTC  OI1 = -4     ach/h VOut   Ventilation exhaust from zone OI2
CTC  OI1 < -10    kg/kg C      Concentration of pollutant nr. (-OI1-10)
CTC                            in zone OI2 
CTC  
CTC  INFO           
CTC  INFO(3)                   number of inputs
CTC  INFO(4)                   number of parameters
CTC  INFO(5)                   number of derivatives
CTC  INFO(6)                   number of outputs
CTC
CTC
CTC
Ch**********************************************************************


C-----------------
C     DECLARATIONS
C-----------------

CWK    TRNSYS acess functions (allow to acess TIME etc.) 
      USE TrnsysConstants
      USE TrnsysFunctions
CWK/end

C     ! All parameters in the include files are described in the Programmer's
C     ! Guide.
      IMPLICIT NONE

      INCLUDE 'comv-inp.inc'
      INCLUDE 'comv-uni.inc'
      INCLUDE 'comv-phy.inc'



CTC  Pass parameters:
          DOUBLE PRECISION XIN(200),OUT(3)
          DIMENSION T(1),DTDT(1),PAR(300),INFO(15)
	    INTEGER INFO,J,K,PNR,ZNR,ICNTRL
          REAL TIME,TIME0,oldtime,PAR,NLS,NSO,NSI,T,DTDT,DELT
CTC  Local
          INTEGER PARLAST
CTC/end
C     ! NSteps  = number of timesteps (not necessarily the actual number
C     !           of steps, assigned in DuplTm RestTMS or ReuseTMS to read
C     !           every record)
C     ! tmloop  = count variable for timeloop
C     ! tmstep  = count variable for the number of timesteps
C     ! chgkey  = Change Key, indicator if we have to run the ventilation
C     !           part again
C     ! SetFile = flag saying whether SET file exists and to use it
C     ! Fmunit  = unitstrings of massflow. Unitstrings start at column 1,10,20....
C     ! Fmconv  = conversion factor from kg/s to the given units in Fmunit
C     ! nDkey   = number of keywords in the string Keys
C@NBI PGS 2003Apr28 - nDname was not being used
CC    ! nDname  = number of names in the string name

      DOUBLE PRECISION TLotus
      CHARACTER*31 DTstring
      INTEGER NSteps,tmloop,tmstep,chgkey
      LOGICAL last
      LOGICAL SETFILE
C@NBI PGS 2003Apr28 - nDname was not being used
CC    INTEGER nDname
      INTEGER i
      INTEGER Key
      character*160 Line
      INTEGER Lstr,Lstr2
      INTEGER NSecDay,Jday1,numcreate
      CHARACTER STR*40,str2*40
      LOGICAL FirstStep
      CHARACTER*30 flname
      INTEGER iNstep
      LOGICAL iinTime
      INTEGER LENSTR,iCheckNet
      INTEGER NextEvent, CurTime
      INTEGER nextpoltime, lastpol
      DOUBLE PRECISION CreateTm
      integer stpInit, ICindex
CTC 
CTC  Flag saying whether SET is opened by TRNSYS
	LOGICAL there
	INTEGER NRZ
      INTEGER errk,btime
      DOUBLE PRECISION starttime

C@empa aw 2005nov01
CC      COMMON /SIM/ TIME0,TFINAL,DELT
      REAL TFINAL

      SAVE starttime
	SAVE firststep
	SAVE /Versio/ 
CNB        SAVE /KeysNam/
      SAVE /KeyNam2/
	SAVE /DAFACCESS/
      SAVE /USRuni1/
      SAVE /USRUNI2/
      SAVE /CIFPROB/
      SAVE /PRCONT/ 
      SAVE /AFC1/
      SAVE /AFC2/
      SAVE /AFC3/
      SAVE /TRANSIT/
      SAVE /NETZON1/
      SAVE /NETZON2/
      SAVE /NETZL1/
      SAVE /NETZL2/
      SAVE /NETZP1/
      SAVE /NETZP2/
      SAVE /NETEXT1/
      SAVE /NETEXT2/
      SAVE /CPVALUES1/
      SAVE /CPVALUES2/
      SAVE /NETLIN1/ 
      SAVE /NETLIN2/ 
      SAVE /LINK1/
      SAVE /Cp/
      SAVE /ENV/
      SAVE /MET1/
      SAVE /MET2/
      SAVE /ENVOUT/
      SAVE /POLLUT1/
      SAVE /OCCUPAN1/
      SAVE /OCCUPAN2/
CNB      SAVE /D3BUIL/
      SAVE /NORMCR/
CNB      SAVE /PRECAL/
CNB      SAVE /ZONVENT/
      SAVE /LINKVENT/
CNB      SAVE  /CGOLuns/
      SAVE  /KeyWords1/
      SAVE  /KeyWords2/
CNB      SAVE  /CGOlns/
CNB      SAVE  /CGORanges/ 
      SAVE  /OutLn/ 
      SAVE  /OutLn2/ 
      SAVE /PROUT/ 
      SAVE /SCHTIM/
      SAVE /SCHTIM2/
      SAVE /MULTI1/
      SAVE /MULT2/
CNB      SAVE /CHECKCIF/
      SAVE /lst10l/
	SAVE /COMVUNITS/
	SAVE /REPORT/

CTC/end



C-------------------
C     INITIALIZATION
C-------------------


CWK    SET THE VERSION INFORMATION FOR TRNSYS: run in TRNSYS 15 mode
      IF(INFO(7).EQ.-2) THEN
	   INFO(12)=15
	   RETURN 1
	ENDIF

	VersionStr = 'COMIS for TRNSYS 16 version 3.2.0 (2005-11-01)  '//
     &'/Empa/NBI/CSTB/TNO/LBL/'
      CopyrightStr='Copyright (C) 2001 - 2005  Empa'
	

C@empa aw 2005nov01
! Perform last call manipulations
      if(INFO(8) == -1) then
	  TFINAL = getSimulationStopTime()
        DELT = getSimulationTimeStep()
	  Tmstep=TIME/DELT
        call message(Tmstep)
        CLOSE(DAF,STATUS='DELETE')
        return 1
      endif 


CWK/end
CTC
CTC  Initialisations only if it is the first call to this TYPE
        IF (INFO(7).EQ.-1)THEN
	     INFO(6)=500
	     CALL TYPECK(1,INFO,-1,-1,0)
	  
CTC/end

C@empa aw 2000dec22 There was no initialisation for unit number TIF
C     ! KBD = unit number for the keyboard
C     ! CRT = unit number for (the screen )  for peliminary output file
C     ! CIF = unit number for data input file
C     ! COF = unit number for data output file
C     ! CER = unit number for error output file
C     ! SET = unit number for settings input file
C     ! MSF = unit number for multi schedule file
C     ! IFS = unit number for schedules input file (F:) or (f:)
C     ! UOF = unit number for user output file
C     ! TIF = unit number for temperary input file 
C     ! CMF = unit number for meteo file 
C     ! DAF = unit number for temporary file
C     ! TMS = unit number for temporary file
C     ! TSV = unit number for temporary file
C     ! TMM = unit number for temporary file

      KBD = 5
      CRT = 6
CT      CIF = 10
CT      COF = 11
      CER = 12
CT      SET = 13
      MSF = 14
      IFS = 15
      COU = 16
      TIF = 17
      DAF = 20
      TMS = 21
      TSV = 22
      TMM = 23
C     TMM < StartFUnitForCSO <= MaxFUnitNr

      Switch = 0

C     ! UOFunf = Flag to tell how to write UOF (1 = unformated)
      COUunf = 0

C     ! Initialize default input/output file names
C     ! These would be used if there is no SET file and no command-line parms
      CIFs = ' '
      COFs = ' '
      COUs = ' '
      COSs = ' '

C     ! Default separator string for spreadsheet output (can be changed in COMIS.SET)
C     ! Tab delimiter seems to work best for opening in Excel
C     ! by mouse-clicking the file icon in Windows Explorer.  You
C     ! must first "associate" the .CSO file extension with Excel.
      SepStr = '"	"'
      multifile=' '

C     ! Initialize some Physical constants COMMON /PHYSIC/
      GasConst =  287.055
C@NBI PGS 2000Oct31 - MolVol is strictly not a constant, but depends
C@NBI                 upon temperature & pressure, and is governed by
C@NBI                 the ideal gas law (PV=nRT).  At STP (273.15 K and
C@NBI                 101325 Pa) MolVol = 22.414, but in COMIS, it is
C@NBI                 more sensible to operate with MolVol at room
C@NBI                 temperature of, say 20°C (24.055 m3/kmol) as we usually
C@NBI                 aren't interested in pollutant concentration at
C@NBI                 other temperatures. This is important, because the
C@NBI                 error in pollutant concentration can exceed 7%.
CC    MolVol   =   22.414
      MolVol   =   24.055
      Tzero    =  273.15
      MMair    =   28.9645
      MMH2O    =   18.01534
      CpAir    = 1004.
      LamAir   =    0.025
C     ! Standardized barometer pressure is in DPbMet=101.325 kPa

      TmStep   = 0
      Polstep  = 0
      Polstep1 = 0
      pol1time = 0
      Polstep2 = 0
      FlgPolDes= 0


C----------------------
C     OPEN & READ FILES
C----------------------

C     ! First write version number to screen

      WRITE(CRT,1002) VersionStr(1:lenstr(versionstr)),CopyrightStr               
1002  FORMAT(/1X,A/1X,A/1X, 55("-"))


C     Open .CER (error) output file
C----------------------------------

      OPEN(CER,FILE='COMIS.CER',STATUS='OLD',ERR=10)
      CLOSE(CER,STATUS='DELETE')
10    CONTINUE
      OPEN(CER,FILE='COMIS.CER',STATUS='NEW')


C     Read .SET (default settings) input file
C--------------------------------------------

C     ! Initialize default echo settings before reading .SET file, if it exists
      UseCRT   = 0
      Test     = 0
      IEcho    = 2
      PEcho    = 0
      SEcho    = 0
      OEcho    = 3

CTC     Added code to allow TRNSYS user to specify COMIS input, output and SET  
CTC     file in TRNSYS input file as last Parameters and ASSIGN statement.
CTC     TRNSYS internally opens all assigned files.
        PARLAST = INFO(4)
        SET = PAR(PARLAST-2) 
        CIF = PAR(PARLAST-1)
	  COF = PAR(PARLAST)

CTC/end


C     ! As unit conversions can occur while reading COMIS.SET, the unitStr
C     ! and Conv matrix must be filled beforehand, and the conversion factors
C     ! ifact(NoUnit) and ofact(NoUnit) must be set to a default 1.

C     ! ifact(NoUnit) and ofact(NoUnit)
      do i=1,NoUnit
         ifact(i)=1.0
         ofact(i)=1.0
      ENDDO
C     ! set temperature offset to 0.0
      ifact(UnitToff)=0.0
      ofact(UnitToff)=0.0
      CALL FilUnit

CTC If COMIS is Used as TRNSYS-Type , SET is already opened
CTC                   from assign command in the DCK
C@empa aw 2004oct01 option to skip the SET file unit 
      SETFILE = .TRUE.
      INQUIRE (CIF,NAME=CIFs)
      IF (SET.NE. 0.) THEN
        INQUIRE (SET, OPENED = there)
        IF (there) THEN
          REWIND SET
	    READ(SET,'(A)',END=12,ERR=12) Line
          REWIND SET
        ELSE
          OPEN(SET,FILE='COMIS.SET',STATUS='OLD',ERR=12)
        ENDIF
      ELSE
        OPEN(SET,FILE='COMIS.SET',STATUS='OLD',ERR=12)
CT	  SETFILE = .FALSE. 
      ENDIF
C     ! flag saying SET file exists and to use it
      GOTO 14
	
12    CONTINUE
       SETFILE = .FALSE.
C      ! just a NOTE now to say SET file doesn't exist
       write(CRT,*) 'No SET-file; using defaults'
          

14    CONTINUE
C     ! Read first 2 parts of COMIS.SET file, if it exists
C     ! Routine setting also tells us whether CRT is used before any
C     ! error message can occur (UseCRT).
      if (SETFILE) CALL setting

C     ! error messages will just go to CER until COF is openned
      IF (UseCRT.EQ.0)  CRT=0

C     ! Read last part of COMIS.SET file, if it exists
CTC   ! Filnames are assigned in TRNSYS deck file
CT      if (SETFILE) then
CT         REWIND(SET)
CT         CALL ReadSet('&-COMIS',CIFs,COFs,UOFs,UOFunf,CSOs,
CT     &      SepStr,OCase,Variab,Clines10)
CT         CLOSE(SET)
CT      endif


C     Read command-line options  (e.g. input or output files)
C------------------------------

CT      CALL RdCmdLn(CIFs,COFs,UOFs,UOFunf)


C     Open output files  (if specified)
C----------------------

CT      if (lenstr(COFs).eq.0)  COF = 0
      if (lenstr(COUs).eq.0)  COU = 0
      IF (UseCRT.EQ.0)        CRT = COF

C     ! If there is a user output file specified, no COF is written.
      if (COU .ne. 0) then
         COF=0
C        ! when we have UOF and no CRT, the output for CRT goes to UOF
C        ! EXCEPT when the user output file is UNFORMATTED
         IF (UseCRT.EQ.0) then
            if (COUunf .eq. 0) THEN
               CRT=COU
            else
C              ! must have SOME place for output even if the user says no
               crt=6
            endif
         ENDIF
C        ! write user output file
CT         if (UOFunf .eq. 0) then
CT            OPEN(UOF,FILE=UOFs)
CT            WRITE(UOF,1002) VersionStr(1:lenstr(versionstr))
CT         else
CT            OPEN(UOF,FILE=UOFs,FORM='UNFORMATTED')
CT         endif
      endif

      if (COF .ne. 0) then
C        ! write comis output file
CTC If COMIS is Used as TRNSYS-Type , COF is already opened from assign command in the DCK
          INQUIRE (COF, OPENED = there)
          IF (there) THEN
            REWIND COF
          ELSE
	      OPEN(COF,file=COFs,status='unknown')
          ENDIF
CT         OPEN(COF,FILE=COFs)
CTC/end

         WRITE(COF,1002) VersionStr(1:lenstr(versionstr)),CopyrightStr
C        ! note about no .SET file in the .COF file
         if (.NOT. SETFILE)
     &      write(COF,'(/A/)') ' No COMIS.SET file; using defaults'
      endif


C     Read .CIF input file
C-------------------------

C     ! Fill arrays with list of keywords used in the *.CIF data file
      CALL filkey(Keys,KeysU,nDkey)

C     ! Fill array AFCname with prefixes (2-letter) for airflow compomenents
C@NBI PGS 2003Apr28 - nDname was not being used
CC    CALL filname(AFCname,nDname,SiStart)
      CALL filname(AFCname,SiStart)

C     ! Initialize some Default values for .CIF file data
      Call IniDefault

C     ! open CIF (Comis Sequential Input File)
CCT Added code to allow TRNSYS user to specify COMIS input file in DCK-file
CT      OPEN(CIF,FILE=CIFs,STATUS='OLD',ERR=150)
CT      GOTO 155
CT150   CALL ERROR('The specified *.CIF file ('//CIFs(1:LenStr(CIFs))
CT     &//') does not exist, or is empty.  It was specified in '//
CT     &'either the COMIS.SET file or on the command-line',1)
CT      GOTO 900
CT155   CONTINUE
      REWIND (CIF)
CTC/end

      FlgEnd=.FALSE.
      nCifLine=0

C     ! Open DAF (Direct Acces File). In this file the complete data and keyword
C     ! contents of CIF will be stored + pointers to keywords and *names
      OPEN(DAF,FILE='COMIS.DAF',ACCESS='DIRECT',RECL=160,FORM=
     &'UNFORMATTED',STATUS='UNKNOWN')

C     ! READ the data from the CIF (input file)
      CALL INH

      if (outputoptions(2).gt.NconcPoldes) then
         call intdis(outputoptions(2),str,Lstr)
         call intdis(NconcPoldes,str2,Lstr2)
C@NBI PGS 2000Nov05 - Better readability.  Should be downrated from a
C@NBI                 "WARNING" to a "NOTE" because it occurs whenever
C@NBI                 you use default value (5). Also don't need singular
C@NBI                 and plural versions.
CC         if (NconcPoldes.eq.1) then
CC            CALL INERR('At &-PR-SIMUlation '//str(1:Lstr)//
CC     &      ' output pollutants are requested.',
CC     &      'At &-POL-DES there is only '//Str2(1:lstr2)//
CC     &      '.  Output is reduced to '//Str2(1:lstr2)//
CC     &       ' pollutant.',.FALSE.,1)
CC         else
            CALL INERR('At &-PR-SIMUlation '//str(1:Lstr)//
     &      ' output pollutants are requested, but at',
     &      '&-POL-DES there are only '//Str2(1:lstr2)//
     &      '.  Output is reduced to '//Str2(1:lstr2)//
     &       ' pollutants.',.FALSE.,0)
CC         end if
         Outputoptions(2)=Nconcpoldes
      end if
      nconc=outputoptions(2)

C     ! CIF file can be closed now
      CLOSE(CIF)

C     ! Clear last 10 lines and keep track of current line from now on (we
C     ! may have several schedule/whatever files open).
C     ! Also set flag so readlin won't keep the last 10 anymore
      Keep10 = .false.
      Plines10 = 0
      do i=1,10
         Lines10(i) = ' '
      ENDDO


C-----------------------------
C     Precalculations & checks
C-----------------------------

C     ! Subroutine to check if the defined arrays are
C     ! big enough to build the binary trees in NamSort
      CALL ArrayCheck

C     ! Build binary trees
      CALL NamSort(nz,ZoNa,Zonr,ZoTree,ZoTreeN,ZoLarg,ZoSmal)
      CALL NamSort(nWind,ExNa,ExNr,ExTree,ExTreeN,ExLarg,ExSmal)
      CALL NamSort(nTyp,UsrAfc,AFCNr,UsTree,UsTreeN,UsLarg,UsSmal)
      CALL NamSort(nl,LiNa,LiNr,LiTree,LiTreeN,LiLarg,LiSmal)
      CALL NamSort(nF,FeNa,FeNr,FeTree,FeTreeN,Felarg,FeSmal)

      IF(test.ge.1 .AND. iecho.ge.4) THEN
         CALL PriTree('zone',nz,ZoNa,Zonr,ZoTree,ZoTreeN,ZoLarg,ZoSmal)
         CALL PriTree('exte',nWind,ExNa,ExNr,ExTree,ExTreeN,
     &             ExLarg,ExSmal)
         CALL PriTree('Uafc',nTyp,UsrAfc,AFCNr,UsTree,UsTreeN,
     &             UsLarg,UsSmal)
         CALL PriTree('lina',nl,LiNa,LiNr,LiTree,LiTreeN,LiLarg,LiSmal)
      ENDIF

C     ! Reorganisation of the zone layer and zone pollutant data according to the
C     ! internal zone sequence
      CALL InzReord

C     ! FromTo contains the Zone names and External names, we need to have the
C     ! sequence number of the zone and the sequence number of the external node.
C     ! in FrTo we convert with the aid of Lstat and the bintree's into these
C     ! numbers that are in ZoTreeN and ExTreeN

C     ! Lstat is the link status:
C     ! Lstat  From        To
C     ! 0      zone      zone
C     ! 1       ext      zone
C     ! 2      spec      zone
C     ! 3      zone       ext
C     ! 4       ext       ext
C     ! 5      spec       ext
C     ! 6      zone      spec
C     ! 7       ext      spec
C     ! 8      spec      spec
C
C     ! In the routine FrTo two things are done all with the names:
C     ! 1 A pointer is made from the link-squence number (number of the
C     !   dataline under NET-LIN) to the first element that belongs to that
C     !   link in the array LDat.
C     ! 2 The array FromTo is filled with the zonenumbers externalnode numbers
C     !   and the number of the special pressures. This is done with the arrary
C     !   FromToS which contains the names for zones, ext.nodes and the special
C     !   pressures.
C     !   The Lstat array indicates whether the number is a zone, ext.node, or
C     !   spec-pressure.
      CALL FrTo

C     ! Similar to what FrTo does for the Air flow network, we do have to
C     ! replace indices to ZoNa and the numbers of the external nodes in
C     ! WalLin with the Zone sequence numbers and the cp numbers
C     !  NWL = the number of wall links
      if ((test.ge.1) .and. (iecho.ge.5))
     &   write(cof,*) ' Number of wall links NWL=',NWL
      if (NWL.gt.0)  CALL FrToW

      CALL CheckNames('&-NET-ZONEes: Zone ID: ',ZoNa,nZ)
      CALL CheckNames('&-NET-EXternal-node: External Node Nr: ',
     &                 ExNa,nWind)
      CALL CheckNames('&-NET-AIR flow component: AFC-type : ',
     &                 UsrAFC,nTyp)
      CALL CheckNames('&-NET-LINKs: Link ID: ',LiNa,nL)
      CALL CheckNames('&-CP-VALUes: Facade Element Nr : ',FeNa,nF)

      CALL CheckLink

C     ! Check and prepare the data for single sided ventilation
      CALL CheckSSVInP

C     ! Call routine prejunc which preprocesses the input information about
C     ! the HVAC-junctions
      CALL PreJunc

C     ! Call routine preRF which preprocesses the input information about the
C     ! RF components (Related Flowrates). PreRF puts the linknumber of the
C     ! link, which name is in RefLink(i), in iRFlist(i) for a link that is an
C     ! RF component
      CALL PreRF

C     ! for Pollutant names Cname
C     ! Check for Pollutant Schedule names at zones to start with a letter and
C     ! not with a number. Example:
C     !     12CO2 is wrong
C     !     CO2human is right
C     ! Pollutant source and sink schedules at &-NET-ZONes must match the
C     ! pollutant names. These schedules start with 'S'=sink or 'Q'=source
C     ! the S and Q may be followed by an integer, number of 1 or more digits
C     ! (no sign then should follow the first part of a pollutant name. This
C     ! part must match only one pollutant name
      CALL PreSchZ

C     ! Check if used schedules are defined
C     ! all schedules used at NET-ZONes and NET-LINk must be defined
      CALL PreSch

C     ! Check for the characteristic of fans
      CALL CheckFan()

C     ! Check the network. Are all zones reachable? Connected to at least one
C     ! known pressure?
C     ! Keep in mind that resetting a link to 0 in the timeloop may cut the
C     ! network in two parts. In fact that is not allowed and should be
C     ! checked in the timeloop
      if (INT(iCheckNet()+0.5).ne.1) then
         CALL ERROR2('The Network is not properly connected:',
     &      'PROGRAM STOP',2)
         FInpErr=1
      end if

C     ! Print the SET and the CIF file into the the COF file
C     ! if the output options SET and INPUT are set in the CIF file.
      if (COF.NE.0) then
         if (OutPutOptions(6).EQ.1) then
CWK : added IF to skip SET file reading when not used
		if (SETFILE) THEN
            OPEN(SET,FILE='COMIS.SET',STATUS='OLD')
            REWIND(SET)
            WRITE (COF,'(///A///)') ' HERE STARTS THE COMIS.SET FILE: '
20          CONTINUE
            READ(SET,'(A)',END=30) Line
            write(COF,'(A)') Line
            GOTO 20
30          CONTINUE
            WRITE (COF,'(//////)')
		ELSE
		  WRITE (COF,'(///A///)') 'No SET file, using defaults.'
		END IF
         ENDIF

         IF (OutPutOptions(5).EQ.1) THEN
            OPEN (CIF,FILE=CIFs,STATUS='OLD')
            nCifLine=0
            REWIND (CIF)
            WRITE (COF,'(///A///)') ' HERE STARTS THE INPUT FILE: '
     &      //CIFs(1:LenStr(CIFs))
40          CONTINUE
            READ (CIF,'(A)',END=50) Line
            nCifLine=nCifLine+1
            write (COF,'(A)') Line
            GOTO 40
50          CONTINUE
            WRITE (COF,'(//////)')
            CLOSE (CIF)
         ENDIF
      ENDIF

C     ! here follows an intended write to screen
      if (Outputoptions(11).gt.0)
     &   Write(*,*) 'Initialization of the TimeLoop'

C     ! Check to see if H2O is used as pollutant (sets ipolH2O and UseMetH2O)
C     ! ipolH2O is used in TMSmain..ReadDAF  POL-DES
      CALL preH2OPol

C     ! initialize the timeloop
      CALL TMSMAIN(NSteps)
CTC Set starttime; btime is julian day of 1900jan01
      call juld(btime,1,1,1900,errk)
      starttime=1+(JdStart-2415020)+h1/24.+M1/1440.+S1/86400.
CC      starttime=1+(JdStart-bTime)+h1/24.+M1/1440.+S1/86400.
CTC/end

C     ! initialise multischedules only when needed
CT      if (multifile .ne. ' ')  CALL MULTIMAIN()

C     ! total length of the simulation in seconds
C     ! for use in OnScreen progress
      SiMuLen=(jdstop-jdstart)*86400+(sec2-sec1)

C     ! initialize the graphical output
CT      CALL IniGrOut

C     ! Check whether the maximum nr of occupants at Histograms have been
C     ! defined. Note that this check uses the occupants in histograms at
C     ! &-PR-SIMU read by IniGrout
      CALL preOCC

C     ! Check the occupant generated pollutants versus the defined pollutants
C     ! at POL-DES
      CALL preOCCPol

C     ! Check and assign the pollutant names of fictive sources with the
C     ! pollutant names in POL-DES
      CALL preFicSPol

C     ! Check to see if all used histograms have been defined
CT      CALL preHis

C     ! Check zone IDs in occupant schedules
      call CheOccZo

      IF (CerCount(2).GE.1) THEN
C@NBI PGS 2000Aug16 - This is itself interpreted as a SEVERE and is counted
C@NBI                 as an additional error for the message in routine
C@NBI                 MESSAGE, which is a bit misleading.  It's better
C@NBI                 just to give a normal NOTE message before aborting.
CC       CALL ERROR2('ERRORS IN INPUT FILE DATA :  ',
CC   &      'PROGRAM STOP',2)
         CALL LastERRor2('Program aborted due to errors in input data.',
     &   '(See error messages above)',2)
         GOTO 900
      ENDIF

C@NBI PGS 2003Mar16 - Moved block on flow unit conversion here from subroutine VENT2OUT
CC    if(COF.NE.0) write(COF,'(/A)') 'THE OUTPUT STARTS HERE!!'
      IF(COF.NE.0) THEN
        write(COF,'(/A)') ' THE OUTPUT STARTS HERE!!'
        IF ((ICindex(oUnit(UnitFma),'m3').EQ.0).AND.(OEcho.GE.1))
     &    WRITE (cof,'(/A)')
     &    'Volume flowrates [m3/s] = Mass flowrates [kg/s] / 1.2'
      ENDIF

      if (outputOptions(10).gt.0) then
          write(cof,'(/A/A/A/A/A)')
     &    ' You have ''EchoSch'' in &-PR-SIMUl datablock.  Schedules wi'
     &    //'ll thus report all changes:'
     &    ,' For weather   (SCH3): Time  Sched.name  Vwind  Dwind  Temp'
     &    //'  Xh  Patm'
     &    ,' For occupants (SCH4): Time  Sched.name  ZoneNr  ActFactor '
     &    //' Nr.Occupants'
     &    ,' For ext.conc. (SCH6): Time  Sched.name  Pollutant  FacadeN'
     &    //'r  AmbientConc  Conc.atFacade'
     &    ,' All others    (SCH0): Time  Sched.name  Sched.factor  Fina'
     &    //'lValue'
      end if

CT      IF (metfile.NE.' ') THEN
CT         CALL METINIT
CT         call wrtBefore()
CT         CALL DUPLTM(0,0,0,' ',' ',iNstep,iinTime,' ')
CT      end if

CT      IF (Switch.EQ.0) THEN
CT         flname='COMIS.TMS'
CT      ELSE
CT         flname='COMIS2.TMS'
CT      ENDIF
CT      OPEN(TMS,file=flname,access='direct',recl=30,form='unformatted')

CT      if (outputoptions(13).ge.2)                           call EchoTMS
CT      if (outputoptions(13).eq.1.or.outputoptions(13).eq.3) call EchoDAF

C     ! check TMS for a schedule with more than one event at the same time
CT      Call CheckTMS()

C     ! timeloop initialisation
CT     tmloop      = 2
CT      last        =.FALSE.
      FirstStep   =.True.
      stpInit     = stp1Init
      lastpol     = 0
      nextpoltime = 0

CTC ---Initialisations for plltrans
      oldtime=time
      DO 221 i=1,nz 
         DO 222 j=1,nconc
            Cold(j,i)=C(j,i)
            OrSource(j,i)=Source(j,i)
            OSink(j,i)=Sink(j,i)
222      CONTINUE
221   CONTINUE 
CTC end of first call of TYPE
      endif
CTC/end


C-------------------------------
C     START OF AIRFLOW TIME LOOP
C-------------------------------
CTC  
C@empa aw 2004jan28 Starttime corresponds to time = 1 --> Starttime+(time-1)/24   
CC	  TLotus=Starttime+(time)/24
	  TLotus=Starttime+(time-1)/24

CTC Call pollutant routine just once in the first iteration of each timestep with old values
CTC calculate interval
        interval= int (DelT*3600)
        if (time.NE.oldtime)THEN
           if (outputoptions(2).gt.0) then
             if (outputoptions(12).eq.1) then
CC               CALL Pollutant(TLotus,0)
               CALL Pollutant(0)
             else
CC               CALL Pollutant(TLotus,interval)
               CALL Pollutant(interval)
             end if
CTC PolOut prints Cold that is in the stand alone COMIS the actual concentration
CTC calculated in the last timestep. As we calculate in COMIS-TRNSYS the actual  
CTC concentration C in the first iteration of the actual timestep we have to put 
CTC C into Cold bevor the call to PolOut. 
             DO 231 i=1,nz 
                DO 232 j=1,nconc
                   Cold(j,i)=C(j,i)
232             CONTINUE
231          CONTINUE 
             
C@empa aw 2004jan28
CX             CALL PolOut(COF,last,DTstring)
             oldtime=time
	     endif
        endif


CTC TRNSYS-TYPE parameters and inputs
        NRZ=PAR(1)
        k=1
        Vmet=XIN(k)
        k=k+1
        Dmet=XIN(k)
        k=k+1
        Tmet=XIN(k)
        k=k+1
        Xhmet=XIN(k)
        k=k+1
        Pbmet=XIN(k)
        k=k+1  
        DO 245 J=1,5
           Cout(J)=XIN(k)
           k=k+1
           DO 240 i=1,Nwind
              ExtConc(J,i)=Cout(J)*OuCF(i)
240         CONTINUE
245       CONTINUE
        
        DO 255 i=1, NRZ
           Tz(i)=XIN(k)
           k=k+1
255      CONTINUE
CTC  link schedule:
CTC  For the links which CIF link sequence number given under Type57/PARAMETER
CTC  in the TRNSYS deck, Mf is overwritten by the value provided by XIN. 
CTC  For the other links, the value as given in the CIF file is valid.
        NLS=PAR(2)
        DO 265 i=1,NLS
             Mf(NINT(PAR(i+2)))=XIN(k) 
             k=k+1
265      CONTINUE
CTC      source schedule
        NSO=PAR(NLS+3)
        DO 275 i=1 ,NSO
             Pnr=INT(PAR(NLS+3+i))
             Znr=NINT(100*(PAR(NLS+3+i)-PNr))
             IF (Pnr*Znr.NE.0) THEN
               Source(Pnr,ZNr)=OrSource(Pnr,Znr)*XIN(k)   
     &         *Ifact(UnitPSou)     
             ENDIF                     
             k=k+1
275      CONTINUE
CTC       sink schedule 
         NSI=PAR(NLS+NSO+4)
         DO 285 i=1,NSI
             Pnr=INT(PAR(NLS+NSO+4+i))
             Znr=NINT(100*(PAR(NLS+NSO+4+i)-PNr))
             IF (Pnr*Znr.NE.0) THEN
               Sink(Pnr,ZNr)=Osink(Pnr,Znr)*XIN(k)   
     &         *Ifact(UnitPSin)     
             ENDIF                     
             k=k+1
285      CONTINUE
         
CTC/end




100   CONTINUE

C     ! Have to keep the previous Occupant Activity and
C     ! Occupant numbers (might be possible to store them at the histograms)
      if (IsHopt)  call keepprevious

CT      if (last) goto 800
CT      IF (test.gt.0 .or. OEcho.gt.2)  WRITE(COF,1000)
C@NBI PGS 2000Aug18 - Differentiate between airflow & pollutant transport time steps
CC1000  FORMAT(/,'=new=timestep',67('='))
CT1000  FORMAT(/,'=new=airflow=timestep',59('='))

CT      CALL TIMESTEP(tmloop,TLotus,chgkey,last)
C     ! Get time of next ventilation event
CT      if (.not. last) READ(TMS,REC=tmloop+1) NextEvent
CT      NextEvent=min(NextEvent,CompDay,MultiSchedTime)
C     ! Get the current time in seconds since start
CT      CurTime = NumCreate(TLotus)

C     ! See if we should switch to Polstep2 yet
CT      if (pol1time.gt.0 .and. (Curtime.ge.pol1time)) Polstep=Polstep2
C     ! First "extra" pollutant calculation is polstep seconds after FIRST
C     ! vent event
CT      if (Polstep .gt. 0)  interval = min(interval, Polstep)

CT      tmstep=tmstep+1

CT      if(Outputoptions(11).gt.0.and.FirstStep) call EchoOnScreen(TLotus)

C     ! At the moment chgkey is not used actually. For every schedule type the
C     ! variable is set to 1, so that the ventilation part is always performed.
C     ! Calculation of ventilation also when no schedules.
CT      chgkey=1
C     ! decide if we have to run the ventilation part again
CT      IF (chgkey.EQ.0) GOTO 200

C     ! Echo the input data for ventilation
CT      IF(COF.NE.0)  CALL echodat(CIFs)

      CALL CreateJdTim(numcreate(TLotus),Jday1,NSecDay)
      CALL CONVDAT1(Jday1,NSecDay,DTstring)
      CALL HeadOut(COF,DTstring)


C     Precalculations
C--------------------

      call PRECAL(TLotus,firstStep,stpInit)

      if ((COF .ne. CRT) .and. (COF .ne. 0))
     &   WRITE(CRT,*) 'At time = ',DTstring,',interval = ',
     &   interval,' seconds'


C     Ventilation solver
C-----------------------

C     ! Reset FVoldDefined (used in MatVec)
      FVoldDefined=.False.
      if( slvSel.ge.0 .and. slvSel.le.5 ) then
         call STRSLV(nZ,nL,LStat,
     &      LDat,pLiLDat,DpL,FromTo,Mf,Mp,epsFA,epsFR,epsCJ,
     &      RhoL,SqrRhoL,MuL,Pz,FvNew,Fv2,Ft,Fmb,
     &      nIter,key,nNewt,difLim,relaxV,slvSel,
     &      ofact(UnitFma),mIter,DMZ,stpInit,rhoSync,RSqrtDff,
     &      nJunc,JuncNr,DpJ,LinkDat,pLinkDat,
     &      TL,TempL,normCrRho,normCrMu,
     &      DpProf,RhoProfF,RhoProfT,ProfPtr,IrfList,Vmet,rhoOut,
     &      FVold,FVOldDefined)
      else
         call SLV01(nZ,nL,LStat,
     &      LDat,pLiLDat,DpL,FromTo,Mf,Mp,epsFA,epsFR,
     &      RhoL,SqrRhoL,MuL,Pz,FvNew,Fv2,Ft,Fmb,
     &      nIter,key,difLim,
     &      ofact(UnitFma),mIter,DMZ,stpInit,rhoSync,RSqrtDff,
     &      nJunc,JuncNr,DpJ,LinkDat,pLinkDat,
     &      TL,TempL,normCrRho,normCrMu,
     &      DpProf,RhoProfF,RhoProfT,ProfPtr,IrfList,Vmet,rhoOut,
     &      FVold,FVOldDefined)
      endif

      if( key.gt.0 ) then
         write(CRT,*) ' '
         write(CRT,*) ' VENTILATION NETWORK SOLVER REPORTS ERROR'
         if( key.eq.99 ) then
            write(CRT,*) ' '
            write(CRT,*) 'There were too many iterations.'
            write(CRT,*) 'The maximum allowed (see *CIF'//
     &      ' &-PR-CONTrol) is:',mIter
         endif
      else if( test.ge.1 ) then
         write(CRT,*) ' '
         write(CRT,*) ' No ventilation errors reported'
      endif

C     ! In HistCalc the values at the end of the last interval were stored.
C     ! But they have to be overwritten with the new values now at the beginning
C     ! of the next interval. This is because we assume constant values during
C     ! an interval (except concentrations).
      if (IsHopt .and. HistActive)  call ROoptHCalc


C     Pollutant transport solver
C-------------------------------

200   CONTINUE

C     ! Call Pollutant only if this data is needed for output.
C     ! Call Pollutant also in the last timestep, for updating Cold
C     ! and calculating the steady state solution.

CT      if (outputoptions(2).gt.0) then
CT        if (outputoptions(12).eq.1) then
C@NBI PGS 2000Aug02 - "TLotus" no longer used by routine POLLUTANT as removed
CC          CALL Pollutant(TLotus,0)
CTC   Pollutant transport routine moved up. It is now called in the first TRNSYS
CTC   iteration of each timestep before updating the input values. That means,
CTC   we take the final found flows from the last timestep therefore POLLUTANT delivers
CTC   the concentrations of the actual timestep.
CT            CALL Pollutant(0)
CT         else
CC          CALL Pollutant(TLotus,interval)
CT            CALL Pollutant(interval)
CT         end if
CT         lastpol = CurTime
CT         nextpoltime = nextpoltime + interval
CT      endif


C     Post calculations
C----------------------

C     ! Flow matrix, mean age of air, etc.
      if (CalcFM)  CALL PostCal

C     ! Optional check for condensation risk
      IF(outputoptions(4).EQ.1)  CALL CheckCondensation


C     Now output results
C-----------------------

C     ! Formatted output file (*.COF)
C@NBI PGS 2003Mar16 - Bugfix: Pollutant results were being printed even when OEcho=0
C@NBI                 so added check for OEcho here. (OEcho=2 gives only building summary per
C@NBI                 timestep. OEcho >= 3 is now needed for zonewise timestep results)
CC    if (COF .ne. 0) then
      if ((COF.NE.0).and.(OEcho.GE.2)) then
C        ! write comis output file
         if (outputoptions(1) .eq. 1) then
C           ! output standard ventilation data
            CALL VentOut(COF)
         elseif (outputoptions(1) .eq. 2) then
C           ! output detailed ventilation data
            CALL Vent2Out(COF)
         endif
C        ! output pollutant data
         if (outputoptions(2).gt.0)  CALL PolOut(COF,last,DTstring)

      endif

C     ! Optional user output files (either to FORMATTED or UNFORMATTED file)
CT      if (UOF .ne. 0) then
CT         if (UOFunf .eq. 0) then
CT            CALL UserOutF(UOF,TLotus,DTstring)
CT         else
CT            CALL UserOutU(UOF,TLotus,DTstring)
CT         endif
CT      endif

C     ! write results for graphical output (.CSO files)
CT      CALL WriGrOut(TLotus)

C     ! Real-time update on-screen of solution progress
CT      if (Outputoptions(11).gt.0)  call EchoOnScreen(TLotus)


C----------------------------------------------
C     START OF POLLUTANT TRANSPORT TIME SUBLOOP
C----------------------------------------------

CT     if ((Polstep .ne. 0) .and. (outputoptions(2) .gt. 0) .and.
CT     &   (.not. last) .and. (nextpoltime .lt. NextEvent)) then

CT300      CONTINUE

C        ! Loop here if there are more pollutant calcs to be done even if
C        ! no ventilation events

C        ! If the user has specified a pollutant report interval, call the
C        ! pollutant subroutine at that interval in addition to the call that
C        ! happens after each ventilation event (TimeStep).  This was added so
C        ! that in the cases where the user wants pollutant calculations
C        ! without the extra cost of ventilation calculations (when the
C        ! ventilation isn't changing).

C@NBI PGS 2000Aug18 - Need a divider between pollutant transport time steps
CT         IF (test.gt.0 .or. OEcho.gt.2)  WRITE(COF,1003)
CT1003     FORMAT(/,  '=new=pollutant-transport=timestep',47('='))


C        ! Get the current time in seconds since start
CT         CurTime = NumCreate(TLotus)

C        ! See if we should switch to Polstep2 yet
CT         if (pol1time .gt. 0 .and. (Curtime+Polstep .ge. pol1time)) then
CT            Polstep  = Polstep2
CT            interval = Polstep
CT         endif

CT         interval = min(interval, NextEvent - nextpoltime)
CT         TLotus=CreateTM(nextpoltime)
CT         CALL CreateJdTim(numcreate(TLotus),Jday1,NSecDay)
CT         CALL CONVDAT1(Jday1,NSecDay,DTstring)

C@NBI PGS 2000Aug18 - Standardize the COF divider for airflow and pollutant transport time steps
CC         write(COF,*) DTstring,' lastpol=',lastpol,
CC     &      ' nxtpol=',nextpoltime,
CC     &      ' interval=',interval
CT         WRITE(COF,*) 'Date/time '//DTstring(1:lenstr(DTstring))//
CT     &   '  (Interval',interval,'sec;',lastpol,'->',nextpoltime,'sec)'

C        ! do a pol calc
CT         if (outputoptions(12).eq.1) then
C@NBI PGS 2000Aug02 - "TLotus" no longer used by routine POLLUTANT as removed
CC          CALL Pollutant(TLotus,0)
CT            CALL Pollutant(0)
CT         else
CC          CALL Pollutant(TLotus,interval)
CT            CALL Pollutant(interval)
CT         endif
C        ! print it
CT         CALL PolOut(COF,last,DTstring)
C        ! and write the graphic output
CT         CALL WriGrOut(TLotus)

CT         lastpol = nextpoltime
CT         interval = Polstep
CT         if ((nextpoltime .lt. NextEvent) .and.
CT     &      (nextpoltime + Polstep .gt. NextEvent)) then
CT            nextpoltime = NextEvent
CT            IF(secho.GT.1) write(COF,*) 'Shortening to NextEvent at '
CT     &      ,NextEvent
CT         else
CT            nextpoltime = nextpoltime + Polstep
CT         endif
CT         IF(secho.GT.1) write(COF,*) 'End of Pol loop, lastpol=',lastpol
CT     &      ,' nextpoltime=',nextpoltime


C        Next pollutant transport sub-timestep?
C----------------------------------------------

C        ! Loop if the next pollutant report time is before the next ventilation event
CT         if (nextpoltime .lt. NextEvent ) goto 300
CT      endif


C     Next ventilation timestep?
C-------------------------------

C     ! increment time loop counter
CT      tmloop=tmloop+1
C     ! Not the first step anymore
      if( firstStep ) then
         firstStep = .false.
         stpInit = stp2Init
      endif
CT      goto 100
CTC   TRNSYS Output  
      CALL TrnsOut(PAR(NLS+NSO+NSI+5),OUT,NRZ)


C------------------------
C     TIME LOOP ENDS HERE
C------------------------

CT800   CONTINUE

      IF (test.gt.0 .or. OEcho.gt.2)  WRITE(COF,1001)
1001  FORMAT(/,'=end=of=simulation',62('='))

C     ! MeanOut routine for T-Options output
CT      if (IsTOpt)  call MeanOut(COF)

C     ! Close *.CSO files before the call off HistOut,
C     ! then we can use the same unit numbers
CT      CALL ClGrOut

C     ! HistOut routine for H-Options output
CT      if (IsHOpt .and. HistActive) call HistOut

C     ! Jumps here when program aborts due to error
900   CONTINUE
CTC   close the files 
C@empa aw 2005nov01 last call manipulations moved to the top of the routine	     
CC      IF (TIME .GE. TFINAL) THEN
CTC/end
CC        call message(Tmstep)

C     ! Close files
CT      if (COF .ne. 0) CLOSE(COF)
CT      if (UOF .ne. 0) CLOSE(UOF)
CC        CLOSE(DAF,STATUS='DELETE')
CT      CALL CloseTMS
CT      CALL ClGrOut
CTC
CC      ENDIF
        RETURN 1
CTC/END

      END


Ch**********************************************************************
      SUBROUTINE EchoOnscreen(TLotus)
C***********************************************************************
C     Give a line with progress and data on screen during the simulation
C     only called if the keyword OnScreen in at PR-SIMU
C Changes:
C@tno jcp 1996May01_11:49:36 new routine
C@tno jcp 1996Apr19_13:55:51 Progress=percentage of time done for on screen info
C@NBI PGS 1999Aug12 - Fortran 90 option for non-advance output (currently commented out)
C@NBI PGS 2000Jul19 - Tidied up code; no syntax change
Ch**********************************************************************
      IMPLICIT NONE
      include 'comv-inp.inc'
      include 'comv-uni.inc'
      INTEGER LenStr
      INTEGER IcIndex
      DOUBLE PRECISION TLotus
      INTEGER i,lstr,NSecDay,Jday1,number,numcreate,k,l,ipol,
     &pos,izone
      REAL Progress,conc,totfmb
      CHARACTER*160 Line,str
      CHARACTER*31 DTstring

C     Precalcs
C-------------
      l=lenstr(Onscreen)
      line=' '
      number=numcreate(TLotus)

C     Time
C---------
      IF (ICINDEX(OnScreen,'time').gt.0) THEN
         CALL CreateJdTim(numcreate(TLotus),Jday1,NSecDay)
         CALL CONVDAT1(Jday1,NSecDay,DTstring)
         Line=DTstring
      ENDIF

C     Progress
C-------------
      IF (ICINDEX(OnScreen,'progress').gt.0) then
         if(SiMULen.gt.0) then
            progress=REAL(number)/REAL(Simulen)*100
         else
            progress=100
         end if
         str=' '
         i=int(progress)
         call intdis(I,str,Lstr)
         str=str(1:Lstr)//'%'
         call Longn(str,lstr,5)
         Line=line(1:lenstr(line))//str(1:Lstr)
      end if

C     No of iterations
C---------------------
      IF (ICINDEX(OnScreen,'Niter').gt.0) then
         str=' '
         call intdis(Niter,str,Lstr)
         call Longn(str,lstr,2)
         Line=line(1:lenstr(line))//' '//str(1:Lstr)
      end if

C     Total flow rate
C--------------------
C     This option presently does nothing. Totalinf is an empty string.
      IF (ICINDEX(OnScreen,'TotFlow').gt.0)
     &   Line=line(1:lenstr(line))//' '//TotalInf

C     Concentrations
C-------------------
      if (outputoptions(2).gt.0) then
C@empa aw 2000dec01 repeat if more than one polx
         pos=0
         k=1
         pos=ICINDEX(OnScreen,'Pol')
         Do while (pos.gt.0)
            variab='read Pol number from OnScreen str'
C           ! pos+2 is the position One before the number
            k=k-1+pos+2
            call getwi(onscreen,k,l,ipol,1)
            if (ipol.le.outputoptions(2)) then
               conc=0
               do izone=1,Nz
                  if (c(ipol,izone).gt.conc)  conc=c(ipol,izone)
               ENDDO
C@NBI PGS 2000Oct08 - Can now define different I/O units for each pollutant,
C@NBI                 so changed UnitPol-1 to UnitPolCo for concentration
CC             conc=conc*ofact(ipol+Unitpol-1)
               conc=conc*ofact(UnitPolCo+iPol)
               Call RelDis(conc,2,str,Lstr,1)
               Line=line(1:lenstr(line))//' '//str(1:7)//' '//
C@NBI PGS 2000Oct09 - Can now define different I/O units for each pollutant.
CC   &            Ounit(UnitPConc)(1:lenstr(Ounit(UnitPConc)))
     &            Ounit(UnitPolCo+iPol)(1:lenstr(Ounit(UnitPolCo+iPol)))
            else
               write (*,*) 'requested pollutant is out of'//
     &         ' the simulated range'
            end if
C@empa aw 2000dec01
CC            end if
            pos=ICINDEX(OnScreen(k:),'Pol')
         end do
C        ! end if pos>0 pol
      end if
C     ! end if outputOptions(2)

C     Residual error
C-------------------
      IF (ICINDEX(OnScreen,'Error').gt.0) then
         totfmb=0
         do izone=1,nz
            totfmb=totfmb+abs(fmb(izone))
         ENDDO
         Call RelDis(totFMB,2,str,Lstr,0)
         Line=line(1:lenstr(line))//' '//str(1:7)//' '//
     &   oUnit(UnitFma)(1:lenstr(oUnit(UnitFma)))
      end if

C     Write to screen
C--------------------
C     ! here follows an intended write to screen
C$    ! Just de-comment the appropriate line for your compiler
C$    ! Fortran 90 option is for non-advancing output
C$    write(*,'(A,A1)',ADVANCE='NO') line(1:lenstr(line)),char(13) !Fortran 90
C$    write(*,*) line(1:lenstr(line))                              !Fortran 77
      write(*,*) line(1:lenstr(line))

      return
      end


Ch**********************************************************************
      SUBROUTINE Message(tmstep)
C***********************************************************************
C     End-of-simulation message, written to the files and on screen
C
C     This is the only place in the whole program, where we can use "write(*".
C     Because this message goes to the screen anyway.
C
C Changes:
C@tno jcp 1996May01_11:49:46 new routine
C@tno jcp 1996Apr05_16:37:50 mention how many errors there are and in all files
C@NBI PGS 1999May07 - Output to CER should only occur if there were
C@NBI                 messages, i.e. if Imess > 0
C@NBI               - Also tidied output layout.  Simplified routine a lot by
C@NBI                 writing same info. to all three outputs.  I therefore
C@NBI                 created a new 'internal' subroutine "Message1" that
C@NBI                 writes same string to CER, COF and Screen outputs.
C@NBI PGS 2000Nov05 - Improved the output logic in "Any severe errors?" section
Ch**********************************************************************
      IMPLICIT NONE
      include 'comv-inp.inc'
      include 'comv-uni.inc'
      INTEGER ierr,imess,lstr,tmstep,lenstr
      character*160 str

      Imess=CERCount(0)+CERCount(1)+CERCount(2)+CERCount(3)
      Ierr=                         CERCount(2)+CERCount(3)

C---------------
C     Completed?
C---------------

      WRITE(str,'(A)')'------------------------------------------'
     &//'----------------------'
      IF(Imess.NE.0) call Message1(str,Imess)
      call intdis(tmstep,str,lstr)
      IF(tmstep.EQ.0)THEN
         Str='Aborted before starting simulation'
      ELSEIF(tmstep.EQ.1)THEN
         Str='One timestep'
      ELSE
         Str='There were '//str(1:lstr)//' timesteps'
      ENDIF
      Str='COMIS finished.  ('//str(1:lenstr(str))//')'
      call Message1(str,Imess)

C----------------
C     Any errors?
C----------------

      call intdis(imess,str,lstr)
      if (imess.eq.0) then
         str='There were no errors.'
      else if (imess.eq.1) then
C@empa aw 2001mar23 ***CER** changed to ***COMIS ERROR***
         str='There is one message, marked as COMIS MESSAGE ***** '//
     &   'in your CER & COF files.'
      else if (imess.gt.1) then
         str='There are '//str(1:lstr)//' messages, '//
     &   'marked as COMIS MESSAGE ***** in your CER & COF files.'
      endif
      call Message1(str,Imess)

C-----------------------
C     Any severe errors?
C-----------------------

      if (ierr.gt.0) then
         if ((ierr.EQ.1).AND.(imess.EQ.1.)) then
            str='There was a severe error.'
         else
C@NBI PGS 2000dec23 - Forgot to fill string STR with value of IERR
            call intdis(ierr,str,lstr)
            str=str(1:lstr)//' of the messages are severe errors.'
         endif
         call Message1(str,Imess)
      endif         
      END

C------------------------
C     INTERNAL SUBROUTINE
C------------------------

      SUBROUTINE Message1(str,Imess)
C        See description above in parent subroutine "Message"
         IMPLICIT NONE
         include 'comv-inp.inc'
         include 'comv-uni.inc'
         INTEGER Imess,IcIndex
         character*(*) str
C       .Write to COF, CER (if any messages), and Screen
C@NBI PGS 2000Jul16 - To prevent duplicate write to screen when no .COF
CC       call WRT80(COF,str,wCRT)
         IF (COF.NE.0) call WRT80(COF,str,wCRT)
         IF (Imess.NE.0) call WRT80(CER,str,wCRT)
C@NBI PGS 2000Jul16 - Always output to screen
CC       IF (ICINDEX(OnScreen,'Error').gt.0) call WRT80(0,str,wCRT)
         call WRT80(0,str,wCRT)
      END


Ch**********************************************************************
        SUBROUTINE KeepPrevious
C***********************************************************************
C KeepPrevious keeps the current occupant activity per room and occupant
C numbers per room in the arrays L(ast)OccAct and LOccNum.
C These are necessary to follow the concentration of an occupant while
C filling the histograms.
C This routine could be skippend if no histograms are used, which is indicated
C by the conditional CALL to KeepPrevious only if LOGICAL IsHopt=.TRUE.
C This updating cannot be done while assigning new values to OccAct and
C OccNum in the Schedule routines because at that point the previous zone
C is unknown.
C Changes:
C@tno jcp 1996May01_11:49:59 new routine
C@NBI PGS 2000Jul19 - Tidied up code; no syntax change
Ch**********************************************************************

      IMPLICIT NONE
      INCLUDE 'comv-inp.inc'
      INCLUDE 'comv-uni.inc'
      INTEGER i,j
      DO j=1,Nz
         DO i=1,MaxO
            LOccAct(i,j)=OccAct(i,j)
            LOccNum(i,j)=OccNum(i,j)
         ENDDO
      ENDDO
      return
      end


Ch**********************************************************************
      subroutine ho(str1,str2)
C Small pause routine to be called from COMIS, while debugging
Ch**********************************************************************

      IMPLICIT NONE
      character*(*) str1,str2
C     ! here follows an intended write to screen
      write(*,*) str1
      write(*,*) str2
      write(*,*) ' '
      pause
      return
      end


CTC
Ch*************************************************************************
       SUBROUTINE TRNSOUT(PAR,OUT,NRZ)
C*************************************************************************
       include 'comv-inp.inc'
       include 'comv-uni.inc'
C      QT    FLow matrix
C      VIn  Ventilation: supply air
C      VOut Ventilation: exhaust air
C      Inf  Infiltration
C      Exf  Exfiltration     

      DOUBLE PRECISION OUT(500)
      REAL PAR(300),QT(MaxZ,MaxZ),Inf(MaxZ),VIn(MaxZ),VOut(MaxZ)
Cdeb       DOUBLE PRECISION OUT(200)
Cdeb       REAL PAR(200),QT(MaxZ,MaxZ),Inf(MaxZ),VIn(MaxZ),VOut(MaxZ)
       REAL Exf(MaxZ),fromR,ToR

       INTEGER NRZ,i,j,k,count,from,to

C ------- Initialisations -------
       DO 5 i=1,NRZ
          DO 7 j=1,NRZ
             QT(i,j)=0
7         CONTINUE
          Inf(i)=0
          VIn(i)=0
          VOut(i)=0
C@empa aw 1994jun16 initialisation of Exf
          Exf(i)=0
5      CONTINUE

C ------- Fill up the QT matrix and the Inf and the VIn, VOut vectors
       DO 10 i=1,Nl
       FromR=FromTo(1,i)
       ToR  =FromTo(2,i)
	 From=NINT(FROMR)
	 To  =NINT(ToR)
       LTyp=LDat(PLiLDat(i))
       IF (Lstat(i).EQ.0) THEN
          IF (From.LE.NRZ) THEN
             IF (To.LE.NRZ) THEN
                QT(From,To)=QT(From,To)+FV2(1,i)
                QT(To,From)=QT(To,From)+FV2(2,i)
             ELSE
                VIn(From) =VIn(From)+FV2(2,i)
                VOut(From)=VOut(From)+FV2(1,i)
             ENDIF
          ELSE
             IF (TO.LE.NRZ) THEN
                VIn(To)  =Vin(To)+FV2(1,i)
                VOut(To) =VOut(To)+FV2(2,i)
             ENDIF
          ENDIF
       
       ELSE 
          IF(Lstat(i).EQ.1.OR.Lstat(i).EQ.2) THEN
C            cp ---> zn  ; spec ---> zn          
             IF (To.LE.NRZ) THEN
                IF (LTyp.EQ.2.OR.LTyp.EQ.3) THEN
C                  AFC is Fan or DS  ---> Ventilation
                   VIn(To)=VIn(To)+FV2(1,i)
                   VOut(To)=VOut(To)+FV2(2,i)
                ELSE
C                  AFC is not an HVAC component ---> Infiltration,Exfiltration
                   Inf(To)=Inf(To)+FV2(1,i)
                   Exf(To)=Exf(To)+FV2(2,i)
                ENDIF
             ENDIF
          ENDIF
          IF (Lstat(i).EQ.3.OR.Lstat(i).EQ.6) THEN
C            zn ---> cp  ;  zn ---> spec
             IF (From.LE.NRZ) THEN
                IF (LTyp.EQ.2.OR.LTyp.EQ.3) THEN
C	           AFC is Fan or DS ---> Ventilation
                   VIn(From)=VIn(From)+FV2(2,i)
                   VOut(From)=VOut(From)+FV2(1,i)
                ELSE
C                  AFC is not an HVAC component ---> Infiltration,Exfiltration
                   Inf(From)=Inf(from)+FV2(2,i)
                   Exf(From)=Exf(From)+FV2(1,i)
                ENDIF
             ENDIF 
          ENDIF
       ENDIF
10     CONTINUE
C-------- Fill the OUT array ------------

       count=1
       DO 110 k=1,PAR(1) 
          i=INT(PAR(1+k)+0.005) 
C@cstb nb 10MAR 1998
c          j=NINT(ABS(100*(PAR(1+k)-i)))
C@empa aw 21apr1998 No, what is if we have more then ten zones?
CC          j=NINT(ABS(10*(PAR(1+k)-i)))
          j=NINT(ABS(100*(PAR(1+k)-i)))
          IF (i.GT.0)THEN         
            OUT(count)=3600*QT(i,j) 
          ELSE IF(i.EQ.-1) THEN
C@cstb nb 10Mar1998 Added check for Vz(j)=0
	      IF (VZ(j).EQ.0.0) THEN
             OUT(count)=999
	      ELSE
             OUT(count)=3600*Inf(j)/rhoout/Vz(j)
	      ENDIF
          ELSE IF(i.EQ.-2) THEN
	      IF (VZ(j).EQ.0.0) THEN
             OUT(count)=999
	      ELSE
             OUT(count)=3600*VIn(j)/rhoout/Vz(j)
	      ENDIF
          ELSE IF(i.EQ.-3) THEN
	      IF (VZ(j).EQ.0.0) THEN
             OUT(count)=999
	      ELSE
             OUT(count)=3600*Exf(j)/rhoout/Vz(j)
            ENDIF  
          ELSE IF(i.EQ.-4) THEN
	      IF (VZ(j).EQ.0.0) THEN
             OUT(count)=999
	      ELSE
             OUT(count)=3600*VOut(j)/rhoout/Vz(j)    
            ENDIF
          ELSE IF(i.LT.-10) THEN
            i=-i-10
C@empa aw 2004jan28 convert to user units and use Cold
CC            OUT(count)=C(i,j)
            OUT(count)=Cold(i,j)*Ofact(UnitPolCo+i)
C@empa aw 2003oct08 new output link pressure difference
C@empa aw 2004feb19 get rid of the sign from OSR-option
          ELSE if (i.EQ.-5) THEN
	      OUT(count)=DpL(3,j)*ABS(Ofact(UnitP))
            
C@empa aw 2004jan27 new output: link flow FROM --> TO
          ELSE if (i.EQ.-6) THEN
	      OUT(count)=FV2(1,j)*Ofact(UnitFma)

C@empa aw 2004jan27 new output: link flow TO --> FROM
          ELSE if (i.EQ.-7) THEN
	      OUT(count)=FV2(2,j)*Ofact(UnitFma)
          
C@empa aw 2004jan27 new output: net link flow 
          ELSE if (i.EQ.-8) THEN
	      OUT(count)=FVnew(j)*Ofact(UnitFma)
            
C@empa aw 2004jan27 new output: zone pressure 
          ELSE if (i.EQ.-9) THEN
	      IF (Ofact(UnitP).LT.0) THEN
	         OUT(count)=(PZ(j)+G*zz(j)*RhoOut)*(-Ofact(UnitP))
            ELSE		
	        OUT(count)=PZ(j)*Ofact(UnitP)
            ENDIF
          
C@empa aw 2004jan27 new output: zone pressure -OSR 
          ELSE if (i.EQ.-10) THEN
	      OUT(count)=(PZ(j)+G*zz(j)*RhoOut)*abs(Ofact(UnitP))
          ENDIF

            
          count=count+1
110    CONTINUE
       RETURN
       END

