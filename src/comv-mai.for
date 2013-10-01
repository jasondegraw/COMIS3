C+*********************************************************** comv-mai.f
      PROGRAM COMIS
Ch**********************************************************************
C Copyright (C) 2001 - 2011 Empa
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
C
C     This program is free software: you can redistribute it and/or modify
C     it under the terms of the GNU  Lesser General Public License as published by
C     the Free Software Foundation, either version 3 of the License, or
C     (at your option) any later version.
C 
C     This program is distributed in the hope that it will be useful,
C     but WITHOUT ANY WARRANTY; without even the implied warranty of
C     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
C     GNU Lesser General Public License for more details.
C 
C     You should have received a copy of the GNU  Lesser General Public License 
C     along with this program.  If not, see <http://www.gnu.org/licenses/>.
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
C@empa aw 2000jan17 Close *.COS files before the call off HistOut,
C                   then we can use the same unit numbers
C@empa aw 2000feb01 preFicSPol
C@empa aw 2000mar15 FVold, FVoldDefined added
C@empa aw 2000apr25 If we have multischedule or meteo schedule file, NextEvent could be
C@empa aw           CompDay or MultiSchedTime
C@NBI PGS 2000Jul17 - But it could be command-line error too, so message modified
C@NBI PGS 2000Jul18 - Changed file extensons .cho to .coh, .cso to .cos,
C@NBI                 and .uof to .cou, so that all output file extensions
C@NBI                 start with .co ("o" for output) and all input files
C@NBI                 should have extension .ci? ("i" for input).  This
C@NBI                 logical naming convention makes it easier to manage
C@NBI                 input & output files.
C@NBI PGS 2000Jul19 - Tidied up code; no syntax change
C@NBI PGS 2000Jul19 - "UseCRT" and "Test" weren't initialized, but now are
C@NBI PGS 2000Jul19 - Simplified command-line syntax to COMIS [ifile] [ofile],
C@NBI                 Read about it in routine RdCmdLn (COMV-UNX.FOR)
C@NBI PGS 2000Oct09 - FlgIConc and FlgOConc now redundant, so removed
C@empa aw 2001feb05 CopyrightStr added
C@NBI PGS 2003Mar16 - Harmonized/unified OEcho levels (explained in COMV-DAT.FOR)
C@NBI PGS 2003Mar16 - "ICindex" now used so declatation added.
Ch**********************************************************************


C-----------------
C     DECLARATIONS
C-----------------

C     ! All parameters in the include files are described in the Programmer's
C     ! Guide.
      IMPLICIT NONE
      INCLUDE 'comv-inp.inc'
      INCLUDE 'comv-uni.inc'
      INCLUDE 'comv-phy.inc'

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


C-------------------
C     INITIALIZATION
C-------------------

      VersionStr = 'COMIS version 3.2.1 (2011-06-15)  '//
     &'/Empa/NBI/TNO/LBNL/'
C@empa aw 2001feb05 CopyrightStr added
      CopyrightStr='Copyright (C) 2001 - 2005  Empa'
C@empa aw 2000dec22 There was no initialisation for unit number TIF
C     ! KBD = unit number for the keyboard
C     ! CRT = unit number for (the screen )  for peliminary output file
C     ! CIF = unit number for data input file
C     ! COF = unit number for data output file
C     ! CER = unit number for error output file
C     ! SET = unit number for settings input file
C     ! MSF = unit number for multi schedule file
C     ! IFS = unit number for schedules input file (F:) or (f:)
C     ! COU = unit number for user output file
C     ! TIF = unit number for temperary input file 
C     ! CMF = unit number for meteo file 
C     ! DAF = unit number for temporary file
C     ! TMS = unit number for temporary file
C     ! TSV = unit number for temporary file
C     ! TMM = unit number for temporary file

      KBD = 5
      CRT = 6
      CIF = 10
      COF = 11
      CER = 12
      SET = 13
      MSF = 14
      IFS = 15
      COU = 16
      TIF = 17
      DAF = 20
      TMS = 21
      TSV = 22
      TMM = 23
C     TMM < StartFUnitForCOS <= MaxFUnitNr

      Switch = 0

C     ! COUunf = Flag to tell how to write COU (1 = unformated)
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
C     ! must first "associate" the .COS file extension with Excel.
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
C@empa aw 2001feb05 CopyrightStr added
C@empa aw 2005jun06 The following line may be used to open a child window in a
C@empa aw 2005jun07 QuickWin Application 
CC         OPEN (UNIT= CRT, FILE= 'USER', TITLE= 'CRT_OUTPUT')

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
      OEcho    = 5

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

      OPEN(SET,FILE='COMIS.SET',STATUS='OLD',ERR=12)
C     ! flag saying SET file exists and to use it
      SETFILE = .TRUE.
      GOTO 14
12    CONTINUE
C     ! just a NOTE now to say SET file doesn't exist
      write(CRT,*) 'No SET-file; using defaults'
C     ! and flag saying SET file does NOT exist
      SETFILE = .FALSE.
14    CONTINUE

C     ! Read first 2 parts of COMIS.SET file, if it exists
C     ! Routine setting also tells us whether CRT is used before any
C     ! error message can occur (UseCRT).
      if (SETFILE) CALL setting

C     ! error messages will just go to CER until COF is openned
      IF (UseCRT.EQ.0)  CRT=0

C     ! Read last part of COMIS.SET file, if it exists
      if (SETFILE) then
         REWIND(SET)
         CALL ReadSet('&-COMIS',CIFs,COFs,COUs,COUunf,COSs,
     &      SepStr,OCase,Variab,Clines10)
         CLOSE(SET)
      endif


C     Read command-line options  (e.g. input or output files)
C------------------------------

C@NBI PGS 2000Aug01 - All arguments are in COMMON block already;
C@NBI               - Also add a GOTO to end of COMIS when error
CC    CALL RdCmdLn(CIFs,COFs,COUs,COUunf)
      CALL RdCmdLn
      IF (CerCount(3).GE.1) GOTO 999


C     Open output files  (if specified)
C----------------------

      if (lenstr(COFs).eq.0)  COF = 0
      if (lenstr(COUs).eq.0)  COU = 0
      IF (UseCRT.EQ.0)        CRT = COF

C     ! If there is a user output file specified, no COF is written.
      if (COU .ne. 0) then
         COF=0
C        ! when we have COU and no CRT, the output for CRT goes to COU
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
C@NBI PGS 2000Jul18 - up til now COUS is file rootname (i.e. excluding
C@NBI                 extension): from now on it is full filename
C@NBI               - Also changed file extension from .uof to .cou
C@NBI                 so that all output file extensions start with .co
         COUs=COUs(1:LenStr(COUs))//'.cou'
         if (COUunf .eq. 0) then
            OPEN(COU,FILE=COUs)
C@empa aw 2001feb05 CopyrightStr added
            WRITE(COU,1002)VersionStr(1:lenstr(versionstr)),CopyrightStr
         else
            OPEN(COU,FILE=COUs,FORM='UNFORMATTED')
         endif
      endif

      if (COF .ne. 0) then
C@NBI PGS 2000Jul18 - up til now COFs is file rootname (i.e. excluding
C@NBI                 extension): from now on it is full filename
         COFs=COFs(1:LenStr(COFs))//'.cof'
C        ! write comis output file
         OPEN(COF,FILE=COFs)
C@empa aw 2001feb05 CopyrightStr added
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
C@NBI PGS 2000Jul18 - up til now CIFS is file rootname (i.e. excluding
C@NBI                 extension): from now on it is full filename
      CIFs=CIFs(1:LenStr(CIFs))//'.cif'
      OPEN(CIF,FILE=CIFs,STATUS='OLD',ERR=150)
      GOTO 155
150   CALL ERROR('The specified *.CIF file ('//CIFs(1:LenStr(CIFs))
     &//') does not exist, or is empty.  It was specified in '//
     &'either the COMIS.SET file or on the command-line',1)
      GOTO 900
155   CONTINUE
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
            OPEN(SET,FILE='COMIS.SET',STATUS='OLD')
            REWIND(SET)
            WRITE (COF,'(///A///)') ' HERE STARTS THE COMIS.SET FILE: '
20          CONTINUE
            READ(SET,'(A)',END=30) Line
            write(COF,'(A)') Line
            GOTO 20
30          CONTINUE
            WRITE (COF,'(//////)')
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
C@empa aw 2005jun06
CC     &   Write(*,*) 'Initialization of the TimeLoop'
     &   Write(CRT,*) 'Initialization of the TimeLoop'

C     ! Check to see if H2O is used as pollutant (sets ipolH2O and UseMetH2O)
C     ! ipolH2O is used in TMSmain..ReadDAF  POL-DES
      CALL preH2OPol

C     ! initialize the timeloop
      CALL TMSMAIN(NSteps)

C     ! initialise multischedules only when needed
      if (multifile .ne. ' ')  CALL MULTIMAIN()

C     ! total length of the simulation in seconds
C     ! for use in OnScreen progress
      SiMuLen=(jdstop-jdstart)*86400+(sec2-sec1)

C     ! initialize the graphical output
      CALL IniGrOut

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
      CALL preHis

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

      IF (metfile.NE.' ') THEN
         CALL METINIT
         call wrtBefore()
         CALL DUPLTM(0,0,0,' ',' ',iNstep,iinTime,' ')
      end if

      IF (Switch.EQ.0) THEN
         flname='COMIS.TMS'
      ELSE
         flname='COMIS2.TMS'
      ENDIF
      OPEN(TMS,file=flname,access='direct',recl=30,form='unformatted')

      if (outputoptions(13).ge.2)                           call EchoTMS
      if (outputoptions(13).eq.1.or.outputoptions(13).eq.3) call EchoDAF

C     ! check TMS for a schedule with more than one event at the same time
      Call CheckTMS()

C     ! timeloop initialisation
      tmloop      = 2
      last        =.FALSE.
      FirstStep   =.True.
      stpInit     = stp1Init
      lastpol     = 0
      nextpoltime = 0


C-------------------------------
C     START OF AIRFLOW TIME LOOP
C-------------------------------

100   CONTINUE

C     ! Have to keep the previous Occupant Activity and
C     ! Occupant numbers (might be possible to store them at the histograms)
      if (IsHopt)  call keepprevious

      if (last) goto 800
      IF (test.gt.0 .or. OEcho.GE.2)  WRITE(COF,1000)
C@NBI PGS 2000Aug18 - Differentiate between airflow & pollutant transport time steps
CC1000  FORMAT(/,'=new=timestep',67('='))
1000  FORMAT(//,'=new=airflow=timestep',59('='))

      CALL TIMESTEP(tmloop,TLotus,chgkey,last)
C     ! Get time of next ventilation event
      if (.not. last) READ(TMS,REC=tmloop+1) NextEvent
      NextEvent=min(NextEvent,CompDay,MultiSchedTime)
C     ! Get the current time in seconds since start
      CurTime = NumCreate(TLotus)

C     ! See if we should switch to Polstep2 yet
      if (pol1time.gt.0 .and. (Curtime.ge.pol1time)) Polstep=Polstep2
C     ! First "extra" pollutant calculation is polstep seconds after FIRST
C     ! vent event
      if (Polstep .gt. 0)  interval = min(interval, Polstep)

      tmstep=tmstep+1

      if(Outputoptions(11).gt.0.and.FirstStep) call EchoOnScreen(TLotus)

C     ! At the moment chgkey is not used actually. For every schedule type the
C     ! variable is set to 1, so that the ventilation part is always performed.
C     ! Calculation of ventilation also when no schedules.
      chgkey=1
C     ! decide if we have to run the ventilation part again
      IF (chgkey.EQ.0) GOTO 200

C     ! Echo the input data for ventilation
      IF(COF.NE.0)  CALL echodat(CIFs)

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

      if (outputoptions(2).gt.0) then
         if (outputoptions(12).eq.1) then
C@NBI PGS 2000Aug02 - "TLotus" no longer used by routine POLLUTANT as removed
CC          CALL Pollutant(TLotus,0)
            CALL Pollutant(0)
         else
CC          CALL Pollutant(TLotus,interval)
            CALL Pollutant(interval)
         end if
         lastpol = CurTime
         nextpoltime = nextpoltime + interval
      endif


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
         if ((outputoptions(2).gt.0).AND.(OEcho.GE.3))
     &      CALL PolOut(COF,last,DTstring)
      endif

C     ! Optional user output files (either to FORMATTED or UNFORMATTED file)
      if (COU .ne. 0) then
         if (COUunf .eq. 0) then
            CALL UserOutF(COU,TLotus,DTstring)
         else
            CALL UserOutU(COU,TLotus,DTstring)
         endif
      endif

C     ! write results for graphical output (.COS files)
      CALL WriGrOut(TLotus)

C     ! Real-time update on-screen of solution progress
      if (Outputoptions(11).gt.0)  call EchoOnScreen(TLotus)


C----------------------------------------------
C     START OF POLLUTANT TRANSPORT TIME SUBLOOP
C----------------------------------------------

      if ((Polstep .ne. 0) .and. (outputoptions(2) .gt. 0) .and.
     &   (.not. last) .and. (nextpoltime .lt. NextEvent)) then

300      CONTINUE

C        ! Loop here if there are more pollutant calcs to be done even if
C        ! no ventilation events

C        ! If the user has specified a pollutant report interval, call the
C        ! pollutant subroutine at that interval in addition to the call that
C        ! happens after each ventilation event (TimeStep).  This was added so
C        ! that in the cases where the user wants pollutant calculations
C        ! without the extra cost of ventilation calculations (when the
C        ! ventilation isn't changing).



C        ! Get the current time in seconds since start
         CurTime = NumCreate(TLotus)

C        ! See if we should switch to Polstep2 yet
         if (pol1time .gt. 0 .and. (Curtime+Polstep .ge. pol1time)) then
            Polstep  = Polstep2
            interval = Polstep
         endif

         interval = min(interval, NextEvent - nextpoltime)
         TLotus=CreateTM(nextpoltime)
         CALL CreateJdTim(numcreate(TLotus),Jday1,NSecDay)
         CALL CONVDAT1(Jday1,NSecDay,DTstring)

C@NBI PGS 2000Aug18 - Standardize the COF divider for airflow and pollutant transport time steps
CC         write(COF,*) DTstring,' lastpol=',lastpol,
CC     &      ' nxtpol=',nextpoltime,
CC     &      ' interval=',interval
C@NBI PGS 2000Aug18 - Need a divider between pollutant transport time steps
C@NBI PGS 2003Mar16 - Modified OEcho switch for this header
         IF (test.gt.0 .or. OEcho.ge.5)THEN  
           WRITE(COF,1003)
1003       FORMAT(/,  '=new=pollutant-transport=timestep',47('='))
C@empa aw 2005apr28 call HeadOut to output time and interval
CC         WRITE(COF,*) 'Date/time '//DTstring(1:lenstr(DTstring))//
CC     &   '  (Interval',interval,'sec;',lastpol,'->',nextpoltime,'sec)'
           CALL HeadOut(COF,DtString)
         ENDIF
C        ! do a pol calc
         if (outputoptions(12).eq.1) then
C@NBI PGS 2000Aug02 - "TLotus" no longer used by routine POLLUTANT as removed
CC          CALL Pollutant(TLotus,0)
            CALL Pollutant(0)
         else
CC          CALL Pollutant(TLotus,interval)
            CALL Pollutant(interval)
         endif
C        ! print it
C@NBI PGS 2003Mar16 - Bugfix: Pollutant results were being printed even when OEcho=0
C@NBI                 so added check for OEcho here. OEcho >= 5 for these intermediate results.
CC       CALL PolOut(COF,last,DTstring)
         IF(OEcho.GE.5) CALL PolOut(COF,last,DTstring)
C        ! and write the graphic output
         CALL WriGrOut(TLotus)

         lastpol = nextpoltime
         interval = Polstep
         if ((nextpoltime .lt. NextEvent) .and.
     &      (nextpoltime + Polstep .gt. NextEvent)) then
            nextpoltime = NextEvent
            IF(secho.GT.1) write(COF,*) 'Shortening to NextEvent at '
     &      ,NextEvent
         else
            nextpoltime = nextpoltime + Polstep
         endif
         IF(secho.GT.1) write(COF,*) 'End of Pol loop, lastpol=',lastpol
     &      ,' nextpoltime=',nextpoltime


C        Next pollutant transport sub-timestep?
C----------------------------------------------

C        ! Loop if the next pollutant report time is before the next ventilation event
         if (nextpoltime .lt. NextEvent ) goto 300
      endif


C     Next ventilation timestep?
C-------------------------------

C     ! increment time loop counter
      tmloop=tmloop+1
C     ! Not the first step anymore
      if( firstStep ) then
         firstStep = .false.
         stpInit = stp2Init
      endif
      goto 100


C------------------------
C     TIME LOOP ENDS HERE
C------------------------

800   CONTINUE

      IF (test.gt.0 .or. OEcho.ge.2)  WRITE(COF,1001)
1001  FORMAT(/,'=end=of=simulation',62('='))

C     ! MeanOut routine for T-Options output
      if (IsTOpt)  call MeanOut(COF)

C     ! Close *.COS files before the call off HistOut,
C     ! then we can use the same unit numbers
      CALL ClGrOut

C     ! HistOut routine for H-Options output
      if (IsHOpt .and. HistActive) call HistOut

C     ! Jumps here when program aborts due to error
900   CONTINUE
      call message(Tmstep)

C     ! Close files
      if (COF .ne. 0) CLOSE(COF)
      if (COU .ne. 0) CLOSE(COU)
      CLOSE(DAF,STATUS='DELETE')
      CALL CloseTMS
      CALL ClGrOut
999   CONTINUE

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
C@NBI PGS 2001Apr15 - Changed keyword "progress" to also output ETA and
C@NBI                 estimated remaining time left.  Uses new subroutine
C@NBI                 "RunTime"
C@NBI               - Also changed order in which things are displayed in line.
C@NBI               - Also padded "ADVANCE='NO'" line with 2 spaces so that
C@NBI                 you don't get hanging remains when a shorter line is printed.
Ch**********************************************************************
 ! USE statements to include routine interfaces
      use dflib
      use dfwin
      IMPLICIT NONE
 ! Data declarations
      integer nlines, ncols
      integer fhandle,ypos
      logical lstatx,curerr
      Type(T_COORD) wpos
	Type(T_SMALL_RECT) sr
      Type(T_CONSOLE_SCREEN_BUFFER_INFO) cinfo



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

C     Progress
C-------------
      IF (ICINDEX(OnScreen,'progress').gt.0) then
         if(SiMULen.gt.0) then
            progress=REAL(number)/REAL(Simulen)
         else
            progress=1.
         end if
         CALL RunTime(progress,DTstring)
         Line=DTstring
      end if

C     Time
C---------
      IF (ICINDEX(OnScreen,'time').gt.0) THEN
         CALL CreateJdTim(numcreate(TLotus),Jday1,NSecDay)
         CALL CONVDAT1(Jday1,NSecDay,DTstring)
         Line=line(1:lenstr(line))//DTstring
      ENDIF

CCC     Progress
CCC-------------
CC      IF (ICINDEX(OnScreen,'progress').gt.0) then
CC         if(SiMULen.gt.0) then
CC            progress=REAL(number)/REAL(Simulen)*100
CC         else
CC            progress=100
CC         end if
CC         str=' '
CC         i=int(progress)
CC         call intdis(I,str,Lstr)
CC         str=str(1:Lstr)//'%'
CC         call Longn(str,lstr,5)
CC         Line=line(1:lenstr(line))//str(1:Lstr)
CC      end if

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
C$    write(*,'(1X,A)',ADVANCE='NO')line(1:lenstr(line))//'  '//char(13) !Fortran 90
C$    write(*,*) line(1:lenstr(line))                                    !Fortran 77
C@empa aw 2005jun07 Different routines for compiler and platform specific I/O.
C@empa aw 2005jun07 (ADVANCED='NO' does not work with CVF! CVF does not write the
C@empa aw 2005jun07 non-advancing output to the screen before the end of a record, 
C@empa aw 2005jun07 that means, only the next advancing output will write the whole record 
C@empa aw 2005jun07 to the screen. As this only will happen at the end of the simulation, this is useless) 
      call WriteProgressCVF_CA(line) ! for CVF console application (File COMV-CCA.FOR)
C$      call WriteProgressCVF_QW(line) !for CVF QuickWin application (File: COMV-CQW.FOR)
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
         str='There is one message, marked as ***COMIS ERROR*** '//
     &   'in your CER & COF files.'
      else if (imess.gt.1) then
         str='There are '//str(1:lstr)//' messages, '//
     &   'marked ***COMIS ERROR*** in your CER & COF files.'
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


C@NBI 2001Apr15 - New subroutine
C-----------------------------------------------------------------------
      SUBROUTINE RunTime(fractionDone,ETA)
C
C FUNCTION:
C   - This generic utility calculates estimated time at end of simulation
C     and the remaining run time, and outputs a CHARACTER*30 string ETA.
C   - The first call initializes this subroutine, and should be done
C     before start of simulation loop.
C NOTE:
C   - This routine assumes the Julian calendar when counting years.
C   - This routine is compiler-specific; just decomment relevant $ lines
C     or you may have have to add the specific code for your compiler.
C   - (c) 2001 : P.G.Schild, Norwegian Building Research Institute
C.......................................................................

      IMPLICIT NONE
C    .Passed arguments:
      REAL fractionDone
C     (ETA declaration can actually be 30 characters, but an empty space
C     was added to end here so that it ca be used in COMIS using existing
C     declaration for "DTstring")
      CHARACTER*31 ETA
C    .Local variables:
      INTEGER startTime,nowTime,restTime,endTime,mday(0:12),i,yy,mon,dd
     &,hh,mm,ss,percentLeft,values(8)
      LOGICAL initialize

C$   .Just de-comment the appropriate line for your compiler
c     - Declarations for standard FORTRAN 90  (DATE_AND_TIME)
      character*8 f90date
      character*10 f90time
      character*5 f90zone
C$c   - Declarations for DEC f77 compiler
C$    INTEGER time,tarray(9)
C$    external LTIME,TIME
C$c   - Declarations for MIPS f77 compiler extension (Silicon Graphics)
C$    INTEGER*2 mipsy,mipsm,mipsd
C$c   (end of compiler-specific code patch)

      DATA mday /0,0,31,59,90,120,151,181,212,243,273,304,334/
      DATA initialize /.TRUE./
      SAVE startTime,initialize
C-----
C$   .Just de-comment the appropriate line for your compiler
c     - Standard FORTRAN 90  (DATE_AND_TIME)
      CALL date_and_time(f90date,f90time,f90zone,values)
C$c   - DEC f77 compiler
C$    CALL LTIME(TIME(),tarray)
C$    values(1)=tarray(6)
C$    values(2)=tarray(5)+1
C$    values(3)=tarray(4)
C$    values(5)=tarray(3)
C$    values(6)=tarray(2)
C$    values(7)=tarray(1)
C$c   (end of compiler-specific code patch)

      nowTime=NINT(REAL(values(8))*0.001)+values(7)+values(6)*60
     &+values(5)*3600+(values(3)+mday(values(2)))*86400
      IF(initialize.OR.(fractionDone.LE.0.0))THEN
         startTime=nowTime
         initialize=.FALSE.
         WRITE(ETA,1002)
      ELSE
         restTime=NINT(DBLE(nowTime-startTime)
     &   *(1.0-fractionDone)/fractionDone)
         endTime=nowTime+restTime
         yy=values(1)+endTime/31536000
         endTime=MOD(endTime,31536000)
         DO mon=12,1,-1
            IF((mday(mon)*86400.LT.endTime).OR.(mon.EQ.1)) GOTO 1001
         END DO
1001     dd=endTime/86400-mday(mon)
         hh=MOD(endTime,86400)/3600
         mm=MOD(endTime,3600)/60
         ss=MOD(endTime,60)
         percentLeft=NINT((1.0-fractionDone)*100.0)
         IF(restTime.GT.86400)THEN
            WRITE(ETA,1003) yy+values(1),mon,dd
     &      ,REAL(restTime)/86400.0,percentLeft
            DO i=5,14
               IF(ETA(i:i).EQ.' ') ETA(i:i)='0'
            ENDDO
         ELSE
            IF(restTime.GT.3600)THEN
               WRITE(ETA,1004) hh,mm
     &         ,NINT(REAL(restTime)/3600.0),'hrs',percentLeft
            ELSEIF(restTime.GT.60)THEN
               WRITE(ETA,1004) hh,mm
     &         ,NINT(REAL(restTime)/60.0),'min',percentLeft
            ELSE
               WRITE(ETA,1004) hh,mm
     &         ,restTime,'sec',percentLeft
            ENDIF
            DO i=5,9
               IF(ETA(i:i).EQ.' ') ETA(i:i)='0'
            ENDDO
         ENDIF
      ENDIF
1002  FORMAT('End time not known (100% left)')
1003  FORMAT('End ',I4,'-',I2,'-',I2,' (',F5.1,' d. ',I3,'%)')
1004  FORMAT('End ',I2,':',I2,' (',I2,1X,A3,':',I3,'% left)')
      END
