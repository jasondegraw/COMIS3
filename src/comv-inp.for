C+*********************************************************** comv-inp.f
C@tno jcp 1996Mar28_12:33:51 this is the first part of the former comv-inp.f
C@lbl bvs June1994
C There have been many changes to this file, comv-inh and comv-in2 for the
C new CIF input reading as of version 1.3
C@NBI PGS 2000Aug13 - Tidied up & detabbed whole module - no syntax change
C
Ch**********************************************************************
      SUBROUTINE inpdat(key,line, k, LinTyp)
C                       |   |     |
C                       |   |     first non-blank in string
C                       |   string Line with data
C                       keyword number to be processed
C***********************************************************************
C inpdat= input data
C
C Pass parameters:
C
C IO # Name    description
C I     key    keyword to be processed
C I/O   line   current line/next line after keyword is processed
C O     k      first non-blank position in next line
C
C inpdat gets a line read from CIF by inh, together with the sequence number
C of the keyword of this data section +the sequencenumber of the dataline under
C the keyword, and puts the data in the parameters. The parameters are in the
C commonblocks.
C For the NET-AIR datasection, all coefficients that will be used by comven
C are read in LDat. The number of elements used for every component type is
C fixed. This means that sometimes we have some unnecessary zeros in this array
C LDat.
C The first element is always the sequence number of the AFCname (CR=1,FA=2,DS=3
C DF=4,F1,..,4=5,..,8,WI=9,TD=10 maybe more will come)
C This number will be used in FEQN to select the proper formula for this AFC
C Then a number of elements follows: 4 for a CR , 16 for a FA,....
C Last, 5 values for the filter effect of this link are stored.
C Below you see a list of Keywords and the number of elements used in LDat
C The WI and TD (TestData) AFC has a non-fixed number of elements.
C After the TD number(10) follows the number of data pairs that are internally
C counted by inpdat, followed by all other data given in CIF
C
C hcp sept 1989
C Changes:
C@EMPA AW/oct. 1990  Variablename LineNr corrected.
C@TNO JCP 1990dec27 INTEGER Eflag declared to avoid typemismatch
C above all GETWR and GETWI andso added 2lines
C added INTEGER FlgErr
C FlgErr set to 0 here
C Keyword explanation deleted, see MAIN
C A variable has to be made in the commonblock
C for this filename mentioned in the CIF data file itself
C Changed to dry air as default
C@lbl bs 1991jul02  Parameter LineNr added
C@tno jcp 1996Jan17_12:03:16 unfortunately somewhere this LineNr has been delete
C this causes a crash in routines F3 and F4 if there are less data pairs for
C a curve than the number of coefficients stated before.
C@empa vd 1992jan22  Routine CS: New parameters and cosmetic changes
C@empa aw 1993may26 COMMON block NCPN canceled and keep the values
C@empa              of NCp1 and NCp2 in the routine IN33 with SAVE
C@empa aw 1993jun03 FlgErr canceled, I use the COMMON parameter FInpErr
C@tno jcp 1996May30_17:28:42 read through the header in InpDat
C@tno jcp 1996Jun05_16:11:48 added 56 to call inafc-PS
C@tno jcp 1996Jun05_16:12:56 PS added
C@tno jcp 1996Jun10_14:26:04 allow an F: (f:) schedule line to have *name (So:wi
C@tno jcp 1996Jun27_16:04:47 allow F: and f: to signal a file
C@tno jcp 1996Jun10_14:22:46 patch F: or f: and name starts with *
C@tno jcp 1996Jun14_14:44:08 added 57 as a call to EQN-WIN
C@tno jcp 1996Jun14_14:45:31 call EQN-WIN
C@tno jcp 1996Jul16_18:25:20 SCH-POL 38 is a schedule with a name and must have
C@tno jcp 1996Jun27_16:04:47 allow F: and f: to signal a file
C@lbl bvs 1999Mar11 increased length of name from 12 to 160 (it may be used
C                   for schedule filename)
C@NBI PGS 2000dec23 - Added INTEGER i
Ch**********************************************************************

      IMPLICIT NONE
      INCLUDE 'comv-inp.inc'
      INTEGER LenStr
      CHARACTER*(*) Line
      character*160 name,namword
      INTEGER key,k,L,LinTyp,i
C-----
C     ! Ptr L always points at the next free element in LDat (The array
C     ! in which all AFC coefficients are.
C     ! LayPtr points to the next free element in LayDat

C     ! Get first line with data or name

10    Call readlin(line, LinTyp, K, .False.)
      if (FlgEnd) goto 99
         if (LinTyp .eq. TKEY) goto 99
         if (LinTyp .eq. THead) goto 10
         L = lenstr(line)

C             1  2  3  4  5  6  7  8  9  0
         GOTO(01,02,03,04,99,06,06,06,06,06,
     &        06,06,06,06,06,16,17,18,19,20,
     &        21,22,23,88,88,88,88,88,88,88,
     &        88,32,33,34,35,36,37,38,39,99,
     &        99,99,99,44,45,46,47,48,49,50,
C@empa aw 2000feb01 added 58, call to inPOLFIC
CC   &        51,52, 6, 6,55,6) key
C@empa aw 2001may28 added 59 , call to inTRNIPN
C@empa aw 2002aug12 canceled again
C@NBI PGS 2003Apr29 - New component type "TV": Thermostatic vent
CC   &        51,52, 6, 6,55,6,57,58) key
     &        51,52, 6, 6,55,6,57,58,6) key

C     ! illegal key, just return
      goto 99

C     &-CIFs name
C     -----
C     ! 1993Jun9 CIF keyword no longer used (but is read and ignored)
1     call inCIF(Line, LinTyp, L, K)
      GOTO 99

C     &-PR-IDEN ProblemName and VersionName
C     ---------
2     call inVERS(Line, LinTyp, L, K)
      GOTO 99

C     &-PR-OUTP or &-PR-SIMU
C     ----------------------
3     continue
48    continue
      call inSIMU(Line, LinTyp, L, K)
      GOTO 99

C     &-PR-CONTrol parameters
C     ---------
4     call inCTRL(Line, LinTyp, L, K)
      GOTO 99
C@NBI PGS 2003Apr29 - New 14th component type "TV": Thermostatic vent
C                           1  2  3  4  5  6  7  8  9  10 11 12 13 14
C     &-NET-AIR components (CR,FA,DS,DF,F1,F2,F3,F4,WI,TD,PE,RF,PS,TV)
C     ---------
6     call inAFC(Line, LinTyp, L, K)
      GOTO 99

C     &-TRANSITions: relam, returb
C     ---------
16    call inTRANS(Line, LinTyp, L, K)
      GOTO 99

C     &-NET-HVAc
C     ---------
17    call inHVAC(Line, LinTyp, L, K)
      GOTO 99

C     &-NET-ZONes
C     ---------
18    call inZONES(Line, LinTyp, L, K)
      GOTO 99

C     &-NET-ZL (zone layers)
C     --------
19    call inZL(Line, LinTyp, L, K)
      GOTO 99

C     &-NET-ZP (zone pollutants)
C     --------
20    call inZP(Line, LinTyp, L, K)
      GOTO 99

C     &-NET-EXTernal nodes
C     ---------
21    call inEXT(Line, LinTyp, L, K)
      GOTO 99

C     &-NET-LINks
C     ---------
22    call inLINK(Line, LinTyp, L, K)
      GOTO 99

C     &-SCH-MAIn
C     ---------
23    call inerr('&-SCH-MAIn: Not available yet ! ',
     &'Delete this section in your input file !',.FALSE.,2)
      GOTO 99

C     &-CP-BUILding
C     ---------
32    call inCPBUI(Line, LinTyp, L, K)
      GOTO 99

C     &-CP-VALUes
C     ---------
33    call inCPVAL(line, LinTyp, L, K)
      GOTO 99

C     &-ENV-BUIlding (environment)
C     ---------
34    call inENVBUI(Line, LinTyp, L, K)
      GOTO 99

C     &-ENV-WINd
C     ---------
35    call inENVWIN(Line, LinTyp, L, K)
      GOTO 99

C     &-SCH-MET meteo schedule
C     ---------
C@NBI PGS 2000dec23 - Write schedule keyword in DAF.  This code has been
C@NBI                 moved here from subroutine INH.
36    pKeyRec(key)=pDAF
      buf((MOD(pDAF-1,mBuf)+1))='Schedule SCH-MET'
      IF (MOD(pDAF,mBuf).EQ.0) CALL wDAF(pDAF,mbuf,buf)
      pDAF=pDAF+1
C@NBI PGS 2000dec23   (end of moved code)      
      call inSCHMET(Line, LinTyp, L, K)
      GOTO 99

C     &-POL-DES pollutant description
C     ---------
37    call inPOLDES(Line, LinTyp, L, K)
      GOTO 99

C     &-SCH-POL outdoor pollutant schedule
C     ---------
C@NBI PGS 2000dec23 - Write schedule keyword in DAF.  This code has been
C@NBI                 moved here from subroutine INH.
38    pKeyRec(key)=pDAF
      buf((MOD(pDAF-1,mBuf)+1))='Schedule SCH-POL'
      IF (MOD(pDAF,mBuf).EQ.0) CALL wDAF(pDAF,mbuf,buf)
      pDAF=pDAF+1
C@NBI PGS 2000dec23   (end of moved code)      
      call inSCHPOL(Line, LinTyp, L, K)
      GOTO 99

C     &-OCCUPANt description
C     ---------
39    call inOCCU(Line, LinTyp, L, K)
      GOTO 99

C     &-NORM-CR normalized crack temperature
C     ---------
44    call inNORMCR(Line, LinTyp, L, K)
      GOTO 99

C     &-SCH-MUL (multi-schedules)
C     ---------
45    call inSCHMUL(Line, LinTyp, L, K)
      GOTO 99

C     &-USERdat (User input data)
C     ------
46    call inUSER(Line, LinTyp, L, K)
      GOTO 99

C     &-NET-ZT (zone thermal properties)
C     --------
47    call inZT(Line, LinTyp, L, K)
      GOTO 99

C     &-PR-UNITs
C     ---------
49    call inUnits(Line, LinTyp, L, K)
      GOTO 99

C     &-WALL-MAterials (Data of Materials to be used in Wall Layers)
C     ---------
50    call inWaMa(Line, LinTyp, L, K)
      GOTO 99

C     &-WALL-TYpes (Types of Wall Laters composed of WALL-MA and
C     ---------     to be used in NET-WALls)
51    call inWaTy(Line, LinTyp, L, K)
      GOTO 99

C     &-NET-WALls (Linking of wall layers with other wall layers, zones and outside )
C     ---------
52    call inNeWa(Line, LinTyp, L, K)
      GOTO 99

C     &-HISTOgram definition of number of classes and upper and lower classes
C     -------
55    call inHISTO(Line, LinTyp, L, K)
      GOTO 99

C     &-EQN-WIN
C     ---------
C     ! read coefficients of the window schedule equation (function of meteo)
57    call inEqnWin(Line, LinTyp, L, K)
      GOTO 99

C     &-POL-FIC
C     ---------
C@empa aw 2000feb01 call to inPOLFIC
58    call inPOLFIC(line,LinTyp,L,K)
      GOTO 99

C@empa aw 2001may28 added 59 , call to inTInp
C@empa aw 2002aug12 canceled
CC59    call inTRNIPN(line,LinTyp,L,K)
      GOTO 99

C     All other schedules
C     -------------------
C     ! All schedules except SCH-MET, SCH-POL and SCH-MUL
C     ! Write into the DAF file for this schedule
88    CONTINUE
C@NBI PGS 2000dec23 - Write schedule keyword in DAF.  This code has been
C@NBI                 moved here from subroutine INH.
      pKeyRec(key)=pDAF
      i=key*10
      buf((MOD(pDAF-1,mBuf)+1))='Schedule '//keys(i-9:i)
      IF (MOD(pDAF,mBuf).EQ.0) CALL wDAF(pDAF,mbuf,buf)
      pDAF=pDAF+1
C@NBI PGS 2000dec23   (end of moved code)      
C@empa aw 2001jan12 Writing keyword shouldn't be in the loop, otherwise we 
C@empa              write it before each data line. Start of the loop moved here
89    CONTINUE
C     ! Get the schedule name and put in name list
C     ! Either a file (F:)(f:) or * type
      if (line(k+1:k+1).eq.'*' .or. line(k+1:k+2).eq.'F:'
     &.or. line(k+1:k+2).eq.'f:') then
         IF (line(k+1:k+2).EQ.'F:'.or.line(k+1:k+2).EQ.'f:') THEN
            k = k+2
            variab='get F: schedule name in inpDat'
            CALL GETWRD(Line,k,12,name)
            if (name(1:1).ne.'*') then
               namword(1:1)='*'
               namword(2:) = name
               name=namword
            end if
         else
            variab='get schedule name in inpDat'
            CALL GETWRD(line,K,l,name)
C@empa aw 2000jan20 check first character of the name of the schedule
            call CheSchNam(name,key,line)
         endif
         nName = nName+1
         aName(pname:) = name//' '
         pname = pname+lenstr(name)+1
      endif
C     ! Write this entry into the DAF file
      buf((MOD(pDAF-1,mBuf)+1))=LINE
      IF (MOD(pDAF,mBuf).EQ.0) CALL wDAF(pDAF,mbuf,buf)
      pDAF=pDAF+1
      Call readlin(line, LinTyp, K, .True.)
      if (FlgEnd) goto 99
C     ! Loop for next data/name line
      if (LinTyp .ne. TDATA .and. LinTyp .ne. TNAME) goto 99
C@empa aw 2001jan12 Start of the loop changed
CC      goto 88
      goto 89
      
99    continue

C@tno jcp 1996Jun11_14:59:29 reset FlgEnd
c     FlgEnd=.False.
      RETURN
      END

Ch**********************************************************************
      SUBROUTINE inCIF(Line, LinTyp, L, K)
C CIFs name
C 1993Jun9 CIF keyword no longer used (but is read and ignored)
Ch**********************************************************************

      IMPLICIT NONE
      INCLUDE 'comv-inp.inc'
      CHARACTER*(*) line
      INTEGER k,L,LinTyp
C-----
      Variab='CIFs name (not used anymore)'
C     ! read next line for next keyword
      Call readlin(line, LinTyp, K, .True.)
      RETURN
      END


Ch**********************************************************************
      SUBROUTINE inVERS(Line, LinTyp, L, K)
C ProblemName and VersionName
C Changes:
C@tno jcp 1996Jul05_08:18:20 skip too long ProName input (800 char max)
C@tno jcp 1996Jul05_08:31:08 added variab
C@NBI PGS 1999May06 - INCLUDE 'comv-uni.inc' to make Oecho & Test available
C@NBI PGS 1999May06 - Write out Proname & Vername at top of .CIF
Ch**********************************************************************

      IMPLICIT NONE
      INCLUDE 'comv-inp.inc'
      INCLUDE 'comv-uni.inc'
      CHARACTER*(*) line
      INTEGER k,L,LinTyp,N
      INTEGER LenStr

C-----------------------------
C     Read problem description
C-----------------------------

C     ! Note that K (the position of the first non-blank in the line) is passed
C     ! to us, so we can remove leading blanks from the first line of the problem name
      N = 0
      Variab='Problem description'
C     ! Loop here for each line of problem name/description
      N = 1
10    continue
      If((N+L-k).gt.800) then
         call inerr('&-PR-IDENtification: more than 10*80 '//
     &   'characters problem description. Excess characters skipped.',
     &   'Make this section smaller.',.true.,1)
         GOTO 99
      end if
C     ! put the problem name in proname()
C     ! readlin returns a k one before the word (so GetWrd can read it)
C     ! so begin at k+1
      Proname(N:) = Line(k+1:L)//';'
      N=N+L-k+1
C     ! Get the next line
      Call readlin(line, LinTyp, K, .False.)
      if (FlgEnd) goto 999
      l=lenstr(line)
C     ! if the next line is a keyword then stop here
      if (LinTyp .eq. TKEY) goto 999
C     ! if the next line not is a header then loop back for the next problem line
      if (LinTyp .ne. THEAD) goto 10
      Proname(N:) = ';'

C----------------
C     CIF version
C----------------

C     ! Now put the version name in vername
      Variab='CIF-Version ID'
      Call readlin(line, LinTyp, K, .True.)
      if (FlgEnd) goto 999
      l=lenstr(line)
C     ! Make sure there is a version supplied
      if (LinTyp .ne. TDATA) goto 999
      Vername = Line(k+1:)
C@NBI PGS 2000Aug13 - redundant line... commented out
CC    goto 99

C-----------------------
C     End of datasection
C-----------------------

99    continue
C     ! read next line for next keyword
      Call readlin(line, LinTyp, K, .True.)
C@tno jcp 1996Jul05_08:24:59 loop here too (in case of too long ProName)
      if (FlgEnd) goto 999
      if (LinTyp .ne. TKEY) goto 99
C@tno jcp 1996Jul05_08:24:59 end
999   continue
C     ! remove the last semi-colon from ProName
      proname(N-1:N-1)=' '

C-------------------------------------------------
C     Echo Filename, Version & Problem description
C-------------------------------------------------
C@empa aw 2001mar22 check also COF.NE.0
      IF ((test.GT.0 .OR. OEcho.GT.0).AND.(COF.NE.0)) THEN
C@empa aw 2000jan28 - Write CIF name
C@NBI PGS 2000Jul21 - ... align it
         WRITE(COF,*)'INPUT FILENAME:   ',CIFs(1:LenStr(CIFs))
         IF (vername.NE.' ') WRITE(COF,*)
     &      'CIF FILE VERSION: ',Vername(1:lenstr(Vername))
         IF (ProName.NE.' ') then
            WRITE (COF,'(A)') ' MODEL NAME:'
            CALL wrt80(COF,proname,wcrt)
         ENDIF
         WRITE(COF,*)
      ENDIF
C@tno jcp 1996Jun11_15:00:09 reset flgend
c     FlgEnd=.False.
      RETURN
      END


Ch**********************************************************************
      SUBROUTINE inSIMU(Line, LinTyp, L, K)
C***********************************************************************
C PR-SIMUlation options
C
C VENT     ventlation results go into COMIS.COF
C 2VENT    ventlation results go into COMIS.COF (different format)
C CHECKC   Check for condensation
C CONC     no effect (CHECKC is now in its place)
C POLSTEP  time step (in seconds) to do pollutant calculations in addition to
C          regularly scheduled ventilation/pollutant calcs
C POL [n]  pollutant routines are called.
C          Option [n] means take the first n pollutants into the simulation
C          (more couldbe defined in &-POL-DES, but only n will be active)
C HEAT     no effect
C INPUT    CIF copied to COF
C DEFAULT  used defaults reported in *.COF (lack of data and d[e[f[a[u[l[t]]]]]]
C DEF      ?? not found
C HEAD     ?? not found
C SET      *.SET copied to *.COF
C UNIT     implemented units/conversions are summarized in *.COF
C KEYWORD  implemented keywords          are summarized in *.COF
C EchoSCHedule echo all changes due to schedules in COF
C OnScreen Time Progress Niter TotFlow Error
C STEADY   Steady state concentrations for all time steps
C DEBUG    ECHO 1=DAF, 2=TMS, 3=DAF+TMS
C LOOPRHO  recalculate Rho zone during the network pressure iteration loop
C
C NOTE: STARTtime and STOPtime are parsed later in RDPERIOD
C
C Changes:
C@lbl bvs 24Apr1995 - comment about new/old PR-OUTP with new PR-SIMU
C ignore any unrecognized options - for compatibility with older CIF files,
C PR-OUTP section can use the PR-SIMU keywords plus the PR-OUTP options
C@lbl/end
C@tno jcp 1996Mar14_11:49:04 new parts for PR-SIMU outputoptions
C@tno jcp 1996Mar14_12:28:00 ExStr for use in Explain Keywords
C@tno jcp 1996Mar14_11:50:27 new part to summarize all defined keywords
C@tno jcp 1996Apr15_11:44:39 POL n does not work now
C@tno jcp 1996Apr30_10:32:30 steady state concentrations only added
C@tno jcp 1996Apr30_10:32:30 debug Echoes DAF TMS files
C@tno jcp 1996May24_18:02:21 added for level of echoed DEFAULTS
C@tno jcp 1996May31_14:01:37 added LOOPRHO to set UseOpz=2 to recalculate RHO
C@tno jcp 1996May31_14:02:14 UseOpz can also be entered in &-PR-CONT
C@tno jcp 1996Jun14_00:14:41 variab for GetWI
C@tno jcp 1996Jul09_16:00:28 there may follow an error message that says which
C@tno jcp 1996Jul10_15:57:16 STR to see that these output options are OK
C variable is being read
C@tno jcp 1996Oct24 forgotten: assign l as the length of the unitstr(i)
C@tno jcp 1996Oct24 next line can now test l
C@lbl bvs 1999Jun24 added keyword POLSTEP for pollutant loop
C@NBI PGS 1999Aug11 - The defunct option 'CONCentration' has now been hijacked
C@NBI                 for option 'CHECKCondensation' that warns for condensation
C@NBI                 Is this alright?
C@lbl dml 1999nov19 Rename variable Newton in COMMON block /PRCONT/
C   to nNewt.
C@lbl dml 1999nov22 Replace solver control parameters noInit and useOpz
C   with stp1Init, stp2Init, and rhoSync.
C@empa aw 1999dec08 Output Units only at  the first call of inSIMU
Ch**********************************************************************

      IMPLICIT NONE
      INCLUDE 'comv-inp.inc'
      INCLUDE 'comv-uni.inc'
      INTEGER LenStr,IcIndex
      CHARACTER*(*)line
      character*40 dum,dum2
      character*20 word
      character*180 line2,STR
      character*65 ExStr
      INTEGER i,j,k,L,LinTyp,Length
C     ! counter added for temperature offset printing
      INTEGER numtemps
C-----
C@empa aw 2000jan31 O1..O5 and Y1..Y5 added
C@empa aw 2000feb01 F1..F5 added
C@empa aw 2002dec04 PL (pressure difference per link) added
C@empa aw 2005apr26 cancel EF, it was replaced by the keywords Fn
      str='PZ-TZ-HZ-FZ-FL-TL-VL-WA-TA-HA-C1-C2-C3-C4-C5-'
      STR=STR(1:LENSTR(STR))//'Q1-Q2-Q3-Q4-Q5-S1-S2-S3-S4-S5-'
      STR=STR(1:LENSTR(STR))//'PE-IZ-AZ-MZ-EZ-FB-IB-AB-MB-RB-'
CC      STR=STR(1:LENSTR(STR))//'NB-EB-LB-FW-EF-O1-O2-O3-O4-O5-'
      STR=STR(1:LENSTR(STR))//'NB-EB-LB-FW----O1-O2-O3-O4-O5-'
      STR=STR(1:LENSTR(STR))//'Y1-Y2-Y3-Y4-Y5-F1-F2-F3-F4-F5-'
C@empa aw 2005aug29 DL (Cd- value per link) added
      STR=STR(1:LENSTR(STR))//'PL-DL-'


C-----------------------------------------
C     Loop here reading each output option
C-----------------------------------------

10    continue
      k=0
      variab='interpreting Simu/Output options'
      CALL GETWRD(line,K,l,word)
      CALL UPPERC(word)
      if (word(1:4).eq.'VENT') then
         outputoptions(1) = 1
      else if (word(1:5).eq.'2VENT') then
         outputoptions(1) = 2
      else if (word(1:7).eq.'POLSTEP') then
C        ! polstep1 is timestep (seconds) for pol calcs *until* pol1time
C        ! then polstep2 is used pollutant calc interval for remainder of simulation
         Variab='Pollutant calculation timestep'
         CALL GETWI(line,K,l,polstep1,0)
         CALL GETWI(line,K,l,pol1time,0)
         CALL GETWI(line,K,l,polstep2,0)
C        ! Start with first polstep
         polstep = polstep1
      else if (word(1:3).eq.'POL') then
C        ! get the optional number after POL, and use if to limit the nr
C        ! of pollutants in the output. Default 5 pollutants
         Variab='the number of pollutants for output'
         CALL GETWI(line,K,l,outputoptions(2),5)
      else if (word(1:4).eq.'HEAT') then
C        ! This option is redundant but is kept for possible future use
         outputoptions(3) = 1
      else if (word(1:6).eq.'CHECKC') then
         outputoptions(4) = 1
      else if (word(1:5).eq.'INPUT') then
         outputoptions(5) = 1
      else if (word(1:3).eq.'SET') then
         outputoptions(6) = 1
      else if (word(1:7).eq.'DEFAULT') then
         Variab='get level of output for defaults'
CC       ! 1 = echo defaults because 'd' has been read in the input file
CC       ! 2 = there is no more data on the input line
CC       ! 3 = both
C@NBI PGS 2000Aug18 - Bugfix: This hasn't been inplemented properly; option 2
C@NBI                 presently is the same as 3.  So I suggest another method
C@NBI                 which requires values 1 and 3 to be swapped. It seems logical
C@NBI                 to do this in the CIF file and not swap the values internally
C@NBI                 in COMIS (confusing). Backward compatibility of old CIF files
C@NBI                 will not be an issue, and the new convention can be described
C@NBI                 in the 3.1 User Guide.  The new options become:
C@NBI                 OutputOptions(7) = 0 for no echo of defaults ("DEFAULT" keyword not used)
C@NBI                                  = 1 to echo ALL defaults (Default option)
C@NBI                                  = 2 to echo ONLY defaults used whenever "d[e[f[a[u[l[t]]]]]]" appears in input file
C@NBI                                  = 3 to echo ONLY defaults used whenever there is no more data at the end of input line
         CALL GETWI(line,K,l,outputoptions(7),1)
C@NBI PGS 2000Aug18   (end)

C        ! report all possible unit conversions
      else if (word(1:4).eq.'UNIT') then
         outputoptions(8) = 1
C@empa aw 2000nov22 "KEYWORD" canceled. We prefer to keep the UG up to date. 
CC        else if (word(1:7).eq.'KEYWORD') then
C        ! report all possible keywords
CC                outputoptions(9) = 1
      else if (word(1:7).eq.'ECHOSCH') then
C        ! report all changes in schedules at run time
           outputoptions(10) = 1
      else if (word(1:8).eq.'ONSCREEN') then
C        ! report various items on screen during the simulation, just to
C        ! follow the progress, so you may stop the run if not OK
         outputoptions(11) = 1
         OnScreen=Line(k:l)
      else if (word(1:6).eq.'STEADY') then
C        ! make steady state concentration output for every schedule time step
C        ! used at a parametric study of conditions
         outputoptions(12) = 1
      else if (word(1:6).eq.'DEBUG') then
C        ! Echoes DAF TMS files
C        ! read the number after DEBUG, 1=DAF 2=TMS 3=DAF and TMS
          Variab='the level of Debug output'
          CALL GETWI(line,K,l,outputoptions(13),3)
      else if (word(1:7).eq.'LOOPRHO') then
C@NBI PGS 2000Aug13 - Someone forgot to set RHOSYNC here, so here it is
         RhoSync=.TRUE.
      else if (word(1:5).eq.'START' .or.
     &         word(1:5).eq.'SCHED' .or.
     &         word(1:5).eq.'HISTO' .or.
     &         word(1:4).eq.'STOP'  .or.
     &         word(1:2).eq.'NO'    .or.
     &         ICindex(str,word(1:3)).gt.0 ) then
C@tno jcp 1996Jul09_15:40:21 this is just to avoid error messages of
C uninterpreted keywords, start and stoptimes Schedule and histo start are
C read by RDperiod
      else
C@NBI PGS 2000Jul16 - Improve grammar & clarity (I hope)
CC      call inerr('At &-PR-SIMUlation or &-PR-OUTPut options, an'//
CC   &  ' input line contains an unrecognised option/'//
CC   &  ' keyword. The line remains uninterpreted. Line in error:',
CC   &   line(1:lenstr(line)),.true.,2)
         call inerr('In &-PR-SIMUlation or &-PR-OUTPut datablock, an'//
     &   ' input line contains an unrecognised option/keyword.'//
     &   ' The line remains uninterpreted. The erroneous line is:',
     &   line(1:lenstr(line)),.true.,2)
      endif
C-----
C     ! Loop if there is another line in this datablock
      Call readlin(line,  LinTyp, K, .True.)
      if (FlgEnd) goto 99
      l=lenstr(line)
      if (LinTyp .eq. TDATA) goto 10

99    continue
C@tno jcp 1996Jun11_15:00:59 reset FlgEnd
c        FlgEnd=.False.

C---------------------------------
C     List I/O unit options in COF
C---------------------------------

C     ! reading COMIS.CIF could be stopped by a fatal error
C     ! but some one could have asked for possible unit conversions. Let's Write them
C     ! first to COF!
      if ((outputoptions(8).eq.1).and.FirstInSimu) then

C        1. Write header for units
C        -------------------------

         write(COF,*)' In &-PR-SIMUlation the word ''UNIT'' is found.'
         write(COF,*)' Here follows an overview '//
     &   'of the units COMIS'
         write(COF,*)' will recognize, with their conversion '//
     &   'factors: SI*Factor=UserUnit'
         write(COF,*)' In capitals the LITERAL name for the quantity'//
     &   ' to be used as ID'
         write(COF,*)' Example:'
         write(COF,*)'&-PR-UNITs (in the input file).'
         write(COF,*)'         input output'
         write(COF,*)'massflow  kg/s  m3/s'
         write(COF,*)'airleak   ELA4  ELA4'
         write(COF,*)' '

C        2. Loop through available units
C        -------------------------------

C@NBI PGS 2000Oct08 - Can now define different I/O units for each pollutant,
C@NBI                 Change DO loop to stop at last defined unit (UnitToff-1)
CC       do 100 i=1,(NoUnit-MaxC)
         do 100 i=1,(UnitToff-1)
CC          if (i .eq. UnitToff) goto 100
            l=lenstr(UnitStr(i))
            if (l.gt.0) then
C              ! there are units defined in FilUnit for this quantity

C              2.1 write the name of the quantity
C              ----------------------------------

               write(COF,*)
               if (i.eq.UnitCm) write(COF,*)
     &            KeysU(1:10),' Air leakage'
               if (i.eq.UnitFma) write(COF,*)
     &            KeysU(11:20),' (ventilation) Mass flow rate'
               if (i.eq.UnitP) write(COF,*)
     &            KeysU(21:30),' (Air) Pressure (differences)'
               if (i.eq.UnitTmul) write(COF,*)
     &            KeysU(31:40),' Temperature'
               if (i.eq.UnitXh) write(COF,*)
     &            KeysU(41:50),' Humidity'
               if (i.eq.UnitPsou) write(COF,*)
     &            KeysU(51:60),' Pollutant Source'
               if (i.eq.UnitPSin) write(COF,*)
     &            KeysU(61:70),' Pollutant Sink'
               if (i.eq.UnitPConc) write(COF,*)
     &            KeysU(71:80),' Pollutant concentration '
               if (i.eq.UnitFva) write(COF,*)
     &            KeysU(81:90),' Fan flowrate (from input to m3/s)'
               if (i.eq.UnitW) write(COF,*)
     &            KeysU(91:100),' Air (wind) velocity'
               if (i.eq.UnitRate) write(COF,*)
     &            KeysU(101:110),' Air change rate'
               if (i.eq.UnitAge) write(COF,*)
     &            KeysU(111:120),' Mean Age of air'
               if (i.eq.UnitE) write(COF,*)
     &            KeysU(121:130),' Energy'
               if (i.eq.UnitProf) write(COF,*)
     &            KeysU(131:140),' Wind profile'

C              2.2 write the units defined
C              ---------------------------

               call wrt80(COF,unitstr(i)(1:l),WCRT)

C              2.3 additional notes for specific units
C              ---------------------------------------

               line2=' '
               if (i.eq.UnitCm) call wrt80(COF,' Air leakage conversi'//
     &            'ons are made by routine ConvCs and depend on the v'//
     &            'alue of the FlowExponent.',WCrt)
C              ! added part to mention -OSR
               if (i.eq.UnitP) call wrt80(COF,' Add ''-OSR'' to the p'//
     &            'ressure unit to get Outside Stack pressure as Refe'//
     &            'rence in the output e.g. ''Pa-OSR''',WCrt)
               if (i.eq.UnitProf) call wrt80(COF,' Wind velocity prof'//
     &            'iles have no fixed conversion. Routine Wind switch'//
     &            'es the function.',WCrt)
               if (i.eq.UnitPConc) call wrt80(COF,'The first 8 units '//
     &            'can be combined like ''ug/m3'' the last 5 can be u'//
     &            'sed as given. Conversions are made by routine CnvC'//
     &            'onc and use the molar mass of the pollutant.',WCrt)

C              2.4 write the conversion factors assuming 0 is not filled (left out)
C              --------------------------------

               do j=1,ConvEnt
C@empa aw 2005aug30 don't leave the loop as we might have additional factors at the end of conv
CC                  if (conv(j,i).eq.0.0) goto 95
C                 ! keep track of the number of temperature conversions
C                 ! for printing the offsets in the next loop
                  if (i .eq. UnitTmul) numtemps=j
                  call reldis(conv(j,i),5,dum,Length,0)
C@empa aw 2005aug30 suppress the output of zeros
                  if (conv(j,i).eq.0.0) dum=''
C@empa aw 2005aug30 the following line produces a compiler warning
CC                  dum2=dum//'                '
                  dum2=dum
                  length=length+1
                  if (length.lt.10)  length=10
                  l=lenstr(line2)
                  if (l.lt.((j-1)*10))  l=(j-1)*10
                  line2(l+1:)=dum2(1:Length-1)
               ENDDO
95             continue
               l=lenstr(line2)
               if (l.gt.0) then
                  if (l.gt.WCrt) then
                     CALL wrt80(COF,line2(1:l),WCrt)
                  else
                     write(COF,*) line2(1:l)
                  end if
               end if

               line2=' '
               if (i.eq.UnitTmul) then
C                 ! this is temperature. It has a second coefficient
                  do 91 j=1,numtemps
                     call reldis(conv(j,UnitToff),5,dum,Length,0)
C@empa aw 2005aug30 suppress the output of zeros
                     if (conv(j,i).eq.0.0) dum=''
C@empa aw 2005aug30 the following line produces a compiler warning
CC                  dum2=dum//'                '
                     dum2=dum
                     length=length+1
                     if (length.lt.10)  length=10
                     l=lenstr(line2)
                     if (l.lt.((j-1)*10))  l=(j-1)*10
                     line2(l+1:)=dum2(1:Length-1)
91                continue
                  l=lenstr(line2)
                  if (l.gt.0)  CALL wrt80(COF,line2(1:l),WCrt)
               end if
            end if
100      continue
         write(COF,*)'------- end of available conversion units -----'
      end if

C------------------------------------
C     List all keyword options in COF
C------------------------------------
C@empa aw 2005apr26 Outputoption(9) "KEYWORD" was canceled on 2000nov22 (see routine inSIMU) 
CC      if ((outputoptions(9).eq.1).and.FirstInSimu) then
CC         write(COF,*)' Here follows an overview ',
CC     &   'of the keywords COMIS will recognize, and what'
CC         write(COF,*) ' they define:'
CC         DO I=1,NdKey
CC            call explain(Keys((I-1)*10+1:(I*10)),exstr)
CC            Write(COF,1000) I,Keys((I-1)*10+1:(I*10)),exstr
CC         ENDDO
CC         write(COF,*)'------- end of available keywords -----'
CC
CC         write(COF,*)' Here follows a list of two letter keywords'
CCC@empa aw 1999dec08 As &-PR-SIMU and &-PR-OUTP are synonyms this is not wrong,
CCC@empa              but according to UG they should go under &-PR-OUTP
CCCC       write(COF,*)' That may be used at &-PR-SIMUlation for output'//
CC         write(COF,*)' That may be used in &-PR-OUTPut options.'
CC         write(COF,*)' Most of them may be followed by a list of zone'//
CC     &   '/node/link numbers/names.'
CC         call simukey
CC         write(COF,*)'---- end of available keywords in &-PR-OUTP ----'
CC      end if
C-----

C@empa aw 1999dec08 now no longer the first call of InSimu
      FirstInSimu=.false.
      RETURN
1000  FORMAT(1X,I3,1X,A10,1X,A)
      END


Ch**********************************************************************
        BLOCK DATA IniOutpO
C initialization of named COMMON are now in BLOCKDATA
C@empa aw 1998jun25 take just the needed COMMON block instead of includefile
C***********************************************************************

      INTEGER outputoptions(15), UOFunf,InvMatrix
      COMMON/PROUT/ outputoptions,UOFunf,InvMatrix
C@tno jcp 1996Apr07_13:29:30 15 zero's to outputoptions data
      DATA outputoptions / 0,0,0,0,0, 0,0,0,0,0, 0,0,0,0,0 /
      END


Ch**********************************************************************
      SUBROUTINE inCTRL(Line, LinTyp, L, K)
C***********************************************************************
C PR-CONTrol parameters
C
C Changes:
C@lbl bvs 1995Jun Since the solver selection has been removed for the
C general user we must decide whether there were three columns of input
C or four.  The former would mean that the solver number WAS NOT input
C and the third column is the maximum number of iterations, the latter
C means that the solver number WAS input and the fourth column is the
C max number of iterations.
C@lbl dml 1999nov22 Replace solver control parameters noInit and useOpz
C   with stp1Init, stp2Init, and rhoSync.
C@lbl dml 2000jan05 Enable slvSel=6 valid input.
C@empa aw 2000apr11 As slvSel is a hidden parameter, I move it at the end.
C@empa aw           There will be a note in COF, if slvSel is not default.
Ch**********************************************************************

      IMPLICIT NONE
      INCLUDE 'comv-inp.inc'
      CHARACTER*(*) line
      INTEGER LenStr
      INTEGER k,l,LinTyp
C@empa aw 2000apr11 LStr,SlvStr
      INTEGER LStr
      CHARACTER slvStr*5

C------------------------
C     Read Data section 1
C------------------------

C      ____________________________________________________________________
C     |1.| Under | Tolerances                         |Start  |Link Flow   |
C     |__| Relax-|____________________________________|Number |Pressure    |
C     |    ation | absolute  | Relative |CORR*JAC(i,i)|of Ite-|Laminar Flow|
C     |    Factor|  EpsFA    |  EpsFR   | EpsCJ       |rations|DifLim      |
C     |    [-]   |  [flo]    |  [-]     |  [flo]      |  [-]  |   [pre]    |
C     |__________|___________|__________|_____________|_______|____________|

      k=0
      Variab='RelaxV at PR-CONTrol'
      CALL GETWR(Line,k,l,RelaxV,1.0,.TRUE.)

      Variab='EpsFA'
      CALL GETWR(Line,k,l,EpsFA ,DEpsFA,.TRUE.)
      EpsFA = EpsFA*ifact(UnitFma)

      Variab='EpsFR'
      CALL GETWR(Line,k,l,EpsFR ,DEpsFR,.TRUE.)

      Variab='EpsCJ'
      CALL GETWR(Line,k,l,EpsCJ ,DEpsCJ,.TRUE.)
      EpsCJ = EpsCJ*ifact(UnitFma)

      Variab='nNewt'
      CALL GETWI(Line,k,L,nNewt,DnNewt)

      Variab='DifLim'
      CALL GETWR(Line,k,l,DifLim,DDifLim,.TRUE.)
      DifLim = DifLim*ABS(ifact(UnitP))

C------------------------
C     Read Data section 2
C------------------------

      Call readlin(line, LinTyp, K, .True.)
      if (FlgEnd) goto 99
      if (LinTyp .ne. TDATA) goto 900
      l=lenstr(line)
C      ________________________________________________________________
C     |2.| Init pres | Init pres | Max itns| Solver selector (OPTIONAL)|
C     |__| first step| later step| allowed |                           |
C     |0=zero        |0=zero     |         |1=Newton (with given Relax)|
C     |1=lin init    |1=lin init |         |3=Walton Steffensen        |
C     |              |2=use prev |         |5=Walton 2 fixed relax.fact|
C     |              |           |         |6=trust region line search |
C     |stp1Init      |stp2Init   |mIter    |slvSel                     |
C     |[-]           |[-]        |[-]      |[-]                        |
C     |______________|___________|_________|___________________________|

C     ! GETWI= get word first and put it in an INTEGER
      Variab = 'stp1Init'
      call GetWI(Line,k,L,stp1Init,Dstp1Init)
      if( stp1Init.lt.0 .or. stp1Init.gt.1 ) stp1Init = Dstp1Init

      Variab = 'stp2Init'
      call GetWI(Line,k,L,stp2Init,Dstp2Init)
      if( stp2Init.lt.0 .or. stp2Init.gt.2 ) stp2Init = Dstp2Init

      Variab='Miter'
C@empa aw 2000oct06 get Miter not slvSel here 
CC    CALL GETWI(Line,k,L,slvSel,DMiter)
      CALL GETWI(Line,k,L,Miter,DMiter)

C     ! Check if any more input on this line (optional fourth column)
      Variab='slvSel'
      CALL GETWI(Line,k,L,slvSel,DslvSel)
C@empa aw 2000dec14 message cancelled (SlvSel is not hidden any more)
CC      if (slvSel.ne.DslvSel)then
CC         CALL intdis(SlvSel,slvStr,LStr)
CC         CALL inerr('&-PR-CONTrol: Solver Nr. '//slvstr(1:LStr)//
CC     &   ' is selected. This is not the default solver!',
CC     &   ' ',.false.,0)
CC      endif
C     ! The solver selection has been reduced to 1,3,5,6
      if ((SlvSel .ne. 1) .and. (SlvSel .ne. 3) .and.
     &   (slvSel.ne.5) .and. (slvSel.ne.6) ) then
         call inerr('Only solvers 1,3,5,6 are available.'
     &   //' Using default',' ',.true.,0)
         SlvSel = DSlvSel
      endif

C     ! read next line for next keyword
      Call readlin(line, LinTyp, K, .True.)
      goto 99

900   call inerr('No data for section 2 of PR-CONTrol',' ',.TRUE.,2)

99    continue
      RETURN
      END

