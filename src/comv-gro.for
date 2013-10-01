C+*********************************************************** comv-gro.f
Ch**********************************************************************
C     MODULE GrOut.FOR
C***********************************************************************
C Purpose: Creates spreadsheet readable files which can be
C          used to display graphs.
C
C July 1990 wpk (Werner P. Keilholz)
C Changes:
C@empa aw 1993feb23 all calls to UERROR and UERROR2 replaced with ERROR
C                   and ERROR2 from module COMV-UTI.FOR
C                   Subroutine UERROR, UERROR2, UGOON removed.
C@empa aw 1994mar10 Option to create a COS-file (Comis Outputfile for
C                   Spreadsheet) with separators.
C                   According to the options in the SET-file, a COS- file
C                   will be produced.
C@empa aw 1994mar10 Dimensions for the outputline arrays and the record length
C                   in the outputfiles according the max parameter MCOSCol.
C@empa aw 1994mar10 Checks for the length of outputlines and number of columns
C                   in the outputfiles added.
C@empa hf/aw 1995mar15 New version with many changes:
C                       - New output options added
C                       - Changed some keywords of old output options
C                       - Output of flow matrix added
C                       - T-option for mean values added
C                       - Allocation of file unit numbers changed
C                       - Error message when keyword not valid
C                       - Input line with link-, zone names and not sequence
C                         numbers any more.
C                       - Error message when a specified link or zone name
C                         does not exist
C                       - Unit conversion to user output units added
C                       - Direction sign '<' or '>' added at keyword 'FL' to
C                         give the possibility to output the two way flow.
C@lbl bvs 1995Jul no more lotus output option.  Generic spreadsheet only.
C
C Abreviations:
C    COS = Comis Spreadsheet Output (= extension of output files)
C
C *  Values per zone:
C    PZ = Pressure per Zone
C    TZ = air Temperature per Zone
C    HZ = Humidity per Zone
C    FZ = Flow per Zone
C    Fn = Effective flow calculated with fictive source "n" added
C    IZ = outdoor air Infiltration per Zone
C    AZ = outdoor Air change rate per Zone
C    MZ = Mmean age of air per Zone
C    EZ = air change Efficiency per Zone
C    On = number of Occupant "n"
C    Yn = activitY of occupant "n"
C *  Values per zone and gas:
C    Cn = Concentration per gas and zone, 1<= n <= 5; n = gas number
C    Qn = Pollutant source Strength per gas and zone, 1<= n <= 5
C    Sn = pollutant Sink per gas and zone, 1<= n <= 5
C *  Values per link:
C    FL = mass Flow per Link
C    TL = Link Temperature
C    VL = actual Value per Link
C    PL = Pressure per Link
C    DL = Cd Value per Link (lage opening)
C *  Values per external node:
C    PE = wind Pressure per External node
C *  Ambient conditions
C    WA = Wind velocity (Ambient)
C    TA = Ambient air Temperature
C    HA = Ambient Humidity
C *  Values per building
C    FB = Flow matrix Building
C@tno jcp 1996May24_09:29:03 explain FW option
C    FW = Flow matrix Building With External (Wind) nodes
C    IB = air Infiltration per Building
C    AB = Air change rate per Building
C    MB = arithmetic Mean age per Building
C    RB = Rms of mean age per Building
C    NB = Nominal time constant per Building
C    EB = air change Efficiency per Building
C    LB = ventiation heat Loss energy per Building (not included yet)
C
C The reference structure of the routines in this module is:
C
C
C       1  INIGROUT      GETUNITNR
C       2                INERR
C       3                INTDIS
C       4                LENSTR
C       5                MKTAB  GETDATA       ERROR2
C       6                       GETLIZONA     LASTCHAR
C       7                       INERR
C       8                       INTDIS
C       9                       LASTCHAR
C      10                       READNUM       INERR
C      11                                     ISNAM
C      12                                     LOOKNAM
C      13                       SKIPSPC
C      14                READOPT       INERR
C      15                              LOWERC
C      16                              MKTAB  > 5
C      17                              READLIN
C      18                              UPPERC
C
C
C     1.1  WRIGROUT      ERROR2
C     1.2                LENSTR
C     1.3                MKTAB  > 5
C     1.4                TOPTCALC      GETDATA       > 5
C     1.5                WRIFLOWMAT    INERR
C     1.6                              LENSTR
C
C
C     2.1  CLGROUT
C
Ch**********************************************************************


Ch**********************************************************************
        SUBROUTINE IniGrOut
C***********************************************************************
C Purpose: Opens *.COS files according to contents of
C          variables in GrOut COMMON - block "CGOlns" (see grout.inc)
C          called once, before the time loop in COMIS
C
C July 1990 wpk
C Module : GrOut.FOR
C Changes:
C@empa hf/aw 1995mar15 New version with many changes:
C                       - old markers deleted
C                       - new output options added
C                       - changed some keywords of old output options
C                       - selection of units put into a new function
C                         GetUnitNr
C                       - changes for output flow matrix.
C@NBI PGS 2000Aug01 - Tidied up routine; No syntax change
C@NBI PGS 2000Aug02 - "j" no longer used so removed from decralations
C@NBI PGS 2000Nov05 - Added CHARACTER "word2" 
C Limits :
C
C
C example: call IniGrout(PKeyRec)
C call
C USES:
C IO # Name      Units  Description
C I  * pKeyRec   [-]    pointers to DAF file
C
Ch**********************************************************************

      IMPLICIT NONE
      INCLUDE 'comv-inp.inc'
      INCLUDE 'comv-uni.inc'

C     ! local Parameters
      INTEGER i,n,LStr,SepLen,Pos,LenStr,GetUnitNr,k,l
      character  OutStr*(MCOScol*16),SepStr1*15, MaxColStr*3,word*40
     &,word2*10
C-----

C     ! Init. possible keywords in &-PR-OUT section of *.CIF file
      KeyW(1)='PZ-'
      KeyW(2)='TZ-'
      KeyW(3)='HZ-'
      KeyW(4)='FZ-'
      KeyW(5)='FL-'
      KeyW(6)='TL-'
      KeyW(7)='VL-'
      KeyW(8)='WA-'
      KeyW(9)='TA-'
      KeyW(10)='HA-'
      KeyW(11)='C1-'
      KeyW(12)='C2-'
      KeyW(13)='C3-'
      KeyW(14)='C4-'
      KeyW(15)='C5-'
      KeyW(16)='Q1-'
      KeyW(17)='Q2-'
      KeyW(18)='Q3-'
      KeyW(19)='Q4-'
      KeyW(20)='Q5-'
      KeyW(21)='S1-'
      KeyW(22)='S2-'
      KeyW(23)='S3-'
      KeyW(24)='S4-'
      KeyW(25)='S5-'
      KeyW(26)='PE-'
      KeyW(27)='IZ-'
      KeyW(28)='AZ-'
      KeyW(29)='MZ-'
      KeyW(30)='EZ-'
      KeyW(31)='FB-'
      KeyW(32)='IB-'
      KeyW(33)='AB-'
      KeyW(34)='MB-'
      KeyW(35)='RB-'
      KeyW(36)='NB-'
      KeyW(37)='EB-'
      KeyW(38)='LB-'
C@tno jcp 1996Apr02_12:16:44 added KeyW(39) effective flowrate
C@NBI PGS 2000Aug01 - EF is not a valid keyword any more (renumbered the rest)
CC    KeyW(39)='EF-'
C@tno jcp 1996May24_09:29:52 added KeyW(40) Flowmatrix With external nodes
      KeyW(39)='FW-'
C@empa aw 2000jan31 Ox number of Occupant x and Yx activitY of occupant x added
      KeyW(40)='O1'
      KeyW(41)='O2'
      KeyW(42)='O3'
      KeyW(43)='O4'
      KeyW(44)='O5'
      KeyW(45)='Y1'
      KeyW(46)='Y2'
      KeyW(47)='Y3'
      KeyW(48)='Y4'
      KeyW(49)='Y5'
C@empa aw 2000feb01 Fx effective Flow calculated with fictive source x added
      KeyW(50)='F1'
      KeyW(51)='F2'
      KeyW(52)='F3'
      KeyW(53)='F4'
      KeyW(54)='F5'
C@empa aw 2002dec04 PL (pressure difference per link) added
      KeyW(55)='PL'
C@empa aw 2005aug29 DL (Cd- value per link) added
      KeyW(56)='DL'

      NumKeyW=MaxCOSk

C     ! Init. logical unit numbers for *.COS files set to 9
      do i=1,NumKeyW
         COSlun(i)=9
      ENDDO

C     ! Tablin = number of entries in *.COS table; Must not be changed !
      TabLin=5

C     ! Get the units associated with each keyword
C@tno jcp 1996Jul16_11:39:53 IniGrout Loop 109 added to assign KWunitNr used by
      do i=1,MaxCOSk
C@empa aw 2000apr03 EF is not a valid keyword any more
CC       if (keyw(i)(1:2).eq.'EF') then
CC       j=GetUnitNr('F')
CC       else
CC       j=GetUnitNr(KeyW(i)(1:1))
CC       end if
CC       KWUnitNr(i)=j
C@empa aw 2000dec18 We need now two characters
C@empa aw 2000dec18 This is to get an individual user unit number per pollutant 
CC         KWUnitNr(i)=GetUnitNr(KeyW(i)(1:1))
         KWUnitNr(i)=GetUnitNr(KeyW(i)(1:2))
      ENDDO

C     ! Check what *.COS are desired by user, and open the files
      CALL ReadOpt

C     ! Initialize column-delimiter string
      SepLen=Lenstr(SepStr)
      if (SepLen.gt.2)then
C        ! Remove quotation marks around column-delimiter string
         SepStr1=Sepstr(2:Seplen-1)
         SepLen=SepLen-2
      else
         SepLen=0
      endif

C------------------------------------
C     Loop through openned .COS files   i.e. loop through used keywords
C------------------------------------

      do 110 i=1,MaxCOSk
C@tno jcp 1996Apr08_20:58:04 troubled by 'EF', sorry for spoiling the structure
C but FE was also not possible (conflict with external node)
C@NBI PGS 2000Aug01 - 'EF' keyword must be removed here too.
C@NBI                 Also redundant duplication : filling KWUnitNr array
C@NBI                 is already done above.
CC       if (keyw(i)(1:2).eq.'EF') then
CC       j=GetUnitNr('F')
CC       else
CC       j=GetUnitNr(KeyW(i)(1:1))
CC       end if
CC       KWUnitNr(i)=j

         if (COSline(i) .le. 0) goto 110

         OutIDX=1
C@NBI PGS 1999Aug09 - Minus sign is more befitting than a zero
CC       COutLine(OutIDX)='0'
         COutLine(OutIDX)='-'
         OutIDX=OutIDX+1
         call MkTab(COSRange(i),i,2)
         OutIDX=OutIDX-1

C@tno jcp 1996May24_10:57:22 added check for width at SIMU option FW
CC       if ((OutIDX.gt.MCOSCol).or.
CC     & ((KeyW(i).eq.'FB-').and.(Nz+3.gt.MCOSCol))) then
         if ((OutIDX.gt.MCOSCol).or.
     &      ((KeyW(i).eq.'FB-').and.(Nz+3.gt.MCOSCol)).or.
     &      ((KeyW(i).eq.'FW-').and.(Nz+1+Nwind.gt.MCOSCol))) then
            call intdis(MCOSCol,MaxColStr,LStr)
            call Inerr('&-PR-OUTPut: Too many columns for the '//
     &         'file: '//Keyw(i)(1:2)//'*.C?O',
     &         'Maximum allowed number of columns: '
     &         //MaxColStr(1:LStr),.FALSE.,2)
            return
         endif

C@tno jcp 1996May24_09:32:37 included FW option in if statement
CC       if ((OutIDX*(15+SepLen).gt.(16*MCOSCol)).or.
CC     &    ((KeyW(i).eq.'FB-').and.((Nz+3)*(15+SepLen).gt.
CC     &    (16*MCOSCol)))) then
         if ((OutIDX*(15+SepLen).gt.(16*MCOSCol)).or.
     &      ((KeyW(i).eq.'FB-').and.((Nz+3)*(15+SepLen).gt.
     &      (16*MCOSCol))).or.
     &      ((KeyW(i).eq.'FW-').and.((Nz+1+Nwind)*(10+SepLen).gt.
     &      (16*MCOSCol)))
     &          ) then
C@NBI PGS 1999May05 - Changed fileNam2 from XX*.COS to *-XX.COS
            call Inerr('&-PR-OUTPut: Output lines too long '//
     &       'for the file: *-'//Keyw(i)(1:2)//'.COS',' ',.FALSE.,2)
            return
         endif

C        Write 1st row of .COS file header  :  units
C        ---------------------------------

         k=0
         l=lenstr(COSrange(i))
         variab='get COS item name in iniGrOut '
         call getwrd(COSrange(i),k,l,word)
C@NBI PGS 2000Nov05 - Bugfix units for occupants
         IF(KeyW(i)(1:1).EQ.'O')THEN
           word2='#occupants'
         ELSEIF(KeyW(i)(1:1).EQ.'Y')THEN
           word2='MET'
         ELSE
           word2=OUnit(KWUnitNr(i))
         ENDIF
C@NBI PGS 1999Aug09 - Sould be in separate columns, so use separator
C@NBI PGS 2000Aug01 - Better to use KWUnitNr(i) than j (j no longer defined)
CC       write(COSlun(i),'(3A)',err=900)OUnit(j),' ',word
         write(COSlun(i),'(3A)',err=900) word2,SepStr1(1:SepLen),word
         Pos=1
         OutStr=' '

C        Write 2nd row of .COS file header  :  names
C        ---------------------------------

         do 107 n=1,OutIDX
            LStr=lenstr(CoutLine(n))
C@empa aw 1999oct29 the first entry in OutStr has 14 caracters the others 10
CC          write(OutStr(Pos:Pos+7),'(A8)') COutLine(n)(1:LStr)
CC          Pos=Pos+8
            if (n.eq.1)then
               write(OutStr(Pos:Pos+14),'(A15)') COutLine(n)(1:LStr)
               Pos=Pos+15
            else
               write(OutStr(Pos:Pos+9),'(A10)') COutLine(n)(1:LStr)
               Pos=Pos+10
            endif
            if (SepLen.gt.0) OutStr(Pos:Pos+SepLen-1)=SepStr1(1:SepLen)
            Pos=Pos+SepLen
107      continue
         write(COSlun(i),'(A)',err=900)
     &   OutStr(1:Pos-1)

110   continue
      RETURN

C-------------------
C     Error handling
C-------------------

C     ! error writing COS file --- fatal
900   call inerr('&-PR-OUTPut: '//KeyW(i),
     &   'Error writing COS-file',.FALSE.,3)
      END


Ch**********************************************************************
        SUBROUTINE WriGrOut(time)
C***********************************************************************
C
C    As an option *.COS (Comis Spreadsheet Output) files can be produced.
C    Called once for every timestep in COMIS
C
C COMMON-Block DAFACCESS was created only for this routine.
Cxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
Cxxx Purpose: fills graphical output file(s), to be read by spreadsheet xxx
Cxxx programs.    *.COS means COMIS Spreadsheet Output file             xxx
Cxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
C
C July 1990 wpk
C Module : GrOut.FOR
C Changes: lbl bs may 1991  time changed from REAL to DOUBLE PRECISION
C@empa aw 1994mar10 output for *.COS files added
C@empa aw 1995feb25 write flow matrix
C@tno jcp 1996May23_17:23:11 new write FmMat
C@NBI PGS 2000Aug01 - Tidied up routine; No syntax change
C Limits :
C
C example: call WriGrOut(time)
C call
C USES: outLine, outIDX
C
C IO # Name  Units  Description
C I  1 time  [-]    current timestep of the simulation
C
C ERROR RETURN IF: none
Ch**********************************************************************

      IMPLICIT NONE
      INCLUDE 'comv-inp.inc'
      INCLUDE 'comv-uni.inc'

C     ! passed subroutine arguments
      DOUBLE PRECISION time
C     &&& "time" is not defined yet !

C     ! local parameters
      INTEGER LenStr,i,j,SepLen,Pos
      character OutStr*(MCOSCol*16),SepStr1*10

C------------------------------------
C     Loop through openned .COS files   i.e. loop through used keywords
C------------------------------------

      do 100, i=1,NumKeyW
         if (COSline(i) .le. 0) goto 100

C        ! write flow matrices, if specified
         if (COSRange(i)(1:2).EQ.'FB')then
            call WriFlowMat(COSlun(i),time,.false.)
            goto 100
         ELSEIF (COSRange(i)(1:2).EQ.'FW')then
C           ! write flow matrix FmMat
            call Wri2FlowMat(COSlun(i),time,.false.)
            goto 100
         endif
 
         OutIDX=1
         OutLine(OutIDX)=time
         OutIDX=OutIDX+1 
         call MkTab(COSRange(i),i,3)
         OutIDX=OutIDX-1
 
C        ! Initialize column-delimiter string
         SepLen=Lenstr(SepStr)
         if (SepLen.gt.2)then
C           ! Remove quotation marks around column-delimiter string
            SepStr1=Sepstr(2:Seplen-1)
            SepLen=SepLen-2
         else
            SepLen=0
         endif
 
C        Write line of data to .COS file
C        -------------------------------

         Pos=1
         OutStr=' '
         do 10 j=1,OutIDX
            if (j.eq.1)then
C              ! this is the Lotus time format (also used in Excel etc) ddddd.tttttt
C@lbl bvs 1997Oct20 need 15 digits for negative time (schedules without dates
C                   are way in the past)
               write (OutStr(Pos:Pos+17),'(F15.6)')Outline(j)
               Pos=Pos+15
            else
C@lbl bvs 1997Jul24 didn't allow xxxE-10 !
CC             write (OutStr(Pos:Pos+7),'(E8.3E1)')Outline(j)
C@lbl bvs 1997Nov5 made 4 significant figures
CC             write (OutStr(Pos:Pos+8),'(E9.3E2)')Outline(j)
               write (OutStr(Pos:Pos+9),'(E10.4E2)')Outline(j)
               Pos=Pos+10
            endif
            if (SepLen.gt.0) OutStr(Pos:Pos+SepLen-1)=SepStr1(1:SepLen)
            Pos=Pos+SepLen
10       continue
         write(COSlun(i),'(A)',err=900) OutStr(1:Pos-1)

100   continue
C-----

C@tno jcp 1996Apr01_11:24:05 we don't want to call TOptCalc if -T is not used?
      if (IsTopt) call TOptCalc(time)

C@tno jcp 1996Apr01_11:24:05 we don't want to call HistCalc if -T is not used?
      if (.NOT. HistConc) then
C histogram has not been called from Pollutant, so we do it here
         if (IsHopt .and. HistActive) call HistCalc(real(interval))
C@empa aw 2000apr25 call HistCalc with interval  
CC          call HistCalc(time)
      end if
      RETURN

C-------------------
C     Error handling
C-------------------

C     ! error writing COS file --- fatal
900   call error2('Error writing COS file',KeyW(i),4)
      END


Ch**********************************************************************
        SUBROUTINE ReadOpt
C***********************************************************************
C Purpose: Subroutine to IniGrOut; finds out what *.COS -
C          Files shall be produced by reading the words belonging to
C          the &-PR-OUTP Keyword.
C
C July 1990 wpk
C Module : GrOut.FOR
C Changes: lbl bs april 1991
C@empa aw 1995mar06 recl is not alowded for sequential access files in F77
C@                  standard. If your compiler complains, comment the line with
C@                  "recl=" out and take the line after. With changing parame-
C@                  ter MCGOcol in COMV-PAR.INC you have to make shure, that the
C@                  length of the lines produced for *.CGO/ *.COS files is not
C@                  longer than the record length of a sequential access file.
C@                  (The length of a line is MCGOcol*16)
C@empa hf/aw 1995mar15 New version with many changes:
C                       - old markers deleted
C                       - T-option for mean values added
C                       - H-option for histograms added
C                       - Allocation of file unit numbers changed
C                       - Structure of loop for reading &-PROUTP section
C                         changed: - Break inner loop when valid keyword found
C                                  - Error message when keyword not valid
C                                  - Condition for break when end of section
C                                    reached put at the beginning of the loop
C                       - Set logical flags for mean value calculation (IsTOpt)
C                         and flow matrix calculations (calcfm)
C                       - Checking of COS file opend for flowmatrix
C                       - don't stop when &-PR-OUTP keyword not found
C@tno jcp 1996May13_12:39:09 must allow for &-PR-SIMU too
C@tno jcp 1996May24_09:27:28 also calcfm if option FW is used
C@tno jcp 1996May24_11:05:12 this loop test the necessity for flowmatrix
C inversion in PostCal
C@tno jcp 1996May24_09:35:24 this check must also look for FW. Copied below
C@tno jcp 1996Mar29_15:59:57 added section for Histograms
C@tno jcp 1996Jun11_14:58:37 reset FlgEnd
C@tno jcp 1996Jul05_08:36:31 added variab
C@tno jcp 1996Jul08_17:14:46 nCifLine the line number is reset after opening cif
C@tno jcp 1996Jul11_12:12:05 added variab for READOPT
C@tno jcp 1996Jun11_15:41:17 reset FlgEnd
C@lbl bvs 1997Jul16 keep going if PR-OUTP or PR-SIMU next
C@bvs 1999Mar11 increased max length of project part from 6 to 72 characters
C@NBI PGS 1999May05 - Bugfixed fileNam2 naming code. The string had
C@NBI                 spurious charcters at end  (using NAG Ftn90 compiler) 
C@NBI               - Also changed .COS filename format from XX*.COS
C@NBI                 to *-XX.COS  (Seems more logical)
C@empa aw 1999nov09 Error Message improved
C@empa aw 1999nov11 Since CGO are not used anymore -P is not an option any longer.
C@empa aw 1999nov11 Check the "-" later, here we don't get an error message 
C                   if it is missing
C@empa aw 1999nov11 Check here also the "-" in front of the mode option,
C                   then we get an error message if it is missing
C@empa aw 1999nov11 check also the "-" in front of the mode option
C@empa aw 1999nov22 - OK, I agree, but for compatibility with IIsibat and UG I want 
C@empa                to leave it  XX*.COS. 
C@NBI PGS 1999May05 - New local variables Len_COSs & FirstOne      
C@NBI PGS 1999May05 - Now lists the opened files in .COF (Uses LOGICAL
C@NBI                 FirstOne to decide whether to write out header)
C@NBI PGS 1999oct17 - Bugfix: Option "FW" crashes because letter "W" is
C@NBI                 not tested for here.
C@empa aw 1999dec08 Part 0 is obsolet here, checks are also made in part 5. 
C@empa              We dont want to have the messages twice 
C@empa aw 2000jan10 Error message when concentration output if pollutant transport
C         is not calculated
C@empa aw 2000jan10 endif outpupoption(2) and C
C@empa aw 2000jan10 endif -S
C@empa aw 2000jan10 Lenstr
C@empa aw 2000jan26 ULine       
C@empa aw 2000jan26 uppercase of line in ULine for checkings
C@empa aw 2000jan26 check if occ is allowed
C@empa aw 2000mar21 insert parameter=1 also for H-Option but one position further.
C@empa              Pointer p added for that. Otherwise we crash with histograms
C@empa              for 'Global' entity      
C@empa aw 2000mar21 p added
C@empa need to increment k
C@NBI PGS 2000Aug01 - Tidied up routine; No syntax change; Moved comments up here
C@NBI PGS 2001Apr17 - Bugfix: EMPA's 2000nov08 modification prevented jump
C@NBI                 to label 135 because RETURNs when FlgEnd is TRUE above,
C@NBI                 so options FW, FB, RB, MZ, MB, EZ & EB stopped working.
C@NBI         remedy .Removed "linecnt" - it was only used to check whether
C@NBI                 "linecnt.gt.500" - unnecessary redundancy, because "FlgEnd"
C@NBI                 is good enough to check that end of file is reached.
C@NBI                .So label 119 no longer needed, GOTO 119 now routed to 120
C@NBI                .Added LOGICAL "Found" to decide wether to postrocess at end.
C@NBI                .Now only one exit point, at end of subroutine, so
C@NBI                 "CLOSE (CIF)" moved to end of subroutine, new label "900"
C
C USES: upperc
Ch**********************************************************************

      IMPLICIT NONE
      INCLUDE 'comv-uni.inc'
      INCLUDE 'comv-inp.inc'

C     local variables:
CC    INTEGER linecnt,k,i,j,p,LinTyp,FUnitNrCounter,FBNr,Len_COSs,LenStr
      INTEGER k,i,j,p,LinTyp,FUnitNrCounter,FBNr,Len_COSs,LenStr
      character*160 line,linewrd,ULine
      character*80 fileNam2
      character*2 word
      LOGICAL FirstOne,Found
      DATA FirstOne/.TRUE./
C-----

      variab='Reading CIF for PR-OUTP/PR-SIMU options.'
      KeyWTCnt=0
      KeyWHCnt=0
      FUnitNrCounter=StartFUnitForCOS
      IsTOpt=.FALSE.
      IsHOpt=.FALSE.
      calcfm=.FALSE.
      Found= .FALSE.

C@NBI PGS 1999May05 - Bugfix.  Replaced with new code patch below:
C@NBI PGS 2000Aug16 - This code causes problems on my NAG FTN90 compiler;
C@NBI                 This way of defining only substrings of fileNam2
C@NBI                 means that it isn't terminated by a nul-string, so
C@NBI                 the filename has many spurions characters to LEN.
C@NBI PGS 2000Aug16 - COS naming convention reinstated for personal use.
C@NBI                 To remove again just decomment lines with CC| comment
C@empa aw 2000dec04 - Ok, code below is obsolete but I keep "official"
C@empa                naming convention later on. 
CC|      i=1
CC|      j=3
CC|      fileNam2(1:2)='12'
CC|CCc   ! here filename(3:) is made equal to the previous COSs filename
CC|
CC|20    IF ((COSs(i:i) .EQ. ' ') .OR. (j .GE. 75) .or.
CC|     &    (COSs(i:i) .eq. '.')) GOTO 22
CC|         fileNam2(j:j)=COSs(i:i)
CC|         i=i+1
CC|         j=j+1
CC|      goto 20
CC|22    continue
CC|      fileNam2(j:j+3)='.COS'
C@NBI PGS 1999May05   (Start of patch)  COSs length is limited to 72 char.
C@NBI PGS 2000Nov05 - No longer reason to look for first "." COSs, as the
C@NBI                 root name may include a ".", eg. "..\cases\case1"
      DO Len_COSs=1,72
         IF (COSs(Len_COSs+1:Len_COSs+1) .EQ. ' ') GOTO 25
      ENDDO
25    CONTINUE
C@NBI PGS 1999May05   (End of patch)

C     ! Set Flags to default values
      do i=1,NumKeyW
         COSLine(i)=0
      ENDDO

C@tno jcp 1996Apr01_11:02:47 it is a pity that the input file is reopened and
C@ read again. I think this could have been done from inh/inpdat etc. @@@@
      OPEN(CIF,file=CIFs,access='sequential', status='old')
      FlgEnd=.False.
      nCifLine=0

C     ! find the PR-OUTPut section in the CIF
      variab='find the PR-OUTPut section'
110   call readlin(line,LinTyp,K,.True.)
C@empa aw 2000nov08 seccond entry after finding the first &-PR-OUTP/&-PR-SIMU 
111   continue
CC    if (FlgEnd) goto 118
      if (FlgEnd)THEN
         IF(Found) GOTO 135
         GOTO 900
CC    if (Lintyp.eq.TKEY)then
      ELSEif (Lintyp.eq.TKEY)then
CC       if (line(K+1:K+9).eq.'&-PR-OUTP') goto 119
CC       if (line(K+1:K+9).eq.'&-PR-SIMU') goto 119
         if (line(K+1:K+9).eq.'&-PR-OUTP') goto 120
         if (line(K+1:K+9).eq.'&-PR-SIMU') goto 120
      endif
      goto 110
CC 118     close(CIF)
CC        return


C-------------------------------------------------
C     Scan through lines in &-PR-OUTP or &-PR_SIMU
C-------------------------------------------------
      
CC 119   continue
CC    linecnt = 0

120   continue
      Found=.TRUE.
      variab='Reading Options at PR-OUTP or -SIMU'
      call readlin(line,LinTyp,K,.True.)
C@empa aw 2000nov29 need to increment k
          linewrd = line(k+1:)
      line    = linewrd
CC    linecnt = linecnt+1
c need to exit at EOF , next Keyword, more than 500 lines here at SIMU/OUTP
C@lbl bvs 1997Jul16 keep going if PR-OUTP or PR-SIMU next
CC	  if ((FlgEnd).or.(line(1:1).eq.'&')
CC     &                .or.(linecnt.gt.500)) goto 135
C@empa aw 2000nov08 repeat to look for &-PR-OUTP/&-PR-SIMU if next keyword is found 
CC          if ((line(1:9).eq.'&-PR-OUTP') .or. 
CC     &		(line(1:9).eq.'&-PR-SIMU')) goto 120
CC
CC          if ((FlgEnd) .or. (linecnt.gt.500) .or.
CC     &		(line(1:1).eq.'&')) goto 135

      if ((line(1:1).eq.'&').OR.FlgEnd) goto 111
CC    if ((FlgEnd) .or. (linecnt.gt.500)) goto 135
     		 
      call upperc(line(1:4))
      ULine=line
      call upperc(ULine)

C     -------------------------------
C     SubLoop to find keyword on line
C     -------------------------------

      do 130 i=1,NumKeyW
         if (KeyW(i)(1:2) .ne. line(1:2)) goto 130
C        ! ...valid keyword found

C        ! Is it a flow matrix ?
C@NBI PGS 2000Aug16 - EF replaced by Fn so we don't need to check for EF any more
CC       if (line(1:2).ne.'EF') then
         if ((index('AEIMNR',line(1:1)).ne.0).or.
     &      (line(1:2).eq.'FW').or.
     &      (line(1:2).eq.'FB')) then
            calcfm=.true.
         endif
CC       endif

C        ! Occupants are only allowed with -H option (histograms)
         p=5
         if ((line(3:4).eq.'-S').or.(line(3:4).eq.'-T')) then
            if (index(ULine,'OCC').ne.0) then
               call inerr('&-PR-OUTPut:  Occupant not allowed '//
     &         'with -S or -T mode',
     &         'Error in line: '//line,.FALSE.,2)
            endif
         else if (line(3:4).eq.'-H') then 
            p=p+1
         endif
             
C        ! Global keywords need no input parameter, so just add dummy parameter "1"
         if (   (line(2:2).eq.'A')
     &      .or.(line(2:2).eq.'B')
     &      .or.(line(2:2).eq.'W')) then
C           ! 'Global' entity => Parameter always set to '1'
            line(p:p+1)=' 1'
            line(p+2:80)=' '
         endif

C        -S (store)
C        ----------

         if (line(3:4).eq.'-S') then
C@NBI PGS 2000Aug02 - Add possibility to switch off production of all .COS files
C@NBI                 by setting empty COSs filename (in -SET file or command-line)
            IF(LenStr(COSs).EQ.0)THEN
C              ! Empty COSs filename (in -SET file or command-line) so do not output .COS file.  Do nothing here
CC          if ((outputoptions(2).eq. 0).and.(line(1:1).eq.'C')) then
            ELSEIF((outputoptions(2).eq.0).and.(line(1:1).eq.'C'))then
C@NBI PGS 2000Aug02   (end)
C@NBI PGS 2000Aug16 - COS naming convention reinstated for personal use.
C@NBI                 To remove again just decomment lines with CC| comment
C@NBI                 and comment-out next line.
                  fileNam2=COSs(1:Len_COSs)//'-'//KeyW(i)(1:2)//'.COS'
CC|               fileNam2=KeyW(i)(1:2)//COSs(1:Len_COSs)//'.COS'
               call upperc(fileNam2)
               call inerr('&-PR-OUTPut:  No output file: "'//
     &         fileNam2(:lenstr(fileNam2))//'" produced!;'//
     &         '"POLlutant" is missing under &-PR-SIMU! '//
     &         'Line ignored: ',line,.FALSE.,1)
C              ! Delete the corresponding *.COS from a preceding run.
               open(COSlun(i),file=fileNam2,status='unknown')
               close(COSlun(i),status='delete')
            else
C              ! Check for errors in 'line'
               call MkTab(line,i,0)
               if (COSline(i) .ne. 0) then
C                 ! File has already been open
C                 ! => error, science only one 'store' entry per
C                 ! keyword allowed !
                  call inerr('&-PR-OUTPut:  '//KeyW(i)(1:3)//
     &            ':  More than one -S(tore) entry encountered !',
     &            'line ignored: '//line,.FALSE.,1)
               else
C@NBI PGS 1999May05 - Changed fileNam2 from XX*.COS to *-XX.COS
C@NBI                 so that all files are grouped together alphabetically
C@NBI PGS 2000Aug16 - COS naming convention reinstated for personal use.
C@NBI                 To remove again just decomment lines with CC| comment
C@NBI                 and comment-out next line.
                  fileNam2=COSs(1:Len_COSs)//'-'//KeyW(i)(1:2)//'.COS'
CC|               fileNam2=KeyW(i)(1:2)//COSs(1:Len_COSs)//'.COS'
                  call lowerC(fileNam2)
                  if (FUnitNrCounter.gt.MaxFUnitNr) goto 930
                  COSlun(i)=FUnitNrCounter
                  OPEN(COSlun(i),file=fileNam2,access='sequential',
     &               form='formatted',
     &               status='unknown',err=920)
C@NBI PGS 1999May05 - Tell which files are openned
C@NBI PGS 2003Mar16 - Do this whenever a COF file is written.
                  IF(COF.NE.0)THEN
                    IF(FirstOne)THEN
                      FirstOne=.FALSE.
                      WRITE(CRT,'(/A)')
     &            ' THERE IS OUTPUT TO THE FOLLOWING SPREADSHEET FILES:'
                    ENDIF
                    WRITE(CRT,*)
     &                FunitNrCounter-StartFUnitForCOS+1,'   ',fileNam2
                  ENDIF
C@NBI PGS 1999May05   (end of patch)
                  FUnitNrCounter=FUnitNrCounter+1

C                 ! memorize ranges:
                  COSRange(i)=line

C                 ! increase counter for number of lines (=> >0,
C                 ! i.e. Keyword will be used by WriGrOut)
C                 ! (actual number of lines is COSline(i)-1)
                  COSline(i)=1
               endif
            endif

C        -T (tabulate mean values)
C        -------------------------

         else if (line(3:4).eq.'-T') then
C           ! check for errors in line
            if ((outputoptions(2).eq. 0).and.(line(1:1).eq.'C')) then
               call inerr('&-PR-OUTPut:  No mean values '//
     &         'for concentrations produced!;'//
     &         '"POLlutant" is missing under &-PR-SIMU! '//
     &         'Line ignored: ',line,.FALSE.,1)
            else
C              ! prepare arays for meanvalue calculation
               call MkTab(line,i,4)
               IsTOpt=.TRUE.
            endif

C        -H (histogram)
C        --------------

         else if (line(3:4).eq.'-H') then
C@NBI PGS 2000Aug02 - Add possibility to switch off production of all .COH files
C@NBI                 by setting empty COSs filename (in -SET file or command-line)
            IF(LenStr(COSs).EQ.0)THEN
C              ! Empty COSs filename (in -SET file or command-line) so do not output .COH file.  Do nothing here
C@empa aw 2000jan10 Error message when concentration output if pollutant transport
C         is not calculated
C@NBI PGS 2000Jul20 - Spelling/grammar correction etc.
CC          if ((outputoptions(2).eq. 0).and.((line(1:1).eq.'C')
            ELSEIF ((outputoptions(2).eq. 0).and.((line(1:1).eq.'C')
C@NBI PGS 2000Aug02   (end)
C@empa aw 2000dec04 "EF" was substituted by "Fn"
CC     &         .or.(line(1:2).eq.'EF'))) then
     &         .or.(line(1:1).eq.'F').and.(
     &            index('12345',line(2:2)).NE.0))) then
               call inerr('&-PR-OUTPut:  No histograms '//
     &         'for concentrations or effective flows produced! '//
     &         ' Keyword "POLlutant" is missing from &-PR-SIMU! '//
     &         'Following line is ignored: ',line,.FALSE.,1)
CC            else
C@empa aw 2000jan26 check occ with -H option
C@NBI PGS 2000Jul20 - Syntax error: I presume you mean ".NOT."
C@NBI                 Also, I assume you mean "(... .NE.0)" on 2nd line
C@NBI                 Can you verify this, Andreas?
C@empa aw 2000dec04 Ok, I agree
C@NBI               - Also, IF should be ELSEIF
CC             if (not((line(1:1).eq.'F').and.(
CC   &         index('12345zZ',line(2:2)).eq.1))
            ELSEif (.NOT.((line(1:1).eq.'F').and.(
     &            index('12345zZ',line(2:2)).NE.0))
     &            .and.(line(1:1).ne.'C')
     &            .and.(index(ULine,'OCC').ne.0))then
                  call inerr('&-PR-OUTPut:  Occupant not allowed '//
     &            'in histogram with this two letter keyword. '//
     &            'Only FZ, Fx and Cx can be used for occupants.  '//
     &            'The erroneous line is:',line,.FALSE.,2)
CC               endif
            ELSE
C              ! prepare arrays for Histogram calculation. Call part 5 in MkTab
               call MkTab(line,i,5)
               IsHOpt=.TRUE.
C@NBI PGS 2000Nov05 - List .COH files in .COF
               IF(OEcho.GT.0)THEN
                 IF(FirstOne)THEN
                   FirstOne=.FALSE.
                   WRITE(CRT,'(/A)')
     &           ' THERE IS OUTPUT TO THE FOLLOWING SPREADSHEET FILES:'
                 ENDIF
            fileNam2=COSs(1:Len_COSs)//line(3:LenStr(line(1:6)))//'.coh'
                 WRITE(CRT,*) '-    ',fileNam2
               ENDIF
            endif

C        unrecognized option
C        -------------------

         else
            call inerr('&-PR-OUTPut:  '//KeyW(i)(1:2)//
     &     ' : invalid output option mode! Valid modes are: -S  -T  -H',
     &      'line ignored: '//line,.FALSE.,1)
         endif
130   continue

C     ! loop until end of &-PR-OUTP/&-PR-SIMUlaton section
      goto 120

C-----------------
C     End of loops
C-----------------

C     Check for FB-T option, which needs FB-S option
C     ----------------------------------------------

135   continue
      IF(OEcho.GT.0) WRITE(CRT,*)
      close(CIF)
      do i=1,NumKeyW
         if (KeyW(i).eq.'FB-') FBNr=i
      ENDDO
      j=1

210   continue
      if (IOOptT(1,j).eq.FBNr) then
         if (COSLun(FBNr).eq.9) then
C@NBI PGS 1999May05 - Changed fileNam2 from XX*.COS to *-XX.COS
            call inerr('&-PR-OUTP: FB-T : Output file *-FB.COS '//
     &      'has not been opened !',
     &      '(FB-S option or the separator string under '//
     &      'TABLES in COMIS.SET is missing!)',.false.,2)
            goto 220
         endif
      endif
      j=j+1
      if ((IOOptT(1,j).gt.0).and.(j.le.MaxOOT)) goto 210

C     Check for FW-T option, which needs FW-S option
C     ----------------------------------------------

220   continue
      do i=1,NumKeyW
         if (KeyW(i).eq.'FW-') FBNr=i
      ENDDO
      j=1

410   continue
      if (IOOptT(1,j).eq.FBNr) then
         if (COSLun(FBNr).eq.9) then
C@NBI PGS 1999May05 - Changed fileNam2 from XX*.COS to *-XX.COS
CC          call inerr('&-PR-OUTP: FW-T : Output file FW*.COS '//
            call inerr('&-PR-OUTP: FW-T : Output file *-FW.COS '//
     &      'has not been opened !',
     &      '(FW-S option or the separator string under '//
     &      'TABLES in COMIS.SET is missing!)',.false.,2)
            goto 420
         endif
      endif
      j=j+1
      if ((IOOptT(1,j).gt.0).and.(j.le.MaxOOT)) goto 410

C     Check for options RB, MZ, MB, EZ & EB
C     -------------------------------------

420   continue
      j=1
      do i=1,numkeyw
C@NBI PGS 2000Aug17 - Bugfix. If only the -T option was used for these
C@NBI                 keywords, the output was zero!  The -T option for
C@NBI                 these keywords does not need presence of -S option.
CC       if (COSline(i).gt.0) then
         word=keyw(i)(1:2)
C@NBI PGS 2001Apr17 - Was option "EB" accidentally omitted here?
CC       if (index('RB MZ MB EZ',word).gt.0)  InvMatrix=1
         if (index('RB MZ MB EZ EB',word).gt.0)  InvMatrix=1
CC       end if
      ENDDO
900   close(CIF)
      RETURN

C-------------------
C     Error handling
C-------------------

C     ! error opening COS file --- fatal
920   call inerr('cannot open COS file for writing',KeyW(i),.FALSE.,3)

C     ! error not enough FileUnitNumbers, too many Outputoption Keywords --- fatal
930   call inerr('cannot open so many COS Files',' ',.FALSE.,3)

      END
