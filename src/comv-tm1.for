C+*********************************************************** comv-tm1.f
Ch**********************************************************************

        FUNCTION EMPTY(Line)
C***********************************************************************
C lbl bs mar25, 1991
C
C Purpose:
C The value returned by this function is .TRUE. if the line consists
C only of blanks or binary zeros. Otherwise .FALSE. is returned.
C@tno jcp 1996May28_10:29:38 whatch out, in EMPTY(Line) TAB's are not empty!
Ch**********************************************************************
        IMPLICIT NONE
        CHARACTER*(*) Line
        INTEGER l,i
        LOGICAL empty
        INTEGER LenStr
        l=lenstr(Line)
        EMPTY=.FALSE.
        DO 20 I=1,l
           IF (ichar(Line(i:i)).NE.0 .AND. Line(i:i).NE.' ') GOTO 99
20      CONTINUE
        EMPTY=.TRUE.
99      RETURN
        END
  
Ch**********************************************************************

        SUBROUTINE STRIP(word,first,last)
C***********************************************************************
C lbl bs mar26, 1991
C
C Purpose:
C Find the first and the last non-blank position in the word
C
C Parameters:
C
C (I)   word            string to test
C (O)   first           position in word
C (O)   last            position in word
Ch**********************************************************************
        IMPLICIT NONE
        CHARACTER*(*) word
        INTEGER first,last,l

        l=len(word)
        first=1
10      IF (word(first:first).EQ.' ' .AND. first.LT.l) THEN
           first=first+1
           GOTO 10
        ENDIF
        last=first
20      IF (word(last:last).NE.' ' .AND. last.LT.l) THEN
           last=last+1
           GOTO 20
        ENDIF
        last=last-1
        RETURN
        END
Ch**********************************************************************

        SUBROUTINE GETLAST(Line,Word)
C***********************************************************************
C
C lbl bs 26mar, 1991
C
C Purpose:
C Reads the last word of the line into the parameter "word".
C
C Parameters:
C (I)    Line
C (O)    Word
Ch**********************************************************************
        IMPLICIT NONE
        CHARACTER*(*) Line,Word
        CHARACTER ch
        INTEGER l,i
        INTEGER LenStr

        l=lenstr(Line)
        i=l
10      continue
        ch=Line(i:i)
        IF (ch.NE.' ' .AND. ICHAR(ch).NE.9 .AND. i.GT.1) THEN
           i=i-1
           GOTO 10
        ENDIF
        Word=Line(i+1:l)

        RETURN
        END

Ch**********************************************************************

        SUBROUTINE READDAF(NSteps,elast,efirst)
C***********************************************************************
C lbl bs mar19, 1991
C
C Purpose:
C Reads parts of the direct access file and writes into the TMS file.
C NSteps (Output parameter) = number of datalines in the TMS file.
C elast  (Input parameter) = time of last file entry in old TMS file.
C efirst (Input parameter) = time of first file entry used.
C
C@lbl bs 1991aug05  parameter 'reuse' deleted. Routine gets info from
C@lbl    common block.
Ch**********************************************************************
        IMPLICIT NONE
        INCLUDE 'comv-uni.inc'
        INCLUDE 'comv-inp.inc'
        INTEGER IcIndex

C MaxSch = number of schedules
C pSch   = pointer to the first record of the schedule in the DAF
C pLSch  = pointer to the last  record of the schedule in the DAF
C pSchNa = pointer to the first name of the schedule in aName
        INTEGER MaxSch
        PARAMETER (MaxSch=11)
        INTEGER pSch(MaxSch), pLSch(MaxSch), pSchNa
        INTEGER iocheck,CifEnd
        INTEGER LenStr
C@tno jcp 1996Jun26_13:48:46 posmeteo added
        INTEGER NSteps,k,l,i,j,pos1,pos2,elast,efirst,posmeteo
C@lbl bvs 1999Mar11 increased length of flname from 40 to 160
        CHARACTER SchNam*10, dum*10, tmword*30, flname*160
        CHARACTER RecLine*160, Line*160, sortopt*7
        LOGICAL empty,InTime,Before
C@tno jcp 1996Jun21_17:17:27 definition schedule numbers definition
C definition scheduletype numbers definition
C keyword  name     schedule number
C       1 =' CIF'     -
C       2 ='PR-IDEN'  -
C       3 ='PR-SIMU'  -
C       4 ='PR-CONT'  -
C       5 ='NET-AIR'  -
C       6 ='CR'       -
C       7 ='FA'       -
C       8 ='DS'       -
C       9 ='DF'       -
C       10='F1'       -
C       11='F2'       -
C       12='F3'       -
C       13='F4'       -
C       14='WI'       -
C       15='TD'       -
C       16='TRANSIT'  -
C       17='NET-HVA'  -
C       18='NET-ZON'  -
C       19='NET-ZL'   -
C       20='NET-ZP'   -
C       21='NET-EXT'  -
C       22='NET-LIN'  -
C       23='SCH-MAI'  01
C       24='SCH-LIN'  02
C       25='SCH-WIN'  03
C       26='SCH-FAN'  04
C       27='SCH-TEM'  05
C       28='SCH-HUM'  06
C       29='SCH-SIN'  07
C       30='SCH-SOU'  08
C       31='SCH-OCC'  09
C       32='CP-BUIL'  -
C       33='CP-VALU'  -
C       34='ENV-BUI'  -
C       35='ENV-WIN'  -
C       36='SCH-MET'  10
C       37='POL-DES'  -
C       38='SCH-POL'  11
C       39='OCCUPAN'  -
C       40='CPR-BUI'  -
C       41='CPR-FAC'  -
C       42='CPR-WIN'  -
C       43='3D-BUIL'  -
C       44='NORM-CR'  -
C       45='SCH-MUL'  - <---has no number here
C       46='USER'     -
C       47='NET-ZT'   -
C       48='PR-OUTP'  -
C       49='PR-UNIT'  -
C       50='WALL-MA'  -
C       51='WALL-TY'  -
C       52='NET-WAL'  -
C       53='PE'       -
C       54='RF'       -
C       55='HISTO'    -
C       56='PS'       -
C       57='EQN-WIN'  -
C       58='POL-FIC'  -
C@NBI PGS 2003Apr28 - New component type: Thermostatic vent
C       59='TV'       -

c       call ho('in readDaf 1','')
C init pSch array  ------------------------------------------------
C@tno jcp 1996Jun21_17:26:00 pKeyRec points to a record in DAF if this Keyword
C is found in the input file

        DO 20 I=1,9
           pSch(I)=pKeyRec(I+22)
20      CONTINUE
        pSch(10)=pKeyRec(36)
        pSch(11)=pKeyRec(38)

C init pLSch array  ------------------------------------------------
C I have to do that in a loop because the keywords in
C the input file can be in arbitrary sequence.
        DO 21 I=1,11
           IF (pSch(I).EQ.0) GOTO 21
           k=9999
C determine the last line per schedule in the direct access file DAF
C This is done by looking for the start of the first keyword after each schedule
C@tno jcp 1996Jun21_17:31:27 45=keyword number of last schedule (SCH-MUL)
           DO 22 J=1,45
              IF (pKeyRec(J).GT.pSch(I) .AND. pKeyRec(J).LT.k) THEN
                 k=pKeyRec(J)
              ENDIF
22         CONTINUE
           pLSch(I)=k-1
           IF (pLSch(I).EQ.9998) pLSch(I)=-1
21      CONTINUE
C at the end of this part pLSch(I) can be -1, when the
C schedule I is the last in the .CIF file
c       call ho('in readDaf 2','')


C -------------------------------------------------------
C read DAF and create array to sort the data
C -------------------------------------------------------
        CifEnd=pDAF-1
        DO 30 I=1,11

c       call ho('in readDaf 2.1','')
C SchNam='--' means that there is no name for the schedule
           SchNam='--'
           pSchNa=0
           IF (pSch(I).NE.0) THEN
c       call ho('in readDaf 2.2','')
              IF (pLSch(I).EQ.-1) pLSch(I)=CifEnd
              DO 40 J=pSch(I)+1,pLSch(I)
                 READ(DAF,REC=J,IOSTAT=iocheck) RecLine
       
C@tno jcp 1996Jun11_17:59:21 try to keepline from DAF
                 call KeepLine(RecLine)

                 IF (iocheck.NE.0) GOTO 35
                 IF (empty(RecLine)) GOTO 35
                 l = lenstr(RecLine)
C schedules with 2 headers, first header processed in ELSE-branch
                 IF (I.GT.1 .AND. I.LT.10 .OR. J.GT.(pSch(I)+1)) THEN
c       call ho('in readDaf 2.3','not first header')

C@tno jcp 1996Jun21_17:33:49 try to analyse SCH-POL
C if the word 'meteo' is found, it is replaced by -1.
C -1 is an impossible H2O concentration and at timestep we will fill out the
C last value read from the meteo schedule for H2O. But only if ipolH2O>0,
C otherwise H2O is not modelled at all
                 if (i.eq.11) then
C SCH-POL
c                 call ho('in READ DAF','at schedule 11')
                   if (ipolH2O.gt.0) then
C H2O simulated as pollutant
                     posmeteo=ICindex(RecLine,'METEO')
                     if (posmeteo.gt.0) then
C 'meteo' found
c                       write(*,*) recline
C@NBI PGS 1999Aug09 - Bugfix.  COMIS crashed when there was only 1 space or
C@NBI                 tab between the time string and the 'meteo'
CC                     call replaceword(RecLine,posmeteo-1,'-1')
                       call replaceword(RecLine,posmeteo,'-1')
c                       write(*,*) recline
c                       call ho('SCH-POL meteo found in READ DAF','')
C@NBI PGS 1999Aug09 - To avoid unnecessary repetition, this should only be
C@NBI                 done when UseMetH2O = 1
CC                   else
                     elseIF (UseMetH2O.EQ.1) THEN
C the word 'meteo' is not found, values might be given for outdoor H2O so:
C donot use the values from meteo
c                       write(*,*) 'Sch-POL',recline,' has no meteo'
C@NBI PGS 1999Aug09 - Make this a clear message
C@empa aw 2000apr05 Message slightly changed
C@NBI PGS 1999Aug09 - H2O spells with "O" not "zero"
C@empa aw 2000dec07  The semicolon after &-SCH-POL forces a linefeed  
C@                   at the correct place. (without we get an empty line)
                       CALL ERROR2('If you want outdoor concentration '
     &                 //'of H2O to be automatically taken from meteo ',
     &                 'data, type ''meteo'' in the H2O column '
     &                 //'in datablock &-SCH-POL, '
     &                 //'or leave &-SCH-POL;empty',0)
                       UseMetH2O=0
                     end if
                   end if
                 end if
C@tno jcp 1996Jun21_17:33:49 end analyse SCH-POL

C determine if there is a new name
                    k=index(RecLine,'*')-1

C@tno jcp 1996Jun10_14:16:44 Patch F: *schedulename
                    if (index(RecLine,'F:').gt.0) then
                      k=-1
                    end if
C@tno jcp 1996Jun27_16:04:47 allow F: and f: to signal a file
                    if (index(RecLine,'f:').gt.0) then
                      k=-1
                    end if
C@tno jcp 1996Jun10_14:16:44 end Patch F: or f:

                    IF (k.NE.-1) THEN
C@tno jcp 1996Jul02_22:48:59 wrtBefore writes values of previous schedule at sta
C TMS and DAF
c                        write(*,*) RecLine
c                        call ho('in Read daf found name',' ')
                        call wrtBefore
c                        call ho('after WrtBefore',' ')
                        CALL GETWRD(RecLine,k,l,SchNam)
                        CALL STRIP(SchNam,pos1,pos2)
                        pSchNa=ICindex(aName,SchNam(pos1:pos2+1))
                    ELSE
C look for input file assignment
                        k=index(RecLine,'F:')
C@tno jcp 1996Jun27_16:04:47 allow F: and f: to signal a file
                        if (k.eq.0) then
                          k=index(RecLine,'f:')
                        end if

                        IF (k.GT.0) THEN
C@tno jcp 1996Jul02_22:48:59 wrtBefore writes values of previous schedule at sta
C TMS and DAF
C                        write(*,*) RecLine
C                        call ho('in Read daf found F:',' ')
                        call wrtBefore
                           k=k+1
                           RecLine(k:k)=' '
                           CALL GETWRD(RecLine,k,l,SchNam)
                           CALL STRIP(SchNam,pos1,pos2)
C@tno jcp 1996Jun10_14:11:27 allow an F: schedule name to start with *
                           if (SchNam(pos1:pos1).eq.'*') then
                             pos2=pos2-1
                           else
                             dum(2:)=SchNam(pos1:pos2+1)
                             dum(1:1)='*'
                             SchNam(pos1:pos2+2)=dum(1:)
                           end if
C@tno jcp 1996Jun10_14:11:27 end
                           pSchNa=ICindex(aName,SchNam(pos1:pos2+2))
                           CALL GETWRD(RecLine,k,l,flname)
C@tno jcp 1996Jun13_23:48:56 added variab= before getWS sortopt
          variab='the sortoption from a schedule file'
                           CALL GETWS(RecLine,k,l,sortopt,' ')
                           CALL UpperC(sortopt)
c                           write(*,*)'sortopt 1',ICHAR(sortopt(1:1))
c                           write(*,*)'sortopt 2',ICHAR(sortopt(2:2))
c                           write(*,*)'sortopt 3',ICHAR(sortopt(3:3))
c                           write(*,*)'sortopt 4',ICHAR(sortopt(4:4))
c                           write(*,*)'sortopt 5',ICHAR(sortopt(5:5))
c                           write(*,*)'sortopt 6',ICHAR(sortopt(6:6))
c                           write(*,*)'sortopt 7',ICHAR(sortopt(7:7))
CC                           IF (sortopt.NE.' '.AND.sortopt.NE.'-SORTED')
CC     &                     sortopt=' '
                           IF (sortopt.NE.'-SORTED')
     &                     sortopt=' '
c from here the sortopt=' ' and only '-SORTED' if that was the input

C@lbl bvs 1998Dec03 check for error in opening schedule file
CC                           OPEN(IFS,file=flname, STATUS = 'OLD')
                           OPEN(IFS,file=flname, STATUS = 'OLD',ERR=49)

c the next call to DuplTm writes the array in DuplTM to the TMS file
                           IF (sortopt.NE.' ')
     &                     CALL DUPLTM(0,0,0,' ',sortopt,NSteps,
     &                     InTime,RecLine)
                           Before=.TRUE.
                           READ(IFS,'(A)',END=49) Line
C write data from IFS file to DAF file
31                         IF (.NOT.empty(Line)) THEN
                              CALL FINDTIME(Line,tmword)
                              CALL DUPLTM(pDAF,I,pSchNa,tmword,
     &                        sortopt,NSteps,InTime,Line)
                              IF (InTime) THEN
                                 Before=.FALSE.
                                 WRITE(DAF,REC=pDAF) Line
                                 pDAF=pDAF+1
                              ENDIF

                              IF(.NOT.(InTime.OR.Before).AND.
     &                        sortopt.NE.' ') GOTO 38
c exit if we are after the simulation interval

                           ENDIF
                           READ(IFS,'(A)',END=38) Line
                           GOTO 31

38                         CLOSE(IFS)
                           CALL DUPLTM(0,0,0,' ',sortopt,NSteps,
     &                     InTime,Line)
                           GOTO 40
                        ENDIF
c end if k>0   'f:'
                    ENDIF
c                    call ho('after endif of name','')
c end if k=-1  no name or 'f:'
                    sortopt=' '
c                    call ho('before findtime','')
                    CALL FINDTIME(RecLine,tmword)
C@tno jcp 1996May03_12:33:01 temp
c                    write(*,*)'ReadDaf, tmword=',tmword
c                    call ho('after findtime','')

C@tno jcp 1996Jul04_14:52:59 here is an error. I come from above, no F: and
C have the data in RecLine, while here Line is written, which is empty.
C Why Line AND RecLine. Can I change it to RecLine, or could the data
C sometimes be in Line.
c                    write(*,*) '   line=',line
c                    write(*,*) 'Recline=',Recline
c                    call ho('before Dupltm','')

                    CALL DUPLTM(J,I,pSchNa,tmword,sortopt,NSteps,
CC     &              InTime,Line)
     &              InTime,RecLine)
c                    write(*,*)'after Dupltm, tmword=',tmword
c                    call ho('after Dupltm','')


                 ELSE
C first line of the schedules 1,10 or 11



c                    call ho('schedules 1,10,11','first part')
                    k=index(RecLine,'F:')
C@tno jcp 1996Jun27_16:04:47 allow F: and f: to signal a file
                        if (k.eq.0) then
                          k=index(RecLine,'f:')
                        end if
                    IF (k.GT.0) THEN
C file assignment without schedule name
                        k=k+1
                        RecLine(k:k)=' '
                        CALL GETWRD(RecLine,k,l,flname)
C weather file ?
                        IF (I.EQ.10) THEN
                           metfile=flname
c                           call ho('filename=',flname)
C option added to read a DOE-2 weather file
C@tno jcp 1996Jun13_23:48:56 added variab= before getWS sortopt
          variab='the Option Comis/DOE-2 for the meteo file'
                           CALL GETWS(RecLine,k,l,metopt,'COMIS')
                           CALL UPPERC(metopt)
                           IF (metopt.NE.'COMIS'.AND.metopt.NE.'DOE2')
     &                     THEN
                               CALL ERROR2('&-SCH-MET: Wrong '//
     &                         'filename option in meteo schedule: '//
     &                          metopt//' !',
     &                         'Using default option: COMIS !',0)
                               metopt='COMIS'
                           ENDIF
                           GOTO 40
                        ENDIF

c below here we only have schedules 1 (main) and 11 (outdoor pollutant)
c                       call ho('before sortoption from RecLine','')
C@tno jcp 1996Jun13_23:48:56 added variab= before getWS sortopt
          variab='sortoption for SCH-MAI/SCH-POL'
                        CALL GETWS(RecLine,k,l,sortopt,' ')
                        CALL UpperC(sortopt)
c                           write(*,*)'sortopt 1',ICHAR(sortopt(1:1))
c                           write(*,*)'sortopt 2',ICHAR(sortopt(2:2))
c                           write(*,*)'sortopt 3',ICHAR(sortopt(3:3))
c                           write(*,*)'sortopt 4',ICHAR(sortopt(4:4))
c                           write(*,*)'sortopt 5',ICHAR(sortopt(5:5))
c                           write(*,*)'sortopt 6',ICHAR(sortopt(6:6))
c                           write(*,*)'sortopt 7',ICHAR(sortopt(7:7))
CC                        IF (sortopt.NE.' '.AND.sortopt.NE.'-SORTED')
CC     &                  sortopt=' '
                           IF (sortopt.NE.'-SORTED')
     &                     sortopt=' '
C@lbl bvs 1998Dec03 check for error in opening schedule file
CC                        OPEN(IFS,file=flname, STATUS = 'OLD')
                        OPEN(IFS,file=flname, STATUS = 'OLD',ERR=49)
                        Before=.TRUE.
                        READ(IFS,'(A)',END=49) Line
C write data from IFS file to DAF file
c                        call ho('dupltm for IFS file','')
                        IF (sortopt.NE.' ')
     &                  CALL DUPLTM(0,0,0,' ',sortopt,NSteps,
     &                  InTime,Line)
c                        call ho('before loop 41','')
41                      IF (.NOT.empty(Line)) THEN
                           CALL FINDTIME(Line,tmword)
                           CALL DUPLTM(pDAF,I,pSchNa,tmword,sortopt,
     &                     NSteps,InTime,Line)
                           IF (InTime) THEN
                              Before=.FALSE.
                              WRITE(DAF,REC=pDAF) Line
                              pDAF=pDAF+1
                           ENDIF
                           IF(.NOT.(InTime.OR.Before).AND.
     &                     sortopt.NE.' ') GOTO 48
                        ENDIF
                        READ(IFS,'(A)',END=48) Line
                        GOTO 41

48                      CLOSE(IFS)
                        CALL DUPLTM(0,0,0,' ',sortopt,NSteps,
     &                   InTime,Line)
                        GOTO 40
                    ENDIF
C error: wrong filename
                 ENDIF
c                 call ho('after end if of first l schedule 1,10,11','')
                 GOTO 40
C@lbl bvs 1998Dec03 file may be any schedule, not just weather
CC49             CALL ERROR2('&-SCH-MET: The specified weather '//
49               CALL ERROR2('&-SCH-???: The specified schedule '//
C@NBI PGS 2000dec23 - Should be severe so that COMIS aborts before starting
C@NBI                 simulation, not just a warning, otherwise the error
C@NBI                 may go unnoticed!
CC     &           'file does not exist or is empty: ',flname,1)
     &           'file does not exist or is empty: ',flname,2)
                 CLOSE(IFS,status='DELETE')
40            CONTINUE
           ENDIF
C@tno jcp 1996Jul04_16:34:02 inserted lines at LABEL 30 TO CALL WRTBEFORE
        call wrtbefore()
30      CONTINUE

c       call ho('in readDaf 3','na loop 30')


C@tno jcp 1996Jul04_12:04:16 35 moved to here to catch the next lines
35      continue
c        call ho('at the end if readdaf','')
C@tno jcp 1996Jul04_11:42:39 write any last schedules just-before data into Dupl
        call wrtBefore()

C write the rest of the data from the array in the TMS file --------
C@tno jcp 1996Jul04_12:04:42 label 35 moved up to call wrtBefore
CC35      CALL DUPLTM(0,0,0,' ',sortopt,NSteps,InTime,Line)
        CALL DUPLTM(0,0,0,' ',sortopt,NSteps,InTime,Line)
C complete TMS file using the data from the last run  --------------
        IF (cont.EQ.'CONT') CALL RESTTMS(NSteps,elast,efirst)
        RETURN
        END

Ch**********************************************************************

        SUBROUTINE RDPERIOD
C***********************************************************************
C
C bs mar19, 1991
C
C Purpose:
C reads start and stop time from the CIF and stores the
C time data in COMMON variables
C If the start or the stop time is omitted, 1900jan01 is assumed as a
C default.
C This routine also reads the schedule time, that is the time the
C program starts reading the schedules before the simulation. The
C default is schedule time = start time.
C Changes:
C@lbl bvs 1994May17 Keep track of input line for error reporting
C@empa aw 1995feb27 Start- and stoptime are in "&-PR-SIMU"-section now
C@empa aw 1995mar10 Lintyp added
C@empa aw 1995mar30 message added, when default values for start or stop time us
C@empa aw 1995mar30 initial values for cont and keep added.
C@tno jcp 1996Jun10_13:45:34 loop through both SIMU and OUT to search for
Cstart and stop times and fill in the time span of the meteo if no time is found
C@empa aw 2005apr26 Allow "default" (1900jan01_00:00:00) for tmstart, tmstop, tmsched and tmhist 
C@empa aw 2005apr26 1900 is used if year is not specified  
C@empa aw 2005apr26 1900jan01 is used if day is not specified
Ch**********************************************************************
        IMPLICIT NONE
        INCLUDE 'comv-uni.inc'
        INCLUDE 'comv-inp.inc'

C@empa aw 2000jan14 errkey2
CC        INTEGER k,l,errkey,Lintyp
        INTEGER k,l,errkey,errkey2,Lintyp
        INTEGER LenStr
        INTEGER JdaySecSim,juldif
        CHARACTER RecLine*160,word*160
C@tno jcp 1996Apr25_3:56:32 tmhist added
C@tno jcp 1996Jul03_13:19:25 Tmstart moved to commonblock
CC        CHARACTER*40 tmstart,tmstop,tmsched,tmhist
        CHARACTER*40 tmstop,tmsched,tmhist
C@tno jcp 1996Jun10_13:03:32 flags for &-PR-OUT &-PR-SIMU found
        LOGICAL FlgOut,FlgSimu

        FlgOut =.FALSE.
        FlgSimu=.FALSE.

        tmstart=' '
        tmstop =' '
        tmsched=' '
        tmhist =' '
        cont=' '
        keep=' '

        OPEN(CIF,file=CIFs,status='old')
        FlgEnd = .FALSE.
C@tno jcp 1996Jul08_17:14:46 nCifLine the line number is reset after opening cif
        nCifLine=0

C       find the PR-OUTPut or &-PR-SIMUlation section in the CIF
C       REPEAT

11      call readlin(Recline,LinTyp,K,.True.)

10      call KeepLine(Recline)

        if (FlgEnd) then
C@lbl bvs 1997Aug22 needed extra parentheses around (FlgOut.eqv..FALSE.) etc.
CC          if (FlgOut.eqv..FALSE. .and. FlgSimu.eqv..FALSE.) then
          if ((FlgOut.eqv..FALSE.) .and. (FlgSimu.eqv..FALSE.)) then
           goto 29
          else
           goto 30
          end if
        end if
        if (LinTyp.eq.TKEY)then
C Start- and stoptime are in "&-PR-SIMU"-section now
C However, for backward compatibility, allow user to keep that info in PR-OUTP
           if (Recline(K+1:K+9).eq.'&-PR-OUTP') then
             FlgOut=.TRUE.
c             write(cof,*) 'RDPERIOD PR-Out seen',recline
C@empa aw 1995dec22 Check whether &-PR-OUTP stands alone (no &-PR-SIMU follows)
C          now scan for &-PR-SIMU
12             call readlin(Recline,LinTyp,K,.True.)
               call KeepLine(Recline)
               if (FlgEnd) then
C                &-PR-SIMU not found: spool to &-PR-OUTP again
                 rewind(CIF)
C@empa aw 2005jun07 reset also nCifLine
                 nCifLine=0        

C@lbl bvs 1997Jan24 "false" should be .false.
CC                 FlgEnd=false
                 FlgEnd=.false.
13               call readlin(Recline,LinTyp,K,.True.)
                   call KeepLine(Recline)
                   if (LinTyp.eq.TKEY)then
                     if (Recline(K+1:K+9).eq.'&-PR-OUTP') goto 20
                   endif
                 goto 13
               endif
               if (LinTyp.eq.TKEY)then
C                &-PR-SIMU found: process this section
                 if (Recline(K+1:K+9).eq.'&-PR-SIMU') goto 20
               endif
               goto 12
           endif
C@empa aw 1995dec22 end
           if (Recline(K+1:K+9).eq.'&-PR-SIMU') then
             FlgSimu=.TRUE.
c             write(cof,*) 'RDPERIOD PR-Simu seen',recline
             goto 20
           end if
        endif
        goto 11

C       now scan through the CIF
C     REPEAT

20      call readlin(Recline,LinTyp,K,.True.)
        call KeepLine(Recline)
        if (FlgEnd) goto 30
        if (LinTyp .eq. TKEY) then
          if (FlgOut.eqv..FALSE. .or. FlgSimu.eqv..FALSE.) then
C one of the keywords has not been read yet, continue the loop through the file
c             write(cof,*) 'continue Loop for next OUT/SIMU keyword',
c     &       recline
            goto 10
          else
C &..OUTP and &..SIMU have been read
            goto 30
          end if
        end if
C@empa aw 2000apr05 Variab strings changed
        l = lenstr(RecLine)
        CALL GETWRD(RecLine,k,l,word)
        IF (word(1:5).EQ.'START') THEN
           variab='STARTtime'
C@empa aw 2005apr25 use GetWS to have a default 
CC           CALL GETWRD(RecLine,k,l,tmstart)
           CALL GETWS(RecLine,k,l,tmstart,' ')

c           write(cof,*) 'RDPERIOD start time=',tmstart
C@tno jcp 1996Jun13_23:48:56 added variab= before getWS sortopt
           variab='the word after the STARTtime'
           CALL GETWS(RecLine,k,l,cont,' ')
        ELSE IF (word(1:4).EQ.'STOP') THEN
         variab='STOPtime'
C@empa aw 2005apr25 use GetWS to have a default 
CC         CALL GETWRD(RecLine,k,l,tmstop)
           CALL GETWS(RecLine,k,l,tmstop,' ')
c           write(cof,*) 'RDPERIOD stop  time=',tmstop
C@tno jcp 1996Jun13_23:48:56 added variab= before getWS sortopt
           variab='the word after the STOPtime'
           CALL GETWS(RecLine,k,l,keep,' ')
        ELSE IF (word(1:5).EQ.'SCHED') THEN
         variab='SCHEDtime'
C@empa aw 2005apr25 use GetWS to have a default 
CC         CALL GETWRD(RecLine,k,l,tmsched)
           CALL GETWS(RecLine,k,l,tmsched,' ')
c           write(cof,*) 'RDPERIOD sched time=',tmsched
C@tno jcp 1996Apr25_13:59:36 added tmhist
        ELSE IF (word(1:5).EQ.'HISTO') THEN
           variab='HISTOtime'
C@empa aw 2005apr25 use GetWS to have a default 
CC           CALL GETWRD(RecLine,k,l,tmHist)
           CALL GETWS(RecLine,k,l,tmHist,' ')
c           write(cof,*) 'RDPERIOD Hist time=',tmHist
        ENDIF
C@tno jcp 1996Apr15_17:36:16 if all are filled then we donot goto 20 and run
C into the 29 error message that &PR-SIMU is missing, changed the if structure
C@tno jcp 1996Apr25_13:59:36 added tmhist
        IF ( (tmstop.EQ.' ' .OR. tmhist.EQ.' '
     &          .OR. tmstart.EQ.' ' .OR. tmsched.EQ.' ')) then
          GOTO 20
        else
          GOTO 30
        end if
C     END REPEAT

29      continue
C@tno jcp 1996Apr26_22:42:04 changed missing PR-SIMU to a warning.
C if there is meteo data directly in CIF then we will use those times
        CALL INERR('&-PR-SIMUlation section is missing!',
     &  ' ',.FALSE.,1)
        If (MetTimeStart.ne.' '.and.MetTimeStop.ne.' ') then
           tmstart=MetTimeStart
           tmsched=tmstart
           tmhist =tmstart
           tmstop =MetTimeStop
          Write(Cof,*)'Default Start and Stop times from '//
     &      '&-SCH-METeo data have been used:'
          Write(Cof,*)'Start =',tmstart
          Write(Cof,*)'Stop  =',tmstop
        else
          CALL INERR('&-PR-SIMUlation and &-SCH-METeo section are '//
     &    'missing!',
     &    'You may want to add one or both to the input ',.FALSE.,1)
        end if


30      continue
        CLOSE(CIF)

C@tno jcp 1996Apr27_00:40:13 errors changed into warnings. A timeless simulation
C must be possible.
C set defaults
C Message when default values for start or stop time used.

C@lbl bvs 1997Jul28 make VENT output default if no output options specified
        if (outputoptions(1).eq.0 .and.
     &       tmstart.EQ.' ') then
          outputoptions(1)=1
        end if

        IF (tmstart.EQ.' ') THEN

          If (MetTimeStart.ne.' ') then
          CALL RTIMDAT(MetTimeStart,jdstart,IYYY1,MM1,ID1,H1,M1,S1,
     &    errkey,variab)
            if (jdstart.gt.1721788) then
              tmstart=MetTimeStart
              Write(Cof,*)'Default Start time from '//
     &        '&-SCH-METeo data has been used:'
              Write(Cof,*)'Start =',tmstart
            else
              tmstart='1900jan01_'
              CALL INERR('&-PR-SIMU: Start time not found '//
     &        'and the meteo time schedule has no Year. ',
     &        'Default value: "1900jan01_" used ',.FALSE.,1)
            end if
          else
            tmstart='1900jan01_'
            CALL INERR('&-PR-SIMU: Start time not found ',
     &      'Default value: "1900jan01_" used ',.FALSE.,1)
          end if
        ENDIF
        IF (tmstop.EQ.' ') THEN
          If (MetTimeStop.ne.' ') then
          CALL RTIMDAT(MetTimeStart,jdstart,IYYY1,MM1,ID1,H1,M1,S1,
     &    errkey,variab)
            if (jdstart.gt.1721788) then
              tmstop=MetTimeStop
              Write(Cof,*)'Default Stop time from '//
     &        '&-SCH-METeo data has been used:'
              Write(Cof,*)'Stop  =',tmstop
            else
              tmstop ='1900jan01_'
              CALL INERR('&-PR-SIMU: Stop time not found ',
     &        'Default value: "1900jan01_" used ',.FALSE.,1)
            end if
          else

            tmstop ='1900jan01_'
            CALL INERR('&-PR-SIMU: Stop time not found ',
     &      'Default value: "1900jan01_" used ',.FALSE.,1)
          end if
        ENDIF

        IF (tmsched.EQ.' ') tmsched=tmstart
C@tno jcp 1996Apr25_13:59:36 added tmhist
        IF (tmHist .EQ.' ') tmHist =tmstart
C -------------------------------------------------------
C store start and stop data
C -------------------------------------------------------

c        write(*,*) 'tmstart=',tmstart
c        call ho('before rtimdat jdstart','')
C@tno jcp 1996Jun17_16:16:25 added variab
C@empa aw 2000jan14 Give an additional error message to improve the localization
C                   of the error
        errkey2=0
        CALL RTIMDAT(tmstart,jdstart,IYYY1,MM1,ID1,H1,M1,S1,errkey,
     &  variab)
         if (errkey.ne.0)then
           CAll INERR('The above error was found in:', 'STARTtime '//
     &               tmstart,.false.,2)
             errkey2=errkey
           endif
C@empa aw 2005apr25 If no year is specified we use 1900
           if (Iyyy1==1)then
              jdstart=jdstart+693597
              IYYY1=1900
         endif
C@empa aw 2005apr25 If no day is specified we use 1900jan01
           if (jdstart==-1)then
              jdstart=2415021
              IYYY1=1900
              MM1=1
              ID1=1
         endif

c        write(*,*) tmstart,jdstart,iyyy1,mm1,id1,m1,m1,s1
c        call ho('na rtimdat jdstart','')
C@tno jcp 1996Jun17_16:16:25 added variab
        CALL RTIMDAT(tmstop ,jdstop ,IYYY2,MM2,ID2,H2,M2,S2,errkey,
     &  variab)
C@empa aw 2000jan14 Give an additional error message to improve the localization
C                   of the error
         if (errkey .ne.0)then
           CAll INERR('The above error was found in:', 'STOPtime '//
     &               tmstop,.false.,2)
             errkey2=errkey
         endif
C@empa aw 2005apr25 If no year is specified we use 1900
           if (Iyyy2==1)then
              jdstop=jdstop+693597
              IYYY2=1900
         endif
C@empa aw 2005apr25 If no day is specified we use 1900jan01
           if (jdstop==-1)then
              jdstop=2415021
              IYYY2=1900
              MM2=1
              ID2=1
         endif

C@tno jcp 1996Jun17_16:16:25 added variab
        CALL RTIMDAT(tmsched,jdsched,IYYY3,MM3,ID3,H3,M3,S3,errkey,
     &  variab)
C@empa aw 2000jan14 Give an additional error message to improve the localization
C                   of the error
         if (errkey.ne.0) then
            CAll INERR('The above error was found in SCHEDtime; '//
     &      '(if not explicit input, look at STARTtime):',
     &               tmsched,.false.,2)
              errkey2=errkey
           endif
C@empa aw 2005apr25 If no year is specified we use 1900
           if (Iyyy3==1)then
              jdsched=jdsched+693597
              IYYY3=1900
         endif
C@empa aw 2005apr25 If no day is specified we use 1900jan01
           if (jdsched==-1)then
              jdsched=2415021
              IYYY3=1900
              MM3=1
              ID3=1
         endif
C@tno jcp 1996Apr25_14:13:33 jdhist added
C@tno jcp 1996Jun17_16:16:25 added variab
        CALL RTIMDAT(tmhist ,jdhist ,IYYY4,MM4,ID4,H4,M4,S4,errkey,
     &  variab)
C@empa aw 2000jan14 Give an additional error message to improve the localization
C                   of the error
         if (errkey.ne.0) then
            CAll INERR('The above error was found in HISTtime; '//
     &       '(if not explicit input, look at STARTtime):',
     &               tmhist,.false.,2)
              errkey2=errkey
           endif
C@empa aw 2005apr25 If no year is specified we use 1900
           if (Iyyy4==1)then
              jdhist=jdhist+693597
              IYYY4=1900
         endif
C@empa aw 2005apr25 If no day is specified we use 1900jan01
           if (jdhist==-1)then
              jdhist=2415021
              IYYY4=1900
              MM4=1
              ID4=1
         endif
C@empa aw 2000jan14 return if error
        if (errkey2.ne.0) goto 500
        CALL SECONDS(sec1,H1,M1,S1)
        CALL SECONDS(sec2,H2,M2,S2)
        CALL SECONDS(sec3,H3,M3,S3)
C@tno jcp 1996Apr25_14:13:33 sec4..s4 is for Histogram start
        CALL SECONDS(sec4,H4,M4,S4)

C@tno jcp 1996Jun28_14:04:26 added the direct Nsec from simulation start for
C stop time, schedule and Histogram start time.
C this allows much easier comparisons fro time (before, in, after etc)
C@empa aw 2005may02 we must check for a maximum period of 12 years otherwise an 
C@empa aw 2005may02 endless loop within DUPLTM may occur
        juldif=jdstop-jdstart
        if (abs(juldif).gt.4381) then
          call inerr('&-PR-SIMU: Stop time: '//tmStop,
     &    'must be within a period of 12 years after;'// 
     &    'start time: '//tmstart(1:lenstr(tmstart))//' !',
     &    .false.,3)
        endif
        juldif=jdsched-jdstart 
          if (abs(juldif).gt.4381) then
          call inerr('&-PR-SIMU: Schedule time: '//tmSched,
     &    'must be within a period of 12 years before;'// 
     &    'start time: '//tmstart(1:lenstr(tmstart))//' !',
     &    .false.,3)
        endif
        juldif=jdhist-jdstart
        if (abs(juldif).gt.4381) then
          call inerr('&-PR-SIMU: Histogram time: '//tmHist,
     &    'must be within a period of 12 years before or '// 
     &    'after ;start time: '//tmstart(1:lenstr(tmstart))//' !',
     &    .false.,3)
        endif

        NSecSim2=JdaySecSim(Jdstop ,Sec2,0)
        NSecSim3=JdaySecSim(Jdsched,Sec3,0)
        NSecSim4=JdaySecSim(Jdhist ,Sec4,0)

500     RETURN
        END

Ch**********************************************************************

        SUBROUTINE FINDTIME(LINE,TMWORD)
C***********************************************************************
C
C bs mar20, 1991
C
C Purpose:
C Finds the time string in a Line.
C TmWord = time word, part of the line which contains the time
C@tno jcp 1996Jul11_6:11:55 added a check if any value is after the time
Ch**********************************************************************
        IMPLICIT NONE
C@empa aw 1995mar10 changed Line from string with assumed size to character*160
C@                  (concatenation with assumed sized strings is not allowded
C@                   in f77 standard)
        CHARACTER Line*160
        CHARACTER tmword*30
        INTEGER kword,l
        INTEGER LenStr

        kword=0
        l=lenstr(line)
        CALL GETWRD(Line,kword,l,tmword)
        IF (index(tmword,'*').GT.0) THEN
C first word is a name, read next word
          CALL GETWRD(Line,kword,l,tmword)

        ENDIF
C@tno jcp 1996Jul11_16:14:44 added a check here if there is any data after this
C@NBI PGS 2000Jul21 - Reformatted
        if (kword.ge.l) then
          call inerr('The schedule contains no value after the time.'
     &      //' Correct this line and rerun the program:',
     &      line(1:lenstr(line)),.false.,2)
        end if
        RETURN
        END
