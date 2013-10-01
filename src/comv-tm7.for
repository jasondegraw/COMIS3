C+*********************************************************** comv-tm7.f
Ch**********************************************************************

	SUBROUTINE SAVETMS(flname,elast,efirst)
C**********************************************************************
C
C lbl bs may 06 1991
C
C Saves the old TMS file.
C flname (Input parameter) = name of old TMS file.
C elast	 (Output parameter)= time of last element in TMS file.
C efirst (Output parameter)= time of first used element in TMS file.
Ch*********************************************************************
        IMPLICIT NONE
	INCLUDE 'comv-uni.inc'
	INCLUDE 'comv-inp.inc'
	CHARACTER*30 flname
        INTEGER e1,e2,e3,elast,efirst, JulX,i,j,iocheck

	OPEN(TSV,file='TMP003.TMS',access='direct',recl=30,
     &	form='unformatted')
	OPEN(TMS,file=flname,access='direct',recl=30,
     &	form='unformatted', STATUS = 'OLD')
	i=1
	j=1
C read stop time of old TMS file and compare it with new start time
	READ(TMS,REC=i) e1,e2,e3
	i=i+1
	IF (e1.NE.jdstart .OR. e3.NE.sec1) THEN
	    CALL INERR('&-PR-OUTPut: Start time option: CONT ',
     &	   'Start time is not the same as the Stop time of the '//
     &      'last run !',.FALSE.,2)
	ENDIF
10	READ(TMS,REC=i,IOSTAT=iocheck) e1,e2,e3
	IF (iocheck.NE.0) GOTO 19
	i=i+1
	WRITE(TSV,REC=j) e1,e2,e3
	j=j+1
	GOTO 10
19	elast=e1
	efirst=elast+JulX(JdStart,sec1,JdSched,sec3)
	CLOSE(TSV)
	CLOSE(TMS,status='delete')
	RETURN
	END
Ch*********************************************************************

	SUBROUTINE RESTTMS(NSteps,elast,efirst)
C**********************************************************************
C
C lbl bs may 06 1991
C
C Puts the old TMS file and the new data together into one file.
C NSteps (Output parameter) = number of lines in the file.
C elast	 (Input parameter)  = time of last element in old TMS file.
C efirst (Input parameter)  = time of first element used from old file.
C
C Changes:
C@empa aw 1993jun16 e2=0 filtered out too. Otherwise runtime error in TIMESTEP
C@empa              when reading DAF with recordnr =e2=0
Ch*********************************************************************
        IMPLICIT NONE
	INCLUDE 'comv-uni.inc'
	INCLUDE 'comv-inp.inc'
        INTEGER NSteps,elast,efirst,e1,e2,e3,i,j,iocheck
	CHARACTER*30 flname,flname2

	NSteps=0
	IF (Switch.EQ.0) THEN
	   flname='COMIS.TMS'
	   flname2='COMIS2.TMS'
	ELSE
	   flname='COMIS2.TMS'
	   flname2='COMIS.TMS'
	ENDIF
	Switch=MOD(Switch+1,2)
C write old TMS file into current file	---------------------------
	OPEN(TMM,file=flname2,recl=30,access='direct',
     &		form='unformatted')
	OPEN(TSV,file='TMP003.TMS',recl=30,access='direct',
     &		form='unformatted')
C write new stop time as the first line
	WRITE(TMM,REC=1) jdstop,0,sec2
	j=2
C i is not assigned!!!!
C can i be set to 0 first?????
	i=0
08	i=i+1
	  READ(TSV,REC=i,IOSTAT=iocheck) e1,e2,e3
	  IF (iocheck.NE.0) GOTO 10
	  IF (e1.EQ.elast) GOTO 10
	  IF (e1.LT.efirst) GOTO 08
C convert the time of old file according to new start time
	  e1=e1-elast
C second line in the file as start loop indicator
	  IF (NSteps.EQ.0) THEN
	     WRITE(TMM,REC=j) e1,0,0
	     j=j+1
	     NSteps=NSteps+1
	  ENDIF
	  WRITE(TMM,REC=j) e1,e2,e3
	  j=j+1
	  NSteps=NSteps+1
	GOTO 08
10	CLOSE(TSV,status='delete')
C write new part of TMS file  -------------------------------------
	OPEN(TMS,file=flname,access='direct',recl=30,form='unformatted')
	i=1
18	  READ(TMS,REC=i,IOSTAT=iocheck) e1,e2,e3
	  i=i+1
	  IF (iocheck.NE.0) GOTO 20
	  IF ((e3.EQ.0 .OR. (e2.EQ.0 .AND. e3.GE.0)) .AND.
     +		NSteps.GT.0) GOTO 18
	  WRITE(TMM,REC=j) e1,e2,e3
	  j=j+1
	  NSteps=NSteps+1
	GOTO 18
20	CLOSE(TMM)
	CLOSE(TMS,status='delete')

	RETURN
	END
Ch*********************************************************************

	SUBROUTINE FLFIND(flname)
C**********************************************************************
C
C lbl bs may 06 1991
C
C Finds out the name of the old TMS file and puts it into the output
C parameter flname. If no file is found, flname is blank.
C@lbl bs 1991aug06
C If a TMS file is found but the CONT or REUSE option is not selected,
C the TMS file is deleted.
Ch*********************************************************************
        IMPLICIT NONE
	INCLUDE 'comv-uni.inc'
	INCLUDE 'comv-inp.inc'
	CHARACTER*30 flname

	OPEN(TSV,file='COMIS.TMS',status='old',ERR=10)
	flname='COMIS.TMS'
	GOTO 90

10	OPEN(TSV,file='COMIS2.TMS',status='old',ERR=20)
	flname='COMIS2.TMS'
	GOTO 90

20	flname=' '
90	IF (flname.NE.' ' .AND. cont.EQ.' ') THEN
	   CLOSE(TSV,status='DELETE')
	ELSE
	   CLOSE(TSV)
	ENDIF
	RETURN
	END
Ch*********************************************************************

        INTEGER FUNCTION JulX(tmstart,secstart,Jnumber,sec)
C                             -start simulat.- ---event---
C**********************************************************************
C
C lbl bs mar 25, 1991
C
C Purpose:
C Create a number for the TMS file by which the data lines are
C sorted.
C The number of seconds of 'event' after 'start simulation'
C Changes
C@empa aw1995mar30 error message to prevent an out of range runtime error
C@                 in the calculation of julx
Ch**********************************************************************
        IMPLICIT NONE
        INTEGER Jnumber,tmstart,sec,secstart,juldif

        juldif=Jnumber-tmstart
C@empa aw 2001nov20 Check  with absolut value of juldif. Message changed.
        if (abs(juldif).gt.24855) then
          call error2('Your start, stop and schedule timesteps must be '
     &    ,'all together within a maximum period of 68 years!',3)
        endif
	JulX=juldif*86400+sec-secstart

	RETURN
	END

Ch*********************************************************************

	SUBROUTINE SECONDS(SEC,H,M,S)
C***********************************************************************
C
C lbl bs mar 20, 1991
C
C purpose:
C calculates the total amount of seconds out of a time consisting
C of hours (H), minutes (M) and seconds (S)
Ch**********************************************************************
        IMPLICIT NONE
        INTEGER sec, H, M, S

	sec=(H*60+M)*60+S

	RETURN
	END
Ch**********************************************************************

	SUBROUTINE REUSETMS(tmsfile,NSteps)
C***********************************************************************
C
C@lbl bs 1991aug12  new subroutine to reuse a complete TMS file from
C        the last run (same start and stop time, same schedules, etc.)
C
C Parameters:
C
C (I)  tmsfile	  name of the old TMS file which has to be reused
C (O)  NSteps	  number of datalines in the TMS file
Ch**********************************************************************
        IMPLICIT NONE
	INCLUDE 'comv-uni.inc'
	INCLUDE 'comv-inp.inc'

C MaxSch = number of schedules
C pSch	 = pointer to the first record of the schedule in the DAF
C pLSch	 = pointer to the last	record of the schedule in the DAF
        INTEGER MaxSch
	PARAMETER (MaxSch=11)
        INTEGER pSch(MaxSch), pLSch(MaxSch)
        INTEGER iocheck, NSteps,i,j,k,l,CifEnd
C@lbl bvs 1999Mar11 increased length of flname from 40 to 160
	CHARACTER flname*160, tmsfile*30, dum*30,sortopt*7
C@tno jcp 1996Jun27_22:23:33 Recline(100) and line(80) now *160
	CHARACTER RecLine*160, Line*160, tmword*30
	LOGICAL empty,InTime,Before
        INTEGER LenStr

C init pSch array  ------------------------------------------------
	DO 20 I=1,9
	   pSch(I)=pKeyRec(I+22)
20	CONTINUE
	pSch(10)=pKeyRec(36)
	pSch(11)=pKeyRec(38)
C init pLSch array  ------------------------------------------------
C I have to do that in a loop because the keywords in
C the input file can be in arbitrary sequence.
	DO 21 I=1,11
	   IF (pSch(I).EQ.0) GOTO 21
	   k=9999
	   DO 22 J=1,44
	      IF (pKeyRec(J).GT.pSch(I) .AND. pKeyRec(J).LT.k) THEN
		 k=pKeyRec(J)
	      ENDIF
22	   CONTINUE
	   pLSch(I)=k-1
	   IF (pLSch(I).EQ.9998) pLSch(I)=-1
21	CONTINUE
C at the end of this part pLSch(I) can be -1, when the
C schedule I is the last in the .CIF file
C -------------------------------------------------------
C read DAF and add schedules with file assignment to DAF
C -------------------------------------------------------
	CifEnd = pDAF-1
	DO 30 I=1,11
	   IF (pSch(I).NE.0) THEN
	      IF (pLSch(I).EQ.-1) pLSch(I)=CifEnd
	      DO 40 J=pSch(I)+1,pLSch(I)
		 READ(DAF,REC=J,IOSTAT=iocheck) RecLine
C@tno jcp 1996Jun11_17:59:21 try to keepline from DAF
          call KeepLine(RecLine)
		 IF (iocheck.NE.0) GOTO 35
		 IF (empty(RecLine)) GOTO 35
		 l=lenstr(RecLine)
C schedules with 2 headers, first header processed in ELSE-branch
		 IF (I.GT.1 .AND. I.LT.10 .OR. J.GT.(pSch(I)+1)) THEN
C look for input file assignment
                        k=index(RecLine,'F:')
C@tno jcp 1996Jun27_16:04:47 allow F: and f: to signal a file
                        if (k.eq.0) then
                          k=index(RecLine,'f:')
                        end if

			IF (k.GT.0) THEN
			   k=k+1
			   RecLine(k:k)=' '
			   CALL GETWRD(RecLine,k,l,dum)
			   CALL GETWRD(RecLine,k,l,flname)
C@tno jcp 1996Jun13_23:48:56 added variab= before getWS sortopt
             variab='the sort option for a schedule file'
                           CALL GETWS(RecLine,k,l,sortopt,' ')
			   CALL UpperC(sortopt)
c                           IF (sortopt.NE.' '.AND.sortopt.NE.'-SORTED')
c     &                     sortopt=' '
                           IF (sortopt.NE.'-SORTED')
     &                     sortopt=' '

c                           if (sortopt.eq.' ') then
c                             write(*,*) 'now sortopt is empty'
c                           end if

C@lbl bvs 1998Dec03 check for error in opening schedule file
CC                           OPEN(IFS,file=flname, STATUS = 'OLD')
                           OPEN(IFS,file=flname, STATUS = 'OLD',ERR=49)
			   IF (sortopt.NE.' ')
     &                     CALL DUPLTM(0,0,0,' ',sortopt,NSteps,
     &                      InTime,Line)
			   Before=.TRUE.
			   READ(IFS,'(A)',END=49) Line

C write data from IFS file to DAF file
31			   IF (.NOT.empty(Line)) THEN
			      CALL FINDTIME(Line,tmword)
C DUPLTM is used here only to fill the variable InTime
			      CALL DUPLTM(pDAF,I,0,tmword,sortopt,
     &                        NSteps,InTime,Line)
			      IF (InTime) THEN
				 Before=.FALSE.
				 WRITE(DAF,REC=pDAF) Line
				 pDAF=pDAF+1
			      ENDIF
			      IF(.NOT.(InTime.OR.Before).AND.
     &			      sortopt.NE.' ') GOTO 38
			   ENDIF
			   READ(IFS,'(A)',END=38) Line
			   GOTO 31
38			   CLOSE(IFS)
                           CALL DUPLTM(0,0,0,' ',sortopt,NSteps,
     &                      InTime,Line)
			   GOTO 40
		       ENDIF
		 ELSE
C first line of the schedules 1,10 or 11
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
C@tno jcp 1996Jun13_23:48:56 added variab= before getWS sortopt
          variab='the option COMIS/DOE-2 for the meteo file'
                           CALL GETWS(RecLine,k,l,metopt,'COMIS')
			   CALL UPPERC(metopt)
			   IF (metopt.NE.'COMIS'.AND.metopt.NE.'DOE2')
     &			   THEN
			       CALL ERROR2('&-SCH-MET: Wrong '//
     &				 'filename option in meteo schedule :'//
     &                  	 metopt//' !',
     &				 'Using default option: COMIS !',0)
			       metopt='COMIS'
			   ENDIF
			   GOTO 40
			ENDIF
C@tno jcp 1996Jun13_23:48:56 added variab= before getWS sortopt
          variab='the sortoption for the meteo file'
			CALL GETWS(RecLine,k,l,sortopt,' ')
			CALL UpperC(sortopt)
CC                        IF (sortopt.NE.' '.AND.sortopt.NE.'-SORTED')
CC     &                     sortopt=' '
                           IF (sortopt.NE.'-SORTED')
     &			   sortopt=' '
c                           if (sortopt.eq.' ') then
c                             write(*,*) 'now sortopt is empty'
c                           end if
C@lbl bvs 1998Dec03 check for error in opening schedule file
CC                        OPEN(IFS,file=flname, STATUS = 'OLD')
                        OPEN(IFS,file=flname, STATUS = 'OLD',ERR=49)
			IF (sortopt.NE.' ')
     &                     CALL DUPLTM(0,0,0,' ',sortopt,NSteps,
     &                     InTime,Line)
			Before=.TRUE.
			READ(IFS,'(A)',END=49) Line
C write data from IFS file to DAF file
41			IF (.NOT.empty(Line)) THEN
			      CALL FINDTIME(Line,tmword)
C DUPLTM is used here only to fill the variable InTime
			      CALL DUPLTM(pDAF,I,0,tmword,sortopt,
     &                                  NSteps,InTime,Line)
			      IF (InTime) THEN
				 Before=.FALSE.
				 WRITE(DAF,REC=pDAF) Line
				 pDAF=pDAF+1
			      ENDIF
			      IF(.NOT.(InTime.OR.Before).AND.
     &				 sortopt.NE.' ') GOTO 48
			ENDIF
			READ(IFS,'(A)',END=48) Line
			GOTO 41
48			CLOSE(IFS)
                        CALL DUPLTM(0,0,0,' ',sortopt,NSteps,
     &                  InTime,Line)
			GOTO 40
		    ENDIF
C error: wrong filename
		 ENDIF
		 GOTO 40
49		 CALL ERROR2('&-SCH-MET: The specified weather '//
C@tno jcp 1996Jul01_08:39:53 this is severe 1=now 2
C@NBI PGS 1999Sep20 - This should be a fatal error, i.e. 3
CC   &                  'file does not exist or is empty: ',flname,2)
     &                  'file does not exist or is empty: ',flname,3)
		 CLOSE(IFS,status='DELETE')
40	      CONTINUE
	   ENDIF
30	CONTINUE

C get number of lines in the old TMS file
35	OPEN(TMS,file=tmsfile,access='direct',recl=30,
     &	form='unformatted', STATUS = 'OLD')
	NSteps=1
81	READ(TMS,REC=NSteps,IOSTAT=iocheck) i,j,k
	IF (iocheck.NE.0) GOTO 80
	NSteps=NSteps+1
	GOTO 81
80	CLOSE(TMS)
	NSteps=NSteps-1
	RETURN
	END

Ch**********************************************************************

	SUBROUTINE TMSMAIN(NSteps)
C***********************************************************************
C
C lbl bs march 1991
C
C Purpose:
C
C This routine is made to initialise the timeloop and provide all the
C necessary data for a stepwise processing.
C
C Parameters:
C (O)  NSteps
C
C@lbl bs 1991aug05 Changes for TMS-file "continue" and "keep" options.
C@empa aw 1995mar30 Keyword &-PR-OUTPut changed to &-PR-SIMU
Ch**********************************************************************
        IMPLICIT NONE
	INCLUDE 'comv-uni.inc'
	INCLUDE 'comv-inp.inc'

C@tno jcp 1996Apr25_14:16:02 k3 added
        INTEGER NSteps,k1,k2,k3,l,k,elast,efirst,JulX
	CHARACTER metbuf*160,flname*30
        INTEGER LenStr

c        call ho('in TMS 1','')
	metfile=' '
C notime=0 means : assume there is time in the meteo file
	notime=0
	CALL RDPERIOD
c        call ho('in TMS 2','')

	CALL UpperC(cont)
	IF (cont.NE.'CONT'.AND.cont.NE.'REUSE'.AND.cont.NE.' ') THEN
	    CALL INERR('&-PR-SIMU: Wrong start time option: '//cont,
     &			' ',.FALSE.,2)
	ENDIF
	CALL UpperC(keep)
	IF (keep.NE.'KEEP' .AND. keep.NE.' ') THEN
	    CALL INERR('&-PR-SIMU: Wrong stop time option: '//keep,
     &			' ',.FALSE.,2)
	ENDIF
	CALL FLFIND(flname)
	IF (cont.EQ.'REUSE') THEN
	   IF (flname.EQ.' ') THEN
	      CALL INERR('&-PR-SIMU: Old TMS file not found !',
     &		'Remove start time option: "REUSE" !',.FALSE.,2)
	   ELSE
	      CALL REUSETMS(flname,NSteps)
	   ENDIF
	   GOTO 30
	ENDIF
	IF (cont.EQ.'CONT') THEN
	   IF (flname.EQ.' ') THEN
              CALL INERR('&-PR-SIMU: Old TMS file not found !',
     &      'Remove start time option: "CONT" !',.FALSE.,2)
           ELSE
	      CALL SAVETMS(flname,elast,efirst)
	   ENDIF
C next two lines are for DUPLTM call
	   JdSched=JdStart
	   sec3=sec1
        ENDIF

        OPEN(TMS,file='COMIS.TMS',access='direct',
     &  recl=30,form='unformatted')
c        call ho('in TMS 3','')

C calculate schedule start time
	k1=JulX(jdstart,sec1,jdsched,sec3)
	IF (k1.GT.0) THEN
           CALL INERR('&-PR-SIMU: Start time less than schedule '//
     &    'time !',' ',.FALSE.,2)
	ENDIF
C write stop time in the first record in TMS file
	WRITE(TMS,REC=1) jdstop,0,sec2
C write schedule start time next
        WRITE(TMS,REC=2) k1,0,0

C@tno jcp 1996Apr25_14:18:32 inserted TMS record 3 as histogram start time
C calculate histogram start time . e3=-2 as flag
        k3=JulX(jdstart,sec1,jdhist ,sec4)
        WRITE(TMS,REC=3) k3,0,-2

C calculate stop time
	k2=JulX(jdstart,sec1,jdstop,sec2)
	IF (k2.LT.0) THEN
           CALL INERR('&-PR-SIMU: Stop time less than start time !',
     &     ' ',.FALSE.,2)
	ENDIF
C@tno jcp 1996Apr25_14:18:15 TMS record 3 is record 4 now
C write stop time into the fourth record
        WRITE(TMS,REC=4) k2,0,-1
	CLOSE(TMS)
c        call ho('in TMS 5','')

        CALL READDAF(NSteps,elast,efirst)
c        call ho('in TMS 6','')
C@tno jcp 1996Jul04_11:28:03 write from the last schedule the data before
C       starttime to TMS
C@tno jcp 1996Jul04_11:44:32 wrtbefore is already called at the end of read daf
C        call wrtbefore()
C NSteps = number of time steps
C it is 0 when there is no schedule in the CIF file
	IF (NSteps.EQ.0) NSteps=2
	IF (Switch.EQ.0) THEN
	   flname='COMIS.TMS'
	ELSE
	   flname='COMIS2.TMS'
	ENDIF
C@tno jcp 1996Jul04_11:52:32 tms file is opened after METINIT from MAIN
CC30      OPEN(TMS,file=flname,access='direct',
30      CONTINUE
CC     OPEN(TMS,file=flname,access='direct',
CC     &  recl=30,form='unformatted')
C@tno jcp 1996Jul04_11:52:32 end

C weather file
	IF (metfile .NE. ' ') THEN
	   IF (NSteps.EQ.2) NSteps=3
	   IF (metopt .EQ. 'DOE2') THEN
	      OPEN(IFS,file=metfile,FORM='UNFORMATTED', STATUS='OLD',
     &        ERR=29)
           ELSE
	      OPEN(IFS,file=metfile, STATUS = 'OLD',ERR=29)
	   ENDIF
C option to read a DOE-2 weather file
	   IF (metopt .EQ. 'DOE2') THEN
	      notime=1
	      buftim='jan01_'
	      bufsec=3600
c          call ho('in tmsmain at return',' ')
	      GOTO 32
	   ENDIF
22	   READ(IFS,'(A)',END=29) metbuf

	   call KeepLine(metbuf)

	   IF (metbuf.EQ.' ') GOTO 22
	   k=index(metbuf,'*')
	   IF (k.GT.0) THEN
	      notime=1
              l=lenstr(metbuf)
c              write(*,*) 'len=',l
c              call ho('get times from metbuf',metbuf)
C@tno jcp 1996Jun13_23:48:56 added variab= before getWS sortopt
          variab='the date_time from the meteo file'
	      CALL GETWS(metbuf,k,l,buftim,'jan01_')
C@tno jcp 1996Jun14_00:20:38 variab for GetWI
              variab='get seconds from the meteo file date_time'
              CALL GETWI(metbuf,k,l,bufsec,3600)
C@tno jcp 1996Jul04_08:37:43 TMSMAIN:bufsec must be acceptable
              if (bufsec.lt.1) then
               call error2('TMSMain reads the time increment from the'//
     &         ' weather file.;This must be 3600 or >=1. Increment '//
     &         'made 3600 now.;The first line is:',metbuf,2)
               bufsec=3600
              end if
C@tno jcp 1996Jul04_08:37:43 end

c              write(*,*) buftim,bufsec
c              call ho('buftime and bufsec',' ')
           ELSE
c the first line did contain normal data for a Comis Meteo file, so basckspace
c not to skip this line
	      BACKSPACE(IFS)
           ENDIF
c           call ho('in tmsmain at return',' ')
	   GOTO 32

29         CALL ERROR2('&-SCH-MET: The specified weather '//
C@tno jcp 1996Jul01_08:39:53 this is severe 1=now 2
C@NBI PGS 1999Sep20 - This should be a fatal error, i.e. 3
CC   &     'file does not exist or is empty: ',metfile,2)
     &     'file does not exist or is empty: ',metfile,3)
	   metfile=' '
	ENDIF

C the direct access file TMS is created now
C the time loop can start

32	RETURN
	END
