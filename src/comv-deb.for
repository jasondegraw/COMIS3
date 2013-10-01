C+*********************************************************** comv-deb.f
C debug routines for COMIS
Ch***********************************************************************
        SUBROUTINE EchoTMS
C       Echo TMS prints the contents of the *.TMS file in *.COF
C       e1=seconds since start of simulation
C       e2=record number of the schedule data
C       e3=ScheduleType*f+pointerScheduleName
C       f depends on the Parameter MaName (=1500, maximum number of names)
C       and is here 10000 (always the decade above MaName)

C       The first record is different
C       e1=stop julian day nr
C       e2=0
C       e3=seconds from 00:00:00 on the stop day

C       The record with schedule start is different
C       e1=schedule start time in seconds since the start of the simulation
C          (time/date as given in &-PR-SIMU)
C       e2=0
C       e3=0

C       The records with histo start is different
C       e1=schedule start time in seconds since the start of the simulation
C          (time/date as given in &-PR-SIMU)
C       e2=0
C       e3=-2

C       The Last record is different
C       e1=stop time in seconds since the simulation start
C       e2=0
C       e3=-1
Ch***********************************************************************
        IMPLICIT NONE
        INCLUDE 'comv-uni.inc'
	INCLUDE 'comv-inp.inc'

        INTEGER LenStr
        CHARACTER*30 flname
c        CHARACTER sort*7
        INTEGER SchTyp, pNam
        INTEGER e1,e2,e3,iocheck
        INTEGER F,lenAname,k
        INTEGER pTMS
        DOUBLE PRECISION JdayLotus,Tlotus1
        INTEGER number
        INTEGER Time2,Jday1,Lstr
        CHARACTER DTstring*31,schName1*10,str*40

        lenAname=lenstr(aName)

        f=10**int(log10(REAL(MaName))+1)
        IF (Switch.EQ.0) THEN
	   flname='COMIS.TMS'
	ELSE
	   flname='COMIS2.TMS'
        ENDIF
        write(cof,*) ' '
        write(cof,*) 'facts about TMS:'
        write(cof,*) '                    Julian day, seconds from 0:00'
        Tlotus1=JdayLotus(Jdstart,sec1)
        Call TimeStr(Tlotus1,DTstring)
        write(cof,*) 'StartTime      =',Jdstart,sec1,' ',DTstring
        Tlotus1=JdayLotus(Jdstop,sec2)
        Call TimeStr(Tlotus1,DTstring)
        write(cof,*) 'StopTime       =',Jdstop ,sec2,' ',DTstring
        Tlotus1=JdayLotus(Jdsched,sec3)
        Call TimeStr(Tlotus1,DTstring)
        write(cof,*) 'Schedule start =',Jdsched,sec3,' ',DTstring
        Tlotus1=JdayLotus(Jdhist,sec4)
        Call TimeStr(Tlotus1,DTstring)
        write(cof,*) 'Histogram start=',Jdhist ,sec4,' ',DTstring

        write(cof,*) ' '

        write(cof,*) 'ECHO contents of '//flname(1:lenstr(flname))//
     & ' file.'
C TMS is still open
c        OPEN(TMS,file=flname,access='direct',
c     &  recl=30,form='unformatted',STATUS = 'OLD')
        pTMS=0
110     ptms=ptms+1
	READ(TMS,REC=pTMS,IOSTAT=iocheck) e1,e2,e3
        IF (iocheck.NE.0) GOTO 333
C first and last record have different contents
C also record with e3= 0 schedulestart
C                  e3=-2 histogramstart

        if (ptms.eq.1) then
          number=(e1-jdstart)*86400+(e3-sec1)

c          write(cof,*) 'e1=',e1,' e2=',e2
c          write(cof,*) 'Jdstart=',Jdstart,' sec1=',sec1
c          write(cof,*) 'Echo tms rec=1 number=',number
c          call ho('','')
          CALL CreateJdTim(number,Jday1,time2)
c          write(cof,*) 'Jday1=',jday1,'Time2=',time2
c          call ho('','')
          CALL CONVDAT1(Jday1,time2,DTstring)
c          write(cof,*) 'timestring=',DTstring
c          call ho('na convdat in EchoTMS','')
c          write(cof,*) 'Jday1=',jday1,'Time2=',time2

          write(cof,*) 'TMSrecord 1 = SimulationStopTime: JulianDay'//
     &     '            0    Seconds (0..24:00)'
          write(cof,*) DTstring,e1,' ',e2,' ',e3
          write(cof,*) ' ----date/time---------day--- -e1-'//
     &    '-since-start-  -e2-DAF-     -e3-Schedule-'
          write(cof,*) '                                    '//
     &    'seconds       record         Type  Name'

        else if (e3.eq.0) then
          CALL CreateJdTim(e1,Jday1,time2)
c          write(cof,*) 'e1=',e1
c          write(cof,*) 'Jday1=',jday1,'Time2=',time2
c          call ho('','')
          CALL CONVDAT1(Jday1,time2,DTstring)
c          write(*,*) 'DTstring=',DTstring
c          call ho('','')
          call intdis(ptms,str,lstr)
          write(cof,*) 'TMSrecord '//str(1:lstr)//
     &     ' = starttime schedules: made active'
          write(cof,*) DTstring,e1,' ',e2,' ',e3
        write(cof,*) ' ----date/time---------day--- -e1-'//
     &  '-since-start-  -e2-DAF-     -e3-Schedule-'
        write(cof,*) '                                    '//
     &  'seconds       record         Type  Name'
        else if (e3.eq.-2) then
          CALL CreateJdTim(e1,Jday1,time2)
c          write(*,*) 'Jday1=',jday1,'Time2=',time2
c          call ho('','')
          CALL CONVDAT1(Jday1,time2,DTstring)
c          write(*,*) 'DTstring=',DTstring
c          call ho('','')
C@lbl bvs 1997jun6 no wCRT parameter in INTDIS
CC          call intdis(ptms,str,lstr,wCRT)
          call intdis(ptms,str,lstr)
          write(cof,*) 'TMSrecord '//str(1:lstr)//
     &     ' = starttime histograms: made active'
          write(cof,*) DTstring,e1,' ',e2,' ',e3
        write(cof,*) ' ----date/time---------day--- -e1-'//
     &  '-since-start-  -e2-DAF-     -e3-Schedule-'
        write(cof,*) '                                    '//
     &  'seconds       record         Type  Name'
        else
          CALL CreateJdTim(e1,Jday1,time2)
c          write(*,*) 'Jday1=',jday1,'Time2=',time2
c          call ho('','')
          CALL CONVDAT1(Jday1,time2,DTstring)
c          write(*,*) 'DTstring=',DTstring
c          call ho('','')
          if (e3.eq.-1) then
C last record
C@lbl bvs 1997jun6 no wCRT parameter in INTDIS
CC          call intdis(ptms,str,lstr,wCRT)
          call intdis(ptms,str,lstr)
            write(cof,*) 'last TMSrecord='//str(1:lstr)//'  :'
            write(cof,*) DTstring,e1,' ',e2,' ',e3

          else
C records  2..N-1
            SchTyp=e3/f
            pNam  =e3-SchTyp*f
            k=pnam-1
c          write(*,*)'in echotms'
c         call ho('','')
            if (k.ge.0) then
              variab='get schedule name in EchoTMS'
              Call GetWrd(aName,k,lenAname,SchName1)
            else
              if (schtyp.eq.10) then
                SchName1='meteo'
              else
                SchName1=' '
              end if
            end if
C@empa aw 2000apr14 Format output
CC            write(cof,*) DTstring,e1,' ',e2,' ',SchTyp,' ',schName1
            write(cof,10) DTstring,e1,e2,SchTyp,schName1
10            format (A30, I14, I13, I13, A12)
          end if
        end if

        goto 110

333     continue
C        CLOSE(TMS)    if I close this file a run time error follows later
        return
        end

C@tno jcp 1996Apr30 moved RE from comv-inh to comv-deb.f; renamed to EchoDAF
Ch***********************************************************************
CC      SUBROUTINE re
	SUBROUTINE EchoDAF
C***********************************************************************
C EchoDAF=Echo DAF file to COF
C Purpose:
C EchoDAF is a trial to READ the DAF file written by inh
C Module : #, TG, hcp/ aug 08, 1989
C Changes: mm dd yy
C Limits :
C
C Pass parameters:
C
C IO # Name    unit		 description
C sfile(1:80)	  filename of the serial file *CIF
C
Ch***********************************************************************
        IMPLICIT NONE
	INCLUDE 'comv-uni.inc'
C@tno jcp 1996Jul04_07:46:04 include inp at EchoDAF
        INCLUDE 'comv-inp.inc'
C local
        INTEGER pDAS,IOCheck

        CHARACTER line*160
        CHARACTER*10 str
        INTEGER Lstr
C@NBI PGS 2000dec23 - Redundant duplicate line        
CC	pDAS=1

C@tno jcp 1996Jun27_22:20:32 reclength 100->160
	open(DAF,file='COMIS.DAF',access='direct',recl=160,form=
     &	'unformatted')
        write(cof,*) ' '
        write(cof,*) 'ECHO contents of COMIS.DAF'
        PDAS=1
20      CONTINUE
          READ(DAF,REC=pDAS,IOSTAT=iocheck) LINE
C@tno jcp 1996Jun11_17:54:46 would keep line work at Read Daf, but not here at t
c          call KeepLine(Line)

          if (iocheck.ne.0) goto 333
          call intdis(pDas,Str,Lstr)
          call WRT80(COF,str(1:Lstr)//' '//line,wCRT)
	  IF (MOD(pDAS,20).EQ.0) THEN
            WRITE(CRT,*) 'rec=',pDAS
	  ENDIF
          pDAS=pDAS+1
        goto 20
333     continue
C file is left open
	RETURN
	END
