C+*********************************************************** comv-tm4.f
Ch**********************************************************************

        SUBROUTINE JustBefore(SchTyp,SchNa,SecSim,Line)
C                             I      I     I      I
C                             i      i     i      c
C                             |      |     |      Line from *.CIF
C                             |      |     seconds after Simulation start (neg=b
C                             |      INTEGER pointer to the schedule name
C                             Schedule type number 1..11 see ReadDaf



C***********************************************************************
C@tno jcp 1996Jul02_22:19:14
C Just before is called From DuplTm preferrably with a time just before
C or at the start of the simulation
C As all lines (with events and data) that belong to one schedule and one
C *name within that schedule are processed in succesion, we can keep track
C here of the event that is the closest just before StartTime.
C The routine WriteBefore puts this closest line and data in TMS and DAF
C Comis does then start with the proper start values from the schedules and
C meteo file.
C just before events that are on SecSim=0 (simulation start) are also included
C here, while they are not written in TMS by WrtBefore, because the normal
C DuplTM procedure will get those in the TMS file already. This is to prevent
C other moments to be placed over this best value valid for the start time.
C Storage of more events of the same schedule at the same timestep is unwanted
C because a routine CheckTMS looks for those to report errors on them.
C Setting a shower 'on' and 'off' at the same timestep leads to unpredictible
C situations as the sort algoritm might alter the sequence of events at the same
C timestep

C (I)  SchTyp	  schedule type
C (I)  SchNa	  pointer to schedule name
C (I)  SecSim     number of seconds from Simulation StartTime, neg=before
C (I)  Line       line with data from the schedule
C
C
Ch**********************************************************************
        IMPLICIT NONE
	INCLUDE 'comv-uni.inc'
	INCLUDE 'comv-inp.inc'
        INTEGER SchTyp,SchNa,SecSim
        CHARACTER*(*) Line
c      write(*,*) 'SecSim =',SecSim
c      write(*,*) 'SchTyp =',SchTyp
c      write(*,*) 'PrvSec =',PrvSec
c      write(*,*) 'Line   =',Line
c      write(*,*) 'PrvLine=',PrvLine
c      call ho('Just before',' ')

        if (SecSim.GT.PrvSec .and. SecSim.LE.0) then
c closer before start store these values in the prv* variables
c         call ho('STORE data in Just before',' ')
          PrvSec   =SecSim
          PrvSchTyp=SchTyp
          PrvSchNa =SchNa
          PrvLine  =Line
        end if

        RETURN
        END

Ch**********************************************************************

        SUBROUTINE WrtBefore()
C***********************************************************************
C@tno jcp 1996Jul02_22:19:14
C Wrt before is called From ReadDaf before another schedule starts or after
C the last schedule has been processed.
C Probably this has to be tested for multischedule files????
C The closest values of the current schedule have been kept by routine Just
C before in the prv* variables.
C The routine WriteBefore puts this closest line and data in TMS and DAF
C This is done only if the closest time is not already at the start time (
C secSim=0 seconds) as such events are already placed in TMS by DuplTM.

C (I)  RecNr	  record number
C (I)  SchTyp	  schedule type
C (I)  SchNa	  pointer to schedule name
C (I)  SecSim     number of seconds from Simulation StartTime; neg=before
C (I)  Line       line with data from the schedule
C
C
Ch**********************************************************************
        IMPLICIT NONE
	INCLUDE 'comv-uni.inc'
	INCLUDE 'comv-inp.inc'
        INTEGER instep
        Logical iintime
C@tno jcp 1996Jul11_16:38:05 !!!!
C        return

        instep=1
c         write(*,*) 'PrvSec=',prvSec
c         write(*,*) '-IntMax=',-IntMax
c         write(*,*) 'PrvSchTyp=',prvSchTyp
c         write(*,*) 'PrvSchNa=',prvSchNa
c         write(*,*) 'PrvLine=',prvLine
c         write(*,*) 'tmstart=',tmstart
c        call ho('in WrtBefore,',' ')

        if (PrvSec.GT.-IntMax .and. PrvSec.NE.0) then
C If there was no schedule before, PrvSec would have been -IntMax.
C If PrvSec=0 this event is already at starttime and is placed already by
C DuplTm.
c        if (prvsec.ge.0) then
c        write(*,*) 'prvsec',prvsec
c        call ho('in wrtbefore','')
c        end if

c           call ho('in WrtBefore, calling Dupltm ',' ')
c           CALL DUPLTM(0,0,0,' ',
c     &    ' ',iNstep,iinTime,' ')

c          call ho('in WrtBefore, calling Dupltm with tmstart=',tmstart)
C@tno jcp 1996Jul04_1:46:07 the event that took place 'just-before' or at
C simulation start time is written to TMS at Simulation starttime. The data
C in DAF will have the 'just-before' time, but that is no longer interpreted.
C TMS time is THE time used in the simulation. DAF serves only the data.
          CALL DUPLTM(pDAF,PrvSchTyp,PrvSchNa,tmstart,
     &    ' ',iNstep,iinTime,prvLine)
c          call ho('in WrtBefore, after calling Dupltm tmstart',' ')

C@tno jcp 1996Jul04_1:45:45 write this 'just-before' data also in DAF
          WRITE(DAF,REC=pDAF) prvLine
          pDAF=pDAF+1
c          Write(*,*)' after writeDAF pDaf=',pDaf
c          call ho(' ',' ')

        end if
C make sure the next schedule will replace the values as the time is closer to
C simulation start
        PrvSec   =-IntMax
c          call ho('wrtBefore, after if>-IntMax',' ')

        RETURN
        END



Ch**********************************************************************

        SUBROUTINE DUPLTM(RecNr,SchTyp,SchNa,tmWord,sort,NSteps,
     &                    InTime,Line)
C***********************************************************************
C
C bs mar20, 1991
C
C Purpose:
C Duplicates the datalines and calculates the time for every line
C The datalines are temporarly written into an array and then merged
C into the sorted TMS file.
C
C Changes:
C@empa aw 1992oct29 change the factor for SchTyp
C
C Parameters:
C
C (I)  RecNr	  record number
C (I)  SchTyp	  schedule type
C (I)  SchNa	  pointer to schedule name
C (I)  TMWORD	  time string
C (I)  sort	  sort option for file input
C (O)  NSteps	  number of created datalines
C (O)  InTime     indicator if data are within the simulation time
C
C
C@tno jcp 1996Jun17_18:06:18 keep the meteo schedules (at the same time
C interval) first
C Shedule type is stored in e3 e3=SchTyp*f+SchNa
C meteo schedules have e3>f*10 <=f*11

Ch**********************************************************************
        IMPLICIT NONE
	INCLUDE 'comv-uni.inc'
	INCLUDE 'comv-inp.inc'

        INTEGER RecNr, SchTyp, SchNa, NSteps
        CHARACTER*(*) TmWord,line
        LOGICAL InTime
C@tno jcp 1996Jul04_17:03:48 DuplTM: if Sort is fixed length you cannot test on
CC        CHARACTER sort*7
        CHARACTER*(*) sort
C --local
	CHARACTER*30 flname,flname2
        INTEGER i, Jday
        INTEGER Jd1,Jd2,e1,e2,e3,errkey,iocheck,MaxEl,WriteFirst
C@tno jcp 1996Jul17_11:59:54 Cycle is the number of days a date_time repeats its
C@tno jcp 1996Jul17_11:59:54 Expected the number of events of this date_time
C between schedulestart and stop
C@tno jcp 1996Jul17_13:43:59 iY1 and iY2 are start and stop years
C@tno jcp 1996Jul17_13:43:59 istep is used to step up or down through days
        INTEGER F,Cycle,Expected,iY1,iY2,istep
        PARAMETER (MaxEl=4381)
        INTEGER IYYY,MM0,ID,H,M,S,sec,weekday,JulX
        INTEGER EL(MaxEl),EL2(MaxEl), EL3(MaxEl),pEL,pTMM,pTMS
C@tno jcp 1996Jul01_14:19:35 element for calculation of EL3
        INTEGER Element3
C@tno jcp 1996Jun17_18:08:13 NewTime
        REAL NewTime
C@tno jcp 1996Jun17_18:08:13 InewTime,met1,met2
        INTEGER InewTime,met1,met2,SecSim
C@empa aw 2000jan14 word, lenstr,k,l
        CHARACTER word*20
C@NBI PGS 2000Aug02 - "k" no longer used so removed from decralations
	  INTEGER lenstr,l
	SAVE pEL,WriteFirst
C@NBI PGS 1999Aug11 - Bufix. I was getting some crazy values of EL3.  
C@NBI                 EL, EL2 & EL3 must be SAVEd between calls!?!
c@NBI                 This makes the executable larger, though
      SAVE EL,EL2,EL3
	DATA pEL /0/
	DATA WriteFirst /0/
C-----
	WriteFirst=0
	InTime=.FALSE.
	IF (pEL.EQ.0) pEL=1
        f=10**int(log10(REAL(MaName))+1)
C@tno jcp 1996Jul01_14:19:35 element for assignment of EL3
        Element3=SchTyp*f+SchNa

C@tno jcp 1996Jun17_18:09:12 met1..met2 range for e3 when it is a meteo schedule
        met1=f*10
        met2=met1+f
c         write(*,*) 'RecNr' ,RecNr
c         write(*,*) 'SchTyp',SchTyp
c         write(*,*) 'SchNa ',SchNa
c         write(*,*) 'tmWord',tmWord
c         write(*,*) 'sort  ',sort
c         write(*,*) 'NSteps',NSteps
c         write(*,*) 'InTime',InTime
c         write(*,*) 'Line  ',Line
c         CALL HO('IN dUPLTM',' ')

C SchTyp = 0 means write the rest of the array to the TMS-file
        IF (SchTyp.EQ.0) THEN
c           write(*,*)'in Dupltm schtyp=0, pel=',pel,' sort=',sort
c           write(*,*)'sort(1:1)',ICHAR(sort(1:1))
c           write(*,*)'sort(2:2)',ICHAR(sort(2:2))
c           write(*,*)'sort(3:3)',ICHAR(sort(3:3))
c           write(*,*)'sort(4:4)',ICHAR(sort(4:4))
c           write(*,*)'sort(5:5)',ICHAR(sort(5:5))
c           write(*,*)'sort(6:6)',ICHAR(sort(6:6))
c           write(*,*)'sort(7:7)',ICHAR(sort(7:7))
c           if (sort.eq.' ') then
c             write(*,*) 'sort='' '''
c           end if
c           call ho('in dupltm','')
	   IF (pEL.GT.1) GOTO 90
	   RETURN
	ENDIF


C@tno jcp 1996Jun17_16:17:53 added variab
CC        CALL RTIMDAT(tmword,jday,IYYY,MM0,ID,H,M,S,errkey)
        CALL RTIMDAT(tmword,jday,IYYY,MM0,ID,H,M,S,errkey,variab)
c        write(cof,*) 'DuplTm     time=',tmword,'jday',jday,'iyyy',iyyy,
c     &   'mm0',mm0,'id',id,'h',h,'m',m,'s',s
C@empa aw 2000jan14 give an additional error message to improve the 
C         localization of the error
      IF (errkey.ne.0) then
        l=lenstr(aname)
        variab='schedule name'
        CALL GETWRD(aname,Schna,l,word)
        CAll INERR('The above error was found in the schedule *'//
     &              word(1:Lenstr(word))//' in the following line:',
     &             line,.false.,2)
      ENDIF
	CALL SECONDS(sec,H,M,S)
c        write(*,*) 'jday',jday
c        write(*,*) 'sec',sec
c        write(*,*) 'Line  ',Line
c        CALL HO('IN dUPLTM',' ')

C----------------------------------------------- time only: repeated every day
21      IF (Jday.EQ.-1) THEN
           Cycle=1
           Jd1=JdStart
	   Jd2=JdStop

c schedule time is after the schedule start, look also at the day before
           IF (sec.GT.sec3) Jd1=Jd1-Cycle

c schedule time is after the simulation stop time, move Jd2 to the day before st
           IF (sec.GT.sec2) Jd2=Jd2-Cycle
c           write(*,*)'jd1,jd2=',jd1,jd2
c           call ho('in Dupltm','')
           expected=(Jd2-Jd1)/cycle+1
           IF (expected .GT. MaxEl-pEL) THEN
c there are more events from this schedule time than place in the array
	      WriteFirst=1
	      GOTO 90
           ENDIF

c loop through all daily events
	   DO 20 I=Jd1,Jd2

                SecSim=JulX(JdStart,sec1,I,sec)
c           write(*,*)'secsim=',secsim
c           call ho('in loop in Dupltm','')
            if (secsim.le.0) then
              call JustBefore(SchTyp,SchNa,SecSim,Line)
            end if

C@tno jcp 1996Jul02_22:59:21 keep track of the just before StartTime values
                if ((i.lt.jdsched).or.
     &              (i.eq.jdsched .and. sec.lt.sec3))
     &          then
                  goto 20
                end if

                EL(pEL)=SecSim
		EL2(pEL)=RecNr
                EL3(pEL)=Element3
c           write(*,*)'in dupltm stored, intime=',intime,'pel=',pel,
c     &           'Sec=',el(pel)
                pEL=pEL+1
                InTime=.TRUE.

c           call ho(' ',' ')
20         CONTINUE
	   GOTO 99
	ENDIF

C------------------------------------------- weekday data: repeated every week
C Jday0=monday .. Jday6=sunday
C@tno jcp 1996Apr19_14:50:52 RtimDat returns MON as Jday=0
31      IF (Jday.LE.6 .AND. Jday.GE.0) THEN
          Cycle=7
          Jd1=JdStart
          Jd2=JdStop
c          write(*,*)'Jd1,jd2',Jd1,jd2

C@tno jcp 1996Jul17_13:17:29 look for the first day that matches
32        weekday=MOD(Jd1+1,7)
          if (weekday.eq.0) weekday=7
c          write(*,*) 'weekday,jd1,jday=',weekday,jd1,jday
          IF (weekday.NE.(Jday+1)) THEN
            Jd1=Jd1+1
c          write(*,*) 'Jd+1 weekday,jd1=',weekday,jd1
            goto 32
          end if

c schedule time is after the schedule start, look also at the week before
          IF (jd1.gt.jdstart .or.
     &     (jd1.eq.jdstart .and. sec.GT.sec3)) Jd1=Jd1-Cycle
c          write(*,*) 'Jd-Cycle? weekday,jd1=',weekday,jd1

C@tno jcp 1996Jul17_13:17:29 look for the last day that matches
33        weekday=MOD(Jd2+1,7)
          if (weekday.eq.0) weekday=7
c          write(*,*) 'weekday,jd2,jday=',weekday,jd1,jday
          IF (weekday.NE.(Jday+1)) THEN
            Jd2=Jd2-1
            goto 33
          end if

c schedule time is after the simulation stop time, move Jd2 to the week before s
           IF (jd2.gt.jdstop .or.
     &      jd2.eq.jdstop .and. sec.GT.sec2) Jd2=Jd2-Cycle

c        write(cof,*) '0<=JDay<6'
           expected=(Jd2-Jd1)/cycle+1
           IF (expected.GT. MaxEl-pEL) THEN
	      WriteFirst=2
	      GOTO 90
           ENDIF

           DO 30 I=Jd1,Jd2
C weekday from JDay: +1 mod 7 weekday0=7
                weekday=MOD(I+1,7)
                if (weekday.eq.0) weekday=7
c               write(cof,*),'weekday=',weekday
C must be Jday+1;this is weekday number 1..7
                IF (weekday.EQ.(Jday+1)) THEN
c                 write(cof,*),'weekday=Jday'
                  SecSim=JulX(JdStart,sec1,I,sec)
c                 write(*,*)'in dupltm weekdata, SecSim=',SecSim
                    if (secsim.le.0) then
                      call JustBefore(SchTyp,SchNa,SecSim,Line)
                    end if

C@tno jcp 1996Jul02_22:59:21 keep track of the just before StartTime values
                  if ((i.lt.jdsched).or.
     &               (i.eq.jdsched .and. sec.lt.sec3))
     &               then
                    goto 30
                  end if

                  EL(pEL)=SecSim
                  EL2(pEL)=RecNr
                  EL3(pEL)=Element3
c           write(*,*)'in dupltm stored, intime=',intime,'pel=',pel,
c     &           'Sec=',el(pel)
                  pEL=pEL+1
                  InTime=.TRUE.

c           call ho(' ',' ')
                ENDIF
30	   CONTINUE
	   GOTO 99
	ENDIF

C------------------------------------------------------- Weekend / working day
C@tno jcp 1996Jun13_16:06:21 new part to analyze WDY WND weekday /weekend
C WDY or WND = working day or weekend repeated every week
C Jday7=monday..friday  Jday8=saturday,sunday
C@tno jcp 1996Apr19_14:50:52 RtimDat returns MON as Jday=0
61      IF (Jday.EQ.7 .OR. Jday.EQ.8) THEN
c        write(cof,*) '7= WDY Weekday or 8= WND weekend'
          Cycle=7
          Jd1=JdStart
          Jd2=JdStop

c loop to get the first day at or after Jd1 that matches Weekend/weekday
          istep=1
62        weekday=MOD(Jd1+1,7)
          if (weekday.eq.0) weekday=7
          if (weekday.lt.6) then
C mon..fri
            weekday=7
          else
C sat,sun
            weekday=8
          end if
          IF (weekday.NE.(Jday)) THEN
            Jd1=Jd1+istep
            goto 62
          end if

c event is after the simulation start, look also at days before
          IF (jd1.gt.jdstart .or.
     &     jd1.eq.jdstart .and. sec.GT.sec3) then
            istep=-1
            Jd1=Jd1+istep
C still look further back for a day that matches
            goto 62
          end if



c loop to get the first day at or before Jd2 that matches Weekend/weekday
          istep=-1
63        weekday=MOD(Jd1+1,7)
          if (weekday.eq.0) weekday=7
          if (weekday.lt.6) then
C mon..fri
            weekday=7
          else
C sat,sun
            weekday=8
          end if
          IF (weekday.NE.(Jday)) THEN
            Jd1=Jd1+istep
            goto 63
          end if


c schedule time is after the simulation stop time, move Jd2 further back
           IF (jd2.gt.jdstop .or.
     &      jd2.eq.jdstop .and. sec.GT.sec2) then
              istep=-1
              Jd2=Jd2+istep
C still look further back for a day that matches
              goto 63
            end if

C@tno jcp 1996Jul17_13:15:06 now there could be one event per day
           expected=(Jd2-Jd1)+1
           IF (expected.GT. MaxEl-pEL) THEN
              WriteFirst=5
	      GOTO 90
           ENDIF

           DO 60 I=Jd1,Jd2
C@tno jcp 1996Apr29_16:15:32 weekday from JDay: +1 mod 7 weekday0=7
c                weekday=MOD(I,7)
                weekday=MOD(I+1,7)
                if (weekday.eq.0) weekday=7
c                write(cof,*),'weekday=',weekday
                 if (weekday.lt.6) then
C mon..fri
                   weekday=7
                 else
C sat,sun
                   weekday=8
                 end if

                IF (weekday.EQ.(Jday)) THEN
c                 write(cof,*),'weekday=Jday'
                   SecSim=JulX(JdStart,sec1,I,sec)
c           write(*,*)'in dupltm week/weekend, SecSim=',SecSim

                    if (secsim.le.0) then
                      call JustBefore(SchTyp,SchNa,SecSim,Line)
                    end if

C@tno jcp 1996Jul02_22:59:21 keep track of the just before StartTime values
                  if ((i.lt.jdsched).or.
     &               (i.eq.jdsched .and. sec.lt.sec3))
     &               then
                    goto 60
                  end if

c                  write(cof,*),'weekday=Jday and in time'
                   EL(pEL)=SecSim
		   EL2(pEL)=RecNr
                   EL3(pEL)=Element3
c           write(*,*)'in dupltm stored, intime=',intime,'pel=',pel,
c     &           'Sec=',el(pel)
                   pEL=pEL+1
		   InTime=.TRUE.

c           call ho(' ',' ')
                ENDIF
60         CONTINUE
	   GOTO 99
	ENDIF
C@tno jcp 1996Apr19_14:53:57 end WDY WND

C----------------------------------- month data, any year: repeated every year
41	IF (Jday.GE.1721424 .AND. Jday.LE.1721788) THEN
          Cycle=1
          iY1=IYYY1
          iY2=IYYY2
          CALL JULD(Jday,MM0,ID,IY1,errkey)
c schedule time is after the schedule start, look also at the year before
          IF (Jday.gt.Jdstart .or.
     &        Jday.eq.Jdstart .and. sec.GT.sec3) iY1=iY1-Cycle

          CALL JULD(Jday,MM0,ID,IY2,errkey)
c schedule time is after the the stop date_time at the last year, move one year
          IF (Jday.gt.Jdstop .or.
     &        Jday.eq.Jdstop .and. sec.GT.sec2) iY2=iY2-Cycle

           expected=(iY2-iY1)/cycle+1
           IF (expected.GT. MaxEl-pEL) THEN
	      WriteFirst=3
	      GOTO 90
	   ENDIF

           DO 40 I=IY1,IY2
	      CALL JULD(Jday,MM0,ID,I,errkey)

              SecSim=JulX(JdStart,sec1,Jday,sec)
c           write(*,*)'in dupltm month date, SecSim=',SecSim
                    if (secsim.le.0) then
                      call JustBefore(SchTyp,SchNa,SecSim,Line)
                    end if

                  if ((i.lt.jdsched).or.
     &               (i.eq.jdsched .and. sec.lt.sec3))
     &               then
                    goto 40
                  end if

              EL(pEL)=SecSim
	      EL2(pEL)=RecNr
	      EL3(pEL)=SchTyp*f+SchNa
c           write(*,*)'in dupltm stored, intime=',intime,'pel=',pel,
c     &           'Sec=',el(pel)
              pEl=pEl+1
	      InTime=.TRUE.

c           call ho(' ',' ')
40         CONTINUE
	   GOTO 99
	ENDIF

C-------------------------------- absolute date with year, month, day and time
C@tno jcp 1996May03_12:42:48 trick with Jdays did not allow negative years, if
C extended to LT 1721424
C CalDat has a problem with the negative years because of the definition of
C leap yers
51      IF (Jday.GT.8 .and. Jday.LT.1721424
     &          .or. Jday.GT.1721788) THEN
c full absolute date_time
	   IF (Jday.EQ.JdStop  .AND. sec.GT.sec2)   GOTO 99
c after simulation_stop
           IF (Jday.GT.JdStop) GOTO 99

           SecSim=JulX(JdStart,sec1,Jday,sec)
c           write(*,*)'in dupltm abs date, SecSim=',SecSim
c           call ho(' ',' ')
             if (secsim.le.0) then
               call JustBefore(SchTyp,SchNa,SecSim,Line)
             end if
           IF (Jday.LE.JdSched) then
c before/at schedule_start
c             write(*,*)'in dupltm abs date called just before SecSim=',
c     &        SecSim
c             call ho(' ',' ')
             IF (Jday.LT.JdSched .or.
     &           (Jday.EQ.JdSched .AND. sec.LT.sec3)) GOTO 99
           end if
c           write(*,*)'in dupltm storing, SecSim=',SecSim
c           call ho(' ',' ')

	   IF (pEL .GT. MaxEl) THEN
c           write(*,*)'in dupltm pel>max'
c           call ho(' ',' ')
              WriteFirst=4
	      GOTO 90
           ENDIF

           EL(pEL)=SecSim
	   EL2(pEL)=RecNr
C@tno jcp 1996Jul01_14:20:52 Element3 assigned before loop
CC           EL3(pEL)=SchTyp*f+SchNa
           EL3(pEL)=Element3
c           write(*,*)'in dupltm stored, intime=',intime,'pel=',pel,
c     &           'Sec=',el(pel)
           pEL=pEL+1
	   InTime=.TRUE.
c           call ho(' ',' ')
           GOTO 99
	ENDIF


C sort array and merge into TMS file -----------------------------------

99      CONTINUE
c        write(*,*) 'went to 99, no sorting of array'
	IF (pEL.LT.MaxEl) GOTO 400
	IF (cont.EQ.'REUSE') GOTO 89
C@tno jcp 1996Jul04_12:44:12 may 2 be sorted?
CC90      IF (sort.EQ.' ' .AND. pEL.GT.2) then
90      IF (sort.EQ.' ' .AND. pEL.GT.2) then
c         write(*,*) 'sorting array'
c         call ho(' ',' ')
          CALL SORT4(pEL-1,EL,EL2,EL3)
          
        end if

C@tno jcp 1996Jun17_18:10:04 put meteo schedules on the first line of an interva
        InewTime=1
        NewTime=EL(1)
        Do 500 i=1,pEL-1
          if (el(i).gt.newtime) then
c this is a new time interval
            Newtime=EL(i)
            iNewTime=i
          end if
          if (i.gt.iNewtime .and.
     &       el3(i).ge.met1 .and. el3(i).lt.met2) then
c this is a meteoschedule that is not the first line for this interval
c            write(cof,*) 'before swap e3met e3',el3(i),el3(iNewtime)
            call swapi(el2(i),el2(iNewTime))
            call swapi(el3(i),el3(iNewTime))
c            write(cof,*) 'after  swap e3met e3',el3(iNewtime),el3(i)
          end if

c          if (el3(i).ge.met1 .and. el3(i).lt.met2) then
c            write(*,*) el(i),el2(i),el3(i), ' meteo'
c          else
c            write(*,*) el(i),el2(i),el3(i)
c          end if

          if (MOD(i,18).eq.0) then
c            call ho('sorting',' ')
          end if
500     continue

c        call ho('in dupl tm after sort loop 500',' ')
C@tno jcp 1996Jun17_18:10:04 end meteo schedules first



	Switch=MOD(Switch+1,2)
C flname = name of temporary file, flname2 = name of current TMS file
	IF (Switch.EQ.0) THEN
	   flname='COMIS.TMS'
	   flname2='COMIS2.TMS'
	ELSE
	   flname='COMIS2.TMS'
	   flname2='COMIS.TMS'
        ENDIF
c        write(*,*) 'writing TMS data to  ',flname
c        write(*,*) 'reading TMS data from',flname2
	OPEN(TMM,file=flname,access='direct',recl=30,
     &	form='unformatted')
	OPEN(TMS,file=flname2,access='direct',recl=30,
     &	form='unformatted', STATUS = 'OLD')
C write stop time as the first line into file
	WRITE(TMM,REC=1) jdstop,0,sec2
	pTMM=2
	pTMS=2
	I=1
	READ(TMS,REC=pTMS) e1,e2,e3
c        write(*,*) 'read  file',flname2,e1,e2,e3
100	IF (I.GE.pEL) GOTO 110
	IF (EL(I).LT.e1) THEN
c           write(*,*) 'write array',flname,EL(I),EL2(I),EL3(I)
           WRITE(TMM,REC=pTMM) EL(I),EL2(I),EL3(I)
	   I=I+1
        ELSEIF (EL(i).eq.e1) then
c same time
          if (e3.ge.met1 .and. e3.lt.met2) then
c file had meteo, write first
c           write(*,*) 'write meteo ',flname,e1,e2,e3
            WRITE(TMM,REC=pTMM) e1,e2,e3
            pTMS=pTMS+1
            READ(TMS,REC=pTMS) e1,e2,e3
c        write(*,*) 'read file',flname2,e1,e2,e3
            IF (e3.EQ.-1) GOTO 222
          else
            if (el3(i).ge.met1 .and. el3(i).lt.met2) then
c array had meteo, write first (if file AND array both have meteo, array comes a
c           write(*,*) 'write meteo array',flname,EL(I),EL2(I),EL3(I)
              WRITE(TMM,REC=pTMM) EL(I),EL2(I),EL3(I)
              I=I+1
            else
c no meteo, write file (current array value will come later)
c           write(*,*) 'write file',flname,e1,e2,e3
              WRITE(TMM,REC=pTMM) e1,e2,e3
              pTMS=pTMS+1
              READ(TMS,REC=pTMS) e1,e2,e3
c        write(*,*) 'read file',flname2,e1,e2,e3
              IF (e3.EQ.-1) GOTO 222
            end if
          end if
        ELSE
c file comes first array later
c           write(*,*) 'write file',flname,e1,e2,e3
	   WRITE(TMM,REC=pTMM) e1,e2,e3
	   pTMS=pTMS+1
	   READ(TMS,REC=pTMS) e1,e2,e3
c        write(*,*) 'read file',flname2,e1,e2,e3
	   IF (e3.EQ.-1) GOTO 222
	ENDIF
	pTMM=pTMM+1
	GOTO 100
C write rest of TMS file into temporary file --------------------
110     CONTINUE
c        write(*,*) 'write file',flname,e1,e2,e3
        WRITE(TMM,REC=pTMM) e1,e2,e3
	pTMS=pTMS+1
	pTMM=pTMM+1
	READ(TMS,REC=pTMS,IOSTAT=iocheck) e1,e2,e3
c        write(*,*) 'read file',flname2,e1,e2,e3
	IF (iocheck.NE.0) GOTO 333
	GOTO 110
C write rest of array into temporary file -----------------------
222	pTMM=pTMM+1
c        write(*,*) 'write array',flname,EL(I),EL2(I),EL3(I)
	WRITE(TMM,REC=pTMM) EL(I),EL2(I),EL3(I)
	I=I+1
	IF (I.LT.pEL) GOTO 222
C write last line into temporary file ---------------------------
	pTMM=pTMM+1
c        write(*,*) 'write  file',flname,e1,e2,e3
	WRITE(TMM,REC=pTMM) e1,e2,e3
	pTMM=pTMM+1
C close files ---------------------------------------------------
333	CLOSE(TMS,status='delete')
	CLOSE(TMM)
	NSteps=pTMM-1
89	pEL=1
C@tno jcp 1996Jun13_16:31:01 inserted 61 to goto for WDY and WND
CC        GOTO (21,31,41,51) WriteFirst
        GOTO (21,31,41,51,61) WriteFirst
400     CONTINUE
c        call ho('at end of dupltm ',' ')
        RETURN
	END
