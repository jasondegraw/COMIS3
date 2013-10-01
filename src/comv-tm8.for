C+*********************************************************** comv-tm8.f
Ch**********************************************************************

C@tno jcp 1996May28_11:45:48 in CreateTM 'number' renamed to NSecSim
        DOUBLE PRECISION FUNCTION CreateTM(NSecSim)
C**********************************************************************
C       Create the Lotus time from the NSecSim  of seconds since the start of th
C       simulation
C@tno jcp 1996Apr02_11:03:05 NSecSim is a normal 4 byte fortran INTEGER I
C assume. Its range is 0 to 2,147,483,648 which is 68 years, the limit for
C a simulation run with Comis. (if it were a 2 byte INTEGER Comis would crash
C after 9 hours)
C
C lbl bs 18april, 1991
C
C Purpose: Create time format for LOTUS
Ch*********************************************************************
        IMPLICIT NONE
	INCLUDE 'comv-inp.inc'
        INTEGER NSecSim
C Julian Day of 1 Jan. 1900 is 2415021. Since Lotus does not know that
C 1900 was no leap-year I have to add 1 when the date is after feb 28
C 1900.
        CREATETM=JdStart+(sec1+NSecSim)/86400.D0-2415020.D0
	IF (CREATETM.GE.60.D0) CREATETM=CREATETM+1.D0
	RETURN
	END

Ch*********************************************************************

        INTEGER FUNCTION NumCreate(TLotus)
C**********************************************************************
C       Create the NSecSim number of seconds since the start of the
C       simulation from the Lotus TLotus (inverse of createTM)
C@tno jcp 1996May01_18:40:06
Ch*********************************************************************
        IMPLICIT NONE
	INCLUDE 'comv-inp.inc'
        INCLUDE 'comv-uni.inc'
        DOUBLE PRECISION TLotus
        DOUBLE PRECISION TLotus1
C Julian Day of 1 Jan. 1900 is 2415021. Since Lotus does not know that
C 1900 was no leap-year I have to add 1 when the date is after feb 28
C 1900.
        TLotus1=TLotus
        IF (TLotus1.GE.61.D0) TLotus1=TLotus1-1.D0
        NumCreate=IDNINT((TLotus1-DBLE(Jdstart)+2415020.D0)*86400)-sec1
	RETURN
	END

C@tno jcp 1996May28_11:42:08 'number' renamed to NSecSim the number of seconds
C since start of the simulation
Ch*********************************************************************
        SUBROUTINE CreateJdTim(NSecSim,Jday,NSecDay)
C**********************************************************************
C       Create the JDay (Julian day number) and NSecDay (number of seconds
C       since the start of the day) from the NSecSim number of seconds since
C       the start of the simulation

C@tno jcp 1996Apr02_11:03:05 NSecSim is a normal 4 byte fortran INTEGER I
C assume. Its range is 2.147.000.000 that is 68 year, which is the limit for
C a simulation run with Comis. (if it were a 2 byte INTEGER Comis would crash
C after 9 hours)
C
C@tno jcp 1996May28_11:50:40
C
Ch*********************************************************************
        IMPLICIT NONE
	INCLUDE 'comv-inp.inc'
        INCLUDE 'comv-uni.inc'
        INTEGER NSecSim,jDay,NSecDay
        INTEGER Days
C Julian Day of 1 Jan. 1900 is 2415021. Since Lotus does not know that
C 1900 was no leap-year I have to add 1 when the date is after feb 28
C 1900.
c        write(cof,*),'NSecSim=',NSecSim
        days=(sec1+NSecSim)/86400
        JDay=JdStart+Days
        NSecDay=(sec1+NSecSim)-Days*86400.0
C@empa aw 2005may03 process negative NsecDay correctly
        IF(NSecDay<0)THEN
           JDay=Jday-1
	     NSecDay=86400+NSecDay
        ENDIF 
        RETURN
	END


Ch*********************************************************************

C@tno jcp 1996May28_11:43:44 Sec renamed to NSecDay
        DOUBLE PRECISION FUNCTION JdayLotus(Jday,NSecDay)
C**********************************************************************
C       Create the Lotus time from the Julian day and the number of seconds on
C       that day from 0:00
C@tno jcp 1996May01_11:12:23
Ch*********************************************************************
        IMPLICIT NONE
	INCLUDE 'comv-inp.inc'
        INTEGER Jday,NSecDay

C Julian Day of 1 Jan. 1900 is 2415021. Since Lotus does not know that
C 1900 was no leap-year I have to add 1 when the date is after feb 28
C 1900.
c        write(*,*) 'Jday in F(jdayLotus)=', Jday
        JdayLotus=Jday*1.D0+NSecDay/86400.D0-2415020.D0
c        write(*,*) 'JdayLotus=', JdayLotus
        IF (JdayLotus.GE.60.D0) JdayLotus=JdayLotus+1.D0
	RETURN
	END

Ch*********************************************************************
C@tno jcp 1996Jun28_12:52:41 new function like JdayLotus and NumCreate
        INTEGER FUNCTION JdaySecSim(Jday1,NSecDay,NsSim)
C                                   Ii    Ii      Ii
C                                   |     |       'current' time
C                                   |     seconds from 00:00 on Jday1
C                                   Julianday of this schedule/event
C**********************************************************************
C       Create NSecSim, the Number of seconds since simulation start, from
C       the event on Julian day Jday1 and the number of seconds on that day
C       from 00:00.
C       If Jday1 is an incomplete date (month, weekday, time only) the current
C       or next event is calculated. NsSim is the current time as seconds from
C       the start of the simulation.
C       The simulation starts on Jdstart and sec1
C       The returned number of seconds is clipped at INTMax (+/-2147438647)
C       that happens for Jday's more than 69 year apart.
C
Ch*********************************************************************
        IMPLICIT NONE
	INCLUDE 'comv-inp.inc'
        INTEGER NSecDay,NsSim
        INTEGER NSecEvent,CurJday,NSecCurDay,DT,Dif
        INTEGER JdEvent,Iy,Im,Id,CurIy,CurIm,CurId
        INTEGER Jday1,CurWeekDay,EventWeekday,errkey

c current time of day (since 00:00)
        NSecCurDay=MOD(NsSim+sec1,86400)
c current julian day number
c                      delta_S start - now            =delta_days
        CurJday=Jdstart+(NsSim+sec1-NsecCurDay)/86400
c        write(*,*)'NsSim=',NsSim
c        write(*,*)'NsCurday=',NsecCurDay
c        write(*,*)'curjulday=',curjday
c number of seconds to the event on a 24 h clock (not looking at the difference
        DT=NsecDay-NsecCurday
c        Write(*,*)'NsSim+sec1',NsSim,sec1
c        Write(*,*)'DT=NsecDay-NsecCurday',DT,NsecDay,NsecCurday
c        call ho(' ',' ')

C time only: repeated every day
        IF (Jday1.EQ.-1) THEN
C          number of seconds from current time to the event time
           if (DT.lt.0) then
C            next event = next day
             NSecEvent=NsSim+86400+DT
           else if (DT.gt.0) then
C            next event = DT seconds further
             NSecEvent=NsSim+DT
           else
C            next event is now!
             NSecEvent=NsSim
           end if
        ENDIF


C weekday data: repeated every week
C Jday0=monday .. Jday6=sunday
C@tno jcp 1996Apr19_14:50:52 RtimDat returns MON as Jday=0
        IF (Jday1.LE.6 .AND. Jday1.GE.0) THEN
C@tno jcp 1996Apr29_16:15:32 weekday from JDay1: +1 mod 7 weekday0=7
           CurWeekday=MOD(CurJday+1,7)
           if (CurWeekday.eq.0) CurWeekday=7

           EventWeekday=MOD(Jday1+1,7)
           if (EventWeekDay.eq.0) EventWeekDay=7

           NsecEvent=NsSim+(EventWeekDay-CurWeekDay)*86400+DT

           Dif=NsecEvent-NsSim

           if (Dif.lt.0) then
C next event = next day
             NsecEvent=NsSim+86400*7+Dif
           else if (Dif.gt.0) then
C next event = DT seconds further
             NsecEvent=NsSim+Dif
           else
C next event is now!
             NSecEvent=NsSim
           end if
           goto 99
        ENDIF

C part to analyze WDY WND weekday /weekend
C WDY or WND = working day or weekend repeated every week
C Jday7=monday..friday  Jday8=saturday,sunday
C MON = Jday1=0
        IF (Jday1.EQ.7 .OR. Jday1.EQ.8) THEN
           CurWeekday=MOD(CurJday+1,7)
           if (CurWeekday.eq.0) CurWeekday=7

           if (Jday1.eq.7) then
C working day:  days 1..5
             if (DT.gt.0) then
               if (CurWeekday.lt.6) then
C mon..friday event still to come on the 24 h clock
                 NsecEvent=NsSim+DT
               else
C sat or sun
                 if (CurWeekday.eq.6) then
C saturday
                   NsecEvent=NsSim+86400*2+DT
                 else
                   NsecEvent=NsSim+86400+DT
                 end if
               end if
             else
C Dt<0 event already passed on the 24 h clock
               if (CurWeekday.lt.5) then
C mon..thursday: event will come next day
                 NsecEvent=NsSim+86400+DT
               else
C friday..sun  : next event will be monday =(8-curweekday) days ahead
C (8-CurWeekday) =1 on sunday(7) =2 on saturday(6) =3 on friday(5)
                 NsecEvent=NsSim+86400*(8-Curweekday)*3+DT
               end if
             end if
C end if of DT at workingday
           else
C Jday=8 event is a weekend: days 6,7
C working day:
             if (DT.gt.0) then
               if (CurWeekday.gt.5) then
C sat, sun: event still to come on the 24 h clock
                 NsecEvent=NsSim+DT
               else
C working day: next event is Saturday. (6-CurWeekday) days ahead
                 NsecEvent=NsSim+86400*(6-Curweekday)+DT
               end if
             else
C Dt<0 event already passed on the 24 h clock
               if (CurWeekday.eq.6) then
C saturday : event will come next day (sunday)
                 NsecEvent=NsSim+86400+DT
               else if(curweekday.eq.7) then
C sun : next event will be saturday
                 NsecEvent=NsSim+86400*6+DT
               else
C mon..fri: next event will be saturday
                 NsecEvent=NsSim+86400*(6-Curweekday)+DT
               end if

             end if
C end if of DT at weekend
           end if
           goto 99
        end if
C@tno jcp 1996Apr19_14:53:57 end WDY WND

C month data, any year: repeated every year
        IF ((Jday1.GE.1721424 .AND. Jday1.LE.1721788) 
C@empa aw 2005may02 jday of "jan31_24:00" is 1721789  
     &      .or.(Jday1==1721789.and.NSecDay<3600))	  THEN
c        write(*,*) 'jday, sec',jday1,nSecDay
c        call ho('in Jday SecSim: month +day ',' ')
           CALL Caldat(Jday1,iy,im,id)
c           write(*,*)'from event iy,im,id',iy,im,id
           CALL Caldat(CurJday,CurIy,CurIm,CurId)
c           write(*,*)'from CurDay CurIy,CurIm,CurId',CurIy,CurIm,CurId
           Call JULD(JdEvent,im,id,CurIy,errkey)
c        write(*,*) 'CurJday  ',CurjDay
c        write(*,*) 'Jdayevent',Jdevent
c        write(*,*) 'DT=',DT
           if (DT.ge.0) then
             if (JdEvent.gt.CurJday) then
c event still to come this year
               NsecEvent=NsSim+DT+86400*(JdEvent-CurJday)
             else if (JdEvent.eq.CurJday) then
c event this day
               NsecEvent=NsSim+DT
             else
c event passed this year so try next year
               Call JULD(JdEvent,im,id,CurIy+1,errkey)
               NsecEvent=NsSim+DT+86400*(JdEvent-CurJday)
             end if
           else
c DT<0 so the event is in a next 24 hour (depending on the date)
             if (JdEvent.gt.CurJday) then
c event still to come this year
               NsecEvent=NsSim+DT+86400*(JdEvent-CurJday)
             else
c event passed this year so try next year
               Call JULD(JdEvent,im,id,CurIy+1,errkey)
               NsecEvent=NsSim+DT+86400*(JdEvent-CurJday)
             end if
           end if
c           write(*,*) 'NsecEvent',NsecEvent,NsSim
           goto 99
	ENDIF

C absolute date with year, month and day:
C@tno jcp 1996May03_12:42:48 trick with Jdays did not allow negative years, if
C extended to LT 1721424
C CalDat has a problem with the negative years because of the definition of
C leap yers
        IF (Jday1.LT.1721424 .or. Jday1.GT.1721788) THEN
           CALL Caldat(CurJday,CurIy,CurIm,CurId)
c           write(*,*)'Routine JdaySimSec: Absolute data +Time'
c           write(*,*)'CurJday,CurIy,CurIm,CurId',
c     &      CurJday,CurIy,CurIm,CurId
c           write(*,*) 'Jday1',Jday1

           if (DT.gt.0) then
c             write(*,*) 'DT>0 DT=',DT
             if (Jday1.gt.CurJday) then
c event still to come
               NsecEvent=NsSim+DT+86400*(Jday1-CurJday)
             elseif (Jday1.eq.CurJday) then
c event this day
               NsecEvent=NsSim+DT
             else
c event passed NsecEvent is negative
               NsecEvent=NsSim+DT+86400*(Jday1-CurJday)
             end if
           else
c               write(*,*) 'DT<= DT=',DT
c DT<0 so the event is in a next 24 hour (depending on the date)
             if (Jday1.gt.CurJday) then
c event still to come
               NsecEvent=NsSim+DT+86400*(Jday1-CurJday)
             else
c event passed Nsecevent<0
               NsecEvent=NsSim+DT+86400*(Jday1-CurJday)
             end if
           end if
c          call ho('abs date in JdaySecsim',' ')
          GOTO 99
	ENDIF

99      continue
        JdaySecSim=NsecEvent

	RETURN
	END


Ch*********************************************************************
        INTEGER FUNCTION LotusJday(TLotus)
C**********************************************************************
C       calculate the Julian day number from the LOTUS time
C
C@tno jcp 1996Apr29_20:40:02
C
Ch*********************************************************************
        IMPLICIT NONE
	INCLUDE 'comv-inp.inc'
        DOUBLE PRECISION TLotus,TLotus2
        INTEGER Jday1
C Julian Day of 1 Jan. 1900 is 2415021. Since Lotus does not know that
C 1900 was no leap-year I have to add 1 when the date is after feb 28
C 1900.
        TLotus2=TLotus
        IF (TLotus2.GE.61.D0) TLotus2=TLotus2-1.D0
        Jday1=INT(TLotus2)+2415020
        LotusJday=Jday1
	RETURN
	END
Ch*********************************************************************

	SUBROUTINE CLOSETMS
C**********************************************************************
C
C lbl bs may 06 1991
C
Ch*********************************************************************

        IMPLICIT NONE
	INCLUDE 'comv-uni.inc'
	INCLUDE 'comv-inp.inc'

	IF (keep.EQ.'KEEP') THEN
	   CLOSE(TMS)
	ELSE
	   CLOSE(TMS,status='DELETE')
	ENDIF
	RETURN
	END
C**********************************************************************
