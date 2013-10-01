C+*********************************************************** comv-tim.f
C@NBI PGS 2000Jul21 - Added comments about passed subroutine arguments.
Ch***********************************************************************
C@tno jcp 1996Jun17_16:14:48 added variab
CC        SUBROUTINE rtimdat(T,JDAY,IYYY,MM,ID,H,M,S,ERRKEY)
        SUBROUTINE rtimdat(T,JDAY,IYYY,MM,ID,H,M,S,ERRKEY,variab)
C***********************************************************************
C READ IYYY upto S from the string T. Formats can be different:
C 1968may23_ or 19680523_ for the date (Year can be omitted THEN as a default
C year 1 is returned, because year 0 doesnot exist)
C overview of tricks to handle time, weekday, monthday
C                           Comis uses
C        input              julian day         year           valid for
C      time (no date)          -1              -4714          every day
C    MON..SUN(_time)          0..6         -4714..-4713       every week
C    WDY..WND(_time)          7..8            -4713           every week
C    MonthDay(_time)    1721424..1721788        1             every year
C    YYYYMMDD_hh:mm:ss         ..                        exactly that date_time
C
C If only time is input Comis uses -4714dec30 which is Julian day -1
C
C If a week day is used Comis uses -4714dec31 for monday JulDay=0
C -4713jan01 tuesday=1 etc ..-4713jan06  sunday=6
C
C month and days
C set the year to 1 . THEN the Julian day number will be 1721424...1721788 and
C in COMIS we will only interpret the month and day then (regardless of the
C year) . Year 1 is not a leap-year s feb29 does not exist.
C
C 8 or 8: or 830 or 8:30 or 83002 or 8:30:02 are valid time inputs
C the complete string T can be 1968may23_83002
C not allowed is may1 (should be may01)
C
C  Pass parameters:
C  IO   Name   Units Description
C  I    T      -     Time/date string
C   O   JDAY   -     Julian day
C   O   IYYY   -     Year
C   O   MM     -     Month
C   O   ID     -     Day of month
C   O   H      -     Hour
C   O   M      -     Minute
C   O   S      -     Seconds
C   O   Errkey -     =1 if error occurred during processing input string T, else =0
C   -   variab -     There is no intention to input/output variab here,
C                    but "variab" has necessarily been added as an argument 
C                    just to get its address, so that we don't need to 
C                    INCLUDE CONV-INP.INC for calls to routines GETWI or GETWR.
C Created: hcp aug 3 1989
C Modifications:
C@empa   VD 12.mar.91  Error key, error handling
C@tno jcp 1996May02_16:56:43 if the time goes beyond 23:59:59, then
C excess time is converted to extra days, that are added to JulD
C
C ERROR return if: Can't comprehend date/time string "T", for the following reasons:
C                  - Year number zero
C                  - Not a valid month number or string (could also be
C                    a jarbled weekday)
C                  - "time" is empty string;
C                  - Minutes or seconds >= 60
Ch***********************************************************************

        IMPLICIT NONE
        include 'comv-uni.inc'

C passed parameters
C@tno jcp 1996Jun17_16:15:10 added variab
CC        CHARACTER*(*) T
        CHARACTER*(*) T,variab
C@empa aw 2000jan14 errkey1
        INTEGER Jday,IYYY,MM,ID,H,M,S,ERRKEY,ERRKEY1
C local parameters
	CHARACTER time*160,date*160,pattrn*1
        INTEGER L,pos,start,FJday,extradays
        INTEGER DAYNR

        INTEGER LENSTR,ISTRSTR
C@empa aw 2000apr11 lenw,EndA,begina,found
        INTEGER lenw,EndA,begina,found
C-----
C FJday flags if the JDAY has been set or not , this can be done by the weekday
	FJday=0
	H=0
	M=0
	S=0
        extradays=0
	ERRKEY=0

c        write(*,*) 'rtimdat t=',t
c        call ho('rtimdate','')

	l=lenstr(T)
C@empa aw 2000apr11 Test for the length of the alpha substring. Even for unique
C@empa aw           month strings like 'O' for october, we get a wrong result from 
C@empa aw           DAYQ ('mOn') and RDate will then not be called.    
	CALL lowerc(T)
C call FAlpha to find the first substring that consists of characters a..z only
	CALL FAlpha(T,L,begina,EndA,found)
	IF (FOUND.EQ.1) THEN
	  lenw=EndA-begina+1
        IF (lenw.lt.3) THEN
C@NBI PGS 2000Jul21 - We need to know where the error occurs in .CIF file,
C@NBI                 so reduce severity flag to 2 and return to calling
C@NBI                 routine where error message will be given.
CC        CALL INERR('Time string: '//T(1:l),'Month or weekday '//
CC   &               'name has to be at least 3 characters long!',
CC   &               .false.,3)
          CALL INERR('Date/time string: '//T(1:l),
     &    'Month or weekday name must be at least 3 characters long!',
     &               .false.,2)
C@empa aw 2005apr25 set errkey and return to get the more precise message
          errkey=1	      
        ENDIF
C@empa aw 2005apr25 chaeck for a valid month or weekday name
      IF (INDEX('janfebmaraprmayjunjulaugsepoctnovdecmontuewed'//
     &      'thufrisatsunwdywnd',T(begina:begina+2))==0)THEN
          CALL INERR('Date/time string: '//T(1:l),
     &    'Name of month or weekday is not correct!',
     &               .false.,2)
          errkey=1	      
      ENDIF

      ENDIF 
	IF (ERRKEY.EQ.1) GOTO 900

C@empa aw 2000apr11 end patch


C scan the string for a day name: mon tue wed thu fri sat sun
C If found FJDAY=1 skip the other date procedures, and POS is set to the
C character after the last character of the weekday to be able to decide
C if there is still a time after the weekday
	CALL DAYQ(T,L,DAYNR,JDAY,TIME,FJDAY,pos,errkey)
	IF (ERRKEY.EQ.1) GOTO 900

	IF (FJDAY.EQ.0) THEN
C the Julian Day Number has not been found yet
	    start=0
	    pattrn='_'

            pos=istrstr(T,pattrn,start)
C@tno jcp 1996Jun10_18:13:07 also look for mmm in case_ has been omitted
c            if (pos.eq.0) then
c              errkey=0
c              call monthnr(T,out,errkey)
c              if (errkey.eq.0) then
c                pos=lenstr(out)
c                T=out
c              end if
c            end if
C@tno jcp 1996Jun10_18:13:07 end


	    IF (pos.GT.0) THEN
C the string T contains a date_

	       date=T(1:pos-1)

	       IF (L.GT.pos) THEN
C the string T contains probably a time behind pos

		  time=T(pos+1:)
	       ELSE
C the string T contains no time
		  time=' '
	       ENDIF

	    ELSE
C the string contains only time, no date_ and no weekday name
	       time=T
C put the default date in the date string -4714dec30. This RETURNs
C Julian Day Nr=-1
C in the program the time will then be made valid for any day
               date='-4714dec30'
c            write(*,*)' took the default date'
	    ENDIF
C ENDIF pos GT 0
	ENDIF
C ENDIF FJDAY=0
c         write(*,*) 'rtimedat t',t,'date=',date
c         write(*,*) 'length(t)=',l,'pos=',pos

	IF (L.GT.pos) THEN
c         write(*,*) 'calling rtime'

C@tno jcp 1996Jun17_15:57:55 variab added
CC          CALL rtime(time,h,m,s,extradays,errkey)
          CALL rtime(time,h,m,s,extradays,errkey,variab)
c          write(*,*) 'h=',h,' m=',m,' s=',s,' errkey=',errkey
c          call ho('','')
C@empa aw 2000jan14 Make the whole process, also if errkey=1
CC          IF (ERRKEY.EQ.1) GOTO 900
	ENDIF
	IF (FJDAY.EQ.0 ) THEN
c         write(*,*) 'calling rdate'
C@tno jcp 1996Jun17_15:57:06 variab added
CC          CALL rdate(date,mm,id,iyyy,errkey)
CC          CALL rdate(date,mm,id,iyyy,errkey,variab)
c          write(*,*) 'mm=',mm,' id=',id,' a=',iyyy,' errkey=',errkey
c          call ho('na rdate','')
CC          IF (ERRKEY.EQ.1) GOTO 900
CC          CALL JULD(JDAY,MM,ID,IYYY,ERRKEY)
          errkey1=0
          CALL rdate(date,mm,id,iyyy,errkey1,variab)
          If (errkey1.ne.0) errkey=errkey1
          errkey1=0
          CALL JULD(JDAY,MM,ID,IYYY,errkey1)
          If (errkey1.ne.0) errkey=errkey1
C@empa aw 2000jan14 end bugfix

	ENDIF
C@tno jcp 1996May02_17:06:48 if extradays are found in time, then add and CALDAT
          if (extradays.ne.0) then
C@NBI PGS 1999Aug03 - Shouldn't be written out by default
CC            write(cof,*) 'extradays=',extradays
            IF(Pecho.GE.2) write(cof,*) 'extradays=',extradays
            Jday=Jday+Extradays
C@tno jcp 1996Jul01_12:05:45 Y M D in Caldat!!
CC            CALL CALDAT(JDAY,MM,ID,IYYY)
            CALL CALDAT(JDAY,IYYY,MM,ID)
          end if

C@empa
900	CONTINUE
	RETURN
	END


C@tno jcp 1996May28_11:19:42 JulDay renamed to JDay through the whole program
Ch***********************************************************************
        SUBROUTINE JULD(JDay,MM,ID,IYYY,ERRKEY)
C***********************************************************************

C JDay = Julian Day
C Description:
C this routine is taken from "Numerical Recipies", Chapt 1, Cambridge press.
C In this function JULD RETURNs the Julian Day Number, which begins at noon of
C the calendar date specified by month mm,day id, and year Iyyy, all INTEGER
C Variables. Positive year signifies A.D.; negative, B.C. Remember that the
C year after 1 B.C. was 1 A.D.
C The Gregorgian calendar was adopted on Oct. 15, 1582
C
C Input :  MM	 Int	  Month
C	   ID	 Int	  Day of month
C	   IYYY	 int	  Year
C
C testpoint:May 23 1968 is JDay 2440000 thursday
C Julian day 1 = -4713jan01 tuesday
C hcp aug 3 1989
C overview of tricks to handle time, weekday, monthday
C                           Comis uses
C        input              julian day         year           valid for
C      time (no date)          -1              -4714          every day
C    MON..SUN(_time)          0..6         -4714..-4713       every week
C    MonthDay(_time)    1721424..1721788        1             every year
C    YYYYMMDD_hh:mm:ss         ..                        exactly that date_time

C If only time is input Comis uses -4714dec30 which is Julian day -1

C If a week day is used Comis uses -4714dec31 for monday JDay=0
C -4713jan01 tuesday=1 etc ..-4713jan06  sunday=6

C month and days
C set the year to 1 . THEN the Julian day number will be 1721424...1721788 and
C in COMIS we will only interpret the month and day then (regardless of the
C year) . Year 1 is not a leap-year s feb29 does not exist.
C Modifications:
C@empa	 VD/22.Feb.91
C@empa			- Pass parameters are not changed in the routine
C@empa			- Introduction of auxiliary parameter IYYY1
C@empa			- Improved Error handling
C@lbl bs 1991may31    correction for year 1900/1901
C
C ERROR return if:  Year number zero
Ch***********************************************************************

        IMPLICIT NONE
        INTEGER IGREG
	PARAMETER (IGREG=15+31*(10+12*1582))
        INTEGER JDay,MM,ID,IYYY,ERRKEY
C@tno jcp 1996Jul02_13:50:36 IYYY1 renamed to IYYYa in routine JulD
CC        INTEGER JY,JM,JA,IYYY1
        INTEGER JY,JM,JA,IYYYa
	IF (IYYY.EQ.0) THEN
	 CALL ERROR ('There is no Year Zero',2)
	 ERRKEY=1
	ENDIF
        IYYYa=IYYY
        IF (IYYY.LT.0) IYYYa=IYYYa+1
	IF (MM.GT.2) THEN
        JY=IYYYa
	JM=MM+1
	ELSE
        JY=IYYYa-1
	JM=MM+13
	ENDIF
        JDay=INT(365.25*JY)+INT(30.6001*JM)+ID+1720995
	IF (ID+31*(MM+12*IYYY).GE.IGREG) THEN
C@empa aw 2000nov09 Add a small amount before INT to prevent the error described below
C                   
CC	JA=INT(0.01*JY)
	JA=INT(0.01*JY+0.0001)
        JDay=JDay+2-JA+INT(0.25*JA)
	ENDIF

C error correction: between march 01 1900 and feb 28 1901 the
C Julian day was always one too high

C@empa aw 2000nov09 Same error happens every 100 years except the years 
C  divisible by 400 (eg. year 2000) 
C  It's a rounding error: INT(0.01*100) delivers 0 instead of 1                
C@tno jcp 1996Apr29_20:49:24 Not sure, was this the mistake in the LOTUS time?
cc      IF (JDay.LE.2415445 .AND. JDay.GE.2415081) THEN
cc         IF (.NOT.(JDay.EQ.2415445 .AND. JM.EQ.4)) JDay=JDay-1
cc	ENDIF

	RETURN
	END


Ch***********************************************************************
        SUBROUTINE DAYNR(JDay,WEEKDAY)
C***********************************************************************
C DAYNR= DAY Number
C DAYNR converts the JULIAN Day Number into the day of the week.
C@empa 1=Monday, 2=Tuesday 3=Wednesday 4=Thursday 5=Friday 6=Saturday 7=Sunday
C 1968 may 23 is julian day 2440000 and a thursday
C hcp aug 3 1989
C Modifications:
C@empa	VD 19.mar.91  weekday 1..7
Ch***********************************************************************

        IMPLICIT NONE
        INTEGER JDay, WEEKDAY
        character*40 str
        INTEGER Lstr

C@tno jcp 1996Apr29_20:50:23 should be JDay+1
cc      WEEKDAY=MOD(JDay,7)
        WEEKDAY=MOD(JDay+1,7)

        if (weekday.lt.0) then
          call intdis(JDay,str,lstr)
        call error('subroutine DayNr is called with '//str(1:lstr)//
     & ' which results in a negative week day number. JDay must be'//
     & ' positive.',2)
C Bluntly reset to sunday
          weekday=7
        end if
	IF (WEEKDAY.EQ.0) WEEKDAY=7

	RETURN
	END


Ch***********************************************************************
	SUBROUTINE monthNr(in,out,errkey)
C***********************************************************************
C monthNr=MONTH Number (=1,..12)
C monthNr replaces the name of the month in the string in with its number
C only for the first occurance of a month name or part of it
C take care if the in string contains only a character "r" it will be found
C in the month january and there fore replaced as month 1
C hcp aug 3 1989
C Modifications:
C@empa	VD 22.Feb.91  Names
C@empa	vd 19.mar.91
C		Error if nr>0 but no match
C
C ERROR return if:  Not a valid month number or string (could also be
C                   a jarbled weekday)
Ch***********************************************************************

        IMPLICIT NONE
	CHARACTER*(*) in, out
        INTEGER L,begina,EndA,found,ERRKEY

	CHARACTER dum*160,word*160, nrstr*2, monstr*120
        INTEGER LENw,Nr
        INTEGER LENSTR

C to translate in another language change here. Check names of the days
C they must not be a part in the names of the months
	monstr(1:)='january'
	monstr(11:)='february'
	monstr(21:)='march'
	monstr(31:)='april'
	monstr(41:)='may'
	monstr(51:)='june'
	monstr(61:)='july'
	monstr(71:)='august'
	monstr(81:)='september'
	monstr(91:)='october'
	monstr(101:)='november'
	monstr(111:)='december'

	out=in
	CALL lowerc(out)

	L=LENSTR(out)
C call FAlpha to find the first substring that consists of characters a..z only
	CALL FAlpha(out,L,begina,EndA,found)
c        write(*,*) 'Falpha out',out,'l',l,'begina',begina,'enda',
c     &   enda,'found',found
	IF (FOUND.EQ.1) THEN

	  word=out(begina:EndA)
	  lenw=EndA-begina+1
	  nr=index(monstr,word(1:lenw))

	  IF (nr .gt. 0) THEN
	    nr=(nr-1)/10+1
            WRITE(nrstr,2000) nr
C month nr should have the format 01  or 02 or 11...
2000    FORMAT(I2.2)

C now construct the new out string of the first and last part(IF existing)
C and the monthnumber in the middle
	    IF (L.GT.EndA) THEN
	      dum=out(EndA+1:)
	      if (lenstr(dum) .lt. 2) then
		dum(2:2)=dum(1:1)
		dum(1:1)='0'
	      endif
	    ENDIF
	    out(begina:)=nrstr(1:2)
	    IF (L.GT.EndA) THEN
	      out(begina+2:)=dum
	    ENDIF

	  ELSE
C@empaVD if found>0 but nr=0
	   CALL ERROR ('Not a valid weekday or month string',2)
	   ERRKEY=1
	  ENDIF

	ENDIF
C ENDIF found>0
c        write(*,*) 'out=',out
	RETURN
	END

C@tno jcp 1996May28_11:26:30 renamed Dates to Dstring
Ch***********************************************************************
C@tno jcp 1996Jun17_15:54:38 variab added
CC        SUBROUTINE rdate(Dstring,mm,id,iyyy,errkey)
        SUBROUTINE rdate(Dstring,mm,id,iyyy,errkey,variab)
C***********************************************************************
C rdate=READ Date
C rdate gets the years, months and days out of the input Dstring string
C possible formats of Dstring:
C 1 (day 1) 11 (day 11) 211(feb 11) 1214(december 14) 681214 (1968 dec 14)
C 19681214 (1968 dec 14)
C no spaces are allowed between the numbers
C hcp aug 3 1989
C Modifications:
C@empa vd 12.Mar.91 Read(t,*) replaced by getwi routines
C@empa	    (with defaults arbitrarly =1)
C@empa vd 19.3.91  errkey
C@empa	  27.3.91
C
C  Pass parameters:
C  IO   Name    Units Description
C  I    Dstring -     Time/date string
C   O   iyyy    -     Year
C   O   mm      -     Month
C   O   id      -     Day of month
C   O   Errkey  -     =1 if error occurred during processing input string T, else =0
C   -   variab  -     There is no intention to input/output variab here,
C                     but "variab" has necessarily been added as an argument 
C                     just to get its address, so that we don't need to 
C                     INCLUDE CONV-INP.INC for calls to routines GETWI or GETWR.
C
C ERROR return if:  Not a valid month number or string (could also be
C                   a jarbled weekday)
Ch***********************************************************************
        IMPLICIT NONE
        include 'comv-uni.inc'
        INTEGER mm,id,iyyy,errkey
        INTEGER L,L1,K
C@tno jcp 1996Jun17_15:54:18 variab added
CC        CHARACTER*(*) Dstring
        CHARACTER*(*) Dstring,variab
        CHARACTER t*160 , dum*160
        INTEGER LENSTR

        dum=' '
C@tno jcp 1996May02_17:25:55 default dd mm iyyy to 1
        mm=1
        Iyyy=1
        id=1

        t=Dstring
        CALL monthnr(t,t,errkey)
c        write(*,*) 'rdate replaced month in t',t

	IF (ERRKEY.EQ.1) GOTO 900

C		    d dd ;mdd mmdd;yymmdd   yyyymmdd
C select strings of 1 to2; 3 to 4 ;  6	and   8 characters

	l=lenstr(t)
	K=0
c        write(cof,*) 'getting the date from ',t
	IF(l .LE. 2) THEN
C only days
          IF (l.GT.0) THEN
            write(cof,*) 'getting a day from ',t
C@tno jcp 1996Jun14_00:20:38 variab for GetWI
             variab='the day number from the time string'
             CALL GETWI(T,K,L,ID,1)
c            write(cof,*) 'the day is=',ID
	  ELSE
C put the date for Julian Day Nr=0 . This will indicate to take the time on
C any day for simulation in COMIS
	     IYYY=-4713
	     MM=1
	     ID=0
	  ENDIF

	ELSE IF (l .LE.4) THEN
C month and days
C set the year to 1 . THEN the Julian day number will be 1721424...1721788 and
C in COMIS we will only interpret the month and day THEN (regardless of the
C year) . Year 1 is not a leap-year s feb29 does not exist.
	  IYYY=1
	  dum(1:2)=t(1:l-2)
	  dum(4:5)=t(l-1:l)
	  K=0
	  L1=LENSTR(DUM)
C@tno jcp 1996Jun14_00:20:38 variab for GetWI
          variab='the Month number from time'
	  CALL GETWI(DUM,K,L1,MM,1)
C@tno jcp 1996Jun14_00:20:38 variab for GetWI
          variab='the Day number from time'
	  CALL GETWI(DUM,K,L1,ID,1)

	ELSE IF (l .LE.6) THEN
C year (2 digit) month and day
C a 2 digit year nn will be interpreted as 19nn.
C IF you want to play around with negative years, THEN input as -0001, -nnnn
	  dum(1:2)=t(1:l-4)
	  dum(4:5)=t(l-3:l-2)
	  dum(7:8)=t(l-1:l)
	  K=0
	  L1=LENSTR(DUM)
C@tno jcp 1996Jun14_00:20:38 variab for GetWI
          variab='the Year number from time'
	  CALL GETWI(DUM,K,L1,IYYY,1)
C@tno jcp 1996Jun14_00:20:38 variab for GetWI
          variab='the Month number from time'
	  CALL GETWI(DUM,K,L1,MM,1)
C@tno jcp 1996Jun14_00:20:38 variab for GetWI
          variab='the Day number from time'
	  CALL GETWI(DUM,K,L1,ID,1)
C IF somebody is willing to change this into 2000 on jan 1 2000 ,I'll be happy
C@empa aw 2000apr06 Now it's time to do that
CC	  IYYY=IYYY+1900
	  IYYY=IYYY+2000

	ELSE
C year (4..5 digit) month and day
C in the string we output 5 digits for the year to allow playing with negative
C years up to -9999
	  dum(1:5)=t(1:l-4)
	  dum(7:8)=t(l-3:l-2)
	  dum(10:11)=t(l-1:l)
	  K=0
	  L1=LENSTR(DUM)
C@tno jcp 1996Jun14_00:20:38 variab for GetWI
          variab='the Year number from time'
	  CALL GETWI(DUM,K,L1,IYYY,1)
C@tno jcp 1996Jun14_00:20:38 variab for GetWI
          variab='the Month number from time'
	  CALL GETWI(DUM,K,L1,MM,1)
C@tno jcp 1996Jun14_00:20:38 variab for GetWI
          variab='the Day number from time'
          CALL GETWI(DUM,K,L1,ID,1)
c        write(*,*) 'rdate getwi Iyyy,mm,id',iyyy,mm,id
c        call ho('','')
	ENDIF
C ENDIF select	L length of t string
C ENDIF length t string > 0
900	CONTINUE
	RETURN
	END

C@tno jcp 1996May28_11:29:08 in rTime renamed 'TP' into Tstring
Ch***********************************************************************
C@tno jcp 1996Jun17_15:55:15 variab added
CC        SUBROUTINE rtime(Tstring,h,m,s,extradays,errkey)
        SUBROUTINE rtime(Tstring,h,m,s,extradays,errkey,variab)
C***********************************************************************
C rtime= READ Time
C rtime gets the h hours m minutes and s seconds out of the input t Tstringtring
C possible formats of Tstring:
C 90830 (9h 8m 30s) 9 (9h) 908 (9h 8m)
C 9:08:30 (9h 8m 30s)  9:08 (9h 8m)
C no spaces are allowed between the numbers
C IF the string contains impossible hours or minutes,seconds they are just
C made in 0..24 and 0...60, with error warning
C hcp aug 3 1989
C Modifications:
C@empa 12.3.91 VD Read(t,*) replaced by getwi routines
C@empa 19.3.91 vd error handling
C
C  Pass parameters:
C  IO   Name      Units Description
C  I    Tstring   -     Time/date string
C   O   h         -     Hour
C   O   m         -     Minute
C   O   s         -     Seconds
C   O   Extradays -     =1 if 24:mm, because time is set to 00:mm and day is
C                       increased in calling routine
C   O   Errkey    -     =1 if error occurred during processing input string T, else =0
C   -   variab    -     There is no intention to input/output variab here,
C                       but "variab" has necessarily been added as an argument 
C                       just to get its address, so that we don't need to 
C                       INCLUDE CONV-INP.INC for calls to routines GETWI or GETWR.
C
C ERROR return if: "Tstring" is empty string; Minutes or seconds >= 60
Ch***********************************************************************

        IMPLICIT NONE
        include 'comv-uni.inc'
        INTEGER h,m,s,extradays,errkey
        INTEGER pos,pos2,l,l1,k
C@tno jcp 1996Jun17_16:08:53 variab added
CC        CHARACTER*(*) Tstring
        CHARACTER*(*) Tstring,variab
	CHARACTER t*160 , dum*160
        INTEGER LENSTR
	dum=' '
	  h=0
	  m=0
          s=0
          extradays=0
        t=Tstring
	l=lenstr(t)
	IF (l.GT.0) THEN
C first look for strings with colons in it like '8:14' or '8:14:30'
	  pos=index(t,':')
	  dum=t
	  IF (pos .gt.0 ) THEN
	    pos2=index(t(pos+1:),':')
	    IF (pos2 .gt. 0) THEN
C the string contains two ':''s
	      pos2=pos2+pos
	      t(pos:)=dum(pos+1:pos2-1)
	      t(pos2-1:)=dum(pos2+1:)
	    ELSE
C the string contains one ':'
	      t(pos:)=dum(pos+1:)
	    ENDIF
C the ':' are moved out of the string t
	    L=lenstr(t)
	  ENDIF

	  dum=' '

C select strings of 1 to2; 3 to 4 and 5 to 6 characters

	  IF(l .LE. 2) THEN
C only hours
	      K=0
C@tno jcp 1996Jun14_00:20:38 variab for GetWI
              variab='get Hour from time'
              CALL GETWI(T,K,L,H,1)
C@NBI PGS 1999Aug03 - This extraday check should be done for all time formats,
C@NBI                 not only when time string is 2 characters long.
C@NBI                 For example, "24:00" or "24:00:00" are not uncommon,
C@NBI                 therefore this code bit has been moved further down.
CC              if (h.gt.23) then
CC                extradays=h/24
CC                h=h-24*extradays
CC                write(cof,*)'extradays=',extradays
CC              end if

	  ELSE IF (l .LE.4) THEN
C hours and minutes
	     dum(1:2)=t(1:l-2)
	     dum(4:5)=t(l-1:l)
             K=0
c             write(*,*) 't  =',t
c             write(*,*) 'dum=',dum
c             call ho(' ',' ')
	     L1=LENSTR(DUM)
C@tno jcp 1996Jun14_00:20:38 variab for GetWI
              variab='get Hour from time'
	     CALL GETWI(DUM,K,L1,H,1)
C@tno jcp 1996Jun14_00:20:38 variab for GetWI
              variab='get Minutes from time'
             CALL GETWI(DUM,K,L1,M,1)

	  ELSE IF (l .LE.6) THEN
C hours minutes and seconds
	    dum(1:2)=t(1:l-4)
	    dum(4:5)=t(l-3:l-2)
	    dum(7:8)=t(l-1:l)
	    K=0
	    L1=LENSTR(DUM)
C@tno jcp 1996Jun14_00:20:38 variab for GetWI
              variab='get Hour from time'
	    CALL GETWI(DUM,K,L1,H,1)
C@tno jcp 1996Jun14_00:20:38 variab for GetWI
              variab='get Minute from time'
	    CALL GETWI(DUM,K,L1,M,1)
C@tno jcp 1996Jun14_00:20:38 variab for GetWI
              variab='get Seconds from time'
	    CALL GETWI(DUM,K,L1,S,1)
	  ENDIF
C ENDIF select	L length of t string

	ELSE
C ELSE IF length t string > 0
          CALL ERROR ('The Tstring is an empty string',1)
	  ERRKEY=1
	ENDIF
C ENDIF length t string > 0
C@NBI PGS 1999Aug03 - Code for extraday moved from above. (see comment above)
C@NBI               - The line "h=h-24*extradays" is superfluous, due to
C@NBI                 line "h=MOD(h,24)" below, so commented out
C@NBI               - The line "write(cof,*)'extradays..." is also superfluous
              if (h.gt.23) then
                extradays=h/24
CC              h=h-24*extradays
CC              write(cof,*)'extradays=',extradays
              end if
C@NBI PGS 1999Aug03 - No longer need to error check for hour >= 24
C@NBI               - "min/sec >60" should be "min or sec >= 60"
CC	IF (H.GE.24.OR.M.GE.60.OR.S.GE.60) THEN
	IF (M.GE.60.OR.S.GE.60) THEN
	    ERRKEY = 1
CC	    CALL ERROR('hour>24 or min/sec >60',1)
CC	    CALL ERROR('minutes or seconds >= 60',1)
	    CALL inERR('minutes or seconds >= 60','',.false.,2)
	ENDIF
	h=MOD(h,24)
	m=MOD(m,60)
	s=MOD(s,60)
	RETURN
	END



C@tno jcp 1996May28_11:29:08 in DayQ renamed 'TP' into DTstring
C@tno jcp 1996May28_11:29:08 in DayQ renamed 'TIME' into Tstring
Ch***********************************************************************
        SUBROUTINE DAYQ(DTstring,L,DAYNR,JDAY,Tstring,FJDAY,pos,errkey)
C***********************************************************************
C@tno jcp 1996May28_11:34:50 explanation adjusted
C DAYQ=DAY QUESTION MARK
C DAYQ looks in the string DTstring for a name of a weekday mon tue wed thu
C fri sat sun
C no characters a...z are allowed to preceed a weekday name
C@tno jcp 1996Apr29_17:26:29 1..7 = 0..6 again
C If found the DAYNR is assigned and JDAY is set to the Julian Days 0..6
C which is in -4714dec31 -4713jan06
C Flag FJDAY is set to 1 to indicate that the JDAY is set
C IF there is a time after the weekday name it is copied into Tstring and pos
C is set to the beginning of that Tstring string in the Tstring, because in
C rtimdat there is an if-test on that
C
C sept 1989 hcp
C modifications:
C@empa vd 27.mar.91  errkey, return at the end not in if loop
C ERROR return if:  (doesn't find errors)
Ch***********************************************************************


        IMPLICIT NONE
C pass parameters
        CHARACTER*(*) DTstring,Tstring
        INTEGER L,DAYNR,JDAY,FJDAY,POS
        INTEGER ERRKEY

C local parameters
	character*160 T
        INTEGER begina,EndA,found

	ERRKEY=0


C put characters to lower case in T (sure this is done in MONTHNR too but
C for the sake of modularity eh.)

        t = DTstring
	CALL lowerc(t)

	CALL FAlpha(T,L,begina,EndA,found)

        IF (FOUND.EQ.1) THEN

C An a..z part has been found ,now look IF it is a weekday
C EndA should be at least begina+1 (two characters but may be more
C we test only on the first 3 charachters)

C@tno jcp 1996Jun13_15:44:26 try to include WDY WND Weekday (Jday=7) and
C Weekend (Jday=8)
CC        DAYNR=INDEX('montuewedthufrisatsun',
          DAYNR=INDEX('montuewedthufrisatsunwdywnd',
     &	  T(begina: min(EndA,begina+2))	      )
	  IF(DAYNR.GT.0) DAYNR=(DAYNR-1)/3+1

C@tno jcp 1996Jun13_15:45:44 Jday range for weekdays extended with 7 WDY, 8 WND
CC          IF (DAYNR.GE.1 .AND. DAYNR.LE.7) THEN
          IF (DAYNR.GE.1 .AND. DAYNR.LE.9) THEN
C we found a weekday name. In our definition we will consider the low Jday
C numbers as weekdays only regardles of the calendar date in the timesimulation
C@tno jcp 1996Apr29_20:56:10 Daynr-1
CC	    JDAY=DAYNR
	    JDAY=DAYNR-1

C@tno jcp 1996Jun13_15:46:41 Jday 0..6 is now 0..8
CC Flag that JDAY has been set to 0....6
C Flag that JDAY has been set to 0....8
	    FJDAY=1

	    IF (L.GT.EndA) THEN
C the string contains still characters (non a...z) possibly a time
	      POS=INDEX(T,'_')

	      IF (POS.GT.EndA .AND. L.GT.POS) THEN
C the underscore _ is found after the weekday name and the string contains
C characters after that underscore _
                Tstring=T(POS+1:)

	      ELSE
C there is no underscore but there are characters
                Tstring=T(EndA+1:)

	      ENDIF
C ENDIF IF POS>EndA and L>POS

	    ELSE
C there is nothing after this dayname in the string, set pos at L to signal to
C the if-statement in rdate that there is no time
	      POS=L
	    ENDIF
C ENDIF IF L>EndA

	  ENDIF
C ENDIF IF 1<=weekday<=7

	ELSE
C no characters a...z found
	ENDIF

	RETURN

	END
