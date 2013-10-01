C+*********************************************************** comv-tm3.f
Ch**********************************************************************

        SUBROUTINE EchoSCH3(TLotus,SchNam,V1,D1,T1,X1,P1 )
C***********************************************************************
C  EchoSch echos changes made by the meteo schedule
C  format: TLotus ScheduleName Vmet, dmet, tmet, xhmet, pbmet
C@tno jcp 1996Apr15_12:23:29
C
C  TLotus     = time
C  SchNam   = Schedule name
Ch**********************************************************************
        IMPLICIT NONE
        INCLUDE 'comv-uni.inc'
	INCLUDE 'comv-inp.inc'
        DOUBLE PRECISION TLotus
        REAL V1,D1,T1,X1,P1
        CHARACTER*(*) SchNam
        INTEGER LenStr
C local
        INTEGER lstr
        INTEGER Jday1,NSecDay,numcreate
        Character*31 DTstring
        Character*40 SchNam2

        Schnam2=SchNam
        Call Long10(SchNam2,Lstr)

        CALL CreateJdTim(numcreate(TLotus),Jday1,NSecDay)
c        write(*,*) 'TLotus=',TLotus
c        write(*,*) 'jDay',jDay,' NSecDay=',NSecDay
        CALL CONVDAT1(Jday1,NSecDay,DTstring)
c        write(*,*) 'DTstring=',DTstring
c        call ho('main timeloop',' ')
cc        Call ConvDate(TLotus,DTstring)
C@NBI PGS 1999Aug09 - Added clarification
        write(cof,*) 'SCH3: ',DTstring(1:lenstr(DTstring)),' ',
CC   &   SchNam2(1:lstr),' ',V1,D1,T1,X1,P1
     &   SchNam2(1:lstr),' Vw=',V1,' Dw=',D1,' T=',T1,' Xh=',X1,' P=',P1
        return
        end


Ch**********************************************************************
        SUBROUTINE EchoSCH4(TLotus,SchNam,iZone,Actfactor,NumOcc)
C***********************************************************************
C  EchoSch echos changes made by a schedule
C same as EchoSCH but now with an affected roomnumber or link number and
C number of occupants
C@tno jcp 1996Apr15_12:23:29
C
C  TLotus  = time
C  SchNam= Schedule name
C  value = value of the changed parameter
Ch**********************************************************************
        IMPLICIT NONE
        INCLUDE 'comv-uni.inc'
	INCLUDE 'comv-inp.inc'
        DOUBLE PRECISION TLotus
        REAL ActFactor
        INTEGER iZone,NumOcc
        CHARACTER*(*) SchNam
        INTEGER LenStr
C local
        INTEGER lstr, lstr2
        INTEGER Jday1,NSecDay,numcreate
        Character*31 DTstring
        Character*40 str,SchNam2

c        write(*,*) 'schnam',schnam
        Schnam2=SchNam
        Call Long10(SchNam2,Lstr)
c        write(*,*) 'schnam2',schnam2
c        write(*,*) 'lstr',lstr
c        call ho(' ',' ')

        call intdis(iZone,str,lstr2)
        CALL CreateJdTim(numcreate(TLotus),Jday1,NSecDay)
c        write(*,*) 'TLotus=',TLotus
c        write(*,*) 'jDay',jDay,' NSecDay=',NSecDay
        CALL CONVDAT1(Jday1,NSecDay,DTstring)
c        write(*,*) 'DTstring=',DTstring
c        call ho('main timeloop',' ')
cc        Call ConvDate(TLotus,DTstring)
C@NBI PGS 1999Aug09 - Added clarification
        write(cof,*) 'SCH4: ',DTstring(1:lenstr(DTstring)),' ',
     &   SchNam2(1:lstr),' zone='//str(1:lstr2)//
CC     &   ' ',ActFactor,' ',NumOcc
     &   ' Act=',ActFactor,' #Occ=',NumOcc
        return
        end


C@NBI PGS 2000Jul20 - This subroutine wasn't being used, so commented out
CCCh**********************************************************************
CC        SUBROUTINE EchoSCH5(TLotus,SchNam,iext,Actfactor,value)
CCC***********************************************************************
CCC  EchoSch5 echos changes made by a schedule (ext conc)
CCC same as EchoSCH2 but now with an affected external node number
CCC and the final value at the end
CCC@tno jcp 1996Apr15_12:23:29
CCC@NBI PGS 1999Aug06 - This subroutine 'EchoSCH5' is not used. Delete it?
CCC
CCC  TLotus= time
CCC  SchNam= Schedule name
CCC  actfactor = factor in the schedule
CCC  value = value of the changed parameter
CCCh**********************************************************************
CC        IMPLICIT NONE
CC        INCLUDE 'comv-uni.inc'
CC	INCLUDE 'comv-inp.inc'
CC        DOUBLE PRECISION TLotus
CC        REAL ActFactor,value
CC        INTEGER iext
CC        INTEGER Jday1,NSecDay,numcreate
CC        CHARACTER*(*) SchNam
CC        INTEGER LenStr
CCC local
CC        INTEGER lstr, lstr2
CC        Character*31 DTstring
CC        Character*40 str,SchNam2
CC
CC        Schnam2=SchNam
CC        Call Long10(SchNam2,Lstr)
CC
CC        call intdis(iext,str,lstr2)
CC        CALL CreateJdTim(numcreate(TLotus),Jday1,NSecDay)
CCc        write(*,*) 'time=',TLotus
CCc        write(*,*) 'jDay',jDay,' NSecDay=',NSecDay
CC        CALL CONVDAT1(Jday1,NSecDay,DTstring)
CCc        write(*,*) 'DTstring=',DTstring
CCc        call ho('main timeloop',' ')
CCcc        Call ConvDate(TLotus,DTstring)
CC        write(cof,*) 'SCH5: ',DTstring(1:lenstr(DTstring)),' ',
CC     &   SchNam2(1:lstr),' Fe='//str(1:lstr2)//
CC     &   ' ',ActFactor,value
CC        return
CC        end


Ch**********************************************************************
        SUBROUTINE EchoSCH6(TLotus,SchNam,ipol,iext,Actfactor,value)
C***********************************************************************
C  EchoSch6 echos changes made by a schedule (ext conc)
C same as EchoSCH2 but now with an affected pollutant number and external node
C number and the final value at the end
C@tno jcp 1996Apr15_12:23:29
C
C  TLotus= time
C  SchNam= Schedule name
C  actfactor = factor in the schedule
C  value = value of the changed parameter
Ch**********************************************************************
        IMPLICIT NONE
        INCLUDE 'comv-uni.inc'
	INCLUDE 'comv-inp.inc'
        DOUBLE PRECISION TLotus
        REAL ActFactor,value
        INTEGER ipol,iext
        INTEGER Jday1,NSecDay,numcreate
        CHARACTER*(*) SchNam
        INTEGER LenStr
C local
        INTEGER lstr, lstr2,lstr3
        Character*31 DTstring
        Character*40 str,str3,SchNam2

        Schnam2=SchNam
        Call Long10(SchNam2,Lstr)

        call intdis(ipol,str3,lstr3)
        call intdis(iext,str,lstr2)
        CALL CreateJdTim(numcreate(TLotus),Jday1,NSecDay)
c        write(*,*) 'time=',TLotus
c        write(*,*) 'jDay',jDay1,' NSecDay=',NSecDay
        CALL CONVDAT1(Jday1,NSecDay,DTstring)
c        write(*,*) 'DTstring=',DTstring
c        call ho('main timeloop',' ')
cc        Call ConvDate(TLotus,DTstring)
C@NBI PGS 1999Aug09 - Added clarification
        write(cof,*) 'SCH6: ',DTstring(1:lenstr(DTstring)),' ',
     &   SchNam2(1:lstr),' pol='//str3(1:lstr3),' Fe='//str(1:lstr2)//
CC   &   ' ',ActFactor,value
     &   'AmbC=',ActFactor,' FacC=',value
        return
        end

Ch**********************************************************************

	SUBROUTINE FILLBUF(bufvm,bufdm,buftm,bufxh,bufpb)
C***********************************************************************
C
C lbl bs 01april, 1991
C
C Purpose:
C Fills the buffer containing the data of the next weather file line.
Ch**********************************************************************
        IMPLICIT NONE
	INCLUDE 'comv-uni.inc'
	INCLUDE 'comv-inp.inc'

        REAL bufvm,buftm,bufdm,bufxh,bufpb
	character*160 metbuf
        INTEGER bjday,by,bmm,bid,bh,bm,bs,bsec
        INTEGER k,d,errkey,JulX,endflag
        INTEGER LenStr
        INTEGER JdaySecSim
C@tno jcp 1996Jul03_12:32:34 spare value for compday to see if time goes back
        INTEGER CompDay2
        Compday2=compday

C ------------------------------------------
C fill buffer variables with next line data
C ------------------------------------------
	IF (metfile.EQ.' ') RETURN
C option to read a DOE-2 weather file
02	k=0
	IF (metopt .EQ. 'DOE2') THEN
	   CALL DOE2READ(metbuf,endflag)
	   IF (endflag.EQ.1) GOTO 900
	ELSE
C@tno jcp 1996Jul02_14:20:00 new label if at end of the meteo file
CC01         READ(IFS,'(A)',END=900) metbuf
01         READ(IFS,'(A)',END=1900) metbuf
C@tno jcp 1996Jul02_14:26:20 flag here if the meteo file did contain data
c           write(*,*) metbuf(1:lenstr(metbuf))
c           call ho('FillBuf',' ')
           IF (metbuf.EQ.' ') then
             GOTO 01
           else
C data has been found. If at EOF we could reopen this file (if cyclic i.e
C monthly data no year)
             EmptyMeteo=0
           end if
	   IF (notime.EQ.0) THEN
C@tno jcp 1996Jun13_23:48:56 added variab= before getWS
c@NBI PGS 1999Jun29 - Spelling error
          variab='the time for the meteo file in FillBuf'
CC        variab='the time for the meteo file in FilBuf'
	      CALL GETWS(metbuf,k,80,buftim,'jan01_')
	   ENDIF
	ENDIF

	call KeepLine(Metbuf)

        CALL GETWR(metbuf,k,80,bufvm,DVmet,.TRUE.)
        CALL GETWR(metbuf,k,80,bufdm,DDmet,.TRUE.)
        CALL GETWR(metbuf,k,80,buftm,DTmet,.TRUE.)
        CALL GETWR(metbuf,k,80,bufxh,DXhmet,.TRUE.)
        CALL GETWR(metbuf,k,80,bufpb,DPbMet,.TRUE.)
C@NBI PGS 1999Aug09 - Following code block is moved to new subroutine
C@NBI                 'ProcessWeather' to cut down on duplication.
C@NBI                 'ProcessWeather' is called from subroutine 'XMETDAT'
CC	Call CheckPb(bufpb*1000,Zmet,' Meteo Barometric')
CC
CCC check limits on VMet, Dmet, XhMet and PbMet
CC	if (bufvm .lt. 0.0) goto 995
CC	if (bufdm .lt. 0.0 .or. Dmet .gt. 360.0) goto 996
CC	if (bufxh .lt. 0.0) goto 997
CC	if (bufpb .lt. 0.0) goto 998
CC
CCC convert user wind velocity to m/s
CC	bufvm = bufvm * ifact(UnitW)
CC
CCC  Convert humidity from user units to kg/kg
CC	bufxh=bufxh * ifact(UnitXh)
CC
CCC convert user barometric pressure to Pa
CCC multiply by 1000.0 to change kPa to Pa
CCC The barometric pressure values are NOT converted by the user pressure units
CC	bufpb=bufpb*1000.0
C@NBI PGS 1999Aug09   (end of moved code)

C ------------------------------
C calculate next compday value
C ------------------------------
	IF (notime.EQ.0) THEN
C@tno jcp 1996Jun17_16:15:32 added variab
C@tno jcp 1996Jul02_14:58:35 FILLBUF part fails to run cyclic meteo files
C next part changed
          CALL RTIMDAT(buftim,bjday,by,bmm,bid,bh,bm,bs,errkey,variab)
          lastBjDay=Bjday
c          write(cof,*)'Bjday=',bjday,' cyclicmeteo=',cyclicmeteo
          if ((Bjday.gt.9 .and. Bjday.lt.1721424) .or.
     &        bjday.gt.1721788) then
C this is absolute data_time
            if (CyclicMeteo.eq.1) then
              Call Error2(
     & 'The last line line of your meteo file is a cyclic date. '//
     & 'The reopened weather file also has non cyclic dates. Make '//
     & 'the date format in your file uniform, like: mmdd_hh '//
     & '(cyclic) or YYYYmmdd_hh (absolute). To avoid a possible '//
     & 'endless loop, Comis has to stop here. Line from file '//
     &   metfile(1:lenstr(metfile))//' in error:',metbuf,3)
            end if
          end if
c to be able to use incomplete date/time formats like 0521_10:00 for may 21 10:0
c we first call JdaySecSim  Jday,NsecDay -> SecSimCap
c and then      createTM     SecSimCap   -> TLotus
c JdaySecSim finds the current or next event of the defined date in the meteo fi

          CALL SECONDS(bsec,bh,bm,bs)

c            TLotusCompD=CreateTM(CompDay)
c            call TimeSTr(TlotusCompD,FilTime)
c            write(*,*)'Buftim=',Buftim
c            write(*,*)'Compday->',FilTime
c            write(*,*)'BjDay, bsec',BjDay,Bsec
c            write(*,*)'compday before =',compday,compday/24/3600
c            write(*,*)'into JdaySimSec=',bjday,bsec

            CompDay=JdaySecSim(bjday,bsec,compday)
c            write(*,*)'compday after=',compday,compday/24/3600
c            TLotusCompD=CreateTM(CompDay)
c            call TimeSTr(TlotusCompD,FilTime)
c            write(*,*)'Compday->',FilTime
c            call ho('in fillbuf',' ')
            if (compday.LT.Compday2) then
c           write(cof,*) 'compday,2,metbuf',compday,compday2,metbuf
C@NBI PGS 1999Jun29 - remove extra space
              call Error2('Your weather file tries to set the time '//
     &         'back (line and time ignored) in line:',
CC   &         ' back (line and time ignored) in line:',
     &          metbuf,2)
              compday=compday2
            end if

	ELSE
	   compday=compday+bufsec
        ENDIF

C --------------------------------------
C compday within valid time interval ?
C --------------------------------------
	d=JulX(JdStart,sec1,JdStop,sec2)
	IF (compday.LE.d) RETURN
C end of processing weather file
900     metfile=' '
	CLOSE(IFS)
	RETURN

C@tno jcp 1996Jul02_14:20:27 new label if at and of meteo file
1900    if(EmptyMeteo.ne.1) then
          CLOSE(IFS)
          if (LastBjday.ge.1721424 .and. Lastbjday.le.1721788) then
C these are PROBABLY cyclic dates, open the file again
            CYCLICmeteo=1
            OPEN(IFS,file=metfile, STATUS = 'OLD',ERR=1929)
            goto 02
          else
c blank the metfile, this avoids further reading
            metfile=' '
            return
          end if
        else
          RETURN
        end if

C@NBI PGS 1999Aug09 - Following code block is moved to new subroutine
C@NBI                 'ProcessWeather' to cut down on duplication
CCC errors for limits on VMet, Dmet, XhMet and PbMet
CC
CC995	Call Error(
CC     & 'Value out of range for Wind Speed from DOE-2 file',3)
CC996	Call Error(
CC     & 'Value out of range for Wind Direction from DOE-2 file',3)
CC997	Call Error(
CC     & 'Value out of range for Humidity Ratio from DOE-2 file',3)
CC998	Call Error(
CC     & 'Value out of range for Barometric Pressure from DOE-2 file',3)
C@NBI PGS 1999Aug09   (end of moved code)

1929    Call Error('Meteo file:'//metfile//' non existent',2)
	GOTO 02
	END

