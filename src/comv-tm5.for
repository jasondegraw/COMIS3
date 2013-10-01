C+*********************************************************** comv-tm5.f
Ch**********************************************************************

Ch***********************************************************************
	SUBROUTINE METINIT
C************************************************************************
C
C lbl bs 02april, 1991
C
C Purpose:
C Initialise the weather file. This means, read until the time is in
C the simulation interval . At the end of this subroutine the file-
C pointer is positioned one line before the first line in the interval.
C compday is the time value of the next line.
C
C Changes:
C@NBI PGS 2000dec23 - Debug variable IOCHECK no longer needed, so deleted
Ch************************************************************************
        IMPLICIT NONE
	INCLUDE 'comv-uni.inc'
	INCLUDE 'comv-inp.inc'

	character*160 metbuf
        INTEGER bjday,by,bmm,bid,bh,bm,bs,bsec
        INTEGER k,errkey,JulX,endflag

C@tno jcp 1996Jul03_09:13:07 added Prv values in MetINIT to have the meteo
C values at or before simulation start.
        PrvSchTyp=10
        PrvSchNa=0
        PrvSec=-IntMax
        prvLine=' '

c notime=1 in case of a DOE2 meteo file (buftime is set to jan01)
c               or a COMIS file with * meteostartdate_time meteotimestep
c               or a COMIS file with * meteostartdate_time
c the default meteotimestep=3600
	IF (notime.EQ.1) GOTO 02

01	READ(IFS,'(A)',END=95) metbuf
	call KeepLine(metbuf)
c        write(*,*) metbuf
c        call ho('MetInit',' ')

	IF (metbuf.EQ.' ') GOTO 01
	k=0
C@tno jcp 1996Jun13_23:48:56 added variab= before getWS
c@NBI PGS 1999Jun29 - Spelling error
        variab='the time for the meteo file in routine MetInit'
CC      variab='the time for the meteo file in FilBuf'
        CALL GETWS(metbuf,k,80,buftim,'jan01_')
C@tno jcp 1996Jun17_16:15:32 added variab
        CALL RTIMDAT(buftim,bjday,by,bmm,bid,bh,bm,bs,errkey,variab)
	IF (bjday.LT.1721424) GOTO 99
	IF (bjday.LE.1721788) CALL JULD(bjday,bmm,bid,IYYY1,errkey)
	CALL SECONDS(bsec,bh,bm,bs)
	compday=JulX(JdStart,sec1,bjday,bsec)

        if (compday.LT.0 .and. CompDay.GT.PrvSec) then
          call JustBefore(10,0,CompDay,metbuf)
        end if

	IF (compday.GE.0) GOTO 90

	GOTO 01

C@tno jcp 1996Jun17_16:15:32 added variab
02      CALL RTIMDAT(buftim,bjday,by,bmm,bid,bh,bm,bs,errkey,variab)
	IF (bjday.LT.1721424) GOTO 99
	IF (bjday.LE.1721788) CALL JULD(bjday,bmm,bid,IYYY1,errkey)
	CALL SECONDS(bsec,bh,bm,bs)
        compday=JulX(JdStart,sec1,bjday,bsec)
c here compday will be a negative number of seconds (meteo files start is
c before simulation start) i.i meteo starts jan01(same year as simulation start)
c simulation starts at mar01 compday will be about -2*30*24*3600

C option to read a DOE-2 weather file
        IF (metopt .EQ. 'DOE2') THEN
c loop from jan01 through all hours of DOE2 until you are at simulation start
c (while Doe2READ has a system to 'directly' iterate to IM2 ID IH...!!! why
c not use that?)
04	   IF (compday.LT.0) THEN
C@tno jcp 1996Jul03_09:35:49 line moved down
CC              compday=compday+bufsec
c you may increase Compday here, but the first line read from Doe2 is Jan01
C and belongs to Jan01_00:00  (IH=1)

	      CALL DOE2READ(metbuf,endflag)
C@tno jcp 1996Jul03_09:19:14 keep track of the meteo just before SimulationStart
              if (compday.LT.0 .and. CompDay.GT.PrvSec) then
                call JustBefore(10,0,CompDay,metbuf)
              end if
C@tno jcp 1996Jul03_09:36:15 line came from before Call Doe2Read
              compday=compday+bufsec

              IF (endflag.EQ.1) GOTO 95
	      GOTO 04
	   ENDIF
	   RETURN
	ELSE
c loop from meteostart through all meteotimesteps of COMIS meteo file until
c you are at simulation start. Yes have to read all lines here as the file is
c sequential, but you can calculate in advance how many line are to be read!!!
03	   READ(IFS,'(A)',END=95) metbuf
	   IF (metbuf.EQ.' ') GOTO 03
	   IF (compday.LT.0) THEN
C@tno jcp 1996Jul03_09:19:14 keep track of the meteo just before SimulationStart
              if (CompDay.GT.PrvSec) then
                call JustBefore(10,0,CompDay,metbuf)
              end if
              compday=compday+bufsec
	      GOTO 03
	   ENDIF
	ENDIF

90	continue

	call KeepLine(metbuf)

        BACKSPACE(IFS)
	RETURN
C there are no weather data in the simulation interval
95      CALL ERROR('WEATHER FILE: No data in the simulation interval',
     &  2)
96	metfile=' '
	CLOSE(IFS)
	RETURN
C@tno jcp 1996Jul03_09:05:04 added line and use Error2
99      CALL ERROR2('WEATHER FILE: Wrong time format !',' ',2)
	GOTO 96
	END

Ch**********************************************************************
C@NBI PGS 1999Aug09 - Introduced Tlotus so that PocessWeater can call EchoSch
	SUBROUTINE XMETDAT(bufvm,bufdm,buftm,bufxh,bufpb,Tlotus)
C***********************************************************************
C
C lbl bs 03april, 1991
C
C Purpose:
C Changes the meteo data. The data stored in the meteo buffer become
C actual data.
Ch**********************************************************************
        IMPLICIT NONE
	INCLUDE 'comv-inp.inc'
      REAL bufvm,bufdm,buftm,bufxh,bufpb
      DOUBLE PRECISION Tlotus
	Vmet=bufvm
	Dmet=bufdm
	Tmet=buftm
	XhMet=bufxh
	PbMet=bufpb
C@NBI PGS 1999Aug09 - New subroutine 'ProcessWeather'
      CALL ProcessWeather(Tlotus)
	RETURN
	END

Ch***********************************************************************
	SUBROUTINE XSCHED(oldsch,newsch,elem)
C************************************************************************
C
C lbl bs 10april, 1991
C
C Purpose:
C Schedule name exchanger for main schedules. Replaces the oldsch with
C newsch for all elements defined in elem. The affected elements can be
C air flow component short names or zone numbers or link numbers accord-
C ing to the schedule type specified.
C@NBI PGS 1999Aug09 - This routine is redundant; &-SCH-MAI isn't yet implemented
Ch***********************************************************************
        IMPLICIT NONE
	INCLUDE 'comv-inp.inc'

	CHARACTER oldsch*10,newsch*10,elem*30,dum*30
	CHARACTER typ, elnam*7, list*400
        INTEGER i,k,l,iz,j,eflag,INTCON,p
        INTEGER IcIndex

	typ=oldsch(1:1)
	CALL UPPERC(elem)
	IF (typ.EQ.'T' .OR. typ.EQ.'H') GOTO 100
	IF (typ.EQ.'F' .OR. typ.EQ.'W') GOTO 200

	GOTO 900

C --------------------------------------------------------------------
C affected elements are zone numbers
C --------------------------------------------------------------------
100	CONTINUE
	list=' '
	CALL RDLIST(1,elem,list)
	l=len(list)
	k=0
C change schedule names
C@tno jcp 1996Jun14_00:20:38 variab for GetWI
        variab='the affected zonenumber for a schedule'
110	CALL GETWI(list,k,l,i,0)
	IF (i.GT.0) THEN
C look for zone nr i (it's infact a name)
	   iz=1
112	   IF (INTCON(ZoNa(iz),eflag).NE.i .AND. iz.LT.Nz) THEN
	      iz=iz+1
	      GOTO 112
	   ENDIF
	   IF (iz.LE.Nz) THEN
C use SchZ which is for T H S Q schedules
           p=ICindex(SchZ(iz),OldSch)
           If(p.gt.0) then
             Call ReplaceWord(SchZ(iz),p,newsch)
           end if
	   ENDIF
	   GOTO 110
	ENDIF
	GOTO 900

C ---------------------------------------------------------------------
C affected elements are air flow components
C ---------------------------------------------------------------------
200	CONTINUE
	IF (elem(1:3).NE.'ALL' .AND.
     &	index('0123456789',elem(1:1)).EQ.0) GOTO 250
C (a) range specified -------------------------------------------------
	list=' '
	CALL RDLIST(2,elem,list)
	l=len(list)
	k=0
C change schedule names
C@tno jcp 1996Jun14_00:20:38 variab for GetWI
        variab='the affected link numbers for a schedule'
210	CALL GETWI(list,k,l,i,0)
	IF (i.GT.0) THEN
C look for link nr i (it's infact a name)
	   iz=1
212	   IF (INTCON(LiNa(iz),eflag).NE.i .AND. iz.LT.Nl) THEN
	      iz=iz+1
	      GOTO 212
	   ENDIF
	   IF (iz.LE.Nl) THEN
	      IF (SoL(i).EQ.oldsch) SoL(i)=newsch
	   ENDIF
	   GOTO 210
	ENDIF

	GOTO 900
C process air flow component short names ---------------------------
250	CONTINUE
	i=1
251	IF (elem.EQ.' ') GOTO 900
	IF (index('/ ',elem(i:i)).GT.0 .AND. i.GT.1) THEN
	   elnam=elem(1:i-1)
	   j=1
252	   IF (LiTyNa(j).NE.elnam .AND. j.LT.Nl) THEN
	      j=j+1
	      GOTO 252
	   ENDIF
	   IF (LiTyNa(j).EQ.elnam .AND. SoL(j).EQ.oldsch) THEN
	      SoL(j)=newsch
	   ENDIF
	   dum=elem(i+1:)
	   elem=dum
	   GOTO 250
	ELSE
	   i=i+1
	   GOTO 251
	ENDIF

900	RETURN
	END


C@NBI PGS 1999Aug09 - New subroutine called after reading weather data
Ch**********************************************************************

      SUBROUTINE ProcessWeather(Tlotus)

C***********************************************************************
C Purpose: Does basic processing of weather data after it has been read.
C          It is called whenever weather data is read from meteo file
C          or from the &-SCH-MET datablock in the .CIF file.
C          It contains a code block moved from routine TIMESTEP.
C          To reduce a lot of duplication, elements common to both
C          routines FILLBUF and TIMESTEP were been moved here. 
C Created: P.G.Schild 1999Aug09
C Changes: 
Ch**********************************************************************

        IMPLICIT NONE
	INCLUDE 'comv-inp.inc'
      INTEGER i
      REAL Tout_mean,fade
      DOUBLE PRECISION Tlotus
      DATA fade/1.0/
C     This common block also occurs in routine TIMESTEP
      COMMON/Tout_mean/Tout_mean
      SAVE fade

C     Unit conversion
C     ---------------

      VMet  = VMet  * ifact(UnitW)
      TMet  = TMet  * ifact(UnitTmul) + ifact(UnitToff)
      XhMet = XhMet * ifact(UnitXh)
C     Unit conversion : multiply by 1000.0 to change kPa to Pa
C     The barometric pressure values are NOT converted by the user pressure units
      PbMet = PbMet * 1000.0
              
C     Vet weather data
C     ----------------

      if (VMet  .lt. 0.0) Call Error(
     & 'Value out of range for Wind Speed in meteo schedule/file',3)
C     convert user wind velocity to m/s
      if (Dmet  .lt. 0.0 .or. Dmet .gt. 360.0) Call Error(
     & 'Value out of range for Wind Direction in meteo schedule/file',3)
      if (XhMet .lt. 0.0) Call Error(
     & 'Value out of range for Humidity Ratio in meteo schedule/file',3)
C     Convert humidity from user units to kg/kg
      if (PbMet .lt. 0.0) Call Error(
     & 'Strange value of Barometric Pressure in meteo schedule/file',3)
      Call CheckPb(Pbmet,Zmet,' Meteo Barometric')

C     Echo weather schedule
C     ---------------------
      if (outputoptions(10).gt.0) 
     &  call echosch3(TLotus,'Meteo',Vmet,Dmet,Tmet,Xhmet,PbMet)

C     Set outside H2O conc. from Xhmet
C     --------------------------------

C@tno jcp 1996Jun26_14:39:04 use meteo moisture as outdoor pollutant?
      if (UseMetH2O.gt.0 .and. ipolH2O.le.nconc) then
        Cout(ipolH2O)=XhMet
C       distribute the concentration over the external nodes/facade elements
        DO I=1,Nwind
          ExtConc(IpolH2O,I)=cout(ipolH2O)*OuCF(I)
          if (outputoptions(10).gt.0) call echosch6(TLotus,'SCH-POL'
     &      ,ipolH2O,i,Cout(ipolH2O),ExtConc(ipolH2O,I))
        ENDDO
      endif

C     Fading memory mean outside temp.
C     --------------------------------

C   - 'Tout_mean' and 'fade' are for fading-memory mean temperature for
C     adaptive thermal comfort model, to set room temperature to ideal
C     comfort temperature, with special temperature schedule option :Mx
C   - Presently only works properly with 1-hour timesteps, with chronological,
C     not binned, weather data.
      Tout_mean=(1.0-fade)*Tout_mean+fade*Tmet
      fade=MAX( 0.01 , fade/(1.0+fade) )

      END

