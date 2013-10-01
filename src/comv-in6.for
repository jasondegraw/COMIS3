C+*********************************************************** comv-in6.f

Ch**********************************************************************
C SCH-MET meteo schedule
	SUBROUTINE inSCHMET(Line, LinTyp, L, K)
C@tno jcp 1996Apr26_22:50:13 keep the first and last time at Meteo for
C use in MetTimeStart,MetTimeStop, if &-PR-SIMU is not in the file and use
C them as Simulation start- and stop-times
Ch**********************************************************************

        IMPLICIT NONE
	INCLUDE 'comv-inp.inc'
       INCLUDE 'comv-uni.inc'
        INTEGER LenStr

        CHARACTER*(*) line
        INTEGER k,l,LinTyp

C@tno jcp 1996Apr26_22:47:26 Word+ Logical first Line added to inSchMet
        INTEGER k1,l1
        CHARACTER Word*160
        LOGICAL FirstLine

C@tno jcp 1996Jul09_17:26:44 variab added to inschmet
        variab='reading SCH-METeo'

C SCH MET only the first line of the schedule is read here.
C If there are more, read from DAF later
C@tno jcp 1996Apr26_22:45:40 added to inSchMet:all lines are read, the time from
C the first and last are kept in MetTimeStart MetTimeStop

        FirstLine=.True.
C@tno jcp 1996May30_11:35:58 reintroduced if HeaderNr<>2
C@empa aw 2005oct19 According to UG MetName may be also after header 2. But there will be an error if it is missing. 
CC        if (headerNr.ne.2) then
          Variab='MetName'
          k=0
          CALL GETWS(Line,K,L,MetName,DMetName)
CC        else
CCC aparently there was no data below Header1
CC          MetName=Dmetname
CC
CCC@tno jcp 1996May30_18:06:45 put this name in DAF
CC          buf((MOD(pDAF-1,mBuf)+1))=MetName
CC          IF (MOD(pDAF,mBuf).EQ.0) CALL wDAF(pDAF,mbuf,buf)
CC          pDAF=pDAF+1
CC
CCC@tno jcp 1996Jul24_07:54:38 get the start time from this first data line
CC          l1=lenstr(Line)
CC          k1=0
CC          if (l1.gt.0) then
CC            Variab='Meteo Start time'
CC            Call GetWrd(Line,k1,L1,word)
CC            MetTimeStart=Word
CC            FirstLine=.False.
CC          end if
CC        end if

C read the schedule data and write to the DAF file (loop here)

10      continue

C
C Write this entry into the DAF file
C
	buf((MOD(pDAF-1,mBuf)+1))=LINE
	IF (MOD(pDAF,mBuf).EQ.0) CALL wDAF(pDAF,mbuf,buf)
	pDAF=pDAF+1

	Call readlin(line, LinTyp, K, .True.)

C@tno jcp 1996Apr26_23:07:44inSchMet:MetTimeStart=word if in the first line.
C the stop time is stored at the end of the routine
        if (linTyp .eq.TDATA) then
          l1=lenstr(Line)
          k1=0
          if (l1.gt.0) then
            Variab='Meteo Start or Stop time'
            Call GetWrd(Line,k1,L1,word)
            if (firstLine)  then
               MetTimeStart=Word
               FirstLine=.False.
            end if
          end if
        end if

        if (FlgEnd) goto 99
	if (LinTyp .ne. TDATA) goto 99
	l=lenstr(line)

	goto 10

99      continue

C@tno jcp 1996Apr26_22:49:39 store the last read word as stop time
        MetTimeStop=Word
	RETURN
	END

C@tno jcp 1996Jul16_18:27:24 copied inSCHMET to make inSCHPOL
Ch**********************************************************************
C SCH-POL outside pollutant concentration schedule
        SUBROUTINE inSCHPOL(Line, LinTyp, L, K)
Ch**********************************************************************

        IMPLICIT NONE
	INCLUDE 'comv-inp.inc'
       INCLUDE 'comv-uni.inc'

        CHARACTER*(*) line
        INTEGER k,l,LinTyp
        INTEGER LenStr

C@tno jcp 1996Jul09_17:26:44 variab added to inschmet
        variab='reading SCH-POL'

C SCH POL only the first line of the schedule is read here.
C If there are more, read from DAF later

cc        FirstLine=.True.
C@tno jcp 1996May30_11:35:58 reintroduced if HeaderNr<>2
        if (headerNr.ne.2) then
          Variab='Outside Pollutant schedule Name'
          k=0
          CALL GETWS(Line,K,L,SchPolName,DSchPolName)
C@tno jcp 1996May30_11:36:38 headernr end if
        else
C aparently there was no data below Header1
C@tno jcp 1996May30_11:38:49 would MetName make it into DAF?
          SchPolName=DSchPolname

C@tno jcp 1996May30_18:06:45 put this name in DAF
          buf((MOD(pDAF-1,mBuf)+1))=SchPolName
          IF (MOD(pDAF,mBuf).EQ.0) CALL wDAF(pDAF,mbuf,buf)
          pDAF=pDAF+1
        end if

C read the schedule data and write to the DAF file (loop here)

10      continue

C
C Write this entry into the DAF file
C
	buf((MOD(pDAF-1,mBuf)+1))=LINE
	IF (MOD(pDAF,mBuf).EQ.0) CALL wDAF(pDAF,mbuf,buf)
	pDAF=pDAF+1

	Call readlin(line, LinTyp, K, .True.)

        if (FlgEnd) goto 99
	if (LinTyp .ne. TDATA) goto 99
	l=lenstr(line)

	goto 10

99      continue
	RETURN
	END

Ch**********************************************************************
C POL-DES pollutant description
      SUBROUTINE inPOLDES(Line, LinTyp, L, K)
C***********************************************************************
C POL DES : PolNr; pollutant name; Molar mass; Diffusion coef
C@lbl rw 1992jun17
C       I introduced the variable "nconcpoldes" in the commonblock.
C       This variable is used as a counter of the number of pollutants
C       in the &-POL-DES part. There is further on the variable nconc
C       which has the value of the number of concentrations used in
C       the &-NET-ZP part. At the end of the subroutine inh there are
C       check statements which are checking whether both values fit
C       together.
C@tno jcp 1996Apr08_10:34:43 continuation line for the fictive sources
C@NBI PGS 2000Oct09 - Detabbed & tidied up; no syntax change
C@NBI PGS 2000Oct09 - Now we can define different I/O units for each pollutant,
C@NBI PGS 2000Oct09 - FlgIConc and FlgOConc now redundant, so removed
C@empa aw 2000nov30 Pollutant numbers should follow the sequence 1,2,3...
C                   With missing pollutant numbers in between we have problems 
C                   later on in pollutant routines.
Ch**********************************************************************

      IMPLICIT NONE
      INCLUDE 'comv-inp.inc'
      INCLUDE 'comv-uni.inc'
      INTEGER LenStr
      CHARACTER*(*) line
      CHARACTER MaxCStr*3
        INTEGER k,l,PolN,LStr,LinTyp,PolI
C-----

C@empa aw 2000nov30 PolI
      PolI=0
C     ! Loop here for each pollutant description
10    continue
      PolI=PolI+1
      Variab='PolN pollutant sequence number'
      k=0
C@tno jcp 1996Apr18_15:56:07 crashed when no integer as first word
CC    CALL GETWRD(Line,K,l,word)
C@tno jcp 1996Jun14_00:14:41 variab for GetWI

      Variab='get number of the pollutant'
      CALL GETWI(Line,K,l,PolN,0)
CC    Poln=intcon(word,Eflag)
C@tno jcp 1996Apr18_16:00:30 check added
C@empa aw 2000nov30  Check changed. I want to have strictly the sequence 1,2,3.. 
CC        if (poln.eq.0) then
        if (poln.ne.PolI) then
           call intdis(MaxC,MaxCstr,LStr)
CC           call inerr('&-POL-DES: Pollutant data line must start'//
CC     & ' with a pollutant number 1..'//MaxCstr(1:LStr)//' .',
CC     &     'Line skipped. Correct the input. ',
         call inerr('&-POL-DES: Pollutant data line must start'//
     & ' with pollutant number in sequence 1..'//MaxCstr(1:LStr)//' .',
     &      'Line skipped. Correct the input. ',
     &      .TRUE.,2)
         GOTO 88
      else if(poln.gt.MaxC) then
         call intdis(MaxC,MaxCstr,LStr)
         call inerr('&-POL-DES: Pollutant number larger than '//
     &   MaxCstr(1:LStr)//' .',
     &   'Line skipped. Correct the input. ',
     &   .TRUE.,2)
         GOTO 88
      end if
      nconcpoldes=max(nconcpoldes,poln)
C@tno jcp 1996Apr06_09:57:22 check if you redefine a pollutant
      if (MM(polN).gt.0) then
         call intdis(Poln,MaxCstr,LStr)
         call inerr('&-POL-DES: Pollutant '//MaxCstr(1:LStr)//
     &      ' has already been defined.',
     &      'Use every Pollutant number only once here. ',
     &      .TRUE.,2)
C skip this line but read on for the next line
         goto 88
      end if
      if (nconcpoldes .GT. MaxC) then
         call intdis(MaxC,MaxCstr,LStr)
         call inerr('&-POL-DES: Too many pollutants defined !',
     &      'Maximum number of pollutants: '//MaxCstr(1:LStr),
     &      .FALSE.,2)
         GOTO 88
      endif

      Variab='Cname(PolN)'
      CALL GETWS(Line,K,L,Cname(PolN),'pollutantx')

      Variab='MM(PolN)'
      CALL GETWR(Line,K,L,MM(PolN), DMM,.TRUE.)

C@lbl bvs 1999jun01 added input for first order decay term rk() (already
C          implemented in comv-pol.f)
      Variab='Decay(PolN)'
      CALL GETWR(Line,K,L,rk(PolN), 0.0,.TRUE.)

C@lbl bvs 1999jun01 Pollutant Diffusion and limit terms were never implemented
CCC   ! Pol diffusion added
CC    Variab='Diffusion(PolN)'
CC    CALL GETWR(Line,K,L,Diffusion(PolN), DDiffusion,.TRUE.)

CCC   ! Pol Limit Concentration
CC    Variab='PolLimCon(PolN)'
CC    CALL GETWR(Line,K,L,PolLimCon(PolN), 1E-6,.TRUE.)


C----------------------------------
C     Concentration unit conversion
C----------------------------------

C     ! New part added to determine the concentration conversion factors
C     ! ifact(UnitPolCo+1..UnitPolCo+5)   &   ofact(UnitPolCo+1..UnitPolCo+5)
C     ! This flag FlgPolDes says that molar mass (MM) has been read and future
C     ! unit conversions can take place.
      FlgPolDes=1

C@empa aw 2000jan07 Take IUnit array not OUnit
CC    CALL CnvIConc(OUnit(UnitPConc),UnitStr(UnitPConc),
C@NBI PGS 2000Oct09 - Now we can define different I/O units for each pollutant,
CC    CALL CnvIConc(IUnit(UnitPConc),UnitStr(UnitPConc),
CC   &   Conv(1,UnitPConc),ifact(UnitPol),VM,mm)
      CALL CnvIConc(IUnit(UnitPolCo+PolN)
     &   ,UnitStr(UnitPConc),Conv(1,UnitPConc),VM,mm(PolN)
     &   ,ifact(UnitPolCo+PolN))

C@NBI PGS 2000Oct09 - Now we can define different I/O units for each pollutant,
CC    CALL CnvConc(OUnit(UnitPConc),UnitStr(UnitPConc),
CC   &   Conv(1,UnitPConc),ofact(UnitPol),VM,mm)
      CALL CnvConc(OUnit(UnitPolCo+PolN)
     &   ,UnitStr(UnitPConc),Conv(1,UnitPConc),VM,mm(PolN)
     &   ,ofact(UnitPolCo+PolN))
C-----

C     ! See if there is a continuation line
      Call readlin(line, LinTyp, K, .True.)
      if (FlgEnd) goto 99
      l=lenstr(line)
      if (LinTyp .eq. TDATA) goto 10
      if (LinTyp .eq. TCont) goto 110
      goto 99


C----------------------
C     Continuation line
C----------------------

110   continue
C     ! skip the first character (that is the '& ')
      K=2

C     ! Process the continuation line with fictive source strengths used
C     ! to calculate effective flowrates
      Variab='PolQzon(PolN,0) zone fixed fictive source'
      CALL GETWR(Line,K,L,PolQzon(PolN,0), 1E-6,.TRUE.)

      Variab='PolQzon(PolN,1) zone floor area fictive source'
      CALL GETWR(Line,K,L,PolQzon(PolN,1), 1E-6,.TRUE.)

      Variab='PolQzon(PolN,2) zone wall area fictive source'
      CALL GETWR(Line,K,L,PolQzon(PolN,2), 1E-6,.TRUE.)

      Variab='PolQzon(PolN,3) zone volume fictive source'
      CALL GETWR(Line,K,L,PolQzon(PolN,3), 1E-6,.TRUE.)

      Variab='PolQocc(PolN) occupant fictive source'
      CALL GETWR(Line,K,L,PolQocc(PolN), 1E-6,.TRUE.)

C     ! See if there another line of data
88    continue
      Call readlin(line, LinTyp, K, .True.)
      if (FlgEnd) goto 99
      l=lenstr(line)
      if (LinTyp .eq. TDATA .or. LinTyp .eq. TCont) goto 10
      goto 99
C-----

99    CONTINUE
      RETURN
      END


Ch**********************************************************************
C POL-FICtive Pollutant fictive source
      SUBROUTINE inPOLFIC(Line, LinTyp, L, K)
C POL-FIC : Fictive source Nr; pollutant name; fictive source; Dependency
C@empa aw 2000feb01
Ch**********************************************************************

      IMPLICIT NONE
      INCLUDE 'comv-inp.inc'
      INCLUDE 'comv-uni.inc'
      INTEGER LenStr
      CHARACTER*(*) line
      CHARACTER Str*3
      INTEGER k,l,FSoN,LStr,LinTyp
C-----

C Loop here for each fictive source
10    continue
      k=0

      Variab='fictive source Nr'
      CALL GETWI(Line,K,l,FSoN,0)
      if ((FSoN.lt.1).or.(FSoN.gt.MaxFSo)) then
         call intdis(MaxFSo,Str,LStr)
         call inerr('&-POL-FIC: fictive source data line must start'//
     &      ' with a number 1..'//Str(1:LStr)//' .',
     &      'Line skipped. Correct the input. ',
     &      .TRUE.,2)
         GOTO 88
      end if
      if (LenStr(FSPolN(FSoN)).gt.0) then
         call intdis(FSoN,Str,LStr)
         call inerr('&-POL-FICtive Source '//Str(1:LStr)//
     &      ' has already been defined.',
     &      'Use every fictive source number only once here. ',
     &      .TRUE.,2)
         goto 88
      end if

      Variab='Pollutant(FSoN)'
      CALL GETWS(Line,K,L,FSPolN(FSoN),'pollutantx')

      Variab='FSo(FSoN)'
C@NBI PGS 2000Jul20 - Default value "DFSo" not declared or initialized, so...
CC    CALL GETWR(Line,K,L,FSo(FSoN), DFSo,.TRUE.)
      CALL GETWR(Line,K,L,FSo(FSoN), 0.0,.TRUE.)
C@NBI PGS 2000Oct08 - Can now define different I/O units for each pollutant.
CC    FSo(FSoN)=ifact(UnitPSou)*FSo(FSoN)
      FSo(FSoN)=ifact(UnitPolSo+FSoN)*FSo(FSoN)

      Variab='FSDep(FSoN)'
      CALL GETWI(Line,K,L,FSDep(FSoN), 0)
      if (FSDep(FSoN).gt.6) then
         call intdis(FSoN,Str,LStr)
         call inerr('Fictive source Nr '//Str(1:LStr)//
     &      ': Dependency parameter must be less than 7  '//
     &      'Error in line: ',line,
     &      .false.,2)
      endif

88    continue
      Call readlin(line, LinTyp, K, .True.)
      if (FlgEnd) goto 99
      if (LinTyp .ne. TDATA) goto 99
      l=lenstr(line)
      goto 10

99    continue
      RETURN
      END
