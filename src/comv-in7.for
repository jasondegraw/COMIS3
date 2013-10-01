C+*********************************************************** comv-in7.f

Ch**********************************************************************
C OCCUPANt description
	SUBROUTINE inOCCU(Line, LinTyp, L, K)
C@NBI PGS 2000Nov01 - added integer "i"
Ch**********************************************************************
        IMPLICIT NONE
	INCLUDE 'comv-inp.inc'
	INCLUDE 'comv-uni.inc'
        INTEGER LenStr
	CHARACTER*(*) line
        INTEGER k,l,LinTyp,i
        INTEGER IcIndex
C@empa aw 2000mar22 eflag,intcon,word,word2 added
        INTEGER eflag,intcon
        INTEGER Occnr,Lstr
        REAL Adubois,FnAdubois
        character str*40
	  character word*3, word2*2
        logical isman,iswoman,ismix

C Loop here for each occupant

10      continue
C@empa aw 2000mar22 Now first line with "*" and continuation line without "&"
C@empa              this is more conform with COMIS standard

	IF (LinTyp .ne. TNAME) goto 900
	k=0
	Variab='occupant'
	CALL GETWRD(line,k,l,word)
	word2=word(2:)
	OccNr= intcon(word2,eflag)
	if (eflag.eq.1) goto 900
C@NBI PGS 2000Nov02 - This line is redundant duplication - so now commented out
CC	Variab='Occupant'
C@tno jcp 1996Apr18_18:02:57 getwrd now GetWI in inOccu
CC        CALL GETWI(Line,K,l,OccNr,0)
C@tno jcp 1996Apr01_11:50:49 here I see Occupant is still a number, OK!
        if (OccNr.lt.1) then
CC          call intdis(OccNr,str,LStr)
          call inerr('&-OCCUPANt: occupant number is '//word2//
     &    ', which is lower than 1!',
     &    'Correct the input file.',
     &    .FALSE.,2)
          goto 88
        end if
        if (OccNr.gt.MaxO) then
          call intdis(MaxO,str,LStr)
          call inerr('&-OCCUPANt: Too many occupants defined !',
     &    'Maximum number of occupants: '//str(1:LStr),
     &    .FALSE.,2)
          goto 88
        end if
        if (OccAge(OccNr).gt.0) then
          call intdis(OccNr,str,LStr)
          call inerr('&-OCCUPANt: Occupant '//str(1:lstr)//' has '//
     &    'already been defined','Correct the input',
     &    .True.,2)
          goto 88
        end if

        if (OccNr.gt.MaxOccNrD) MaxOccNrD=OccNr

	Variab='OccSex(OccNr)'
        CALL GETWS(Line,K,L,OccSex(OccNr),'MIX')
C@tno jcp 1996Apr22_15:41:02 test the input for Occupant Sex being MAN WOMAN MIX
        str=(OccSex(OccNr))
        if ((isMan(str).eqv. .FALSE.  ) .and.
     &      (IsWoman(str).eqv. .False.) .and.
     &      (isMix(str).eqv. .False.  )) then
          call intdis(OccNr,str,LStr)
          call inerr('&-OCCUPANt: Occupant '//str(1:lstr)//' has '//
     &    'Sex='//OccSex(OccNr),
     &    'Must be ''man'' ''woman'' or ''mix'' . Correct the input',
     &    .True.,2)
        end if

	Variab='OccAge(OccNr) [years]'
        CALL GETWR(Line,K,L,OccAge(OccNr),20.0,.TRUE.)

	Variab='OccL(OccNr) [metres]'
C@tno jcp 1996Apr22_15:44:10 if default, the formula OccLength is used
        CALL GETWR(Line,K,L,  OccL(OccNr),0.0,.FALSE.)
C@tno jcp 1996Apr22_15:38:37 default formula added for occupant length
        if (OccL(OccNr).lt.REALmin) then
          Call OccLength(OccL(OccNr),OccAge(OccNr),OccSex(OccNr))
C@NBI PGS 2000Aug18 - outputoptions(7) bugfixed:
CC        if (outputoptions(7).gt.0) then
          if (MOD(outputoptions(7),FlgDefault).gt.0) then
C@NBI PGS 2000Jul16 - Improved output
            write(COF,101) OccL(OccNr),variab(1:lenstr(variab))
     &      ,line(1:lenstr(line))
          end if
        end if

	Variab='OccM(OccNr) [kg]'
C@tno jcp 1996Apr22_15:44:10 if default, the formula OccMass is used
        CALL GETWR(Line,K,L,  OccM(OccNr),0.0,.FALSE.)
C@tno jcp 1996Apr22_15:38:37 default formula added for occupant Mass
        if (OccM(OccNr).lt.REALmin) then
          Call OccMass(OccM(OccNr),OccAge(OccNr),OccSex(OccNr))
C@NBI PGS 2000Aug18 - outputoptions(7) bugfixed:
CC        if (outputoptions(7).gt.0) then
          if (MOD(outputoptions(7),FlgDefault).gt.0) then
C@NBI PGS 2000Jul16 - Improved output
            write(COF,101) OccM(OccNr),variab(1:lenstr(variab))
     &      ,line(1:lenstr(line))
          end if
        end if

C OccAct is a 2-dimensional array now. It is filled
C with the respective values, when the program reads an occupant
C schedule. The original occupant activity which is read here,
C is stored in the array OOccAct.
C@empa aw 2000dec05 I wouldn't use MET as unit here. As far as I know
C@                  1Met is fixed defined by Fanger as 58W/m2, but here 
C@                  we can use any basal metabolic rate (see also routine
C@                  OccMet, where depending on age and sex the metabolic 
C@                  rate reaches from 37 up to 60 W/m2.)
C@NBI PGS 2000dec23 - I just thought that a clarification might be handy
C@NBI                 to prevent possible misunderstanding.  OOccAct is
C@NBI                 the metabolic rate [Watts] of the person at rest;
C@NBI                 you get the real rate by multiplying OOccAct by the
C@NBI                 "Activity factor", hence the clarification below.
C@NBI                 I agree with you Andreas in that the activity factor
C@NBI                 used in COMIS is equivalent to "Met" in the context
C@NBI                 of being a factor to be multiplied with metabolic
C@NBI                 rate when seated at rest, but not in the context
C@NBI                 of being a constant 58 W/m².
C@NBI               - Another clarification may be useful:
C@NBI                 The body's metabolic rate is BMR + WL, where BMR is
C@NBI                 "basal metabolic rate", and WL is the "metabolic
C@NBI                 free energy production".  WL is 15 W/m² for sedentary
C@NBI                 sitting (1 MET), and BMR is approx. 43 W/m² for an
C@NBI                 average human, thus BMR + WL = approx. 58 W/m² for
C@NBI                 an average human seated at rest (1 MET). Ref. ISO 8996
CC	Variab='OOccAct(OccNr) [Watts @1Met.]'
CC	Variab='OOccAct(OccNr) [W]'
CC	Variab='OOccAct(OccNr) [Watts @ activity factor of 1]'
        Variab='OOccAct(OccNr) [Watts @ activity level of 1 MET]'
C@tno jcp 1996Apr22_15:44:10 if default, the formula OccMet is used
        CALL GETWR(Line,K,L,OOccAct(OccNr),0.0,.FALSE.)
C@tno jcp 1996Apr22_15:38:37 default formula added for occupant specific metabol
        if (OOccAct(OccNr).lt.REALmin) then
          Call OccMet(OOccAct(OccNr),OccAge(OccNr),OccSex(OccNr))
          Adubois=FNAdubois(OccM(OccNr),OccL(OccNr))

          OOccAct(OccNr)=OOccAct(OccNr)*Adubois
C OOccAct is the metabolic rate (W) of the person OccNr seated at rest 
C You get the REAL rate by multiplying by the activity factor, 1..1.8..3
C@NBI PGS 2000Aug18 - outputoptions(7) bugfixed:
CC        if (outputoptions(7).gt.0) then
          if (MOD(outputoptions(7),FlgDefault).gt.0) then
C@NBI PGS 2000Jul16 - Improved output
            write(COF,101) OOccAct(OccNr),variab(1:lenstr(variab))
     &      ,line(1:lenstr(line))
          end if
        end if

	Variab='OccSmo(OccNr) [cig./h]'
        CALL GETWR(Line,K,L,OccSmo(OccNr),0.0,.TRUE.)

	Variab='OccName(OccNr) Name of the occupant'
C@NBI PGS 2000Aug18
CC	CALL GETWS(Line,K,L,OccName(OccNr),'OccName ')
	CALL GETWS(Line,K,L,OccName(OccNr),'(No_name_given) ')

C----------------inserted part to read continuation lines

C see if there is a continuation line


C@empa aw 2000mar22 Now first line with "*" and continuation line without "&"
C@empa              this is more conform with COMIS standard
20    continue
      call readlin(line, LinTyp, K, .True.)
	if (FlgEnd) goto 99
	l=lenstr(line)
CC	if (LinTyp .eq. TDATA) then
	if (LinTyp .eq. TNAME) goto 10
CC	if (LinTyp .eq. TCont) then
	if (LinTyp .eq. TDATA) goto 110
	goto 99

110     continue
C skip the first character (that is the '& ')
C@empa aw 2000mar22 Now first line with "*" and continuation line without "&"
CC        K=2
          k=1
C process the continuation line with fictive source strengths used to calculate
C effective flowrates
C@NBI PGS 2000Nov01 - Unnecessary duplication : Three almost-identaical
C@NBI                 bits of code can be rationalized into a DO loop.
C@NBI               - Added "odour" (bioeffluent emission: olf/decipol)
        DO i=1,MaxC

          WRITE(word2,'(I2)') i
          Variab='Name of occupant pollutant nr.'//word2
          CALL GETWS(Line,K,L,OccPolN(OccNr,i),' ')

C@empa aw 2005oct06 String is too long for the string variable  
CC          Variab='Source/sink strength of occupant pollutant nr.'//word2
          Variab='Source/sink strength of occ pol. nr.'//word2
          CALL GETWR(Line,K,L,OccPol(OccNr,i), 0.0,.false.)

C@tno jcp 1996Jun12_14:30:51 FlgDefault is now an integer
          if (FlgDefault.gt.0) then
            call intdis(OccNr,str,lstr)
            if (ICindex(OccPolN(OccNr,i),'CO2').gt.0) then
              Call OccCO2(OccPol(OccNr,i),OccNr)
C@NBI PGS 2000Aug18 - outputoptions(7) bugfixed:
CC            if (outputoptions(7).ge.FlgDefault) then
              if (MOD(outputoptions(7),FlgDefault).gt.0)
C@NBI PGS 2000Jul16 - Improve & standardize all output for default values
     &          write(COF,100) OccPol(Occnr,i),'CO2',str(1:lstr)
     &          ,OccName(Occnr)(1:lenstr(OccName(Occnr)))
            else if (ICindex(OccPolN(OccNr,i),'O2').gt.0) then
              Call OccO2(OccPol(OccNr,i),OccNr)
C@NBI PGS 2000Aug18 - outputoptions(7) bugfixed:
CC            if (outputoptions(7).ge.FlgDefault) then
              if (MOD(outputoptions(7),FlgDefault).gt.0)
C@NBI PGS 2000Jul16 - Improve & standardize all output for default values
     &          write(COF,100) OccPol(Occnr,i),'O2',str(1:lstr)
     &          ,OccName(Occnr)(1:lenstr(OccName(Occnr)))
            else if (ICindex(OccPolN(OccNr,i),'H2O').gt.0) then
              Call OccH2O(OccPol(OccNr,i),OccNr)
C@NBI PGS 2000Aug18 - outputoptions(7) bugfixed:
CC            if (outputoptions(7).ge.FlgDefault) then
              if (MOD(outputoptions(7),FlgDefault).gt.0)
C@NBI PGS 2000Jul16 - Improve & standardize all output for default values
     &          write(COF,100) OccPol(Occnr,i),'H2O',str(1:lstr)
     &          ,OccName(Occnr)(1:lenstr(OccName(Occnr)))
            else if (ICindex(OccPolN(OccNr,i),'ODOUR').gt.0) then
              Call OccOlf(OccPol(OccNr,i),OccNr)
              if (MOD(outputoptions(7),FlgDefault).gt.0)
     &          write(COF,100) OccPol(Occnr,i),'ODOUR',str(1:lstr)
     &          ,OccName(Occnr)(1:lenstr(OccName(Occnr)))
            end if
          end if
        ENDDO

CC        Variab='OccPolN(OccNr,2) name of 2nd base source'
CC        CALL GETWS(Line,K,L,OccPolN(OccNr,2),' ')
CC
CC        Variab='OccPol(OccNr,2) 2nd base sourcestrength'
CC        CALL GETWR(Line,K,L,OccPol(OccNr,2), 0.0,.false.)
CCC@tno jcp 1996Jun12_14:30:51 FlgDefault is now an INTEGER
CC        if (FlgDefault.gt.0) then
CC          call intdis(OccNr,str,lstr)
CC          if (ICindex(OccPolN(OccNr,2),'CO2').gt.0) then
CC            Call OccCO2(OccPol(OccNr,2),OccNr)
CCC@NBI PGS 2000Aug18 - outputoptions(7) bugfixed:
CCCC          if (outputoptions(7).ge.FlgDefault) then
CC            if (MOD(outputoptions(7),FlgDefault).gt.0) then
CCC@NBI PGS 2000Jul16 - Improve & standardize all output for default values
CC              write(COF,100) OccPol(Occnr,2),'CO2',str(1:lstr)
CC     &        ,OccName(Occnr)(1:lenstr(OccName(Occnr)))
CC            end if
CC          else if (ICindex(OccPolN(OccNr,2),'O2').gt.0) then
CC            Call OccO2(OccPol(OccNr,2),OccNr)
CCC@NBI PGS 2000Aug18 - outputoptions(7) bugfixed:
CCCC          if (outputoptions(7).ge.FlgDefault) then
CC            if (MOD(outputoptions(7),FlgDefault).gt.0) then
CCC@NBI PGS 2000Jul16 - Improve & standardize all output for default values
CC              write(COF,100) OccPol(Occnr,2),'O2',str(1:lstr)
CC     &        ,OccName(Occnr)(1:lenstr(OccName(Occnr)))
CC            end if
CC          else if (ICindex(OccPolN(OccNr,2),'H2O').gt.0) then
CC            Call OccH2O(OccPol(OccNr,2),OccNr)
CCC@NBI PGS 2000Aug18 - outputoptions(7) bugfixed:
CCCC          if (outputoptions(7).ge.FlgDefault) then
CC            if (MOD(outputoptions(7),FlgDefault).gt.0) then
CCC@NBI PGS 2000Jul16 - Improve & standardize all output for default values
CC              write(COF,100) OccPol(Occnr,2),'H2O',str(1:lstr)
CC     &        ,OccName(Occnr)(1:lenstr(OccName(Occnr)))
CC            end if
CC          end if
CC        end if
CC        Variab='OccPolN(OccNr,3) name of 3rd base source'
CC        CALL GETWS(Line,K,L,OccPolN(OccNr,3),' ')
CC
CC        Variab='OccPol(OccNr,3) 3rd base sourcestrength'
CC        CALL GETWR(Line,K,L,OccPol(OccNr,3), 0.0,.false.)
CCC@tno jcp 1996Jun12_14:30:51 FlgDefault is now an INTEGER
CC        if (FlgDefault.gt.0) then
CC          call intdis(OccNr,str,lstr)
CC          if (ICindex(OccPolN(OccNr,3),'CO2').gt.0) then
CC            Call OccCO2(OccPol(OccNr,3),OccNr)
CCC@NBI PGS 2000Aug18 - outputoptions(7) bugfixed:
CCCC          if (outputoptions(7).ge.FlgDefault) then
CC            if (MOD(outputoptions(7),FlgDefault).gt.0) then
CCC@NBI PGS 2000Jul16 - Improve & standardize all output for default values
CC              write(COF,100) OccPol(Occnr,3),'CO2',str(1:lstr)
CC     &        ,OccName(Occnr)(1:lenstr(OccName(Occnr)))
CC            end if
CC          else if (ICindex(OccPolN(OccNr,3),'O2').gt.0) then
CC            Call OccO2(OccPol(OccNr,3),OccNr)
CCC@NBI PGS 2000Aug18 - outputoptions(7) bugfixed:
CCCC          if (outputoptions(7).ge.FlgDefault) then
CC            if (MOD(outputoptions(7),FlgDefault).gt.0) then
CCC@NBI PGS 2000Jul16 - Improve & standardize all output for default values
CC              write(COF,100) OccPol(Occnr,3),'O2',str(1:lstr)
CC     &        ,OccName(Occnr)(1:lenstr(OccName(Occnr)))
CC            end if
CC          else if (ICindex(OccPolN(OccNr,3),'H2O').gt.0) then
CC            Call OccH2O(OccPol(OccNr,3),OccNr)
CCC@NBI PGS 2000Aug18 - outputoptions(7) bugfixed:
CCCC          if (outputoptions(7).ge.FlgDefault) then
CC            if (MOD(outputoptions(7),FlgDefault).gt.0) then
CCC@NBI PGS 2000Jul16 - Improve & standardize all output for default values
CC              write(COF,100) OccPol(Occnr,3),'H2O',str(1:lstr)
CC     &        ,OccName(Occnr)(1:lenstr(OccName(Occnr)))
CC            end if
CC          end if
CC        end if
C@NBI PGS 2000Nov01   (end)

88	Call readlin(line, LinTyp, K, .True.)
	if (FlgEnd) goto 99
	l=lenstr(line)
C@empa aw 2000mar22 Now first line with "*" and continuation line without "&"
CC       if (LinTyp .eq. TDATA .or. LinTyp .eq. TCont) goto 10
       if (LinTyp .eq. TName.or.Lintyp.eq.TData ) goto 10
	goto 99
C----------end-inserted part to read continuation lines

C@NBI PGS 2000Jul16 - New format statements for Improved output
100   FORMAT(' Def. value ',G10.4,' used for ',A,
     &' source strength for occupant ',A,1X,'"',A,'"')
101   FORMAT(' Def. value ',G10.4,' used for ',A,' in line: ',A)

900   CALL INERR('&-OCCUPAN: Occupant number must be defined  '//
     &                  'with: "*No" ',line,.false.,2)
      call readlin(line, LinTyp, K, .True.)
	if (FlgEnd) goto 99
	if (LinTyp .eq. TNAME.or.Lintyp.eq.TData) goto 10


99    continue
	RETURN
	END


Ch**********************************************************************
C NORM-CR normalized crack temperature
	SUBROUTINE inNORMCR(Line, LinTyp, L, K)
Ch**********************************************************************
        IMPLICIT NONE
	INCLUDE 'comv-inp.inc'
	CHARACTER*(*) line
        INTEGER k,l,LinTyp
C@tno jcp 1996Apr07_16:05:46 1 line added
        REAL Zref1

C NORM-CR temperature to which all input crack data is normalised
	Variab='NormCrT'
	k=0
        CALL GETWR(Line,K,L,NormCrT,DNormCrT,.TRUE.)
C unit conversion
	NormCrT=ifact(UnitToff)+ifact(UnitTmul)*NormCrT
	Variab='NormCrPb'
        CALL GETWR(Line,K,L,NormCrPb,DNormCrPb,.TRUE.)
	NormCrPb=1000*NormCrPb

C@tno jcp 1996Apr07_16:06:06 zref=sealevel
        Zref1=0.0
        Call CheckPb(NormCrPb,Zref1,' Standard Crack Barometric')

	Variab='NormCrXh'
        CALL GETWR(Line,K,L,NormCrXh,DNormCrXh,.TRUE.)
C unit conversion
	NormCrXh=ifact(UnitXh)*NormCrXh

C read next line for next keyword
	Call readlin(line, LinTyp, K, .True.)

	RETURN
	END


Ch**********************************************************************
C SCH-MUL (multi-schedules)
	SUBROUTINE inSCHMUL(Line, LinTyp, L, K)
Ch**********************************************************************
        IMPLICIT NONE
	INCLUDE 'comv-inp.inc'
	CHARACTER*(*)line
	CHARACTER name*30
      INTEGER k,l,col,LinTyp,Lenstr


C@tno jcp 1996Jul09_17:28:06 variab added to inSCHMUL
        variab='reading multi schedule data'

C@tno jcp 1996May30_14:18:57 check the headerNr?

        col = index(line,'F:')
C@tno jcp 1996Jun27_16:01:26 try again for f:
        if (col.eq.0) then
          col=index(line,'f:')
        end if
	if (col .ne. 0) then

C          its the definition of the filename

	   k = 0
	   Call GETWRD(line(col+2:),k,l,multifile)

C          Now read the schedule names from the next line

	   Call readlin(line, LinTyp, K, .True.)
	   if (FlgEnd) goto 900
	   if (LinTyp .ne. TDATA) goto 900
	   multischednames = line

	else
C          No file, just the schedule name
	   multischednames = line
	endif

C@empa aw 05jan2000 Multischedul names has to be in aName string for the check of
C                   all used schedules in PreSch
      k = 0
      l=lenstr(multischednames)
 10   CONTINUE
        CALL GETWRD(multischednames,k,l,name)
        nName = nName+1
        aName(pname:) = '*'//name//' '
        pname = pname+lenstr(name)+2
      if (k .lt. l) goto 10

C read next line for next keyword
	Call readlin(line, LinTyp, K, .True.)
	goto 99

900     call inerr('&-SCH-MUL: Schedule name(s) missing from'//
     &          ' multi-schedule',' ',.true.,2)
	goto 99

99      continue
	RETURN
	END


Ch**********************************************************************
C USERdat (User input data)
      SUBROUTINE inUSER(Line, LinTyp, L, K)
C Changes:
C@NBI PGS 2000Oct09 - Tidied up; no syntax change
Ch**********************************************************************

      IMPLICIT NONE
      INCLUDE 'comv-inp.inc'
      INTEGER LenStr,k,l,LinTyp,i,j,LenDum,LenDum2
      CHARACTER*(*)line
      CHARACTER DumStr*40, DumStr2*40
C-----

C     ! Get up to 10 lines each with up to 10 (REAL) values
      do i=1,10
         CALL IntDis(I,dumstr,LenDum)
         do j=1,10
            CALL IntDis(J,dumstr2,LenDum2)
            variab='User input data: Userdat('//
C@NBI PGS 2000Aug18 - Someone forgot last bracket
CC   &         dumstr(1:LenDum)//','//dumstr2(1:lendum2)
     &         dumstr(1:LenDum)//','//dumstr2(1:lendum2)//')'
            CALL GETWR(Line,K,L,USERDAT(i,j),0.0,.TRUE.)
         ENDDO
C        ! Read next line of user data
         Call readlin(line, LinTyp, K, .True.)
         if (FlgEnd) goto 99
         if (LinTyp .ne. TDATA) goto 99
         l=lenstr(line)
      ENDDO

99    continue
      return
      end


Ch**********************************************************************
      SUBROUTINE inUNITS(Line, LinTyp, L, K)
C inUNITS is called from inpdat while processing the input file
C In the input file the 49-th keyword &-PR-UNITS is found
C
C     In the input file data could look like:
C&-PR-UNits
C#    Name         input       output
C     airleakage    m2         kg/s@1Pa
C     massflow      kg/s       kg/h
C     pressure      Pa         Pa
C     temperature   degC       F
C     humidity      g/kg       g/kg
C     source        kg/s       kg/s
C     sink          m3/s       m3/s
C     concentration kg/kg      ug/m3
C     fan           m3/s       m3/s
C     velocity      m/s        Kt
C     ach           ---        1/h
C     mean age      ---        h
C     energy        ---        kwh
C     profile       alpha      ---
C line 15=tempoffset
C
C But after the keyword &-PR-UNITS the word INPUT or OUTPUT can follow
C which will switch to only input or output units
C&-PR-UNITS
C  INPUT
C#    Name         only input
C     airleakage    m2
C     massflow      kg/h
C
C Changes:
C@NBI PGS 2000Oct09 - Tidied up; no syntax change
C@NBI PGS 2000Oct08 - Now we can define different I/O units for each pollutant,
C@NBI                 for SINK, SOURce and CONCentration, simply by adding
C@NBI                 a new line for each pollutant in &-PR-UNITS in .CIF file !!
C@NBI                 This method is backwardly compatible with old CIF files.
C@NBI                 However this hasn't been implemented in SET file;
C@NBI                 all pollutants are defined with same units there.
C@NBI               - INTEGERs iPol, iPolSi, iPolSo, iPolCo added.
C@NBI               - FlgIConc and FlgOConc now redundant, so removed
Ch**********************************************************************

      IMPLICIT NONE
      INCLUDE 'comv-inp.inc'
      INCLUDE 'comv-uni.inc'
      INTEGER LenStr,IstrStr,k,l,LinTyp,nr,start,IOunit,kq,lq,lw,iPol
     &   ,iPolSi,iPolSo,iPolCo
      CHARACTER*(*) line
      character word*40, subwrd*10, wordq*10
C-----

C     ! switch IOUNIT for 0=I+O 1=Input only 2=Output only
C     ! The switch IOUNIT is triggered by a word INPUT or OUTPUT after
C     ! keyword &-PR-UNits
      IOUnit=0
C     ! Zero counters for pollutant sink, source & concentration
      iPolSi=0
      iPolSo=0
      iPolCo=0

      Variab='User unit '
110   CALL GETWRD(Line,K,L,Word)
C@tno jcp 1996Jun27_12:58:33 there are 7 characters defined for the KeysU string
C@empa aw 2000jun30 changed to 4 characters
CC    if (lenstr(word).gt.7) then
CC       word=word(1:7)
      if (lenstr(word).gt.4)  word=word(1:4)
      SubWrd=Word

      call UPPERC(word)
      if (word.eq.'INPU') then
         IOUnit=1
         Call readlin(line, LinTyp, K, .True.)
         l=lenstr(line)
C        ! See if we are at the end of the data yet
         if (FlgEnd) goto 88
         Variab='Quantity in inUnit'
         CALL GETWRD(Line,K,L,WORD)
         Call UpperC(Word)
C@tno jcp 1996Jun27_12:58:33 there are 7 characters defined for the KeysU string
C@empa aw 2000jun30 changed to 4 characters
CC       if (lenstr(word).gt.7) then
CC          word=word(1:7)
         if (lenstr(word).gt.4)  word=word(1:4)
         SubWrd=Word

      else if (word.eq.'OUTP') then
         IOUnit=2
         Call readlin(line, LinTyp, K, .True.)
         l=lenstr(line)
C See if we are at the end of the data yet
         if (FlgEnd) goto 88
         Variab='Quantity in inUnit'
         CALL GETWRD(Line,K,L,WORD)
         Call UpperC(Word)
C@tno jcp 1996Jun27_12:58:33 there are 7 characters defined for the KeysU string
C@empa aw 2000jun30 changed to 4 characters
CC       if (lenstr(word).gt.7) then
CC          word=word(1:7)
         if (lenstr(word).gt.4)  word=word(1:4)
         SubWrd=Word
C@empa aw 2000nov22 check length of the word
         lw=lenstr(word)
	   if (lw.lt.3) then
            call INERR('&-PR-UNITS: The identifier '//
     &      word(1:lw)//' is not valid.'//
     &      ' Quantity identifiers have to be at least three'//
     &      ' characters long.'//
     &      ' Line in error is:',
     &      line,.false.,1)
            call LastError2(
     &      'Valid identifiers are:'//KeysU(1:lenstr(keysU))//';'//
     &      'You may indicate'//
     &      ' with the keywords INPUT and/or OUTPUT, that '//
     &      'either input or output units follow (one unit per line).',
     &      ' ',0)
            goto 77
         endif
      end if
C-----

      start=1
      Variab='Quantity '//SubWrd(1:4)//':'
      nr=istrstr(KeysU,word,start)
C     ! check if nr=1,11,21...
      if (mod((nr-1),10).ne.0) then
C@empa aw 2000nov22 Message changed slightly
          call INERR('&-PR-UNITS: The identifier '//
     &  word(1:lenstr(word))//
     &  ' is not valid. Units are not updated. '//
C@NBI PGS 2000Jul16 - Improve grammar (I hope)
CC   &      'Line in error is:',
     &      'The erroneous line is:',
     &      line,.false.,1)
         call LastError2(
     & 'Valid identifiers are:'//KeysU(1:lenstr(keysU))//';'//
     & 'You may indicate'//
     &      ' with the keywords INPUT and/or OUTPUT, that '//
     &      'either input or output units follow (one unit per line).',
     &      ' ',0)
         goto 77
      end if
C@tno jcp 1996Jun27_12:59:28 new check for the given Quantity to match KeysU
C@empa aw 2000nov22 This check is not necessary. Above we found the word in KeysU,
C                   thus the spelling is always equal.
CC        kq=nr-1
CC        lq=lenstr(keysU)
CC        call getwrd(KeysU,kq,lq,wordq)
CC        lq=lenstr(wordq)
CC        lw=lenstr(word)
CC        if (wordq(1:lq).ne.word(1:lw)) then
CC          call INERR('After &-PR-UNITS, the quantity '//
CC     &  word(1:lenstr(word))//' looks like '//wordq(1:lenstr(wordq))//
CC     &  ' but is not spelled right. Units are not updated. '//
CC   &      'Line in error:',
CC     &  line,.false.,1)
CC          call LastError2(
CC     & 'Valid Quantities are:'//KeysU(1:lenstr(keysU))//';'//
CC     & 'Only the first 3..7 characters of these quantities are '//
CC     & 'interpreted.;'//
CC     & 'After the line &-PR-UNITS you may indicate'//
CC     & ' with the keywords INPUT and/or OUTPUT, that '//
CC     & 'either input or output units follow (one unit per line).',
CC     & ' ',0)
CC         goto 77
CC
CC        end if
C@tno jcp 1996Jun27_12:59:28 end new check
C-----

      nr=(nr-1)/10+1
C@empa aw 2000jun27 Old Identifiers for massflow and component airflow still
C@empa              valid.
      if (nr.eq.15) nr=UnitFma
      if (nr.eq.16) nr=UnitFva


C--------------------------------
C     Conversion from Input Units
C--------------------------------

      if (IOUNit.LT.2) then
C        ! input only or both input and output. Read input unit here
         CALL GETWRD(line,k,l,word)
         IUNIT(nr)=word
          if ((test.ge.1) .and. (iecho.ge.5)) THEN
            write(cof,*) Variab(1:17),' input  Unit=',word
          end if


C@tno jcp 1996Jun27_13:29:48 call to checkCnvCs added
         if (nr .eq. UnitCm ) THEN
C           ! Air leakage
C           ! CALL ConvCs is only possible per link when the flow exponent
C           ! is known but check the units
            call CheckCnvCs(IUnit(UnitCm),UnitStr(UnitCm))
C@NBI PGS 2000oct09 - Bugfix? This GOTO was commented out - no longer needed now
C           goto 98
         elseif (nr .eq. UnitFma) THEN
C           ! Ventilation (mass) flowrate
            CALL CnvIUnit(IUnit(UnitFma),UnitStr(UnitFma),
     &         Conv(1,UnitFma),ifact(UnitFma))
         elseif (nr .eq. UnitP) THEN
C           ! (air) pressure (differences)
            CALL CnvIUnit(IUnit(UnitP),UnitStr(UnitP),
     &         Conv(1,UnitP),ifact(UnitP))
C@tno 1996 always kPa
            DPbMet     = ODPbMet
C@tno 1996 always kPa
            DNormCrPb  = ODNormCrPb
         elseif (nr .eq. UnitTmul) THEN
C           ! Temperature  this conversion uses two constants
C           ! stored in position UnitTmul
            CALL CnvIT(IUnit(UnitTmul),UnitStr(UnitTmul),
     &         Conv(1,UnitToff),Conv(1,UnitTmul),
     &         ifact(UnitToff),ifact(UnitTmul))
C           ! unit conversion
            DTmet      = (ODTmet-ifact(UnitToff))/Ifact(UnitTmul)
            DNormCrT   = (ODnormCrT-ifact(UnitToff))/ifact(UnitTmul)
            DTz        = (ODTz-ifact(UnitToff))/ifact(UnitTmul)
         elseif (nr .eq. UnitXh) THEN
C           ! Humidity
            CALL CnvIUnit(IUnit(UnitXh),UnitStr(UnitXh),
     &         Conv(1,UnitXh),ifact(UnitXh))
C           !  unit conversion
            DXhMet     = ODXhMet/Ifact(UnitXh)
            DNormCrXh  = ODNormCrXh/ifact(UnitXh)
            DXhz       = ODXhz/ifact(UnitXh)
         elseif (nr .eq. UnitPSou) THEN
C           ! Source strength flowrate
C@NBI PGS 2000Oct08 - Now we can define different I/O units for each pollutant.
CC          CALL CnvIUnit(IUnit(UnitPSou),UnitStr(UnitPSou),
CC   &         Conv(1,UnitPSou),ifact(UnitPSou))
            iPolSo=iPolSo+1
            DO iPol=UnitPolSo+iPolSo,UnitPolSo+MaxC
               Iunit(iPol)=word
               CALL CnvIUnit(IUnit(iPol),UnitStr(UnitPSou),
     &            Conv(1,UnitPSou),ifact(iPol))
            ENDDO
         elseif (nr .eq. UnitPSin) THEN
C           ! Sink strength (=air flowrate from which pollutant is removed)
C@NBI PGS 2000Oct08 - Now we can define different I/O units for each pollutant.
CC            CALL CnvIUnit(IUnit(UnitPSin),UnitStr(UnitPSin),
CC     &         Conv(1,UnitPSin),ifact(UnitPSin))
            iPolSi=iPolSi+1
            DO iPol=UnitPolSi+iPolSi,UnitPolSi+MaxC
               Iunit(iPol)=word
               CALL CnvIUnit(IUnit(iPol),UnitStr(UnitPSin),
     &            Conv(1,UnitPSin),ifact(iPol))
            ENDDO
         elseif (nr .eq. UnitPConc) THEN
C           ! Concentration of air pollutants
C           ! This can only be done after the Molar Masses (mm) are known,
C           ! that is after &-POLDES has been read.
C@NBI PGS 2000Oct08 - Now we can define different I/O units for each pollutant.
CC          CALL CnvIConc(IUnit(UnitPConc),UnitStr(UnitPConc),
CC   &         Conv(1,UnitPConc),ifact(UnitPol),VM,mm)
            iPolCo=iPolCo+1
            DO iPol=UnitPolCo+iPolCo,UnitPolCo+MaxC
               IUnit(iPol)=word
               If (FlgPolDes.eq.1)
     &            CALL CnvIConc(IUnit(iPol),UnitStr(UnitPConc)
C@empa aw 2000dec07 parameter ipol is not requested (in fact it is wrong)
CC     &            ,Conv(1,UnitPConc),VM,mm,iPol,ifact(iPol))
     &            ,Conv(1,UnitPConc),VM,mm,ifact(iPol))
            ENDDO
         elseif (nr .eq. UnitFva) THEN
C           ! Conversion for fan flowrate (for output is not used)
C           ! As this is different from the m3/s that is used in COMIS as input for
C           ! the fan curves. We use the mass flow conversion UnitFma.
C           ! The fan flowrates that are calculated by the routine
C           ! FEQN are massflowrates.
C@empa aw 1995dec18 use UnitFva
CC59        CALL CnvUnit(IUnit(UnitFma),UnitStr(UnitFma),
CC     &            Conv(1,UnitFma),ifact(UnitFma))
            CALL CnvIUnit(IUnit(UnitFva),UnitStr(UnitFva),
     &         Conv(1,UnitFva),ifact(UnitFva))
C@empa aw 1995dec18 ifact(UnitFvFlag) Flag: volume or mass flow
            CALL CnvUnit(IUnit(UnitFva),UnitStr(UnitFva),
     &         Conv(1,UnitFvFlag),ifact(UnitFvFlag))
         elseif (nr .eq. UnitW) THEN
C           ! air/wind velocity
            CALL CnvIUnit(IUnit(UnitW),UnitStr(UnitW),
     &         Conv(1,UnitW),ifact(UnitW))
         elseif (nr .eq. UnitProf) THEN
C           ! check the word given for the windprofile unit
C           ! wind profile ALPHA or Z0
            call UpperC(word)
C@tno jcp 1996May10_11:44:36 if allows also Profile=Zo
            if (word.EQ.'ZO') word='Z0'
            if ((word.NE.'ALPHA').AND.(word.NE.'Z0')) then
               call inerr('The unit given for the windprofile is: '//
     &         Iunit(unitProf),
     &         'This should be alpha or Z0.',.TRUE.,2)
C@tno jcp 1996May10_11:42:55 else part added to put the upper case in Iunit(nr)
            else
C@lbl bvs 1998Sep15 changed iUnit(nr) to iUnit(UnitProf) for clarity
               iUnit(UnitProf)=word
            endif
         endif
      endif
C     This end if belongs to if (IOUNit.LT.2) then

C     ! If there is "input and output" and if we are reading the
C     ! WindProfile, then skip the next, and go out, to 77.
      if ((IOUnit.EQ.0) .AND. (nr.EQ.UnitProf)) goto 77


C-------------------------------
C     Conversion to Output Units
C-------------------------------

      if (IOUNit.NE.1) then
C        ! output or both input and output. Read output unit here
         CALL GETWRD(line,k,l,word)
         if ((test.ge.1) .and. (iecho.ge.5))
     &      write(cof,*) Variab(1:17),' output Unit=',word
         OUNIT(nr)=word

C call the appropriate Output Conversion routine

C@tno jcp 1996Jun27_13:32:49 check the Cs Unit
         if (nr .eq. UnitCm ) THEN
C           ! Air leakage
C1          ! CALL ConvCs is only possible per link when the flow exponent is known
            call CheckCnvCs(OUnit(UnitCm),UnitStr(UnitCm))
C@NBI PGS 2000oct09 - Bugfix? This GOTO was commented out - no longer needed
C           goto 98
         elseif (nr .eq. UnitFma) THEN
C           ! Ventilation (mass) flowrate
            CALL CnvUnit(OUnit(UnitFma),UnitStr(UnitFma),
     &         Conv(1,UnitFma),ofact(UnitFma))
         elseif (nr .eq. UnitP) THEN
C           ! (air) pressure (differences)
            CALL CnvUnit(OUnit(UnitP),UnitStr(UnitP),
     &         Conv(1,UnitP),ofact(UnitP))
         elseif (nr .eq. UnitTmul) THEN
C           ! Temperature this conversion uses two constants: UnitTmul and UnitToff
            CALL CnvT(OUnit(UnitTmul),UnitStr(UnitTmul),
     &         Conv(1,UnitToff),Conv(1,UnitTmul),
     &         ofact(UnitToff),ofact(UnitTmul))
         elseif (nr .eq. UnitXh) THEN
C           ! Humidity
            CALL CnvUnit(OUnit(UnitXh),UnitStr(UnitXh),
     &         Conv(1,UnitXh),ofact(UnitXh))
         elseif (nr .eq. UnitPSou) THEN
C           ! Source strength flowrate
C@NBI PGS 2000Oct08 - Now we can define different I/O units for each pollutant.
CC          CALL CnvUnit(OUnit(UnitPSou),UnitStr(UnitPSou),
CC   &         Conv(1,UnitPSou),ofact(UnitPSou))
            IF(IOUnit.EQ.2) iPolSo=iPolSo+1
            DO iPol=UnitPolSo+iPolSo,UnitPolSo+MaxC
               Ounit(iPol)=word
               CALL CnvUnit(OUnit(iPol),UnitStr(UnitPSou),
     &            Conv(1,UnitPSou),ofact(iPol))
            ENDDO
         elseif (nr .eq. UnitPSin) THEN
C           ! Sink strength (=air flowrate from which pollutant is removed)
C@NBI PGS 2000Oct08 - Now we can define different I/O units for each pollutant.
CC          CALL CnvUnit(OUnit(UnitPsin),UnitStr(UnitPsin),
CC   &         Conv(1,UnitPsin),ofact(UnitPsin))
            IF(IOUnit.EQ.2) iPolSi=iPolSi+1
            DO iPol=UnitPolSi+iPolSi,UnitPolSi+MaxC
               Ounit(iPol)=word
               CALL CnvUnit(OUnit(iPol),UnitStr(UnitPSin),
     &            Conv(1,UnitPSin),ofact(iPol))
            ENDDO
         elseif (nr .eq. UnitPConc) THEN
C           ! Concentration of air pollutants.
C           ! This can only be done after the Molar Masses (mm) are known,
C           ! that is when &-POLDES has been read.
C@NBI PGS 2000Oct08 - Now we can define different I/O units for each pollutant.
CC          CALL CnvConc(OUnit(UnitPConc),UnitStr(UnitPConc),
CC   &         Conv(1,UnitPConc),ofact(UnitPol),VM,mm)
            IF(IOUnit.EQ.2) iPolCo=iPolCo+1
            DO iPol=UnitPolCo+iPolCo,UnitPolCo+MaxC
               OUnit(iPol)=word
               if (FlgPolDes.eq.1)
     &            CALL CnvConc(OUnit(iPol),UnitStr(UnitPConc)
     &            ,Conv(1,UnitPConc),VM,mm(iPol),ofact(iPol))
            ENDDO
         elseif (nr .eq. UnitFva) THEN
C           ! Conversion for fan flowrate (for output is not used)
C           ! As this is different from the m3/s that is used in COMIS as input for
C           ! the fan curves. We use the mass flow conversion UnitFva. The fanflowra
C           ! that are calculated by the routine FEQN are massflowrates.
            CALL CnvUnit(OUnit(UnitFva),UnitStr(UnitFva),
     &         Conv(1,UnitFva),ofact(UnitFva))
C@empa aw 1995dec18 ofact(UnitFvFlag) Flag: volume or mass flow
            CALL CnvUnit(OUnit(UnitFva),UnitStr(UnitFva),
     &         Conv(1,UnitFvFlag),ofact(UnitFvFlag))
         elseif (nr .eq. UnitW) THEN
C           ! Air/wind velocity
            CALL CnvUnit(OUnit(UnitW),UnitStr(UnitW),
     &         Conv(1,UnitW),ofact(UnitW))
         elseif (nr .eq. UnitRate) THEN
C           ! Air change rate
            CALL CnvUnit(OUnit(UnitRate),UnitStr(UnitRate),
     &         Conv(1,UnitRate),ofact(UnitRate))
         elseif (nr .eq. UnitAge) THEN
C           ! Mean age of the air
            CALL CnvUnit(OUnit(UnitAge),UnitStr(UnitAge),
     &         Conv(1,UnitAge),ofact(UnitAge))
         elseif (nr .eq. UnitE) THEN
C           ! Energy
            CALL CnvUnit(OUnit(UnitE),UnitStr(UnitE),
     &         Conv(1,UnitE),ofact(UnitE))
         elseif (nr .eq. UnitProf) THEN
C           ! check the word given for the windprofile unit
C           ! wind profile ALPHA or Z0
            call UpperC(word)
            call error2('At output units you defined WindProfile '//
     &         word(1:lenstr(word))//
     &         '. This can only be an input unit.',
     &         'The unit has been assigned to input now.'//
     &         ' Move it to INPUT in your *.CIF file.',1)
C@tno jcp 1996May10_11:50:03 assignment to Ounit(unitprof) is in if below
CC          Iunit(unitProf)=Ounit(unitProf)
C@tno jcp 1996May10_11:46:53 if allows profile=Zo
            if (word.EQ.'ZO')  Word='Z0'
            if ( (word.NE.'ALPHA').AND.(word.NE.'Z0').AND.
     &         (word.NE.'ZO') ) then
               call inerr('The unit given for the windprofile is: '//
     &            Iunit(unitProf),
     &            'This should be alpha or Z0.',.TRUE.,2)
C@tno jcp 1996May10_11:47:54 else part moves Uppercase in iUnit(unitProf)
            else
               Iunit(unitProf)=word
            end if
         endif
      endif
C     ! This end if belongs to if (IOUNit.NE.1) then
C-----

C     ! See if we are at the end of the data yet
77    Call readlin(line, LinTyp, K, .True.)
      l=lenstr(line)
      if (FlgEnd) goto 88
C     ! is this a next UserUnit data line
      if (LinTyp .eq. TDATA) goto 110
88    continue


C-----------------
C     Debug output  (optional)
C-----------------

C     ! echo all units after parsing the &-PR-UNITS section
      if ((test.ge.1) .and. (iecho.ge.5)) THEN
         WRITE(CRT,*) 'User units after the keyword &-PR-UNITS'
         WRITE(CRT,*) 'Item                     Input       Output '
         WRITE(CRT,*) 'airleak                = ',
     &      Iunit(UnitCm)(1:10),Ounit(UnitCm)(1:10)
         WRITE(CRT,*) 'airflow                = ',
     &      Iunit(UnitFma)(1:10),Ounit(UnitFma)(1:10)
         WRITE(CRT,*) 'pressure               = ',
     &      Iunit(UnitP)(1:10),Ounit(UnitP)(1:10)
         WRITE(CRT,*) 'temperature            = ',
     &      Iunit(UnitTmul)(1:10),Ounit(UnitTmul)(1:10)
         WRITE(CRT,*) 'moisture content       = ',
     &      Iunit(UnitXh)(1:10),Ounit(UnitXh)(1:10)
C@NBI PGS 2000Oct08 - Now we can define different I/O units for each pollutant,
         DO iPol=1,MaxC
            WRITE(CRT,*) 'pollutant',iPol,'source       = '
     &         ,Iunit(UnitPolSo+iPol)(1:10)
     &         ,Ounit(UnitPolSo+iPol)(1:10)
            WRITE(CRT,*) 'pollutant',iPol,'sink         = '
     &         ,Iunit(UnitPolSi+iPol)(1:10)
     &         ,Ounit(UnitPolSi+iPol)(1:10)
            WRITE(CRT,*) 'pollutant',iPol,'concentration= '
     &         ,Iunit(UnitPolCo+iPol)(1:10)
     &         ,Ounit(UnitPolCo+iPol)(1:10)
         ENDDO
         WRITE(CRT,*) 'fan flowrate           = ',
     &      Iunit(UnitFva)(1:10),Ounit(UnitFva)(1:10)
         WRITE(CRT,*) 'wind velocity          = ',
     &      Iunit(UnitW)(1:10),Ounit(UnitW)(1:10)
         WRITE(CRT,*) 'air change rate        = ',
     &      '(n/a)     ',Ounit(UnitRate)(1:10)
         WRITE(CRT,*) 'mean age of air        = ',
     &      '(n/a)     ',Ounit(UnitAge)(1:10)
         WRITE(CRT,*) 'energy                 = ',
     &      '(n/a)     ',Ounit(UnitE)(1:10)
C        ! wind profile selector
         WRITE(CRT,*) 'windprofile            = ',
     &      Iunit(UnitProf)(1:10),'(n/a)     '
      endif

      RETURN
      END
C     ! end of inUNITS


Ch**********************************************************************
      SUBROUTINE inWaMa(Line, LinTyp, L, K)
C***********************************************************************
C input routine for Wall Materials
C stores up to MaxWE (Maximum Wall Elements) per pollutant in WDat
C WDat means Wall Data.
C Names of materials are registered in WName and the direct index into
C WDat per name is kept in WNind.
C labels
C 1      reads the data line after the *Wall-material-name
C 100    reads the data line with data of the wall
C 200    reads data of adsorbers etc. One line per pollutant
C
C A counter NConCWallTy holds the minimum number of adsorption data lines
C found under any WallType. This number is later checked against NConc.
C (NConcWallTy>=NConc)
C WDat contains a string of data elements: first all Wall materials followed
C      by all Wall Types.
C The WDAT WAll Material data elements are:
C
C  5 coef material1
C      rhoW,   density of the wall
C      porW,   porosity factor 0..1
C      porS,   pore surface per m3 pore volume. r_pore=0.05 mm-> 30000 here
C      perW,   permeability 1.E-4...1E-20 m2
C      RoughW, macroscopic roughness: ratio rough but smooth surface/flat
C      RoughS  microscopic roughness: ratio rough surface/smooth surface
C              * The smooth wall area is the area*RoughW used for area of the
C                diffusion boundary layer.
C              * The rough wall area is the area of the surface layer and
C                determines how much of a pollutant can be adsorbed at the
C                monomolecular surface layer.
C
C  9 coef pollutant1
C      ads,    Fraction of volume adsorber in the volume of the bulk material
C      RhoAds, density of the absorber
C      Mads,   Molmass of adsorber
C      Dads,   Diffusion coef
C      SurE,   Surface energy or binding energy
C      adsorptn, fraction for adsorption
C      desorptn, fraction for desorption
C      source,  source strength in this layer
C      Cinit    initial concentration in the absorber
C             (!!!do we need initial surface concentrations, pore
C              concentrations?)
C
C  9 coef pollutant2 etc
C
C  6 coef material2
C  9 coef pollutant1
C  9 coef pollutant2
C
C  etc
C--------- first wall type
C  2 coef
C      N,               N    = number of layers
C      Area
C  N*2 sets of coef
C      ind,             Ind  = index to material first element in Wdat
C      Width            Width= width of the layer (m)
C
C--------- next wall type
C  N Area ind W ind W ind W
C  etc
C  unit conversions have not been done here
C
Ch**********************************************************************

      IMPLICIT NONE
	INCLUDE 'comv-inp.inc'
	INCLUDE 'comv-uni.inc'
        INTEGER LenStr
	CHARACTER*(*) line
        character*40 word
        INTEGER k,l,LinTyp,MinNConc
C Counter with the current number of data lines for absorption under
C a WallMaterialName
        INTEGER NCdata

C@tno jcp 1996Jul09_17:28:06 variab added to inWaMa
        variab='reading Wall Materials'

C NConc is the number of zone pollutants under &-NET-ZP
        MInNConc=NConc
C NConcPolDes is the number of pollutants under &-POL-DEScription
        if (NConcPolDes .gt. MinNConc) then
          MinNConc=NConcPolDes
        end if
C Now I hope MinNConc has a value

C@tno jcp 1996May31_16:44:25 outputoptions(2)>0 instead of =1
CC        If ((outputoptions(2).EQ.1) .and. (MinNConc .eq. 0)) THEN
        If ((outputoptions(2).gt.0) .and. (MinNConc .eq. 0)) THEN
         call INERR('&-WAL-MAterial. Nconc=0 &-NET-ZP and /or '//
     &   ' &-POL-DEScription must come first.',line(1:l),
     &   .false.,1)
          write(cof,*) ' Reorder the sequence of the input file. '
          write (cof,*) ' '
         end if

        NConcWaTy=0

	k=0
C this line should start with a *WallMaterialName
10      IF (LinTyp .ne. TName) goto 99
        variab='WallMaterialName'
        if ((test.ge.1) .and. (iecho.ge.5)) THEN
          write(cof,*) ' inWaMa',line
          write(cof,*) ' ptrW=',ptrW
        end if
        NCdata=0
        CALL GETWRD(Line,K,L,WORD)
        Word=Word(2:)
        if (lenstr(Word).gt.20) then
         call INERR('&-WAL-MAterial name of material '//word//
     &   ' exceeds 20 characters. Now truncated.',line,.true.,1)
         word=word(1:20)
        end if
C fill the string array WName
        ptrWN=ptrWN+1
        WName(ptrWN)=Word
        if ((test.ge.1) .and. (iecho.ge.5)) THEN
          write(cof,*) 'inWaMa WName(',PtrWN,')=',word
        end if
C put the index pointer to WDat in WNind
        WNind(ptrWN)=ptrW

C Label 1: Read the next data line
        Call readlin(line, LinTyp, K, .True.)
        l=lenstr(Line)

C FlgEnd should not be TRUE now (dataline is missing in that case)
C  ________________________________________________________
C | rhoW  | porW   | porS   | perW  | RoughW  | RoughS    |
C |density|porosity|Surface |permea-| wall    |microscopic|
C |       |        | m2/m3  |bility |roughness|roughness  |
C | kg/m3 | factor |        |  m2   | factor  |  factor   |
C |_______|________|________|_______|_________|___________|

	if (FlgEnd) goto 99

	Variab='WallMaterial: Rho Wall'
        CALL GETLDat(Line,k,l,WDat(ptrW),ptrW,1000.0)

	Variab='WallMaterial: Porosity factor'
        CALL GETLDat(Line,k,l,WDat(ptrW),ptrW,0.5)

	Variab='WallMaterial: Pore Surface m2/m3pore'
C this default pore surface per m3pore volume is right for pores of d=0.2 mm
        CALL GETLDat(Line,k,l,WDat(ptrW),ptrW,30000.0)

	Variab='WallMaterial: Permeability m2'
        CALL GETLDat(Line,k,l,WDat(ptrW),ptrW,1.0E-10)

	Variab='WallMaterial: Wall Roughness'
        CALL GETLDat(Line,k,l,WDat(ptrW),ptrW,0.05)

	Variab='WallMaterial: Microsc. Roughness factor'
        CALL GETLDat(Line,k,l,WDat(ptrW),ptrW,2.0)

C LABEL 100
        Call readlin(line, LinTyp, K, .True.)
        l=lenstr(line)

C See if we are at the end of the data yet
	if (FlgEnd) goto 88

C is this a next WallMaterial Name then goto 10
	if (LinTyp .eq. TNAME) goto 10

C is this a next Absorption data line under the same
C WallMaterial Name ? then read the next data
C  _______________________________________________________________________
C |ads   |RhoAds | Mads  | Dads  |SurE   |adsorptn|desorptn|source|Cinit  |
C |fract |density|molmass|diffusn|surface| binding|emission|      |       |
C |      | kg/m3 |kg/kmol|  m2/s |energy |fraction|fraction| kg/s | kg/kg |
C |______|_______|_______|_______|_______|________|________|______|_______|
30	if (LinTyp .eq. TDATA) then
          NCdata=NCdata+1
          if (NCData .le. NConc) then
C store these coefficients. The data is for one of the defined pollutants.
C id NCData > NConc the absorption data will not be usable as NConc, the number
C of pollutants, taken into account in this simulation is smaller.
	    Variab='WallMaterial: Adsorber fraction'
            CALL GETLDat(Line,k,l,WDat(ptrW),ptrW,0.2)

	    Variab='WallMaterial: Density Adsorber'
            CALL GETLDat(Line,k,l,WDat(ptrW),ptrW,900.0)

	    Variab='WallMaterial: Molar mass of the adsorber'
            CALL GETLDat(Line,k,l,WDat(ptrW),ptrW,50.0)

	    Variab='WallMat.: Diffusion,poll. in adsorber'
            CALL GETLDat(Line,k,l,WDat(ptrW),ptrW,1.0E-5)

	    Variab='WallMaterial: Surface energy'
            CALL GETLDat(Line,k,l,WDat(ptrW),ptrW,1.0)

	    Variab='WallMat.: Adsorption binding fraction'
            CALL GETLDat(Line,k,l,WDat(ptrW),ptrW,0.2)

            Variab='WallMat.: Desorption emission fraction'
            CALL GETLDat(Line,k,l,WDat(ptrW),ptrW,0.05)

            Variab='WallMaterial: Source '
            CALL GETLDat(Line,k,l,WDat(ptrW),ptrW,0.0)

            Variab='WallMaterial: Initial concentration '
            CALL GETLDat(Line,k,l,WDat(ptrW),ptrW,0.0)

            end if
C end if of (NCdata .le. NConc)
        end if
C end if of TData

C LABEL 200
	Call readlin(line, LinTyp, K, .True.)
        l=lenstr(line)
C See if we are at the end of the data yet
	if (FlgEnd) goto 88

C is this a next WallMaterial Name then goto 10
	if (LinTyp .eq. TNAME) goto 10

C is this a next Absorption data line under the same
C WallMaterial Name ? then 30goto 30
	if (LinTyp .eq. TDATA) goto 30

88	continue

99	continue
        if ((NConcWaTy.eq.0) .and. (NCData.gt.0)) THEN
          NConcWaTy=NCData
        end if
        if (NCdata .lt. NConcWaTy) THEN
C keep updating the minimum number of adsorption data lines under &-WAL-TYPe
          NConcWaTy=NCData
        end if

	RETURN
	END


Ch**********************************************************************
	SUBROUTINE inWaTy(Line, LinTyp, L, K)
C***********************************************************************
C input routine for Wall Types
C stores data in WDat
C Names of Types are registered in WName and the direct index into
C WDat per name is kept in WNind.
C Wdat elements per WallType are :
C     Number of layers of this type
C     Area of this type
C     index of material name in WName(during reading) index to WDat (afterwards)
C     width of the layer
C     (repetition of the last two, index, Width)
C see for the actual configuration of Wdat data the routine inWaMa in this file
Ch**********************************************************************

        IMPLICIT NONE
	INCLUDE 'comv-inp.inc'
	INCLUDE 'comv-uni.inc'
        INTEGER LenStr
	CHARACTER*(*) line
        character*40 word
        INTEGER k,k1,l,LinTyp
C Counter with the number of WallLayers in this wallType
        INTEGER NLdata

C@tno jcp 1996Jul09_17:28:06 variab added to inWaTy
        variab='reading Wall Types'


	k=0
C this line should start with a *WallTYpeName
10      IF (LinTyp .ne. TName) goto 99

C save the current position in WDat, later store NLdata (number of layers) there
        ptrW0=ptrW
C reserve space for the number of layers used in this Type
        ptrW=ptrW+1

        NLdata=0
        if ((test.ge.1) .and. (iecho.ge.5)) THEN
          write(cof,*) Line
        end if
C read the wall type name
        variab='reading Wall Type Name'
        CALL GETWRD(Line,K,L,WORD)
C remove the leading * from the walltype name
        Word=Word(2:)

        if ((test.ge.1) .and. (iecho.ge.5)) THEN
          write(cof,*) ' Walltype',word,' in WDat op ptr=',ptrW0
        end if
        if (lenstr(word).gt.20) then
         call INERR('&-WAL-TYpe name of Type '//word//
     &   ' exceeds 20 characters. Now truncated.',line,.true.,1)
         word=word(1:20)
        end if
        CALL FINDWaMa(word,k1)
        if (k1 .gt. 0) then
         call INERR('&-WAL-TYpe name of Type name '//word//
     &   ' is already used as Material name',line,.true.,2)
        end if

C fill the string array WName
        ptrWN=ptrWN+1
        WName(ptrWN)=Word
C put the index pointer to WDat in WNind
        WNind(ptrWN)=ptrW0
C  ______________
C | ID     | A   |
C |        |area |
C |        | m2  |
C |________|_____|

C read the area of this layer
C@NBI PGS 1999May13 - 'Variab' statement was missing so added
	Variab='WallType: Wall area'
        CALL GETLDat(Line,k,l,WDat(ptrW),ptrW,1.0)
        if ((test.ge.1) .and. (iecho.ge.5)) THEN
          write(cof,*) ' Area ',Wdat(ptrW-1)
        end if


C Read the next data line
        Call readlin(line, LinTyp, K, .True.)
        l=lenstr(line)
        if ((test.ge.1) .and. (iecho.ge.5)) THEN
          write(cof,*) Line
        end if
20      CONTINUE
C FlgEnd should not be TRUE now (dataline is missing in that case)
	if (FlgEnd) goto 99

        NLdata=NLdata+1
C  ___________________________________
C | used          | w   |Source| Cinit|
C | WALL-MAterial |layer|ratio | ratio|
C |               |width|      |      |
C |_______________|_____|______|______|
C store these coefficients.

	Variab='WallType: uses Wall Material'
        CALL GETWRD(Line,k,l,WORD)
        CALL FINDWaMa(WORD,k1)
C the returned k1 points directly into WDat(*)
        if ((test.ge.1) .and. (iecho.ge.5)) THEN
          write(cof,*) ' WallMaterial ',word
        end if
        if (k1 .eq. 0) then
         call INERR('Referred WallMaterial '//word//
     &   ' not found.',line,.true.,3)
        end if
        WDat(ptrW)=k1
        ptrW=ptrW+1

	Variab='WallType: Layer width'

        CALL GETLDat(Line,k,l,WDat(ptrW),ptrW,0.01)
        if ((test.ge.1) .and. (iecho.ge.5)) THEN
          write(cof,*) ' Layer width ',Wdat(ptrW-1)
        end if

	Variab='WallType: Source factor'

        CALL GETLDat(Line,k,l,WDat(ptrW),ptrW,1.0)
        if ((test.ge.1) .and. (iecho.ge.5)) THEN
          write(cof,*) ' Source factor ',Wdat(ptrW-1)
        end if

	Variab='WallType: Initial Conc factor'

        CALL GETLDat(Line,k,l,WDat(ptrW),ptrW,1.0)
        if ((test.ge.1) .and. (iecho.ge.5)) THEN
          write(cof,*) ' Initial Conc factor ',Wdat(ptrW-1)
        end if

        Call readlin(line, LinTyp, K, .True.)
        l=lenstr(line)
        if ((test.ge.1) .and. (iecho.ge.5)) THEN
          write(cof,*) Line
        end if
C See if we are at the end of the data yet
	if (FlgEnd) goto 88

C is this a next WallType Name then goto 10
	if (LinTyp .eq. TNAME) then
C register the number of WallLayers for the type read before
        if ((test.ge.1) .and. (iecho.ge.5)) THEN
          write(cof,*) ' This line is a Type name and the previous',
     &    NLdata
          write(cof,*) ' number of layers is placed in WDat(',ptrW0,')'
        end if

          WDat(ptrW0)=NLdata
          goto 10
        end if

C is this a next Absorption data line under the same
C WallType Name ? then goto 20
	if (LinTyp .eq. TDATA) goto 20


88	continue
C register the number of WallLayers for the type read before
        WDat(ptrW0)=NLdata
        if ((test.ge.1) .and. (iecho.ge.5)) THEN
          write(cof,*) 'this line doesnot belong to WAL-TYPes',
     &    NLdata,' number of layers is placed in WDat(',ptrW0,')'
        end if

99	continue
	RETURN
	END

