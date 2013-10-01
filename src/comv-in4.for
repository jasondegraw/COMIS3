C+*********************************************************** comv-in4.f

Ch*********************************************************************
	SUBROUTINE DSPREC(Dat)
C**********************************************************************
C
C Purpose: Calculates the missing parameters which the Walton dwc model
C	   needs from the given ones.
C
C Module:  empa aw 1991July05
C
C@empa aw 2001sep07 ZETA of the fitting stored separatly (not added to the DS-ZETA anymore)
C@empa aw 2001sep07 Calculate ZETA of the fitting for both flow directions  
C@empa aw 2001sep07 Explicit input of ZETA of the DS  for each flow direction
C
C Pass parameters:
C
C IO # Name    unit name  description
C		    (Walton)
C
C IO   DAT(*)  multi	  data about the duct
C      Dat(1)=3 -	  means this is a staight duct
C I    Dat(2)	m	  Diam1
C I    Dat(3)	m	  Diam2 (round duct if only Diam1 is given)
C I    Dat(4)	mm   e	  roughness dimension
C I    Dat(5)	m    L	  Lduct length of the duct
C IO   Dat(6)	-    C	  Zeta of the DS and the DF given in Dat(7)
C I    Dat(7)	-	  Type duct fitting
C I    Dat(8)	-	  Parameter 1 of the fitting
C I    Dat(9)	-	  Parameter 2 of the fitting
C O    Dat(10)	m    D	  hydraulic diameter
C O    Dat(11)	m2   A	  cross sectional area
C O    Dat(12)	-    k	  laminar friction loss coefficient
C O    Dat(13)	-    K	  laminar dynamic loss coefficient
C O    Dat(14)	-	  laminar initialization coefficient
C O    Dat(15)	-    L/D  relative length,
C O    Dat(16)	-    a	  1.14 - 0.868589*ln(e/1000/D)
C@empa aw 2001sep07
C I    Dat(17) -    ZETA of the duct for positive flow (FROM to TO)
C I    Dat(18) -    ZETA of the duct for negative flow (TO to FROM)
C O    Dat(19) -    ZETA of the fitting for positive flow (FROM to TO)
C O    Dat(20) -    ZETA of the fitting for negative flow (TO to FROM)
C
Ch**********************************************************************

      IMPLICIT NONE
         REAL     Dat(*),ZETA,ZETA2,PI
         INTEGER  KEY
         REAL FNPI

C    dynamic loss coefficient ZETA
	ZETA=0
	CALL WHICHDF(Dat(7),ZETA,ZETA2,KEY)
	Dat(6)=Dat(6)+ZETA
C@empa aw 2001sep07
      Dat(19)= ZETA
      Dat(20)=ZETA2

C
C    hydraulic diameter D
	IF (Dat(3).NE.0.0) THEN
	    Dat(10)=((Dat(2)*Dat(3))**0.625/(Dat(2)+Dat(3))**0.25)*1.3
	ELSE
	    Dat(10)=Dat(2)
	ENDIF
C
C    cross sectional area A
	IF (Dat(3).NE.0.0) THEN
	    Dat(11)=Dat(2)*Dat(3)
	ELSE
	    PI=FNPI( )
	    Dat(11)=Dat(2)*Dat(2)*PI/4
	ENDIF
C    As we do not have an input for the laminar friction loss, dynamic loss
C    and initialization coefficient I give here default values which I think
C    are usefull.
C    laminar friction loss coefficient k
	Dat(12)= 64.0
C
C    laminar dynamic loss coefficient K
	Dat(13)= 0.0

C    laminar initialization coefficient
	Dat(14)= 128.0
C
C    relative length L/D
	Dat(15)=Dat(5)/Dat(10)
C
C    a=1.14 -0.868589*ln(e/D)
	Dat(16)=1.14-0.868589*LOG(Dat(4)/1000/Dat(10))
	RETURN
	END


C@NBI PGS 2000Jul20 - This subroutine was no longer used, so commented out
CCCh***********************************************************************
CC        SUBROUTINE CompTime(Jday1,NSec,NsecSim,NsecEvent,result)
CCC                           Ii    Ii   Ii      Oi        Oi
CCC                           |     |    |       |         |
CCC                           |     |    |       |        -3..+2
CCC                           |     |    | NsecSim for Jday1 plus Nsec, always the
CCC                           |     |    | current/next event (never back in time)
CCC                           |     |    'current' time within Comis timestep
CCC                           |     seconds from 00:00 on Jday1 (from schedule
CCC                           |                                  or (meteo) file)
CCC                           Julian Day number (from schedule or (meteo) file)
CC
CCC inTime is a new routine, which comes from the hart of routine dupltm.
CCC it compares Jday1 plus NSec (time to investigate from file or schedule) and
CCC             NSecTim         (current Comis simulated time)
CCC
CCC Result:
CCC   -3   T1<start time          (as given in &-PR-SIMU)
CCC   -2   T1<schedule start time (as given in &-PR-SIMU)
CCC   -1   T1<Nsectim
CCC    0   T1=Nsectim
CCC   +1   T1>Nsectim
CCC   +2   T1>stop time           (as given in &-PR-SIMU)
CCC  T1 may be
CCC           full date_time
CCC          | 2-digit year (assuming 1900)
CCC          | | month-
CCC          | | |  weekday/weekend-, day-, time-only
CCC          | | |  day-, time-only
CCC          | | |  |  time-only
CCC          | | |  |  |
CCC          | | |  |  10:      hour only
CCC          | | |  |  10:30    hour and minutes
CCC          | | |  |  10:30:59 full time
CCC          | | | MON_10:30:59 day of the week
CCC          | | | ...
CCC          | | | SUN_10:30:59 ,,
CCC          | | | WDY_10:30:59 week day MON..FRI
CCC          | | | WND_10:30:59 weekend  SAT..SUN
CCC          | | May21_10:30:59
CCC          | 96May21_10:30:59
CCC          1996May21_10:30:59
CCC
CCC The month may be a 2-digit number too (interpreted the same way)
CCC    19960521_10:30:59
CCC
CCC The Time may be a without ':' colons (interpreted the same way)
CCC    19960521_103059
CC
CCC overview of tricks to handle time, weekday, monthday
CCC                           Comis uses
CCC        input              julian day         year           valid for
CCC      time (no date)          -1              -4714          every day
CCC    MON..SUN(_time)          0..6         -4714..-4713       every week
CCC    WDY..WND(_time)          7..8            -4713           every week
CCC    MonthDay(_time)    1721424..1721788        1             every year
CCC    YYYYMMDD_hh:mm:ss         ..                        exactly that date_time
CC
CC
CCCh***********************************************************************
CCC
CC      IMPLICIT NONE
CC        INCLUDE 'comv-uni.inc'
CC	INCLUDE 'comv-inp.inc'
CC        INTEGER Jday1,NSec,NsecSim,NsecEvent,result
CC        INTEGER JdaySecSim
CC
CCC calculate Nsecsim1 the Nsec since the start of the simulation of the
CCC investigated moment Jday1 plus Nsec
CC        Nsecevent=JdaySecSim(Jday1,Nsec,NsecSim)
CCC variables SecSim.. are asssigned in RdPeriod
CCC   -3   T1<start time          (as given in &-PR-SIMU, SecSim=0)
CCC   -2   T1<schedule start time (as given in &-PR-SIMU, SecSim3 )
CCC   -1   T1<NsecSim
CCC    0   T1=NsecSim
CCC   +1   T1>NsecSim
CCC   +2   T1>stop time           (as given in &-PR-SIMU, SecSim2 )
CC        if (NsecEvent.lt.0) then
CC          result=-3
CC        elseif (NsecEvent.lt.Nsecsim3) then
CC          result=-2
CC        elseif (NsecEvent.lt.Nsecsim) then
CC          result=-1
CC        elseif (NsecEvent.gt.Nsecsim) then
CC          result=+1
CC        elseif (NsecEvent.gt.Nsecsim2) then
CC          result=+2
CC        else
CC          result=0
CC        end if
CC
CC        RETURN
CC        END
CCc end CompTime


C@tno jcp 1996Mar14_12:18:47 new routine
Ch***********************************************************************
C explain keywords
C************************************************************************
        SUBROUTINE explain(keyword, exstr)
C************************************************************************
C explain Explains a keyword
C the keyword is in the 10 char string 'keyword'
C explanation is put in a 60 char string 'exstr'
C example
C keyword=NET-AIR
C Explanation= defines the network of links
C************************************************************************

      IMPLICIT NONE
        INTEGER LenStr
        CHARACTER*(*) keyword,exstr
        Character*40 str
        exstr=' '

        if (index(Keyword,'NET-').EQ.1) then
          str=' Network:'
          if (index(Keyword,'-WAL').GT.1) then
          exstr=str(1:lenstr(str))//' Walls for the pollutant model'
          else if (index(Keyword,'-HVA').GT.1) then
          exstr=str(1:lenstr(str))//' HVAC elements'
          else if (index(Keyword,'-AIR').GT.1) then
          exstr=str(1:lenstr(str))//' Air Flow components to be used'
          else if (index(Keyword,'-ZON').GT.1) then
          exstr=str(1:lenstr(str))//' Zones'
          else if (index(Keyword,'-ZL').GT.1) then
          exstr=str(1:lenstr(str))//' Layers per zone'
          else if (index(Keyword,'-ZP').GT.1) then
          exstr=str(1:lenstr(str))//' Pollutants per zone'
          else if (index(Keyword,'-ZT').GT.1) then
          exstr=str(1:lenstr(str))//' Thermal properties per zone'
          else if (index(Keyword,'-EXT').GT.1) then
          exstr=str(1:lenstr(str))//' the External nodes (wind etc)'
          else if (index(Keyword,'-LIN').GT.1) then
          exstr=str(1:lenstr(str))//' the actual Links of air flow '//
     &     'components'
          endif
        endif
        if (index(Keyword,'PR').EQ.1) then
          exstr=' Program options'
        endif
        if (index(Keyword,'PR').EQ.1) then
          exstr=' Program options'
        endif
        if (index(Keyword,'SCH').EQ.1) then
         str=' Schedule '
         if (index(Keyword,'-MAI').GT.1) then
         exstr=str(1:lenstr(str))//' Main (not operational yet)'
         else if (index(Keyword,'-LIN').GT.1) then
         exstr=str(1:lenstr(str))//' Links'
         else if (index(Keyword,'-WIN').GT.1) then
         exstr=str(1:lenstr(str))//' Windows, large openings (links)'
         else if (index(Keyword,'-FAN').GT.1) then
         exstr=str(1:lenstr(str))//' Fans'
         else if (index(Keyword,'-TEM').GT.1) then
         exstr=str(1:lenstr(str))//' the temperature of zones'
         else if (index(Keyword,'-HUM').GT.1) then
         exstr=str(1:lenstr(str))//' the humidity of zones'
         else if (index(Keyword,'-SIN').GT.1) then
         exstr=str(1:lenstr(str))//' Sink of polluted air in zones'
         else if (index(Keyword,'-SOU').GT.1) then
         exstr=str(1:lenstr(str))//' pollutant Sources per zone'
         else if (index(Keyword,'-OCC').GT.1) then
         exstr=str(1:lenstr(str))//' Occupants'
         else if (index(Keyword,'-MET').GT.1) then
         exstr=str(1:lenstr(str))//' Meteo values wind, temperature etc'
         else if (index(Keyword,'-POL').GT.1) then
         exstr=str(1:lenstr(str))//' (removed function for pollutant)'
         else if (index(Keyword,'-MUL').GT.1) then
        exstr=str(1:lenstr(str))//' Multi, schedules more than one item'
         endif
        endif
        if (index(Keyword,'WAL').EQ.1) then
          exstr=' Wall type or material'
        endif
        if (index(Keyword,'CP').EQ.1) then
          exstr=' Wind pressure coefficients'
        endif
        if (index(Keyword,'ENV').EQ.1) then
          exstr=' Environment of meteo and building'
        endif
        if (index(Keyword,'POL').EQ.1) then
          exstr=' Pollutants for the concentration model'
        endif
        if (index(Keyword,'CPR').EQ.1) then
          exstr=' Geometry for an external Cp generator'
        endif
        if (index(Keyword,'USER').EQ.1) then
          exstr=' User defined input data'
        endif
        if (index(Keyword,'CIF').EQ.1) then
          exstr=' obsolete'
        endif
        if (index(Keyword,'TRANSIT').EQ.1) then
          exstr=' Re for Transiton between Laminar and Turbulent flow'
        endif
        if (index(Keyword,'OCCUPAN').EQ.1) then
          exstr=' occupant data'
        endif
        if (index(Keyword,'3D-BUIL').EQ.1) then
          exstr=' shape of the buiding for an external Cp gererator'
        endif
        if (index(Keyword,'NORM-CR').EQ.1) then
          exstr=' standard conditions used for CRack data'
        endif
        if (index(Keyword,'HISTO').EQ.1) then
          exstr=' size and ranges for histograms'
        endif
        if (index(Keyword,'ECHOSCH').EQ.1) then
          exstr=' Echo all changes from Schedules at run time'
        endif
        if (index(Keyword,'EQN-WIN').EQ.1) then
          exstr=' Equation(meteo) for a window schedule''s factor'
        endif

        if (lenstr(Keyword).EQ.2) then
          if (index(Keyword,'CR').EQ.1) then
            exstr=' Crack for use in a link'
          end if
          if (index(Keyword,'FA').EQ.1) then
            exstr=' FAn for use in a link'
          end if
          if (index(Keyword,'DS').EQ.1) then
            exstr=' Duct_Straight (incl. a Fitting) for use in a link'
          end if
          if (index(Keyword,'DF').EQ.1) then
            exstr=' Duct_Fitting not used separately'
          end if
          if (index(Keyword,'F1').EQ.1) then
            exstr=' Flowcontroller 1 for use in a link'
          end if
          if (index(Keyword,'F2').EQ.1) then
            exstr=' Flowcontroller 2 for use in a link'
          end if
          if (index(Keyword,'F3').EQ.1) then
            exstr=' Flowcontroller 3 for use in a link'
          end if
          if (index(Keyword,'F4').EQ.1) then
            exstr=' Flowcontroller 4 for use in a link'
          end if
          if (index(Keyword,'WI').EQ.1) then
            exstr=' WIndow (Door or Large opening) for use in a link'
          end if
          if (index(Keyword,'TD').EQ.1) then
            exstr=' TestData (measured range of (dP,Flow) '//
     &     'for use in a link'
          end if
          if (index(Keyword,'PE').EQ.1) then
            exstr=' Powerlaw Element for use in a link'
          end if
          if (index(Keyword,'RF').EQ.1) then
            exstr=' Related Flow element, flow as a function of'//
     &    ' another link'
          end if
C@tno jcp 1996Jun05_16:10:17 PS added
          if (index(Keyword,'PS').EQ.1) then
            exstr=' Passive Stack Flow element, mount from inside'//
     &    ' to outside'
          end if
C@NBI PGS 2003Apr28 - New component type: Thermostatic vent
          if (index(Keyword,'TV').EQ.1) then
            exstr=' Therostatic vent for use in a link'
          end if
        endif

        RETURN
        END

C************************************************************************
C new routine for the RF (Related Flow) component
Ch***********************************************************************
        SUBROUTINE inRF(Line, LinTyp, L, K)
C the RF routine is copied from the TD routine
Ch***********************************************************************

      IMPLICIT NONE
	INCLUDE 'comv-inp.inc'
	CHARACTER*(*) line
	character*160 nline
        INTEGER k,k1,l,LinTyp
C flag whether flows are massflow or volume flow
        INTEGER FLAG
        REAL R
        INTEGER LenStr

	k=0
C reserve space for the number of data pairs used for the RF
C Define ptrL0 as the element in Ldat where Npairs is stored
C (ptrL0 is defined in include file)
	ptrL0=ptrl
	ptrL=ptrl+1

           Variab='RF: Flag (1=massflow or 2=volumeflow)'
           CALL GETLDat(Line,k,l,LDat(ptrl),ptrl,0.0)
C   Save flag for converting the flows
	   FLAG = LDat(ptrl-1)

           Variab='RF: RhoI'
C@empa aw 2000nov21 default for RhoI: 1.2
CC           CALL GETLDat(Line,k,l,LDat(ptrl),ptrl,0.0)
           CALL GETLDat(Line,k,l,LDat(ptrl),ptrl,1.2)

	Npair=0
        ptrl0=ptrl-3

C lines with data pairs


        Call readlin(line, LinTyp, K, .True.)
C FlgEnd should not be TRUE now (2nd dataline is missing in that case)
	if (FlgEnd) goto 99

C If a name, then another RF follows
	if (LinTyp .eq. TNAME) goto 99
        if (LinTyp .ne. TDATA) goto 99

C Read ahead one line to see if we are past this RF data. If so, then Line
C contains the filter data and Nline the data for the next keyword.

C Loop through all datalines of this RF

100	Call readlin(nline, LinTyp, K1, .True.)
C See if we are at the end of the data yet
	if (FlgEnd) goto 88
	if (LinTyp .ne. TDATA) goto 200

        l=lenstr(line)

C Keep looping until we have all data from this line

10      continue
        Variab='RF: datapoints'
	CALL GETLDat(Line,k,l,LDat(ptrl),ptrl,0.0)
	Npair=Npair+1
C both columns are flows,
C first one is the independent (input) flowrate that may flow through the
C reference link.
C second one is the outout flow rate that will flow through this RF component
	if (mod(Npair,2).eq.0) then
C         Even position, RFflow (output)
	    if (flag .eq. 1) then
C             flag=1, volume flow
C convert from user units to COMIS kg/s
		LDat(ptrl-1) = LDat(ptrl-1)*ifact(UnitFva)
	    else
C	      flag=2, mass flow
		LDat(ptrl-1) = LDat(ptrl-1)*ifact(UnitFma)
	    endif
	else
C         Odd position, input flowrate (the flow through the reference link)
            if (flag .eq. 1) then
C	      flag=1, volume flow
		LDat(ptrl-1) = LDat(ptrl-1)*ifact(UnitFva)
	    else
C	      flag=2, mass flow
		LDat(ptrl-1) = LDat(ptrl-1)*ifact(UnitFma)
	    endif
        endif
C@lbl/end
	IF (K.LT.L) GOTO 10

C shift the Nline, just read at 100, into Line to process it
        line=Nline
        k=k1

        GOTO 100

200	continue
C Now Nline contains something else and therefore Line is the filter data

C check, calculate and store Npairs
C check for an even number of data

	R=Npair/2.
	IF (ABS(R-INT(R)) .LT. 0.1) THEN
C OK Npair is even
	  Npair=Npair/2
C store Npair at the place kept free
	  LDat(ptrl0)=Npair
	ELSE
               CALL INERR('&-RF: uneven number of data under '//
     &         'header two !',line,.true.,2)
	ENDIF

	CALL FILTER(Line,K,L)
	goto 88

88	continue
	k=k1
	line=Nline

99      continue
	RETURN
        END


C@tno jcp 1996Jun05_16:16:35 new routine to read PS Passive Stack
Ch***********************************************************************
        SUBROUTINE inPS(Line, LinTyp, L, K)
Ch***********************************************************************

      IMPLICIT NONE
	INCLUDE 'comv-inp.inc'
	CHARACTER*(*) line
        INTEGER k,l,LinTyp,LStr
        INTEGER LenStr

        INTEGER k2,l2
        CHARACTER FiltStr*160,str*20
C PS: Agrille; D duct, Lduct, Lambda duct, Zeta Cowl, Cp Cowl

        Variab='PS:Area grille'
	k=0
        CALL GETLDat(Line,k,l,LDat(ptrl),ptrl,0.01)

        Variab='PS:Diameter duct'
	k=0
        CALL GETLDat(Line,k,l,LDat(ptrl),ptrl,0.02)

        Variab='PS:Diameter 2 duct'
	k=0
        CALL GETLDat(Line,k,l,LDat(ptrl),ptrl,0.02)

        Variab='PS:Length duct'
	k=0
        CALL GETLDat(Line,k,l,LDat(ptrl),ptrl,5.0)

        Variab='PS:Friction duct'
	k=0
        CALL GETLDat(Line,k,l,LDat(ptrl),ptrl,0.05)

        Variab='PS:Zeta duct'
	k=0
        CALL GETLDat(Line,k,l,LDat(ptrl),ptrl,0.0)

        Variab='PS:Zeta cowl'
	k=0
        CALL GETLDat(Line,k,l,LDat(ptrl),ptrl,2.5)

        Variab='PS:extra Cp cowl'
	k=0
        CALL GETLDat(Line,k,l,LDat(ptrl),ptrl,-0.35)

	Call readlin(line, LinTyp, K, .True.)
	if (FlgEnd) goto 99
	if (LinTyp .ne. TDATA) goto 900
	l=lenstr(line)
	CALL FILTER(Line,K,L)
C read next line for next keyword
	Call readlin(line, LinTyp, K, .True.)
	goto 99

900	continue
C@tno jcp 1996Apr27_17:13:24 why an error? put the defaults in?
        str=UsrAfc(nUsrAfc)
        Lstr=Lenstr(str)
        if (outputoptions(2).gt.0) then
          call inerr('CRack:'//str(1:Lstr)//
     &     ' has  no filter data',
     &     'Default no filtering, data has been set to 0.0 ',
     &     .false.,1)
        end if
        k2=1
        CALL RptStr(k2,MaxC,'0.0 ',FiltStr)
        L2=MaxC*4
        k2=1
	CALL FILTER(FiltStr,K2,L2)

	goto 99

99	continue
	RETURN
	END



Ch***********************************************************************

        SUBROUTINE inHISTO(Line, LinTyp, L, K)
C@empa aw 2000feb08 Input parameter used for fictive sources canceled 
C@empa              (now in &-POL-FIC)
C input routine for HISTOGRAMs
C here follows a part of the COMIS.CIF input file

C The actual histograms are 'opted' for at PR-SIMU and read in the program
C by the routines IniGrout,ReadOpt,ReadNum

C&-HISTO defines ranges for histograms
C# histograms are called in by:
C# C1-H1 zon1, zon2, occ1  <---concentration histogram
C# FZ-H4 zon1, zon2, occ1  <---Flowrate histogram
C# FE-H2 zon1, zon2, occ1  <---Effective flowrate histogram
C# C2-H1 zon1, zon3        <---concentration in a previously used histogram
C# effective flowrate = (Norm Source)/(running Concentration)
C# Norm Conc can be used to signal undesirable situations
C  __________________________________
C |Histo|Number |  Lower  |  Upper  |
C |gram |Classes|  Class  |  Class  | 
C |_____|_______|_________|_________|
C    1     100     0          10      
C    2     100     0          10      
C    3     100     0          10      
C the sequence of histograms mentioned (zon1,zon2,occ1) under &-PR-SIMU options
C defines the actual sequence of the histogram 'columns' in the big HistoAr(*)
C array
C The information in the &-HISTO section goes into HistoX(MHisTy,3) processed by
C Routine inHisto.
C The histogram names (in HistUsedN) types and start-positions in HistoAr(*) are
C by IniGrout, MkTab(part=5) and stored in HistUsed(*).
C Actual filling of the histograms is done by the routines  WriGrout, HistCalc.
C Output is make by routine HistOut, like MeanOut in *.COF.

Ch***********************************************************************

      IMPLICIT NONE
	INCLUDE 'comv-inp.inc'
	INCLUDE 'comv-uni.inc'
	CHARACTER*(*) line
        INTEGER k,l,LinTyp
        INTEGER LenStr
C local
C@NBI PGS 2000Aug02 - "number" no longer used so removed from decralations
        INTEGER ihisto,Nclass
        REAL Lower,Upper,Width
C Nhisto common counter for the 1..max20 histograms

C@tno jcp 1996Jul09_17:28:06 variab added to inHisto
        variab='reading Histogram definition'

        NHisto=0
C this line should contain numbers (data)
10      IF (LinTyp .ne. TData) goto 99
        variab='Histogram definition'
        if ((test.ge.1) .and. (iecho.ge.5)) THEN
          write(cof,*) ' inHISTO',line
        end if

        Variab='Histogram definition: HistogramNumber'
        CALL GETWI(Line,k,l,ihisto,1)
C@empa aw 1999dec07 If the integer was invalid, then ihisto is zero. 
C@empa              Check this! An error message is allready given in GetWI
       if (ihisto.ne.0) then  
        if (Histox(ihisto,1).ne.0) then
          CALL INERR('&-HISTO: double defined histograms '//
     &    'repair the histogram numbers(1..max20) !',line,.true.,2)
        end if

C keep track of the maximum number defined (NHisto can be used at output)
        if (iHisto.gt.Nhisto) Nhisto=iHisto

        Variab='Histogram definition: Number of Classes'
        CALL GETWi(Line,k,l,Nclass,100)
        if ((Nclass.lt.2).or.(Nclass.gt.1000)) then
          CALL INERR('&-HISTO: number of classes is less than 2 or '//
     &    'larger than 1000 !',line,.true.,2)
        end if
        Histox(ihisto,1)=Nclass

        Variab='Histogram definition: Lower Class'
        CALL GETWR(Line,k,l,Lower,0.0,.TRUE.)

        Variab='Histogram definition: Higer Class'
        CALL GETWR(Line,k,l,Upper,100.0,.TRUE.)
C move from class center to lower class boundary
        width=(upper-lower)/(Nclass-1)
        HistoX(ihisto,2)=lower-width/2
        HistoX(ihisto,3)=upper-width/2

C@empa aw 2000feb08 moved to &-POL-FIC  
CC        Variab='Histogram definition: Roomsize dependency'
CC        CALL GETWI(Line,k,l,Number,0)
CC        Histox(ihisto,4)=Number

CC        Variab='Histogram definition: Occupant dependency'
CC        CALL GETWI(Line,k,l,Number,0)
CC        Histox(ihisto,5)=Number

C@empa aw 1999dec07 endif of non invalid histogramm numbers ihisto  
       endif
	Call readlin(line, LinTyp, K, .True.)
        l=lenstr(line)
C See if we are at the end of the data yet
        if (FlgEnd) goto 99

C is this a next Histogram then goto 10
        if (LinTyp .eq. TDATA) goto 10

99	continue

	RETURN
	END
Ch***********************************************************************

        SUBROUTINE inEqnWin(Line, LinTyp, L, K)
C input routine for equations to be used for window schedules
C here follows a part of the COMIS.CIF input file
C&-EQN-WINdow equation
C; this equation accepts variables var= Vmet Dmet Tmet
C; note that between the two expressions there is an operator opr= *, /, +, -
C; the result will be clipped between 0..1
C  _____________________________________________________________________________
C |Schedule|(C0+C1*var+C2*var^2) * (C0+C1*var+C2*var^2) * (C0+C1*var+C2*var^2) |
C |*Name   |                     |                      |                      |
C | (-)    | C0    C1   C2  var opr1 C0     C1   C2 var opr2 C0    C1   C2 var |
C |________|_____________________|______________________|______________________|
C*WIa        0.01 -0.001 0  Vmet *  0.002 0.0004  0 Tmet
C this leads to an equation:
C Eqn=(0.01-0.001*Vmet+0*Vmet^2)*(0.002+0.0004*Tmet+0*Tmet^2)+(0+0*zero+0*zero^2
C the terms between brackets are evaluated first
C the 3 terms are processed from left to right:
C eqn=(term1)opr1(term2)
C eqn=(eqn)opr2(term3)
C opening factor is Eqn*ActFact at &-NET-LIN

C a limited number of variables is defined (limited because each needs an if-sta
C variable number used  expanation
C  Vmet      1          the wind velocity as used in the meteo(file)
C  Dmet      2                   direction
C  Tmet      3                   temperature
C  ..        4
C  ..        5
C  Vbui      6          the wind velocity as calculates at building reference he
C  Pwind     7          Pwind is the wind pressure at the current window
C  Dp        8          pressure difference over the window
C
C the last 2 mean that we have to look at what window this schedule is used,
C then caluclate with it's Pwind or Dp.

C the 2 operators between the 3 terms may be:
C opr  number
C +    1
C -    2
C *    3
C /    4


Ch***********************************************************************

      IMPLICIT NONE
	INCLUDE 'comv-inp.inc'
	INCLUDE 'comv-uni.inc'
	CHARACTER*(*) line
        INTEGER k,l,LinTyp
        character word*40

        INTEGER LenStr

C@tno jcp 1996Jul09_17:28:06 variab added to inEqnWin
        variab='reading Equations for window opening'


C this line should contain data, starting with a (schedule) name
10      IF (LinTyp .ne. TName) goto 99
        EqnCount=EqnCount+1
        variab='Window Equation definition'
        if ((test.ge.1) .and. (iecho.ge.5)) THEN
          write(cof,*) ' inEqnWin',line
        end if
C reset coef , var (numbers to indentify the variables to be used)
C          and opr (numbers that identify the operator to be used)
        EqnCoef(EqnCount,1)=0
        EqnCoef(EqnCount,2)=0
        EqnCoef(EqnCount,3)=0
        EqnCoef(EqnCount,4)=0
        EqnCoef(EqnCount,5)=0
        EqnCoef(EqnCount,6)=0
        EqnCoef(EqnCount,7)=0
        EqnCoef(EqnCount,8)=0
        EqnCoef(EqnCount,9)=0

        EqnVar(EqnCount,1)=0
        EqnVar(EqnCount,2)=0
        EqnVar(EqnCount,3)=0

        EqnOpr(EqnCount,1)=1
        EqnOpr(EqnCount,2)=1

        Variab='Window Equation definition ScheduleName'
        CALL GETWS(Line,k,l,EqnSchName(EqnCount),'--')
        if (eqnSchname(EqnCount)(1:1).eq.'*') then
           eqnSchname(EqnCount)(1:)=eqnSchname(EqnCount)(2:)
        end if

c read term 1

        Variab='Window Equation C0 of term1'
        CALL GETWR(Line,k,l,EqnCoef(EqnCount,1),0.0,.TRUE.)

        Variab='Window Equation C1 of term1'
        CALL GETWR(Line,k,l,EqnCoef(EqnCount,2),0.0,.TRUE.)

        Variab='Window Equation C2 of term1'
        CALL GETWR(Line,k,l,EqnCoef(EqnCount,3),0.0,.TRUE.)

        Variab='Window Equation variable of term1'
        CALL GETWS(Line,k,l,word,' ')
        Call UpperC(word)
        if (word(1:4).eq.'VMET') then
          EqnVar(EqnCount,1)=1
        else if (word(1:4).eq.'DMET') then
          EqnVar(EqnCount,1)=2
        else if (word(1:4).eq.'TMET') then
          EqnVar(EqnCount,1)=3
        else if (word(1:4).eq.'VBUI') then
          EqnVar(EqnCount,1)=6
        else if (word(1:4).eq.'PWIN') then
          EqnVar(EqnCount,1)=7
        else if (word(1:2).eq.'DP') then
          EqnVar(EqnCount,1)=8
        else
          write(cof,*) 'variable name at EQN-WIN not interpreted in'//
     & 'line',
     &   line(1:lenstr(line))
        end if

        Variab='Window Equation operator of (term1)opr(term2)'
        CALL GETWS(Line,k,l,word,' ')
        if (word.eq.'+') then
          EqnOpr(EqnCount,1)=1
        else if (word.eq.'-') then
          EqnOpr(EqnCount,1)=2
        else if (word.eq.'*') then
          EqnOpr(EqnCount,1)=3
        else if (word.eq.'/') then
          EqnOpr(EqnCount,1)=4
        else
          write(cof,*) 'operator 1=',word,
     &   '  at EQN-WIN not interpreted in line',
     &   line(1:lenstr(line))
        end if

        if (K.gt.l) goto 89

c read term 2

        Variab='Window Equation C0 of term1'
        CALL GETWR(Line,k,l,EqnCoef(EqnCount,4),0.0,.TRUE.)

        Variab='Window Equation C1 of term1'
        CALL GETWR(Line,k,l,EqnCoef(EqnCount,5),0.0,.TRUE.)

        Variab='Window Equation C2 of term1'
        CALL GETWR(Line,k,l,EqnCoef(EqnCount,6),0.0,.TRUE.)

        Variab='Window Equation variable of term1'
        CALL GETWS(Line,k,l,word,' ')
        Call UpperC(word)
        if (word(1:4).eq.'VMET') then
          EqnVar(EqnCount,2)=1
        else if (word(1:4).eq.'DMET') then
          EqnVar(EqnCount,2)=2
        else if (word(1:4).eq.'TMET') then
          EqnVar(EqnCount,2)=3
        else if (word(1:4).eq.'VBUI') then
          EqnVar(EqnCount,2)=6
        else if (word(1:4).eq.'PWIN') then
          EqnVar(EqnCount,2)=7
        else if (word(1:2).eq.'DP') then
          EqnVar(EqnCount,2)=8
        else
          write(cof,*) 'variable name at EQN-WIN not interpreted in '//
     & 'line',
     &   line(1:lenstr(line))
        end if

        if (K.gt.l) goto 89

        Variab='Window Equation operator of (term1)opr(term2)'
        CALL GETWS(Line,k,l,word,' ')
        if (word.eq.'+') then
          EqnOpr(EqnCount,2)=1
        else if (word.eq.'-') then
          EqnOpr(EqnCount,2)=2
        else if (word.eq.'*') then
          EqnOpr(EqnCount,2)=3
        else if (word.eq.'/') then
          EqnOpr(EqnCount,2)=4
        else
          write(cof,*) 'operator 2=',word(1:lenstr(word)),
     &   '  at EQN-WIN not interpreted in line',
     &   line(1:lenstr(line))
        end if

        if (K.gt.l) goto 89

c read term 3    Coef 7,8,9  var 3

        Variab='Window Equation C0 of term1'
        CALL GETWR(Line,k,l,EqnCoef(EqnCount,7),0.0,.TRUE.)

        Variab='Window Equation C1 of term1'
        CALL GETWR(Line,k,l,EqnCoef(EqnCount,8),0.0,.TRUE.)

        Variab='Window Equation C2 of term1'
        CALL GETWR(Line,k,l,EqnCoef(EqnCount,9),0.0,.TRUE.)

        Variab='Window Equation variable of term1'
        CALL GETWS(Line,k,l,word,' ')
        Call UpperC(word)
        if (word(1:4).eq.'VMET') then
          EqnVar(EqnCount,3)=1
        else if (word(1:4).eq.'DMET') then
          EqnVar(EqnCount,3)=2
        else if (word(1:4).eq.'TMET') then
          EqnVar(EqnCount,3)=3
        else if (word(1:4).eq.'VBUI') then
          EqnVar(EqnCount,3)=6
        else if (word(1:4).eq.'PWIN') then
          EqnVar(EqnCount,3)=7
        else if (word(1:2).eq.'DP') then
          EqnVar(EqnCount,3)=8
        else
          write(cof,*) 'variable name at EQN-WIN not interpreted in '//
     & 'line',
     &   line(1:lenstr(line))
        end if


89      Call readlin(line, LinTyp, K, .True.)
        l=lenstr(line)
C See if we are at the end of the data yet
        if (FlgEnd) goto 99

C is this a next equation then goto 10
        if (LinTyp .eq. TName) goto 10

99	continue

	RETURN
	END

Ch***********************************************************************
      SUBROUTINE IniDefault
C***********************************************************************
C iniDefaults initializes variables that contain defaults values that
C may have to be used when data is missing in input lines of *.CIF
C
C@lbl dml 1999nov19 Rename variable Newton in COMMON block /PRCONT/
C   to nNewt.
C@lbl dml 1999nov22 Replace solver control parameters noInit and useOpz
C   with stp1Init, stp2Init, and rhoSync.
C
C***********************************************************************
Ch***********************************************************************

      IMPLICIT NONE
      INCLUDE 'comv-inp.inc'
      INCLUDE 'comv-uni.inc'
      INCLUDE 'comv-phy.inc'

C     ! PR-CONT
      DepsFR  = 1.0e-4
      ODepsFA  = 1.0E-6
      DepsFA  = ODepsFA/(ABS(ifact(UnitFma)))
      ODepsCJ  = 2.0e-7
      DepsCJ  = ODepsCJ/(ABS(ifact(UnitFma)))
      ODdifLim = 1.0e-4
      Ddiflim = ODdiflim/(ABS(ifact(UnitP)))
      DnNewt = 1
CC    DuseOpz = 0
CC    DnoInit = 0
      Dstp1Init = 1
      Dstp2Init = 2
      DrhoSync = .false.
C@NBI PGS 2000Aug18 - Surely the default solver should be no. 6 now that
C@NBI                 D.Lorenzetti has revamped the solver ?
CC    DslvSel = 5
      DslvSel = 6
      DmIter  = 50

C     ! CP-BUILding
      DZref = 10.0

C     ! ENV-BUIlding
      DZentr  = 0.0
      DAngBui = 0.0
      DLat    = 43.0
      DLon    = 0.0

C     ! ENV-WINd windspeed ref height, altitude meteo station and
C     ! Alpha Meteo correction for wind speed
      DZVmet   = 10.0
      DZmet    = 0.001
      DAlphMet = 0.14

C     ! ENV-WINd WindExponent
      DWExp   = 0.18

C     ! SCH-MET header 2 Time, wind speed, wind direction, temp, Xh, Pbarom
      DMetName   = 'Meteoname'
      DMetTime   = 'jan01_'
      DVmet      =  0.0
      DDmet      =  0.0
C new input conversions
      ODTmet     = 20.0
      DTMet      = (ODTmet-ifact(UnitToff))/Ifact(UnitTmul)

C new input conversions
      ODXhMet    =  0.0
      DXhMet     = ODXhMet/Ifact(UnitXh)

C new input conversions
      ODPbMet    = 101.325
        DPbMet     = ODPbMet

C@tno jcp 1996Jul16_18:33:06 new SCH-POL
C SCH-POL header 2

        DSchPolName   = 'OutsidePollutant'


C NORM-CR Normalized crack temperature, pressure and humidity ratio

C new input conversions
      ODNormCrT  = 20.0
      DNormCrT   = (ODnormCrT-ifact(UnitToff))/ifact(UnitTmul)

C new input conversions
C NormCrPb always kPa (as PbMet)
        ODNormCrPb = 101.3250
C never convert this unit
        DNormCrPb  = ODNormCrPb


C new input conversions
C@NBI PGS 2003Apr28 - Standard air density of 1.2 kg/m³ at 20°C and
C@NBI                 101325 Pa implies a humidity of 39%RH (5.65 g/kg)
      ODNormCrXh = 0.00565
      DNormCrXh  = ODNormCrXh/ifact(UnitXh)

C Duct transitions: relam, returb

      DReLam     = 2300.0
      DReTurb    = 3500.0

C NET-ZONes

C    temperature
C new input conversions
      ODTz  = 20.0
      DTz   = (ODTz-ifact(UnitToff))/ifact(UnitTmul)
C    ref height
      DZz   =  0.0
C    Volume
C@empa aw 2000dec01 change default for volume from 50 to 75
      DVz   = 75.0
C    Humidity
      ODXhz =  0.0
      DXhz  = ODXhz/ifact(UnitXh)

C Pollutant molecular mass

      DMM = 28.9645

C@tno jcp 1996Apr06_10:24:22 might be safe but using an undefined pollutant is
C also an error. next part left out. MM(*) is used to see if Pollutants are
C defined more than once
CC safer to initialize MM for the full array to DMM
CC MM is read by inpoldes 1..nconcpoldes and 1..nconc in zones but the unit
CC conversions might go from 1..MaxC and might divide by an uninitialized
CC variable
C     do 80 i=1,MaxC
C       MM(i)=DMM
C80   continue

C Pollutant diffusion in air
      DDiffusion=2.0E-5


C *** END OF INITIALIZING DEFAULTS ***
C@tno jcp 1996Apr06_10:25:56 there are more data initializations in routine Inh
        RETURN
        END


