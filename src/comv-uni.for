C+*********************************************************** comv-uni.f
C@NBI PGS 2000Aug12 - Tidied up & detabbed whole module - no syntax change
Ch**********************************************************************
      SUBROUTINE CnvT(unit,choice,factor0,factor1,fact0,fact1)
C***********************************************************************
C CnvT (Conversion of Temp)
C unit  = string containing the units with known conversions from routine FilUnit
C choice= is the user unit
C factor= the array with conversion factors for unit
C fact1 = the conversion factor from SI to the user unit choice
C
C sept 1989 hcp
C Changes:
C@tno jcp 1996Jun27_12:40:29 entities must be units
C@NBI PGS 1999May13 - spelling
Ch**********************************************************************

      IMPLICIT NONE

C     ! Subroutine arguments
      CHARACTER*(*) unit,choice
      REAL fact0,fact1,factor0(*),factor1(*)

C     ! Internal variables
      INTEGER start,Nr,LENSTR,icStrStr
      character*40 dum
C-----
      start = 1
C     ! unitstrings in choice start at columns 1,11,21,31..
      nr=iCstrstr(Choice,unit,start)
C     ! check if nr=1,11,21...
      if (mod((nr-1),10).ne.0) then
         dum=unit
         CALL INERR('After &-PR-UNITS, name of the unit '//
     &   dum(1:lenstr(dum)),
     &   ' is not valid. Units are not updated. ',.TRUE.,1)
         call ContError2('Valid units are:',
     &   Choice(1:lenstr(Choice)))
         call LastError2('Now the conversion coefficient is '//
     &   'reset to 1.0 and a unit of:',
     &   'C (interpreted as degrees Celsius)',1)
         fact0=0
         fact1=1.0
         goto 77
      end if
      nr=(nr-1)/10+1

      fact0=factor0(nr)
      fact1=factor1(nr)

77    CONTINUE
      RETURN
      END

Ch**********************************************************************
      SUBROUTINE CnvIT(unit,choice,factor0,factor1,fact0,fact1)
C***********************************************************************
C CnvIT (Conversion of input Temp)
C unit        = string containing the units with known conversions from
C               routine FilUnit
C choice      = is the user unit
C factor      = the array with conversion factors for unit
C fact0,fact1 = the conversion factor from the user unit to SI
C apr 1995 jcp
Ch**********************************************************************

      IMPLICIT NONE
      REAL fact0,fact1,factor0(*),factor1(*)
      CHARACTER*(*) unit,choice
      call CnvT(unit,choice,factor0,factor1,fact0,fact1)
      fact0=-fact0/fact1
      fact1=1/fact1
      RETURN
      END


Ch**********************************************************************
      SUBROUTINE CnvUnit(unit,choice,factor,fact1)
C***********************************************************************
C CnvUnit (Conversion of Units)
C unit  = is the user unit
C choice= string containing the units with known conversions from routine FilUni
C factor= the array with conversion factors for choice
C fact1 = the conversion factor from SI to the user unit
C
C sept 1989 hcp
C Changes:
C@tno jcp 1996Jun27_12:36:16 here it is not known which quantity is taken
C I want to have that for error report
C@tno jcp 1996Apr07_15:49:52 start=1 added
C@tno jcp 1996Jun27_12:39:44 call inerr to show the false input line
C@tno jcp 1996Jun27_12:40:29 entities must be units
Ch**********************************************************************

      IMPLICIT NONE

C     ! Subroutine arguments
      REAL fact1,factor(*)
      CHARACTER*(*) unit,choice

C     ! Internal variables
      CHARACTER UnitRes*30
      character*40 dum
      INTEGER start,Nr,OSR,LENSTR,icStrStr
C-----
      start = 1
      UnitRes=Unit
      Osr = iCStrStr(unitres,'-OSR',start)
      if (OSR.GT.0) unitres(osr:osr+3)='    '

      start = 1
C     ! must test here on unitres (Pa without Pa-OSR)
      nr = iCstrstr(choice,unitres,start)
C     ! error report if unit is not found

C     ! check if nr=1,11,21...
      if (mod((nr-1),10).ne.0) then
         dum=unitres
         CALL INERR('After &-PR-UNITS, name of the unit '//
     &   dum(1:lenstr(dum)),
     &   ' is not valid. Units are not updated. ',.TRUE.,1)
         call ContError2('Valid units are:',
     &   Choice(1:lenstr(Choice)))
         call LastError2('Now the conversion coefficient is '//
     &   'reset to 1.0','The unit string remains fault.',1)
         fact1=1.0
         goto 77
      end if

C     ! unit strings in choice start at columns 1,11,21,31..
      nr=(nr-1)/10+1
      fact1=factor(nr)

77    CONTINUE
C     ! flag OSR (out side as reference) for pressures as a minus
      if (osr.gt.0) fact1=-fact1
      RETURN
      END


Ch**********************************************************************
      SUBROUTINE CnvIUnit(unit,choice,factor,fact1)
C***********************************************************************
C CnvIUnit (Conversion of Input Units)
C unit  = is the user unit
C choice= string containing the units with known conversions from routine FilUni
C factor= the array with conversion factors for choice
C fact1 = the conversion factor from the user unit to SI
C apr 1995 jcp
Ch**********************************************************************

      IMPLICIT NONE
      REAL fact1,factor(*)
      CHARACTER*(*) unit,choice
C-----
      call CnvUnit(unit,choice,factor,fact1)
      fact1=1/fact1
      RETURN
      END


Ch**********************************************************************
      SUBROUTINE FilUnit
C***********************************************************************
C in FilUnit the array with conversion coefficients (Conv(ConvEnt,NoUnit))
C and strings (UnitStr(*)) with possible unit names are filled.
C@lbl bvs 1995Jun16
C Also, the first conversion factor Conv(1,UnitXXX) is assigned to
C ifact(i) and ofact(i) as default conversion when there is no SET file
C and no &-PR-UNITS specification in the CIF file.
C default units
C@lbl bvs/end
C
C The data is not given in a datablock or file to keep the possibility to input
C values like 5/9.
C to convert from SI to userunits : multiply the SI parameter with the factor
C sept 1989 hcp
C changes
C norm_m3/s dm3/s m3/h dm3/h added (divide by 1.2kg/m3)
C
C explanation of the unit conversions for the Programmers Guide
C
C The routines CnvT and CnvIT deliver directly:
C ofact(UnitTmul),ofact(UnitToff) and
C ifact(UnitTmul),ifact(UnitToff) which both work like:
C        output = ofact(UnitToff) +  SI   * ofact(UnitTmul)
C            SI = ofact(UnitToff) + input * ofact(UnitTmul)
C
C Outside this routine the user units given in the *.SET file or under the
C keyword &-PR-UNITS are looked up in the string UnitStr and the connected
C conversion factor is copied into the quantities element in ifact(*) and ofact(*)
C This is done by the routine "CnvUnit" for all general entities except
C for "CnvT" for temperature and "CnvConc" for concentrations.
C
C             (user input) * (ifact) => (COMIS internal SI unit)
C (COMIS internal SI unit) * (ofact) => (user output)
C       See comv-par.inc
C
C       index 1 = AIRL(eakage)                   UnitCm
C             2 = FLOW (vent. flowrates)         UnitFma
C             3 = PRES(sure)                     UnitP
C             4 = TEMP(erature)                  UnitTmul
C             5 = HUMI(dity)                     UnitXh
C             6 = SOUR(ce)                       UnitPSou
C             7 = SINK                           UnitPSin
C             8 = CONC(entration of pollutants)  UnitPConc
C             9 = CFLO(w rate of fans)           UnitFva
C            10 = VELO(city of wind)             UnitW
C            11 = ACH  (air change rate)         UnitRate
C            12 = MEAN (age of air)              UnitAge
C            13 = ENER(rgy)                      UnitE
C            14 = PROF(ile, wind profile type)   UnitProf
C If quantities have to be added then insert them here--------------<
C and move UnitToff, UnitPol?? and UnitFvFlag up
C            15 = temperature offset             UnitToff
C            16 = Flag=1 :fan volume flow rate   UnitFvFlag
C                 Flag=0 :fan mass flow rate
C            17 = concentration pollutant 1      UnitPolCo + 1
C            18 = concentration pollutant 2      UnitPolCo + 2
C            19 = concentration pollutant 3      UnitPolCo + 3
C            ...
C         16 +n = concentration pollutant n      UnitPolCo + n
C   MaxC +16 +n = source strength, pollutant n   UnitPolSo + n
C 2*MaxC +16 +n = sink strength, pollutant n     UnitPolSi + n
C
C For concentration conversions a different approach is used. This is because
C a concentration can be a division of two units like kg/kg ug/m3 etc. and some
C direct units like ppm,ppb,vol%,mass%,part.
C With the 8+5 units implemented here there would be 64+5 combinations.
C Therefore the Counter and Denominator are treated separately by looking
C in the user unit for the division character '/'.
C
C Additionally there is the mass or volume conversion, which requires a
C multiplication of division by :
C (MolarMass of the pollutant)/(Molar mass of Air)
C Therefore the VM(*) array indicates whether a unit is Volume or Mass.
C The routine CnvConc determines the MaxC conversion factors from the internal
C COMIS SI kg/kg to the given user unit which can be of the form: ppm,vol%,
C part... ug/ml,ug/m3,m3/kg etc.
C
C NOTE If you plan to add units and conversion factors:
C The unit strings are found by matching the first occurence of the unit found in
C the input file on the UnitStr. Thus in UnitStr 'g/s' must come before 'kg/s'
C
C Note that for the array CONV() the last index is the quantity number
C As in FORTRAN the sequence of array elements is the first index changes the
C fastest, the last index the slowest. Thus we can pass a complete set of the
C first index of an array by passing Array(LastIndex) to a routine. If that
C routine dimensions that array for the full set of the first index.
C example main program has:
C        REAL Conv(10,16)
C        CALL subprg(Conv(14))
C In the subprg we need:
C        SUBROUTINE SubPrg(C)
C        REAL C(10)
C Now: C(1)=Conv(1,14)
C      C(2)=Conv(2,14)
C     C(10)=Conv(10,14)
C
C Changes:
C@empa aw 1995dec18 UnitFvFlag added
C@empa aw 1995dec18 default for ifact/ofact(UnitFvFlag)
C@tno jcp 1996Jun19_11:22:03 g/H must be g/h
C@lbl bvs 1996Nov27 mg/s to kg/s is 1e6, not 1e3 and ug/s to kg/s is 1e9 not 1e6
C@lbl bvs 1996Nov27 ug/s to kg/s is 1e9 not 1e6
C@lbl bvs 1998Sep21 m3/s@1Pa must come before dm3/s@1Pa
C@lbl bvs 1998Sep21 m3/h must come before dm3/h
C@NBI PGS 1999May05 - ml not mL
C@NBI PGS 1999Aug07 - Added possibility to output % relative humidity'
C@NBI PGS 2000Oct08 - Now we can define different I/O units for each pollutant,
C@NBI                 for SINK, SOURce and CONCentration, simply by adding
C@NBI                 a new line for each pollutant in &-PR-UNITS in .CIF file !!
C@NBI               - INTEGER "iPol" added
C@NBI PGS 2000Oct09 - FlgIConc and FlgOConc now redundant, so removed
C@NBI               - FlgPolDes is never =1 in this routine, because we
C@NBI                 we do not know the Molar Masses (mm), so we have to
C@NBI                 wait until &-POLDES to calculate unit conversion
C@NBI                 factors for concentration.  FlgPolDes test removed.
C@NBI PGS 2000Oct10 - Olf and Decipol added
C@NBI PGS 2002jul04 - BugFix: Corrected "m3/H" to "m3/h" for UnitCm
Ch**********************************************************************

      IMPLICIT NONE
      INCLUDE 'comv-inp.inc'
      INCLUDE 'comv-phy.inc'
      INTEGER iPol


C Fill in '-' in the NoUnit position for GetUnitNr
      Iunit(NoUnit) = '-'
      Ounit(NoUnit) = '-'

C-------------------
C     (1) AIRLeakage
C-------------------

      UnitStr(UnitCm)      ='kg/s@1Pa  kg/h@1Pa  kg/s@10Pa kg/h@10Pa'
      UnitStr(UnitCm)(41:) ='m3/s@1Pa  dm3/s@1Pa m3/h@1Pa  dm3/s@10Pa'
      UnitStr(UnitCm)(81:) ='m3/s@10Pa m3/h@10Pa ELA4      ELA10'
      UnitStr(UnitCm)(121:)='m2TNO     cm2TNO'

      IUnit(UnitCm) = UnitStr(UnitCm)(1:10)
      OUnit(UnitCm) = IUnit(UnitCm)
C     ! As the conversion factors need ExpN this
C     ! units conversion is handled by ConvCs and no Conv(*) is filled here.

C------------------------------
C     (2) VENTilation flow rate
C------------------------------

      UnitStr(UnitFma)     ='g/s       g/h       kg/s      kg/h'
      UnitStr(UnitFma)(41:)='m3/s      dm3/s     m3/h      dm3/h'

      Conv(1,UnitFma) = 1000.0
      Conv(2,UnitFma) = 3600.0*1000.0
      Conv(3,UnitFma) = 1.0
      Conv(4,UnitFma) = 3600.0
      Conv(5,UnitFma) = 1.0/1.2
      Conv(6,UnitFma) = 1000.0/1.2
      Conv(7,UnitFma) = 3600.0/1.2
      Conv(8,UnitFma) = 3600.0*1000.0/1.2

C     ! set default ifact/ofact values from first Conv value
C     ! Default is kg/s
      IUnit(UnitFma) = 'kg/s'
      OUnit(UnitFma) = IUnit(UnitFma)
      CALL CnvIUnit(IUnit(UnitFma),UnitStr(UnitFma),
     &            Conv(1,UnitFma),ifact(UnitFma))
      CALL CnvUnit(OUnit(UnitFma),UnitStr(UnitFma),
     &            Conv(1,UnitFma),ofact(UnitFma))

C----------------------------
C     (3) PRESsure difference
C----------------------------

      UnitStr(UnitP)     ='Pa        mmH2O     mmHg      inH2O'
      UnitStr(UnitP)(41:)='inHg      hPa       kPa'

      conv(1,UnitP) = 1.0
      conv(2,UnitP) = 1/9.80665
      conv(3,UnitP) = 1/133.322
      conv(4,UnitP) = 1/9.80665/25.4
      conv(5,UnitP) = 1/133.322/25.4
      conv(6,UnitP) = 1/100.0
      conv(7,UnitP) = 1/1000.0

C     ! Default is Pa
      IUnit(UnitP) = 'Pa'
      OUnit(UnitP) = IUnit(UnitP)
      CALL CnvIUnit(IUnit(UnitP),UnitStr(UnitP),
     &            Conv(1,UnitP),ifact(UnitP))
      CALL CnvUnit(OUnit(UnitP),UnitStr(UnitP),
     &            Conv(1,UnitP),ofact(UnitP))

C--------------------
C     (4) TEMPerature
C--------------------

C     ! The temperature in user units is :
C       (x userunit n) =   conv(UnitToff,n)
C                        + (x degC) / conv(UnitTmul,n)
C     ! The temperature in degC  is :
C       (x degC      ) = - conv(UnitToff,n) / conv(UnitTmul,n)
C                        + (x user unit n) / conv(UnitTmul,n)
      UnitStr(UnitTmul)     ='C         K         F         Ra'
      UnitStr(UnitTmul)(41:)='Re'

      conv(1,UnitToff) = 0.0
      conv(1,UnitTmul) = 1.0
      conv(2,UnitToff) = Tzero
      conv(2,UnitTmul) = 1.0
      conv(3,UnitToff) = 32.
      conv(3,UnitTmul) = 9./5.
      conv(4,UnitToff) = Tzero*1.8
      conv(4,UnitTmul) = 9./5.
      conv(5,UnitToff) = 0.0
      conv(5,UnitTmul) = 1.25

C     ! Default is C
      IUnit(UnitTmul) = 'C'
      OUnit(UnitTmul) = IUnit(UnitTmul)
      CALL CnvIT(IUnit(UnitTmul),UnitStr(UnitTmul),
     &            Conv(1,UnitToff),Conv(1,UnitTmul),
     &            ifact(UnitToff),ifact(UnitTmul))
      CALL CnvT(OUnit(UnitTmul),UnitStr(UnitTmul),
     &            Conv(1,UnitToff),Conv(1,UnitTmul),
     &            ofact(UnitToff),ofact(UnitTmul))

C-----------------
C     (5) HUMIdity
C-----------------

      UnitStr(UnitXh)     ='g/kg      kg/kg     mass%     %rh'

      conv(1,UnitXh) = 1000.0
      conv(2,UnitXh) = 1.0
      conv(3,UnitXh) = 100.0
      conv(4,UnitXh) = 100.0

C     ! Default is g/kg
      IUnit(UnitXh) = 'g/kg'
      OUnit(UnitXh) = IUnit(UnitXh)
      CALL CnvIUnit(IUnit(UnitXh),UnitStr(UnitXh),
     &            Conv(1,UnitXh),ifact(UnitXh))
      CALL CnvUnit(OUnit(UnitXh),UnitStr(UnitXh),
     &            Conv(1,UnitXh),ofact(UnitXh))
      IF(iUnit(UnitXh)(1:3).EQ.'%rh') CALL ERROR2(
     & 'Unit ''%rh'' can only be used for output of air humidity,'
     &,'but not for input.  Choose another input unit for humidity.'
     &,2)

C--------------------------------------
C     (6) SOURce strenght for pollutats
C--------------------------------------

C     ! This is for source strengths but at the moment we use flowrate for it.
C     ! However, that does not include the mm/mmAir conversions!
      UnitStr(UnitPSou)     ='g/s       g/h       kg/s      kg/h'
C@empa aw 2000apr06 As long as we don't take the correct densities for each
C@empa aw 2000apr06 pollutant, we may not allow volume flow units
C@empa aw 2000apr06 (see unitconversion of concentrations)
CC    UnitStr(UnitPSou)(41:)='m3/s      dm3/s     m3/h      dm3/h'
C@NBI PGS 2000Aug12 - someone forgot to comment out this line...  Now done.
CC    UnitStr(UnitPSou)(81:)='ug/s'
C@NBI PGS 2000Oct10 - Olf and Decipol added
      UnitStr(UnitPSou)(81:)='mg/s      ug/s      olf'

      Conv(1,UnitPSou) = 1000.0
      Conv(2,UnitPSou) = 3600.0*1000.0
      Conv(3,UnitPsou) = 1.0
      Conv(4,UnitPsou) = 3600.0
CC    Conv(5,UnitPsou) = 1.0/1.2
CC    Conv(6,UnitPsou) = 1000.0/1.2
CC    Conv(7,UnitPsou) = 3600.0/1.2
CC    Conv(8,UnitPsou) = 3600.0*1000.0/1.2
      Conv(9,UnitPsou) = 1E6
      Conv(10,UnitPsou)= 1E9
C@NBI PGS 2000Oct10 - Olf and Decipol added. The conversion factor is an
C@NBI                 arbitrary one, just to ensure that it is a realistically
C@NBI                 low concentration when converted to internal SI units (kg/s)
C@NBI                 If you change this conversion factor, you must also
C@NBI                 change it in subroutine "OccOlf"
      Conv(11,UnitPsou)= 1.0E+7

C     ! Default is kg/s
C@NBI PGS 2000Oct08 - Now we can define different I/O units for each pollutant,
      DO iPol=UnitPolSo+1,UnitPolSo+MaxC
         IUnit(iPol) = 'kg/s'
         OUnit(iPol) = IUnit(iPol)
         CALL CnvIUnit(IUnit(iPol),UnitStr(UnitPSou),
     &      Conv(1,UnitPSou),ifact(iPol))
         CALL CnvUnit(OUnit(iPol),UnitStr(UnitPSou),
     &      Conv(1,UnitPSou),ofact(iPol))
      ENDDO

C-------------------------------------
C     (7) SINK strength for pollutants
C-------------------------------------

C     ! This is for sink strengths but at the moment we use flowrate for it, which
C     ! is not so wrong because that defines the airflow from which all pollutant is
C     ! removed.
      UnitStr(UnitPSin)     ='g/s       g/h       kg/s      kg/h'
C@empa aw 2000apr06 As long as we don't take the correct densities for each
C@empa aw 2000apr06 pollutant, we may not allow volume flow units
C@empa aw 2000apr06 (see unitconversion of concentrations)
CC    UnitStr(UnitPSin)(41:)='m3/s      dm3/s     m3/h      dm3/h'
C@NBI PGS 2000Aug13 - To make SOURce and SINK the same, add "mg/s"
C@NBI PGS 2000Oct10 - Olf and Decipol added
CC    UnitStr(UnitPSin)(81:)='ug/s'
      UnitStr(UnitPSin)(81:)='mg/s      ug/s      olf'

      Conv(1,UnitPSin) = 1000.0
      Conv(2,UnitPSin) = 3600.0*1000.0
      Conv(3,UnitPSin) = 1.0
      Conv(4,UnitPSin) = 3600.0
CC    Conv(5,UnitPSin) = 1.0/1.2
CC    Conv(6,UnitPSin) = 1000.0/1.2
CC    Conv(7,UnitPSin) = 3600.0/1.2
CC    Conv(8,UnitPSin) = 3600.0*1000.0/1.2
C@NBI PGS 2000Aug13 - To make SOURce and SINK the same, added "mg/s"
CC    Conv(9,UnitPSin) = 1E9
      Conv(9,UnitPSin) = 1E6
      Conv(10,UnitPSin)= 1E9
C@NBI PGS 2000Oct10 - Olf and Decipol added
      Conv(11,UnitPSin)= 1.0E+7

C     ! Default is kg/s
C@NBI PGS 2000Oct08 - Now we can define different I/O units for each pollutant,
      DO iPol=UnitPolSi+1,UnitPolSi+MaxC
         IUnit(iPol) = 'kg/s'
         OUnit(iPol) = IUnit(iPol)
         CALL CnvIUnit(IUnit(iPol),UnitStr(UnitPSin),
     &      Conv(1,UnitPSin),ifact(iPol))
         CALL CnvUnit(OUnit(iPol),UnitStr(UnitPSin),
     &      Conv(1,UnitPSin),ofact(iPol))
      ENDDO

C-------------------------------------
C     (8) CONCcentration of pollutants
C-------------------------------------

      UnitStr(UnitPConc)      ='g         kg        mg        ug'
      UnitStr(UnitPConc)(41:) ='m3        dm3       ml        ul'
      UnitStr(UnitPConc)(81:) ='mass%     vol%      ppm       ppb'
C@NBI PGS 2000Oct10 - Olf and Decipol added
      UnitStr(UnitPConc)(121:)='part      decipol'

      conv(1,UnitPConc) = 1.0E+3
        VM(1)=0
      conv(2,UnitPConc) = 1.0
        VM(2)=0
      conv(3,UnitPConc) = 1.0E+6
        VM(3)=0
      conv(4,UnitPConc) = 1.0E+9
        VM(4)=0
      conv(5,UnitPConc) = 1.0
        VM(5)=1
      conv(6,UnitPConc) = 1.0E+3
        VM(6)=1
      conv(7,UnitPConc) = 1.0E+6
        VM(7)=1
      conv(8,UnitPConc) = 1.0E+9
        VM(8)=1
      conv(9,UnitPConc) = 100.0
        VM(9)=0
      conv(10,UnitPConc)= 100.0
        VM(10)=1
      conv(11,UnitPConc)= 1.0E+6
        VM(11)=1
      conv(12,UnitPConc)= 1.0E+9
        VM(12)=1
      conv(13,UnitPConc)= 1.0
        VM(13)=1
C@NBI PGS 2000Oct10 - Olf and Decipol added
      conv(14,UnitPConc)= 1.0E+5
        VM(14)=0

C     ! Default is kg/kg
C     ! At this stage (initialization) we do not yet know the Molar Masses (mm),
C     ! so we have to wait until &-POLDES to calculate unit conversion factors for conc.
C@NBI PGS 2000Oct08 - Now we can define different I/O units for each pollutant,
      DO iPol=UnitPolCo+1,UnitPolCo+MaxC
         IUnit(iPol) = 'kg/kg'
         OUnit(iPol) = IUnit(iPol)
C        Conversion factors calculated in subroutine inPolDes, when molar masses are read
      ENDDO

C-----------------------------------
C     (9) CFLOw rate for components, e.g. fans
C-----------------------------------

C     ! This is for component (e.g. fan)x flowrate, it uses the same units as air flow rate
C     ! however the conversion is made for an internal COMIS unit of m3/s, as
C     ! fan formula's are normally in m3/s.
C     ! Conversion factors are from m3/s*Conv(*)=unit
C     ! so m3/s*1.2=kg/s and Ifact(UnitFva)=1/1.2 Thus kg/s*ifact(UnitFva)=m3/s

      UnitStr(UnitFva)     ='g/s       g/h       kg/s      kg/h'
      UnitStr(UnitFva)(41:)='m3/s      dm3/s     m3/h      dm3/h'

C@empa aw 1995dec18 Changed fan flow conversion factors. Fan flow input is now
C@                also converted to mass flow unit kg/s.
C@                Routine ConvFan has ben changed in order to have
C@                mass flow related fan input parameters in feqn.
C@                The new unit conversion factor Conv(x,UnitFvFlag) indicates
C@                whether the input unit is volume flow or massflow related.
C@                Volume flow related input has to be multiplied with RhoI
C@                from the actual fan, TD or Flowcontroller to get mass flow.
      Conv(1,UnitFva) = 1000.0
      Conv(1,UnitFvFlag) = 0
      Conv(2,UnitFva) = 3600.0*1000.0
      Conv(2,UnitFvFlag) = 0
      Conv(3,UnitFva) = 1.0
      Conv(3,UnitFvFlag) = 0
      Conv(4,UnitFva) = 3600.0
      Conv(4,UnitFvFlag) = 0
      Conv(5,UnitFva) = 1.0
      Conv(5,UnitFvFlag) = 1.0
      Conv(6,UnitFva) = 1000.0
      Conv(6,UnitFvFlag) = 1.0
      Conv(7,UnitFva) = 3600.0
      Conv(7,UnitFvFlag) = 1.0
      Conv(8,UnitFva) = 3600.0*1000.0
      Conv(8,UnitFvFlag) = 1.0

C     ! The conversions are done by the routine ConvFa
C     ! Default is m3/s
      IUnit(UnitFva) = 'm3/s'
      OUnit(UnitFva) = IUnit(UnitFva)
      CALL CnvIUnit(IUnit(UnitFva),UnitStr(UnitFva),
     &     Conv(1,UnitFva),ifact(UnitFva))
      CALL CnvUnit(OUnit(UnitFva),UnitStr(UnitFva),
     &     Conv(1,UnitFva),ofact(UnitFva))
C     ! default for ifact/ofact(UnitFvFlag)
      CALL CnvUnit(IUnit(UnitFva),UnitStr(UnitFva),
     &     Conv(1,UnitFvFlag),ifact(UnitFvFlag))
      CALL CnvUnit(OUnit(UnitFva),UnitStr(UnitFva),
     &     Conv(1,UnitFvFlag),ofact(UnitFvFlag))

C--------------------------
C     (10) VELOcity of wind
C--------------------------

      UnitStr(UnitW)     ='m/s       cm/s      kt        ft/min'
      UnitStr(UnitW)(41:)='km/h      mile/h'

      conv(1,UnitW) = 1.0
      conv(2,UnitW) = 100.0
      conv(3,UnitW) = 1/0.514
      conv(4,UnitW) = 196.85
      conv(5,UnitW) = 3.6
      conv(6,UnitW) = 1.60934/3.6

C     ! Default is m/s (wind velocity conversion is output only)
      OUnit(UnitW) = 'm/s'
      CALL CnvUnit(OUnit(UnitW),UnitStr(UnitW),
     &     Conv(1,UnitW),ofact(UnitW))

C-------------------------------
C     (11) ACH - Air change rate
C-------------------------------

      UnitStr(UnitRate)     ='1/h       1/s'

      conv(1,UnitRate) = 1.0
      conv(2,UnitRate) = 1.0/3600.0

C     ! Default is 1/hr (air change rate conversion is output only)
      OUnit(UnitRate) = '1/h'
      CALL CnvUnit(OUnit(UnitRate),UnitStr(UnitRate),
     &     Conv(1,UnitRate),ofact(UnitRate))

C-------------------------
C     (12) MEAN age of air
C-------------------------

      UnitStr(UnitAge)     ='s         h'

      conv(1,UnitAge) = 1.0
      conv(2,UnitAge) = 1.0/3600.0

C     ! Default is seconds (mean age of air conversion is output only)
      OUnit(UnitAge) = 'h'
      CALL CnvUnit(OUnit(UnitAge),UnitStr(UnitAge),
     &            Conv(1,UnitAge),ofact(UnitAge))

C----------------------------
C     (13) ENERgy consumption
C----------------------------

      UnitStr(UnitE)     ='J         kwh'

      conv(1,UnitE) = 1.0
      conv(2,UnitE) = 1.0/(3.6*1000.0*1000.0)

C     ! Default is kwh (Energy conversion is output only)
      OUnit(UnitE) = 'J'
      CALL CnvUnit(OUnit(UnitE),UnitStr(UnitE),
     &     Conv(1,UnitE),ofact(UnitE))

C-----------------
C     (14) PROFile (Wind profile)
C-----------------

C     ! Windprofile selection done in routine Win
      UnitStr(UnitProf)   ='alpha     Z0'
      IUnit(UnitProf)     ='alpha'

      RETURN
      END


Ch**********************************************************************
      subroutine convcs(unit,cs,expn,choice)
C***********************************************************************
C
C Convert the cs value to kg/s @ Pa
C
C The basis of the conversion is:
C - the mass flow eqn is:
C   FMA= (rhoNorm/rho)**(n-1)*(muNorm/mu)**(2n-1)*Cm*(Dp)**n  Fundamentals (32)
C - for Volumetric C-values:
C   Cm=Cv*RhoNorm
C - for C-values defined at different pressures
C   Cm=Cmp/((dp)**n)
C Possible units are now:
C
C    kg/s@1Pa
C    kg/h@1Pa
C
C    kg/s@10Pa
C    kg/h@10Pa
C
C    m3/s@1Pa
C   dm3/s@1Pa
C    m3/h@1Pa
C
C    m3/s@10Pa
C   dm3/s@10Pa
C    m3/h@10Pa
C
C    ELA4       cm2 with the use of the actual exponent !
C    ELA10      cm2  "    "   "  "   "    "       "
C
C   cm2TNO
C    m2TNO
C
C     rw 1992jul04
C Changes:
C@empa aw 1995dec20 1.2 replaced with NormCrRho
C@empa aw 1995dec20 include comv-inp.inc to have NormCrRho
C@tno jcp 1996Jun19_10:37:22 explanation for Cs unit extended
C@tno jcp 1996Jun27_12:42:10 call inerr instead of error2
C@tno jcp 1996Jun27_12:46:12 choice added
C@tno jcp 1996Jun27_12:46:19 choice added
C@lbl bvs 1997Jan24 NorCrRho should be NormCrRho
Ch**********************************************************************

      IMPLICIT NONE
      include 'comv-inp.inc'
      CHARACTER*20 unit
      REAL cs,expn
      CHARACTER*(*) choice
      INTEGER LENSTR
C-----
      if ( unit(1:8).EQ.'kg/s@1Pa' ) then
         cs=cs
      else if ( unit(1:8).EQ.'kg/h@1Pa') then
         cs=cs/3600.
      else if ( unit(1:8).EQ.'kg/s@10Pa' ) then
         cs=cs/((10)**ExpN)
      else if ( unit(1:8).EQ.'kg/h@10Pa' ) then
         cs=cs/3600./((10)**ExpN)
      else if (unit(1:8).EQ.'m3/s@1Pa') then
         cs=cs*NormCrRho
      else if (unit(1:9).EQ.'dm3/s@1Pa') then
         cs=cs*NormCrRho*0.001
      else if (unit(1:8).EQ.'m3/h@1Pa') then
         cs=cs*NormCrRho/3600.
      else if (unit(1:9).EQ.'m3/s@10Pa') then
         cs=cs*NormCrRho/((10)**ExpN)
      else if (unit(1:10).EQ.'dm3/s@10Pa') then
         cs=cs*NormCrRho*0.001/((10)**ExpN)
      else if (unit(1:9).EQ.'m3/h@10Pa') then
         cs=cs*NormCrRho/3600./((10)**ExpN)
      else if ( unit(1:4).EQ.'ELA4' ) then
C        ! From ELA4 we use the mass flowrate at 4 Pa which we convert
C        ! using the actual flow exponent to kg/s@1Pa
         cs=cs*0.0001*NormCrRho*SQRT(2*4/NormCrRho)/((4)**ExpN)
      else if ( unit(1:5).EQ.'ELA10' ) then
C        ! From ELA10 we use the mass flowrate at 10 Pa which we convert
C        ! using the actual flow exponent to kg/s@1Pa
C        ! 0.61 is the contraction
         cs=cs*0.0001*0.61*NormCrRho*SQRT(2*10/NormCrRho)/((10)**ExpN)
      else if ( unit(1:6).EQ.'cm2TNO' ) then
C        ! cm2 and m2 in the formula: FMA=m2 . (2 Dp)^expn  . SQRT(Rho)
         cs=cs/10000.0 * ((2.0)**ExpN) * SQRT(NormCrRho)
      else if ( unit(1:5).EQ.'m2TNO' ) then
         cs=cs * ((2.0)**ExpN) * SQRT(NormCrRho)
      else
         call inerr('Input unit '//unit//
     &   'for air leakage values unknown.',' ',.TRUE.,2)
         call Conterror2(' Now the conversion factor is '//
     &   'reset to 1.0 and the unit is:','kg/s@1Pa')
         call Lasterror2(' Valid units are:',
     &   Choice(1:lenstr(Choice)),0)
      endif
      return
      end


Ch**********************************************************************
C@tno jcp 1996Jun27_12:46:19 new checking routine for the user Unit for Cs
      subroutine CheckCnvCs(unit,choice)
Ch**********************************************************************

      IMPLICIT NONE

C     ! Subroutine arguments
      CHARACTER*20 unit
      CHARACTER*(*) choice

C     ! Internal variables
      CHARACTER wordc*20
      INTEGER pos,kc,lc,lu,LENSTR,IcIndex
C-----
      lu=lenstr(unit)
      pos=ICindex(choice,unit(1:lu))
      kc=pos-1
      lc=lenstr(choice)
      call getwrd(choice,kc,lc,wordc)
      lc=lenstr(wordc)
      if (wordc(1:lc).ne.unit(1:lu)) then
         call INERR('After &-PR-UNITS, the airleak unit '//
     &   unit(1:lu)//
     &   ' is not spelled right.;Units are not updated. ',' ',.true.,1)
         call Conterror2('Valid units are:',
     &   choice(1:lenstr(choice)))
         call Lasterror2('Unit is now reset to kg/s@1Pa',
     &   'After &-PR-UNITS you may indicate with'//
     &   ' the keywords INPUT and/or OUTPUT, that '//
     &   ' either input or output units follow (one unit per line).',0)
         unit='kg/s@1Pa'
      end if
      return
      end


Ch**********************************************************************
      SUBROUTINE CnvConc(unit,choice,factor,volm,molm,fact1)
C***********************************************************************
C CnvConc (Conversion of Concentration Units)
C Calculates "ofact" for concentration of a pollutant.
C The conversion factor (Fact1) is from the SI unit kg/kg of pollutant
C concentration to the user output unit.
C This routine is called once for each pollutant, as it is clear that
C every pollutant will need its own user unit, e.g. % of oxygen, ppm CO2
C ug/m3 smoke part(icles/m3) for dust.
C
C I/O Passed arguments:
C I  unit       = is the user unit
C I  choice     = is the string with possible units from FilUnit
C I  factor()   = the array with conversion factors for choice
C I  volm(MaxC) : 1= volumetric 0=mass
C I  molm       = molar mass
C  O fact1      = the conversion factor from SI to the user output unit
C
C Internal variables:
C  unitCount  = string containing the Counters of the units with known
C               conversions from routine FilUnit
C  unitDenom  = string containing the Denominator of the units with known
C               conversions from routine FilUnit
C
C Changes:
C@tno jcp 1996Jun19_09:25:59 nr and nrc (Counter) nrd (Denominator) added
C@tno jcp 1996Jun19_10:13:29 for temp use, str*40
C@tno jcp 1996Jun19_10:17:36 changed part to replace a proper Unit string
C@tno jcp 1996Jun19_10:20:46 routine changed at many places to patch ill units
C@lbl bvs 1998Feb6 should only loop to nconcpoldes pollutants
C@NBI PGS 2000Oct09 - Now the routine is called separetely for each pollutant.
C@NBI               - We no longer need to send whole MOLM array, just MOLM(iPol)
C@NBI               - We no longer need to send whole FACT1 array, just FACT1(iPol)
Ch**********************************************************************

      IMPLICIT NONE
      INCLUDE 'comv-inp.inc'
      INCLUDE 'comv-phy.inc'

C     ! Subroutine arguments
      CHARACTER*(*) unit,choice
      REAL fact1,factor(*),molm
      INTEGER volm(*)

C     ! Internal variables
      REAL Fact
      INTEGER start,nr,nrc,nrd,ISTRSTR,LENSTR,icStrStr
      CHARACTER*20 UnitCount,UnitDenom
      character*40 str
C-----

      start = 1
      nr = istrstr(Unit,'/',start)
      if (nr .gt. 0) then
C        ! Choice= kg/kg..ul/m3
         UnitCount=Unit(1:nr-1)
         UnitDenom=Unit(nr+1:)
      else
C        ! Choice= ppm, ppb, vol%, mass%, decipol
         UnitCount=Unit
C        ! sorry, this is a miss in the system. we have to
C        ! check if the 'denominator' asks for airdensity correction
         if (unit .ne. 'mass%') then
C           ! make sure the correction for air density is made by
C           ! doing as if we have a unit like '../m3'
            UnitDenom='m3'
         else
C           ! mass% don't correct for air density
            UnitDenom=' '
         end if
      end if

C     ! Get the factor for the Counter, the part above/before the divsion
C     ! character '/'
      nrc = iCstrstr(Choice,UnitCount,start)
C     ! unitstrings in choice start at columns 1,11,21,31..
      if (mod((nrc-1),10).ne.0) then
         nrc=1
         call error2('There is no available unit '''//
     &   unitCount(1:lenstr(UnitCount))//''' for your unit string ',
     &   unit,2)
         call conterror2('Available is: ',choice)
         str=unit
         if (nr.gt.3) then
            Unit(3:)=str(nr:)
         else if (nr.lt.3) then
            Unit(3:)=str(nr:)
         end if
         Unit(1:3)='kg/'

         call lasterror2(' Now the conversion coefficient is '//
     &   'made for',Unit,1)
         fact=1.0
C@tno jcp 1996Jun19_09:24:14 here the i/o Unit string must be reset to kg/ as well
         goto 77
      end if
      nrc=(nrc-1)/10+1
      fact=factor(nrc)
C     ! is pollutant unit volumetric ?
      if (volm(nrc) .eq. 1) fact=fact*MolVol/molm
77    continue

C     ! Divide factor by the factor for the Denominator, the part under/after
C     ! the divsion character '/'
      if (UnitDenom .ne. ' ') then
         start=1
         nrd = iCstrstr(Choice,UnitDenom,start)
C        ! unitstrings in choice start at columns 1,11,21,31..
         if (mod((nrd-1),10).ne.0) then
            nrd=1
            call error2('There is no available unit '''//
     &      unitDenom(1:lenstr(unitDenom))//
     &      ''' for your unit string ',unit,2)
            call conterror2('Available is: ',choice)
C@tno jcp 1996Jun19_09:24:14 here the i/o Unit string must be reset to /kg as well
            Unit(nr:)='/kg'
            call lasterror2(' Now the conversion coefficient is '//
     &      'made for',unit,1)
            goto 177
         end if
         nrd=(nrd-1)/10+1
         fact=fact/factor(nrd)
C        is air unit volumetric ?
         if (volm(nrd) .eq. 1) fact=fact/MolVol*MMair
      end if
C     ! end if (ChoiceDenom .ne. ' ')
177   continue
      fact1=fact
      RETURN
      END


Ch**********************************************************************
      SUBROUTINE CnvIConc(unit,choice,factor,volm,molm,fact1)
C***********************************************************************
C CnvIConc (Conversion of Input Concentration Units)
C Calculates "ifact" for concentration of a pollutant.
C Gets conversion factor (Fact1) from the user input unit to the SI unit
C kg/kg of pollutant concentration for the pollutant.
C This routine is called once for each pollutant, as it is clear that
C every pollutant will need its own user unit, e.g. % of oxygen, ppm CO2
C ug/m3 smoke part(icles/m3) for dust.
C
C I/O Passed arguments:
C I  unit       = is the user unit
C I  choice     = is the string with possible units from FilUnit
C I  factor()   = the array with conversion factors for choice
C I  volm(MaxC) : 1= volumetric 0=mass
C I  molm       = molar mass
C  O fact1      = the conversion factor from user input unit to SI units
C
C Internal variables:
C  unitCount  = string containing the Counters of the units with known
C               conversions from routine FilUnit
C  unitDenom  = string containing the Denominator of the units with known
C               conversions from routine FilUnit
C
C Changes:
C@NBI PGS 2000Oct09 - Now the routine is called separetely for each pollutant.
C@NBI               - We no longer need to send whole MOLM array, just MOLM(iPol)
C@NBI               - We no longer need to send whole FACT1 array, just FACT1(iPol)
Ch**********************************************************************

      IMPLICIT NONE
      INCLUDE 'comv-inp.inc'
      REAL fact1,factor(*),molm
      INTEGER volm(*)
      CHARACTER*(*) unit,choice
C-----
      call CnvConc(unit,choice,factor,volm,molm,fact1)
      fact1=1.0/fact1
      return
      end
