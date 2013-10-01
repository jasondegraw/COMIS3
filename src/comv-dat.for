C+*********************************************************** comv-dat.f
Ch**********************************************************************
C@tno jcp 1995Apr26 this routine needed comv-inp.inc and thus no
C identical pass parameter
C     SUBROUTINE setting(Iunit,Ounit)
      SUBROUTINE setting

C***********************************************************************
C Purpose: setting READs parameters that are used to determine the amount
C          of echo and results printed . It also READs two sets of units,
C          one for input one for output.
C          Only a few units are included yet, you can see them in the
C          routine FilUnit. Units have to be typed exactly as used there.
C
C@lbl bvs 1995Jun New style COMIS.SET file inserted as example
C example:
C
C 00 (1=write(CRT.*) on CRT; 0=write(CRT.*) on COF (output file))
C 01 (value for test. Range 0,1)
C 00 (value for IEcho: Input Echo. Range 0,20)
C 05 (value for PEcho: Precalculations Echo. Range 0,20)
C 05 (value for SEcho: Solver Echo. Range 0,20)
C 05 (value for OEcho: Output Echo. Range 0,20)
C 10 (number of user units for input)
C 13 (number of user units for output)
C kg/s@1Pa ( input userunit for  air leakage  Cm =flow kg/s @ 1)
C kg/s  ( input userunit for  ventilation   massflow  kg/s )
C Pa    ( input userunit for  ventilation   pressure  Pa   )
C C     ( input userunit for  temperature             C    )
C g/kg  ( input userunit for  concentration humidity  g/kg )
C kg/s  ( input userunit for  pollutant source        kg/s )
C kg/s  ( input userunit for  pollutant sink          kg/s )
C kg/kg ( input userunit for  pollutant concentration kg/kg)
C m3/s  ( input userunit for  flow through fan        m3/s )
C m/s   ( input userunit for  wind velocity           m/s  )
C Cm    (output userunit from air leakage             Cm   )
C kg/h  (output userunit from ventilation massflow    kg/s )
C Pa    (output userunit from ventilation pressure    Pa   )
C C     (output userunit from temperature             C    )
C g/kg  (output userunit from humidity                g/kg )
C kg/s  (output userunit from pollutant source        kg/s )
C kg/s  (output userunit from pollutant sink          kg/s )
C kg/kg (output userunit from pollutant concentration kg/kg)
C m3/s  (output userunit from flow through fan        m3/s )
C m/s   (output userunit from wind velocity           m/s  )
C 1/h   (output userunit from air change rate         1/h  )
C h     (output userunit from mean age                s    )
C kwh   (output userunit from energy                  J    )
C EOF
C #here follows the former file COMIS.CWF
C &-COMIN
C   INPUT comisman.cif
C  OUTPUT comis.cif
C
C &-COMVEN
C   INPUT comis.cif
C OUTPUT comis.cof
C
C &-COMOUT
C  INPUT viktor.COF
C OUTPUT viktor.O1
C
C sept 1989 hcp
C Changes: april 25 , 1989
C@tno jcp 1995Apr26 all labels have been increased with 100 to
C be able to copy the unitconversion routines that are also used in inUnits
C in file comv-i25.f
C@NBI PGS 2000Oct08 - Detabbed and tidied up; no syntax change.
C@NBI PGS 2000Oct08 - Now we can define different I/O units for each pollutant,
C@NBI                 for SINK, SOURce and CONCentration, simply by adding
C@NBI                 a new line for each pollutant in &-PR-UNITS in .CIF file !!
C@NBI                 However this hasn't been implemented in SET file;
C@NBI                 all pollutants are defined with same units here.
C@NBI               - INTEGER "iPol" added
C@NBI               - FlgIConc and FlgOConc now redundant, so removed
C@NBI               - FlgPolDes is never =1 in this routine, because we
C@NBI                 we do not know the Molar Masses (mm), so we have to
C@NBI                 wait until &-POLDES to calculate unit conversion
C@NBI                 factors for concentration.  FlgPolDes test removed.
C@NBI PGS 2003Mar16 - Unified OEcho levels defined:
C@NBI                 OECHO     Type of output (cumulative)
C@NBI                 =====     ==============
C@NBI                   0       None
C@NBI                   1       Summary at end of simulation
C@NBI                   2       Every vent timestep: Building & zone-group summary
C@NBI                   3       Every vent timestep: Zones
C@NBI                   4       Every vent timestep: Link & facade elements
C@NBI                   5       Every internal timestep (pollutant timestep)
Ch**********************************************************************

      IMPLICIT NONE
      INCLUDE 'comv-uni.inc'
      INCLUDE 'comv-inp.inc'

C@tno jcp 1995Apr26 already in comv-inp.inc line deleted
C      CHARACTER*(*) Iunit(20),Ounit(20)

      INTEGER Eflag,iPol
      CHARACTER WORD*20,Dum*160
C@empa aw/hf 1995mar14 NIUnits,NOUnits introduced instead of Nunits
CC    INTEGER K,L,I,NUnits
      INTEGER K,L,I,NIUnits,NOUnits
C@lbl bvs 1997Oct20 added INTEGER decls for intcon, iempty and lenstr
      INTEGER INTCON,IEMPTY,LENSTR
C@lbl bvs 1997Jul28 added LENSTR to decl
      EXTERNAL INTCON,IEMPTY,LENSTR


C---------------------
C     Read echo ranges
C---------------------

      READ(SET,*) UseCRT
      READ(SET,*) Test
      READ(SET,*) IEcho
      READ(SET,*) PEcho
      READ(SET,*) SEcho
      READ(SET,*) OEcho


C------------------------------
C     Read no of I/O user units
C------------------------------

C     ! No. of input user units
      READ(SET,900) Dum
C@tno jcp 1995Apr26 label 30 is now 130
      IF (dum.eq.'EOF'.or. iempty(dum,1).eq.1) GOTO 130
      k=0
      l=lenstr(Dum)
      CALL GETWRD(Dum,k,l,word)
C@empa aw/hf 1995mar14 read NIunits and NOUnits and make error check
      NIunits=intcon(word,Eflag)
      IF (Eflag.ge.1) then
         CALL INERR('COMIS.SET: number of input units not found!',
     &      ' ',.false.,2)
      endif

C     ! No. of output user units
      READ(SET,900) Dum
C@tno jcp 1995Apr26 label 30 is now 130
      IF (dum.eq.'EOF'.or. iempty(dum,1).eq.1) GOTO 130
      k=0
      l=lenstr(Dum)
      CALL GETWRD(Dum,k,l,word)
      NOunits=intcon(word,Eflag)


C--------------------------
C     Read user input units
C--------------------------

C@tno jcp 1995Apr26 label 10 is now 110
C@empa aw/hf 1995mar14 NIunits now
      DO 110 I=1,NIunits
         READ(SET,900) Dum
         k=0
         CALL GETWRD(Dum,k,l,word)
C        ! Here we rely on the fact that the line number in the SET file
C        ! is the same as the Unit number (e.g. line 3 = UnitP)
         Iunit(i)=word

C@tno jcp 1995Apr26 part inserted from routine inUnits

C        ! Air leakage is not treated here;
C        ! CALL ConvCs is only possible per link when the flow exponent is known

C        ! Call the appropriate Input Conversion routine
         if (i .eq. UnitFma) THEN
C           ! Ventilation (mass) flowrate
            CALL CnvIUnit(IUnit(UnitFma),UnitStr(UnitFma),
     &         Conv(1,UnitFma),ifact(UnitFma))
         elseif (i .eq. UnitP) THEN
C           ! (air) pressure (differences)
            CALL CnvIUnit(IUnit(UnitP),UnitStr(UnitP),
     &         Conv(1,UnitP),ifact(UnitP))
         elseif (i .eq. UnitTmul) THEN
C           ! Temperature. This conversion uses two constants. the offset is
C           ! stored in the UnitToff position and the multiplier is stored in
C           ! the UnitTmul position
            CALL CnvIT(IUnit(UnitTmul),UnitStr(UnitTmul),
     &         Conv(1,UnitToff),Conv(1,UnitTmul),
     &         ifact(UnitToff),ifact(UnitTmul))
         elseif (i .eq. UnitXh) THEN
C           ! Humidity
            CALL CnvIUnit(IUnit(UnitXh),UnitStr(UnitXh),
     &         Conv(1,UnitXh),ifact(UnitXh))
         elseif (i .eq. UnitPSou) THEN
C           ! Source strength flowrate
C@NBI PGS 2000Oct08 - Now we can define different I/O units for each pollutant,
CC          CALL CnvIUnit(IUnit(UnitPSou),UnitStr(UnitPSou),
CC     &       Conv(1,UnitPSou),ifact(UnitPSou))
            DO iPol=UnitPolSo+1,UnitPolSo+MaxC
               Iunit(iPol)=word
               CALL CnvIUnit(IUnit(iPol),UnitStr(UnitPSou),
     &            Conv(1,UnitPSou),ifact(iPol))
            ENDDO
         elseif (i .eq. UnitPSin) THEN
C           ! Sink strength (=air flowrate from which pollutant is removed)
C@NBI PGS 2000Oct08 - Now we can define different I/O units for each pollutant,
CC          CALL CnvIUnit(IUnit(UnitPSin),UnitStr(UnitPSin),
CC  &         Conv(1,UnitPSin),ifact(UnitPSin))
            DO iPol=UnitPolSi+1,UnitPolSi+MaxC
               Iunit(iPol)=word
               CALL CnvIUnit(IUnit(iPol),UnitStr(UnitPSin),
     &            Conv(1,UnitPSin),ifact(iPol))
            ENDDO
         elseif (i .eq. UnitPConc) THEN
C           ! Concentration of air pollutants.
C           ! At this stage (reading the SET file) we do not know the Molar Masses (mm),
C           ! so we have to wait until &-POLDES to calculate unit conversion factors for conc.
C           ! Note that we fill ifact() from UnitPolCo+1 to UnitPolCo+MaxC, not UnitPConc
C@NBI PGS 2000Oct08 - Now we can define different I/O units for each pollutant,
CC          CALL CnvIConc(IUnit(UnitPConc),UnitStr(UnitPConc),
CC   &         Conv(1,UnitPConc),ifact(UnitPol),VM,mm)
            DO iPol=UnitPolCo+1,UnitPolCo+MaxC
               Iunit(iPol)=word
C              Conversion factors calculated in subroutine inPolDes, when molar masses are read
            ENDDO
         elseif (i .eq. UnitFva) THEN
C           ! Conversion for fan flowrate
            CALL CnvIUnit(IUnit(UnitFva),UnitStr(UnitFva),
     &         Conv(1,UnitFva),ifact(UnitFva))
         elseif (i .eq. UnitW) THEN
C           ! Air/wind velocity
            CALL CnvIUnit(IUnit(UnitW),UnitStr(UnitW),
     &         Conv(1,UnitW),ifact(UnitW))
         ENDIF
C@empa/tno jcp 1995Apr26/end
110   CONTINUE


C---------------------------
C     Read user output units
C---------------------------

C@tno jcp 1995Apr26 label 20 is now 120
C@lbl bvs 1995Jun13 NOunits
      DO 120 I=1,NOunits
         READ(SET,900) Dum
         k=0
         CALL GETWRD(Dum,k,l,word)
         Ounit(i)=word
C@tno jcp 1995Apr26 part inserted from routine inUnit

C        ! Air leakage is not treated here;
C        ! CALL ConvCs is only possible per link when the flow exponent is known

C call the appropriate Output Conversion routine
         if (i .eq. UnitFma) THEN
C           ! Ventilation (mass) flowrate
            CALL CnvUnit(OUnit(UnitFma),UnitStr(UnitFma),
     &         Conv(1,UnitFma),ofact(UnitFma))
         elseif (i .eq. UnitP) THEN
C           ! (air) pressure (differences)
            CALL CnvUnit(OUnit(UnitP),UnitStr(UnitP),
     &         Conv(1,UnitP),ofact(UnitP))
         elseif (i .eq. UnitTmul) THEN
C           ! Temperature. This conversion uses two constants. the offset is
C           ! stored in the UnitToff position and the multiplier is stored in
C           ! the UnitTmul position
            CALL CnvT(OUnit(UnitTmul),UnitStr(UnitTmul),
     &         Conv(1,UnitToff),Conv(1,UnitTmul),
     &         ofact(UnitToff),ofact(UnitTmul))
         elseif (i .eq. UnitXh) THEN
C           ! Humidity
            CALL CnvUnit(OUnit(UnitXh),UnitStr(UnitXh),
     &         Conv(1,UnitXh),ofact(UnitXh))
         elseif (i .eq. UnitPSou) THEN
C           ! Source strength flowrate
C@NBI PGS 2000Oct08 - Now we can define different I/O units for each pollutant,
CC          CALL CnvUnit(OUnit(UnitPSou),UnitStr(UnitPSou),
CC   &         Conv(1,UnitPSou),ofact(UnitPSou))
            DO iPol=UnitPolSo+1,UnitPolSo+MaxC
               Ounit(iPol)=word
               CALL CnvUnit(OUnit(iPol),UnitStr(UnitPSou),
     &            Conv(1,UnitPSou),ofact(iPol))
            ENDDO
         elseif (i .eq. UnitPSin) THEN
C           ! Sink strength (=air flowrate from which pollutant is removed)
C@NBI PGS 2000Oct08 - Now we can define different I/O units for each pollutant,
CC          CALL CnvUnit(OUnit(UnitPSin),UnitStr(UnitPSin),
CC   &         Conv(1,UnitPSin),ofact(UnitPSin))
            DO iPol=UnitPolSi+1,UnitPolSi+MaxC
               Ounit(iPol)=word
               CALL CnvUnit(OUnit(iPol),UnitStr(UnitPSin),
     &            Conv(1,UnitPSin),ofact(iPol))
            ENDDO
         elseif (i .eq. UnitPConc) THEN
C           ! Concentration of air pollutants.
C           ! At this stage (reading the SET file) we do not yet know the Molar Masses (mm),
C           ! so we have to wait until &-POLDES to calculate unit conversion factors for conc.
C           ! Note that we fill ofact() from UnitPolCo+1 to UnitPolCo+MaxC, not UnitPConc
C@NBI PGS 2000Oct08 - Now we can define different I/O units for each pollutant,
CC          CALL CnvConc(OUnit(UnitPConc),UnitStr(UnitPConc),
CC   &         Conv(1,UnitPConc),ofact(UnitPol),VM,mm)
            DO iPol=UnitPolCo+1,UnitPolCo+MaxC
               Ounit(iPol)=word
C              Conversion factors calculated in subroutine inPolDes, when molar masses are read
            ENDDO
         elseif (i .eq. UnitFva) THEN
C           ! Conversion for fan flowrate
            CALL CnvUnit(OUnit(UnitFva),UnitStr(UnitFva),
     &         Conv(1,UnitFva),ofact(UnitFva))
         elseif (i .eq. UnitW) THEN
C           ! Air/wind velocity
            CALL CnvUnit(OUnit(UnitW),UnitStr(UnitW),
     &         Conv(1,UnitW),ofact(UnitW))
         elseif (i .eq. UnitRate) THEN
C           ! Air change rate
            CALL CnvUnit(OUnit(UnitRate),UnitStr(UnitRate),
     &         Conv(1,UnitRate),ofact(UnitRate))
         elseif (i .eq. UnitAge) THEN
C           ! Mean age of air
            CALL CnvUnit(OUnit(UnitAge),UnitStr(UnitAge),
     &         Conv(1,UnitAge),ofact(UnitAge))
         elseif (i .eq. UnitE) THEN
C           ! Energy conversion
            CALL CnvUnit(OUnit(UnitE),UnitStr(UnitE),
     &         Conv(1,UnitE),ofact(UnitE))
         ENDIF
C@empa/tno jcp 1995Apr26/end
120   CONTINUE


C-----------------
C     Debug output  (optional)
C-----------------

130   IF ((Test.GT.0) .and. (IEcho .ge. 1)) THEN
         WRITE(CRT,*)'**********************************'
         WRITE(CRT,*)'Reading file COMIS.SET           *'
         WRITE(CRT,*)'**********************************'
         WRITE(CRT,*)'UseCRT=',UseCRT
         IF (UseCRT.GT.0) THEN
            WRITE(CRT,*) 'CRT is normally used for output in'
     &      //'WRITE(CRT.)'
         else
            WRITE(CRT,*) 'Output to CRT is redirected to the',
     &      ' *.COF output file.'
         ENDIF
C@tno jcp 1995Apr27 straightened the text in the next 6 lines
         WRITE(CRT,*)' Test              = ',Test
         WRITE(CRT,*)' Amount of output printed: '
         WRITE(CRT,*)'   Input           = ',IEcho
         WRITE(CRT,*)'   Precalculations = ',PEcho
         WRITE(CRT,*)'   Solver          = ',SEcho
         WRITE(CRT,*)'   Output          = ',OEcho
C@tno jcp 1995Apr27/end

C@tno jcp 1995Apr26 if statement added
C@tno jcp 1995Jul15_00:34:16 conversion factors added
         if ((NIUnits .gt. 0) .or. (NOUnits .gt. 0)) then
            WRITE(CRT,*) 'User units from the file COMIS.SET'
            WRITE(CRT,*) 'Item                     Input       Output '
            WRITE(CRT,*) 'airleak                = ',
     &         Iunit(UnitCm)(1:10),Ounit(UnitCm)(1:10)
            WRITE(CRT,*) 'airflow                = ',
     &         Iunit(UnitFma)(1:10),Ounit(UnitFma)(1:10),
     &         ifact(UnitFma),Ofact(unitFma)
            WRITE(CRT,*) 'pressure               = ',
     &         Iunit(UnitP)(1:10),Ounit(UnitP)(1:10),
     &         ifact(UnitP),Ofact(unitP)
            WRITE(CRT,*) 'temperature            = ',
     &         Iunit(UnitTmul)(1:10),Ounit(UnitTmul)(1:10),
     &         ifact(UnitTmul),ifact(unitToff),
     &         ofact(UnitTmul),Ofact(unitToff)
            WRITE(CRT,*) 'moisture content       = ',
     &         Iunit(UnitXh)(1:10),Ounit(UnitXh)(1:10),
     &         ifact(UnitXh),Ofact(unitXh)
C@NBI PGS 2000Oct08 - Now we can define different I/O units for each pollutant,
C@NBI                 (However this hasn't been implemented in SET file)
            DO iPol=1,MaxC
               WRITE(CRT,*) 'pollutant',iPol,'source       = '
     &            ,Iunit(UnitPolSo+iPol)(1:10)
     &            ,Ounit(UnitPolSo+iPol)(1:10)
     &            ,ifact(UnitPolSo+iPol),Ofact(unitPolSo+iPol)
               WRITE(CRT,*) 'pollutant',iPol,'sink         = '
     &            ,Iunit(UnitPolSi+iPol)(1:10)
     &            ,Ounit(UnitPolSi+iPol)(1:10)
     &            ,ifact(UnitPolSi+iPol),Ofact(unitPolSi+iPol)
               WRITE(CRT,*) 'pollutant',iPol,'concentration= '
     &            ,Iunit(UnitPolCo+iPol)(1:10)
     &            ,Ounit(UnitPolCo+iPol)(1:10)
     &            ,ifact(UnitPolCo+iPol),Ofact(unitPolCo+iPol)
            ENDDO
C@tno jcp 1995Apr25 fan flowrate was missing here
            WRITE(CRT,*) 'fan flowrate           = ',
     &         Iunit(UnitFva)(1:10),Ounit(UnitFva)(1:10),
     &         ifact(UnitFva),Ofact(unitFva)
            WRITE(CRT,*) 'wind velocity          = ',
     &         Iunit(UnitW)(1:10),Ounit(UnitW)(1:10),
     &         ifact(UnitW),Ofact(unitW)
C@lbl bvs 1995Jun13 new output units added
            WRITE(CRT,*) 'air change rate        = ',
     &         '(n/a)      ',Ounit(UnitRate)(1:10),
     &         '-          ',Ofact(unitRate)
            WRITE(CRT,*) 'mean age of air        = ',
     &         '(n/a)      ',Ounit(UnitAge)(1:10),
     &         '-          ',Ofact(unitAge)
            WRITE(CRT,*) 'energy                 = ',
     &         '(n/a)      ',Ounit(UnitE)(1:10),
     &         '-          ',Ofact(unitE)
C@tno jcp 1995Jul15_00:27:07 added wind profile
C power law (Alpha) or log profile (Z0)
            WRITE(CRT,*) 'wind profile           = ',
     &         Iunit(UnitProf)(1:10),' (n/a)     '
         endif
      ENDIF

900   FORMAT(A)
      RETURN
      END
