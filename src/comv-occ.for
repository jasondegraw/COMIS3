C+*********************************************************** comv-occ.f
Ch**********************************************************************
C@NBI PGS 2000Nov01 - Added argument "Met", removed argument "qH2O"
         SUBROUTINE OccCO2O2(qCO2,qO2,Met,OccNr)
C***********************************************************************
C Purpose    : OccCO2 calculates the H20 CO2 flow kg/s produced and Oxygen flow
C              kg/s consumed by occupants with the number OccNr
C
C Module     : jcp 1996Apr22
C Limitation : Dutch data , just 3 pollutants modelled, qH2O simplified no sweat
C Changes:
C@NBI PGS 2000Nov01 - Added passed argument "Met", removed argument "qH2O"
C@NBI               - tidied up code; no syntax change apart from were commented
C@NBI               - Internal variables "BasalMet" & "Cpr" no loner needed.
C@NBI               - Pregnancy (Cp) is best handled by adjusting MET value in input file.
C
C Pass parameters:
C
C IO  Name    Unit        Description
C
C  O  qCO2    kg/s        CO2 produced
C  O  qO2     kg/s        O2  consumed
C  O  Met     W           Metabolic rate when seated at rest.  Note that
C                         this is larger than the basal metabolic rate (BMR);
C                         See comment in SUBROUTINE OccMet
C I   OccNr   -           sequence number of the occupant
C
C Common block parameters used in this routine:
C
C I   OccAge  year        Age
C I   OOccAct -           total activity for this person at activity 1.0
C
C Internal variales:
C
C -   Rq      -           =0.83 respiratory quotient (q CO2 / q O2 )
C                         range 0.7....0.9 (>1 short periods)
C                         depends upon the diet and "burned" food. Ave is 0.83
C
Ch**********************************************************************

C     ! Global declarations 
      IMPLICIT NONE
      INCLUDE 'comv-uni.inc'
      INCLUDE 'comv-inp.inc'
      INCLUDE 'comv-phy.inc'
      REAL qCO2,qO2,qH2O
      INTEGER OccNr
C     ! Local declarations
      REAL Rq,Met,Qom
      INTEGER Lstr
      CHARACTER*40 str
C-----
C     ! checks
      if(OccNr.gt.MaxO) then
        CALL intdis(MaxO,Str,Lstr)
        CALL Error('The OccupantNumber in a call of OccCO2 is '//
     &    'larger than MaxO='//str(1:lstr),3)
      elseif(OccAge(OccNr).lt.REALMin) then
        CALL intdis(OccNr,Str,Lstr)
        CALL Error('The Occupant with Number='//str(1:lstr)//
     &  ' in a call of OccCO2 is not defined',3)
      end if
C-----
C     ! Calculations
      Rq=0.83
CC    Cpr=1.0
      Qom=1/((0.23*Rq+0.77)*5.87)/3600/1000
C     ! m3/s O2 per Watt basic metabolic rate (BMR)
C     ! Qom=Quotient oxygen metabolism
C@NBI               - Internal variables "BasalMet" & "Cpr" no loner needed
CC    BasalMet=OOccAct(Occnr)
      Met=OOccAct(Occnr)
C     ! W
C Note that the occupant activity in a room is controlled by the schedule, and
C this one is not, it is coming from the fixed OOccAct(OccNr) which is the
C total activity for this person at activity 1.0 MET
C@NBI               - Internal variables "BasalMet" & "Cpr" no loner needed
CC    Met=Cpr*BasalMet
      qO2=Qom*Met
C     ! m3/s
      QCO2=Rq*Qom*Met
C     ! m3/s
C@NBI PGS 2000Nov01 - Moved calculation of qH2O up to routine "OccH2O"
CCC      H20 kg/s 50 g/h at 1.3Activity*1.8m2*42Watt
CCC 50 g/h /3600s /1000g /1.3Act /1.8m2 /42Watt=1.4E-7
CC
CC        QH2O=1.4E-7*Met
CC      m3/s
C@NBI PGS 2000Nov01 - Moved calculation of qO2 (kg/s) up to routine "OccO2"
C@tno jcp 1996Apr24_10:16:10 oxigen flow is a negative source
CC      Qo2=-qO2*1.293*32/MMair
CC      kg/s
C@NBI PGS 2000Nov01 - Moved calculation of qCO2 (kg/s) up to routine "OccCO2"
CC      QCO2=QCO2*1.293*44/MMair
CC      kg/s
      return
      end


Ch**********************************************************************
       REAL FUNCTION FNAdubois(Mass,length)
C***********************************************************************
C Purpose    : FN Adubois returns the Dubois skin area from the given Mass and
C              Length of a person
C Module     : jcp 1996Apr22
C Changes:
C
C Pass parameters:
C IO  #    Name           Unit        Description
C
C  O  1    qCO2           m2          skin area
C I       mass            kg          mass of the person
C I       length          m           length of the person
C
C***********************************************************************

       IMPLICIT NONE
       REAL Mass,Length
       REAL adubois
       Adubois=0.2025*mass**0.425*length**0.725
       FNadubois=adubois
       return
       end


Ch**********************************************************************
         SUBROUTINE OccCO2(qCO2,OccNr)
C***********************************************************************
C Purpose    : OccCO2 calculates the CO2 flow kg/s produced
C              by the occupants with the number OccNr
C
C Module     : jcp 1996Apr22
C Limitation : Dutch data
C Changes:
C@NBI PGS 2000Nov01 - Added argument "Met", removed argument "qH2O"
C@NBI               - Moved calculation of QCO2 (kg/s) here from routine "OccCO2O2"
C@NBI               - Also had to INCLUDE 'comv-phy.inc'
C
C Pass parameters:
C   IO: Name:   Units:  Description:
C    O  qCO2    kg/s    CO2 produced
C   I   OccNr   -       sequence number of the occupant
Ch**********************************************************************

C     ! Global declarations 
      IMPLICIT NONE
      INCLUDE 'comv-uni.inc'
      INCLUDE 'comv-inp.inc'
      INCLUDE 'comv-phy.inc'
      REAL qCO2
      INTEGER OccNr
C     ! Local declarations
      REAL qO2,Met
C-----
CC    Call OccCO2O2(qCO2,qO2,qH2O,OccNr)
      Call OccCO2O2(qCO2,qO2,Met,OccNr)
C@NBI PGS 2000Oct31 - 1.293 kg/m³ is too dense (air at 0°C). It is better
C@NBI                 to assume room temp., hence 1.204 kg/m³ dry air, as
C@NBI                 governed by ideal gas law.
CC    QCO2=QCO2*1.293*44/MMair
      QCO2=QCO2*1.204*44/MMair
C     kg/s net CO2 exhalation
      return
      end


Ch**********************************************************************
         SUBROUTINE OccO2(qO2,OccNr)
C***********************************************************************
C Purpose    : OccO2 calculates the O2 flow kg/s consumed
C              by the occupants with the number OccNr
C
C Module     : jcp 1996Apr22
C Limitation : Dutch data
C Changes:
C@NBI PGS 2000Nov01 - Added argument "Met", removed argument "qH2O"
C@NBI               - Moved calculation of QCO2 (kg/s) here from routine "OccCO2O2"
C@NBI               - Also had to INCLUDE 'comv-phy.inc'
C
C Pass parameters:
C   IO: Name:   Units:  Description:
C    O  qO2     kg/s    O2 consumed
C   I   OccNr   -       sequence number of the occupant
Ch**********************************************************************

C     ! Global declarations 
      IMPLICIT NONE
      INCLUDE 'comv-uni.inc'
      INCLUDE 'comv-inp.inc'
      INCLUDE 'comv-phy.inc'
      REAL qO2
      INTEGER OccNr
C     ! Local declarations
      REAL qCO2,Met
C-----
CC    Call OccCO2O2(qCO2,qO2,qH2O,OccNr)
      Call OccCO2O2(qCO2,qO2,Met,OccNr)
C@tno jcp 1996Apr24_10:16:10 oxygen flow is a negative source
C@NBI PGS 2000Oct31 - 1.293 kg/m³ is too dense (air at 0°C). It is better
C@NBI                 to assume room temp., hence 1.204 kg/m³ dry air, as
C@NBI                 governed by ideal gas law.
CC    Qo2=-qO2*1.293*32/MMair
      QO2=-qO2*1.204*32/MMair
C     kg/s net O2 inhalation
      return
      end


Ch**********************************************************************
      SUBROUTINE OccH2O(qH2O,OccNr)
C***********************************************************************
C Purpose    : OccO2 calculates the H2O flow kg/s produced
C              by the occupants with the number OccNr
C              No sweating
C
C Module     : jcp 1996Apr22
C Limitation : 50 g/h at 1.2 total met
C Changes:
C@NBI PGS 2000Nov01 - Added argument "Met", removed argument "qH2O"
C
C Pass parameters:
C   IO: Name:   Units:  Description:
C    O  qH2O    kg/s    H2O produced
C   I   OccNr   -       sequence number of the occupant
Ch**********************************************************************

C     ! Global declarations 
      IMPLICIT NONE
      INCLUDE 'comv-uni.inc'
      INCLUDE 'comv-inp.inc'
      REAL qH2O
      INTEGER OccNr
C     ! Local declarations
      REAL qCO2,qO2,Met
C-----
CC    Call OccCO2O2(qCO2,qO2,qH2O,OccNr)
      Call OccCO2O2(qCO2,qO2,Met,OccNr)
C@NBI PGS 2000Nov01 - Moved up here from outine "OccCO2O2"
C     ! H20 kg/s 50 g/h at 1.3Activity*1.8m2*42Watt
C     ! 50 g/h /3600s /1000g /1.3Act /1.8m2 /42Watt = 1.4E-7
C@NBI PGS 2000Nov01 - This is a gross simplification, as it is in reality
C@NBI                 highly temperature dependent - at 10°C QH2O=30 g/h,
C@NBI                 whilst at 30°C QH2O=102 g/h for 1 MET and 1 CLO
      QH2O=1.4E-7*Met
      return
      end


Ch**********************************************************************
      SUBROUTINE OccOlf(olf_kgs,OccNr)
C***********************************************************************
C Purpose    : OccOlf calculates the rate of bioeffluent generation
C              by the occupants with the number OccNr
C
C Module     : PGS (NBI) 2000Nov01
C Changes:
C
C Pass parameters:
C   IO: Name:   Units:  Description:
C    O  olf_kgs kg/s    olf (converted to internal kg/s units)
C   I   OccNr   -       sequence number of the occupant
Ch**********************************************************************

C     ! Global declarations 
      IMPLICIT NONE
      INCLUDE 'comv-uni.inc'
      INCLUDE 'comv-inp.inc'
      REAL olf_kgs
      INTEGER OccNr
C     ! Local declarations
      REAL qCO2,qO2,Met
C-----
      Call OccCO2O2(qCO2,qO2,Met,OccNr)
C     ! - The factor 103.4 below is metabolic rate (W) @ 1 MET for standard COMIS human.
C     ! - The factor 1.0E+7 is an arbitrary one, to convert from olf to
C     !   kg/s, and ensures a low kg/s rate, and is also used in subroutine "FilUnit"
      olf_kgs=Met/103.4/1.0E+7
      return
      end


Ch**********************************************************************
         LOGICAL FUNCTION IsMan(str)
C                                 1
C***********************************************************************
C Purpose    : is the passed string meaning 'man' ? then TRUE
C              Used from OCC routines OCCl OCCM OCCMet  OCCCO2O2
C
C Module     : jcp 1996Apr22
C Changes:
C
C Pass parameters:
C
C IO  #	   Name		  Unit	      Description
C
C I  1    str            -           string containin man / male etc
C Return TRUE (if a man) or False
C
Ch**********************************************************************

        IMPLICIT NONE
C Declarations Global:
        Character*(*) Str
C Local
        call UpperC(str)
        if (str.eq.'MAN'  .or.
     &       str.eq.'MALE') then
          IsMan=.TRUE.
        else
          IsMan=.FALSE.
        end if
        return
        end

Ch**********************************************************************
         LOGICAL FUNCTION IsWoman(str)
C                                 1
C***********************************************************************
C Purpose    : is the passed string meaning 'woman' ? then TRUE
C              Used from OCC routines OCCl OCCM OCCMet  OCCCO2O2
C
C Module     : jcp 1996Apr22
C Changes:
C
C Pass parameters:
C
C IO  #	   Name		  Unit	      Description
C
C I  1    str            -           string containin man / male etc
C Return TRUE (if a woman) or False
C
Ch**********************************************************************

        IMPLICIT NONE
C Declarations Global:
        Character*(*) Str
C Local
        call UpperC(str)
        if (str.eq.'WOMAN'  .or.
     &       str.eq.'FEMALE') then
          IsWoman=.TRUE.
        else
          IsWoman=.FALSE.
        end if
        return
        end

Ch**********************************************************************
         LOGICAL FUNCTION IsMix(str)
C                                 1
C***********************************************************************
C Purpose    : is the passed string meaning 'mix' ? then TRUE
C              Used from OCC routines OCCl OCCM OCCMet  OCCCO2O2
C
C Module     : jcp 1996Apr22
C Changes:
C
C Pass parameters:
C
C IO  #	   Name		  Unit	      Description
C
C I  1    str            -           string containin man / male etc
C Return TRUE (if a mix) or False
C
Ch**********************************************************************

        IMPLICIT NONE
C Declarations Global:
        Character*(*) Str
C Local
        call UpperC(str)
        if (str.eq.'MIX'  .or.
     &       str.eq.'MIXED') then
          IsMix=.TRUE.
        else
          IsMix=.FALSE.
        end if
        return
        end

Ch**********************************************************************
         SUBROUTINE OccLength(Length,Age,Sex)
C                             1      2   3
C***********************************************************************
C Purpose    : OccLength calculates the Length of an occupant of given sex and
C              age. m
C
C Module     : jcp 1996Apr22
C Limitation : Dutch data
C Changes:
C
C Pass parameters:
C
C IO  #	   Name		  Unit	      Description
C
C  O  1    Length         kg          Length of the person
C I         Age           a           age of the person
C I         Sex           -           man,woman,mix
C
Ch**********************************************************************

C Declarations Global:
C
        IMPLICIT NONE
	INCLUDE 'comv-uni.inc'
        INCLUDE 'comv-inp.inc'

        REAL Length,Age
        Character*(*) Sex
        INTEGER LENSTR

C Declarations Local
        INTEGER I,ii
        CHARACTER*40 str
        LOGICAL IsMan,IsWoman,IsMix
        REAL AgeAr(12),ManAr(12),WomanAr(12)
        Data ageAr /-0.001,1,5,7,10,14,16,18,20,30,60,200/
        Data ManAr /0.52,0.77,1.11,1.24,1.4,1.61,1.73,1.77,
     &  1.78,1.78,1.72,1.6/
        Data WoManAr/0.51,0.755,1.11,1.24,1.39,1.615,1.66,1.67,
     &  1.67,1.67,1.61,1.5/

C Initialisations
        i=0
10      continue
          i=i+1
          if (AgeAr(i).gt.Age) goto 20
        goto 10

20      continue
c        write(*,*) 'age between =',ageAr(i-1),ageAr(i)
c        call ho(' in OccLength ',' ')
        ii=i-1
        str=sex

        if (IsMan(str)) then
         Length=ManAr(ii)+(manAr(i)-ManAr(ii))/
     &   (ageAr(i)-AgeAr(ii)) * (age-ageAr(ii))

        else if (IsWoman(str)) then
         Length=WoManAr(ii)+(WoManAr(i)-woManAr(ii))/
     &   (ageAr(i)-AgeAr(ii))*(age-ageAr(ii))

        else if (IsMix(str)) then
         Length=ManAr(ii)+(manAr(i)-ManAr(ii))/
     &   (ageAr(i)-AgeAr(ii))*(age-ageAr(ii))
         Length=Length+WoManAr(ii)+(WoManAr(i)-woManAr(ii))/
     &   (ageAr(i)-AgeAr(ii))*(age-ageAr(ii))
         Length=Length/2
        else
          Call ERROR('sex passed to OccLength must be man/woman/mix'//
     &  ' and is now '//str(1:lenstr(str)),3)
        end if
        return
        end



Ch**********************************************************************
         SUBROUTINE OccMass(Mass,Age,Sex)
C                           1    2   3
C***********************************************************************
C Purpose    : OccMass calculates the mass of an occupant of given sex and age.
C              kg
C
C Module     : jcp 1996Apr22
C Changes:
C
C Pass parameters:
C
C IO  #	   Name		  Unit	      Description
C
C  O  1    Mass           kg          mass of the person
C I         Age           a           age of the person
C I         Sex           -           man,woman,mix
C
Ch**********************************************************************

C Declarations Global:
C
        IMPLICIT NONE
	INCLUDE 'comv-uni.inc'
        INCLUDE 'comv-inp.inc'

        REAL Mass,Age
        Character*(*) Sex
        INTEGER LENSTR

C Declarations Local
        INTEGER I,ii
        CHARACTER*40 str
        LOGICAL IsMan,IsWoman,IsMix
        REAL AgeAr(12),ManAr(12),WomanAr(12)
        Data ageAr /-0.001,1,5,7,10,14,16,18,20,30,60,200/
        Data ManAr /3.4,10.5,19.3,24,31.5,47.5,59,66,69,75,75,65/
        Data WoManAr/3.3,10.2,18.9,23,31.5,49.5,56,58,59.5,64,64,55/
c        call ho('occmass sex=',sex)

C Initialisations
        i=0
10      continue
          i=i+1
          if (AgeAr(i).gt.Age) goto 20
        goto 10

20      continue
        ii=i-1
        str=sex
c        call ho('occmass sex=',str)
        if (IsMan(str)) then
c        call ho('occmass part man=',str)
         Mass=ManAr(ii)+(ManAr(i)-ManAr(ii))/
     &   (ageAr(i)-AgeAr(ii))*(age-ageAr(ii))
        else if (IsWoman(str)) then
c        call ho('occmass part woman=',str)
         Mass=WoManAr(ii)+(WoManAr(i)-woManAr(ii))/
     &   (ageAr(i)-AgeAr(ii))*(age-ageAr(ii))
        else if (IsMix(str)) then
c        call ho('occmass part mix=',str)
         Mass=ManAr(ii)+(ManAr(i)-ManAr(ii))/
     &   (ageAr(i)-AgeAr(ii))*(age-ageAr(ii))
         Mass=Mass+WoManAr(ii)+(WoManAr(i)-woManAr(ii))/
     &   (ageAr(i)-AgeAr(ii))*(age-ageAr(ii))
         Mass=Mass/2
        else
c        call ho('occmass part error=',str)
          Call ERROR('sex passed to OccMass must be man/woman/mix'//
     &  ' and is now '//str(1:lenstr(str)),3)
        end if
        return
        end



Ch**********************************************************************
         SUBROUTINE OccMet(Met,Age,Sex)
C***********************************************************************
C Purpose    : OccMet calculates the specific metabolism (or metabolic
C              rate) of an occupant of given sex and age per W/m2 DuBois
C              (skin area).
C@NBI PGS 2000Nov01 - Bugfix: The body's metabolic rate is BMR + WL
C@NBI                 where BMR is "basal metabolic rate", and WL is the
C@NBI                 "metabolic free energy production".  WL is 15 W/m²
C@NBI                 for sedentary sitting (1 MET), see reference ISO 8996.
C@NBI                 So this routine had to be changed, by adding 15 W/m².
C@NBI               - Internal REAL variable "Met" added.
C@NBI PGS 2000Nov02 - The empiricl data used here is probably from
C@NBI                 (or seems very similar to) that of Fleisch,
C@NBI                 "Helv. med. Acta", no.18, vol.23 (1951), which is
C@NBI                 better quality data than the older data of
C@NBI                 Aub & Du Bois from 1917.
C@NBI PGS 2000dec23 - P.O.Fanger's PMV/PPD model assumes that
C@NBI                 BMR + WL = 58 W/m² for an average human seated at
C@NBI                 rest (1 MET), but becuase COMIS can vary BMR
C@NBI                 according to age/sex/height, a human with an activity
C@NBI                 factor of 1 (equivalent to 1 MET) will not necessarily
C@NBI                 have 58 W/m².
C
C Module     : jcp 1996Apr22
C Changes:
C
C Pass parameters:
C
C IO     Name      Unit        Description
C
C  O     Met       W/m2        Specific metabolism /m2 Dubois
C I      Age       years       Age of the person
C I      Sex       -           Man,woman,mix
C
Ch**********************************************************************

C Declarations Global:
C
        IMPLICIT NONE
        INCLUDE 'comv-uni.inc'
        INCLUDE 'comv-inp.inc'

        REAL BasalMet,Age,Met
        Character*(*) Sex
        INTEGER LENSTR

C Declarations Local
        INTEGER I,ii
        CHARACTER*40 str
        LOGICAL IsMan,IsWoman,IsMix
        REAL AgeAr(18),ManAr(18),WomanAr(18)
C@NBI PGS 2000Nov02 - Fits known empirical data better if age 10 is chaged to 9.
C@NBI               - Also adjusted data for children under 10 years, the
C@NBI                 values given here were lower than all known empirical data!
C@NBI                 New value for 0 years old extrapolated from the
C@NBI                 average of LEWIS, DUVAL & ILIFF (1943); FLEISCH (1951);
C@NBI                 and ROBERTSON & REID (1952).
        Data ageAr /-0.001,9,14,15,16,17,18,20,25,30,35,40,
     &  45,50,55,60,65,200/
        Data ManAr /68.5,55.5,50.8,50.8,50,48.8,47,46.3,44.7,
     &   43.7,43,42.5,42.2,41.8,41.3,40.4,39.6,18/
        Data WoManAr/64.7,52.1,45.7,44.5,43.7,42,41.5,41,40.8,
     &   40.7,40.5,39.9,39.4,38.8,38.3,37.7,37.1,20.9/

C Initialisations
        i=0
10      continue
          i=i+1
          if (AgeAr(i).gt.Age) goto 20
        goto 10

20      continue
        ii=i-1
        str=sex
        if (IsMan(str)) then
         BasalMet=ManAr(ii)+(ManAr(i)-ManAr(ii))/
     &   (ageAr(i)-AgeAr(ii))*(age-ageAr(ii))

        else if (IsWoman(str)) then
         BasalMet=WoManAr(ii)+(WoManAr(i)-woManAr(ii))/
     &   (ageAr(i)-AgeAr(ii))*(age-ageAr(ii))

        else if (IsMix(str)) then
         BasalMet=ManAr(ii)+(ManAr(i)-ManAr(ii))/
     &   (ageAr(i)-AgeAr(ii))*(age-ageAr(ii))
         BasalMet=BasalMet+WoManAr(ii)+(WoManAr(i)-woManAr(ii))/
     &   (ageAr(i)-AgeAr(ii))*(age-ageAr(ii))
         BasalMet=BasalMet/2

        else
          Call ERROR('sex passed to OccMet must be man/woman/mix'//
     &  ' and is now '//str(1:lenstr(str)),3)
        end if
C@NBI PGS 2000Nov01 - See comment at top.
C       ! Metabolism when sedentary seated (1 MET) = BMR + WL
        Met = BasalMet + 15
        return
        end
