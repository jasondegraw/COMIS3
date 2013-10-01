C+*********************************************************** comv-che.f

Ch***********************************************************************


C@tno jcp 1996Jul08_12:25:59 GetSchZOne get zone name range,Occupant schedule,
C DAF
Ch***********************************************************************
        SUBROUTINE GetSchZone(e2,SchZone)
C                              I   O
C                              i   c
C                              |   |
C                              |   range of zone names/numbers
C                              record number in DAF
C       Get the zne name (range) from DAF for a record that contains an
C       occupant schedule
C************************************************************************
        IMPLICIT NONE
        INTEGER e2
        CHARACTER*(*) SchZone

        INCLUDE 'comv-uni.inc'
	INCLUDE 'comv-inp.inc'

        INTEGER LenStr

        INTEGER DafRec,l,k
        CHARACTER line*160,word*40

        DafRec=e2
        READ(DAF,REC=DafRec) Line
        l=lenstr(line)
        k=0
        variab='CheckTMS: Get schZone word 1'
        CALL GETWRD(Line,k,l,word)
        IF (index(word,'*').GT.0) CALL GETWRD(Line,k,l,word)
        variab='CheckTMS: Occupant schedule zones'
        CALL GETWS(Line,k,l,SchZone,'ALL')

        RETURN
        END



Ch***********************************************************************
        SUBROUTINE CheckTMS()
C***********************************************************************
C Purpose: Checks for double definition of schedule entries  
C
C Changes:
C@empa aw 2000mar24 Completely redesigned the algorithm. Old version delivered 
C@empa              sometimes erroneos warnings.
C
C       At one value of e1 (time) there may not be more records with the same
C       schedule+name
C       Except for the occupant schedule (9), they often have a move out of one
C       room and a move into another room at the same timestep,
C       Read for those the zone number (first data after the time) and report
C       an error only if the zone number is also the same.

C       e1=seconds since start of simulation
C       e2=record number of the schedule data
C       e3=ScheduleType*f+pointerScheduleName
C       f depends on the Parameter MaName (=1500, maximum number of names)
C       and is here 10000 (always the decade above MaName)

C       The first record is different
C       e1=stop julian day nr
C       e2=0
C       e3=seconds from 00:00:00 on the stop day

C       The record with schedule start is different
C       e1=schedule start time in seconds since the start of the simulation
C          (time/date as given in &-PR-SIMU)
C       e2=0
C       e3=0

C       The records with histo start is different
C       e1=schedule start time in seconds since the start of the simulation
C          (time/date as given in &-PR-SIMU)
C       e2=0
C       e3=-2

C       The Last record is different
C       e1=stop time in seconds since the simulation start
C       e2=0
C       e3=-1

C definition scheduletype numbers definition
C keyword  name     schedule number
c       23='SCH-MAI'  1
c       24='SCH-LIN'  2
c       25='SCH-WIN'  3
c       26='SCH-FAN'  4
c       27='SCH-TEM'  5
c       28='SCH-HUM'  6
c       29='SCH-SIN'  7
c       30='SCH-SOU'  8
c       31='SCH-OCC'  9
c       36='SCH-MET'  10
c       38='SCH-POL'  11
c       45='SCH-MUL'  - <---has no number here

Ch***********************************************************************
        IMPLICIT NONE
        INCLUDE 'comv-uni.inc'
	  INCLUDE 'comv-inp.inc'

        INTEGER   MaxEl,LenZonStr
        INTEGER   LenStr
        PARAMETER (MaxEl=100)
	  PARAMETER (LenZonStr=30)
        INTEGER   i,j, SchTyp, pNam, pTMS
        INTEGER   e1,e2,e3,laste1,iocheck
        INTEGER   F,lenAname,k
        INTEGER   EL3(MaxEl),Nel
        INTEGER   Time2,Jday1,Lstr,Lstr2
        CHARACTER DTstring*31,str*40
C@NBI PGS 2000Jul20 - The NAGware f90 compiler was fussy about *LenZonStr
CC      CHARACTER lastzone(MaxEl)*LenZonStr,SchZone*30
        CHARACTER*(LenZonStr) lastzone(MaxEl)
        CHARACTER SchZone*30
C@NBI PGS 2000Aug02 - Line unused so commented out
CC	  CHARACTER line*70


       lenAname=lenstr(aName)
       f=10**int(log10(REAL(MaName))+1)

C      First store all e3 and for occupant schedules additionally the zone names
C      of one timestep in an array
       pTMS=2
	  laste1=0
10      read(TMS,REC=pTMS,IOSTAT=iocheck) e1,e2,e3
        if (iocheck.NE.0) goto 60
        call ClearI(El3,MaxEl)
        call ClearC(LastZone,MaxEl*LenZonStr)
        Nel=1
C       first and last record have different contents
C       also record with e3= 0 schedulestart and e3=-2 histogramstart
20      if (e3.gt.0) then
C         this is a normal schedule event
	    laste1=e1
          El3(Nel)=e3
          SchTyp=e3/f
          if (schtyp.eq.9) then
C           occupant schedule: get the zone number from DAF
            call GetSchZone(e2,SchZone)
            lastZone(Nel)=SchZone
          endif
          Nel=Nel+1
        endif
        ptms=ptms+1 
        read(TMS,REC=pTMS,IOSTAT=iocheck) e1,e2,e3
        if (iocheck.NE.0) goto 30
        if (e1.eq.laste1) goto 20 

C       Now check the schedule entries of this timestep for double definition          
30      do 50 i=2,Nel-1
          do 40 j=1,i-1
            if (El3(i).eq.El3(j)) then
              CALL CreateJdTim(laste1,Jday1,time2)
              CALL CONVDAT1(Jday1,time2,DTstring)
              Lstr2=lenstr(DTstring)
              SchTyp=El3(i)/f
              pNam  =El3(i)-SchTyp*f
              k=pnam-1
              if (k.ge.0) then
                variab='get schedule name in CheckTMS'
                Call GetWrd(aName,k,lenAname,Str)
              else
                if (schtyp.eq.10) then
                  Str='meteo'
                else
                  Str=' '
                end if
              end if
              Lstr=lenstr(str)

              if (SchTyp.eq.9) then
                if (LastZone(i).eq.LastZone(j))then
                  CALL Error2
     &            ('Your Occupant Schedule '//str(1:lstr)//' has more'//
     &            ' than one event at the same timestep (second)'//
     &            ' at '//DTstring(1:lstr2)//' for the same set of'//
     &            ' zones: '//LastZone(i)(1:lenstr(LastZone(i)))//'.',
     &            'Input DEBUG 3 as &-PR-SIMUlation option to get an'//
     &            ' echo of the contents of TMS in the output.; '//
     &            ' Remove the double definition.',0)
                endif
              else
                CALL Error2('Your Schedule '//str(1:lstr)//
     &          ' has more than one event at the same timestep '//
     &          '(second) at '//DTstring(1:lstr2)//';Input DEBUG '//
     &          '3 as &-PR-SIMUlation option to get an echo of the '//
     &          'contents of TMS in the output.',
     &          'Correct the schedule. Remove the '//
     &          'double events',2)
              end if
            endif
40        continue
50      continue
        goto 10
              
60      continue
        return
        end
C end checkTMS

Ch**********************************************************************
      SUBROUTINE CheOccZo
C***********************************************************************
C Purpose:
C Checks the ZoneIDs used in the occupant schedule
C@empa aw 2000jan04
C@empa aw 2000dec13 New version checks also occupant schedules in 
C                   separate schedule file.  
C@empa aw 2005jun07 check also the activity level and number of occupants 
C
Ch**********************************************************************
      IMPLICIT NONE
      INCLUDE 'comv-uni.inc'
      INCLUDE 'comv-inp.inc'

C MaxSch = number of schedules
C pSch     = pointer to the first record of the schedule in the DAF
C pLSch    = pointer to the last  record of the schedule in the DAF
      INTEGER pSch, pLSch
C@NBI PGS 2000dec23 - "CifEnd" no longer used, so removed
      INTEGER iocheck
      INTEGER LenStr
      INTEGER k,l,i,j,kr,lr
C@empa aw 2005jun07
      INTEGER ONum 
      REAL ActL     
      CHARACTER SchNam*10
      CHARACTER RecLine*160 
      CHARACTER word*80
      CHARACTER ZOcNa*20
      CHARACTER flname*80
      INTEGER p,ZoneNr
      INTEGER LStr1,Lstr2,key
      LOGICAL empty,SchedF

      SchedF=.FALSE.
C     ! 'SCH-OCC' is keywordNr 31. pKeyRec points to the first record in DAF,
C     ! if a schedule is found in the input file
      pSch=pKeyRec(31)
C     ! Find pSchL, the pointer to the last record of the occupant schedule in DAF.    
C     ! I have to do that in a loop because the keywords in the input file can be in 
C     ! arbitrary sequence.
      IF (pSch.NE.0) THEN  !occupant schedule found
         k=9999
C        ! Find the last line of occupant schedule in the direct access file DAF.
C        ! This is done by looking for the start of the first keyword after occupant schedule
C        ! Loop for all keywords 45=keyword number of last schedule (SCH-MUL)
         DO 22 J=1,45
            IF (pKeyRec(J).GT.pSch .AND. pKeyRec(J).LT.k) THEN
               k=pKeyRec(J)
            ENDIF
22       CONTINUE
         pLSch=k-1
C        ! If occupant schedule was the last schedule in DAF, the last line is pDaf1-1. 
C        ! pDaf1 points to the first line in DAF, which comes from a Schedule file.
         IF (pLSch.EQ.9998) pLSch=pDaf1-1
         pSch=pSch+1
         READ(DAF,REC=pSch,ERR=220) RecLine
         SchedF=.FALSE.
         DO WHILE ((pSch.LE.pLSch).AND.(.NOT.(empty(RecLine))))
            l = lenstr(RecLine)
            k=0
            variab='Occupant schedule name/time'
            CALL GETWRD(RecLine,k,l,word)
            IF (index(word,'*').GT.0) then
               SchNam=word(2:)
               variab='Occupant schedule time'
               CALL GETWRD(RecLine,k,l,word)
C@empa aw 2004jun11 Check for F: not only F
CC            ELSE IF (index(word,'F').GT.0) then
            ELSE IF (index(word,'F:').GT.0) then
               variab='Occupant schedule name/time'
               CALL GETWRD(RecLine,k,l,SchNam)
               variab='Occupant schedule file name'
               CALL GETWRD(RecLine,k,l,flname)
               OPEN(TIF,file=flname,STATUS='OLD',ERR=210)
               CALL READTIF(Recline,SchedF,SchNam,flname)
               IF (SchedF) THEN
                  k=0
C@empa aw 2001jan24 also l has to be reset here
                  l = lenstr(RecLine)
                  variab='Occupant schedule time'
                  CALL GETWRD(RecLine,k,l,word)
               ENDIF
60          ENDIF
            IF (.NOT.(empty(recline))) THEN
               variab='Occupant schedule zone list'
               CALL GETWRD(RecLine,k,l,word)
C              ! Replace separators with blanks          
               p=INDEX(word,'/')
               DO WHILE (p.GT.0)
                  word(p:p)=' '
                  p=INDEX(word,'/')
               ENDDO
               p=INDEX(word,'-')
               DO WHILE (p.GT.0)
                  word(p:p)=' '
                  p=INDEX(word,'-')
               ENDDO
C@empa aw 2005jun07 save k and l
               lr=l
               kr=k
               l=len(word)  
               k=0
               variab='Occupant schedule: zonenr'
C              ! Check all referenced zone IDs    
               CALL GETWS(word,k,l,ZOcNa,'')
               DO WHILE (ZOcNa.NE.'') 
                  CALL LookNam(Nz,ZOcNa,ZoTree,ZoTreeN,
     &            ZoneNr,ZoLarg,ZoSmal,key)
                  IF (key.EQ.1) THEN
                     LStr1=LENSTR(SchNam)
                     LStr2=LENSTR(ZOcNa)
                     CALL INERR('&-SCH-OCC:  '//SchNam(1:LStr1)//
     &               ': Zone "'//ZOcNa(1:LStr2)//'" does not '//
     &               'exist in &-NET-ZONes !',RecLine,.FALSE.,2)
                  ENDIF
                  CALL GETWS(word,k,l,ZOcNa,'')
               ENDDO 
C@empa aw 2005jun07 check also activity Level and number of occupants
               
                 
                 variab='Occupant activity level'
                 CALL GETWR(RecLine,kr,lr,ActL,1.0,.true.)
                 IF (ActL.LT.0.)THEN
                     LStr1=LENSTR(SchNam)
                     LStr2=LENSTR(ZOcNa)
                     CALL INERR('&-SCH-OCC:  '//SchNam(1:LStr1)//
     &               ': Activity Level may not '//
     &               'be negative !',RecLine,.FALSE.,2)
                
                 ENDIF
                 variab='Number of occupants'
                 CALL GETWI(RecLine,kr,lr,ONum,1)
                 IF (ONum.LT.0.)THEN
                     LStr1=LENSTR(SchNam)
                     LStr2=LENSTR(ZOcNa)
                     CALL INERR('&-SCH-OCC:  '//SchNam(1:LStr1)//
     &               ': Number of Occupants may not '//
     &               'be negative !',RecLine,.FALSE.,2)
                 ENDIF


               IF (SchedF) CALL READTIF(Recline,SchedF,SchNam,flname)
            
            ENDIF  ! .not.empty(recline)
130         IF (.NOT.SchedF) THEN
               pSch=pSch+1
               IF (pSch.LE.pLSch) READ(DAF,REC=pSch,ERR=220) RecLine
            ENDIF
         ENDDO
         GOTO 150

210      LStr1=LENSTR(SchNam)
         LStr2=LENSTR(flname)
         CALL INERR('&-SCH-OCC:  '//SchNam(1:LStr1)//
     &   ': Schedule file '//flname(1:LStr2)//' not found'//
     &   '!','',.FALSE.,2)
         GOTO 150 

220      LStr1=LENSTR(SchNam)
         CALL INERR('&-SCH-OCC:  '//SchNam(1:LStr1)//
     &   ': Error in DAF '//
     &   '!','',.FALSE.,2)

150   ENDIF    ! occupant schedule found   
      RETURN 
      END


Ch***********************************************************************
      SUBROUTINE READTIF(Recline,SchedF,SchNam,flname)
C***********************************************************************
C   Purpose: reads a next data line  from schedule file.
C
C@empa aw 2000dec22
C
C Pass parameters:
C
C IO # Name         unit   description
C  O 1 Recline             data line
C  O 2 SchedF              flag: TRUE if schedule file is open
C  I 3 SchNam              Schedule name
C  I 4 flname              file name
Ch***********************************************************************

      IMPLICIT NONE
      INCLUDE 'comv-uni.inc'
      INCLUDE 'comv-inp.inc'

C     ! Pass parameter
      CHARACTER RecLine*160 
      CHARACTER SchNam*10
      CHARACTER flname*80
      LOGICAL SchedF

C     ! Local variables  
      INTEGER LenStr
      INTEGER i
      INTEGER LStr1,Lstr2
      LOGICAL empty
      
      SchedF=.FALSE.
20    Recline=''
10    READ(TIF,'(A)',END=200,ERR=210) Recline
      IF (empty(Recline)) GOTO 10
      i=1
      DO WHILE ((Recline(i:i).EQ.' ').OR. (Recline(i:i).EQ.Char(09)))
        i=i+1
      ENDDO
      IF (Recline(i:i).EQ.'#') GOTO 20
      
      SchedF=.TRUE.
      GOTO 220

210   LStr1=LENSTR(SchNam)
      LStr2=LENSTR(flname)
      CALL INERR('&-SCH-OCC:  '//SchNam(1:LStr1)//
     &': Error in schedule file '//flname(1:LStr2)//
     &'!','',.FALSE.,2)


200   CLOSE (TIF)
220   RETURN

      END
      
      

Ch***********************************************************************
C@lbl dml 1999oct28 Replace CHECK1 with function FLWCNV. Change name
CC   to indicate nature of check. Change to function with logical
CC   return type in order to make calling code more transparent.
CC
CC      SUBROUTINE CHECK1(Nz,FMBNEW,FT,EpsFA,EpsFR,LOOP)
CC
CCC***********************************************************************
CCC Purpose: CHECK1 checks the convergence of the norm of the mass flow
CCC   balances, but IF the relative error is acceptable, the iteration
CCC   ends.
CCC
CCC Module : #4.4.4, TG V, MKH/February 7, 1989
CCC Changes: may 26 1989 hcp check with EpsFR relative allowable error
CCC@tno jcp improved if statement and added spare Variables F and fa_
CCC@lbl bs 1991jun04  I had to change the variable fa_ into fa0 because
CCC@lbl I got an error when I compiled it on the VAX (f77).
CCC Limits : N/A
CCC
CCC Pass parameters:
CCC
CCC IO # Name  unit    description
CCC I  1 Nz  [-]    Order of matrix JAC
CCC I  2 FMBNEW(maxz)  [kg/s]    New flow balance vector
CCC I  3 FT(maxz)  [kg/s]    Total flow into each zone
CCC I  4 EpsFA  [kg/s]    Break limit for convergence
CCC I  5 EpsFR  [-/-]    Relative Break limit for convergence
CCC  O 6 LOOP  [-]    Logical key, new loop=.TRUE.
CCC
CCC ERROR RETURN IF: none
CCCh***********************************************************************
CC
CC      include 'comv-par.inc'
CC
CC        INTEGER Nz
CC        DOUBLE PRECISION FMBNEW(maxz)
CC        REAL FT(maxz),EpsFA,EpsFR,fa0,F
CC      LOGICAL LOOP
CC        INTEGER I
CC
CC
CC      LOOP=.TRUE.
CC
CCC Look for a zone with Fmb>EpsFA and Fmb/FT>EpsFR
CCC But IF Fmb<EpsFA THEN EpsFR is not checked. At low flows through the zones
CCC Fmb can meet EpsFA but the relative unbalance can be larger than EpsFR
CC
CC      DO 10 I=1,Nz
CC        fa0=ABS(FmbNew(I))
CC        IF (fa0.GT.EpsFA) THEN
CCC IF EpsA=0 THEN FT(i) could be 0, so we check first before the relative check
CC          F=FT(I)
CC          IF (F.NE.0.0) THEN
CC            IF (fa0/F.GT.EpsFR) GOTO 11
CCC result is not acceptable keep iterating. Loop remains True.
CC          ENDIF
CC        ENDIF
CC10    CONTINUE
CC      Loop=.FALSE.
CC
CC11    RETURN
CC
CC      END
C@lbl/end


Ch***********************************************************************

      logical function FLWCNV( nZ,FNet,FTot,epsFA,epsFR )

C***********************************************************************
C Purpose: Check the convergence of the mass flow balances against
C   both absolute and relative tolerances. Return .TRUE. if converged.
C
C Pass parameters:
C
C IO # Name         unit   description
C I  1 nZ            -     Order of matrix JAC
C I  2 FNet(maxz)   kg/s   Net flow into each zone (flow mass balance)
C I  3 FTot(maxz)   kg/s   Total flow into each zone (sum abs flows)
C I  4 epsFA        kg/s   Absolute limit for convergence
C I  5 epsFR         -     Relative limit for convergence
C
C ERROR RETURN IF: none
Ch***********************************************************************

      IMPLICIT NONE
      include 'comv-par.inc'

C     ! Arguments.
      integer nZ
      double precision FNet(maxz)
      real FTot(maxz),epsFA,epsFR

C     ! Local variables.
      integer i
      double precision fNetZone

C     !   If any zone has FNet>epsFA and also FNet/FTot>epsFR, then
C     ! airflow network has not converged, so return FALSE. Note
C     ! may have FTot=0, but then FNet also zero.

      flwcnv = .FALSE.

      do 10 i=1, nZ
        fNetZone = abs( FNet(i) )
        if( fNetZone .gt. epsFA ) then
          if( fNetZone .gt. FTot(i)*epsFR ) return
        endif
10    continue

      flwcnv = .TRUE.
      return
      end
C     ! End function FLWCNV().


Ch***********************************************************************
C@lbl dml 1999oct28 Replace CHECK2 with function PRSSTG. Change name
CC   to indicate nature of check. Change to function with logical
CC   return type in order to make calling code more transparent.
CC
CC      SUBROUTINE CHECK2(Nz,PzNew,PZOLD,EpsCJ,LOOP)
CC
CCC***********************************************************************
CCC Purpose: CHECK2 checks the convergence of the norm of pressures
C
CCC Module : #4.4.7, TG V, MKH/February 6, 1989
CCC Changes: none
CCC Limits : N/A
C
CCC
CCC Pass parameters:
C
CCC
CCC IO # Name   unit      description
CCC I  1 Nz   [-]      Number of zones
CCC I  2 PzNew(maxz)[Pa]      New pressure for zones
CCC I  3 PZOLD(maxz)[Pa]      Old pressure for zones
CCC I  4 EpsCJ   [Pa]      Break limit for convergence
CCC  O 5 LOOP   [-]      Logical key, new loop=.TRUE.
C
CCC
CCC ERROR RETURN IF: none
CCCh***********************************************************************

CC        IMPLICIT NONE
CC
CC      include 'comv-par.inc'

CC
CC        INTEGER Nz
CC        DOUBLE PRECISION PzNew(maxz),PZOLD(maxz)
CC        REAL EpsCJ
CC      LOGICAL LOOP

CC
CC        REAL MAX,DIFF
CC        INTEGER I

CC
CC      LOOP=.TRUE.
CC      MAX=0.0

CC
CC      DO 10 I=1,Nz
CC      DIFF=ABS(PzNew(I)-PZOLD(I))
CC      IF (DIFF.GT. MAX) MAX=DIFF
CC10    CONTINUE
CC
CC      IF (MAX .LT. EpsCJ)  LOOP=.FALSE.
CC
CC      RETURN
CC      END


Ch***********************************************************************

      logical function PRSSTG( nZ,PzNew,PzOld,epsCJ )

C***********************************************************************
C Purpose: Check for stagnation in the changes made to the pressure
C   vector. Return .TRUE. if stagnated.
C     This function reproduces subroutine CHECK2. The description
C   given there, "checks the convergence of the norm of pressures",
C   incorrectly implies that small changes to the pressures means the
C   solver has converged. In fact, it means the solver took such a
C   short step that the flows may not register any change at the new
C   pressures.
C     This function also improves the implementation CHECK2, by
C   returning as soon as it establishes that stagnation has not
C   occurred. This avoids stepping through the entire vector.
C     Note one could improve the efficiency of this function by passing
C   the current pressure vector and the changes, rather than two
C   pressure vectors. This would avoid subtracting to find a number
C   that was added only a few machine cycles ago, in the solver.
C
C Pass parameters:
C
C IO # Name         unit  description
C I  1 nZ            -    Number of zones
C I  2 PzNew(maxz)   Pa   New pressure for zones
C I  3 PzOld(maxz)   Pa   Old pressure for zones
C I  4 epsCJ         Pa   Break limit for convergence
C
C ERROR RETURN IF: none
Ch***********************************************************************

      IMPLICIT NONE
      include 'comv-par.inc'

C     ! Arguments.
      integer nZ
      double precision PzNew(maxz),PzOld(maxz)
      real epsCJ

C     ! Local variables.
      integer i

C     !   If any zone has change in pressures > epsCJ, then not
C     ! stagnated, so return FALSE.

      prsstg = .FALSE.

      do 10 i=1, nZ
        if( abs(PzNew(i)-PzOld(i)) .ge. epsCJ) return
10    continue

      prsstg = .TRUE.
      return
      end
C     ! End function PRSSTG().


Ch***********************************************************************

	SUBROUTINE CheckPb(Pb,H,Var1)

C***********************************************************************
C Purpose: CheckPb checks the barometric pressure to be within +/- 5 kPa from
C          the standard barometric profile
C          Tmeteo (C) and ODPbMet*1000 (Pa) is assumed at sealevel.
C          (ODPbMet=Original Default for PbMet)
C          From sealevel to the given height H the expected barometric pressure
C          is calculated with function PBarom.
C          This expected value is compared with the passed Pb
C          The routine InErr is used for
C            warnings (+/-  5 kPa off)
C            errors   (+/- 10 kPa off)
C          CheckPb is used for Barometric pressures given at Meteo Schedules
C                              Barometric Standard pressure for Crack data
C          CheckPb could give an indication that Barometric pressure is input
C          in the wrong unit, it should be kPa.
C
C
C Pass parameters:
C
C IO # Name   unit          description
C I  1 Pb     [Pa]          Barometric pressure Internal units
C I  2 H      [m]               altitude above sea level
C ERROR routine Inerr is called
Ch***********************************************************************

        IMPLICIT NONE
	INCLUDE 'comv-inp.inc'
        INTEGER LenStr

        REAL Pb,H
        Character*(*) Var1
        REAL RhoSea,PbCheck,zref1,Difference
        Character*40 Var,Expected,PbStr
        INTEGER L,L2,L3

        REAL RhoF,GF

        L=lenstr(Var1)
        Var=Var1(1:L)

C air density at sea level assuming meteo conditions but standard pressure
	RhoSea=RhoF(ODPbMet*1000,Tmet,Xhmet)

C calculate the 'ideal' PbCheck pressure from sealevel to given Height H
C but first check if G has been assigned.
        if (g.lt.9.0 .or. g.gt.10.5) then
C G is not assigned. Just get a value here close to REALity or 9.8 m/s2
          if (Lat.gt.-90 .and. Lat.lt.90) then
C there is a Latitude that makes sense
            G=GF(Lat)
          else
C@tno jcp 2001oct16 gravity constant corrected a bit
CC            G=9.8065
            G=9.80665
          end if
        end if

C@tno jcp 1996Apr07_14:16:06 call pbarom with variable, not constant 0
        zref1=0
	CALL PBAROM(PbCheck,H,Zref1,ODPbMet*1000,RhoSea,G)

        IF (Pb.LT.0.0)THEN
              CALL INERR(Var(1:L)//' pressure',
     &        ' is below 0 '//
     &        ' Input should be about 100 kPa',.FALSE.,2)
        ENDIF

        Difference=Pb-PbCheck
        Call RelDis(PbCheck/1000,8,Expected,L2,0)
        Call RelDis(Pb/1000,8,PbStr,L3,0)

        IF (Difference.GT.10000)THEN
              CALL INERR(Var(1:L)//' pressure='//
     &        PbStr(1:L3)//'kPa.',
     &        ' Value is more than 10kPa above expected '//
     &        Expected(1:L2)//'kPa !',.FALSE.,2)
        ELSE
        IF (Difference.GT.5000)THEN
              CALL INERR(Var(1:L)//' pressure='//
     &        PbStr(1:L3)//'kPa.',
     &        ' Value is more than 5kPa above expected '//
     &        Expected(1:L2)//'kPa !',.FALSE.,1)
          ENDIF
        ENDIF

        IF (Difference.LT.-20000)THEN
              CALL INERR(Var(1:L)//' pressure='//
     &        PbStr(1:L3)//'kPa.',
     &        ' Value is more than 20kPa below expected '//
     &        Expected(1:L2)//'kPa !',.FALSE.,2)
        ENDIF

	RETURN
	END
