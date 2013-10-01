C+*********************************************************** comv-tm2.f
Ch**********************************************************************

C@NBI PGS 1999Aug10 - 'interval' is now in the INCLUDE file
CC      SUBROUTINE TimeStep(Nr,TLotus,interval,chgkey,last)
        SUBROUTINE TimeStep(Nr,TLotus,chgkey,last)
C***********************************************************************
C
C Comments only:
C
C   The following temporary variables are eventually copied into
C       the actual variables in XMETDAT
C
C   bufvm = wind velocity [m/s]
C   bufdm = wind direction  [deg]
C   bufxh = absolute humidity [g H2O/kg Air]
C   bufpb = barometric pressure [kPa]
C
C lbl bs 25mar, 1991
C
C Changes:
C@empa vd 1992may6  Array Cout(MaxC) reintroduced
C@empa vd 1992jun16 Input for Xhz schedule is in gr/kg ,Data converted to kg/kg
C@empa 1992Oct26 vd the window opening schedule does affect Mf not Ldat!
C@empa aw 1992oct29 change the factor for SchTyp
C@NBI PGS 1999Jun30 - Tidied up routine to make it more readable
C@NBI                 (converted all tabs to spaces, 8-spaces in left column,
C@NBI                 thereafter 2 spaces per indentation level)
C
C Purpose:
C Reads a record from the TMS file and changes the data according to
C the schedule line
C
C Parameters:
C
C (I/O) Nr     = number of the time step
C (O) TLotus     = time in LOTUS format
C (O) chgkey   = Change Key, indicator if the program has to run the
C                ventilation part again (chgkey = 1) or not (chgkey = 0).
C (O) interval = distance in seconds from the current to the next
C                timestep.  This variable is in INCLUDE file comv-inp.inc
C (O) last     = if last == 1 this was the last run of the timeloop
C
C At the moment chgkey is not used actually. For every schedule type the
C variable is set to 1, so that the ventilation part is always performed.
C@tno jcp 1996Jun21_17:17:27 definition of the schedule numbers definition
C keyword  name     schedule number
C       1 =' CIF'     -
C       2 ='PR-IDEN'  -
C       3 ='PR-SIMU'  -
C       4 ='PR-CONT'  -
C       5 ='NET-AIR'  -
C       6 ='CR'       -
C       7 ='FA'       -
C       8 ='DS'       -
C       9 ='DF'       -
C       10='F1'       -
C       11='F2'       -
C       12='F3'       -
C       13='F4'       -
C       14='WI'       -
C       15='TD'       -
C       16='TRANSIT'  -
C       17='NET-HVA'  -
C       18='NET-ZON'  -
C       19='NET-ZL'   -
C       20='NET-ZP'   -
C       21='NET-EXT'  -
C       22='NET-LIN'  -
C       23='SCH-MAI'  01
C       24='SCH-LIN'  02
C       25='SCH-WIN'  03
C       26='SCH-FAN'  04
C       27='SCH-TEM'  05
C       28='SCH-HUM'  06
C       29='SCH-SIN'  07
C       30='SCH-SOU'  08
C       31='SCH-OCC'  09
C       32='CP-BUIL'  -
C       33='CP-VALU'  -
C       34='ENV-BUI'  -
C       35='ENV-WIN'  -
C       36='SCH-MET'  10
C       37='POL-DES'  -
C       38='SCH-POL'  11
C       39='OCCUPAN'  -
C       40='CPR-BUI'  -
C       41='CPR-FAC'  -
C       42='CPR-WIN'  -
C       43='3D-BUIL'  -
C       44='NORM-CR'  -
C       45='SCH-MUL'  - <---has no number here
C       46='USER'     -
C       47='NET-ZT'   -
C       48='PR-OUTP'  -
C       49='PR-UNIT'  -
C       50='WALL-MA'  -
C       51='WALL-TY'  -
C       52='NET-WAL'  -
C       53='PE'       -
C       54='RF'       -
C       55='HISTO'    -
C       56='PS'       -
C       57='EQN-WIN'  -
C       58='POL-FIC'  -
C@NBI PGS 2003Apr28 - New component type: Thermostatic vent
C       59='TV'       -

Ch**********************************************************************
        IMPLICIT NONE
        INCLUDE 'comv-uni.inc'
        INCLUDE 'comv-inp.inc'

        INTEGER Nr,DafRec,SchTyp,e1,e2,e3,e4,pNam,k,eflag
C@NBI PGS 1999Aug10 - 'interval' is now in the INCLUDE file
C@empa aw 1996nov26 zonenr is an array
CC      INTEGER zonenr,interval
        INTEGER zonenr(MaxZ)
        INTEGER chgkey,nrcall,i,j,l,INTCON, iocheck
        INTEGER LS
        INTEGER LenStr
        REAL RELCON
        INTEGER IcIndex

        LOGICAL last
C@tno jcp 1996Apr08_14:43:20 OccNum1 added
        INTEGER f,OccNum1,ipol
        CHARACTER Line*160, word*30, datword*30, SchNam*30,DUM*30
        CHARACTER oldsch*10, newsch*10, elem*30
C@NBI PGS 2000Aug02 - "list" no longer used so removed from decralations
CC      CHARACTER list*400
        CHARACTER DumStr*40
C@empa aw 1999nov23 ZOcNa
C@empa aw 2001jan30 MaxOStr
      CHARACTER ZOcNa*20, ZOc2Na*40, MaxOStr*2
C@tno jcp 1996Jun26_18:38:51 INTEGER kw, lw added (length of a word)
        INTEGER LenDum,LSchNam,C1Pol,lw,kw
C@empa aw 1999nov26 p,startNr,endNr,iz,zNr,k2,l2,LStr1,Lstr2,key
        INTEGER p,startNr,endNr,iz,niz,Znr,k2,l2,LStr1,Lstr2,key
        DOUBLE PRECISION TLotus,createtm
C@tno jcp 1996Apr28_09:20:11 spare variable for TLotus
        DOUBLE PRECISION TLotus1
        REAL bufvm,buftm,bufdm,bufxh,bufpb
C@NBI PGS 1996Jun30 - Bugfix: Save weather data between calls (because
C@NBI                 some compilers don't save by default).
C@NBI                 This is necessary because XMETDAT called before FILLBUF
        SAVE bufvm,buftm,bufdm,bufxh,bufpb
C@tno jcp 1996Apr15_12:22:11 Value added for output in new routine EchoSCH
        REAL actfct,conc,Value,opening
        INTEGER iCol1ABC
C@NBI PGS 1999Aug05 - Added 'Tout_mean'.  See comment below
        REAL Tout_mean
        COMMON/Tout_mean/Tout_mean
C@NBI PGS 1999Aug05   (end)
        chgkey=0
        READ(TMS,REC=Nr) e1,e2,e3

C       e1 = time in seconds since , e2 = record number , e3 = SchTyp & SchNam
C            start of simulation          in DAF
C@tno jcp 1996Apr25_14:28:45 look at the ELSE part that has been changed to get
C       e3 =  0 -> schedules active
C       e3 = -1 -> end
C       e3 = -2 -> start histograms


C-------------------------------------
C       Change data by using schedules
C-------------------------------------

111     IF (e3.GT.0) THEN

C@tno jcp 1996Apr29_18:00:01 needed TLotus already here, to be used at EchoSch
          TLotus=CREATETM(e1)
          if (multifile .ne.' ' .and. multischedtime.lt.compday) then
            if (multischedtime .lt. e1) then
C             Multischedule is the first
              TLotus=CREATETM(multischedtime)
              interval=multischedtime
c             write(cof,*)'interval=multischedtime',interval
              chgkey = 1
              Call DoMulti()
              interval=MIN(multischedtime,e1)-interval
c             write(cof,*)'interval=MIN(multischedtime,e1)-interval',interval
              Nr=Nr-1
              RETURN
            endif
          else
            IF ((metfile.NE.' ') .AND. (e1.GT.compday)) THEN
C             change only meteo data and do not increase Nr
              TLotus=CREATETM(compday)
              interval=compday
c             write(cof,*)'interval=compday',interval
              chgkey=1
              if (multischedtime .eq. compday) then
C               a multischedule at the same TLotus
C               (if there is any meteodata in it it gets overwritten by
C               the data in the meteofile)
                Call DoMulti()
              endif
C@NBI PGS 1999Aug09 - Introduced 'Tlotus' so that EchoSch can be called
C@empa aw 2004jan26 meteo buffer is empty in the first timestep
              IF (bufpb.eq.0.) THEN
                 CALL FILLBUF(bufvm,bufdm,buftm,bufxh,bufpb)
                   BACKSPACE(IFS)
              ENDIF
         
              CALL XMETDAT(bufvm,bufdm,buftm,bufxh,bufpb,Tlotus)
              CALL FILLBUF(bufvm,bufdm,buftm,bufxh,bufpb)
              interval=MIN(compday,e1)-interval
c             write(cof,*)'interval=MIN(compday,e1)-interval',interval
              Nr=Nr-1
              RETURN
            ENDIF
          endif

          if (multifile.ne.' ' .and. e1.EQ. multischedtime) THEN
C           multi-schedules at the same TLotus as others
            chgkey=1
            Call DoMulti()
          endif

          IF (metfile.NE.' ' .AND. e1.EQ.compday) THEN
C           change meteo data at the same TLotus as other data
            chgkey=1
C@empa aw 2004jan26 meteo buffer is empty in the first timestep
            IF (bufpb.eq.0.) THEN
               CALL FILLBUF(bufvm,bufdm,buftm,bufxh,bufpb)
                 BACKSPACE(IFS)
            ENDIF
C@NBI PGS 1999Aug09 - Introduced 'Tlotus' so that EchoSch can be called
            CALL XMETDAT(bufvm,bufdm,buftm,bufxh,bufpb,Tlotus)
            CALL FILLBUF(bufvm,bufdm,buftm,bufxh,bufpb)
          ENDIF

          DafRec=e2
          f=10**int(log10(REAL(MaName))+1)
          SchTyp=e3/f
          pNam  =e3-SchTyp*f
          READ(DAF,REC=DafRec) Line
          l=lenstr(Line)

          call KeepLine(Line)


C         1 = main schedule
C         -----------------

          IF (SchTyp.EQ.1) THEN
C@NBI PGS 1999Aug09 - This is redundant code; &-SCH-MAI isn't yet implemented
            chgkey=1
            k=0
            CALL GETWRD(Line,k,l,word)
C@tno jcp 1996Jun13_23:48:56 added variab= before getWS sortopt
            variab='the name of the old schedule'
            CALL GETWS(Line,k,l,oldsch,' ')
C@tno jcp 1996Jun13_23:48:56 added variab= before getWS sortopt
            variab='the name of the new schedule'
            CALL GETWS(Line,k,l,newsch,' ')
C@tno jcp 1996Jun13_23:48:56 added variab= before getWS sortopt
            variab='the range to apply this schedule to'
            CALL GETWS(Line,k,l,elem,'ALL')
            IF (oldsch.EQ.' ' .OR. newsch.EQ.' ') THEN
              CALL ERROR2('&-SCH-MAI: Old or new schedule name '//
     &        'is missing',line,2)
              GOTO 900
            ENDIF
            IF (oldsch(1:1).NE.newsch(1:1)) THEN
              CALL ERROR2('&-SCH-MAI: Schedule type mismatch',line,2)
              GOTO 900
            ENDIF
            CALL XSCHED(oldsch,newsch,elem)
            GOTO 900
          ENDIF


C         2 = link exchanger schedule
C         ---------------------------

          IF (SchTyp.EQ.2) THEN
C           link exchanger is not implemented
            GOTO 900
          ENDIF

C         Get the length of the aName array
          LS=LENSTR(aName)


C         3 = window schedule
C         -------------------

          IF (SchTyp.EQ.3) THEN
            chgkey=1
            IF (pNam.GT.0) THEN
              k=pNam-1
              CALL GETWRD(aName,k,ls,word)
              SchNam=word(2:)
              CALL GETLAST(Line,datword)
C@tno jcp 1996Jun17_13:13:36 line moved up out of loop 30
              variab='value for window schedule'
              value=RELCON(datword,eflag)

C@tno jcp 1996Jun17_13:12:51 new part for A27 Window opening factor routine
              if (index(SchNam,':A27').gt.0) then
C               Routine A27 is found as a user defined routine un comv-usr.f
C               the second parameter (1) is used to limit the window to a few
C               fixed positions
                call A27(opening,1)
                value=value*opening
C@NBI PGS 1999Aug05 - Joined 3 mutually exclusive IF-ENDIFs using ELSEIF
C@NBI                 Makes code run a weeny bit faster
CC            end if
CC            if (index(SchNam,':U01').gt.0) then
              elseif (index(SchNam,':U01').gt.0) then
                call U01(opening,1)
                value=value*opening
CC            end if
CC            if (index(SchNam,':U02').gt.0) then
              elseif (index(SchNam,':U02').gt.0) then
C@NBI PGS 1999Aug05   (end of conversion to ELSEIF)
                call U02(opening,1)
                value=value*opening
              end if
C@tno jcp 1996Jun17_13:12:51 end new part for A27

              DO 30 I=1,Nl
                IF (SoL(I).EQ.SchNam) THEN
C@tno jcp 1996Jun17_13:13:08 line moved to before call of A27 opening routine
CC                value=RELCON(datword,eflag)
                  Mf(I)=value
                  if (outputoptions(10).gt.0) then
                    call echosch(TLotus,SchNam,value,mf(i))
                  end if
                ENDIF
30            CONTINUE
            ENDIF
            GOTO 900
          ENDIF


C         4 = fan schedule
C         ----------------

          IF (SchTyp.EQ.4) THEN
            chgkey=1
            IF (pNam.GT.0) THEN
              k=pNam-1
              CALL GETWRD(aName,k,ls,word)
              SchNam=word(2:)
              CALL GETLAST(Line,datword)
              DO 40 I=1,Nl
                IF (SoL(I).EQ.SchNam) THEN
                  variab='value for link schedule'
                  Value=RELCON(datword,eflag)
                  mf(i)=OFanSp(I)*value
                  if (outputoptions(10).gt.0) then
                    call echosch(TLotus,SchNam,value,mf(i))
                  end if
                ENDIF
40            CONTINUE
            ENDIF
            GOTO 900
          ENDIF


C         5,6,7,8 = Temperature, Humidity, Sink and Source
C         ------------------------------------------------

          IF (SchTyp.GT.4 .and. SchTyp.LT.9) THEN

            chgkey=1
            IF (pNam.GT.0) THEN
              k=pNam-1
              CALL GETWRD(aName,k,ls,word)
              SchNam=word(2:)
              LSchNam=Lenstr(SchNam)
              CALL GETLAST(Line,datword)
              value = RELCON(datword,eflag)
c             write(cof,*)'value from schedule=',value

              DO 50 I=1,Nz

                if (ICindex(SchZ(i),SchNam(1:Lschnam)).gt.0) then
                  GOTO (51,6,7,8) SchTyp-4


C                 5 = Temperature
C                 ---------------

 51               continue
C@NBI PGS 1999Aug05 - Added possibility to control temperature by equation
C@NBI PGS 2001Apr14 - Added more possibilities to control temperature by
C@NBI                 linear equation. Now (i) funcion of monthly mean
C@NBI                 outside temperature, e.g. adaptive thermal comfort
C@NBI                 model, and (2) function of hourly outside temperature.
C@NBI               - And now you can have more than one function, each one
C@NBI                 has its own line in &-USER datablock.
C@NBI               - Also changed the name of the special function-
C@NBI                 schedule T:U01 to T:Mx were x is line number in
C@NBI                 &-USER datablock

                  if (SchNam(2:2).EQ.':') THEN
C@NBI                Here zone temperature can be set as a linear function
C@NBI                of outside temperature:
C@NBI                  Tzone = A + B * Tout
C@NBI                Where the following come from &-USER input datablock:
C@NBI                - Row j, Column 1   :   Coefficient A
C@NBI                - Row j, Column 2   :   Coefficient B
C@NBI                Moreover, you can specify MAX and MIN limits for heating/cooling:
C@NBI                - Row j, Column 3   :   MAX top limit temperature (cooling setpoint)
C@NBI                - Row j, Column 4   :   MIN below outdoor temp (-ve is below; +ve above)
C@NBI                                        (set to -100°C if you don't want to use this)
C@NBI                - &-SCH-TEM Value   :   MIN heating set point (can be changed hourly, e.g. night setback)
C@NBI                - In the schedule's "temperature" column  :  MIN limit.
C@NBI                  Thus a different MIN value can be specified for
C@NBI                  any hour, e.g. for night-time / weekend setback.

C                    j is row number in &-USER datablock for the coefficients.
                     READ(SchNam(4:4),*) j

                     if (SchNam(3:3).EQ.'M') THEN
C@NBI                 - Schedule "T:Mx" where x is line number in &-USER
C@NBI                   datablock, so there can be up to 9 such schedules.
C@NBI                 - Here "Tout" is a fading memory mean outside
C@NBI                   temperature which is equivalent to MONTHLY MEAN.
C@NBI                 - This is ideal for specifying a function for optimum
C@NBI                   comfort temp. according to adaptive thermal comfort
C@NBI                   model, e.g. ASHRAE RP-884 database. For nat.vent.bldgs.
C@NBI                      Tcomfort = 17.8 + 0.31 * Tout_mean
C@NBI                   Where coefficients 17.8 and 0.31 are given in the
C@NBI                   &-USER input datablock.
C@NBI                  -Tout_mean changes rather slowly, so it should suffice
C@NBI                   to for Tz to be updated a couple or more times per day.
                        Tz(i)=UserDat(j,1)+UserDat(j,2)*Tout_mean

                     elseif (SchNam(3:3).EQ.'H') THEN
C@NBI                 - Schedule "T:Hx" where x is line number in &-USER
C@NBI                   datablock, so there can be up to 9 such schedules.
C@NBI                 - Here "Tout" is the hourly outside temperature.
C@NBI                 - The schedule should have an entry for each hour
C@NBI                   to ensure that zone temperatures are updated for
C@NBI                   hourly changes in outside temperature.
                        Tz(i)=UserDat(j,1)+UserDat(j,2)*Tout
                     endif

C                    Limit within MIN and MAX heating & cooling set-points
                     Tz(i)=MIN(UserDat(j,3)
     &                    ,MAX(Value,UserDat(j,4)+Tout,Tz(i)))

                  else
C                   standard schedule
                    Tz(i)=value*ifact(UnitTMul) + ifact(UnitToff)
                  endif
C@NBI PGS 1999Aug05   (end of patch)
C@NBI PGS 2001Apr14   (end of expanded code)
                  if (outputoptions(10).gt.0) then
                    call echosch(TLotus,SchNam,value,Tz(i))
                  end if
                  goto 50


C                 6 = Humidity
C                 ------------

  6               continue
                  if (value .lt. 0) goto 920
C@NBI PGS 1999Aug05 - Added possibility to control humidity by equation
CC                XHz(i) = value*ifact(UnitXh)
C@NBI PGS 2001Apr14 - Changed name of special function schedule H:U01 to H:H
                  if (SchNam(2:3).EQ.':H') THEN
C@NBI                 XHz = Outside humid. + Humid rise set in schedule.
C@NBI                 To make sure that XHz is recalculated every timestep,
C@NBI                 the :M schedule must have one row for each hour of day.
                    XHz(i) = Xhmet + Value*ifact(UnitXh)
C@NBI PGS 1999Sep21 - Commented out : Option for heat recovery with 50% moisture recaim
CC                    XHz(i) = 2.0*XHz(i)
                  elseif (SchNam(2:3).EQ.':P') THEN
C@NBI                 XHz = Concentration calculated by pollution transport
C@NBI                 model for H20.
C@NBI                 To make sure that XHz is recalculated every timestep,
C@NBI                 the :P schedule must have one row for each hour of day.
C@NBI                 The limitation with this method is that C() is value from
C@NBI                 previous timestep.  Should ideally be a coupled solution.
                    If(UseMetH2O.EQ.1) Xhz(i) = C(ipolh2O,i)
                  else
C                   standard schedule
                    XHz(i) = value*ifact(UnitXh)
                  endif
C@NBI PGS 1999Aug05   (end of patch)
                  if (outputoptions(10).gt.0) then
                    call echosch(TLotus,SchNam,value,XHz(i))
                  end if
                  goto 50


C                 7 = Sink
C                 --------

  7               continue
                  if (value .lt. 0) goto 920
                  do 71 ipol=1,Nconc
                    C1Pol=iCol1ABC(2,SchNam)
C                   Dum contains the partial string starting at C1Pol
                    DUM=SchNam(C1Pol:lenstr(SchNam))
                    if (ICindex(CName(ipol),DUM).GT.0) then
C@empa aw 2000apr06 This is definitely wrong! As schedule value is a factor,
C@empa aw 2000apr06 it is independent on input unit.
CC                      Sink(ipol,i) = OSink(ipol,i) * value *
CC     &                  ifact(UnitPSin)
                      Sink(ipol,i) = OSink(ipol,i) * value
                      if (outputoptions(10).gt.0) then
                        call echosch(TLotus,SchNam,value,Sink(ipol,i))
                      end if
                    endif
  71              continue
C@empa aw 2000jul05 goto 50 was missing
                  goto 50


C                 8 = Source
C                 ----------

  8               continue
                  if (value .lt. 0) goto 920
                  do 81 ipol=1,Nconc
                    C1Pol=iCol1ABC(2,SchNam)
C                   Dum contains the partial string starting at C1Pol
                    DUM=SchNam(C1Pol:lenstr(SchNam))
                    if (ICindex(CName(ipol),DUM).GT.0) then
C@empa aw 2000apr06 This is definitely wrong! As schedule value is a factor,
C@empa aw 2000apr06 it is independent on input unit.
CC                      Source(ipol,i) = OrSource(ipol,i) * value *
CC     &                  ifact(UnitPSou)
                      Source(ipol,i) = OrSource(ipol,i) * value
                      if (outputoptions(10).gt.0) then
c                       write(cof,*)'ipol=',ipol,
c     &                 'OrSource=',OrSource(ipol,i)
                        call echosch(TLotus,SchNam,value,Source(ipol,i))
                      end if
                    endif
  81              continue

C                 If not a source schedule then it is a new number of occupants
C                 of occ.type 1 is datword (has to be done***)
C@empa aw 2000jan19 I think we should leave that and cancel the corresponding
C@empa entry in the UG, because this option would interfere with &-SCH-OCC.
C@empa With &-SCH-OCC we have already all possibilities to realize an occupant
C@empa as source in a zone.

C               end if index(SchZ(I..)>
                endif
 50           continue
C           end if PNam>0
            end if
            GOTO 900
C         end if from (SchType.LT.9)
          endif


C         9 = occupant schedule
C         ---------------------

C@tno jcp 1996Apr08_14:23:22 reading occupant schedule NOT according to input:CH
          IF (SchTyp.EQ.9) THEN
            chgkey=1
            IF (pNam.GT.0) THEN
              k=pNam-1
C@tno jcp 1996Apr08_14:21:24 added message
              variab='Occupant schedule name'
              CALL GETWRD(aName,k,ls,word)
              SchNam=word(2:)
              k=0

              variab='Occupant schedule name/time'
              CALL GETWRD(Line,k,l,word)
              variab='Occupant schedule time'
              IF (index(word,'*').GT.0) CALL GETWRD(Line,k,l,word)
              variab='Occupant schedule zones'
C@empa aw 1999nov26 ALL is not implemented, we rather take ' '
CC		  CALL GETWS(Line,k,l,datword,'ALL')
                  CALL GETWS(Line,k,l,datword,' ')
              variab='Occupant schedule: actfct'
              CALL GETWR(Line,k,l,actfct,1.0,.TRUE.)

C@tno jcp 1996Apr08_14:41:20 added: read number of occupants from the schedule
C@tno jcp 1996Jun14_00:20:38 variab for GetWI
              variab='get number of occupants'
              CALL GETWI(Line,k,l,OccNum1,1)
C this remains compatible with older CIF files, that had no number of occupants
C but included that in the activity factor.

C@empa aw 1999nov26 We want to use ID's not sequence numbers. Look for the Zone ID's
C@empa              in ZoTree
CC		  list=' '
CC		  CALL RDLIST(3,datword,list)
CC		  l=len(list)
85          CONTINUE
                p=INDEX(datword,'/')
                IF (p.GT.0) THEN
                  datword(p:p)=' '
                GOTO 85
              ENDIF
            l=len(datword)
              k=0
C@tno jcp 1996Apr08_14:44:05 occupant sequence number
C@empa aw 2001jan30 give  clear error messages
              LSchNam=Lenstr(SchNam)
              variab='Occupant type number in "'
     &        //schnam(1:LSchNam)//'"'
              J=INTCON(SchNam(4:LSchNam),eflag)
              IF ((J.LT.1).OR.(J.GT.MaxO)) THEN
                 LStr1=LENSTR(aName)
                 CALL INTDIS(MaxO,MaxOStr,Lstr2)
                 CALL INERR('&-SCH-OCC:  '//aName(1:LStr1)//
     &           ': ''OCCn'' Occupant type number n has to be: '//
     &           '1 <= n <= '//MaxOStr(1:LStr2) //'!',Line,.FALSE.,3)
              ENDIF
              variab='Occupant schedule: zonenr/list'
CC90		  CALL GETWI(list,k,l,zonenr,0)
            iz=1
90		  CALL GETWS(datword,k,l,ZOcNa,'')
            IF (ZOcNa.NE.'') THEN
                p=INDEX(ZOcNa,'-')
              IF (p.GT.0)THEN
                  ZOcNa(p:p)=' '
                  ZOc2Na=ZOcNa
                  k2=0
                  l2=len(ZOc2Na)
                      CALL GETWS(ZOc2Na,k2,l2,ZOcNa,'')
                  CALL LookNam(Nz,ZOcNa,ZoTree,ZoTreeN,
     &	      startNr,ZoLarg,ZoSmal,key)
                IF (key.EQ.1) THEN
                  LStr1=LENSTR(aName)
                  LStr2=LENSTR(ZOcNa)
                  CALL INERR('&-SCH-OCC:  '//aName(1:LStr1)//
     &            ': Zone "'//ZOcNa(1:LStr2)//'" does not '//
     &            'exist in &-NET-ZONes !',Line,.FALSE.,2)
                  GOTO 90
                ENDIF
                      CALL GETWS(ZOc2Na,k2,l2,ZOcNa,'')
                  CALL LookNam(Nz,ZOcNa,ZoTree,ZoTreeN,
     &	      endNr,ZoLarg,ZoSmal,key)
                IF (key.EQ.1) THEN
                  LStr1=LENSTR(aName)
                  LStr2=LENSTR(ZOcNa)
                  CALL INERR('&-SCH-OCC:  '//aName(1:LStr1)//
     &            ': Zone "'//ZOcNa(1:LStr2)//'" does not '//
     &            'exist in &-NET-ZONes !',Line,.FALSE.,2)
                  GOTO 90
                ENDIF

                DO 95 zNr=startNr,endNr,sign(1,(endNr-startNr))
                    ZoneNr(iz)=zNr
                    iz=iz+1
95              CONTINUE
                ELSE
                  CALL LookNam(Nz,ZOcNa,ZoTree,ZoTreeN,
     &	      ZoneNr(iz),ZoLarg,ZoSmal,key)
                IF (key.EQ.1) THEN
                  LStr1=LENSTR(aName)
                  LStr2=LENSTR(ZOcNa)
                  CALL INERR('&-SCH-OCC:  '//aName(1:LStr1)//
     &            ': Zone "'//ZOcNa(1:LStr2)//'" does not '//
     &            'exist in &-NET-ZONes !',Line,.FALSE.,2)
                  GOTO 90
                ENDIF

                  iz=iz+1
              ENDIF
                GOTO 90
              ENDIF
              niz=iz-1
            DO 100 iz = 1, niz
                    IF (zonenr(iz).GT.0) THEN
C@tno jcp 1996Apr23_17:43:56 here again add and reset occupants)activity
C  factor) to OccAct
                     OccAct(j,zonenr(iz))=actfct
C@tno jcp 1996Apr08_14:44:48 also assign the array OccNum(*,*)
                     OccNum(j,zonenr(iz))=OccNum1
                     IF (outputoptions(10).gt.0) THEN
c                       write(*,*) 'schnam',schnam
                       CALL echosch4(TLotus,SchNam,zonenr(iz),
     &                  actfct,occnum1)
                     ENDIF
C                    write(cof,*) TLotus,'occupant',J,' to zone',zonenr
               ENDIF
100         CONTINUE
                  ENDIF
C@empa aw nov26 End of patched lines for Zone ID's instead of sequence numbers
              GOTO 900
            ENDIF


C         10 = meteo schedule
C         -------------------

          IF (SchTyp.EQ.10) THEN
C@tno jcp 1996Apr25_13:23:10 added for EchoSch
            SchNam='meteo'
            chgkey=1
            k=0
            variab='Meteo schedule: Meteo time'
            CALL GETWS(Line,k,l,MetTime,DMetTime)
            variab='Meteo schedule: V wind'
            CALL GETWR(Line,k,l,Vmet,DVmet,.TRUE.)
C           Check limits on VMet, Dmet, XhMet and PbMet
C@NBI PGS 1999Aug09 - Code prefixed with 'CC' is moved to new subroutine
C@NBI                 'ProcessWeather' to cut down on duplication, since
C@NBI                 they are also done if reading from meteo file.
CC          if (VMet .lt. 0.0) goto 995
CC          VMet = VMet * ifact(UnitW)
            variab='Meteo schedule: Meteo D wind'
            CALL GETWR(Line,k,l,Dmet,DDmet,.TRUE.)
CC          if (Dmet .lt. 0.0 .or. Dmet .gt. 360.0) goto 996
            variab='Meteo schedule: Meteo Temp'
            CALL GETWR(Line,k,l,Tmet,DTmet,.TRUE.)
CC          TMet = TMet * ifact(UnitTmul) + ifact(UnitToff)
            variab='Meteo schedule: Xhumidity'
            CALL GETWR(Line,k,l,XhMet,DXhMet,.TRUE.)
CC          if (XhMet .lt. 0.0) goto 997
CC          XhMet=XhMet * ifact(UnitXh)
CCC@tno jcp 1996Jun26_14:39:04 use meteo moisture as outdoor pollutant?
CCc           write(*,*) 'useMetH2O in Timestep=',UseMetH2O
CCc           write(*,*) 'ipolh2O=',ipolh2O
CC
CC            if (UseMetH2O.gt.0 .and. ipolH2O.le.nconc) then
CCc             write(*,*) 'assigned XhMet to Cout=',Xhmet
CCc             we can fill out the meteo moisture as outdoor concentration
CC              Cout(ipolH2O)=XhMet
CCc             write(*,*) 'Cout(ipolH2O)=',Cout(ipolH2O)
CC
CCC             distribute the concentration over the external nodes/facade elements
CC              DO 102 I=1,Nwind
CC                ExtConc(IpolH2O,I)=cout(ipolH2O)*OuCF(I)
CCc               write(*,*) 'ExtConc(IpolH2O,Iext),iext=',
CCc     &         ExtConc(IpolH2O,I),i
CC                if (outputoptions(10).gt.0) then
CC                  call echosch6(TLotus,'SCH-POL',ipolH2O,i
CC     &              ,Cout(ipolH2O),ExtConc(ipolH2O,I))
CC                end if
CC102           CONTINUE
CCc             call ho('na loop door Nwind in timestep',' ')
CC            end if

            variab='Meteo schedule: P barometer'
C           DPbMet has been made kPa in comv-mai
            CALL GETWR(Line,k,l,PbMet,DPbMet,.TRUE.)
CC            if (PbMet .lt. 0.0) goto 998
CCC           Unit conversion : multiply by 1000.0 to change kPa to Pa
CCC           The barometric pressure values are NOT converted by the user pressure units
CC            PbMet=PbMet*1000.0
CC            Call CheckPb(Pbmet,Zmet,' Meteo Barometric')
CC            if (outputoptions(10).gt.0) then
CC              call echosch3(TLotus,SchNam,Vmet,Dmet,Tmet,
CC     &          Xhmet,PbMet)
CC            end if
C@NBI PGS 1999Aug09   (end of moved code)
C@NBI PGS 1999Aug09 - New subroutine 'ProcessWeather'
            CALL ProcessWeather(Tlotus)
            GOTO 900
          ENDIF


C         11 = Outside pollutant schedule
C         -------------------------------

C         I have for every facade element 5 outdoor pollutants. The initial
C         concentration is given in the schedule (default=0.0). The factor is
C         specified in the external node definition.
          IF (SchTyp.EQ.11) THEN
            chgkey=1
            k=0
            CALL GETWRD(Line,k,l,word)
            CALL UPPERC(Line)
            i=index(Line,'FEF')
            IF (i.EQ.0) THEN
C@tno jcp 1996May31_16:39:07 1,5 is now 1, Nconc the number of Pollutants used
CC            DO 110 J=1,5
c             write(*,*)'in timestep nconc=',nconc
c             write(*,*)'in ipolH2O, UseMetH2O=',ipolH2O,UseMetH2O
              DO 110 J=1,Nconc
c               write(*,*)'pollutant=',j
                CALL IntDis(J,dumstr,LenDum)
                variab='Pol schedule: outside conc pollutant '//
     &          dumstr(1:LenDum)
C@tno jcp 1996Jun26_15:46:35 first read the word and test if it is meteo
                CALL GETWRD(Line,k,l,word)
c               write(*,*)'outside conc word=',word
c               call ho(' ' ,' ')
                if (ICindex(word,'meteo').gt.0) then
                  conc=-1.0
                else
                  lw=lenstr(word)
                  kw=0
                  CALL GETWR(word,kw,lw,conc,0.0,.TRUE.)
c                 write(*,*)'outside conc word=',conc
c                 call ho(' ' ,' ')
C@empa aw 2000jan07 Do the unit conversion here.
C@NBI PGS 2000Oct08 - Can now define different I/O units for each pollutant,
C@NBI                 so changed UnitPol-1 to UnitPolCo for concentration
CC	              conc=ifact(UnitPol-1+j)*conc
                      conc=ifact(UnitPolCo+j)*conc
                end if
C@tno jcp 1996Jun26_14:14:43 here see if the pollutant read is H2O and if the
Cconcentration =-1 that is a flag to use the meteo value
                if (UseMetH2O.eq.1 .and. j.eq.ipolh2O) then
C                 take the meteo Xh value kg/kg
                  conc=XhMet
                end if
C@tno jcp 1996Jun26_14:17:54 I see that no unit conversion is done for outside
C concentrations
C@empa aw 2000jan07 I think this is in fact not correct. We should do the unit
C conversion, but before we have XhMet.
                Cout(J)=conc

                DO 112 I=1,Nwind
                  ExtConc(J,I)=conc*OuCF(I)
                  if (outputoptions(10).gt.0) call
     &              echosch6(TLotus,'SCH-POL',j,i,conc,ExtConc(j,I))
112             CONTINUE
110           CONTINUE
            ELSE
              k=i+3
              nrcall=1
C@tno jcp 1996Jun13_23:48:56 added variab= before getWS sortopt
113           variab=' read the word before the outside conc'
              CALL GETWS(Line,k,l,word,' ')
              IF (word.NE.' ') THEN
                variab='Outside concentration: pollutant'//
     &            dumstr(1:LenDum)
C@tno jcp 1996Apr23_22:33:18 TLotus added in xconc
C@empa aw 2005apr27 Keyword 'fef' does not work correct.Subroutine XCONC removed
CC                CALL XCONC(TLotus,word,nrcall)
                nrcall=nrcall+1
                GOTO 113
              ENDIF
            ENDIF
c           DO 114 J=1,5
c             DO 115 I=1,Nwind
c115          CONTINUE
c114        CONTINUE
            GOTO 900
          ENDIF


C------------------------------
C       Run loop the first time
C------------------------------

        ELSE IF (e3.EQ.0) THEN

c         write(*,*) 'Timestep: e3=0'
c         call ho('E3 check ',' ')

C         Store original fan speed (for every link a value is stored,
C         but only if the link is a fan the value is actually the speed)
          DO 120 I=1,Nl
C           Mf() is INITIALLY the value read from the input
C           file - store it in OFanSp (Original Fan Speed).  Later, we
C           set Mf() = OFanSp * value from schedule, where value = "on factor"
            OFanSp(I)=mf(i)
120       CONTINUE

C         Store original sink and source strength for every zone  for 1..Nconc
          DO 121 I=1,Nz
            DO 122 J=1,Nconc
              OSink(J,I)=Sink(J,I)
              OrSource(J,I)=Source(J,I)
122         CONTINUE
121       CONTINUE

C         Read first data line of the weather file into buffer
          IF (metfile.NE.' ') THEN
C@tno jcp 1996Jul03_16:18:58 I get the feeling that MetINIT can be moved
C freely some where to before the timeloop close to Call tmsmain
c           CALL METINIT
c           call ho(' in Timestep after metinit, before wrtBefore',' ')
C@tno jcp 1996Jul03_16:05:10 this call to write before causes an error on the
C further reading from TMS
c           call wrtBefore()
C           OK. Clear. Wrt before calls DuplTM and that flips TMM and TMS files
C           and closes them.  This whole part should be before the timestep
C           routine
            CALL FILLBUF(bufvm,bufdm,buftm,bufxh,bufpb)
            IF (notime.EQ.1) compday=compday-bufsec
            IF (compday.EQ.e1) THEN
C@NBI PGS 1999Aug09 - Introduced 'Tlotus' so that EchoSch can be called
              TLotus=CREATETM(compday)
              CALL XMETDAT(bufvm,bufdm,buftm,bufxh,bufpb,Tlotus)
              CALL FILLBUF(bufvm,bufdm,buftm,bufxh,bufpb)
            ENDIF
c           call ho('in timestep ','1after fillbuf after wrtBefore ')

          ELSE
C@tno jcp 1996Jul10_15:01:16 9999999 may not be large enough for 68 years or
C steps of a few months
CC          compday=9999999
            compday=INTmax
          ENDIF

c         call ho('in timestep ','2after if statement')
          if (multifile.ne. ' ' ) then
C           multischedtime = multischedtime - multitimestep
            if (multischedtime .eq. e1)  Call DoMulti()
          else
CC          multischedtime=9999999
            multischedtime=INtMax
          endif
          chgkey=1
c         call ho('in timestep ','2.1after if statement')


C@tno jcp 1996Apr25_14:31:26 split up ELSE part in =-1 and =-2
        ELSE if (e3.eq.-2) then
c         write(*,*) 'Timestep:3 after ELSE if (e3.eq.-2) then'
          HistActive=.TRUE.
C         do a calculation now
C@tno jcp 1996Jul03_15:08:15 chkey is not used
CC        chkey=1
          chgkey=1

C@NBI PGS 1999Aug09 - Does this code do anything useful?  It seems vestigial
C         time is not updated here
          if (outputoptions(10).gt.0) then
            TLotus1=createtm(0)
            call echosch(TLotus1+e1/86400.D0,
     &      'HistActive',1.0,1.0)
          end if


C-------------------------------
C         Run loop the last time  (or when normal schedules are exausted)
C-------------------------------

        ELSE
C         (e3 .lt. 0)
C         Double output for last timestep when using a weather file corrected
c         write(*,*)' Timestep:4 in ELSE  part last loop/exhausted'//
c     &   ' schedules'

          if (multifile.NE. ' ' .and. multischedtime .lt. compday) then
            TLotus=CREATETM(multischedtime)
            interval=multischedtime
c           write(cof,*)'interval=multischedtime',interval
            chgkey=1
            if (multischedtime .ne. e1) then
              Call DoMulti()
              interval=MIN(multischedtime,e1)-interval
c             write(cof,*)'interval=MIN(multischedtime,e1)-interval',interval
              Nr=Nr-1
              RETURN
            else
              Call DoMulti()
              Close(MSF)
            endif
          endif
          IF (metfile.NE.' ') THEN
c           write(*,*)' Timestep:If MetFile <> empty '
            TLotus=CREATETM(compday)
            interval=compday
c           write(cof,*)'interval=compday',interval
            chgkey=1
            IF (compday.NE.e1) THEN
              if (multischedtime .eq. compday) then
C               a multischedule at the same time
C               (if there is any meteodata in it it gets overwritten by
C               the data in the meteofile)
                Call DoMulti()
              endif
C@NBI PGS 1999Aug09 - Introduced 'Tlotus' so that EchoSch can be called
              CALL XMETDAT(bufvm,bufdm,buftm,bufxh,bufpb,Tlotus)
              CALL FILLBUF(bufvm,bufdm,buftm,bufxh,bufpb)
              interval=MIN(e1,compday)-interval
c             write(cof,*)'interval=MIN(e1,compday)-interval',interval
              Nr=Nr-1
              RETURN
            ELSE
              if (multischedtime .eq. compday) then
                Call DoMulti()
              endif
C@NBI PGS 1999Aug09 - Introduced 'Tlotus' so that EchoSch can be called
              CALL XMETDAT(bufvm,bufdm,buftm,bufxh,bufpb,Tlotus)
              CALL FILLBUF(bufvm,bufdm,buftm,bufxh,bufpb)
              CLOSE(IFS)
            ENDIF
          ENDIF
c         write(*,*)'Timestep: last run of timestep'
C         When we get here this was the last run of TIMESTEP
          last = .true.
        ENDIF
c       write(*,*)'timestep: 5after EndIf e3 '


C       Is there a record with the same time or is the time negative, that
C       means we are still in the initialisation part ? When yes, run the
C       timestep routine again without calculation between the steps.

900     READ(TMS,REC=Nr+1,IOSTAT=iocheck) e4,e2,e3
c       write(*,*)'timestep: 6 reading TMS '
        IF (iocheck.NE.0) THEN
          e4=e1
          GOTO 994
        ENDIF
        IF (e4.EQ.e1 .OR. e1.LT.0) THEN
          Nr=Nr+1
          e1=e4
          GOTO 111
        ENDIF

994     interval = MIN(multischedtime,MIN(e4,compday))-e1
c       write(cof,*)'interval=MIN(multischedtime,MIN(e4,compday))-e1'
c     & ,interval
c       write(cof,*)'e1     =',e1
c       write(cof,*)'e4     =',e4
c       write(cof,*)'compday=',compday
C@tno jcp 1996Apr29_20:56:58 line copied to beginning of timestep, delete here??
        TLotus=CREATETM(e1)
        RETURN

920     write(Cof,*) 'Error in line:'
        write(Cof,*) Line
        write(Cof,*) 'word with the data=', DatWord, 'Value=',value
        Call Error2('Value out of range for',SchNam,3)
C@NBI PGS 1999Aug09 - Following code block is moved to new subroutine
C@NBI                 'ProcessWeather' to cut down on duplication
CC995     Call Error('Wind Speed out of range in Meteo schedule',3)
CC996     Call Error('Wind Direction out of range in Meteo schedule',3)
CC997     Call Error('Humidity Ratio out of range in Meteo schedule',3)
CC998     Call Error(
CC     &    'Barometric Pressure out of range in Meteo schedule',3)
C@NBI PGS 1999Aug09   (end of moved code)

        END

Ch**********************************************************************

        SUBROUTINE EchoSCH(TLotus,SchNam,SCHvalue,Value)
C***********************************************************************
C  EchoSch echos changes made by a schedule
C  format: TLotus ScheduleName ValueInTheSchedule FinalVariableValue
C@tno jcp 1996Apr15_12:23:29
C
C  TLotus   = time in Lotus format
C  SchNam   = Schedule name
C  SCHvalue = factor in the schedule
C  value    = value of the changed parameter
Ch**********************************************************************
        IMPLICIT NONE
        INCLUDE 'comv-uni.inc'
        INCLUDE 'comv-inp.inc'
        DOUBLE PRECISION TLotus
        REAL SCHValue,Value
        CHARACTER*(*) SchNam
        INTEGER LenStr
C local
        INTEGER lstr
        INTEGER Jday1,NSecDay,numcreate
        Character*31 DTstring
        Character*40 SchNam2

c        write(*,*) 'Schnam=',schnam
c        call ho ('in EchoSch',' ')
        SchNam2=SchNam
        call Long10(schNam2,lstr)
        CALL CreateJdTim(numcreate(TLotus),Jday1,NSecDay)
c        write(*,*) 'TLotus=',TLotus
c        write(*,*) 'jDay',jDay,' NSecDay=',NSecDay
        CALL CONVDAT1(Jday1,NSecDay,DTstring)
c        write(*,*) 'DTstring=',DTstring
c        call ho('main timeloop',' ')

cc        Call ConvDate(TLotus,DTstring)
C@NBI PGS 1999Aug09 - Added clarification
        write(cof,*) 'SCH0: ',DTstring(1:lenstr(DTstring)),' ',
CC   &   SchNam2(1:lstr),' ',SCHvalue,' ',value
     &   SchNam2(1:lstr),' Fac=',SCHvalue,' Val=',value
        return
        end


C@tno jcp 1996May01_11:27:52 new routine to pad strings to 10*n long
Ch**********************************************************************
        SUBROUTINE Long10(str,Lstr)
Ch**********************************************************************
C add spaces before string str to make it a multiple of 10 long
C the final length is in Lst
Ch**********************************************************************
        IMPLICIT NONE
        CHARACTER*(*) str
        INTEGER Lstr
        INTEGER Ldes,ifnge
        CHARACTER*120 str2
        INTEGER LenStr

        Lstr=lenstr(str)
C@tno jcp 1996Apr30_14:06:24 desired length
        Ldes=iFnGe(Lstr,10)
c        write(*,*) 'str=',str
c        write(*,*) 'lstr=',lstr
c        write(*,*) 'ldes=',ldes
c        call ho ('in Long10',' ')
        if (lstr.lt.ldes) then
          Call RPTstr(1,ldes-lstr,' ',Str2)
          Str2=str2(1:ldes-Lstr)//Str
        else
          Str2=str
        end if
        str=str2
        lstr=ldes
        return
        end


C@tno jcp 1996May01_11:27:52 new routine to pad strings to n*n long
Ch**********************************************************************
        SUBROUTINE LongN(str,Lstr,n)
Ch**********************************************************************
C add spaces before string str to make it a multiple of n long
C the final length is in Lst
Ch**********************************************************************
        IMPLICIT NONE
        CHARACTER*(*) str
        INTEGER Lstr,N
        INTEGER Ldes
        CHARACTER*120 str2
        INTEGER LenStr
        INTEGER iFnGe

        Lstr=lenstr(str)
C@tno jcp 1996Apr30_14:06:24 desired length
        Ldes=iFnGe(Lstr,n)
c        write(*,*) 'str=',str
c        write(*,*) 'lstr=',lstr
c        write(*,*) 'ldes=',ldes
c        call ho ('in longN',' ')
        if (lstr.lt.ldes) then
          Call RPTstr(1,ldes-lstr,' ',Str2)
          Str2=str2(1:ldes-Lstr)//Str
        else
          Str2=str
        end if
        str=str2
        lstr=ldes
        return
        end


C@NBI PGS 2000Jul20 - This subroutine wasn't being used, so commented out
CCCh**********************************************************************
CC        SUBROUTINE EchoSCH2(TLotus,SchNam,iZone,Actfactor)
CCC***********************************************************************
CCC  EchoSch echos changes made by a schedule
CCC same as EchoSCH but now with an affected roomnumber or link number
CCC@tno jcp 1996Apr15_12:23:29
CCC@NBI PGS 1999Aug06 - This subroutine 'EchoSCH2' is not used. Delete it?
CCC
CCC  TLotus= time
CCC  SchNam= Schedule name
CCC  value = value of the changed parameter
CCCh**********************************************************************
CC        IMPLICIT NONE
CC        INCLUDE 'comv-uni.inc'
CC	INCLUDE 'comv-inp.inc'
CC        DOUBLE PRECISION TLotus
CC        REAL ActFactor
CC        INTEGER iZone
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
CC        call intdis(iZone,str,lstr2)
CC        CALL CreateJdTim(numcreate(TLotus),Jday1,NSecDay)
CCc        write(*,*) 'time=',TLotus
CCc        write(*,*) 'jDay',jDay,' NSecDay=',NSecDay
CC        CALL CONVDAT1(Jday1,NSecDay,DTstring)
CCc        write(*,*) 'DTstring=',DTstring
CCc        call ho('main timeloop',' ')
CCcc        Call ConvDate(TLotus,DTstring)
CC        write(cof,*) 'SCH2: ',DTstring(1:lenstr(DTstring)),' ',
CC     &   SchNam2(1:lstr),' zone='//str(1:lstr2)//
CC     &   ' ',ActFactor
CC        return
CC        end

