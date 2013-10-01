C+*********************************************************** comv-in2.f
Ch**********************************************************************
        SUBROUTINE FILTER(Line,K,L)
Ch**********************************************************************
        IMPLICIT NONE
        INCLUDE 'comv-inp.inc'
        CHARACTER*(*) line
        INTEGER k,l

C LastLine: Filt(1); Filt(2); Filt(3); Filt(4); Filt(5)
        Variab='Filt(1)'
        k=0
        CALL GETLDat(Line,K,L,Ldat(PtrL),PtrL,0.0)
        Variab='Filt(2)'
        CALL GETLDat(Line,K,L,Ldat(PtrL),PtrL,0.0)
        Variab='Filt(3)'
        CALL GETLDat(Line,K,L,Ldat(PtrL),PtrL,0.0)
        Variab='Filt(4)'
        CALL GETLDat(Line,K,L,Ldat(PtrL),PtrL,0.0)
        Variab='Filt(5)'
        CALL GETLDat(Line,K,L,Ldat(PtrL),PtrL,0.0)

        RETURN
        END


Ch**********************************************************************
C Duct transitions: relam, returb
        SUBROUTINE inTRANS(Line, LinTyp, L, K)
Ch**********************************************************************
        IMPLICIT NONE
        INCLUDE 'comv-inp.inc'
        CHARACTER*(*) line
        INTEGER k,l,LinTyp

C Duct Transitions: Relam; Returb
        k=0
        Variab='Flow transitions: Re Laminair'
        CALL GETWR(Line,k,l,ReLam ,DReLam,.TRUE.)
        Variab='Flow transitions: Re Turbulent'
        CALL GETWR(Line,k,l,ReTurb,DReTurb,.TRUE.)
C read next line for next keyword
        Call readlin(line, LinTyp, K, .True.)

        RETURN
        END

Ch**********************************************************************
C NetHvac
        SUBROUTINE inHVAC(Line, LinTyp, L, K)
C@NBI PGS 2000Aug14 - This data section is currently being used by COMIS.
Ch**********************************************************************
        IMPLICIT NONE
        INCLUDE 'comv-inp.inc'
        CHARACTER*(*) line
        INTEGER l,k,LinTyp

        k=0

C NetHvac IF it does not fit the allocated array space Flag to READ it from DAF
C do not forget to eat0 from the duct name
C@akk 23Sep1991: don't use DAF, use CIF instead (data not in DAF anymore)

C skip to end of data to next keyword

10      continue
        Call readlin(line, LinTyp, K, .True.)
        if (FlgEnd) goto 99
        if (LinTyp .eq. TDATA) goto 10
        goto 99

99      continue
        RETURN
        END

Ch**********************************************************************
C NET-ZONes
        SUBROUTINE inZONES(Line, LinTyp, L, K)
Ch**********************************************************************
        IMPLICIT NONE
        INCLUDE 'comv-inp.inc'
        INTEGER LenStr
        CHARACTER*(*) line
        INTEGER I,k,l,LinTyp
        INTEGER kw,lw,pos,Lstr,kk
C@tno jcp 1996Jul08_11:25:49 zone schedules are also read via Word and can be 50
CC        character word*40
        character word*80
C@empa aw 2001jul11
        REAL value


C loop here for each zone description

10      continue

        k=0
        Nz=Nz+1
        Variab='Zone name'
        CALL GETWS(Line,k,l,ZoNa(Nz),'      ')
        Lstr=LENSTR(ZoNa(Nz))
C@empa aw 1999nov29 check "-" in Name
      IF (INDEX(ZoNa(Nz),'-').GT.0) THEN
          CALL INERR ('&-NET-ZON: ZoneNr: '//ZoNa(Nz)(1:Lstr)//
     &  ' ; "-" is not allowed in Zone ID ! ',' ',.TRUE.,2)
      ENDIF
        ZoNr(Nz)=Nz
        Variab='user name for zone'
        CALL GETWS(Line,k,l,ZoNaU(Nz),'      ')
        Variab='Temp zone Tz'
        CALL GETWR(Line,k,l,Tz(Nz),DTz,.TRUE.)
C unit conversion
        Tz(Nz)=ifact(UnitToff)+ifact(UnitTmul)*Tz(Nz)

        Variab='Ref height zone Zz'
        CALL GETWR(Line,k,l,Zz(Nz),DZz,.TRUE.)
C option to input H/D/W Instead of Volume
        kk=k
        CALL GETWS(Line,k,l,Word,'      ')
        kw=0
        lw=LENSTR(Word)
        pos=INDEX(Word,'/')
        IF (pos.EQ.0)THEN
          Variab='Volume zone Vz'
          CALL GETWR(Word,kw,lw,Vz(Nz),DVz,.TRUE.)
C@tno jcp 1996Jul05_09:06:11 add start position kk here for Clines10
          Clines10=kk+Clines10
        ELSE
          Variab='Height/Depth/Width zone  Hz/Dz/Wz'
          Word(pos:pos)=' '
          pos=INDEX(Word,'/')
          IF (pos.EQ.0)THEN
            CALL INERR ('&-NET-ZON: ZoneNr:'//ZoNa(Nz)(1:Lstr)//
     &      ' ; invalid H/D/W input found !',' ',.TRUE.,2)
          ELSE
            Word(pos:pos)=' '
            Variab='Height zone Hz'
C@empa aw 2000dec01 change defaulvalues for H/D/W from 0/0/0 to 3/5/5 
            CALL GETWR(Word,kw,lw,Hz(Nz),3.0,.TRUE.)
C@tno jcp 1996Jul05_09:06:11 add start position kk here for Clines10
            Clines10=kk+Clines10
            IF (Hz(Nz).LE.0.0)THEN
              CALL INERR ('&-NET-ZON: ZoneNr:'//ZoNa(Nz)(1:Lstr)//
     &        ' ;Room height is less or equal zero !',' ',.TRUE.,2)
            ENDIF
            Variab='Depth zone Dz'
            CALL GETWR(Word,kw,lw,Dz(Nz),5.0,.TRUE.)
C@tno jcp 1996Jul05_09:06:11 add start position kk here for Clines10
            Clines10=kk+Clines10
            IF (Dz(Nz).LE.0.0)THEN
              CALL INERR ('&-NET-ZON: ZoneNr:'//ZoNa(Nz)(1:Lstr)//
     &       ' ;Depth of the room is less or equal zero !',' ',.TRUE.,2)
            ENDIF
            Variab='Width zone Wz'
            CALL GETWR(Word,kw,lw,Wz(Nz),5.0,.TRUE.)
C@tno jcp 1996Jul05_09:06:11 add start position kk here for Clines10
            Clines10=kk+Clines10
            IF (Wz(Nz).LE.0.0)THEN
              CALL INERR ('&-NET-ZON: ZoneNr:'//ZoNa(Nz)(1:Lstr)//
     &       ' ;Width of the room is less or equal zero !',' ',.TRUE.,2)
            ENDIF
            Vz(Nz)=Hz(Nz)*Dz(Nz)*Wz(Nz)
          ENDIF
        ENDIF
        Variab='Humidity zone Xhz'
        CALL GETWR(Line,k,l,Xhz(Nz),DXhz,.TRUE.)
C unit conversion
        Xhz(Nz)=ifact(UnitXh)*xhz(Nz)


C set schedules that don't exist to '-'
          SchZ(Nz)='-'
C all zone schedules
        if (k.lt.l) then
          Variab='Schedules per zone'
C@empa aw 2005apr22 use GetWS instead of GetWrd in order to check for "def".. or "#" 
CC          CALL GETWRD(Line,k,l,word)
          CALL GETWS(Line,k,l,word,'')
          lw=lenstr(Word)
C@tno jcp 1996Jul08_11:32:02 Lw for Zone schedule names limited to 50 characters
          if (lw.gt.50) then
C@tno jcp 1996Jul08_11:32:02 report a too long schedule-string per zone
          call inerr('&-NET-ZONes: the string for Zone schedules is '//
     &    'too long. The maximum length is 50 characters.',
     &    'Use short Schedules names.'//
     &    ' Correct the input file. ',.TRUE.,2)
            lw=50
          end if
          DO 149 i=1,Lw
            if (word(i:i).EQ.'/') word(i:i)=' '
149       continue
          SchZ(Nz)=word(1:lw)
        end if
C@empa aw 2001jul11 For COMIS in TRNSYS only: 
C                   Read reference to BUI variable
        CALL GETWR(Line,k,l,value,-9999.,.FALSE.)
        if (value.NE.-9999.) THEN
          NPC=NPC+1
          TASch(NPC)=value
          CALL GETWR(Line,k,l,value,-9999.,.FALSE.)
          if (value.NE.-9999.) THEN
            XhASch(NPC)=value
          ELSE
             call inerr('&-NET-ZONes: humidity is missing '//
     &        'in auxilliary node with predefined condition!',
     &        ZoNa(NZ),.FALSE.,2)
          ENDIF
        ENDIF          
C@tno jcp 1996Jul08_11:32:02 end

        Call readlin(line, LinTyp, K, .True.)
        if (FlgEnd) goto 99
        l=lenstr(line)
        if (LinTyp .eq. TDATA) goto 10

99      RETURN
        END

Ch**********************************************************************
C NET-ZL (zone layers)
        SUBROUTINE inZL(Line, LinTyp,L, K)
Ch**********************************************************************
        IMPLICIT NONE
        INCLUDE 'comv-inp.inc'
        INTEGER LenStr
        CHARACTER*(*) line
        INTEGER k,l,LinTyp
        character word*40,word2*40


C@tno jcp 1996Jul09_17:10:28 variab added to inZT
        variab='NET-ZL gradient layers of zones'




C Net ZL: zone or first data=startheight


C Loop here for each zone

10      continue
        Nzl=Nzl+1

C Must start with a zone name
        IF (LinTyp .ne. TNAME) goto 900

        Variab='Layers zone or startheight'
        k=0
        CALL GETWRD(line,k,l,word)
        word2=word(2:)

C The array Layptr has to be reordered according to the REAL
C zone sequence using ZLNa after the zone tree has been established
C in routine inzreord.

        ZLNa(Nzl)= word2
        LayPtr(1,Nzl)=LayPtr1

C Loop here for each layer in this zone

20      continue

C Keep updating the second (END pointer) so it will point at the last element
C after leaving this zone
C StartHeight; TempGrad; HumGrad; PollGrad; VolumeFract; SourceFract; SinkFract;
        Variab='Zone Layer Startheight'
        CALL GETLDat(Line,K,L,Laydat(LayPtr1),LayPtr1,0.0)

        Variab='Zone Layer TempGrad'
        CALL GETLDat(Line,K,L,Laydat(LayPtr1),LayPtr1,0.0)
C unit conversion only scale! no shift
C This is in degrees C/meter, not fraction of original as humidity and poll. are
C       Note the element is now in LayPtr-1
        Laydat(LayPtr1-1)=ifact(UnitTmul)*Laydat(LayPtr1-1)

        Variab='Zone Layer HumGrad'
C no unit conversion here since this is a fraction of full scale
        CALL GETLDat(Line,K,L,Laydat(LayPtr1),LayPtr1,0.0)

        Variab='Zone Layer PollGrad'
        CALL GETLDat(Line,K,L,Laydat(LayPtr1),LayPtr1,0.0)
C no unit conversion here since this is a fraction of full scale

        Variab='Zone Layer VolumeFract'
        CALL GETLDat(Line,K,L,Laydat(LayPtr1),LayPtr1,0.0)

        Variab='Zone Layer SourceFract'
        CALL GETLDat(Line,K,L,Laydat(LayPtr1),LayPtr1,0.0)

        Variab='Zone Layer SinkFract'
        CALL GETLDat(Line,K,L,Laydat(LayPtr1),LayPtr1,0.0)

C Flow to next Layer Factor; Flow to zone Factor
        Variab='Flow to next Layer Factor'
        CALL GETLDat(Line,K,L,Laydat(LayPtr1),LayPtr1,0.0)

        Variab='Flow to zone Factor'
        CALL GETLDat(Line,K,L,Laydat(LayPtr1),LayPtr1,0.0)
        LayPtr(2,Nzl)=LayPtr1-1

        Call readlin(line, LinTyp, K, .True.)
        if (FlgEnd) goto 99
        l=lenstr(line)
C  If a new zone
        if (LinTyp .eq. TNAME) goto 10
C  If a new layer within this zone
        if (LinTyp .eq. TDATA) goto 20
        goto 99

900     CALL INERR('&-NET-ZL: A Zone name must be defined '//
     &        'in the first data line: ',' ',.true.,2)
              ZLNa(Nzl)='<No Name>'
        goto 99

99      CONTINUE
        RETURN
        END

Ch**********************************************************************
C NET-ZP (zone pollutants)
      SUBROUTINE inZP(Line, LinTyp, L, K)
C
C Changes:
C@NBI PGS 2000oct09 - Detabbed & tidied up routine; no syntax change
Ch**********************************************************************

      IMPLICIT NONE
      INCLUDE 'comv-inp.inc'
      INTEGER LenStr,k,l,LinTyp,Np,Eflag,pocc,pplus,pmult,nfilled,LStr
      REAL RELCON
      CHARACTER*(*) line
      character word*40,word2*40,MaxCStr*3
C-----

C@tno jcp 1996Jul09_17:10:28 variab added to inZP
      variab='reading NET-ZP pollutant data of zones'

C     ! Nzp has been initialized in comv-inh.f
C     ! Loop here for each zone
10    Nzp=Nzp+1

C     ! Must start with a zone name
      IF (LinTyp .ne. TNAME) goto 900

      k=0
      Variab='Zone Pollutants'
      CALL GETWRD(line,k,l,word)
      word2=word(2:)
      ZPNa(Nzp)= word2

C     ! pollutant number for this zone
      Np = 1

C     ! Loop here for each pollutant in this zone
20    continue

C     ! The array C has to be reordered according to the REAL
C     ! zone sequence using ZPNa after the zone tree has been established
C     ! see routine inzreord

C     ! check number of pollutants against MaxC
      if( Np.GT.MaxC ) then
         call intdis(MaxC,MaxCStr,LStr)
         call inerr('&-NET-ZP:   Zone Name: '//ZpNa(Nzp),
     &      'Too many pollutants for this zone !  '//
     &      'Maximum number of pollutants: '//
     &      MaxCStr(1:LStr),.FALSE.,2)
         GOTO 99
      endif

      Variab='Concentration'
      CALL GETWR(Line,k,l,C(Np,Nzp),0.0,.TRUE.)
C     ! unit conversion
C@NBI PGS 2000Oct08 - Can now define different I/O units for each pollutant,
C@NBI                 so changed UnitPol-1 to UnitPolCo
CC    C(Np,Nzp)=ifact(UnitPol-1+Np)*C(Np,Nzp)
      C(Np,Nzp)=ifact(UnitPolCo+Np)*C(Np,Nzp)

C@empa aw 2000dec18 delete occupant option here
CC      IF (K.LT.L) THEN
CC         Variab='occupant? or source'
CC         CALL GETWRD(line,k,l,word)
CC         CALL lowerC(word)
CC         pocc=index(word,'occ')
CC         IF (pocc.EQ.0) THEN
CC         variab='pollutant source'
CC         Source(Np,Nzp)=RELCON(word,Eflag)
C           ! unit conversion !!! only one conversion for now.
C           !!!THIS DOESN'T INCLUDE POLLUTANT DENSITY rho source which is in MM(MaxC)!!!
C           ! like the comvension for concentrations there should be an indicator for
C           ! Volume/Mass conversion
C@NBI PGS 2000Oct08 - Can now define different I/O units for each pollutant.
CC         Source(Np,Nzp)=ifact(UnitPSou)*Source(Np,Nzp)
CC         Source(Np,Nzp)=ifact(UnitPolSo+Np)*Source(Np,Nzp)
CC         ELSE
C@tno jcp 1996Apr24_16:28:35 let us delete the next possibility, No occupants he
C now a negative source means a pointer into array OccSour, OccSour has every
C time 5 elements: 1=number 2=OCCnr 3=number 4=OCCnr 5=Source
C the formula that will be used to get the source of this pollutant is:
C Source=number*OCCprod+number*OCCprod+Source
C The Occ production must come from a formula, a routine that RETURNs the
C actual production of the 5 gasses taken into account, produced by the Occupant
C@tno jcp 1995Apr26 !!!I 'm lost. What to do with unit conversions for
CC            Source(Np,Nzp)=-pSource
CC            nfilled=0
CC202         pplus=index(word,'+')
CC            IF (pplus.GT.0) THEN
CC               CALL twoword(word,Osource(psource),nfilled)
CC               goto 202
CC            ELSE
CC               pmult=index(word,'*')
CC               IF (pmult.GT.0) THEN
CC                  Variab='EpsFR'
CC                  CALL getnrOc(word,Osource(psource),nfilled)
CC               ELSE
CC                  pocc=index(word,'occ')
CC                  IF(pocc.eq.0) THEN
CC                     variab='occupant pollutant source'
CC                     Osource(psource+4)=RELCON(word,EFlag)
CC                  ELSE
CC                     word2='1*'//word
CC                     Variab='the number of occupants'
CC                     CALL getnrOc(word2,Osource(psource),nfilled)
CC                  ENDIF
CC               ENDIF
CC            ENDIF
CCC           ! end of IF pplus
CC            pSource=pSource+5
CC         ENDIF
CCC        ! end of IF pocc
CC
CC      ELSE
CCC        ! K>L (line contains no more data)
CC         Source(Np,Nzp)=0.0
CC      ENDIF

      Variab='Source term'
      CALL GETWR(Line,k,l,Source(Np,Nzp),0.0,.TRUE.)
      Source(Np,Nzp)=ifact(UnitPolSo+Np)*Source(Np,Nzp)

      Variab='Sink term'
      CALL GETWR(Line,k,l,Sink(Np,Nzp),0.0,.TRUE.)
C     ! unit conversion !!! only one conversion for now
C@NBI PGS 2000Oct09 - Can now define different I/O units for each pollutant.
CC    Sink(Np,Nzp)=ifact(UnitPSin)*Sink(Np,Nzp)
      Sink(Np,Nzp)=ifact(UnitPolSi+Np)*Sink(Np,Nzp)

C@empa aw 2000dec18 read first order decay per zone
      Variab='decay'
      CALL GETWR(Line,k,l,rkz(Np,Nzp),0.0,.TRUE.)
C@empa aw 2001jul11 For COMIS in TRNSYS only: 
C      Read TRNSYS input/schedule number for source and sink 
       CALL GETWI(Line,k,l,TRSoIS(Np,Nzp),0)
       CALL GETWI(Line,k,l,TRSiIS(Np,Nzp),0)
      
C     ! every new zonename restarts the line counting, so the max Np is also Nconc
      Nconc=MAX(Np,NConc)
      Np = Np + 1

C     ! Check if another line
      Call readlin(line, LinTyp, K, .True.)
      if (FlgEnd) goto 99
      l=lenstr(line)
C     ! Loop to 10 if another zone name
      if (LinTyp .eq. TNAME) goto 10
C     ! Loop to 20 if another pollutant
      if (LinTyp .eq. TDATA) goto 20
      goto 99

900   CALL INERR('&-NET-ZP: A zone name must be defined in '//
     &   'the first data line:',' ',.true.,2)
      ZpNa(Nzp)='<No Name>'
      GOTO 99

99    CONTINUE
      RETURN
      END


Ch**********************************************************************
C NET-ZT (zones with thermal properties)
      SUBROUTINE inZT(Line, LinTyp,L, K)
Ch**********************************************************************
        IMPLICIT NONE
      INCLUDE 'comv-inp.inc'
        CHARACTER*(*) line
        INTEGER k,l,LinTyp,LStr
        INTEGER LenStr
        character word*40,word2*40

C Net ZT: zone name

C@tno jcp 1996Jul09_17:10:28 variab added to inZT
        variab='NET-ZT thermal properties of zones'


C Loop here for each zone

10      continue
        Nzt=Nzt+1

C Must start with a zone name
        IF (LinTyp .ne. TNAME) THEN
          CALL INERR('&-NET-ZT: A Zone name must be defined: '
     &        ,' ',.true.,2)
              ZTNa(Nzt)='<No Name>'
          goto 99
        ENDIF
        Variab='Thermal properties zone '
        k=0
        CALL GETWRD(line,k,l,word)
        word2=word(2:)
        ZTNa(Nzt)= word2
        Variab='Zone thermal properties conductivity'
        CALL GETWR(Line,K,L,LamdaW(Nzt),0.0,.TRUE.)
        Variab='Zone thermal properties density'
        CALL GETWR(Line,K,L,RhoW(Nzt),0.0,.TRUE.)
        Variab='Zone thermal properties capacity'
        CALL GETWR(Line,K,L,CapW(Nzt),0.0,.TRUE.)
        Variab='Zone thermal properties wall thickness'
        CALL GETWR(Line,K,L,EW(Nzt),0.0,.TRUE.)
        if ((LamdaW(Nzt)*RhoW(Nzt)*CapW(Nzt)*EW(Nzt)).eq.0.0) then
          LStr=LenStr(ZTNa(Nzt))
          CALL INERR('&-NET-ZT: '//ZTNa(Nzt)(1:LStr)// ': one of the '//
     &    'zone thermal properties is zero !',' ',.false.,2)
        endif
        Call readlin(line, LinTyp, K, .True.)
        if (FlgEnd) goto 99
        l=lenstr(line)
C Loop to 10 if another zone name
        if (LinTyp .eq. TNAME) goto 10

99      CONTINUE
        RETURN
        END

Ch***********************************************************************
C@NBI PGS 2003Apr25 - New component type: Thermostatic vent
      SUBROUTINE in_TV(Line, LinTyp, L, K)
Ch***********************************************************************

        IMPLICIT NONE
        INCLUDE 'comv-inp.inc'

C       Passed arguments
        CHARACTER*(*) line
        INTEGER k,l,LinTyp

C       Internal variables
        INTEGER LenStr,LStr,n
        CHARACTER FiltStr*160,str*20
C------
        k=0

        DO n=1,3
C         Read conditions when device is fully closed (or smallest opening)
          Variab='TV:Temperature'
          CALL GETLDat(Line,k,l,LDat(ptrl),ptrl,-5.0)
          Variab='TV:Cs'
          CALL GETLDat(Line,k,l,LDat(ptrl),ptrl,0.001)
          Variab='TV:Expn'
          CALL GETLDat(Line,k,l,LDat(ptrl),ptrl,0.5)
C         density-correct Cs
          call convcs(iunit(UnitCm),ldat(ptrl-2),ldat(ptrl-1)
     &    ,UnitStr(UnitCm))
        ENDDO

C       Read conditions when device is fully open (or smallest opening)
        Variab='TV:One_way'
        CALL GETLDat(Line,k,l,LDat(ptrl),ptrl,0.0)

        if ((LDat(ptrl-1).NE.0.0).AND.(LDat(ptrl-1).NE.1.0))
     &    CALL INERR('&-TV: One-way damper: Enter 0 or 1',' ',.false.,2)
C-------

C       Read Filter data
        Call readlin(line, LinTyp, K, .True.)
        if (FlgEnd) goto 99
        IF (LinTyp .NE. TDATA) THEN
           Lstr=LENSTR(UsrAfc(nUsrAfc))
           CALL INERR('&-TV:  *'//UsrAfc(nUsrAfc)(1:Lstr)//':  3.line:',
     &                'No filter data!',.FALSE.,2)
           GOTO 99
        ENDIF

        l=lenstr(line)
        CALL FILTER(Line,K,L)

C       read next line for next keyword
        CALL readlin(line, LinTyp, K, .True.)

99      continue
        RETURN
      END

