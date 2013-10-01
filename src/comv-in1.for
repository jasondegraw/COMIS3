C+*********************************************************** comv-in1.f
Ch***********************************************************************
C@tno jcp 1996Jun05_16:13:09 PS added
C@NBI PGS 2003Apr27 - New component type: Thermostatic vent
C Net-air components (CR,FA,DS,DF,F1,F2,F3,F4,WI,TD,PE,RF,PS,TV)
        SUBROUTINE inAFC(Line, LinTyp, L, K)
Ch***********************************************************************
        IMPLICIT NONE
        INCLUDE 'comv-inp.inc'
        CHARACTER*(*) line
        INTEGER k,L,LinTyp,ind,lstr
        INTEGER LenStr

        character word*40

C@tno jcp 1996Jul09_16:14:55 variab added to inAFC
        variab='looking for an Air Flow Component'

C  Loop here for each AFC name

10      continue

        l = lenstr(line)
C If we hit a keyword we are done with all the AFC's
        if (LinTyp .eq. TKEY) goto 99
C Should start with a name
        if (LinTyp .ne. TNAME) goto 700

        nUsrAfc=nUsrAfc+1
        AFCNr(nUsrAfc)=nUsrAfc
        Variab='AFCname'
        k=0
        CALL GETWRD(Line,k,l,word)
C AFCname = string with AFC (2char) names
        ind=index(AFCname,word(2:3))
        IF (ind.EQ.0) THEN
           LStr=LENSTR(word)
           CALL INERR('&-NET-AIR: '//word(1:lstr)//
     &          ':  AFC-name is not valid!','It has to begin with: '//
C@NBI PGS 2003Apr27 - New component type: Thermostatic vent;
C@NBI                 and PS is presently not functional.
CC   &          'CR, FA, DS, DF, F1, F2, F3, F4, WI, TD, PE, RF or PS',
     &          'CR, FA, DS, DF, F1, F2, F3, F4, WI, TD, TV, PE or RF',
     &          .FALSE.,2)
        ENDIF
        Ltyp=(ind-1)/10+1
        UsrAfc(nUsrAfc)=word(2:)
        Ldat(ptrl)=Ltyp
C pUsrAfc is the pointer from the sequencenumber of the UsrAfc to Ldat
        pUsrAfc(nUsrAfc)=ptrl
C later on we will create the direct LLprt from Link (seqnumber) to Ldat
C now update PtrL to the next free element in LDat
        ptrl=ptrl+1

C Read the next data line
        Call readlin(line, LinTyp, K, .True.)
        if (FlgEnd) goto 99
        if (LinTyp .ne. TDATA) goto 800
        l=lenstr(line)

C the name has been found this is a REAL data line

C@tno jcp 1996Jun05_16:14:34 PS added
C@NBI PGS 2003Apr28 - New component type: Thermostatic vent
        GOTO (101,102,103,104,105,106,107,108,109,110,120,130,
C             |   |   |   |   |   |   |   |   |   |   |   |
C             CR  FA  DS  DF  F1  F2  F3  F4  WI  TD  PE  RF
     &        140,150                                         ) Ltyp
C             |   |
C             PS  TV

C error, unknown type
        GOTO 900

C@tno jcp 1996Jul09_17:35:01 names changed to inNN where NN=Cr, FA etc
101     call inCR(Line, LinTyp, L, K)
        GOTO 10

102     CALL inFA(Line, LinTyp, L, K)
        GOTO 10

103     CALL inDS(Line, LinTyp, L, K)
        GOTO 10

104     CALL inDF(Line, LinTyp, L, K)
        GOTO 10

105     CALL inF1(Line, LinTyp, L, K)
        GOTO 10

106     CALL inF2(Line, LinTyp, L, K)
        GOTO 10

107     CALL inF3(Line, LinTyp, L, K)
        GOTO 10

108     CALL inF4(Line, LinTyp, L, K)
        GOTO 10

109     CALL inWI(Line, LinTyp, L, K)
        GOTO 10

110     CALL inTD(Line, LinTyp, L, K)
        GOTO 10

120     CALL inPE(Line, LinTyp, L, K)
        GOTO 10

130     CALL inRF(Line, LinTyp, L, K)
        GOTO 10

C@tno jcp 1996Jun05_16:15:46 PS added
140     CALL inPS(Line, LinTyp, L, K)
        GOTO 10
C@NBI PGS 2003Apr27 - New component type: Thermostatic vent
150     CALL in_TV(Line, LinTyp, L, K)
        GOTO 10

700     call inerr('AFC must start with name',line,.TRUE.,2)
        goto 99
800     call inerr('AFC: no data specified',line,.TRUE.,2)
        goto 99
900     call inerr('AFC: unknown component name',line,.TRUE.,2)
        goto 99

99      CONTINUE
        RETURN
        END

Ch***********************************************************************
C@tno jcp 1996Jul09_17:37:14 name changed from CS to inCR
        SUBROUTINE inCR(Line, LinTyp, L, K)
Ch***********************************************************************
        IMPLICIT NONE
        INCLUDE 'comv-inp.inc'
        CHARACTER*(*) line
        INTEGER LenStr
        INTEGER k,l,LinTyp,LStr

C@tno jcp 1996Apr27_17:33:41 added vars to AFC component for default filter data
        INTEGER k2,l2
        CHARACTER FiltStr*160,str*20
C CR: Cs;ExpN;Length;WallThickn;WallUval

        Variab='CR:Cs'
        k=0
        CALL GETLDat(Line,k,l,LDat(ptrl),ptrl,0.001)


        Variab='CR:ExpN'
        CALL GETLDat(Line,k,l,LDat(ptrl),ptrl,0.65)
C@tno jcp 1996Jun27_12:49:17 added the possible Cm units for error messages
        call convcs(iunit(UnitCm),ldat(ptrl-2),ldat(ptrl-1),
     &   UnitStr(UnitCm))

C                              |           |
C                              C-value     ExpN

        Variab='CR:Length'
          CALL GETLDat(Line,k,l,LDat(ptrl),ptrl,1.0)
          IF (ldat(ptrl-1).eq.0.0) THEN
            LStr=LENSTR(UsrAfc(nUsrAfc))
            CALL INERR('&-CR:  *'//UsrAfc(nUsrAfc)(1:LStr)//
     &      ':  Crack length is zero ! ',
     &      'I have changed this to length=1.0',.FALSE.,1)
            Ldat(ptrl-1)=1.0
          ENDIF


        Variab='CR:WallThickn'
        CALL GETLDat(Line,k,l,LDat(ptrl),ptrl,0.0)
C Default WallThickn =0.0 --> No crack temperature correction in FEQN

        Variab='CR:WallUval'
        CALL GETLDat(Line,k,l,LDat(ptrl),ptrl,0.3)

        Call readlin(line, LinTyp, K, .True.)
        if (FlgEnd) goto 99
        if (LinTyp .ne. TDATA) goto 900
        l=lenstr(line)
        CALL FILTER(Line,K,L)
C read next line for next keyword
        Call readlin(line, LinTyp, K, .True.)
        goto 99

900     continue
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

99      continue
        RETURN
        END

Ch***********************************************************************
C@tno jcp 1997Jun23_16:07:32 inFA changed for fan datapoints, catmull interp.
        SUBROUTINE inFA(Line, LinTyp, L, K)
c  ______________________________________________________________
c |1.|               |                                           |
c |__| Prefix + name |    Description                            |
c |         (-)      |        [-]                                |
c |__________________|___________________________________________|
c  _____________________________________________________________
c |2.|# Flag: 1=use Polynomial C0,..C5                          |
c |__|        2=use Data pairs to calculate C0,..Cni            |
c | ____________________________________________________________|
c | Flag|Exp Polynom.|RhoI    | NfI    |   Cs          | Exp n  |
c |(-)  |(-)         |(kg/m3) |[rpm]   |[kg/s@1Pa]     |[-]     |
c |_____|____________|________|________|_______________|________|
c  ____________________________________________
c |3.| Pmin  |  Pmax    |Slope     | Intercept |
c |__| (Pa)  |  (Pa)    |(m3/s/Pa) |(m3/s)     |
c |__________|__________|__________|___________|
c  _________________________________________________________________
c |4.|C0     |C1        |C2        |C3         |C4        |C5       |
c |__|(m3/s) |[m3/s/Pa] |[../Pa2 ] |[../Pa3  ] |[../Pa4 ] |[../Pa5] |
c |__________|__________|__________|___________|__________|_________|
c  ______________________________________________________________________
c |5.     Fan Curve Pressure Rise vs. FlowRate, maximum 4 Lines          |
c |      Data Pairs    minimum 3 Pairs,    maximum 12 Pairs              |
c |(Pa)       |(m3/s)     |(Pa)       |(m3/s)     |(Pa)       |(m3/s)    |
c |___________|___________|___________|___________|___________|__________|
c  __________________________________________________________
c |9.|Filter 1   | Filter 2 | Filter 3 | Filter 4 | Filter 5 |
c |__|   (-)     |    [-]   |    [-]   |    [-]   |    [-]   |
c |______________|__________|__________|__________|__________|
c
Ch***********************************************************************
        IMPLICIT NONE
        INCLUDE 'comv-inp.inc'
        INTEGER LenStr
        CHARACTER*(*) line
        character*160 nline
        INTEGER k,K1,l,LinTyp,LStr
        INTEGER k2,l2,CountLin
C@empa aw 2000apr12 ptrlflg
        INTEGER ptrlflg
        CHARACTER FiltStr*160,str*20
C@lbl bvs 1997Dec11 declare R
        REAL R
C@empa aw 1995dec18 RhoI added
C@empa aw 1998jul09 changes from 1995dac18 reintegrated 
        REAL RhoI

C FA:ExpPolynomial;Flag;RhoI;NfI;Cm;ExpN

C Read data section 2
        Variab='FA:Flag'
C@empa aw 2000apr12 store pointer for the flag
      ptrlflg=ptrl
        k=0
        CALL GETLDat(Line,k,l,LDat(ptrl),ptrl,1.0)
          Variab='FA:ExpPoly'
        CALL GETLDat(Line,k,l,LDat(ptrl),ptrl,5.0)
          Variab='FA:RhoI'

        CALL GETLDat(Line,k,l,LDat(ptrl),ptrl,1.2)
C@empa aw 1995dec18 RhoI
        RhoI=Ldat(ptrl-1)
        IF (LDat(ptrl-1).EQ.0.) THEN
           Lstr=LENSTR(UsrAfc(nUsrAfc))
           CALL INERR('&-FA:  *'//UsrAfc(nUsrAfc)(1:Lstr)//
     &             ':  1.line, 3.input:',
     &             'Air density at fantest is zero for this fan !'
     &             ,.FALSE.,2)
        ENDIF
          Variab='FA:NfI'
        CALL GETLDat(Line,k,l,LDat(ptrl),ptrl,1.0)
        IF (LDat(ptrl-1).EQ.0.) THEN
           Lstr=LENSTR(UsrAfc(nUsrAfc))
           CALL INERR('&-FA:  *'//UsrAfc(nUsrAfc)(1:Lstr)//
     &             ':  1.line, 4.input:','Rotating speed at fantest'//
     &             ' is zero for this fan !'
     &             ,.FALSE.,2)

        ENDIF

          Variab='FA:Cm'
        CALL GETLDat(Line,k,l,LDat(ptrl),ptrl,0.005)
          Variab='FA:ExpN'
C@empa aw 2000nov22 Default ExpN=0.65
CC      CALL GETLDat(Line,k,l,LDat(ptrl),ptrl,0.56)
        CALL GETLDat(Line,k,l,LDat(ptrl),ptrl,0.65)
C@tno jcp 1996Jun27_12:49:17 added the possible Cm units for error messages
        call convcs(iunit(UnitCm),ldat(ptrl-2),ldat(ptrl-1),
     &   UnitStr(UnitCm))

C Read data section 3

        Call readlin(line, LinTyp, K, .True.)

        if (FlgEnd) goto 99
        if (LinTyp .ne. TDATA) goto 800
        l=lenstr(line)

C Pmin;Pmax;slope;intercept
          Variab='FA:Pmin'
        CALL GETLDat(Line,k,l,LDat(ptrl),ptrl,0.0)
C unit conversion
        LDat(ptrl-1)=(ABS(ifact(UnitP)))*LDat(ptrl-1)

          Variab='FA:Pmax'
        CALL GETLDat(Line,k,l,LDat(ptrl),ptrl,200.0)
C unit conversion
        LDat(ptrl-1)=(ABS(ifact(UnitP)))*LDat(ptrl-1)

          Variab='FA:slope'
C@empa aw 2000nov22 Defailt slope=-0.0005
CC      CALL GETLDat(Line,k,l,LDat(ptrl),ptrl,0.0005)
        CALL GETLDat(Line,k,l,LDat(ptrl),ptrl,-0.0005)
C@tno jcp 1997Jul03_13:52:55 UNIT CONVERSION CHECK WHAT COMIN MADE HERE!!
C unit conversion
C@empa aw 1995dec18 convert to kg/s/Pa with actual RhoI
        LDat(ptrl-1)=ifact(UnitFva)*LDat(ptrl-1)*RhoI**ifact(UnitFvFlag)
        LDat(ptrl-1)=LDat(ptrl-1)/(ABS(ifact(UnitP)))

          Variab='FA:intercept'
        CALL GETLDat(Line,k,l,LDat(ptrl),ptrl,0.1)
C unit conversion
C@empa aw 1995dec18 convert to kg/s with actual RhoI
CC      LDat(ptrl-1)=ifact(UnitFva)*LDat(ptrl-1)
        LDat(ptrl-1)=ifact(UnitFva)*LDat(ptrl-1)*RhoI**ifact(UnitFvFlag)

C Read data section 4

        Call readlin(line, LinTyp, K, .True.)
        if (FlgEnd) goto 99
        if (LinTyp .ne. TDATA) goto 910
        l=lenstr(line)

C@tno jcp 1997Jul03_14:27:29 C0..C5 get only conversion for flow rate Not Pressu
C C0;C1;C2;C3;C4;C5
C@empa aw 2005apr21 Default values for C0 to C2 changed to 0.0, as otherwise the  
C@empa aw 2005apr21 the characteristic changes unintentionally if ExpPoly does not 
C@empa aw 2005apr21 correspond to the given number of coefficients 
          Variab='FA:C0'
        CALL GETLDat(Line,k,l,LDat(ptrl),ptrl,0.0)
C unit conversion
C@empa aw 1995dec18 convert to kg/s with actual RhoI
CC      LDat(ptrl-1)=ifact(UnitFva)*LDat(ptrl-1)
        LDat(ptrl-1)=ifact(UnitFva)*LDat(ptrl-1)*RhoI**ifact(UnitFvFlag)

          Variab='FA:C1'
        CALL GETLDat(Line,k,l,LDat(ptrl),ptrl,0.0)
C unit conversion
C@empa aw 1995dec18 convert to kg/s/Pa with actual RhoI
CC      LDat(ptrl-1)=ifact(UnitFva)*LDat(ptrl-1)
        LDat(ptrl-1)=ifact(UnitFva)*LDat(ptrl-1)*RhoI**ifact(UnitFvFlag)
        LDat(ptrl-1)=LDat(ptrl-1)/(ABS(ifact(UnitP)))

          Variab='FA:C2'
        CALL GETLDat(Line,k,l,LDat(ptrl),ptrl,0.0)
C unit conversion
C@empa aw 1995dec18 convert to kg/s/Pa^2 with actual RhoI
CC      LDat(ptrl-1)=ifact(UnitFva)*LDat(ptrl-1)
        LDat(ptrl-1)=ifact(UnitFva)*LDat(ptrl-1)*RhoI**ifact(UnitFvFlag)
        LDat(ptrl-1)=LDat(ptrl-1)/(ABS(ifact(UnitP)))**2

          Variab='FA:C3'
        CALL GETLDat(Line,k,l,LDat(ptrl),ptrl,0.0)
C unit conversion
C@empa aw 1995dec18 convert to kg/s/Pa^3 with actual RhoI
CC      LDat(ptrl-1)=ifact(UnitFva)*LDat(ptrl-1)
        LDat(ptrl-1)=ifact(UnitFva)*LDat(ptrl-1)*RhoI**ifact(UnitFvFlag)
        LDat(ptrl-1)=LDat(ptrl-1)/(ABS(ifact(UnitP)))**3

          Variab='FA:C4'
        CALL GETLDat(Line,k,l,LDat(ptrl),ptrl,0.0)
C unit conversion
C@empa aw 1995dec18 convert to kg/s/Pa^4 with actual RhoI
CC      LDat(ptrl-1)=ifact(UnitFva)*LDat(ptrl-1)
        LDat(ptrl-1)=ifact(UnitFva)*LDat(ptrl-1)*RhoI**ifact(UnitFvFlag)
        LDat(ptrl-1)=LDat(ptrl-1)/(ABS(ifact(UnitP)))**4

          Variab='FA:C5'
        CALL GETLDat(Line,k,l,LDat(ptrl),ptrl,0.0)
C unit conversion
C@empa aw 1995dec18 convert to kg/s/Pa^5 with actual RhoI
CC      LDat(ptrl-1)=ifact(UnitFva)*LDat(ptrl-1)
        LDat(ptrl-1)=ifact(UnitFva)*LDat(ptrl-1)*RhoI**ifact(UnitFvFlag)
        LDat(ptrl-1)=LDat(ptrl-1)/(ABS(ifact(UnitP)))**5

C@tno jcp 1997Jul03_12:58:51 new part to include the fan datapoints

C@tno jcp 1997jul03_12:54:27 added part to allow default filter data
C two possibilities: No data follows at all-> default filterdata
C                    Data follows : this is the fan curve data .THAT MUST be
C                    Followed by the filter data line, as we don't know how many
C                    datapairs are input.
        CountLin=0
        npair=0
C reserve space for the number of data pairs used for the TD
C Define ptrL0 as the element in Ldat where Npairs is stored
C (ptrL0 is defined in include file)
        ptrl0=ptrl
        ptrl=ptrl+1

C Read ahead one line to see if we are past this FA data. If so, then Line
C contains the filter data and Nline the data for the next keyword.
        Call readlin(line, LinTyp, K, .True.)
        if (FlgEnd) goto 99
C If a name, then another FA follows
C@empa aw 1998jul09 if no additional dataline, Filterdata is missing
CC      if (LinTyp .eq. TNAME) goto 99
CC      if (LinTyp .ne. TDATA) goto 99
        if (LinTyp .eq. TNAME) goto 900
        if (LinTyp .ne. TDATA) goto 900


C Read data section 5 the fan datapoints
C Loop through all datalines of this FA


100     Call readlin(nline, LinTyp, K1, .True.)

        CountLin=Countlin+1
C See if we are at the end of the data yet
        if (FlgEnd) goto 88
        if (LinTyp .ne. TDATA) goto 200

        l=lenstr(line)

C Keep looping until we have all data from this line

10      continue
        Variab='FA: datapoints'
        CALL GETLDat(Line,k,l,LDat(ptrl),ptrl,0.0)
        Npair=Npair+1

C unit conversion, check whether pressure or flow column
        if (mod(Npair,2).eq.0) then
C         Even position, flow
C@empa aw 1998jul15 Unit conversion with RhoI
CC              LDat(ptrl-1) = LDat(ptrl-1)*ifact(UnitFva)
          LDat(ptrl-1)=ifact(UnitFva)*LDat(ptrl-1)*
     &                  RhoI**ifact(UnitFvFlag)
        else
C         Odd position, pressure
C@empa aw 2000jun26 use here also ABS of ifact
CC          LDat(ptrl-1) = LDat(ptrl-1)*ifact(UnitP)
            LDat(ptrl-1) = LDat(ptrl-1)*ABS(ifact(UnitP))
        endif
        IF (K.LT.L) GOTO 10

C shift the Nline, just read at 100, into Line to process it
        line=Nline
        k=k1

        GOTO 100


C Last Data Line FILTER

200     continue
C check, calculate and store Npairs
C check for an even number of data

        R=Npair/2.
        IF (ABS(R-INT(R)) .LT. 0.1) THEN
C OK Npair is even
          Npair=Npair/2
C store Npair at the place kept free
          LDat(ptrl0)=Npair
        ELSE
C@empa aw 2000apr12 Error message changed
        LStr=Lenstr(UsrAfc(nUsrAfc))     
        CALL INERR('&-FA: *'//UsrAfc(nUsrAfc)(1:LStr)//
     &  ': Uneven number of data under '//
     &  'header 5 !',line,.true.,2)
        ENDIF

C@empa aw 1998jul09 with CountLin.eq.1 Line contains the filter data as well
CC        if (CountLin.eq.1) then
C already the first line contains no data ->default filter data
CC          goto 900
CC        end if
C now we hope that this line contains the filter data
        CALL FILTER(Line,K,L)

C we already have the next line for the next keyword (for inh)
C no need to call readlin again
        line = Nline
        k=k1
        goto 99
C@empa aw 2000apr12 Error messages changed
CC800   call inerr('FAn: no section 2 data',' ',.TRUE.,2)
800   LStr=Lenstr(UsrAfc(nUsrAfc))     
        call inerr('&-FA: *'//UsrAfc(nUsrAfc)(1:LStr)//
     &           ': No section 3 data found',' ',.TRUE.,2)
        goto 99
C@tno jcp 1996Apr27_17:49:36 900 now 910  (900 is for filter data)
CC910   call inerr('FAn: no section 3 data',' ',.TRUE.,2)
910   LStr=Lenstr(UsrAfc(nUsrAfc))     
        call inerr('&-FA: *'//UsrAfc(nUsrAfc)(1:LStr)//
     &           ': No section 4 data found',' ',.TRUE.,2)

        goto 99

900     continue
C@tno jcp 1996Apr27_17:13:24 put the defaults in?
        str=UsrAfc(nUsrAfc)
        Lstr=Lenstr(str)
        if (outputoptions(2).gt.0) then
        call inerr('&-FA: *'//str(1:Lstr)//
     &   ' has  no filter data',
     &   'Default no filtering, data has been set to 0.0 ',.false.,1)
        end if
        k2=1
        CALL RptStr(k2,MaxC,'0.0 ',FiltStr)
        L2=MaxC*4
        k2=1
        CALL FILTER(FiltStr,K2,L2)

C@empa aw 2000apr12 next line was already read in line not in Nline
CC        goto 88
        goto 99
88      continue
        k=k1
        line=Nline

99      continue
C@empa aw 2000apr12 check number of Datapairs
      if ((Ldat(ptrlflg).eq.3).and.(NPair.eq.0)) then
               Lstr=Lenstr(UsrAfc(nUsrAfc))
               CALL INERR('&-FA: *'//UsrAfc(nUsrAfc)(1:LStr)// 
     &         ' : No data for the fan curve found!;'//
     &         'With flag = 3 at least one data pair has to be input! ',
     &         'Last data line is filter data.',.false.,2)
      endif

        RETURN
        END

Ch***********************************************************************
C@tno jcp 1996Jul09_17:45:58 DS=inDS
        SUBROUTINE inDS(Line, LinTyp, L, K)
Ch***********************************************************************
      IMPLICIT NONE
        INCLUDE 'comv-inp.inc'
        INTEGER LenStr
        CHARACTER*(*) line
        INTEGER k,l,LinTyp
C@tno jcp 1996Apr27_17:33:41 added vars to AFC component for default filter data
        INTEGER k2,l2,lstr
        CHARACTER FiltStr*160,str*20
C@empa aw 1999dec09 ERFLG
        LOGICAL ERFLG
C@empa aw 2001sep07 StartPtrl
        INTEGER StartPtrl

      ERFLG=.false. 
      StartPtrl=ptrl-2 
C DS: Diam1; Diam2; Rough; Lduct; zeta; fittingType; param1; param2
        k=0
          Variab='DS:Diam1'
        CALL GETLDat(Line,k,l,LDat(ptrl),ptrl,0.1)
C@empa aw 1999dec09 check Diameter
      if (Ldat(ptrl-1).lt.1.e-10) then
        ERFLG=.true. 
          str=UsrAfc(nUsrAfc)
        Lstr=Lenstr(str)
          call inerr('DS:'//str(1:Lstr)//
     &   ' Diameter 1 is zero. This is not a valid value!',
     &   '',.true.,2)
        end if
          Variab='DS:Diam2'
        CALL GETLDat(Line,k,l,LDat(ptrl),ptrl,0.0)
          Variab='DS:Rough'
        CALL GETLDat(Line,k,l,LDat(ptrl),ptrl,0.1)
C@empa aw 1999dec09 check roughness
      if (Ldat(ptrl-1).lt.1.e-10) then
        ERFLG=.true. 
          str=UsrAfc(nUsrAfc)
        Lstr=Lenstr(str)
          call inerr('DS:'//str(1:Lstr)//
     &   ' Roughness is zero. This is not a valid value!',
     &   '',.true.,2)
        end if
          Variab='DS:Lduct'
        CALL GETLDat(Line,k,l,LDat(ptrl),ptrl,10.0)
C@empa aw 1999dec09 check length
      if (Ldat(ptrl-1).lt.1.e-10) then
        ERFLG=.true. 
          str=UsrAfc(nUsrAfc)
        Lstr=Lenstr(str)
          call inerr('DS:'//str(1:Lstr)//
     &   ' Length is zero. This is not a valid value!',
     &   '',.true.,2)
        end if
          Variab='DS:zeta'
        CALL GETLDat(Line,k,l,LDat(ptrl),ptrl,0.0)
          Variab='DS:fittingType'
        CALL GETLDat(Line,k,l,LDat(ptrl),ptrl,0.0)
          Variab='DS:fitting param1'
        CALL GETLDat(Line,k,l,LDat(ptrl),ptrl,0.0)
          Variab='DS:fitting param2'
        CALL GETLDat(Line,k,l,LDat(ptrl),ptrl,0.0)
C@empa aw 2001sep07 ZETA1 and ZETA2
      LDat(StartPtrl+17)=LDat(StartPtrl+6) 
      ptrl=StartPtrl+18
        Variab='DS:zeta2'
        CALL GETLDat(Line,k,l,LDat(ptrl),ptrl,0.0)
C     ! reserve space for missing parameters for the dwc model
        Ptrl=StartPtrl+21
C calculate the missing parameters for dwc model
C@empa aw 1999dec09 only if all data is valid
      if (.not.ERFLG)  then
C@empa aw 2001sep07 use StartPtrl 
CC        CALL DSPREC(LDat(ptrl-9))
          CALL DSPREC(LDat(StartPtrl+1))
      endif

C section 2 - Specific Duct Leakage is ignored for now
C@NBI PGS 2000Aug14 - COMIS 3.1 User Guide has removed this datasection, so now commented out
CC      Call readlin(line, LinTyp, K, .True.)
CC      if (FlgEnd) goto 99
CC      if (LinTyp .ne. TDATA) goto 800

C Last section(3) - filter data
        Call readlin(line, LinTyp, K, .True.)
        if (FlgEnd) goto 99
        if (LinTyp .ne. TDATA) goto 900
        l=lenstr(line)
        CALL FILTER(Line,K,L)

C read next line for next keyword
        Call readlin(line, LinTyp, K, .True.)
        goto 99

C@NBI PGS 2000Aug14 - COMIS 3.1 User Guide has removed this datasection, so commented out now
CC800   call inerr('DS: no specific duct leakage data',' ',.TRUE.,2)
CC      goto 99

900     continue
C@tno jcp 1996Apr27_17:13:24 why an error? put the defaults in?
        str=UsrAfc(nUsrAfc)
        Lstr=Lenstr(str)
        if (outputoptions(2).gt.0) then
        call inerr('DS:'//str(1:Lstr)//
     &   ' has no filter data',
     &   'Default no filtering, data has been set to 0.0 ',.false.,1)
        end if
        k2=1
        CALL RptStr(k2,MaxC,'0.0 ',FiltStr)
        L2=MaxC*4
        k2=1
        CALL FILTER(FiltStr,K2,L2)

        goto 99

99      continue
        RETURN
        END

Ch***********************************************************************
C@tno jcp 1996Jul09_17:45:58 DF=inDF
        SUBROUTINE inDF(Line, LinTyp, L, K)
Ch***********************************************************************
        IMPLICIT NONE
        INCLUDE 'comv-inp.inc'
        CHARACTER*(*) line
        INTEGER k,l,LinTyp

C DF:FittingType; param1; param2
        k=0
        Variab='DF:FittingType'
        CALL GETLDat(Line,k,l,LDat(ptrl),ptrl,0.0)
        Variab='DF:fitting param1'
        CALL GETLDat(Line,k,l,LDat(ptrl),ptrl,0.0)
        Variab='DF:fitting param2'
        CALL GETLDat(Line,k,l,LDat(ptrl),ptrl,0.0)

C DF has no Filter coefficients

C read next line for next keyword
        Call readlin(line, LinTyp, K, .True.)
        RETURN
        END

