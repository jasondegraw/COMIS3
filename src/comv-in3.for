C+*********************************************************** comv-in3.f

C***********************************************************************
C Flow Controllers F1 - F4
C@empa aw 1995nov28 New inputroutines F1 - F4 for new flow controller routines
C***********************************************************************
Ch***********************************************************************

        SUBROUTINE inF1(Line, LinTyp, L, K)

Ch***********************************************************************

      IMPLICIT NONE
        INCLUDE 'comv-inp.inc'
        CHARACTER*(*) line
        INTEGER k,l,LinTyp
        REAL RhoI
C@lbl bvs 1997Jul24 Lstr must be INTEGER !
        INTEGER Lstr
C@lbl bvs 1997Jul24 declare lenstr
        INTEGER LENSTR

        k=0
        Lstr=LENSTR(UsrAfc(nUsrAfc))
C       Range 1:
          Variab='F1:RhoI'
        Clines10=k
        CALL GETLDat(Line,k,l,LDat(ptrl),ptrl,1.2)
        RhoI=Ldat(ptrl-1)
C@NBI PGS 2003-04-25 : simplification
CC      IF (LDat(ptrl-1).EQ.0.) THEN
        IF (RhoI.EQ.0.) THEN
           CALL INERR('&-F1:  *'//UsrAfc(nUsrAfc)(1:Lstr)//
     &             ':  1.line, 1.input: RhoI',
     &             'Air density at test conditions is zero!'
     &             ,.FALSE.,2)
        ENDIF

          Variab='F1:Cq'
        Clines10=k
        CALL GETLDat(Line,k,l,LDat(ptrl),ptrl,0.1)
C       convert to kg/s with actual RhoI
        LDat(ptrl-1)=ifact(UnitFva)*LDat(ptrl-1)*RhoI**ifact(UnitFvFlag)
          Variab='F1:ExpN'
        Clines10=k
        CALL GETLDat(Line,k,l,LDat(ptrl),ptrl,0.65)
C       convert C to kg/s/@1Pa
        LDat(ptrl-2)=LDat(ptrl-2)/(ABS(ifact(UnitP)))**LDat(ptrl-1)
          Variab='F1:Fva_Setpoint'
        Clines10=k
        CALL GETLDat(Line,k,l,LDat(ptrl),ptrl,0.)
C       convert to kg/s with actual RhoI
        LDat(ptrl-1)=ifact(UnitFva)*LDat(ptrl-1)*RhoI**ifact(UnitFvFlag)

C Facade leakage
        CALL readlin(line, LinTyp, K, .True.)
        IF (FlgEnd) GOTO 99
        IF (LinTyp .NE. TDATA) THEN
           CALL INERR('&-F1:  *'//UsrAfc(nUsrAfc)(1:Lstr)//
     &             ':  2.line:',
     &             'No data for facade leakage!'
     &             ,.FALSE.,2)
           goto 99
        ENDIF
        l=lenstr(line)
          Variab='F1:Facade_Cs'
        Clines10=k
        CALL GETLDat(Line,k,l,LDat(ptrl),ptrl,0.001)
          Variab='F1:Facade_ExpN'
        Clines10=k
        CALL GETLDat(Line,k,l,LDat(ptrl),ptrl,0.65)
C       convert Cs to kg/s/@1Pa
C@tno jcp 1997Jun16 (missed this one on June 1996)
C		added the possible Cm units for error messages
        CALL convcs(iunit(UnitCm),ldat(ptrl-2),ldat(ptrl-1),
     &   UnitStr(UnitCm))

C filter
        CALL readlin(line, LinTyp, K, .True.)
        IF (FlgEnd) GOTO 99
        IF (LinTyp .NE. TDATA) THEN
           CALL INERR('&-F1:  *'//UsrAfc(nUsrAfc)(1:Lstr)//
     &             ':  3.line:',
     &             'No filter data!'
     &             ,.FALSE.,2)
           GOTO 99
        ENDIF

        l=lenstr(line)
        CALL FILTER(Line,K,L)

C read next line for next keyword
        CALL readlin(line, LinTyp, K, .True.)

99	CONTINUE
        RETURN
        END

Ch***********************************************************************
        SUBROUTINE inF2(Line, LinTyp, L, K)
Ch***********************************************************************

      IMPLICIT NONE
        INCLUDE 'comv-inp.inc'
        CHARACTER*(*) line
        INTEGER k,l,LinTyp
        REAL RhoI
C@lbl bvs 1997Jul24 Lstr must be INTEGER !
        INTEGER Lstr
C@lbl bvs 1997Jul24 declare lenstr
        INTEGER LENSTR

        k=0
        Lstr=LENSTR(UsrAfc(nUsrAfc))
C       Range 1:
          Variab='F2:RhoI'
        Clines10=k
        CALL GETLDat(Line,k,l,LDat(ptrl),ptrl,1.2)
        RhoI=Ldat(ptrl-1)
C@NBI PGS 2003-04-25 : simplification
CC      IF (LDat(ptrl-1).EQ.0.) THEN
        IF (RhoI.EQ.0.) THEN
           CALL INERR('&-F2:  *'//UsrAfc(nUsrAfc)(1:Lstr)//
     &             ':  1.line, 1.input: RhoI',
     &             'Air density at test conditions is zero!'
     &             ,.FALSE.,2)
        ENDIF
          Variab='F2:Cq'
        Clines10=k
C@empa aw 2000apr12 default according UG
CC	CALL GETLDat(Line,k,l,LDat(ptrl),ptrl,0.001)
        CALL GETLDat(Line,k,l,LDat(ptrl),ptrl,0.1)
C       convert to kg/s with actual RhoI
        LDat(ptrl-1)=ifact(UnitFva)*LDat(ptrl-1)*RhoI**ifact(UnitFvFlag)
          Variab='F2:ExpN'
        Clines10=k
        CALL GETLDat(Line,k,l,LDat(ptrl),ptrl,0.65)
C       convert C to kg/s/@1Pa
        LDat(ptrl-2)=LDat(ptrl-2)/(ABS(ifact(UnitP)))**LDat(ptrl-1)
          Variab='F2: Fva-setpoint'
        Clines10=k
        CALL GETLDat(Line,k,l,LDat(ptrl),ptrl,0.)
C       convert to kg/s with actual RhoI
        LDat(ptrl-1)=ifact(UnitFva)*LDat(ptrl-1)*RhoI**ifact(UnitFvFlag)
          Variab='F2: Fva-setpoint negative flow'
        Clines10=k
        CALL GETLDat(Line,k,l,LDat(ptrl),ptrl,0.)
C       convert to kg/s with actual RhoI
        LDat(ptrl-1)=ifact(UnitFva)*LDat(ptrl-1)*RhoI**ifact(UnitFvFlag)

C Facade leakage
        CALL readlin(line, LinTyp, K, .True.)
        IF (FlgEnd) GOTO 99
        IF (LinTyp .NE. TDATA) THEN
           CALL INERR('&-F2:  *'//UsrAfc(nUsrAfc)(1:Lstr)//
     &             ':  2.line:',
     &             'No data for facade leakage!'
     &             ,.FALSE.,2)
           GOTO 99
        ENDIF
        l=lenstr(line)
          Variab='F2:Facade_Cs'
        Clines10=k
        CALL GETLDat(Line,k,l,LDat(ptrl),ptrl,0.001)
          Variab='F2:Facade_ExpN'
        Clines10=k
        CALL GETLDat(Line,k,l,LDat(ptrl),ptrl,0.65)
C       convert Cs to kg/s/@1Pa
C@tno jcp 1997Jun16 (missed this one on June 1996)
C		added the possible Cm units for error messages
        CALL convcs(iunit(UnitCm),ldat(ptrl-2),ldat(ptrl-1),
     &   UnitStr(UnitCm))

C filter
        CALL readlin(line, LinTyp, K, .True.)
        IF (FlgEnd) GOTO 99
        IF (LinTyp .NE. TDATA) THEN
           CALL INERR('&-F2:  *'//UsrAfc(nUsrAfc)(1:Lstr)//
     &             ':  3.line:',
     &             'No filter data!'
     &             ,.FALSE.,2)
           GOTO 99
        ENDIF


C read next line for next keyword
        CALL readlin(line, LinTyp, K, .True.)

99	CONTINUE
        RETURN
        END

Ch***********************************************************************
        SUBROUTINE inF3(Line, LinTyp, L, K)
C@empa aw 1995jul12 new version for new flow controller routine
Ch***********************************************************************

      IMPLICIT NONE
        INCLUDE 'comv-inp.inc'
CC	INCLUDE 'comv-uni.inc'
        CHARACTER*(*) line
        INTEGER k,l,LinTyp,n
        REAL RhoI
C@lbl bvs 1997Jul24 Lstr must be INTEGER !
        INTEGER Lstr
C@lbl bvs 1997Jul24 declare lenstr
        INTEGER LENSTR

        REAL Cf(0:5),Fvad,slope,PresR
        DOUBLE PRECISION R3Fva1,R3Fva2,PresD
        LOGICAL NotFound

        k=0
        Lstr=LENSTR(UsrAfc(nUsrAfc))
C ptrl0 points to the start of F3 data.
        ptrl0=ptrl-1
C       Range 1:
          Variab='F3:RhoI'
        Clines10=k
        CALL GETLDat(Line,k,l,LDat(ptrl),ptrl,1.2)
        RhoI=Ldat(ptrl-1)
C@NBI PGS 2003-04-25 : simplification
CC      IF (LDat(ptrl-1).EQ.0.) THEN
        IF (RhoI.EQ.0.) THEN
           CALL INERR('&-F3:  *'//UsrAfc(nUsrAfc)(1:Lstr)//
     &             ':  1.line, 1.input: RhoI',
     &             'Air density at test conditions is zero!'
     &             ,.FALSE.,2)
        ENDIF

          Variab='F3:Cq'
        Clines10=k
C@empa aw 2000apr12 default according UG
CC	CALL GETLDat(Line,k,l,LDat(ptrl),ptrl,0.001)
        CALL GETLDat(Line,k,l,LDat(ptrl),ptrl,0.1)
C       convert to kg/s with actual RhoI
        LDat(ptrl-1)=ifact(UnitFva)*LDat(ptrl-1)*RhoI**ifact(UnitFvFlag)
          Variab='F3:ExpN'
        Clines10=k
        CALL GETLDat(Line,k,l,LDat(ptrl),ptrl,0.65)
C       convert C to kg/s/@1Pa
        LDat(ptrl-2)=LDat(ptrl-2)/(ABS(ifact(UnitP)))**LDat(ptrl-1)

C       Range 2:
        Call readlin(line, LinTyp, K, .True.)
        IF (FlgEnd) GOTO 99
        IF (LinTyp .NE. TDATA) THEN
           CALL INERR('&-F3:  *'//UsrAfc(nUsrAfc)(1:Lstr)//
     &             ':  2.line:',
     &             'No data for range 2!'
     &             ,.FALSE.,2)
           GOTO 99
        ENDIF

        l=lenstr(line)
          Variab='F3:R2Dp1'
        Clines10=k
        CALL GETLDat(Line,k,l,LDat(ptrl),ptrl,0.0)
        IF (LDat(ptrl-1).EQ.0.) THEN
           CALL INERR('&-F3:  *'//UsrAfc(nUsrAfc)(1:Lstr)//
     &             ':  2.line, 1.input: R2Dp1',
     &             'Estimation for pressure where range 2 starts'//
     &             'is zero !'
     &             ,.FALSE.,2)
        ENDIF
C       convert to Pa
        LDat(ptrl-1)=(ABS(ifact(UnitP)))*LDat(ptrl-1)
          Variab='F3:R2C0'
        Clines10=k
        CALL GETLDat(Line,k,l,LDat(ptrl),ptrl,0.0)
C       convert to kg/s with actual RhoI
        LDat(ptrl-1)=ifact(UnitFva)*LDat(ptrl-1)*RhoI**ifact(UnitFvFlag)
         Variab='F3:R2C1'
        Clines10=k
        CALL GETLDat(Line,k,l,LDat(ptrl),ptrl,0.0)
C       convert to kg/s/Pa with actual RhoI
        LDat(ptrl-1)=ifact(UnitFva)*LDat(ptrl-1)*RhoI**ifact(UnitFvFlag)
        LDat(ptrl-1)=LDat(ptrl-1)/(ABS(ifact(UnitP)))
          Variab='F3:R2C2'
        Clines10=k
        CALL GETLDat(Line,k,l,LDat(ptrl),ptrl,0.0)
C       convert to kg/s/Pa^2 with actual RhoI
        LDat(ptrl-1)=ifact(UnitFva)*LDat(ptrl-1)*RhoI**ifact(UnitFvFlag)
        LDat(ptrl-1)=LDat(ptrl-1)/(ABS(ifact(UnitP)))**2
          Variab='F3:R2C3'
        Clines10=k
        CALL GETLDat(Line,k,l,LDat(ptrl),ptrl,0.0)
C       convert to kg/s/Pa^3 with actual RhoI
        LDat(ptrl-1)=ifact(UnitFva)*LDat(ptrl-1)*RhoI**ifact(UnitFvFlag)
        LDat(ptrl-1)=LDat(ptrl-1)/(ABS(ifact(UnitP)))**3
          Variab='F3:R2C4'
        Clines10=k
        CALL GETLDat(Line,k,l,LDat(ptrl),ptrl,0.0)
C       convert to kg/s/Pa^4 with actual RhoI
        LDat(ptrl-1)=ifact(UnitFva)*LDat(ptrl-1)*RhoI**ifact(UnitFvFlag)
        LDat(ptrl-1)=LDat(ptrl-1)/(ABS(ifact(UnitP)))**4
          Variab='F3:R2C5'
        Clines10=k
        CALL GETLDat(Line,k,l,LDat(ptrl),ptrl,0.0)
C       convert to kg/s/Pa^5 with actual RhoI
        LDat(ptrl-1)=ifact(UnitFva)*LDat(ptrl-1)*RhoI**ifact(UnitFvFlag)
        LDat(ptrl-1)=LDat(ptrl-1)/(ABS(ifact(UnitP)))**5

C       Range 3:
        CALL readlin(line, LinTyp, K, .True.)
        IF (FlgEnd) GOTO 99
        IF (LinTyp .NE. TDATA) THEN
           CALL INERR('&-F3:  *'//UsrAfc(nUsrAfc)(1:Lstr)//
     &             ':  3.line:',
     &             'No data for range 3!'
     &             ,.FALSE.,2)
           GOTO 99
        ENDIF
        l=lenstr(line)
          Variab='F3:R3Dp1'
        Clines10=k
        CALL GETLDat(Line,k,l,LDat(ptrl),ptrl,0.0)
        IF (LDat(ptrl-1).EQ.0.) THEN
           CALL INERR('&-F3:  *'//UsrAfc(nUsrAfc)(1:Lstr)//
     &             ':  3.line, 1.input: R3Dp1',
     &             'Estimation for pressure where range 3 starts'//
     &             'is zero !'
     &             ,.FALSE.,2)
        ENDIF
C       convert to Pa
        LDat(ptrl-1)=(ABS(ifact(UnitP)))*LDat(ptrl-1)
          Variab='F3:R3C0'
        Clines10=k
        CALL GETLDat(Line,k,l,LDat(ptrl),ptrl,0.0)
C       convert to kg/s with actual RhoI
        LDat(ptrl-1)=ifact(UnitFva)*LDat(ptrl-1)*RhoI**ifact(UnitFvFlag)
          Variab='F3:R3C1'
        Clines10=k
        CALL GETLDat(Line,k,l,LDat(ptrl),ptrl,0.0)
C       convert to kg/s/Pa with actual RhoI
        LDat(ptrl-1)=ifact(UnitFva)*LDat(ptrl-1)*RhoI**ifact(UnitFvFlag)
        LDat(ptrl-1)=LDat(ptrl-1)/(ABS(ifact(UnitP)))
          Variab='F3:R3C2'
        Clines10=k
        CALL GETLDat(Line,k,l,LDat(ptrl),ptrl,0.0)
C       convert to kg/s/Pa^2 with actual RhoI
        LDat(ptrl-1)=ifact(UnitFva)*LDat(ptrl-1)*RhoI**ifact(UnitFvFlag)
        LDat(ptrl-1)=LDat(ptrl-1)/(ABS(ifact(UnitP)))**2
          Variab='F3:R3C3'
        Clines10=k
        CALL GETLDat(Line,k,l,LDat(ptrl),ptrl,0.0)
C       convert to kg/s/Pa^3 with actual RhoI
        LDat(ptrl-1)=ifact(UnitFva)*LDat(ptrl-1)*RhoI**ifact(UnitFvFlag)
        LDat(ptrl-1)=LDat(ptrl-1)/(ABS(ifact(UnitP)))**3
          Variab='F3:R3C4'
        Clines10=k
        CALL GETLDat(Line,k,l,LDat(ptrl),ptrl,0.0)
C       convert to kg/s/Pa^4 with actual RhoI
        LDat(ptrl-1)=ifact(UnitFva)*LDat(ptrl-1)*RhoI**ifact(UnitFvFlag)
        LDat(ptrl-1)=LDat(ptrl-1)/(ABS(ifact(UnitP)))**4
          Variab='F3:R3C5'
        Clines10=k
        CALL GETLDat(Line,k,l,LDat(ptrl),ptrl,0.0)
C       convert to kg/s/Pa^5 with actual RhoI
        LDat(ptrl-1)=ifact(UnitFva)*LDat(ptrl-1)*RhoI**ifact(UnitFvFlag)
        LDat(ptrl-1)=LDat(ptrl-1)/(ABS(ifact(UnitP)))**5
          Variab='F3:Pmax'
        Clines10=k
        CALL GETLDat(Line,k,l,LDat(ptrl),ptrl,0.0)
        IF (LDat(ptrl-1).EQ.0.) THEN
           CALL INERR('&-F3:  *'//UsrAfc(nUsrAfc)(1:Lstr)//
     &             ':  3.line, 8.input: Pmax',
     &             'Pmax is zero !'
     &             ,.FALSE.,2)
        ENDIF

C       convert to Pa
        LDat(ptrl-1)=(ABS(ifact(UnitP)))*LDat(ptrl-1)
C       reserve space for slope
        ptrl=ptrl+1

C Filter
        CALL readlin(line, LinTyp, K, .True.)
        IF (FlgEnd) GOTO 99
        IF (LinTyp .NE. TDATA) THEN
           CALL INERR('&-F3:  *'//UsrAfc(nUsrAfc)(1:Lstr)//
     &             ':  4.line:',
     &             'No filter data!'
     &             ,.FALSE.,2)
           GOTO 99
        ENDIF
        l=lenstr(line)
        CALL FILTER(Line,K,L)

C read next line for next keyword
        Call readlin(line, LinTyp, K, .True.)

C Correct the start and end of range values to the exact cross points of the
C curves.
C We take here DDifLim because it is not sure that DifLim is already read.
        CALL Cross(NotFound,Ldat(ptrl0+4),LDat(ptrl0+5),LDat(ptrl0+2),
     &  LDat(ptrl0+3),DDifLim)
        IF (NotFound) THEN
           CALL INERR('&-F3:  *'//UsrAfc(nUsrAfc)(1:Lstr)//
     &             ': No cross point found between range 1 and 2 !'
     &             ,' ',.FALSE.,2)
        ENDIF
        DO 10 n=0,5
           Cf(n)=LDat(ptrl0+5+n)-LDat(ptrl0+12+n)
10      CONTINUE
        CALL Cross(NotFound,Ldat(ptrl0+11),Cf,0.,0.,DDifLim)
        IF (NotFound) THEN
           CALL INERR('&-F3:  *'//UsrAfc(nUsrAfc)(1:Lstr)//
     &             ': No cross point found between range 2 and 3 !'
     &             ,' ',.FALSE.,2)
        ENDIF
        PresD=Ldat(ptrl0+11)
        CALL FONX(0.,0.,PresD,LDat(ptrl0+12),DDifLim,R3Fva1,Fvad)
        PresD=Ldat(ptrl0+18)
        CALL FONX(0.,0.,PresD,LDat(ptrl0+12),DDifLim,R3Fva2,Fvad)
        slope=R3Fva2/Ldat(ptrl0+18)
        IF (R3Fva1.GT.R3Fva2)THEN
C          range 3 is decreasing --> look for a root near Pmax
           PresR=LDat(ptrl0+18)
           CALL Cross(NotFound,PresR,LDat(ptrl0+12),0.,0.,DDifLim)
           IF (NotFound.OR.(PresR.LT.LDat(ptrl0+11))) THEN
             CALL INERR('&-F3:  *'//UsrAfc(nUsrAfc)(1:Lstr)//
     &             ': No root found near Pmax in range 3 ! ',
     &             'Flow will increase linear beyond Pmax !'
     &             ,.FALSE.,1)
           ELSE
             LDat(ptrl0+18)=PresR
             slope=0.
           ENDIF
        ENDIF
        Ldat(ptrl0+19)=slope

99	CONTINUE
        RETURN
        END

Ch***********************************************************************
        SUBROUTINE inF4(Line, LinTyp, L, K)
C@empa aw 1995jul12 new version for new flow controller routine
Ch***********************************************************************

      IMPLICIT NONE
        INCLUDE 'comv-inp.inc'
CC	INCLUDE 'comv-uni.inc'
        CHARACTER*(*) line
        INTEGER k,l,LinTyp,n
        REAL RhoI
C@lbl bvs 1997Jul24 Lstr must be INTEGER !
        INTEGER Lstr
C@lbl bvs 1997Jul24 declare lenstr
        INTEGER LENSTR

        REAL Cf(0:5),Fvad,slope,PresR
        DOUBLE PRECISION R3Fva1,R3Fva2,PresD
        LOGICAL NotFound

        k=0
C ptrl0 points to the start of F4 data
        ptrl0=ptrl-1
        Lstr=LENSTR(UsrAfc(nUsrAfc))
C       Range 1:
          Variab='F4:RhoI'
        Clines10=k
C@empa aw 2000apr12 default according UG
CC	CALL GETLDat(Line,k,l,LDat(ptrl),ptrl,0.0)
        CALL GETLDat(Line,k,l,LDat(ptrl),ptrl,1.2)
        RhoI=Ldat(ptrl-1)
C@NBI PGS 2003-04-25 : simplification
CC      IF (LDat(ptrl-1).EQ.0.) THEN
        IF (RhoI.EQ.0.) THEN
           CALL INERR('&-F4:  *'//UsrAfc(nUsrAfc)(1:Lstr)//
     &             ':  1.line, 1.input: RhoI',
     &             'Air density at test conditions is zero!'
     &             ,.FALSE.,2)
        ENDIF
          Variab='F4:Cq'
        Clines10=k
C@empa aw 2000apr12 default according UG
CC	CALL GETLDat(Line,k,l,LDat(ptrl),ptrl,0.0)
        CALL GETLDat(Line,k,l,LDat(ptrl),ptrl,0.1)
C       convert to kg/s with actual RhoI
        LDat(ptrl-1)=ifact(UnitFva)*LDat(ptrl-1)*RhoI**ifact(UnitFvFlag)
          Variab='F4:ExpN'
        Clines10=k
C@empa aw 2000apr12 default according UG
CC	CALL GETLDat(Line,k,l,LDat(ptrl),ptrl,0.0)
        CALL GETLDat(Line,k,l,LDat(ptrl),ptrl,0.65)
C       convert C to kg/s/@1Pa
        LDat(ptrl-2)=LDat(ptrl-2)/(ABS(ifact(UnitP)))**LDat(ptrl-1)

C       Range 2:
        CALL readlin(line, LinTyp, K, .True.)
        IF (FlgEnd) GOTO 99
        IF (LinTyp .NE. TDATA) THEN
           CALL INERR('&-F4:  *'//UsrAfc(nUsrAfc)(1:Lstr)//
     &             ':  2.line:',
     &             'No data for range 2!'
     &             ,.FALSE.,2)
           GOTO 99
        ENDIF
        l=lenstr(line)
          Variab='F4:R2Dp1'
        Clines10=k
        CALL GETLDat(Line,k,l,LDat(ptrl),ptrl,0.0)
        IF (LDat(ptrl-1).EQ.0.) THEN
           CALL INERR('&-F4:  *'//UsrAfc(nUsrAfc)(1:Lstr)//
     &             ':  2.line, 1.input: R2Dp1',
     &             'Estimation for pressure where range 2 starts'//
     &             'is zero !'
     &             ,.FALSE.,2)
        ENDIF
C       convert to Pa
        LDat(ptrl-1)=(ABS(ifact(UnitP)))*LDat(ptrl-1)
          Variab='F4:R2C0'
        Clines10=k
        CALL GETLDat(Line,k,l,LDat(ptrl),ptrl,0.0)
C       convert to kg/s with actual RhoI
        LDat(ptrl-1)=ifact(UnitFva)*LDat(ptrl-1)*RhoI**ifact(UnitFvFlag)
          Variab='F4:R2C1'
        Clines10=k
        CALL GETLDat(Line,k,l,LDat(ptrl),ptrl,0.0)
C       convert to kg/s/Pa with actual RhoI
        LDat(ptrl-1)=ifact(UnitFva)*LDat(ptrl-1)*RhoI**ifact(UnitFvFlag)
        LDat(ptrl-1)=LDat(ptrl-1)/(ABS(ifact(UnitP)))
          Variab='F4:R2C2'
        Clines10=k
        CALL GETLDat(Line,k,l,LDat(ptrl),ptrl,0.0)
C       convert to kg/s/Pa^2 with actual RhoI
        LDat(ptrl-1)=ifact(UnitFva)*LDat(ptrl-1)*RhoI**ifact(UnitFvFlag)
        LDat(ptrl-1)=LDat(ptrl-1)/(ABS(ifact(UnitP)))**2
          Variab='F4:R2C3'
        Clines10=k
        CALL GETLDat(Line,k,l,LDat(ptrl),ptrl,0.0)
C       convert to kg/s/Pa^3 with actual RhoI
        LDat(ptrl-1)=ifact(UnitFva)*LDat(ptrl-1)*RhoI**ifact(UnitFvFlag)
        LDat(ptrl-1)=LDat(ptrl-1)/(ABS(ifact(UnitP)))**3
          Variab='F4:R2C4'
        Clines10=k
        CALL GETLDat(Line,k,l,LDat(ptrl),ptrl,0.0)
C       convert to kg/s/Pa^4 with actual RhoI
        LDat(ptrl-1)=ifact(UnitFva)*LDat(ptrl-1)*RhoI**ifact(UnitFvFlag)
        LDat(ptrl-1)=LDat(ptrl-1)/(ABS(ifact(UnitP)))**4
          Variab='F4:R2C5'
        Clines10=k
        CALL GETLDat(Line,k,l,LDat(ptrl),ptrl,0.0)
C       convert to kg/s/Pa^5 with actual RhoI
        LDat(ptrl-1)=ifact(UnitFva)*LDat(ptrl-1)*RhoI**ifact(UnitFvFlag)
        LDat(ptrl-1)=LDat(ptrl-1)/(ABS(ifact(UnitP)))**5

C       Range 3:
        CALL readlin(line, LinTyp, K, .True.)
        IF (FlgEnd) GOTO 99
        IF (LinTyp .NE. TDATA) THEN
           CALL INERR('&-F4:  *'//UsrAfc(nUsrAfc)(1:Lstr)//
     &             ':  3.line:',
     &             'No data for range 3!'
     &             ,.FALSE.,2)
           GOTO 99
        ENDIF
        l=lenstr(line)
          Variab='F4:R3Dp1'
        Clines10=k
        CALL GETLDat(Line,k,l,LDat(ptrl),ptrl,0.0)
        IF (LDat(ptrl-1).EQ.0.) THEN
           CALL INERR('&-F4:  *'//UsrAfc(nUsrAfc)(1:Lstr)//
     &             ':  3.line, 1.input: R3Dp1',
     &             'Estimation for pressure where range 3 starts'//
     &             'is zero !'
     &             ,.FALSE.,2)
        ENDIF
C       convert to Pa
        LDat(ptrl-1)=(ABS(ifact(UnitP)))*LDat(ptrl-1)
          Variab='F4:R3C0'
        Clines10=k
        CALL GETLDat(Line,k,l,LDat(ptrl),ptrl,0.0)
C       convert to kg/s with actual RhoI
        LDat(ptrl-1)=ifact(UnitFva)*LDat(ptrl-1)*RhoI**ifact(UnitFvFlag)
          Variab='F4:R3C1'
        Clines10=k
        CALL GETLDat(Line,k,l,LDat(ptrl),ptrl,0.0)
C       convert to kg/s/Pa with actual RhoI
        LDat(ptrl-1)=ifact(UnitFva)*LDat(ptrl-1)*RhoI**ifact(UnitFvFlag)
        LDat(ptrl-1)=LDat(ptrl-1)/(ABS(ifact(UnitP)))
          Variab='F4:R3C2'
        Clines10=k
        CALL GETLDat(Line,k,l,LDat(ptrl),ptrl,0.0)
C       convert to kg/s/Pa^2 with actual RhoI
        LDat(ptrl-1)=ifact(UnitFva)*LDat(ptrl-1)*RhoI**ifact(UnitFvFlag)
        LDat(ptrl-1)=LDat(ptrl-1)/(ABS(ifact(UnitP)))**2
          Variab='F4:R3C3'
        Clines10=k
        CALL GETLDat(Line,k,l,LDat(ptrl),ptrl,0.0)
C       convert to kg/s/Pa^3 with actual RhoI
        LDat(ptrl-1)=ifact(UnitFva)*LDat(ptrl-1)*RhoI**ifact(UnitFvFlag)
        LDat(ptrl-1)=LDat(ptrl-1)/(ABS(ifact(UnitP)))**3
          Variab='F4:R3C4'
        Clines10=k
        CALL GETLDat(Line,k,l,LDat(ptrl),ptrl,0.0)
C       convert to kg/s/Pa^4 with actual RhoI
        LDat(ptrl-1)=ifact(UnitFva)*LDat(ptrl-1)*RhoI**ifact(UnitFvFlag)
        LDat(ptrl-1)=LDat(ptrl-1)/(ABS(ifact(UnitP)))**4
          Variab='F4:R3C5'
        Clines10=k
        CALL GETLDat(Line,k,l,LDat(ptrl),ptrl,0.0)
C       convert to kg/s/Pa^5 with actual RhoI
        LDat(ptrl-1)=ifact(UnitFva)*LDat(ptrl-1)*RhoI**ifact(UnitFvFlag)
        LDat(ptrl-1)=LDat(ptrl-1)/(ABS(ifact(UnitP)))**5
          Variab='F4:Pmax'
        Clines10=k
        CALL GETLDat(Line,k,l,LDat(ptrl),ptrl,0.0)
        IF (LDat(ptrl-1).EQ.0.) THEN
           CALL INERR('&-F4:  *'//UsrAfc(nUsrAfc)(1:Lstr)//
     &             ':  3.line, 8.input:',
     &             'Pmax is zero !'
     &             ,.FALSE.,2)
        ENDIF
C       convert to Pa
        LDat(ptrl-1)=(ABS(ifact(UnitP)))*LDat(ptrl-1)
C       reserve space for slope
        ptrl=ptrl+1

C       negative range 2:
C       In the flow controller routine FlCon4 we process with ABS(dp).
C       for that reason the polynom coefficients for the negative range are
C       transformed here for a rotation of 180 degrees around the origin.
C       That means: Ci  = -Ci if (i mod 2).eq.0
C                   Dpi= -Dpi
        CALL readlin(line, LinTyp, K, .True.)
        IF (FlgEnd) GOTO 99
        IF (LinTyp .NE. TDATA) THEN
           CALL INERR('&-F4:  *'//UsrAfc(nUsrAfc)(1:Lstr)//
     &             ':  4.line:',
     &             'No data for negative range 2!'
     &             ,.FALSE.,2)
           GOTO 99
        ENDIF
        l=lenstr(line)
          Variab='F4:R2Dp1n'
        Clines10=k
        CALL GETLDat(Line,k,l,LDat(ptrl),ptrl,0.0)
        IF (LDat(ptrl-1).EQ.0.) THEN
           CALL INERR('&-F4:  *'//UsrAfc(nUsrAfc)(1:Lstr)//
     &             ':  4.line, 1.input: R2Dp1n',
     &             'Estimation for pressure where range 2 starts'//
     &             'is zero !'
     &             ,.FALSE.,2)
        ENDIF
C       convert to Pa
        LDat(ptrl-1)=(ABS(ifact(UnitP)))*LDat(ptrl-1)
        LDat(ptrl-1)=-LDat(ptrl-1)
          Variab='F4:R2C0n'
        Clines10=k
        CALL GETLDat(Line,k,l,LDat(ptrl),ptrl,0.0)
C       convert to kg/s with actual RhoI
        LDat(ptrl-1)=ifact(UnitFva)*LDat(ptrl-1)*RhoI**ifact(UnitFvFlag)
        LDat(ptrl-1)=-LDat(ptrl-1)
          Variab='F4:R2C1n'
        Clines10=k
        CALL GETLDat(Line,k,l,LDat(ptrl),ptrl,0.0)
C       convert to kg/s/Pa with actual RhoI
        LDat(ptrl-1)=ifact(UnitFva)*LDat(ptrl-1)*RhoI**ifact(UnitFvFlag)
        LDat(ptrl-1)=LDat(ptrl-1)/(ABS(ifact(UnitP)))
          Variab='F4:R2C2n'
        Clines10=k
        CALL GETLDat(Line,k,l,LDat(ptrl),ptrl,0.0)
C       convert to kg/s/Pa^2 with actual RhoI
        LDat(ptrl-1)=ifact(UnitFva)*LDat(ptrl-1)*RhoI**ifact(UnitFvFlag)
        LDat(ptrl-1)=LDat(ptrl-1)/(ABS(ifact(UnitP)))**2
        LDat(ptrl-1)=-LDat(ptrl-1)
          Variab='F4:R2C3n'
        Clines10=k
        CALL GETLDat(Line,k,l,LDat(ptrl),ptrl,0.0)
C       convert to kg/s/Pa^3 with actual RhoI
        LDat(ptrl-1)=ifact(UnitFva)*LDat(ptrl-1)*RhoI**ifact(UnitFvFlag)
        LDat(ptrl-1)=LDat(ptrl-1)/(ABS(ifact(UnitP)))**3
          Variab='F4:R2C4n'
        Clines10=k
        CALL GETLDat(Line,k,l,LDat(ptrl),ptrl,0.0)
C       convert to kg/s/Pa^4 with actual RhoI
        LDat(ptrl-1)=ifact(UnitFva)*LDat(ptrl-1)*RhoI**ifact(UnitFvFlag)
        LDat(ptrl-1)=LDat(ptrl-1)/(ABS(ifact(UnitP)))**4
        LDat(ptrl-1)=-LDat(ptrl-1)
          Variab='F4:R2C5n'
        Clines10=k
        CALL GETLDat(Line,k,l,LDat(ptrl),ptrl,0.0)
C       convert to kg/s/Pa^5 with actual RhoI
        LDat(ptrl-1)=ifact(UnitFva)*LDat(ptrl-1)*RhoI**ifact(UnitFvFlag)
        LDat(ptrl-1)=LDat(ptrl-1)/(ABS(ifact(UnitP)))**5

C       negative range 3:
        CALL readlin(line, LinTyp, K, .True.)
        IF (FlgEnd) GOTO 99
        IF (LinTyp .NE. TDATA) THEN
           CALL INERR('&-F4:  *'//UsrAfc(nUsrAfc)(1:Lstr)//
     &             ':  5.line:',
     &             'No data for negative range 3!'
     &             ,.FALSE.,2)
           GOTO 99
        ENDIF

        l=lenstr(line)
          Variab='F4:R3Dp1n'
        Clines10=k
        CALL GETLDat(Line,k,l,LDat(ptrl),ptrl,0.0)
        IF (LDat(ptrl-1).EQ.0.) THEN
           CALL INERR('&-F4:  *'//UsrAfc(nUsrAfc)(1:Lstr)//
     &             ':  5.line, 1.input: R3Dp1n',
     &             'Estimation for pressure where range 3 starts'//
     &             'is zero !'
     &             ,.FALSE.,2)
        ENDIF
C       convert to Pa
        LDat(ptrl-1)=(ABS(ifact(UnitP)))*LDat(ptrl-1)
        LDat(ptrl-1)=-LDat(ptrl-1)
          Variab='F4:R3C0n'
        Clines10=k
        CALL GETLDat(Line,k,l,LDat(ptrl),ptrl,0.0)
C       convert to kg/s with actual RhoI
        LDat(ptrl-1)=ifact(UnitFva)*LDat(ptrl-1)*RhoI**ifact(UnitFvFlag)
        LDat(ptrl-1)=-LDat(ptrl-1)
          Variab='F4:R3C1n'
        Clines10=k
        CALL GETLDat(Line,k,l,LDat(ptrl),ptrl,0.0)
C       convert to kg/s/Pa with actual RhoI
        LDat(ptrl-1)=ifact(UnitFva)*LDat(ptrl-1)*RhoI**ifact(UnitFvFlag)
        LDat(ptrl-1)=LDat(ptrl-1)/(ABS(ifact(UnitP)))
          Variab='F4:R3C2n'
        Clines10=k
        CALL GETLDat(Line,k,l,LDat(ptrl),ptrl,0.0)
C       convert to kg/s/Pa^2 with actual RhoI
        LDat(ptrl-1)=ifact(UnitFva)*LDat(ptrl-1)*RhoI**ifact(UnitFvFlag)
        LDat(ptrl-1)=LDat(ptrl-1)/(ABS(ifact(UnitP)))**2
        LDat(ptrl-1)=-LDat(ptrl-1)
          Variab='F4:R3C3n'
        Clines10=k
        CALL GETLDat(Line,k,l,LDat(ptrl),ptrl,0.0)
C       convert to kg/s/Pa^3 with actual RhoI
        LDat(ptrl-1)=ifact(UnitFva)*LDat(ptrl-1)*RhoI**ifact(UnitFvFlag)
        LDat(ptrl-1)=LDat(ptrl-1)/(ABS(ifact(UnitP)))**3
          Variab='F4:R3C4n'
        Clines10=k
        CALL GETLDat(Line,k,l,LDat(ptrl),ptrl,0.0)
C       convert to kg/s/Pa^4 with actual RhoI
        LDat(ptrl-1)=ifact(UnitFva)*LDat(ptrl-1)*RhoI**ifact(UnitFvFlag)
        LDat(ptrl-1)=LDat(ptrl-1)/(ABS(ifact(UnitP)))**4
        LDat(ptrl-1)=-LDat(ptrl-1)
          Variab='F4:R3C5n'
        Clines10=k
        CALL GETLDat(Line,k,l,LDat(ptrl),ptrl,0.0)
C       convert to kg/s/Pa^5 with actual RhoI
        LDat(ptrl-1)=ifact(UnitFva)*LDat(ptrl-1)*RhoI**ifact(UnitFvFlag)
        LDat(ptrl-1)=LDat(ptrl-1)/(ABS(ifact(UnitP)))**5
          Variab='F4:Pmaxn'
        Clines10=k
        CALL GETLDat(Line,k,l,LDat(ptrl),ptrl,0.0)
        IF (LDat(ptrl-1).EQ.0.) THEN
           CALL INERR('&-F4:  *'//UsrAfc(nUsrAfc)(1:Lstr)//
     &             ':  5.line, 8.input: Pmaxn',
     &             'Pmaxn is zero !'
     &             ,.FALSE.,2)
        ENDIF
C       convert to Pa
        LDat(ptrl-1)=(ABS(ifact(UnitP)))*LDat(ptrl-1)
        LDat(ptrl-1)=-LDat(ptrl-1)
C       reserve space for slope
C@empa aw 2000jan20 Yes, but do it!
CC        ptrl=ptrl+0
        ptrl=ptrl+1

C Filter
        CALL readlin(line, LinTyp, K, .True.)
        IF (FlgEnd) GOTO 99
        IF (LinTyp .NE. TDATA) THEN
           CALL INERR('&-F4:  *'//UsrAfc(nUsrAfc)(1:Lstr)//
     &             ':  6.line:',
     &             'No filter data!'
     &             ,.FALSE.,2)
           GOTO 99
        ENDIF

        l=lenstr(line)
        CALL FILTER(Line,K,L)

C read next line for next keyword
        Call readlin(line, LinTyp, K, .True.)

C Correct the start and end of range values to the exact cross points of the
C curves.
C We take here DDifLim because it is not shure that DifLim is already read.
        CALL Cross(NotFound,Ldat(ptrl0+4),LDat(ptrl0+5),LDat(ptrl0+2),
     &  LDat(ptrl0+3),DDifLim)
        IF (NotFound) THEN
           CALL INERR('&-F4:  *'//UsrAfc(nUsrAfc)(1:Lstr)//
     &             'No cross point found between positive range '//
     &             '1 and 2 !',' ',.FALSE.,2)
        ENDIF
        DO 10 n=0,5
           Cf(n)=LDat(ptrl0+5+n)-LDat(ptrl0+12+n)
10      CONTINUE
        CALL Cross(NotFound,Ldat(ptrl0+11),Cf,0.,0.,DDifLim)
        IF (NotFound) THEN
           CALL INERR('&-F4:  *'//UsrAfc(nUsrAfc)(1:Lstr)//
     &             'No cross point found between positive range '//
     &             '2 and 3 !',' ',.FALSE.,2)
        ENDIF

        PresD=Ldat(ptrl0+11)
        CALL FONX(0.,0.,PresD,LDat(ptrl0+12),DDifLim,R3Fva1,Fvad)
        PresD=Ldat(ptrl0+18)
        CALL FONX(0.,0.,PresD,LDat(ptrl0+12),DDifLim,R3Fva2,Fvad)
        slope=R3Fva2/Ldat(ptrl0+18)
        IF (R3Fva1.GT.R3Fva2)THEN
C          range 3 is decreasing --> look for a root near Pmax
           PresR=LDat(ptrl0+18)
           CALL Cross(NotFound,PresR,LDat(ptrl0+12),0.,0.,DDifLim)
           IF (NotFound.OR.(PresR.LT.LDat(ptrl0+11))) THEN
             CALL INERR('&-F4:  *'//UsrAfc(nUsrAfc)(1:Lstr)//
     &             ': No root found near Pmax in positive range 3 ! ',
     &             'Flow will increase linear beyond Pmax !'
     &             ,.FALSE.,1)
           ELSE
             LDat(ptrl0+18)=PresR
             slope=0.
           ENDIF
        ENDIF
        Ldat(ptrl0+19)=slope

        CALL Cross(NotFound,Ldat(ptrl0+20),LDat(ptrl0+21),LDat(ptrl0+2),
     &  LDat(ptrl0+3),DDifLim)
        IF (NotFound) THEN
           CALL INERR('&-F4:  *'//UsrAfc(nUsrAfc)(1:Lstr)//
     &             'No cross point found between negative range '//
     &             '1 and 2 !',' ',.FALSE.,2)
        ENDIF
        DO 20 n=0,5
           Cf(n)=LDat(ptrl0+21+n)-LDat(ptrl0+28+n)
20      CONTINUE
        CALL Cross(NotFound,Ldat(ptrl0+27),Cf,0.,0.,DDifLim)
        IF (NotFound) THEN
           CALL INERR('&-F4:  *'//UsrAfc(nUsrAfc)(1:Lstr)//
     &             'No cross point found between negative range '//
     &             '2 and 3 !',' ',.FALSE.,2)
        ENDIF

        PresD=Ldat(ptrl0+27)
        CALL FONX(0.,0.,PresD,LDat(ptrl0+28),DDifLim,R3Fva1,Fvad)
        PresD=Ldat(ptrl0+34)
        CALL FONX(0.,0.,PresD,LDat(ptrl0+28),DDifLim,R3Fva2,Fvad)
        slope=R3Fva2/Ldat(ptrl0+34)
        IF (R3Fva1.GT.R3Fva2)THEN
C          range 3 is decreasing --> look for a root near Pmax
           PresR=LDat(ptrl0+34)
           CALL Cross(NotFound,PresR,LDat(ptrl0+28),0.,0.,DDifLim)
           IF (NotFound.OR.(PresR.LT.LDat(ptrl0+27))) THEN
             CALL INERR('&-F3:  *'//UsrAfc(nUsrAfc)(1:Lstr)//
     &             ': No root found near Pmax in negative range 3 ! ',
     &             'Flow will increase linear beyond Pmax !'
     &             ,.FALSE.,1)
           ELSE
             LDat(ptrl0+34)=PresR
             slope=0.
           ENDIF
        ENDIF
        Ldat(ptrl0+35)=slope

99	CONTINUE
        RETURN
        END

Ch***********************************************************************
C WIndow
C@tno jcp 1996Jul09_17:45:11 WI=inWI
        SUBROUTINE inWI(Line, LinTyp, L, K)
Ch***********************************************************************

      IMPLICIT NONE
        INCLUDE 'comv-inp.inc'
        INTEGER LenStr

        CHARACTER*(*) line
        character*160 nline
        INTEGER k,k1,l,Lstr,LinTyp
        INTEGER Nopen
        REAL HFactSum
        REAL ChkCdProd, ChkCdSum

        ChkCdProd=1.
        ChkCdSum=0.
        k=0
C reserve space for the number of lines used for defining the open situation
C Define ptrL0 as the element in Ldat where the Nopening data are stored
C in consecutive calls of WI  (ptrL0 is defined in include file)
        ptrL0=ptrl

        ptrl=ptrl+1
C@lbl dml 2000mar24 Add initialization.
C     ! Initialize Nopen to zero.
      LDat(ptrL0) = 0.

C closed: Cs; ExpN; Constrtyp; Lwmax; Lhmax; Lextra

          Variab='WI:closed Cs/m'
        CALL GETLDat(Line,k,l,LDat(ptrl),ptrl,0.0001)
          Variab='WI:closed ExpN'
        CALL GETLDat(Line,k,l,LDat(ptrl),ptrl,0.7000)
        IF ((Ldat(ptrl-1).GT.1.).OR.(Ldat(ptrl-1).LT.0.5))THEN
          Lstr=LENSTR(UsrAfc(nUsrAfc))
          CALL INERR('&-WI:  *'//UsrAfc(nUsrAfc)(1:Lstr)//
     &    ': Flow exponent is not within the '//
     &    'range 0.5...1.0 !',line,.FALSE.,2)
        ENDIF
C ExpN is needed for the conversion
C@tno jcp 1996Jun27_12:49:17 added the possible Cm units for error messages
        call convcs(iunit(UnitCm),ldat(ptrl-2),ldat(ptrl-1),
     &   UnitStr(UnitCm))

          Variab='WI:Constrtyp'
        CALL GETLDat(Line,k,l,LDat(ptrl),ptrl,1.0)
C@empa aw 2005may31 now we have 5 LVO types
        IF ((Ldat(ptrl-1).GT.5.).OR.(Ldat(ptrl-1).LT.1.0))THEN
          Lstr=LENSTR(UsrAfc(nUsrAfc))
          CALL INERR('&-WI:  *'//UsrAfc(nUsrAfc)(1:Lstr)//
     &    ':At the moment only LVO types 1 to 5 are available !',
     &    line,.FALSE.,2)
        ENDIF
          Variab='WI:Lwmax'
        CALL GETLDat(Line,k,l,LDat(ptrl),ptrl,1.0000)
          Variab='WI:Lhmax'
        CALL GETLDat(Line,k,l,LDat(ptrl),ptrl,1.0000)
          Variab='WI:Lextra;AxisHeight'
        CALL GETLDat(Line,k,l,LDat(ptrl),ptrl,0.0000)
C check AxisHeight of horizontal pivoting axis window
        IF (LDat(ptrl-4).EQ.2.)THEN
          IF ((Ldat(ptrl-1).GT.LDat(ptrl-2)).OR.
     &    (Ldat(ptrl-1).LT.0.)) THEN
            Lstr=LENSTR(UsrAfc(nUsrAfc))
            CALL INERR('&-WI:  *'//UsrAfc(nUsrAfc)(1:Lstr)//
     &      ': Axis height is negative or greater than Lhmax !',
     &      line,.FALSE.,2)
          ENDIF
        ENDIF

C If no more windows then there may be data describing the opening factor
C Lines 2,3,...
C open: openfract; contr; areaFact; heightFact; startHeight; Wscoop;FrameDir;
C	N opening;d Height between openings

        Call readlin(line, LinTyp, K, .True.)
        if (FlgEnd) goto 99
C If a name, then another window follows
        if (LinTyp .eq. TNAME) goto 99
        if (LinTyp .ne. TDATA) goto 99
        l=lenstr(line)

C Read ahead one line to see if we are past our data.  If so, then Line
C  contains the filter data and nline the data for the next keyword.

100	Call readlin(nline, LinTyp, K1, .True.)
C See if we are at the end of the data yet
        if (FlgEnd) goto 88
        if (LinTyp .ne. TDATA) goto 200

          Variab='WI:openfract'
        CALL GETLDat(Line,k,l,LDat(ptrl),ptrl,0.0000)
          Variab='WI:contr'
        CALL GETLDat(Line,k,l,LDat(ptrl),ptrl,0.0000)
C Check Cd
C  As the correlation Cd=f(Hopening/Hroom) is given, input of Cd=0 is possible
C@empa aw 2000dec14 Valid range for Cd changed according the valid range used for
C@                  Cd calculation in Precal
CC       IF ((Ldat(ptrl-1).GT.1.).OR.(Ldat(ptrl-1).LT.0.0))THEN
        IF ((Ldat(ptrl-1).GT.1.5).OR.(Ldat(ptrl-1).LT.0.0))THEN
          Lstr=LENSTR(UsrAfc(nUsrAfc))
          CALL INERR('&-WI:  *'//UsrAfc(nUsrAfc)(1:Lstr)//
     &    ': Discharge coefficient is not within the range 0...1.5 !',
     &    line,.FALSE.,2)
        ENDIF
C check whether all Cds are zero or all are given
          ChkCdSum=ChkCdSum+Ldat(ptrl-1)
          ChkCdProd=ChkCdProd*Ldat(ptrl-1)

          Variab='WI:widthFact'
        CALL GETLDat(Line,k,l,LDat(ptrl),ptrl,1.0000)
        IF (Ldat(ptrl-1).GT.1.0.OR.Ldat(ptrl-1).LT.0.0) THEN
               Lstr=LENSTR(UsrAfc(nUsrAfc))
               CALL INERR('&-WI:  *'//UsrAfc(nUsrAfc)(1:Lstr)//
     &         ': Width Factor is negative or greater than 1 !',
     &         line,.FALSE.,2)
        ENDIF
          Variab='WI:heightFact'
        CALL GETLDat(Line,k,l,LDat(ptrl),ptrl,1.0000)
        IF (Ldat(ptrl-1).GT.1.0.OR.Ldat(ptrl-1).LT.0.0) THEN
               Lstr=LENSTR(UsrAfc(nUsrAfc))
               CALL INERR('&-WI:  *'//UsrAfc(nUsrAfc)(1:Lstr)//
     &         ': Height Factor is negative or greater than 1 !',
     &         line,.FALSE.,2)
        ENDIF
          Variab='WI:start Height Factor'
        CALL GETLDat(Line,k,l,LDat(ptrl),ptrl,0.0000)
        IF (Ldat(ptrl-1).GT.1.0.OR.Ldat(ptrl-1).LT.0.0) THEN
               Lstr=LENSTR(UsrAfc(nUsrAfc))
               CALL INERR('&-WI:  *'//UsrAfc(nUsrAfc)(1:Lstr)//
     &         ': Start Height Factor is negative or greater than 1 !',
     &         line,.FALSE.,2)
        ENDIF
        HFactSum=Ldat(ptrl-1)+Ldat(ptrl-2)
        IF (HFactSum.GT.1.0.OR.HFactSum.LT.0.0) THEN
               Lstr=LENSTR(UsrAfc(nUsrAfc))
               CALL INERR('&-WI:  *'//UsrAfc(nUsrAfc)(1:Lstr)//
     &         ': Sum of Start Height- and Height Factor is '//
     &         '< 0 or > 1 !',line,
     &         .FALSE.,2)
        ENDIF

          Variab='WI:wscoop'
        CALL GETLDat(Line,k,l,LDat(ptrl),ptrl,0.0000)
          Variab='WI:framedir'
        CALL GETLDat(Line,k,l,LDat(ptrl),ptrl,0.0000)
          Variab='WI:n-openings'
        CALL GETLDat(Line,k,l,LDat(ptrl),ptrl,1.0000)
          Variab='WI:height-openings'
        CALL GETLDat(Line,k,l,LDat(ptrl),ptrl,0.0000)

        Nopen=LDat(ptrL0)
        Nopen=Nopen+1
        Ldat(ptrL0)=Nopen

C If the last read hit eof go to 88
        if (FlgEnd) goto 88
C We already have the next line
        Line = Nline
        k=k1
        l=lenstr(line)
C get next line of data
        goto 100

C At this point the filter data is in Line and the next line is in nline
200	continue

        IF (Ldat(ptrl0+7).NE.0.0) THEN
            Lstr=LENSTR(UsrAfc(nUsrAfc))
C@NBI PGS 2000Jul16 - Made more understandable
            CALL INERR('&-WI:  *'//UsrAfc(nUsrAfc)(1:Lstr)//
CC   &		':  First line at header 2 does not start with 0.0 ! ',
     &		':  First line of opening-factor data must start with 0.0',
     &		' ',.FALSE.,2)
        ENDIF

C For type 2 opening (horizontal pivoting axis) the first entry of a line is
C the open factor for which: opening angle = 90 degree --> opening factor = 1.0
         IF (Ldat(ptrl-9).NE.1.0)THEN
            Lstr=LENSTR(UsrAfc(nUsrAfc))
            CALL INERR('&-WI:  *'//UsrAfc(nUsrAfc)(1:Lstr)//
     &	        ':  Last line in the table must be for open factor 1.0',
     &		'or line with filter data is missing !',
     &		.FALSE.,2)
         ENDIF
C Check whether all Cds are zero or all are given
        IF (ChkCdProd.EQ.0.0) THEN
          IF (ChkCdSum.EQ.0.) THEN

C@empa aw 2003jun03 There is now a Cd-Calculation also for horizontaly pivoted windows
CC            IF (LDat(ptrl0+3).NE.1.) THEN
CCC error message: not a rectangular LVO
CC	    Lstr=LENSTR(UsrAfc(nUsrAfc))
CC	    CALL INERR('&-WI:  *'//UsrAfc(nUsrAfc)(1:Lstr)//
CC     &	        ' is not a rectangular opening!',
CC     &		' Cd has to be input here!',
CC     &		.FALSE.,2)
CC            ENDIF
        ELSE
            Lstr=LENSTR(UsrAfc(nUsrAfc))
            CALL INERR('&-WI:  *'//UsrAfc(nUsrAfc)(1:Lstr)//
     &	        ': Cd is zero just in some lines! ',
     &		'It has to be zero in every line (Cd will be '//
     &          'calculated) or in no line.',
     &		.FALSE.,2)
         ENDIF
        ENDIF
        CALL FILTER(Line,K,L)
        goto 88

88	continue
        k=k1
        line=Nline

99	continue
        RETURN
        END

C@tno jcp 1996Apr06_10:36:32 new routine copied from the part in routine inh
Ch***********************************************************************
      SUBROUTINE IniData
C***********************************************************************
C iniData initializes data that may not be provided by the input file
C *.CIF
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

      INTEGER i,j


C------------------------------------------------------- initialisations
      PtrL=1
C     ! pointer for Walls with absorption
      PtrW=1
C     ! pointer for Wall materials and type names
      PtrWN=0
      LayPtr1 =10
C@empa aw 1994oct25 The first 9 elements of laydat is for a prototype
C   zone (Zone=0) without any layers (this is needed when a layered
C   zone is linked to an unlayered zone with a large opening).
      DO 10 I=1,9
         LayDat(i)=0
10    CONTINUE
      LayPtr(1,0)=1
      LayPtr(2,0)=9
      nUsrAfc =0
      NCpDir  =1
      Nspec   =0
      Nwind   =0
      pSource =1
      NConc   =0
      FInpErr =0
      Nz      =0
      Nzl     =0
      Nzp     =0
      Nzt     =0
      Nl      =0
C@empa aw 2001may28 NTInp
CC      NTInp   =0

      proname=' '
      vername=' '

      nconcpoldes=0
C     ! Flag that will be 1 if data under &-WAL-LINks is found (used
C     ! for testing NConcWaTy against NConc).
      NetWaLinF=0

C     ! Initialize "last 10 lines" counter
      Plines10 = 0
C     ! And set flag so readlin keeps track of the last 10
      Keep10 = .true.

C     WRITE(CRT,*) afcname

      DO 1 I=1,200
        pnamrec(i)=0
1     continue

      DO 2 I=1,MaxL
        RefLink(I)='     '
2     CONTINUE

C     ! pname =pointer to the next free position in name
C     ! nName =the number of the name
      pName = 1
      nName = 0

      pDAF=1

C     ! Set parameters to a value that might not get a default if the
C     ! whole line is not used.

C@tno jcp 1996Mar29_17:45:39 added initialization for PR-SIMU HistStart
C     ! PR-SIMU
C     ! reset de start position for used histograms to HistoAr(1)
      HistStart=1

C     ! PR-CONT
      epsFA = DepsFA
      epsFR = DepsFR
      epsCJ = DepsCJ
      nNewt = DnNewt
      difLim = DdifLim
CC    useOpz = DuseOpz
CC    noInit = DnoInit
      stp1Init = Dstp1Init
      stp2Init = Dstp2Init
      slvSel = DslvSel
      mIter = DmIter
      rhoSync = DrhoSync

C     ! CP-BUILding
      Zref = DZref

C     ! ENV-BUIlding
      Zentr = DZentr
      AngBui = DAngBui
      Lat = DLat
      Lon = DLon

C     ! ENV-WINd windspeed ref height, altitude meteo station and
C     ! Alpha Meteo correction for wind speed
      ZVmet = DZVmet
      Zmet = DZmet
      AlphMet = DAlphMet

C     ! ENV-WINd Direction, WindExponent
      WExp(1) = DWExp
C     ! Will fill in EnvDir(1) and (MEnvDir+2) after all input is done
      EnvDir(2) = 0.0
      MEnvDir = 1
C@tno jcp 1996Jul24_14:08:41 added Denvdir (useful?)
      DEnvDir = 360

C     ! SCH-MET header 2 Time, wind speed, wind direction, temp, Xh, Pbarom
      MetName = DMetName
      MetTime = DMetTime
      Vmet = DVmet
      Dmet = DDmet
      Tmet = DTmet
      XhMet = DXhMet
C     ! Pbmet is finally in Pa, DPbMet in kPa
      PbMet = DPbMet * 1000

C     ! NORM-CR Normalized crack temperature, pressure and humidity ratio
      NormCrt = DNormCRT
C     ! DNormCrPb is in kPa
      NormCrPb = DNormCrPb *1000
      NormCrXh = DNormCrXh

C@lbl bvs 1997Jul24 Initialize pollutant name array
      DO 15 I=1,MAXC
         Cname(i) = ' '
15    CONTINUE

C     ! OCCUPANTS (updated in inSimu (H) and OCCupant definition ,
C     ! checked later)
      MaxOccNrH=0
      MaxOccNrD=0
      DO 3 I=1,MaxO
         OccAge (I)=0
         OccL   (I)=0
         OccM   (I)=0
C        ! OOccAct O(riginal)Occupant Activity
         OOccAct(I)=0
         OccSmo (I)=0
C@lbl bvs 1997Jul24 occupant/pollutant names must be initialized
         DO 22 j=1,MaxC
           OccPoln(i,j)=' '
22       continue
         OccName(i)=' '
         Do 8 j=1,MaxZ
           LoccAct(i,j)=0.0
           LoccNum(i,j)=0
8        continue
3     continue

C     ! PolDes. Reset MM (molar mass) for every possible pollutant to
C     ! 0. This allows check on double defined pollutants in inPolDes
      DO 4 I=1,MaxC
         MM (I)=0
4     continue

C     ! Set histogram: number of classes=0, this also indicates unused
C     ! histograms
      Do 5 i=1,MHisTy
         HistoX(i,1)=0
5     continue

C     ! reset the whole Histogram storage space (5000 elements)
      Do 6 i=1,MHisAr
         HistoAr(i)=0
6     continue

C     ! set HistUsed to 0
      Do 7 i=1,MHisUs
         HistUsed(i,1)=0
         HistUsed(i,2)=0
         HistUsed(i,3)=0
7     continue

C@tno jcp 1996Apr22_16:33:41 tell that no default has been used in one of the
C   routines GetLDAT GetW?
C@tno jcp 1996Jun12_14:30:51 FlgDefault is now an integer
CC    FlgDefault=.FALSE.
C@NBI PGS 2000Aug18 - outputoptions(7) bugfixed. The New convention is:
C@NBI                 FlgDefault = 1 for Normal data entry (no default)
      FlgDefault=1

C@tno jcp 1996Apr22_16:56:05 reset flag for Occupants used as source (name of
C pollutant matches)
      OccAsSource=.False.

C@tno jcp 1996Apr25_14:34:48 set Histograms not active to start with
      HistActive=.FALSE.
C@tno jcp 1996Apr25_14:34:48 set Histograms are not using Concentrations
      HistConc=.FALSE.

C@tno jcp 1996Apr26_23:17:42 MetTimeStart/Stop reset
      MetTimeStart=' '
      MetTimeStop =' '

C@tno jcp 1996Apr29_10:52:10 clear OnScreen string
      OnScreen=' '

C@tno jcp 1996May29_09:44:27 flag that no flowmatrices have been written yet for
C@lbl bvs 1997Jul24 this is not defined nor is it ever used
CC    FirstFlowMat=.TRUE.

C@tno jcp 1996Jun14_16:13:53 reset EqnCount for EqnWin
      EqnCount=0

C@tno jcp 1996Jun26_21:23:37 Use months Name as default in time strings
      UseMonthName=1

C@tno jcp 1996Jun27_17:37:41 define the default width of the output
C@NBI PGS 2000-10-14 - Should be 79, not 80, as We get an unwanted space
C@NBI                  at the start of line, the word with the 80th
C@NBI                  non-space character spills over.
      WCRT=80
C@tno jcp 1996Jul02_14:28:29 Empty meteop is 1 if no data has(yet) been found
C in the meteo file. As there is read over empty lines in the meteo file and
C if at EOF the file is reopened as the file may be cyclic, we donot want to
C keep looping in a meteo file that contains only blank lines.
      EmptyMeteo=1

C@tno jcp 1996Jul03_11:24:00 lastBjDay is used in FillBuf to see if a meteo
Cfile has cyclic Month+day data, 9 is not Julian day -1(repeat day),0..8(
Crepeat week..Weekends) and not 1721424..788 (repeat year)
      LastBjDay=9


C@tno jcp 1996Jul04_12:07:57 set the values used in the JustBefore and
C WrtBefore routines. That keep track of values in schedules just before the
C start of the simulation
      PrvSchTyp=0
      PrvSchNa=0
      PrvSec=-IntMax
      prvLine=' '

C@tno jcp 1996Jul05_07:42:16 CyclicMeteo=1 if meteo data like month+day (no year
      CyclicMeteo=0

C@tno jcp 1996Jul11_11:37:45 set need for matrix inversion to 0 (set at readopt)
      InvMatrix=0

C ***** FINISHED initializing with Default Data values *****

C@tno jcp 1996Apr06_10:25:56 there are default initializations in routine
C IniDefault, called from Main
      RETURN
      END
C end inidata
