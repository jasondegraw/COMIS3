C+*********************************************************** comv-ou3.f
Ch**********************************************************************
        SUBROUTINE VentOut(file)
C
C pass parameter # =  1   2  3  4  5  6
C
C***********************************************************************
C
C
C
C Purpose: writes the ventilation data formated
C
C sept 1989 hcp
C Module : This module is a temporay one and has no number
C Changes: april 25 , 1989
C          VD 20.2.90
C          VD 24.Oct 90 : Format 2001
C          JCP 1990dec27 FTDIS and FMBDIS inserted for the call of
C          RELDIS
C@lbl bvs 1994May26 Dpint not used
C Changed CRT into COF if-statement test.or.OEcho
C F removed from write statement
C made more elements in the RELDIS arrays
C count of % of links dp<diflim
C added linInit
C added DOUBLE Fma, and REAL Dp1
C Dp is now DOUBLE, Dp1 used
C@lbl bs 1991jul02   number of significant digits in RELDIS calls changed
C@lbl bs 1991jul03   update FV vector
C@empa aw 1991july03 Out of range error with string manipulation fixed by
C@empa         making all elements of RELDL bigger than 0.
C@empa aw 1991July11 Dp=DpL
C@empa aw 1991july30 Addition of DpJ to Dp
C@lbl bs 1991aug06  parameter 'time' added
C@lbl akk 6nov1991 parameter file added
C@lbl akk 12nov91 parameters time and interval removed
C@empa aw 1992oct06 Skip first column, otherwise the first character will be
C                   supressed when you print the output file
C@empa aw 1992okt06 local variables DFma, Fma1, Fma2, Start1 added
C@empa aw 1992oct06 call to feqn instead to feqn2, then we get Tlink and FV2
C@empa aw 1992okt06 Take tout and RhoOut, when node is spec. pressur or wind
C@empa aw 1993jan25 new locals: DFma1,DFma2
C@empa aw 1993jan25 dfma1, dfma2 as parameter in the call of feqn
C@empa aw 1993jul01 new locals: Dpint,DDpL,SI,LiStaFl,LiStaFlag
C@empa aw 1993jul01 Interpolation between DpL(1,I) and DpL(2,I)
C@empa aw 1993jul01 report interpolated link stack pressure
C@empa aw 1993sep13 made the interpolation range symmetric for from to direction
C@empa aw 1994oct12 Pressure difference and density profile for large vertical
C@                  openings added
C@empa aw 1994nov04 Check Diflim just for Cracks
C many changes to only multiply with ofact(..)
C@NBI PGS 1999Jun30 - Tidied up source code to make it more legible
C@NBI                 (No syntax change. Just converted all tabs to spaces,
C@NBI                 8-spaces in left column, thereafter 2 spaces per
C@NBI                 indentation level)
C@lbl dml 1999nov19 Change integer variable initFlg to logical linInit.
C@NBI PGS 2000Jul21 - Straightened up columns & deleted obsolete lines 
C@NBI                 and variables.  Output uses function RightAlign
C@NBI               - RhoLink no longer used (it was only an estimate anyway)
C@NBI                 but if you want output that uses RhoOut then put it
C@NBI                 in routine VENT2OUT instead, where RhoOut remains
C@NBI PGS 2003Mar16 - Harmonized/unified OEcho levels (explained in COMV-DAT.FOR)
C@NBI                 This routine is now called only if OEcho >= 2
C
C Limits :
C
C Pass parameters:
C
C IO # Name    unit    description
C
C example:
C call
Ch**********************************************************************
        IMPLICIT NONE
        INCLUDE 'comv-inp.inc'
        INCLUDE 'comv-uni.inc'

C       passed subroutine arguments
        INTEGER file

C       local parameters
        REAL PZDIS
C@NBI PGS 1999May10 - Added str1 & str2
CC      CHARACTER*20 OunitP
        CHARACTER*20 OunitP,str1,str2
        REAL ProcL
        INTEGER To,From,k
        INTEGER START
        INTEGER START1
        INTEGER RelDL(8),Nlam,L,I,J
        INTEGER LiStaFl,LiStaFlag
        CHARACTER*30 RelD(8)
        CHARACTER nodetyp(9)*4,RightAlign*15
        DOUBLE PRECISION Fma,Dp
        REAL DFma,fma1,fma2
        REAL DDpL,SI,Dpm
        INTEGER ProfStart
C@empa aw 1997sep18 DpProfnew changed to DOUBLE
        DOUBLE PRECISION DpProfNew(NrInt+2)
        DOUBLE PRECISION DFma1,DFma2
        INTEGER ICSTRSTR
C-----

C@lbl bvs 1997Jul28 linInit must be declared *AND* initialized
C@empa aw 1997sep08 OK, but we have to initialize it with 0. Here we
C@empa    call FEqn just once and that must not be with linInit=1.
CC    integer linInit
CC    linInit = 1
CC    linInit = 0
      logical linInit
      linInit = .false.

        nodetyp(1)='znzn'
        nodetyp(2)='exzn'
        nodetyp(3)='spzn'
        nodetyp(4)='znex'
        nodetyp(5)='exex'
        nodetyp(6)='spex'
        nodetyp(7)='znsp'
        nodetyp(8)='exsp'
        nodetyp(9)='spsp'


C-------------
C       Header
C-------------

        IF (Test.GE.1 .or. OEcho.GE.3) THEN
C         new header of ventilation output created
C@NBI PGS 1999May06 - Neater standardized output
CC        WRITE(file,*) ' '
          WRITE(file,*) Niter,' iterations with solver no.',SlvSel
CC        WRITE(file,*) ' '
CC        WRITE(file,*)
CC     &'=============================================================='//
CC     &'================='
          WRITE(file,*) ' '
          WRITE(file,*) ' '
          WRITE(file,*) '*******************************'
          WRITE(file,*) 'Ventilation output            *'
          WRITE(file,*) '*******************************'
C@NBI PGS 1999May06   (end)
C@empa end


C--------------------
C         Zone output
C--------------------


          WRITE(file,'(//a)')'+--ZONES---------------  -----Flow-'//
     &    'rates-----'
          WRITE(file,'(a)')'| Zone-ID      Pressure      Total  '//
     &    'Imbalance'
          WRITE(file,'(a1,1X,10X,3A11)')'|'
     &    ,RightAlign(oUnit(UnitP)   ,11)
     &    ,(RightAlign(oUnit(UnitFma),11),j=1,2)
          WRITE(file,'(a)')'+----------------------  -----------'//
     &    '---------'


          DO 800 I=1,Nz
            IF (ofact(UnitP).LT.0) THEN
C             subtract the outside pressure at the same height as the zone
C             ref level.
              PzDIS=pz(i)-(-G*zz(i)*RhoOut)
            ELSE
              PzDIS=Pz(i)
            END IF
C           convert to user units (ABS to remove the - of OSR)
            CALL RELDIS(PzDIS*ABS(ofact(UnitP))
     &                                        ,3,RelD(1),RelDL(1),0)
            CALL RELDIS(FT(i)*ofact(UnitFma)  ,4,RelD(2),RelDL(2),0)
            CALL RELDIS(REAL(FMB(i))*ofact(UnitFma) 
     &                                        ,4,RelD(3),RelDL(3),0)
            WRITE(file,'(2X,A10,3A11)')
     &         ZoNa(i),(RelD(j)(1:RelDL(j)),j=1,3)

800       CONTINUE
        ENDIF


C------------------
C       Link output
C------------------

        IF (Test.GE.1 .or. OEcho.GE.4) THEN

C         remove -OSR used to flag Oustide Stack Pressure Reference, if any
          oUnitP=oUnit(UnitP)(1:20)
          k=1
          I = iCStrStr(oUnitP,'-OSR',k)
          if (I.GT.0) oUnitP(I:I+3)='    '

          WRITE(file,'(/A)') '+--LINKS-----------------  --From---'//
     &    '  ---To----  T-link   Dp-link    From->To   To->From'
          WRITE(file,'(A,A6,A10,1X,2A11)')'| # Link-ID    Type   '//
     &    '     Type:Name  Type:Name  '
     &    ,RightAlign(oUnit(UnitTmul),6),RightAlign(oUnitP,10)
     &    ,(RightAlign(oUnit(UnitFma),11),j=1,2)
          WRITE(file,'(A)')  '+------------------------  ---------'//
     &    '  ---------  ------  --------  ---------------------'

          Nlam=0
C         initialize LiStaFlag
          LiStaFlag=0
          DO 805 I=1,Nl
            LiStaFl=0
            FROM=FromTo(1,I)
            TO=FromTo(2,I)

C           pointer to the first coefficient of this link in Ldat
            start=pLiLDat(I)
C           Start1 points at the first data element of this link in LinkDat(*)
            Start1=pLinkDat(I)

            L=Lstat(i)
            IF (L .EQ. 0) THEN
C             zone to zone
              Dp=Pz(FROM)-Pz(TO)+DpL(1,I)+DpJ(I)
            ELSE IF (L.EQ.3 .or. L.EQ.6) THEN
C             zone to ex or zone to special
              Dp=Pz(FROM)+DpL(1,I)+DpJ(I)
            ELSE IF (L.EQ.1 .or. L.EQ.2) THEN
C             ex to zone or special to zone
              Dp=-Pz(TO)+DpL(1,I)+DpJ(I)
            ELSE
C             ex to ex, special to ex, ex to special or special to special
C             (any case where the pressure wasn't calculated in the solver)
              Dp=DpL(1,I)
              IF (Dp.LT.0.0) THEN
                Dp=DpL(2,I)
              ENDIF
C             Set pointer to pressure difference profile for large openings
C             and Update DpProfNew
              IF (LDat(Start).EQ.9.) THEN
                ProfStart=ProfPtr(i)
C@tno jcp 1996Jun12_12:44:18 is this calculation of dpProfNew OK?
                DO 10 j=1,(NrInt+2)
                  DpProfNew(j)=dpprof(ProfStart-1+j)
10              CONTINUE
              ELSE
                ProfStart=1
              ENDIF
              CALL FEQN(Fma,DFma,dfma1,dfma2,Dp,LDat(Start),
     &          Mf(I),Mp(I),RhoL(1,I),RhoL(2,i),
     &          sqrRhoL(1,I),sqrRhoL(2,I),
     &          Mul(1,I),Mul(2,I),linInit,fma1,fma2,
     &          DifLim,ofact(UnitFma),
     &          RSqrtDff(I),LinkDat(Start1),
     &          Tl(I),Lstat(I),TempL(1,I),TempL(2,I), NormCrRho,
     &          NormCrMu,DpProfNew,RhoProfF(ProfStart),
C@tno jcp 1996Jun13_23:18:26 for PS Vmet and RhoOut
     &          RhoProfT(ProfStart),i,FVnew,iRFlist,Vmet,RhoOut)

C             fma is the sum of fma1,fma2. Save these 2 flows in FV2(*)
              FV2(1,i)=fma1
              FV2(2,i)=fma2
              FVnew(I) = Fma
            ENDIF

            DDpL=(DpL(1,I)-DpL(2,I))
            SI=SIGN(1.,DDpL)
            DDpL=ABS(DDpL)
C           make the interpolation range symmetric for from to direction
            Dpm=(DpL(1,I)+DpL(2,I))/2+Dp-DpL(1,I)
            IF (ABS(Dpm).LT.DDpL) THEN
              Dp=Dpm+Dpm*0.5*SI
              LiStaFl=1
              LiStaFlag=1
            ELSE IF (Dpm.LE.(-DDpL)) THEN
              Dp=Dp-DpL(1,I)+DpL(2,I)
            ENDIF

            ltyp=Ldat(start)
            IF (ltyp.eq.1 .or. ltyp.eq.9) THEN
            ELSE
              IF (tl(i).eq.0.0) THEN
                IF (FVnew(I).GT.0.0) THEN
C                 Take tout when node is spec. pressur or wind
                  IF ((L.eq.0).or.((L.eq.3).or.(L.eq.6)))THEN
                    Tl(I)=Tz(from)
                  ELSE
                    Tl(I)=Tout
                  ENDIF
                ELSE
                  IF (L.le.2) THEN
                    Tl(I)=Tz(to)
                  ELSE
                    Tl(I)=Tout
                  ENDIF
                ENDIF
              ENDIF
            ENDIF

            CALL RELDIS(ofact(UnitToff)+ofact(UnitTmul)*tl(i)
     &                                         ,5,RelD(1),RelDL(1),0)
            CALL RELDIS(REAL(Dp)*ABS(ofact(UnitP))
     &                                         ,3,RelD(2),RelDL(2),1)
            IF(FV2(1,i).EQ.0.0)THEN
               RelD(3)='0'
               RelDL(3)=1
            ELSE
               CALL RELDIS(FV2(1,i)*ofact(UnitFma),4,RelD(3),RelDL(3),1)
            ENDIF
            IF(FV2(2,i).EQ.0.0)THEN
               RelD(4)='0'
               RelDL(4)=1
            ELSE
               CALL RELDIS(FV2(2,i)*ofact(UnitFma),4,RelD(4),RelDL(4),1)
            ENDIF

C           report laminarized link with an '*'
            RelDL(2)=RelDL(2)+1
            IF ((abs(DP).LT.DifLim).AND.
     &        (Ldat(pLiLDat(i)).EQ.1.)) then
              RelD(2)(RelDL(2):RelDL(2)+1)='*'
              Nlam=Nlam+1
            ELSE
              RelD(2)(RelDL(2):RelDL(2)+1)=' '
            ENDIF

C           report interpolated link stack pressure
C           by flagging the value with a '#'
            RelDL(2)=RelDL(2)+1
            IF(LiStaFl.EQ.1) THEN
              RelD(2)(RelDL(2):RelDL(2)+1)='#'
            ELSE
              RelD(2)(RelDL(2):RelDL(2)+1)=' '
            ENDIF

            DO J=1,4
              IF (RELDL(J).LT.1) RELDL(J)=1
            ENDDO

            WRITE(file,9001) I,LiNa(I),LiTyNa(i)(1:10)
     &      ,Nodetyp(L+1)(1:2),':',FromToS(1,I)(1:7)
     &      ,NodeTyp(L+1)(3:4),':',FromToS(2,I)(1:7)
     &      ,(Reld(j)(1:reldl(j)),j=1,4)
9001        FORMAT(I3,2(1X,A10),2X,2(A2,A1,A7,1X),A5,A13,A10,A11)
805       CONTINUE

C@NBI PGS 1999Jun30 - Bugfix: Moved further down because there's no point
C@NBI                 reporting "Nlam" or "LiStaFlag" unless they are known
CC        ENDIF
CCC       [ ENDIF (Test.GE.1 .or. OEcho.GE.2) ]

          ProcL=100.0*Nlam/Nl

          IF (Nlam.GT.0) then
            WRITE(file,*) ' '
C@NBI PGS 1999May10 - Neater format
CC          write (file,2003) ProcL,'% (',Nlam,
CC     &    ') of the (',Nl,') links has Laminar flow, Indicated with Dp* .'
            call intdis(Nlam,str1,I)
            call intdis(Nl,str2,J)
            WRITE(file,'(F5.1,A)') ProcL,'% (='//str1(1:I)//') of the '
     &      //str2(1:J)//' links have laminar flow, indicated by Dp*'
C@NBI PGS 1999May10   (end)
          ENDIF

C         report interpolated link stack pressure
C@NBI PGS 1999jun30 - Replaced with clear compact description
          IF (LiStaFlag.NE.0) CALL wrt80(file,'Links indicated by '
     &      //'''Dp #'' means that the link''s stack pressure is an '
     &      //'interpolation between the stack pressures when flow is '
     &      //'positive and when flow is negative.  This interpolation '
     &      //'prevents convergence problems.',wcrt)

C@NBI PGS 1999Jun30 - Bugfix: ENDIF moved here from above
        ENDIF
C       [ ENDIF (Test.GE.1 .or. OEcho.GE.4) ]


C       check output for single sided ventilation
        CALL CheckSSVOutp

        RETURN
        END


C@NBI PGS 1999Aug07 - New function to optionally convert to relative humidity
Ch**********************************************************************

      REAL FUNCTION Output_Xh(T,P,Xh)

C***********************************************************************
C Purpose: Checks if humidity Xh [kg/kg] is to be converted to %rh before
C          output.  If so, then the function returns relative humidity,
C          otherwise it returns Xh unchanged [kg/kg].
C Created: P.G.Schild 1999Aug07
C Changes: 
C Pass parameters:
C IO # Name      unit             description
C O  - Output_Xh [kg/kg] or [-]   Output as humidity ratio or as 
C                                 relative humidity (fraction, 0->1).
C I  1 T         [deg.C]          Air dry bulb temperature.
C I  2 P         [Pa]             Air static pressure in zone, relative
C                                 to the absolute barometric air pressure
C                                 at the building's reference plane (Pbz).
C I  3 Xh        [kg/kg]          Air humidity ratio (moisture content).
Ch**********************************************************************

      IMPLICIT NONE
      INCLUDE 'comv-inp.inc'
      REAL T,P,Xh,RelHumid
      IF(oUnit(UnitXh)(1:3).EQ.'%rh') THEN
        Output_Xh = RelHumid(T,P+Pbz,Xh)
      ELSE
        Output_Xh = Xh
      ENDIF
      END


