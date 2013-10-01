C+*********************************************************** comv-ou4.f
Ch**********************************************************************
        SUBROUTINE Vent2Out(file)

C***********************************************************************
C
C Purpose: writes the ventilation data formated
C          conditions
C          windpressures
C          table of links
C          % laminar
C          table of rooms (+inf +vent)
C          building inf
C
C 1993 nov hcp
C Module : This module is copied from the regular VentOut and changed
C Limits :
C
C Pass parameters:
C
C IO # Name    unit    description
C I    file    -         Output file unit (should be = COF)
C
C Changes:
C@tno jcp 1995Apr27_09:36:46 first copy to oUnitP (line moved up)
C@tno jcp 1996Apr24_18:07:49 building flowrate in string for output OnScreen
C@tno jcp 1996Apr24_18:20:35 Lstr added, now 11 output items in Reld
C@tno jcp 1996Apr27_21:19:21 skip zone output if there are no zones
C@tno jcp 1996May10_10:31:46 LiNa space from 4 to 10 characters
C@tno jcp 1996May10_10:32:53 LinkName space changed multiplier added
C@tno jcp 1996Jun11_21:19:10 different output for Large opening (WI):
C@tno                        fma1 and fma1 followed by velocity
C@tno jcp 1996Jun13_16:43:52 now 11 output items in Reld
C@tno jcp 1996Jun13_23:18:26 for PS Vmet and RhoOut
C@tno jcp 1996Jul24_10:41:15 Vent2Out: rholink is never used!!!
C@lbl bvs 1997Jul28 added iCStrStr declaration
C@lbl bvs 1997Jul28 linInit must be declared *AND* initialized
C@empa aw 1997sep08 OK, but we have to initialize it with 0. Here FEqn is
C@empa              called just once and that must not be with linInit=1
C@empa aw 1997sep18 DpProfnew changed to DOUBLE
C@NBI PGS 1999May06 - Bugfix.  Profstart was only correct for large openings;
C@NBI                 Profstart is now returned from DPLinkI()
C@NBI               - Bugfix. 'Lstat1' replaced with 'L'
C@NBI               - Some tiding-up of all output format
C@NBI PGS 1999May10 - Added str1 & str2
C@NBI PGS 1999Jun30 - Bugfix: Moved further down because there's no point
C@NBI                 reporting "Nlam" or "LiStaFlag" unless they are known
C@NBI               - Tidied up source code to make it more legible
C@NBI                 (No syntax change. Just converted all tabs to spaces,
C@NBI                 8-spaces in left column, thereafter 2 spaces per
C@NBI                 indentation level, then removed all obsolete code
C@NBI                 & comments.  Moved old editing comments from code up
C@NBI                 here to 'Changes' section)
C@NBI PGS 1999Jul01 - Bugfix: "IF OEcho.GE.1" was nested within "IF OEcho.GE.2"
C@NBI               - IF nz>0  is moved further up,
C@NBI                 ENDIF of nz>0  is moved further down
C@NBI               - Bugfix: ENDIF Moved further down since there's no point
C@NBI                 reporting "InfTot" unless it is known
C@NBI PGS 1999Aug05 - Bugfix. FMA must be predefined (at least approx. value,
C@NBI                 as it is needed to solve crack temperature correction)
C@NBI PGS 1999Aug06 - Added 'RightAlign' function
C@NBI PGS 1999Aug06 - Added possibility to output % relative humidity
C@NBI                 done simply by calling function 'Output_Xh'
C@lbl dml 1999nov19 Change integer variable initFlg to logical linInit.
C@NBI PGS 2000Aug02 - "lstr" no longer used so removed from decralations
C@NBI PGS 2003Mar16 - Harmonized/unified OEcho levels (explained in COMV-DAT.FOR)
C@NBI                 This routine is now called only if OEcho >= 2
C@NBI               - "ICindex" no longer used so declatation removed.
C@NBI               - Moved building summary to end of routine.
C@NBI               - Declared new variables InfTotG & VzTotG for zone groups.
C@NBI               - Cleaned up Link output: 2 columns for flowrate (2 directions)
Ch**********************************************************************
      IMPLICIT NONE
      INCLUDE 'comv-inp.inc'
      INCLUDE 'comv-uni.inc'

C    .Passed subroutine arguments
      INTEGER file

C     .Local parameters
      INTEGER To,From,k,START,START1,RelDL(11),Nlam,L,I,J,ToYes
CC   &,LiStaFl,LiStaFlag,LENSTR,IcIndex,iCStrStr,ProfStart
     &,LiStaFl,LiStaFlag,LENSTR,iCStrStr,ProfStart
      REAL FvVeloc,ProcL,RhoLink,PzDis,DFma,fma1,fma2,Dpint,DDpL,SI
     &,Output_Xh,InfTotG,VzTotG
      DOUBLE PRECISION Fma,Dp,DpProfNew(NrInt+2),DFma1,DFma2
      CHARACTER RelD(11)*30,nodetyp(9)*4,OunitP*20
     &,LookStr*20,str1*20,str2*20,RightAlign*15
C     InfZ(1..MaxZ) infiltration flow per zone (flow from ext and spec)
C     VenZ(1..MaxZ) ventilation  flow per zone (flow from other zones)
C     InfTot        sum of InfZ(*)
      REAL InfZ(MaxZ),VenZ(MaxZ),InfTot,VzTot,VenTot,HadZ(MaxZ)
      logical linInit

C    .Common block declarations
C     Added for output use in Vent2Out from CRACK and CRtemp (they have this
C     common block too)
      common /vent2/ FvVeloc


C-----------
C     Header
C-----------

CC    WRITE(crt,*)
      WRITE(crt,*)
      WRITE(crt,*) '*******************************'
      WRITE(crt,*) 'Ventilation output (detailed) *'
      WRITE(crt,*) '*******************************'
      WRITE(crt,*)
C@NBI PGS 2003Mar16 - Niter output moved here from VENTOUT
      IF(SEcho.GE.1)
     &WRITE(crt,*) Niter,' iterations with solver no.',SlvSel

C     see the input file &-NET-EXT for the coupling with cp's
      nodetyp(1)='znzn'
      nodetyp(2)='exzn'
      nodetyp(3)='spzn'
      nodetyp(4)='znex'
      nodetyp(5)='exex'
      nodetyp(6)='spex'
      nodetyp(7)='znsp'
      nodetyp(8)='exsp'
      nodetyp(9)='spsp'

      if (nl.gt.0) then


C-----------------------
C       Key weather data
C-----------------------

        CALL RelDis(Vmet*ofact(UnitW)  ,4,RelD(1),RelDL(1),0)
        CALL RelDis(VeRef*ofact(UnitW) ,4,RelD(2),RelDL(2),0)
        CALL RelDis(Dmet*1.0           ,4,RelD(3),RelDL(3),0)
        CALL RelDis(ofact(UnitToff)+Tmet*ofact(UnitTmul)
     &                                 ,4,RelD(4),RelDL(4),0)
        CALL RelDis(Output_Xh(Tmet,Pbmet-Pbz,Xhmet)*ofact(UnitXh)
     &                                 ,4,RelD(5),RelDL(5),0)

C       write Vmet,Dmet,Tmet,  Niter,SlvSel
        WRITE(FILE,*)'METEO:  Vmeteo='//RelD(1)(1:RelDL(1))//
     &  oUnit(UnitW)(1:LenStr(oUnit(UnitW)))//
     &  '  (Vroofheight='//RelD(2)(1:RelDL(2))//
     &  oUnit(UnitW)(1:LenStr(oUnit(UnitW)))//')'//
     &  '  Direction='//RelD(3)(1:RelDL(3))//'deg'//
     &  '  Temp='//RelD(4)(1:RelDL(4))//
     &  oUnit(UnitTmul)(1:LenStr(oUnit(UnitTmul)))//
     &  '  Xh='//RelD(5)(1:RelDL(5))//
     &  oUnit(UnitXh)(1:LenStr(oUnit(UnitXh)))

        if ((Vmet.GT.0.0).AND.OEcho.GE.4) then
          write(file,'(1X,A)') 'Cp.nr      Cp      Pa windpressure'
          DO I=1,Nwind
            WRITE(file,'(1X,I3,2(F12.3))') i,Cp1(i),Pwind(i)
          ENDDO
        endif

C@NBI PGS 2003Mar16 - Moved this block to MAIN, so not written every timestep
CC        if (ICindex(oUnit(UnitFma),'m3').EQ.0) WRITE(file,*)
CC     &    'Volume flowrates [m3/s] = Mass flowrates [kg/s] / 1.2'
CCC         ..m3.. is found in oUnit(UnitFma)


C------------------
C       Link output
C------------------

        IF (Test.GE.1 .or. OEcho.GE.4) THEN

C         remove -OSR used to flag Oustide Stack Pressure Reference, if any
          oUnitP=oUnit(UnitP)(1:20)
          k=1
          I = iCStrStr(oUnitP,'-OSR',k)
          if (I.GT.0) oUnitP(I:I+3)='    '

          WRITE(file,'(/A)') '+--LINKS---------------------------'//
     &    '  --From---  ---To----'//
     &    '  T-link  Velocity   Dp-link    From->To   To->From'
          WRITE(file,'(A,A6,2A10,1X,2A11)')'| # Link-ID    Type'//
     &    '      Multiplier  Type:Name  Type:Name  '
     &    ,RightAlign(oUnit(UnitTmul),6)
     &    ,RightAlign(oUnit(UnitW),10),RightAlign(oUnitP,10)
     &    ,(RightAlign(oUnit(UnitFma),11),j=1,2)
          WRITE(file,'(A)')  '+----------------------------------'//
     &    '  ---------  ---------'//
     &    '  ------ --------- ---------   --------------------'

          Nlam=0
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
            ltyp=Ldat(start)

            call DPLinkI(DP,DpProfNew,I,Ltyp,From,To,
     &      Pz,DpL,L,DpJ(i),DpProf,Profptr(i),Profstart)

C           call feqn to calculate the FvVeloc
            FvVeloc = 0
CC          InitFlg = 0
            linInit = .false.
            Fma=FVnew(I)
            CALL FEQN(Fma,DFma,dfma1,dfma2,Dp,LDat(Start),
     &      Mf(I),Mp(I),RhoL(1,I),RhoL(2,i),
     &      sqrRhoL(1,I),sqrRhoL(2,I),
     &      Mul(1,I),Mul(2,I),linInit,fma1,fma2,
     &      DifLim,ofact(UnitFma),
     &      RSqrtDff(I),LinkDat(Start1),
     &      Tl(I),Lstat(I),TempL(1,I),TempL(2,I), NormCrRho,
     &      NormCrMu,DpProfNew,RhoProfF(ProfStart),
     &      RhoProfT(ProfStart),i,FVnew,iRFlist,Vmet,RhoOut)
            FV2(1,i)=fma1
            FV2(2,i)=fma2
            FVnew(I) = Fma

            DDpL=(DpL(1,I)-DpL(2,I))
            SI=SIGN(1.,DDpL)
            DDpL=ABS(DDpL)
            IF (ABS(Dp).LT.DDpL) THEN
              Dpint=(DDpL+Dp)*0.5*SI
              Dp=Dp-DpL(1,I)+DpL(2,I)+Dpint
              LiStaFl=1
              LiStaFlag=1
            ELSEIF (Dp.LE.(-DDpL)) THEN
              Dp=Dp-DpL(1,I)+DpL(2,I)
            ENDIF

            IF (ltyp.eq.1 .or. ltyp.eq.9) THEN
            ELSE
              IF (tl(i).eq.0.0) THEN
                IF (FVnew(I).GT.0.0) THEN
C                 Take tout and RhoOut, when node is spec. pressure or wind
                  IF ((L.eq.0).or.((L.eq.3).or.(L.eq.6)))THEN
                    Tl(I)=Tz(from)
                    RhoLink=RhoL(1,i)
                  ELSE
                    Tl(I)=Tout
                    RhoLink=RhoOut
                  ENDIF
                ELSE
                  IF (L.le.2) THEN
                    Tl(I)=Tz(to)
                    RhoLink=RhoL(2,i)
                  ELSE
                    Tl(I)=Tout
                    RhoLink=RhoOut
                  ENDIF
                ENDIF
              ENDIF
            ENDIF

            CALL RELDIS(Mf(i)                  ,5,RelD(1),RelDL(1),1)
            CALL RELDIS(ofact(UnitToff)+ofact(UnitTmul)*tl(i)
     &                                         ,5,RelD(2),RelDL(2),0)
            CALL RELDIS(FvVeloc*ofact(UnitW)   ,3,RelD(3),RelDL(3),0)
            CALL RELDIS(REAL(Dp)*ABS(ofact(UnitP))
     &                                         ,3,RelD(4),RelDL(4),1)

            IF(FV2(1,i).EQ.0.0)THEN
               RelD(5)='0'
               RelDL(5)=1
            ELSE
               CALL RELDIS(FV2(1,i)*ofact(UnitFma),4,RelD(5),RelDL(5),1)
            ENDIF
            IF(FV2(2,i).EQ.0.0)THEN
               RelD(6)='0'
               RelDL(6)=1
            ELSE
               CALL RELDIS(FV2(2,i)*ofact(UnitFma),4,RelD(6),RelDL(6),1)
            ENDIF

C           report laminarized link with an '*'
            RelDL(4)=RelDL(4)+1
C@NBI PGS 1999Aug16 - Maybe the following line shouldn't be commented out, as
C@NBI                 not all links are treated as laminar in routine CRACKS
C@NBI PGS 2000Jul21 - ... now it's decommented
            IF ((abs(DP).LT.DifLim).AND.
     &        (Ldat(pLiLDat(i)).EQ.1.)) then
              Nlam=Nlam+1
              RelD(4)(RelDL(4):RelDL(4)+1)='*'
            ELSE
              RelD(4)(RelDL(4):RelDL(4)+1)=' '
            ENDIF

C           report interpolated link stack pressure
C           by flagging the value with a '#'
            RelDL(4)=RelDL(4)+1
            IF(LiStaFl.EQ.1) THEN
              RelD(4)(RelDL(4):RelDL(4)+1)='#'
            ELSE
              RelD(4)(RelDL(4):RelDL(4)+1)=' '
            ENDIF

            DO J=1,7
              IF (RELDL(J).LT.1) RELDL(J)=1
            ENDDO

            WRITE(file,9001) I,LiNa(I),LiTyNa(i)(1:10)
     &      ,Reld(1)(1:reldl(1))
     &      ,Nodetyp(L+1)(1:2),':',FromToS(1,I)(1:7)
     &      ,NodeTyp(L+1)(3:4),':',FromToS(2,I)(1:7)
     &      ,(Reld(j)(1:reldl(j)),j=2,6)
9001        FORMAT(I3,2(1X,A10),A10,2X,2(A2,A1,A7,1X)
     &      ,A6,A10,A12,A10,A11)

805       CONTINUE

          ProcL=100.0*Nlam/Nl
          IF (Nlam.GT.0) then
            WRITE(file,*)
            call intdis(Nlam,str1,I)
            call intdis(Nl,str2,J)
            WRITE(file,'(F5.1,A)')
     &        ProcL,'% (='//str1(1:I)//') of the '//str2(1:J)//
     &        ' links have laminar flow, indicated by Dp*'
          ENDIF

C         report interpolated link stack pressure
          IF (LiStaFlag.NE.0) CALL wrt80(file,'Links indicated by '
     &    //'''Dp #'' means that the link''s stack pressure is an '
     &    //'interpolation between the stack pressures when flow is '
     &    //'positive and when flow is negative.  This interpolation '
     &    //'prevents convergence problems.',wcrt)

        ENDIF
C       End IF of (Test.GE.1 .or. OEcho.GE.4)


C------------------
C       Zone output
C------------------

C       Now FV2(*) is known, so the infiltration can be quickly calculated for zones:

        if (nz.gt.0) then

          IF(OEcho.GE.3)THEN
            WRITE(file,'(/a)')'+--ZONES---------------  ----------'//
     &      '------Flow rates----------------  --------------------'
            WRITE(file,'(a)')'| Zone-ID      Pressure    Infiltr. '//
     &      '   Ventil.     Total  Imbalance       Temp.  Humidity'
            WRITE(file,'(a1,1X,10X,7A11)')'|'
     &      ,RightAlign(oUnit(UnitP)   ,11)
     &      ,(RightAlign(oUnit(UnitFma),11),j=1,4)
     &      ,RightAlign(oUnit(UnitTmul),11)
     &      ,RightAlign(oUnit(UnitXh)  ,11)
            WRITE(file,'(a)')'+----------------------  -----------'//
     &      '-------------------------------  --------------------'
          ENDIF

          DO I=1,Nz
            InfZ(I)=0.0
            VenZ(I)=0.0
          ENDDO

C         Lstat is the link status:
C         Lstat From   To
C          0    zone  zone
C          1     ext  zone
C          2    spec  zone
C          3    zone   ext
C          4     ext   ext
C          5    spec   ext
C          6    zone  spec
C          7     ext  spec
C          8    spec  spec

          DO I=1,NL
            FROM=FromTo(1,I)
            TO=FromTo(2,I)
            L=Lstat(i)
            IF (L .EQ. 0) THEN
C             zone-zone
              VenZ(FROM)=VenZ(FROM)+FV2(2,I)
              VenZ(TO  )=VenZ(TO  )+FV2(1,I)
            ELSE IF (L.EQ.3 .or. L.EQ.6) THEN
C             zone-ext or zone-spec
              InfZ(FROM)=InfZ(FROM)+FV2(2,I)
            ELSE IF (L.EQ.1 .or. L.EQ.2) THEN
C             ext-zone or spec-zone
              InfZ(TO  )=InfZ(TO   )+FV2(1,I)
            ENDIF
          ENDDO

          InfTot=0
          VzTot=0
          DO I=1,Nz
            IF(OEcho.GE.3)THEN
C             zone pressure correction: i.e. Pa-OSR
              IF (ofact(UnitP).LT.0) THEN
C               substract the outside pressure at the same height as the
C               zone ref level.
                PzDIS=pz(i)-(-G*zz(i)*RhoOut)
              ELSE
                PzDIS=Pz(i)
              END IF
C             convert to user units (ABS to remove the - of OSR)
              CALL RELDIS(PzDIS*ABS(ofact(UnitP))
     &                                          ,3,RelD(1),RelDL(1),0)
              CALL RELDIS(InfZ(i)*ofact(UnitFma),4,RelD(2),RelDL(2),0)
              CALL RELDIS(VenZ(i)*ofact(UnitFma),4,RelD(3),RelDL(3),0)
              CALL RELDIS(FT(i)*ofact(UnitFma)  ,4,RelD(4),RelDL(4),0)
              CALL RELDIS(REAL(FMB(i))*ofact(UnitFma)
     &                                          ,4,RelD(5),RelDL(5),0)
              CALL RELDIS(Tz(i)*ofact(UnitTmul)
     &                          +ofact(UnitToff),4,RelD(6),RelDL(6),0)
              CALL RELDIS(Output_Xh(Tz(i),REAL(Pz(i)),Xhz(i))
     &                            *ofact(UnitXh),4,RelD(7),RelDL(7),0)
              WRITE(file,'(2X,A10,7A11)')
     &          ZoNa(i),(RelD(j)(1:RelDL(j)),j=1,7)
            ENDIF
            Inftot=InfTot+InfZ(i)
            VzTot=VzTot+Vz(i)
          ENDDO


C------------------------------------
C         Results for groups of rooms
C------------------------------------

C         here grouping rooms with identical first two characters M1 M2 M3

          WRITE(file,'(/a)')'+--ZONE-GROUPS--  -----------Flo'//
     &    'w rates----------  --------Air change rates-------'
          WRITE(file,'(a)')'| Name    Volume    Infiltr.    '//
     &    'Ventil.     Total    Infiltr.    Ventil.     Total'
          WRITE(file,'(a1,1X,2X,1X,7A11)')'|','         m3'
     &    ,(RightAlign(oUnit(UnitFma) ,11),j=1,3)
     &    ,(RightAlign(oUnit(UnitRate),11),j=1,3)
          WRITE(file,'(a)')'+---------------  --------------'//
     &    '-----------------  -------------------------------'

C         HadZ(i)=1 means we 've had zone i
          DO J=1,Nz
            HadZ(j)=0
          ENDDO

          i=0
900       continue
          i=i+1
          if (hadZ(i).eq.0) then
          LookStr=ZoNa(i)(1:2)
C           got one, Exit this loop
            goto 902
          end if
          goto 900
902       continue

          InfTotG=0
          VzTotG=0
          DO I=1,Nz
            if (ZoNa(i)(1:2).eq.LookStr(1:2)) then
              HadZ(i)=1
              InftotG=InfTotG+InfZ(i)
              VzTotG=VzTotG+Vz(i)
            end if
          ENDDO

C         loop through all links to find links that are connected to
C         rooms that fit the group identified by LookStr. But don't count
C         if both FROM and TO are with in the LookStr group, because that
C         would be a flow 'circulating' within the group.
C         Count incomming flows: add  Fv2(2,i)->FROM  and  Fv2(1,i)->TO

          VenTot=0
          DO I=1,NL
            FROM=FromTo(1,I)
            TO=FromTo(2,I)
            L=Lstat(i)
            IF (L .EQ. 0) THEN
C             zone-zone
              ToYes=0
              if (ZoNa(To  )(1:2).eq.LookStr(1:2)) ToYes=1
              if (ZoNa(From)(1:2).eq.LookStr(1:2)) then
                if (ToYes.eq.0) Ventot=VenTot+FV2(2,I)
C               From belongs to the group and To not, Add Fv2(2,i)
              else
                if (ToYes.eq.1) Ventot=VenTot+FV2(1,I)
C               To belongs to the group and From not, Add Fv2(1,i)
              end if
            ENDIF
          ENDDO

          if (VztotG.gt.0.0) then
            CALL RELDIS(VzTotG                ,4,RelD(1),RelDL(1),0)
            CALL RELDIS(InfTotG*ofact(UnitFma),4,RelD(2),RelDL(2),0)
            CALL RELDIS(VenTot*ofact(UnitFma) ,4,RelD(3),RelDL(3),0)
            CALL RELDIS((InfTotG+VenTot)*ofact(UnitFma)
     &                                        ,4,RelD(4),RelDL(4),0)
            CALL RELDIS(InfTotG*3600/(VzTotG*1.2)*ofact(UnitRate)
     &                                        ,4,RelD(5),RelDL(5),0)
            CALL RELDIS(VenTot*3600/(VzTotG*1.2)*ofact(UnitRate)
     &                                        ,4,RelD(6),RelDL(6),0)
            CALL RELDIS((InfTotG+VenTot)*3600/(VzTotG*1.2)
     &                       *ofact(UnitRate) ,4,RelD(7),RelDL(7),0)
            WRITE(file,'(2X,A2,1X,7A11)')
     &        Lookstr(1:2),(RelD(j)(1:RelDL(j)),j=1,7)
          end if

C         look for zones we have not yet had ( HadZ(i)=0 then )
          i=0
920       i=i+1
          if (i.gt.nz) goto 930
          if (hadz(i).eq.0) then
C           have not yet had this zone name, Reset i (i=the zone number)
            i=0
C           and loop 900 again
            goto 900
          end if
          goto 920
930       continue


C-----------------------------------
C         Results for whole building
C-----------------------------------

          if ((VzTot.gt.0).AND.(Nz.GT.0)) then
            WRITE(file,*)
            WRITE(file,*)'Total infiltration    ='
     &      ,InfTot*ofact(UnitFma),' ',oUnit(UnitFma)
            WRITE(file,*)'Total air change rate =',InfTot*3600/
     &        (VzTot*1.2)*ofact(UnitRate),' ',oUnit(UnitRate)
            WRITE(file,*)'Total building volume =',(VzTot),' ','m3'
          endif

        ENDIF
C       end of if Nz>0
      ENDIF
C     end of if Nl>0
      RETURN
      END


Ch*****************************************************************************
         SUBROUTINE TimeStr(Time,DTstring)
C****************************************************************************
C Purpose:
C call numcreate to convert the timeloop time to Julian time
C call CreateJdTim to get the Julian day and Time of the day
C call ConvDat1 to get the DTstring from Jday1 and NSecDay
C
C UseMonthName controls the format 1 -> 1996may21_10:30:59
C                                  0 ->  19960521_10:30:59
C
C
Ch***************************************************************************
        IMPLICIT NONE
         include 'comv-uni.inc'
         DOUBLE PRECISION Time
         character*31 DTstring
         INTEGER NSecDay,Jday1,NumCreate

         CALL CreateJdTim(NumCreate(Time),Jday1,NSecDay)
         CALL CONVDAT1(Jday1,NSecDay,DTstring)
         return
         end


C@tno jcp 1996May28_11:21:26 string with Date-Time renamed to DTstring
Ch*****************************************************************************
         SUBROUTINE CONVDAT1(Jday,TDay,DTstring)
C****************************************************************************
C Purpose:
C CONVDAT1 converts Jday TDay into a date-time string
C call CalDat for year month day
C call CalTim for hour, min, sec
C
C@tno jcp 1996May01_15:56:21
C  Jday= Julian day number
C  TDay= seconds in this day
C
Ch***************************************************************************
        IMPLICIT NONE
         include 'comv-uni.inc'
C@tno jcp 1996Jun27_12:02:32 include inp for UseMonthName
         include 'comv-inp.inc'
         INTEGER Jday,TDay
        INTEGER LenStr
         character*31 DTstring

C global:
         INTEGER year,month,day,hour,minute,second,lstr


C local:

C arryear: contains the month days for a year
C@tno jcp 1996Apr15_12:34:58 WDayNr added
         INTEGER WdayNr
         CHARACTER*10 DayName
         CHARACTER*3 monstr(12)
         Character*6 WDystr(7)


         DATA monstr / 'jan','feb','mar','apr','may','jun','jul',
     & 'aug','sep','oct','nov','dec' /
         DATA Wdystr / '   Mon',
     &                 '  Tues',
     &                 'Wednes',
     &                 ' Thurs',
     &                 '   Fri',
     &                 ' Satur',
     &                 '   Sun' /
C@tno jcp 1996Apr15_12:37:27 WdayStr added
         call CalDat(JDay,year,month,day)
c         write(cof,*) 'after CalDat',year,month,day
         call CalTim(TDay,hour,minute,second)
c         write(cof,*) 'after CalTim',hour,minute,second
c         call ho('','')

C now put the data in a DTstring
         DTstring=' '
c         write(*,*) 'in convdat1, UseMonthName=',UseMonthName
c         call ho('','')
         if (UseMonthName.gt.0) then
           WRITE (DTstring(1:5),60) year
           DTstring(6:8) = monstr(month)
c           write(*,*) 'DTstring',DTstring
           WRITE (DTstring(9:10),50) day
c           write(*,*) 'DTstring',DTstring
           DTstring(11:11)='_'
           WRITE (DTstring(12:14),51) hour
c           write(*,*) 'DTstring',DTstring
           WRITE (DTstring(15:17),51) minute
c           write(*,*) 'DTstring',DTstring
          WRITE (DTstring(18:19),50) second
        else
C@tno jcp 1996Jun26_1:31:11 format for Capsol signalled by UseMonthName=0
C sure this can write more efficiently using a proper format
           WRITE (DTstring(1:5),60) year
           WRITE (DTstring(6:7),50) Month
           WRITE (DTstring(8:9),50) day
           DTstring(10:10)='_'
           WRITE (DTstring(11:13),51) hour
c           write(*,*) 'DTstring',DTstring
           WRITE (DTstring(14:16),51) minute
c           write(*,*) 'DTstring',DTstring
          WRITE (DTstring(17:18),50) second
          IF (second .LT. 10) DTstring(17:17)='0 '
        end if
c         write(*,*) 'DTstring',DTstring
c         call ho('','')

C@tno jcp 1996Apr15_12:33:34 get the name of the weekday
        call DayNr(Jday,WdayNr)
c        write(*,*) 'JulDayNr=',Jday,'DayNr=',Wdaynr
c        call ho('','')
        DayName=' '//WdyStr(WdayNr)
        lstr=lenstr(dayname)
        DayName=DayName(1:LSTr)//'day'
        DTstring(20:20+LStr+2)=DayName
c         write(*,*) 'DTstring',DTstring
c         call ho('','')




	RETURN
50      FORMAT (I2.2)
51      FORMAT (I2.2,':')
C@tno jcp 1996May02_18:26:22 I5.4= 5wide INTEGER; 4wide filled with leading zero
60      FORMAT (I5.4)
	END



Ch*****************************************************************************
C@tno jcp 1996May01_15:19:35 routine placed again
      SUBROUTINE CALDAT(JDay,iYYY,MM,ID)
Ch*****************************************************************************
        IMPLICIT NONE
      INTEGER JDay
      INTEGER JDay1
      INTEGER iYYY,MM,ID

      INTEGER Jalpha,igreg,ja,jb,jc,jd,je

      PARAMETER (IGREG=2299161)
      IF(JDay.GE.IGREG)THEN
        JALPHA=INT(((JDay-1867216)-0.25)/36524.25)
        JA=JDay+1+JALPHA-INT(0.25*JALPHA)
      ELSE
        JA=JDay
      ENDIF
      JB=JA+1524
      JC=INT(6680.+((JB-2439870)-122.1)/365.25)
      JD=365*JC+INT(0.25*JC)
      JE=INT((JB-JD)/30.6001)
      ID=JB-JD-INT(30.6001*JE)
      MM=JE-1
      IF(MM.GT.12)MM=MM-12
      IYYY=JC-4715
      IF(MM.GT.2)IYYY=IYYY-1
      IF(IYYY.LE.0)IYYY=IYYY-1

C@tno jcp 1996May03_13:05:55 all days in negative years that are not in the
C leap years jan01..feb28 are one too high
      if(IYYY.lt.0) then
        if( MOD(Iyyy,4).eq.0 .and.
     &    (mm.eq.1 .or. (mm.eq.2 .and. Jday.lt.28)) ) then
        else
          if( MOD(Iyyy+1,4).eq.0 .and.
     &       (mm.gt.2 .or.(mm.eq.2 .and. Jday.eq.29)) ) then
          else
C substract a day and calculate the date again with a copy of this routine witho
            JDay1=JDay-1
            call CALDATNEG(JDay1,iYYY,MM,ID)
          end if
        end if
      end if

      RETURN
      END

Ch*****************************************************************************
C@tno jcp 1996May03_13:05:31 this is a patch for the mistake at negative years
      SUBROUTINE CALDATNEG(JDay,iYYY,MM,ID)
Ch*****************************************************************************

        IMPLICIT NONE
      INTEGER Igreg
      PARAMETER (IGREG=2299161)

      INTEGER Jday,Iyyy,MM,Id
      INTEGER Jalpha,Ja,JB,JC,JD,JE

      IF(JDay.GE.IGREG)THEN
        JALPHA=INT(((JDay-1867216)-0.25)/36524.25)
        JA=JDay+1+JALPHA-INT(0.25*JALPHA)
      ELSE
        JA=JDay
      ENDIF
      JB=JA+1524
      JC=INT(6680.+((JB-2439870)-122.1)/365.25)
      JD=365*JC+INT(0.25*JC)
      JE=INT((JB-JD)/30.6001)
      ID=JB-JD-INT(30.6001*JE)
      MM=JE-1
      IF(MM.GT.12)MM=MM-12
      IYYY=JC-4715
      IF(MM.GT.2)IYYY=IYYY-1
      IF(IYYY.LE.0)IYYY=IYYY-1
      RETURN
      END

Ch*****************************************************************************
C@tno jcp 1996May01_15:51:07 alalog to CalDat
      SUBROUTINE CALTIM(TDay,h,m,s)
Ch*****************************************************************************
        IMPLICIT NONE
c      TDay= seconds since the start of the day, 00:00
      INTEGER TDay,h,m,s
      INTEGER timehelp

      timehelp=tDay
      h=INT(timehelp/3600)

      timehelp=timehelp-h*3600
      m=INT(timehelp/60)

      timehelp=timehelp-(m*60)
      s=INT(timehelp)

      RETURN
      END


C@NBI PGS 2000Jul20 - This subroutine was no longer used, so commented out
CCC@tno jcp 1996May28_11:09:27 changed the word 'number' into TLotus
CCC@tno jcp 1996May28_11:09:27 changed the word 'string' into DTstring
CCCh*****************************************************************************
CC         SUBROUTINE CONVDATE(TLotus,DTstring)
CCC****************************************************************************
CCC Purpose:
CCC CONVDATE converts a Double 'TLotus' that is used by the COMIS-output-
CCC files into a date DTstring
CCC LOTUS uses a special time format: the digits before the decimal point are
CCC the days and the digits behind the decimal point are the time.
CCC calculation of the days: beginning at 01/01/1900 all days are added and
CCC    that gives the TLotus INTEGER part for the days
CCC calculation of the time: a day has 86400 seconds and so one second of a
CCC    day is the 86400th part of that day. So every seconds of the day are
CCC    summarized to the current second (second*1/86400)
CCC@tno jcp 1996Apr15_12:32:18 added the weekday name
CCC@tno jcp 1996May01_15:08:09 unfortunately this Lotus time has been chosen as
CCC a core in COMIS despite the JulDay (including time) approach was much better
CCC founded and in the program with a conversion back to a date. The ConvDate
CCC function fails in many respects.
CCC Worse: the CalDat Routine (that had no short commings) has been deleted
CCC
CCC original version: 05/28/91 lbl whb
CCC modified for COMVEN: 08/06/91 lbl bs
CCC
CCCh***************************************************************************
CC
CC        IMPLICIT NONE
CCC global:
CC         INTEGER year,month,day,hour,minute,second
CC         DOUBLE PRECISION TLotus
CCC@tno jcp 1996Apr15_12:42:43 DTstring from 18 to 31 char
CC         INTEGER LotusjDay
CC        INTEGER LenStr
CC         CHARACTER*31 DTstring
CC
CC
CCC local:
CC         INTEGER arryear(12)
CC         COMMON /Month1/ArrYear
CC         CHARACTER monStr(12)*3,WDystr(7)*6
CC         COMMON /Month2/monStr,WDystr
CC
CCC arryear: contains the month days for a year
CCC@tno jcp 1996Apr15_12:34:58 WDayNr added
CC         INTEGER i,help,time,date,WDayNr
CC         DOUBLE PRECISION timehelp
CCC@tno jcp 1996May28_12:43:57 Time1 renamed into JDay1 (2 more in the rest of the
CC         INTEGER Jday1
CC         CHARACTER*10 DayName
CC
CC
CC
CCC TLotus is converted into a date (year,month,...)
CCC first: split the TLotus in two parts:
CCC the days (before the decimal point)...
CC          date=IDINT(TLotus)
CCC@tno jcp 1996May01_14:51:59 to correct for negative Lotus times
CC          if (TLotus.lt.0) then
CC            date=date-1
CC          end if
CCc          write(*,*) 'convdate date=',date
CCC ...and the time (behind the decimal point)
CC          timehelp=TLotus-DBLE(date)
CCc          write(*,*) 'convdate timehelp=',timehelp
CCC@tno jcp 1996May01_14:52:34 time should always be positive now
CC          time=IDINT(timehelp*1000000)
CCc          write(*,*) 'convdate time=',time
CC
CC
CC
CC
CCC in the first half the year,month and day are calculated
CCC The year is relative to 1900 so 1993 would be 93
CC          year=INT(REAL(date-1)/365.25)
CC          help=INT(REAL(year)/4)
CC	  IF (MOD(year,4) .EQ. 0) THEN
CC	     arryear(2)=29
CC	  ELSE
CC	     help=help+1
CC	     arryear(2)=28
CC	  ENDIF
CC	  date=date-(year*365+help)
CC
CC	  DO 20 i=1,12
CC	     month=i
CC	     IF (date .LE. arryear(i)) GOTO 30
CC	     date=date-arryear(i)
CC20	  CONTINUE
CC
CC30	  day=date
CCc          write(*,*) 'year =',year
CCc          write(*,*) 'month=',month
CCc          write(*,*) 'day  =',day
CC
CCC in the second half the time is calculated
CC	  timehelp=DBLE((time+10)*0.0864)
CC	  hour=IDINT(timehelp/3600)
CC
CC	  timehelp=timehelp-DBLE(hour*3600)
CC	  minute=IDINT(timehelp/60)
CC
CC	  timehelp=timehelp-DBLE(minute*60)
CC	  second=IDINT(timehelp)
CC
CCC to avoid impossible results...
CC	  IF (hour .GE. 24) THEN
CC	     IF (day .EQ. arryear(month)) THEN
CC		IF (month .EQ. 12) THEN
CC		   year=year+1
CC		   month=1
CC		ELSE
CC		  month=month+1
CC		ENDIF
CC		day=1
CC	     ELSE
CC		day=day+1
CC	     ENDIF
CC	     hour=hour-24
CC	  ENDIF
CCC now put the data in a DTstring
CC
CC         WRITE (DTstring(1:2),50) day
CC         IF (day .LT. 10) DTstring(1:1)='0'
CC
CC         DTstring(3:5) = monstr(month)
CC
CC         WRITE (DTstring(6:9),60) year+1900
CC         IF (year .LT. 10) DTstring(8:8)='0'
CC
CC         DTstring(10:10)='_'
CC         WRITE (DTstring(11:12),50) hour
CC         IF (hour .LT. 10) DTstring(11:11)='0'
CC
CC         DTstring(13:13)=':'
CC         WRITE (DTstring(14:15),50) minute
CC         IF (minute .LT. 10) DTstring(14:14)='0'
CC
CC        DTstring(16:16)=':'
CC        WRITE (DTstring(17:18),50) second
CCC@tno jcp 1996Apr15_14:09:48 1 space added to time DTstring
CC        IF (second .LT. 10) DTstring(17:17)='0 '
CC
CCC@tno jcp 1996Apr15_12:33:34 get the name of the weekday
CCCfirst get the JDay Day number
CC        Jday1=LotusJday(TLotus)
CC        call DayNr(JDay1,WdayNr)
CCc        write(cof,*) 'JulDayNr=',NSecDay,' DayNr=',Wdaynr
CC        DayName=' '//WdyStr(WdayNr)
CC        DayName=DayName(1:LenSTr(DayName))//'day'
CC        DTstring(19:)=DayName
CC
CC
CC
CC
CC	RETURN
CC50	FORMAT (I2)
CC60	FORMAT (I4)
CC        END
CC
CCC initialization of named COMMON/month/ are now in BLOCKDATA
CC
CC        BLOCK DATA Month
CC         INTEGER arrYear(12)
CC         CHARACTER monStr(12)*3,WDystr(7)*6
CC         COMMON /Month1/ArrYear
CC         COMMON /Month2/monStr,WDyStr
CC         DATA arrYear / 31,28,31,30,31,30,31,31,30,31,30,31 /
CC         DATA monStr / 'jan','feb','mar','apr','may','jun','jul',
CC     & 'aug','sep','oct','nov','dec' /
CC         DATA WdyStr / '   Mon',
CC     &                 '  Tues',
CC     &                 'Wednes',
CC     &                 ' Thurs',
CC     &                 '   Fri',
CC     &                 ' Satur',
CC     &                 '   Sun' /
CCC@tno jcp 1996Apr15_12:37:27 WdayStr added
CC
CC        END

Ch**********************************************************************
	SUBROUTINE MeanOut(file)
C***********************************************************************
C
C
C Purpose: write the mean values out to COF
C
C created @empa 3jan95 hf
C
C changes:
C@NBI PGS 1999May05 - Moved .COF header further down in routine.
C@NBI                 This is because the routine didn't properly check if
C@NBI                 header is needed when options are written to .COS files
C@NBI                 i.e. for keywords FW & FB
C@NBI               - 'NoHeader' toggle ensures that header written only once
C@NBI               - Added time-averaging calculations for "-OSR" option.
C@NBI                 This is done in subroutine GetData
C@NBI PGS 2003Mar16 - Added INCLUDE comv-uni to make OEcho available
C
C Pass parameters:
C
C       file      INTEGER       filedesc(channel)
Ch**********************************************************************
        IMPLICIT NONE
      INCLUDE 'comv-inp.inc'
      INCLUDE 'comv-uni.inc'

        INTEGER file, i, KwNr, LiZoNr,flowdir
        REAL Offset, Multiply, OldV, SumVdt
        CHARACTER KW*3, LiZoNa*15, GetLiZoNa*15
C@NBI PGS 1999May05   (see above comment about header)
      LOGICAL NoHeader
      DATA NoHeader/.TRUE./
C-----------------------------------------------------------------------

        i=1
C start of loop
100     Continue
C if no more or end of array then endloop
        if ((IOOptT(1,i).eq.0).or.(i.gt.MaxOOT)) then
          goto 110
        endif
C set the short values for this loop
        KwNr=IOOptT(1,i)
        LiZoNr=IOOptT(2,i)
        FlowDir=IOOptT(3,i)
        SumVdt=ROOptT(1,i)
        OldV=ROOptT(2,i)
C Findout the Namestring from IOOptT
        KW=KeyW(KwNr)
        LiZoNa=' '

c---------------------------------------------
c     Check if output to separate file (*.COS)
c---------------------------------------------

        if (KW(1:2).eq.'FB') then
          call wriflowmat(COSLun(KwNr),0.D0,.true.)
C@tno jcp 1996May24_09:38:18 added else if for FW option
        elseif (KW(1:2).eq.'FW') then
          call wri2flowmat(COSLun(KwNr),0.D0,.true.)

c-----------------------------
c     Else output to .COF file
c-----------------------------

C@NBI PGS 2003Mar16 - Only write out if OEcho>0.
CC      else
        elseif (OEcho.GT.0) THEN
          IF(NoHeader)THEN
            NoHeader=.FALSE.
C           write header out to COF
C@NBI PGS 1999May06 - Tidied up output
            WRITE(file,1001)
1001        FORMAT(//,
     &      ' *******************************',/,
     &      ' Mean values for whole period  *',/,
     &      ' *******************************',/,/
     &      '  KeyWord   Li/Zo-Name         Value        Unit',/,
     &      '  ------------------------------------------------')
          ENDIF

C if not only one timestep then calculate mean value
C@NBI PGS 1999Aug11 - As regards ventilation heat loss (option LB), we want
C@NBI                 to know the total heat loss for whole period, not the
C@NBI                 average value of heat energy loss per timestep.
          if (KwUnitNr(kwNr).EQ.UnitE) then
            OldV=SumVdt
CC        if (SumDT.gt.0.0) then
          elseif (SumDT.gt.0.0) then
C@NBI PGS 1999Aug11   (end of patch)
            OldV=SumVdt/SumDT
          endif
C calculate the output Unit
          Offset=0.0
C   For temperature, get offset; others have 0 offset
          if (KWUnitNr(KwNr) .eq. UnitTmul) Offset=ofact(UnitToff)
C   Get multiplier
C@tno jcp 1996Mar29_13:05:40 now incorrect conversion of the concentrations
C they need ofact(UnitPolCo+pollutant) , ie. 17,18,19,20,21
C while for concentrations was used here number 8
          if (KwUnitNr(kwNr).eq.UnitPconc) then
C this is number 8 the wrong number. It should be 16+pollutant-1
C KW contains the C1..C5 string.
C@NBI PGS 2000Oct08 - Can now define different I/O units for each pollutant,
C@NBI                 so changed UnitPol-1 to UnitPolCo
            Multiply=ofact(UnitPolCo+index('12345',kw(2:2)))
          else
            Multiply=ofact(KWUnitNr(KwNr))
          end if

          OldV=Offset+(OldV*Multiply)
C search the Link-/Zonename for LiZoNa
C@empa aw 2000apr04 FlowDir is now parameter of GetLiZoNa the differentiation is now
C                   made there
CC          if (FlowDir.eq.0) then
CC            LiZoNa=GetLiZoNa(KW,LiZoNr)
CC          else
CC            if (FlowDir.eq.1) then
CC               LiZoNa(1:1)='>'
CC            else if (flowDir.eq.2) then
CC               LiZoNa(1:1)='<'
CC            endif
CC            LiZoNa(2:15)=GetLiZoNa(KW,LiZoNr)
CC          endif
            LiZoNa=GetLiZoNa(KW,LiZoNr,FlowDir)

C write the result out to COF
C@NBI PGS 1999May06 - Tidied up output
CC        WRITE(file,1000) '    ',KW,'T  ',LiZoNa,OldV,'   ',
          WRITE(file,1000) KW,LiZoNa,OldV,
     &      OUnit(KWUnitNr(KwNr))
1000	  FORMAT(2X,A3,'T',6X,A12,E17.6E4,3X,A10)
C@NBI PGS 1999May06 - End of tidied up output
CC1000	FORMAT(1X,A4,A3,A3,A12,E17.6E4,A3,A10)
        endif
        i=i+1
C go back to start of loop
        goto 100

110     Continue
C end of routine
        WRITE(file,*) ' '
        WRITE(file,*) ' '

        return
        END
