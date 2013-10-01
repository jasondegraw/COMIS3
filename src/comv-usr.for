C+*********************************************************** comv-usr.f
Ch**********************************************************************

        SUBROUTINE U01(opening,limit)
C***********************************************************************
C U01 could be used to define the opening factor of windows
C The routine is called from Timestep at Window Schedules that have :U01 in
C their name.
C The user of this routine has to define Term 1 and Term2 in order to
C get an opening factor that might be limited to pre defined positions.
C limitation occurs if the Limit parameter in the call is >0

C@tno jcp 1996Jun17_13:20:40
C
Ch**********************************************************************
        IMPLICIT NONE
	INCLUDE 'comv-uni.inc'
        INCLUDE 'comv-inp.inc'
        REAL opening,area,term1,term2
        INTEGER limit

        term1=(1-0.1*Vmet)
        if (term1.lt.0.0) then
          term1=0.0
        end if
        term2=(Tmet/25+0.2)
        if (term2.lt.0.0) then
          term2=0.0
        end if
        opening=term1*term2

        if (opening .gt. 1.0) then
          opening=1.0
        end if

        if (test.ge.1 .and. iecho.ge.5 ) then
             write(cof,*) 'A27 original opening V,T =',
     &     opening,Vmet,Tmet
        end if


        if (limit.gt.0) then
          area=0.8*0.6*opening
c limit
          if (area.le.0) then
            opening=0.0
          elseif (area.le.0.02) then
            opening=0.021
          elseif (area.le.0.16) then
            opening=0.189
          elseif (area.le.0.38) then
            opening=0.564
          else
            opening=0.896
          end if
          if (test.ge.1 .and. iecho.ge.5 ) then
             write(cof,*) 'A27  limited opening V,T =',
     &     opening,Vmet,Tmet
          end if
        end if

        RETURN
        END

Ch**********************************************************************

        SUBROUTINE U02(opening,limit)
C***********************************************************************
C U02 could be used to define the opening factor of windows
C The routine is called from Timestep at Window Schedules that have :U02 in
C their name.
C see routines U01 and A27

C@tno jcp 1996Jun17_13:20:40
C
Ch**********************************************************************
        IMPLICIT NONE
	INCLUDE 'comv-uni.inc'
        INCLUDE 'comv-inp.inc'
C@lbl bvs 1996Nov1 area, term1, term2 not used
CC        REAL opening,area,term1,term2
        REAL opening
        INTEGER limit

        if (limit.gt.0) then
c limit
        end if

        RETURN
        END

Ch**********************************************************************

        SUBROUTINE A27(opening,limit)
C***********************************************************************
C A27 defines the opening factor of a large bedroom window for opening between
C 9:00 and 10:00 for airing.
C The routine is called from Tiestep at Window Schedules that have :A27 in
C their name.
C A27 defined window airing in document CSTB: "Assumptions for the simulations"
C E8-1
C Opening positions are limited to 4 positions 0.0 0.021 0.189 0.564 0.896
C limits were based on a 0.6 wide 0.8 m high window

C@tno jcp 1996Jun17_13:20:40
C
Ch**********************************************************************
        IMPLICIT NONE
	INCLUDE 'comv-uni.inc'
        INCLUDE 'comv-inp.inc'
        REAL opening,area,term1,term2
        INTEGER limit

        term1=(1-0.1*Vmet)
        if (term1.lt.0.0) then
          term1=0.0
        end if
        term2=(Tmet/25+0.2)
        if (term2.lt.0.0) then
          term2=0.0
        end if
        opening=term1*term2

        if (opening .gt. 1.0) then
          opening=1.0
        end if

        if (test.ge.1 .and. iecho.ge.5 ) then
             write(cof,*) 'A27 original opening V,T =',
     &     opening,Vmet,Tmet
        end if


        if (limit.gt.0) then
          area=0.8*0.6*opening
c limit
          if (area.le.0) then
            opening=0.0
          elseif (area.le.0.02) then
            opening=0.021
          elseif (area.le.0.16) then
            opening=0.189
          elseif (area.le.0.38) then
            opening=0.564
          else
            opening=0.896
          end if
          if (test.ge.1 .and. iecho.ge.5 ) then
             write(cof,*) 'A27  limited opening V,T =',
     &     opening,Vmet,Tmet
          end if
        end if

        RETURN
        END


C*****************************************************************************
C@NBI PGS 1999Aug10 - 'interval' is now in the INCLUDE file.  I know this isn't
C@NBI                 a tidy solution, but at least we can now solve heat loss.
CC      SUBROUTINE UserOutF(file,TLotus,interval,DTstring)
        SUBROUTINE UserOutF(file,TLotus,DTstring)
C*****************************************************************************
C*    1991oct28	  ENVUXR
C*    This subroutine allows the user to create his own formatted output file.
C*    A list with names of variables that are available to be used as variables
C*    follows. Following the data description an example is given.
C*    'UserOutF' is part of the module 'comv-usr.f'.
C Write Vwind Dwind Tout grouproom_n1...nn
C n1..nn id the total ventilation.
C from this output is is not complex to make a graph
C every output item is appended to DispStr

C
C@lbl dml 1999nov19 Change integer variable initFlg to logical linInit.
C   Find variable not declared or initialized. Declare as logical, with
C   initial value false.
C
C*****************************************************************************
C
C parameter/dimension
C nw	=  number of wind pressure points (max=maxw)
C nz	=  number of zones (max=maxz)
C nl	=  number of links (max=maxl)
C maxc	=    5	( max. number of concentrations, different gases )
C maxcp = 12000	( max. number of Cp*number of directions in the input file )
C maxl	=  1500	( max. number of links
C maxw	=  1500	( max. number of wind pressure points )
C maxz	=  200	( max. number of zones )
C end of dimension ***
C
C
C name/dimension     unit    type	desciption
C =======================================================
C Pz(maxz)	     Pa	     double	Pressure per Zone
C FMB(maxz)	     kg/s    double	Flow Mass Balance per zone.
C Tz(maxz)           C       REAL       air temperature per zone.
C					Gradients are possible with
C					Layers.
C Xhz(maxz)          C       REAL       absolute moisture content per
C					zone. Gradients are possible
C					with Layers.
C FT(maxz)           kg/s    REAL       total incomming flow per zone
C					(in fact half the sum of all
C					flows)
C C(maxc,maxz)       kg/kg   REAL       concentration of 5 gasses per zone.
C					Gradients are possible with Layers
C					but only for the first gas C(1,..)
C@tno jcp 1996Jun26_16:25:32 Cout id for the MaxC gasses only
CC cout(maxc,nw)      10-6g/kg REAL      concentration of 5 gasses per outside
C cout(maxc)         10-6g/kg REAL      concentration of MaxC gasses
C					facade element (parallel with the
C					cp values, no gradients)
C Source(maxc,maxz)  10-6g/s REAL       pollutant source strength per zone
C Sink               m/s*m2  REAL       pollutant sink strength per zone
C FVnew(maxl)           m3/s    REAL       mass flow vector
C Tl(maxl)           C       REAL       temperature per link
C FV2(2,maxl)        kg/m3   REAL       Flow vector with the 2-way flows
C					per link
C Cp(maxcp)          [-]     REAL       wind pressure coefficient
C Pwind(maxw)        Pa      REAL       windpressure per wind pressure point
C Vmet               m/s     REAL       meteo velocity
C Tmet               C       REAL       meteo air temperature
C Xhmet              g/kg    REAL       absolute humidity at meteo
C FromTo(2,maxl)     [-]     INTEGER    the both zones a link connects,
C					FromTo(1,I) is the zone link I departs
C					From. FromTo(2,I) is the zone it goes
C					To. Negative flows go the other
C					way, but are dealt properly
C					i.e. at summing the flows per zone
C LStat(maxl)        [-]     INTEGER    Link Status:0,1,2,3,4,5,6,7,8
C Cname		     [-]     character	name of pollutant
C MM                 g/mol   REAL       molar mass of the pollutant
C Nconc              [-]     INTEGER    number of concentration
C-------------------------------------------------------------------------------
C Parameter
C file               [-]     INTEGER    unit number for data output file
C TLotus             [sec]   double     current simulation time in Lotus format
C interval           [sec]   INTEGER    time interval
C DTstring         datetime string    current date/time in string
C*******************************************************************************
        IMPLICIT NONE
C the following include files have to be part of this subroutine :
	INCLUDE 'comv-inp.inc'
	INCLUDE 'comv-uni.inc'

C@NBI PGS 1999Aug10 - 'interval' is now in the INCLUDE file
CC      INTEGER file,interval
        INTEGER file
        DOUBLE PRECISION TLotus
C@tno jcp 1996Apr15_12:45:48 DTstring now 31 long
        CHARACTER*31 DTstring


C local parameters

        REAL RhoLink
C@NBI PGS 1999Aug17 - FMBDIS wasn't declared
CC        REAL FTDIS,InfZDIS,VenZDIS
        REAL FTDIS,InfZDIS,VenZDIS,FMBDIS
C@tno jcp 1996Apr12_16:44:17 ProcL never used
CC        REAL ProcL
        INTEGER To,From
        INTEGER START
        INTEGER START1
        INTEGER RelDL(8),Nlam,L,I,J
        INTEGER LiStaFl,LiStaFlag
        CHARACTER*30 RelD(8)
	CHARACTER nodetyp(9)*4
        CHARACTER*180 DispStr
        DOUBLE PRECISION Fma,Dp
        REAL DFma,fma1,fma2
        REAL Dpint,DDpL,SI,temp
        INTEGER ProfStart
        DOUBLE PRECISION DpProfNew(NrInt+2)

        DOUBLE PRECISION DFma1,DFma2

C@tno jcp 1996Apr12_16:45:04 OutUnitP never used
CC        CHARACTER*20 OunitP,LookStr
        CHARACTER*20 LookStr
        REAL PzDis
C InfZ(1..MaxZ) infiltration flow per zone (flow from ext and spec)
C VenZ(1..MaxZ) ventilation  flow per zone (flow from other zones)
C InfTot        sum of InfZ(*)
C VzTot total volume of all zones
        REAL InfZ(MaxZ),VenZ(MaxZ),InfTot,VzTot,VenTot,HadZ(MaxZ)
        INTEGER ToYes


        INTEGER LENSTR
        logical linInit

C see the input file &-NET-EXT for the coupling with cp's
	nodetyp(1)='znzn'
	nodetyp(2)='exzn'
	nodetyp(3)='spzn'
	nodetyp(4)='znex'
	nodetyp(5)='exex'
	nodetyp(6)='spex'
	nodetyp(7)='znsp'
	nodetyp(8)='exsp'
	nodetyp(9)='spsp'

	IF (Test.GE.1 .or. OEcho.GE.2) THEN
          CALL RelDis(Vmet*ofact(UnitW),4,RelD(1),RelDL(1),0)
          CALL RelDis(Dmet*1.0,4,RelD(2),RelDL(2),0)
          CALL RelDis(ofact(UnitToff)+Tmet*ofact(UnitTmul),4,
     &    RelD(3),RelDL(3),0)

C Vmet,Dmet,Tmet
          DispStr=' '//
     &   RelD(1)(1:4)//' '//
     &   RelD(2)(1:4)//' '//
     &   RelD(3)(1:4)


	IF (Test.GE.1 .or. OEcho.GE.1) THEN

	  Nlam=0
C initialize LiStaFlag
          LiStaFlag=0
	  DO 805 I=1,Nl
C initialize LiStaFl
            LiStaFl=0
	    FROM=FromTo(1,I)
	    TO=FromTo(2,I)

C pointer to the first coefficient of this link in Ldat
	    start=pLiLDat(I)
C initialize Start1
C Start1 points at the first data element of this link in LinkDat(*)
 	    Start1=pLinkDat(I)


C the pressure differences across the links are not stored in an array
C Lstat is the link status:
C Lstat From	 To
C  0	zone	zone
C  1	 ext	zone
C  2	spec	zone
C  3	zone	 ext
C  4	 ext	 ext
C  5	spec	 ext
C  6	zone	spec
C  7	 ext	spec
C  8	spec	spec
C
	    L=Lstat(i)
	    IF (L .EQ. 0) THEN
C zone-zone

	      Dp=Pz(FROM)-Pz(TO)+DpL(1,I)+DpJ(I)

	    ELSE IF (L.EQ.3 .or. L.EQ.6) THEN
C zone-ext or zone-spec

	      Dp=Pz(FROM)+DpL(1,I)+DpJ(I)

	    ELSE IF (L.EQ.1 .or. L.EQ.2) THEN
C ext-zone or spec-zone

	      Dp=-Pz(TO)+DpL(1,I)+DpJ(I)

	    ELSE
C ext-ext or spec-ext or ext-spec or spec-spec

		Dp=DpL(1,I)
		IF (Dp.LT.0.0) THEN
		   Dp=DpL(2,I)
		ENDIF

C Set pointer to pressure difference profile for large
C openings and Update DpProfNew
              IF (LDat(Start).EQ.9.) THEN
                ProfStart=ProfPtr(i)
                DO 10 j=1,(NrInt+2)
                 DpProfNew(j)=dpprof(ProfStart-1+j)
10              CONTINUE
              ELSE
                ProfStart=1
              ENDIF
            linInit = .false.
	      CALL FEQN(Fma,DFma,dfma1,dfma2,Dp,LDat(Start),
     &			Mf(I),Mp(I),RhoL(1,I),RhoL(2,i),
     &			sqrRhoL(1,I),sqrRhoL(2,I),
     &			Mul(1,I),Mul(2,I),linInit,fma1,fma2,
     &			DifLim,ofact(UnitFma),
     &			RSqrtDff(I),LinkDat(Start1),
     &			Tl(I),Lstat(I),TempL(1,I),TempL(2,I), NormCrRho,
     &                  NormCrMu,DpProfNew,RhoProfF(ProfStart),
C@tno jcp 1996Jun13_23:18:26 for PS Vmet and RhoOut
     &          RhoProfT(ProfStart),i,FVnew,iRFlist,Vmet,RhoOut)



C fma is the sum of fma1,fma2. Save these 2 flows in FV2(*)

	        FV2(1,i)=fma1
        	FV2(2,i)=fma2

              FVnew(I) = Fma

	    ENDIF

          DDpL=(DpL(1,I)-DpL(2,I))
          SI=SIGN(1.,DDpL)
          DDpL=ABS(DDpL)
          IF (ABS(Dp).LT.DDpL) THEN
             Dpint=(DDpL+Dp)*0.5*SI
       	     Dp=Dp-DpL(1,I)+DpL(2,I)+Dpint
             LiStaFl=1
             LiStaFlag=1
          ELSE IF (Dp.LE.(-DDpL)) THEN
	      Dp=Dp-DpL(1,I)+DpL(2,I)
	  ENDIF

C count of % of links dp<DifLim
	    IF (ABS(Dp).LT.DifLim) then
	    Nlam=Nlam+1
	    ENDIF

	    ltyp=Ldat(start)
	    IF (ltyp.eq.1 .or. ltyp.eq.9) THEN
	    ELSE
	      IF (tl(i).eq.0.0) THEN
                IF (FVnew(I).GT.0.0) THEN
C Take tout and RhoOut, when node is spec. pressure or wind
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

805	  CONTINUE
	ENDIF


C now FVnew(*) and FV2(*) are known, so the infiltration can be calculated

C zero Infiltration
          DO 780 I=1,Nz
            InfZ(I)=0.0
            VenZ(I)=0.0
 780      CONTINUE

C Lstat is the link status:
C Lstat From	 To
C  0	zone	zone
C  1	 ext	zone
C  2	spec	zone
C  3	zone	 ext
C  4	 ext	 ext
C  5	spec	 ext
C  6	zone	spec
C  7	 ext	spec
C  8	spec	spec
C

          DO 790 I=1,NL
	    FROM=FromTo(1,I)
	    TO=FromTo(2,I)
	    L=Lstat(i)
	    IF (L .EQ. 0) THEN
C zone-zone
              VenZ(FROM)=VenZ(FROM)+FV2(2,I)
              VenZ(TO  )=VenZ(TO  )+FV2(1,I)

	    ELSE IF (L.EQ.3 .or. L.EQ.6) THEN
C zone-ext or zone-spec
              InfZ(FROM)=InfZ(FROM)+FV2(2,I)

	    ELSE IF (L.EQ.1 .or. L.EQ.2) THEN
C ext-zone or spec-zone
              InfZ(TO  )=InfZ(TO   )+FV2(1,I)

	    ENDIF

 790      CONTINUE

C zero InfTot
          InfTot=0
          VzTot=0


	  DO 800 I=1,Nz

	    FTDIS=FT(i)*ofact(UnitFma)
	    FMBDIS=FMB(i)*ofact(UnitFma)
	    InfZDIS=InfZ(i)*ofact(UnitFma)
	    VenZDIS=VenZ(i)*ofact(UnitFma)
            Inftot=InfTot+InfZ(i)
            VzTot=VzTot+Vz(i)

C zone pressure correction: i.e. Pa-OSR

             IF (ofact(UnitP).LT.0) THEN
C substract the outside pressure at the same heigth as the zone ref level.
               PzDIS=pz(i)-(-G*zz(i)*RhoOut)
             ELSE
               PzDIS=Pz(i)
             END IF

800	  CONTINUE
	ENDIF


C        WRITE(file,*)'Total infiltration   =',InfTot*ofact(UnitFma),
C     &   ' ',oUnit(UnitFma)

C air change rate
C        WRITE(file,*)'Total air change rate=',InfTot*3600/
C     &  (VzTot*1.2)*ofact(UnitRate),
C     &   ' ',oUnit(UnitRate)
C        WRITE(file,*)'Total building volume=',
C     &  (VzTot),
C     &   ' ','m3'



C present results for groups of rooms

C here grouping rooms with identical first two characters M1 M2 M3

C        WRITE(file,*)'-----Room---- -----------flow rates--------',
C     &   '  ------air change rates-------'
C        WRITE(file,*)' name   volume      inf      vent     total'//
C     &   '       inf      vent     total '
C        WRITE(file,2005)'          m3     ',
C     &   oUnit(UnitFma), oUnit(UnitFma),oUnit(UnitFma),
C     &   oUnit(UnitRate), oUnit(UnitRate),oUnit(UnitRate)


C  HadZ(i)=1 means we 've had zone i
          DO 899 J=1,Nz
            HadZ(j)=0
899       CONTINUE

          i=0
900       continue
          i=i+1
          if (hadZ(i).eq.0) then
            LookStr=ZoNa(i)(1:2)
C got one, Exit this loop
            goto 902
          end if
          goto 900

902       continue


C zero InfTot
          InfTot=0
          VenTot=0
          VzTot=0


          DO 910 I=1,Nz
            if (ZoNa(i)(1:2).eq.LookStr(1:2)) then
              HadZ(i)=1
              Inftot=InfTot+InfZ(i)
              VzTot=VzTot+Vz(i)
            end if
910     CONTINUE



C loop through all links to find links that are connected to rooms that fit
C the group identified by LookStr. But don't count if both FROM and TO are
C with in the LookStr group, because that would be a flow 'circulating'
C within the group.
C Count incomming flows : add Fv2(2,i) -> FROM   and Fv2(1,i) -> TO

          DO 915 I=1,NL
	    FROM=FromTo(1,I)
	    TO=FromTo(2,I)
            L=Lstat(i)
            IF (L .EQ. 0) THEN
C zone-zone
              ToYes=0
              if (ZoNa(To  )(1:2).eq.LookStr(1:2)) ToYes=1

              if (ZoNa(From)(1:2).eq.LookStr(1:2)) then
                if (ToYes.eq.0) then
C From belongs to the group and To not, Add Fv2(2,i)
                  Ventot=VenTot+FV2(2,I)
                end if
              else
                if (ToYes.eq.1) then
C To belongs to the group and From not, Add Fv2(1,i)
                  Ventot=VenTot+FV2(1,I)
                end if
              end if
            ENDIF
C this endif belongs to (L.eq.0)


915      CONTINUE


C              FTDIS=FT(i)*ofact(UnitFma)
C              InfZDIS=InfZ(i)*ofact(UnitFma)
C              VenZDIS=VenZ(i)*ofact(UnitFma)

        if (Vztot.gt.0.0) then
C        WRITE(file,2004) Lookstr(1:2),VzTot*ofact(UnitFma),
C     &   InfTot*ofact(UnitFma),VenTot*ofact(UnitFma),
C     &   (InfTot+VenTot)*ofact(UnitFma),
C     &   InfTot*3600/(VzTot*1.2)*ofact(UnitRate),
C     &   VenTot*3600/(VzTot*1.2)*ofact(UnitRate),
C     &   (InfTot+VenTot)*3600/(VzTot*1.2)*ofact(UnitRate)
c          write(*,*) Lookstr(1:2),' Inftot',inftot,'VenTot',ventot
          temp=(InfTot+VenTot)*3600/(VzTot*1.2)*ofact(UnitRate)
          CALL RelDis(temp,4,RelD(4),RelDL(4),0)
          DispStr=DispStr(1:lenstr(dispstr))//' '//RelD(4)(1:6)
        end if

C look for zones we have not yet had ( HadZ(i)=0 then )
        i=0
920     i=i+1
        if (i.gt.nz) goto 930

        if (hadz(i).eq.0) then
C have not yet had this zone name, Reset i (i=the zone number)
          i=0
C and loop 900 again
          goto 900
        end if
        goto 920

930     continue

        write(file,*) DispStr

	RETURN
	END


C*****************************************************************************




C*****************************************************************************
C@NBI PGS 1999Aug10 - 'interval' is now in the INCLUDE file.
CC      SUBROUTINE UserOutU(file,TLotus,interval,DTstring)
        SUBROUTINE UserOutU(file,TLotus,DTstring)
C*****************************************************************************
C*    1991oct28	  ENVUXR
C*    This subroutine allows the user to create his own unformatted output file.
C*    A list with names of variables that are available to be used as variables
C*    follows. Following the data description an example is given.
C*    'UserOutF' is part of the module 'comv-usr.f'.
C*****************************************************************************
C
C parameter/dimension
C nw	=  number of wind pressure points (max=maxw)
C nz	=  number of zones (max=maxz)
C nl	=  number of links (max=maxl)
C maxc	=    5	( max. number of concentrations, different gases )
C maxcp = 12000	( max. number of Cp*number of directions in the input file )
C maxl	=  1500	( max. number of links
C maxw	=  1500	( max. number of wind pressure points )
C maxz	=  200	( max. number of zones )
C end of dimension ***
C
C
C name/dimension     unit    type	desciption
C =======================================================
C Pz(maxz)	     Pa	     double	Pressure per Zone
C FMB(maxz)	     kg/s    double	Flow Mass Balance per zone.
C Tz(maxz)           C       REAL       air temperature per zone.
C					Gradients are possible with
C					Layers.
C Xhz(maxz)          C       REAL       absolute moisture content per
C					zone. Gradients are possible
C					with Layers.
C FT(maxz)           kg/s    REAL       total incomming flow per zone
C					(in fact half the sum of all
C					flows)
C C(maxc,maxz)       kg/kg   REAL       concentration of 5 gasses per zone.
C					Gradients are possible with Layers
C					but only for the first gas C(1,..)
C@tno jcp 1996Jun26_16:24:42 no more Cout per Nw
CC cout(maxc,nw)      10-6g/kg REAL      concentration of 5 gasses per outside
C cout(maxc)         10-6g/kg REAL      concentration of 5 gasses
C					facade element (parallel with the
C					cp values, no gradients)
C Source(maxc,maxz)  10-6g/s REAL       pollutant source strength per zone
C Sink               m/s*m2  REAL       pollutant sink strength per zone
C FVnew(maxl)           m3/s    REAL       mass flow vector
C Tl(maxl)           C       REAL       temperature per link
C FV2(2,maxl)        kg/m3   REAL       Flow vector with the 2-way flows
C					per link
C Cp(maxcp)          [-]     REAL       wind pressure coefficient
C Pwind(maxw)        Pa      REAL       windpressure per wind pressure point
C Vmet               m/s     REAL       meteo velocity
C Tmet               C       REAL       meteo air temperature
C Xhmet              g/kg    REAL       absolute humidity at meteo
C FromTo(2,maxl)     [-]     INTEGER    the both zones a link connects,
C					FromTo(1,I) is the zone link I departs
C					From. FromTo(2,I) is the zone it goes
C					To. Negative flows go the other
C					way, but are dealt properly
C					i.e. at summing the flows per zone
C LStat(maxl)        [-]     INTEGER    Link Status:0,1,2,3,4,5,6,7,8
C Cname		     [-]     character	name of pollutant
C MM                 g/mol   REAL       molar mass of the pollutant
C Nconc              [-]     INTEGER    number of concentration
C-------------------------------------------------------------------------------
C Parameter
C file               [-]     INTEGER    unit number for data output file
C TLotus             [sec]   double     current simulation time in Lotus format
C interval           [sec]   INTEGER    time interval
C DTstring         datetime string    current date/time in string
C*******************************************************************************
        IMPLICIT NONE
C the following include files have to be part of this subroutine :
	INCLUDE 'comv-inp.inc'
	INCLUDE 'comv-uni.inc'

C@NBI PGS 1999Aug10 - 'interval' is now in the INCLUDE file
CC      INTEGER file,interval
        INTEGER file
        DOUBLE PRECISION TLotus
C@tno jcp 1996Apr15_12:43:37 DTstring expanded from 18 to 31 to house the week
        CHARACTER*31 DTstring

C This example shows how to print the pressure per zones (Pz) and the average
C of all zone pressures. The data is written into the user output file 'file'.
C======================================================================
C
C local variables determined by user :
C => pz_cumulated consists the sum of all pz(i)
C => pz_average is the result of pz_cumulated divided by i
         REAL pzcumulated
         REAL pzaverage
         INTEGER i

	 pzcumulated = 0
C this command writes the pressure per zones into the output file :
	 Write(file) ( pz(i), i=1,nz)
C this command calculates the average per pressure :
	 DO 10 i=1, nz
	   pzcumulated = pzcumulated + pz(i)
10	 CONTINUE
	 pzaverage = pzcumulated/i

C this command writes the average into the output file :
	 WRITE(file) pzaverage

	 RETURN
	 END
C*****************************************************************************
