C+*********************************************************** comv-phy.f
Ch***********************************************************************
C@empa aw 1997sep12 DOUBLE PRECISION
CC      REAL FUNCTION psz(Pz0,Rho0,beta,z0,z,g)
	DOUBLE PRECISION FUNCTION psz(Pz0,Rho0,beta,z0,z,g)
C
C***********************************************************************
C
C                             PSZ
C
C  Purpose: This function  enables to determine the pressure due to buoyancy
C           in a stratified density environment.
C  Changes: hcp included G in the passparameter list
C           z=z-z0 changed into dz=z-z0
C           dz*dz changed into dz*ABS(dz); as we will allow z0 to be above z,
C           but define beta always in positive height direction, dz*dz would
C           be positive and not take into account the negative direction of dz
C@empa vd 1992feb12  p0 removed, psz gives only pressure difference from
C                    zo to z
C@empa aw 1992may05  -Definition of density gradient changed
C		      (see COMIS fundamental p. 44):
C		      old: Rho=Rho0-beta*Rho0*(z-z0)
C		      new: Rho=Rho0+beta*(z-z0)
C                    -psz should be called always with z0=bottom and z=top
C		      of the layer --> - dz is positive
C		         	       - psz is differential pressure from
C				         z to z0
C	                               - ABS(dz) is not needed
C  Module : 4.1.7.1, Task IV, fa020989
C  Limits : linear density stratification
C
C  Pass Parameters:
C  IO     Name   Unit         Description
C  I  1   Rho0   kg/m**3      density at altitude z0
C  I  2   beta   kg/m**4      density gradient
C  I  3   z0     m            reference altitude
C  I  4   z      m            altitude
C  I  5   g      N/kg         gravity field strength
C  O      psz    Pa           Differential pressure from z to z0
Ch***********************************************************************
C
        IMPLICIT NONE
C@empa aw 1997sep12 DOUBLE PRECISION
CC      REAL Pz0,Rho0,z0,beta,z,g
CC      REAL dz,rho
      DOUBLE PRECISION Pz0,Rho0,beta,z,z0
      DOUBLE PRECISION dz,rho
      REAL g

      dz=z-z0
      rho=(Rho0+beta*dz/2)
      Psz=-Pz0*(1-EXP(-dz*rho*g/Pz0))

      RETURN
      END




Ch***********************************************************************

	SUBROUTINE CombDpL(DpL,DpProf,ProfPtr,DpSt,Pwind,Pspec,
     &  P3d,FromTo,Nl,Lstat,Ldat,PLiLdat)
C***********************************************************************
C Name   : CombDpL combine DpL
C Purpose: CombDpL combines stack,Pwind,Pspec,P3D into two pressures across the
C          link. The array Lstat is used to determine if zones, windpressures,
C          specialpressures are involved.
C          DpL(1,..) is for positive flows (From ---->  To)
C          DpL(2,..) is for negative flows (From  <---- To)
C          The difference in those is that the flowpath of the link might go
C          up or down, and will have a stack effect depending on the tempera-
C          ture of the air in the link. This temperature will depend on the
C          flow direction, so both stack effects can be different if the air
C          temperature on both sides is different. The routine CFT simulates the
C          temperature of a crack.
C
C Module :
C Changes:
C@empa vd 1992feb14 New header text
C@empa aw 1994oct11 old markers and codelines deleted
C@empa aw 1994oct11 variable declarations restructured
C@empa aw 1994oct11 pressure difference profile for large openings added
C Limits :
C
C Pass parameters:
C
C IO # Name    Unit     Description
C  O   Dpl     Pa       Pressure difference across link
C IO   DpProf  Pa       Pressure difference profile for large openings
C I    ProfPtr -        Pointer to the first element of a pressure difference
C                       profile
C I    DpSt    Pa       Stack pressure difference
C I    Pwind   Pa       Wind pressure
C I    Pspec   Pa       Special pressure
C I    P3D     Pa       3D- Pressure
C I    FromTo   -       From and To Zone info for link
C I    Nl       -       Number of links
C I    Lstat    -       Link connectivity status info array
C I    Ldat     multi   Data for each linktype
C I    PLiLdat  -       Pointer to first element of linktype data in Ldat
Ch***********************************************************************
        IMPLICIT NONE
	include 'comv-par.inc'
	include 'comv-uni.inc'

C        Pass parameters:
C@empa aw 1997sep18 DpL, DpSt, DpProf as Double Precision
C@empa aw 2002dec04 DpL enlarged from DpL(2,MaxL) to DpL(3,MaxL) to have a space
C@empa aw 2002dec04 for the final pressure differnce
        DOUBLE PRECISION DpL(3,maxl),DpSt(2,maxl)
	DOUBLE PRECISION DpProf(MaxLo*(NrInt+2))
        REAL Pwind(maxw),Pspec(maxs),P3d(maxl),Ldat(*)
        INTEGER ProfPtr(MaxL),FromTo(2,maxl),Nl,Lstat(maxl)
        INTEGER PLiLdat(MaxL)

C        Local variables:
        INTEGER From,To
        INTEGER I,j,L
        INTEGER LTyp, ProfStart

        DO 10 i=1,Nl
          From=FromTo(1,i)
          To  =FromTo(2,i)
          DpL(1,i)=0.
          DpL(2,i)=0.
          l=Lstat(i)
          LTyp=LDat(PLiLDat(i))
          IF (l.eq.0) THEN
          ELSE IF(l.eq.1) THEN
C wind pressure
              DpL(1,i)=DpL(1,i)+Pwind(From)
              DpL(2,i)=DpL(2,i)+Pwind(From)
          ELSE IF(l.eq.2) THEN
C special pressure
              DpL(1,i)=DpL(1,i)+Pspec(From)
              DpL(2,i)=DpL(2,i)+Pspec(From)
          ELSE IF(l.eq.3) THEN
C wind pressure
              DpL(1,i)=DpL(1,i)-Pwind(To)
              DpL(2,i)=DpL(2,i)-Pwind(To)
          ELSE IF(l.eq.4) THEN
C wind pressure
              DpL(1,i)=DpL(1,i)+Pwind(From)-Pwind(To)
              DpL(2,i)=DpL(2,i)+Pwind(From)-Pwind(To)
          ELSE IF(l.eq.5) THEN
C wind pressure
              DpL(1,i)=DpL(1,i)+Pspec(From)-Pwind(To)
              DpL(2,i)=DpL(2,i)+Pspec(From)-Pwind(To)
          ELSE IF(l.eq.6) THEN
C special pressure
              DpL(1,i)=DpL(1,i)-Pspec(To)
              DpL(2,i)=DpL(2,i)-Pspec(To)
          ELSE IF(l.eq.7) THEN
C special pressure
              DpL(1,i)=DpL(1,i)+Pwind(From)-Pspec(To)
              DpL(2,i)=DpL(2,i)+Pwind(From)-Pspec(To)
          ELSE IF(l.eq.8) THEN
C special pressure
              DpL(1,i)=DpL(1,i)+Pspec(From)-Pspec(To)
              DpL(2,i)=DpL(2,i)+Pspec(From)-Pspec(To)
          ENDIF

C@tno jcp 1996May17_15:03:50 temp output
c       if (i.eq.6) then
c        write(cof,*) 'link 6 pressure=',dpl(1,i),dpl(2,i)
c       end if
          IF (LTyp.EQ.9) THEN
             ProfStart=ProfPtr(i)
             DO 20 j=ProfStart,ProfStart+NrInt+1
                DpProf(j)=DpL(1,i)+DpProf(j)+P3d(i)
20           CONTINUE
          ENDIF
C@empa end
C here the stack pressure is added +P3D
          DpL(1,i)=DpL(1,i)+DpSt(1,i)+P3d(i)
          DpL(2,i)=DpL(2,i)+DpSt(2,i)+P3d(i)
C@tno jcp 1996May17_15:03:50 temp output
c       if (i.eq.6) then
c        write(cof,*) 'link 6 pressure=',dpl(1,i),dpl(2,i)
c       end if


 10     CONTINUE
        RETURN
        END

Ch***********************************************************************

      SUBROUTINE Rhocc1(Rho,p,t,xh,c,mm,nconc)

C
C***********************************************************************
C
C                                RhoCC1
C
C
C  Purpose: This routine enables to determine the density of moist air Rho,
C           with nconc pollutants. Here only one concentration is passed
C  Module : 4.1.5.1, Task IV, fa061589
C  Changes:
C      IF (np.eq.0) GO TO 20 | JCP 1990dec28  np changed into Nconc
C  Limits : physical values of the Variables
C
C  Pass Parameters:
C  IO    Name     Unit          Description
C  O     Rho      kg/m*3        Air density of moist air
C  I     p        Pa            Absolute pressure
C  I     t        oC            Air temperature
C  I     xh   kg H2O/kg dry air SpecIFic humidity.
C  I     nconc   nu             Number of different pollutants.
C  I     c(i)    kg/kg dry air  Specific concentration of pollutant i.
C  I     mm(i)   g              Molar mass of pollutant i.
C
C@tno jcp 1996Jan17_09:31:09 RhoCC must be RhoCC1
C   RhoCC1(Rho,101325,20.,.005,.005,44.,1) will RETURN Rho=1.203 kg/m**3
Ch***********************************************************************
C
        IMPLICIT NONE
	INCLUDE 'comv-phy.inc'

        REAL Rho,p,t,xh,c,mm
        INTEGER nconc
        REAL Sigma,Wi

	sigma=xh/(MMH2O/MMAir)
	wi=xh
	IF (Nconc.eq.0) GO TO 20
	IF (mm.NE.0.0)  sigma=sigma+c/(mm/MMair)
	wi=wi+c
20	CONTINUE
	Rho=p/(GasConst*(t+Tzero)*(1+sigma))*(1+wi)
	RETURN
	END

Ch***********************************************************************
C@empa aw 1997sep18 same routine as Rhocc1 but with RHo and p as DOUBLES
      SUBROUTINE Rhocc1d(Rho,p,t,xh,c,mm,nconc)

C
C***********************************************************************
C
C                                RhoCC1
C
C
C  Purpose: This routine enables to determine the density of moist air Rho,
C           with nconc pollutants. Here only one concentration is passed
C  Module : 4.1.5.1, Task IV, fa061589
C  Changes:
C      IF (np.eq.0) GO TO 20 | JCP 1990dec28  np changed into Nconc
C  Limits : physical values of the Variables
C
C  Pass Parameters:
C  IO    Name     Unit          Description
C  O     Rho      kg/m*3        Air density of moist air
C  I     p        Pa            Absolute pressure
C  I     t        oC            Air temperature
C  I     xh   kg H2O/kg dry air SpecIFic humidity.
C  I     nconc   nu             Number of different pollutants.
C  I     c(i)    kg/kg dry air  Specific concentration of pollutant i.
C  I     mm(i)   g              Molar mass of pollutant i.
C
C@tno jcp 1996Jan17_09:31:09 RhoCC must be RhoCC1
C   RhoCC1(Rho,101325,20.,.005,.005,44.,1) will RETURN Rho=1.203 kg/m**3
Ch***********************************************************************
C
        IMPLICIT NONE
	INCLUDE 'comv-phy.inc'

        REAL t,xh,c,mm
        DOUBLE PRECISION p,rho
        INTEGER nconc
        REAL Sigma,Wi

	sigma=xh/(MMH2O/MMAir)
	wi=xh
	IF (Nconc.eq.0) GO TO 20
	IF (mm.NE.0.0)  sigma=sigma+c/(mm/MMair)
	wi=wi+c
20	CONTINUE
	Rho=p/(GasConst*(t+Tzero)*(1+sigma))*(1+wi)
	RETURN
	END


Ch***********************************************************************

      SUBROUTINE Rhocc(Rho,p,t,xh,c,mm,nconc)

C***********************************************************************
C
C                                RhoCC
C
C
C  Purpose: This routine enables to determine the density of moist air Rho,
C           with nconc pollutants.
C  Module : 4.1.5.1, Task IV, fa061589
C  Changes: IF (np.eq.0) GO TO 20 |jcp np changed into Nconc 1990dec28
C  Limits : physical values of the Variables
C
C  Pass Parameters:
C  IO    Name     Unit          Description
C  O     Rho      kg/m*3        Air density of moist air
C  I     p        Pa            Absolute pressure
C  I     t        oC            Air temperature
C  I     xh   kg H2O/kg dry air SpecIFic humidity.
C  I     nconc   nu             Number of different pollutants.
C  I     c(i)    kg/kg dry air  SpecIFic concentration of pollutant i.
C  I     mm(i)   g              Molar mass of pollutant i.
C
C   Rhocc(Rho,101325,20.,.005,.005,44.,1) will RETURN Rho=1.203 kg/m**3
C@NBI PGS 1999Aug04 - Introduced check for negative density, which can occur
C@NBI                 under extreme conditions due to floating point error.
Ch***********************************************************************
C

        IMPLICIT NONE
	INCLUDE 'comv-phy.inc'

        REAL Rho,p,t,xh,c(*),mm(*)
        INTEGER nconc
        REAL Sigma,Wi
        INTEGER I
C
C     MMH2O g = molar mass of water vapor
C     MMair g  = molar mass of dry air
C
      sigma=xh/(MMH2O/MMAir)
      wi=xh
C     IF (np.eq.0) GO TO 20 |jcp np changed into Nconc
      IF (Nconc.eq.0) GO TO 20
      DO 10 i=1,nconc
      IF (mm(i).NE.0.0) sigma=sigma+c(i)/(mm(i)/MMAir)
      wi=wi+c(i)
10    CONTINUE
20    CONTINUE
      Rho=p/(GasConst*(t+Tzero)*(1+sigma))*(1+wi)
      IF(Rho.LT.0.0) CALL ERROR2("Calculated air density is negative!"
     &,"Your input data is suspect",3)
      RETURN
      END
Ch***********************************************************************
        REAL FUNCTION RhoF(p,t,Xh)
C
C***********************************************************************
C
C                                Rho
C
C
C  Purpose: This FUNCTION  enables to determine the density of moist air Rho
C  Changes: New version fa020389, (COMIS Standards)
C  Module : 4.1.3, Task IV, fa111688
C
C  Limits : physical values of the Variables
C
C      OUTPUT.......>           air density            Rho (kg/m*3)
C
C      INPUTS........>          reference pressure     P (Pa)
C                               temperature            T (oC)
C                               specIFic humidity      XH (kg water/kg dry air)
C
Ch***********************************************************************
        IMPLICIT NONE
C for Tzero
	INCLUDE 'comv-phy.inc'

        REAL p,t,Xh
        RhoF=(p/(461.518*(t+Tzero)*(Xh+.62198)))*(1+Xh)
        RETURN
        END


Ch***********************************************************************
	SUBROUTINE PStack(Nl,FromTo,Zl,RhoZ,RhoOut,
     &  Zz,G,DpL,DpProf,ProfPtr,RhoProfF,RhoProfT,
     &  RhoL,LayPtr,LayDat,Pz,
     &  Tz,Xhz,C,Pbz,MM,RhoDrL,lstat,TempL,XhL,CL,
     &  Tout,Xhout,ExtConc,Plinkdat,Linkdat,pLiLdat,Ldat,ZoNa,Mf,Hfl)
C***********************************************************************
C Purpose: module calculates the stack pressures for
C          a link between two zones
C
C Module : #4.1.7, TG n/a, hef/feb. 1, 1989
C Version: empa aw 1994sep09 complete new version:
C                            - Old markers deleted
C                            - Calculation of a whole stack pressure difference
C                              profile for large vertical openings.
C                              Also the profiles of the density in the "From"
C                              and the "To" zone are calculated.
C                              This is made by calling LClimb at every
C                              StartHeight of a layer which is within the top-
C                              and the bottom-height of a large opening.
C                              With these values for density and stack pressures
C                              we go into the routine PresProfile which cal-
C                              culates the whole profile with NrInt (mumber of
C                              integration intervals) values.
C                            - Call of LClimb also for unlayered or external
C                              zones. In that case the parameter "zone" is 0
C                              that means LayPtr points to the first section in
C                              Laydat where StartHeight and all gradients are 0.
C
C Changes:
C@empa aw 1997sep18 Several changes from REAL to DOUBLE PRECISION to improve
C                   the accuracy in case of no driving forces (flow should be
C                   zero)
C Limits:  no obvious limits
C
C Passing Parameters:
C
C
C IO #  Name       units      description
C I     Nl            -       number of links
C I     FromTo(2,MaxL)-       with link information (zone numbers)
C I     Zl(2,MaxL)    m       height above reference height for each link
C I     Zz(MaxZ)      m       reference height for each zone
C I     Rhoz(MaxZ)    kg/m3   air density for each zone
C I     RhoOut        kg/m3   air density outside
C I     G             N/kg    gravity field strength
C O     DpL(2,MaxL)   Pa      array of stack pressures in link
C O     DpProf(MaxLo*NrInt)Pa    aray of differential pressure profiles for
C                             Large openings
C O     ProfPtr(MaxL) -       Pointer to the start of an individual
C                             differential pressure profile
C O     RhoProfF(MaxLo*NrInt)  array of density profiles in From zones for large
C                             openings
C O     RhoProfT(MaxLo*NrInt)  array of density profiles in to zones for large
C                             openings
C O     RhoL(2,MaxL)  kg/m3   array of air density in the links
C I     LayPtr(2,Nz)  -       pointer:zone to start and stop element in LayDat
C I     LayDat(MaxLay)multi   serial:LhL,BetaT(temp),BetaXfct(hum),
C                             BetaCfct (conc)
C I     Pz(MaxZ)      Pa      Zone pressures at ref level
C I     Tz(MaxZ)      C       Zone temp at ref level
C I     Xhz(MaxZ)     g/kg    Zone humidity at ref level
C I     C(MaxC,MaxW:MaxZ+MaxWC) kg/kg   Zone concentrations at ref level
C I     Pbz           Pa      Pbarom at entrance level
C I     MM(Nconc)     (gr)    Molar mass of the Nconc gasses
C O     RhoDrL(2,MaxL)kg/m3   dry air density on both sides of the link
C I     Lstat
C O     TempL(2,MaxL) C       Temp in From and To zone at link level
C O     Xhl(2,MaxL)   g/kg    Humidity in From and To zone at link level
C O     CL(2,MaxL)    kg/kg   Concentration of the first pollutant in From
C                             and To zone at link level
C I	Tout	      C       outside temperature
C I	Xhout	      g/kg    outside humidity
C I     ExtConc(MaxC,Maxz)kg/kg  outside pollutant concentrations
C I 	Plinkdat(MaxL)  -     Pointer to first element of individual link data
C			      in the array Linkdat
C I     Linkdat	      multi   Datas for each individual link
C I     pLiLdat         -     pointer to first element of linktype data in Ldat
C I     Ldat	      multi   Datas for each linktype
C I     ZoNa                  Names of Zones
C I     Mf(MaxL)        -     Multiplication factors for links
C I     Hfl             -     Own height factor for large (slanted) openings
Ch***********************************************************************

        IMPLICIT NONE
        include 'comv-par.inc'
        include 'comv-uni.inc'
        include 'comv-phy.inc'
C
C       Pass parameters:
C
        REAL Zl(2,maxl),RhoZ(maxz),RhoOut,Zz(maxz),G
C@ empa aw 1997 sep18 DOUBLE PRECISION DpL
        DOUBLE PRECISION DpL(2,maxl)
C@empa aw 1997sep12 DpProf as DOUBLE PRECISION
	DOUBLE PRECISION DpProf(MaxLo*(NrInt+2))

        REAL RhoL(2,maxl),LayDat(maxlay),Tz(maxz),Xhz(maxz),
     &		C(MaxC,-MaxW:MaxZ+MaxWC)
C@empa aw 2002may29 bounds according definition in common bloc
CC        REAL RhoProfF(MaxLo*NrInt),RhoProfT(MaxLo*NrInt)
        REAL RhoProfF(MaxLo*(NrInt+2)),RhoProfT(MaxLo*(NrInt+2))
        REAL Pbz,MM(maxc),RhoDrL(2,maxl)
        REAL TempL(2,maxl),XhL(2,maxl),CL(2,maxl),Tout,Xhout
        REAL ExtConc(MaxC,Maxz)
        REAL Linkdat(maxd),Ldat(*),Mf(maxl),Hfl(maxl)

        DOUBLE PRECISION Pz(maxz)

        INTEGER Nl,FromTo(2,maxl)
        INTEGER ProfPtr(Maxl),LayPtr(2,0:MaxZ),Lstat(maxl)
        INTEGER Plinkdat(maxl),pLiLdat(maxl)
	CHARACTER ZoNa(*)*20

C       Local Variables:

C@empa aw 1997sep12 DOUBLE PRECISION 
	DOUBLE PRECISION DpF(MaxNrLay+2),DpP,DpT(MaxNrLay+2)
	DOUBLE PRECISION PSZ,H

        REAL RhoStF(MaxNrLay+2),RhoStT(MaxNrLay+2),RhoDrDummi
        REAL BetaStF(MaxNrLay+1),BetaStT(MaxNrLay+1)
        REAL T,X,Cn,HSt(MaxNrLay+2)
        REAL TzFrom,XhzFrom,CzFrom
        REAL TzTo, XhzTo,CzTo
        REAL ActLh,ActLOwnh
C@empa aw 1997sep12 DOUBLE PRECISION 
        DOUBLE PRECISION Pref

        DOUBLE PRECISION PzFrom,PzTo
C@empa aw 1997sep18 Rhold,Rhostd introduced
	DOUBLE PRECISION RhoLd(2),RhoStd
        INTEGER From,To,Fromz,Toz,Ltyp
        INTEGER I,ll,j,k,LOCtr,Pprof

        CHARACTER ZoNaFrom*20, ZoNaTo*20

        LOCtr=0
C@empa aw 1997sep18 Rhold,Rhostd introduced
        RhoLd(1)=1.2d0
        RhoLd(2)=1.2d0
        RhoStd=1.2d0
        DO 40 I=1,Nl

C       Initialisation :

        From=FromTo(1,I)
        To=FromTo(2,I)
        LL=Lstat(I)
        Ltyp=Ldat(pLiLdat(I))
        IF (Ltyp.EQ.9) THEN
          ActLh=LinkDat(PLinkDat(I)+2)
          ActLOwnh=ActLh*Hfl(i)
        ELSE
          ActLh=0
          ActLOwnh=0
        ENDIF
        IF (LL.eq.0 .OR. LL.EQ.3 .OR. LL.EQ.6) THEN
C	From=zone
          TempL(1,I)=Tz(From)
          XhL(1,I)=Xhz(From)
          CL(1,I)=C(1,From)
          TzFrom=Tz(From)
          XhzFrom=Xhz(From)
          CzFrom=C(1,From)
          PzFrom=Pz(From)
          ZoNaFrom=ZoNa(From)
          RhoL(1,I)=RhoZ(From)
        ELSE
C       From= outside
	  TempL(1,I)=Tout
	  XhL(1,I)=Xhout
	  CL(1,I)=ExtConc(1,From)
	  TzFrom=Tout
	  XhzFrom=Xhout
	  CzFrom=ExtConc(1,From)
          PzFrom=0
          ZoNaFrom='outside'
          RhoL(1,I)=RhoOut
          From=0
        ENDIF
        IF (LayPtr(1,From).EQ.0) THEN
           Fromz=0
        ELSE
           Fromz=From
        ENDIF

        IF (LL.LT.3) THEN
C       To=zone
          TempL(2,I)=Tz(To)
          XhL(2,I)=Xhz(To)
          CL(2,I)=C(1,To)
          TzTo=Tz(To)
          XhzTo=Xhz(To)
          CzTo=C(1,To)
          PzTo=Pz(To)
          ZoNaTo=ZoNa(To)
          RhoL(2,I)=RhoZ(To)
        ELSE
C       To= outside
	  TempL(2,I)=Tout
	  XhL(2,I)=Xhout
	  CL(2,I)=ExtConc(1,To)
	  TzTo=Tout
	  XhzTo=Xhout
	  CzTo=ExtConc(1,To)
          PzTo=0
          ZoNaTo='outside'
          RhoL(2,I)=RhoOut
          To=0
        ENDIF
        IF (LayPtr(1,To).EQ.0)  THEN
           Toz=0
        ELSE
           Toz=To
        ENDIF

C       RhoDrL is Rho at link level without pollutant but with humidity
C@tno jcp 1996May17_15:59:52 sorry:No RhoDrL is here still at zone reference
C heigth
        CALL Rhocc1(RhoDrL(1,i),Pbz+SNGL(PzFrom),TempL(1,i),
     &		XhL(1,i),0.0,0.0,0)
        CALL Rhocc1(RhoDrL(2,i),Pbz+SNGL(PzTo),TempL(2,i),
     &		XhL(2,i),0.0,0.0,0)


C  End initialisation

C calculate DpF the difference between Pz and P at level Z(1,i)

          J=LayPtr(1,Fromz)
C@empa aw 1996jan29 Calculate Rho at link height only if we have large
C@                  openings or layered zones.
C@empa aw 1997sep05 This change is obsolet with Hans' change from 1996may18
CC	  IF((J.NE.1).or.(Ltyp.EQ.9)) THEN
            k=1
C Layers are defined for this "From" zone
C Climb from the reflevel up through the layers up to Zl(1,i),to determine DpF
C for links with a negative Zl(.. we climb down
            CALL LClimb(G,RhoLd(1),Zl(1,I),TempL(1,I),XhL(1,I),
     &			CL(1,I),LayPtr,LayDat,DpF(k),Fromz,PzFrom,Pbz,
     &			mm,RhoDrL(1,I),ZoNaFrom)
            RhoL(1,i)=Rhold(1)
C For large openings calculate the stack pressure difference profile and the
C density profile within the the top- and the bottom- height of the large
C opening
C@empa aw 2003jun02 Do it also for horizontal windows (ActLOwnh=0)
CC            IF (ActLOwnh.GT.0.0) THEN
            IF (ActLOwnh.GE.0.0) THEN
              HSt(k)=Zl(1,I)
              RhoStF(k)=RhoL(1,I)

              k=k+1
              Hst(k)=laydat(j)

12            CONTINUE

C Search for the first startheight of a layer which is within the top- and the
C bottom- height of the large opening.

              IF ((J.GT.layptr(2,Fromz)).OR.(Hst(k).GT.Zl(1,i)))
     &           GOTO 15
              J=J+9
              Hst(k)=laydat(j)
              GOTO 12

15            CONTINUE
C Calculate Rho and stack pressure for every StartHeight of a layer which is 
C within the top- and the bottom-height of the  large opening. 
              IF ((J.GT.layptr(2,Fromz)).OR.
     &            (Hst(k).GE.(Zl(1,i)+ActLOwnh))) GOTO 16
	      T=TzFrom
              X=XhzFrom
              Cn=CzFrom
              CALL LClimb(G,RhoStd,HSt(k),T,X,Cn,LayPtr,Laydat,
     &		DpF(k),Fromz,PzFrom,Pbz,MM,RhoDrDummi,ZoNaFrom)
              RhoStF(k)=Rhostd
              J=J+9
              k=k+1
              Hst(k)=laydat(j)
              GOTO 15

16            CONTINUE
C Stack pressure difference and rho for top-height of the large opening
              HSt(k)=Zl(1,i)+ActLOwnh
              T=TzFrom
              X=XhzFrom
              Cn=CzFrom
              CALL LClimb(G,RhoStd,HSt(k),T,X,Cn,LayPtr,Laydat,
     &		DpF(k),Fromz,PzFrom,Pbz,MM,RhoDrDummi,ZoNaFrom)
              RhoStF(k)=RhoStd

              DO 17 J=1,(k-1)
C@empa aw 2003jun02 for horizontal openings
                 IF ((HSt(J+1)-HSt(J)).GT.0.)THEN
                    BetaStF(J)=(RhoStF(J+1)-RhoStF(J))/(HSt(J+1)-HSt(J))
                 ELSE
	              BetaStF(J)=0
	           ENDIF
17            CONTINUE
C	    (If ActLOwnh.GT.0.0)
            ENDIF
C@empa aw 1996jan29 Calculate stack pressure difference for none LO links
C@                  between unlayered zones with Rho at zone reference height.
C@empa aw 1997sep05 This change is obsolet with Hans' change from 1996may18
CC	  ELSE
CC	    DpF(1)=-G*RhoL(1,I)*Zl(1,I)
CC	  ENDIF

C repeat procedure for the "To" node, DpT

          J=LayPtr(1,Toz)
C@empa aw 1996jan29 Calculate Rho at link height only if we have large
C@                  openings or layered zones.
C@empa aw 1997sep05 This change is obsolet with Hans' change from 1996may18
CC	  IF((J.NE.1).or.(Ltyp.EQ.9)) THEN
            k=1
C Layers are defined for this "To" zone
C Climb from the reflevel up through the layers up to Zl(1,i),to determine DpT
            CALL LClimb(G,RhoLd(2),Zl(2,I),TempL(2,I),XhL(2,I),
     &          CL(2,I),LayPtr,LayDat,DpT(k),Toz,PzTo,Pbz,mm,
     &          RhoDrL(2,I),ZoNaTo)
            RhoL(2,i)=RhoLd(2)

C For large openings calculate the stack pressure difference profile and the 
C density profile within the the top- and the bottom- height of the large
C opening
C@empa aw 2003jun02 Do it also for horizontal windows (ActLOwnh=0)
CC            IF (ActLOwnh.GT.0.0) THEN
            IF (ActLOwnh.GE.0.0) THEN
              HSt(k)=Zl(2,I)
              RhoStT(k)=RhoL(2,I)
 
              k=k+1
              Hst(k)=laydat(j)

20            CONTINUE
              IF ((J.GT.layptr(2,Toz)).OR.(Hst(k).GT.Zl(2,i)))
     &           GOTO 25
              J=J+9
              Hst(k)=laydat(j)
              GOTO 20

25            CONTINUE
C Calculate Rho and stack pressure for every StartHeight of a layer which is 
C within the top- and the bottom-height of the  large opening. 
              IF ((J.GT.layptr(2,Toz)).OR.
     &           (Hst(k).GE.(Zl(2,i)+ActLOwnh))) GOTO 27
	      T=TzTo
              X=XhzTo
              Cn=CzTo
              CALL LClimb(G,RhoStd,HSt(k),T,X,Cn,LayPtr,Laydat,
     &		DpT(k),Toz,PzTo,Pbz,MM,RhoDrDummi,ZoNaTo)
              RhoStT(k)=RhoStd

              J=J+9
              k=k+1
              Hst(k)=laydat(j)
              GOTO 25

27            CONTINUE
C Stack pressure difference and rho for top-height of the large opening
              HSt(k)=Zl(2,i)+ActLOwnh
              T=TzTo
              X=XhzTo
              Cn=CzTo
              CALL LClimb(G,RhoStd,HSt(k),T,X,Cn,LayPtr,Laydat,
     &		DpT(k),Toz,PzTo,Pbz,MM,RhoDrDummi,ZoNaTo)
              RhoStT(k)=RhoStd

              DO 30 J=1,(k-1)
C@empa aw 2003jun02 for horizontal openings
                IF ((HSt(J+1)-HSt(J)).GT.0.)THEN
                   BetaStT(J)=(RhoStT(J+1)-RhoStT(J))/(HSt(J+1)-HSt(J))
                ELSE
	             BetaStT(J)=0
	          ENDIF
30            CONTINUE
            ENDIF


C@empa aw 1996jan29 Calculate stack pressure difference for none LO links
C@                  between unlayered zones with Rho at zone reference height.
C@empa aw 1997sep05 This change is obsolet with Hans' change from 1996may18
CC	  ELSE
CC	    DpT(1)=-G*RhoL(2,I)*Zl(2,I)
CC	  ENDIF


C CALCULATE STACK PRESSURE FOR THE PATH ITSELF
C for different flow directions
C we donot assume any gradient in the crack itself

C@empa aw 1997sep18 As H is now double, use DBLE(ZL)
          H=DBLE((Zl(2,I))-DBLE(Zl(1,I)))
          IF (LL.eq.0 .OR. LL.EQ.3 .OR. LL.EQ.6) THEN
              H=H-zz(From)
          ENDIF
          IF (LL.LT.3) THEN
              H=H+Zz(To)
          ENDIF

C@tno jcp 1996May17_16:09:50 here the calculation goes one step too fast,
C as Rho is not constant over this height, we have to work with at least the
C value at the average height of the link. It would be fair to have RhoL at this
C half-crack-height
C the official way is to call PBarom in all height trajects

C IF AIR FLOWS from "From" to "To"
C@tno jcp 1996May18_13:06:26 better
          Pref=Pbz+Pzfrom+DpF(1)
C@empa aw1997sep05 use PSZ for DpP
CC          DpP=-Pref*(1-EXP(-H*rhoL(1,i)*g/Pref))
          DpP=psz(Pref,RhoLd(1),0.0d0,0.0d0,H,G)

          DpL(1,I)=(DpF(1)-DpT(1)+DpP)

C IF AIR FLOWS from "To" to "From"
C@tno jcp 1996May18_13:08:05 better: dp from barometric height formula
C@tno jcp 1996May18_14:49:39 note we are now gowing down, so H gets the opposite
C sign and so does Pref.
          Pref=Pbz+Pzto+DpT(1)
C@empa aw1997sep05 use PSZ for DpP
CC          DpP=Pref*(1-EXP(H*rhoL(2,i)*g/Pref))
            DpP=-psz(Pref,RhoLd(2),0.0d0,0.0d0,-H,G)
          DpL(2,I)=(DpF(1)-DpT(1)+DpP)

          IF (Ltyp.EQ.9) THEN
              Pprof=LOCtr*(NrInt+2)+1
              ProfPtr(i)=Pprof
              LOCtr=LOCtr+1
CC@lbl bvs 1997Jan24 line too long for some compilers
CC              CALL PresProfile(DpProf(Pprof),RhoProfF(Pprof),RhoProfT(Pprof),
              CALL PresProfile(DpProf(Pprof),RhoProfF(Pprof),
     &		RhoProfT(Pprof),
     &		G,DpF,DpT,BetaStF,BetaStT,RhoStF,RhoStT,
     &		LayPtr,LayDat,From,To,Zl(1,I),ActLh,Hfl(I))

          ENDIF

40      CONTINUE
        RETURN
        END

Ch***********************************************************************
	SUBROUTINE LClimb(G,Rho,Z,T,X,C,
C pass parameter # =      1 2   3 4 5 6
     &  LayPtr,LayDat,Dp,zone,Pz,Pbz,MM,RhoDr,ZoNa)
C       7      8      9  10   11 12  13 14    15
C***********************************************************************
C          LClimb means Layer Climber, climbs through the gradient layers
C Purpose: LClimb calculates the differential pressure from the reflevel
C          in a zone To Z, the level of a link. While in LayDat different
C          Layers can be defined with gradients in Temperature, Humidity
C          and Concentration. Only the first concentration is taken into
C          these gradients.
C          Keep in mind that looping up through these layers we have to use
C          the Beta's of the last layer and the Height of the current layer.
C          The nice advantage of these layer definitions is that they are
C          pure additional information. No layers have to be given, and the
C          number of layers one wants to define is not limited (except the
C          dimension of LayDat.
C
C Module : 4.1.7.2, TG IV, hcp/june 1 1989
C
C Version: empa aw 1992may06
C          This is a new version with many changes to the previous.
C          Structure and individual statements have been changed.
C          Markers are left; most important changes are:
C	    - Laydat has 9 elements per layer
C	    - BetaXfct and BetaCfct are gradient factors
C	    - several changes of the sequence of the statements
C	    - parameter beta=betarho cancelled
C	    - Rhodr with x but without c
C           - Check layerprofile
C Changes:
C
C@empa aw 1993dec20 Links above ref height: The link is in this layer also if
C		    it is on the top of it.
C@empa aw 1993dec20 Links below ref height: The link is in this layer also if
C		    it is on the bottom of it.
C@empa aw1993dec20  IF (Hbot-Htop.EQ.0): Keep the Dp we already have
C@empa aw 1995jan11 call to INERR instead of ERROR2 because Lclimb is now
C@                  called oncefor each zone just after input reading for
C@                  checking the profile
C@empa aw 1997sep18 Several changes from REAL to DOUBLE PRECISION to improve
C                   the accuracy in case of no driving forces (flow should be
C                   zero)
C
C Limits :linear gradients.
C
C Pass parameters:
C
C IO # Name    Unit              Description
C I  1 G       N/kg              gravitation field strength
C IO 2 Rho     kg/m3             Density link level (initialized with rho zone)
C IO 3 Z       m                 Height of the link above the zone reference
C IO 4 T       deg C             temperature at link level
C IO 5 X       g/kg              absolute humidity at link level
C IO 6 C       kg/kg             concentration at link level
C I  7 LayPtr(2,maxz) -             start and stop element in LayDat
C I  8 LayDat(maxlay)kg/s*Pa**n)      Serial:H,BetaT,BetaXfct,BetaCfct
C O  9 Dp      Pa                Stackpressure to the linklevel
C I 10 zone    -                 Zone number
C I 11 Pz      Pa                Zone Pressure (reflevel)
C I 12 Pbz     Pa                Barometric pressure at entrance level
C I 13 MM(Nconc)(kg/kg)          Molar mass of the Nconc gasses
C O 14 Rhodr                     Air density of dry air on the link level
C                                used for the concentration routine
C I 15 ZoNa                      Name of the Zone
C
C example:
C call
Ch***********************************************************************
        IMPLICIT NONE
	include 'comv-par.inc'

        REAL G,Z,T,X,C
        REAL LayDat(maxlay),Pbz,MM(maxc),RhoDr
C@empa aw 1997sep12 Dp DOUBLE PRECISION
        DOUBLE PRECISION Dp,Rho 
        DOUBLE PRECISION Pz
        INTEGER LayPtr(2,0:MaxZ),zone
        INTEGER LenStr
	CHARACTER ZoNa*20

C H     = Start Height of the layer
C BetaT = Temperature gradient of this layer
C BetaXfct = Humidity    gradient factor of this layer
C BetaCfct = Concentration 1 gradient factor of this layer
        REAL H,BetaT,BetaXfct,BetaCfct
        REAL X0,C0
C@empa aw 1997sep15 DOUBLE PRECISION P,Psz,Hbot,Htop
        DOUBLE PRECISION P,PSZ,Htop,Hbot 
        DOUBLE PRECISION Rho0,Rho1,Betarho
        INTEGER L,LStr
C------------------------------------------------------------

        Dp=0.0
        Rho0=Rho
	x0=x
        c0=c
        IF (Z.GT.0.0) THEN
C initialize start values
         L=LayPtr(1,zone)
         H=LayDat(L)
         BetaT=0.0
         BetaXfct=0.0
         BetaCfct=0.0
         BetaRho=0.0
C@empa aw 1997sep15 DOUBLE PRECISION
         Hbot=0.0d0

 1       IF (H.LT.0.0) THEN
C loop until H>0 ; The start of the layer is above 0
          BetaT=LayDat(L+1)
          BetaXfct=LayDat(L+2)
          BetaCfct=LayDat(L+3)
          L=L+9
          IF (L.GE.LayPtr(2,zone)) THEN
           H=Z+1.0
          ELSE
           H=LayDat(L)
          ENDIF
         GOTO 1
         ENDIF

C The link is in this layer also if it is on the top of it.
 2	IF (H.GE.Z) THEN

C the link ends in this layer , we reached the final Dp and BetaRho
          Htop=Z
	  P=Pz+Dp
          IF (Htop.NE.Hbot)THEN
C	     We go up from Hbot to Htop
C	     Rho0 at Hbot
C@empa aw 1997sep15 DOUBLE PRECISION
             CALL Rhocc1d(Rho0,DBLE(Pbz)+p,t,x,c,mm(1),1)
             T=T+(Htop-Hbot)*BetaT
             X=X+(Htop-Hbot)*BetaXfct*X0
             C=C+(Htop-Hbot)*BetaCfct*C0
C            check layer profile
C call INERR instead of ERROR2 because Lclimb is now called once for each zone
C just after input reading for checking the profile
             Lstr=LenStr(ZoNa)
	     IF(X.LT.0.0)THEN
              CALL INERR
     &       ('&-NET-ZL: *'//ZoNa(1:LStr)//': The  humidity layer '//
     &        'profile goes into negative !',' ',.FALSE.,2)
             ENDIF
	     IF(C.LT.0.0)THEN

              CALL INERR
     &       ('&-NET-ZL: *'//ZoNa(1:LStr)//': The concentration layer'//
     &        ' profile goes into negative !',' ',.FALSE.,2)
             ENDIF
C	     Rho1 at Htop
C@empa aw 1997sep15 DOUBLE PRECISION
             CALL Rhocc1d(Rho1,DBLE(Pbz)+p,t,x,c,mm(1),1)
             BetaRho=(Rho1-Rho0)/(Htop-Hbot)
C@tno jcp 1996May18_13:24:35 Pbz+p added in call of Psz
             Dp=Dp+psz(DBLE(Pbz)+p,Rho0,BetaRho,Hbot,Htop,G)
          ENDIF
C	  Rho and RhoDr at height z
C@empa aw 1997sep15 convert dp to single when passing to Rhocc1
          CALL Rhocc1(Rhodr,Pbz+SNGL(pz)+SNGL(dp),t,x,0.0,0.0,0)
C@empa aw 1997sep15 DOUBLE PRECISION
          CALL Rhocc1d(Rho,DBLE(Pbz)+pz+dp,t,x,c,mm(1),1)
          RETURN

         ELSE
C  bottom of the layer is below Z  (Z above ref)
          Htop=H
C P is the pressure up to the start height of the layer we just reached
          P=Pz+dp
          IF (Htop.NE.Hbot)THEN
C	    we go up from Hbot to Htop
C	    Rho0 at Hbot
C@empa aw 1997sep15 DOUBLE PRECISION
            CALL Rhocc1d(Rho0,DBLE(Pbz)+p,t,x,c,mm(1),1)
            T=T+(Htop-Hbot)*BetaT
            X=X+(Htop-Hbot)*BetaXfct*X0
            C=C+(Htop-Hbot)*BetaCfct*C0
C           check layer profile
C call INERR instead of ERROR2 because Lclimb is now
C called once for each zone just after input reading for checking the profile
             Lstr=LenStr(ZoNa)
	     IF(X.LT.0.0)THEN
              CALL INERR
     &       ('&-NET-ZL: *'//ZoNa(1:LStr)//': The  humidity layer'//
     &        ' profile goes into negative !',' ',.FALSE.,2)
             ENDIF
	     IF(C.LT.0.0)THEN
              CALL INERR
     &       ('&-NET-ZL: *'//ZoNa(1:LStr)//': The concentration layer'//
     &        ' profile goes into negative !',' ',.FALSE.,2)
             ENDIF
C           Rho1 at Htop
C@empa aw 1997sep15 DOUBLE PRECISION
            CALL Rhocc1d(Rho1,DBLE(Pbz)+p,t,x,c,mm(1),1)
            BetaRho=(Rho1-Rho0)/(Htop-Hbot)
C Psz RETURNs the proper pressure difference (linear gradient) between Htop and
C Hbot.
C Dp remains the pressure difference between the current Height and the
C reference plane
C@tno jcp 1996May18_13:24:35 Pbz+p added in call of Psz
C@empa aw 1997sep15 DOUBLE PRECISION
            Dp=Dp+psz(DBLE(Pbz)+p,Rho0,BetaRho,Hbot,Htop,G)
          ENDIF

C@tno jcp 1996May18_14:00:18 the next 3 lines added to return the proper Rho
C because I am not sure that it is finally calculated elsewhere
C         C	  Rho and RhoDr at height z
C@empa aw 1997sep15 convert dp to single when passing to Rhocc1
          CALL Rhocc1(Rhodr,Pbz+SNGL(pz)+SNGL(dp),t,x,0.0,0.0,0)
C@empa aw 1997sep15 DOUBLE PRECISION
          CALL Rhocc1d(Rho,DBLE(Pbz)+pz+dp,t,x,c,mm(1),1)

C place current values Hbot and Beta's
          Hbot=H
          BetaT=LayDat(L+1)
          BetaXfct=LayDat(L+2)
          BetaCfct=LayDat(L+3)
          L=L+9
          IF (L.GE.LayPtr(2,zone)) THEN
C we hit the last element of the last defined layer for this zone.
C the gradients up to the highest link in this zone remain those of this last
C layer.
C trick to exit by making the next H>Z, with the proper current Beta's
           H=Z+1.0
          ELSE
           H=LayDat(L)
          ENDIF

         ENDIF
C ENDIF H>Z
         GOTO 2

        ELSE
C This is the ELSE for negative linkheights Z
C Z is negative ; Z is below the refplane
C initialize start values
C L points at the 4 values of the last layer
         L=LayPtr(2,zone)-8
         H=LayDat(L)
         BetaT=0.0
         BetaXfct=0.0
         BetaCfct=0.0
         BetaRho=0.0
C@empa aw 1997sep15 DOUBLE PRECISION
         Htop=0.0d0

 3       IF (H.GT.0.0) THEN
C         loop until H<0 ; The start of the layer is below the zone refplane
          L=L-9
          IF (L.LT.LayPtr(1,zone)) THEN
C with H=Z (negative) this loop will exit, no data for interval Z-refplane
           H=Z
           BetaT=0.0
           BetaXfct=0.0
           BetaCfct=0.0
           BetaRho=0.0
          ELSE
           H=LayDat(L)
           BetaT=LayDat(L+1)
           BetaXfct=LayDat(L+2)
           BetaCfct=LayDat(L+3)
          ENDIF
         GOTO 3
         ENDIF

C The link is in this layer also if it is on the bottom of it.
 4       IF (H.LE.Z) THEN
C         we hit the layer that includes the link
          Hbot=Z
          P=Pz+dp
          IF (Htop.NE.Hbot)THEN
C           Rho1 at Htop
C@empa aw 1997sep15 DOUBLE PRECISION
            CALL Rhocc1d(Rho1,DBLE(Pbz)+p,t,x,c,mm(1),1)
C           now we go from Htop to Hbot
            T=T+(Hbot-Htop)*BetaT
            X=X+(Hbot-Htop)*BetaXfct*X0
            C=C+(Hbot-Htop)*BetaCfct*C0
C           Check layer profile
C call INERR instead of ERROR2 because Lclimb is now
C called once for each zone just after input reading for checking the profile
             Lstr=LenStr(ZoNa)
	     IF(X.LT.0.0)THEN
              CALL INERR
     &       ('&-NET-ZL: *'//ZoNa(1:LStr)//': The  humidity layer '//
     &        'profile goes into negative !',' ',.FALSE.,2)
             ENDIF
	     IF(C.LT.0.0)THEN
              CALL INERR
     &       ('&-NET-ZL: *'//ZoNa(1:LStr)//': The concentration layer'//
     &        ' profile goes into negative !',' ',.FALSE.,2)
             ENDIF
C           Rho0 at Hbot
C@empa aw 1997sep15 DOUBLE PRECISION
            CALL Rhocc1d(Rho0,DBLE(Pbz)+p,t,x,c,mm(1),1)
            BetaRho=(Rho1-Rho0)/(Htop-Hbot)
C@tno jcp 1996May18_13:24:35 Pbz+p added in call of Psz
            Dp=Dp-psz(DBLE(Pbz)+p,Rho0,BetaRho,Hbot,Htop,G)
          ENDIF
C	  Rho and RhoDr at height z
C@empa aw 1997sep15 convert dp to single when passing to Rhocc1
          CALL Rhocc1(Rhodr,Pbz+SNGL(pz)+SNGL(dp),t,x,0.0,0.0,0)
C@empa aw 1997sep15 DOUBLE PRECISION
          CALL Rhocc1d(Rho,DBLE(Pbz)+pz+dp,t,x,c,mm(1),1)
          RETURN

         ELSE
C  bottom of the layer is below Z  (Z below ref)
          Hbot=H
          P=Pz+dp
          IF (Htop.NE.Hbot)THEN
C           Rho1 at Htop
C@empa aw 1997sep15 DOUBLE PRECISION
            CALL Rhocc1d(Rho1,DBLE(Pbz)+p,t,x,c,mm(1),1)
C T,X,C calculated for the lower height
            T=T+(Hbot-Htop)*BetaT
            X=X+(Hbot-Htop)*BetaXfct*X0
            C=C+(Hbot-Htop)*BetaCfct*C0
C           Check layer profile
C call INERR instead of ERROR2 because Lclimb is now
C called once for each zone just after input reading for checking the profile
             Lstr=LenStr(ZoNa)
	     IF(X.LT.0.0)THEN
              CALL INERR
     &       ('&-NET-ZL: *'//ZoNa(1:LStr)//': The  humidity layer '//
     &        'profile goes into negative !',' ',.FALSE.,2)
             ENDIF
	     IF(C.LT.0.0)THEN
              CALL INERR
     &       ('&-NET-ZL: *'//ZoNa(1:LStr)//': The concentration layer'//
     &        ' profile goes into negative !',' ',.FALSE.,2)
             ENDIF
C           Rho0 at Hbot
C@empa aw 1997sep15 DOUBLE PRECISION
            CALL Rhocc1d(Rho0,DBLE(Pbz)+p,t,x,c,mm(1),1)
            BetaRho=(Rho1-Rho0)/(htop-hbot)
C psz is the pressure difference from Htop to Hbot, so we have it to subtract
C@tno jcp 1996May18_13:24:35 Pbz+p added in call of Psz
            Dp=Dp-psz(DBLE(Pbz)+p,Rho0,BetaRho,Hbot,Htop,G)
          ENDIF

C@tno jcp 1996May18_14:00:18 the next 3 lines added to return the proper Rho
C because I am not sure that it is finally calculated elsewhere
C         C	  Rho and RhoDr at height z
C@empa aw 1997sep15 convert dp to single when passing to Rhocc1
          CALL Rhocc1(Rhodr,Pbz+SNGL(pz)+SNGL(dp),t,x,0.0,0.0,0)
C@empa aw 1997sep15 DOUBLE PRECISION
          CALL Rhocc1d(Rho,DBLE(Pbz)+pz+dp,t,x,c,mm(1),1)

C place current values Hbot and Beta's
          Htop=H
          L=L-9
          IF (L.LT.LayPtr(1,zone)) THEN
C trick to exit by making the next H>0.0, with the proper current Beta's
C we are below the lowest layer
           H=Z-1.0
           BetaT=0.0
           BetaXfct=0.0
           BetaCfct=0.0
          ELSE
           H=LayDat(L)
           BetaT=LayDat(L+1)
           BetaXfct=LayDat(L+2)
           BetaCfct=LayDat(L+3)
          ENDIF

         ENDIF
C ENDIF H<Z
         GOTO 4
        ENDIF

        RETURN
        END



Ch***********************************************************************
C
      REAL FUNCTION muF(t)
C
C***********************************************************************
C
C                               MuF
C
C
C  Purpose: This SUBROUTINE enables to determine the dynamical viscosity
C           of dry air MU using a curve fitting between -20C and 40C.
C
C  Module : 4.1.5, Task IV, fa 011989.
C  Changes: New version fa020389 (COMIS Standards)
C  Limits : dry air, ambient temperature ( -20<t<40 c )
C
C      OUTPUT.......>           dynamical viscosity        MU (Pa.s)
C
C      INPUTS........>
C                               temperature            T (oC)
C
C  mu(20)=18.2 E-06
C
Ch***********************************************************************

        IMPLICIT NONE
        REAL t
        muF=(17.16+.0519*t)*1.E-06
        RETURN
        END


Ch***********************************************************************
C@empa aw 1995dec19 Input is massflow now (FmaI instead of FvaI)
C@lbl bvs 1997Jan24 I missed this changed in Dec1995 - now correct
C        SUBROUTINE CONVFAN(PDI,FvaI,RhoI,RhoO,NFI,NFO,
        SUBROUTINE CONVFAN(PDI,FmaI,RhoI,RhoO,NFI,NFO,
C pass parameter number    1    2   3    4    5   6
     &  	PDO,FmaO)
C        	7    8
C***********************************************************************
C Original name RhoFan
C This routine has been changed to fit in the comis program. The input
C is now the pressure in the link, the output the pressure to look up
C the fan mass flowrate from the curve. THEN this mass flowrate is
C multiplied with FmaO/FmaI to give the massflow through the fan.
C Purpose: this routine calculates the effects of air density, fan
C          rotating speed on the fan performance. That is, the fan
C          pressure and the mass flow rate at an air density and at
C          a fan rotating speed under consideration are calculated using
C          the data set of fan pressure, fan mass flow rate, air density
C          and rotating speed when the test was made.
C
C Module : 4.2.3.1, TG IV, hxy/February 6, 1989/March 9, 1989
C Change : june 16 1989 hcp see note above
C to avoid warnings return1=0
C@empa aw 1993jun09 error return and Key canceled
C@empa aw 1993jun09 call error instead of write(CER
C Limit  : only in the case of the same fan size
C
C Pass parameters:
C
C IO # Name    unit        description
C O  1 PDI     (Pa)        fan pressure for the tested fan
C I  2 FmaI    (kg/s)      mass flow rate for the tested fan
C I  3 RhoI    (kg/m3)     air density when the test was made
C I  4 RhoO    (kg/m3)     air density under consideration
C I  5 NFI     (r/s)       fan rotational speed when the test was made
C I  6 NFO     (r/s)       fan rotational speed under consideration
C I  7 PDO     (Pa)        fan pressure  in the networks link
C O  8 FmaO    (kg/s)      mass flow rate in the networks link
C
C example:
C call RhoFAN(300.0,0.1,1.2,1.3,600,650,PDO,FmaO,*,KEY)
C      PDO=381.424, FmaO=0.108333
Ch***********************************************************************


C
        IMPLICIT NONE
        REAL PDI,FmaI,RhoI,RhoO,PDO,FmaO,A,B,NFI,NFO

C
        IF(NFI.EQ.0.0) THEN
C we will never come to here. This error is already trapped in the routine FA
          CALL ERROR('Rotating speed when fantest was made is zero',3)
          GOTO 900
        ENDIF

        IF(RhoI.EQ.0.0) THEN
C we will never come to here. This error is already trapped in the routine FA
          CALL ERROR('Air density when fan test was made is zero',3)
          GOTO 900
        ENDIF
C
        A=NFO/NFI
        B=RhoO/RhoI
C this line has been FmaO=FvaI*A
C@empa aw 1995dec18 Input is now massflow (FmaI instead of FvaI)
CC        FmaO=FVAI*A*RhoO
        FmaO=FmaI*A*B
C       PDO=PDI*A**2*B
C we have PDO (the Dp across the link) and want to know at what PDI to
C look in the fancurve
        PDI=PDO/(A**2*B)
 900    CONTINUE
        RETURN
C
        END




Ch***********************************************************************

        REAL FUNCTION   GF(LAT)

C***********************************************************************
C Purpose: this routine calculates
C
C Module : #4.1.1, TG4, hcp/mar 15, 1989
C Changes: mar 15 1989
C FNPI is used instead of PIF
C Limits : unknown inaccuracy and source
C Range  : input LAT= 0 , 90 ; output G= 9.78  , 9.83
C Used   : pIF
C
C Pass parameters:
C
C IO # Name    unit              description
C  O r GF      N/kg or m/s2      gravity field strength
C I  1 LAT     degree            geographical latitude (equator=0 pole=90)
C
C example:
C@tno jcp 2001 oct 16 gravity constant corrected a bit
CC   G=GF(45.3550) should RETURN G=9.8065 (N/kg)
C   G=GF(45) should RETURN G=9.80665 (N/kg)


Ch***********************************************************************
C
        IMPLICIT NONE
        REAL LAT
        REAL CONV,PI
        REAL FNPi

        pi=FNPI()
        CONV=LAT*2*PI/360
C@tno jcp 2001 oct 16 gravity constant corrected a bit 9.80665 at 45 deg
CC        GF=9.780373*(1+.0052891*(SIN(CONV))**2-.0000059*
CC     &     (SIN(2*CONV))**2)
        GF=9.78084246*(1+.0052891*(SIN(CONV))**2-.0000059*
     &     (SIN(2*CONV))**2)

        RETURN
        END

Ch***********************************************************************

        SUBROUTINE PBAROM(PBz,Z   ,Zref ,PBREF,RhoREF,G)
C pass parameter # =    1   2    3     4     5      6
C***********************************************************************
C Purpose: THIS ROUTINE CALCULATES the barometric pressure as a
C          FUNCTION of the elevation
C
C Module : 4.1.2, TG IV, hcp/ January 28, 1989
C Changes: feb 03 89 comment updated
C Limits : temperature constant
C          gravity constant with height
C
C Pass parameters:
C
C IO # Name    unit              description
C O  1 PBz     (Pa)              barometric pressure at elevation
C I  2 Z       (m)               elevation level  above ref plane
C I  3 Zref    (m)               elevation of the ref plane
C I  4 PBREF   (Pa)              Barometric pressure at ref level
C I  5 RhoREF  (kg/m3)           airdensity at ref level
C I  6 G       (N/kg or m/s2)    gravitational field strength
C
C example:
C  PBAROM(PbZ,1000,0,101300,1.2,9.8065) should RETURN PbZ=90190 Pa
Ch***********************************************************************


        IMPLICIT NONE
        REAL PbZ,Z,Zref,PBREF,RhoREF,G

        Pbz=PBREF*EXP(-(Z-Zref)/(PBREF/(G*RhoREF)))
        RETURN
        END


Ch***********************************************************************

	SUBROUTINE WIND (ZMET,ZRef,VEM,AlphMet,ALPHA,WindProf,VREF,KEY)

C***********************************************************************
C
C Purpose:      Routine calculates wind speed at building Cp's reference height
C               often eave/gutter level
C
C Module:       #4.3.6, TG III, hef, Feb 20, 1988
C
C Changes:      March 3, 1989, hef;  name of parameters
C@empa aw 1993jun09 call error instead of write(CER
C Switch with WindProf between Powerlaw and log
C profile. (Used  literature Wierenga, J. and J. Rijkoord 'Windclimate of
C Netherland' (in Dutch)).
C The profiles are calculated to 60 m the commonly called boundary layer in
C which windprofiles are determined by the roughness of the earth surface.
C Above very rough terrain (City) the profile stretches higher. For this the
C height of the boundary layer is approximated here. Estimated at 40*Z0 or
C a similar curve for Alpha.
C
C Z0 (say: zet zero) is the roughness height and can be roughly estimated from
C the height, H, of important and frequent occurring obstacles in the terrain
C and the fraction, b, of the ground area occupied by these obstacles.
C The terrain is a direction sector of 30 to 45 deg in the wind direction over
C a distance of a few kilometers.
C estimate : Z0=0.5*b*H
C
C If the Heights in the powerlaw are Z1 and Z2  (ZMet, Zbound, ZRef)
C the approximate relation between Alpha and Z0 is:
C  1/Alpha   ln(sqrt(Z1*Z2)/Z0)
C
C list of comparable Z0 and Alpha in the height range of 10 to Zbound (m)
C   z0    alpha Zbound terrain (according to meteorological definitions)
C  (m)    (-)    (m)
C  0.0002 0.085   60   sea
C  0.005  0.118   60   flat         no obstacles, beach, ice plain, snow
C  0.01   0.128   60
C  0.03   0.149   60   open         low grass, field without crop (fallow land)
C  0.07   0.171   60
C  0.10   0.182   60   roughly open low crops, low hedges, few trees, very few h
C  0.25   0.218   60   rough        high and low crops, large obstacles at dista
C                                   15*H, dense rows of trees, low orchards
C  0.5    0.257   60   very rough   obstacles at distances of 10*H, spread wood
C                                   farm buildings, vineyards
C  1.0    0.313   60   closed       obstacles at distances of less than a few ti
C                                   large woods, villages small cities
C  2.0    0.377   80   city centre  alternated low and highrise, old woods with
C  5.0    0.456  200                trees alternated with frequent open areas
C 10.0    0.542  400
C 20.0    0.668  800
C 40.0    0.869 1600
C
C Note that 'our' buildings are always in the class 'very rough'
C Warnings and errors are created by the routine inSchMet
C Error ranges  (for both meteo and building)
C Z0     Alpha
C 0.0002 0.085    error (program will not run, unREAL smooth)
C 40.0   0.87     error (program will not run, too rough)
C
C Warning ranges for meteo
C Z0     Alpha
C <0.01  <0.128   warning (unrealistically smooth)
C >0.07  >0.183   warning (too rough for meteo)
C
C Warning ranges for the building
C Z0     Alpha
C <0.005 <0.118   warning (unREAListic smooth for a building)
C >5.0   >0.456   warning (very rough for a building. profiles are very inaccurate
C
C
C Limits: Wind profiles are a limited way to simulate REAL wind
C         The important temperature profile (stable neutral unstable)
C         is not taken into account.
C         None of the profiles is valid near the ground and below the
C         height of frequent obstacles. Unfortunately this is where most
C         buildings are situated.
C         Profiles are not valid in the wake (eddy) area behind an
C         obstacle. The closest distance to stream up obstacle must be:
C         dist > 12*Hobstacle
C         For very frequent obstacles that occupy the major fraction of the
C         ground surface a displacement height of 0.5..0.75 Hobstacle is used in
C         literature but not in this program. Meteo stations are always in flat
C         terrain and there the displacement height is 0. Above Z0=1m there
C         should be a displacement height correction.
C         Profiles can be used above 20*Z0 . Below that one must do windtunnel
C         tests to get local velocity/direction profiles.
C
C Passing Parameters:
C IO    Name    Units   Description
C I     ZMET    [m]     height at which wind speed is measured
C I     ZRef    [m]     eave height
C I     VEM     [m/s]   wind speed at height of meteo-station
C I     AlphMet [-]     windprofile exponent for meteo site /Z0
C I     ALPHA   [-]     exponent describing the wind profile /Z0
C I     WindProf -      string with:
C                        ALPHA (->powerlaw)    or  Z0 (->log profile)
C                        Alpha contains Alpha      Alpha contains Z0
C                        AlphMet contains AlphMet  AlphMet contains Z0Met
C                       WindProf comes from the user 'unit' Iunit(UnitProf)
C O     VREF    [m/s]   wind speed at 'eave' or other building reference height
C O     KEY     [-]     is set to one, IF error appears in this routine
C
C Error:        values for exponent outside .10 < Alpha < .50
C       ZMET = 0; division by zero
C Example:      CALL WIND(10.,20.,2.,0.3,2.46,0)
C
Ch***********************************************************************


        IMPLICIT NONE
        REAL ZMET,ZRef,VEM,ALPHA,AlphMet
	CHARACTER*(*) WindProf
        INTEGER KEY
        REAL VREF
C       Zbound is the top of the boundary layer (m)
        REAL VBound,Zbound
        INTEGER IcIndex

C ZMet mustbe > 0.001   ; this is checked at inENVWIN in comv-IN2.f
C ALPHA mustbe 0.085..0.87; this is checked at inENVWIN in comv-IN2.f
C If WindProf=Z0 then ALPHA contains Z0 and must be 0.01.. 40.0 m


C selection based on what is in WindProf
        if (ICindex(WindProf,'alpha') .gt. 0)  then
C------------------------------------------------- Powerlaw profile

C ZBound is the height of the boundary layer
C 60 m for non rough terrain (Alpha < 0.34)
C increasing up to a few hundred meter above a City centre (Alpha up to 0.5)
          ZBound=60
          if (Alpha.gt.0.34) then
C this function approximates Vbound=40*Z0
            Zbound=60+(Alpha-0.34)*(10800*(alpha-0.34)+440)
          end if
	  VBound=VEM   *(Zbound/ZMet)**AlphMet
	  VREF = VBound*(ZRef/Zbound)**ALPHA

        else
C------------------------------------------------- Logarithmic profile
C determined by Z0 the roughness height (m)
C now AlphMet contains Zo at Meteo
C and Alpha   contains Zo at the building site
C Zbound is going up to a few hundred meter at rough terrain
          Zbound=AlphMet*40
          if (Zbound .lt. 60.0) Zbound=60.0
	  VBound=VEM *  LOG((Zbound+AlphMet)/AlphMet)/
     &                  LOG((ZMet+AlphMet)/AlphMet)
	  VREF  =VBound*LOG((Zref+Alpha)/Alpha)/
     &                  LOG((Zbound+Alpha)/Alpha)
	endif

	RETURN

        END

Ch***********************************************************************

        SUBROUTINE PRESS (CP,NWIND,VREF,RhoOUT,PWIND)

C*********************************************************************
C
C Purpose:      Routine calculates one outside pressure point
C       from Cp-value and wind speed at eave height.
C
C Module:       #4.3.7, TG III, hef, Feb 20, 1989
C
C Version:      20.march 1991
C Changes:      March 3, 1989, hef; calculate whole array of Cp-values
C               rather than single value.
C@empa          aw/oct90; CP and PWIND specified as assumed size array.
C Limits:       n/a
C
C Passing Parameters:
C
C IO    Name    Units   Description
C I     CP      [-]     pressure coefficient related to eave height
C I     NWIND   [-]     number of outside pressure points
C I     RhoOUT  [kg/m**3]       density of outside air
C I     VREF    [m/s]   wind speed at eave height
C O     PWIND   [Pa]    outside pressure
C
C Limits:       n/a
C
C Example:      CALL PRESS(0.1,0.3,0.5,0.7,0.9,5,2.,1.2,0.24,0.72,1.20,1.68,2.16
C
Ch***********************************************************************

        IMPLICIT NONE
C@NBI PGS 1999Aug09 - Redundant line
CC      INCLUDE 'comv-uni.inc'

        REAL CP(*),VREF,RhoOUT
        INTEGER NWIND

        REAL PDYN

        REAL PWIND(*)
        INTEGER I

        PDYN=0.5*RhoOUT*VREF*VREF

        DO 10 I=1,NWIND
        PWIND(I)=CP(I)*PDYN
10      CONTINUE

        RETURN
        END
        

C@NBI PGS 1999Aug05 - New function to calculate enthalpy of moist air
Ch**********************************************************************

      REAL FUNCTION Enthalpy(T,Xh)

C***********************************************************************
C Purpose: CALCULATES ENTHALPY OF MOIST AIR PER KILOGRAM DRY AIR AT
C          ARBITRARY PRESSURE.  (T & Xh are dependent on pressure)
C Example: When T=20 deg.C & Xh=0.004 kg/kg, Enthalpy = ca. 30268 J/kg
C Source:  ASHRAE Fundamentals, 1993, Section 6, Equation 30
C Changes: 
C Limits :
C Pass parameters:
C IO # Name      unit             description
C O  - Enthalpy  (J/kg)           Enthalpy
C I  1 T         (deg.C)          Air dry bulb temperature
C I  2 Xh        (kg/kg)          Air humidity ratio (moisture content)
Ch**********************************************************************

        IMPLICIT NONE
      REAL T,Xh
      Enthalpy = 1006.0*T + Xh*(2501000.0+1805.0*T)
      END


C@NBI PGS 1999Aug07 - New function to calculate relaive humidity
Ch**********************************************************************

      REAL FUNCTION RelHumid(T,P,Xh)

C***********************************************************************
C Purpose: CALCULATES RELATIVE HUMIDITY MOIST AIR AT ARBITRARY PRESSURE
C Example: When T=20 deg.C & Xh=0.004 kg/kg, Enthalpy = ca. 30268 J/kg
C Source:  ASHRAE Fundamentals, 1993, Section 6
C Changes: Relhumid(20 deg.C, 0.0072621 kg/kg, 101325 Pa) = 0.5 (50 %rh)
C Limits :
C Pass parameters:
C IO # Name      unit             description
C O  - RelHumid  [-]              Relative humidity (fraction, 0->1)
C I  1 T         [deg.C]          Air dry bulb temperature
C I  2 P         [Pa]             Air static pressure
C I  3 Xh        [kg/kg]          Air humidity ratio (moisture content)
Ch**********************************************************************

        IMPLICIT NONE
      REAL T,Xh,P,Pws,Psat,Ws,mu
C     Saturation pressure at temperature:
       Pws = Psat(T)
C     Humidity ratio at saturation:
       Ws = 0.62198*Pws/(P-Pws)
C     Degree of saturation:
       mu = Xh/Ws
C     Relative humidity:
       RelHumid = mu/(1.0-(1.0-mu)*(Pws/P))
      END


C@NBI PGS 1999Aug07 - New function to calculate relaive humidity
Ch**********************************************************************

      REAL FUNCTION Psat(T)

C***********************************************************************
C Purpose: Solves water vapour saturtion pressure at temperature T deg.C
C Example: When T = 20 deg.C,  Psat = 2.3388 Pa
C Eqns from [Hyland & Wexler 1983] cited in ASHRAE FUNDAMENTALS 6.7
C Created: P.G.Schild 1999May18
C Changes: 
C Limits : -100 < T < +200 deg.C
C Pass parameters:
C IO # Name      unit             description
C O  - Psat      [Pa]             Saturated vapour pressure  
C I  1 T         [deg.C]          Air dry bulb temperature
Ch**********************************************************************

        IMPLICIT NONE
      REAL T,Kelvin
      Kelvin=T+273.15
      IF (T<0.0) THEN
C       .Saturation vapour pressure over ice [Pa] (-100 to 0 deg.C)
         Psat=EXP(-5.6745359E+03/Kelvin    
     &            -5.1523058E-01          
     &            -9.6778430E-03*Kelvin    
     &            +6.2215701E-07*Kelvin**2 
     &            +2.0747825E-09*Kelvin**3 
     &            -9.4840240E-13*Kelvin**4 
     &            +4.1635019    *LOG(Kelvin))
      ELSE
C       .Satutation vap. press. over liquid water [Pa] (0 to +200 deg.C)
         Psat=EXP(-5.8002206E+03/Kelvin    
     &            -5.5162560              
     &            -4.8640239E-02*Kelvin    
     &            +4.1764768E-05*Kelvin**2 
     &            -1.4452093E-08*Kelvin**3 
     &            +6.5459673    *LOG(Kelvin))
      ENDIF
C     Convert kPa to Pa 
      Psat=Psat*1000.0
      END

