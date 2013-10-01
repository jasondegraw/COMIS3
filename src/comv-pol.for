C+*********************************************************** comv-pol.f
Ch**********************************************************************
C@tno jcp 1996Apr25_16:03:22 added time to Pollutant for use in HistCalc
C@NBI PGS 2000Aug02 - "time" no longer used so removed from decralations
CC      SUBROUTINE Pollutant (time,DeltaT)
        SUBROUTINE Pollutant (DeltaT)

C***********************************************************************

C				 POLLUTANT
C
C      Module : VD 6.May 1992/3.Juni 1992
C
C	This version is a merged version from LBL and empa in jun1992.

C
C      Items to be improved or reviewed are marked with 'C!!' in the
C      first three coloumns.
C
C      Changes:
C@empa aw 1993mar09 save concentrations from the last calculation in Cold
C@empa aw 1993mar11 preset Cold with zeros
C@empa aw 1994oct31 Timesteps changed to 1 when DeltaT is 0 in order to have
C@                  one call to PolTrans for calculating the steady state
C@                  solution.
C@empa aw 1994
C@empa rs dec1994   pollutant concentration gradients in layered zones and
C		    pollutant transport through large openings: a new factor
C		    Ffactl(2,MaxL) is introduced to replace the massflow array
C		    fv2(2,MaxL). For LVO Ffactl is the integral of massflow
C		    times the relative "from-zone" concentration c(z)/c(0)
C		    over the opening, for other links Ffactl is fv2*c(z)/c(0).
C		    Ffactl is calculated with a new function FFACT which
C		    calls a new subroutine FMAPROFCAL for calculation of
C		    the massflow and height profile through a LVO.
C		    The relative zone concentration c(z)/c(0) is calculated
C		    with a new function PROFCALZ.
C
C@empa rs dec1994   a consistent application of the definition of pollutant
C		    concentration as mass of pollutant in a certain volume
C		    of polluted air per mass of dry and clean air in the same
C		    volume of polluted air leads to new factors for the
C		    pollutant mass transported through a link (Efactz(MaxZ),
C		    Efactext(MaxW),Efactext0) and for the total mass of
C                   pollutant in the zone volume(Dfactz(MaxZ)). These factors
C		    are updated after each internal timestep. They are
C		    calculated with functions EFACT and DFACT. The densities
C		    are not needed anymore.
C@NBI PGS 2000Oct31 - Eh? The above method is flawed : The definition should be
C@NBI                 the mass of a pollutant in a certain volume of polluted air
C@NBI                 per mass DRY POLLUTED air in the same volume.  This is due
C@NBI                 to three reasons:
C@NBI                 (1) Concentration can never exceed 100% (or 1 million ppm)
C@NBI                     but the above method lets this happen !
C@NBI                 (2) Many so-called "pollutants" that are simuated in COMIS
C@NBI                     are natural constituents of dry clean air (eg. CO2,O2).
C@NBI                     Air is 21% O2, so it is meaningless to calculate O2
C@NBI                     concentration as a fraction of the remaining 79%.
C@NBI                 (3) If a pollutant is called "H2O" then COMIS would be
C@NBI                     subtracting it twice from the mass of air in the room
C@NBI                     to find the mass of dry air!  This is an artefact of
C@NBI                     the way COMIS is programmed to handle humidity.
C@NBI               - The functions EFACT and DFACT have therefore been replaced
C@NBI                 by simple in-line code, and are thus redundant.
C@empa aw 2000dec12 We keep the original definition. Peter, see my mail from 12.12.2000
C@empa rs dec1994   zones with volume 0 are not taken into account for
C		    calculation of the timestep for the internal timeloop.
C@empa rs dec1994   b(MaxC,MaxZ) has been redimensioned to b(MaxZ)
C@empa rs dec1994   the diagonal elements a(i,i) are now calculated with the
C		    total mass of outflowing polluted air instead of the total
C		    mass of inflowing polluted air.
C		    The case a(i,i)=0 (possible for steady state calculation
C		    or zone volume 0) could not be solved. Now for the case
C		    b(i)=0 the solution is c(t+dt) = c(t). For b(i)>0 there is
C		    no solution.
C@empa rs dec1994   the reactivity rk is now defined by the pollutant mass
C		    balance:
C				dm(poll)/dt=-rk*m(poll)
C		    rk is defined as a zone independent property of each
C		    pollutant, e.g. a radioactive decay constant: rk(MaxC).
C		    In addition a zone- and pollutant specific reactivity
C		    rkz(MaxC,MaxZ) is defined to describe processes like
C		    adsorption. rkz can be explicit CIF-input or if there
C		    is an adsorption model rkz can be calculated and updated
C		    after each internal timestep.
C@empa rs dec1994   cm(j) is initialized with the concentrations from the
C		    preceding timestep, before GSSOLV is called.
C@NBI PGS 1999Aug10 - Had to change name of variable 'Interval' to 'DeltaT',
C@NBI                 to avoid confusion with the variable 'Interval'
C@NBI                 (i.e. schedule deltaT) in the INCLUDE file.
C
C
C      Pass Parameters:
C      IO  Name     Units   Description
C      I   DeltaT   [sec]   Time interval from schedules
C
C
C      Local parameters:
C
C
Ch**********************************************************************

        IMPLICIT NONE
	INCLUDE 'comv-inp.inc'
	INCLUDE 'comv-uni.inc'
C@NBI PGS 2000Oct31 - Functions DFACT & EFACT replaced by inline code,
C@NBI                 so had to INCLUDE 'comv-phy.inc' here
C@empa aw 2000dec12 We keep the original definition. Peter, see my mail from 12.12.2000
CC      INCLUDE 'comv-phy.inc'

C Pass parameter
C@NBI PGS 2000Aug02 - "time" no longer used so removed from decralations
CC      DOUBLE PRECISION Time
        INTEGER DeltaT

C@lbl bvs 1999Jun01 moved rk(maxc) to global area and added code in
C	comv-inp.inc so users can use the decay term for pollutants
C Local
C@empa rs 1994dec09 new definition of reactivity as pollutant specific
C		    decay constant, assuming exponential
C                   (e.g. radioactive) decay. Redimensioned rk:
C                   rk(maxc). Rkmax: maximum of all rk(i)
C                   In addition to rk(maxc) a pollutant- and zone-specific
C		    reactivity rkz(maxc,maxz) is introduced for modelling of
C		    effects like: adsorption, chemical decay depending on
C		    zone properties (e.g. temperature, humidity) or similar
C 		    mechanisms. As for rk(polnr) for rkz(polnr,zonenr) it is
C		    assumed that the total loss of pollutant in a zone per
C		    timestep is proportional to the product of rk or rkz and
C		    the actual mass m of pollutant in the zone (which is
C		    portional to the zone concentration of the pollutant):
C                   dm / dt = - m * ( rk(polnr) + rkz(polnr,zonenr) )
C		    If an adsorption model is introduced, rkz can be
C		    calculated from e.g. wallproperties instead of being
C		    explicit CIF-input. Taking into account the dependence of
C		    rkz from the history of the adsorbing system, rkz has to
C                   be updated after each internal timestep (only if not
C		    explicit CIF-input).
C		    rkmaxz: maximum of all rkz(i,j).
C@NBI PGS 2000Nov01 - Added dummy "a" and cigarette pollution variable "cigs"
C@empa aw 2000dec18 rkz moved to common block
CC        REAL rkmax,rkz(MaxC,MaxZ),rkmaxz,cigs,a
        REAL rkmax,rkmaxz,cigs,a
        REAL s(MaxC,MaxZ),cm(MaxZ)
        REAL matrix(MaxZ,MaxZ),vector(MaxZ)
        Real FFact,Efact,Tpz,Dfact

C factors for massflow of dry and clean air
C and total mass of dry and clean air in a zone
        REAL Efactz(Maxz),Efactext(Maxw),Efactext0,Dfactz(MaxZ)
C@NBI PGS 2000Oct31 - Functions DFACT & EFACT replaced by inline code,
C@NBI                 so array "actc0" no longer needed
C@empa aw 2000dec12 We keep the original definition. Peter, see my mail from 12.12.2000
        REAL actc0(MaxC)
        REAL Taufact

C@empa rs 1994dec15 factors Ffactl(1,linknr) and Ffactl(2,linknr)
C                   replace fv2(1,linknr) and fv2(2,linknr)
C		    in case of non homogenous zones
C		    (layers with concentration gradients)
        REAL Ffactl(2,MaxL)
C@tno jcp 1996Jun18_10:22:30 MaxConc
C@NBI PGS 2000Aug18 - This line should have been deleted when it was
C@NBI                 moved on 2000mar28 to routine POLOUT
CC      REAL MaxConc

C@NBI PGS 2000Nov01 - Added "IcIndex"
        INTEGER key,timesteps,i,ii,j,mband,errorflag,nit,n,m,IcIndex
        REAL eps,dt
C@tno jcp 1996Apr24_13:43:10 OccAct1 added
C@empa aw 2000mar21 OccNum1 added
        REAL zt(MaxZ), Tau , OccAct1,OccNum1
C@tno jcp 1996Apr24_13:43:22 iocc,inr added
        INTEGER pos,k,iocc,inr

C@tno jcp 1996Jun18_10:34:16 for error reports Str and Str2

C@NBI PGS 2000Aug02 - "lstr", "lstr2", "str", "str2" no longer used so
C@NBI                 removed from decralations
CC      INTEGER lstr,lstr2
C@NBI PGS 2000Aug18 - IPOLMAXCONC & IZonMAXCONC should have been deleted
C@NBI                 when moved on 2000mar28 to routine POLOUT
CC      integer ni, IPOLMAXCONC, IZonMAXCONC
        integer ni
CC      character*40 str,str2


CC        INTEGER NREL
CC        PARAMETER (NREL=MaxC*MaxZ)
C@tno jcp 1996Jan17_16:38:21 moved to Block Data
CC        DATA Cold /NREL*0/



C --------------------------------------------------------- executable
C@empa rs 1994dec15 calculation of factors Ffactl(1,linknr) and
C                   Ffactl(2,linknr) which replace fv2(1,linknr) and
C		    fv2(2,linknr) in case of non homogenous zones
C                   (layers with concentration gradients). For homoge-
C                   ous zones Ffactl(j,i) and fv2(j,i) are identical.
        DO 5 i=1,nl
          DO 6 j=1,2
            Ffactl(j,i)=Ffact(j,i,3)
6         CONTINUE
5       CONTINUE

        rkmax=0
        rkmaxz=0
	DO 10 j=1,nconc
C@lbl bvs 1999 jun01
C here input for rk(j) is required
CC	   rk(j)=0.0
           IF (rk(j).GT.rkmax) THEN
              rkmax=rk(j)
           ENDIF
	   DO 15 i=1,nz
C save concentrations from the last calculation
              Cold(j,i)=C(j,i)
C input for or calculation of rkz(j,i) is required here
C@empa aw 2000dec18 rkz is now input variable read from &-NET-ZP
CC              rkz(j,i)=0
              IF (rkz(j,i).GT.rkmaxz) THEN
                 rkmaxz=rkz(j,i)
              ENDIF
15	   CONTINUE
10	CONTINUE

        rkmax=rkmax+rkmaxz

C	find filter efficiency
	DO 20 i=1,nl


C	Find the right filter data for the links.
C	There are two pointer arrays.
C	1. y=pusrafc(x) x:= number of airflow component
C			y:= start position of x in LDat
C	2. y=pLiDat(x)	x:= number of the link
C			y:= start position of x in LDat

C	Get the position in "LDat" where the data for link #i starts.
	   pos=plildat(i)
	   k=0
C	Find the number of the airflow component for which the data
C	starts at position "pos" in "LDat".
	   do 11 ii=1,nusrafc
		if( pos.EQ.pusrafc(ii)) k=ii
11	   continue

C	Get the position of the last data for this airflow component in "Ldat".
C	If the current airflow component is the last one in the "LDat",
C	we can use the value of "mldat".
	   if ( k.LT.nusrafc) then
		pos=pusrafc(k+1)-1
	   else
		pos=mldat
	   endif

C	Write the filter data into the array.
	   DO 25 j=1,MaxC
	     filtL(j,i)=LDat(pos-MaxC+j)
25	   CONTINUE
20	CONTINUE


C Initialization of parameters for GSSOLV
	Eps=0.000001
C	nit=50
C@tno jcp 1996Mar04_12:33:58 temp set from 150 to 550 (to solve steady state)
        nit=550

C   Initialization of factors Efactz(zonenr),
C   Efactext(wpnr), Efactext0 and Dfactz(zonenr).
C   Efactz and Dfactz depend on the actual zone concentrations
C   and have to be updated after each internal timestep.
          DO 270 n=1,nz
C@NBI PGS 2000Oct31 - Functions DFACT & EFACT replaced by inline code. See top comment.
C@empa aw 2000dec12 We keep the original definition. Peter, see my mail from 12.12.2000
          DO 280 m=1,nconc
            actc0(m)=C(m,n)
280       CONTINUE
          Efactz(n)=Efact(Xhmz(n),actc0,crelmz(n))
CC            Efactz(n)=1.0/(1.0+Xhmz(n))
	    Tpz = PZ(n)+Pbz
          Dfactz(n)=Dfact(Xhmz(n),actc0,crelmz(n),Tpz,Tmz(n))
CC            Dfactz(n)=Tpz/Tmz(n)/GasConst/(1.0+Xhmz(n)*MMair/MMH2O)
270       CONTINUE

          DO 370 n=1,nwind
          DO 380 m=1,nconc
            actc0(m)=ExtConc(m,n)
380       CONTINUE
          Efactext(n)=Efact(Xhout,actc0,1.0)
CC            Efactext(n)=1.0/(1.0+Xhout)
370       CONTINUE

          EFactExt0=Efact(Xhout,Cout,1.0)
CC          EFactExt0=1.0/(1.0+Xhout)
C@NBI PGS 2000Oct31   (end patch)


C  If DeltaT = 0:   Stationary solution  ( dC/dt =0 )

        IF (DeltaT .GT. 0) THEN

C The schedule time interval is subdivided into several timesteps
C No of timesteps= DeltaT/dt

C Initialisation of dt
           dt= DeltaT

C Determination of dt --> dt << min (time constant of all zones)
C A value of dt= Tau_min/100 and dt >= 1sec is assumed

C Time constant of the zones

           DO 27 i=1,nz
C tau is not calculated for zones with volume 0
              IF (Vz(i).GT.0) THEN
                IF (FT(i).eq.0.0) THEN
                   Tau= 1.0E+10
                ELSE

C Calculation of Tau with a factor Taufact instead of
C RhoDrz(i). RhoDrz(i) is not consistent with the definition
C of pollutant concentration used in COMIS.
C Tau = Vz(i)* RhoDrz(i)/ABS(Ft(i))
                  Taufact=Dfactz(i)/Efactz(i)
                  Taufact=Taufact*Min(1.0,crelmz(i))
                  Tau = Vz(i)* Taufact/ABS(Ft(i))

                ENDIF
C tau taking into account reactivity
                Tau=Tau/(rkmax*Tau+1)
C@tno jcp 1996Apr19_16:11:15 I think Tau/10.0 is enough for me
CC                dt= MIN(dt,(Tau/100.0))
                dt= MIN(dt,(Tau/10.0))
              ENDIF

27         CONTINUE

           IF (dt.LT.1.0) THEN
C@lbl bvs 1998Dec23 Change "100 sec" to "10 sec" to reflect change above of Tau/10.0
C@NBI PGS 2000Oct31 - Join split phrase "Tau_zone < 10 sec"
              CALL ERROR2('Pollutant transport solver : Small timestep'
     &        ,'needed (Tau < 10 sec), but is limited to 1 sec',1)
              dt=1
           ENDIF

C round to next higher number of steps
           timesteps=DeltaT/dt
C Now calculate a dt that is an integral division of the interval and timesteps
C This will insure that our last timestep is exactly where it should be
	   dt = 1.0*DeltaT/timesteps

        ELSE
C Timesteps changed to 1 when DeltaT is 0 in order to have
C one call to PolTrans for calculating the steady state
C solution.
           timesteps =1
           dt=0
        ENDIF

C@tno jcp 1996Jun18_10:21:55 Keep track of the maximum concentration
C@NBI PGS 2000Aug18 - This line should have been deleted when it should
C@NBI                 have been moved on 2000mar28 to routine POLOUT
CC        maxConc=0
C Here starts the pollutant internal timeloop---------------------------
        DO 30 k=1,timesteps

C update of factors Efactz,Dfactz
          DO 70 n=1,nz
C@NBI PGS 2000Oct31 - Functions DFACT & EFACT replaced by inline code. See top comment.
C@empa aw 2000dec12 We keep the original definition. Peter, see my mail from 12.12.2000
            DO 80 m=1,nconc
              actc0(m)=C(m,n)
80          CONTINUE
            Efactz(n)=Efact(Xhmz(n),actc0,crelmz(n))
CC            Efactz(n)=1.0/(1.0+Xhmz(n))
	      Tpz = PZ(n)+Pbz
            Dfactz(n)=Dfact(Xhmz(n),actc0,crelmz(n),Tpz,Tmz(n))
CC            Dfactz(n)=Tpz/Tmz(n)/GasConst/(1.0+Xhmz(n)*MMair/MMH2O)
C@NBI PGS 2000Oct31   (end patch)

C update rkz(m,n) (if rkz(m,n) is not explicit CIF-input)
C@lbl bvs 1998Feb6 this is outside of the DO loop for m, hence m is illegal here!
CC                rkz(m,n)=rkz(m,n)

70        CONTINUE
C end update of factors Efactz

           DO 40 i=1,Nconc
             DO 45 j=1,nz
C  Sink characteristic:  Sink = 0 if C = 0

C I think it's wrong to set the global variable Sink(i,j) zero,
C because in the next timesteps the value for C(i,j) can increase
C again. I introduced source code in the subroutine gssolv to avoid
C concentrations below zero. We can't check that before, because of the
C influence of the flow from other zones.
C                IF ( C (i,j) .LE. 0.0 ) Sink(i,j)=0.0

C  Source strength S = Source - Sink
C  If schedules are defined:
C     Source strength S = Source * Source _factor - Sink * Sink_factor
C  This multiplication is done already in timestep, so source and
C  sink are already the actual values.
C
C@tno jcp 1996Apr25_13:31:50 delete these 2 lines
CCC  Source has to be checked: If source<0 : value is a pointer into array
CCC  OccSour, see routine IN20
C@tno jcp 1996Apr24_13:44:52 lets abandon the negative sources (as mentioned
C here)
C!! Preliminary: In this case source is set to 0

                IF (source(i,j).LT.0.0) THEN
                  source(i,j)=0.0
C@NBI PGS 2000Jul16 - Reading clarity
                  CALL ERROR('Occupant source calculation not available'
     &//' yet, term set to zero',1)
                ENDIF

                s(i,j)= source(i,j) - sink(i,j)
C@tno jcp 1996Apr24_16:38:21 for PolOut I want to have the Source generated by o
                OccSource(i,j)=0.0

C@tno jcp 1996Apr24_13:32:35 this is the best place to introduce occupant source
C i=ipol j=izone
                if (OccAsSource) then
C there are (some) occupants that produce one (or more) of the pollutants used
C here
                  do 44 iocc=1,MaxO
                    OccAct1=occact(iocc,j)
C@empa aw 2000mar21 OccNum1 added
                    OccNum1=OccNum(iocc,j)
CC                    if (OccAct1.gt.0) then
                    if ((OccAct1*OccNum1).gt.0) then
C occupant(s) iocc are in zone j
                      do 43 inr=1,MaxC
                       if (occPolNr(iocc,inr).eq.i) then
C@NBI PGS 2000Nov01 - Added cigarette pollution:
                        cigs=0.0
                        IF(OccSmo(iocc).GT.0.0)THEN
                         IF(ICindex(OccPolN(iocc,inr),'ODOUR').gt.0)then
C                         ! - 1 cigarette/hour is 1 olf.
C                         ! - The factor 1.0E+7 is an arbitrary one, to convert from olf to
C                         !   kg/s, and ensures a low kg/s rate, and is also used in subroutine "FilUnit"
                          cigs=OccSmo(iocc)/1.0E+7
                         ENDIF
                        ENDIF
                        a=(OccPol(Iocc,inr)*OccAct1 + cigs) * OccNum1
C@NBI PGS 2000Nov01   (end cigarette patch)
C this is the same source
CC                        s(i,j)=s(i,j)+OccPol(Iocc,inr)*OccAct1
C@NBI PGS 2000Nov01 - All occupant pullutants lumped in temporary variable "a"
CC                        s(i,j)=s(i,j)+OccPol(Iocc,inr)*OccAct1*OccNum1
                        s(i,j)=s(i,j)+a
C@tno jcp 1996Apr24_16:40:11 keep the occupant generated source for PolOut
CC                          OccSource(i,j)=OccSource(i,j)+
                        OccSource(i,j)=OccSource(i,j)+a
CC     &                    OccPol(Iocc,inr)*OccAct1
CC     &                    OccPol(Iocc,inr)*OccAct1*OccNum1
C@NBI PGS 2000Nov01   (end "a" patch)

       if (test.ge.1 .and. iecho.ge.7 ) then
       write(cof,*) 'occupant',iocc,' is in zone',
     & j,'with activity',OccAct1,'OccPol=',OccPol(Iocc,inr)
       write(cof,*) 'and produces pollutant',i,
     & OccPol(Iocc,inr)*OccAct1,s(i,j)
       write(cof,*) 'occupant source=',OccSource(i,j)

       end if
                       end if
43                    continue
                    end if
44                continue
                end if
C end if occupant as source

45           CONTINUE

C build the linear system

             CALL POLTRANS(dt,nz,nl,i,fromto,
     &            rk,rkz,s,FiltL,Vz,C,ExtConc,Cout,
C@tno jcp 1996Jul24_11:20:13 Poltrans: Fv deleted
CC     &            Fv,Fv2,matrix,vector,key,zt,lstat,
     &            Fv2,matrix,vector,key,zt,lstat,
     &            Efactz,Efactext,Efactext0,Dfactz,Ffactl,crelmz)

	     IF (key.eq.1) Call ERROR('Error in POLTRANS ',3)


C Calculation of the bandwidth of the matrix
	CALL BandWidth(matrix,nz,mband)

C for a(i,i)=0 there is a solution, if s(polnr,i)=0
C   -> s is now a passparameter from POLLUTANT

C initialization of cm with concentrations from last timestep

             DO 61 j=1,nz
	       cm(j)=C(i,j)
61	     CONTINUE


C@tno jcp 1996Mar05_08:54:22 added: call different solver for steady state
           if (dt.eq.0) then
CC             write(cof,*)'take SolveLS2'
CC@tno jcp 1997Nov11 new errorflag parameter to solvels2
CC             call solveLS2(Nz,matrix,vector,Cm)
             call solveLS2(Nz,matrix,vector,Cm,errorflag)
	     errorflag = 0
           else
             CALL gssolv (cm,errorflag,ni,matrix,vector,Eps,nz
     &  ,mband,nit,i,s)

C@tno jcp 1996Mar05_08:54:22 end/added
           end if
CC             write(crt,*) 'vector cm, n iterations=',ni
CC             call wrtarr(cm,nz,maxz)


	if (errorflag .eq. 1) CALL error(
     &		'Poltrans solver : singular Jacobian',3)
	if (errorflag .eq. 2) CALL error(
     &		'Poltrans solver reached max. iterations',3)


C Update concentrations
             DO 60 j=1,nz

               C(i,j)=cm(j)
C@NBI PGS 2000Aug18 - This code should have been deleted when it was
C@NBI                 moved on 2000mar28 to routine POLOUT
CC               if (cm(j).gt.MaxConc) then
CC                 MaxConc=Cm(j)
CC                 ipolMaxConc=i
CC                 izonMaxConc=j
CC               end if
C@NBI PGS 2000Aug18   (end)
60	     CONTINUE
40         CONTINUE


C@tno jcp 1996Jun18_10:24:19 make error warnings if concentrations are too high
C@empa aw 2000mar28 Error message is now in POLOUT
CC           if (MaxConc.gt.0.5) then
CC             call intdis(ipolMaxconc,str,lstr)
CC             call intdis(izonMaxconc,str2,lstr2)
CC             Call ERROR2('Concentrations > 0.5 kg/kg in zone '//
CC     &       str2(1:lstr2)//' for pollutant '//str(1:Lstr)//' .',
CC     &       'Increase ventilation or decrease source strengths.',3)
CC           end if
C@tno jcp 1996Jun18_10:24:19 end if concentrations too high

C@tno jcp 1996Apr25_15:12:25 call HistCalc every timestep??!!! Maybe split
C@empa aw 2000apr26 I leave it, as it is. It works perfect !
C HistCalc for concentrations and other things
        if (HistConc.and.HistActive) then
C time is in LOTUS format (days from 1900 with fractional the 86400 sec a day)
C@empa aw 2000apr18 Call HistCalc with dt
CC          CALL HistCalc(time+DBLE(k*dt/86400.D0))
          CALL HistCalc(dt)
        end if

C End timeloop

30      CONTINUE

C@tno jcp 1996Jun18_10:24:19 make error warnings if concentrations are too high
C@empa aw 2000mar28 Error message is now in POLOUT
CC         if (MaxConc.gt.0.025) then
CC          call intdis(ipolMaxconc,str,lstr)
CC          call intdis(izonMaxconc,str2,lstr2)
CC          if (MaxConc.gt.0.5) then
CC            Call ERROR2('Concentrations > 0.5 kg/kg in zone '//
CC     &      str2(1:lstr2)//' for pollutant '//str(1:Lstr)//' .',
CC     &       'Increase ventilation or decrease source strengths.',3)
CC          else
CC
CC            Call ERROR('Concentrations > 0.025 kg/kg in zone '//
CC     &      str2(1:lstr2)//' for pollutant '//str(1:Lstr),1)
CC          end if
CC        end if
C@tno jcp 1996Jun18_10:24:19 end if concentrations too high

C@NBI PGS 2000Aug18 - Maybe it's time pension this misleading line.
C@NBI                 Isn't it best just to say nothing when there are no errors?
CC        write(CRT,*)' NO poltrans    ERRORS REPORTED'
	RETURN
	END

C initialization of named COMMON are now in BLOCKDATA

Ch**********************************************************************
        BLOCK DATA IniCold
Ch**********************************************************************
C@empa aw 1998jun25 take just the needed COMMON block instead of includefile
C@empa aw 2000dec18 rkz(MaxC,MaxZ) first order decay per pollutant and zone added
C@empa aw 2000dec18 Osource(MaxSP) deleted
CC	INCLUDE 'comv-inp.inc'
        INCLUDE 'comv-par.inc'
        REAL C(MaxC,-MaxW:MaxZ+MaxWC),Cold(MaxC,MaxZ),
     &  C1(MaxC),Source(MaxC,MaxZ),OccSource(MaxC,MaxZ),
     &  Sink(MaxC,MaxZ)
        REAL OSink(MaxC,MaxZ),OrSource(MaxC,MaxZ)
        REAL rkz(MaxC,MaxZ)
        INTEGER Nzp,Nconc,pSource
        COMMON/NETZP1/C,Cold,C1,Source,Sink,OccSource,
     &  OSink,OrSource,Nzp,Nconc,pSource,rkz
        INTEGER NREL
        PARAMETER (NREL=MaxC*MaxZ)
        DATA Cold /NREL*0/
        END




Ch**********************************************************************

      SUBROUTINE POLTRANS(dt,nz,nl,polnr,fromto,
     &     rk,rkz,s,FiltL,Vz,Cinit,ExtConc,Cout,
C@tno jcp 1996Jul24_11:20:44 Poltrans:Fv Deleted
     &     Fv2,a,b,key,zt,lstat,
     &     Efactz,Efactext,Efactext0,Dfactz,Ffactl,crelmz)


C***********************************************************************
C
C                          POLTRANS
C
C      This routine builds the linear system corresponding to a multizone
C      pollutant transport in a nzone building using a purely implicit
C      scheme for time integration.
C
C      Outside concentrations:
C          For external wind pressures nodes, the values in array ExtConc are
C          considered and for fixed pressure nodes the value in Cout
C
C      Module........, task IV, fa051589
C      Changes: 071389(comis standards)
C@lbl bvs 1994feb15 made dt REAL
C Rhoz and Nw are never used, to avoid the warnings I
C assigned them to them selves but check if this is OK
C RhoZ is never used is this correct?
C@lbl whb, 1991June17: 2 changes (see below)
C@lbl akk 28oct91: added parameter polnr
C@empa vd 1992feb20: Array  s(maxc,maxz) leaving unchanged,
C                    Array  b(maxc,maxz) introduced as the second element
C@empa vd 1992jun01: Complete revised version of this routine, individual
C                    lines are not marked:
C                    Arrays redimensioned, Introduction of Fv2 , Fromto adapted
C                    using Lstat
C
C      Limits : 1 concentration/zone
C
C      Pass Parameters:
C      IO   Name   Unit  Description
C-----------------------------------------------------------------------
C      I    MaxC    -       Max number of concentrations
C      I    MaxZ                of zones
C      I    MaxL                of links
C      I    dt      s       Time step (if dt=0, steady state)
C      I    nz      nu      Number of zones (max=maxz)
C      I    nl      nu      Number of link (max=maxl)
C      I    polnr   nu      Number of polutant to process (max=maxc)
C      I    fromto  nu      Link conectivity information (2,nl)
C      I    lstat           Link status information
C      I    rk      1/s     Decay constant (maxc)
C      I    s       kg/s    Source term (maxc,nz)
C      I    FiltL   nu      Filter efficiency/link (maxc,nl)
C      I    vz      m**3    Volume of each zone (vector dimension nz)
C      I    cinit   kg/kg   Initial concentration (maxc,nz)
C      I    ExtConc kg/kg   Outside Concentration at external nodes (maxc,nw)
C      I    cout    kg/kg    Outside concentration
C      I    Fv      kg/s    Mass flow of air/link (nl)
C      I    Fv2     kg/s    Two way flows from large openings
C
C      O    a       nu      Transfer Matrix (dimension nzone,nzone)
C
C      O    b       nu      Second Member  (vector dimension nzone)
C      O    key     nu      Control Parameter (key=1-> error in data)
C
C     Local zt      nu      zt=vz/dt  Local parameter, in pass parameter list
C                                     only for reasons of dimensioning
C
Ch**********************************************************************
        IMPLICIT NONE
	include 'comv-par.inc'
C Pass parameters:
       REAL rk(MaxC),rkz(MaxC,MaxZ)
C@NBI PGS 2001May07 - Bugfix : Array's 2nd parameter dimension should be "MaxL"
CC     REAL FiltL(MaxC,MaxZ), s(MaxC,MaxZ),Vz(MaxZ)
       REAL FiltL(MaxC,MaxL), s(MaxC,MaxZ),Vz(MaxZ)
       REAL Cinit(MaxC,-MaxW:MaxZ+MaxWC),ExtConc(MaxC,maxw),Cout(MaxC)
       REAL Fv2(2,MaxL)
       REAL a(maxZ,maxZ),b(maxZ)
       REAL flow
       REAL crelmz(Maxz)

C new factors for massflow of dry and clean air
C and total mass of dry and clean air in a zone
        REAL Efactz(Maxz),Efactext(Maxw),Efactext0,Dfactz(MaxZ)

C factors Ffactl(1,linknr) and Ffactl(2,linknr)
C replace fv2(1,linknr) and fv2(2,linknr)
C in case of non homogenous zones
C (layers with concentration gradients)
        REAL Ffactl(2,MaxL)

       INTEGER i,j,l
       INTEGER nz,nl,polnr, FromTo(2,MaxL),Lstat(MaxL),key
       REAL dt
C Local
       REAL zt(MaxZ)

       key=0

C initialize the whole(!) matrix (otherwise wrong results)
       DO 11 i=1,nz
         DO 12 j=1,nz
            a(i,j)=0.
12       CONTINUE
11     CONTINUE

       DO 10 i=1,nz

         IF(dt.eq.0.0) THEN
C dt=0 (steady state)
            zt(i)=0.0
         ELSE
            zt(i)=vz(i)/dt
         ENDIF


C  Diagonal term (storage + reactivity )
C  Source term (second member)

         IF (polnr.EQ.1) THEN
           b(i)=s(polnr,i)+zt(i)*cinit(polnr,i)*Dfactz(i)*crelmz(i)
           a(i,i)=Dfactz(i)*crelmz(i)*(zt(i)+Vz(i)*
     &            (rk(polnr)+rkz(polnr,i)))
         ELSE
           b(i)=s(polnr,i)+zt(i)*cinit(polnr,i)*Dfactz(i)
           a(i,i)=Dfactz(i)*(zt(i)+Vz(i)*
     &            (rk(polnr)+rkz(polnr,i)))
         ENDIF

10     CONTINUE

C  Exchanges with other zones and outside

       DO 100 l=1,nl

C We are using for all airflow components the vector Fv2.
C It is assumed that Fv2(1,l) = flow FROM -> TO and Fv2(2,l) = To ->FROM
C  and both Fv2(1,l) and Fv2(2,l) are GE 0.0

            DO 110 i=1,2
C factors Ffactl(1,linknr) and Ffactl(2,linknr)
C replace fv2(1,linknr) and fv2(2,linknr)
C in case of non homogenous zones
C (layers with concentration gradients)
C		flow=fv2(i,l)
                IF (polnr.EQ.1) THEN
                  flow=Ffactl(i,l)
                ELSE
                  flow=fv2(i,l)
                ENDIF
       	        if( i.EQ.2 ) flow=-flow

            CALL POLTR1 (polnr,flow,FromTo(1,l),
     &            Lstat(l),FiltL(polnr,l),
     &            ExtConc,Cout,a,b,key,
     &            Efactz,Efactext,Efactext0)


110         CONTINUE

100    CONTINUE
       RETURN
       END

Ch**********************************************************************
       SUBROUTINE POLTR1 (polnr,Flowfact,FromTo,LSt,
     &                    Filt,ExtConc,Cout,a,b,key,
     &                    Efactz,Efactext,Efactext0)
C***********************************************************************
C
C      SUB- subroutine of POLTRANS V.Dorer 1.June 1992
C      for routine header text see POLTRANS
C
C      Pass Parameters:
C      IO  Name     Units   Description
C       O  Key      [-]     Returned error code (not presently used)
C
C Changes:
C@lbl  "New structure" of this subroutine, so that it is more
C	clear what the subroutine is doing.
C
Ch**********************************************************************
        IMPLICIT NONE
	include 'comv-par.inc'

       REAL flowfact,Filt
       REAL ExtConc(MaxC,maxw)
       REAL Cout(maxC)
       REAL a(MaxZ,MaxZ), b(maxz)
       INTEGER polnr,FromTo(2),LSt,key

       REAL Efactz(Maxz),Efactext(Maxw),Efactext0

C Local
       REAL Cext
       INTEGER I,J,Isign,IRho,Iw



C positive flow *******************************************
	if( flowfact.GE.0 ) then
		i=FromTo(2)
		iRho=1
		isign=1

C From zone to zone
		if( LSt.EQ.0 ) then
			j=FromTo(1)
			GO TO 400

C From cp to zone
C no contribution to a or b from links with status 4,5,7,8
		elseif (LSt.EQ.1) then
			iw=FromTo(iRho)
			Cext=ExtConc(polnr,iw)
C new factors for massflow of dry and clean air
			b(i)=b(i)+
     &                       isign*flowfact*EfactExt(iw)
     &     		     *(1-Filt)*Cext
C no contribution from this case to any diagonal element of a
                        GOTO 100

C From spec to zone
		elseif (LSt.EQ.2) then
			Cext=Cout(polnr)
			b(i)=b(i)+
     &                       isign*flowfact*EfactExt0
     &     		     *(1-Filt)*Cext
C no contribution from this case to any diagonal element of a
                        GOTO 100

C the cases 3 and 6 contribute to a(j,j)
C From zone to cp or from zone to spec
		elseif (LSt.EQ.3 .OR. LSt.EQ.6) then
                        j=FromTo(1)
                        GOTO 500

		else
			GOTO 100
		endif
	else
C negative flow ***************************************
		i=FromTo(1)
		iRho=2
		isign=-1

C From zone to zone
		if ( LSt.EQ.0 ) then
			j=FromTo(2)
			GO TO 400

C From cp to zone
C no contribution to a or b from links with status 4,5,7,8
		elseif ( LSt.EQ.3 ) then
			iw=FromTo(iRho)
			Cext=ExtConc(polnr,iw)
			b(i)=b(i)+
     &                       isign*Flowfact*EfactExt(iw)
     &		             *(1-Filt)*Cext
                        GOTO 100

C From spec to zone
		elseif ( LSt.EQ.6 ) then
			Cext=Cout(polnr)
			b(i)=b(i)+
     &                       isign*Flowfact*EfactExt0
     &		             *(1-Filt)*Cext
                        GOTO 100

C the cases 1 and 2 contribute to a(j,j)
C From cp to zone or from spec to zone
		elseif (LSt.EQ.1 .OR. LSt.EQ.2) then
                        j=FromTo(2)
                        GOTO 500

		else
			GOTO 100
		endif
	endif
C *******************************************


C@empa rs 1994dec12 The non diagonal term a(k,l) represents the mass of
C		    pollutant flowing from zone l to zone k. The diagonal
C		    term a(k,k) represents the mass of pollutant flowing out
C                   of zone k. In this routine the zone from which the
C		    polluted air is flowing out is always indexed with j.
C		    The zone which the polluted air is flowing into is always
C		    indexed with i. So we have contributions to a(i,j) and
C  	 	    a(j,j).

C  Non diagonal term (transport + filter effect from other zones)

400	CONTINUE
	a(i,j)=a(i,j)-isign*Flowfact*Efactz(j)
     &         *(1.0 -Filt)

C  Diagonal term (transport effect from other zones)

500      CONTINUE

         a(j,j)=a(j,j)+isign*Flowfact*Efactz(j)
         GOTO 100

100      CONTINUE
         RETURN
         END





Ch**********************************************************************
C
C for a(i,i)=0 there is a solution, if s(actpolnr,i)=0
C  -> s is now a passparameter from POLLUTANT
      SUBROUTINE GSSOLV (xnp1,ier,ni,a,b,Eps,n,m,nit,actpolnr,s)
C                        |    |   |  | | |   | | |   |        |
C                        |    |   |  | | |   | | |   |        reactivity
C                        |    |   |  | | |   | | |   pollutant sequence number
C                        |    |   |  | | |   | | maximum number of iterations
C                        |    |   |  | | |   | band width
C                        |    |   |  | | |   number of equations
C                        |    |   |  | | tolerance (0.000001)
C                        |    |   |  | vector
C                        |    |   |  coefficient matrix
C                        |    |   iteration counter
C                        |    error flag
C                        concentration

C***********************************************************************
C 	Written by lbl rw 1992april21
C
C
C      This subroutine enables to solve a system of linear equations using
C      Gauss-Seidel iterative procedure. THIS IS NOT GENERAL SOLUTION OF A
C	LINEAR SYSTEM OF EQUATIONS.
C
C	Be careful!!! When xnp1(i) is going below zero, the b term in this
C	row will change to a value, so that xnp1(i) is zero. This was
C	introduced in order to prevent negative concentrations!!!
C
C	I'm taking care about the bandwith of the matrix this means, that
C	I'm only calculating with matrix elements near the diagonal (depending
C	on the with of the band of the matrix).
C
C      OUTPUT	 ...........>	  {xnp1} solution vector (dimension n)
C				  ier error code ier = 0 no error ,
C						 ier = 1 singular jacobian
C						 ier = 2 maximum number
C							 of iterations reached
C				  ni number of iterations
C
C      INPUT	 ...........>	  [a] Square matrix ( dimension n x n)
C				  {b} Second member   ( dimension n)
C				  Eps Desired precision
C				  n   Number of equation
C				  m   Half width of the band
C				  nit Maximum number of iterations
C				  actpolnr Number of polutant to process
C                                 s   reactivity
C
C      INTERNAL PARAMETER ...>	  {xn} Work vector ( dimension n)
C@NBI PGS 2003May13 - detabbed,indented & tidied up code. No syntax change
Ch**********************************************************************
        IMPLICIT NONE
C
C       the concentrations are needed for a(i,i)=s(actpolnr,i)=0
C       ->xnp1(i)=C(actpolnr,i)
C       To avoid multiple declarations polnr is changed
C       locally to actpolnr
C       include 'comv-uni.inc'
        include 'comv-inp.inc'

C       for a(i,i)=0 there is a solution, if s(actpolnr,i)=0
C       -> s is now a passparameter from POLLUTANT
        REAL s(MaxC,MaxZ)
        dimension a(maxz,maxz),b(maxz),xnp1(maxz)
        REAL XN(maxz)
        INTEGER actpolnr
        REAL Eps
        REAL asn,p
C@tno jcp 1996Mar04_13:45:45 added simple concentration variables
        REAL xni,xn1
        INTEGER ier,ni,n,m,nit,I,J,ip,k,counter
        REAL xnP1,A,B
C-----

        ier=0
        ni=0
        p=Eps+1

C-------------------------------------------------------
C       check of the jacobian and initialization of {xn}
C-------------------------------------------------------

        DO 20 i=1,n
          xn(i)=b(i)
C         case a(i,i)=0, s(actpolnr,i)=0 -> no massflow, no reactivity
C         no effective source -> C(actpolnr,i)=const.
C         case a(i,i)=0, s(actpolnr,i)><0 -> no solution
          IF (a(i,i).eq.0) THEN
            IF (s(actpolnr,i).NE.0) THEN
              ier=1
            ELSE
              xnp1(i)=C(actpolnr,i)
            ENDIF
          ENDIF
20      CONTINUE
        IF(ier.eq.1)GO TO 999

C------------------
C       calculation
C------------------

10      CONTINUE
        ip=0
        DO 61 i=1,n
C         case a(i,i)=0, s(actpolnr,i)=0: xnp1(i) is already determined
C         -> these zones must not be evaluated in this loop.
C         A zero division .../a(i,i) remains excluded.
          IF (a(i,i).NE.0) THEN
            asn=0.0
            k=i-m
            if( k.LT.1 ) k=1
            DO 62 j=k,i-1
              asn=asn+a(i,j)*xnp1(j)
CC            write(cof,*) 'i=',i,'j=',j
62          continue
            k=i+m
            if( k.GT.n ) k=n
            DO 63 j=i+1,k
              asn=asn+a(i,j)*xnp1(j)
CC            write(cof,*) 'i=',i,'j=',j
63          continue
C           If the b term is less than asn, we are getting negative concentrations.
C           To avoid that, b(i) is set equal asn. Following xnp1(i) is determined
C           to zero.
C@tno jcp 1996Mar04_13:52:12 I want to mention this temp
            if( (b(i)-asn).LT.0.) then
              b(i)=asn
CC            write(cof,*) 'b(',i,')= asn=',asn
            end if
            xnp1(i)=( b(i)-asn) /a(i,i)
CC          write(cof,*)'i=',i,'asn=',asn
          ENDIF
61      continue
C@tno jcp 1996Mar04_13:56:59 temp
CC      write(cof,*) 'x1=',xnp1(1),' x2=',xnp1(2)

C-----------------------
C       convergence test
C-----------------------

C       I'm only calculating the error for concentrations greater than 1e-30,
C       in order to avoid an error by determine (xnp1(i)-xn(i))/xnp1(i) .
C       I think this is o.k., because the difference between xnp1 and xn
C       is small enough ( the number of timesteps depends on the tau value).
C       If the concentrations for each room is less than 1e-30, the solution is
C       also found. Therefor I introduced the variable counter.
C       It is still possible, that a underflow warning could occur. But this is
C       actual not an error. It's only a warning and it has no influence during
C       the calculation.

        p=0.
        counter=0
        ip=0
        DO 140 i=1,n
C@tno jcp 1996Mar04_13:40:59 test on largest, introduce simple xni xn1
          xni=xn(i)
          xn1=xnp1(i)
CC        if( xnp1(i).GT. 1e-30) then
CC        p=ABS((xnp1(i)-xn(i))/xnp1(i))
          if (xn1.ge.xni) then
            if( xn1.GT. 1e-30) then
              p=ABS((xn1-xni)/xn1)
            else
              counter=counter+1
            endif
C@tno jcp 1996Mar04_13:45:02 added
          else
            if( xni.GT. 1e-30) then
              p=ABS((xn1-xni)/xni)
            else
              counter=counter+1
            endif
          end if
C@tno jcp 1996Mar04 end/added
          IF(p.GT.Eps) ip=1
140     CONTINUE

        if( counter.EQ.n) GOTO 999
        ni=ni+1

C-----------------
C       stop tests
C-----------------

C       Is the exactness ok?
        IF(ip.NE.1)GO TO 999

C       Too many interrations?
        IF(ni.GE.nit)GO TO 170

        DO 160 i=1,n
          xn(i)=xnp1(i)
160     continue
        GOTO 10
170     ier=2
999     CONTINUE
        RETURN
      END


Ch**********************************************************************
C
	SUBROUTINE BandWidth(a,n,band)
C***********************************************************************
C	Written by rw 1992april21
C
C	Purpose: This subroutine calculates the width of the band
C		 of the matrix a.
C
C	Example: 1 0 0 0 0
C		 0 1 0 0 0
C		 0 0 2 0 1
C		 1 0 0 3 0
C		 0 0 0 0 3
C
C
C		 band = 3
C
C
Ch**********************************************************************
        IMPLICIT NONE
	include 'comv-par.inc'

        REAL    a(maxz,maxz)
        INTEGER n,band

        INTEGER i,k,flg

	flg=0

	do 20 i=n,1,-1
		do 10 k=1, n-i+1
			if( a(i+k-1,k).NE.0.0 ) flg=1
			if( a(k,i+k-1).NE.0.0 ) flg=1
10		continue
	if ( flg.EQ. 1 ) then
		band= i-1
		GOTO 30
	endif
20	continue
30	continue

	return
	end


Ch**********************************************************************
      REAL FUNCTION Efact(actxh,actc0,actcrelm)
C@NBI PGS 2000Oct31 - Bugfix : The method seems flawed - The definition of
C@NBI                 concentration should be the mass of a pollutant in a
C@NBI                 certain volume of polluted air per mass DRY POLLUTED air
C@NBI                 in the same volume.  See comments at top of thie module.
C@NBI               - The functions EFACT and DFACT have therefore been replaced
C@NBI                 by in-line code, and are thus redundant.
C@empa aw 2000dec12 We keep the original definition. Peter, see my mail from 12.12.2000
C***********************************************************************
C     EMPA rs 1994dec12
C
C Purpose: This function calculates the transformation factor
C          of total massflow to massflow of dry and clean air
C
C Pass Parameters:
C  IO Name       Unit  Description
C  I  actxh     kg/kg  Zone humidity
C  I  actc0     kg/kg  Zone concentrations at reference height
C  I  actcrelm      -  Mean value of concentration profile of first
C                      pollutant relative to concentration at
C                      reference height
Ch**********************************************************************

      IMPLICIT NONE
      INCLUDE 'comv-inp.inc'
C     Passed arguments:
      REAL actxh,actc0(MaxC),actcrelm
C     Local variables:
      INTEGER j
      REAL denom
C-----
      denom=1+actxh+actc0(1)*actcrelm
      IF (nconc.GT.1) THEN
        DO 100 j=2,nconc
          denom=denom+actc0(j)
100     CONTINUE
      ENDIF
      Efact=1/denom
      END


Ch**********************************************************************
      REAL FUNCTION Dfact(actxh,actc0,actcrelm,actpressz,acttm)
C@NBI PGS 2000Oct31 - Bugfix : The method seems flawed - The definition of
C@NBI                 concentration should be the mass of a pollutant in a
C@NBI                 certain volume of polluted air per mass DRY POLLUTED air
C@NBI                 in the same volume.  See comments at top of thie module.
C@NBI               - The functions EFACT and DFACT have therefore been replaced
C@NBI                 by in-line code, and are thus redundant.
C***********************************************************************
C     EMPA rs 1994dec12
C
C Purpose: This function calculates the factor which gives,
C          multiplied with the zone volume, the total mass
C          of dry and clean air in a zone. For non homogenous
C          pollutant distribution in the zone, an additional factor
C          has to be taken into account, representing the mean value of
C          the relative concentration profile in the zone (approximation).
C
C Pass Parameters:
C  I  actxh     kg/kg  Zone humidity
C  I  actc0     kg/kg  Zone concentrations at reference height
C  I  actcrelm      -  Mean value of concentration profile of first
C                      pollutant relative to concentration at
C                      reference height
C  I  actpressz    Pa  Zone pressure at reference height
C  I  acttm         K  Mean temperature in zone
Ch**********************************************************************

      IMPLICIT NONE
      INCLUDE 'comv-inp.inc'
      INCLUDE 'comv-phy.inc'
C     Passed arguments:
      REAL actxh,actc0(MaxC),actcrelm,actpressz,acttm
C     Local Parameters:
      INTEGER j
      REAL denom
C-----
      denom=1+actxh*MMair/MMH2O+
     &        actc0(1)*actcrelm*MMair/MM(1)
      IF (nconc.GT.1) THEN
        DO 100 j=2,nconc
          denom=denom+actc0(j)*MMair/MM(j)
100     CONTINUE
      ENDIF
      Dfact=actpressz/acttm/GasConst/denom
      RETURN
      END


Ch**********************************************************************
C
        REAL FUNCTION Ffact(FromOrTo,actlnr,ProfType)
C
C***********************************************************************
C       EMPA rs 1994dec15
C
C	Purpose: This function calculates the product of massflow and
C		 temperature (ProfType 1) or humidity (ProfType 2) or
C		 relative pollutant concentration (ProfType 3).
C		 LVO: For each level of the pressure difference profile
C		      the massflow is calculated and multiplied with the
C		      temp./hum./rel.conc. at this level in the zone from
C		      which the air is flowing out. Ffact is the positive sum
C		      of these products (sum over positive flow direction for
C		      FromOrTo=1, negative direction for FromOrTo=2).
C		 Other link types: Ffact is the product of massflow and
C  		      temp./hum./rel.conc. at link height in the zone
C 	 	      from which the air is flowing out.
C		 (relative concentration at height z means
C		 conc.(z)/conc.(ref.hght.).)
C
C       Pass Parameters:
C
C       IO   Name        Unit 	Description
C
C    	I   FromOrTo      -	1 for flow direction FROM-zone -> TO-zone
C				2 for flow direction TO-zone -> FROM-zone
C	I   actlnr	  -	actual link number
C       I   proftype      - 	profile type. 1:temp., 2:hum., 3:poll.conc.
C
C       Local parameters:
C
C   	    actFromz	  -	FROM-zone for FomOrTo=1
C				TO-zone for FromOrTo=2
C	    actzlink	m	actual link height
C	    actdpref    Pa	actual reference pressure difference
C	    fmaprof	kg/s	massflow profile for LVO
C	    hghtprof    m	profile of evaluation heights for massflow
C				profile. hghtprof(1,i): rel. to zref(FROM)
C					 hghtprof(2,i): rel. to zref(TO)
Ch**********************************************************************

        IMPLICIT NONE
	INCLUDE 'comv-inp.inc'
	INCLUDE 'comv-phy.inc'

C	Pass Parameters:
       INTEGER FromOrTo,actlnr,ProfType

C	Local Parameters:
       INTEGER actFromz,i
       REAL actzlink,actdpref,ProfCalZ
       REAL fmaprof(NrInt+2),hghtprof(2,NrInt+2),Ffactsum

       actFromz=FromTo(FromOrTo,actlnr)
       IF (ProfType.EQ.3) THEN
         Ffact=fv2(FromOrTo,actlnr)
       ELSEIF (ProfType.EQ.2) THEN
         Ffact=fv2(FromOrTo,actlnr)*Xhz(actfromz)
       ELSEIF (ProfType.EQ.1) THEN
         Ffact=fv2(FromOrTo,actlnr)*(Tz(actfromz)+Tzero)
       ENDIF

       IF (Lstat(actlnr).EQ.0) THEN
C      zone -> zone

         IF (LayPtr(1,actFromz).NE.0)  THEN
C        actual from zone non homogenous

           IF (Ldat(pLiLdat(actlnr)).EQ.9) THEN
C          LVO

             Ffact=0
             Ffactsum=0
             actdpref=pz(FromTo(1,actlnr))-pz(FromTo(2,actlnr))+
     &                DpJ(actlnr)
             CALL FMAPROFCAL(fmaprof,hghtprof,actlnr,actdpref)
             DO 200 i=1,NrInt+2
               Ffactsum=fmaprof(i)*profcalz
     &                  (actFromz,hghtprof(FromOrTo,i),ProfType)
               IF (FromOrTo.EQ.1 .AND. Ffactsum.GT.0) THEN
                 Ffact=Ffact+Ffactsum
               ENDIF
               IF (FromOrTo.EQ.2 .AND. Ffactsum.LT.0) THEN
                 Ffact=Ffact-Ffactsum
               ENDIF
200          CONTINUE

C          end LVO
           ELSE
C          non LVO

             actzlink=Zl(FromOrTo,actlnr)
             Ffact=fv2(FromOrTo,actlnr)
     &             *profcalz(actFromz,actzlink,ProfType)

C          end non LVO
           ENDIF

C        end actual from zone non homogenous
         ENDIF

C      end zone -> zone
       ENDIF


       IF (Lstat(actlnr).EQ.1 .OR. Lstat(actlnr).EQ.2) THEN
C      cp -> zone or spec -> zone

         IF (FromOrTo.EQ.2) THEN
C        flow to(=zone) -> from

           IF (LayPtr(1,actFromz).NE.0)  THEN
C          non homogenous zone

             IF (Ldat(pLiLdat(actlnr)).EQ.9) THEN
C            LVO

               Ffact=0
               Ffactsum=0
               actdpref=-pz(FromTo(2,actlnr))+DpJ(actlnr)
               CALL FMAPROFCAL(fmaprof,hghtprof,actlnr,actdpref)
               DO 210 i=1,NrInt+2
                 Ffactsum=fmaprof(i)*profcalz
     &                    (actFromz,hghtprof(FromOrTo,i),ProfType)
                 IF (Ffactsum.LT.0) THEN
                   Ffact=Ffact-Ffactsum
                 ENDIF
210            CONTINUE

C            end LVO
             ELSE
C            non LVO

               actzlink=Zl(FromOrTo,actlnr)
               Ffact=fv2(FromOrTo,actlnr)
     &               *profcalz(actFromz,actzlink,ProfType)

C            end non LVO
             ENDIF

C          end non homogenous zone
           ENDIF

C        end flow to(=zone) -> from
         ENDIF

C      end cp -> zone or spec -> zone
       ENDIF


       IF (Lstat(actlnr).EQ.3 .OR. Lstat(actlnr).EQ.6) THEN
C      zone -> cp or zone -> spec

         IF (FromOrTo.EQ.1) THEN
C        flow from(=zone) -> to

           IF (LayPtr(1,actFromz).NE.0)  THEN
C          non homogenous zone

             IF (Ldat(pLiLdat(actlnr)).EQ.9) THEN
C            LVO

               Ffact=0
               Ffactsum=0
               actdpref=pz(FromTo(1,actlnr))+DpJ(actlnr)
               CALL FMAPROFCAL(fmaprof,hghtprof,actlnr,actdpref)
               DO 220 i=1,NrInt+2
                 Ffactsum=fmaprof(i)*profcalz
     &                    (actFromz,hghtprof(FromOrTo,i),ProfType)
                 IF (Ffactsum.GT.0) THEN
                   Ffact=Ffact+Ffactsum
                 ENDIF
220            CONTINUE

C            end LVO
             ELSE
C            non LVO

               actzlink=Zl(FromOrTo,actlnr)
               Ffact=fv2(FromOrTo,actlnr)
     &               *profcalz(actFromz,actzlink,ProfType)

C            end non LVO
             ENDIF

C          end non homogenous zone
           ENDIF

C        end flow from(=zone) -> to
         ENDIF

C      end zone -> cp or zone -> spec
       ENDIF

       RETURN
       END




Ch**********************************************************************
C
        REAL FUNCTION PROFCALZ(actzonenr,acthght,proftype)
C
C***********************************************************************
C       EMPA rs 1994dec15
C
C	Purpose: This function calculates temperature (proftype=1) or
C		 humidity (proftype=2) or pollutant concentration (proftype=3)
C		 in zone 'actzonenr' at height 'acthgth'. The required data
C		 are stored in the arrays LayDat and LayPrec.
C
C       Pass Parameters:
C
C       IO   Name        Unit 	Description
C
C    	I    actzonenr	   -	actual zone number
C	I    acthght	 m	actual height for evaluation of temp.-, hum.-
C				or poll.conc.-profile
C       I    proftype      - 	profile type. 1:temp., 2:hum., 3:poll.conc.
C
C       Local parameters:
C
C	     actl	   -	actual index of layer
C
Ch**********************************************************************

        IMPLICIT NONE
	INCLUDE 'comv-inp.inc'

C	Pass Parameters:
       REAL acthght
       INTEGER proftype,actzonenr
C proftype: 1=temperature, 2=humidity, 3=pollutant concentration c1

C	Local Parameters:
       INTEGER actl


         IF (LayPtr(1,actzonenr).EQ.0) THEN
           actl=0
         ELSE
           actl=(LayPtr(2,actzonenr)-LayPtr(1,actzonenr)+1)/9
910        CONTINUE
             IF (LayDat(LayPtr(1,actzonenr)+(actl-1)*9).LE.acthght)
     &            GOTO 920
             actl=actl-1
             IF (actl.EQ.0) GOTO 920
             GOTO 910
920        CONTINUE
         ENDIF
         IF (actl.EQ.0) THEN
           profcalz=LayPrec(PZoLP(1,actzonenr)+proftype-1)
         ELSE
           profcalz=LayPrec(PZoLP(1,actzonenr)+actl*3+proftype-1)+
     &       acthght*LayDat(LayPtr(1,actzonenr)+(actl-1)*9+proftype)
         ENDIF
       RETURN
       END


Ch**********************************************************************
C
	SUBROUTINE FMAPROFCAL(fmaprof,hghtprof,actlnr,dpref)
C
C***********************************************************************
C       EMPA rs 1994dec15
C
C  THIS ROUTINE IS CLOSELY RELATED TO THE ROUTINE VERTICOP !
C
C Purpose: This subroutine calculates the massflow profile fmaprof
C          and the height profile hghtprof for the LVO with linknr
C          actlnr. The heights hghtprof(1,i) are calculated relative
C          to reference height of the FROM-zone, hghtprof(2,i)
C                relative to reference height of the TO-zone.
C          Dpref is the pressure difference at reference height.
C
C Changes:
C@NBI PGS 1999Aug10 - Had to change name of variable 'Interval' to 'DeltaZ',
C@NBI                 to avoid confusion with the variable 'Interval'
C@NBI                 (i.e. schedule deltaT) in the INCLUDE file.
C@NBI PGS 2003Apr30 - Bugfix: This code was using wrong definition of
C@NBI                 opening factors "Cfact" for LVO type 2:
C@NBI                 Cfact is 1.0 when opening angle is 90°
C@NBI PGS 2003Apr30 - Replaced calculation routine for LVO2 opening, more
C@NBI                 accurate at all opening angles, especially small
C@NBI                 angles (no discretization error for opening area).
C@NBI               - Also tidied up the code (no syntax change apart from LVO2).
C@empa aw 2005jun01 - LVO2 according to old geometry
C@empa aw 2005jun01 - New LVO3: Horizontal pivoting axis according to geometry from PGS 
C@empa aw 2005jun01 - New LVO4 and LVO5: Triangles with vertex at the bottom and at the top  
C
C Passed arguments:
C
C IO  Name       Units    Description
C
C  O  fmaprof    [kg/s]   massflow profile for LVO
C  O  hghtprof   [m]      profile of evaluation heights for massflow
C                         profile. hghtprof(1,i): rel. to zref(FROM)
C                         hghtprof(2,i): rel. to zref(TO)
C I   actlnr     [-]      actual link number
C I   dpref      [Pa]     actual reference pressure difference
C
C Local variables:
C
C     ActCD     [-]          Actual value for discharge coefficient.
C     ActLh     [m]          Actual height of opening.
C     ActLw     [m]          Actual width of opening.
C     alpha     [-]          Open fraction, in the range 0.0 to 1.0.
C     Area      [m²]         A or Cd·A for actual window opening size.
C     Axishght  [m]          Axis height of horizontal pivot window.
C     Cfact     [-]          opening factor
C     Cs        [kg/s/Pa**n] Massflow coefficient for crack in case of
C                            closed opening.
C     deltaZ    [-]          Interval (distance) between two evaluation points.
C     DpProfNew [Pa]         absolute pressure difference profile
C     dZ42      [m]          The part of a detlaZ strip that is blocked off by window.
C     evalHght  [m]          Height of evaluation plane of pressure and
C                            density profile (midheight of a deltaZ strip).
C     Expn      [-]          Mass flow exponent for crack in case of
C                            closed opening.
C     fmasum,dfmasum         Massflow through actual interval and derivative.
C     hh2,hh4   [m]          Only for LVO2: Height from bottom frame to
C                            bottom and top of open window.
C     Lextra    [m]          Extra crack length for closed opening.
C     oo2,oo4   [m]          Only for LVO2: Narrowest opening width at bottom
C                            and top of window: Perpendicular distance from
C                            glass to horizontal window frame.
C     pp2,pp4   [m]          Only for LVO2: horizontal width of the botton and
C                            top open trianges at each side of the open window.
C     Prefact   [-]          Mass flow coefficient.
C     rholink   [kg/m³]      Density of air in the link.
C     Type      [-]          Opening type.
Ch**********************************************************************

        IMPLICIT NONE
        INCLUDE 'comv-inp.inc'

C       Passed arguments:
        REAL fmaprof(NrInt+2),hghtprof(2,NrInt+2),dpref
        INTEGER actlnr

C       Local variables:
        INTEGER i
        REAL ActLh,ActLw,Lextra,Axishght,ActCD,Cs,expn,Type,deltaZ
     &      ,Prefact,Cfact,hh2,hh4,alphalpha,cc1,cc2,DpProfNew(NrInt+2)
     &      ,EvalHght,RhoLink,oo2,oo4,LL2,LL4,pp2,pp4,sinAlpha,cosAlpha
     &      ,dZ42,area,fma1,fma3,fmaprof1(NrInt+2)
C-----

C       Extracting variables from LinkDat and LDat
        ActLw=LinkDat(pLinkDat(actlnr)+1)
        ActLh=LinkDat(pLinkDat(actlnr)+2)
        ActCD=LinkDat(pLinkDat(actlnr)+3)
        Cs=LDat(pLiLDat(actlnr)+2)
        Expn=LDat(pLiLDat(actlnr)+3)
        Type=LDat(pLiLDat(actlnr)+4)
C@empa aw 2005jun01 types 3,4,5
       IF ((Type.EQ.1).OR.(Type.EQ.4).OR.(Type.EQ.5)) THEN
          Lextra=LDat(pLiLDat(actlnr)+7)
          AxisHght=0
        ELSE IF ((Type.EQ.2).OR.(Type.EQ.3)) THEN
          Lextra=0
          AxisHght=LDat(pLiLDat(actlnr)+7)
        ENDIF
        Cfact=Mf(actlnr)

C       Initialization
        deltaZ=ActLh/NrInt
        DO 50 i=1,NrInt
          DpProfNew(i+1)=DpProf(ProfPtr(actlnr)+i)+dpref
          fmaprof(i+1)=0
          hghtprof(1,i+1)=zll(1,actlnr)+(i-0.5)*deltaZ*
     &                                      Hfl(actlnr)
          hghtprof(2,i+1)=zll(2,actlnr)+(i-0.5)*deltaZ*
     &                                      Hfl(actlnr)
50      CONTINUE
        DpProfNew(1)=DpProf(ProfPtr(actlnr))+dpref
        DpProfNew(NrInt+2)=DpProf(ProfPtr(actlnr)+NrInt+1)+dpref
        fmaprof(1)=0
        fmaprof(NrInt+2)=0
        hghtprof(1,1)=zll(1,actlnr)
        hghtprof(1,NrInt+2)=zll(1,actlnr)+ActLh*Hfl(actlnr)
        hghtprof(2,1)=zll(2,actlnr)
        hghtprof(2,NrInt+2)=zll(2,actlnr)+ActLh*Hfl(actlnr)


C----------------
C       Closed LO
C----------------

        IF (Cfact.EQ.0) THEN
C@empa aw 2005jun01
C         bottom crack
C@empa aw 2005may31 bottom and top crack only for types 1 to 3
         IF (TYPE.LE.3)THEN

          IF (DpProfNew(1).GT.0) THEN
            fmaprof(1)=Cs*ActLw*(DpProfNew(1))**expn
          ELSE
            fmaprof(1)=-Cs*ActLw*(-DpProfNew(1))**expn
          ENDIF

C         top crack
          IF (DpProfNew(NrInt+2).GT.0) THEN
            fmaprof(NrInt+2)=Cs*ActLw*(DpProfNew(NrInt+2))**expn
          ELSE
            fmaprof(NrInt+2)=-Cs*ActLw*(-DpProfNew(NrInt+2))**expn
          ENDIF
         ENDIF
C         side and extra cracks
C@empa aw 2005may31 The total crack length is only in lextra for Type 4 and 5. 
C@empa aw 2005may31 If not defined, a length of 1 is assumed 
        IF (TYPE.LE.3)THEN
          Prefact=deltaZ*(2+lextra/ActLh)*Cs
        ELSEIF ((TYPE.EQ.4).OR.(TYPE.EQ.5))THEN
          IF (lextra.lt.1e-10)lextra=1 
            Prefact=deltaZ*(lextra/ActLh)*Cs
        ENDIF
          DO i=2,NrInt+1
            IF (DpProfNew(i).GT.0) THEN
              fmaprof(i)=Prefact*(DpProfNew(i))**expn
            ELSE
              fmaprof(i)=-Prefact*(-DpProfNew(i))**expn
            ENDIF
          ENDDO

C       End Closed LO
        ELSE

C------------------------
C         Open LO, type 1 and as reference for type 3
C------------------------

C@empa aw 2005may31 calculate rectangle opening also as reference 
C@empa aw 2005may31 (totally opened window) for type 3  
          IF ((type.EQ.1).OR.(type.EQ.3)) THEN

            Prefact=ActLw*ActCd*deltaZ*SQRT(2.0)
            fma1=0
            DO i=2,NrInt+1
              IF (DpProfNew(i).GT.0) THEN
                fmaprof(i)=Prefact*SQRT(RhoProfF(ProfPtr(actlnr)+i-1)*
     &                                  DpProfNew(i))
              ELSE
                fmaprof(i)=-Prefact*SQRT(-RhoProfT(ProfPtr(actlnr)+i-1)*
     &                                   DpProfNew(i))
              ENDIF
C@empa aw 2005jun01 calculate the total flow
              fma1=fma1+abs(fmaprof(i))
            ENDDO
C@empa aw 2005jun01 save the profile 
            fmaprof1=fmaprof
C------------------------
C         Open LO, type 2
C------------------------
          ELSEIF (type.EQ.2) THEN
****
C           Initialization
            alphalpha=Cfact*1.5707963
            hh2=Axishght*(1-COS(alphalpha))
            hh4=Axishght+(ActLh-Axishght)*COS(alphalpha)
            IF (Cfact.EQ.1) THEN
              hh2=Axishght
              hh4=Axishght
            ENDIF


C           Calculation of massflow
            DO i=2,NrInt+1
             EvalHght=deltaZ*(i-1.5)

             IF (DpProfNew(i).GT.0) THEN
              rholink=RhoProfF(ProfPtr(actlnr)+i-1)
             ELSE
              rholink=RhoProfT(ProfPtr(actlnr)+i-1)
             ENDIF
             
             IF ((EvalHght.LE.hh2).OR.(EvalHght.GE.hh4)) THEN
C             rectangular opening on top and bottom of LO
              fmaprof(i)=ActCd*ActLw*deltaZ*
     &                   SQRT(2.0*rholink*ABS(DpProfNew(i)))
             ELSE
C             triangular opening at the side of LO
              cc1=ActCd*ActLw*deltaZ*SQRT(2.0*rholink)
              cc2=2*ActCd*ABS((Axishght-Evalhght))*TAN(alphalpha)*
     &           deltaZ*SQRT(2.0*rholink)
              IF ((cc1.NE.0).AND.(cc2.NE.0)) THEN
                fmaprof(i)=SQRT(ABS(DpProfNew(i))/
     &                     (1/cc1/cc1+1/cc2/cc2))
              ELSE
                fmaprof(i)=0
              ENDIF
             ENDIF

             IF (DpProfNew(i).LT.0) THEN
              fmaprof(i)=-fmaprof(i)
             ENDIF

            ENDDO
          ENDIF
          IF (type.GE.3) THEN

C           Initialization
            alphalpha=Cfact*1.5707963
            cosAlpha=COS(alphalpha)
            sinAlpha=SQRT(MAX(0.0,1.0-cosAlpha*cosAlpha))

100         CONTINUE
            oo2=axisHght*sinAlpha
            LL2=axisHght*cosAlpha
            oo4=(actlH-axisHght)*sinAlpha
            LL4=(actlH-axisHght)*cosAlpha
C@empa aw 2005jun01 I compare the total flows not the areas of the totally and partly opened 
C@empa aw 2005jun01 window (see below), because the area of the partly opened window might be bigger
C@empa aw 2005jun01 but the flow is still smaller than in the totally opend window
CC            IF (oo2+oo4+(oo2*LL2+oo4*LL4)/actlW.GT.actlH+REALMin) THEN
CCC             The window is almost (but not totally) openned. Nevetheless,
CCC             the actual rectanular window frame itself (actlH*actlW) is
CCC             the smallest flow area:
CC              cosAlpha=0.0
CC              sinAlpha=1.0
CC              GOTO 100
CC            ENDIF

            hh2=oo2*sinAlpha
            hh4=actlH-oo4*sinAlpha
            pp2=oo2*cosAlpha
            pp4=oo4*cosAlpha

C           Calculation of mass flow and its derivative
            fma3=0
            DO i=2,NrInt+1
              EvalHght=deltaZ*(i-1.5)

              IF (DpProfNew(i).GT.0) THEN
                rholink=RhoProfF(ProfPtr(actlnr)+i-1)
              ELSE
                rholink=RhoProfT(ProfPtr(actlnr)+i-1)
              ENDIF

C             Calculate opening area at top and bottom, for strip "evalHght"
              IF (TYPE.EQ.3)THEN
               dZ42=MIN(hh4,evalHght+deltaZ*0.5)
     &            -MAX(hh2,evalHght-deltaZ*0.5)
               IF(dZ42.LT.deltaZ-REALMin)THEN
                area=(deltaZ-MAX(0.0,dZ42))/sinAlpha*actlW
               ELSE
                area=0.0
               ENDIF

C              Calculate opening area in side triangles, for strip "evalHght"
               IF (evalHght.LE.hh2) THEN
                IF(hh2.GT.0.0) area=area+deltaZ*2.0*pp2*evalHght/hh2
               ELSEIF (evalHght.LE.axisHght) THEN
                IF(axisHght-hh2.GT.0.0) area=area+deltaZ*2.0*pp2
     &            *(axisHght-evalHght)/(axisHght-hh2)
               ELSEIF (evalHght.LE.hh4) THEN
                IF(hh4-axisHght.GT.0.0) area=area+deltaZ*2.0*pp4
     &            *(evalHght-axisHght)/(hh4-axisHght)
               ELSE
                IF(actlH-axisHght.GT.0.0) area=area+deltaZ*2.0*pp4
     &            *(actlH-evalHght)/(actlH-hh4)
               ENDIF
C	      type 4: Triangle with vertex at the bottom
              ELSEIF (TYPE.EQ.4)THEN
               area = evalHght*ActLw/ActLh*deltaZ  
C	      type 5: Triangle with vertex at the top
              ELSEIF (TYPE.EQ.5)THEN
               area = (ActLh-evalHght)*ActLw/ActLh*deltaZ  
              ENDIF
              IF (area.GT.0.0) THEN
                fmaprof(i)=ActCd*area*
     &                     SQRT(2.0*rholink*ABS(DpProfNew(i)))
                IF (DpProfNew(i).LT.0) fmaprof(i)=-fmaprof(i)
              ELSE
                fmaprof(i)=0
              ENDIF
C@empa aw 2005jun01 calculate the total flow
              fma3=fma3+abs(fmaprof(i))
            ENDDO
C@empa aw 2005jun01 select the profile with the smaller total flow
            IF ((TYPE.EQ.3).AND.(fma1.LT.fma3)) fmaprof=fmaprof1
          ENDIF
C         End Open LOs
        ENDIF
        RETURN
      END


C@NBI PGS 2000Jul20 - This subroutine not being used, so commented out for now
CCCh**********************************************************************
CC      SUBROUTINE wrtmat(a,n,m)
CCCh**********************************************************************
CC        IMPLICIT NONE
CC      INTEGER n,m
CC      REAL a(m,m)
CC      INTEGER i,j
CC
CC      include 'comv-uni.inc'
CC      do 20 i=1,n
CC        Write(CRT,*) (A(i,j), j=1,n)
CC20    continue
CC      return
CC      end


C@NBI PGS 2000Jul20 - This subroutine not being used, so commented out for now
CCCh**********************************************************************
CC      SUBROUTINE wrtarr(a,n,m)
CCCh**********************************************************************
CC        IMPLICIT NONE
CC      INTEGER n,m
CC      REAL a(m)
CC      INTEGER j
CC
CC      include 'comv-uni.inc'
CC        Write(CRT,*) (A(j), j=1,n)
CC      return
CC      end
