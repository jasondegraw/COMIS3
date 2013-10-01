C+*********************************************************** comv-pre.f
C@tno jcp 1996May20_09:28:28 here a new precal routine (PreRho) is inserted
Ch************************************************************************
      SUBROUTINE PreRho
C   PreRho is copied from PreCal and repeats the part where the (zone
C pressure dependent) air densities are calculated. These have an
C influence on the stack pressures.
C   This routine is called if we are close to the solution and if
C the &-PR-CONtrol parameter UseOPz (use old zone pressures) is set to 2
C The meaning of UseOPz: 0 = Reset zone pressures before every time step
C                        1 = Use zone pressures from the previous step
C                        2 = Use PreRho in the iteration loop to update
C                            pressure dependent vars.
C   The result is very close to that of two successive calculations with
C the same meteo and schedule data and the UseOPz variable set to 1.
C In rare cases (high non-horizontal links and high zone reference
C levels) this procedure might lead to divergence.
C@tno jcp 1996May20_09:36:56
C
C@lbl dml 1999nov22 Replace solver control parameters noInit and useOpz
C   with stp1Init, stp2Init, and rhoSync. Since these get passed via a
C   COMMON block, this change does not actually affect routine PreRho().
C
Ch************************************************************************

      IMPLICIT NONE
      include 'comv-inp.inc'
      include 'comv-uni.inc'
      include 'comv-phy.inc'

      real MuF
      integer LenStr
      real ppp
      integer i,j
      integer plink

      do 630 i=1,nZ
C        ! Copy zone concentrations into the C1 array.
C@tno jcp 1996May31_16:39:07 1,5 is now 1, Nconc the number of Pollutants used
CC       do 629 j=1,5
         do 629 j=1,Nconc
           C1(j)=C(j,i)
629      continue
C        ! Calculate the zone air density
         ppp=pbz+pz(i)
         CALL Rhocc(Rhoz(i),ppp,tz(i),xhz(i),c1,mm,nconc)
C        ! Calculate RhoDrZ
         CALL Rhocc1(RhoDrz(i),ppp,tz(i),xhz(i),0.0,0.0,0)
C        ! Calculate the zone dynamic viscosity MuZ
         MuZ(i)=MuF(Tz(i))
         if (test.ge.1 .AND. pecho.ge.2) then
           WRITE(CRT,*) 'Rho=',i,Rhoz(i),'kg/m3',Muz(i),'Pa.s'
         endif
630   continue
C     end loop for zones

C     ! Initialize the link state parameters TempL, XhL, CL with the
C     ! values of the respective from and to zone. Calculate the stack
C     ! effect including effect of layers.
      CALL PSTACK(Nl,FromTo,Zll,RhoZ,RhoOut,
     &  Zz,G,DpSt,DpProf,ProfPtr,RhoProfF,RhoProfT,
     &  RhoL,LayPtr,LayDat,Pz,
     &  Tz,Xhz,C,Pbz,MM,RhoDrL,lstat,TempL,XhL,CL,
     &  Tout,Xhout,ExtConc,Plinkdat,Linkdat,pLiLdat,Ldat,ZoNa,Mf,Hfl)

      do 608 i=1,Nl
         SQRRhoL(1,i)=SQRT(RhoL(1,i))
         SQRRhoL(2,i)=SQRT(RhoL(2,i))
608   continue

      if (test.ge.1 .AND. pecho.ge.2) then
         WRITE(CRT,*) '    Lnr  positive   negative flow direction',
     &     ' RhoL(1,.) RhoL(2,.)'
         do 607 i=1,Nl
           WRITE(CRT,*)'DpSt=',i,DpSt(1,i),DpSt(2,i),'Pa',RhoL(1,i),
     &       RhoL(2,i)
607      continue
         WRITE(CRT,*)'Linknr. Link Temp in from and to zone [C]'
         do 215 i=1,Nl
           WRITE(CRT,*) i,TempL(1,i),TempL(2,i)
215      continue
         WRITE(CRT,*)'Linknr. Link Humidity in from and to '//
     &     'zone [Kg/Kg]'
         do 216 i=1,Nl
           WRITE(CRT,*) i,XhL(1,i),XhL(2,i)
216      continue
         WRITE(CRT,*)'Linknr. Link Concentration in from and '//
     &     'to zone [Kg/Kg]'
         do 217 i=1,Nl
           WRITE(CRT,*) i,CL(1,i),CL(2,i)
217      continue
         WRITE(CRT,*)'Linknr. BetaRho for Link in from and '//
     &     'to zone Kg/m**3/m]'
         do 218 i=1,Nl
           WRITE(CRT,*) i,BetaL(1,i),BetaL(2,i)
218      continue
C        ! output actual values for large opening
         WRITE(CRT,*)
         WRITE(CRT,*)'Actual values for large openings'
         WRITE(CRT,'(6A10)')'LinkNr','LinkTyp','Openfact','ActLw',
     &     'ActLh','ActCd'
         do 219 i=1,Nl
           if (LDat(plildat(i)).EQ.9.) then
             plink=PLinkDat(i)
             WRITE(CRT,'(2A10,4F10.4)') LiNa(i)(1:LenStr(LiNa(i))),
     &         LiTyNa(i)(1:LenStr(LiTyNa(i))),Mf(i),
     &         LinkDat(plink+1),LinkDat(plink+2),LinkDat(plink+3)
           endif
219      continue
      endif

C     ! Combine  DpL=DpSt +Pwind and Pspec and P3d
      CALL combDpL(DpL,DpProf,ProfPtr,DpSt,Pwind,Pspec,
     &  P3d,FromTo,Nl,Lstat,Ldat,PLiLdat)

      if (test.ge.1 .AND. pecho.ge.2) then
         WRITE(CRT,*) ' '
         WRITE(CRT,*) 'This is the final pressure added'//
     &     'to Pz(i) for the link'
         WRITE(CRT,*) '      Lnr positive   negative flow direction'
         do 610 i=1,Nl
           WRITE(CRT,*) 'DpL=',i,DpL(1,i),DpL(2,i),'Pa'
610      continue
      endif

C     ! Calculate the viscosity per link. As link temperatures can change
C     ! due to the flow and the flow direction this could lead to changes
C     ! in viscosity during one calculation of the Pz.
      do 225 i=1,Nl
         MuL(1,i)=Muf(TempL(1,i))
         MuL(2,i)=Muf(TempL(2,i))
225     continue

      CALL PreProf

      if (test.ge.1 .AND. pecho.ge.2) then
         do 612 i=1,Nl
           WRITE(CRT,*) 'MuL=',i,MuL(1,i),MuL(2,i),'Pa.s'
612      continue
      endif
      RETURN
      END
C     ! End subroutine PreRho().


Ch************************************************************************
      SUBROUTINE PreCal(time,frstStep,stpInit)

C Precal performs all precalculations to allow a CALL of netslv.
C The main parameters are air densities, viscosity, stack and wind
C pressures.
C sept 1989 hcp
C@empa aw 1990oct22 Fetch expn from LDat(point+2) before call Sct.
C   Set lstyp1 to 0 before call Sct.(ct will be 1)
C   Mistyped Fact1 changed into Fract1 at Farea
C   for the openwindow  a=a+(Lwmax*LHmax)*areaF
C   nZ removed
C   MaxL removed from call muls
C@empa vd 1991sep13 Complete revision of routine (especially for
C   window link type) on the basis of comven1.3lbl. Routines for crack
C   temperature moved to FEQN
C@empa aw 1992jan29 Calculation of MuL has to be made with TempL not Tz
C@empa aw 1992jan29 NormCrRho, NormCrMu: Crack testconditions
C@empa aw 1992apr04 Give a fix value for NormCrPb and NormCrXh
C   they should come from inputfile like NormCrT
C@empa aw 1992apr28 Plinkdat,Linkdat added in the parameterlist of Pstack
C@empa aw 1992may06 MaxC added in the parameterlist of Pstack
C@empa aw 1992may06 calculate RhoDrZ
C@empa aw 1992may06 for calculation af Rhoout take Cout into account
C@empa aw 1992may07 write the outdoor concentration in testruns
C@lbl rw 1992aug12  Do not use old pressures for the calculation.
C@empa aw 1993jan07 Zz(from),Zz(to)have the wrong sign
C@empa aw 1993feb09 linkheight Zll=Zl+ActualStartHeight introduced
C@empa vd 1993feb18 environment and cp value interpolation moved
C   from main routine into this one.
C@empa aw 1993mar24 Dwind introduced
C@empa aw 1993mar24 Building angle taken into account
C   I notice here inadequate physics behind the  calculation of VeRef.
C   The procedure (and data) should include AlphaMeteo.
C   1. calculate from meteo to the boundary layer height (60 to 100 m)
C   the VBound.
C   2. calculate with the local Alpha from VBound to VeRef on the
C   refheight for the Cpset.
C@empa aw 1994oct12 Section for large openening changed
C@empa rs 1994nov23 Call PreProf
C@empa aw 1997sep04 Rirststep
C@lbl dml 1999nov22 Replace solver control parameters noInit and useOpz
C   with stp1Init, stp2Init, and rhoSync. Note choice of stp1Init or
C   stp2Init appears in value assigned input arg stpInit.
C@lbl dml 1999nov29 Clean up indentation to reflect logical structure.
C@lbl dml 1999nov30 Combine code in successive "if (test.ge.1..." blocks.
C@empa aw 2001sep19 Cd calculation according: "Natürlicher Luftaustausch durch Kippfenster"
C@empa              from Cadloni, Ferrazzini 1996
C@empa              Remember, this relationship was found for single sided ventilation.
C@empa              Here, I use it also for cross ventilation. This should be 
C@empa              changed, when an other relationship for cross ventilation is available.
C
C Pass parameter:
C IO Name       Unit     Description
C ----------------------------------------------------------
C O  FErrIn              =1 if error detected in input data
C
C Common block parameters used in this routine:
C
C Name       Unit        Description
C ---------------------------------------------------------------
C
C Linkdat    multi       Array similar to Ldat for individual links
C
C        For Large opening (ltype=9) :
C
C        Element Parameter Unit     Description
C
C        1       Cs       kg/s@1Pa  Coefficient opening closed : Cs/m *Ltotal
C        2       ActLw    m         Actual width of opening
C        3       ActLh    m         Actual height of opening
C        4       ActCd    -         Actual discharge coefficient
C        .       ...
C        .   10 elements are reserved per link
C       10       ...
C
C plinkdat   [-]         This is the pointer to the first element of a
C                        particular link.
C
C Local parameters:
C
C Name       Unit        Description
C ---------------------------------------------------------------
C MuF                    is a real FUNCTION
C XX
C ppp
C Zfrom      m           Absolute height of link reference height at from
C Zto        m           Absolute height of link reference height at to
C
C For large openings:
C Cs         kg/m3       Cs (kg/m3 and m) * Length
C widthF                 Width factor
C heightF                Height factor
C startHF                Start height factor
C
Ch************************************************************************

      IMPLICIT NONE
      include 'comv-inp.inc'
      include 'comv-uni.inc'
      include 'comv-phy.inc'

C     ! Input arguments.
      double precision time
      logical frstStep
      integer stpInit

C     ! Local variables.
      real MuF
      real xx(20)
      real ppp,lwmax,lhmax,length,lh,rrr
      integer point,from,to,i,j,nn,WiTyp
      real H,WIDTHF,ACTLW,HEIGHTF,ACTLH,STARTHF,ACTSH,ACTCD

      real RhoMet,Fract1
      real XCs
      real GF,RHOF
      real angle
c@empa aw 1997sep18 PSZ
      double precision PSZ
      integer KEY,L
      integer plink

      integer LenStr

C K,K1,k2= pointer into EnVDir and CpDir to interpolate with locate
      integer k,k1,k2
      real Dwind
      real HRel,Gr,Re,Ti,DT,Tm,Ve
      integer zone


C     ! Locate is used to find out between which elements in a data table
C     ! have to be interpolated to find the value at the current
C     ! condition parameter.
C     ! EnvDir = array with start angles of environment directions
C     ! MEnvDir= maximum number of environment directions
C     ! Dmet   = meteo wind direction
C     ! k      = element beyond Dmet
C     ! fract1 = fraction of environ dir above k1
C     ! The environment directions include sectors with a certain terrain
C     ! roughness or wind exponent (Wexp) Alpha.
      Call locate(EnvDir,MEnvDir+2,Dmet,k,fract1)

      k1=k-1
      if (k1.LT.1) k1=MenvDir
      k2=k
      if (k2.GT.MenvDir) k2=1
      Alpha=Fract1*Wexp(k1)+(1-Fract1)*Wexp(k2)

C     ! Same interpolation now for the cp values
      Dwind=Dmet-angbui
25    if (Dwind.lt.0) then
         Dwind =dwind+360
         goto 25
      endif
      Call locate(CpDir,NCpDir+2,Dwind,k,fract1)
      k1=k-1
      if (k1.LT.1) k1=Ncpdir
      k2=k
      if (k2.GT.NCpDir) k2=1
      do 600 i=1, Nwind
C        ! Cp1 is the single array that will be used to calculate wind
C        ! pressures for this wind direction Dmet
         Cp1(i)=Fract1*Cp(i+(k1-1)*Nwind)+(1-Fract1)*Cp(i+(k2-1)*Nwind)
600   continue

CC    if( useOpz.eq.0 ) then
      if( stpInit .lt. 2 ) then
C        ! stpInit=0 or 1 reinitializes pressures at each time step.
         do 2 i=1,nZ
           pz(i)=0.0
2        continue
      endif

      do 1 i=1,Nl
         Tl(i)=0.0
1     continue

C     ! C1 = the nconc gasses but only for one zone at a time, they are
C     !      copied from C(nconc,zone) in order to be able to CALL Rhocc

C     ! Calculate G at building's latitude
      G=GF(lat)

      if (test.ge.1 .AND. pecho.ge.2) then
         WRITE(CRT,*) ' '
         WRITE(CRT,*) 'As test>1 and PEcho>5 here follows an '//
     &     'extended dump of precalculations'
         WRITE(CRT,*) ' '
         WRITE(CRT,*) 'G=',G
CC    endif
CC    if (test.ge.1 .AND. pecho.ge.2) then
         WRITE(CRT,*) 'XhMet,Tmet,Pbmet,AlphMet',
     &     XhMet,Tmet,Pbmet,AlphMet
      endif

C     ! NormCrRho, NormCrMu: Crack test conditions
C     ! given NormCrPb (pressures) and NormCrXh (humidity ratio)
      NormCrRho=RhoF(NormCrPb,NormCrT,NormCrXh)
      NormCrMu=Muf(NormCrT)

C calculate RhoMet, air density at meteo level
      RhoMet=RhoF(Pbmet,Tmet,Xhmet)

      if (test.ge.1 .AND. pecho.ge.2) then
         WRITE(CRT,*) 'RhoMet=',RhoMet
CC    endif
CC    if (test.ge.1 .AND. pecho.ge.2) then
         WRITE(CRT,*) 'Zentr,Zmet,Pbmet,RhoMet,G',Zentr,Zmet,
     &     Pbmet,RhoMet,G
         WRITE(CRT,*) 'NormCrRho,NormCrMu',NormCrRho,NormCrMu
      endif

C     ! Calculate Pbz from Pbmet to the level of the building entrance.
C     ! In fact we use RhoMet in this traject. There could exist another
C     ! temperature between the meteo site and the building. Then it is
C     ! better to give that value as meteo temperature. As the effect of
C     ! Pbz on ventilation is small don't worry, any value will do.
      CALL PBAROM(Pbz,Zentr,Zmet,Pbmet,RhoMet,G)

      if (test.ge.1 .AND. pecho.ge.2) then
         WRITE(CRT,*) 'Pbz=',Pbz
      endif
C     ! Here we did not have a different temperature outside the building
C     ! in *CIF, so the Tout will be Tmet
      Tout=Tmet
C@empa aw 2001jan12 Don't take Xhmet as Xhout if already Cout(H2O) = Xhmet
C@empa              ('meteo' in the H2O pollutant schedule). H2O would be taken
C@empa              twice into account for the calculation of Rhoout.
CC      Xhout=Xhmet
      if (UseMetH2O.ne.1) Xhout=Xhmet
      if (test.ge.1 .AND. pecho.ge.2) then
         WRITE(CRT,*) 'XhOut,Tout,Pbz',XhOut,Tout,Pbz
         WRITE(CRT,*) 'Cout(1..MaxC)',(Cout(j),j=1,MaxC)
      endif

      CALL Rhocc(Rhoout,Pbz,Tout,Xhout,Cout,mm,5)

      if (test.ge.1 .AND. pecho.ge.2) then
         WRITE(CRT,*) 'RhoOut=',RhoOut
CC    endif
CC    if (test.ge.1 .AND. pecho.ge.2) then
         WRITE(CRT,*) 'zVmet,ZRef,Vmet,Alpha',zVmet,ZRef,Vmet,Alpha
      endif

C     ! Calculate Vref
C     ! Add IUnit(UnitProf) selector for powerlaw or log
C     ! profile. This is a string of MUUstrL=20 long.
      CALL wind(zVmet,ZRef,Vmet,AlphMet,Alpha,
C@lbl bvs 1998Sep15 changed IUnit(UnitProf) to wprofu (assigned in inENVWIN)
CC     &   IUnit(UnitProf),VeRef,Key)
     &  wprofu,VeRef,Key)

      if (test.ge.1 .AND. pecho.ge.2) then
         WRITE(CRT,*) 'Nwind,Veref,RhoOut=',Nwind,Veref,RhoOut
CC    endif
CC    if (test.ge.1 .AND. pecho.ge.2) then
         WRITE(CRT,*) '   nr   Cp1(nr)'
         do 635 i=1,Nwind
           WRITE(CRT,*) 'Cp1=',i,Cp1(i)
635      continue
      endif

C     ! Calculate the wind pressures from Cp1*RhoOut*VeRef**2
      CALL press(Cp1,Nwind,VeRef,RhoOut,Pwind)

      if (test.ge.1 .AND. pecho.ge.2) then
         WRITE(CRT,*) '      nr  Pwind(nr)'
         do 636 i=1,Nwind
           WRITE(CRT,*) 'Pwind=',i,Pwind(i),'Pa'
636      continue
         WRITE(CRT,*) '      nr  Pspec(nr)'
         do 611 i=1,Nspec
           WRITE(CRT,*) 'Pspec=',i,Pspec(i),'Pa'
611      continue
      endif

C     ! We have the large opening precalculations here before calling
C     ! PSTACK because the reference height of the link will change if
C     ! start height is changing. VD 1991sep17
C     ! Initialisation of Array Linkdat
      do 60  i=1,MaxD
         LinkDat(i)=0
60    continue
      do 70  i=1,MaxL
         pLinkDat(i)=0
70    continue
      pLink=1

C     ---------------------------------------- Loop 100 for all links
      do 100 i=1,Nl
         point=pLiLDat(i)
         ltyp=LDat(point)
         plinkdat(i)= plink
         Zll(1,i)=Zl(1,i)
         Zll(2,i)=Zl(2,i)
C@empa aw 1994oct12  Section for large openening has changed:
C@       - own height factor "Hfl" (for slanted windows) not taken
C@       into account for Lhmax, but for ActualStartHeight.
C@       - check horizontal link also when window is closed
C@       - ActLh, ActLw for closed window or opening with
C@       horizontal pivoting axis
C@       Old markers and codelines are deleted
C        ---------------------------------------- Large opening only
         if (Ltyp.eq.9) then
           WITyp=Ldat(point+4)
           Lwmax = Ldat(point+5)
           Lh = Ldat(point+6)
C          ! Lhmax for slanted window = Lh* Own height factor Hfl of link
           Lhmax = Lh
C@empa aw 2003jun03 for WITyp 2 Ldat(point +7) is the axisheight --> don't add to length  
           IF (WITyp.EQ.1)THEN   
              Length= (Lwmax+Lhmax)*2+Ldat(point+7)
C@empa aw 2005may31 Crack length for type 4 and 5 
           ELSEIF ((WITyp.EQ.4).OR.(WITyp.EQ.5))THEN
              length= Ldat(point+7)
           ELSE
              Length= (Lwmax+Lhmax)*2
		 ENDIF 

C          ! Cs= Length* Cs/m  Cs/m: Element 3 in Ldat / Element 1 in Linkdat
           rrr = ldat(point+2)
           XCs  = Length*rrr
           Linkdat(plink)=XCs
C          ! Check link heights from and to heights:
C          ! For a large opening the total height of the link must be
C          ! equal at from and to side. Assumption: An allowable error
C          ! of 10mm for Delta- Height is assumed.
           from=fromto(1,i)
           to=fromto(2,i)
           l=lstat(i)
           H=(Zl(1,i)-Zl(2,i))
           if (L.eq.0 .or. L.eq.3 .or. L.eq.6) then
             H=H+Zz(from)
           endif
           if (L.LT.3) then
             H=H-Zz(to)
           endif
           if( ABS(H).gt.0.1 ) then
             CALL ERROR2('From and To total height of this large '//
     &         'opening link are not within 0.1m:  ',LiNa(i),1)
           endif
C          ! Actual opening fraction of window = Link Multiplication
C          ! factor.
C@empa aw 2005aug30 check wether opening factor is within the allowed range
           IF ((Mf(i).GT.1.0).OR.(Mf(i).LT.0.)) THEN
             CALL INERR('&-NET-LINks:  LinkNr: '//LiNa(I),
     &         'Opening factor of window is not within '//
     &         'the range 0...1!',
     &         .FALSE.,3)
           ENDIF
           if (Mf(i).GT.0.0) then


C            ! open window only
C            ! Interpolate actual parameter values from given input
C            ! data per opening fraction
C            ! nn:   Ndatalines for open window
             nn=ldat(point+1)
             do 110 j=1,nn
C              ! get the openfractions defined in NET-AIR AFC in xx
               xx(j)=Ldat(point+8+(j-1)*9)
110          continue
             CALL locate(xx,nn,Mf(i),j,fract1)
C            ! Interpolate the area factor between the two Ldat elements
C            ! indicated by j. The first element is in 11 the next 9
C            ! higher if the window is full open Mf(i) will be 1 and if
C            ! all is ok j points to the last value in LDat for this
C            ! window. The element j+1 is than no longer from this
C            ! window, but the fraction for that element has to be zero,
C            ! so it won't have an influence.
C            ! Actual parameters are written into the array Linkdat
C            ! Linkdat (plink+1) = ActLw
C            ! Linkdat (plink+2) = ActLh
C            ! Linkdat (plink+3) = ActCd
             widthF= Ldat(point+10+(j-1)*9)*Fract1+
     &         LDat(point+10+(j)*9)*(1-Fract1)
             ActLw = widthF*Lwmax
             Linkdat(plink+1)= ActLw
             heightF=Ldat(point+11+(j-1)*9)*Fract1+
     &         LDat(point+11+(j)*9)*(1-Fract1)
             ActLh = heightF*Lhmax
             if ((WITyp.EQ.1).and.(ActLh.EQ.0.0)) then
               CALL ERROR2
     &  ('The actual height of large opening on this link is zero! ',
     &           'Link: '//LiNa(i),3)
             endif
             Linkdat(plink+2)= ActLh
             StartHF=Ldat(point+12+(j-1)*9)*Fract1+
     &         LDat(point+12+(j)*9)*(1-Fract1)
C            ! Actual start height: Hfl=Own height factor (for slanted windows)
             ActSh  =StartHF*Lhmax*Hfl(i)
C            ! Add actual start height to both Zl values
             Zll(1,i)=Zl(1,i)+ActSh
             Zll(2,i)=Zl(2,i)+ActSh
             ActCd  =Ldat(point+9 +(j-1)*9)*Fract1+
     &         LDat(point+9 +(j)*9)*(1-Fract1)
C            ! Calculate ActCd if it is 0.0
             if (ActCd.EQ.0.0) then
C@empa aw 2001sep19 for LVO Type 1
             if (WiTyp.EQ.1)then
               if (L.EQ.0) then
C                ! internal door:
                 if (Zll(1,i).GT.0.05) then
C                  ! error message: not a door
                   CALL ERROR2('&-NET-LINKs: LinkNr: '//LiNa(i)//
     &               ' Cd calculation for internal doors:',
     &               'The doorstep is higher than 0.05m. '//
     &               'Cd cannot be calculated !',3)
                 endif
C                ! Calculate Cd according to Roger Pelletret CSTB: Cd=f(Hrel)
                 HRel=ActLh/Hz(from)
                 if (HRel.LT.0.2) then
                   ActCd= 0.0558
                   CALL ERROR2('&-NET-LINKs: LinkNr: '//LiNa(i)//
     &               ' Cd calculation for internal doors:  '//
     &               'Hdoor/Hroom is','below the application '//
     &               'range 0.2:0.9. Cd is set to fixed value: '//
     &               '0.0558!',1)
                 else if (HRel.GT.0.9) then
                   ActCd= 0.4821
                   CALL ERROR2('&-NET-LINKs: LinkNr: '//LiNa(i)//
     &               ' Cd calculation for internal doors:  '//
     &               'Hdoor/Hroom is','above the application '//
     &               'range 0.2:0.9. Cd is set to fixed value: '//
     &               '0.4821!',1)
                 else
                   ActCd = 0.609*HRel-0.066
                 endif
               else if ((L.EQ.1).OR.(L.EQ.3)) then
C                ! External opening:  Calculate Cd according to Mat Santamouris
                 if (L.EQ.1) then
                   zone=to
                 else
C                  ! L.eq.3
                   zone=from
                 endif
                 Ti=Tz(zone)
                 DT=abs(Ti-Tout)
                 Tm=(Ti+Tout)/2
C                ! Mu is not needed, its shortened
CC               Mu=Muf(Tm)
                 Tm=Tm+Tzero
CC               Gr=g*DT*ActLh**3/(Tm*Mu**2)
                 Gr=g*DT*ActLh**3/Tm
                 if (Gr.LT.1E-5) then
                   Gr=1E-5
                 endif
C                ! Ve=wind velocity at 10m height
C                ! add IUnit(UnitProf) selector for powerlaw or log
C                ! profile. This is a string of MUUstrL=20 long
                 CALL wind(zVmet,10.,Vmet,AlphMet,Alpha,
C@lbl bvs 1998Sep15 changed IUnit(UnitProf) to wprofu (assigned in inENVWIN)
CC     &   IUnit(UnitProf),Ve,Key)
     &             wprofu,Ve,Key)
CC               Re=Ve*Dz(zone)/Mu
                 Re=Ve*Dz(zone)
                 if (Re**2.LT.1E-5) Re=SQRT(1E-5)
                 ActCd=0.08*(Gr/Re**2)**(-0.38)
                 if (ActCd.GT.1.5) then
                   ActCd=1.5
                 else if (ActCd.LT.0.6) then
                   ActCd=0.6
                 endif
               endif
C              ! endif L=0,1,or 3
C@empa aw 2001sep19 Cd calculation according: "Natürlicher Luftaustausch durch Kippfenster"
C@empa              from Cadloni, Ferrazzini 1996
C@empa              Remember, this relationship was found for single sided ventilation.
C@empa              Here, I use it also for cross ventilation. This should be 
C@empa              changed, when a other relationship for cross ventilation is available.

             else if (WITyp.eq.2)then
             ! LVO Type = 2: horizontal pivoting axis 
             ! ActCd =f(angle,LhMax/LhMax)   -->   0.2 < ActCd < 0.7
cc               angle=min(mf(i)*90.,20.)
               angle=mf(i)*90.
               ActCd=min(max((angle*0.0147-0.0928*LhMax/LwMax+0.4116),
     &         0.2),0.6)
             endif 
             ! endif LVO Type 2
             endif
C            ! end calculate ActCd if it is 0.0
             Linkdat(plink+3)= ActCd
           endif
C          ! endif mf(i).gt.0
           if ((Mf(i).EQ.0.0).OR.(WITyp.EQ.2)) then
C            ! Closed window, or opening with horizontal pivoting axis
             ActLw=Lwmax
             ActLh=Lhmax
             Linkdat(plink+1)= ActLw
             Linkdat(plink+2)= ActLh
           endif
C          ! We have a window, 10 elements in Linkdat are reserved per window link
           plink=plink+10
         endif
C        ! endif Ltyp.eq.9
100   continue
C     ---------------------------------------------------end loop 100

C     ! Calculate Tz(i) for single sided ventilation case
      CALL PrecSSV(time)

C     ! Loop for zones put here after call PrecSSV
C     ! calculate Rhozone and MuZ
      if (test.ge.1 .AND. pecho.ge.2) then
         WRITE(CRT,*) ' '
         WRITE(CRT,*) '    Znr Rho(Znr)'
      endif

      do 630 i=1,nZ
C        ! At the first run Pz(i) will be zero, but at succesive runs
C        ! one might want to take into account the last pressure to
C        ! calculate Rhoz(*)
C        ! Copy the concentrations for this zone into the C1 array
C@tno jcp 1996May31_16:39:07 1,5 is now 1, Nconc the number of Pollutants used
CC        do 629 j=1,5
         do 629 j=1,Nconc
           C1(j)=C(j,i)
629      continue
C        ! Calculate the air density per zone
         ppp=pbz+pz(i)
         CALL Rhocc(Rhoz(i),ppp,tz(i),xhz(i),c1,mm,nconc)
C        ! calculate RhoDrZ
         CALL Rhocc1(RhoDrz(i),ppp,tz(i),xhz(i),0.0,0.0,0)
C        ! calculate the dynamic viscosity MuZ per zone
         MuZ(i)=MuF(Tz(i))
C@empa aw 1997sep04 Initialize Pz(i)
         if (frstStep) then
           pz(i)=psz(DBLE(Pbz),DBLE(RhoZ(i)),0.0d0,0.0d0,DBLE(Zz(i)),G)
           ppp=pbz+pz(i)
           CALL Rhocc1(Rhoz(i),ppp,Tz(i),Xhz(i),c1(1),mm(1),nconc)
         endif
         if (test.ge.1 .AND. pecho.ge.2) then
           WRITE(CRT,*) 'Rho=',i,Rhoz(i),'kg/m3',Muz(i),'Pa.s'
         endif
630   continue
C     ! end loop for zones

C     ! Initialize the link state parameters TempL, XhL, CL with the
C     ! values of the respective from and to zone and calculate the
C     ! stack effect including effect of layers.
      CALL PSTACK(Nl,FromTo,Zll,RhoZ,RhoOut,
     &  Zz,G,DpSt,DpProf,ProfPtr,RhoProfF,RhoProfT,
     &  RhoL,LayPtr,LayDat,Pz,
     &  Tz,Xhz,C,Pbz,MM,RhoDrL,lstat,TempL,XhL,CL,
     &  Tout,Xhout,ExtConc,Plinkdat,Linkdat,pLiLdat,Ldat,ZoNa,Mf,Hfl)

      do 608 i=1,Nl
         SQRRhoL(1,i)=SQRT(RhoL(1,i))
         SQRRhoL(2,i)=SQRT(RhoL(2,i))
608   continue

      if (test.ge.1 .AND. pecho.ge.2) then
         WRITE(CRT,*) '    Lnr  positive   negative flow direction',
     &     ' RhoL(1,.) RhoL(2,.)'
         do 607 i=1,Nl
           WRITE(CRT,*)'DpSt=',i,DpSt(1,i),DpSt(2,i),'Pa',RhoL(1,i),
     &     RhoL(2,i)
607      continue
         WRITE(CRT,*)'Linknr. Link Temp in from and to zone [C]'
         do 215 i=1,Nl
            WRITE(CRT,*) i,TempL(1,i),TempL(2,i)
215      continue
         WRITE(CRT,*)'Linknr. Link Humidity in from and to zone [Kg/Kg]'
         do 216 i=1,Nl
           WRITE(CRT,*) i,XhL(1,i),XhL(2,i)
216      continue
         WRITE(CRT,*)'Linknr. Link Concentration in from and '//
     &     'to zone [Kg/Kg]'
         do 217 i=1,Nl
           WRITE(CRT,*) i,CL(1,i),CL(2,i)
217      continue
         WRITE(CRT,*)'Linknr. BetaRho for Link in from and '//
     &     'to zone Kg/m**3/m]'
         do 218 i=1,Nl
           WRITE(CRT,*) i,BetaL(1,i),BetaL(2,i)
218      continue
C        ! Output actual values for large opening
         WRITE(CRT,*)
         WRITE(CRT,*)'Actual values for large openings'
         WRITE(CRT,'(6A10)')'LinkNr','LinkTyp','Openfact','ActLw',
     &     'ActLh','ActCd'
         do 219 i=1,Nl
           if (LDat(plildat(i)).EQ.9.) then
             plink=PLinkDat(i)
             WRITE(CRT,'(2A10,4F10.4)') LiNa(i)(1:LenStr(LiNa(i))),
     &         LiTyNa(i)(1:LenStr(LiTyNa(i))),Mf(i),
     &         LinkDat(plink+1),LinkDat(plink+2),LinkDat(plink+3)
           endif
219      continue
      endif

C     ! Combine  DpL=DpSt +Pwind and Pspec and P3d
      CALL combDpL(DpL,DpProf,ProfPtr,DpSt,Pwind,Pspec,
     &  P3d,FromTo,Nl,Lstat,Ldat,PLiLdat)

      if (test.ge.1 .AND. pecho.ge.2) then
         WRITE(CRT,*) ' '
         WRITE(CRT,*) 'This is the final pressure added'//
     &     'to Pz(i) for the link'
         WRITE(CRT,*) '      Lnr positive   negative flow direction'
         do 610 i=1,Nl
           WRITE(CRT,*) 'DpL=',i,DpL(1,i),DpL(2,i),'Pa'
610      continue
      endif

C     ! Calculate the viscosity per link. As link temperatures can
C     ! change due to the flow and the flow direction this could lead to
C     ! changes in viscosity during one calculation of the Pz.
      do 225 i=1,Nl
         MuL(1,i)=Muf(TempL(1,i))
         MuL(2,i)=Muf(TempL(2,i))
225   continue

      CALL PreProf

      if (test.ge.1 .AND. pecho.ge.2) then
         do 612 i=1,Nl
           WRITE(CRT,*) 'MuL=',i,MuL(1,i),MuL(2,i),'Pa.s'
612      continue
CC    endif
CC    if (test.ge.1 .AND. pecho.ge.2) then
         WRITE(CRT,*) ' '
C        ! Lstat is the link status:
         WRITE(CRT,*) 'Link-status:',
     &     ' 0=zone-zone; 1=ext-zone; 2=spec-zone;'
         WRITE(CRT,*) '            ',
     &     ' 3=zone-ext ; 4=ext- ext; 5=spec-ext ;'
         WRITE(CRT,*) '            ',
     &     ' 6=zone-spec; 7=ext-spec; 8=spec-spec'
         WRITE(CRT,*) '  Lnr Lstat(Lnr)'
         do 620 i=1,Nl
           WRITE(CRT,*) 'Lstat=',i,Lstat(i)
620      continue
         WRITE(CRT,*) ' '
      endif
      if (PEcho.GE.1 .AND. test.ge.1 ) then
         WRITE(CRT,*) '****************************************'
         WRITE(CRT,*) 'end of precalculations , start solving *'
         WRITE(CRT,*) '****************************************'
         WRITE(CRT,*) ' '
      endif

      RETURN
      END


Ch***********************************************************************
      SUBROUTINE locate(xx,n,x,j,Fract1)
C               |  | | | |
C               |  | | | Y=fract1*yy(j1)+(1-fract1)*yy(j2)
C               |  | | j is the element in xx just left of x
C               |  | x is the value looked for in the array xx
C               |  n is the dimension of the array xx
C               xx is the array containing the header of a table
C               yy is the contents of the table
C***********************************************************************
C To avoid "off scale" problems the first element in xx is the lowest
C possible value, and xx(n) is the highest possible value.
C In the case of winddirections 0, 90, 180, 270
C  The column numbers in yy are:1,  2,     3,   4.
C
C xx should have 6 elements with: xx(1)=last direction-360
C                   xx(2)=0
C                   xx(3)=90
C                   xx(4)=180
C                   xx(5)=270
C                   xx(6)=first direction+360
C
C The array xx has always two more elements than the original header of the
C cp part.
C However the array yy will not be copied in the same way with extra columns
C on both sides. So the best thing would be to recalculate the column numbers in
C yy that have to be computed like  Y=fract1*yy(j1)+(1-fract1)*yy(j2)
C There fore we do: J1=j-1
C             if (J1<1) then J1=N-2
C             J2=j
C             if (J2>N-2) then J2=1
C
C This routine is taken from the book Numerical Recipies CH 3.4
C Changed for the interpolation of wind pressure coefficients.
C sept 1989 hcp
C
C@lbl bs 1991jul01  Error: Fract1 got a negative value which is wrong
C@empa vd 1991sep06
Ch***********************************************************************

      IMPLICIT NONE
      real xx(*),x,fract1
      integer j,Jl,Ju,Jm,N
      real Rdist

      jl=1
      ju=N+1

10    if(ju-jl.GT.1) then
         jm=(ju+jl)/2
         if((xx(n).GT.xx(1)).eqv.(x.GT.xx(jm))) then
           jl=jm
         else
           ju=jm
         endif
         goto 10
      endif

      j=jl

      RDist=xx(j+1)-xx(j)
      if (Rdist.NE.0.0) then
         Fract1=(xx(j+1)-x)/Rdist
      else
         fract1=0.5
      endif

      RETURN
      END

Ch***********************************************************************
      SUBROUTINE PreProf
C***********************************************************************
C
C Purpose: This routine calculates the values of the temperature-
C        and humidity- and pollutant concentration profile for each layer
C        extrapolated to height z=0. First element: non layered part of zone.
C        Also the mean values of the profiles in the zone are determined.
C
C Module:
C
C Changes:
C
C Pass Parameters:
Ch***********************************************************************

      IMPLICIT NONE
      include 'comv-inp.inc'
      include 'comv-phy.inc'

C     ! Local variables.
      integer i,k,AnzLay,ActPZoLP(2),zerolayer
      integer actlay
      real actzli,actdt1,actdt2,actdh1,actdh2,actdc1,actdc2
      real htop,hbot,ProfSumT,ProfSumXh,ProfSumC
      real actzbot,actztop

C     ! Initialization.
      ActPZoLP(1)=0
      ActPZoLP(2)=0

C     ! Loop over zones.
      do 100 i=1,nZ
C        ! Initialization. For the initilisation hbot=0 it is assumed,
C        ! that the zone reference height (Zz(i)) corresponds to the
C        ! level of the floor of that zone. If there should be created
C        ! once a possibility to input an offset between zone reference
C        ! height and floor level, the initialisation should be changed
C        ! to :
C        ! hbot=offset
C        ! htop=hz(i)+offset
         hbot=0
         htop=hz(i)
         if (LayPtr(1,i).EQ.0) then
           AnzLay=0
         else
           AnzLay=(LayPtr(2,i)-LayPtr(1,i)+1)/9
         endif
         ActPZoLP(1)=ActPZoLP(2)+1
         ActPZoLP(2)=ActPZoLP(1)+3*(AnzLay+1)-1
         PZoLP(1,i)=ActPZoLP(1)
         PZoLP(2,i)=ActPZoLP(2)
C        ! Calculation of LayPrec-array
         if (AnzLay.EQ.0) then
C          ! zone without layers
           LayPrec(PZoLP(1,i))=Tz(i)
           LayPrec(PZoLP(1,i)+1)=Xhz(i)
           LayPrec(PZoLP(1,i)+2)=1
           Tmz(i)=Tz(i)
           Xhmz(i)=Xhz(i)
           Crelmz(i)=1
         else
C          ! layered zone
           zerolayer=0
           k=1
110        continue
           if (LayDat(LayPtr(1,i)+(k-1)*9).GT.0) GOTO 120
           zerolayer=zerolayer+1
           k=k+1
           if (k.GT.AnzLay) GOTO 120
           GOTO 110
120        continue
           LayPrec(PZoLP(1,i)+3*zerolayer)=Tz(i)
           LayPrec(PZoLP(1,i)+3*zerolayer+1)=Xhz(i)
           LayPrec(PZoLP(1,i)+3*zerolayer+2)=1
           if (zerolayer.GT.0) then
             do 200 k=1,zerolayer
               actzli=LayDat(LayPtr(1,i)+(zerolayer-k)*9)
               actdt1=LayDat(LayPtr(1,i)+(zerolayer-k)*9+1)
               actdh1=LayDat(LayPtr(1,i)+(zerolayer-k)*9+2)*Xhz(i)
               actdc1=LayDat(LayPtr(1,i)+(zerolayer-k)*9+3)
               if (k.EQ.zerolayer) then
                 actdt2=0
                 actdh2=0
                 actdc2=0
               else
                 actdt2=LayDat(LayPtr(1,i)+(zerolayer-k-1)*9+1)
                 actdh2=LayDat(LayPtr(1,i)+(zerolayer-k-1)*9+2)*Xhz(i)
                 actdc2=LayDat(LayPtr(1,i)+(zerolayer-k-1)*9+3)
               endif
               LayPrec(PZoLP(1,i)+3*(zerolayer-k))=
     &           actzli*(actdt1-actdt2)+
     &           LayPrec(PZoLP(1,i)+3*(zerolayer-k+1))
               LayPrec(PZoLP(1,i)+3*(zerolayer-k)+1)=
     &           actzli*(actdh1-actdh2)+
     &           LayPrec(PZoLP(1,i)+3*(zerolayer-k+1)+1)
               LayPrec(PZoLP(1,i)+3*(zerolayer-k)+2)=
     &           actzli*(actdc1-actdc2)+
     &           LayPrec(PZoLP(1,i)+3*(zerolayer-k+1)+2)
200          continue
           endif
           if (zerolayer.LT.AnzLay) then
             do 300 k=1,(AnzLay-zerolayer)
               actzli=LayDat(LayPtr(1,i)+(zerolayer+k-1)*9)
               if ((zerolayer+k).EQ.1) then
                 actdt1=0
                 actdh1=0
                 actdc1=0
               else
                 actdt1=LayDat(LayPtr(1,i)+(zerolayer+k-2)*9+1)
                 actdh1=LayDat(LayPtr(1,i)+(zerolayer+k-2)*9+2)*Xhz(i)
                 actdc1=LayDat(LayPtr(1,i)+(zerolayer+k-2)*9+3)
               endif
               actdt2=LayDat(LayPtr(1,i)+(zerolayer+k-1)*9+1)
               actdh2=LayDat(LayPtr(1,i)+(zerolayer+k-1)*9+2)*Xhz(i)
               actdc2=LayDat(LayPtr(1,i)+(zerolayer+k-1)*9+3)
               LayPrec(PZoLP(1,i)+3*(zerolayer+k))=
     &           actzli*(actdt1-actdt2)+
     &           LayPrec(PZoLP(1,i)+3*(zerolayer+k-1))
               LayPrec(PZoLP(1,i)+3*(zerolayer+k)+1)=
     &           actzli*(actdh1-actdh2)+
     &           LayPrec(PZoLP(1,i)+3*(zerolayer+k-1)+1)
               LayPrec(PZoLP(1,i)+3*(zerolayer+k)+2)=
     &           actzli*(actdc1-actdc2)+
     &           LayPrec(PZoLP(1,i)+3*(zerolayer+k-1)+2)
300          continue
           endif
         endif
C        ! end of calculation of PreProf (endif Anzlay.eq.0 & else)
C        ! Calculation of 'pseudo mean values' of temperature,
C        ! humidity and concentration of pollutant 1:
C        ! Tmz(i),Xhmz(i),Crelmz(i)
         if (AnzLay.EQ.0) then
C          ! zone without layers
           Tmz(i)=Tz(i)
           Xhmz(i)=Xhz(i)
           Crelmz(i)=1
         else
C          ! layered zone
           ProfSumT=0
           ProfSumXh=0
           ProfSumC=0
           actlay=0
400        continue
           if (actlay.EQ.anzlay) GOTO 410
           if (hbot.LT.LayDat(LayPtr(1,i)+actlay*9)) GOTO 410
           actlay=actlay+1
           GOTO 400
410        continue
           actzbot=hbot
           if (hbot.EQ.htop) then
             if (actlay.EQ.0) then
               Tmz(i)=LayPrec(PZoLP(1,i))
               Xhmz(i)=LayPrec(PZoLP(1,i)+1)
               Crelmz(i)=LayPrec(PZoLP(1,i)+2)
             else
               Tmz(i)=LayPrec(PZoLP(1,i)+actlay*3)+
     &           hbot*LayDat(LayPtr(1,i)+(actlay-1)*9+1)
               Xhmz(i)=LayPrec(PZoLP(1,i)+actlay*3+1)+
     &           hbot*LayDat(LayPtr(1,i)+(actlay-1)*9+2)*xhz(i)
               Crelmz(i)=LayPrec(PZoLP(1,i)+actlay*3+2)+
     &           hbot*LayDat(LayPtr(1,i)+(actlay-1)*9+3)
             endif
           else
420          continue
             if (actlay.EQ.AnzLay) then
               actztop=htop
             else
               actztop=MIN(htop,LayDat(LayPtr(1,i)+(actlay)*9))
             endif
             if (actlay.EQ.0) then
               ProfSumT=ProfSumT+(actztop-actzbot)*LayPrec(PZoLP(1,i))
               ProfSumXh=ProfSumXh+(actztop-actzbot)*
     &           LayPrec(PZoLP(1,i)+1)
               ProfSumC=ProfSumC+(actztop-actzbot)*LayPrec(PZoLP(1,i)+2)
             else
               ProfSumT=ProfSumT+(actztop-actzbot)*
     &           LayPrec(PZoLP(1,i)+3*actlay)+
     &           0.5*(actztop*actztop-actzbot*actzbot)*
     &           LayDat(LayPtr(1,i)+9*(actlay-1)+1)
               ProfSumXh=ProfSumXh+(actztop-actzbot)*
     &           LayPrec(PZoLP(1,i)+3*actlay+1)+
     &           0.5*(actztop*actztop-actzbot*actzbot)*
     &           LayDat(LayPtr(1,i)+9*(actlay-1)+2)*xhz(i)
               ProfSumC=ProfSumC+(actztop-actzbot)*
     &           LayPrec(PZoLP(1,i)+3*actlay+2)+
     &           0.5*(actztop*actztop-actzbot*actzbot)*
     &           LayDat(LayPtr(1,i)+9*(actlay-1)+3)
             endif
             if (actztop.EQ.htop) GOTO 430
             actlay=actlay+1
             actzbot=actztop
             GOTO 420
430          continue
             Tmz(i)=ProfSumT/(htop-hbot)
             Xhmz(i)=ProfSumXh/(htop-hbot)
             Crelmz(i)=ProfSumC/(htop-hbot)
           endif
         endif
C        ! end of calculation of profile mean values
C        ! Conversion from deg Celsius to deg Kelvin
         Tmz(i)=Tmz(i)+Tzero
         do 550 k=1,AnzLay+1
           LayPrec(PzoLp(1,i)+3*(k-1))=
     &       LayPrec(PzoLp(1,i)+3*(k-1))+Tzero
550      continue
100   continue
C     ! end of loop over zones

      END
