C+*********************************************************** comv-slv.f

Ch***********************************************************************

C@empa aw 2000mar15 FVold, FVoldDefined added
      subroutine SLV01(nZ,nL,LStat,
     &  LDat,pLiLDat,DpL,FromTo,Mf,Mp,epsFA,epsFR,
     &  RhoL,SqrRhoL,MuL,Pz,Fv,Fv2,Ft,Fmb,
     &  nIter,key,difLim,fmFact,mIter,DMZ,stpInit,rhoSync,RSqrtDff,
     &  nJunc,JuncNr,DpJ,LinkDat,pLinkDat,
     &  TL,TempL,normCrRho,normCrMu,
     &  DpProf,RhoProfF,RhoProfT,ProfPtr,IrfList,vMet,rhoOut,
     &  FVold,FVOldDefined)

C***********************************************************************
C Purpose: steering program for the solver
C
C Changes:
C@lbl dml 2000jan12 Original version of subroutine.
C
C Limits : N/A
C
C Pass parameters:
C
C IO name        unit     description
C I  nZ          -        Actual number of zones
C I  nL          -        Actual number of links
C I  LStat(*)    -        Link status= see main
C I  LDat(*)     multi    Link data coefficients (K,n,...)
C I  pLiLDat(*)  -        Link leakage pointer to start element in LDa
C I  DpL(2,*)    Pa       Extra pressures per link
C I  FromTo(2,*) -        Node numbers for each link
C I  Mf(*)       -        Multiplication for mass flow
C I  Mp(*)       -        Multiplication for pressure
C I  epsFA       kg/s     Break limit for flow balances
C I  epsFR       -/-      Break limit for relative flow imbalance
C IO RhoL(2,*)   kg/m3    Air density per link,pos and neg direction
C IO SqrRhoL(2,*)         SQRT of RhoL
C IO MuL(2,*)    Pa.s     Dynamic viscosity per link,pos and neg dir
C  O Pz(*)       Pa       Current pressure for each zone
C  O Fv(*)       kg/s     Current mass flow for each link
C    Fv2(2,*)    kg/s     Two way flow of large opening link
C  O Ft(*)       kg/s     Total flow into each zone
C  O Fmb(*)      kg/s     Mass flow balance per zone
C  O nIter       -        Number of iterations (Jacobian factorizations)
C  O key         -        Error key, Error=1  OK=0
C I  difLim      Pa       Limit for crack flow linearization
C    fmFact      ?        Flow*fmFact=userunits
C I  mIter       -        Maximum number of iterations allowed
C    DMZ         kg/s     Changes of the Mass per Zone, (ie temperature)
C I  stpInit     -        Method for initializing current time step.
C                          0=zero pressures,1=take one iteration on
C                          system with linear flow models,2=retain
C                          pressures from previous time step. Assume
C                          routine not called with stpInit=2 on the
C                          first time step.
C I  rhoSync     -        Rule for updating zone densities within time step
C                          False=fix density using initial zone
C                          pressure,True=adjust density as zone pressure
C                          guess changes.
C    RSqrtDff             Reciprocal of the square root of the Darcy
C                          friction factor
C I  nJunc       -        Number of T-junctions
C I  JuncNr(maxZ,5)-      Array with T-junction data
C  O DpJ         Pa       Extra pressure from T-junction per link
C I  LinkDat     multi    Additional data arry for individual links
C I  pLinkDat    -        Pointer array for LinkDat
C O  TL(MaxL)    0C       Mean temperatures of the air in the links
C I  TempL(2,MaxL) 0C     Air temperature in from and to zone at link
C                          height
C I  normCrRho   kg/m^3   Air density at crack test conditions
C I  normCrMu    Pa.s     Dynamic air viscosity at crack test conditions
C I  DpProf      Pa       Array with differential pressure profile for
C                          large Openings
C I  RhoProfF    kg/m3    Array with density profile in FROM zone
C I  RhoProfT    kg/m3    Array with density profile in TO zone
C I  ProfPtr     -        Pointer from link number to the start of the
C                          corresponding profile (differential pressure
C                          and density)
C I  IrfList(maxL) -      List of reference links for RF component
C    vMet
C    rhoOut
C
C ERROR return if: Singular matrix
Ch***********************************************************************

      implicit none

      include 'comv-uni.inc'
      include 'comv-par.inc'

C     ! Arguments.
      integer nZ,nL,LStat(maxL)
      real LDat(maxd)
      integer pLiLDat(maxL)
C@empa aw 2002dec04 DpL enlarged from DpL(2,MaxL) to DpL(3,MaxL) to have a space
C@empa aw 2002dec04 for the final pressure differnce
      double precision DpL(3,maxL)
      integer FromTo(2,maxL)
      real Mf(maxL),Mp(maxL),epsFA,epsFR,
     &   RhoL(2,maxL),SqrRhoL(2,maxL),MuL(2,maxL)
      double precision Pz(maxZ)
      real Fv(maxL),Fv2(2,maxL),Ft(maxZ)
      double precision Fmb(maxZ)
      integer nIter,key
      real difLim,fmFact
      integer mIter
      real DMZ(maxZ)
      integer stpInit
      logical rhoSync
      real RSqrtDff(maxL)
      integer nJunc,JuncNr(maxZ,5)
      real DpJ(maxL),LinkDat(maxd)
      integer pLinkDat(maxL)
      real TL(maxL),TempL(2,maxL),normCrRho,normCrMu
      double precision DpProf(maxLo*(NrInt+2))
      real RhoProfF(maxLo*(NrInt+2)),RhoProfT(maxLo*(NrInt+2))
      integer ProfPtr(maxL),IrfList(maxL)

      real vMet,rhoOut
C@empa aw 2000mar15  FVoldDefined added
      logical FVoldDefined
C     ! Local variables.
      double precision Jac(maxZ,maxZ),PzOld(maxZ),Corr(maxZ)
      real FvOld(maxL)
      integer iFail,i
C     ! zonenumber & L for error report if matrix is singular.
      character*5 zonenumber

C     ! Cost function (r-square) at current and next iterate.
      double precision cfk,cfkp1a,cfkp1b
C     ! Trust length and length of Newton-Raphson step.
      double precision trustLen,stpLenNr
C     ! Relaxation coefficient
      double precision dampa,dampb
C     ! Helper variables.
      double precision temp1,temp2,c1,c2
      logical linInit

C     ! Function declarations.
      logical FLWCNV
C-----

C     ! Initialize counters.
      nIter = 0


C     !   Linear initialization if necessary. Note routine PRECAL()
C     ! already zeros Pz when stpInit=0 or 1.
      if( stpInit .eq. 1 ) then
         ! Evaluate residuals.
         linInit = .true.
         call MATVEC(nZ,nL,Jac,Fmb,
     &     Pz,Fv,FvOld,Fv2,Ft,DpL,FromTo,
     &     LStat,Mf,Mp,pLiLDat,LDat,RhoL,
     &     SqrRhoL,MuL,linInit,difLim,fmFact,DMZ,RsqrtDff,
     &     nJunc,JuncNr,DpJ,LinkDat,pLinkDat,
     &     TL,TempL,normCrRho,normCrMu,
     &     DpProf,RhoProfF,RhoProfT,ProfPtr,IrfList,vMet,rhoOut,
C@NBI PGS 1999Aug19 - Added argument 'FVOldDefined'
     &     FVOldDefined)

C        ! Step to solution of linearized system.
         call LINSLV(Jac,Fmb,Corr,maxZ,nZ,iFail,key)
         nIter = 1
         do i=1,nZ
            Pz(i) = Pz(i) + Corr(i)
         ENDDO
C        ! Update densities if necessary.
         if( rhoSync ) call PreRho
      endif


C     !   Set up for first step. Initialize control variables, residuals,
C     ! and cost function at current guess. If current guess meets
C     ! convergence test, do not continue.
      trustLen = -1.0
      linInit = .false.
      call MATVEC(nZ,nL,Jac,Fmb,
     &   Pz,Fv,FvOld,Fv2,Ft,DpL,FromTo,
     &   LStat,Mf,Mp,pLiLDat,LDat,RhoL,
     &   SqrRhoL,MuL,linInit,difLim,fmFact,DMZ,RsqrtDff,
     &   nJunc,JuncNr,DpJ,LinkDat,pLinkDat,
     &   TL,TempL,normCrRho,normCrMu,
     &   DpProf,RhoProfF,RhoProfT,ProfPtr,IrfList,vMet,rhoOut,
C@NBI PGS 1999Aug19 - Added argument 'FVOldDefined'
     &     FVOldDefined)
      if( FLWCNV(nZ,Fmb,Ft,epsFA,epsFr) ) goto 200
      cfkp1b = 0.0D0
      do i=1,nZ
         cfkp1b = cfkp1b + Fmb(i)*Fmb(i)
      ENDDO
      goto 101


C     ! Update trust length for next iteration.
100   continue
      dampb = (1.0-dampb)*(1.0-dampb)
      if( cfkp1b .le. (0.25+0.75*dampb)*cfk ) then
         trustLen = 2.0*trustLen
      else if( cfkp1b .gt. (0.9+0.1*dampb)*cfk ) then
CC    else if( cfkp1b .gt. (0.75+0.25*dampb)*cfk ) then
         trustLen = 0.5*trustLen
      endif


C     !   Start search from new iterate. Here, assume have flow data
C     ! (Fmb, Ft) and Jacobian (Jac) evaluated at current pressure
C     ! vector (Pz). Use dampa to mark fact that just started new
C     ! iteration. Note cost function at current iterate is cf from
C     ! end of last step.
101   continue
      dampa = -1.0
      cfk = cfkp1b

C     ! Print if necessary.
      if( test.ge.1 .and. secho .ge. 3 ) then
         write(CRT,*) 'zone  p         Fmb'
         do i=1,nZ
            write(CRT,'(1X,I4,2(1X,E10.3))') i,Pz(i),Fmb(i)
         ENDDO
      endif

C     !   Find Newton-Raphson step, check for singular matrix, and
C     ! find NR step length.
      call LINSLV(Jac,Fmb,Corr,maxZ,nZ,iFail,key)
      nIter = nIter + 1
      if( iFail .ne. 0 ) then
         zonenumber=' '
         call IntDis(iFail,zonenumber,i)
C@NBI PGS 1999Aug04 - Zone number was mistakenly written on next line
CC   call error2
CC   &('Error in the ventilation part! MATRIX SINGULAR FOR ZONE # ',
      call error
     &('Error in the ventilation part!  MATRIX SINGULAR FOR ZONE '// 
C@empa aw 2001aug29 change severity, don't stop.
CC     &     zonenumber(1:i),3)
     &     zonenumber(1:i),2)
         return
      endif
      stpLenNr = 0.0D0
      do i=1,nZ
         stpLenNr = stpLenNr + Corr(i)*Corr(i)
      ENDDO
      stpLenNr = sqrt( stpLenNr )

C     ! Back up node pressures for later use.
      do i=1,nZ
         PzOld(i) = Pz(i)
      ENDDO


C     !   Evaluate current step. Take step, find residuals, and
C     ! perform acceptance testing. Note do not test step for stagnation
C     ! (function PRSSTG), but do enforce a minimum damping coefficient
C     ! below.
102   continue
      if( (trustLen .ge. stpLenNr) .or. (trustLen .le. 0.0) ) then
C        ! Take full NR step.
         trustLen = stpLenNr
         dampb = 1.0D0
         do i=1,nZ
            Pz(i) = PzOld(i) + Corr(i)
         ENDDO
      else
C        ! Take damped NR step.
         dampb = trustLen / stpLenNr
         do i=1,nZ
            Pz(i) = PzOld(i) + dampb*Corr(i)
         ENDDO
      endif

C     ! Find flows, sum of flows, and Jacobian at current (dampb) step.
      if( rhoSync ) then
C        ! Update air densities to match current pressures.
         call PreRho
      endif
      call MATVEC(nZ,nL,Jac,Fmb,
     &   Pz,Fv,FvOld,Fv2,Ft,DpL,FromTo,
     &   LStat,Mf,Mp,pLiLDat,LDat,RhoL,
     &   SqrRhoL,MuL,linInit,difLim,fmFact,DMZ,RsqrtDff,
     &   nJunc,JuncNr,DpJ,LinkDat,pLinkDat,
     &   TL,TempL,normCrRho,normCrMu,
     &   DpProf,RhoProfF,RhoProfT,ProfPtr,IrfList,vMet,rhoOut,
C@NBI PGS 1999Aug19 - Added argument 'FVOldDefined'
     &   FVOldDefined)

C     !   Check termination conditions. Note in order to avoid useless
C     ! checks of residual vector elements, assume that a strongly
C     ! damped step did not solve the problem.
      if( dampb .gt. 0.75 ) then
         if( FLWCNV(nZ,Fmb,Ft,epsFA,epsFr) ) goto 200
      endif
      if( nIter .gt. mIter ) then
         key=99
         call error('Ventilation network solver reached maximum '//
     &     'number of Jacobian factorizations. Increase '//
     &     '&-PR-CONTrol Miter, or adjust the network.',2)
         goto 200
      endif

C     ! Find cost function.
      cfkp1b = 0.0D0
      do i=1,nZ
         cfkp1b = cfkp1b + Fmb(i)*Fmb(i)
      ENDDO

C     ! Step acceptance testing.
      if( (dampa .gt. 0.0) .and. (dampa .lt. dampb) ) then
C        !   Current step uses longer trust length than a previous
C        ! successful step during this iteration.
         if( cfkp1a .le. cfkp1b ) then
C           ! Longer step gave worse cost function. Reinstate old step.
            trustLen = dampa*stpLenNr
            dampa = 2.0
            goto 102
         else if( (dampb .le. 0.7) .and.
     &      (abs((1-dampb)*(1-dampb)*cfk-cfkp1b).le.0.1*(cfk-cfkp1b)) )
     &      then
C           ! Good reduction, so increase trust length again.
            dampa = dampb
            cfkp1a = cfkp1b
            trustLen = 2*trustLen
            goto 102
         else
C           !   Longer step gave better cf, but not good enough to
C           ! increase length again. Start next iteration.
            goto 100
         endif
      else if( cfkp1b .le. (1.0-2E-4*dampb)*cfk ) then
C        ! Nonincreased step gives sufficient decrease in cost function.
         if( dampa > dampb ) then
C           !   Step results from backtracking, or from rejecting an
C           ! increased trust length. Accept with no further testing.
            goto 100
         else if( (dampb .le. 0.7) .and.
     &      (abs((1-dampb)*(1-dampb)*cfk-cfkp1b).le.0.1*(cfk-cfkp1b)) )
     &      then
C           ! Good reduction in cost function, so increase trust length.
            dampa = dampb
            cfkp1a = cfkp1b
            trustLen = 2*trustLen
            goto 102
         else
C           !   Step passes acceptance test but does not warrant
C           ! increasing trust length for this iteration. Accept step and
C           ! move on to next iteration.
            goto 100
         endif
      else if( dampb .le. 1E-6 ) then
C        !   Step did not pass acceptance test, but falls below
C        ! minimum allowed damping fraction. Accept step despite
C        ! possibly worse cost function.
         goto 100
      endif


C     !   Here, current (dampb) step did not meet any acceptance
C     ! criterion. Backtrack by interpolating parabola or cubic to
C     ! known cost function point(s) and slope.
      if( dampa .le. 0.0 ) then
C        ! Quadratic fit.
         temp1 = dampb*cfk / (cfkp1b-cfk+2*dampb*cfk)
      else
C        ! Cubic fit.
         temp1 = (cfkp1a-cfk+2*dampa*cfk) / (dampa*dampa)
         temp2 = (cfkp1b-cfk+2*dampb*cfk) / (dampb*dampb)
         c1 = (temp1-temp2) / (dampa-dampb)
         c2 = (dampa*temp2-dampb*temp1) / (dampa-dampb)
         if( c1 .eq. 0.0 ) then
            temp1 = cfk / c2
         else
            temp1 = c2*c2 + 6*c1*cfk
            if( temp1 .lt. 0.0 ) then
               temp1 = 0.0
            else
               temp1 = (sqrt(temp1)-c2) / (3.0*c1)
            endif
         endif
         temp1 = temp1 / dampb
      endif
C     ! Force fraction of last length onto [0.1, 0.5].
C@NBI PGS 2000Jul21 - Must have consistent argument data types for intrinsics
CC    temp1 = max( temp1, 0.1 )
CC    temp1 = min( temp1, 0.5 )
      temp1 = max( temp1, 1.0D-1 )
      temp1 = min( temp1, 5.0D-1 )
      trustLen = temp1*trustLen
C     ! Store information about rejected step, then take shorter one.
      dampa = dampb
      cfkp1a = cfkp1b
      goto 102


C     ! Termination condition met.
200   continue
      if( test.ge.1 .and. secho.ge.1 ) then
         write(CRT,'(1X,I4)') nIter
      endif

      return
      end
C     ! End subroutine SLV01().


Ch***********************************************************************

      subroutine STRSLV(nZ,nL,LStat,
     &  LDat,pLiLDat,DpL,FromTo,Mf,Mp,epsFA,epsFR,epsCJ,
     &  RhoL,SqrRhoL,MuL,Pz,Fv,Fv2,Ft,Fmb,
     &  nIter,key,nNewt,difLim,relaxV,slvSel,
     &  fmFact,mIter,DMZ,stpInit,rhoSync,RSqrtDff,
     &  nJunc,JuncNr,DpJ,LinkDat,pLinkDat,
     &  TL,TempL,normCrRho,normCrMu,
     &  DpProf,RhoProfF,RhoProfT,ProfPtr,IrfList,vMet,rhoOut,
C@NBI PGS 1999Aug19 - Bugfix. FVOLD added as new argument so that it is saved
C@NBI               - Also added argument 'FVOldDefined'.  Read why in
C@NBI                 subroutine 'MatVec' and 'NetSlv'
     & FVold,FVOldDefined)

C***********************************************************************
C Purpose: steering program for the solver
C
C Changes:
C@lbl dml 1999dec20 Removed all obsolete lines. Cleaned up format.
C@lbl dml 2000jan06 Enforce only solvers 1,3,5 allowed. Move code
C   fragment for slvSel=5 (most likely choice) above slvSel=3 in
C   the if-elseif tree. Note removing slvSel=4 allows removal of local
C   variables n1 and c1. Removing slvSel=0 allows removal of local
C   variable relaxO.
C
C Limits : N/A
C
C Pass parameters:
C
C IO name        unit     description
C I  nZ          -        Actual number of zones
C I  nL          -        Actual number of links
C I  LStat(*)    -        Link status= see main
C I  LDat(*)     multi    Link data coefficients (K,n,...)
C I  pLiLDat(*)  -        Link leakage pointer to start element in LDa
C I  DpL(2,*)    Pa       Extra pressures per link
C I  FromTo(2,*) -        Node numbers for each link
C I  Mf(*)       -        Multiplication for mass flow
C I  Mp(*)       -        Multiplication for pressure
C I  epsFA       kg/s     Break limit for flow balances
C I  epsFR       -/-      Break limit for relative flow imbalance
C I  epsCJ       Pa       Break limit for pressures
C IO RhoL(2,*)   kg/m3    Air density per link,pos and neg direction
C IO SqrRhoL(2,*)         SQRT of RhoL
C IO MuL(2,*)    Pa.s     Dynamic viscosity per link,pos and neg dir
C  O Pz(*)       Pa       New pressure for each zone
C  O Fv(*)       kg/s     New mass flow for each link
C    Fv2(2,*)    kg/s     Two way flow of large opening link
C  O Ft(*)       kg/s     Total flow into each zone
C  O Fmb(*)      kg/s     Mass flow balance per zone
C  O nIter       -        Number of iterations
C  O key         -        Error key, Error=1  OK=0
C I  nNewt       -        Number of Newton Raphson iterations before
C                          looking at the optimal relaxation factor
C I  difLim      Pa       Limit for crack flow linearization
C I  relaxV       -       Value for Relaxation coeff given in inputfile
C I  slvSel       -       Solver Selector 0=optimum relax Herrlin,
C                          1=Newton,2=Steffensen,3=Walton-Steff.,
C                          4=avg.Steff
C    fmFact      ?        Flow*fmFact=userunits
C I  mIter       -        Maximum number of iterations allowed
C    DMZ         kg/s     Changes of the Mass per Zone, (ie temperature)
C I  stpInit     -        Method for initializing current time step.
C                          0=zero pressures,1=take one iteration on
C                          system with linear flow models,2=retain
C                          pressures from previous time step. Assume
C                          routine not called with stpInit=2 on the
C                          first time step.
C I  rhoSync     -        Rule for updating zone densities within time step
C                          False=fix density using initial zone
C                          pressure,True=adjust density as zone pressure
C                          guess changes.
C    RSqrtDff             Reciprocal of the square root of the Darcy
C                          friction factor
C I  nJunc       -        Number of T-junctions
C I  JuncNr(maxZ,5)-      Array with T-junction data
C  O DpJ         Pa       Extra pressure from T-junction per link
C I  LinkDat     multi    Additional data arry for individual links
C I  pLinkDat    -        Pointer array for LinkDat
C O  TL(MaxL)    0C       Mean temperatures of the air in the links
C I  TempL(2,MaxL) 0C     Air temperature in from and to zone at link
C                          height
C I  normCrRho   kg/m^3   Air density at crack test conditions
C I  normCrMu    Pa.s     Dynamic air viscosity at crack test conditions
C I  DpProf      Pa       Array with differential pressure profile for
C                          large Openings
C I  RhoProfF    kg/m3    Array with density profile in FROM zone
C I  RhoProfT    kg/m3    Array with density profile in TO zone
C I  ProfPtr     -        Pointer from link number to the start of the
C I                        corresponding profile (differential pressure
C                          and density)
C I  IrfList(maxL) -      List of reference links for RF component
C    vMet
C    rhoOut
C
C ERROR return if: Singular matrix
Ch***********************************************************************

      implicit none

      include 'comv-uni.inc'
      include 'comv-par.inc'

C     ! Arguments.
      integer nZ,nL,LStat(maxL)
      real LDat(maxd)
      integer pLiLDat(maxL)
C@empa aw 2002dec04 DpL enlarged from DpL(2,MaxL) to DpL(3,MaxL) to have a space
C@empa aw 2002dec04 for the final pressure differnce
      double precision DpL(3,maxL)
      integer FromTo(2,maxL)
      real Mf(maxL),Mp(maxL),epsFA,epsFR,epsCJ,
     &   RhoL(2,maxL),SqrRhoL(2,maxL),MuL(2,maxL)
      double precision Pz(maxZ)
      real Fv(maxL),Fv2(2,maxL),Ft(maxZ)
      double precision Fmb(maxZ)
      integer nIter,key,nNewt
      real difLim,relaxV
      integer slvSel
      real fmFact
      integer mIter
      real DMZ(maxZ)
      integer stpInit
      logical rhoSync
      real RSqrtDff(maxL)
      integer nJunc,JuncNr(maxZ,5)
      real DpJ(maxL),LinkDat(maxd)
      integer pLinkDat(maxL)
      real TL(maxL),TempL(2,maxL),normCrRho,normCrMu
      double precision DpProf(maxLo*(NrInt+2))
      real RhoProfF(maxLo*(NrInt+2)),RhoProfT(maxLo*(NrInt+2))
      integer ProfPtr(maxL),IrfList(maxL)
      real vMet,rhoOut
      real FVOLD(maxl)
C@NBI PGS 1999Aug19 - Added argument 'FVOldDefined'
      logical FVOldDefined

C     ! Local variables.
      double precision Jac(maxZ,maxZ),PzOld(maxZ),Corr(maxZ),relaxD
CC    integer iFail,i,n,N1,L
      integer iFail,i,n,L
C     ! SFmbN = the sum of the absolute values of     Fmb (1..nZ)
C     ! SFt   = the sum of of the total flow in and out per zone
      real SFmbN,SFt
C     ! conv0 = relative convergence of previous step
C     ! conv1 = current relative convergence
      real conv0,conv1
C     ! accel = Flag to go for Steffensen corrections
      integer accel
C     ! PCorr = Previous pressure correction factors (Pa)
C     ! CEF   = Steffensen correction factor on Corr
      real PCorr(maxZ),CEF(maxZ)
CC    real convR,C,C1,relaxO
      real convR,C
C     ! zonenumber & L for error report if matrix is singular.
      character*5 zonenumber
      logical linInit
C     ! Cost function (r-square) at next iterate.
      double precision cfkp1b

C     ! Function declarations.
      logical FLWCNV, PRSSTG
C-----
C@NBI PGS 2000Aug17 - Bugfix:  conv1 not initialized, so crashes on some
C@NBI                 compilers.  Use DATA statement because must be
C@NBI                 initialized only for 1st call; also need to SAVE it
C@NBI               - Bugfix:  "SAVE convR" reinstated.  It had probably
C@NBI                 accidentally been deleted when this routine was
C@NBI                 revamped for COMIS version 3.1
      DATA conv1/0.0/
      SAVE conv1,convR
CC    if( slvSel .eq. 0 ) then
CC       call error('Solver 0 not working',3)
CC    else if( slvSel .eq. 2 ) then
CC       call error('Solver 2 not available',3)
CC    endif
      if( slvSel.ne.1 .and. slvSel.ne.3 .and. slvSel.ne.5 ) then
         call error('STRSLV requires slvSel=1,3,5',3)
      endif

C     ! Initialize. Note routine PRECAL() already zeros Pz when
C     ! stpInit=0 or 1.
      nIter = 0
      accel = 0
      relaxD = relaxV
      if( stpInit .eq. 1 ) then
         linInit = .true.
      else
         linInit = .false.
      endif
      do i=1,nZ
         PCorr(i) = 0.0
         cef(i) = 0.0
      ENDDO

C--------------------------
C     Top of iteration loop
C--------------------------

10    continue

      call MATVEC(nZ,nL,Jac,Fmb,
     &  Pz,Fv,FvOld,Fv2,Ft,DpL,FromTo,
     &  LStat,Mf,Mp,pLiLDat,LDat,RhoL,
     &  SqrRhoL,MuL,linInit,difLim,fmFact,DMZ,RsqrtDff,
     &  nJunc,JuncNr,DpJ,LinkDat,pLinkDat,
     &  TL,TempL,normCrRho,normCrMu,
     &  DpProf,RhoProfF,RhoProfT,ProfPtr,IrfList,vMet,rhoOut,
C@NBI PGS 1999Aug19 - Added argument 'FVOldDefined'
     &  FVOldDefined)

C     ! Check termination conditions for noninitializing steps.
      if( .not.linInit ) then
         if( FLWCNV(nZ,Fmb,Ft,epsFA,epsFR) ) then
            goto 200
         else if( nIter .gt. mIter ) then
            key=99
            call error('The maximum number of iteration steps has '//
     &      'been reached in the ventilation network solver. You may'//
     &      ' increase &-PR-CONTrol mIter or adjust the network.',2)
            goto 200
         endif
      endif

C     ! Find cost function.
      cfkp1b = 0.0D0
      do i=1,nZ
         cfkp1b = cfkp1b + Fmb(i)*Fmb(i)
      ENDDO

C     ! Find flow sums.
      SFmbN = 0.0
      do i=1,nZ
         SFmbN = SFmbN + abs( Fmb(i) )
      ENDDO
      SFt = 0.0
      do i=1,nZ
         SFt = SFt + Ft(i)
      ENDDO
C     ! Avoid division by zero when SFt = SFmbN = 0.
      if( SFt .eq. 0.0 ) SFt=1.0

      call LINSLV(Jac,Fmb,Corr,maxZ,nZ,iFail,key)
      nIter=nIter+1

      if( test .ge. 1 ) then
         if( secho .ge. 3 ) then
            write(CRT,*) 'zone  p         Fmb   Corr'
            do i=1,nZ
               write(CRT,1001) i,Pz(i),Fmb(i)*fmFact,Corr(i)
            ENDDO
1001        format( 1X,I4,3(1X,E10.3) )
         endif
            if( secho .ge. 5 ) then
               write(CRT,*) 'diagonal'
               do i=1,nZ
                  write(CRT,*) Jac(i,i)
               ENDDO
            endif
         endif

C     ! Test for singular matrix.
      if( iFail .ne. 0 ) then
         zonenumber=' '
         call IntDis(iFail,zonenumber,L)
         call error2
     &    ('Error in ventilation part- MATRIX SINGULAR FOR ZONE # ',
     &    zonenumber(1:L),3)
         return
      endif

C     ! Save new pressures as old.
      do i=1,nZ
         PzOld(i) = Pz(i)
      ENDDO

C     ! Choose relaxation rule. Note solver selection has been
C     ! reduced to 1,3,5 (checked in comv-inp.f).
      if( linInit ) then
C        ! Pure Newton-Raphson.
C@empa aw 2000nov09 reset linInit at the end of iteration loop
CC         linInit = .false.
         call NEWPR(nZ,Pz,PzOld,Corr,1.0D0)
      else if( slvSel.eq.1 .or. nIter.le.nNewt
     &   .or. (stpInit.eq.1 .and. nIter.le.nNewt+1) ) then
C        ! Pure Newton-Raphson or fixed relaxation parameter.
         if( relaxV.gt.0.0 .and. relaxV.le.1.0 ) then
            call NEWPR(nZ,Pz,PzOld,Corr,relaxD)
         else
            call NEWPR(nZ,Pz,PzOld,Corr,1.0D0)
         endif
      else if( slvSel .eq. 5 ) then
C        ! Walton- two relaxation factors
         conv0 = conv1
         conv1 = SFmbN/SFt
         relaxD = 1.0D0
C@empa aw 2000jul10 prevent from division by zero
         if (conv0.eq.0.)then
            relaxD=0.5
         else
            if( nIter .eq. nNewt+2 ) then
               convR = conv1/conv0
            endif
            if( nIter .ge. nNewt+2 ) then
               if( convR .gt. conv1/conv0 ) then
C                 ! New convergence is better (smaller).
                  convR = conv1/conv0*0.3+0.7*convR
               else
                  convR = conv1/conv0*0.9+0.1*convR
               endif
               if( convR .gt. 0.7 ) then
                  relaxD=0.50D0
               else if( convR .gt. 0.3 ) then
                  relaxD=0.75D0
               endif
            endif
         endif
         do n=1,nZ
            Pz(n) = Pz(n)+relaxD*Corr(n)
         ENDDO
      else if( slvSel .eq. 3 ) then
C        ! Walton-Steffensen per zone.
         conv0 = conv1
         conv1 = SFmbN/SFt
         if( accel .eq. 1 ) then
            accel=0
         else
            if( nIter.gt.2 .and. conv1.gt.0.3*conv0 ) accel=1
         endif
         do n=1,nZ
            CEF(n) = 1.0
            if( accel .eq. 1 ) then
               c = Corr(n)/PCorr(n)
               if (c.lt. 0.5) CEF(n) = 1.0/(1.0-C)
               C = Corr(n)*CEF(n)
            else
               PCorr(n) = Corr(n)
               C = Corr(n)
            endif
C           ! Note Corr already has the - sign
            Pz(n)=Pz(n)+C
         ENDDO
CC       else if( slvSel .eq. 0 ) then
CCC         ! Optimum relaxation according to Herrlin.
CC          call Relax(nZ,nL,PzOld,Pz,
CC   &      difLim,DpL,FromTo,LStat,Mf,Mp,pLiLDat,LDat,RhoL,
CC   &      SqrRhoL,MuL,Corr,fmFact,DMZ,linInit,relaxD,relaxO,RSqrtDff)
CC       else if( slvSel .eq. 4 ) then
CCC         ! Newton-one average Steffensen
CC          conv0 = conv1
CC          conv1 = SFmbN/SFt
CC          C = 1.0
CC          if( nIter.gt.2 .and. conv1.gt.0.5*conv0 ) then
CC             C=0.0
CC             N1=0
CC             do 100 n=1,nZ
CC                c1=Corr(n)/PCorr(n)
CCC               ! Only take the oscillations (the negative ratio Corr/PCorr).
CC                if( c1 .lt. 0.0) then
CC                   c = c+c1
CC                   n1 = n1+1
CC                endif
CC100          continue
CC             if( n1 .gt. 0 ) then
CC                C = C/N1
CC             else
CC             C = 1.0
CC          endif
CC          if( c .lt. 0.5 ) then
CC             C = 1.0/(1.0-C)
CC          else
CC             C = 1.0
CC          endif
CC       endif
CC       do 120 n=1,nZ
CC          Pz(n) = Pz(n)+C*Corr(n)
CC          PCorr(n) = Corr(n)
CC120    continue
CC       relaxD=C
      endif

C     ! Check for stagnation in pressure vector.
      if( epsCJ .gt. 0.0 ) then
C@empa aw 2000nov09 dont stop with linear initialisation
CC         if( PRSSTG(nZ,Pz,PzOld,epsCJ) ) then
         if(( PRSSTG(nZ,Pz,PzOld,epsCJ) ).and..not.linInit) then
C@NBI PGS 2000Aug18 - No need to write this info out unless the user
C@NBI                 wants to know.  Use SECHO to decide whether this
C@NBI                 is to be written         
CC          write(COF,'(A)') 'Stagnant pressure vector'
            IF(secho.GT.0) write(COF,*) 'Stagnant pressure vector'
            goto 200
         endif
      endif

C     ! Optionally update air densities to match current pressures
      if( rhoSync )  call PreRho

C     ! Continue iterative search.
C@empa aw 2000nov09 reset linInit
      if (linInit) linInit=.false.
      goto 10


200   continue
      if( test.ge.1 .and. secho.ge.1 )  write(CRT,'(1X,I4)') nIter

      return
      end
C     ! End subroutine STRSLV().
