C+*********************************************************** comv-gr2.f
C@tno jcp 1996Apr04_14:26:24 unfortunately ran into a steady BM_fixxup error
C here is the second part of comv-gro

Ch***********************************************************************
      SUBROUTINE MkTab(line,indx,part)

C pass parameter # =   1    2    3
C***********************************************************************
C Purpose: Create one or more lines for  *.COS files as described in the
C          documentation; this is done by parsing the string 'line'
C          which contains either:
C          (a) a definition for a table entry, such as "PZ-T 1-3,4", or
C          (b) a definition of which objects to store, such as "PZ-S 1-3,4".
C
C IO # Name       Units      Description
C I  1 line       [-]        specification of outputfile from *.CIF
C I  2 indx       [-]        pointer to KeyW and COSlun
C I  3 part       [-]        see below:
C
C      part = 0 => Test run to parse 'line' and check for and display
C                  errors.  Done only once, before the time loop in
C                  COMIS, to avoid same error-message everty time step.
C      part = 1 => Create first part of table (scaling etc.), for type (a) above.
C      part = 2 => Create index-line, for type (b) above.
C      or   = 3 => Create second part of table (data).
C      part = 4 => Write KeyW(Nr) and Link/Zone(Nr) into IOOptT array.
C      part = 5 => Write KeyW(Nr) and Link/Zone(Nr) into IOOptH array.
C
C Limits :
C
C Example: call MkTab(line,i,1)
C
C Uses   : SkipSpc, ReadNum
C
C ERROR RETURN IF: none
C
C July 1990 wpk
C Module : GrOut.FOR
C Changes:
C@empa hf/aw 1995mar15 New version with many changes:
C                       - Old markers deleted
C                       - New part 4 for preparing arrays for meanvalue
C                         calculation
C                       - Error message when not finding a specified link or
C                         zone name put into the subroutine readnum
C                       - Unit conversion to user output units added
C                       - Direction sign '<' or '>' added at keyword FL to
C                         give the possibility to output the two way flow.
C                         ReadNum gives back the new parameter FlowDir (directio
C                         which is used as new input parameter in GetData
C                       - Getting the different link- or zone names with the
C                         new subroutine GetLiZoNa
C@tno jcp 1996Mar29_16:03:00 added section for Histograms
C@tno jcp 1996Mar29_17:59:03 readNum returns occupants as negative numbers
C@tno jcp 1996Apr01_10:57:55 added for histograms part=5 in MkTab
C@tno jcp 1996Apr02_10:09:38 UnitNr..Nclass new integers
C@tno jcp 1996Apr07_11:45:59 if C1..n store the concentration number in HistUsed
C@tno jcp 1996Apr12_07:59:52 for occupants we always take the first pollutant
C@tno     for NormConc and SourceStrength and the calculation of the NormFlow
C@tno     Also for EF the first pollutant is taken
C@tno jcp 1996Apr25_15:08:48 flag HistConc to be able to call HistCalc from Poll
C@tno jcp 1996Jun14_00:14:41 variab for GetWI
C@tno jcp 1996Jul16_17:50:36 keep in mind that UnitPconc=8 for strings, (
C@tno     UnitPolCo+ipol) is used for ofact and ifact. UnitPolCo = 17-1 = 16
C@empa aw 1999Nov1  Get the multiplication factor to user units (ABS to remove the - of OSR)
C@empa aw 1999nov10 HystType=0 produces Fortran error: array bounds exceeded
C@                  I'm not quite shure whether I may set HystType=1 here
C@empa aw 1999nov30 This change is obsolet now with the change from nov30 in GetOccConc
C@empa aw 1999Nov1 output zone pressure with outside pressure at zone level when OSR is referenced
C@empa aw 1999Nov22 PGS debugged that in his version on 1999may05 in GetData
C@empa aw 1999dec07 Check for a valid Histogramm type
C@empa aw 1999dec07 endif for valid Histogramm types
C@empa aw 1999dec08 give message also if part 4 or 5
C@empa aw 2000jan19 Moved initialization (zeroing) of arrays used in Part 4
C                   to IniDat where also the arrays for the histograms are
C                   initialized.
C@empa aw 2000apr04 FlowDir is now parameter of GetLiZoNa the differentiation is now
C                   made there
C@empa aw 2000apr05 Many changes:
C                   - Enable ext. node ID for concentration output.
C                   - Warning if the ID is defined in NET-EXT and NET-Zones.
C                   - Better error messages and warnings if invalid output
C                     ranges are defined.
C                   - Backward definition of a range is allowed
C                   - Warning if the from and to element of a range are not the same
C                     type
C@empa aw 2000apr05 from1 added
C@empa aw 2000apr05 Errormessage if range is not consistent
C@empa aw 2000apr05 range is backwards
C@empa aw 2000apr05 move error message up
C@empa aw 2000apr11
C@NBI PGS 2000Jul16 - The routine was ripe for tidying up, to aid legibility
C@NBI                 but syntax unchanged.
C@NBI               - Moved comments up here to reduce clutter in the code.
C@NBI               - CHARACTER "xstr" removed (it wasted memory).
C@NBI               - Diplication! FUNCTION LastChar and LenStr did same thing
C@NBI               - Added error message for when the line has no names
C@NBI PGS 2000Aug02 - "i" and "j" unused so removed from decralations
Ch***********************************************************************

      IMPLICIT NONE
      INCLUDE 'comv-inp.inc'
      INCLUDE 'comv-uni.inc'

      INTEGER LenStr,IntCon,indx,idx,part,k,l,Eflag,pointer,HistType
     &   ,oldpointer,num,ipol,From,To,from1,LStr,Undx,FlowDir,dirdum
     &   ,UnitNr,Nclass
      REAL GetData, Offset, Multiply
      LOGICAL error, exit
      CHARACTER KW*3, GetLiZoNa*15, name*20, DisStr*3, line*160
C-----
C     INITIALIZE
      exit=.false.
      pointer=5
C     e.g.: PZ-T 1,2,5-7
C               ^----------pointer

      if (part.eq.5.or.part.eq.0) then
C        Histogram could be longer; move pointer
         if(line(4:4).eq.'H') then
            pointer=6
            if (index('0123456789',line(6:6)).gt.0) pointer=7
            k=4
            l=pointer
            Variab='get HistType'
            CALL GetWI(Line,k,l,Histtype,1)
         end if
      end if

C-----
C     REPEAT
100   call SkipSpc(line,pointer)

      if (pointer .gt. LenStr(line)) then
         CALL InErr('&-PR-OUTPut option '//Line(1:LenStr(Line))//
     &   ' needs a list of names of zones, occupants, links or external'
     &   //' nodes as appropriate.  Or simply delete or comment out the'
     &   //' line.',' ',.TRUE.,3)
      endif

C     Occupants always have from=to (single occupant). So never: occ1-5 but
C     occ1,occ3,occ5,occ4,occ2 or so
      oldpointer=pointer
      call readNum(line,pointer,part,error,num,FlowDir)
      if (line(pointer:pointer).eq.',') then
         name=line(oldpointer:pointer-1)
      else
         name=line(oldpointer:pointer)
      end if

C     recognize end of line
      if (pointer .ge. LenStr(line)) exit=.true.

      if (.NOT. error) then
         From=num
         call SkipSpc(line,pointer)
         if (line(pointer:pointer) .eq. '-') then
C           Range specified
            pointer=pointer+1
            call SkipSpc(line,pointer)
            if (line(pointer:pointer) .eq. ',') then
               error=.true.
            else
               call readNum(line,pointer,part,error,num,dirdum)
            endif
            if (error.and.
     &         ((part .eq. 0).or.(part.eq.4.).or.(part.eq.5))) then
               call inerr('&-PR-OUTPut:  '//
     &         'No ID found after  ''-'' !  ',' ',
     &         .True.,2)
            endif
C           recognize end of line
            if ((Flowdir.ne.Dirdum) .and.(.not.error).and.
     &         ((part .eq. 0).or.(part.eq.4.).or.(part.eq.5))) then
               call inerr('&-PR-OUTPut:  '//
     &         'The from and to element of the range '''//
     &         line(oldpointer:pointer-1)//
     &         ''' are not of the ;same type!',
     &         '',.TRUE.,2)
               error=.true.
               exit=.true.
            endif
            if (pointer .ge. LenStr(line)) exit=.true.
            if (.NOT. error) then
               To=Num
               if (from.gt.to) then
               from1=from
               from =to
               to=from1
            endif
         else
            if (part .eq. 0) then
               To=0
               exit=.true.
               error=.true.
               endif
            endif
         else
C           one object only
            To=From
         endif
      else
C        error in conversion
         exit=.true.
         error=.true.
         return
      endif

C     At this point, From and To contain correct values or exit is .true.

C-------------
C     part = 0
C-------------

C     Do nothing more !
C     (part = 0 only checks for errors)

C-------------
C     part = 1
C-------------

      if ((part .eq. 1).and.(OCase.lt.5)) then
C        increase counter for number of lines in first part
         COSline(indx)=COSline(indx)+1

C-------------
C     part = 2   Create part 2 of table (Headline)
C-------------

      else if (part .eq. 2) then
C        Create index-line for all rows
         do idx=from,to
C@empa aw 2001mar26 check OutIDX changed (message when to large)
            if (OutIDX.le.MCOSCol) then
               KW=COSrange(indx)
               COutLine(OutIDX)=GetLiZoNa(KW,IDX,FlowDir)
               OutIDX=OutIDX+1
            else
              call intdis(MCOSCol,DisStr,LStr)
              call Inerr('&-PR-OUTPut: Too many entries for the '//
     &         'keyword '//line(1:2),
     &         'Maximum allowed number of entries: '
     &         //DisStr(1:LStr),.FALSE.,3)
            endif
CC            if (OutIDX.gt.(MCOSCol+1)) return
         ENDDO

C-------------
C     part = 3   Create part 3 of table
C-------------

      else if (part .eq. 3) then
         Offset=0.0
C        For temperature, get offset; others have 0 offset
         if (KWUnitNr(indx) .eq. UnitTmul) Offset=ofact(UnitToff)
C        Get multiplier
C@tno jcp 1996Jul16_17:30:29 this is insufficient for pollutants
         Undx=KWunitNr(indx)
         if (Undx.eq.UnitPconc) then
C@NBI PGS 2000Oct08 - Can now define different I/O units for each pollutant,
C@NBI                 so changed UnitPol-1 to UnitPolCo
CC          Multiply=ofact(UnitPol+index('12345',Line(2:2))-1)
            Multiply=ofact(UnitPolCo+index('12345',Line(2:2)))
         else
            Multiply=ABS(ofact(Undx))
         end if
         do idx=from,to
C@NBI PGS 2000Aug02 - HistType is now a redundant argument after change by
C                     empa aw 2000jul03 (COMV-GR2.FOR), so can now be removed again.
CC          HistType=0
            OutLine(OutIDX)=
CC   &       GetData(COSrange(indx)(1:2),idx,FlowDir,HistType)
     &       GetData(COSrange(indx)(1:2),idx,FlowDir)
            OutLine(OutIDX)=Offset+OutLine(OutIDX)*Multiply
            OutIDX=OutIDX+1
         ENDDO

C-------------
C     part = 4   Write parameters into Array IOOprT (Mean Values)
C-------------

      else if (part .eq. 4) then
         do idx=from,to
            if (KeyWTCnt .lt. MaxOOT) then
               KeyWTCnt=KeyWTCnt+1
               IOOptT(1,KeyWTCnt)=indx
               IOOptT(2,KeyWTCnt)=idx
               IOOptT(3,KeyWTCnt)=FlowDir
            else
               call intdis(MaxOOT,DisStr,Lstr)
               call INERR('&-PR-OUTPut: Too many Output Options '//
     &         'xy-T !','Maximum '//DisStr(1:Lstr)//' allowded.',
     &         .FALSE.,3)
            end if
         ENDDO

C-------------
C     part = 5   Histograms
C-------------

      else if (part .eq. 5) then
C        This part organises the space for Nclasses with 10 extra trailing cells for
C        useful figures (see below)
         If (HistType.ne.0) then
            do idx=from,to
               if (KeyWHCnt .lt. MaxOOH) then
                  KeyWHCnt=KeyWHCnt+1
C                .So this is the sequence number of the individual Histogram
C                 at &-PR-SIMU and is probably not necessary for the histograms.
C                .Note Indx is the pointer to the keyW number, 11 for C1 etc.
                  IOOptH(1,KeyWHCnt)=indx
C                .idx is the zone/occupant number this idx must include
C                 occupants and zones. Let's say occupants are given as
C                 negative indices zones as positive indices
                  IOOptH(2,KeyWHCnt)=idx
C                .HistUsed(i,1) is start position in HistoAr(*)
                  if (HistStart.gt.MHisAr) then
                    call intdis(MHisAr,DisStr,Lstr)
                    call INERR('&-PR-OUTPut: Too large Histogram spa'//
     &              'ace -H !','Maximum '//DisStr(1:Lstr)//'elements'//
     &              ' allowded.',.TRUE.,3)
                  else
                    HistUsed(KeyWHCnt,1)=HistStart
                    HistUsed(KeyWHCnt,2)=HistType
                    if (Line(1:1).eq.'C') then
                       ipol=intcon(Line(2:2),Eflag)
                       HistUsed(KeyWHCnt,3)=ipol
                       HistConc=.TRUE.
                    else
                       HistUsed(KeyWHCnt,3)=1
                    end if

C                   Determine the start for a next histogram
C                                          Nclass +11
C                   10 extra  =  0 lower     1 higher    2 max count
C                   elements     3 average   4 SUM time  5 minimum
C                                6 maximum   7 free      8 free
C                                9 user unit offset     10 user unit multiplier
                    Nclass=HistoX(HistType,1)
                    HistUsedN(KeyWHCnt)=line(1:3)//name(1:lenstr(name))
C                  .Calculate the output UnitNr
                    Offset=0.0
                    UnitNr=KWUnitNr(indx)
C                  .For temperature, get offs<et; others have 0 offset
                    if (UnitNr .eq. UnitTmul) Offset=ofact(UnitToff)
C                  .Get multiplier
C@tno jcp 1996Mar29_13:05:40 now incorrect conversion of the concentrations
C                   they need ofact(UnitPolCo+pollutant), ie. 17,18,19,20,21
C                   while for concentrations was used here number 8
                    if (UnitNr.eq.UnitPconc) then
                      if(FlgPolDes.lt.1)
     &  call INERR('&-PR-OUTPut: Concentrations used in Histograms'//
     &  'while &-POL-DES is not found!',
     &  'Create &-POL-DES in *.CIF (i.e. see ComisMan.CIF)',
     &  .TRUE.,3)
C                     This is number 8 the wrong number, it should be
C                     16+pollutant-1.  Line(1:2) contains the C1..C5 string
C                         16                        C1..5
C@NBI PGS 2000Oct08 - Can now define different I/O units for each pollutant,
C@NBI                 so changed UnitPol-1 to UnitPolCo
CC                    Multiply=ofact(UnitPol+index('12345',Line(2:2))-1)
                      Multiply=ofact(UnitPolCo+index('12345',Line(2:2)))
                    else
                      Multiply=ofact(UnitNr)
c                     write(*,*)'UnitNr  ',UnitNr
c                     write(*,*)'Multiply',Multiply
                    end if
C                  .Conversion goes like this (in HistCalc):
C                      OldV=Offset+(OldV*Multiply)
C                   Store the conversion coefficients at the back of the the
C                   Histogram!  One Histogram type can be used for more items
C                   (NOx and NO2) and can have different conversion factors,
C                   so they must be stored in every individual histogram
C                   (could have been in HistUsed, but that is integer).
                    HistoAr(HistStart+Nclass+9)=offset
                    HistoAr(HistStart+Nclass+10)=Multiply
C                  .Initialize minimum as a large + REAL
                    HistoAr(Histstart+Nclass+5)=+REALMax
C                  .Initialize maximum as a large - REAL
                    HistoAr(Histstart+Nclass+6)=-REALMax
C                  .Move HistAtart to the start position for a next histogram,
C                   if any
                    HistStart=HistStart+Nclass+11
                  end if
               else
                  call intdis(MaxOOH,DisStr,Lstr)
                  call INERR('&-PR-OUTPut: Too many Output Options '//
     &            'xy-H !','Maximum '//DisStr(1:Lstr)//' allowded.',
     &            .FALSE.,3)
               end if
            ENDDO
            OldH=0.0
         endif
      end if
C-----

      call SkipSpc(line,pointer)
      if (line(pointer:pointer) .eq. ',') pointer=pointer+1
      if (line(pointer:pointer) .eq. ';') exit=.true.

C     loop until end of 'line'
      if ((COSline(indx) .le. TabLin) .and. (.not. exit)) goto 100

      RETURN
      end


Ch***********************************************************************
C@tno jcp 1996Apr06_22:00:13 HistType added at the end (hope that doen't
C conflict)
C@NBI PGS 2000Aug02 - HistType is now a redundant argument after change by
C                     empa aw 2000jul03, so can now be removed again.
CC      REAL FUNCTION GetData(KeyWrd,idx,FlowDir,HistType)
        REAL FUNCTION GetData(KeyWrd,idx,FlowDir)

C pass parameter # =    1    2
C***********************************************************************
C Purpose: This functions maps a keyword and it's index to the actual
C        data computed by COMIS
C        eg.: input: KeyWrd='PZ', idx=5  => output: GetData = PZ(5)
C
C July 1990 wpk
C Module : GrOut.FOR
C Changes: lbl bs april 1991  include file added. Because of that I had to
C        change the parameter name KeyW into KeyWrd.
C@empa hf/aw 1995mar15 New version with many changes:
C                       - Old markers deleted
C                       - New output options added
C                       - Changed some keywords of old output options
C                       - New parameter FlowDir added to indicate which flow
C                         (from-to or to-from) of the two way flow should be
C                         output
C
C@tno jcp 1996Mar14_13:48:02 if you make changes in function GetData would you
C then also make those changes in the routine SimuKey
C@tno jcp 1996Apr05_20:07:29 computed the number C1..5 S1..5 Q1..5 (which then
C is not limited to 5 (may be over 10)
C@tno jcp 1996Apr05_20:09:29 added occupants to be caught at C1...n
C@empa aw 2000mar08 we dont have negative idx any more. Occupants are handeled in ?? now
C@empa aw 2000mar08 EF canceled
C
C Limits :
C
C                              1..Nz 0..2     1..20
C example: result=GetData('PZ',iZone,iFlowDir,iHisType)
C          result=GetData('FL',iLink,iFlowDir,iHisType)
C iFlowDir is only used for links FL
C iHistType is only used for Histograms (C1..n, FZ, FE)
C call
C USES:
C
C IO # Name     unit          description
C I  1 KeyWrd   [-]           Keyword from *.CIF file
C I  2 idx      [-]           index within keyword (e.g. zone-number)
C I  3 FlowDir    [-]               direction of flow for keyword FL
C                                   FlowDir=0   Netto flow (FVnew(idx))
C                                   FlowDir=1   FromTo     (FV2(1,idx))
C                                   FlowDir=2   ToFrom     (FV2(2,idx))
C  O F GetData      [depends]     Data computed by COMIS, belonging to KeyWrd/idx
C
C ERROR RETURN IF: none
Ch***********************************************************************

        IMPLICIT NONE
      INCLUDE 'comv-inp.inc'
C@empa aw 2000apr20 KeyWrd may be longer if Getdata is called from HistCalc
CC      character*2 KeyWrd
      character*(*) KeyWrd

C@NBI PGS 1999Aug05 - Added variables L, From, To   (for option "LB")
C@NBI PGS 2000Aug02 - "nrooms", "weight" "iocc" unused so removed from decralations
CC      INTEGER idx,i
        INTEGER idx,i,L,From,To
C@NBI PGS 2000Aug02 - HistType is now a redundant argument after change by
C                     empa aw 2000jul03 (COMV-GR2.FOR), so can now be removed again.
CC      INTEGER FlowDir,iconc,izone,HistType,Eflag
        INTEGER FlowDir,iconc,izone,Eflag
C@NBI PGS 1999Aug06 - Added functions 'Output_Xh' & 'Enthalpy'
CC      REAL VB,Value,weight
        REAL VB,Value,Output_Xh,Enthalpy
C@empa aw 2000feb02 iFSo
        INTEGER IntCon,iFSo
C@empa aw 2000jul03 Hist
        LOGICAL Hist

C *  Values per zone:
      if (KeyWrd .eq. 'PZ') then
C       PZ = Pressure per Zone                     - KeyW(1)
        GetData=PZ(idx)
C@NBI PGS 1999May05 - Added calculations for "-OSR" option.  Gee "-OSR" is
C@NBI                 cryptic!  It stands for 'subtrcact Outside Stack
C@NBI                 pressure Reference', i.e. subtracts outside barometric
C@NBI                 pressure at same height as the zone ref. height.
C@empa aw 2004feb23 wrong sign
CC        IF(ofact(UnitP).LT.0) GetData=-Getdata-G*zz(idx)*RhoOut
        IF(ofact(UnitP).LT.0) GetData=Getdata+G*zz(idx)*RhoOut
      else if (KeyWrd .eq. 'TZ') then
C       TZ = air Temperature per Zone              - KeyW(2)
        GetData=TZ(idx)
      else if (KeyWrd .eq. 'HZ') then
C       HZ = Moisture per Zone                     - KeyW(3)
C@NBI PGS 1999Aug06 - Added possibility to output % relative humidity
C@NBI                 done simply by calling function 'Output_Xh'
CC      GetData=XhZ(idx)
        GetData=Output_Xh(Tz(idx),REAL(Pz(idx)),Xhz(idx))
      else if (KeyWrd .eq. 'FZ') then
C       FZ = Flow per Zone                         - KeyW(4)
          GetData=FT(idx)

C *  Values per link:
      else if (KeyWrd .eq. 'FL') then
C       FL = mass Flow per Link                    - KeyW(5)
          if (FlowDir.NE.0) then
             GetData=FV2(FlowDir,idx)
          else
             GetData=FVnew(idx)
          endif
      else if (KeyWrd .eq. 'TL') then
C       TL = Link Temperature                      - KeyW(6)
        GetData=TL(idx)
      else if (KeyWrd .eq. 'VL') then
C       VL = Actual value                          - KeyW(7)
        GetData=Mf(idx)
C@empa aw 2002dec04 PL added 
C       PL = Pressure difference across the link   - KeyW(55)
      else if (KeyWrd .eq. 'PL') then
        GetData=DpL(3,idx)
C@empa aw 2005aug29 DL (Cd- value per link) added  - KeyW(56)
      else if (KeyWrd .eq. 'DL') then
        GetData=Linkdat(plinkdat(idx)+3)
C *  Values per Building:
      else if (KeyWrd .eq. 'WA') then
C       WA = Wind velocity                         - KeyW(8)
        GetData=Vmet
      else if (KeyWrd .eq. 'TA') then
C       TA = Outdoor air temperature               - KeyW(9)
        GetData=Tmet
      else if (KeyWrd .eq. 'HA') then
C       HA = Outdoor air humidity                  - KeyW(10)
C@NBI PGS 1999Aug06 - Added possibility to output % relative humidity
C@NBI                 done simply by calling function 'Output_Xh'
CC      GetData=Xhmet
C@empa aw 2001jan12 Take Xhout. Xhmet is either used for Xhout or for Cout(H2O)
C@empa              ('meteo' in H2O pollutant schedule). In the latter case 
C@empa              Xhout is zero. This should be comprehensible in the output.  
CC        GetData=Output_Xh(Tmet,Pbmet-Pbz,Xhmet)
        GetData=Output_Xh(Tmet,Pbmet-Pbz,Xhout)
C *  Values per zone and gas:                        - KeyW(11-15)
C     Cn = Concentration per gas and zone, 1<= n <= 5; n = gas number

        else if (keyWrd(1:1) .eq. 'C') then
          iconc=intcon(Keywrd(2:2),Eflag)
C         write(cof,*) 'GetData:iconc=',iconc,' ',Keywrd
C@empa aw 2000apr04 flowdir=3 indicates an external node
          if (flowdir.eq.3) then
            GetData=ExtConc(iconc,IDX)
          else
            GetData=Cold(iconc,IDX)
          endif
C@empa aw 2000apr20 GetData called from Histcalc
          if (len(keyWrd).ge.4)then
            if  (keyWrd(4:4).eq.'H')then
              GetData=C(iconc,IDX)
            endif
          endif
        else if (keyWrd(1:1) .eq. 'Q') then
C         Qn = Pollutant source Strength per gas and zone, 1<= n <= 5
C                                                    - KeyW(16-20)
          iconc=intcon(Keywrd(2:2),Eflag)
C@empa aw 2000mar21 +OccSource
CC          GetData=Source(iconc,IDX)
          GetData=Source(iconc,IDX)+OccSource(iconc,IDX)
        else if (KeyWrd(1:1) .eq. 'S') then
C         Sn = Pollutant Sink per gas and zone, 1<= n <= 5
C                                                    - KeyW(21-25)
          iconc=intcon(Keywrd(2:2),Eflag)
          GetData=Sink(iconc,IDX)

C *  Values per windpressure points:
        else if (KeyWrd .eq. 'PE') then
C         PE = Wind pressure per wind pressure point - KeyW(26)
          GetData=Pwind(IDX)

        else if (KeyWrd .eq. 'IZ') then
C         IZ = Outdoor air infiltration              - KeyW(27)
          GetData=-Q(IDX,0)
        else if (KeyWrd .eq. 'AZ') then
C         AZ = Outdoor air change rate               - KeyW(28)
          if (Vz(IDX).ne.0.0) then
            GetData=-Q(IDX,0)*3600/Vz(IDX)/Rhoz(IDX)
          else
            GetData=0.
          endif
        else if (KeyWrd .eq. 'MZ') then
C         MZ = Room mean age of air                  - KeyW(29)
          GetData=TauR(IDX)
        else if (KeyWrd .eq. 'EZ') then
C         EZ = Air change efficiency per room        - KeyW(30)
          if ((ACB*TauR(IDX)).ne.0.0) then
            GetData=3600/(2*ACB*TauR(IDX))
          else
            GetData=0.
          endif
        else if (KeyWrd .eq. 'IB') then
C         IB = Outdoor air infiltration              - KeyW(32)
          GetData=-Q(0,0)
        else if (KeyWrd .eq. 'AB') then
C         AB = Outdoor air change rate               - KeyW(33)
          GetData=ACB
        else if (KeyWrd .eq. 'MB') then
C * Building mean age of air
C         MB = Outdoor air change rate               - KeyW(34)
          GetData=TauB
        else if (KeyWrd .eq. 'RB') then
C         RB = RMS (t)                               - KeyW(35)
          GetData=0.
          VB=0
          DO 10 i=1,Nz
            GetData = GetData + (TauR(i)-TauB)**2*Vz(i)**2
            VB=VB+Vz(i)
10        CONTINUE
          if (VB.ne.0.0) then
            GetData=SQRT(GetData/VB**2)
          else
            Getdata=0.
          endif
        else if (KeyWrd .eq. 'NB') then
C         NB = Nominal time constant                 - KeyW(36)
          if (ACB.ne.0.0) then
            GetData=3600/ACB
          else
            GetData=0.
          endif
        else if (KeyWrd .eq. 'EB') then
C         EB = Air change efficiency                 - KeyW(37)
          if ((ACB*TauB).ne.0.0) then
            GetData=3600/(2*ACB*TauB)
          else
            GetData=0.
          endif
        else if (KeyWrd .eq. 'LB') then
C         LB = Ventilation heat loss energy          - KeyW(38)
          GetData=0.0
C@NBI PGS 1999Aug05 - Added calculation of ventilation heat loss [J].
C@NBI                 Positive value of "GetData" means that there is a net
C@NBI                 sensible+latent heat loss from building.
C@NBI                 If you want to know just the sensible component, then
C@NBI                 you must run COMIS with zero humidity - but that will
C@NBI                 change the air density very slightly, so won't be exact.
          DO I=1,Nl
            FROM=FromTo(1,I)
            TO=FromTo(2,I)
            L=Lstat(i)
            IF((L.EQ.1).OR.(L.EQ.2)) THEN
C             Flow into building:  cp-->zn  or  sp-->zn
              if (Nz+from.gt.0) GetData=GetData
     &          -Fv2(1,i)*Enthalpy(Tout,Xhout)
     &          +Fv2(2,i)*Enthalpy(Tz(to),Xhz(to))
            ELSEIF((L.EQ.3).OR.(L.EQ.6)) THEN
C             Flow out of building:  zn-->cp  or  zn-->sp
              if(Nz+to.gt.0) GetData=GetData
     &          -Fv2(2,i)*Enthalpy(Tout,Xhout)
     &          +Fv2(1,i)*Enthalpy(Tz(from),Xhz(from))
            ENDIF
          ENDDO
C         [J]     = [W]     * [s]    Note: bufsec is NOT zero on last timestep
          GetData = GetData * Interval
C@NBI                 (end of patch)
C@empa aw 2000jan31 Ox number of occupant x and Yx activitY of occupant x added
C                                                    - KeyW(41-44)
          else if (KeyWrd(1:1) .eq. 'O') then
            iconc=intcon(Keywrd(2:2),Eflag)
            GetData=OccNum(iconc,IDX)
C     Yn = activitY of occupant per occupant type n and zone, 1<= n <= 5
C                                                    - KeyW(45-48)
          else if (KeyWrd(1:1) .eq. 'Y') then
            iconc=intcon(Keywrd(2:2),Eflag)
            GetData=OccAct(iconc,IDX)


C@empa aw 2000feb02 Fn effective Flow calculated with fictive source n 1<= n <= 5
          else if (KeyWrd(1:1) .eq. 'F') then
C           Fn = effective Flow calculated with fictive source n 1<= n <= 5
C                                                    - KeyW(49-53)
            iFSo=index('123456789',Keywrd(2:2))
            if (iFSo.ne.0)then
C@empa aw 2000apr20 GetData called from Histcalc
              Hist=.false.
              if (len(keyWrd).ge.4)then
                if  (keyWrd(4:4).eq.'H')Hist=.true.
              endif
              izone=idx
C@empa aw 2000jul03 Histtype replaced by logical Hist
CC              call GetZoneEflow(izone,iFSo,Histtype,value)
              call GetZoneEflow(izone,iFSo,Hist,value)
              GetData=Value
            endif



          else

C no mapping defined

           GetData=0
CC          call ERROR2('Keyword not defined in grout.f, function '//
           call inERR('Keyword not defined in grout.f, function '//
CC     &   'GetData [&-PR-OUTPut]',KeyWrd,2)
     &     'GetData [&-PR-OUTPut] '//KeyWrd,' ',.true.,2)
      endif


      RETURN

      END

Ch***********************************************************************
C@empa aw 2000jul03 Hist logical
CC            SUBROUTINE GetZoneEflow(izone,iFSo,Histtype,value)
            SUBROUTINE GetZoneEflow(izone,iFSo,Hist,value)
C***********************************************************************
C Purpose: Get Effective flowrate from a zone
C          Called from GetData
C
C jcp 1996Apr06
C Module : Comv-Gr2.FOR
C Changes:
C@empa aw 2000feb01 Data for fictive sources from new keyword &-POL-FIC
C Limits :
C
C
C example:call GetZoneEFlow(2,5,value)
C value is effective flowrate for zone 2 accoring to the settings in Histogram 5
C call
C USES:
C
C IO  Name    Units     Description
C I   izone   [-]       zone sequence number
C I   iFSO    [-]
C I   Hist    [-]       .TRUE. if histogram
C  O  Value   [kg/s]    effective flowrate
C
Ch***********************************************************************

        IMPLICIT NONE
        include 'comv-inp.inc'
        include 'comv-uni.inc'
C@empa aw 2000jul03 Histtype canceled
CC        INTEGER izone,HistType,poln,iocc,num,iFSo
        INTEGER izone,poln,iocc,num,iFSo
        REAL Value,weight,DC

        INTEGER Lstr,Dep
C@empa aw 2000apr11 LenStr,Lstr2
        INTEGER LenStr,LStr2
C@empa aw 2000jul03 Hist
        LOGICAL Hist
        CHARACTER*30 str

c        write(*,*) 'in GetZoneEflow zone=',izone,' Histtype=',histtype
c        write(*,*) 'COld(1,izone)=',Cold(1,izone)
c        write(*,*) ''
c        pause

        poln=FSPolNr(iFSo)
C it is a zone
        if (poln.eq.0) then
           call intdis(iFSo,Str,Lstr)
           call inerr('Fictive source F'//Str(1:Lstr)//
     &     ' is not defined !',' ',.false.,2)
        else
          Dep=FSDep(iFSo)
          if (Dep.eq.0) then
            weight=1
          else if (Dep.eq.1) then
C           zone floor area dependency
            weight=Dz(izone)*Wz(izone)
C@empa aw 2000apr11 Error message if zone volume not given with H/D/W-option
            if (weight.eq.0)then
              call intdis(iFSo,Str,Lstr)
              Lstr2=lenStr(ZoNa(izone))
              call inerr('&-PR-OUTPut: Floor area dependent fictive '//
     &        'source F'//Str(1:Lstr)//
     &        ' cannot be calculated in zone:  '//
     &        ZoNa(izone)(1:Lstr2)//'!;'//
     &        'Zone volume has to be input with H/D/W-option '//
     &        'in &-NET-ZONe.',' ',.false.,3)
            end if
          else if (Dep.eq.2) then
C           zone wall area dependency
            weight=2*Dz(izone)*Wz(izone)+
     &             Hz(izone)*(2*Wz(izone)+2*Dz(izone))
C@empa aw 2000apr11 Error message if zone volume not given with H/D/W-option
            if (weight.eq.0)then
              call intdis(iFSo,Str,Lstr)
              Lstr2=lenStr(ZoNa(izone))
              call inerr('&-PR-OUTPut: Wall area dependent fictive '//
     &        'source F'//Str(1:Lstr)//
     &        ' cannot be calculated in zone:  '//
     &        ZoNa(izone)(1:Lstr2)//'!;'//
     &        'Zone volume has to be input with H/D/W-option '//
     &        'in &-NET-ZONe.',' ',.false.,3)
            end if

          else if (Dep.eq.3) then
C           zone volume dependency
            weight=Vz(izone)
          else if (Dep.gt.3) then
            weight=0
            num=0
            do 10 iocc=1,MaxO
C              occupant number*activity dependency
               weight=weight+Occnum(iocc,izone)*OccAct(iocc,izone)
               num=num+Occnum(iocc,izone)
10          continue
            if (Dep.eq.5) then
C              occupant number dependency
               weight=num
            else if (Dep.eq.6)then
C              average occupant activity dependency
               weight=weight/num
            endif
          end if
C@empa aw 2000feb08 No message, weight could be zero from occupant dependency.
CC          if (weight.eq.0) then
CC            call intdis(izone,Str4,Lstr4)
CC            call reldis(Wz(izone),2,Str,Lstr,0)
CC            call reldis(Dz(izone),2,Str1,Lstr1,0)
CC            call reldis(Hz(izone),2,Str2,Lstr2,0)
CC            call reldis(Vz(izone),2,Str3,Lstr3,0)
CC            call INERR('While calculating effective flow '//
CC     &      'Zone dimensions are zero for zone '//Str4(1:Lstr4),
CC     &      'Width='//Str(1:Lstr)//
CC     &      ' Depth='//Str1(1:Lstr1)//
CC     &      ' Height='//Str2(1:Lstr2)//
CC     &      ' Volume='//Str3(1:Lstr3)//' Correct the input',
CC     &      .FALSE.,3)
CC          end if
          DC=COld(poln,izone)-Cout(1)
C@empa aw 2000jul03 if called from histcalc C and not Cold has to be taken
          if (hist) DC=C(poln,izone)-Cout(1)
          if (DC.gt.REALMin) then
            Value=FSo(iFSo)*weight/DC
c          write(*,*) 'izone                 =',izone
c          write(*,*) 'Roomdep               =',Roomdep
c          write(*,*) 'PolQzon(1,Roomdep)=',PolQzon(1,Roomdep)
c          write(*,*) 'value=',value,' weight= ',weight
c          call ho('value in get Flow zone calculated','')
          else
C C=0 then effective ventilation is infinite
            Value=REALMax
          end if
        end if
       return
       end


Ch***********************************************************************
C@empa 2000mar08 Subroutine GetOccConc obsolete now occupants handled in HistCalc now.
C@empa 2000mar08 Subroutine GetOccWeight obsolete now occupants handled in HistCalc now.
C@empa 2000mar08 Subroutine GetOccFlow obsolete now occupants handled in HistCalc now.
C@empa 2000mar08 Subroutine GetOccEFlow obsolete now occupants handled in HistCalc now.
Ch***********************************************************************
