C+*********************************************************** comv-in5.f
Ch**********************************************************************
C NET-EXTernal Nodes
	SUBROUTINE inEXT(Line, LinTyp, L, K)
Ch**********************************************************************
        IMPLICIT NONE
	INCLUDE 'comv-inp.inc'
C@empa aw 1999nov29 LStr
        INTEGER LStr
        INTEGER LenStr
	CHARACTER*(*) line
        INTEGER k,l,LinTyp

C@tno jcp 1996Jul09_17:10:28 variab added to inEXT
        variab='NET-EXT translation of ext.node to cp name'

C@tno jcp 1996Apr26_18:20:35 check added for nwind Net-Ext and CP-Values
C check if Nwind=0, if not then probably it has been filled by InCp because
C that came first in the input file. InCpVal by default made Facade element
C names (Cp) equal to the External node names.
        if (Nwind.gt.0) then
c         call inerr('&-CP-VALues: The Section &-NET-EXTernal '//
c     &     'must appear before &-CP-VALues !',line,.FALSE.,2)

          CALL INERR('&-CP-VALues: The Section &-NET-EXTernal '//
     & '&-NET-EXT','Move &-NET-EXT to above &-CP-VALUEs',.FALSE.,3)
        end if

C Loop here for each external node

10      continue
	Nwind=Nwind+1
	Variab='ExNa (Nwind)'
	k=0
	CALL GETWS(Line,K,L,ExNa(Nwind),'1')
C@empa aw 1999nov29 check for "-" in Name
      IF (INDEX(ExNa(Nwind),'-').GT.0) THEN
	  LStr=LenStr(ExNa(Nwind))
	  CALL INERR ('&-NET-EXT: external node: '//ExNa(Nwind)(1:Lstr)//
     &  ' ; "-" is not allowed in external node ID ! ',' ',.TRUE.,2)
      ENDIF
	ExNr(Nwind)=nwind
	Variab='FacadeNa(Nwind)'
	CALL GETWS(Line,K,L,FacadeNa(Nwind),'-')
	Variab='OuCF(Nwind)'
        CALL GETWR(Line,K,L,OuCF(Nwind),1.0,.TRUE.)
	Call readlin(line, LinTyp, K, .True.)
	if (FlgEnd) goto 99
	l=lenstr(line)
	if (LinTyp .eq. TDATA) goto 10
	goto 99

C after this, ExNa,Facade,OuCF needs to be regrouped in a tree and
C translated in a cp-sequence number in the linkpart

99      continue
	RETURN
	END

Ch**********************************************************************
C NET-LINks
	SUBROUTINE inLINK(Line, LinTyp, L, K)
C Changes:
C@tno jcp 1996Feb08_09:25:50 Reflink for DS, DU and RF
C@empa aw 1992feb23 Reflink also when DU
C@empa aw 1993jun09 call INERR instead of write CRT and pause
C@empa aw 1993jun14 new structure at reading of FromTo
Ch**********************************************************************

        IMPLICIT NONE
	INCLUDE 'comv-inp.inc'
	INCLUDE 'comv-uni.inc'
C@empa aw 1999nov29 Lstr
	INTEGER  LStr
        INTEGER LenStr
	CHARACTER*(*) line
        INTEGER k,l,LinTyp,Eflag,P
	character word*40,word2*40
        REAL RELCON

C Loop here for each link

10      continue

C Increment number of links
	Nl=Nl+1
	k=0
	Variab='LiNa(Nl)'
	CALL GETWRD(Line,K,l,LiNa(Nl))
C@empa aw 1999nov29 check for "-" in Name
      IF (INDEX(LiNa(Nl),'-').GT.0) THEN
	  LStr=LenStr(LiNa(Nl))
	  CALL INERR ('&-NET-LINKS: Link ID: '//LiNa(Nl)(1:Lstr)//
     &  ' ; "-" is not allowed in Link ID ! ',' ',.TRUE.,2)
      ENDIF
	LiNr(Nl)=Nl
	Variab='LiTyNa(Nl)'
	CALL GETWS(Line,K,L,LiTyNa(Nl),AFCName(1:2))
C one of these arrays is not necessary, but THEN it needs to be restored to:
C 1,2,3,4,5....,Nl after the namsort (for LinkName) to be used for the second
C namsort (for the linkType)
	TyNr(Nl)=Nl

	Variab='From zone(or node)'
	CALL GETWRD(line,k,l,word)

C Lstat is the link status:
C Lstat From     To
C  0    zone    zone
C  1     ext    zone
C  2    spec    zone
C  3    zone     ext
C  4     ext     ext
C  5    spec     ext
C  6    zone    spec
C  7     ext    spec
C  8    spec    spec
C
C this is the part to get the link analysed : Zone, Ext node, Pspec
C here again Pa, and will not work with pa or PA or p
        p=index(word,'Pa')
	IF (p.GT.0) THEN
	  nSpec=nSpec+1
	  word2=word(1:p-1)
          variab='special pressure'
	  Pspec(nSpec)=RELCON(word2,EFLAG)
C unit conversion
	  Pspec(nSpec)=(ABS(ifact(UnitP)))*Pspec(nSpec)
C directly put the number in FromTo
	  FromTo(1,Nl)=nSpec
	  Lstat(Nl)=2
	    FromToS(1,Nl)=Word
	ELSE
	  IF (Word(1:1).eq.'-') THEN
C it is a Cp point
C here I need to get the cp sequence number at the END, first via a tree
C the external node sequence number and the facade element number afterwards
	    Lstat(Nl)=1
	    FromToS(1,Nl)=Word(2:)
	  ELSE
C it is a normal zone name
C look up the zone ID from the binary tree afterwards
	    Lstat(Nl)=0
	    FromToS(1,Nl)=Word
	  ENDIF
	ENDIF

	Variab='To zone (or node)'
	CALL GETWRD(line,k,l,word)
C here again Pa, and will not work with pa or PA or p
        p=index(word,'Pa')
	IF (p.GT.0) THEN
	  nSpec=nSpec+1
	  word2=word(1:p-1)
          variab='special pressure'
	  Pspec(nSpec)=RELCON(word2,EFLAG)
C unit conversion
	  Pspec(nSpec)=(ABS(ifact(UnitP)))*Pspec(nSpec)
C directly put the number in FromTo
	  FromTo(2,Nl)=nSpec
	  Lstat(Nl)=Lstat(Nl)+6
	    FromToS(2,Nl)=Word
	ELSE
	  IF (Word(1:1).eq.'-') THEN
C it is a Cp point
C here I need to get the cp sequence number at the END, first via a tree
C the external node sequence number and the facade element number afterwards
	    Lstat(Nl)=Lstat(Nl)+3
	    FromToS(2,Nl)=Word(2:)
	  ELSE
C it is a normal zone name
C look up the zone ID from the binary tree       afterwards
C           Lstat(Nl)= ++ 0 (No change)
	    FromToS(2,Nl)=Word
	  ENDIF
	ENDIF

        IF (test.GE.1 .AND. iecho.ge.4) THEN
	  WRITE(CRT,*) 'link',Nl,'lstat',lstat(Nl)
	ENDIF

	Variab='Zl(1,Nl)'
        CALL GETWR(Line,K,L,Zl(1,Nl),0.0,.TRUE.)
	Variab='Zl(2,Nl)'
        CALL GETWR(Line,K,L,Zl(2,Nl),0.0,.TRUE.)
	Variab='HfL(Nl)'
C default value for own height factor must be 1.
        CALL GETWR(Line,K,L,HfL(Nl),1.0,.TRUE.)
	IF ((Hfl(Nl).GT.1).OR.(Hfl(Nl).LT.0.0))THEN
	  CALL INERR('&-NET-LINks:  LinkNr: '//LiNa(Nl)//
     &    ' Own Height Factor is not within the range 0...1!',
     &    ' ',.FALSE.,2)
	ENDIF

	Variab='Mf(Nl)'
        CALL GETWR(Line,K,L,Mf(Nl),1.0,.TRUE.)
	Variab='P3D(Nl)'
        CALL GETWR(Line,K,L,P3D(Nl),0.0,.TRUE.)
C unit conversion
	P3D(Nl)=(ABS(ifact(UnitP)))*P3D(Nl)

C reflink also for RF=Related Flow (component 12)
C Reflink also when DU and RF
	IF ((LiTyNa(Nl)(1:2).EQ.'DS').OR.
     &          (LiTyNa(Nl)(1:2).EQ.'DU').OR.
     &          (LiTyNa(Nl)(1:2).EQ.'RF')) THEN
	    Variab='RefLink(Nl)'
	   CALL GETWS(Line,K,L,REFLINK(Nl),'     ')
C the duct angle will not be used for the RF component, but will not harm the
C process if read as is done here?
	     Variab='DuAngle(Nl)'
           CALL GETWR(Line,K,L,DuAngle(Nl),90.0,.TRUE.)
C set the not used Sol to '     ' or it will contain binary null's
           SoL(Nl)='     '
	ELSE
C read the schedule name for this link
           Variab='SoL(Nl)'
	   CALL GETWS(Line,K,L,SoL(Nl),'     ')
C@empa aw 2002jun18 Added read of DUAngle although it is not used here, its just to have 
C@empa aw 2002jun18 have TRLiIS for all link types at the same position
	   Variab='DuAngle(Nl)'
         CALL GETWR(Line,K,L,DuAngle(Nl),90.0,.TRUE.)

	ENDIF
C@empa aw 2001jun01 For COMIS in TRNSYS only:
C@empa Read TRNSYS input/schedule number  
      CALL GETWR(Line,K,L,TRLiIS(Nl),0.,.TRUE.)

	Call readlin(line, LinTyp, K, .True.)
	if (FlgEnd) goto 99
	l=lenstr(line)
C If more data go do another link
	if (LinTyp .eq. TDATA) goto 10
	goto 99

99      continue
	RETURN
	END


Ch**********************************************************************
C CP-BUILding
	SUBROUTINE inCPBUI(Line, LinTyp, L, K)
Ch**********************************************************************
        IMPLICIT NONE
	INCLUDE 'comv-inp.inc'
	CHARACTER*(*) line
        INTEGER LinTyp,k,l

        Variab='Zref height at building for Vref->Cp'
	k=0
        CALL GETWR(Line,K,L,Zref,DZref,.TRUE.)
	Call readlin(line, LinTyp, K, .True.)
	RETURN
	END

Ch**********************************************************************
C CP-VALUes
	SUBROUTINE inCPVAL(line, LinTyp, L, K)
Ch**********************************************************************
        IMPLICIT NONE
        INCLUDE 'comv-uni.inc'
	INCLUDE 'comv-inp.inc'

        INTEGER LenStr
        CHARACTER*160 line
        INTEGER NCp1,NCp2
        INTEGER I,k,l,LinTyp,Lstr
C@empa aw 1996jan08 CIFT, j
	INTEGER CIFT,j
        INTEGER FacadeNr,BlockNr,iExtNr,ilink,ils
        character word*120,str*40
	LOGICAL FlgErr
C@empa aw 1996jan08 CpFile
	LOGICAL CpFile


C@tno jcp 1996Jul09_17:08:17 variab added to inCPval
        Variab='reading CpValues'
C The array CP(I) has first the Cpvalues of all FacadeNr's for direction 1 ,
C then all for direction 2 etc

C Check whether the &-NET-EXTernal has been read already.
C how many external nodes have been used at Net-LINk ?
C put those in NET-EXT ExNr, ExNa, FacadeNa, OuCf
C how many links have been defined?
        if (nwind.eq.0) then
C aparently Net-Ext has not been read.
C if Nl>0 then  Net-Link has been read

          if(nl.gt.0) then
            Do 41 ilink=1,Nl

              ils=Lstat(ilink)

              if (iLs.eq.1 .or. iLs.eq.4 .or. iLs.eq.7 ) then
                str=FromToS(1,ilink)

C loop through already stored names in ExNa
                iExtNr=0
42              continue
                  iextNr=IextNr+1
                  if (iextnr.gt.nwind) goto 46
C reached the end of the stored names in ExNa, add the current name
                  if (str.eq.ExNa(iExtNr)) goto 47
C found name. Already stored, jump out of the loop
                goto 42

46              continue
C store this name in ExNa add defaults for FacadeNa (same name) and OuCf=1.0
                nwind=nwind+1
                ExNr(Nwind)=Nwind
                ExNa(nWind)=str
                FacadeNa(nWind)=str
                OuCf(nWind)=1.0

47              continue
C jump out of the loop
              end if
C iLs=1 4 or 7

              if (iLs.eq.3 .or. iLs.eq.4 .or. iLs.eq.5 ) then
                str=FromToS(2,ilink)
C loop through already stored names in ExNa
                iExtNr=0
52              continue
                  iextNr=IextNr+1
                  if (iextnr.gt.nwind) goto 56
C reached the end of the stored names in ExNa, add the current name

                  if (str.eq.ExNa(iExtNr)) goto 57
C found name. Already stored, jump out of the loop
                goto 52

56              continue
C store this name in ExNa add defaults for FacadeNa (same name) and OuCf=1.0
                nwind=nwind+1
                ExNr(Nwind)=Nwind
                ExNa(nWind)=str
                FacadeNa(nWind)=str
                OuCf(nWind)=1.0

57              continue
C jump out of the loop
              end if
C iLs=3 4 5
41          continue
            call intdis(nwind,str,lstr)
            CALL INERR('&-Net-Ext not found before &-Cp-VALUEs.'//
     &   ' Comis will use the Cp-names found at &-NET-LINk.','The '//
     &   str(1:lstr)//' Cp-names in CP-VALUes have to match '//
     &   'the Cp-names in &-NET-LINk. If there is no match, '//
     &   'additional error messages will report this.'
     &   ,.FALSE.,0)
          call Wrt80(Cof,'Here follow the Cp-names in order of '//
     &     'appearance at Net-LINks:',WCRT)
            do 4411 i=1,Nl
              ils=lstat(i)
              if ((iLs.eq.1) .or. (iLs.eq.4) .or. (iLs.eq.7)) then
                word=word(1:lenstr(word))//' '//fromtoS(1,i)
              end if
              lstr=lenstr(word)
              if (lstr.ge.60) then
                call wrt80(cof,word,wCRT)
                word=' '
                lstr=1
              end if
              if ((iLs.ge.3) .and. (iLs.le.5)) then
                word=word(1:lstr)//' '//fromtoS(2,i)
              end if
              lstr=lenstr(word)
              if (lstr.ge.60) then
                call wrt80(cof,word,wCRT)
                word=' '
                lstr=1
              end if
4411        continue
            if (lstr.ge.1) then
              call wrt80(cof,word,wCRT)
              word=' '
              lstr=1
            end if
            write(COF,*) ' '
          end if
C nl
        end if
C nwind
	IF (nwind .EQ. 0) goto 800

	BlockNr=1
        FlgErr=.false.

C@tno jcp 1996May30_11:35:58 reintroduced if HeaderNr<>2 in inCpVal
C@empa aw 2002jul16 Dataset Name may be after the second header
CC        if (headerNr.eq.2) then
        if ((headerNr.eq.2).and.(line(k+1:k+1).EQ.'*'))  then
C aparently there was no data below Header1
          CPname='CpName'
        else
          Variab='CpName'
C@empa aw can't zero k because we want to get the filename for the F:
CC          k=0
C@empa aw 1996jan08 read input from assigned file:
	  cpfile=.false.
	  IF (line(k+1:k+2).EQ.'F:') THEN
	    cpfile=.true.
	    i=k
	    k=k+2
	    line(k:k) = ' '
	    j=k
	    l=lenstr(line)
	    variab=' reading a filename: CPname'
	    CALL GETWS(line,k,l,CPName,'CPname')
	    IF (CPName.NE.' ') LinTyp=TDATA
	    line(j:j)=':'
	    k=i
	    CIFT=CIF
	    CIF=TIF
	    OPEN(TIF,FILE=CPName,STATUS='OLD',ERR=950)
	  ELSE
C@empa aw 1996jan08/end
	    CALL GETWS(Line,K,L,CPName,'CpName')
	  endif
	  Call readlin(line, LinTyp, K, .True.)
	  if (FlgEnd) goto 99
C@empa aw 1996jan08 moved one line further down and removed second line
C          l = lenstr(line)
C          if (LinTyp .eq. TDATA) goto 10
        endif
C@tno jcp 1996May30_11:36:38 headernr endif

C entry point for loop for each wind direction block
5       continue

C@empa aw 1996jan08 moved following line from above
	l = lenstr(line)

	FacadeNr=0
C save the first position in Line in k0
C@tno jcp 1996Apr12_17:05:36 K0 never used
CC        k0=k
C@NBI PGS 2000Jul16 - Bad description
CC	Variab='get directions'
	Variab='* expected here'
	Call GETWRD(Line,K,l,word)
C First column of first line (section 2) must start with asterisk
	if (word(1:1) .ne. '*') goto 900
	NCp1=NCpDir

C CpDir is shifted one element to the right , to be able to make the array
C cyclic to both sides. in inh the first element will be copied from the
C last READ value and after the last READ value the first will be placed.
C The routine locate will RETURN a pointer that can be converted to two
C columns between which the interpolation has to take place.

c get the wind directions from this *-line
160     Variab='CpDir(NCpDir+1)'
        CALL GETWR(Line,K,L,CpDir(NCpDir+1),0.0,.TRUE.)
	NCpDir=NCpDir+1
	IF (K.LT.L) GOTO 160

	NCp2=NCpDir-1

C Loop here for each facade element

10      continue
        Call readlin(line, LinTyp, K, .True.)

C loop for the next wind directions block
	if (LinTyp .eq. TNAME) Then
	  BlockNr=BlockNr+1
C loop for the next block of wind directions
	  goto 5
        endif

	if (FlgEnd) goto 99
	if (LinTyp .ne. TDATA) goto 99

	l=lenstr(line)
C Get the facade element name
	Call GETWRD(Line,K,l,word)

	DO 1601 I=1,Nwind
	    IF (FacadeNa(I).EQ.WORD) GOTO 1605
1601    continue
	goto 10

C Only facade elements which are used for an external node in &-NET-EXT are
C read here. Other Facade elements are skipped.That is because the structure
C of the array CP(I) allows just Nwind (number of external nodes) Cp's per
C wind direction.

1605    continue
        FacadeNr=FacadeNr+1

C Check the facade element names of the further wind direction blocks.
        IF(BlockNr.EQ.1)THEN
C@tno jcp 1996May31_17:42:08 added the assignment of the array Facade (which wil
C not be sorted. If you use 'fef' numbers they must follow the sequence number
C of the USED Cp-facade elements (and must not count the not-used elements)
C It would be best to delete not used elements if you use fe
C@tno jcp 1996May31_17:45:17 this Facade assignment line is new
          Facade(FacadeNr)=FacadeNr

	  FeNa(FacadeNr)=Word
	  FeNr(FacadeNr)=FacadeNr
	  Nf=FacadeNr
	ELSEIF (FeNa(FacadeNr).NE.Word) THEN
c          FlgErr=.True.
          call intdis(BlockNr,str,lstr)
C@NBI PGS 2000Jul16 - Improve grammar & clarity (I hope)
CC        call inerr('&-CP-VALues: Trying to read '//
CC   &  FeNa(FacadeNr)(1:lenstr(FeNa(FacadeNr)))//
CC   & ' from the wind directions block '//str(1:lstr)//' but find '//
CC   &  word(1:lenstr(word))//'. The facade element ID''s '//
CC   &     'are not in the same sequence as in '//
CC   &     'wind directions block 1! Line in error:',line,.true.,2)
            CALL inerr('&-CP-VALues: Trying to read '//
     &      FeNa(FacadeNr)(1:lenstr(FeNa(FacadeNr)))//
     &     ' from the wind directions block '//str(1:lstr)//' but find '
     &      //word(1:lenstr(word))//'. The facade element IDs are not'//
     &     ' in the same sequence as in wind directions block 1. '//
     &     'The erroneous line is:',line,.true.,2)
            call Wrt80(cof,'Here follows the list of Facade element'//
     &      ' ID''s in the expected sequence as found at wind '//
     &      'directions block 1:',wCRT)
            str=' '
            do 5511 i=1,Nwind
              str=str(1:lenstr(str))//' '//FeNa(i)
              lstr=lenstr(str)
              if (lstr.ge.60) then
                call wrt80(cof,str,wCRT)
                str=' '
                lstr=1
              end if
5511        continue
            if (lstr.ge.1) then
              call wrt80(cof,str,wCRT)
              str=' '
              lstr=1
            end if

	ENDIF
	DO 161 I=NCp1,NCp2
	      Variab='Cp'
              CALL GETWR(Line,K,L,Cp(FacadeNr+(I-1)*Nwind),0.0,.TRUE.)
161     CONTINUE

C loop again for next facade element ID
	goto 10

800     call inerr('&-CP-VALUes: No used External pressure nodes '//
     &     ' have been found yet. The Section &-NET-EXTernal '//
     &     'must appear before &-CP-VALues ! Or if &-NET-LINks '//
     &     'doesn''t use external pressures you have to delete/'//
     &     'comment out the &-CP-VALUes section! Line at which '//
     &     'the error was reported:',
     &     line,.FALSE.,3)
	goto 99

C@NBI PGS 2000Jul16 - Better description
CC900   call inerr('&-CP-Values: First column of directions must '//
CC   &          'have "*"',line,.true.,2)
900     call inerr('&-CP-Values: 1st column of 1st row must have a'//
     &    ' "*" (Maybe you omitted the Dataset name or filename above)'
     &    //'.  The erroneous line is:',line,.true.,2)
	goto 99

950     call inerr('&-CP-VALues: CP-data file not found:',
     &             CPname,.FALSE.,2)

99      continue
        if (FlgErr)  Then
           call InErr('&-CP-VALues: The facade element ID''s '//
     &     'are not in the same sequence in every ',
     &     'wind directions block!.',.true.,2)
	endif

C@empa aw 1996jan08 close datafile with CP values.
         if (cpfile) then
           cpfile=.false.
           close(TIF)
           CIF=CIFT
           FlgEnd=.false.
	   call readlin(line, LinTyp, K, .True.)
         endif

	RETURN
	END

Ch**********************************************************************
C ENV-BUIlding (environment)
	SUBROUTINE inENVBUI(Line, LinTyp, L, K)
Ch**********************************************************************
        IMPLICIT NONE
	INCLUDE 'comv-inp.inc'
	CHARACTER*(*) line
        INTEGER k,l,LinTyp

C ENV BUI: Zentrance; AngBui; Lat; Lon
	k=0
        Variab='Zentr ground level at the building (entrance)'
        CALL GETWR(Line,K,L,Zentr ,DZentr,.TRUE.)
        Variab='AngBui North to x-axis of Building'
        CALL GETWR(Line,K,L,AngBui,DAngBui,.TRUE.)
        Variab='Lattitude of the building'
        CALL GETWR(Line,K,L,Lat   ,DLat,.TRUE.)
        Variab='Longitude of the building'
        CALL GETWR(Line,K,L,Lon   ,DLon,.TRUE.)

C read next line for next keyword
        Variab='reading next keyword after ENV BUIlding'
	Call readlin(line, LinTyp, K, .True.)

	RETURN
	END

Ch**********************************************************************
C ENV-WINd
        SUBROUTINE inENVWIN(Line, LinTyp, L, K)
C@NBI PGS 2000dec23 - Extended warning limit for alpha from 0.171 to 0.183
C@NBI                 because there are many weather stations with such
C@NBI                 semi-open terrain.
Ch**********************************************************************
        IMPLICIT NONE
        INCLUDE 'comv-inp.inc'
        CHARACTER*(*) line
        INTEGER k,l,LinTyp
        INTEGER LenStr

C@tno jcp 1996Jul24_14:10:52 added Pad
C@NBI PGS 2000Aug02 - "pad" no longer used so removed from decralations
CC      REAL PAD
C@empa aw 2000jul04 Dummy
        REAL Dummy
C@lbl bvs 1998Sep15 save the current iUnit(UnitProf) (alpha or ZO) here
C	so that only the one that is just before this section is used
C	later when the wind profile is used.  See also comv-inp.inc for wprofu
	wprofu = iUnit(UnitProf)

C ENV WIN: ZVmet; Zmet
        Variab='ZVmet'
        k=0
        CALL GETWR(Line,K,L,ZVmet,DZVmet,.TRUE.)

        IF (ZVmet.LT.1.0) THEN
              CALL INERR('&-ENV-WINd: Header 1,  Column 1:',
     &        'The height of meteo station wind speed mast is '//
     &        'less than 1.0m (norm=10m)!',.FALSE.,2)
        ELSE
          IF (ZVmet.LT. 5.0) THEN
              CALL INERR('&-ENV-WINd: Header 1,  Column 1:',
     &        'The height of meteo station wind speed mast is '//
     &        'less than 5.m (norm=10m)!',.FALSE.,1)
          ENDIF
        ENDIF
C allow people to use a 400m high local meteo mast
        IF (ZVmet.GT.400.0) THEN
              CALL INERR('&-ENV-WINd: Header 1,  Column 1:',
     &        'The height of meteo station wind speed mast is '//
     &        'more than 400m (norm=10m)!',.FALSE.,2)
        ELSE
          IF (ZVmet.GT. 15.0) THEN
              CALL INERR('&-ENV-WINd: Header 1,  Column 1:',
     &        'The height of meteo station wind speed mast is '//
     &        'more than 15.m (norm=10m)!',.FALSE.,1)
          ENDIF
        ENDIF
        Variab='Zmet'
        CALL GETWR(Line,K,L,Zmet , DZmet,.TRUE.)

        Variab='AlphMet'
        CALL GETWR(Line,K,L,AlphMet , DAlphMet,.TRUE.)

C better check ranges
C Error ranges  (for both meteo and building)
C Z0     Alpha
C 0.0002 0.085    error (program will not run, unREAL smooth)
C 40.0   0.87     error (program will not run, too rough)
C
C Warning ranges for meteo
C Z0     Alpha
C <0.01  <0.128   warning (unREAListic smooth)
C >0.07  >0.183   warning (too rough for meteo)
C

C if Z0 is used then check another range
        if (Iunit(UnitProf).eq.'Z0') then
C log profile
C wider range for people that use a local meteo mast
        IF (AlphMet.LT.0.0002 .OR. AlphMet.GT.40.0)THEN
              CALL INERR('&-ENV-WINd: Header 1,  Column 3',
     &        'Value Z0 for meteowind profile is outside the range '//
     &        'of: 0.0002 < Z0 < 40.0 !',.TRUE.,2)
        ELSE
          IF (AlphMet.LT.0.01.OR.AlphMet.GT.0.07)THEN
              CALL INERR('&-ENV-WINd: Header 1,  Column 3',
     &        'Value Z0 for meteowind profile is outside the range '//
     &        'of: 0.01 < Z0 < 0.07 (norm=0.03m) !',.TRUE.,1)
          ENDIF
        ENDIF
        else
C power law
C wider range for people that use a local meteo mast
C@empa aw 2005jul01 ok, but change message too
        IF (AlphMet.LT.0.085 .OR. AlphMet.GT.0.87)THEN
              CALL INERR('&-ENV-WINd: Header 1,  Column 3',
     &        'The meteo wind profile exponent is outside the range '//
CC     &        'of: 0.1 < alpha < 0.5 !',.TRUE.,2)
     &        'of: 0.085 < alpha < 0.87 !',.TRUE.,2)
        ELSE
C wider range for people that use a local meteo mast
          IF (AlphMet.LT.0.128 .OR. AlphMet.GT.0.183)THEN
              CALL INERR('&-ENV-WINd: Header 1,  Column 3',
     &        'The meteo wind profile exponent is outside the range '//
     &        'of: 0.128< alpha <0.183 (norm 0.14)!',.TRUE.,1)
          ENDIF
        ENDIF

        endif
C this endif belongs to the selection Alpha/Z0

        MEnvDir=0

C Loop here for each wind direction

10      continue
        Call readlin(line, LinTyp, K, .True.)
        if (FlgEnd) goto 99
        if (LinTyp .ne. TDATA) goto 99
        MEnvDir=MEnvDir+1
        l=lenstr(line)

C Direction angle; PlanAREADensity; WindprofileExponent
C EnvDir starts one element rotated to be capable to get it cyclic
C EnvDir occupies two more elements that there are wind directions here
C *** Plan Area Density is only used in COMIN ***

        Variab='EnvDir(MEnvDir+1)'
C@tno jcp 1996Jul24_14:07:11 DMemvdir must have been DenvDir
CC        CALL GETWR(Line,K,L,EnvDir(MEnvDir+1), DMenvDir,.TRUE.)
        CALL GETWR(Line,K,L,EnvDir(MEnvDir+1), DenvDir,.TRUE.)
C@empa aw 2000jul04 PAD has been canceled
CC        Variab='PAD(MEnvDir)'
CC        CALL GETWR(Line,K,L,PAD, 0.0,.TRUE.)
        Variab='WExp(MEnvDir)'
        CALL GETWR(Line,K,L,WExp(MEnvDir), DWExp,.TRUE.)
C@empa aw 2000jul04 check for more input (from an old CIF)
        CALL GETWR(Line,K,L,Dummy, 0.,.TRUE.)
        if (Dummy.ne.0.) then
           CALL INERR('&-ENV-WINd: Header 2, more than  '//
     &     '2 input parameters were found!  ',
     &     'Only wind direction angle and velocity profile '//
     &     'is requested.',.false.,2)
         endif





C test the wind profile data for the building
C Error ranges  (for both meteo and building)
C Z0     Alpha
C 0.0002 0.085    error (program will not run, unREAL smooth)
C 40.0   0.87     error (program will not run, too rough)
C
C Warning ranges for the building
C Z0     Alpha
C <0.005 <0.118   warning (unREAListic smooth for a building)
C >5.0   >0.456   warning (very rough for a building. profiles are very inaccura
C

C if Z0 is used then check another range 0.01..0.05
        if (Iunit(UnitProf).eq.'Z0') then
C log profile
C wider check ranges
        IF (WExp(MEnvDir).LT.0.0002 .OR. WExp(MEnvDir).GT.40)THEN
              CALL INERR('&-ENV-WINd: Header 2, Column 2. The ',
     &        'building wind profile Z0 is outside the range '//
     &        'of: 0.0002 < Z0 < 40 !',.TRUE.,2)
        ELSE
C wider check ranges
          IF (WExp(MEnvDir).LT.0.005 .OR. WExp(MEnvDir).GT.5.0)THEN
              CALL INERR('&-ENV-WINd: Header 2, Column 2. The ',
     &        'building wind profile Z0 is outside the range '//
     &        'of: 0.005 < Z0 < 5.0 !',.TRUE.,1)
          ENDIF
        ENDIF
        else
C power law
C wider check ranges
        IF (WExp(MEnvDir).LT.0.085 .OR. WExp(MEnvDir).GT.0.87)THEN
              CALL INERR('&-ENV-WINd: Header 2, Column 2. The ',
     &        'building wind profile exponent is outside the range '//
     &        'of: 0.085 < alpha < 0.87 !',.TRUE.,2)
        ELSE
C wider check ranges
          IF (WExp(MEnvDir).LT.0.118 .OR. WExp(MEnvDir).GT.0.456)THEN
              CALL INERR('&-ENV-WINd: Header 2, Column 2. The ',
     &        'building wind profile exponent is outside the range '//
     &        'of: 0.118< alpha < 0.456 !',.TRUE.,1)
          ENDIF
        ENDIF

        endif
C this endif belongs to the selection Alpha/Z0

        goto 10

99      continue
        if (MEnvDir .eq. 0) then
            EnvDir(2) = 0.0
            MEnvDir = 1
        endif
        RETURN
        END

