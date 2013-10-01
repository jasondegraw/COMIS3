C+*********************************************************** comv-in8.f
Ch***********************************************************************
       SUBROUTINE FindWaMa(WallMa,k1)
C
C Find the Name of the wall Material (or WallType) WallMa in the string array
C WName(*)(20). The index in k1 points at the data for this
C wallMaterial (or WallType) in WDat.
C Before this routine: names for WallMaterials and WallTypes have been stored in
C the string array WName*20(200)
C
Ch***********************************************************************

        IMPLICIT NONE
	INCLUDE 'comv-inp.inc'
	INCLUDE 'comv-uni.inc'
      CHARACTER*(*) WallMa
      INTEGER K1

      INTEGER I,L
        INTEGER LenStr

C if K1 remains 0 then the name is not found
      K1=0
      L=LenStr(WallMa)


C loop through all names registered until now
      DO 10 I=1,ptrWN
        if (WName(I)(1:L) .eq. WallMa) then
          if ((test.ge.1) .and. (iecho.ge.6)) THEN
            write(cof,*) ' Wallmaterial ',WallMa(1:L),
     &     ' found in WName(',I,')=',Wname(i)(1:L)
          end if
          K1=WNind(I)
          goto 20
        end if
10    CONTINUE

20    CONTINUE

      RETURN
      END


C@NBI PGS 2000Jul20 - This subroutine wasn't being used, so commented out
CCCh***********************************************************************
CC       SUBROUTINE FindStrA(Ndim,Nfilled,StrArray,SearchS,Ptr)
CCC                           |    |       |        |       |
CCC                           |    |       |        | outp.:index of SearchS in St
CCC                           |    |       |        String to be found in StrArray
CCC                           |    |       StringArray*(*)(Ndim)
CCC                           |    Last filled index in StrArray
CCC                           Dimensioned number of array elements in StrArray
CCC
CCC Find the index of a SearchString (SearchS) in the unsorted string array
CCC (StrArray*(*)(Ndim))
CCC The unsorted string array (StrArray*(*)(Ndim)) is filled from (1:Nfilled)
CCC   Nfilled<=Ndim
CCC If found, Ptr is the element number where SearchS is in StrArray
CCC   StrArray(Ptr)=SearchS
CCC If SearchS is not found Ptr=0
CCC
CCCh***********************************************************************
CC
CC        IMPLICIT NONE
CC	INCLUDE 'comv-inp.inc'
CC	INCLUDE 'comv-uni.inc'
CC      INTEGER Ndim,Nfilled,Ptr
CC      CHARACTER*(*) StrArray(Ndim)
CC      CHARACTER*(*) SearchS
CC
CC        INTEGER LenStr
CC
CC      INTEGER I,L,l1
CC
CCC if K1 remains 0 then the name is not found
CC      Ptr=0
CC      L=LenStr(SearchS)
CC      if ((test.ge.1) .and. (iecho.ge.5)) THEN
CC        write(cof,*) ' in FindStrA SearchS=',SearchS
CC        write(cof,*) ' LenStr L=',L
CC        write(cof,*) ' Ndim=',Ndim
CC        write(cof,*) ' Nfilled=',Nfilled
CC      end if
CC
CCC loop through all words 1:Nfilled
CC      DO 10 I=1,Nfilled
CC        if (StrArray(I)(1:L) .eq. SearchS) then
CC          if ((test.ge.1) .and. (iecho.ge.5)) THEN
CC            write(cof,*) ' StrArray(i)=',StrArray(i)(1:L)
CC          end if
CC          L1=LenStr(StrArray(i))
CC          if (l .eq.L1) then
CC            ptr=I
CC            if ((test.ge.1) .and. (iecho.ge.5)) THEN
CC              write(cof,*) 'String is equal at element =',ptr
CC            end if
CC          end if
CC          goto 20
CC        end if
CC10    CONTINUE
CC
CC20    CONTINUE
CC
CC      RETURN
CC      END


Ch***********************************************************************

	SUBROUTINE inNeWa(Line, LinTyp, L, K)
C input routine for NEtwork linking of WAll Types
C inNeWa stores the data in WalLin (Wall Linking)
C WalLin=ind, From, To, Orientation, Afact, .. etc
C 1 ind        = pointer into WDat (for the wall type)
C   From and To node names are stored first in FromToW(1..2,MaxL)*20char
C 2 From       = the internal number of the From node of this wall type
C                (negative= cp number 0=Not Connected positive= zone number)
C                During reading LookNam cannot be called yet as the node trees a
C                constructed afterwards in comv-mai.f
C 3 To         = the internal number of the To   node of this wall type
C 4 Orientation=[tenths] the direction of the normal to the surface
C 5 AFact      =[tenths] the factor this surface is larger than that defined at
C 6 Hfrom      =[tenths] the start height of the wall surface in the from zone
C 7 Hto        =[tenths] the start height of the wall surface in the to zone
C 8 OwnH       =[tenths] the vertical size of the wall surface (assumed to be
C               the same in the from and to zone
C As the WalLin array is INTEGER, the REAL are multiplied by 10 and then stored.
C This limits the accuracy to 0.1 m 0.1 deg 0.1 area factor. Don't try to model
C miniature houses with this routine.
C
C pointer array  meaning        contents
C ptrW    WDat   Wall data      Wall materials & types (data)
C ptrWN   WName  Wall name      Wall materials & types (names)
C ptrWL   WalLin Wall Linking   from and to nodes of types + area
C NWL                           Number of Wall-types in Links
C
Ch***********************************************************************

        IMPLICIT NONE
	INCLUDE 'comv-inp.inc'
	INCLUDE 'comv-uni.inc'
	CHARACTER*(*) line
        character*40 word
        INTEGER k,k1,l,LinTyp
        REAL R
        INTEGER IR
        INTEGER LenStr


C@tno jcp 1996Jul09_17:28:06 variab added to inNeWa
        variab='reading Network of wall types'


C       zero the number of walltypes used here
        NWL=0
C       set the flag that walls have been linked (used to test Nconc against
C       NConcWaTy in Routine inh)
        NetWaLinF=1
C  __________________ ______ _______________________
C |Type | Zone ID    |Orient|Area  |  Start  | Own  |
C |     |____________|up=90 |factor|  height |      |
C |Name | From | To  |vert=0|      |_________|      |
C |(-)  | (-)  | (-) |dn=270|      |From|To  |Height|
C |     |nc=Not Conne|      |      |[m] |[m] | [m]  |
C |_____|______|_____|______|______|____|____|______|

        ptrWL=1
10      CALL GETWRD(Line,K,L,WORD)
        Variab='Wall Type in NET-WAL'
        if (lenstr(word).gt.20) then
         call INERR('&-NET-WALls name of Type '//word//
     &   ' exceeds 20 characters. Now truncated.',line,.true.,1)
         word=word(1:20)
        end if
        CALL FindWaMa(word,k1)
C       directly place the index to WDat in WalLin. FindWaMa searches the
C       WallType
        NWL=NWL+1
        if ((test.ge.1) .and. (iecho.ge.5)) THEN
          write(cof,*) ' NeWaLI wall type=',word
          write(cof,*) ' wall type found in WDat at =',k1
        end if

C here the strings of the To and From node of a wall type are only stored
C after CIF is read, in main FromToW is called to interpret and place indices
C in WalLin at position 2 and 3 per Wall-link.

        WalLin(PtrWL)=k1
        if ((test.ge.1) .and. (iecho.ge.5)) THEN
          write(cof,*) ' WalLin(',ptrWL,')=',WalLin(ptrWL)
        end if
        ptrWL=ptrWL+1
	Variab='From zone(or node)'
	CALL GETWRD(line,k,l,word)
        if ((test.ge.1) .and. (iecho.ge.5)) THEN
          write(cof,*) Variab,'=',word
        end if
        FromToW(1,NWL)=word

C ptrWL is increased to leave a place for the node index placed in routine
C FromToW called later in main

        ptrWL=ptrWl+1

        Variab='To zone(or node)'
	CALL GETWRD(line,k,l,word)
        if ((test.ge.1) .and. (iecho.ge.5)) THEN
          write(cof,*) Variab,'=',word
        end if
        FromToW(2,NWL)=word
C ptrWL is increased to leave a place for the node index placed in routine
C FromToW called later in main
        ptrWL=ptrWl+1

	Variab='Net-Walls: orientation'
        CALL GETWR(Line,k,l,R,0.0,.TRUE.)
        R=R*10
        IR=INT(R)
        WalLin(ptrWL)=IR
        if ((test.ge.1) .and. (iecho.ge.5)) THEN
          write(cof,*) ' WalLin(',ptrWL,')=',WalLin(ptrWL)
        end if
        ptrWL=ptrWL+1

	Variab='Net-Walls: Area Factor'
        CALL GETWR(Line,k,l,R,0.0,.TRUE.)
        R=R*10
        IR=INT(R)
        WalLin(ptrWL)=IR
        if ((test.ge.1) .and. (iecho.ge.5)) THEN
          write(cof,*) ' WalLin(',ptrWL,')=',WalLin(ptrWL)
        end if
        ptrWL=ptrWL+1

	Variab='Net-Walls: height from'
        CALL GETWR(Line,k,l,R,0.0,.TRUE.)
        R=R*10
        IR=INT(R)
        WalLin(ptrWL)=IR
        if ((test.ge.1) .and. (iecho.ge.5)) THEN
          write(cof,*) ' WalLin(',ptrWL,')=',WalLin(ptrWL)
        end if
        ptrWL=ptrWL+1

	Variab='Net-Walls: height to'
        CALL GETWR(Line,k,l,R,0.0,.TRUE.)
        R=R*10
        IR=INT(R)
        WalLin(ptrWL)=IR
        if ((test.ge.1) .and. (iecho.ge.5)) THEN
          write(cof,*) ' WalLin(',ptrWL,')=',WalLin(ptrWL)
        end if
        ptrWL=ptrWL+1

	Variab='Net-Walls: own height'
        CALL GETWR(Line,k,l,R,0.0,.TRUE.)
        R=R*10
        IR=INT(R)
        WalLin(ptrWL)=IR
        if ((test.ge.1) .and. (iecho.ge.5)) THEN
          write(cof,*) ' WalLin(',ptrWL,')=',WalLin(ptrWL)
        end if
        ptrWL=ptrWL+1

	Call readlin(line, LinTyp, K, .True.)
        l=lenstr(line)
C See if we are at the end of the data yet
	if (FlgEnd) goto 88

C is this a next Wall Link data line
	if (LinTyp .eq. TDATA) goto 10

88      continue
	RETURN
	END

Ch**********************************************************************
      SUBROUTINE FrToW
C***********************************************************************
C
C in the routine FrToW the node names, given in the input under the keyword
C NET-Walls, that were stored in the string array FromToW(2,NWL), are
C interpreted.
C They can be:                              example
C      a zone name                           r1
C      an external node (connected to a Cp)  -2
C      a 'NOT CONNECTED' node.               - or (case insensitive) -nc
C The 2nd and 3rd element per Wall-Link in WalLin(*) are filled with the
C corresponding zone number, cp number(with a minus sign) or 0 for a not
C connected node.
C
Ch**********************************************************************

        IMPLICIT NONE
	INCLUDE 'comv-inp.inc'
	INCLUDE 'comv-uni.inc'
      INTEGER NWall,L,L2,K1,Lstr,ExtNr,key,CpNr
      CHARACTER*20 word,WordUp,NWallS

        INTEGER LenStr

C@@@ Here From and To have to be reordenend too???? I don't think so
      CpNr=0
      ExtNr=0

      DO 100 NWall=1,NWL
C this is the part to get the link analyzed : Zone, Ext node
C nc means Not Connected

        ptrWL=(NWall-1)*5+2

        word=FromToW(1,NWall)
        L=LenStr(word)
        if ((test.ge.1) .and. (iecho.ge.5)) THEN
          write(cof,*) ' from zone=',word
        end if

        wordUP=word
        CALL UPPERC(wordUP)
        if ((wordUP(1:3).eq.'-NC') .OR. (wordUP(1:L).eq.'-')) then
	  WalLin(ptrWL)=0
          if ((test.ge.1) .and. (iecho.ge.5)) THEN
            write(cof,*) ' WalLin(',ptrWL,')=',WalLin(ptrWL)
          end if
        ELSE
	  IF (Word(1:1).eq.'-') THEN
C It is a Cp point
C Here we first store the negative number in WalLin .
C Later we follow a procedure like in FrTo to replace the number in WalLin
C by the linked cp number.

C            CALL GETWI(word,1,lenstr(word),WalLin(ptrWL),-32768)


     	    CALL LookNam(Nwind,word(2:),ExTree,ExTreeN,
     &	    ExtNr,ExLarg,ExSmal,key)
            IF (key.EQ.1) THEN
             CALL INTDIS(Nwall,NWallS,L2)
             CALL INERR('&-NET-WAL:ls  LinkNr: '//NWallS(1:l2),
     &       'From Ext Node: '//Word(1:L)//' does not '//
     &       'exist in &-NET-EXT !',.FALSE.,2)
            ENDIF

	    CALL LookNam(Nf,FacadeNa(ExtNr),FeTree,FeTreeN,
     &	    CpNr,FeLarg,FeSmal,key)
            WalLin(ptrWL)=-CpNr
            IF (key.EQ.1) THEN
             LStr=LENSTR(FacadeNa(ExtNr))
             CALL INERR('&-NET-WAL:ls  ',
     &       'Facade Element: '//FacadeNa(ExtNr)(1:LStr)//
     &       'exist, in all wind directions blocks, in &-CP-VALUes !',
     &       .FALSE.,2)
            ENDIF
          if ((test.ge.1) .and. (iecho.ge.5)) THEN
            write(cof,*) ' WalLin(',ptrWL,') cp=',WalLin(ptrWL)
          end if

	  ELSE
C it is a normal zone name
C Store in WalLin the sequence number of this ZoneID that can be found in the ar
C ZoNa(1..Nz). But only if the Zone names are already read. So this data part
C must come after &-NET-ZONes in the input file.
C If this is not acceptable, then a string array like FrToS must be used for
C this WalLin.

C FindStrA directly digs in the unsorted list of zone names and checks every
C zone name. The returned K1 is the zone sequence number (in the input)
            CALL LOOKNAM (Nz,word,ZoTree,ZoTreeN,K1,ZoLarg,
     &       ZoSmal,key)
C            CALL FindStrA(MaxZ,Nz,ZoNa,word,K1)
            If (key .eq. 1) then
C name is not found
             CALL INTDIS(Nwall,NWallS,L2)
             CALL INERR('&-NET-WALls:From Zone: '//word(1:L)//
     &       ' does not exist in &-NET-ZONes !',
     &       'linenr under Net-walls='//Nwalls,.FALSE.,2)
            ENDIF
            WalLin(ptrWL)=K1
          if ((test.ge.1) .and. (iecho.ge.5)) THEN
            write(cof,*) ' WalLin(',ptrWL,')=',WalLin(ptrWL)
          end if
	  ENDIF
	ENDIF

C To zone(or node)

        word=FromToW(2,NWall)
        ptrWL=(NWall-1)*5+3

C       this is the part to get the link analysed : Zone, Ext node
C       nc means Not Connected
        if ((test.ge.1) .and. (iecho.ge.5)) THEN
          write(cof,*) ' to zone=',word
        end if
        L=LenStr(word)
        wordUP=word
        CALL UPPERC(wordUP)
        if ((wordUP(1:3).eq.'-NC') .OR. (wordUP(1:L).eq.'-')) then
          WalLin(ptrWL)=0
          if ((test.ge.1) .and. (iecho.ge.5)) THEN
          write(cof,*) ' WalLin(',ptrWL,')=',WalLin(ptrWL)
          end if
        ELSE
	  IF (Word(1:1).eq.'-') THEN

C           It is a Cp point
C           Here we first store the negative number in WalLin .
C           Later we follow a procedure like in FrTo to replace the number in
C           WalLin by the linked cp number.

C            CALL GETWI(word,1,lenstr(word),WalLin(ptrWL),-32768)


     	    CALL LookNam(Nwind,word(2:),ExTree,ExTreeN,
     &	    ExtNr,ExLarg,ExSmal,key)
            IF (key.EQ.1) THEN
             CALL INTDIS(Nwall,NWallS,L2)
             CALL INERR('&-NET-WAL:ls  LinkNr: '//NWallS(1:l2),
     &       'To Ext Node: '//Word(1:L)//' does not '//
     &       'exist in &-NET-EXT !',.FALSE.,2)
            ENDIF

	    CALL LookNam(Nf,FacadeNa(ExtNr),FeTree,FeTreeN,
     &	    CpNr,FeLarg,FeSmal,key)
            WalLin(ptrWL)=-CpNr
            IF (key.EQ.1) THEN
             LStr=LENSTR(FacadeNa(ExtNr))
             CALL INERR('&-NET-WAL:ls  ',
     &       'Facade Element: '//FacadeNa(ExtNr)(1:LStr)//' does not '//
     &       'exist, in all wind directions blocks, in &-CP-VALUes !',
     &       .FALSE.,2)
            ENDIF
            if ((test.ge.1) .and. (iecho.ge.5)) THEN
              write(cof,*) ' WalLin(',ptrWL,') cp=',WalLin(ptrWL)
            end if

	  ELSE
C           it is a normal zone name
C           Store in WalLin the sequence number of this ZoneID that can be found
C           in the array ZoNa(1..Nz)

            CALL LOOKNAM (Nz,word,ZoTree,ZoTreeN,K1,ZoLarg,
     &       ZoSmal,key)
C            CALL FindStrA(MaxZ,Nz,ZoNa,word,K1)
            If (Key .eq. 1) then

C             name is not found
             CALL INTDIS(Nwall,NWallS,L2)
             CALL INERR('&-NET-WALls:From Zone: '//word(1:L)//
     &       ' does not exist in &-NET-ZONes !',
     &       'linenr under Net-walls='//Nwalls,.FALSE.,2)
            ENDIF
            WalLin(ptrWL)=K1
            if ((test.ge.1) .and. (iecho.ge.5)) THEN
              write(cof,*) ' WalLin(',ptrWL,')=',WalLin(ptrWL)
            end if
	  ENDIF
	ENDIF

100     CONTINUE
C 100 is the loop through all Wall-links

        return
        END

Ch***********************************************************************
C
C@ empa aw 1995dec19  Flag for mass/volume flow canceled.
C@                   This flag could lead to the very confusing situation
C@                   where we have the flag set to volume flow but the input
C@                   units for volume flow (fan flow) is assigned to a massflow
C@                   unit. The input has then to be in massflow unit against
C@                   the setting with the flag.
C@                 The new approach is to select mass or volume flow only
C@                 with assigning volume flow (fan flow) with the desired unit.
C@lbl bvs 1995Jun unit conversion for Pressures and Flows
Ctno jcp fixes for new input method that didn't make it to version 1.3
C
C@empa aw 1993junxx Many changes in order to make the error messages more
C@empa              consistent. The only routine which is used now to
C@empa              report errors during input file reading is INERR.

        SUBROUTINE inTD(Line, LinTyp, L, K)
Ch***********************************************************************
        IMPLICIT NONE
	INCLUDE 'comv-inp.inc'
	CHARACTER*(*) line
	character*160 nline
        INTEGER k,k1,l,LinTyp
C@lbl bvs 1997Oct20 added LENSTR decl
	INTEGER LENSTR
C flag whether flows are massflow or volume flow
C@ empa aw 1995dec19 Flag for mass/volume flow canceled.
CC        INTEGER FLAG
        REAL R
C@empa aw 1995dec18 RhoI
	REAL RhoI

	k=0
C reserve space for the number of data pairs used for the TD
C Define ptrL0 as the element in Ldat where Npairs is stored
C (ptrL0 is defined in include file)
	ptrL0=ptrl
	ptrL=ptrl+1

C@ empa aw 1995dec19 Flag for mass/volume flow canceled.
CC	   Variab='TD: Flag (1=massflow or 2=volumeflow)'
CC           CALL GETLDat(Line,k,l,LDat(ptrl),ptrl,0.0)
CCC   Save flag for converting the flows
CC	   FLAG = LDat(ptrl-1)

	Variab='TD: RhoI'
C@empa aw 2000nov21 Default for RhoI=1.2
CC        CALL GETLDat(Line,k,l,LDat(ptrl),ptrl,0.0)
        CALL GETLDat(Line,k,l,LDat(ptrl),ptrl,1.2)
C@empa aw 1995dec18 RhoI
	RhoI=Ldat(ptrl-1)

	Npair=0
C@empa aw 2000jan20 this line is wrong ptrl0 is already set before
CC       ptrl0=ptrl-3

C lines with data pairs


        Call readlin(line, LinTyp, K, .True.)
C FlgEnd should not be TRUE now (2nd dataline is missing in that case)
	if (FlgEnd) goto 99

C If a name, then another TD follows
	if (LinTyp .eq. TNAME) goto 99
	if (LinTyp .ne. TDATA) goto 99

C Read ahead one line to see if we are past this TD data. If so, then Line
C contains the filter data and Nline the data for the next keyword.

C Loop through all datalines of this TD

100	Call readlin(nline, LinTyp, K1, .True.)
C See if we are at the end of the data yet
	if (FlgEnd) goto 88
	if (LinTyp .ne. TDATA) goto 200

        l=lenstr(line)

C Keep looping until we have all data from this line

10      continue
	Variab='TD: datapoints'
	CALL GETLDat(Line,k,l,LDat(ptrl),ptrl,0.0)
	Npair=Npair+1
C unit conversion, check whether pressure or flow column
	if (mod(Npair,2).eq.0) then
C	  Even position, flow
C@ empa aw 1995dec19 Flag for mass/volume flow canceled.
C@empa aw 1995dec18 convert to kg/s with actual RhoI
CC	    if (flag .eq. 1) then
CC	      flag=1, volume flow
CC		LDat(ptrl-1) = LDat(ptrl-1)*ifact(UnitFva)
CC	    else
CC	      flag=2, mass flow
CC		LDat(ptrl-1) = LDat(ptrl-1)*ifact(UnitFma)
CC	    endif
        LDat(ptrl-1)=ifact(UnitFva)*LDat(ptrl-1)*RhoI**ifact(UnitFvFlag)
	else
C	  Odd position, pressure
	    LDat(ptrl-1) = LDat(ptrl-1)*ifact(UnitP)
	endif
	IF (K.LT.L) GOTO 10

C shift the Nline, just read at 100, into Line to process it
        line=Nline
        k=k1

        GOTO 100

200	continue
C Now Nline contains something else and therefore Line is the filter data

C check, calculate and store Npairs
C check for an even number of data

	R=Npair/2.
	IF (ABS(R-INT(R)) .LT. 0.1) THEN
C OK Npair is even
	  Npair=Npair/2
C store Npair at the place kept free
	  LDat(ptrl0)=Npair
	ELSE
	       CALL INERR('&-TD: uneven number of data under '//
     &         'header two !',line,.true.,2)
	ENDIF

	CALL FILTER(Line,K,L)
	goto 88

88	continue
	k=k1
	line=Nline

99      continue
	RETURN
        END

Ch***********************************************************************
C new empty routine for PE the Walton Powerlaw Element
        SUBROUTINE inPE(Line, LinTyp, L,K)
Ch***********************************************************************
        IMPLICIT NONE
	CHARACTER*(*) line
        INTEGER k,l,LinTyp
        include 'comv-uni.inc'
        write(cof,*) 'Routine PE is still empty'
        return
        end


C@empa aw 2005apr26 This routine is not used any more as the outputoption(9)
C "KEYWORD" was canceled on 2000nov22
Ch***********************************************************************
CCC explain keywords
CCC************************************************************************
CC        SUBROUTINE SimuKey
CCC************************************************************************
CCC SimuKey writes explanation for the two letter keywords for graphical output
CCc The contents of this is copied from function GetData and must be kept
CCC parallel.
CCC@NBI PGS 1999Aug11 - New option 'CHECKC' that checks for condensation
CCCh***********************************************************************
CC        IMPLICIT NONE
CC        include 'comv-uni.inc'
CC
CC      write(COF,*) ' *  Values per zone:'
CC      write(COF,*) '  PZ = Pressure per Zone'
CC      write(COF,*) '  TZ = air Temperature per Zone'
CC      write(COF,*) '  HZ = Moisture per Zone'
CC      write(COF,*) '  FZ = Flow per Zone'
CC      write(COF,*) ' *  Values per link:'
CC      write(COF,*) '  FL = mass Flow per Link'
CC      write(COF,*) '  TL = Link Temperature'
CC      write(COF,*) '  VL = Actual value'
CC      write(COF,*) ' *  Values per Building:'
CC      write(COF,*) '  WA = Wind velocity'
CC      write(COF,*) '  TA = Outdoor air temperature'
CC      write(COF,*) '  HA = Outdoor air humidity'
CC      write(COF,*) ' *  Values per zone and gas:'
CC      write(COF,*) '  Cn = Concentration per gas and zone'//
CC     & ' 1<= n <= 5; n = gas number'
CC      write(COF,*) '  C1 = Concentration for gas 1 etc'
CC      write(COF,*) '  Qn = Pollutant source Strength per gas and zone'//
CC     &  ' 1<= n <= 5'
CC      write(COF,*) '  Q1 = Pollutant source Strength for gas 1 etc'
CC      write(COF,*) '  Sn = Pollutant Sink per gas and zone, 1<= n <= 5'
CC      write(COF,*) '  S1 = Pollutant Sink for gas 1 etc'
CC      write(COF,*) ' *  Values per windpressure points:'
CC      write(COF,*) '  PE = Wind pressure per wind pressure point'
CC      write(COF,*) ' *  Inf Ach Age Eff Balance Matrix:'
CC      write(COF,*) '  IZ = Outdoor air infiltration'
CC      write(COF,*) '  AZ = Outdoor air change rate'
CC      write(COF,*) '  MZ = Room mean age of air'
CC      write(COF,*) '  EZ = Air change efficiency per room'
CC      write(COF,*) '  EF = Effective flowrate per room'
CC      write(COF,*) '  FB = Flow matrix'
CC      write(COF,*) '  FW = Flow matrix With External nodes'
CC      write(COF,*) '  IB = Outdoor air infiltration'
CC      write(COF,*) '  AB = Outdoor air change rate'
CC      write(COF,*) ' * Building mean age of air'
CC      write(COF,*) '  MB = Outdoor air change rate'
CC      write(COF,*) '  RB = RMS (t)'
CC      write(COF,*) '  NB = Nominal time constant'
CC      write(COF,*) '  EB = Air change efficiency'
CC      write(COF,*) '  LB = Ventilation heat loss energy'
CC      write(COF,*) ' *  Values per occupant:'
CC      write(COF,*) '  Cn = concentration in the zone the occupant is in'
CC      write(COF,*) '  FZ = flow per zone the occupant is in'
CC      write(COF,*) ' '
CC
CCC@NBI PGS 1999Aug13 - Grammar
CCCC    write(COF,*) ' These two letter accronymes must be followed by'
CC      write(COF,*) ' These two-letter acronyms must be suffixed with:'
CC      write(COF,*) '  -S for spread sheet output separate files'
CC      write(COF,*) '  -T for average values in a table in *.COF'
CC      write(COF,*) '  -H for histogram output in separate files'
CC      write(COF,*) ' '
CC
CC      write(COF,*) ' Example:'
CC      write(COF,*) '  C1-S zon1,zon2  outputs concentration of '//
CC     & 'pollutant 1 for the zones with the '
CC      write(COF,*) '  name zon1 and zon2.'
CC      write(COF,*) '  C1-S occ1,occ2  outputs concentration of '//
CC     & 'pollutant 1 for occupants 1 and 2.'
CC      write(COF,*) ' '
CC
CCC@NBI PGS 1999Aug13 - Update this bit
CCCC    write(COF,*) ' The filenames of the output are composed from the'
CCCC    write(COF,*) ' accronym plus the number plus the text given in'
CCCC    write(COF,*) ' COMIS.SET under &-COMVEN as:'
CCCC    write(COF,*) ' TABLES xy "   " '
CCCC    write(COF,*) ' between the quotes is the number separator'
CCC     CHECK THIS
CC      write(COF,1000)
CC1000  FORMAT("  .COS output filenames are composed of a root name"/
CC     &"  suffixed by an acronym from above.  The root name is defined"/
CC     &"  with command-line option ""COMIS -t name"" or ""-a name"""/
CC     &"  A default name can be set in file COMIS.SET under &-COMIS:"/
CC     &"     TABLES [name] ["",""]"/
CC     &"  Between the quotes is your preferred column separator."/)
CC
CC      write(COF,*) ' More output options under &-PR-SIMU are:'
CC      write(COF,*) ' VENT or 2VENT for ventilation output'
CC      write(COF,*) ' POL [n]       to output [n] pollutants '
CC      write(COF,*) ' INPUT         to include the input file in '//
CC     & 'the output'
CC      write(COF,*) ' SET           to include the Comis.set file in '//
CC     & 'the output'
CC      write(COF,*) ' DEFAULT       to report all defaults in the output'
CC      write(COF,*) ' KEYWORDs      to explain all possible Keywords'
CC      write(COF,*) ' UNITs         to report all possible User Units'
CC      write(COF,*) ' EchoSCH       to report all changes due to '//
CC     & 'schedules'
CC      write(COF,*) ' OnScreen [Time] [Progress] [TotFlow] [Niter] '//
CC     & '[Error]'
CC      write(COF,*) ' DEBUG  1..3   echo *.TMS(1) *.DAF(2) both(3)'
CC      write(COF,*) ' LoopRho       recalculatie Rho(zone) at iteration'
CC      write(COF,*) ' CHECKC        to warn if condensation occurs'
CC
CC       RETURN
CC       END
CC
