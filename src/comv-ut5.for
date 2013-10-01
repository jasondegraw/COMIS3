C+*********************************************************** comv-ut5.f
Ch**********************************************************************
	SUBROUTINE FrTo
C***********************************************************************
C
C in the routine FrTo two things are done all with the names:
C 1 a pointer is made from the link-squence number (number of the dataline under
C   NET-LIN) to the first element that belongs to that link in the array LDat
C 2 the array FromTo is filled with the zonenumbers externalnode numbers and
C   the number of the special pressures. Ths is done with the arrary FromToS
C   which contains the names for zones, ext.nodes and the special pressures.
C   The Lstat array indicates whether the number is a zone, ext.node, or spec-
C   pressure.
C sept 1989 hcp
C Changes:
C@empa aw 1992apr02 Find the facade element numbers in a tree
C@empa aw 1993jun14 key for error return from looknam added,
C@empa              check key and report errors
C@empa aw 1993jun14 LStr added

Ch**********************************************************************

        IMPLICIT NONE
	INCLUDE 'comv-inp.inc'
	INCLUDE 'comv-uni.inc'

        INTEGER Line,L
        INTEGER  ExtNr,key,LStr
        INTEGER LENSTR

C Lstat is the link status:
C Lstat From	 To
C  0	zone	zone
C  1	 ex	zone
C  2	spec	zone
C  3	zone	 ex
C  4	 ex	 ex
C  5	spec	 ex
C  6	zone	spec
C  7	 ex	spec
C  8	spec	spec
C
	Do 100 Line=1,Nl

C get the pointer from Link-Type-Name to the pointer from User-Afc-Name
C (we put that value in pLiTyNa temporary)

	  CALL LookNam(Ntyp,LiTyNa(Line),UsTree,UsTreeN,
     &	  pLiLDat(Line),UsLarg,UsSmal,key)
          IF (key.EQ.1) THEN
             LStr=LENSTR(LiTyNa(Line))
             CALL INERR('&-NET-LINks:  LinkNr: '//LiNa(Line),
     &       'Link Type: '//LiTyNa(line)(1:LStr)//' does not exist '//
     &       'in &-NET-AIR !',.FALSE.,2)
             if (LiTyNa(line)(1:1).eq.'*') then
             call lasterror2('The reference to the Air Flow '//
     &       'Component type '//LiTyNa(line)(1:LStr)//' you used '//
     &       'should not start with an ''*''.','Asterisks are only '//
     &       'used to indicate the definition of a name.',2)
             end if
           ELSE
C now update pLiLDat so that it points to the element in LDat
C pLiLDat points from the link sequencenumber to the first element in Ldat,
C belonging to that link.

		pLiLDat(Line)=pUsrAfc(pLiLDat(Line))
                IF(test.GE.1 .AND. iecho.ge.4) THEN
		    WRITE(CRT,*) 'lnr=',Line,'pLiLDat=',plildat(line)
		ENDIF
	   ENDIF

	   L=Lstat(Line)

C below, the FromTo array is filled. You might expect 8 possibilities, here
C there are only 7 used because the special pressures (given values with "Pa"
C directly behind it) are directly put in FromTo during the reading of
C parameters in the routine inpdat.
C The FromTo array tells which room,extnode,or spec the link is comming from
C and where it goes to. IF the pressure in the From "room" (including stack) is
C higher than in the To "room" the flow will be positive (for passive elements,
C like cracks and all others but, not necessarily for fans)

	  IF (L.EQ.0) THEN
C from zone to zone
	    CALL LookNam(Nz,FromToS(1,Line),ZoTree,ZoTreeN,
     &	    FromTo(1,Line),ZoLarg,ZoSmal,key)
            IF (key.EQ.1) THEN
             LStr=LENSTR(FromToS(1,line))
             CALL INERR('&-NET-LINks:  LinkNr: '//LiNa(Line),
     &       'Frome Zone: '//FromToS(1,line)(1:LStr)//' does not '//
     &       'exist in &-NET-ZONes !',.FALSE.,2)
            ENDIF

	    CALL LookNam(Nz,FromToS(2,Line),ZoTree,ZoTreeN,
     &	    FromTo(2,Line),ZoLarg,ZoSmal,key)
            IF (key.EQ.1) THEN
             LStr=LENSTR(FromToS(2,Line))
             CALL INERR('&-NET-LINks:  LinkNr: '//LiNa(Line),
     &       'To Zone: '//FromToS(2,line)(1:LStr)//' does not '//
     &       'exist in &-NET-ZONes !',.FALSE.,2)
            ENDIF


	  ELSE IF(L.EQ.1) THEN
C from ex to zone
	    CALL LookNam(Nwind,FromToS(1,Line),ExTree,ExTreeN,
     &	    ExtNr,ExLarg,ExSmal,key)
            IF (key.EQ.1) THEN
             LStr=LENSTR(FromToS(1,line))
             CALL INERR('&-NET-LIN:ks  LinkNr: '//LiNa(Line),
     &       'Ext Node: '//FromToS(1,line)(1:LStr)//' does not '//
     &       'exist in &-NET-EXT !',.FALSE.,2)
C@empa aw 1999nov29 Look for facade name only if external node exists.
CC            ENDIF
            ELSE 
	        CALL LookNam(Nf,FacadeNa(ExtNr),FeTree,FeTreeN,
     &	    FromTo(1,Line),FeLarg,FeSmal,key)
              IF (key.EQ.1) THEN
                LStr=LENSTR(FacadeNa(ExtNr))
                CALL INERR('&-NET-EXT:  ',
     &       'Facade Element: '//FacadeNa(ExtNr)(1:LStr)//' does not '//
     &       'exist, in all wind directions blocks, in &-CP-VALUes !',
     &       .FALSE.,2)
              ENDIF
            ENDIF

	    CALL LookNam(Nz,FromToS(2,Line),ZoTree,ZoTreeN,
     &	    FromTo(2,Line),ZoLarg,ZoSmal,key)
            IF (key.EQ.1) THEN
             LStr=LENSTR(FromToS(2,Line))
             CALL INERR('&-NET-LINks:  LinkNr: '//LiNa(Line),
     &       'To Zone: '//FromToS(2,line)(1:LStr)//' does not '//
     &       'exist in &-NET-ZONes !',.FALSE.,2)
            ENDIF

	  ELSE IF(L.EQ.2) THEN
C from spec to zone
	    CALL LookNam(Nz,FromToS(2,Line),ZoTree,ZoTreeN,
     &	    FromTo(2,Line),ZoLarg,ZoSmal,key)
            IF (key.EQ.1) THEN
             LStr=LENSTR(FromToS(2,Line))
             CALL INERR('&-NET-LINks:  LinkNr: '//LiNa(Line),
     &       'To Zone: '//FromToS(2,line)(1:LStr)//' does not '//
     &       'exist in &-NET-ZONes !',.FALSE.,2)
            ENDIF

	  ELSE IF(L.EQ.3) THEN
C from zone to ex
	    CALL LookNam(Nz,FromToS(1,Line),ZoTree,ZoTreeN,
     &	    FromTo(1,Line),ZoLarg,ZoSmal,key)
            IF (key.EQ.1) THEN
             LStr=LENSTR(FromToS(1,line))
             CALL INERR('&-NET-LINks:  LinkNr: '//LiNa(Line),
     &       'Frome Zone: '//FromToS(1,line)(1:LStr)//' does not '//
     &       'exist in &-NET-ZONes !',.FALSE.,2)
            ENDIF

	    CALL LookNam(Nwind,FromToS(2,Line),ExTree,ExTreeN,
     &	    ExtNr,ExLarg,ExSmal,key)
            IF (key.EQ.1) THEN
             LStr=LENSTR(FromToS(2,line))
             CALL INERR('&-NET-LINks:  LinkNr: '//LiNa(Line),
     &       'Ext Node: '//FromToS(2,line)(1:LStr)//' does not '//
     &       'exist in &-NET-EXT !',.FALSE.,2)
            ENDIF

	    CALL LookNam(Nf,FacadeNa(ExtNr),FeTree,FeTreeN,
     &	    FromTo(2,Line),FeLarg,FeSmal,key)
            IF (key.EQ.1) THEN
             LStr=LENSTR(FacadeNa(ExtNr))
             CALL INERR('&-NET-EXT:  ',
     &       'Facade Element: '//FacadeNa(ExtNr)(1:LStr)//' does not '//
     &       'exist, in all wind directions blocks, in &-CP-VALUes !',
     &       .FALSE.,2)
            ENDIF

	  ELSE IF(L.EQ.4) THEN
C from ex to ex
	    CALL LookNam(Nwind,FromToS(1,Line),ExTree,ExTreeN,
     &	    ExtNr,ExLarg,ExSmal,key)
            IF (key.EQ.1) THEN
             LStr=LENSTR(FromToS(1,line))
             CALL INERR('&-NET-LINks:  LinkNr: '//LiNa(Line),
     &       'Ext Node: '//FromToS(1,line)(1:LStr)//' does not '//
     &       'exist in &-NET-EXT !',.FALSE.,2)
            ENDIF

	    CALL LookNam(Nf,FacadeNa(ExtNr),FeTree,FeTreeN,
     &	    FromTo(1,Line),FeLarg,FeSmal,key)
            IF (key.EQ.1) THEN
             LStr=LENSTR(FacadeNa(ExtNr))
             CALL INERR('&-NET-EXT:  ',
     &       'Facade Element: '//FacadeNa(ExtNr)(1:LStr)//' does not '//
     &       'exist, in all wind directions blocks, in &-CP-VALUes !',
     &       .FALSE.,2)
            ENDIF

	    CALL LookNam(Nwind,FromToS(2,Line),ExTree,ExTreeN,
     &	    ExtNr,ExLarg,ExSmal,key)
            IF (key.EQ.1) THEN
             LStr=LENSTR(FromToS(2,line))
             CALL INERR('&-NET-LINks:  LinkNr: '//LiNa(Line),
     &       'Ext Node: '//FromToS(2,line)(1:LStr)//' does not '//
     &       'exist in &-NET-EXT !',.FALSE.,2)
            ENDIF

	    CALL LookNam(Nf,FacadeNa(ExtNr),FeTree,FeTreeN,
     &	    FromTo(2,Line),FeLarg,FeSmal,key)
            IF (key.EQ.1) THEN
             LStr=LENSTR(FacadeNa(ExtNr))
             CALL INERR('&-NET-EXT:  ',
     &       'Facade Element: '//FacadeNa(ExtNr)(1:LStr)//' does not '//
     &       'exist, in all wind directions blocks, in &-CP-VALUes !',
     &       .FALSE.,2)
            ENDIF


	  ELSE IF(L.EQ.5) THEN
C from spec to ex
	    CALL LookNam(Nwind,FromToS(2,Line),ExTree,ExTreeN,
     &	    ExtNr,ExLarg,ExSmal,key)
            IF (key.EQ.1) THEN
             LStr=LENSTR(FromToS(2,line))
             CALL INERR('&-NET-LINks:  LinkNr: '//LiNa(Line),
     &       'Ext Node: '//FromToS(2,line)(1:LStr)//' does not '//
     &       'exist in &-NET-EXT !',.FALSE.,2)
            ENDIF


	    CALL LookNam(Nf,FacadeNa(ExtNr),FeTree,FeTreeN,
     &	    FromTo(2,Line),FeLarg,FeSmal,key)
            IF (key.EQ.1) THEN
             LStr=LENSTR(FacadeNa(ExtNr))
             CALL INERR('&-NET-EXT:  ',
     &       'Facade Element: '//FacadeNa(ExtNr)(1:LStr)//' does not '//
     &       'exist, in all wind directions blocks, in &-CP-VALUes !',
     &       .FALSE.,2)
            ENDIF


	  ELSE IF(L.EQ.6) THEN
C from zone to spec
	    CALL LookNam(Nz,FromToS(1,Line),ZoTree,ZoTreeN,
     &	    FromTo(1,Line),ZoLarg,ZoSmal,key)
            IF (key.EQ.1) THEN
             LStr=LENSTR(FromToS(1,line))
             CALL INERR('&-NET-LINks:  LinkNr: '//LiNa(Line),
     &       'Frome Zone: '//FromToS(1,line)(1:LStr)//' does not '//
     &       'exist in &-NET-ZONes !',.FALSE.,2)
            ENDIF

	  ELSE IF(L.EQ.7) THEN
C from ex to spec
	    CALL LookNam(Nwind,FromToS(1,Line),ExTree,ExTreeN,
     &	    ExtNr,ExLarg,ExSmal,key)
            IF (key.EQ.1) THEN
             LStr=LENSTR(FromToS(1,line))
             CALL INERR('&-NET-LINks:  LinkNr: '//LiNa(Line),
     &       'Ext Node: '//FromToS(1,line)(1:LStr)//' does not '//
     &       'exist in &-NET-EXT !',.FALSE.,2)
            ENDIF

	    CALL LookNam(Nf,FacadeNa(ExtNr),FeTree,FeTreeN,
     &	    FromTo(1,Line),FeLarg,FeSmal,key)
            IF (key.EQ.1) THEN
             LStr=LENSTR(FacadeNa(ExtNr))
             CALL INERR('&-NET-EXT:  ',
     &       'Facade Element: '//FacadeNa(ExtNr)(1:LStr)//' does not '//
     &       'exist, in all wind directions blocks, in &-CP-VALUes !',
     &       .FALSE.,2)
            ENDIF

	  ENDIF

100	CONTINUE
	RETURN
	END


Ch**********************************************************************
	SUBROUTINE PriTree(str,N,NAME,NR,TREE,TREEN,LARGE,SMALL)
C***********************************************************************
C PriTree=Print Tree
C pritree prints the list of names, lineNr's the binary names-tree and
C the co-sorted binary tree with lineNr's . Large and Small are used to
C see where the tree is no further filled and to test if the input name
C can be found in the tree.
C sept 1989 hcp
Ch**********************************************************************
        IMPLICIT NONE
        INTEGER n,nr(*),treen(*)
	CHARACTER*(*) str,name(*),Tree(*),large,small
	INCLUDE 'comv-uni.inc'
        REAL r
        INTEGER I,M

	IF (N.LT.1) RETURN
	r=n
	m=2**(log(r)/log(2.))
	IF (m.LE.n) m=m+1
	WRITE(CRT,*)'Tree follows. The name of the parameter =', str
	WRITE(CRT,*)'Name,LineNr, treeName, Tree number'

	DO 100 i=1,m
	WRITE(CRT,1000) name(i),nr(i),tree(i),treen(i)
100	CONTINUE
1000	FORMAT(1X,A10,I6,1X,A10,I6)
	WRITE(CRT,*) 'border values large, small =',large,small
	WRITE(CRT,*) ' '

	RETURN
	END

Ch**********************************************************************
	SUBROUTINE ArrayCheck
C***********************************************************************
C lbl bs 1991jul02
C
C The subroutine ArrayCheck checks if the parameters which are set in
C the include file comv-inp.inc are big enough. When they are to small
C the subroutine NamSort would write beyond the array border without any
C error message.
C Therefore this routine stops the program if an error is encountered.
C
C Changes:
C@empa aw 1992apr02 check the array for facade element names
C@empa aw 1993feb05 check arrays not with actual values nz,nl etc. but with
C@                  the maxparameters MaxZ, MaxL etc.
C@                  This is made, that the eventual error message will occur
C@                  in the first run after compilation. A user who is not
C@                  also a programmer could nothing do with a massage from
C@                  this routine.
C@empa aw1993may26  "write(*,.." replaced with "call error(.."

Ch**********************************************************************
        IMPLICIT NONE
	INCLUDE 'comv-inp.inc'
        INTEGER min
	character smin*5

C@lbl bvs 1997Jul25 change Ifix to Int so that when the Watcom compiler is
C		run with double precision as default it won't complain that the
C		argument to int() should be real instead of double
	min	 = 2**(Int(log(Float(MaxZ))/log(2.0)+1)) - 1

	IF (min.GT.MZoT) THEN
                write(smin,3000) min
		CALL ERROR2('Parameter MZoT too small.'//
     &		' Must be at least ',smin,3)
	ENDIF

	min	 = 2**(Int(log(Float(MaxW))/log(2.0)+1)) - 1

	IF (min.GT.MExT) THEN
                write(smin,3000) min
		CALL ERROR2('Parameter MExT too small.'//
     &		' Must be at least ',smin,3)
	ENDIF

	min	= 2**(Int(log(Float(MaxL))/log(2.0)+1)) - 1

	IF (min.GT.MLiT) THEN
                write(smin,3000) min
		CALL ERROR2('Parameter MLiT too small.'//
     &		' Must be at least ',smin,3)
	ENDIF

	min = 2**(Int(log(Float(MaxTyp))/log(2.0)+1)) - 1

	IF (min.GT.MTyT) THEN
                write(smin,3000) min
		CALL ERROR2('Parameter MTyT too small.'//
     &		' Must be at least ',smin,3)
	ENDIF

	min	 = 2**(Int(log(Float(MaxF))/log(2.0)+1)) - 1

	IF (min.GT.MFeT) THEN
                write(smin,3000) min
		CALL ERROR2('Parameter MFeT too small.'//
     &		' Must be at least ',smin,3)
	ENDIF

3000    format(i5)

	RETURN
        END

Ch***********************************************************************
        SUBROUTINE DeleteWord(string,p)
C***********************************************************************
C Purpose: DeleteWord  deletes the word that starts ONE after the given position
C          in String.
C Module : #, TG, hcp/ sep 15 1995
C Changes:
C Limits :
C
C Pass parameters:
C
C IO # Name    unit		 description
C IO 1 String  -            string which may contain more words
C I  2 P       -            start position in string
C
C example:
C call
Ch**********************************************************************
        IMPLICIT NONE
        INTEGER p
        CHARACTER*(*) string
        INTEGER k,L,Ls
        character*160 word
        INTEGER LENSTR

        k=p
C@tno jcp 1996Apr07_15:51:37 line moved 2 up
        ls=lenstr(String)
        call GetWrd(String,k,ls,word)
c        write(*,*)'word=',word
        l=lenstr(word)
c        write(*,*)'lsword=',l

        String(p:)=String(p+l+1:Ls)
	RETURN
	END

Ch***********************************************************************
        SubRoutine InsertWord(string,p,word)
C***********************************************************************
C Purpose: InsertWord  inserts the word starting at the position p in string
C          in String.
C Module : #, TG, hcp/ sep 15 1995
C Changes:
C Limits :
C
C Pass parameters:
C
C IO # Name    unit		 description
C IO 1 String  -            string which may contain more words
C I  2 P       -            start position in string
C I  3 P       -            start position in string
C
C example:
C call
Ch**********************************************************************
        IMPLICIT NONE
        INTEGER p,k,L
        CHARACTER*(*) string,word
        character*160 string2
        INTEGER LENSTR

        k=p
        l=lenstr(word)
        String2=string(p:)
        k=lenstr(String2)
        String(p:)=word(1:l)//String2(1:k)
	RETURN
	END


Ch***********************************************************************
        SubRoutine ReplaceWord(string,p,word)
C***********************************************************************
C Purpose: ReplaceWord  deletes the word ONE after the position p in string
C          and inserts the passed Word.
C Module : #, TG, hcp/ sep 15 1995
C Changes:
C Limits :
C
C Pass parameters:
C
C IO # Name    unit		 description
C IO 1 String  -            string which may contain more words
C I  2 P       -            start position in string
C I  3 P       -            start position in string
C
C example:
C call
Ch**********************************************************************
        IMPLICIT NONE
        INTEGER p
        CHARACTER*(*) string,word

c        write(*,*) 'replace word at position p=',p
c        write(*,*) 'before delete',string
        call DeleteWord(string,p)
c        write(*,*) 'after delete',string
        call InsertWord(string,p,word)
c        write(*,*) 'after replace',string
c        call ho(' ',' ')
	RETURN
	END

Ch**********************************************************************
       INTEGER FUNCTION ICindex(line,pattern)
C***********************************************************************
C  ICindex is Case Insensitive index
C  The position of 'pattern' in 'line' is returned
C  Both 'line' and the searched 'pattern' are internally made UpperCase
C  Then the non blank part in 'pattern' is used in a normal index
C
C  This is an expensive routine as both strings are looped through to make
C  then UpperCase and the determination of the non-blank length of Pattern
C  needs another loop
C
C  To avoid 'line' and 'pattern' to be returned as UpperCase they are first
C  copied into the local variables 'Line2' and 'Pattrn2' that have fixed
C  lengths of 160 char.
C
C Limits : maximum length of line and of pattern is 160
C
C Pass parameters:
C
C IO # Name    unit              description
C I  1 line     -                line possibly containing the pattern
C I  2 pattern  -                pattern to be searced in line
C
C Example
C if 'line' contains 'this is a test line inwhich the word MyIndex is found'
C                     |                                     |
C                     1234567890123456789012345678901234567890
C                              1         2         3
C and 'pattern'='myINDEX'
C ICindex(line,pattern) returns 39
C
C
C***********************************************************************
        IMPLICIT NONE
       character*(*) line,pattern
C@empa aw 2000feb07 
CC       character*160 line2,pattrn2
       character*2000 line2,pattrn2
       INTEGER i,L1,L2,Nr
        INTEGER LENSTR

C determine the non-blank length of line and pattern.
C I assume that Line and Pattern are normally much shorter than 160.
C So it is faster to determine the length of the arguments than Line2 and
C Pattrn2

       L1=lenstr(line)
       L2=lenstr(pattern)
C@tno jcp 1996Apr24_14:51:21 added exit and =0 to ICindex if PatternLength=0
       if (L2.eq.0) then
         ICindex=0
         goto 99
       end if

C copy passparameters into local strings

       line2=line
       pattrn2=pattern

C make both UpperCase but do it locally and don't call UPPERC

        DO 10 I=1,L1
          NR=ICHAR(line2(i:i))
	  IF (NR .GE. 97 .AND. NR .LE. 122) THEN
            line2(i:i)=CHAR(NR-32)
	  ENDIF
10      CONTINUE

        DO 20 I=1,L2
          NR=ICHAR(pattrn2(i:i))
	  IF (NR .GE. 97 .AND. NR .LE. 122) THEN
            pattrn2(i:i)=CHAR(NR-32)
	  ENDIF
20      CONTINUE

C find the position

       ICindex=index(line2,pattrn2(1:L2))

C@tno jcp 1996Apr24_14:51:07 added 99 to ICindex
99     continue
       return
       end





Ch***********************************************************************
        INTEGER FUNCTION iCol1ABC(start,string)
C***********************************************************************
C Purpose: iCol1ABC (i=INTEGER Col1=First Column ABC=Alphabetical character)
C          Get the first character in the 'string' passed from position 'start'
C          which is an alphabetical character: A..Z, a..z.
C          this position (column) is returned
C          if not found 0 is returned
C Module : #, TG, hcp/ sep 15 1995
C Changes:
C Limits :
C
C Pass parameters:
C
C IO # Name    unit		 description
C IO 1 String  -            string which may Q12CO2
C I  2 start   -            start position in string
C
C example:
C call
Ch**********************************************************************
        IMPLICIT NONE
        INTEGER start
        CHARACTER*(*) string
        INTEGER L,I,Ic

        INTEGER LENSTR

        L=Lenstr(string)
        I=start
 1      if (i.le.l) then
          ic=Ichar(string(i:i))
          if ( ( (ic.ge.65).and.(ic.le.90)  ).or.
     &         ( (ic.ge.97).and.(ic.le.122) ) )  then
            goto 900
          else
            i=i+1
            goto 1
          end if
        end if

        i=0
900     continue
        iCol1ABC=i
        return

        END
