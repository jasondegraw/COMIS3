C+*********************************************************** comv-ut2.f
Ch**********************************************************************
	SUBROUTINE LookNam(Nz,name1,binTree,binNr,nr,large,small,key)
C			   |  |	    |	    |	  |  large=border of the tree
C			   |  |	    |	    |	  |  small=border of the tree
C			   |  |	    |	    |	  zone number for name1
C			   |  |	    |	    zone number in tree format
C			   |  |	    binary tree of zone names(lowest up,left)
C			   |  name to look up in tree and RETURN zone number
C			   number of zones

C**********************************************************************
C original CALL was:
C	CALL Looknam(Nz,name1,binTree,binNr,nr,large,small)

C Look up a Name in binTree
C lookName goes through the bintree to look for a name1
C the bintree contains the list of names for zones but sorted differently
C when found, the corresponding zone number is RETURNed
C the tree 's limits are indicated by large and small values that go beyond the
C smallest and largest names in the original list of names.
C IF a name1 is out of the range between small and large, or IF it isnot found
C the program pauses.
C sept 1989 hcp
C
C Changes:
C@empa aw 1993jun14 key for error return added
C@empa aw 1993jun14 error report canceled
Ch**********************************************************************
        IMPLICIT NONE

C Nz =number of names in the tree or in the list of names
C binNr=zonenumber for a zonename in the binTree
C nr   = zonenumber
C element= element in binTree
C me	 = maximum number of elements in the binTree, 2**N-1
        INTEGER Nz,binNr(*),nr
C name1= name to look up
C binTree= list of names sorted as a binary tree
C small	 = name one smaller (in ASCII comparison) than the smallest zonename
C large	 = name one larger  (in ASCII comparison) than the largest  zonename
	CHARACTER*(*) name1,bintree(*),large , small
C lastel = the last three names from the tree used in comparison, will be dis
C	   played IF the name is not found.
	CHARACTER lastel(3)*20
C been	 = flag array to keep track of places in the tree we have already been,
C	   this prevents endless jumping up, down at nonexisting names
C *** The dimension of the array been must be a power of two at least as large
C		as the largest of:
C			MaxL (max number of links),
C			MaxZ (max number of zones),
C			MaxF (max number of facade elements) and
C			MaxW (max number of wind pressure points)

C@lbl bvs 1995Oct4 increased 'been' dimension from 300 to 2048 because of
C  larger Link array (MaxL = 1500 at this time)

	INTEGER been(2048) ,element,me
        REAL r
        INTEGER I
        INTEGER KEY

        Key=0
C calculate the maximum number of elements in the tree
        if (nz.GT.0)then
	  r=nz
C@lbl bvs 1998Aug11 add small epsilon for the Digital Fortran compiler
C	which has rounding problems
CC	  me=int(log(r)/log(2.))
	  me=int(log(r)/log(2.)+0.0001)
	  IF ((2**me).LE. nz) me=me+1
	  me=2**me-1
        else
          me=1
        endif
C go out IF the name is outside the range
	IF (name1.GE.large) GOTO 998
	IF (name1.LE.small) GOTO 998

C zero been (we have been on no place in the tree, yet)
	DO 1 I=1,me
	  been(I)=0
1	CONTINUE

	element=1
C initialize (to avoid ForCheck errors)
	lastel(1)='-init-'
	lastel(2)='-init-'

C no test made for non existing names yet (test on the element number)

C loop through the tree
50	been(element)=1
	lastel(3)=lastel(2)
	lastel(2)=lastel(1)
	lastel(1)=bintree(element)

C test IF name1 > = < tree element
	IF (name1.GT.bintree(element)) THEN
	  GOTO 200
	ELSE IF (name1.eq.bintree(element)) THEN
	  GOTO 300
	ELSE
C name1 .LT. bintree(elementr)
	  GOTO 100
	ENDIF

C go one element to the left
100	element=element-1

	if (element .eq. 0) goto 999

C test IF we have been there before, THEN the name doesnot exist and lies
C between surrounding elements

	IF (been(element).eq.1) GOTO 999

C If the value of the tree for this element is small THEN we out of the filled
C area of the tree and the name does not exist
	IF (bintree(element).eq.small) GOTO 999
	GOTO 50

C go one element to the right and down
200	element=element*2+1
	IF (element.GT.me) GOTO 999
	GOTO 50

C found the name1
300	nr=binNr(element)
	RETURN

998     CONTINUE
999     CONTINUE
        KEY=1
	RETURN
	END

Ch**********************************************************************
	SUBROUTINE NamSort(nz,name,nr,binTree,binNr,large,small)
C text here for zone.	   |  |	   |  |	      |
C routine is used for	   |  |	   |  |	      zone numbers in tree format
C other names aswell	   |  |	   |  zone names in tree format
C			   |  |	   zone numbers (same sequence as name)
C			   |  zone names
C			   number of zones used
C**********************************************************************

C Sort the character array name(*),first with heapsort,and THEN into bintree(*).
C I do not know a better way to sort directly into a binary tree, Heapsort uses
C a kind of binary tree first but that tree is not sorted in the left-right di-
C rection. As Heapsort is pretty fast, and the number of zones is usually small,
C (less than 1000) I donot worry about this.
C The accompanying array nr(*) is moved in the same way into binNr(*).
C This means that in the end finding a name in an element of bintree, the
C original zone number is in the same element in the array binNr.
C The zone number is here just the sequence number of the zone names in the
C input file at the datablock &-NET-ZONes.

C The tree looks like this: (element numbers)

C			  (1) (lowest value)
C			   1 <---example of what might be in the tree
C				 Not only numbers are allowed, all ASCII
C
C		 (2)		      (3)
C		  2		       9
C
C	    (4)	      (5)	 (6)	    (7)
C	     3	       6	 10	    9999
C	  (8) (9)  (10) (11)  (12) (13)	 (14) (15) (highest value, if used)
C	   4   5     7	  8   0000 9999
C at a fork, the lower value is at the left, so for instance:
C   (4) < (8) < (9)
C sept 1989 hcp
Ch**********************************************************************
        IMPLICIT NONE


C nz= number of zones
C nlayer=number of layers in the tree
C element=element in the tree
C nlarger=the element to the right and one down in the tree
        INTEGER nz,nr(*),binNr(*)
C name= zone names
C large= a name that is one(in ASCII comparisons) larger than the largest name
C	 in the array name(*)
C small= a name that is one(in ASCII comparisons)smaller than the smallest name
C	 in the array name(*)
	CHARACTER*(*) name(*),bintree(*),large,small
C nr   = the array with the zone numbers (the sequence in which the zone names
C	 appear in the input file.
C me   = maximum number of elements in the tree
C bintree= binary tree of zone names
C binNr	 = zone number (from nr(*)) moved in the same way as name(*)
C inused = while filling the tree we keep track of the next element in a layer
C	   to be filled with names from name(*)
C	   first we fill element 1,2,4,8,... THEN the contents of unused is:
C	   0,3,5,9,.....
C@empa aw 2003nov10 Konrath: 4000 links, 2000 zones
CC        INTEGER unused(10)
        INTEGER unused(12)
        INTEGER me,nLayer,element,nlarger
        INTEGER I,L,M,LAYER
        INTEGER LENSTR
        REAL R

	IF (Nz.LT.1) RETURN

	DO 1 I=1,10
	  unused(i)=0
1	CONTINUE

	IF (nz.GT.1) THEN
C call heap sort from numerical recipies
	CALL Sort2(nz,name,nr)

	ENDIF

C large wil be one CHARACTER value larger in ASCII value than name(Nz)
C I am hesitating to use nonprintables in large and small
	large=name(nz)
	L=lenstr(large)
C the ~ is character 126
	I=ICHAR(large(l:l))
	IF (I.LT.126) THEN
	  large(l:l)=CHAR(I+1)
	ELSE
C this is not a good solution but it will work I think
	  large(1:l)='~'
	ENDIF

C small will be one character value smaller in ASCII than name(1)
	small=name(1)
	L=lenstr(small)
	I=ICHAR(small(l:l))
	IF (I.GT.32)THEN
	  small(l:l)=CHAR(I-1)
	ELSE
	  IF (L.GT.1) THEN
	    small=small(1:l-1)
	  ELSE
	    small=' '
	  ENDIF
	ENDIF

C calculate the number of layers in the tree
	R=nz
	nLayer=int(LOG(R)/LOG(2.))
	IF (2**nLayer.LE.Nz) nLayer=nLayer+1
	IF (2**nLayer.LE.Nz) nLayer=nLayer+1
C maximum number of layers
	me=2**nlayer-1

C start saying we didn't use element 1
	Unused(1)=1
C set the counter for the element to pick from name and nr to 1
	m=1

C loop from low to high through the layers of the tree to find the unused
C element which is the lowest in the tree
C start with a layer below the lowest layer
21	layer=nLayer+1

20	  layer=layer-1
	  element=unused(layer)
C IF the element is 0 it means this value is not asigned look for another
	IF (element.EQ.0) GOTO 20

C set unused for the layer to 0 (to prevent that we might use it another time)
	unused(layer)=0
C fill the tree with the name and BinNr with the nr
	bintree(element)=name(m)
	binNr(element)=nr(m)

C the next larger element might never be filled, but we have to test on that
C value later when using the tree to find a name, therefore we set it to a de-
C fault high name, large , and the left side element to small.
	nlarger=2*element+1
	IF (nlarger.LE.me) THEN
	bintree(nlarger)=large
	bintree(nlarger-1)=small
	ENDIF

C set counter to next name in name(*) and number in nr(*)
	m=m+1
C IF m>Nz we are already through the array Name, the job is finished
	IF (m.GT.Nz) GOTO 40

30	IF (layer.LT.nLayer) THEN
C we go down, to the left, the direction of the flattest descend
	    element=element*2
	    layer=layer+1

C flag that the element to the right is not used yet
	    unused(layer)=element+1
C fill the tree and binNr
	    bintree(element)=name(m)
	    binNr(element)=nr(m)

C the next larger element might never be filled, but we have to test on that
C value later when using the tree to find a name, therefore we set it to a de-
C fault high name, large , and the left side element to small.
	    nlarger=2*element+1
	    IF (nlarger.LE.me) THEN
		bintree(nlarger)=large
		bintree(nlarger-1)=small
	    ENDIF

C set counter to next name in name(*) and number in nr(*)
	    m=m+1

C IF m>Nz we are already through the array Name, the job is finished
	    IF (m.GT.Nz) GOTO 40

C loop to the lower layers
	    GOTO 30

	ELSE
C ELSE Layer=last layer go looking for an unused element in the tree

	    GOTO 21

	ENDIF

40	CONTINUE

	IF (nz.GT.1) THEN
C incase we want to use the names later (for LiTyNa we do) we have to sort
C them back in the original order (order of lineNr in the input)
	 CALL Sort3(nz,nr,name)
	ENDIF

C	WRITE(CRT,*) 'bintree(i),binNr(i)'
C	DO 45 I=1,me
C	  WRITE(CRT,*) i,bintree(i),binNr(i)
C45	CONTINUE
C	WRITE(CRT,*) 'large small '
C	WRITE(CRT,*) large, small

	RETURN
	END


Ch**********************************************************************
	SUBROUTINE SORT2(N,RA,RB)
C                        | |  |
C                        | |  INTEGER; co sorted
C                        | character array; will be sorted
C                        number of elements
C**********************************************************************
C This routine is taken from the book numerical recipies, it is a heap sort.
C Changed to have RA as a character array, also RRA is a character string
C RRA is limited to 20 characters!!!!
C sept 1989 hcp
Ch**********************************************************************


        IMPLICIT NONE
        INTEGER N
        character*(*) RA(N)
        INTEGER RB(N),RRB
	CHARACTER RRA*20
        INTEGER I,J,IR,L
	L=N/2+1
	IR=N
10	CONTINUE
	IF(L.GT.1)THEN
	  L=L-1
	  RRA=RA(L)
	  RRB=RB(L)
	ELSE
	  RRA=RA(IR)
	  RRB=RB(IR)
	  RA(IR)=RA(1)
	  RB(IR)=RB(1)
	  IR=IR-1
	  IF(IR.EQ.1)THEN
	    RA(1)=RRA
	    RB(1)=RRB
	    RETURN
	  ENDIF
	ENDIF
	I=L
	J=L+L
20	IF(J.LE.IR)THEN
	  IF(J.LT.IR)THEN
	    IF(RA(J).LT.RA(J+1))J=J+1
	  ENDIF
	  IF(RRA.LT.RA(J))THEN
	    RA(I)=RA(J)
	    RB(I)=RB(J)
	    I=J
	    J=J+J
	  ELSE
	    J=IR+1
	  ENDIF
	GO TO 20
	ENDIF
	RA(I)=RRA
	RB(I)=RRB
	GO TO 10
	END


Ch**********************************************************************
	SUBROUTINE SORT3(N,RA,RB)
C                        | |  |
C                        | |  string co sorted
C                        | INTEGER will be sorted
C                        number of elements

C**********************************************************************
C This routine is taken from the book numerical recipies, it is a heap sort.
C Changed to have RA as a character array, also RRA is a character string
C RRA is limited to 20 characters!!!!
C sept 1989 hcp
Ch**********************************************************************

        IMPLICIT NONE
        INTEGER N
	character*(*) RB(N)
        INTEGER RA(N),RRA
	CHARACTER RRB*20
        INTEGER I,J,L,IR
	L=N/2+1
	IR=N
10	CONTINUE
	IF(L.GT.1)THEN
	  L=L-1
	  RRA=RA(L)
	  RRB=RB(L)
	ELSE
	  RRA=RA(IR)
	  RRB=RB(IR)
	  RA(IR)=RA(1)
	  RB(IR)=RB(1)
	  IR=IR-1
	  IF(IR.EQ.1)THEN
	    RA(1)=RRA
	    RB(1)=RRB
	    RETURN
	  ENDIF
	ENDIF
	I=L
	J=L+L
20	IF(J.LE.IR)THEN
	  IF(J.LT.IR)THEN
	    IF(RA(J).LT.RA(J+1))J=J+1
	  ENDIF
	  IF(RRA.LT.RA(J))THEN
	    RA(I)=RA(J)
	    RB(I)=RB(J)
	    I=J
	    J=J+J
	  ELSE
	    J=IR+1
	  ENDIF
	GO TO 20
	ENDIF
	RA(I)=RRA
	RB(I)=RRB
	GO TO 10
	END
Ch**********************************************************************
	SUBROUTINE SORT4(N,RA,RB,RC)
C                        | |  |
C                        | |  INTEGER co sorted
C                        | INTEGER will be sorted
C                        number of elements

C**********************************************************************
C This routine is taken from the book numerical recipies, heap sort.
Ch**********************************************************************

        IMPLICIT NONE
        INTEGER N
        INTEGER RA(N),RB(N),RC(N),RRA,RRB,RRC
        INTEGER I,J,L,IR
	L=N/2+1
	IR=N
10	CONTINUE
	IF(L.GT.1)THEN
	  L=L-1
	  RRA=RA(L)
	  RRB=RB(L)
	  RRC=RC(L)
	ELSE
	  RRA=RA(IR)
	  RRB=RB(IR)
	  RRC=RC(IR)
	  RA(IR)=RA(1)
	  RB(IR)=RB(1)
	  RC(IR)=RC(1)
	  IR=IR-1
	  IF(IR.EQ.1)THEN
	    RA(1)=RRA
	    RB(1)=RRB
	    RC(1)=RRC
	    RETURN
	  ENDIF
	ENDIF
	I=L
	J=L+L
20	IF(J.LE.IR)THEN
	  IF(J.LT.IR)THEN
	    IF(RA(J).LT.RA(J+1))J=J+1
	  ENDIF
	  IF(RRA.LT.RA(J))THEN
	    RA(I)=RA(J)
	    RB(I)=RB(J)
	    RC(I)=RC(J)
	    I=J
	    J=J+J
	  ELSE
	    J=IR+1
	  ENDIF
	GO TO 20
	ENDIF
	RA(I)=RRA
	RB(I)=RRB
	RC(I)=RRC
	GO TO 10
	END


C***********************************************************************
C@tno jcp 1996Jun17_15:50:46 variab added
	SUBROUTINE ReadSet(keyw,CIFs,COFs,UOFs,UOFunf,
CC     &          COSs,SepStr,OCase)
     &          COSs,SepStr,OCase,variab,Clines10)
C***********************************************************************
C ReadSet=READ the third part of the file COMIS.SET
C get the word after the keyword keyw and put that in file
C original: sept 1989 hcp (original name: READCWF)
C The keywords have to appear in the following format in the SET file:
C (the other formats are not supported anymore)
C  &-COMIS
C     INPUT [file-in]
C    OUTPUT [comis output file (formatted)]
C    USEROUTF [user file (formatted)] | USEROUTU [user file (unformatted)]
C    TABLES [file-table]
C
C The keywords OUTPUT; USEROUTF, USEROUTU and TABLES are optional.
C
C changes:
C@empa aw 1993may26 "write(*,.." replaced with "call(error2(.."
C@empa aw 1994mar10 Options for separator string and spreadsheet output
C@                filename added at the keyword TABLES
Ch***********************************************************************

        IMPLICIT NONE
	INCLUDE 'comv-uni.inc'

C@tno jcp 1996Jun17_15:50:26 add variab
CC        CHARACTER*(*) keyw,CIFs,COFs,UOFs,COSs,SepStr
        CHARACTER*(*) keyw,CIFs,COFs,UOFs,COSs,SepStr,variab
        INTEGER   UOFunf,OCase,Clines10
	CHARACTER*80 line,word,word1,word2,word3
        INTEGER I,L,K,begin,sawit(4)
        INTEGER ISTRSTR,LENSTR
	DATA sawit/4*0/

C	sawit = 0
C	sawit tells if the keyword has already be seen.
C	index 1 is for 	INPUT
C	      2		OUTPUT
C	      3		USEROUTF, USEROUTU
C	      4		TABLES

2	READ (SET,'(A)',END=999) line
	begin = 1
	I=Istrstr(line,keyw,begin)

	IF (I.EQ.0) THEN
C keep reading lines to look for the keyword
	  GOTO 2
	ENDIF

10      Read(SET,'(A)', END = 100) line

	l = lenstr(line)
	k = 0

C@tno jcp 1996Jun13_23:48:56 added variab= before getWS
        variab=' read 3rd part of set, first word in line '

        clines10=k

        CALL getws(line,k,l,word,' ')


	if (index(word,'&-CO') .gt. 0) goto 100
C@tno jcp 1996Jul11_14:41:13 may not there be an empty line in Set?
C worse: You gEt and empty line if it starts with # from the call of GetWs
CC        if (lenstr(line) .eq. 0) goto 100
        if (lenstr(line) .eq. 0) goto 10
	if (k .ge. l) then
	  CALL Error2('SET-file: No filename specified for keyword',
     &			word,3)
	endif

	if (index(word,'INPUT') .gt. 0) then
		if (sawit(1) .ne. 0) then
		  CALL Error('Multiple INPUT keywords in SET file',3)
		endif
		sawit(1) = 1
C@tno jcp 1996Jun13_23:48:56 added variab= before getWS
        variab=' read CIFs'
        clines10=k
                CALL getws(line,k,l,CIFs,' ')
	endif

	if (index(word,'OUTPUT') .gt. 0) then
		if (sawit(2) .ne. 0) then
		  CALL Error('Multiple OUTPUT keywords in SET file',3)
		endif
		sawit(2) = 1
C@tno jcp 1996Jun13_23:48:56 added variab= before getWS
        variab=' read COFs'
        clines10=k
                CALL getws(line,k,l,COFs,' ')
	endif

C@tno jcp 1996Jul11_14:35:29 USEROUT must start on 1 (so ;USEROUT will be commen
CC        if (index(word,'USEROUT') .gt. 0) then
        if (index(word,'USEROUT') .eq. 1) then
		if (sawit(3) .ne. 0) then
		  CALL Error('Multiple USEROUT keywords in SET file',3)
		endif
		sawit(3) = 1
C@tno jcp 1996Jun13_23:48:56 added variab= before getWS
                variab=' read UserOut unformatted'
                clines10=k
                CALL getws(line,k,l,UOFs,' ')
		if (index(word,'USEROUTU') .gt. 0) then
			UOFunf = 1
		endif
	endif

	if (index(word,'TABLE') .gt. 0) then
		if (sawit(4) .ne. 0) then
		  CALL Error('Multiple TABLE keywords in SET file',3)
		endif
		sawit(4) = 1
C@tno jcp 1996Jun13_23:48:56 added variab= before getWS
        variab=' read COS argument 1'
        clines10=k
                CALL getws(line,k,l,word1,' ')
C@tno jcp 1996Jun13_23:48:56 added variab= before getWS
        variab=' read COS argument 2'
        clines10=k
                CALL getws(line,k,l,word2,' ')
C@tno jcp 1996Jun13_23:48:56 added variab= before getWS
        variab=' read COS argument 3'
        clines10=k
                CALL getws(line,k,l,word3,' ')
                if (word1.eq.' ')then
		  OCase=1
		else if ((word1(1:1).ne.'"').and.(word2.eq.' '))then
		  OCase=2
		  COSs=word1
		else if ((word2(1:1).eq.'"').and.(word3.eq.' '))then
		  OCase=3
		  SepStr=word2
		  COSs=word1
		else if ((word2(1:1).eq.'"').and.(word3.ne.' '))then
		  OCase=4
		  SepStr=word2
		  COSs=word3
		else if ((word1(1:1).eq.'"').and.(word2.eq.' '))then
		  OCase=5
		  SepStr=word1
		else if ((word1(1:1).eq.'"').and.(word2.ne.' '))then
		  OCase=6
		  SepStr=word1
		  COSs=word2
		else
		  CALL Error2('SET-File: Keyword TABLES: ',
     &			'Options not in the correct order!',3)
		endif
	endif

	goto 10

100	CONTINUE


	RETURN

999     CALL ERROR2('This Keyword not found in COMIS.SET: ',keyw,3)

	END
