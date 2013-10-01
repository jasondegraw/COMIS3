C+*********************************************************** comv-ut4.f
C@tno jcp 1996Jul08_13:36:07 new subroutine to clear an array
Ch***********************************************************************
        SUBROUTINE ClearC(Array,Nelement)
C***********************************************************************
C       Clear Character Array(1:Nelement)
C@empa aw 2000mar23
Ch***********************************************************************

        IMPLICIT NONE
        INTEGER Nelement
        CHARACTER Array(1:Nelement)

C Local parameters

        INTEGER i
        DO 10 i=1,Nelement
          Array(i)=' '
10      continue

	RETURN
	END
Ch***********************************************************************
        SUBROUTINE ClearI(Array,Nelement)
C***********************************************************************
C       Clear INTEGER Array(1:Nelement)
C@tno jcp 1996Jul08
Ch***********************************************************************

        IMPLICIT NONE
        INTEGER Nelement
        INTEGER Array(1:Nelement)

C Local parameters

        INTEGER i
        DO 10 i=1,Nelement
          Array(i)=0
10      continue

	RETURN
	END

C@NBI PGS 2000Jul20 - This subroutine wasn't being used, so commented out
CCC@tno jcp 1996Jul08_13:36:07 new subroutine to clear an array
CCCh***********************************************************************
CC        SUBROUTINE ClearR(Array,Nelement)
CCC***********************************************************************
CCC       Clear REAL Array(1:Nelement)
CCC@tno jcp 1996Jul08
CCCh***********************************************************************
CC
CC        IMPLICIT NONE
CC        INTEGER Nelement
CC        REAL Array(1:Nelement)
CC
CCC Local parameters
CC
CC        INTEGER i
CC        DO 10 i=1,Nelement
CC          Array(i)=0.0
CC10      continue
CC
CC	RETURN
CC	END


C@NBI PGS 2000Jul20 - This subroutine wasn't being used, so commented out
CCCh***********************************************************************
CC	SUBROUTINE eat0(word)
CCC***********************************************************************
CCC				   input	 output
CCC eat0= eat zero's from names	  000000.0000  = 0
CCC				  001	       = 1
CCC				  12.30000     =12.3
CCC           These are strange--->   .          = 0  !!!
CCC           Would it give trouble?  ..         = .  !!!
CCC                                   ...0       = .. !!!
CCC hcp sept 1989
CCCh***********************************************************************
CC
CC        IMPLICIT NONE
CC	CHARACTER*(*) word
CC
CCC Local parameters
CC
CC        INTEGER p,k,l
CC	character dum*160
CC        INTEGER LenStr
CC
CC	l=lenstr(word)
CC
CC	dum=word
CC
CCC look for a decimal point here
CC	p=index(word,'.')
CC	IF (p.GT.0) THEN
CCC look at the end of the string for '0'
CC	  K=L+1
CC
CC10	  K=K-1
CC	  IF (K.GT.0) THEN
CC	    IF (word(k:k).eq.'0') goto 10
CC	  ENDIF
CC	  dum=word(1:K)
CCC eat the decimal point IF that is the last one
CC	  IF(dum(k:k).eq.'.') dum(k:k)=' '
CC
CC	ENDIF
CC
CCC now start at the begin of the string to eat '0'
CC	K=0
CC
CC20	K=K+1
CC	IF (k.LE.L) THEN
CC	  IF (dum(k:k).eq.'0') goto 20
CC	ENDIF
CC	word=dum(k:)
CC
CC	l=lenstr(word)
CC
CCC IF the whole string has been eaten we assume it contained '0000' or so
CCC and replace it with '0'
CC
CC	IF (l.eq.0) word='0'
CC
CC	RETURN
CC	END

Ch**********************************************************************
        INTEGER FUNCTION INTCON(ANUM,EFLAG)
C
C this routine has been taken from AIRNET written by George N. Walton NIST
C     DESCRIPTION  *****************************************************
C
C     Convert character string "ANUM" to an INTEGER.
C     The limiting values for short INTEGERs are +/- 32759;
C     for long INTEGERs +/- 2147483649.
C
C TEST changed into TESTC (Test is in COMMON)
C lines to write last 10lines CIF to CRT after error
C added  INCLUDE 'COMV-INP.inc'
C@empa aw 1993jun08 call inerr instead of error2 and w10lin
Ch***********************************************************************
        IMPLICIT NONE
C DECLARATIONS:
C
         INTEGER    EFLAG, I, J, MINUS
	 CHARACTER  ANUM*(*), TESTC*13
C@tno jcp 1996Jun26_14:56:33 xstr 80 char in Intcon
         CHARACTER  XSTR*160
	 LOGICAL    DIGSEEN
        INTEGER LENSTR
C
	INCLUDE 'comv-uni.inc'
	INCLUDE 'comv-inp.inc'
C
C
C     CODE  ************************************************************
C
	DATA TESTC / ' +-0123456789' /
	DIGSEEN = .FALSE.
	INTCON = 0
	MINUS = 1
	DO 40 J=1,LEN(ANUM)
	    I = INDEX(TESTC,ANUM(J:J))
	    IF(I.GT.3) GOTO 10
	    GOTO(50,30,20), I
	    GOTO 60
C   Digit seen here
C@tno jcp 1996May29_15:59:33 intcon stops at 3276, allow INTEGERs to 2140000000
CC10          IF(INTCON.GE.3276) GOTO 70
10          IF(INTCON.GE.2147438647) GOTO 70
	    DIGSEEN = .TRUE.
	    INTCON = INTCON*10+(I-4)
	    GOTO 40
20	    MINUS = -1
30	    IF(J.NE.1) GOTO 60
40	CONTINUE
50	CONTINUE
C complain if we haven't seen any digits
	IF (.NOT. DIGSEEN) GOTO 60
	INTCON = MINUS*INTCON
	EFLAG = 0
	GOTO 999
C			     Error messages.
60	CONTINUE
C@tno jcp 1996Jun26_14:48:48 variab in error message
        XSTR = Variab(1:lenstr(variab))//
     & ' Invalid INTEGER (INTCON): '//
     &   ANUM
	CALL INERR(XSTR,' ',.TRUE.,2)
	GO TO 80
70	CONTINUE
	XSTR = 'Too many digits (INTCON): '//ANUM
	CALL INERR(XSTR,' ',.TRUE.,2)

80	EFLAG = 1
999	RETURN
        END


Ch**********************************************************************

	SUBROUTINE INTDIS(I,ANUM,L)
C
C this routine has been taken from AIRNET written by George N. Walton NIST
C     DESCRIPTION  *****************************************************
C
C     Convert INTEGER I to character string ANUM(1:L) for display.
C
Ch***********************************************************************
C DECLARATIONS:
C
        IMPLICIT NONE
	INCLUDE 'comv-uni.inc'

         INTEGER  I, K, L, M, N, NN
	 CHARACTER*(*)	ANUM
	 CHARACTER*10	DISP
C
C     CODE  ************************************************************
C
	DATA DISP / '0123456789' /
	IF(I.GE.0) THEN
	  L = 0
	ELSE
	  L = 1
        ENDIF

	N = ABS(I)
	K = LEN(ANUM)
10	CONTINUE
	IF(N.LT.10) THEN
	  M = N
	  N = 0
	ELSE
	  NN = N/10
	  M = N-10*NN
	  N = NN
	ENDIF
	IF(K.LE.L) CALL ERROR('String Variable too short (INTDIS)',3)
	ANUM(K:K) = DISP(M+1:M+1)
	K = K-1
	IF(N.GT.0) GOTO 10
	N = LEN(ANUM)
	IF(L.EQ.1) ANUM(L:L) = '-'
20	CONTINUE
	L = L+1
	K = K+1
        ANUM(L:L) = ANUM(K:K)
C@tno jcp 1996May01_11:42:34 clear the used end of the string
        ANUM(K:K)=' '

	IF(K.LT.N) GOTO 20
C
	RETURN
        END


Ch**********************************************************************

	SUBROUTINE KeepLine(Line)
C
C***********************************************************************
C
C Description:
C
C	Keep the latest line read in
C
C Declarations
C	Input:
C	Line    - line to be kept
C
C Version : BVS 17.05.94
C
Ch***********************************************************************

        IMPLICIT NONE
	include 'comv-inp.inc'

	character*(*) Line

	Lines10(1) = Line
	Plines10=1
	return
	end

Ch**********************************************************************
        INTEGER FUNCTION LENSTR(A)
C pass parameter # =	1   2	 3  4	 5	6
C***********************************************************************
C
C NOTE: This is the LENSTR for Unix fortran compilers and the NDP FORTRAN
C	compiler for PCs.   The version for the Watcom compiler is in the
C	comv-win.f file.
C
C Purpose: LENSTR RETURNs the position of the last non blank character in a
C	   string (A), Bisection is used. To speed up the testing of blanks
C	   a string DUM is filled with 160 blanks. So in most cases one test
C	   shows whether the part of the string is empty. For very long strings
C	   a small loop uses DUM several times until we are at the end of the
C	   part to be tested.
C
C Module : #, TG, hcp/july 3,1989
C Changes: 1989 sep 10 included the possibility to detect TAB's at the end
C		       and then look for the true end of the non-white field
C@empa aw 1991sep16 Test if a =' '
C@empa aw 1992oct29 we have to set a new I1 first otherwise we loop endless
C@tno jcp 1996Apr04_10:26:46 LenStr seems usefull at first glance, this function
C fails to detect a string which is filled with binary zero's (NULL character)
C an approach similar to testing TAB's must be done which makes this routine
C very slow in case of a long string of binary zero's
C Limits :
C
C Pass parameters:
C
C IO # Name    unit		 description
C I  1 A			 string to be measured
C
C		 jl	jm     ju		 L
C		 |	|      |		 |
C string= "12345678901234567			  "
C finally		   |
C			   lenstr=jl
C example:
C I=LENSTR("123	 678") should RETURN I=8
Ch**********************************************************************
        IMPLICIT NONE
C     Determine the location of the last non-blank character in "A".
C
C     DECLARATIONS  ****************************************************
C
	INCLUDE 'comv-uni.inc'

	CHARACTER*(*)	A
        INTEGER empty,i1,i2,jl,ju,jm
C@tno jcp 1996Apr04_10:34:19 A1 single character added
        CHARACTER DUM*161, A1
C@tno jcp 1996Apr04_10:38:23 added IA =ICHAR(A1)
        INTEGER L,N,IA
C
C     CODE  ************************************************************
C

	IF ( a.EQ.' ') THEN
	    lenstr = 0
	    GOTO 30
	ENDIF

	DUM=' '

	jl=0
	ju=len(a)
	L=ju
C we have included the last non blank IF ju-jl<1, THEN the position is jl
10	IF (ju-jl.GT.1) THEN
	  jm=(ju+jl)/2
	  empty=1
	  I1=JM
C we cannot test more than 160 characters at a time
15	  I2=min(ju-I1,159)+i1
	  N=I2-i1+1

C blanks?
	  IF (a(i1:i2).eq.dum(1:N)) THEN

C are we at the end of the string part to be tested?
	    IF (I2.eq.ju) GOTO 20

C loop 15 to the end of the part to be tested
C but we have to set a new I1 first, otherwise we loop endless
          I1=I2
	  GOTO 15
	  ELSE
C the string contained characters, set empty to 0
	    empty=0
	  ENDIF
C bisection
20	  IF (empty.eq.1) THEN
	    ju=jm
	  ELSE
	    jl=jm
	  ENDIF
C loop 10 to see IF we included the last non blank
	  GOTO 10
	ENDIF
	lenstr=jl

C here is a trick. We never find if the whole string contains nonblanks
C so we test IF we RETURN the one before the last position and if so we
C test on the last position of the string

	IF (lenstr.eq.L-1) THEN
	  IF(A(l:l).NE.' ') lenstr=L
        ENDIF

C@tno jcp 1996Apr04_10:36:09 use short variable A1
C@empa aw 1999dec13 lenstr .gt.0
        IF (lenstr.gt.0) then
        A1=a(lenstr:lenstr)
        IA=ICHAR(a1)
        IF ((IA.eq.9).or.(IA.eq.0)) THEN
C we were unlucky and found a TAB al last position in string "a"
C so we go again to 10 ; and set the END, of the field to look in the string,
C to strlen-1
C@tno jcp 1996Apr04_10:30:35 what if a string full of TABs ? Loop here for them
100       lenstr=lenstr-1
	  if (lenstr.gt.0) then
            A1=a(lenstr:lenstr)
            IA=ICHAR(a1)
            IF ((IA.eq.9).or.(IA.eq.0) ) goto 100
          end if
	  ju=lenstr
	  L=lenstr
	  jl=0
C if strlen=0  we donot have to look no more
C if strlen>0 the REAL end of 'non-white' characters could be found
	  IF (ju.GT.0) goto 10
	ENDIF
C@empa aw 1999dec13 endif lenstr.gt.0
      endif


30	RETURN
	END
	
