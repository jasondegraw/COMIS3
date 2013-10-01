C+*********************************************************** comv-ut1.f
C@NBI PGS 2000Oct12 - detabbed & tidied up whole module; no syntax change
Ch**********************************************************************
      REAL FUNCTION RELCON(ANUM,EFLAG)
C
C DESCRIPTION
C Converts character string "ANUM" to a REAL number.
C
C This routine has been taken from AIRNET written by George N. Walton NIST
C
C Changes:
C TEST changed into TESTC (Test is in COMMON)
C lines to write last 10lines CIF to CRT after error
C@empa aw 1993jun08 call inerr instead of error2 and w10lin
Ch**********************************************************************

      IMPLICIT NONE
      INCLUDE 'comv-inp.inc'

      INTEGER EFLAG, I, J, MINUS, LENSTR
      REAL     MULT, RNUM
C     ! "decimal seen" and "exponent seen" flags
      LOGICAL  D, E
C     ! "numeric digit seen" flag
      LOGICAL    DIGSEEN
C     ! "numeric exponent digit seen" flag
      LOGICAL    EDIGSEEN

      CHARACTER  ANUM*(*), TESTC*16
C@tno jcp 1996Jun26_14:55:42 Xstr in Relcon 80
      character*160 xstr
C-----

      DATA TESTC / 'eE.-+ 0123456789' /
      DIGSEEN = .FALSE.
      EDIGSEEN = .FALSE.

      RELCON = 0.0
      MINUS = 1
      MULT = 1.
      RNUM = 0.0
      D = .FALSE.
      E = .FALSE.
C                       Process each character until a blank.
      DO 80 J=1,LEN(ANUM)
         I = INDEX(TESTC,ANUM(J:J))
         IF (I-6) 10,100,20
10       GO TO (70,70,30,40,60), I
         GO TO 120
C                         Add digit to number.
20       RNUM = RNUM*10+(I-7)
C        ! Digit seen here
         IF (D) THEN
C           ! after a decimal point
            DIGSEEN = .TRUE.
            MULT = MULT*10.
         ELSE IF (E) THEN
C           ! after a exponent
            EDIGSEEN = .TRUE.
         ELSE
C           ! before a decimal point or exponent
            DIGSEEN = .TRUE.
         ENDIF
         GO TO 80
C        ! Note decimal point.
30       IF (D .OR. E) GO TO 120
         D = .TRUE.
         GO TO 80
C        ! Note minus sign.
40       IF (J.EQ.1) GO TO 50
         IF (ANUM(J-1:J-1).EQ.'E') GO TO 50
         IF (ANUM(J-1:J-1).EQ.'e') GO TO 50
         GO TO 120
50       MINUS = -1
         GO TO 80
C        ! Note plus sign.
60       IF (J.EQ.1) GO TO 80
         IF (ANUM(J-1:J-1).EQ.'E') GO TO 80
         IF (ANUM(J-1:J-1).EQ.'e') GO TO 80
         GO TO 120
C        ! Begin exponent.
70       IF (I.EQ.0) GO TO 120
C        ! If we are already in the exponent then complain
         IF (E) GO TO 120
         RELCON = REAL(RNUM*MINUS)/MULT
         MINUS = 1
         RNUM = 0.0
         D = .FALSE.
         E = .TRUE.
80    CONTINUE
C     ! Finish calculation of number.
100   EFLAG = 0
C     ! complain if we haven't seen any digits
      IF (.NOT. DIGSEEN) GOTO 120
      IF (E) GO TO 110
      RELCON = REAL(RNUM*MINUS)/MULT
      GO TO 999
110   CONTINUE
C     ! If there was no value following the exponent (e or E) then complain
      IF (.NOT. EDIGSEEN) GOTO 120
      RELCON = RELCON*10.**(RNUM*MINUS)
      GO TO 999

C     ! Error messages.
120   CONTINUE
C@tno jcp 1996Jun26_14:48:48 variab in error message
      xstr = Variab(1:lenstr(variab))//
     &   ' Invalid number (RELCON): '//ANUM
      CALL INERR(xstr,' ',.TRUE.,2)
      EFLAG = 1
999   RETURN
      END


Ch**********************************************************************
      SUBROUTINE RELDIS(R,M,ANUM,L,Eform)
C***********************************************************************
C
C DESCRIPTION
C Converts REAL R to character string ANUM(1:L) for display.
C
C This routine has been taken from AIRNET written by George N. Walton NIST
C
C DECLARATIONS
C Input:
C     R     - REAL (single precision) number.
C     M     - number of significant digits in result.
C@lbl rw 1992may07 added passed argument
C     Eform - flag for the format: 1-> number in exponential form
C Output:
C     ANUM  - character string with number beginning at 1.
C     L     - last position of number in ANUM.      [ANUM(1:L)]
C Local:
C     LFC   - position of first character in ANUM.
C     LDEC  - position of decimal point in ANUM.
C     LEXP  - position of "E" in ANUM.
C     LSIG  - position of last significant digit in ANUM.
C     EXPT  - value of the exponent in ANUM.
C     DECML - .TRUE. IF decimal point has been written.
C
C Changes:
C@lbl bvs 1993Aug20 infinite loop beyond array bounds at label 12
C      changed loop to call index()
Ch**********************************************************************

      IMPLICIT NONE
      INTEGER M,Eform, L, LFC, LDEC, LEXP, LSIG, EXPT, I, EFLAG, INTCON
      LOGICAL DECML
      REAL R
      CHARACTER*(*) ANUM
      CHARACTER*30 FORM
C-----

      IF (LEN(ANUM).LT.30) CALL ERROR('Short string (RELDIS)',3)
      I = M-1
      CALL INTDIS(I,ANUM,L)
      FORM = '(1PE30.'//ANUM(1:L)//')'
      WRITE(ANUM,FMT=FORM) R

      i = index (anum, '.')
      if (anum(i-1:i-1) .eq. ' ') anum(i-1:i-1) = '0'

      I = 0
10    I = I+1
      IF (ANUM(I:I).EQ.' ') GO TO 10
      LFC = I
C     ! Prevent long (possibly infinite loop).  Just use index
C     ! to find the decimal point instead of looping.  For NaN (Not a Number) values
C     ! there is no decimal point, so assume that if there is no "." it must be NaN?
      I = INDEX(ANUM,'.')
      IF (I .EQ. 0) then
         ANUM='NaN'
         L=3
         GOTO 999
      ENDIF
      LDEC = I
14    I = I+1
      IF (ANUM(I:I).EQ.'E') GO TO 15
      IF (ANUM(I:I).EQ.'e') GO TO 15
      GO TO 14
15    LEXP = I
16    I = I-1
      IF (ANUM(I:I).EQ.'0') GO TO 16
      LSIG = I
      EXPT = INTCON(ANUM(LEXP+1:30),EFLAG)
      IF (EXPT.LT.-4) GO TO 30
      IF (EXPT.GE.M+3) GO TO 30
      IF (EForm.EQ.1 ) GOTO 30

C------------------------------------
C     Display number in normal format
C------------------------------------

      DECML = .FALSE.
      L = 0
      IF (ANUM(LFC:LFC).EQ.'-') THEN
         L = L+1
         ANUM(L:L) = '-'
      ENDIF
      IF (EXPT.LT.0) THEN
         L = L+1
         ANUM(L:L) = '0'
         L = L+1
         ANUM(L:L) = '.'
         DECML = .TRUE.
      ENDIF
20    CONTINUE
      EXPT = EXPT+1
      IF (EXPT.GE.0) GO TO 22
      L = L+1
      ANUM(L:L) = '0'
      GO TO 20
22    CONTINUE
      L = L+1
      ANUM(L:L) = ANUM(LDEC-1:LDEC-1)
      I = LDEC+1
24    CONTINUE
      EXPT = EXPT-1
      IF (EXPT.EQ.0) THEN
        L = L+1
        ANUM(L:L) = '.'
        DECML = .TRUE.
      ENDIF
      IF (I.GT.LSIG) THEN
        IF (DECML) GO TO 999
        L = L+1
        ANUM(L:L) = '0'
      ELSE
        L = L+1
        ANUM(L:L) = ANUM(I:I)
        I = I+1
      ENDIF
      GO TO 24

C-----------------------------------------
C     Display number in exponential format
C-----------------------------------------

30    CONTINUE
      L = 0
      DO I=LFC,LSIG
         L = L+1
         ANUM(L:L) = ANUM(I:I)
      ENDDO
      DO I=LEXP,30
         L = L+1
         ANUM(L:L) = ANUM(I:I)
      ENDDO
999   RETURN
      END


Ch**********************************************************************
      INTEGER FUNCTION IEMPTY(STRING,K)
C***********************************************************************
C Purpose: IEMPTY RETURNs 1 IF a string contains only blanks after the
C         K'th position.
C
C Module : #, TG, hcp/february 15,1989
C Changes: July 12 1989 changed into a CALL of lenstr
C Limits :
C
C Pass parameters:
C IO # Name    unit  description
C I  1 STRING        STRING of which we want to know IF it is empty
C I  2 K             position in the string from where we want to look
C
C ERROR RETURN IF 0.5 < N > 1.0
Ch**********************************************************************

      IMPLICIT NONE
      INTEGER K, L
      CHARACTER*(*) STRING
      INTEGER LENSTR
C-----

      L= LENSTR(STRING(K:))
      IF (L.eq.0) THEN
         iempty=1
      ELSE
         iempty=0
      ENDIF
      RETURN
      END


Ch**********************************************************************
      SUBROUTINE FAlpha(string,L,begin,End,found)
C***********************************************************************
C FAlpha= Find Alpha
C FAlpha goes through a string and looks for the first substring containing
C only characters a...z IF found, the INTEGER found is set to 1.
C begin and end point to the begin position and the end postion in the string
C only the first substring is found
C L=the length of the string
C sept 1989 hcp
Ch**********************************************************************

      IMPLICIT NONE
C     ! Passed arguments
      CHARACTER*(*) string
      INTEGER L,begin,End,found
C     ! Internal variables
      INTEGER k
      CHARACTER T*160
      INTEGER NrChar
C-----

      T=string
C     ! Loop through the string to find the begin and end of substrings
C     ! containing characters from a...z only.
      K=0
      FOUND=0
      END=0
      begin=0
10    IF (k .LT. L) THEN
         K=K+1
         NrChar=ICHAR(T(k:k))
         IF (NrChar.GE.97 .AND. NrChar.LE.122 ) THEN
            IF (begin.EQ.0) begin=K
         ELSE
            IF (begin.GT.0) THEN
               END=K-1
C              ! a begin and END has been found so set found to 1
               FOUND=1
C              ! K=L makes the loop exit
               K=L+1
            ENDIF
         ENDIF

C        ! This IF statement fixes the bug that no END would be found IF
C        ! the word is up to the end of the string
         IF (begin.GT.0 .AND. K.eq.L) THEN
            END=L
C           ! A begin has been found ,the END is here the end of the string. Set found to 1
            FOUND=1
C           ! K=L makes the loop exit
            K=L+1
         ENDIF
C        ! ENDIF begin>0 and K<L

C        ! Repeat the loop
         GOTO 10
      ENDIF
C     ! ENDIF K<L
      RETURN
      END


Ch**********************************************************************
      SUBROUTINE UPPERC(word)
C***********************************************************************
C UPPERC= UPPER Case characters
C The subroutine UPPERC turns all characters in an input string to upper
C case.
C characters 97 upto 122 are a...z
C           65            90 are A...Z
Ch**********************************************************************

      IMPLICIT NONE
      CHARACTER*(*) word
      INTEGER I,L,Nr
      INTEGER LENSTR
C-----

      L=lenstr(word)
      DO I=1,L
         NR=ICHAR(word(i:i))
         IF (NR .GE. 97 .AND. NR .LE. 122) THEN
            NR=NR-32
            word(i:i)=CHAR(NR)
         ENDIF
      ENDDO
      RETURN
      END


Ch**********************************************************************
      SUBROUTINE LOWERC(word)
C***********************************************************************
C LOWERC=LOWER Case characters
C The function LOWERC turns all characters in an input string to lower case ,
C and RETURNs that string in out
C characters 97 upto 122 are a...z
C           65            90 are A...Z
Ch**********************************************************************

      IMPLICIT NONE
      CHARACTER*(*) word
      INTEGER I,L,Nr
      INTEGER LENSTR
C-----

      L=lenstr(word)
      DO I=1,L
         NR=ICHAR(word(i:i))
         IF (NR .GE. 65 .AND. NR .LE. 90 ) THEN
            NR=NR+32
            word(i:i)=CHAR(NR)
         ENDIF
      ENDDO
      RETURN
      END


Ch**********************************************************************
C@NBI PGS 1999May06 - See "changes" below
CC    SUBROUTINE W10LIN
      SUBROUTINE W10LIN(File)
C***********************************************************************
C Writes 10 lines from the string Lines10(10)*160 to CRT. Plines10 is
C the current line (last one to print here).  Lines10 is filled
C cyclically from CIF when reading a line in.  Clines10 is the column
C where after the error occurs.  Variab is the string that contains the
C name of the Variable we tried to read
C
C Pass parameters:
C IO   Name    Unit             Description
C I    File    -         Output file unit
C
C Changes:
C@empa vd 1993may12  Input Data Error Flag FInpErr set to one when a call
C@                   to this routine is made
C@                   Output stream * changed to CER
C@empa aw 1993jun08 Checking errorflag canceled. Because the only call to
C@                  W10lin is in INERR and that routine does already the flag
C@                  checking.
C@empa aw 1993jun08 The first two outputlines edited
C@empa aw 1993jun15 insert tabs in dum if there are any in last line
C@tno jcp 1996Jul09_15:57:46 part changed to clarify the error message
C@NBI PGS 1999May06 - Tidied up whole subroutine together with INERR.
C@NBI               - Added argument 'File'
C@NBI               - The 'Variable' statement is now written directly
C@NBI                 below the pointer, if the pointer is not too far right
C@NBI PGS 2000Jul16 - Further changes to improve clarity
Ch**********************************************************************

      IMPLICIT NONE
      INCLUDE 'comv-uni.inc'
      INCLUDE 'comv-inp.inc'
      CHARACTER dum*160
      INTEGER i,ii,LENSTR,File,AproxPos
C-----
      if (keep10) then
         DUM='Here are the previous 10 lines from '//
     &   Cifs(1:lenstr(Cifs))//'. The error is at or before "|":'
         call wrt80(File,Dum(1:lenstr(DUM)),wCRT)
         WRITE(File,1000)
      else
C@empa aw 2000apr05 Message changed
CC        write(File,*)'Line in error:'
CC        write(File,*)'Error is in line:'
         write(File,*)'The erroneous line is:'
         write(File,'(A79)') lines10(1)
         return
      endif

      DO i=1,10
         ii=mod(Plines10-1+i,10)+1
         write (File,'(A79)') lines10(ii)
      ENDDO

C     ! Insert tabs in dum if there are any in last line
      AproxPos=0
      DO i=1,Clines10
         IF (lines10(ii)(i:i).EQ.CHAR(9))THEN
            dum(i:i)=CHAR(9)
            AproxPos=AproxPos+8
         ELSE
            dum(i:i)=' '
            AproxPos=AproxPos+1
         ENDIF
      ENDDO
      write (File,'(A)') dum(1:Clines10)//'|'
      IF(AproxPos+11+Lenstr(Variab).GT.80)THEN
         write (File,*) 'Variable =',Variab
      ELSE
         write (File,'(A)') dum(1:Clines10)//'Variable = '//Variab
      ENDIF
      WRITE(File,1000)
1000  FORMAT(32(' -'))

      RETURN
      END

