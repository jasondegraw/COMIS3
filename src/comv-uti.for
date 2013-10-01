C+*********************************************************** comv-uti.f
Ch**********************************************************************
        INTEGER FUNCTION istrstr(string,pattrn,Start)
C     pass parameter # =               1      2     3
C***********************************************************************
C Purpose: istrstr looks for Pattrn in String from startposition Start
C	   in String. The value RETURNed is 0 IF the Pattrn is not found
C	   and it is the position in String where Pattrn starts IF found.
C
C	   NOTE: index(line,pattrn) will fail!!! pattern maybe dimensioned
C		 as 20 characters and you want to look for 'hello' in the
C		 line. pattrn contains 'hello		     ' . So
C		 with 15 blanks behind 'hello' . IF the line only contains
C		 the word 'hello' , THEN it probably has enough blanks after
C		 'hello' so the pattern matches. And you think it works.
C		 But it fails to find pattrn IF anything is behind 'hello'
C		 in line.
C		 So before index(line,pattrn) you have to determine the length
C		 (non blanks) of pattrn. You can DO this with L=lenstr(pattrn).
C		 THEN it might be easier to use this routine istrstr.
C
C		 Start is set to 1 if start <1 when calling the routine
C		 (VD 14.2.91)
C
C Module : #, TG, hcp/ feb 10 1989
C Changes: July 12 1989 replaced the search part with the FTN function INDEX
C	   start=1 if start<1 at the beginning
C tab changed into check on ICHAR(A).eq.9
C Limits :
C
C Pass parameters:
C
C IO # Name    unit		 description
C I  1 String  -	    long string which may contain (more) Pattrn's
C I  2 Pattrn  -	    Pattrn to look for in String
C I  3 Start   -	    Start position to look for Pattrn, so if you
C			    found the pattern on position 1 you can set
C			    Start to 2 to look for a next match.
C
C ERROR RETURN IF 0.5 < N > 1.0
C example:
C call
Ch**********************************************************************
        IMPLICIT NONE
        INTEGER Start,L
	CHARACTER*(*) string,pattrn
        INTEGER LENSTR

	l=lenstr(pattrn)
	istrstr=0
	IF (start.LT.1) start=1
	istrstr=index(string(start:),pattrn(1:L))

C IF you found the pattrn, THEN add the Start position to it, to give the
C REAL position of pattrn in the string .

	IF (istrstr.GT.0) THEN
	  istrstr=istrstr+start-1
	ENDIF
	RETURN
	END

Ch**********************************************************************
        INTEGER FUNCTION iCstrstr(string,pattrn,Start)
C pass parameter # =		   1	  2	3
C***********************************************************************
C Purpose: istrstr looks for Pattrn in String from startposition Start
C          in String, Case Insensitive.
C          The value RETURNed is 0 IF the Pattrn is not found
C	   and it is the position in String where Pattrn starts IF found.
C
C	   NOTE: index(line,pattrn) will fail!!! pattern maybe dimensioned
C		 as 20 characters and you want to look for 'hello' in the
C		 line. pattrn contains 'hello		     ' . So
C		 with 15 blanks behind 'hello' . IF the line only contains
C		 the word 'hello' , THEN it probably has enough blanks after
C		 'hello' so the pattern matches. And you think it works.
C		 But it fails to find pattrn IF anything is behind 'hello'
C		 in line.
C		 So before index(line,pattrn) you have to determine the length
C		 (non blanks) of pattrn. You can DO this with L=lenstr(pattrn).
C		 THEN it might be easier to use this routine istrstr.
C
C		 Start is set to 1 if start <1 when calling the routine
C		 (VD 14.2.91)
C
C Module : #, TG, hcp/ feb 10 1989
C Changes: July 12 1989 replaced the search part with the FTN function INDEX
C	   start=1 if start<1 at the beginning
C tab changed into check on ICHAR(A).eq.9
C Limits :
C
C Pass parameters:
C
C IO # Name    unit		 description
C I  1 String  -	    long string which may contain (more) Pattrn's
C I  2 Pattrn  -	    Pattrn to look for in String
C I  3 Start   -	    Start position to look for Pattrn, so if you
C			    found the pattern on position 1 you can set
C			    Start to 2 to look for a next match.
C
C ERROR RETURN IF 0.5 < N > 1.0
C example:
C call
Ch**********************************************************************
        IMPLICIT NONE
        INTEGER Start,L
	CHARACTER*(*) string,pattrn
        INTEGER LenStr
        INTEGER IcIndex

	l=lenstr(pattrn)
        icstrstr=0
	IF (start.LT.1) start=1
        icstrstr=ICindex(string(start:),pattrn(1:L))

C IF you found the pattrn, THEN add the Start position to it, to give the
C REAL position of pattrn in the string .

        IF (icstrstr.GT.0) THEN
          icstrstr=icstrstr+start-1
	ENDIF
	RETURN
	END

Ch**********************************************************************

      SUBROUTINE ERROR(MSG,SEVR)

C***********************************************************************
C
C this routine has been taken from AIRNET written by George N. Walton NIST
C
C Description:
C     Simple error message WRITEr with fatal error termination.
C
C Declarations
C     Input:
C     MSG   - message to be printed.
C     SEVR  - error severity code
C             0 = NOTE
C             1 = WARNING
C             2 = SEVERE
C             3 = FATAL (program stops)
C
C Version : PGS 1999May06
C@tno jcp 1996Apr05_16:27:14 changed errormessages to be able to find them in CO
C@tno jcp 1996Apr05_16:28:19 increased the length of the error message
C@tno jcp 1996Apr05_16:29:03 count the errors
C@empa aw 1996may26 IF CRT is 0, no output to CRT
C@empa aw 1996jun15 output a final line
C@NBI PGS 1999May06 - Tidied up whole subroutine.  Created a loop
C@NBI                 that directs output to CER & CRT in turn
C@NBI               - The line is now above message, not below
C@NBI PGS 1999Aug03 - Deletes DAF file when program stops due to fatal error
C@empa aw 2001mar23 Message slightly changed; in COMIS TRNSYS use 
C@empa              message can now be better identified as COMIS message  
C@empa aw 2001sep05 For COMIS in TRNSYS I need TIME and INFO in error routines 
C@empa              but I don't want to pass them in each call 
C@empa              --> new common block /TIMINFO/
Ch**********************************************************************
        IMPLICIT NONE
        INCLUDE 'comv-uni.inc'
        INCLUDE 'comv-inp.inc'
        INTEGER  SEVR,I,File
C@empa aw 2001mar23
        INTEGER STP        
        CHARACTER*(*) MSG
        CHARACTER*35  SEVER(0:3)
C@empa aw 2001sep05 For COMIS in TRNSYS I need TIME and INFO in error routines 
C@empa              but I don't want to pass them in each call 
C@empa              --> new common block /TIMINFO/
        DIMENSION INFOS(15)
	  INTEGER INFOS
        REAL TIMES

        COMMON /TIMINFO/ TIMES, INFOS

C@empa aw 2001mar23 ***CER** changed to COMIS MESSAGE
        DATA SEVER / 'COMIS MESSAGE  ***** NOTE *****   ',
     &               'COMIS MESSAGE  ***** WARNING *****',
     &               'COMIS MESSAGE  ***** ERROR *****  ',
     &               'COMIS MESSAGE  ***** ERROR *****  ' /

C--------------------------------------
C       LOOP TWICE: First CRT, then CER
C--------------------------------------

C@empa aw 2001mar23 Bugfix for case CER = CRT (with Compaq VF we get a division
C@empa              by zero error in the next line)
CC        DO File=CRT,CER,CER-CRT
        STP=CER-CRT
        IF(STP.EQ.0) STP=1
        DO File=CRT,CER,STP
          IF(File.GT.0) THEN
             WRITE(File,*)'------------------------------------------'
     &       //'----------------------'
             I=MAX(0,MIN(3,SEVR))
C@empa aw 2001sep05 Message only for COMIS TRNSYS
CTC
             IF ((INFOS(1).NE.0).and.(INFOS(2).NE.0))THEN
               WRITE(FILE,2005) INFOS(1),INFOS(2),TIMES
2005           FORMAT(//1X,'***** WARNING ***** UNIT',I3,' TYPE',I3,
     .         ', TIME = ',1PE11.3)
	     ENDIF
             WRITE(File,*) SEVER(I)
             CALL wrt80(File,MSG,WCrt)
C@NBI PGS 2000dec23 - Next line prevents infinite loop when CER = CRT
C@empa aw 2001mar23 See Bugfix above for this case
CC           IF(CER.EQ.CRT) GOTO 11
          ENDIF 
        ENDDO
CC11      CONTINUE

C-------------------
C       Post process
C-------------------

        CERCount(Sevr)=CERCount(Sevr)+1
        IF(SEVR.GE.3) THEN
          CLOSE(DAF,STATUS='DELETE',ERR=9000)
9000      STOP
        ENDIF
        RETURN
      END


Ch**********************************************************************
      SUBROUTINE wrt80(WrtUnit,Message,Width)
C                         I       I      I
C                         i       c      i
C                         |       |      screen/output width
C                         |       string to output
C                         unit number of the output file (if=0 to CRT, *)
C***********************************************************************
C wrt80 writes a string Message cut at a series of 'width' wide lines
C if a ';' is found the line is wrapped there.
C if the length of the part of Message currently processed reaches 'Width'
C the part until the last space is printed. If no space had been found, the
C message is just cut at 'Width'.
C If the WrtUnit=0 then the output is: write(*,*)
C A tab  (char(9)) or Null (char(0)) is treated as space
C Changes:
C@NBI PGS 2000Oct14 - Tidied up whole subroutine; removed all GOTOs
C@NBI               - Trailing spaces removed, and now we account for
C@NBI                 the fact that WRITE(*,*) has an extrta space prefix.
C@NBI               - Added internal INTEGER "FileUnit" -> shorten code
Ch**********************************************************************
        IMPLICIT NONE
C       ! Passed arguments
        INTEGER WrtUnit,Width
        CHARACTER*(*) Message
C       ! Internal variables
        INTEGER K,L,Len,LastSpace,Kbegin,Lenstr,FileUnit
        CHARACTER A*1
C-----

C       ! File unit = 6 (screen) if WrtUnit = 0
        FileUnit=WrtUnit
        IF(FileUnit.EQ.0) FileUnit=6

C       ! Reset the position of the last space found
        LastSpace=0

C       ! Width should never be 0 or negative. Reset to 80
        if (width.le.0)  width=80
        L=Lenstr(Message)

        Kbegin=1
        len   =0
        K     =0
C-----

        if (L.ne.0) then
C         ! Loop here through the Message
          DO WHILE(k.lt.l)
            K=K+1
            len=len+1
            A=Message(K:K)
            if (A.eq.' '.or.ICHAR(A).eq.9.or.ICHAR(A).EQ.0) LastSpace=K

            if (A.eq.';') then
C             ! Wrap at ';'
              write(FileUnit,*) Message(Kbegin:K-1)
              len=0
              Kbegin=k+1
C@NBI PGS 2000Oct14 - "Len" changed to "Len+1" to account for space in left column
            ELSEIF (len+1.ge.width) then
C             ! Reached width, wrap at last space between words
              if (lastspace.gt.kbegin) then
C               ! Print up to, including, the last space
C@NBI PGS 2000Oct14 - Trailing spaces removed
CC              write(*,*) Message(kbegin:lastSpace)
                write(FileUnit,*) Message(kbegin:lastSpace-1)
                len=k-lastspace
                Kbegin=LastSpace+1
              else
C               ! no space found: just cut at Width
                write(FileUnit,*) Message(kbegin:k)
                len=0
                Kbegin=k+1
              end if
            end if

          ENDDO
        endif
C-----
C       ! write the remainder of the Message
        if (kbegin.lt.k) write(FileUnit,*) Message(kbegin:k)
        RETURN
      END


Ch**********************************************************************

      SUBROUTINE ERROR2(MSG1,MSG2,SEVR)

C***********************************************************************
C
C this routine has been taken from AIRNET written by George N. Walton NIST
C
C Description:
C     Simple error message WRITE (2 messages) with fatal termination.
C
C Declarations:
C   Input:
C     MSG1  - first message to be printed.
C     MSG2  - second message to be printed.
C     SEVR  - error severity code:
C             0 = NOTE
C             1 = WARNING
C             2 = SEVERE
C             3 = FATAL (program stops)
C
C Version : PGS 1999May06
C@empa aw 1994may26 IF CRT is 0, no output to CRT
C@empa aw 1994jun15 output a final line
C@empa aw 1995mar30 write MSG1 and MSG2 into two lines
C@tno jcp 1996Apr05_16:27:14 changed errormessages to be able to find them in CO
C@tno jcp 1996Apr05_16:28:19 increased the length of the error message
C@tno jcp 1996Apr05_16:29:03 count the errors
C@NBI PGS 1999May06 - Tidied up whole subroutine.  Created a loop
C@NBI                 that directs output to CER & CRT in turn
C@NBI               - The line is now above message, not below
C@NBI PGS 1999Aug03 - Deletes DAF file when program stops due to fatal error
C@empa aw 2001mar23 Message slightly changed; in COMIS TRNSYS use 
C@empa              message can now be better identified as COMIS message  
C@empa aw 2001sep05 For COMIS in TRNSYS I need TIME and INFO in error routines 
C@empa              but I don't want to pass them in each call 
C@empa              --> new common block /TIMINFO/
Ch**********************************************************************
        IMPLICIT NONE
        INCLUDE 'comv-uni.inc'
        INCLUDE 'comv-inp.inc'
        INTEGER  SEVR,I,File
C@empa aw 2001mar23
        INTEGER STP        
        CHARACTER*(*) MSG1, MSG2
        CHARACTER*35  SEVER(0:3)
C@empa aw 2001sep05 For COMIS in TRNSYS I need TIME and INFO in error routines 
C@empa              but I don't want to pass them in each call 
C@empa              --> new common block /TIMINFO/
        DIMENSION INFOS(15)
	  INTEGER INFOS
        REAL TIMES

        COMMON /TIMINFO/ TIMES, INFOS

C@empa aw 2001mar23 ***CER** changed to COMIS MESSAGE
        DATA SEVER / 'COMIS MESSAGE  ***** NOTE *****   ',
     &               'COMIS MESSAGE  ***** WARNING *****',
     &               'COMIS MESSAGE  ***** ERROR *****  ',
     &               'COMIS MESSAGE  ***** ERROR *****  ' /

C--------------------------------------
C       LOOP TWICE: First CRT, then CER
C--------------------------------------

C@empa aw 2001mar23 Bugfix for case CER = CRT (with Compaq VF we get a division
C@empa              by zero error in the next line)
CC        DO File=CRT,CER,CER-CRT
        STP=CER-CRT
        IF(STP.EQ.0) STP=1
        DO File=CRT,CER,STP
          IF(File.GT.0) THEN
            WRITE(File,*)'------------------------------------------'
     &      //'----------------------'
            I=MAX(0,MIN(3,SEVR))
C@empa aw 2001sep05 Message only for COMIS TRNSYS
CTC
             IF ((INFOS(1).NE.0).and.(INFOS(2).NE.0))THEN
              WRITE(FILE,2005) INFOS(1),INFOS(2),TIMES
2005          FORMAT(//1X,'***** WARNING ***** UNIT',I3,' TYPE',I3,
     .        ', TIME = ',1PE11.3)
	    ENDIF
            WRITE(File,*) SEVER(I)
            call wrt80(File,MSG1,WCrt)
            call wrt80(File,MSG2,WCrt)
C@NBI PGS 2000dec23 - Next line prevents infinite loop when CER = CRT
C@empa aw 2001mar23 See Bugfix above for this case
CC          IF(CER.EQ.CRT) GOTO 11
          ENDIF
        ENDDO
CC11      CONTINUE

C-------------------
C       Post process
C-------------------

        CERCount(Sevr)=CERCount(Sevr)+1
        IF(SEVR.GE.3) THEN
          CLOSE(DAF,STATUS='DELETE',ERR=9000)
9000      STOP
        ENDIF
        RETURN
      END

Ch**********************************************************************

      SUBROUTINE LastERRor2(MSG1,MSG2,SEVR)
C
C***********************************************************************
C
C Copied from routine error2 to be used for the last of a series of error
C messages belonging to one group
C
C@NBI PGS 1999May06 - Tidied up whole subroutine.  Created a loop
C@NBI                 that directs output to CER & CRT in turn
C@NBI PGS 1999Aug03 - Deletes DAF file when program stops due to fatal error
C@empa aw 2001mar23 Message slightly changed; in COMIS TRNSYS use 
C@empa              message can now be better identified as COMIS message  
C@empa aw 2001sep05 For COMIS in TRNSYS I need TIME and INFO in error routines 
C@empa              but I don't want to pass them in each call 
C@empa              --> new common block /TIMINFO/

Ch**********************************************************************
        IMPLICIT NONE
        INCLUDE 'comv-uni.inc'
        INCLUDE 'comv-inp.inc'
        INTEGER  SEVR,I,File
C@empa aw 2001mar23
        INTEGER STP        
        CHARACTER*(*) MSG1, MSG2
        CHARACTER*35  SEVER(0:3)
C@empa aw 2001sep05 For COMIS in TRNSYS I need TIME and INFO in error routines 
C@empa              but I don't want to pass them in each call 
C@empa              --> new common block /TIMINFO/
        DIMENSION INFOS(15)
	  INTEGER INFOS
        REAL TIMES

        COMMON /TIMINFO/ TIMES, INFOS

        DATA SEVER / 'COMIS MESSAGE  ***** NOTE *****   ',
     &               'COMIS MESSAGE  ***** WARNING *****',
     &               'COMIS MESSAGE  ***** ERROR *****  ',
     &               'COMIS MESSAGE  ***** ERROR *****  ' /

C--------------------------------------
C       LOOP TWICE: First CRT, then CER
C--------------------------------------

C@empa aw 2001mar23 Bugfix for case CER = CRT (with Compaq VF we get a division
C@empa              by zero error in the next line)
CC        DO File=CRT,CER,CER-CRT
        STP=CER-CRT
        IF(STP.EQ.0) STP=1
        DO File=CRT,CER,STP
          IF(File.GT.0) THEN
            I=MAX(0,MIN(3,SEVR))
C@empa aw 2001sep05 Message only for COMIS TRNSYS
CTC
             IF ((INFOS(1).NE.0).and.(INFOS(2).NE.0))THEN
              WRITE(FILE,2005) INFOS(1),INFOS(2),TIMES
2005          FORMAT(//1X,'***** WARNING ***** UNIT',I3,' TYPE',I3,
     .       ', TIME = ',1PE11.3,4X)
	    ENDIF
            WRITE(File,*) SEVER(I)
            call wrt80(File,MSG1,WCrt)
            call wrt80(File,MSG2,WCrt)
            WRITE(File,*)'------------------------------------------'
     &    //'----------------------'
C@NBI PGS 2000dec23 - Next line prevents infinite loop when CER = CRT
C@empa aw 2001mar23 See Bugfix above for this case
CC          IF(CER.EQ.CRT) GOTO 11
          ENDIF
        ENDDO
CC11      CONTINUE

        IF(SEVR.GE.3) THEN
          CLOSE(DAF,STATUS='DELETE',ERR=9000)
9000      STOP
        ENDIF
        RETURN
      END


Ch**********************************************************************

        SUBROUTINE ContERRor2(MSG1,MSG2)
C
C***********************************************************************
C
C Copied from routine error2 to be used for continuation lines of the error
C message.
C Does not count errors and does not stop if Sevr=3
C
Ch**********************************************************************
        IMPLICIT NONE
	INCLUDE 'comv-uni.inc'
	INCLUDE 'comv-inp.inc'

	 CHARACTER*(*) MSG1, MSG2
C
C     CODE  ************************************************************
C
C IF CRT is 0, no output to CRT
	IF (CRT.NE.0) THEN
          call wrt80(CRT,MSG1,WCrt)
          call wrt80(CRT,MSG2,WCrt)
        ENDIF
        call wrt80(CER,MSG1,WCrt)
        call wrt80(CER,MSG2,WCrt)

	RETURN
	END

Ch**********************************************************************

	SUBROUTINE GETWRD(STRING,K,LS,WORD)

C this routine has been taken from AIRNET written by George N. Walton NIST
C     DESCRIPTION  *****************************************************
C
C     Get next "WORD" from the "STRING" of characters.
C     Words are separated by blanks or commas:	 WORD,WORD,WORD	 or
C	 WORD WORD WORD	 or  WORD, WORD, WORD  are all valid.
C changes: july 4 1989. The pointer K is now , after having found the word
C			moved further until the position before the next non
C			blank after the word. This allows also for data like:
C	 WORD , WORD	  ,	 WORD etc
C			To limit the amount of search for the last nonblank,
C			the actual length of the string (last nonblank) is
C			passed as LS. So before you CALL GETWRD	 first do
C			LS=LENSTR(STRING). As you can get many words from
C			one string, it was better not to determine LS every
C			time in GETWRD.
C			To allow the last word to be upto the last element of
C			the string, no error is reported IF no blank or colon
C			is found after the last word, while the pointer K goes
C			over the length of the string.
C
C  at the call		     K			       LS
C			     |			       |
C  string="  TAB  word1 TAB , word2 , TAB word3		"
C  finally				 |
C					 K
C  word=		     "word2	     "
C					    |
C					    LW
C@empa aw 1994mar10 Added the possibility to have words between quotation
C                 marks including spaces or tabs (example "x  y").
C                 GETWRD gives the WORD from one quotation mark to the other
C                 including them selve. This was needed to read separator
C                 strings from SET-file (keyword TABLES) for COS-files
C                 which could be spaces or tabs.
C                 Example:
C  at the call                     K                         LS
C                          |                         |
C  string='  TAB  word1 TAB , "x yTABy" , TAB word3           '
C  finally                             |
C                                      K
C  word=                   '"x yTABz"      '
C                                         |
C                                         LW
C   DECLARATIONS  ****************************************************
C
C   Input/Output:
C     K	      - current position in "STRING".
C     LS      - max length to search for word in string
C     WORD    - word extracted from string
C
C   Local:
C     L	      - current position in "WORD".
C     LW      - maximum length of "WORD".
Ch**********************************************************************
        IMPLICIT NONE
	INCLUDE 'comv-uni.inc'
C@tno jcp 1996Jun17_15:42:46 include inp to be able to use variab
        include 'comv-inp.inc'

	LOGICAL QUOTE
C@tno jcp 1996Jun26_13:40:16 Kbegin stored for error message
        INTEGER  K, LS, L, LW, Kbegin
	CHARACTER*(*) WORD, STRING
	CHARACTER*1  A
        INTEGER LENSTR
C
C     CODE  ************************************************************
C

C@lbl bvs 1997Jul24 QUOTE flag was never initialized!
	QUOTE = .FALSE.

	WORD = ' '
	LW = LEN(WORD)
        L = 0
C@tno jcp 1996Jun26_13:40:34 Kbegin stored
        Kbegin=k
C@tno jcp 1996Jul05_08:49:26 assign CLines10 here
        CLines10=k+1

C			     Skip blanks before the word.
10	CONTINUE
	K = K+1
        IF(K.GT.LS) GO TO 30
	IF(K.LT.1) K=1
	A = STRING(K:K)
C@tno jcp 1996Apr04_10:21:28 GetWrd stores binary 0 as if they are non blanks
CC        IF(A.EQ.' '.OR. ICHAR(A).EQ.9) GO TO 10
C skip over ' ' TAB and NULL
        IF((A.EQ.' ').OR.(ICHAR(A).EQ.9).OR.(ICHAR(A).EQ.0)) GO TO 10
	IF(A.EQ.',') GO TO 999
	IF (LS.GT.K) THEN
         IF((A.EQ.'"').AND.(INDEX(STRING(K+1:LS),'"')).NE.0)THEN
	      QUOTE=.TRUE.
         ENDIF
	ENDIF

C			     Copy "WORD" from "STRING".
20	CONTINUE
C			       Copy character from "STRING" to "WORD".
	L = L+1
	IF(L.GT.LW) GO TO 40
	WORD(L:L) = A
C			       Find separator in "STRING".
	K = K+1
	IF(K.GT.LS) GO TO 999
	A = STRING(K:K)
        IF (QUOTE) THEN
           IF (A.EQ.'"') THEN
	       QUOTE=.FALSE.
           ENDIF
        ELSE
	   IF(A.EQ.' '.OR. ICHAR(A).EQ.9) GO TO 25
	   IF(A.EQ.',') GO TO 999
        ENDIF
	GO TO 20

C move K until the end of the string or to the first nonblank after the word
25	K = K+1
	IF(K.GT.LS) GO TO 999
	A = STRING(K:K)
	IF(A.EQ.' '.OR. ICHAR(A).EQ.9) GO TO 25
C quit if comment indicator # is found
	IF(A.EQ.'#') then
	    K=LS
	    GOTO 999
	end if

	K=K-1
	GO TO 999

C@NBI PGS 2000Jul16 - Reordered so that supplementary text is after error message
30      CALL ERROR('Did not find expected word',2)
CC30    write(cof,*) 'Variable=',variab
        write(cof,*) 'Variable=',variab
        write(cof,*) 'looked from position =',kbegin,' to pos=',k
        write(cof,*) 'for a word in the',ls,' long string:'
        write(cof,*) 'string=',string(1:ls)
CC      CALL ERROR('Did not find expected word',2)
	GO TO 60

C@NBI PGS 2000Jul16 - Reordered so that supplementary text is after error message
40      CALL ERROR('Word too long (parameter WRDLEN too small)',2)
CC40    write(cof,*)'word ',word, ' variable=',variab
        write(cof,*)'word ',word, ' variable=',variab
        write(cof,*)'length',lenstr(word)
CC      CALL ERROR('Word too long (parameter WRDLEN too short)',2)
	GO TO 60

60	CONTINUE
C@NBI PGS 1999May13 - added colon after 'Variable'
C@NBI PGS 2000Jul16 - changed CALL from Error2() to LastError2()
        CALL LastError2('Variable: '//Variab(1:lenstr(variab))//
     &  '.  Could not process the input line listed below:',string,3)
999	CONTINUE

	RETURN
	END

C@empa aw 2000dec18 routine GetNrOc is not used any more
CCCh**********************************************************************
CC	SUBROUTINE GetNrOc(word,array,nfilled)
CCC***********************************************************************
CCC GetNrOc gets a number of occupants and their number or the REAL source
CCC possibilities for word: 1.345*occ4 ;	1.345 ;	 occ4  ;  occ4*1.345
CCC hcp sept 1989
CCCh**********************************************************************
CC        IMPLICIT NONE
CCC@tno jcp 1996Jul11_15:52:20 include added to GetNrOc
CC        include 'comv-inp.inc'
CC	Character*(*) word
CC        REAL array(*)
CC        INTEGER nfilled, Eflag
CC        INTEGER N,N2
CC        REAL RELCON
CC        INTEGER INTCON
CC        INTEGER IcIndex
CC
CC	n=index(word,'*')
CC        n2=ICindex(word,'occ')
CC	IF (n.GT.0) THEN
CC	  IF (n2.GT.n) THEN
CCC "1.345*occ4"
CC            variab='number/factor of occupants'
CC	    array(nfilled*2+1)=RELCON(word(1:n-1),Eflag)
CC	    array(nfilled*2+2)=Intcon(word(n2+3:),Eflag)
CC	    nfilled=1
CC	  ELSE
CCC "occ4*1.234"
CC            variab='number/factor of occupants'
CC	    array(nfilled*2+1)=RELCON(word(n+1:),Eflag)
CC	    array(nfilled*2+2)=Intcon(word(n2+3:n-1),Eflag)
CC	    nfilled=1
CC	  ENDIF
CC	ELSE
CC	  IF (n2.GT.0) THEN
CCC "occ4"
CC	    array(nfilled*2+1)=1.0
CC	    array(nfilled*2+2)=Intcon(word(n2+3:),Eflag)
CC
CC	  ELSE
CC            variab='Constant term for occupant pollutant'
CC	    array(5)=RELCON(word,Eflag)
CC	  ENDIF
CC	ENDIF
CC	RETURN
CC	END

C@empa aw 2000dec18 routine twoword is not used any more
CCCh**********************************************************************
CC	SUBROUTINE twoword(word,array,nfilled)
CCC***********************************************************************
CCC split a word which contains a + in two words and process them
CCC possibilities: 1.345*occ4+12.E-6   12E-6+occ4	  occ4+34*occ5
CCC		 1.2*occ4+23*occ1+12.E-6
CCC hcp sept 1989
CCCh**********************************************************************
CC
CC
CC        IMPLICIT NONE
CC	Character*(*) word
CC        REAL array(*)
CC        INTEGER nfilled
CC        INTEGER N
CC
CC	character*160 word1,word2
CC	n=index(word,'+')
CC	IF (n.GT.0) THEN
CC	  word1=word(1:n-1)
CC	  word2=word(n+1:)
CC          CALL getnroc(word1,array,nfilled)
CC	  word=word2
CC	ENDIF
CC	RETURN
CC	END


Ch**********************************************************************
      SUBROUTINE GETLDat(string,K,L,Value,Ptr,Default)
C***********************************************************************
C GETLDat= Get LDat
C GETLDat Gets a value from string and puts it directly in the next free space
C in the array element passed via Value. It can be LDat (air Leakage Data,
C coefficients) or WDat (Wall data). The pointer PtrL is
C updated to the free element in LDat after putting the Value in it.
C In the case K=>L THEN there is no data in the string (fron *CIF) for this
C data item. Therefore we put the passed Default in Ldat
C hcp sept 1989
C tno jcp 1994Nov16 whole routine changed for default and comment
C processing
C
C Changes:
C@tno jcp 1996Jun12_14:30:51 FlgDefault is now an INTEGER
C@tno jcp 1996Jun12_14:45:58 echo default if options>Flg
C@tno jcp 1996Jul05_08:49:26 assign CLines10 here
C@NBI PGS 2000Jul16 - Tidied up code - no syntax changes
C@NBI PGS 2000Jul16 - Clearer/standardized output (and new FORMAT statement)
C@NBI               - UdeDef changed from INTEGER to LOGICAL
C@NBI PGS 2000Aug18 - outputoptions(7) bugfixed. The New convention is:
C@NBI                 FlgDefault = 1 for Normal data entry (no default)
C@NBI                            = 3 for d[e[f[a[u[l[t]]]]]] data entry
C@NBI                            = 2 for default due to end of input line
Ch**********************************************************************
      IMPLICIT NONE
      INCLUDE 'comv-uni.inc'
      INCLUDE 'comv-inp.inc'

C     ! Passed arguments
      CHARACTER*(*) string
      INTEGER k,l,ptr
      REAL Value,Default

C     ! Internal variables & Function declarations
      INTEGER Eflag, answer, LenStr
      LOGICAL UseDef
      CHARACTER word*40
      REAL RELCON
C-----
C     ! reset the flag to use default
      UseDef=.FALSE.
      CLines10=k+1

      IF (K.LT.L) THEN
C        ! we have data in the string from *CIF
         CALL GETWRD(string,K,l,word)
C        ! added check for default or # comment indicator
         call QDefault(l,word,string,answer)
         IF (ANSWER.EQ.0) then
            Value=RELCON(word,Eflag)
C           !!! what to do if Eflag>0 ? replace Value by its default and warn?
            FlgDefault=1
         ELSE
C           this data item in word contained an indicator for: 'use the default'
            UseDef=.TRUE.
            FlgDefault=3
         ENDIF
      ELSE
C        ! we have no data in the string from *CIF, put the Default in
         UseDef=.TRUE.
         FlgDefault=2
      ENDIF

      if (UseDef) then
C	      ! use the default because input line has no data or had 'def' or #
         Value=Default
C@NBI PGS 2000Aug18 - outputoptions(7) bugfixed:
CC       if (outputoptions(7) .ge. FlgDefault) then
         if (MOD(outputoptions(7),FlgDefault).gt.0)
     &   write(COF,100) value,variab(1:lenstr(variab))
     &   ,string(1:lenstr(string))
      endif
      Ptr=Ptr+1

      RETURN
100   FORMAT(' Def. value ',G10.4,' used for ',A,' in line: ',A)
      END
