C+*********************************************************** comv-ut0.f
Ch***********************************************************************
      SUBROUTINE QDefault(L,word,String,answer)
C***********************************************************************
C QDefault = Question: Is this word a default indicator or #?
C if Yes return answer= 1
C if No  return answer= 0  (then this word contains normal data)
C COMIS input contains data (numbers and strings), but also a 'default
C indicator' and a # character for the beginning of comment.
C the default indicator can be any valid part from the beginning of the word '
C default' so if word is:
C word='default'
C word='defaul'
C word='defau'
C word='defa'
C word='def'
C word='de'
C word='d'
C answer will be 1 (Yes, use the default)
C in all other cases answer=0 (No, use the data in the input line)
C
C jcp nov 1994
C
C Changes:
C@    jcp 1994Nov09 new routine QDefault
C@tno jcp 1996Jun12_13:56:51 L=length of the line added to the call
C@tno jcp 1996Jun12_14:06:02 tried to make Qdefault simpler
C@tno jcp 1996Jun12_14:26:01 added pos to look at the start of # or ;
C@tno jcp 1996Jun12_14:26:55 changed Qdefault #==; and string is emptied after ;
C If the first character of word is # or ; also 1 is returned but the dataline
C here, and the string is emptied.
C@NBI PGS 2000Jul16 - Tidied up; no syntax changes
C@NBI PGS 2000Jul16 - Change useDef from INTEGER to LOGICAL
Ch***********************************************************************

      IMPLICIT NONE
      INCLUDE 'comv-uni.inc'

C     ! Passed arguments
      INTEGER answer, L
      CHARACTER*(*) word, String

C     ! Local variables & Function declarations
      INTEGER LW, pos, LenStr
      LOGICAL useDef
      CHARACTER default*7
C-----
C     ! Initialize the flag useDef (use the default) to .FALSE. (Not default)
      Default='default'
      useDef=.FALSE.

C     ! Here follows a test for default
      LW=LenStr(Word)
      if (LW.LE.7) then
        if (Default(1:LW).EQ.Word(1:LW)) then
          useDef=.TRUE.
        end if
      end if

      if (word(1:1) .EQ. '#'.or. word(1:1) .EQ. ';') then
C        !.The remainder of the line is only comment. Make sure that
C        ! default values are used for all data that would normally be read
C        ! from rest of line.
         pos=index(string,word(1:1))
         string(pos:L)=' '
         L=pos-1
         answer=1
      else if (useDef) then
         answer=1
      else
         answer=0
      end if
      return
      END


Ch***********************************************************************
      SUBROUTINE RPTStr(k,number,RepStr,line)
C                       |  |      |      |
C                       |  |      |      string containing the result
C                       |  |      string to be repeated in Line
C                       |  number of times RepStr has to be placed in Line
C                       start position for RepStr in Line
C***********************************************************************
C RPTStr = Repeat String
C Line(k:...)= 'Repstr' number times
C example
C RPTStr(1,5,'# ',line2)
C will return '# # # # # '
C
C jcp nov 1994
C
C Changes:
C@NBI PGS 2000Jul16 - Tidied up; no syntax changes
Ch***********************************************************************

      IMPLICIT NONE
      INCLUDE 'comv-uni.inc'

C     ! Passed arguments
      INTEGER k, number
      CHARACTER*(*) RepStr, Line

C     ! Local variables & Function declarations
      INTEGER I, LenLine, LenRep, PosLine
C-----
      LenLine=LEN(Line)
      LenRep =LEN(RepStr)
      PosLine=k
      DO I=1,number
C        ! Keep track of the last position in Line that would be filled
         PosLine=PosLine+LenRep
         if (PosLine.LE.LenLine) then
            Line(k+(I-1)*LenRep:)=RepStr
         else
C           ! Line length exceeded. Go out of the loop
            GOTO 200
         end if
      ENDDO
200   CONTINUE
      return
      END


Ch***********************************************************************
        SUBROUTINE GETWR(string,K,L,Value,Default,SayIt)
C I/O                    I      IOI O     I       I
C type                   c      i i r     r       L
C                        |      | | |     |       true->write(cof) default
C                        |      | | |     value=Default if no data or on request
C                        |      | | Returned value read or the default
C                        |      | length of string
C                        |      current position
C                        string containing the data
C***********************************************************************
C GETWR= Get a Word and put it in the REAL Value
C
C hcp sept 1989
C
C Changes:
C whole routine changed for default and comment reading
C@tno jcp 1996Jul05_08:49:26 assign CLines10 here
C@tno jcp 1996Jun12_14:30:51 FlgDefault is now an integer, and assigned above
C@NBI PGS 2000Jul16 - Tidied up; no syntax changes
C@NBI PGS 2000Jul16 - Clearer/standardized output (and new FORMAT statement)
C@NBI PGS 2000Jul16 - Change useDef from INTEGER to LOGICAL
C@NBI PGS 2000Aug18 - outputoptions(7) bugfixed. The New convention is:
C@NBI                 FlgDefault = 1 for Normal data entry (no default)
C@NBI                            = 3 for d[e[f[a[u[l[t]]]]]] data entry
C@NBI                            = 2 for default due to end of input line
Ch***********************************************************************

      IMPLICIT NONE
      INCLUDE 'comv-uni.inc'
      INCLUDE 'comv-inp.inc'
      
C     ! Passed arguments
      CHARACTER*(*) string
      INTEGER k,L
      REAL Value,Default
      LOGICAL SayIt
   
C     ! Local variables & Function declarations
      INTEGER Eflag, answer, LenStr
      LOGICAL useDef
      CHARACTER word*40
      REAL RELCON
C-----

C	   ! Reset the flag to use default
      useDef=.FALSE.
      CLines10=k+1

      if (K.LT.L) then
C        ! we have data in the string from *CIF
         CALL GETWRD(string,K,L,word)
C        ! added check for default or # comment indicator
         if (iecho.ge.9) write(cof,*) 'k,word,string',k,word,string
         call QDefault(L,word,string,answer)
         if (iecho.ge.9) write(cof,*) 'k,word,string',k,word,string
         if (ANSWER.EQ.0) then
            Value=RELCON(word,Eflag)
            FlgDefault=1
         else
C	         ! this data item in word contained an indicator for: 'use the default'
            useDef=.TRUE.
            FlgDefault=3
         endif
      else
C	      ! we have no data in the string from *CIF , put the Default in
         useDef=.TRUE.
         FlgDefault=2
      endif

      if (useDef) then
C	      ! use the default because input line has no data or had 'def'
         Value=Default
C@NBI PGS 2000Aug18 - outputoptions(7) bugfixed:
CC       if (SayIt .AND. outputoptions(7).eq.1) then
         if (SayIt .AND. MOD(outputoptions(7),FlgDefault).gt.0)
     &   write(COF,100) value,variab(1:lenstr(variab))
     &	,string(1:lenstr(string))
      endif

      RETURN
100   FORMAT(' Def. value ',G10.4,' used for ',A,' in line: ',A)
      END


Ch***********************************************************************
      SUBROUTINE GETWI(string,K,L,iValue,Default)
C***********************************************************************
C GetWI= Get a Word and put it in the integer iValue
C
C hcp sept 1989
C
C Changes:
C routine changed to process defaults and comment
C@tno jcp 1996Jun12_14:30:51 FlgDefault is now an integer
C@tno jcp 1996Jul05_08:49:26 assign CLines10 here
C@lbl dml 1999nov30 Clean up indentation to reflect logical structure.
C@lbl dml 1999nov30 Change useDef from integer to logical. Change
C     place where set FlgDefault=0, to make parallel with =1,=2.
C@NBI PGS 2000Jul16 - Tidied up; no syntax changes
C@NBI PGS 2000Jul16 - Clearer/standardized output (and new FORMAT statement)
C@NBI PGS 2000Aug18 - outputoptions(7) bugfixed. The New convention is:
C@NBI                 FlgDefault = 1 for Normal data entry (no default)
C@NBI                            = 3 for d[e[f[a[u[l[t]]]]]] data entry
C@NBI                            = 2 for default due to end of input line
Ch***********************************************************************

      IMPLICIT NONE
      INCLUDE 'comv-uni.inc'
      INCLUDE 'comv-inp.inc'

C     ! Passed arguments.
      CHARACTER*(*) string
      INTEGER k,L,iValue,Default

C     ! Local variables.
      INTEGER Eflag,answer
      LOGICAL useDef
      CHARACTER word*40

C     ! Function declarations.
      INTEGER INTCON
      INTEGER LENSTR
C-----
C     ! Assume do not use default.
      useDef = .false.
      CLines10=k+1

      if( K .lt. L ) then
C        ! Have data in the string from *CIF.
         CALL GETWRD(string,K,L,word)
C        ! Check for default or # comment indicator
         call QDefault(L,word,string,answer)
         if( answer.EQ.0 ) then
            iValue=INTCON(word,Eflag)
            FlgDefault=1
         else
C           ! This data item in word contained an indicator for:
C           ! 'use the default'
            useDef = .true.
            FlgDefault=3
         endif
      else
C        ! Have no data in the string from *CIF , put the default in
         useDef = .true.
         FlgDefault=2
      endif

      if( useDef ) then
C        ! Use the default (input line has no data or had 'def' or #)
         iValue=Default
C@NBI PGS 2000Aug18 - outputoptions(7) bugfixed:
CC       if (outputoptions(7) .eq. 1) then
         if (MOD(outputoptions(7),FlgDefault).gt.0) 
     &   write(COF,100) Ivalue,variab(1:lenstr(variab))
     &   ,string(1:lenstr(string))
      endif

      RETURN
100   FORMAT(' Def. value ',I10,' used for ',A,' in line: ',A)
      END


Ch***********************************************************************
        SUBROUTINE GETWS(string,K,L,Word,DefWrd)
C***********************************************************************
C GetWS= Get a Word and put it in the String word
C
C hcp sept 1989
C
C Changes:
C@tno jcp 1996Jul05_08:49:26 assign CLines10 here
C@tno jcp 1996Jun12_14:30:51 FlgDefault is now an integer, and assigned above
C@lbl bvs Apr 1995 - Added keyword 'default' and 'def' to use default value
C@NBI PGS 2000Jul16 - Tidied up; no syntax changes
C@NBI PGS 2000Jul16 - Clearer/standardized output (and new FORMAT statement)
C@NBI PGS 2000Jul16 - Change useDef from INTEGER to LOGICAL
C@NBI PGS 2000Aug18 - outputoptions(7) bugfixed. The New convention is:
C@NBI                 FlgDefault = 1 for Normal data entry (no default)
C@NBI                            = 3 for d[e[f[a[u[l[t]]]]]] data entry
C@NBI                            = 2 for default due to end of input line
Ch***********************************************************************

      IMPLICIT NONE
      INCLUDE 'comv-uni.inc'
      INCLUDE 'comv-inp.inc'

C     ! Passed arguments
      INTEGER k,L
      CHARACTER*(*) string,Word,DefWrd

C     ! Local variables & Function declarations
      INTEGER answer, LenStr
      LOGICAL useDef
C-----
C     ! reset the flag to use default
      useDef=.FALSE.
      CLines10=k+1

      if (K.LT.L) then
C        ! we have data in the string from *CIF
         CALL GETWRD(string,K,L,word)
C        ! added check for default or # comment indicator
         call QDefault(L,word,string,answer)
         if (ANSWER.EQ.1) then
C          ! this data item in word contained an indicator for: 'use the default'
           useDef=.TRUE.
           FlgDefault=3
         endif
      else
C        ! we have no data in the string from *CIF , put the Default in
         useDef=.TRUE.
         FlgDefault=2
      endif

      if (useDef) then
C        ! use the default because input line has no data or had 'def' or #
         word=DefWrd
C@NBI PGS 2000Aug18 - outputoptions(7) bugfixed:
CC       if (outputoptions(7) .eq. 1) then
         if (MOD(outputoptions(7),FlgDefault).gt.0)
     &   write(COF,100) word,variab(1:lenstr(variab))
     &   ,string(1:lenstr(string))
      else
         FlgDefault=1
      endif
      RETURN
100   FORMAT(' Def. word "',A,'" used for ',A,' in line: ',A)
      END


C@NBI PGS 1999Aug06 - New function to right align a string
Ch***********************************************************************
      CHARACTER*15 FUNCTION RightAlign(String,Length)
C***********************************************************************
C Purpose: RIGHT ALIGNS A STRING, PREFIXED WITH BLANKS.
C          THIS FUNCTION IS USEFUL FOR OUTPUT ROUTINES, WHERE TEXT
C          IS TO BE ALIGNED WITH NUMBER COLUMNS.
C Source:  @NBI PGS 1999Aug06
C Changes: 
C Limits : Length can not exceed 15 characters
C Pass parameters:
C IO # Name       Units        Description
C  O - RightAlign [-]          right-aligned version of 'Str'
C I  1 String     [-]          string to be right-aligned
C I  2 Length     [-]          length of output string 'RightAlign'
Ch***********************************************************************

      IMPLICIT NONE
      CHARACTER*(*) String
      INTEGER Length,Lenstr,L1,L2,i
      L2=MIN(Length,15)
      L1=MIN(Lenstr(String),L2)
      DO i=1,L2-L1
         RightAlign(i:i)=' '
      ENDDO
      RightAlign(L2-L1+1:)=String(1:L1)
      END

