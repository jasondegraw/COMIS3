
Ch**********************************************************************
      SUBROUTINE WriteProgressCVF_QW(line)
C***********************************************************************
C@empa aw 2005jun07
C     Writes the "PROGRESS" line in an extra Window of a CVF QuickWin
C     application
C
C***********************************************************************
!     USE statements to include routine interfaces
      USE dflib
      USE dfwin
      IMPLICIT NONE
!     Data declarations
      INTEGER lenstr
      LOGICAL initialize
      CHARACTER*160 Line
      INTEGER(4) status
      TYPE (QWINFO) winfo
      TYPE (rccoord) curpos
      DATA initialize/.TRUE./ 
      SAVE initialize

      IF(initialize)THEN
         initialize=.FALSE.
         OPEN (UNIT= 13, FILE= 'USER', TITLE= 'PROGRESS')
         winfo.X    = 0
         winfo.Y    = 32
         winfo.H    = 4
         winfo.W    = 110
         winfo.TYPE = QWIN$SET
         status     = SETWSIZEQQ(13, winfo)
      ENDIF
      
      status=SETACTIVEQQ (13) !set the window active without focus
      CALL SETTEXTPOSITION (INT2(0), INT2(0), curpos) !cursor position X=0; Y=0
      call OUTTEXT(' '//line) !write the line to the window
      return
      end

