Ch**********************************************************************
      SUBROUTINE WriteProgressCVF_CA(line)
C***********************************************************************
C@empa aw 2005jun07
C     Writes the "PROGRESS" line to the bottom of the console of a CVF 
C     console application
C
C***********************************************************************
!     USE statements to include routine interfaces
      USE dflib
      USE dfwin
      IMPLICIT NONE
!     Data declarations
      INTEGER fhandle,ypos,lenstr
      LOGICAL lstatx,curerr
      LOGICAL initialize
      Type(T_COORD) wpos
      Type(T_CONSOLE_SCREEN_BUFFER_INFO) cinfo
      CHARACTER*160 Line,str

      fhandle = GetStdHandle(STD_OUTPUT_HANDLE)
      curerr  = GetConsoleScreenBufferInfo(fhandle,cinfo)
      wpos.x = 0 ! 0 characters from left
      wpos.y = cinfo.srwindow.bottom-2 ! 2 lines above bottom
      ypos= cinfo.dwCursorPosition.y ! save the old cursor position
      IF (ypos.ge.wpos.y) ypos=ypos-2 ! old position at least two lines above "PROGRESS" line 
      lstatx = SetConsoleCursorPosition(fhandle, wpos) ! Set cursorposition for "PROGRESS" line
 
      WRITE(6,'(1X,A)')line(1:lenstr(line))//'   ' ! write the "PROGRESS" line
      wpos.y=ypos                                  ! restore the old cursor position
      lstatx = SetConsoleCursorPosition(fhandle, wpos)
      RETURN
      END


