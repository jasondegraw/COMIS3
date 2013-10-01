C+*********************************************************** comv-unx.f
C REMEMBER!  Changes to this file should also be made in file COMV-WIN.FOR
C
C This is the command-line parser for FORTRAN compilers for UNIX and some for
C Intel PCs (e.g. NAGware).
C The only difference between this and the one for Windows is that different
C nonstandard FORTRAN library functions are used instead of the Unix
C iArgC() and iGetArg(,). 
C
Ch***********************************************************************
CC	SUBROUTINE RdCmdLn(InFile,OutFile,UserOutputFile,UOFunf)
	SUBROUTINE RdCmdLn(InFile,OutFile,UserOutputFile,UFunf)
C***********************************************************************
C
C This routine reads the used filenames from the command line.
C
C Commandline Options: comis -i inputfile
C                            -o outputfile
C                            -t projectname
C                            -a basefilename
C			                   -uf useroutputfile (formatted)
C			                   -uu useroutputfile (unformatted)
C@lbl bs 1991aug08
C
C Changes:
C@lbl envuxr 11/1/1991 useroutputfile added
C@lbl envakk 11nov91 option 'A' added and -u replaced by -uf and -uu
C@lbl bvs 1997Jul22 UOFunf changed to UFunf because UOFunf is in comv-inp.inc
C@lbl bvs 1997Jul22 -t option is projectname, not tablename
C@lbl bvs 1997Jul22 -t option added - wasn't parsed before
C@lbl bvs 1997Jul22 comv-inp.inc is needed for -t option
C@lbl bvs 1997Jul22 -t option added for projectname
C@lbl bvs 1997Dec11 declare DUM and IGETARG()
C@CSTB NB 12MAR 1998 Changed NARGS Call for DVF
C@CSTB NB 12MAR 1998 Changed to GETARG for DVF
C@NBI PGS 1999Aug13 - Whole subroutine steamlined.  Older comments in body
C@NBI                 of code moved up here to 'changes'.  Tabs replaced
C@NBI                 by spaces
C@NBI               - Added explanation of command line syntax
C@NBI PGS 2000Jul18 - Added de-comment list for different compilers so that
C@NBI                 source code files COMV-WIN.FOR and COMV-UNX.FOR can be
C@NBI                 more easily maintained and used.
C@empa aw 2000dec07 - Added code for Compaq Visual FORTRAN 
C
C Pass parameters:
C
Ch***********************************************************************

C$    ! Just de-comment the appropriate line for your compiler
C$    USE DFLIB    ! Digital Visual FORTRAN (DVF) or Compaq Visual FORTRAN

      IMPLICIT NONE
      INCLUDE 'comv-uni.inc'
      INCLUDE 'comv-inp.inc'
      CHARACTER*(*) InFile,OutFile,UserOutputFile
      CHARACTER cmdstr*160, opt*3
      INTEGER UFunf,numargs,IargC,LENSTR,DUM,IGETARG,Error
C$    ! Just de-comment the appropriate line-pair for your compiler
C$    INTEGER (2) i    ! DVF or Compaq Visual FORTRAN (i must be INTEGER kind 2)
C$    INTEGER i        ! All other compilers
      INTEGER i

C     Get the number of command-line arguments
C$    ! Just de-comment the appropriate line for your compiler
C$    numargs = IARGC()    ! Watcom compiler
C$    numargs = IARGC() +1 ! Unix; NAGware f90 compilers
C$    numargs = NARGS()    ! DVF or Compaq Visual FORTRAN
      numargs = IARGC() +1

      IF (MOD(numargs,2).EQ.0) THEN
        Error=1
        GOTO 999
      ENDIF

      DO i=1,numargs-1,2
C$      ! Just de-comment the appropriate line-pair for your compiler
C$      dum = iGETARG(i,opt)        ! Watcom compiler
C$      dum = iGETARG(i+1,cmdstr)   !   "       "
C$      CALL GETARG(i,opt)          ! Unix or NAGware F90 compilers
C$      CALL GETARG(i+1,cmdstr)     !   "       "
C$      CALL GETARG(i,opt)          ! DVF or Compaq Visual FORTRAN (1st argument is INTEGER kind 2)
C$      CALL GETARG(i+1_2,cmdstr)   !   "       "
        CALL GETARG(i,opt)
        CALL GETARG(i+1,cmdstr)

        CALL UpperC(opt)
        IF (opt(1:1).NE.'-') THEN
          Error=2
          GOTO 999
        ENDIF

C--------------------------
C       -i input file name

        IF (opt(2:2).EQ.'I') THEN
           InFile = cmdstr

C--------------------------
C       -o output file name

        ELSEIF (opt(2:2).EQ.'O') THEN
          OutFile = cmdstr

C-------------------------------------------------
C       -t project name to attach to .CSO filename

        ELSEIF (opt(2:2).EQ.'T') THEN
          COSs = cmdstr

C------------------------------------------
C       -uf user formatted output requested

        ELSEIF (opt(2:3).EQ.'UF') THEN
          UserOutputFile = cmdstr
          UFunf = 0

C---------------------------------------
C       -uu unformatted output requested

        ELSEIF (opt(2:3).EQ.'UU') THEN
          UserOutputFile = cmdstr
          UFunf = 1

C---------------------------------------
C       -a base name for output filename

        ELSEIF (opt(2:2).EQ.'A') THEN
          InFile = cmdstr(1:lenstr(cmdstr))//'.cif'
C@empa aw 2000jun26 As in almost any case a COF is needed, I cancel the following
C@empa              condition. Now, it is sufficient  to have just -a option and do
C@empa              without COMIS.SET. If sombody still wants to have the possibility
C@empa              to suppress COF with -uu or -uf option together with -a optiion,
C@empa              the next line has to be used again.
CC          if (lenstr(OutFile).ne. 0)
           OutFile = cmdstr(1:lenstr(cmdstr))//'.cof'
          if (lenstr(UserOutputFile) .ne. 0) then
            if (UFunf .eq. 0) then
              UserOutputFile = cmdstr(1:lenstr(cmdstr))//'.uof'
            else
              UserOutputFile = cmdstr(1:lenstr(cmdstr))//'.uuf'
            endif
          endif
          COSs = cmdstr

C-----------------------------------
C       Errors / help syntax message

        ELSE
          Error=2
          GOTO 999
        ENDIF
      ENDDO
      IF (lenstr(InFile) .eq. 0) then
        Error=3
        GOTO 999
      ELSEIF ((lenstr(OutFile)       .eq.0) 
     &  .and. (lenstr(UserOutputFile).eq.0))then
        Error=4
        GOTO 999
      ENDIF
      RETURN

999   CRT=6
C     First write to CRT (screen), then to .CER file
      DO i=CRT,CER,CER-CRT
        IF(Error.EQ.1)THEN
          WRITE(i,*) 'ERROR:  Wrong number of command line arguments.'
        ELSEIF(Error.EQ.2)THEN
          WRITE(i,*) 'ERROR:  Non-valid command-line option: ',opt
        ELSEIF(Error.EQ.3)THEN
          WRITE(i,*) 'ERROR:  No input filename is specified'
        ELSEIF(Error.EQ.4)THEN
          WRITE(i,*) 'ERROR:  No output file is specified'
        ENDIF
        WRITE(i,1000)
C@NBI PGS 2000dec23 - Next line prevents infinite loop when CER = CRT
        IF(CER.EQ.CRT) GOTO 11
      ENDDO
11    CONTINUE      
      
1000  FORMAT(/" SYNTAX    : COMIS [options]"//
     &" OPTIONS "/
     &"-i  <file> : Input filename   (usually with extension .CIF)"/
     &"-o  <file> : Text output file (usually with extension .COF)"/
     &"-t  <name> : Spreadsheet output rootname (no file extension)"/
     &"-uu <file> : Filename for user-defined unformated output"/
     &"-uf <file> : Filename for user-defined   formated output"/
     &"-a  <name> : All in/output files (those mentioned in .SET file"/
     &"             and before on command-line) get a new common name"//
     &" Default values for options -i -o -t -uu and -uf are taken from"/
     &" the COMIS.SET file, under heading &-COMIS, using the keywords"/
     &" INPUT, OUTPUT, TABLES, USEROUTU and USEROUTF respectively."/
     &" You must have at least 1 output option: [-o] [-uu or -uf]"/)
      STOP
      END
      
