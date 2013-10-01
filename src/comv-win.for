C+*********************************************************** comv-win.f
C REMEMBER!  Changes to this file should also be made in file COMV-UNX.FOR
C
C This is the command-line parser for Intel PCs using DVF FORTRAN compiler.
C The only difference between this and the one for Unix is that different
C nonstandard FORTRAN library functions are used instead of the Unix
C iArgC() and iGetArg(,)
C
Ch***********************************************************************
CCCC	SUBROUTINE RdCmdLn(InFile,OutFile,UserOutputFile,UOFunf)
CC	SUBROUTINE RdCmdLnOld(InFile,OutFile,UserOutputFile,UFunf)
CCC***********************************************************************
CCC
CCC This routine reads the used filenames from the command line.
CCC
CCC Commandline Options: comis -i inputfile
CCC                            -o outputfile
CCC                            -t projectname
CCC                            -a basefilename
CCC			                   -uf useroutputfile (formatted)
CCC			                   -uu useroutputfile (unformatted)
CCC@lbl bs 1991aug08
CCC
CCC Changes:
CCC@lbl envuxr 11/1/1991 useroutputfile added
CCC@lbl envakk 11nov91 option 'A' added and -u replaced by -uf and -uu
CCC@lbl bvs 1997Jul22 UOFunf changed to UFunf because UOFunf is in comv-inp.inc
CCC@lbl bvs 1997Jul22 -t option is projectname, not tablename
CCC@lbl bvs 1997Jul22 -t option added - wasn't parsed before
CCC@lbl bvs 1997Jul22 comv-inp.inc is needed for -t option
CCC@lbl bvs 1997Jul22 -t option added for projectname
CCC@lbl bvs 1997Dec11 declare DUM and IGETARG()
CCC@CSTB NB 12MAR 1998 Changed NARGS Call for DVF
CCC@CSTB NB 12MAR 1998 Changed to GETARG for DVF
CCC@NBI PGS 1999Aug13 - Whole subroutine steamlined.  Older comments in body
CCC@NBI                 of code moved up here to 'changes'.  Tabs replaced
CCC@NBI                 by spaces
CCC@NBI               - Added explanation of command line syntax
CCC@NBI PGS 2000Jul18 - Added de-comment list for different compilers so that
CCC@NBI                 source code files COMV-WIN.FOR and COMV-UNX.FOR can be
CCC@NBI                 more easily maintained and used.
CCC@empa aw 2000dec07 - Added code for Compaq Visual FORTRAN 
CCC
CCC Pass parameters:
CCC
CCCh***********************************************************************
CC
CCC$    ! Just de-comment the appropriate line for your compiler
CCC$    USE DFLIB    ! Digital Visual FORTRAN (DVF) or Compaq Visual FORTRAN
CC      USE DFLIB
CC
CC      IMPLICIT NONE
CC      INCLUDE 'comv-uni.inc'
CC      INCLUDE 'comv-inp.inc'
CC      CHARACTER*(*) InFile,OutFile,UserOutputFile
CC      CHARACTER cmdstr*160, opt*3
CC      INTEGER UFunf,numargs,IargC,LENSTR,DUM,IGETARG,Error
CCC$    ! Just de-comment the appropriate line-pair for your compiler
CCC$    INTEGER (2) i    ! DVF or Compaq Visual FORTRAN (i must be INTEGER kind 2)
CCC$    INTEGER i        ! All other compilers
CC      INTEGER (2) i
CC
CCC     Get the number of command-line arguments
CCC$    ! Just de-comment the appropriate line for your compiler
CCC$    numargs = IARGC()    ! Watcom compiler
CCC$    numargs = IARGC() +1 ! Unix or NAGware f90 compilers
CCC$    numargs = NARGS()    ! DVF or Compaq Visual FORTRAN
CC      numargs = NARGS()
CC
CC      IF (MOD(numargs,2).EQ.0) THEN
CC        Error=1
CC        GOTO 999
CC      ENDIF
CC
CC      DO i=1,numargs-1,2
CCC$      ! Just de-comment the appropriate line-pair for your compiler
CCC$      dum = iGETARG(i,opt)        ! Watcom compiler
CCC$      dum = iGETARG(i+1,cmdstr)   !   "       "
CCC$      CALL GETARG(i,opt)          ! Unix or NAGware F90 compilers
CCC$      CALL GETARG(i+1,cmdstr)     !   "       "
CCC$      CALL GETARG(i,opt)          ! DVF or Compaq Visual FORTRAN (1st argument is INTEGER kind 2)
CCC$      CALL GETARG(i+1_2,cmdstr)   !   "       "
CC        CALL GETARG(i,opt)
CC        CALL GETARG(i+1_2,cmdstr)
CC
CC        CALL UpperC(opt)
CC        IF (opt(1:1).NE.'-') THEN
CC          Error=2
CC          GOTO 999
CC        ENDIF
CC
CCC--------------------------
CCC       -i input file name
CC
CC        IF (opt(2:2).EQ.'I') THEN
CC           InFile = cmdstr
CC
CCC--------------------------
CCC       -o output file name
CC
CC        ELSEIF (opt(2:2).EQ.'O') THEN
CC          OutFile = cmdstr
CC
CCC-------------------------------------------------
CCC       -t project name to attach to .CSO filename
CC
CC        ELSEIF (opt(2:2).EQ.'T') THEN
CC          CSOs = cmdstr
CC
CCC------------------------------------------
CCC       -uf user formatted output requested
CC
CC        ELSEIF (opt(2:3).EQ.'UF') THEN
CC          UserOutputFile = cmdstr
CC          UFunf = 0
CC
CCC---------------------------------------
CCC       -uu unformatted output requested
CC
CC        ELSEIF (opt(2:3).EQ.'UU') THEN
CC          UserOutputFile = cmdstr
CC          UFunf = 1
CC
CCC---------------------------------------
CCC       -a base name for output filename
CC
CC        ELSEIF (opt(2:2).EQ.'A') THEN
CC          InFile = cmdstr(1:lenstr(cmdstr))//'.cif'
CCC@empa aw 2000jun26 As in almost any case a COF is needed, I cancel the following
CCC@empa              condition. Now, it is sufficient  to have just -a option and do
CCC@empa              without COMIS.SET. If sombody still wants to have the possibility
CCC@empa              to suppress COF with -uu or -uf option together with -a optiion,
CCC@empa              the next line has to be used again.
CCCC          if (lenstr(OutFile).ne. 0)
CC           OutFile = cmdstr(1:lenstr(cmdstr))//'.cof'
CC          if (lenstr(UserOutputFile) .ne. 0) then
CC            if (UFunf .eq. 0) then
CC              UserOutputFile = cmdstr(1:lenstr(cmdstr))//'.uof'
CC            else
CC              UserOutputFile = cmdstr(1:lenstr(cmdstr))//'.uuf'
CC            endif
CC          endif
CC          CSOs = cmdstr
CC
CCC-----------------------------------
CCC       Errors / help syntax message
CC
CC        ELSE
CC          Error=2
CC          GOTO 999
CC        ENDIF
CC      ENDDO
CC      IF (lenstr(InFile) .eq. 0) then
CC        Error=3
CC        GOTO 999
CC      ELSEIF ((lenstr(OutFile)       .eq.0) 
CC     &  .and. (lenstr(UserOutputFile).eq.0))then
CC        Error=4
CC        GOTO 999
CC      ENDIF
CC      RETURN
CC
CC999   CRT=6
CCC     First write to CRT (screen), then to .CER file
CC      DO i=CRT,CER,CER-CRT
CC        IF(Error.EQ.1)THEN
CC          WRITE(i,*) 'ERROR:  Wrong number of command line arguments.'
CC        ELSEIF(Error.EQ.2)THEN
CC          WRITE(i,*) 'ERROR:  Non-valid command-line option: ',opt
CC        ELSEIF(Error.EQ.3)THEN
CC          WRITE(i,*) 'ERROR:  No input filename is specified'
CC        ELSEIF(Error.EQ.4)THEN
CC          WRITE(i,*) 'ERROR:  No output file is specified'
CC        ENDIF
CC        WRITE(i,1000)
CCC@NBI PGS 2000dec23 - Next line prevents infinite loop when CER = CRT
CC        IF(CER.EQ.CRT) GOTO 11
CC      ENDDO
CC11    CONTINUE      
CC      
CC1000  FORMAT(/" SYNTAX    : COMIS [options]"//
CC     &" OPTIONS "/
CC     &"-i  <file> : Input filename   (usually with extension .CIF)"/
CC     &"-o  <file> : Text output file (usually with extension .COF)"/
CC     &"-t  <name> : Spreadsheet output rootname (no file extension)"/
CC     &"-uu <file> : Filename for user-defined unformated output"/
CC     &"-uf <file> : Filename for user-defined   formated output"/
CC     &"-a  <name> : All in/output files (those mentioned in .SET file"/
CC     &"             and before on command-line) get a new common name"//
CC     &" Default values for options -i -o -t -uu and -uf are taken from"/
CC     &" the COMIS.SET file, under heading &-COMIS, using the keywords"/
CC     &" INPUT, OUTPUT, TABLES, USEROUTU and USEROUTF respectively."/
CC     &" You must have at least 1 output option: [-o] [-uu or -uf]"/)
CC      STOP
CC      END
      
Ch**********************************************************************
      SUBROUTINE RdCmdLn
C***********************************************************************
C
C This routine reads the filenames and output options from the command line.
C SYNTAX  : COMIS  [ifile]  [ofile]  [+o|-o]  [+s|-s]  [+uf|-uf]  [+uu|-uu]
C
C [ifile] - Root name of *.CIF input file (including path but without ext.).
C           If omitted, or ""default"", COMIS uses the default root name
C           given in COMIS.SET file, under heading &-COMIS, at keyword INPUT.
C [ofile] - Root name of output files (without extension). If omitted, [ifile]
C           is used as rootname for all output files. If ""default"", COMIS uses
C           the default root names given in COMIS.SET file, under heading 
C           &-COMIS, using the keywords OUTPUT, TABLES, USEROUTU and USEROUTF.
C [+o|-o] - Force or suppress output of .COF file  (Forced by default unless blank in .SET file)
C [+s|-s] - Force or suppress output of .COS or .COH spreadsheet files  (Forced by default unless blank in .SET file)
C [+uf|-uf] Force or suppress output of .COU formatted user file
C [+uu|-uu] Force or suppress output of .COU unformatted user file
C
C These combinations of [ifile] and [ofile] are possible:
C   COMIS                - Input & output root names given in COMIS.SET
C   COMIS Root1          - Input & output files have same root name ROOT1
C   COMIS Root1   Root2  - Input file ROOT1.CIF; output filenames ROOT2*.*
C   COMIS Root1 Default  - Input file ROOT1.CIF; output root name from COMIS.SET
C   COMIS Default Root2  - Input file from COMIS.SET; output filenames ROOT2*.*
C
C Example of using + and - options:
C   COMIS Root1 -o +uf   - Input & output files have root name ROOT1;
C                          No .COF file is generated (it is supressed),
C                          but formatted user file .COU is generated.
C PASS ARGUMENTS:
C IO  Name     Unit  Description
C
C IO* CIFs     -     Path & file root name (i.e. exlcluding extension) of .CIF input file
C IO* COFs     -     Path & file root name of .COF output file
C IO* COSs     -     Path & file root name of .COS (spreadsheet) and .COH (histogram) output files
C IO* COUs     -     Path & file root name of .COU (user) output file
C IO* COUunf   -     INTEGER flag for .COU file format 0=formatted, 1=unformatted
C ( * means that it is in COMMON block in INCLUDE comv-inp.inc, and is not passed as subroutine argument )
C
C
C@lbl bs 1991aug08
C CHANGES:
C@lbl envuxr 11/1/1991 useroutputfile added
C@lbl envakk 11nov91 option 'A' added and -u replaced by -uf and -uu
C@lbl bvs 1997Jul22 COUunf changed to UFunf because COUunf is in comv-inp.inc
C@NBI PGS 2000Jul31 .. but why not just use COUunf and then there is no need 
C@NBI               to declare as subroutine argument. 
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
C@NBI               - Simplified command-line syntax to COMIS [ifile] [ofile] ...,
C@NBI                 Users almost invariably want the same root name for 
C@NBI                 output files for a given simulation.  The command-line
C@NBI                 can therefore be simplified. We just need the command-line
C@NBI                 to tell us what the common root name is.
C@NBI                 - .COF is generated by default (unless suppressed with -o or
C@NBI                   OUTPUT=' ' in COMIS.SET)
C@NBI                 - .COS and .COH are generated by default as inferred
C@NBI                   from existence of keywords in &-PR-OUTP block (unless
C@NBI                   suppressed with -s or TABLES=' ' in COMIS.SET)
C@NBI                 - Whether .COU is to be generated must be 
C@NBI                   specified on command-line [+uu|+uf] or COMIS.SET
C@NBI                 This is fully backwards-compatible with older versions
C@NBI                 of COMIS .CIF and .SET files!.
C@NBI PGS 2000Jul18 - Changed file extensons .cho to .coh, .cso to .cos,
C@NBI                 and .uof to .cou, so that all output file extensions
C@NBI                 start with .co ("o" for output) and all input files
C@NBI                 should have extension .ci? ("i" for input)
Ch**********************************************************************

C$    ! Just de-comment the appropriate line for your compiler
C$    USE DFLIB    ! DVF compiler
      USE DFLIB

      IMPLICIT NONE
      INCLUDE 'comv-uni.inc'
      INCLUDE 'comv-inp.inc'
C@empa aw  1005oct06 longer strings
      CHARACTER opt*340
      CHARACTER CommonRoot*160
      INTEGER(2) i
      INTEGER numargs,IargC,LENSTR,DUM,IGETARG,Error,nAargs
     &,LenOpt,j,OutRootType
      LOGICAL SetFile

C------------------------------------------
C     Parse command-line for I/O file names
C------------------------------------------

C     Get the number of command-line arguments
C$    ! Just de-comment the appropriate line for your compiler
C$    numargs = IARGC()    ! Watcom compiler; NAGware f90 compilers
C$    numargs = IARGC()+1  ! Unix
C$    numargs = NARGS()    ! DVF compiler
C@empa aw 2005jul15 NARGS gives the numbers of command-line arguments, including the command
CC       numargs = NARGS()
         numargs = NARGS()-1

C     ! OutRootType=1 : Output file root names defined in SET file 
C     ! OutRootType=2 :   "     "    "     "   same as .CIF root name
C     ! OutRootType=3 :   "     "    "     "   defined in command-line
      OutRootType=1

      DO i=1,numargs
C$       ! Just de-comment the appropriate line-pair for your compiler
C$       dum =  iGETARG(i,opt)       ! Watcom compiler
C$       CALL GETARG(i,opt)          ! Unix; NAGware f90; DVF compilers
         CALL GETARG(i,opt)
         LenOpt=LenStr(opt)
C        Don't want to change case of opt because UNIX file system is case sensitive
         IF ((opt(1:1).NE.'-').AND.(opt(1:1).NE.'+')) THEN
C          .It is either [ifile] or [ofile]
            IF (i.EQ.1) THEN
C             .Input file root name - "default" usually only written in lower case
               IF (opt(1:LenOpt).EQ."default") THEN
                  IF (LenStr(CIFs).EQ.0) THEN
                     Error=1
                     GOTO 999
                  ENDIF
               ELSE
                  CIFs=opt(1:LenOpt)
               ENDIF
               OutRootType=2
            ELSEIF (i.EQ.2) THEN
C             .Root name for all output files - "default" usually only written in lower case
               IF (opt(1:LenOpt).EQ."default") THEN
                  OutRootType=1
               ELSE
                  CommonRoot=opt(1:LenOpt)
                  OutRootType=3
               ENDIF
            ELSE
C              .[File] option in wrong place
                Error=2
                GOTO 999
            ENDIF
         ENDIF
      ENDDO

C     ...now force the ouput of .COF/.COS/.COH files by default
C     (can be overridden by option "-" or by defining as empty in COMIS.SET)
      IF (OutRootType.EQ.2) THEN
         COFs=CIFs
         COSs=CIFs
      ELSEIF (OutRootType.EQ.3) THEN
         COFs=CommonRoot
         COSs=CommonRoot
      ENDIF
      IF(LenStr(CIFs).EQ.0) THEN
         Error=3
         GOTO 999
      ENDIF

C------------------------------------------
C     Parse command line for output options
C------------------------------------------

      DO i=1,numargs
C$       ! Just de-comment the appropriate line-pair for your compiler
C$       dum =  iGETARG(i,opt)       ! Watcom compiler
C$       CALL GETARG(i,opt)          ! Unix; NAGware f90; DVF compilers
         CALL GETARG(i,opt)
         CALL UpperC(opt)
         LenOpt=LenStr(opt)
         IF ((opt(1:1).EQ.'-').OR.(opt(1:1).EQ.'+')) THEN
C          .It is a [+..] or [-..] option
            IF (opt(1:LenOpt).EQ.'+O') THEN
               IF (OutRootType.EQ.1) THEN
                  IF (LenStr(COFs).EQ.0) THEN
                     Error=4
                     GOTO 999
                  ENDIF
               ELSEIF (OutRootType.EQ.2) THEN
                  COFs=CIFs
               ELSEIF (OutRootType.EQ.3) THEN
                  COFs=CommonRoot
               ENDIF
            ELSEIF (opt(1:LenOpt).EQ.'-O') THEN
               COFs=' '
            ELSEIF (opt(1:LenOpt).EQ.'+S') THEN
               IF (OutRootType.EQ.1) THEN
                  IF (LenStr(COSs).EQ.0) THEN
                     Error=4
                     GOTO 999
                  ENDIF
               ELSEIF (OutRootType.EQ.2) THEN
                  COSs=CIFs
               ELSEIF (OutRootType.EQ.3) THEN
                  COSs=CommonRoot
               ENDIF
            ELSEIF (opt(1:LenOpt).EQ.'-S') THEN
               COSs=' '
            ELSEIF ((opt(1:LenOpt).EQ.'+UF')
     &          .OR.(opt(1:LenOpt).EQ.'+UU')) THEN
               IF (OutRootType.EQ.1) THEN
                  IF (LenStr(COUs).EQ.0) THEN
                     Error=4
                     GOTO 999
                  ENDIF
               ELSEIF (OutRootType.EQ.2) THEN
                  COUs=CIFs
               ELSEIF (OutRootType.EQ.3) THEN
                  COUs=CommonRoot
               ENDIF
               COUunf=0
               IF (opt(1:LenOpt).EQ.'+UU') COUunf=1
            ELSEIF ((opt(1:LenOpt).EQ.'-UU')
     &          .OR.(opt(1:LenOpt).EQ.'-UF')) THEN
               COUs=' '
            ELSE
C              .Unrecognized option
                Error=5
                GOTO 999
            ENDIF
         ENDIF
      ENDDO
      IF(  (LenStr(COFs).EQ.0)
     &.AND.(LenStr(COSs).EQ.0)
     &.AND.(LenStr(COUs).EQ.0)) THEN
C       .No output files defined
         Error=6
         GOTO 999
      ENDIF
C@empa aw 2005jul15 extensions added
CC      CIFs=CIFs(1:LenStr(CIFs))//'.CIF'
CC      COFs=COFs(1:LenStr(COFs))//'.COF'
CC      COSs=COSs(1:LenStr(COSs))//'.COS'
CC      COUs=COUs(1:LenStr(COUs))//'.COU'

      RETURN

C---------------------------------
C     Errors / help syntax message
C---------------------------------

999   CRT=6
C     First write to CRT (screen), then to .CER file
      DO i=CRT,CER,CER-CRT
         IF(Error.EQ.1)THEN
            WRITE(i,*) 'ERROR:  Default input file root name not '
     &      //'given in COMIS.SET'
         ELSEIF(Error.EQ.2)THEN
            WRITE(i,*) 'ERROR:  Redundant/incorrect [file] option: '
     &      //opt(1:LenOpt)
         ELSEIF(Error.EQ.3)THEN
            WRITE(i,*) 'ERROR:  No input file (*.CIF) root name '
     &      //'given in command-line or in COMIS.SET'
         ELSEIF(Error.EQ.4)THEN
            WRITE(i,*) 'ERROR:  No default was found for '
     &      //'option ['//opt(1:LenOpt)//'] in COMIS.SET, so you must'
     &      //'specify [ofile] on the command-line.'
         ELSEIF(Error.EQ.5)THEN
            WRITE(i,*) 'ERROR:  Unrecognized command line option: '
     &      //opt(1:LenOpt)
         ELSEIF(Error.EQ.6)THEN
            WRITE(i,*) 'ERROR:  No output files specified in '
     &      //'command-line or COMIS.SET'
         ENDIF
         WRITE(i,1000)
      ENDDO      
C                                                        72 columns --->
1000  FORMAT(/' SYNTAX  : COMIS  [ifile]  [ofile]  [+o|-o]  [+s|-s]  [+u
     &f|-uf]  [+uu|-uu]'//' [ifile] - Root name of *.CIF input file (inc
     &luding path but without ext.).'/'           If omitted, or "defaul
     &t", COMIS uses the default root name'/'           given in COMIS.S
     &ET file, under heading &-COMIS, at keyword INPUT.'/' [ofile] - Roo
     &t name of output files (without extension). If omitted, [ifile]'/'
     &           is used as rootname for all output files. If "default",
     & COMIS uses'/'           the default root names given in COMIS.SET
     & file, under heading'/'           &-COMIS, using the keywords OUTP
     &UT, TABLES, USEROUTU and USEROUTF.'/' [+..|-..] Force or suppress 
     &output of files:'/'           o  = .COF output file (*)'/'        
     &   s  = .COS spreadsheet and .COH histogram files (*)'/'          
     & uf = .COU formatted user output file'/'           uu = .COU unfor
     &matted user output file'/'           (* = Forced by default unless
     & root name is blank in .SET file)'/' Examples:'/'  COMIS          
     &     - Input & output file root names given in COMIS.SET'/'  COMIS
     & case1         - Input & output files are given root name "case1"'
     &/'  COMIS case1 -o +uf  - As above, but .COU file is output, and n
     &o .COF file')

C     ! Fatal error - COMIS will abort (without silly "Fortran STOP" message)
      CerCount(3)=1
      RETURN
      END
            
