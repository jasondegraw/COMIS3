C+*********************************************************** comv-out.f
Ch**********************************************************************
C@NBI PGS 1999Aug10 - 'interval' is now in the INCLUDE file
CC    SUBROUTINE HeadOut(file,time,interval)
      SUBROUTINE HeadOut(file,time)
C***********************************************************************
C
C Purpose: write some info on the run into file
C
C created @ lbl 12nov91 akk
C
C changes:
C@NBI PGS 1999Aug05 - Reorganized whole routine to group outputs according
C@NBI                 to Oecho value.  Also tidied up general output format
C@NBI                 & detabbed
C@NBI PGS 2003Mar16 - Harmonized/unified OEcho levels (explained in COMV-DAT.FOR)
C@NBI                 This routine is now only writes stuff if OEcho >= 2
C
C Pass parameters:
C     file      INTEGER       filedesc to write to
C     time      string        time in human readable format
C     interval  INTEGER       current interval.  This variable is now
C                             in INCLUDE file comv-inp.inc
C***********************************************************************
      IMPLICIT NONE
      INCLUDE 'comv-inp.inc'
      INCLUDE 'comv-uni.inc'

C@tno jcp 1996Apr15_13:39:07 expanded 18 to 31 for weekday name
      CHARACTER*31 time
C@NBI PGS 1999Aug10 - 'interval' is now in the INCLUDE file, added 'lenstr'
CC    INTEGER interval
      INTEGER file,lenstr

C---------------------
C     Low level detail
C---------------------
C@empa aw 2001mar22 check also (file.NE.0)
      IF ((test.gt.0 .or. OEcho.ge.2).and.(file.NE.0)) THEN
C@NBI PGS 1999Aug05 - This line has been moved to routine MAIN, before
C@NBI                 call to 'TIMESTEP', so that all messages can be
C@NBI                 attributed to the correct timestep in the COF
CC       WRITE(file,'(/,80('='))')
         IF(SimuLen.GT.0) WRITE(file,*)
     &   'Date/time '//time(1:lenstr(time))//
     &   '    (Time interval',interval,'seconds)'

C------------------------
C       High level detail
C------------------------

C@NBI PGS 2001Apr11 - We don't want reams of unnecessry output from test runs unless we
C@NBI                 specifically ask for it, so "test.gt.o .or." has been removed.
         IF (OEcho.gt.15) THEN
            WRITE (file,*)
            WRITE (file,*)'INPUT FILENAME:   ',CIFs(1:lenstr(CIFs))
C@tno jcp 1996Apr26_16:03:47 suppress output of empty Vername string
            if (vername.ne.' ') write(file,*) 'CIF FILE VERSION: '
     &      ,Vername(1:lenstr(Vername))
C@tno jcp 1996Apr26_23:37:05 if modelname is empty then don't output
            if (ProName.ne.' ') then
               WRITE (file,'(/A)') ' MODEL NAME:'
               call wrt80(file,proname,wcrt)
            ENDIF
C           end if Proname <>' '
         ENDIF
      ENDIF
      return
      END


Ch**********************************************************************
        INTEGER FUNCTION iFnGt(I,divisor)
C***********************************************************************
C Purpose: return the first number >i which is a multiple of divisor
C
C Pass parameters:
C       I         INTEGER       input number
C       divisor   INTEGER       result has to be dividable by divisor
C***********************************************************************

      IMPLICIT NONE
      INCLUDE 'comv-inp.inc'
      INTEGER i,divisor,res
      REAL ri,rdivisor
      ri=i
      rdivisor=divisor
      res=INT(ri/rdivisor)
      res=res*divisor
      if (res.le.i) res=res+divisor
      iFnGt=res
      return
      end

Ch**********************************************************************
      INTEGER FUNCTION iFnGe(I,divisor)
C***********************************************************************
C Purpose: return the first number >=i which is a multiple of divisor
C
C Pass parameters:
C       I         INTEGER       input number
C       divisor   INTEGER       result has to be dividable by divisor
C***********************************************************************

      IMPLICIT NONE
      INCLUDE 'comv-inp.inc'
      INTEGER i,divisor,res
      REAL ri,rdivisor
      ri=i
      rdivisor=divisor
      res=INT(ri/rdivisor)
      res=res*divisor
      if (res.lt.i) res=res+divisor
      iFnGe=res
      return
      end


Ch**********************************************************************
C@NBI PGS 2000oct09 - Subroutine StickS is now redundant
CC        SUBROUTINE StickS(word,line,grid,len)
C***********************************************************************
C Purpose: StickS= Stick a String
C          add word to line but let it stick to the grid (begin at grid
C          intervals), and update the current length of the line
C          if len=0 the string is initialized =' '
C          Used to get unformatted but quite readable output
C Pass parameters:
C       word      string       word to be put in line
C       line      string       line with the word as result
C       grid      INTEGER      interval to stick to 1,1+interv,1*n*interv
C       len       INTEGER      length of the line
C***********************************************************************
CC
CC      IMPLICIT NONE
CC      INCLUDE 'comv-inp.inc'
CC      character*(*) word,line
CC      INTEGER grid,len
CC      INTEGER LenStr
CC      INTEGER iFnGt
CC
CC        INTEGER l2,l3
CC           if (len.eq.0) then
CC             len=1
CC             line=' '
CC           endif
CC
CC           l3=lenstr(word)
CC
CC           l2=len
CC           if (len.gt.1) l2=1+iFnGt(len-1,grid)
CC
CC           if (l2.gt.len) then
CC             call rptstr(len+1,l2-len,' ',line)
CC             len=l2
CC           end if
CC           line(len+1:)=word(1:l3)
CC           len=len+l3
CC         return
CC         end


Ch**********************************************************************
C@NBI PGS 2000oct09 - Subroutine StickI is now redundant
CC        SUBROUTINE StickI(iget,line,grid,len)
C***********************************************************************
C Purpose: StickI= Stick an INTEGER
C          add iget to line but let it stick to the grid (begin at grid
C          intervals), and update the current length of the line
C          if len=0 the string is initialized =' '
C          Used to get unformatted but quite readable output
C Pass parameters:
C
C       iget      string       word to be put in line
C       line      string       line with the word as result
C       grid      INTEGER      interval to stick to 1,1+interv,1*n*interv
C       len       INTEGER      length of the line
C***********************************************************************
CC
CC        IMPLICIT NONE
CC        INCLUDE 'comv-inp.inc'
CC        character*(*) line
CC        INTEGER iget,grid,len
CC        character*40  word
CC        INTEGER l2,l3
CC        INTEGER iFnGt
CC
CC        call intdis(iget,word,l3)
CC
CC           if (len.eq.0) then
CC             len=1
CC             line=' '
CC           endif
CC
CC           l2=len
CC           if (len.gt.1) l2=1+iFnGt(len-1,grid)
CC
CC           if (l2.gt.len) then
CC             call rptstr(len+1,l2-len,' ',line)
CC             len=l2
CC           end if
CC           line(len+1:)=word(1:l3)
CC           len=len+l3
CC         return
CC         end


Ch**********************************************************************
C@NBI PGS 2000oct09 - Subroutine StickR is now redundant
CC        SUBROUTINE StickR(rget,signif,eform,line,grid,len)
C***********************************************************************
C Purpose: StickI= Stick an INTEGER
C          add iget to line but let it stick to the grid (begin at grid
C          intervals), and update the current length of the line
C          if len=0 the string is initialized =' '
C          Used to get unformatted but quite readable output
C Pass parameters:
C
C       iget      string       word to be put in line
C       line      string       line with the word as result
C       grid      INTEGER      interval to stick to 1,1+interv,1*n*interv
C       len       INTEGER      length of the line
C***********************************************************************
CC
CC        IMPLICIT NONE
CC        INCLUDE 'comv-inp.inc'
CC        character*(*) line
CC        INTEGER signif,eform,grid,len
CC        REAL Rget
CC        character*40  word
CC        INTEGER l2,l3
CC        INTEGER iFnGt
CC
CC        call reldis(rget,signif,word,l3,eform)
CC
CC           if (len.eq.0) then
CC             len=1
CC             line=' '
CC           endif
CC
CC           l2=len
CC           if (len.gt.1) l2=1+iFnGt(len-1,grid)
CC           if (l2.gt.len) then
CC             call rptstr(len+1,l2-len,' ',line)
CC             len=l2
CC           end if
CC           line(len+1:)=word(1:l3)
CC           len=len+l3
CC         return
CC         end


Ch**********************************************************************
C@tno jcp 1996Jun18_12:26:33 added DTstring just to understand output better
CC      SUBROUTINE PolOut(file,last)
        SUBROUTINE PolOut(file,last,DTstring)
C***********************************************************************
C
C Purpose: writes the poltrans data
C
C created @ lbl 12nov91 akk
C
C Pass parameters:
C  file    INTEGER     filedesc to write to
C  last    LOGICAL     this is the last timestep,
C
C Changes:
C@empa aw 1993feb10 write the zone name not the sequencenr. of the zone
C@empa aw 1993mar09 take concentration calculated in the last timestep
C@                  this corresponds to the actual concentration
C@empa aw 1994oct31 In the last timestep we make two output loops: In the
C@                  first the concentrations at this time are output, they
C@                  are calculated in the last timestep and saved in Cold.
C@                  In the second loop the concentrations of steady state
C@                  solution are output, they are calculated in this timestep
C@                  and therefore saved in C.
C@lbl bvs 1994Feb14 LAST added to print C instead of Cold for last timestep
C                   print C instead of Cold
C@tno jcp 1996Apr24_16:42:45 2 lines Reld(3) now (4)
C@tno jcp 1996Apr24_16:47:15 added OccSo for occupant generated pollutants
C@tno jcp 1996Apr30_13:39:50 pollutant replaced by ipol
C@tno jcp 1996Jun18_11:43:47 how many occupants are in this zone?
C@tno jcp 1996Jun18_11:44:18 mention the number of occupnats in this zone
C@tno jcp 1996Jun18_11:46:55 Lstr, nOcc for output
C@tno jcp 1996Jun18_11:47:03 str for output
C@empa aw 2000mar28 LimCo1,LimCo2,MaxConc,ipolMaxconc,izonMaxconc added
C@NBI PGS 2000Aug18 - Routine modified for neater standardized output
C@NBI PGS 2000Oct08 - Can now define different I/O units for each pollutant,
C@NBI   Ounit(UnitPolCo+1) ... Ounit(UnitPolCo+5), for the 5 pollutant concentrations.
C@NBI   Ounit(UnitPolSo+1) ... Ounit(UnitPolSo+5), for the 5 pollutant source strengths.
C@NBI   Ounit(UnitPolSi+1) ... Ounit(UnitPolSi+5), for the 5 pollutant sink strengths.
C@NBI PGS 2000Oct13 - ipolMaxconc & izonMaxconc no longer needed; removed
C***********************************************************************

      IMPLICIT NONE
      INCLUDE 'comv-inp.inc'
C@empa aw 2005apr28 include comv-uni.inc
      INCLUDE 'comv-uni.inc'

      INTEGER lenstr,j,file,zone,RelDL(4),L,ipol,iext,lstr,nOcc,iocc
      REAL si,so,co,OccSo,nearzero,LimCo1,LimCo2,MaxConc
      CHARACTER RelD(4)*30,RightAlign*15,str*40,DTstring*31
      LOGICAL last,steady
C@NBI PGS 2000Aug18 - Bugfix:  MaxConc was not initialized when code was
C@NBI                 added on 2000Mar28.  Need to initialize only once.
C@NBI                 Also added SAVE statement so that error message only
C@NBI                 repeated if MaxConc increases, instead of every step
      DATA MaxConc/0.0/
      SAVE MaxConc
C-----

      nearzero = 3e-17
C     3-e17 because it seems when all input values are zero we get
C     something below this range as output
C@NBI PGS 1999May06 - Neater standardized output
CC        WRITE(file,*)
CC     &'=============================================================='//
CC     &'================='
C@empa aw 2005apr28 check OEcho
      IF (Test.GE.1 .or. OEcho.GE.1) THEN

      write(file,*)
      WRITE(file,*)'*******************************'
      write(file,*)'Pollutant transport output    *'
      WRITE(file,*)'*******************************'


C---------------------------
C     Outdoor concentrations
C---------------------------

      WRITE(file,'(/50A)')    '+--OUTDOOR-CONCENTRATION'
     &,('-',j=1,Nconc*11-12)
      WRITE(file,'(A12,5A11)')'| External |'
     &,(RightAlign(Cname(iPol),11),iPol=1,Nconc)
      WRITE(file,'(A12,5A11)')'| node-ID  |'
     &,(Rightalign(Ounit(UnitPolCo+iPol),11),iPol=1,Nconc)
      WRITE(file,'(51A)')     '+----------+',('-',j=1,Nconc*11)
      DO iext=1,nWind
         WRITE(file,'(2X,A10,5G11.3)')ExNa(iext),(ExtConc(ipol,iext)*
     &   ofact(UnitPolCo+iPol),iPol=1,outputoptions(2))
      ENDDO

C     initialize steady
C@tno jcp 1996Apr30_10:34:22 Outputoption 12= steady state output for all time s
      if (outputoptions(12).eq.1) then
         steady=.true.
         write(file,'(/A)')'Steady state solution:'
      else
         steady=.false.
      end if
      ENDIF !OEcho

70    continue

C@empa aw 2005apr28 check OEcho
      IF (Test.GE.2 .or. OEcho.GE.1) THEN

C--------------------------
C     Indoor concentrations - One table for each pollutant
C--------------------------

C@tno jcp 1996Apr30_13:35:40 loop to OutputOptions(2)
      do 90 iPol=1,outputoptions(2)
         L=LenStr(Cname(iPol))
         WRITE(file,'(/A,I1,40A)')'+--POLLUTANT-No.',iPol
     &   ,'-(',Cname(iPol)(1:L),')',('-',j=1,34-L),'  ----------'
         WRITE(file,'(A)')'| Zone-ID    No.of   Occupants      Source'//
     &   '        Sink   Concentr.'
         WRITE(file,'(A18,4A12)')'|            occup'
     &   ,RightAlign('(+)'//oUnit(UnitPolSo+iPol),12)
     &   ,RightAlign('(+)'//oUnit(UnitPolSo+iPol),12)
     &   ,RightAlign('(-)'//oUnit(UnitPolSi+iPol),12)
     &   ,RightAlign(       oUnit(UnitPolCo+iPol),12)
         WRITE(file,'(A)')'+-----------------  ----------------------'//
     &   '------------  ----------'

C        one table entry for each zone
         do 80 zone=1,nz

C       (1) Occupants in zone:
            Nocc=0
            do iocc=1,MaxO
               Nocc=Nocc+OccNum(Iocc,zone)
            ENDDO

C       (2) Occupant source strength in zone:
            occso = OccSource(iPol,zone)*ofact(UnitPolSo+iPol)

C       (3) Source strength in zone:
C           convert units & set very small values to zero
            so = source(iPol,zone)*ofact(UnitPolSo+iPol)
            if (so .le. nearzero) so = 0

C       (4) Sink strength in zone:
            si = sink(iPol,zone)*ofact(UnitPolSi+iPol)
            if (si .le. nearzero) si = 0

C       (5) Concentration in zone:
C@empa aw 1994oct31 In the last timestep we make two output loops: In the
C@        first the concentrations at this time are output, they
C@        are calculated in the last timestep and saved in Cold.
C@        In the second loop the concentrations of steady state
C@        solution are output, they are calculated in this timestep
C@        and therefore saved in C.
            if (steady) then
               co = C(iPol,zone)
            else
C              ! Take concentration calculated in the previous timestep
C              ! this corresponds to the actual concentration
               co = Cold(iPol,zone)
            endif
            if (co .le. nearzero) co = 0
C           ! Convert using ipol as index
C@NBI PGS 2000Oct08 - unit conversion for conc. is now done in RELDIS call.
CC          co=co*ofact(ipol+UnitPol-1)

C       (6) Write out:
C           ! four significant digits are enough
            CALL RELDIS(occso,4,RelD(1),RelDL(1),0)
            CALL RELDIS(so,   4,RelD(2),RelDL(2),0)
            CALL RELDIS(si,   4,RelD(3),RelDL(3),0)
            CALL RELDIS(co*ofact(UnitPolCo+iPol),4,RelD(4),RelDL(4),0)
            write(file,'(2X,A11,I5,4A12)')
     &         ZoNa(zone),Nocc,(RelD(j)(1:RelDL(j)),j=1,4)

C       (7) Warning for high concentration:
C@empa aw 2000mar28 save max concentration
C@NBI PGS 2000Oct13 - Ignore oxygen, as it gives an error at normal conc.
CC          if (co.gt.MaxConc) then
            if (co.gt.MaxConc .AND. Cname(iPol)(1:3).NE."O2 ") then
               MaxConc=co
C@empa aw 2000mar28 warnings for too high concentrations
C@NBI PGS 2000Oct13 - Bugfix moved this patch up from below...
C@NBI                 With NAG F90 compiler, the old code crashed.  Now
C@NBI                 warning is only given when concentration increases.
C@NBI               - Can now define different I/O units for each pollutant,
C@NBI                 so changed UnitPol-1 to UnitPolCo for concentration
C@NBI               - Also minor corrections to output
               LimCo1=0.025
               LimCo2=0.5
               if (MaxConc.gt.LimCo1) then
                  call intdis(iPol,RelD(1),RelDL(1))
                  call intdis(zone,RelD(2),RelDL(2))
                  if (MaxConc.gt.LimCo2) then
CC                   LimCo2=LimCo2*ofact(ipolMaxconc+UnitPol-1)
                     LimCo2=LimCo2*ofact(iPol+UnitPolCo)
                     call reldis(LimCo2,4, RelD(3),RelDL(3),0)
                     call ERROR2
     &               ('Concentration > '//RelD(3)(1:RelDL(3))//
CC   &               ' > '//Ounit(UnitPConc)(1:6)//
     &               ' '//Ounit(UnitPolCo+iPol)(1:7)//' in zone '//
     &               RelD(2)(1:RelDL(2))//' for pollutant '//
     &               RelD(1)(1:RelDL(1))//'.','Increase ventilation'//
     &               ' or decrease source strengths.',2)
                  else
CC                   LimCo1=LimCo1*ofact(ipolMaxconc+UnitPol-1)
                     LimCo1=LimCo1*ofact(iPol+UnitPolCo)
                     call reldis(LimCo1,4, RelD(3),RelDL(3),0)
C@NBI PGS 2003Mar16 - Bugfix changed from SEVERE to NOTE, otherwise showering would stop the program
                     Call ERROR
     &               ('Concentration > '//RelD(3)(1:RelDL(3))//
CC   &               ' > '//Ounit(UnitPConc)(1:6)//
     &               ' '//Ounit(UnitPolCo+iPol)(1:7)//
     &               ' in zone '//RelD(2)(1:RelDL(2))//
     &               ' for pollutant '//RelD(1)(1:RelDL(1))//'.',0)
                  end if
               endif
C@NBI PGS 2000Oct13 - (end)
            end if

80       continue
90    continue

C@NBI PGS 2000Oct13 - High concentration warning moved to top of loop.
C@NBI PGS 2000Oct13   (end)

C loop for output the steady state solution
      IF (last.and..not.steady) THEN
         write(file,*)
         write(file,*)'Steady state solution:'
         write(file,*)'----------------------'
         steady=.TRUE.
         GOTO 70
      ENDIF
      ENDIF ! check OEcho
      return
      END
      

