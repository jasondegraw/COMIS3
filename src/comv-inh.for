C+*********************************************************** comv-inh.f
Ch***********************************************************************
C
CC    SUBROUTINE filname(AFCname,NDName,SiStart)
      SUBROUTINE filname(AFCname,SiStart)
C***********************************************************************
C filname= fill the array AFCname
C filname READs the airflow component names into the string AFCname
C the component names start on position 1,11,21,31,41,....
C a component name has 2 characters
C later the file 'COMNAME' can be replaced by a hard data part in the code
C filname READs the start position of the sink terms (Filter) in Dat or LDat
C too
C sept 1989 hcp
C@tno jcp 1996Jun05_16:06:13 place for 13 AFC's including PS
C@tno jcp 1996Jun05_16:07:47 PS added
C@lbl bvs 1997Jul24 Some stupid compilers need RETURN
C@NBI PGS 2003Apr28 - New 14th component type: Thermostatic vent
C@NBI               - NdName no longer used - removed
C@NBI               - tidied up code
Ch***********************************************************************

        IMPLICIT NONE
C       Passed parameters
        CHARACTER*(*) AFCname
        INTEGER SiStart(*)
C       Internal variables
        CHARACTER a2(14)*2
        INTEGER Dum(14),I
C-----
C                  1    2    3    4    5    6    7    8    9   10
        DATA a2 /'CR','FA','DS','DF','F1','F2','F3','F4','WI','TD',
C                 11   12   13   14
     &           'PE','RF','PS','TV' /

C      The number here is the number of data elements + 2
C      the first element is always the AFCtype number.
        DATA dum/  7 , 18 , 20 ,  0 ,  6 ,  7 , 16 , 27 ,  0 ,  0 ,
     &             0 ,  0 , 10 , 12 /

        DO i=1,14
          AFCname((I-1)*10+1:(I-1)*10+2)=a2(I)
          SiStart(I)=Dum(I)
        ENDDO
        RETURN
      END


Ch***********************************************************************
        SUBROUTINE FilKey(Keys,KeysU,nDkey)
C***********************************************************************
C FilKey assigns the keywords to the string Keys
C the keywords start on position 1,11,21,31,41,....
C a keyword is 7 characters
C changes for UNITS declared in the input file
C A new Keys='PR-UNITS' which has a sub set of keywords in KeysU that follows
C the same sequence as COMIS.SET i.e. massflow, pressure, temperature, humidity
C source, sink, concentration, fanflow, meteowindspeed
Ch***********************************************************************
        IMPLICIT NONE
        include 'comv-par.inc'

        CHARACTER*(mkeys) Keys
        CHARACTER*(Ukeys) KeysU
        INTEGER nDkey

C 1993Jun9 CIF keyword no longer used (but is read and ignored)
C@empa aw 200mar22 The first keyword "CIF" shifted one position to the right.
C@empa             Now keywords can be checked with a leading and ending space,
C@empa             to be sure, not to have just a fragment of the keyword
CC      Keys(   1:10)='CIF'
        Keys(   1:10) =' CIF'
        Keys( 11:20) ='PR-IDEN'
        Keys( 21:30) ='PR-SIMU'
        Keys( 31:40) ='PR-CONT'
        Keys( 41:50) ='NET-AIR'
        Keys( 51:60) ='CR'
        Keys( 61:70) ='FA'
        Keys( 71:80) ='DS'
        Keys( 81:90) ='DF'
        Keys( 91:100)='F1'
        Keys(101:110)='F2'
        Keys(111:120)='F3'
        Keys(121:130)='F4'
        Keys(131:140)='WI'
        Keys(141:150)='TD'
        Keys(151:160)='TRANSIT'
        Keys(161:170)='NET-HVA'
        Keys(171:180)='NET-ZON'
        Keys(181:190)='NET-ZL'
        Keys(191:200)='NET-ZP'
        Keys(201:210)='NET-EXT'
        Keys(211:220)='NET-LIN'
        Keys(221:230)='SCH-MAI'
        Keys(231:240)='SCH-LIN'
        Keys(241:250)='SCH-WIN'
        Keys(251:260)='SCH-FAN'
        Keys(261:270)='SCH-TEM'
        Keys(271:280)='SCH-HUM'
        Keys(281:290)='SCH-SIN'
        Keys(291:300)='SCH-SOU'
        Keys(301:310)='SCH-OCC'
        Keys(311:320)='CP-BUIL'
        Keys(321:330)='CP-VALU'
        Keys(331:340)='ENV-BUI'
        Keys(341:350)='ENV-WIN'
        Keys(351:360)='SCH-MET'
        Keys(361:370)='POL-DES'
        Keys(371:380)='SCH-POL'
        Keys(381:390)='OCCUPAN'
        Keys(391:400)='CPR-BUI'
        Keys(401:410)='CPR-FAC'
        Keys(411:420)='CPR-WIN'
        Keys(421:430)='3D-BUIL'
        Keys(431:440)='NORM-CR'
        Keys(441:450)='SCH-MUL'
        Keys(451:460)='USER'
        Keys(461:470)='NET-ZT'
        Keys(471:480)='PR-OUTP'
C added for declaration of UserUnits in the input
        Keys(481:490)='PR-UNIT'
        Keys(491:500)='WALL-MA'
        Keys(501:510)='WALL-TY'
        Keys(511:520)='NET-WAL'
        Keys(521:530)='PE'
        Keys(531:540)='RF'
        Keys(541:550)='HISTO'
C@tno jcp 1996Jun05_6:08:57 PS added
        Keys(551:560)='PS'
C@tno jcp 1996Jun14_14:38:36 keyword EQN-WIN added  as number 57
        Keys(561:570)='EQN-WIN'
C@empa aw 2000feb01 POL-FIC added
        Keys(571:580)='POL-FIC'
C@NBI PGS 2003Apr28 - Adding 59th Keyword (14th AFC) for new termostatic flow device
        Keys(581:590)='TV'
        NdKey=59

C@tno jcp 1996Jun14_14:38:15 57 keywords
C@empa aw 2000feb01 58 keywords
CC        NdKey55
CC        NdKey=58
C@tno jcp 1996Jun14_14:38:15 57 keywords
CC        NdKey=55
C@NBI PGS 2000dec23 - Bugfix: Next line should have been commented out
CC        NdKey=57

C       WRite(CRT,*) Ndkey,Keys((nDkey-1)*10+1:NdKey*10)

C new part for the subset under Keys=PR-UNITS
C seven significant characters guiding the strings read from
C *CIF to the appropriate line in IUNIT or OUNIT
C@empa aw 2000jun27 Abbreviated and partly renamed Unitkeywords to
C@empa              make them usable in the header lines.

CC      KeysU         ='AIRLEAK'
CC      KeysU( 11: 20)='MASSFLO'
CC      KeysU( 21: 30)='PRESSUR'
CC      KeysU( 31: 40)='TEMPERA'
CC      KeysU( 41: 50)='HUMIDIT'
CC      KeysU( 51: 60)='SOURCE '
CC      KeysU( 61: 70)='SINK   '
CC      KeysU( 71: 80)='CONCENT'
CC      KeysU( 81: 90)='FAN    '
CC      KeysU( 91:100)='VELOCIT'
CC      KeysU(101:110)='ACH    '
CC      KeysU(111:120)='MEANAGE'
CC      KeysU(121:130)='ENERGY '
CCC added PROFILe (wind) sequence number ==14=UnitProf
CC      KeysU(131:140)='PROFILE'
CC      KeysU(141:150)='       '

        KeysU         ='AIRL   '
        KeysU( 11: 20)='FLOW   '
        KeysU( 21: 30)='PRES   '
        KeysU( 31: 40)='TEMP   '
        KeysU( 41: 50)='HUMI   '
        KeysU( 51: 60)='SOUR   '
        KeysU( 61: 70)='SINK   '
        KeysU( 71: 80)='CONC   '
        KeysU( 81: 90)='CFLO   '
        KeysU( 91:100)='VELO   '
        KeysU(101:110)='ACH    '
        KeysU(111:120)='MEAN   '
        KeysU(121:130)='ENER   '
C added PROFILe (wind) sequence number ==14=UnitProf
        KeysU(131:140)='PROF   '

C@empa aw 2000jun27 for compatibility with precceding versions I keep
C@empa old keywords for 2. (mass flow) and 9. (fan flow) quantity,
CC@empa but they are not documented in the UG any more.
        KeysU(141:150)='MASS   '
        KeysU(151:160)='FAN    '
        KeysU(161:170)='       '

        RETURN
        END


Ch***********************************************************************
        SUBROUTINE inh

C pass parameter # =    1        2      3       4       5
C***********************************************************************
C inh=infile hans
C Purpose: This subroutine is reading data form the input file and puts
C          the data into a lot of arrays, which are needed for the calculation.
C
C Limits :
C
C Pass parameters:
C
C  None
C
C Module : #, TG, hcp/ aug 08, 1989
C Version: 09.July 1991
C Changes:
C@empa aw 1993jun03  All error reports which should stop the program changed to
C@                   calls to INERR this routines  doesn't stop the
C@                   program actually but set the flag FInpErr.
C@                   After INH FInpErr is tested and the program stops if set.
C@empa aw 1993jul02 set default value for reference height for Cp data
C@tno jcp 1996Jun10_14:40:59 allow fro lower case characters in keywords
Ch***********************************************************************
        IMPLICIT NONE
        INCLUDE 'comv-inp.inc'
        INCLUDE 'comv-uni.inc'


C------------------------------------------------------ declarations

C global variables

C FlgEnd = Indicator that READlin has found the END on *CIF

C local variables

C LinTyp = 1=comment;2=keyword;3=header;4=name;5=error (  ,);6=data Line
C          of a dataset is in many cases the line with SINK and FILTER data.

        INTEGER LinTyp

C line   = string for the dataline

        CHARACTER Line*160,word*40

C k=  counter used in the position in the dataline (Keyword,header,comment,...)
C pDAF=record number in DAF when writing with wDAF.

        INTEGER k

C local keyword
C nkey=  keywordnumber in the array Keys
C keyword=local string containing the current keyword

        INTEGER nkey
C@empa aw1 2000mar22 Keyword*8 for a leading space
CC      CHARACTER keyword*7
        CHARACTER keyword*8

        INTEGER l1,l2
        INTEGER I,L
C@tno jcp 1996Jun07_17:55:13 str str2 for explanation
        Character*40 str, str2
        INTEGER Lstr,Lstr2


        INTEGER LENSTR

C@lbl bvs1997Dec11 declare RhoF
        REAL RhoF

C------------------------------------------------------- end declarations

C PtrL always points at the next free element in LDat (The array in which
C all AFC coefficients are.
C LayPtr points to the next free element in LayDat
C Maximum Environment wind sector areas with different terrain roughness
C pSource= pointer to the array Osource, during inpdat,
C          Osource is used for complex source definitions at NET-ZP
C          Example '34*OCC2+1.3456*OCC1+1.2345E-9'


C@tno jcp 1996Apr06_10:35:23 call a new routine for initialization in comv-ini.f
        CALL IniData
C@empa aw 1999dec08 As &-PR-SIMU and &-PR-OUTP in fact are synonyms
C                   the corresponding inputroutine 'InSimu' is called for both
C                   of these Keywords. In order to prevent doubling of some
C                   output, a flag for the first call is set here.
          FirstInSimu=.true.

C@ empa aw 1995dec20 Here I search for  &-NORM-CR first, to be sure to have the
C             correct NormCrRho for unit conversion before CS, FA, WI is read.
6        CALL READlin(Line,LinTyp,K, .True.)
       IF (FlgEnd) GOTO 7
         IF (LinTyp .NE.2) GOTO 6
         L=Lenstr(Line)
       l1=k+2
C Limit the length of the keyword
       l2=MIN(k+9,L)
       CALL GETWRD(line,l1,l2,keyword)

C test the keyword from *CIF against Keys
       nKey=index(Keys,keyword)

       IF (nKey.GT.0) THEN
C we have an existing keyword
         nKey=(nKey-1)/10+1
           IF (nKey.EQ.44)THEN
           call inpdat(nkey, line, k, LinTyp)
             GOTO 7
           ENDIF
         ENDIF
         GOTO 6
7        CONTINUE
       NormCrRho=RhoF(NormCrPb,NormCrT,NormCrXh)
C@NBI PGS 2003May05 - optional echo of precalculations
       IF (pecho.ge.1) THEN
         WRITE (crt,*) 'NormCrRho=',NormCrRho
       ENDIF

C@empa aw 1995dec20 end

C rewind *CIF file
        REWIND(CIF)
C@empa aw 2005jun07 reset also nCifLine
        nCifLine=0        

      IF (test.ge.1.or.iecho.ge.1) THEN
            Write (CRT,*)' '
            Write (CRT,*)'*******************************'
            Write (CRT,*)'Reading Input File            *'
            Write (CRT,*)'*******************************'
            Write (CRT,*)' '
C@NBI PGS 1999May06 - So what's the filename?..
C@NBI PGS 2000Jul21 - .. this line now moved to COMV-INP.FOR
CC        WRITE (CRT,*)'INPUT FILENAME:   ',CIFs(1:lenstr(CIFs))
      ENDIF

C **************************************************************************
        FlgEnd=.FALSE.

C@tno jcp 1996May30_17:34:11 read through headers
CC        CALL READlin(Line,LinTyp,K, .True.)
5        CALL READlin(Line,LinTyp,K, .False.)
C                     |    |      |   |
C                     |    |      |   don't skip headers
C                     |    |      pos of first non blank in Line
C                     |    Line type 1,..,6 - comment, ... ,data
C                     The string containing the line from the file CIF
C@tno jcp 1996May30_17:48:46 loop if a header has been found to track the Header
         if (LinTyp.eq.Thead) goto 5

C
C  Loop here reading the CIF file
C

10      CONTINUE

C Check if end of file

        IF (FlgEnd) GOTO 900

        L=Lenstr(Line)

        IF ( test.ge.1 .AND. iecho.ge.5 ) THEN
            WRITE(CRT,'(A80)')Line
        ENDIF

C LinTyp  1=comment;2=keyword;3=header;4=name;5=error (  ,);6=data Line

C GOTO the parts to handle the different types of lines and RETURN to 10 to
C process the next line

C We only want to see keywords here.  If we see a comment or header we ignore it

C@empa aw 1999dec15 LinTyp continuation line LinTyp=7 introduced
CC        GOTO(100,200,100,300,300,300) LinTyp
        GOTO(100,200,100,300,300,300,400) LinTyp
C            |   |   |   |   |   |
C            |   |   |   |   |   data
C            |   |   |   |   error
C            |   |   |   name
C            |   |   header (not here since we read with ReadLin(..,.False.))
C            |   keyword
C            comment (no longer RETURNed here, they are skipped in READlin)

C loop to 10 to READ the next line******************************************
        GOTO 10

C********** EVALUATE THE 6 LINE TYPES *********

C*********************************************************
C       Comment and Header
C*********************************************************

100     CONTINUE

        GOTO 10

C*********************************************************
C       Keyword
C*********************************************************

200     CONTINUE

C@empa aw 2000mar22 check with a leading space to be sure not to have just a
C@empa              fragment of the keyword
CC      l1=k+2
        l1=k+1
C Limit the length of the keyword
      l2=MIN(k+9,L)
      variab='Limit the length of the Keyword'
        CALL GETWRD(line,l1,l2,keyword)
      keyword(1:1)=' '
C test the keyword from *CIF against Keys
C@tno jcp 1996Jun10_14:38:22 allow keywords to be mistyped as lower characters
      CALL UPPERC(KeyWord)      
        nKey=index(Keys,keyword)

        IF (nKey.GT.0) THEN
C we have an existing keyword
C@empa aw 2000mar22
CC         nKey=(nKey-1)/10+1
CC         if (keyword(1:3) .eq. 'SCH' .and.
           nKey=nKey/10+1
C@NBI PGS 2000dec23 - Bugfix.  The datasection for this schedule may be
C@NBI                 empty (only keyword is in CIF file), in which case
C@NBI                 one shouldn't write anything in the DAF. This code
C@NBI                 block is thus moved to subroutine INPDAT, after we
C@NBI                 find out that the datasection is not empty.
CC         if (keyword(2:4) .eq. 'SCH' .and.
CCC@NBI PGS 2000dec22 - Bugfix: Wrong position in string KEYWORD
CCCC   $         keyword(1:7) .ne. 'SCH-MUL') then
CC     &         keyword(2:8) .ne. 'SCH-MUL') then
CCC         only the schedule data goes into the DAF
CC               pKeyRec(nkey)=pDAF
CC               buf((MOD(pDAF-1,mBuf)+1))=LINE
CC               IF (MOD(pDAF,mBuf).EQ.0) CALL wDAF(pDAF,mbuf,buf)
CC               pDAF=pDAF+1
CCC              WRITE(*,*) 'keyword',keyword,'nkey',nkey,'record',pDAF
CCC              WRITE(*,'(A80)')line
CC          endif
CCC@NBI PGS 2000dec23   (end of moved code)
C@NBI PGS 2000dec22 - CALL to INPDAT moved here from below
C      ! Now call the appropriate input routine for this keyword type
            call inpdat(nkey, line, k, LinTyp)
        ELSE
C this should never happen, but typing errors in CIF while editing may occur
C@NBI PGS 2000Oct13 - More legible
C@NBI PGS 2003Apr30 - Changed from SEVERE to FATAL to prevent infinite loop.
            Variab='looking for a keyword'
            CALL INERR('Keyword not valid in following line:',
     &          line(1:lenstr(Line))//';This invalid keyword '//
     &   'might cause other (false) error messages that '//
     &   'will be solved if you correct this keyword.',.TRUE.,3)
        ENDIF

C@NBI PGS 2000dec23 - CALL to INPDAT moved up to inside IF/ENDIF
CCC     Now call the appropriate input routine for this keyword type
CC      call inpdat(nkey, line, k, LinTyp)

C At this point 'line' contains the NEXT input line from the CIF file

        GOTO 10

C*********************************************************
C       Name, Data, Error
C*********************************************************

300     CONTINUE

        CALL INERR('Expected a Keyword got an Unrecognized word in '//
     &  'line: ',LINE,.FALSE.,2)
C read the next line otherwise we loop endless here with this erroneous line.
        CALL READlin(Line,LinTyp,K, .True.)
        GOTO 10

C@empa aw 1999dec15 LinTyp continuation line LinTyp=7 introduced
400     CONTINUE
        variab='Continuation line'
        CALL INERR('Continuation line is not allowed here! ',
     &  '',.true.,1)
C read the next line otherwise we loop endless here with this erroneous line.
        CALL READlin(Line,LinTyp,K, .True.)
        GOTO 10


C*********************************************************
C       END OF FILE
C*********************************************************

C end of file found during reading CIF file

900     CALL wDAF(pDAF-1,mbuf,buf)
C update the pointers
C@empa aw 2000nov28 keep the actual pdaf in pdaf1
      pdaf1=pdaf
        Ntyp=NUsrAfc
        MLDat=ptrl-1
        MLayDat=Layptr1-1

C make the Wind direction array cyclic to both sides

        EnvDir(1)=EnvDir(MenvDir+1)-360.
C the last element is copied from the first element READ
        EnvDir(MEnvDir+2)=EnvDir(2)+360.

        IF (NCpDir.GT.1) NCpDir=NCpDir-1

C make the CpDir array cyclic
        CpDir(1)=CpDir(NCpDir+1)-360.
        CpDir(NCpDir+2)=CpDir(2)+360.
        DO 901 i=1,nl
          mp(i)=1.0
901     CONTINUE

        IF (test.ge.1.or.iecho.ge.5) THEN
          WRITE(CRT,*) ' '
          WRITE(CRT,*) 'Ntyp  .  .  .   N AFC components =',Ntyp
          WRITE(CRT,*) 'Nz                       N Zones =',Nz
          WRITE(CRT,*) 'Nl .  .  .  .  .  .  .   N Links =',Nl
          WRITE(CRT,*) 'Nwind            N windpressures =',Nwind
          WRITE(CRT,*) 'Nspec .  .  .  N fixed pressures =',Nspec
          WRITE(CRT,*) 'Nzl      no of zones with layers =',Nzl
          WRITE(CRT,*) 'Nzp  no of zones with poll input =',Nzp
          WRITE(CRT,*) 'MLayDat     N dataelementsLayDat =',MLayDat
          WRITE(CRT,*) 'Nconc .  Number of possible conc =',Nconc
          WRITE(CRT,*) 'MLDat         N elements in Ldat =',MLDat
          WRITE(CRT,*) 'NCpDir   .  N Wind Cp directions =',NCpDir
          WRITE(CRT,*) 'MEnvDir   N ENVironm. DIRections =',MEnvDir
        ENDIF

C       All pollutants, which are used in "&-NET-ZP zone pollutants"
C       MUST be defined in "POL-DEScription"!!!
C
C       nconcPolDes     := number of pollutants in POL-DES
C       nconc           := number of pollutants in &-NET-ZP
C       If everything is o.k., nconc has the value for the number
C       of pollutants.
C
C       If nconcPolDes is less than nconc, then there are more pollutants
C       used as defined.
C@tno jcp 1996May31_16:45:24 outputoptions(2)=1 must be >0
        if (outputoptions(2).gt.0 .AND. nconcPolDes.LT.nconc) then
          call intdis(nconc,str,Lstr)
          call intdis(nconcPoldes,str2,Lstr2)
            call inerr('&-NET-ZP: There are '//str(1:lstr)//
     &  ' pollutants used, but only '//Str2(1:lstr2)//
     & ' are defined at &-POL-DES  !',' ',
     &  .FALSE.,2)
        endif
C       Data under &-WAL-LINk has been seen (Walls have been linked)
C       nconcWaTy is less than nconc, then there are more pollutants
C       used in Net-ZP than absorber data lines defined for some of
C       the WallTypes.

C I would like to keep track of only those WallTypes that are used under
C &-WAL-LIN (WallTypes not used, are allowed to have less datalines)
C So at the moment this warning could be false

        if (NetWaLinF .gt. 0) then
C@tno jcp 1996May31_16:45:24 outputoptions(2)=1 must be >0
CC          if (outputoptions(2).EQ.1 .AND. nconcWaTy.LT.nconc) then
          if (outputoptions(2).gt.0 .AND. nconcWaTy.LT.nconc) then
            CALL INTDIS(NConc,word,L2)
            call INERR('&-WAL-MAterial. Missing Adsorber data lines.'//
     &          ' I need '//word(1:L2)//'(=Npollutants) .',line(1:l),
     &          .false.,2)
            write(cof,*) ' Look at keyword &-POL-DEScription 37 and ',
     &          '&-NET-ZP 20. The number of pollu-'
            write(cof,*) ' tants there should not be larger than the ',
     &          'number of adsorber data lines under'
            write(cof,*) ' any of the wall types of &-WALL-TYpes ',
     &          '(linked at &-WAL-LINks or not). '
            write(cof,*) ' Nconc at &-NET-ZP=',NConc,
     &          ' NconcPoldes at &-POL-DES=',NconcPoldes
            write(cof,*) ' Been in &-WAL-LIN=',NetWaLinF,
     &          ' NconcWaTy=',NconcWaTy
            write (cof,*) ' '
          endif
        endif



C       If no pollutants are used, but the keyword "POL:lutant" is used
C       in the output option part, the pollutant transport module should
C       not be called.
C@tno jcp 1996May31_16:45:24 outputoptions(2)=1 must be >0
CC        if( outputoptions(2).EQ.1 .AND. nconc.EQ.0 ) then
        if( outputoptions(2).gt.0 .AND. nconc.EQ.0 ) then
            call inerr('&-PR-OUTPut:  Keyword: POLlutant:',
     &        'Can not run the pollutant transport module, '//
     &        'because there is no data for it !',.FALSE.,1)
            outputoptions(2)=0
        endif
        RETURN

        END

Ch***********************************************************************
        SUBROUTINE wDAF(pDAF,mbuf,buf)
C***********************************************************************
C wDAF= WRITE Direct Acces File
C DAF = UNIT number for the direct acces file
C PDAF= line number of the contents
C mBuf= Maximum length (number of lines) in the string buf
C buf = string, contains upto mbuf lines of data
C wDAF is Called as soon as the counter pDAF goes over the next mbuf lines
C therefore the beginning record number to WRITE is the first number dividable
C by mbuf that is lower than pDAF (rec1) . rec1 has the value that pDAF had
C when the first line of buf was filled( 1, 201, 401,....)
C sept 1989 hcp
Ch***********************************************************************

        IMPLICIT NONE
        INCLUDE 'comv-uni.inc'

        INTEGER pDAF,mbuf
        CHARACTER*(*) buf(mBuf)
C local parameters
        INTEGER nrec,rec1,rec

        nrec=MOD(pDAF-1,mbuf)+1
C calculate the recordnumber belonging to the first line in buf
        rec1=pDAF-nrec+1
c        write(*,*) 'from',rec1,' to',pdaf


        DO 10 rec=rec1,pDAF
          WRITE(DAF,REC=rec) buf(MOD(rec-1,mbuf)+1)
c         WRITE(*,*)'rec=',rec,'line=',buf(MOD(rec-1,mbuf)+1)
10      CONTINUE
c        write(*,*) 'done from',rec1,' to',pdaf
c        call ho(' ',' ')

        RETURN
        END

C routine re (Read Daf file) moved to comv-deb.f


Ch***********************************************************************
        SUBROUTINE READlin(Line,LinTyp,K,Skip)
C                     |    |      | |
C                     |    |      | Skip headers if TRUE
C                     |    |      Pos of first non blank in Line
C@tno jcp 1996Apr08_10:21:51 1..7 line types (TCont, continuation line added)
C                     |    Line type (1,...,7)
C                     Line from CIF
C***********************************************************************
C READlin=read a line from CIF
C
C Purpose:
C READlin reads the next non blank non comment line from CIF and sets FlgEnd
C if the end of file is found (in that case Line does not contain the new line)
C and determines the type of the line (1,....,7).
C
C Comments are ignored
C If Skip is true then headers are ignored
C
C       Line Type       Description
C           1           Comment
C           2           Keyword
C           3           Header
C           4           Name
C           5           Error
C           6           Data
C           7           Continuation Data
C
C Module : #, TG, hcp/ aug 08, 1989
C Changes: sep 6 1989 Blanks and tabs are processed equal now
C
C Limits :
C
C Pass parameters:
C
C IO # Name    unit              description
C sfile(1:80)     filename of the serial file *CIF
C
Ch***********************************************************************

        IMPLICIT NONE
        INCLUDE 'comv-inp.inc'
        INCLUDE 'comv-uni.inc'
        INTEGER LenStr
        INTEGER IntCon

        CHARACTER*(*) Line
        INTEGER LinTyp,l,i,j,K
        LOGICAL Skip
C@tno jcp 1996May30_17:42:30 ipos,i2pos,iheader,errflg added
        INTEGER LL,ipos,i2pos,iheader,errflg
          INTEGER COMpos, COMposB
C@tno jcp 1996Apr08_10:33:14 Cha2 added
C@lbl bvs 1999Mar11 increased length of name and flname from 30 to 160
C               for long filename paths
        CHARACTER Cha1*1,Cha2*1, name*160, flname*160

        LL=len(line)
        CLines10=1

10      READ(CIF,'(A)',END=900,ERR=999) Line
C@tno jcp 1996Jul08_17:16:09 increment nCifLine the line number in Cif
        nCifLine=nCifLine+1

C
C Keep track of the latest 10 lines
C
        if (Keep10) then
            Plines10=MOD(Plines10,10)+1
            Lines10(Plines10)=Line
        else
C Or just the latest line (during runtime after reading input file)
            call KeepLine(Line)
        endif
C@empa aw 2005apr22 cut off the line at ";" or "#" the rest is comment 
      COMpos=MIN(INDEX(Line,'#'),INDEX(Line,';'))
      IF (INDEX(Line,'#')==0) THEN 
          COMpos=LL 
        ELSE 
          COMpos=INDEX(Line,'#')
      ENDIF
      IF (INDEX(Line,';')==0) THEN 
          COMposB=LL 
        ELSE 
          COMposB=INDEX(Line,';')
      ENDIF
      COMpos=MIN(COMpos,COMposB)-1
        Line=Line(1:COMpos)
          
C test if the line starts with a blank
        Cha1=Line(1:1)
        IF (Cha1.EQ.' ' .OR. Cha1.EQ.CHAR(9)) THEN
C test if the whole line is empty, and if so, read the next line
          IF (line.EQ.' ') GOTO 10

C loop 20 until K points to the first nonblank
          K=1
20        K=K+1
C Test if nothing else than tabs or ' ' are in the line,
C                   if so, read the next line
          IF(K.GT.LL) GOTO 10
          Cha1=Line(k:k)
          IF (Cha1.EQ.' ' .OR. Cha1.EQ.CHAR(9)) GOTO 20
        ELSE
C the line starts with a nonblank
          K=1
        ENDIF
C GetWRD wants to have K point to the space BEFORE the next word, therefore
C here we subtract 1. K points just before the word!!!!
        K=K-1
C cha1 is the first non blank character
        cha1=line(k+1:k+1)

C look what the character cha1 means by indexing the pointer LinTyp
C                     1234567
C@tno jcp 1996May17_12:35:49 allow ; as equivalence for #
CC        LinTyp=index('#&|_*,',cha1)
        LinTyp=index('#;&|_*,',cha1)
C
C                     #; & | _ * ,
C                     |  | | | | |
C                     |  | | | | error: a data line cannot start with a comma
C                     |  | | | name
C                     |  | \ /
C                     |  |  header (both | and _ )
C                     |  keyword
C                     comment

C@tno jcp 1996May17_12:37:07 new part for ;
C correct for the fact that comment has two possible characters
C ( the # and the ; therefore occupies two positions in the above index)

        IF (Lintyp.GT.1) LinTyp=LinTyp-1

C correct for the fact that the header has two possible characters
C ( the _ and the | therefore occupies two positions in the above index)

        IF (Lintyp.GT.3) LinTyp=LinTyp-1

        if (LinTyp.eq.2) then
C this may be a keyword (&-) or continuation line (& )
C@tno jcp 1996Apr08_10:25:49 look at the second character
          cha2=line(k+2:k+2)
C if cha2=' ' then this is a continuation line (TCont=7)
          if (cha2.eq.' ') then
            Lintyp=7
c            write(*,*) 'line=',line
c            write(*,*) 'type=',LinTyp
c            write(*,*) ''
c            pause
          end if
        end if
c        write(cof,*) 'type=',LinType


C Skip any header lines if the caller requests that
        if (Skip .and. LinTyp .eq. THEAD) goto 10
C@tno jcp 1996May30_11:17:39 ReadLin: nothing happens with the header-nrs now?
C new part reintroduced
C keep the HeaderNr of the current header, so InSchMet and InCpVal can see if
C they have to read a name or values and not loose the first data line
C in case no name has been put up.
        if (LinTyp.eq.Thead) then
c          write(*,*) line
c          call ho('in ReadLin, we got a header','')
          ipos=index(line,' ___')
          if (ipos.ne.0 .and. ipos.lt.3) then
C here starts a new header
            headernr=0
c            call ho(' ___ found headerNr=0','')
          else
C continuation of the header
            ipos=index(Line,'|')
            if (ipos.ne.0 .and. ipos.lt.5) then
C there is a | at the beginning of the line
c              call ho('| at begin','')
              i2pos=index(Line,'.')
              if (i2pos.ne.0 .and. (i2pos-ipos).le.3
     &         .and.(i2pos-ipos).ge.2) then
c                call ho('. after the |','')
C there is '|?.' or '|??.'  ?? could be a number
c                write(*,*) 'Look at ',Line(ipos+1:I2pos-1)
C@empa aw 2000oct30 skip leading spaces
                ipos =ipos+1
                  call SkipSpc(line,ipos)
CC                IHeader=intcon(Line(ipos+1:I2pos-1),ErrFlg)
                IHeader=intcon(Line(ipos:I2pos-1),ErrFlg)
                if (ErrFlg.eq.0) then
                  HeaderNr=Iheader
c                   write(cof,*) 'header=',headernr
c                  call ho('','')
                end if
              end if
C end if '.' after '|'
            end if
C end if '|'

          end if
C end if ' ___'

        end if
C end if (LinTyp.eq.Thead)


C test if we have a file assignment in the input file

C@tno jcp 1996Jun27_16:04:47 allow F: and f: to signal a file
        IF ((line(k+1:k+2).EQ.'F:'.or.line(k+1:k+2).EQ.'f:')
     &      .AND. LinTyp.EQ.0) THEN
           i=k
           k=k+2
           line(k:k) = ' '
           j=k
           l=lenstr(line)
             variab=' reading the word before a filename'
           CALL GETWS(line,k,l,name,' ')
             variab=' reading a filename: flname'
           CALL GETWS(line,k,l,flname,' ')
           IF (flname.NE.' ') LinTyp=TDATA
           line(j:j)=':'
           k=i
        ENDIF

C LinTyp=0 if the character is different. That should be a dataline (we hope)
        IF (Lintyp.EQ.0) LinTyp=TDATA

C IF the line is a comment line THEN loop to 10 to READ the next line
        IF (LinTyp.EQ.TCOM) GOTO 10

        RETURN

900     FlgEnd=.TRUE.
        RETURN

999     CALL INERR('I/O - Error when reading CIF (READlin)',
     &  ' ',.FALSE.,3)

        RETURN
        END


Ch***********************************************************************
      SUBROUTINE INERR(MSG,Line,W10linF,SEVR)
C***********************************************************************
C  Purpose:  Error message writer for input file reading.
C            This is the only routine, which should be used to report errors
C            during input file reading.
C          - Writes a header line if it is the first input error
C          - Writes severity level, a message and a line with the error
C            to CRT and CER
C          - Writes 10 lines from input file if required.
C          - Levels of severity (SEVR):
C            0 : Note only
C            1 : Warning (do not need to stop)
C            2 : Sets the input errorflag --> stop after input reading
C            3 : Stops programm immediatly
C
C  Pass parameters:
C  MSG       Error Message
C  Line      Line from input file containing an error
C  FInpErr   set to 1 as soon as first error occurs
C  W10linF   Flag whether w10lin has to be called
C  SEVR      error severity code
C
C  Uses Common Block Parameter
C  FInpErr   set to 1 as soon as first error occurs if SEVR is 2
C
C  Module:  VD 14.Mar.91
C  Version: PGS 1999May06  This is a revamped version of inerr
C  Changes:
C@tno jcp 1996Apr05_16:27:14 changed errormessages to be able to find them in CO
C@tno jcp 1996Apr05_16:28:19 increased the length of the error message
C@tno jcp 1996Apr05_16:29:03 count the errors
C@NBI PGS 1999May06 - Tidied up whole subroutine.  Created a loop
C@NBI                 that directs output to CER & CRT in turn
C@NBI               - The line is now above message, not below
C@NBI PGS 2000Jul16 - Deletes DAF file when program stops due to fatal error
C@empa aw 2001mar23 Message slightly changed; in COMIS TRNSYS use 
C@empa              message can now be better identified as COMIS message  
C@empa aw 2001sep05 For COMIS in TRNSYS I need TIME and INFO in error routines 
C@empa              but I don't want to pass them in each call 
C@empa              --> new common block /TIMINFO/
Ch**********************************************************************
        IMPLICIT NONE
        INCLUDE 'comv-uni.inc'
        INCLUDE 'comv-inp.inc'
        INTEGER I,LenStr,SEVR,File,lstr
        CHARACTER*(*) Line,MSG
        CHARACTER*35  SEVER(0:3)
        Character*40 str
        LOGICAL w10LINF
C@empa aw 2001mar23
        INTEGER STP 
CTC
        DIMENSION INFOS(15)
          INTEGER INFOS
        REAL TIMES

        COMMON /TIMINFO/ TIMES, INFOS
       
C@empa aw 2001mar23 Message slightly changed
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
          IF (FInpErr.EQ.0 .AND. SEVR.GE.2) THEN
            WRITE(File,*)
C@empa aw 2001mar23 Message slightly changed
            WRITE(File,*)'--------------------- ERRORS IN COMIS INPUT'
     &      //' DATA ----------------'
          ELSE
            WRITE(File,*)'------------------------------------------'
     &      //'----------------------'
          ENDIF
          I=MAX(0,MIN(3,SEVR))
C@empa aw 2001sep05 Message only for COMIS TRNSYS
CTC
          IF ((INFOS(1).NE.0).and.(INFOS(2).NE.0))THEN
            WRITE(FILE,2005) INFOS(1),INFOS(2),TIMES
2005        FORMAT(//1X,'***** WARNING ***** UNIT',I3,' TYPE',I3,
     .      ', TIME = ',1PE11.3)
          ENDIF

          WRITE(File,*) SEVER(I)
          IF (MSG.NE.' ')  CALL wrt80(File,MSG,WCrt)
          IF (Line.NE.' ') CALL wrt80(File,Line,WCrt)
          IF (W10LINF) THEN
            CALL intdis(nCifLine,str,lstr)
            CALL wrt80(File,'The current line number in the input file '
C@NBI PGS 2000Jul16 - Removed space
     &      //Cifs(1:lenstr(Cifs))//' is '//str(1:lstr)//'.',WCrt)
            CALL W10LIN(File)
          ENDIF
C@NBI PGS 2000dec23 - Next line prevents infinite loop when CER = CRT
C@empa aw 2001mar23 See Bugfix above for this case
CC          IF(CER.EQ.CRT) GOTO 11
        ENDDO
CC11      CONTINUE        

C-------------------
C       Post process
C-------------------

        IF(SEVR.GE.2) FInpErr=1
        CERCount(Sevr)=CERCount(Sevr)+1
        IF(SEVR.GE.3) THEN
          CLOSE(DAF,STATUS='DELETE',ERR=9000)
9000      STOP
        ENDIF
        RETURN
      END


Ch***********************************************************************
        SUBROUTINE InzReord
C***********************************************************************
C
C Module :  VD 1992 Oct 30
C Version:
C Changes:
C@empa aw 1995jan11 check source in zero volume zones
C@empa aw 1995jan11 check input of H/D/W of this zone
C@empa aw 1995jan11 check layered zones whether concentration profile goes
C@                  into negative
C@empa aw 1995jan11 check layered zones whether concentration or humidity
C@                  profile goes into negative
C
C Description:
C This routine re-orders the input data arrays for the zone layers and the
C zone pollutants respectively. This data have been read in routine inh
C with unknown zone sequencing. Once the zone name tree is established
C using namesort, the arrays must be reordered according to the REAL
C zone sequencing.
C
Ch***********************************************************************
        IMPLICIT NONE
        INCLUDE 'comv-inp.inc'

        INTEGER LenStr
C Common parameters used in this routine:
C Nz,Nzl,Nzp,ZLNa,ZPNa,Layptr,C, ZoTree,ZoTreeN,ZoLarg, ZoSmal
C MaxZ, MaxC

C Local parameters:
C copies of the common arrays:
        INTEGER LayptrXX(2,MaxZ)
C different indexing for CCX
        REAL CXX(MaxC,-MaxW:MaxZ+MaxWC),
     &          SourceXX(MaxC,MaxZ),SinkXX(MaxC,MaxZ)

C@empa aw 2006apr04 TrSoISXX,TrSiISXX
        REAL TrSoISXX(MaxC,MaxZ),TrSiISXX(MaxC,MaxZ)
C Others
        INTEGER ZNo,key,LStr,i,j
        REAL Rho2,TD,XD,CD
C@empa aw 1997sep18 Rho1,Dp as DOUBLE
        DOUBLE PRECISION Rho1,Dp

C Copy data in common array into temporary arrays:
        DO 10 I=1,Nzl
           DO 20 J=1,2
              LayptrXX(J,I)=Layptr(J,I)
20         CONTINUE
10      CONTINUE

        DO 30 I=1,Nzp
           DO 40 J=1,MAXC
              CXX(J,I)=C(J,I)
              SourceXX(J,I)=Source(J,I)
              SinkXX(J,I)=Sink(J,I)
C@empa aw 2006apr04 TRNSYS source and sink schedules have also to be reordered
              TrSoISXX(J,I)=TrSoIS(J,I)
              TrSiISXX(J,I)=TrSiIS(J,I)
40         CONTINUE
30      CONTINUE

C reset Layptr and C to zero

        DO 50 I=1,MaxZ
           DO 60 J=1,2
              Layptr(J,I)=0
60         CONTINUE
           DO 70 J=1,MaxC
              C(J,I)=0.0
              Source(J,I)=0.0
              Sink(J,I)=0.0
C@empa aw 2006apr04 TRNSYS source and sink schedules have also to be reordered
              TrSoIS(J,I)=0
              TrSiIS(J,I)=0
70         CONTINUE
50      CONTINUE


C reorder Layptr

        DO 100 I=1,Nzl
          CALL LOOKNAM (Nz,ZLNa(I),ZoTree,ZoTreeN,Zno,ZoLarg,ZoSmal,key)
          IF (key.EQ.1) THEN
             LStr=LENSTR(ZLNa(i))
             CALL INERR('&-NET-ZL: ',
     &       'Zone Name: '//ZLNa(I)(1:Lstr)//' does not exist in '//
     &       '&-NET-ZON',.FALSE.,2)

C check input of H/D/W of this zone
          ELSE IF ((Hz(Zno)+Dz(Zno)+Wz(Zno)).EQ.0.) THEN
             LStr=LenStr(ZoNa(Zno))
             CALL INERR ('&-NET-ZONE: '//ZoNa(Zno)(1:LStr)//
     &       ': H/D/W of the zone must be given for layered zone !',
     &       ' ',.false.,2)
          ELSE
            DO 110 J=1,2
              Layptr(J,Zno)=LayptrXX(J,I)
110         CONTINUE
          ENDIF
100     CONTINUE


C reorder C

        DO 120 I=1,Nzp
          CALL LOOKNAM (Nz,ZPNa(I),ZoTree,ZoTreeN,Zno,ZoLarg,ZoSmal,key)
          LStr=LENSTR(ZPNa(I))
          IF (key.EQ.1) THEN
             CALL INERR('&-NET-ZP: ',
     &       'Zone Name: '//ZPNa(I)(1:LStr)//' does not exist in '//
     &       '&-NET-ZON',.FALSE.,2)
          ELSE
C@empa aw 2000nov30 Take only nconc pollutants(there could be more defined 
C                   in &-NET-ZP, but it might be limited by setting POLLUTANT in
C                   &-PR-SIMU
CC            DO 130 J=1,MaxC
            DO 130 J=1,nconc
              C(J,ZNo)=CXX(J,I)
              Source(J,ZNo)=SourceXX(J,I)
              Sink(J,ZNo)=SinkXX(J,I)
C@empa aw 2006apr04 TRNSYS source and sink schedules have also to be reordered
              TrSoIS(J,ZNo)=TrSoISXX(J,I)
              TrSiIS(J,ZNo)=TrSiISXX(J,I)
C check source in zero volume zones
              IF ((Vz(Zno).EQ.0.).AND.(Source(J,Zno).NE.0))THEN
                 CALL INERR('&-NET-ZP:  *'
     &          //ZPNa(I)(1:LStr)//' is a zone with no volume, '//
     &          'no source may be defined','in this zone !',.FALSE.,2)
             ENDIF
130         CONTINUE
          ENDIF

120     CONTINUE
C check layered zones whether concentration or humidity profile goes into
C negative
        DO 140 I=1 ,Nz
           IF (Layptr(1,i).NE.0) THEN
             TD=20.
             XD=Xhz(i)
             CD=C(1,i)
             Rho1=1.2
             CALL LClimb(9.81,Rho1,Hz(i),TD,XD,CD,
     &         LayPtr,LayDat,Dp,i,
     &         0.0D0,101325.,MM,Rho2,ZoNa(i))
           ENDIF
140     CONTINUE

        RETURN
        END
Ch***********************************************************************
        SUBROUTINE CheckLink
C***********************************************************************
C
C Module :  AW 1993jun25
C
C Checks some inputs from NETLinks:
C - Checks the absolute heights (zone height plus link height) on From and
C   To side of the links and gives a message if the difference between the
C   two is not zero.
C - Check opening angle resp. opening factor of windows
C
C
C Changes:
C@empa aw1994oct27 check opening angle/factor of windows
C@tno jcp 1996Jun10_12:35:23 more explanation about heights Hfrom Hto deltaH
Ch***********************************************************************

        IMPLICIT NONE
        INCLUDE 'comv-inp.inc'
        REAL H
        INTEGER LL,I,From,To,izone
C@tno jcp 1996Jun07_17:29:29 str..7, Hfrom-to for explanation 3 lines
        CHARACTER*40 str,str2,str3,str4,str5,str6,str7
        INTEGER Lstr,Lstr2,Lstr3,Lstr4,Lstr5,Lstr6,Lstr7
        REAL Hfrom,Hto
        INTEGER LenStr,nrWin

C check from- to height difference
        DO 10 I=1,nl
            From=FromTo(1,I)
            To=FromTo(2,I)
            LL=Lstat(I)
            Hfrom=zl(1,i)
            Hto  =zl(2,i)
            H=(Zl(2,I)-Zl(1,I))
            IF ((LL.eq.0 .OR. LL.EQ.3 .OR. LL.EQ.6).AND.From.NE.0) THEN
                H=H-zz(from)
                Hfrom=Hfrom+zz(from)
            ENDIF
            IF ((LL.LT.3).AND.To.NE.0) THEN
                H=H+Zz(To)
                Hto=Hto+zz(to)
            ENDIF
C check for NEAR 0 (2cm), not EXACTLY 0.0
            If (abs(H).GT.0.02)THEN
C@tno jcp 1996Jun07_17:26:49 extended explanation
              str=LiNa(I)
              Lstr=lenstr(str)
              str=str//' '
              Lstr=Lstr+1
              str2=LiTyNa(I)
              Lstr2=lenstr(str2)
              str3=FromToS(1,I)
              Lstr3=lenstr(str3)
              call reldis(Hfrom,5,str4,lstr4,0)
              str5=FromToS(2,I)
              Lstr5=lenstr(str5)
              call reldis(Hto,5,str6,lstr6,0)
              call reldis(Hto-Hfrom,5,str7,lstr7,0)
C@NBI PGS 1999Aug18 - Tried to make message tad more understandable
CC                CALL INERR('&-NET-LINks:  LinkID: '//str(1:lstr)//
CC     &             str2(1:lstr2)//' From('//str3(1:Lstr3)//')='//
CC     &             str4(1:lstr5)//'m To('//str5(1:Lstr5)//')='//
CC     &             str6(1:lstr6)//'m deltaH='//Str7(1:lstr7)//'m',
CC     &            'Zone+LinkHeight at From, To side are '//
CC     &            'not equal!'//
CC     &            ' Like a vertical shaft. Yet OK ?',
              CALL ERROR('&-NET-LINks:  LinkID='//str(1:lstr)//' Type='
     &        //str2(1:lstr2)//'  FROM(zone '//str3(1:Lstr3)//')='//
     &        str4(1:lstr5)//'m  TO(zone '//str5(1:Lstr5)//')='//
     &        str6(1:lstr6)//'m  deltaH='//Str7(1:lstr7)//'m.  '//
     &        'ZoneHeight+LinkHeight is different for sides FROM & TO'//
     &        ', like a vertical shaft.  Is this OK ?',0)
C@tno jcp 1996Jun07_17:26:49 end
            ENDIF
C@empa aw1996feb21 plilDat(i) is zero if the link type name was not found
C                  in FRTO. This would cause a run time error here.
            IF (pLiLDat(i).ne.0) THEN
C check opening angle/factor of windows
              IF (LDat(pLiLDat(i)).EQ.9.) THEN
                IF ((Mf(i).GT.1.0).OR.(Mf(i).LT.0.)) THEN
                 CALL INERR('&-NET-LINks:  LinkNr: '//LiNa(I),
     &            'Opening factor of window is not within '//
     &            'the range 0...1!',
     &            .FALSE.,2)
                ENDIF
C@empa aw 2000jul05 LVO type 2 may not be slanted
                IF ((LDat(pLiLDat(i)+4).EQ.2.) .and.
     &             (Hfl(i).lt.1)) THEN
                    LStr=LenStr(LiNa(I))
                 CALL INERR('&-NET-LINks:  LinkNr: '//LiNa(I)(1:LStr)//
     &            ' Large opening Type 2 (horizontal axis) ',
     &            'may not be slanted. Own height factor has to be 1!',
     &            .FALSE.,2)
                ENDIF

C input check for Cd calculation
                IF (LDat(pLiLDat(i)+9).EQ.0.) THEN
C@empa aw 2005aug29 Only LVO Types 1 and 2 are allowed for Cd-calculation
                 IF (LDat(pLiLDat(i)+4).EQ.1.) THEN
C Cd has to be calculated: Check correct input for that
                  IF (LL.EQ.0) THEN
C internal door:
                    IF (Zz(from).NE.Zz(to)) THEN
                      CALL INERR('&-NET-LINks:  LinkNr: '//LiNa(I)//
     &                'Reference levels of from and to zone ',
     &                'are not equal!  Cd can not be calculated!',
     &                .FALSE.,2)
                    ENDIF
                    IF (Hz(from).NE.Hz(to)) THEN
                      CALL INERR('&-NET-LINks:  LinkNr: '//LiNa(I)//
     &                'Room heights of from and to zone ',
     &                'are not equal!  Cd can not be calculated!',
     &                .FALSE.,2)
                    ENDIF
                    IF ((Hz(from).EQ.0.).OR.(Hz(to).EQ.0.)) THEN
                      CALL INERR('&-NET-LINks:  LinkNr: '//LiNa(I)//
     &                'Room heights of from and to zone ',
     &                'have to be input with H/D/W option ! '//
     &                'Otherwise Cd can not be calculated!',
     &                .FALSE.,2)
                    ENDIF
                  ELSE IF ((LL.EQ.1).OR.(LL.EQ.3)) THEN
C external opening
                    IF (LL.EQ.1) izone=to
                    IF (LL.EQ.3) izone=from
C Now error in less than 1 mm
                    IF (Dz(izone).LT. 0.001) THEN
                      CALL INERR('&-NET-LINks:  LinkNr: '//LiNa(I)//
     &                'Room depth ',
     &                'has to be input with H/D/W option ! '//
     &                'Otherwise Cd can not be calculated!',
     &                .FALSE.,2)
                    ENDIF
                  ELSE
                    CALL INERR('&-NET-LINks:  LinkNr: '//LiNa(I)//
     &              'Cd can not be calculated for links with ',
     &              'specified pressure or links between two wind '//
     &              'pressures!',
     &             .FALSE.,2)
                  ENDIF
C@empa aw 2005aug29 Only LVO Types 1 and 2 are allowed for Cd-calculation
                 ELSE IF (LDat(pLiLDat(i)+4).GT.2.) THEN  
                     
                    CALL INERR('&-NET-LINks:  LinkNr: '//LiNa(I)
     &              ,'The Cd-value has to be specified for LVO-types '//
     &               '3,4 and 5.;It can only be calculated for '//
     &               'LVO-types 1 and 2!'
     &              ,.FALSE.,2)

                 ENDIF
C                     end input check for Cd calculation
                ENDIF
C@empa aw 2005apr27 check the number of WI components
              nrWin=nrWin+1
C             end of check opening angle/factor of windows
              ENDIF
C           end check windows
            ENDIF
            IF (nrWin>MaxLo) THEN
              call IntDis(nrWin,str3,lstr3)
              call IntDis(MaxLo,str4,lstr4)
                    CALL INERR('&-NET-LINks: '// str3(1:lstr3)//
     &               ' WI components are used.', 
     &              'But only '//str4(1:lstr4)//' are allowed.',
     &             .FALSE.,2)
             CONTINUE
          ENDIF 
10      CONTINUE
        RETURN
        END
