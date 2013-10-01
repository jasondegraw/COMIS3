C+*********************************************************** comv-mul.f
C     Routines to access the multi-schedule files
C     by Andreas Klingler December 1991
Ch*********************************************************************

      SUBROUTINE MULTIMAIN()
C**********************************************************************
C     lbl akk dec3, 1991
C
C     Purpose:
C     Initialize all necessary data for the processing of the multi-
C     schedule files
C     Changes:
C@empa aw 2000apr14 All messages changed according standard.
C@NBI PGS 2000Aug02 - "i", "j", "icindex" and "schexists" no longer
C@NBI                 used so removed from decralations
Ch*********************************************************************
        IMPLICIT NONE
      INCLUDE 'comv-uni.inc'
      INCLUDE 'comv-inp.inc'

C@empa aw 2000apr14 SchNamMSF,MaxColStr
      CHARACTER SchNamMSF*30, MaxColStr*5
      CHARACTER line*(Maxline),SchNam*30,SCHNamo*30,multistarttimes*30
      INTEGER   k,l,lm, col, ko
      INTEGER   jday,iyyy,month,id,h,m,hs,errorflg,ds
      CHARACTER*30 scheds(MaxCol)
C@empa aw 1997sep18 julx as integer
CC      REAL JulX
      INTEGER JulX
      INTEGER SchedType
      INTEGER LenStr
CC      INTEGER IcIndex
C@empa aw 2000apr14 LStr
      INTEGER LStr
      k = 0
      col = 1
C@empa aw 2000jan05 error message if multischedulefile not exist
CC      OPEN(MSF,FILE=multifile,STATUS='OLD')
      OPEN(MSF,FILE=multifile,STATUS='OLD',ERR=970)

 5    READ(MSF,'(A)',END=970) line
      if (line .eq. ' ') goto 5
      call KeepLine(Line)
C@empa aw 2000apr11 process correct if space after asterisk is missing
      k=index(line,'*')
      if (k .eq. 0) goto 905
      l = lenstr(line)
	variab='Multimain : starttimes'
      CALL GETWS(line,k,l, multistarttimes,'jan01_')
C@empa aw 2000apr11 end
      variab='Multimain : time step'
      CALL GETWI(line,k,l, multitimestep,3600)
C@tno jcp 1996Jun17_16:15:32 added variab
      CALL RTIMDAT(multistarttimes,jday,iyyy,month,id,h,m,hs,errorflg,
     & variab)
      if (errorflg .eq. 1) then
        CALL Error2('&-SCH-MULTI:  Multischedule file: '
     &     //multifile(1:LenStr(multifile))//': Invalid time format '//
     &     ';Error in line:',line,2)
      endif
C     If we have no year use the startyear of the simulation
      if (jday .le. 1721788) CALL JULD(jday,month,id,IYYY1,errorflg)
      CALL SECONDS(ds,h,m,hs)

      multischedtime = JulX(JdStart,sec1, jday,ds)

C     Now get the type of the schedule in each column
      READ(MSF,'(A)') line
      call KeepLine(Line)

      l = lenstr(line)
      k = 0
      ko = 0
      lm=lenstr(multischednames)
 10   CALL GETWRD(line,k,l,SchNam)
      CALL GETWRD(multischednames,ko,lm,SchNamo)
C@empa aw 2000mar27 uppercase schedule names
      SchNamMSF=SchNam
      call upperc(SchNam)
      call upperc(SchNamo)
      if (SchNamo .ne. 'INVALID') then
         if (SchNamo .ne. SchNam) then
          CALL Error('&-SCH-MULTI:  Multischedule file: '
     &    //multifile(1:LenStr(multifile))//
     &    ';Schedule name '''//SchNamMSF(1:LenStr(SchNamMSF))//
     &    ''' does not match with name in CIF!'//
     &    ';Names in CIF: '//
     &      multischednames(1:LenStr(multischednames))//
     &    ';Names in MSF: '//line(1:LenStr(Line)),2)
         endif
C
      endif
      scheds(col) = SchNamo
C save in the schedule type
      multischedtype(col) = SchedType(SchNamo)
      col = col + 1
      if (col .gt. MaxCol+1) then
         call intdis(MaxCol,MaxColStr,LStr)
         CALL Error('&-SCH-MULTI:  Multischedule file: '
     &   //multifile(1:LenStr(multifile))//
     &    ';Maximum number of multischedules ('
     &    //MaxColStr(1:LStr)//') exeeded',2)
     $
      endif
      if (k .lt. l) goto 10

      if (col .le. MaxCol) multischedtype(col) = -1
C@empa aw 2000apr14 I don't understand this check just with zone schedules.
C@empa aw           Anyway, the schedule could be defined in CIF. All used
C@empa aw           schedule names are checked in PreSCH.
C@empa aw
CCC all schedules Temperature, humidity sink and source
CC      do 90 i=1,Nz
CC	  schexists = 0
CC	  do 100 j=1,col-1
CC             if (ICindex(SchZ(i),scheds(j)).eq.1) goto 90
CC             if (SchZ(i) .ne. '-') schexists = 1
CC 100	  continue
CCC	  If there is any source schedule for this zone (schexists) and
CCC	  we come here then it is not one of the names in the file
CC	  if (schexists .eq. 1) goto 960
CC 90   continue
C@empa aw 2000apr14 Check the start of the multischedule file against the stop time
      if (multischedtime.gt.nsecsim2) goto 920

C     Check the end of the multischedulfile against the start time
C     Read the file until the schedules are in the simulation interval
 20   READ(MSF,'(A)',END=910) line
      if (line .eq. ' ') goto 20
      call KeepLine(Line)

      if (multischedtime .lt. 0) then
         multischedtime = multischedtime + multitimestep
         goto 20
      endif

C     Now we have read the first line of schedules which lie in the
C     simulation interval. We put this line back into the file, so we
C     can process it later.
      BACKSPACE MSF

      RETURN


 905  CALL Error2('&-SCH-MULTI:  Multischedule file: '
     &     //multifile(1:LenStr(multifile))//
     &     ';Input format for the first line in multischedule '//
     &     'file is: ''*date_time step'';Error in line:',line,2)
      goto 990

 910  CALL Error('&-SCH-MULTI:  Multischedule file: '
     &     //multifile(1:LenStr(multifile))//
     &     ';File ends before start time!',1)

      goto 990
 920  CALL Error('&-SCH-MULTI:  Multischedule file: '
     &     //multifile(1:LenStr(multifile))//
     &     ';File starts after stop time!',1)

      goto 990

C@empa aw 2000apr14 Message canceled
CC 960  call Error2('No such T H S Q schedule in multischedule file: ',
CC     +  SchZ(i),3)
CC      GOTO 990

C@empa aw 2000jan05 error message if multischedulefile not exist
 970  CALL ERROR('&-SCH-MULTI:  Multischedule file: '
     &     //multifile(1:LenStr(multifile))//
     &     ';File does not exist or is empty!',2)

 990  continue
      CLOSE(MSF)
      MSF = 0
C erase name to signal end of file for domulti
      multifile = ' '
      RETURN
      END
Ch*******************************************************************

      INTEGER FUNCTION SchedType(schedname)
C********************************************************************
C
C     lbl akk dec 5, 1991
C
C     Purpose:
C     Find out the type of a schedule by it's name.
C     The name must conform to the userguide.
C     Type 1 (main schedule) is not supported.
C     Additionally names starting with MET and POL are used to
C     identify the meteo and pollutant schedules
C
C     Parameter:
C     (I) schedname	name of the schedule
C
C********************************************************************

        IMPLICIT NONE
      CHARACTER*(*)schedname
      CHARACTER c
      INTEGER  type

      Call UPPERC(schedname)
      c = schedname(1:1)
C*** replace later by type=indec(c,'LWFTHSQ)  if type>0 then +1
C*** if 0 then check OCC MET and POL INVALID
C*** is the F not conflicting with F: for File? (in routine inSchMul)
      if (c .eq. 'L') then
C        Link exchanger
         type = 2
      else if (c .eq. 'W') then
C        Window
         type = 3
      else if (c .eq. 'F') then
C        Fan
         type = 4
      else if (c .eq. 'T') then
C        Temperatur
         type = 5
      else if (c .eq. 'H') then
C        Humidity
         type = 6
      else if (c .eq. 'S') then
C        Sink
         type = 7
      else if (c .eq. 'Q') then
C        Source
         type = 8
      else if (schedname(1:3) .eq. 'OCC') then
C        Occupant
         type = 9
      else if (schedname(1:3) .eq. 'MET') then
C        Meteodata
         type = 10
      else if (schedname(1:3) .eq. 'POL') then
C        Pollutants
         type = 11
      else if (schedname(1:7) .eq. 'INVALID') then
C        the data in this column is invalid
         type = -1
      else
C@empa aw 2000apr05 Message changed
         Call Error2('&-SCH-MULTI: unknown scheduletype: '//
     &               schedname,'Schedule name has to begin with: '//
     &               'W, F, T, H, S, Q, OCC, MET or POL!',2)
         type = -1
      endif

      SchedType = type
      RETURN
      END
Ch********************************************************************

      SUBROUTINE DoMulti()
C********************************************************************
C
C     Purpose:
C     change the simulation data according to a multischedule line
C
Ch********************************************************************
        IMPLICIT NONE
      INCLUDE 'comv-uni.inc'
      INCLUDE 'comv-inp.inc'

      CHARACTER line*(Maxline), word*30, SchNam*30,Dum*30
C@NBI PGS 2000Aug02 - "list" no longer used so removed from decralations
CC    CHARACTER list*400
C ipol= i for pollutant 1..5
C c1pol is the first character in the schedula name where a pollutant name
C starts. i.e. in Q12CO2 c1pol=4 the position where CO2 begins
      INTEGER   col,j,k,l,lm,ks,i,kl,ll,ipol,c1pol
      INTEGER   zonenr(MaxZ),OccNum1
      INTEGER   eflag
      REAL      value
      INTEGER iCol1ABC
      REAL RELCON
      INTEGER LenStr
      INTEGER IcIndex
      INTEGER IntCon


C@empa aw 1999nov23 ZOcNa
      CHARACTER ZOcNa*20, ZOc2Na*40
C@empa aw 1999nov26 p,startNr,endNr,iz,zNr,k2,l2,LStr1,Lstr2,key
        INTEGER p,startNr,endNr,iz,niz,Znr,k2,l2,LStr1,Lstr2,key
        REAL actfct
      multischedtime=multischedtime+multitimestep
C check if already hit end of multischedule file
      if (multifile .eq. ' ') goto 999

 500  READ(MSF,'(A)',END=999) line
      if (line .eq. ' ') goto 500

      call KeepLine(Line)

C     process the line with the schedule data
      col = 0
      k = 0
      ks = 0
      l = lenstr(line)
      lm=lenstr(multischednames)

C     skip invalid columns
 20   col = col + 1
      Call GETWRD(line,k,l,word)
      Call GETWRD(multischednames,ks,lm,SchNam)
C additional check for invalid schedule (when all are used)
      if (col .gt. MaxCol) then
C        end of schedules, see if more data is there
	 goto 800
      endif
      if (multischedtype(col) .eq. -1) then
C        skip this column (invalid)
         goto 20
      endif


C jump to 5 in cases 5,6,7,8
      GOTO (950,950,3,4,5,5,5,5,9,10,11) multischedtype(col)

C     Window
 3    do 30 i=1,Nl
         if (SoL(i).eq.SchNam) then
            value = RELCON(word,eflag)
            if (value.LT.0.0 .or. value.GT.1.0) goto 920
C@empa aw 1995jan11 actual value of opening factor is in mf(i)
CC	      LDat(pLiLDat(i)+8)=value
	    mf(i)= value

C@empa aw 2000jan05 this line is definitely wrong (see above)
CC            LDat(pLiLDat(i)+8)=value
         endif
 30   CONTINUE
      goto 800

C     Fan
 4    do 40 i=1,Nl
         if (SoL(i).eq.SchNam) then
            value = RELCON(word,eflag)
            if (value.LT. 0.0) goto 920
            mf(i) = OFanSp(i) * value
         endif
 40   continue
      goto 800




C we jump here to look if a zone has the schedule
C found in the MSF (Multi Schedule File)
C for temperature, humidity sinks and sources
C DO loop 50 stretches along these four items
C     Temperature, Humidity, Sink, Source
 5    value = RELCON(word,eflag)
      do 50 i=1,Nz
C@empa aw 2000nov08 It might be not the first schedule in this zone. Check  .ne. 0
CC         if (ICindex(SchZ(i),SchNam).eq.1) then
         if (ICindex(SchZ(i),SchNam).ne.0) then

C now jump further
      GOTO (950,950,950,950,51,6,7,8,950,950,950) multischedtype(col)
 51      continue
C     Temperature
            Tz(i) = value*ifact(UnitTMul) + ifact(UnitToff)
         goto 50
C     Humidity
  6      continue
            if (value .lt. 0) goto 920
            XHz(i) = value*ifact(UnitXh)
         goto 50
C     Sink
  7      continue
            if (value .lt. 0) goto 920
             ipol=0
             if (Nconc.gt.0) then
71           ipol=ipol+1
C get the Pollutant name from the Schedule first and check that versus CName(*).
C    SchNam could be S12CO2 with Cname = CO2andMore
	     C1Pol=iCol1ABC(2,SchNam)

C Dum contains the partial string starting at C1Pol
            DUM=SchNam(C1Pol:lenstr(SchNam))
            if (ICindex(CName(ipol),DUM).GT.0) then
C this schedule matches the pollutant
C@NBI PGS 2000Oct09 - Can now define different I/O units for each pollutant.
CC            Sink(ipol,i) = OSink(ipol,i) * value * ifact(UnitPSin)
              Sink(ipol,i) = OSink(ipol,i) *value* ifact(UnitPolSi+iPol)
C cause a jump out of loop
              ipol=Nconc
            endif
           if (ipol.lt.nconc) goto 71
        end if
C end if Nconc>0

      goto 50
C     Source
  8      continue
            if (value .lt. 0) goto 920

             ipol=0
             if (Nconc.gt.0) then
81           ipol=ipol+1
C get the Pollutant name from the Schedule first and check that versus CName(*).
C    SchNam could be Q12CO2 with Cname = CO2andMore
                     C1Pol=iCol1ABC(2,SchNam)

C Dum contains the partial string starting at C1Pol
            DUM=SchNam(C1Pol:lenstr(SchNam))
            if (ICindex(CName(ipol),DUM).eq.1) then
              Source(ipol,i) = OrSource(ipol,i) * value *
C@NBI PGS 2000Oct09 - Can now define different I/O units for each pollutant.
CC   &                         ifact(UnitPSou)
     &                         ifact(UnitPolSo+iPol)
C cause a jump out of loop
              ipol=Nconc
            endif
           if (ipol.lt.nconc) goto 81
        end if
C end if Nconc>0


         endif
 50   continue
      goto 800
C-----


C     Occupant
 9      variab='activity of occupant'
      Call GETWR(line,k,l,actfct,1.0,.TRUE.)

C@tno jcp 1996Apr08_14:48:34 added: read number of occupants from schedule
C@tno jcp 1996Jun14_00:20:38 variab for GetWI
      variab='Number of occupants'
      Call GETWI(line,k,l,OccNum1,1)
C@empa aw 1999mar27 We want to use ID's not sequence numbers. Look for the Zone ID's
C@empa              in ZoTree

CC      list = ' '
CC      Call RDLIST(3,word,list)
CC      ll = len(list)


85          CONTINUE
	        p=INDEX(word,'/')
	        IF (p.GT.0) THEN
	          word(p:p)=' '
                GOTO 85
              ENDIF
            ll=len(word)
		  kl=0

C i=the occupant number
            i = INTCON(SchNam(4:4),eflag)

		  variab='Occupant schedule: zonenr/list'
CC90		  CALL GETWI(list,k,l,zonenr,0)
            iz=1
90		  CALL GETWS(word,kl,ll,ZOcNa,'')
            IF (ZOcNa.NE.'') THEN
	        p=INDEX(ZOcNa,'-')
              IF (p.GT.0)THEN
	          ZOcNa(p:p)=' '
	          ZOc2Na=ZOcNa
	          k2=0
	          l2=len(ZOc2Na)
		      CALL GETWS(ZOc2Na,k2,l2,ZOcNa,'')
	          CALL LookNam(Nz,ZOcNa,ZoTree,ZoTreeN,
     &	      startNr,ZoLarg,ZoSmal,key)
                IF (key.EQ.1) THEN
                  LStr1=LENSTR(SchNam)
                  LStr2=LENSTR(ZOcNa)
                  CALL INERR('Multischedule file: '
     &            //multifile(1:LenStr(multifile))//
     &            ';Zonename "'//ZOcNa(1:LStr2)//'" does not '//
     &            'exist in &-NET-ZONes !'//
     &            ';Error in Line:' ,Line,.FALSE.,3)
                ENDIF
		      CALL GETWS(ZOc2Na,k2,l2,ZOcNa,'')
	          CALL LookNam(Nz,ZOcNa,ZoTree,ZoTreeN,
     &	      endNr,ZoLarg,ZoSmal,key)
                IF (key.EQ.1) THEN
                  LStr1=LENSTR(SchNam)
                  LStr2=LENSTR(ZOcNa)
                  CALL INERR('Multischedule file: '
     &            //multifile(1:LenStr(multifile))//
     &            ';Zonename "'//ZOcNa(1:LStr2)//'" does not '//
     &            'exist in &-NET-ZONes !'//
     &            ';Error in Line:' ,Line,.FALSE.,3)
                ENDIF

                DO 95 zNr=startNr,endNr,sign(1,(endNr-startNr))
	            ZoneNr(iz)=zNr
	            iz=iz+1
95              CONTINUE
	        ELSE
	          CALL LookNam(Nz,ZOcNa,ZoTree,ZoTreeN,
     &	      ZoneNr(iz),ZoLarg,ZoSmal,key)
                IF (key.EQ.1) THEN
                  LStr1=LENSTR(SchNam)
                  LStr2=LENSTR(ZOcNa)
                  CALL INERR('Multischedule file: '
     &            //multifile(1:LenStr(multifile))//
     &            ';Zonename "'//ZOcNa(1:LStr2)//'" does not '//
     &            'exist in &-NET-ZONes !'//
     &            ';Error in Line:' ,Line,.FALSE.,3)
                ENDIF

	          iz=iz+1
              ENDIF
	        GOTO 90
            ENDIF
	      niz=iz-1
            DO 100 iz = 1, niz
		    IF (zonenr(iz).GT.0) THEN
C@tno jcp 1996Apr23_17:43:56 here again add and reset occupants)activity
C  factor) to OccAct
                     OccAct(i,zonenr(iz))=actfct
C@tno jcp 1996Apr08_14:44:48 also assign the array OccNum(*,*)
                     OccNum(i,zonenr(iz))=OccNum1
CC                     IF (outputoptions(10).gt.0) THEN
c                       write(*,*) 'schnam',schnam
CC                       CALL echosch4(TLotus,SchNam,zonenr(iz),
CC     &                  actfct,occnum1)
CC                     ENDIF
C                    write(cof,*) TLotus,'occupant',J,' to zone',zonenr
              ENDIF
100         CONTINUE
C@empa aw nov26 End of patched lines for Zone ID's instead of sequence numbers
	      GOTO 800
C-----


C i=the occupant number, value= it's multiplier
C      i = INTCON(SchNam(4:4),eflag)
C      variab='Schedule: Occupant in zone nr'
C now may follow a range of zone numbers! For source this is OK, but for
C occupant dosis, we want to know where the occupant resides. Now we just
C add concentration for all rooms
C add concentration*activity/number of occupants for all rooms
C OCCUPANTS-OCCUPANTS
C 90   Call GETWI(list,kl,ll,zonenr,0)
C      if (zonenr .gt. 0) then
C@tno jcp 1996Apr23_17:42:30 here it is the question do we directly assign or
C add and substract these occupants
C         OccAct(i,zonenr) = value
C@tno jcp 1996Apr08_14:49:20 assign array OccNum(*,*) the number of occupants
C         OccNum(i,zonenr) = OccNum1
C         goto 90
C      endif

C      goto 800

C     Meteo
 10   VMet = RELCON(word,eflag)
      if (VMet .lt. 0.0) goto 920
C unit conversion
      Vmet=ifact(UnitW)*Vmet

	variab='Schedule:  D meteo'
      Call GETWR(line,k,l,Dmet,DDmet,.TRUE.)
      if (Dmet .lt. 0.0 .or. Dmet .gt. 360.0) goto 920

	variab='Schedule:  Temp meteo'
      Call GETWR(line,k,l,Tmet,DTmet,.TRUE.)
C unit conversion
      Tmet=ifact(UnitToff)+ifact(UnitTmul)*Tmet

	variab='Schedule:  Xhumidity meteo'
      Call GETWR(line,k,l,XhMet,DXhMet,.TRUE.)
C@tno jcp 1996Jun26_14:41:28 XHmet conversion is 2 lines below
c      XhMet = XhMet/1000.0
      if (XhMet .lt. 0.0) goto 920
C unit conversion
      XhMet=ifact(UnitXh)*XhMet

	variab='Schedule:  Pbarometer meteo'
      Call GETWR(line,k,l,PbMet,DPbMet,.TRUE.)
      PbMet = PbMet * 1000.0

      Call CheckPb(PbMet,Zmet,' Meteo Barometric')
      if (PbMet .lt. 0.0) goto 920
      goto 800

C     Pollutants
C     (only outdoor concentrations)
 11   j = INTCON(SchNam(4:4),eflag)
      value = RELCON(word,eflag)
      if (value .lt. 0.0) goto 920
      do 111 i=1,Nwind
C@NBI PGS 2000Oct09 - Can now define different I/O units for each pollutant.
CC       ExtConc(j,i) = OuCF(i) * value * ifact(UnitPConc)
         ExtConc(j,i) = OuCF(i) * value * ifact(UnitPolCo+j)
 111  continue
      goto 800

 800  continue
      if (k .ge. l) then
C   we are at the end of the dataline
C   if col at limit of schedules then done
         if (col .lt. MaxCol) then
            if (multischedtype(col+1) .ne. -1) then
C        but there is more data requested - error
                goto 910
            endif
         endif
         RETURN
      endif
      goto 20


C@empa aw 2000mar27 improve error message
CC 910  Call Error2('Missing data in line',line,3)
 910  Call Error2('There is more data requestet in multischedule '//
     &             ' file: '//multifile(1:LenStr(multifile))//
     &             ' in line:' ,line,3)

 920  Call Error2('Value out of range for',SchNam,3)

 950  Call Error('Unsupported multischedule type in DoMulti',3)

 999  multifile = ' '
      CLOSE(MSF)
      RETURN
      END

