C+*********************************************************** comv-tm6.f
C@empa aw 2005apr27 Keyword 'fef' does not work correct.Subroutine XCONC removed
Ch**********************************************************************
CC
CC        SUBROUTINE XCONC(TLotus,fefstr,nrcall)
CCC**********************************************************************
CCC
CCC lbl bs may 09 1991
CCC
CCC Purpose:
CCC Changes the outdoor pollutant concentration at the facade numbers
CCC described in the string fefstr (input parameter).
CCC nrcall (input parameter) is the number of calls to this routine
CCC within the current schedule line. 1 for pollutant 1, 2 for pollutant 2,
CCC so lets call it ipol!!!
CCCh*********************************************************************
CC        IMPLICIT NONE
CC	INCLUDE 'comv-inp.inc'
CC        DOUBLE PRECISION TLotus
CC	CHARACTER*30 fefstr
CC        INTEGER l,k,i,j,ix,eflag,fef,st1,st2,nrcall,fefcont,INTCON
CC        REAL conc,RELCON
CC        INTEGER IcIndex
CC
CCc        call ho('xconc, fefstr=',fefstr)
CC	l=len(fefstr)
CC	k=index(fefstr,'=')
CC	IF (k.GT.0) THEN
CC           variab='outside concentration'
CC           conc=RELCON(fefstr(k+1:l),eflag)
CCc           write(*,*) 'conc=',conc
CC           j=ICindex(fefstr,'TO')
CC	   IF (j.GT.0) THEN
CC             st1=INTCON(fefstr(1:j-1),eflag)
CC             st2=INTCON(fefstr(j+2:k-1),eflag)
CC             DO 20 j=st1,st2
CC               DO 30 I=1,Nwind
CC                 IF (Facade(I).EQ.j) THEN
CCC@tno jcp 1996May31_16:39:07 1,5 is now 1, Nconc the number of Pollutants used
CCCC                   DO 25 ix=1,5
CCCC                   DO 25 ix=1,Nconc
CC                     ix=nrcall
CC                     ExtConc(ix,I)=ExtConc(ix,I)*conc
CCC@tno jcp 1996Apr23_15:00:35 echo schedule changes for ExtConc
CC                     if (outputoptions(10).gt.0) then
CC           call echosch6(TLotus,'SCH-POL',ix,j,conc,ExtConc(ix,i))
CC                     end if
CCC25                 CONTINUE
CC                 ENDIF
CC30             CONTINUE
CC20           CONTINUE
CC           ELSE
CCC---------there is no TO in the string
CC              fef=INTCON(fefstr(1:k-1),eflag)
CCc          write(*,*) 'fef=',fef
CCc              call ho('xconc one fef seen',' ')
CC              DO 10 I=1,Nwind
CCc                 write(*,*) 'FacadeNa(',i,')=',facadeNa(i)
CCc                 write(*,*) 'Facade(',i,')=',facade(i)
CC		 IF (Facade(I).EQ.fef) THEN
CCC@tno jcp 1996May31_16:39:07 1,5 is now 1, Nconc the number of Pollutants used
CCCC                      DO 15 ix=1,5
CCC@tno jcp 1996May31_18:05:48 this is all wrong. Xconc handles only one pollutant
CCC its number is given as Nrcall
CCCC                      DO 15 ix=1,Nconc
CC                         ix=nrcall
CCC@tno jcp 1996May30_16:25:26 here is a missinterpretation OuCf(i) is the factor
CCCC                         ExtConc(ix,I)=ExtConc(ix,I)*conc
CC                         ExtConc(ix,I)=OuCF(I)*conc
CCC@tno jcp 1996Apr23_15:00:35 echo schedule changes for ExtConc
CC                     if (outputoptions(10).gt.0) then
CC           call echosch6(TLotus,'SCH-POL',ix,fef,conc,ExtConc(ix,i))
CC                     end if
CCC15                    CONTINUE
CC		   ENDIF
CC10	      CONTINUE
CC	   ENDIF
CC	ELSE
CC	   IF (nrcall.EQ.1) fefcont=1
CC	   k=index(fefstr,'(')
CC	   IF (k.EQ.0) THEN
CC              variab='outside concentration'
CC	      conc=RELCON(fefstr,eflag)
CCc           write(*,*) 'conc=',conc
CC	      DO 40 I=1,Nwind
CC		 IF (Facade(I).EQ.fefcont) THEN
CCC@tno jcp 1996May31_16:39:07 1,5 is now 1, Nconc the number of Pollutants used
CCCC                      DO 45 ix=1,5
CCcc                      DO 45 ix=1,Nconc
CC                        ix=nrcall
CCC@tno jcp 1996May30_16:26:57 here OuCF is the outside concentration factor
CCCC                         ExtConc(ix,I)=ExtConc(ix,I)*conc
CC                         ExtConc(ix,I)=OuCF(I)*conc
CCC@tno jcp 1996Apr23_15:00:35 echo schedule changes fro ExtConc
CC                     if (outputoptions(10).gt.0) then
CC        call echosch6(TLotus,'SCH-POL',ix,fefcont,conc,ExtConc(ix,i))
CC                     end if
CCc45                    CONTINUE
CC		   ENDIF
CC40	      CONTINUE
CC	      fefcont=fefcont+1
CC	   ELSE
CC	      j=index(fefstr,')')
CC              variab='outside concentration'
CC	      conc=RELCON(fefstr(k+1:j-1),eflag)
CCc           write(*,*) 'conc=',conc
CC	      st2=INTCON(fefstr(1:k-1),eflag)+fefcont-1
CC	      DO 60 j=fefcont,st2
CC		DO 50 I=1,Nwind
CC		   IF (Facade(I).EQ.j) THEN
CCC@tno jcp 1996May31_16:39:07 1,5 is now 1, Nconc the number of Pollutants used
CCCC                      DO 55 ix=1,5
CCcc                      DO 55 ix=1,Nconc
CC                        ix=nrcall
CCC@tno jcp 1996May30_16:29:14 outside concentration factor is in OuCF
CCCC                         ExtConc(ix,I)=ExtConc(cix,I)*conc
CC                         ExtConc(ix,I)=OuCf(I)*conc
CCC@tno jcp 1996Apr23_15:00:35 echo schedule changes fro ExtConc
CC                     if (outputoptions(10).gt.0) then
CC        call echosch6(TLotus,'SCH-POL',ix,j,conc,ExtConc(ix,i))
CC                     end if
CCcc55                    CONTINUE
CC		   ENDIF
CC50		CONTINUE
CC60	      CONTINUE
CC	      fefcont=st2+1
CC	   ENDIF
CC	ENDIF
CC
CC	RETURN
CC	END
Ch*********************************************************************

	SUBROUTINE RDLIST(option,zonestr,list)
C**********************************************************************
C
C lbl bs 02may, 1991
C
C Parameter:
C zonestr     (Input)			list	  (Output)
C option      (Input)
C
C Purpose:
C This routine reads a list of elements (zone numbers) specified using
C the keywords ALL, TO and / into a complete list of individual numbers
C which can be max. 3 digits long. Between the numbers there is at least
C one blank in the output string.
C Option = 1 : Zones	 Option = 2 : Links	Option = 3 : Occupants
Ch*********************************************************************
        IMPLICIT NONE
	INCLUDE 'comv-inp.inc'
	CHARACTER*(*) zonestr,list
        INTEGER efrst,elast,idx,zp,i,j,eflag,option,INTCON
	CHARACTER*400 dum
        INTEGER IcIndex

	CALL UpperC(zonestr)
	zp=1
	elast=0
C (a) range specified -------------------------------------------------
	IF (zonestr(1:3).EQ.'ALL') THEN
           efrst=1
	   IF (option.EQ.1) elast=Nz
	   IF (option.EQ.2) elast=Nl
	   IF (option.EQ.3) elast=MaxO
	ELSE
170        idx=ICindex(zonestr,'TO')
	   IF (idx.GT.0) THEN
C number before keyword:
	      i=idx-1
101	      IF (index('0123456789',zonestr(i:i)).GT.0) THEN
		 i=i-1
		 GOTO 101
	      ENDIF
	      efrst=INTCON(zonestr(i+1:idx-1),eflag)
C number after keyword:
	      j=idx+2
102	      IF (index('0123456789',zonestr(j:j)).GT.0) THEN
		 j=j+1
		 GOTO 102
	      ENDIF
	      elast=INTCON(zonestr(idx+2:j-1),eflag)
C delete processed TO-zone from zonestr:
	      IF (i.GT.0) dum(1:)=zonestr(1:i)
	      dum(i+1:)=zonestr(j+1:)
	      zonestr=dum
	      GOTO 170
	   ENDIF
	ENDIF
C write the zone numbers into list, separated by blanks
	IF (elast.GT.0) THEN
	   IF (zonestr(1:3).EQ.'ALL') THEN
	      DO 109 I=efrst,elast
		 IF (option.EQ.1) list(zp:zp+2) = ZoNa(I)
		 IF (option.EQ.2) list(zp:zp+2) = LiNa(I)
		 IF (option.EQ.3) WRITE(list(zp:zp+2),'(I3)') I
		 zp=zp+4
109	      CONTINUE
	   ELSE
	     DO 110 I=efrst,elast
		WRITE(list(zp:zp+2),'(I3)') I
		zp=zp+4
110	     CONTINUE
	   ENDIF
	ENDIF
C (b) process a list of zone numbers -------------------------------
	IF (zonestr(1:3).EQ.'ALL') THEN
	   dum=zonestr(4:)
	   zonestr=dum
	   i=0
	   GOTO 140
	ENDIF

	i=0
120	i=i+1
	IF (zonestr.EQ.' ') GOTO 900
	IF (index('0123456789',zonestr(i:i)).GT.0) GOTO 120
	IF (index('/ ',zonestr(i:i)).GT.0 .AND. i.GT.1) THEN
	   list(zp:)=zonestr(1:i-1)
	   zp=zp+4
	   dum=zonestr(i+1:)
	   zonestr=dum
	ELSE
	   zonestr=' '
	ENDIF
	i=0
	GOTO 120
C process a list of zone numbers after ALL keyword --------------------
C the numbers are deleted from the list
140	i=i+1
	IF (zonestr.EQ.' ') GOTO 900
	IF (index('0123456789',zonestr(i:i)).GT.0) GOTO 140
	IF (index('/ ',zonestr(i:i)).GT.0 .AND. i.GT.1) THEN
           idx=ICindex(list,zonestr(1:i-1))
	   IF (idx.GT.0) THEN
	      DO 141 J=idx+4,zp
		 list(J-4:J-4)=list(J:J)
141	      CONTINUE
	      zp=zp-4
	      list(zp:)=' '
	   ENDIF
	   dum=zonestr(i+1:)
	   zonestr=dum
	ELSE
	   zonestr=' '
	ENDIF
	i=0
	GOTO 140

900	RETURN
	END

