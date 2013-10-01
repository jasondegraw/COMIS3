C+*********************************************************** comv-do2.f
Ch**********************************************************************
	SUBROUTINE DOE2READ(Line,endflag)
C***********************************************************************
C lbl bs jul 23 1991
C
C Routine to read a binary packed DOE-2 weather file and use it for
C Comis.
C Line is the output parameter which contains a dataline in the Comis
C format.

Ch**********************************************************************

        IMPLICIT NONE
	INCLUDE 'comv-uni.inc'

        INTEGER MDAYS(12),IDAT30(1536),IWTH(5),DUM1(6)
        INTEGER LRECX, IRECX, IDIF, IP1, IDX, IH, FirstCall
        INTEGER ID, IM2, IDE, I, endflag
        REAL    XMASK(5,2),CALC(5),DUM2
	CHARACTER*160 Line

C SAVE must be before all data statements
	SAVE FirstCall
	DATA FirstCall /0/

	DATA MDAYS / 31,28,31,30,31,30,31,31,30,31,30,31 /
	DATA XMASK / 0., 0., -99., 0., 15., 1., 1., 1., .0001, .1 /
C@tno jcp 1996Jul03_07:35:16 What is the DOE2READ patch for Feb29


C initialisation
	IF (FirstCall .EQ. 0) THEN
	  LRECX = 0
	  IM2 = 1
	  ID  = 1
	  IH  = 1
	  FirstCall = 1
	  endflag = 0
	ENDIF

C When the stop time is dec 31 23:00, the NEXT data point is accessed
C   (which doesn't exist)  The symptom is that the month is 13!
C@tno jcp 1996Jul03_07:33:07 DOE2READ: Correct problem: Wrong solution! Reopen t
C or you never can simulate over december,jan (i.e jun...jun next year)
	if (im2 .gt. 12) then
		endflag = 1
		return
        endif

C@tno jcp 1996Jul03_08:08:15 Doe2Read: Where does IM2 ID and IH come from at cal
C I don't see them in a common block and they are not saved or assigned here bef

	IDE = MDAYS(IM2)
105	IRECX = IM2*2 + (ID-1)/16 - 1
	IDX = MOD(ID-1,16) + 1

	IF (IRECX-LRECX) 200,400,300

200	IDIF = LRECX - IRECX + 1
	DO 220 I=1,IDIF
	  BACKSPACE 10
220	CONTINUE

300     READ (IFS, END=999) DUM1,DUM2,DUM2,DUM1(1),LRECX,DUM1(1),
     &		 DUM2,DUM2,DUM1(1),IDAT30
	GO TO 105

400	CONTINUE
	IP1 = 96*(IDX-1) + 4*IH - 3
	IWTH(1) = MOD(IDAT30(IP1+3),128)
	IWTH(2) = MOD(IDAT30(IP1+1),16)
	IWTH(3) = MOD(IDAT30(IP1),256)
	IWTH(4) = IDAT30(IP1+2)/128
	IWTH(5) = IDAT30(IP1)/65536

	DO 500 I=1,5
	  CALC(I) = FLOAT(IWTH(I))*XMASK(I,2) + XMASK(I,1)
500	CONTINUE
	CALC(2) = INT(CALC(2) + .01)

C Unit conversions
c   Wind speed - knots to m/s
	CALC(1) = CALC(1) * 0.514
c   Wind direction - 1/16 circle to degrees
	CALC(2) = CALC(2) * 22.5
c   Dry bulb temperature - F to C
	CALC(3) = - 160./9. + CALC(3) / 1.8
c   Humidity ratio - lb H2O/lb Air to g/kg
	CALC(4) = CALC(4) * 1000.
c   Barometric pressure - inches Hg to kPa
	CALC(5) = CALC(5) * 1./0.2953

	WRITE(Line,9000) (CALC(I),I=1,5)
9000	FORMAT(5(F8.3,1X))

C new values
	IH = MOD(IH,24)+1
	IF (IH.EQ.1) ID = MOD(ID,IDE)+1
	IF (ID.EQ.1.AND.IH.EQ.1) IM2 = IM2 + 1

	RETURN
999	endflag = 1
	RETURN
	END
C************************************************************************
