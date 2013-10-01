C+*********************************************************** comv-hva.f
Ch***********************************************************************
	SUBROUTINE WHICHDF(Dat,ZETA1,ZETA2,KEY)
C pass parameter number	   1   2    3
C***********************************************************************
C Purpose: Checks the type of fitting and branches to the routines
C	   which calculate the pressure loss coefficient ZETA.
C
C Module:  empa aw 1991july5
C
C Changes:
C@empa aw 1993feb11 Entry with screen:
C                   According COMIS fundamental page 54 I take case 1 of
C                   the entries plus screen
C@empa aw 2001sep07 ZETA2 for negative flow (TO to FROM) added.
C                   A contraction becomes a diffuser and vice versa. 
C                   An exit becomes an entry and vice versa. 
C
C
C Pass parameters:
C IO # Name	unit	  description
C I  1 Dat(*)	multi	  Array with type and parameters of the fitting
C O  2 ZETA	 -	  Pressure loss coefficient of the fitting
C O  3 ZETA2 -   Pressure loss coefficient for negative flow (TO to FROM)
C O  4 KEY	(-)	  error key, is set to 1 IF error is found
C
C Contents of the array Dat:
C      Dat(1)		  Type of the fitting
C      Dat(2)		  Parameter 1
C      Dat(3)		  Parameter 2
C
C Type Name				 Parameter 1	Parameter 2
C  0   straight duct			 -		-
C  1   entry, round			 t/D		L/D
C  2   entry, round with screen		 screen%	-
C  3   hood				 Type		Angle
C      round                             1
C      rectangular			 2
C  4   exit, round			 -		-
C  5   exit,round with screen		 screen%	-
C  6   elbow				 r/D		-
C  7   diffuser, round			 A1/A2          Angle
C  8   contraction, round		 A2/A1          Angle
C  9   obstraction sreen, round		 screen%	-
C 10   perforated plate			 T/DP           N*DP**2/DD**2
C				  T    :  (m)  thickness of perforated plate
C				  DP   :  (m)  diameter of a perforated hole
C				  N    :  (-)  number of te holes
C				  DD   :  (m)  diameter of a duct
C 11   orifice, ASHRAE			 A1/A2		-
C 12   orifice, DIN			 A1/A2		-
C 13   damper				 angle		-
C
Ch***********************************************************************

        IMPLICIT NONE
        REAL  Dat(3),ZETA,ZETA1,ZETA2
        INTEGER KEY
C
C@empa aw 2001sep12
        ZETA=0
        ZETA2=0
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 0 = no DF
	IF (Dat(1).EQ.0) THEN
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 1 = entry,round
	ELSE IF (Dat(1).EQ.1) THEN
	    CALL ENTRY(Dat(2),Dat(3),ZETA1,KEY)
          ZETA2=1.
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 2 = entry, round with screen
	ELSE IF (Dat(1).EQ.2) THEN
C	    CALL ENTRYSCR(Dat(2),Dat(3),ZETA,KEY)
C  Entry, round with screen would need three parameters: t/D, L/D, screen%
C  I think one should input entry and screen each as a separate fitting
	    CALL SCREEN(Dat(2),ZETA,KEY)
C            entry
             ZETA1=ZETA+0.5
C            exit
             ZETA2=ZETA+1
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 3 = hood
	ELSE IF (Dat(1).EQ.3) THEN
	    CALL HOOD(INT(Dat(2)+0.5),Dat(3),ZETA1,ZETA2,KEY)
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 4 = exit, round
	ELSE IF (Dat(1).EQ.4) THEN
C         exit
	    ZETA1=1
C         entry
          ZETA2=0.5
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 5 = exit, round with screen
	ELSE IF (Dat(1).EQ.5) THEN
	    CALL SCREEN(Dat(2),ZETA,KEY)
C         exit
	    ZETA1=ZETA+1
C         entry
          ZETA2=ZETA+0.5  
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 6 = elbow
	ELSE IF (Dat(1).EQ.6) THEN
	    CALL ELBOW(Dat(2),ZETA1,KEY)
          ZETA2=ZETA1
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 7 = diffuser, round
	ELSE IF (Dat(1).EQ.7) THEN
	    CALL DIFFUSER(Dat(2),Dat(3),ZETA1,KEY)
	    CALL CONTRACT(Dat(2),Dat(3),ZETA2,KEY)
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 8 = contraction, round
	ELSE IF (Dat(1).EQ.8) THEN
	    CALL CONTRACT(Dat(2),Dat(3),ZETA1,KEY)
	    CALL DIFFUSER(Dat(2),Dat(3),ZETA2,KEY)
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 9 = obstraction screen, round
	ELSE IF (Dat(1).EQ.9) THEN
	    CALL SCREEN(Dat(2),ZETA1,KEY)
          ZETA2=ZETA1
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 10 = perforated plate
	ELSE IF (Dat(1).EQ.10) THEN
	    CALL PERF(Dat(2),Dat(3),ZETA1,KEY)
          ZETA2=ZETA1
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 11 = orifice, ASHRAE
	ELSE IF (Dat(1).EQ.11) THEN
	    CALL ORIFICE(Dat(2),ZETA1,KEY)
          ZETA2=ZETA1
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 12 = orifice, DIN 
C This orifice is not available)
C
C	ELSE IF (Dat(1).EQ.12) THEN
C	    CALL DINORIFICE(Dat(2),ZETA,KEY)
C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 13 = damper
C@lbl bvs 1997Nov12 misnumbered as 12
CC	ELSE IF (Dat(1).EQ.12) THEN
	ELSE IF (Dat(1).EQ.13) THEN
	    CALL DAMPER(Dat(2),ZETA1,KEY)
          ZETA2=ZETA1
	ELSE
	    KEY=1
CC            CALL ERROR('invalid DF-type',2)
C@tno jcp 1996Jul25_09:21:12 whichdf:inerr missed argument
            CALL INERR('invalid DF-type',' ',.true.,2)
	    END IF
	RETURN
	END
   
   
Ch***********************************************************************
	SUBROUTINE CONTRACT(RATIO,THETA,ZETA,KEY)
C pass parameter number	     1	   2	  3    4
C***********************************************************************
C Purpose: this routine calculates the pressure loss coefficient through
C	   the contraction including the abrupt reducing. The reference Cross section 
C        is after the contraction. 
C
C Module : 4.***, TG IV, hxy/July 11, 1989
C Changes: empa vd 25.april 91
C a set of C's has been added to the first column of the
C lines with---------------------------------------------- in the
C comment below
C removed unused X
C
C Pass parameters:
C
C IO # Name	unit	  description
C
C I  1 RATIO	(-)	  AREA2/ AREA1
C			  AREA1 area of the duct before the contraction
C			  AREA2 area of the duct after the contraction
C I  2 THETA	(deg)	  angle of the contraction
C O  3 ZETA	(-)	  pressure loss coefficient through the contraction
C O  4 KEY	(-)	  error key, is set to 1 IF error is found
C
C Limit	 :
C error RETURN IF ratio=area2/area1 >1 or theta < 15
C example:
C call CONTRACT(0.2,90,ZETA,KEY)  ZETA=0.1767	  ,  KEY=0
C
Ch***********************************************************************

        IMPLICIT NONE
        REAL RATIO,THETA,ZETA,ZETA1,ZETA2
        INTEGER KEY
        REAL ALPHA

C   ratio=area2/area1
	IF(RATIO.GT.1.0) GO TO 91
	IF(THETA.LT.15)	 GO TO 92
C
	IF(THETA.GE.15.AND.THETA.LT.45) THEN
	     IF (RATIO.LE.0.5) THEN
		  ZETA=0.05
	     ELSE
		  ZETA=-0.1*RATIO+0.1
	     ENDIF

	ELSE IF (THETA.GE.45.AND.THETA.LT.75) THEN
	     IF(RATIO.LE.0.5) THEN
		  ZETA=-0.04354*RATIO+0.081067
	     ELSE
		  ZETA=-0.118594*RATIO+0.118594
	     ENDIF

	ELSE IF (THETA.GE.75.AND.THETA.LT.105) THEN

	     IF(RATIO.LE.0.5) THEN
		  ZETA=-0.155455*RATIO**2-0.080048*RATIO+0.19896
	     ELSE
		  ZETA=-0.240144*RATIO+0.240144
	     ENDIF

	ELSE IF (THETA.GE.105.AND.THETA.LT.135) THEN

	     IF(RATIO.LE.0.5) THEN
		  ZETA=-0.543197*RATIO**2+0.054433*RATIO+0.288733
	     ELSE
		  ZETA=-0.3603*RATIO+0.3603
	     ENDIF

	ELSE IF (THETA.GE.135.AND.THETA.LT.175) THEN
	     IF(RATIO.LE.0.5) THEN
		  ZETA=-0.737068*RATIO**2+0.121673*RATIO+0.36362
	     ELSE
		  ZETA=-0.48038*RATIO+0.48038
	     ENDIF

	ELSE IF (THETA.GE.175) THEN
	     IF(RATIO.LE.0.5) THEN
C   This equation comes from Recknagel, Sprenger, [59]Comis Fundamentals
		  ZETA1=0.5873*(1-RATIO)

C   This equation comes from Feustel [64]Comis Fundamentals
		  ALPHA=0.63+0.37*RATIO**3
		  ZETA2=(1/ALPHA-1)**2
C
C   The two results obtained by these two equations are averaged.
C
		  ZETA=(ZETA1+ZETA2)/2
	     ELSE
		  ZETA=-0.522844*RATIO+0.522844
	     ENDIF
	ENDIF
	GOTO 100

91	CONTINUE
CC        CALL ERROR ('CONTRACT:  RATIO >1; ZETA NOT DEFINED ',2)
        CALL inERR ('CONTRACT:  RATIO >1; ZETA NOT DEFINED ',' ',
     &   .true.,2)
	KEY=1
	GOTO 100

92	CONTINUE
CC        CALL ERROR ('CONTRACT:  THETA <15 DEG, ZETA NOT DEFINED ',2)
        CALL inERR('CONTRACT:  THETA <15 DEG, ZETA NOT DEFINED ',
     &   ' ',.true.,2)
	KEY=1

100	RETURN
	END


Ch***********************************************************************
	SUBROUTINE DAMPER(ANGLE,ZETA,KEY)
C pass parameter number	    1	 2    4
C***********************************************************************
C Purpose: this routine calculates the pressure loss coefficient for
C	   the damper
C
C Module : 4.***, TG IV, hxy/July, 12, 1989
C Changes: EMPA/vd 25.april 91
C
C Limit	 : ANGLE < 75 DEG
C
C Pass parameters:
C
C IO # Name	unit	  description
C
C I  1 ANGLE	(deg)	  angle of the damper in the duct
C O  2 ZETA	(-)	  pressure loss coefficient through the plate
C O  3 KEY	(-)	  error key, is set to 1 if error is found
C
C Example:
C call DAMPER(40,ZETA,KEY)  -> ZETA=11.0 ,  KEY=0
Ch***********************************************************************

        IMPLICIT NONE
        REAL ANGLE,ZETA,X
        INTEGER KEY
C

C This equation is curve fitted from the data of ASHRAE Handbook, 1985

	IF(ANGLE.GT.75) GO TO 90

	X=ANGLE

	ZETA=3.918762e-8*X**6-5.321206e-6*X**5+2.769086e-4*X**4
     &	-6.575832e-3*X**3+0.073384*X**2-0.251754*X

	GOTO 100

90	CONTINUE
CC        CALL ERROR ('DAMPER:  ANGLE > 75 DEG, ZETA NOT DEFINED,' ,2)
        CALL inERR('DAMPER:  ANGLE > 75 DEG, ZETA NOT DEFINED,',
     &   ' ',.true.,2)
	KEY=1

100	RETURN
	END

Ch***********************************************************************
	SUBROUTINE DIFFUSER(RATIO,THETA,ZETA,KEY)
C pass parameter number	    1	  2	 3    4
C***********************************************************************
C Purpose: this routine calculates the pressure loss coefficient through
C	   the conical round diffuser. The reference cross section is before
C        the diffuser.
C
C Module : 4.***, TG IV, hxy/July 11, 1989
C Change : EMPA VD 22.April 1991
C
C
C Pass parameters:
C
C IO # Name	unit	  description
C
C I  1 RATIO	(-)	  RATIO= AREA1/AREA2
C				 AREA1	 area of the duct before diffuser
C				 AREA2	 area of the duct after	 diffuser
C I  2 THETA   (deg)	  angle of the opening of the conical diffuser
C O  3 ZETA	(-)	  pressure loss coefficient through the diffuser
C O  4 KEY	(-)	  error key, is set to 1 if error is found
C
C Limits :
C error RETURN if area1/area2 > 1 or theta > 180 deg.
C example:
C call DIFFUSER(0.4,45.0,ZETA,KEY) -> ZETA= 0.36  , KEY=0
C
Ch***********************************************************************

        IMPLICIT NONE
        REAL RATIO,THETA,ZETA
        INTEGER KEY

C   ratio=area1/area2

	IF(RATIO.GT.1) GO TO 91
	IF(THETA.GT.180) GO TO 92

C  These equations come from Handbook of SHASE, Japan.
C
	IF     (THETA.LT.7.5) THEN
		    ZETA=0.170049*(1-RATIO)**2

	ELSE IF (THETA.GE.7.5.AND.THETA.LT.15.0) THEN
		    ZETA=0.28021*(1-RATIO)**2

	ELSE IF (THETA.GE.15.0.AND.THETA.LT.25.0) THEN
		    ZETA=0.450967*(1-RATIO)**2

	ELSE IF (THETA.GE.25.0.AND.THETA.LT.35.0) THEN
		    ZETA=0.58964*(1-RATIO)**2

	ELSE IF (THETA.GE.35.0.AND.THETA.LT.45.0) THEN
		    ZETA=0.729821*(1-RATIO)**2

	ELSE IF (THETA.GE.45.0.AND.THETA.LT.50.0) THEN
C  This equation comes from Handbook of HVAC.
		    ZETA=(1-RATIO)**2

	ELSE IF (THETA.GE.50.0) THEN
C   This equation comes from Recknagel/Sprenger
		   ZETA=1-1.77795*RATIO+0.784161*RATIO**2
	ENDIF
	GO TO 100

91	CONTINUE
CC        CALL ERROR ('DIFFUSER: RATIO A1/A2 >1, ZETA NOT DEFINED ',2)
        CALL inERR('DIFFUSER: RATIO A1/A2 >1, ZETA NOT DEFINED',
     &   ' ',.true.,2)
	KEY=1
	GOTO 100

92	CONTINUE
CC        CALL ERROR ('DIFFUSER:  THETA >180 DEG, ZETA NOT DEFINED ',2)
        CALL inERR('DIFFUSER:  THETA >180 DEG, ZETA NOT DEFINED ',
     &   ' ',.true.,2)
	KEY=1

100	RETURN
	END


Ch***********************************************************************
	SUBROUTINE ELBOW(RATIO,ZETA,KEY)
C pass parameter number	  1	2   3
C***********************************************************************
C Purpose: this routine calculates the pressure loss coefficient through
C	   the elbow for a radius/diameter ratio.
C
C Module : 4.***, TG IV, hxy/May 24, 1989
C Change : EMPA vd 17.April 1991
C
C Pass parameters:
C
C IO # Name	unit	  description
C
C I  1 RATIO	(-)	  RATIO=RAD/DIA
C				RAD   radius of the elbow
C				DIA   diameter of the elbow
C O  2 ZETA	(-)	  pressure loss coefficient through the elbow
C O  3 KEY	(-)	  error key, is set to 1 if error is found
C
C Limit	 :
C error message if rad/dia is less than 0 or more than 5
C example:
C call ELBOW(1.0,ZETA,KEY) --> ZETA= 0.287317
C
Ch***********************************************************************

        IMPLICIT NONE
        REAL RATIO,ZETA,ZETA1,ZETA2,X
        INTEGER KEY
C
	X=RATIO
	IF(X.LE.0.0.OR.X.GT.5.0) GO TO 91
C
C	This equation comes from German Handbook(Helmut Feustel:
C	Beitrag zur theoretischen Beschreibung der Druck- und
C	Luft- massenstromverteilung in natuerlich und maschinell
C	geluefteten Gebauden,1984).
C
	ZETA1=0.255-0.04278*(1/x)+0.145*(1/x)**2
C
C	This equation shows the fitting curve of data from
C	ASHRAE Handbook, 1985.
C
	ZETA2=0.144818-0.13683*(1/x)+0.209427*(1/x)**2
C
	ZETA=(ZETA1+ZETA2)/2.0
C
	GOTO 100
C
91	CONTINUE
CC        CALL ERROR('ELBOW: RATIO=RAD/DIA <0 OR >5 ,ZETA NOT DEFINED',2)
        CALL inERR('ELBOW: RATIO=RAD/DIA <0 OR >5 ,ZETA NOT DEFINED',
     &  ' ',.true.,2)
	KEY=1

100	RETURN
	END

Ch***********************************************************************
	SUBROUTINE ENTRY(RATIO1,RATIO2,ZETA,KEY)
C pass parameter number	  1	 2	3    4
C***********************************************************************
C Purpose: this routine calculates the pressure loss coefficient for
C	   the duct mounted in wall with a diameter, a length and
C	   a thickness of the duct
C
C Module : 4.***, TG IV, hxy/July, 10, 1989
C Change : empa VD 17.APRIL 91
C
C Pass parameters:
C
C IO # Name	unit	  description
C
C I  1 RATIO1	(-)	  RATIO1=THICK/DIA
C I  2 RATIO2	(-)	  RATIO2=LENGTH/DIA
C				    DIA	     (m)  diameter of the duct
C				    LENGTH   (m)  lenght of the duct
C				    THICK    (m)  thickness of the duct
C O  3 ZETA	(-)	  pressure loss coefficient through the entry
C O  4 KEY	(-)	  error key, is set to 1 if error is found
C
C Limits  : ?
C
C example:  dia= 0.1  thickness= 0.001	lenght= 0.1
C	    ratio1= 0.0001  ratio2=1.0
C call ENTRY(0.0001,1.0,ZETA,KEY) -> zeta=0.719619
C
Ch***********************************************************************

        IMPLICIT NONE
        REAL ZETA,RATIO1,RATIO2
        INTEGER KEY
	key=0

C   This equation comes from ASHRAE Handbook, 1985.

	IF(RATIO1.LE.0.01) THEN
	     ZETA=1-0.5*EXP((-22.024718)*RATIO2)
	ELSE IF(RATIO1.GE.0.05) THEN
	     ZETA=0.5
	ELSE
	     ZETA=0.72-0.22*EXP((-6.357975)*RATIO2)
	ENDIF

	RETURN
	END


Ch***********************************************************************
	SUBROUTINE HOOD(TYPE,ANGLE,ZETA1,ZETA2,KEY)
C pass parameter number	  1	2    3	 4
C***********************************************************************
C Purpose: this routine calculates the pressure loss coefficient for
C	   the hood
C
C Module : 4.***, TG IV, hxy/July, 10, 1989
C Change : EMPA VD 17.April 91
C@empa aw 2001sep17 Zeta for reverse direction
C
C Pass parameters:
C
C IO # Name	unit	  description
C
C I  1 TYPE	 (-)	  Type of hood
C			        TYPE=1: Round
C			        TYPE=2: Square or Rectangular
C I  2 ANGLE (deg)  angle of the hood
C O  3 ZETA1 (-)	  pressure loss coefficient through the entry
C O  4 ZETA2 (-)    pressure loss coefficient for reverse flow direction
C O  4 KEY	 (-)	  error key, is set to 1 if error is found
C
C Limits :  ?
C
C example:
C call HOOD(1,100.0,ZETA,KEY)  -> ZETA=0.1913
C
Ch***********************************************************************

        IMPLICIT NONE
        REAL ANGLE,X,ZETA1,ZETA2
        INTEGER TYPE,KEY
	key=0
C
	X=ANGLE
	IF(TYPE.EQ.1) THEN
	   IF(ANGLE.LE.20.0) THEN
	      ZETA1=0.001111*(x-30)**2
	   ELSE
	      ZETA1=1.220134E-9*X**4-6.237859E-7*X**3+1.239291E-4*X**2
     &		   -7.595473E-3*X+0.213333
	   ENDIF

	ELSE IF(TYPE.EQ.2) THEN
	   IF(ANGLE.LE.20.0) THEN
	      ZETA1=0.000625*(X-40)**2
	   ELSE
	      ZETA1=1.292977E-9*X**4-6.613409E-7*X**3+1.33971E-4*X**2
     &		   -8.462347E-3*X+0.306111
	   ENDIF
	ENDIF

      IF (ANGLE.LE.14.0)THEN
          ZETA2=1-0.05754*x
      ELSE IF (ANGLE.LE.58.33)THEN
          ZETA2=-0.000223*x**2 + 0.0343*x - 0.242
      ELSE 
          ZETA2=1
      ENDIF

	RETURN
	END

Ch***********************************************************************
	SUBROUTINE ORIFICE(RATIO,ZETA,KEY)
C pass parameter number	     1	  2    3
C***********************************************************************
C Purpose: this routine calculates the pressure loss coefficient for
C	   the orifice
C
C Module : 4.***, TG IV, hxy/July, 12, 1989
C Change : EMPA VD 17.April 91
C
C Pass parameters:
C
C IO # Name	unit	  description
C
C I  1 RATIO	(-)	  hole area of the orifice to the duct area
C O  2 ZETA	(-)	  pressure loss coefficient through the plate
C O  3 KEY	(-)	  error key, is set to 1 if error is found
C
C Limit	 :
C error if ratio is more than 1
C example:
C call ORIFICE(0.4,ZETA,KEY) ->ZETA=7.97781
C
Ch***********************************************************************

        IMPLICIT NONE
        REAL RATIO,ZETA,X
        INTEGER KEY
C
	IF(RATIO.GT.1.0) GO TO 90

C  This equation comes from German Handbook B.

	X=RATIO
	ZETA=(1/(X*(0.63+0.37*X**3))-1)**2
	GOTO 100

90	CONTINUE
CC        CALL ERROR('ORIFICE:  RATIO >1.0 ',2)
        CALL inERR('ORIFICE:  RATIO >1.0 ',' ',.true.,2)
	KEY=1

100	RETURN
	END

Ch***********************************************************************
	SUBROUTINE PERF(RATIO1,RATIO2,ZETA,KEY)
C pass parameter number	  1	2      3    4
C***********************************************************************
C Purpose: this routine calculates the pressure loss coefficient for
C	   the perforted plate in a duct
C
C Module : 4.***, TG IV, hxy/July, 11, 1989
C Change : empa vd 17.April 91
C
C Pass parameters:
C
C IO # Name	unit	  description
C
C I  1 RATIO1	(-)	  RATIO1= THICK/DIAP
C				  THICK:  (m)  thickness of perforated plate
C				  DIAP :  (m)  diameter of a perforated hole
C I  2 RATIO2	(-)	  RATIO2= N*DIAP**2/DIAD**2
C				  N	  (-)  number of te holes
C				  DIAD	  (m)  diameter of a duct
C O  3 ZETA	(-)	  pressure loss coefficient through the plate
C O  4 KEY	(-)	  error key, is set to 1 if error is found
C
C Limits  : 0.015 < RATIO1 < 0.8
C	    0.0	  < RATIO2 < 1.0
C example:  thick=0.001, diap=0.01, diad=0.1, n=30
C	    ratio1= 0.1	  , ratio2= 0.3
C call PERF(0.1,0.3,ZETA,KEY)  ->ZETA=17.6504
C
Ch***********************************************************************

        IMPLICIT NONE
        REAL RATIO1,RATIO2,ZETA,X,Y
        INTEGER KEY
C
C	RATIO1=THICK/DIAP
C	RATIO2=N*DIAP**2/DIAD**2
	X= RATIO1
	Y= RATIO2
	IF(X.LT.0.015.OR.X.GE.0.8) GO TO 90
	IF(Y.GT.1.0) GO TO 91
C
	IF     (X.LT.0.05) THEN
		    ZETA=(1/(Y*(0.606579+0.59292*Y**3))-1)**2
	ELSE IF (X.GE.0.05.AND.X.LT.0.3) THEN
		    ZETA=(1/(Y*(0.628494+0.458459*Y**3))-1)**2
	ELSE IF (X.GE.0.3.AND.X.LT.0.5)	 THEN
		    ZETA=(1/(Y*(0.639692+0.383948*Y**3))-1)**2
	ELSE IF (X.GE.0.5.AND.X.LT.0.8)	 THEN
		    ZETA=(1/(Y*(0.667188+0.435501*Y**3))-1)**2
	ENDIF
	GOTO 100

90	CONTINUE
CC        CALL ERROR ('PERF: RATIO1 < 0.015 OR >0.8 ',2)
        CALL inERR('PERF: RATIO1 < 0.015 OR >0.8 ',' ',.true.,2)
	KEY=1
	GOTO 100

91	CONTINUE
CC        CALL ERROR ( 'PERF: RATIO2 > 1.0 ' ,2)
        CALL inERR( 'PERF: RATIO2 > 1.0 ' ,' ',.true.,2)
	KEY=1

100	RETURN
	END

Ch*******************************************************************
	SUBROUTINE SCREEN(RATIO,ZETA,KEY)
C pass parameter number	    1	 2    3
C***********************************************************************
C Purpose: this routine calculates the pressure loss coefficient for
C	   the screen in a duct
C
C Module : 4.***, TG IV, hxy/July, 11, 1989
C Change : empa VD 18.April 91
C@empa aw 1993feb11 according userguide  screen is given in %
C
C Pass parameters:
C
C IO # Name	unit	  description
C
C I  1 RATIO	(-)	  free area ratio of the screen in a duct
C O  2 ZETA	(-)	  pressure loss coefficient through the screen
C O  3 KEY	(-)	  error key, is set to 1 if error is found
C
C Limit	 :  error return if ratio is more than 1
C example:
C call SCREEN(0.5,ZETA,KEY)    --> ZETA=1.66057
C
Ch*******************************************************************

        IMPLICIT NONE
        REAL RATIO,ZETA,X
        INTEGER KEY

	X=RATIO/100

	IF(X.GT.1.0) GO TO 90

	ZETA=-221.201171*X**5+816.805903*X**4-1200.250976*X**3
     &	+884.317032*X**2-332.299312*X+52.62451

	GOTO 100

90	CONTINUE
CC        CALL ERROR ('SCREEN: RATIO > 1.0', 2)
        CALL inERR('SCREEN: RATIO > 1.0',' ',.true., 2)
	KEY=1
100	RETURN
	END


Ch*******************************************************************
	 SUBROUTINE LOSTEE (P1, FVo, RhoLJ, DHydr, DpTJ, KEY)
C pass parameter input	  1   2	    3	   4
C pass parameter output				   5	6
C**************************************************************
C@empa vd 1991Jul12_08:40  Complete change of LOSTEE as well as the TEE1 to
C			   TEE6- Routines:
C			   Node pressure and new arrays + pass-parameters
C
C Purpose: this routine calculates the local pressure losses DpTJ
C	   for the flow junction of tees
C	   The pressure node is assumed to be always at P1
C
C	   Node		  Junction		    Node
C			 P1	P2
C	   O----*******---   O	-------********------O
C		Link 1	     | P3	Link 2
C			     |
C			     |
C
C Module : 4.***, TG IV, hxy/February 23, 1989
C Change : EMPA VD 18.April 1991/1991Jul12
C@empa aw 1991aug07 calculation of VE and Dp corrected
C@empa aw 1991aug07 Parametername FMA to FVo changed.
C@empa aw 1991aug08 DPTJ is always negative
C@empa aw 1991aug16 If one of the TEE routines reports an error then DPTJ=0
C@empa aw 1991aug16 Initilize Key.
C@empa aw 1992sep01 make sure that K-values are not bigger than 1
C@empa aw 1992sep02 DPTJ is not always negative
C
C Limits : This as well as the related routines (TEE1..TEE6) are valid only
C	   for junctions with:
C	   - the main duct size beeing equal to the branch duct size
C	   - T-junction angle = 90 deg
C
C Pass parameters:
C
C IO # Name	unit	  description
C I  1 P1	(Pa)	  Network Node Pressure = P1
C I  2 FVo(3)	(m3/s)	  volume flow rate at P1,P2,P3
C I  3 RhoLJ(3) (kg/m3)	  air density at P1,P2 and P3
C I  4 DHydr	(m)	  hudraulic diameter of the duct
C O  5 DpTJ(3)	(Pa)	  Pressure corrections against PZ at P1,P2 and P3
C O  6 KEY	(-)	  error key, is set to 1 if error is found
C
C
C   Definitions for T-junction routines:
C    ----------------------------------------------------------------------
C
C    1	   -->+	  +<--	  2
C    ---------- O ---------	 Flows are positiv, when towards the
C		|  +		 junction.
C		|  ^		 1-2 : Main duct
C		| 3		 3   : Branch duct , same diameter,
C						     angle 90deg
C
C
C    Flow case		   Subroutine	  Local loss	 Case (as defined
C							 in COMIS Fundamentals
C					  coefficient	 p. 65)
C
C=======================================================================
C
C     1---> O --->2 N	   Tee1		    Y12		   Case 1
C	   /|\				    Y32		   Case 3
C	    | 3
C
C-----------------------------------------------------------------------
C   N 1<--- O <---2	   Tee5		    Y21		   Case 1
C	   /|\				    Y31		   Case 3
C	    | 3
C
C-----------------------------------------------------------------------
C   N 1---> O --->2	   Tee2		    Y12D	   Case 2
C	    |				    Y13D	   Case 6
C	   \|/3
C
C-----------------------------------------------------------------------
C     1<--- O <---2 N	   Tee6		    Y21D	   Case 2
C	    |				    Y23D	   Case 6
C	   \|/3
C
C-----------------------------------------------------------------------
C     1---> O <---2	   Tee3		    Y13	 =	   Case 5
C	    |				    Y23
C	   \|/3	 N
C
C-----------------------------------------------------------------------
C     1<--- O --->2	   Tee4		    Y31D =	   Case 4
C	   /|\				    Y32D
C	    | 3 N
C
C-----------------------------------------------------------------------
C
C example:
C!!  call lostee(.....)	  auch Tabelle erneuern!
C
C Routine    FVo(1)    FVo(2)	  FVo(3)    -->	  PX2		 PX3
C -----------------------------------------------------------------
C TEE1	     0.10   -0.20     0.10	    5.085327	   97.69439
C TEE2	     0.20   -0.10    -0.10	  156.0296	   93.46741
C TEE3	     0.10    0.10    -0.20	  100.0000	   -8.364128
C TEE4	    -0.10   -0.10     0.20	  100.0000	  119.9820
C TEE5	    -0.20    0.10     0.10	  194.9147	  192.6091
C TEE6	    -0.10    0.20    -0.10	   43.97037	   37.43777
Ch*************************************************************


        IMPLICIT NONE
        REAL P1,FVo(3), RhoLJ(3),DpTJ(3)
        REAL K12,K21,K32,K31,K13,K23
        REAL DHydr,PI
        INTEGER KEY


C Local parameters:
        REAL P2,P3,VE(3),DP(3),AREA
        INTEGER j
        REAL FNPI
C-----------------------------------------------------
	Key=0
	PI=FNPI( )
	AREA=PI*(DHydr/2.0)**2
C
	DO 10 J=1,3
C!! das muss sicher noch verbessert werden . FVo kann=0 sein!
	  IF(FVo(J).EQ.0.0) GOTO 91
	  VE(J)=FVo(J)/AREA
	  DP(J)=(RhoLJ(J)/2.0)*VE(J)**2

10	CONTINUE

C make sure that K-values are not bigger than 1
	K12=ABS(FVo(1)/FVo(2))
	IF (K12.GT.1.)THEN
           K12=1
	ENDIF
	K21=ABS(FVo(2)/FVo(1))
	IF (K21.GT.1.)THEN
           K21=1
	ENDIF
	K32=ABS(FVo(3)/FVo(2))
	IF (K32.GT.1.)THEN
           K32=1
	ENDIF
	K31=ABS(FVo(3)/FVo(1))
	IF (K31.GT.1.)THEN
           K31=1
	ENDIF
	K13=ABS(FVo(1)/FVo(3))
	IF (K13.GT.1.)THEN
           K13=1
	ENDIF
	K23=ABS(FVo(2)/FVo(3))
	IF (K23.GT.1.)THEN
           K23=1
	ENDIF
C
C@empa aw 1999nov19 additional parentheses
CC	IF     (FVo(1).GT.0.0.AND.FVo(2).LT.0.0) THEN
	IF  ((FVo(1).GT.0.0).AND.(FVo(2).LT.0.0)) THEN
		  IF (FVo(3).GT.0.0) THEN
		     CALL TEE1(P1,K12,K32,DP,P2,P3,KEY)
		  ELSE
		     CALL TEE2(P1,K21,K31,DP,P2,P3,KEY)
		  ENDIF

	ELSE IF (FVo(1).GT.0.0.AND.FVo(2).GT.0.0) THEN
		     CALL TEE3(P1,K13,K23,DP,P2,P3,KEY)

	ELSE IF (FVo(1).LT.0.0.AND.FVo(2).LT.0.0) THEN
		    CALL TEE4(P1,K13,K23,DP,P2,P3,KEY)

	ELSE IF (FVo(1).LT.0.0.AND.FVo(2).GT.0.0) THEN
		  IF (FVo(3).GT.0.0) THEN
		    CALL TEE5(P1,K21,K31,DP,P2,P3,KEY)
		  ELSE
		    CALL TEE6(P1,K12,K32,DP,P2,P3,KEY)
		  ENDIF
	ENDIF

C Calculate departure from node pressure
C and calculate DpTJ accordingly:  DpTJ(2) = p2- p1 etc.
	  DPTJ(2)=P2-P1
	  DPTJ(3)=P3-P1
	IF (KEY.EQ.1)THEN
	   DPTJ(2)=0
	   DPTJ(3)=0
	ELSE
    	ENDIF

	GOTO 100

91	CONTINUE
CC        CALL ERROR('LOSTEE: VOLUME FLOW RATE IN ONE BRANCH IS ZERO',2)
        CALL inERR('LOSTEE: VOLUME FLOW RATE IN ONE BRANCH IS ZERO',
     &  ' ',.true.,2)
	KEY=1
	GOTO 100

100	RETURN
	END


Ch*******************************************************************
         REAL FUNCTION FNPI ( )
C***********************************************************************
C  Purpose: This routine RETURNs PI
C  Module : 0.1	 hcp/ January 28, 1989
C  Changes: empa vd 18.April 91
C  Limits : only 20 decimal places
C  example: FNPI=3.14159265358979323846
Ch***********************************************************************
	FNPI=3.14159265358979323846
	RETURN
	END

Ch*************************************************************************
	 SUBROUTINE TEE1(P1,K12,K32,DP,P2,P3,KEY)
C pass parameter number 1  2   3   4  5	 6  7
C**************************************************************************
C Purpose: this routine calculates the total  pressures at the main duct
C	   and at the branch duct just after the junctions
C	   in case of converging flow from the branch duct to the main duct,
C	   based on the total  pressure at the main duct just before the
C	   junction and the volume flow rates in three ducts.
C
C Module : 4.***, TG IV, hxy/April 24, 1989
C Change : EMPA VD 18.April 91
C@empa vd 1991Jul12_16:00  New pass parameters
C
C Pass parameters:
C
C IO # Name	unit	  description
C
C I  1 P1	(Pa)	  total pressure at the main duct just before
C			  the junction
C I  2 K12	(-)	  ratio of the volume flow rate in the main duct
C			  before the junction to the volume flow rate in the
C			  main duct after the junction
C I  3 K32	(-)	  ratio of the volume flow rate in the branch duct
C			  before the junction to the volume flow rate in the
C			  main duct after the junction
C I  4 DP	(Pa)	  velocity pressure array

C O  5 P2	(Pa)	  total	 pressure at the main duct just after
C			  the junction
C O  6 P3	(Pa)	  total	 pressure at the branch duct just before
C			  the junction
C O  7 KEY	(-)	  error key, is set to 1 if error is found
C
C ERROR RETURN IF:   K12 < 0.0	or  > 1.0
C		     K32 < 0.0	or  > 1.0
C example: see routine LOSTEE
Ch*************************************************************************

        IMPLICIT NONE
        REAL P1,K12,K32,DP(*),P2,P3
        INTEGER KEY
        REAL Y12,Y32
C
	IF(K12.LT.0.0.OR.K12.GT.1.0) GO TO 91
	IF(K32.LT.0.0.OR.K32.GT.1.0) GO TO 92
C
	Y12 =  0.525 +0.42*K12 -     K12**2
	Y32 = -0.92  +3.49*K32 -1.48*K32**2
	P2  =  P1 - Y12*DP(2)
	P3  =  P2 + Y32*DP(2)

	GOTO 100

CC91      CALL ERROR ('TEE1: K12 < 0.0 or >1.0 ',2)
91      CALL inERR('TEE1: K12 < 0.0 or >1.0 ',' ',.true.,2)
	KEY=1
	GOTO 100
CC92      CALL ERROR ('TEE1: K32 < 0.0 or >1.0 ',2)
92      CALL inERR('TEE1: K32 < 0.0 or >1.0 ',' ',.true.,2)
	KEY=1

100	RETURN
	END


Ch*******************************************************************
	 SUBROUTINE TEE2(P1,K21,K31,DP,P2,P3,KEY)
C pass parameter number 1   2  3   4  5	 6  7
C********************************************************************

C Purpose: this routine calculates the total pressures at the main duct
C	   just after the junction and the branch duct just after the junction
C	   in case of diverging flow from the main duct to the branch duct,
C	   based on the total pressure at the main duct just before the
C	   junction and the volume flow rates in three ducts.
C
C Module : 4.***, TG IV, hxy/April 24, 1989
C Change : EMPA VD 18.April 91
C
C Pass parameters:
C
C IO # Name	unit	  description
C I  1 P1	(Pa)	  total pressure at the main duct just before
C			  the junction
C I  2 K21	(-)	  ratio of the volume flow rate in the main duct
C			  after the junction to the volume flow rate in the
C			  main duct before the junction
C I  3 K31	(-)	  ratio of the volume flow rate in the branch duct
C			  after the junction to the volume flow rate in the
C			  main duct before the junction
C I  4 DP	(Pa)	  velocity pressure array
C O  5 P2	(Pa)	  total pressure at the main duct just after
C			  the junction
C O  6 P3	(Pa)	  total pressure at the branch duct just before
C			  the junction
C O  7 KEY	(-)	  error key, is set to 1 if error is found
C
C ERROR RETURN IF:   K21 < 0.0	or  > 1.0
C		     K31 < 0.0	or  > 1.0
C example: see routine LOSTEE
Ch*************************************************************************
*

        IMPLICIT NONE
        REAL P1,K21,K31,DP(*),P2,P3,Y12D,Y13D
        INTEGER KEY
C
	IF(K21.LT.0.0.OR.K21.GT.1.0) GO TO 91
	IF(K31.LT.0.0.OR.K31.GT.1.0) GO TO 92

	IF(K21.LE.1.0.AND.K21.GT.0.78) THEN
	   Y12D=1.55*(K21-0.78)**2-0.03
	ELSE
	   Y12D=0.65*(0.78-K21)**2-0.03
	ENDIF
	Y13D=0.99-0.82*K31+1.02*K31**2
	P2 = P1- Y12D*DP(1)
	P3 = P1 -Y13D*DP(1)

	GOTO 100

CC91      CALL ERROR ('TEE2: K21 < 0.0 or >1.0 ',2)
91      CALL ineRR('TEE2: K21 < 0.0 or >1.0 ',' ',.true.,2)
	KEY=1
	GOTO 100
CC92      CALL ERROR ('TEE2: K31 < 0.0 or >1.0 ',2)
92      CALL inERR('TEE2: K31 < 0.0 or >1.0 ',' ',.true.,2)
	KEY=1

100	RETURN
	END


Ch*******************************************************************
	 SUBROUTINE TEE3(P1,K13,K23,DP,P2,P3,KEY)
C pass parameter number 1  2  3	  4   5	 6  7
c********************************************************************
C Purpose: this routine calculates the total pressures at the main duct
C	   just after the junction and the branch duct just after the junction
C	   in case of converging flow from the main duct to the branch duct,
C	   based on the total pressure at the main duct just before the
C	   junction and the volume flow rates in three ducts
c
C Module : 4.***, TG IV, hxy/April 24, 1989
C Change : empa vd 18.April 91
C@empa vd 1991Jul12_16:00  New pass parameters
C@empa aw 1991aug07	   calculation of P2 corrected
C
C Pass parameters:
C
C IO # Name	unit	  description
C I  1 PX1	(Pa)	  total pressure at the main duct just before
C			  the junction
C I  2 K13	(-)	  ratio of the volume flow rate in the main duct
C			  before the junction to the volume flow rate in the
C			  branch duct after the junction
C I  3 K23	(-)	  ratio of the volume flow rate in the main duct
C			  after the junction to the volume flow rate in the
C			  branch duct after the junction
C I  4 DP	(Pa)	  velocity pressure array

C O  5 P2	(Pa)	  total	 pressure at the main duct just after
C			  the junction
C O  6 P3	(Pa)	  total	 pressure at the branch duct just before
C			  the junction
C O  7 KEY	(-)	  error key, is set to 1 if error is found
C
C ERROR RETURN IF:   K13 < 0.0	or  > 1.0
C		     K23 < 0.0	or  > 1.0
C example: see routine LOSTEE
Ch**************************************************************************

        IMPLICIT NONE
        REAL P1,K13,K23,DP(*),P2,P3,Y13,Y23
        INTEGER KEY

	IF(K13.LT.0.0.OR.K13.GT.1.0) GO TO 91
	IF(K23.LT.0.0.OR.K23.GT.1.0) GO TO 92

	Y13=0.80-0.95*K13+1.34*K13**2
	Y23=0.80-0.95*K23+1.34*K23**2
	P3 =P1 - Y13*DP(3)
	P2 =P3 + Y23*DP(3)

	GOTO 100

CC91      CALL ERROR ('TEE3: K13 < 0.0 or >1.0 ',2)
91      CALL inERR('TEE3: K13 < 0.0 or >1.0 ',' ',.true.,2)
	KEY=1
	GOTO 100
CC92      CALL ERROR ('TEE3: K23 < 0.0 or >1.0 ',2)
92      CALL inERR('TEE3: K23 < 0.0 or >1.0 ',' ',.true.,2)
	KEY=1

100	RETURN
	END


Ch*******************************************************************
	 SUBROUTINE TEE4(P1,K13,K23,DP,P2,P3,KEY)
C pass parameter number 1   2	3  4  5	 6  7
C****************************************************************************
C Purpose: this routine calculates the total pressures at the main duct
C	   just after the junction and the branch duct just after the junction
C	   in case of diverging flow from the branch duct to the main duct,
C	   based on the total pressure at the main duct just before the
C	   junction and the volume flow rates in three ducts
C
C Module : 4.***, TG IV, hxy/April 24, 1989
C Change : empa vd 18.April 91
C@empa vd 1991Jul12_16:00  New pass parameters
C
C Pass parameters:
C
C IO # Name	unit	  description
C I  1 P1	(Pa)	  total pressure at the main duct just before
C			  the junction
C I  2 K13	(-)	  ratio of the volume flow rate in the main duct
C			  before the junction to the volume flow rate in the
C			  branch duct after the junction
C I  3 K23	(-)	  ratio of the volume flow rate in the main duct
C			  after the junction to the volume flow rate in the
C			  branch duct after the junction
C I  4 DP	(Pa)	  velocity pressure array

C O  5 P2	(Pa)	  total	 pressure at the main duct just after
C			  the junction
C O  6 P3	(Pa)	  total	 pressure at the branch duct just before
C			  the junction
C O  7 KEY	(-)	  error key, is set to 1 if error is found
C
C ERROR RETURN IF:   K13 < 0.0	or  > 1.0
C		     K23 < 0.0	or  > 1.0
C example: see routine LOSTEE
Ch*************************************************************************

        IMPLICIT NONE
        REAL P1,K13,K23,DP(*),P2,P3,Y31D,Y32D
        INTEGER KEY

	IF(K13.LT.0.0.OR.K13.GT.1.0) GO TO 91
	IF(K23.LT.0.0.OR.K23.GT.1.0) GO TO 92

	Y31D=0.59+1.18*K13-0.68*K13**2
	Y32D=0.59+1.18*K23-0.68*K23**2

	P3=P1+Y31D*DP(3)
	P2=P3-Y32D*DP(3)

	GOTO 100

CC91      CALL ERROR ('TEE3: K13 < 0.0 or >1.0 ',2)
91      CALL inERR('TEE3: K13 < 0.0 or >1.0 ',' ',.true.,2)
	KEY=1
	GOTO 100
CC92      CALL ERROR ('TEE3: K23 < 0.0 or >1.0 ',2)
92      CALL inERR ('TEE3: K23 < 0.0 or >1.0 ',' ',.true.,2)
	KEY=1

100	RETURN
	END



Ch*******************************************************************
	 SUBROUTINE TEE5(P1,K21,K31,DP,P2,P3,KEY)
C pass parameter number 1   2	3  4  5	 6  7
C***************************************************************************
C Purpose: this routine calculates the total pressures at the main duct
C	   just after the junction and the branch duct just after the junction
C	   in case of converging flow from the branch duct to the main duct,
C	   based on the total pressure at the main duct just before the
C	   junction and the volume flow rates in three ducts
C
C Module : 4.***, TG IV, hxy/April 24, 1989
C Change : EMPA VD 18.April 91
C@empa vd 1991Jul12_16:00  New pass parameters
C
C Pass parameters:
C
C IO # Name	unit	  description
C I  1 P1	(Pa)	  total pressure at the main duct just before
C			  the junction
C I  2 K21	(-)	  ratio of the volume flow rate in the main duct
C			  after the junction to the volume flow rate in the
C			  main duct before the junction
C I  3 K31	(-)	  ratio of the volume flow rate in the branch duct
C			  after the junction to the volume flow rate in the
C			  main duct before the junction
C I  4 DP	(Pa)	  velocity pressure array

C O  5 P2	(Pa)	  total	 pressure at the main duct just after
C			  the junction
C O  6 P3	(Pa)	  total	 pressure at the branch duct just before
C			  the junction
C O  7 KEY	(-)	  error key, is set to 1 if error is found
C
C ERROR RETURN IF:   K21 < 0.0	or  > 1.0
C		     K31 < 0.0	or  > 1.0
C example: see routine LOSTEE
Ch*************************************************************************

        IMPLICIT NONE
        REAL P1,K21,K31,DP(*),P2,P3,Y21,Y31
        INTEGER KEY
C
	IF(K21.LT.0.0.OR.K21.GT.1.0) GO TO 91
	IF(K31.LT.0.0.OR.K31.GT.1.0) GO TO 92

	Y21=0.525+0.42*K21-K21**2
	Y31=-0.92+3.49*K31-1.48*K31**2

	P2 =P1+Y21*DP(1)
	P3 =P1+Y31*DP(1)

	GOTO 100

CC91      CALL ERROR ('TEE2: K21 < 0.0 or >1.0 ',2)
91      CALL inERR('TEE2: K21 < 0.0 or >1.0 ',' ',.true.,2)
	KEY=1
	GOTO 100
CC92      CALL ERROR ('TEE2: K31 < 0.0 or >1.0 ',2)
92      CALL inERR('TEE2: K31 < 0.0 or >1.0 ',' ',.true.,2)
	KEY=1

100	RETURN
	END



Ch*************************************************************************
	 SUBROUTINE TEE6(P1,K12,K32,DP,P2,P3,KEY)
C pass parameter number 1   2	3  4  5	 6  7
C**************************************************************************
C Purpose: this routine calculates the total pressures at the main duct
C	   just after the junction and the branch duct just after the junction
C	   in case of diverging flow from the main duct to the branch duct,
C	   based on the total pressure at the main duct just before the
C	   junction and the volume flow rates in three ducts
C Module : 4.***, TG IV, hxy/April 24, 1989
C Change : EMPA VD 18.April 91
C@empa vd 1991Jul12_16:00  New pass parameters
C
C Limit	 : only in the case that the size of a main duct is equal
C	   to the size of a branch.
C
C Pass parameters:
C
C IO # Name	unit	  description
C I  1 PX1	(Pa)	  total pressure at the main duct just before
C			  the junction
C I  2 K12	(-)	  ratio of the volume flow rate in the main duct
C			  before the junction to the volume flow rate in the
C			  main duct after the junction
C I  3 K32	(-)	  ratio of the volume flow rate in the branch duct
C			  before the junction to the volume flow rate in the
C			  main duct after the junction
C I  4 DP	(Pa)	  velocity pressure array
C O  5 P2	(Pa)	  total	 pressure at the main duct just after
C			  the junction
C O  6 P3	(Pa)	  total	 pressure at the branch duct just before
C			  the junction
C O  7 KEY	(-)	  error key, is set to 1 if error is found
C
C ERROR RETURN IF:   K12 < 0.0	or  > 1.0
C		     K32 < 0.0	or  > 1.0
C example: see routine LOSTEE
Ch*************************************************************************

        IMPLICIT NONE
        REAL P1,K12,K32,DP(*),P2,P3,Y21D,Y23D
        INTEGER KEY
C
	IF(K12.LT.0.0.OR.K12.GT.1.0) GO TO 91
	IF(K32.LT.0.0.OR.K32.GT.1.0) GO TO 92

	IF (K12.LE.1.0.AND.K12.GT.0.78) THEN
	       Y21D=1.55*(K12-0.78)**2-0.03
	ELSE
	       Y21D=0.65*(0.78-K12)**2-0.03
	ENDIF
	Y23D=0.99-0.82*K32+1.02*K32**2

C!! ich bin nicht sicher wegen dem Vorzeichen, so ist es in der alten Version
C		V
	P2 = P1-(-Y21D)*DP(2)
	P3 = P2 -Y23D*DP(2)

	GOTO 100

CC91      CALL ERROR ('TEE1: K12 < 0.0 or >1.0 ',2)
91      CALL inERR('TEE1: K12 < 0.0 or >1.0 ',' ',.true.,2)
	KEY=1
	GOTO 100
CC92      CALL ERROR ('TEE1: K32 < 0.0 or >1.0 ',2)
92      CALL inERR('TEE1: K32 < 0.0 or >1.0 ',' ',.true.,2)
	KEY=1

100	RETURN
	END
Ch*********************************************************************
	 SUBROUTINE TJUNC (Nl,PzNew,Fma,FromTo,RhoL,Ldat,pLiLDat,
     &			 Njunc,JuncNr, DpJ, KEY)
C**********************************************************************
C Purpose: Steering routine for Junction pressure losses
C Limits:  This	 versions is only for 90deg-T-junctions
C Module:  12.July.91  VD
C Changes: aw 1991aug07
C@empa aw 1994nov04 Check From/To direction and process with DpTJ accordingly
C
C Pass parameters:
C
C IO  #	 Name		 unit	     description
C I   1	 PzNew		 Pa	     Zone pressure array
C I   2
C I   3	 Fma		  kg/s	      Massflow vector
C
Ch*********************************************************************

        IMPLICIT NONE
	 include 'comv-par.inc'

C Declarations Global

         INTEGER           nl,FromTo(2,maxl), pLiLDat(maxl),Njunc
         INTEGER           JuncNr(MaxZ,5),KEY
         REAL              Fma(*), DpJ(maxl),LDat(maxd),RhoL(2,maxl)
         DOUBLE PRECISION  PzNew(maxz)

C Declarations Local:

         INTEGER           LNr, I,j
         REAL              DHydr, DpTJ(3),FVoJ(3),RholJ(3)

C Initialisations:
	DO 50 I=1,Nl
	  DpJ(I)=0
50	CONTINUE

C For all T-junction nodes:
	DO 100 I=1,Njunc

C per T-junct Link :
	   DO 110 J=1,3
	     Lnr=JuncNr(I,J)
	     DpTJ(J)=0

C If Fma >0 then Rho = RhoL(1,..) else Rho= RhoL(2,..)	<--- this has to
C								 be checked
	     IF (Fma(Lnr).GT.0) THEN
		   RhoLJ(J)=RhoL(1,Lnr)
	     ELSE
		   RhoLJ(J)=RhoL(2,Lnr)
	     ENDIF

C Check for from/to of each link and adapt FVoJ(i)
C If T-junc node= TO node : FVoJ(i)=   Fma/RhoLJ(i)
C else			    FVoJ(i)= - Fma/RhoLJ(i)

	     IF (FROMTO(2,Lnr).NE.JuncNr(I,5)) THEN
		FVoJ(J) = -Fma(JuncNr(I,J))/RhoLJ(J)
	     ELSE
		FVoJ(J)=  Fma(JuncNr(I,J))/RhoLJ(J)
	     ENDIF

110	   CONTINUE

	   DHydr= Ldat(pLiLDat(Lnr)+9)

	CALL LOSTEE (SNGL(PZNew(JuncNr(I,5))),FVoJ,RhoLJ,DHydr,DpTJ,KEY)

C	   Fill DpJ array from DpTJ data

	   DO 120 J=1,3
C Check From/To direction and process with DpTJ accordingly
	     Lnr=JuncNr(I,J)
	     IF (FROMTO(2,Lnr).NE.JuncNr(I,5)) THEN
  	       DpJ(Lnr)=DpJ(Lnr) + DpTJ(J)
	     ELSE
  	       DpJ(Lnr)=DpJ(Lnr) - DpTJ(J)
	     ENDIF
120	   CONTINUE
100	CONTINUE
C
	END
