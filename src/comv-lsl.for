C+*********************************************************** comv-lsl.f
Ch************************************************************************

	SUBROUTINE LINSLV(A,B,CORR,LDA,N,IFAIL,KEY)

C***********************************************************************
C
C Purpose: LINSLV solves linear systems of equations that
C are symmetric positive definite, A*CORR=b or JAC*CORR=FMBNEW.
C A warning is given, KEY, for singular matrices and at which
C row, IFAIL, in the matrix the singularity was discovered.
C
C Module : #4.4.5, TG V, MKH/February 7, 1989
C Changes: June 12, 1989: Intoduction of skyline
C new routines SKYRO SKYCOL replace SKYLINE
C Limits : N/A
C
C Pass parameters:
C
C IO # Name	   unit	       description
C I   1 A(LDA,*)   [-]	       Matrix JAC
C I   2 B(*)	   [kg/s]      Flow balance vector
C I   3 LDA	   [-]	       Leading dimension of JAC (max)
C I   4 N	   [-]	       Order of JAC (actual)
C  O  5 CORR(*)	   [Pa]	       Correction vector
C  O  6 IFAIL	   [-]	       Row number with singularity
C  O  7 KEY	   [-]	       Error Key,  Error=1  OK=0
C
C
C ERROR RETURN IF: Singular matrix
Ch************************************************************************

        IMPLICIT NONE
	include 'comv-par.inc'

        INTEGER LDA,N,IFAIL,KEY
        DOUBLE PRECISION A(LDA,LDA),B(LDA),CORR(LDA)
        INTEGER SKYR(maxz),SKYC(maxz)


	KEY=0

C determine skylines automatiCALLy

	CALL SKYROW(A,LDA,N,SKYR)
	CALL SKYCOL(A,LDA,N,SKYC)

C CALL decomposition

	CALL CHOLES(A,LDA,N,SKYR,IFAIL)

	IF(IFAIL .NE. 0) THEN
	KEY=1
	GO TO 50
	ENDIF


* CALL forward and backward substitution

	CALL SUBSTI(A,LDA,N,B,CORR,SKYR,SKYC)

50	CONTINUE

	RETURN
	END

Ch***********************************************************************
	SUBROUTINE SKYROW(A,LDA,N,SKY)

C***********************************************************************
C Purpose: SKYROW determines the skyline of the lower triangular
C	   matrix of JAC by rows
C
C Module : #4.4.5.1, TG V, MKH/February 7, 1989
C Changes: As above
C SKYLINE replaced by SKYROW and SKYCOL
C Limits : N/A
C
C Pass parameters:
C
C IO Type  # Name	unit		description
C ---------------------------------------------------------------------
C I  D.P.  1 A(LDA,*)	[-]		Matrix 'JAC'
C I  INT   2 LDA	[-]		Leading dimension of JAC 'IJAC'
C I  INT   3 N		[-]		Order of JAC 'NN'
C  O INT   4 SKY	[-]		Skyline by rows of lower
C					triangular matrix of JAC
C
C ERROR RETURN IF: none
Ch***********************************************************************

        IMPLICIT NONE
        INTEGER LDA
        INTEGER N,SKY(LDA)
        DOUBLE PRECISION A(LDA,LDA)
        INTEGER I,J

	DO 5  I=1,N

	DO 10 J=1,N
	IF (A(I,J) .NE. 0.D0) THEN
	SKY(I)=J
	GOTO 5
	ENDIF
10	CONTINUE

5	CONTINUE

	RETURN
	END

Ch**********************************************************************
	SUBROUTINE SKYCOL(A,LDA,N,SKY)

C***********************************************************************
C Purpose: SKYCOL determines the skyline of the lower triangular
C	   matrix of JAC by columns
C
C Module : #4.4.5.1, TG V, MKH/June 28, 1989
C Changes: no
C Limits : N/A
C
C Pass parameters:
C
C IO Type  # Name	unit		description
C ---------------------------------------------------------------------
C I  D.P.  1 A(LDA,*)	[-]		Matrix 'JAC'
C I  INT   2 LDA	[-]		Leading dimension of JAC 'IJAC'
C I  INT   3 N		[-]		Order of JAC 'NN'
C  O INT   4 SKY	[-]		Skyline by columns of lower
C					triangular matrix of JAC
C
C ERROR RETURN IF: none
Ch***********************************************************************

        IMPLICIT NONE
        INTEGER LDA
        INTEGER N,SKY(LDA)
        DOUBLE PRECISION A(LDA,LDA)

        INTEGER I,J,K,STP

	STP=1

	DO 20 J=1,N
	K=N
	DO 30 I=N,STP,-1
	IF (A(I,J) .NE. 0.D0) GOTO 25
	K=K-1
30	CONTINUE

25	SKY(J)=K
	STP=K+1

20	CONTINUE

	RETURN
	END

Ch************************************************************************

	SUBROUTINE CHOLES(A,LDA,N,SKYR,IFAIL)

C***********************************************************************
C Purpose: CHOLES performes the decomposition
C
C Module : #4.4.5.2, TG V MKH/February 7, 1989
C Changes: July 12, 1989: Introduction of skyline
C	   Sept	 2, 1989: TEST changed in TESTC (Test is now in COMMON)
C@empa aw 1994apr19 J=0 causes a runtime error. The matrix is then probably
C		singular. This will be checked later.
C Limits : N/A
C
C Pass parameters:
C
C IO # Name	  unit		    description
C IO 1 A(LDA,*)	  [-]		    In: Matrix JAC, Out: Decomposition
C I  2 LDA	  [-]		    Leading dimension of A (max)
C I  3 N	  [-]		    Order of A (actual)
C I  4 SKYR(*)	  [-]		    Skyline by rows
C  O 5 IFAIL	  [-]		    Row number with singularity
C
C
C ERROR RETURN IF: Singular matrix
Ch************************************************************************


        IMPLICIT NONE
        INTEGER LDA
        INTEGER N,SKYR(LDA),IFAIL
        DOUBLE PRECISION A(LDA,LDA)

        INTEGER I,J,K,K1
        DOUBLE PRECISION SUMD,SUMR,TESTC

	DO 10 K=1,N
	K1=K-1
	IFAIL=K
	SUMD=0.D0

	DO 20 J=SKYR(K),K1
C J=0 causes a runtime error. The matrix is then probably
C		singular. This will be checked later.
	  IF (J.NE.0) THEN
	    SUMD=SUMD+A(K,J)*A(K,J)
	  ENDIF
20	CONTINUE
	TESTC=A(K,K)-SUMD
	IF(TESTC .LE. 0.) GOTO 50

C Store diagonal of decomposition in matrix A

	A(K,K)=SQRT(TESTC)

	DO 30 I=K+1,N
	  SUMR=0.D0

	  DO 40 J=SKYR(I),K1
C J=0 causes a runtime error. The matrix is then probably
C		singular. This will be checked later.
	    IF (J.NE.0) THEN
		SUMR=SUMR+A(I,J)*A(K,J)
	    ENDIF
40	  CONTINUE

C Store lower triangle of decomposition in matrix A

	  A(I,K)=(A(I,K)-SUMR)/A(K,K)
30	CONTINUE

10	CONTINUE
	IFAIL=0
50	CONTINUE

	RETURN
	END

Ch************************************************************************

	SUBROUTINE SUBSTI(A,LDA,N,BI,CORR,SKYR,SKYC)

C***********************************************************************
C Purpose: SUBSTI performes the back and forward substitutions
C
C Module : #4.4.5.3, TG V, MKH/February 7, 1989
C Changes: april 26 1989 hcp B(*) replaced by CORR(*) directly
C	   july 12, 1989: Introduction of skyline
C Limits : N/A
C
C Pass parameters:
C
C IO # Name	  unit		  description
C I  1 A(LDA,*)	  [-]		  Decomposition matrix
C I  2 LDA	  [-]		  Leading dimension of A (max)
C I  3 N	  [-]		  Order of A (actual)
C I  4 BI(*)	  [kg/s]	  Flow balance vector
C I  5 SKYR(*)	  [-]		  Skyline by rows
C I  6 SKYC(*)	  [-]		  Skyline by columns
C  O 7 CORR(*)	  [Pa]		  Correction vector
C
C
C ERROR RETURN IF: none
Ch************************************************************************


        IMPLICIT NONE
        INTEGER LDA,N,SKYR(LDA),SKYC(LDA)
        DOUBLE PRECISION A(LDA,LDA),BI(*),CORR(LDA)

        DOUBLE PRECISION SumF,SUMB
        INTEGER I,K

C Make a copy of the input BI(*) righthand vector into CORR(*)
C CORR(*) is changed during this solving process,
C but the passed BI(*) will stay unchanged

	DO 5 I=1,N
	CORR(I)=BI(I)
5	CONTINUE

C Forward Substitution of lower triangle

	DO 10 I=1,N
	SumF=0.D0

	DO 20 K=SKYR(I),I-1
	  SumF=SumF+A(I,K)*CORR(K)
20	CONTINUE
	CORR(I)=(CORR(I)-SumF)/A(I,I)
10	CONTINUE

C Backward Substitution of upper triangle, uses columns in the lower
C triangle, A(I,K)=A(K,I) due to symmetry

	DO 30 I=N,1,-1
	SUMB=0.D0

	DO 40 K=I+1,SKYC(I)
	  SUMB=SUMB+A(K,I)*CORR(K)
40	CONTINUE
	CORR(I)=(CORR(I)-SUMB)/A(I,I)
30	CONTINUE

	RETURN
	END
