C+*********************************************************** comv-new.f
Ch***********************************************************************

      SUBROUTINE NEWPR (Nz, PzNew, PZOLD, CORR, SCALAR)

C***********************************************************************
C Purpose: NEWPR determines the new pressures for the zones
C CALCULATE PzNew(*) = PZOLD(*) + SCALAR * CORR(*)
C
C Module : #4.4.6, TG V, MKH/February 6, 1989
C Changes: June 20, 1989, minor changes to fit RELAX
C Limits : N/A
C
C Pass parameters:
C
C IO # Name	         Units    Description
C I  1 Nz            [-]      Number of zones
C IO 2 PzNew(maxz)   [Pa]     New pressure for each room
C  O 3 PZOLD(maxz)   [Pa]     Old pressure for each room
C I  4 CORR(maxz)    [Pa]     Newton corrections of old pressures
C I  5 SCALAR        [-]      Relaxation
C
C ERROR RETURN IF: none
Ch***********************************************************************

      IMPLICIT NONE
      include 'comv-par.inc'

      INTEGER Nz
      DOUBLE PRECISION PzNew(maxz), PZOLD(maxz), CORR(maxz), SCALAR
      INTEGER I

      DO I = 1, Nz
         PzNew(I) = PZOLD(I) + SCALAR * CORR(I)
      ENDDO

      RETURN
      END

