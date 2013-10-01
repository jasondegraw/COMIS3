C+*********************************************************** comv-pos.f
Ch****************************************************************************
      SUBROUTINE PostCal
C*****************************************************************************
C*****************************************************************************
C Purpose: Sets up the flow matrix and calculates mean ages and building air
C          change rate
C
C VERSION: aw 1995mar
C Changes:
C@empa aw 1996jan EXINF added
C@tno jcp 1996May23_17:16:02 added to1, frm
C@NBI PGS 2000Aug18 - Tidied up a bit -- no syntax change
C*******************************************************************************

      IMPLICIT NONE
      INCLUDE 'comv-inp.inc'

      INTEGER To,From,index( MaxZ),i,j,L,to1,frm
      REAL    Qloc (MaxZ,MaxZ),vv(MaxZ),Qinv(MaxZ,MaxZ),Age(MaxZ,MaxZ)
      REAL    Exf,VB,Eps,EXINF
      LOGICAL errkey

C---------------
C     Initialize
C---------------

      Exf=0
      VB=0
      ACB=0
      DO i=1,Nz
         TauR(i)=0
      ENDDO
C     ! clear flowmatrix
      DO i=0,Nz
         DO j=0,Nz
            Q(i,j)=0
         ENDDO
      ENDDO
C     ! clear flowmatrix FmMat
      DO i=1,Nz+Nwind
         DO j=1,Nz+Nwind
            FmMat(i,j)=0
         ENDDO
      ENDDO

C--------------------------
C     Calculate flow matrix
C--------------------------

C     ! Put the total flow of zone into the flowmatrix
      DO I=1,Nz
         Q(i,i)=Ft(i)
C@tno jcp 1996May23_17:18:20 also for FmMat
         FmMat(i,i)=Ft(i)
      ENDDO

      DO 220 I=1,Nl

         FROM=FromTo(1,I)
         TO=FromTo(2,I)

C@tno jcp 1996May24_10:40:45 assign Frm and To1
         frm=from
         to1=to

C        ! pointer to the first coefficient of this link in Ldat
         L=Lstat(i)
         IF (L.EQ.0) THEN
C           ! fill up the flowmatrix
            Q(TO,FROM)=Q(TO,FROM)-FV2(1,i)
            Q(FROM,TO)=Q(FROM,TO)-FV2(2,i)
            FmMat(from,to)=FmMat(from,to)+Fv2(1,i)
            FmMat(to,from)=FmMat(to,from)+Fv2(2,i)
C@NBI PGS 1999Aug05 - Joined 3 mutually exclusive IF-ENDIFs using ELSEIF
C@NBI                 Makes code run a weeny bit faster
CC       ENDIF
CC       IF((L.EQ.1).OR.(L.EQ.2)) THEN
         ELSEIF((L.EQ.1).OR.(L.EQ.2)) THEN
C           ! save flow to outside
C           ! cp-->zn , sp-->zn
            Q(to,0)=Q(to,0)-Fv2(1,i)
            Q(0,to)=Q(0,to)-Fv2(2,i)
            frm=Nz+from
            if (frm.gt.0) then
               FmMat(frm,to)=FmMat(frm,to)+Fv2(1,i)
               FmMat(to,frm)=FmMat(to,frm)+Fv2(2,i)
            end if
CC       ENDIF
CC       IF((L.EQ.3).OR.(L.EQ.6)) THEN
         ELSEIF((L.EQ.3).OR.(L.EQ.6)) THEN
C@NBI PGS 1999Aug05   (end of conversion to ELSEIF)
C           ! save flow from outside
C           ! zn-->cp , zn-->sp
            Q(from,0)=Q(from,0)-Fv2(2,i)
            Q(0,from)=Q(0,from)-Fv2(1,i)
            to1=Nz+to
            if(to1.gt.0) then
               FmMat(from,to1)=FmMat(from,to1)+Fv2(1,i)
               FmMat(to1,from)=FmMat(to1,from)+Fv2(2,i)
            end if
         ENDIF
220   CONTINUE

      DO i=1,Nz
         Q(0,0)=Q(0,0)-Q(i,0)
         Exf=Exf-Q(0,i)
         VB=VB+Vz(i)
         ACB=ACB-Q(i,0)/Rhoz(i)
      ENDDO
      ACB=ACB*3600/VB
C@empa aw 1996jan30 Building ex-infiltration could be zero
      IF ((Q(0,0).NE.0).AND.(Exf.NE.0))THEN
         EXINF=Q(0,0)
      ELSE IF (Exf.NE.0)THEN
         EXINF=Exf
CC      ELSE
CC         EXINF=1.
      ENDIF
C@empa aw 2003jul03 Take the biggest zone flow if it is bigger than EXINF
C@empa aw 2003jul03 (because the solver balances only zone flows but not EXINF)
	DO i=1,Nz
	 IF (Q(i,i).GT.EXINF) EXINF=Q(i,i)
      ENDDO
	IF (EXINF.EQ.0.)EXINF=1.
CC    Eps=MAX(EpsFA/Q(0,0),EpsFR)
      Eps=MAX(EpsFA/EXINF,EpsFR)

c     ! now eps is the relative flow error used for the network pressure solver
C@tno jcp 1996Jul22_12:06:52 check here on twice that value
CC    Eps=MAX(Eps,1.E-5)
      Eps=MAX(Eps*2,1.E-5)

CC    IF ((ABS(Q(0,0)-Exf)/Q(0,0)).GT.Eps)THEN
C@empa aw 1996jan30 ex-infiltration
      IF ((ABS(Q(0,0)-Exf)/EXINF).GT.Eps)THEN
C@NBI PGS 1999Aug03 - Grammar
CC       CALL error2('Error in flowmatrix !','Building '//
CC     & 'exfiltration and infiltration is not equal !',2)
C@empa aw 2003jul03 Changed message to warning because this can occur even if 
C@empa aw 2003jul03 the solver found a correct solution
         CALL error2('Imbalance in flow matrix !','Building '//
     &   'exfiltration and infiltration are not equal',1)
      ENDIF

C---------------------------------------------------
C     Inverse flow matrix for age-of-air & mean ages  (optional)
C---------------------------------------------------

C@tno jcp 1996Jul11_11:46:55 tried to do this part only if needed flagged by
C the common InvMatrix, set in Readopt
      If(invMatrix.gt.0) then

C        ! Copy Q to Qloc array
         DO i=1,nz
            DO j= 1,nz
               Qloc(i,j) = Q(i,j)
            ENDDO
         ENDDO

C        ! Compute the age-of-air matrix and the mean ages of air
         CALL INVMAT (Qloc,Qinv,nz,MaxZ,index,vv,errkey)

C@tno jcp 1996Jul11_10:24:29 error message about flowmatrix extended
         IF (errkey) THEN
            call error2 ('Flow matrix is singular ! This might be '//
     &      'caused by a zone that has no ventilation at all in '//
     &      'this condition.',
     &      'Age of air and air change efficiency output is invalid !'
     &      ,2)
         else

C           mean ages of air
C           ----------------
            DO i=1,Nz
               DO j=1,Nz
                  Age(i,j)=Qinv(i,j)*Vz(j)*rhoz(j)
                  TauR(i)=Age(i,j)+TauR(i)
               ENDDO
            ENDDO

C           Arithmetic mean age of building
C           -------------------------------
            TauB=0
            DO i=1,Nz
               TauB=TauB+TauR(i)*Vz(i)
            ENDDO
            TauB=TauB/VB
         ENDIF
C        end if (errkey)
      end if
C     end if (InvMatrix)

      RETURN
      END

        
C@NBI PGS 1999Aug11 - New subroutine to check for condensation
Ch**********************************************************************

      SUBROUTINE CheckCondensation

C***********************************************************************
C Purpose: Checks for condensation
C          When coupled with TRNSYS, ideally an additional check
C          could be done for condensation according to surface
C          temperatures e.g. of windows.  Note that Xhz isn't
C          adjusted here to account for to moisture loss from air.
C          Note also that the H2O condensation in the pollutant
C          transport model is currently independent of Xhz, so
C          a separate check could be done for that too.
C Created: P.G.Schild 1999Aug11
C Changes: 
C Limits : Assumes surface temperatures are same as room air temp.
C Pass parameters:
C IO # Name      unit             description
Ch**********************************************************************
      IMPLICIT NONE
      INCLUDE 'comv-inp.inc'
      REAL RelHumid
      INTEGER i
      DO i=1,Nz                
        IF(outputoptions(4).EQ.1)THEN
          IF(RelHumid(Tz(i),REAL(Pz(i))+Pbz,Xhz(i)).GE.1.)THEN
            CALL ERROR('Condensation has occurred!',0)
            outputoptions(4)=0
          ENDIF
        ENDIF
      ENDDO
      END
      
