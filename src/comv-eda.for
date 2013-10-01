C+*********************************************************** comv-eda.f
Ch************************************************************************

      SUBROUTINE echodat(Filenm)

C pass parameter # =  1   2    3  4    5  6
C***********************************************************************
C Purpose: echodat echos the data for the ventilation network from
C   the parameters READ by READdat.
C
C sept 1989 hcp
C Module : This module is a temporay one and has no number
C Changes: april 25 , 1989
C CRT replaced by COF (46 times)
C suppressing blank lines from modelname
C new lines for solver explanation
C@lbl dml 1999nov19 Rename variable Newton in COMMON block /PRCONT/
C   to nNewt.
C@lbl dml 1999nov22 Replace solver control parameters noInit and useOpz
C   with stp1Init, stp2Init, and rhoSync.
C@lbl dml 1999nov22 Add output line for slvSel=6.
C
C Limits :
C
C Pass parameters:
C
C IO # Name    unit        description
C
C example:
C call
Ch************************************************************************

      IMPLICIT NONE
      INCLUDE 'comv-inp.inc'
      INCLUDE 'comv-uni.inc'

      CHARACTER*(*) Filenm
      INTEGER I,j,L,II

C@NBI PGS 2001Apr11 - This patch is unnecessary duplication, fills up output file
CC      IF (test.ge.1) THEN
CC        WRITE (COF,*) 'The input came from file ',Filenm
CC        WRITE (COF,*) 'Project description name :'
CCC@tno jcp 1996Jun27_17:54:11 call wrt80 for the proName
CC          call wrt80(cof,ProName,WCrt)
CC        WRITE (COF,*) 'Version name :'
CC        WRITE(COF,*) VerName
CC      ENDIF
C@NBI PGS 2001Apr11   (end of commented code)

      IF (Test.ge.1 .AND. iecho.ge.4) THEN
        WRITE(COF,*) ' '
CC      WRITE(COF,*) 'As test>=1 and IEcho>=4, here follows an '//
CC   &    'extended echo of the input'
        WRITE(COF,*) 'Solver type used=',SlvSel
C       !   The solver selection has been reduced to 1,3,5 (this is
C       ! checked in comv-inp.f)
        IF (slvsel.eq.0) THEN
          WRITE(COF,*) 'Solver=Optimum relaxation, Herrlin'
        ELSEIF (slvsel.eq.1) THEN
          WRITE(COF,*) 'Solver=Pure Newton'
        ELSEIF (slvsel.eq.2) THEN
          WRITE(COF,*) 'Solver=Newton Steffensen'
        ELSEIF (slvsel.eq.3) THEN
          WRITE(COF,*) 'Solver=Walton Steffensen'
        ELSEIF (slvsel.eq.4) THEN
          WRITE(COF,*) 'Solver=One Average Steffensen'
        ELSEIF (slvsel.eq.5) THEN
          WRITE(COF,*) 'Solver=Walton relax=0.5; 0.75 and 1.0'
        else if( slvSel .eq. 6 ) then
          write(cof,*) 'Solver=Simple trust region line search'
        END IF
        WRITE(COF,*) 'Relaxation factor used for the first Newton '//
     &     'iterations =',relaxV
        WRITE(COF,*) 'Stop criterion for Mass Flow Balance absolute=',
     &     EpsFA,'(kg/s)'
        WRITE(COF,*) 'Stop criterion for Mass Flow Balance relative=',
     &     EpsFR,'(kg/s / kg/s )'
        WRITE(COF,*) 'Stop criterion for Pressure vectors =',
     &     EpsCJ,'(Pa  )'
        WRITE(COF,*) 'Number of Newton iterations before optimum '//
     &     'relaxation',nNewt
CC      IF (NoInit.eq.0) THEN
CC         WRITE(COF,*) 'start with a linearization of the network '//
CC   &       'as pressure initialization'
CC      ELSE
CC         WRITE(COF,*) 'do not initialize the pressures'
CC      ENDIF
        if( stp1Init .eq. 0 ) then
          write(cof,*) 'Initialize first time step with zero pressures'
        else
          write(cof,*) 'Initialize first time step using '//
     &      'linearized network'
        endif
        if( stp2Init .eq. 0 ) then
          write(cof,*) 'Initialize subsequent time steps with zero '//
     &      'pressures'
        else if( stp2Init .eq. 1 ) then
          write(cof,*) 'Initialize subsequent time steps using '//
     &      'linearized network'
        else
          write(cof,*) 'Initialize subsequent time steps with '//
     &      'pressures from previous time step'
        endif
        if( rhoSync ) then
          write(cof,*) 'Update densities to reflect new pressures at '//
     &      'each solver iteration'
        else
          write(cof,*) 'Zone densities reflect initial pressure '//
     &      'estimates'
        endif
        WRITE (COF,*) 'Max number of iterations allowed=',mIter
        WRITE(COF,*) ' '
        WRITE(COF,*) 'the array Ldat(*)'
        WRITE(COF,*) 'nr   Ldat(nr)'
        DO 200 i=1,MLDat
           WRITE(COF,*) I,LDat(i)
200     CONTINUE
        WRITE(COF,*) ' '
        WRITE(COF,*) 'the array pUsrAfc(*) pointer from user',
     &     ' name to Ldat'
        WRITE(COF,*) 'ltyp  pUsrAfc(ltyp)'
        DO 300 i=1,Ntyp
           WRITE(COF,*) I,pUsrAfc(i)
300     CONTINUE
        WRITE(COF,*) ' '
        WRITE(COF,*) 'the array FromTo(*)'
C@tno jcp 1996May17_14:39:10 write statement line changed
        WRITE(COF,*) ' Lnr Linkname      from  to From$     To$'//
     &    '         linktyp startaddress Lstat'
        DO 400 i=1,Nl
          WRITE(COF,1011) I,LiNa(i),FromTo(1,i),FromTo(2,i),
     &      FromToS(1,i),FromToS(2,i),
     &      Ldat(pLiLdat(i)),pLiLDat(i),Lstat(i)
400     CONTINUE
        WRITE(COF,*) ' '
        WRITE(COF,*) 'the array Zl(*)'
C@tno jcp 1996May17_14:47:58 spaces added
        WRITE(COF,*) ' Lnr     Zfrom     Zto (m)   Filt(1...5)'
        DO 500 i=1,Nl
C         ! The first element of this link
          II=pLiLDat(I)
C         ! The linktype number (indicates which equation to use in FEQN)
          Ltyp=LDat(II)
C         ! Find II that points at the first Filter value of this link
          IF (LTyp.EQ.10) THEN
C           ! LDat(II+1) contains the Number of datapairs for TD
            II=II+2+2*LDat(II+1)+1
          ELSE IF ( Ltyp.EQ.9) THEN
C           ! Ldat(II+1) contains the numer of datalines for the opened
C           ! window
            II=II+14+8*LDat(II+1)
          ELSE
            II=II+SiStart(Ltyp)-1
          ENDIF
          WRITE(COF,1112) I,Zl(1,i),Zl(2,i),
     &      Ldat(II+0),
     &      Ldat(II+1),
     &      Ldat(II+2),
     &      Ldat(II+3),
     &      Ldat(II+4)
500     CONTINUE
        WRITE(COF,*) ' '
        WRITE(COF,*) 'the arrays Tz(*),Zz(*),Vz(*)'
C@tno jcp 1996May17_14:49:14 spaces added
        WRITE(COF,*) ' Znr      T (degC) Zzone(m) Vzone(m3),',
     &    'Xhum , Conc1..Conc5,Source1..5,Sink1..5'
        DO 600 i=1,Nz
          WRITE(COF,1113) I,Tz(i),Zz(I),Vz(I),Xhz(I),C(1,I),
     &      C(2,I),C(3,I),C(4,I),C(5,I)
          WRITE(COF,1114) Source(1,I),Source(2,I),
     &      Source(3,I),Source(4,I),Source(5,I)
          WRITE(COF,1114) Sink(1,I),Sink(2,I),
     &      Sink(3,I),Sink(4,I),Sink(5,I)
600     CONTINUE
        WRITE(COF,*) ' '
        WRITE(COF,*) 'the layers in zones:'
        WRITE(COF,*) 'zone layer start-pointer end-pointer'
        DO 605 i=1,Nz
          L=LayPtr(1,i)
          WRITE(COF,*) i,Layptr(1,i),LayPtr(2,I)
          IF (L.GT.0) THEN
601         WRITE(COF,1115) LayDat(L),Laydat(L+1),Laydat(L+2)
            WRITE(COF,1115) LayDat(L+3),Laydat(L+4),Laydat(L+5)
            WRITE(COF,1115) LayDat(L+6),Laydat(L+7),Laydat(L+8)
            L=L+9
C           ! Check if this was the last element
            IF (L.LT.Layptr(2,I)) GOTO 601
          ENDIF
605     CONTINUE
        WRITE(COF,*) 'pollutant 1...5 names'
        WRITE(COF,*) Cname(1),Cname(2),Cname(3),Cname(4),Cname(5)
        if (ptrW.gt.0) then
          j=1
          write(cof,*) ' WDat contains Wall Materials and Types'
          do 607 i=1,ptrW
            if (i.eq.WNind(j)) then
C             ! This line is the start of a wall material or walltype,
C             ! print its name
              write (cof,*) i,WDat(i),WName(j)
              if (j.lt.ptrWN) then
                j=j+1
              endif
            else
              write (cof,*) i,WDat(i)
            endif
 607      continue
        end if
        if (ptrWN.gt.0) then
          write(cof,*) ' index to WDat Wall Name (MAterials and Types)'
          do 608 i=1,ptrWN
            write (cof,*) i,WNind(i),WName(i)
 608      continue
        end if
        if (NWL.gt.0) then
          write(cof,*) ' WalLin contains the linking of walltypes ',
     &      '(&-NET-WALls)'
          write(cof,*) ' Waltype From To Orientation Area factor'
          write(cof,*) ' index '
          write(cof,*) ' to WDat '
          do 606 i=1,NWL
            j=(I-1)*5+1
            write (cof,1020) WalLin(j),WalLin(j+1),
     &        WalLin(j+2),WalLin(j+3)/10.,WalLin(j+4)/10.
 606      continue
        end if
      ENDIF
C     ! end if(test>=1 and Iecho>=4)

1020  FORMAT(1X,I5,I5,I5,F7.1,F7.1)

C@tno jcp 1996May17_14:41:55 1X added
1011  FORMAT(1X,I4,1X,A10,2(I6),1X,2(A10),F10.0,2(I6))
1112  FORMAT(1X,I4,7(F10.3))
1113  FORMAT(1X,I4,F10.2,F10.2,F10.2,F10.4,/,1X,5(E10.2))
1114  FORMAT(1X,5(E10.2))
1115  FORMAT(1X,3(F10.2))

      RETURN
      END
