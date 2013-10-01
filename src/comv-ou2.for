C+*********************************************************** comv-ou2.f
Ch**********************************************************************
C@empa aw 2000jan17 File unit is not a parameter any more
CC        SUBROUTINE HistOut(file)
        SUBROUTINE HistOut
C***********************************************************************
C
C
C Purpose: write the Histograms to COF
C
C created @tno jcp march 1996
C        write(file,*) '+0 freq. below range    ',histoar(ic+0)
C        write(file,*) '+1 freq. over  range    ',histoar(ic+1)
C        write(file,*) '+2 max freq. any class  ',histoar(ic+2)
C        write(file,*) '+3 time weighted average',histoar(ic+3),
C     &  Ounit(UnitNr)(1:6)
C        write(file,*) '+4 tot.simulated time(s)',histoar(ic+4)*3600*24
C        write(file,*) '+5 minimum              ',histoar(ic+5)
C        write(file,*) '+6 maximum              ',histoar(ic+6)
C        write(file,*) '+7 excess values        ',histoar(ic+7)
C        write(file,*) '+8 free                 ',histoar(ic+8)

C        write(file,*) '+9 User Unit offset     ',histoar(ic+9)
C        write(file,*) '+10User unit multiplier ',histoar(ic+10)
C
C changes:
C@NBI PGS 2000Aug16 - New local variables Len_COSs
C
C Pass parameters:
C
C       file      INTEGER       filedesc(channel)
Ch**********************************************************************
        IMPLICIT NONE
	INCLUDE 'comv-inp.inc'
        INCLUDE 'comv-uni.inc'

        INTEGER LenStr,Len_COSs
        INTEGER file, i,ic, Nclass,type,Iempty,
     &          MHistTy,WantedType,ThisType,Start,HistNr,
C@NBI PGS 2000Jul20 - "iRoomDep" & "iOccuDep" no longer used
CC   &   p0,p1,p3,p5,p6,p,iRoomDep,iOccuDep
     &   p0,p1,p3,p5,p6,p
C@empa aw 2000jan17 j,COHlun,l 
C@empa aw 2000mar13 Dep,intcon,eflag
         INTEGER j,COHlun,l,Dep,intcon,eflag
C@NBI PGS 2000Aug02 - "limit" and "rlimitval" no longer used so removed
        REAL Offset, Multiply,
     &        Lower,Upper,Width,Center,
     &        TimeT
C@empa aw 2000dec22 Ln*120 changed to Ln*200
C@empa aw 2000dec22 there should be a message added if lines are to long
        CHARACTER Ln*200,Name*20,unitstr1*11
C@empa aw 2000jan17 HType*2,fileNam*82
        CHARACTER HType*2,fileNam*82
C@empa aw 2000mar13 DepStr
C@NBI PGS 2000Jul20 - Was not strict FORTRAN standard; couldn't compile
CC      CHARACTER DepStr*60(6)
        CHARACTER DepStr(6)*60

        LOGICAL HeadSet
C@empa aw 2000jan11 TypeUsed
	  LOGICAL TypeUsed(MHisTy)

C@empa aw 2000mar13 DepStr
        DATA DepStr / 
     &   'room floor area',
     &   'room wall area',
     &   'room volume',
     &   'number of occupants in the room times their activity',
     &   'number of occupants in the room',
     &   'mean value of the activity of all occupants in the room'/

C p0..10 contain the amount of characters to shift the current printing
C position in Ln for format 1000..1010
        p0=12
        p1=12
        p3=12
        p5=12
        p6=12
C-----------------------------------------------------------------------
C@empa aw 2000jan11 initialize TypeUsed
        DO 50 i=1,MHisTy
	   TypeUsed(i)=.false.
50      CONTINUE
C@empa aw 2000jan17 initialize COHlun
        COHlun=StartFUnitForCOS
        MHistTy=0
        DO 1 i=1,MHisUs
          type=HistUsed(i,2)
          if (type.gt.0) then
C@empa aw 2000jan11 check for all used types
            TypeUsed(type)=.true.
            if (type.gt.MHistTy) MHistTy=type
          end if
1       continue

C normalize the histograms frequencies (but keep the total time untouched)
        CALL NormHist
C@empa aw 2000jan17 header is not needed anymore, as we have separate files for
C                   each histogram type
C write header out to COF
CC	  WRITE(file,*) ' '
CC	  WRITE(file,*) ' '
C@NBI PGS 1999May06 - Neater standardized output
CC          WRITE(file,*)
CC     &'**************************************************************'//
CC     &'*****************'
CC	  WRITE(file,*)
CC	  WRITE(file,*)
CC          WRITE(file,*) '*******************************'
CC          WRITE(file,*) 'Histogram output              *'
CC          WRITE(file,*) '*******************************'
CC	  WRITE(file,*) ' '
C@NBI PGS 1999May06   (end)
C@NBI PGS 2000Aug16 - Find length of COS & COH root file name:
              DO Len_COSs=1,72
                 IF (COSs(Len_COSs+1:Len_COSs+1) .EQ. ' ') GOTO 25
              ENDDO
25            continue
C@NBI PGS 2000Aug16   (end)

        DO 2 WantedType=1,MHistTy
C collect all used types

C as one line must contain all individual histograms (of the wanted type) for
C a class. the loop along the histograms must be inside the loop along the
C classes
          Nclass=HistoX(WantedType,1)
C@empa aw 2000jan11 Process only if this type is used
          if (TypeUsed(WantedType)) then

          if (Nclass.gt.1) then
C@empa aw 2000jan17 Open a file for this histogramtype
C@empa aw 2000nov30 last parameter is an output variable
CC              call intdis(WantedType,HType,2)
              call intdis(WantedType,HType,l)
CC	        l=lenstr(Htype)
C@NBI PGS 2000Aug16 - .COH naming code has been changed in line with the
C@NBI                 compact code for naming .COS files in module COMV-GRO.FOR
C@NBI                 Code for finding length of root filename moved up outside Nclass loop
CC|              fileNam(1:1)='H'
CC|	        fileNam(2:1+l)=HType(1:l)
CC|              i=1
CC|              j=2+l
CC|23            IF ((COSs(i:i) .EQ. ' ') .OR. (j .GE. 75) .or.
CC|     &          (COSs(i:i) .eq. '.')) GOTO 25
CC|                fileNam(j:j)=COSs(i:i)
CC|                i=i+1
CC|                j=j+1
CC|              goto 23
CC|
CC|25            continue
CC|           fileNam(j:j+3)='.cho'
              fileNam=COSs(1:Len_COSs)//'-H'//HType(1:l)//'.coh'
              file=COHlun 
C@empa aw 2000nov30 make line length larger
CC              OPEN(COHlun,file=fileNam,access='sequential',
              OPEN(COHlun,file=fileNam,access='sequential',recl=160,
     &        form='formatted',status='unknown',err=920)
	        COHlun=COHlun+1
              goto 27
C error opening COH file
920	           call inerr('Cannot open '//fileNam(1:lenstr(fileNam))  
     &           //' for writing Histogram '//Htype,'',
     &           .FALSE.,2)
              goto 5  

27          Lower =HistoX(WantedType,2)

            Upper =HistoX(WantedType,3)

            Width =(upper-lower)/(Nclass-1)

            HeadSet=.False.

C -2=title line -1 units 0=first class (Nclass-1)=last class the next are
C extra values (see below)
            Do 3 ic=-2,Nclass+10
              p=1
              Ln=' '
              if (ic.eq.-2) then
C fill the name of the first column in the table of histograms
                write(Ln(p:p+p1-1),1001) 'ClassCenter'
                p=p+p1
                Ln(p:p+4)='     '
                p=p+5
              else if (ic.eq.-1) then
C blanks if the first column (before the unit strings)
                write(Ln(p:p+p1-1),1001) '           '
                p=p+p1
                Ln(p:p+4)='     '
                p=p+5
              else if (ic.lt.Nclass) then
C fill the class center
                Center=Lower+(ic+0.5)*width
                call writer(Center,ln,p,p0)

              else
C fill the names of the 11 extra 'classes'
                GOTO (10,11,12,13,14,15,16,17,18,19,20) (ic-Nclass+1)
10              write(Ln(p:p+p1-1),1001) 'below_range'
                goto 30
11              write(Ln(p:p+p1-1),1001) 'over__range'
                goto 30
12              write(Ln(p:p+p1-1),1001) 'max_freq   '
                goto 30
13              write(Ln(p:p+p1-1),1001) 'average    '
                goto 30
14              write(Ln(p:p+p1-1),1001) 'total_time '
                goto 30
15              write(Ln(p:p+p1-1),1001) 'minimum    '
                goto 30
16              write(Ln(p:p+p1-1),1001) 'maximum    '
                goto 30
17              continue
C@empa aw 2000feb09 we don't evaluate excess values any more
CC                write(Ln(p:p+p1-1),1001) 'excess_vals'
                goto 30
18              continue
CC                write(Ln(p:p+p1-1),1001) 'Limit      '
                goto 30
19              write(Ln(p:p+p1-1),1001) 'Unit_offset'
                goto 30
20              write(Ln(p:p+p1-1),1001) 'Unit_mult. '
                goto 30
30              p=p+p1

              end if
              DO 4 HistNr=1,MHisUs
C loop in sequence along the used histograms to see if they have ThisType=Type
                ThisType=HistUsed(HistNr,2)
                if (ThisType.eq.WantedType) then
C yes this one must be output
C@NBI PGS 2000Nov24 - Can now define different I/O units for each pollutant,
C@NBI   Ounit(UnitPolCo+1) ... Ounit(UnitPolCo+5), for the 5 pollutant concentrations.
                  IF(HistUsedN(Histnr)(1:1).eq.'C')THEN
                    unitstr1=ounit(UnitPolCo
     &                      +intcon(HistUsedN(Histnr)(2:2),Eflag))                    
                  ELSE
C                                           keyword number
                    unitstr1=ounit(KWUnitNr(IOoptH(1,histnr)))
                  ENDIF
C@NBI PGS 2000Nov24   (end of patch)

                  start=HistUsed(HistNr,1)

                if (.NOT. headSet) then
                  HeadSet=.TRUE.

C          WRITE(file,*)
C     &'-----------------------------------------------'
C@empa aw 2000mar13 Dependendcy is stored in FSDep now
CC                  iRoomDep=HistoX(Thistype,4)
CC                  iOccuDep=HistoX(Thistype,5)
                  if (( ((Lower.gt.1e-3) .and. (lower.lt.1e+5)).or.
     &                   Lower+width/2.eq.0
     &                 ).and.(
     &                  ((Upper.gt.1e-3) .and. (Upper.lt.1e+5)).or.
     &             Upper+width/2.eq.0)) then

                  Write(file,2002) 'Histogram',ThisType,
     &             Nclass,' Classes',
     &           ' From',Lower+width/2,
     &           ' To',Upper+width/2, ' width=',width
                  else
                  Write(file,2000) 'Histogram',ThisType,
     &             Nclass,' Classes',
     &           ' From',Lower+width/2,
     &           ' To',Upper+width/2, ' width=',width
                  end if
C@empa aw 2000mar13 report dependency of fictive source
CC                    Write(file,2001)
CC     &             ' Room dependency=',iRoomDep,
CC     &             ' Occupant dependency=',iOccuDep
                  if((HistUsedN(Histnr)(1:1).eq.'F').and.
     &	        (index('123456789',HistUsedN(Histnr)(2:2)).ne.0))then
                    Dep=FSDep(intcon(HistUsedN(Histnr)(2:2),Eflag))
	              if (dep.eq.0) then
	                Write(file,2001)
     &                'Fictive source '// HistUsedN(Histnr)(1:2)//
     &                'has no dependency'
                    else
                      Write(file,2001)
     &                'Fictive source '// HistUsedN(Histnr)(1:2)//
     &                ' is dependent on: '
                      write(file,2001) DepStr(Dep)
                    endif
                  endif
C@empa aw 2000mar13 end of change
                end if
C name
                if (ic.eq.-2) then
                  name=HistUsedN(Histnr)
                  Write(ln(p:p+p3-1),1003) name(1:lenstr(name))
                  p=p+p3

C unit string
                else if (ic.eq.-1) then
                  Write(ln(p:p+p1-1),1001) unitstr1
                  p=p+p1

C Class1..Nclass frequencies
                else if (ic.lt.Nclass) then
                  call writer(HistoAr(start+ic),ln,p,p0)
C under range
                else if (ic.eq.Nclass) then
                  call writer(HistoAr(start+ic),ln,p,p0)
C over range
                else if (ic.eq.Nclass+1) then
                  call writer(HistoAr(start+ic),ln,p,p0)
C max freq in any class
                else if (ic.eq.Nclass+2) then
                  call writer(HistoAr(start+ic),ln,p,p0)
C average
                else if (ic.eq.Nclass+3) then
                  call writer(HistoAr(start+ic),ln,p,p0)
C time
                else if (ic.eq.Nclass+4) then
                  timeT=HistoAr(start+ic)*3600*24
                  if (TimeT.lt.1e11) then
                    Write(ln(p:p+p5-1),1005) timeT
                    p=p+p5
                  else
                    Write(ln(p:p+p6-1),1006) timeT
                    p=p+p6
                  end if

C min
                else if (ic.eq.Nclass+5) then
                  call writer(HistoAr(start+ic),ln,p,p0)

C max
                else if (ic.eq.Nclass+6) then
                  call writer(HistoAr(start+ic),ln,p,p0)
C@empa aw 2000feb09 We don't evaluate explicitly excess values any more
C excess
CC                else if (ic.eq.Nclass+7) then
CC                  call writer(HistoAr(start+ic),ln,p,p0)

C write here the limit that is not stored in HisAr
CC                else if (ic.eq.Nclass+8) then
CC                  Limit=RlimitVal(HistNr)
CC                  call Writer(limit,ln,p,p0)

C free_1
C               else if (ic.eq.Nclass+8) then

C unit conversion: offset
                else if (ic.eq.Nclass+9) then
C@empa aw 2000jan11 first get the offset
                  Offset=HistoAr(start+ic)
                  call writer(Offset,ln,p,p0)

C unit conversion: multiplier
                else if (ic.eq.Nclass+10) then
                  Multiply=HistoAr(start+ic)
                  call writer(Multiply,ln,p,p0)
                end if

              end if
4           continue

C@empa aw 2000jan17 unit is file not cof
CC            if (iempty(ln,1).eq.0) write(cof,*) Ln(1:lenstr(ln))
            if (iempty(ln,1).eq.0) write(file,*) Ln(1:lenstr(ln))
3         continue
C@empa aw 2000jan17 Label 5
5         continue   

          end if
C if Nclass >1
C@empa aw 2000jan11 Process only if this type is used
          end if
C if TypeUsed  

2       Continue

C end of routine
	WRITE(file,*) ' '
	WRITE(file,*) ' '
C text in column 0
1001   FORMAT(1X,11A)

C histogram names above the frequencies
1003   FORMAT(1X,11A)

C total time (seconds)
1005   FORMAT(1X,F11.1)
C total time (seconds) if >1e11
1006   FORMAT(1X,1PE11.4E2)



2000   FORMAT(A,I2,I4,A,2(A,1X,1PE11.2),A,1X,1PE11.2)
2001   FORMAT(2(A,1X,I3))
2002   FORMAT(A,I2,I4,A,2(A,1X,F11.5),A,1X,F11.5)

	return
	END

Ch**********************************************************************
        SUBROUTINE WriteR(R,Str,p,width)
C write REAL R in the string Str starting at P . The used width in the string is
C here fixed to 12 . The routine switches from F to E format.
Ch**********************************************************************

        IMPLICIT NONE
        REAL R
        Character*(*) str
        INTEGER P,width
        if (R.eq.0.0) then
          Write(str(p:p+width-1),5000) R
        else if (R.gt.1E-3 .and. R.lt.1e+5 .or. R.eq.0.0) then
          Write(str(p:p+width-1),1001) R
        else
          Write(str(p:p+width-1),1002) R
        end if
        p=p+width
        return
5000   FORMAT(1X,F7.1,4X)
1001   FORMAT(1X,F11.5)
C1001   FORMAT(1X,1PG11.4E2)
1002   FORMAT(1X,1PG11.4E2)
        end

Ch**********************************************************************
        SUBROUTINE NormHist
C***********************************************************************
C
C
C Purpose: normalize the histograms : SUM frequency=1.0
C          called by HistOut
C
C created @tno jcp march 1996
C
C changes:
C
C Pass parameters:
C
C       file      INTEGER       filedesc(channel)
Ch**********************************************************************
        IMPLICIT NONE
	INCLUDE 'comv-inp.inc'

        INTEGER ic
C local
        INTEGER ih, Nclass,histType
        REAL sumDeltaT

C-----------------------------------------------------------------------
C reset ih the sequence number for the histograms
        ih=1

C start of loop
100     Continue
C if no more or end of array then endloop
        if ((IOOptH(1,ih).eq.0).or.(ih.gt.MaxOOH)) then
          goto 110
        endif
C Findout the Namestring from IOOptH
        HistType=HistUsed(ih,2)
        HistStart=HistUsed(ih,1)
        Nclass=HistoX(HistType,1)
c        class1=HistoX(HistType,2)
        SumDeltaT=HistoAr(HistStart+Nclass+4)
        if (SumDeltaT.ne.0) then
C         normalise the histogram (divide 1..Nclass+3 by HistoAr(..+4)
          Do 120 ic=HistStart,Nclass+3+HistStart
            HistoAr(ic)=HistoAr(ic)/sumDeltaT
120       continue
C excess concentration cell needs normalisation and is number +7
          ic=Nclass+7+HistStart
          HistoAr(ic)=HistoAr(ic)/sumDeltaT
        end if

        ih=ih+1
C go back to start of loop
        goto 100

110     Continue
C end of routine

	return
	END
