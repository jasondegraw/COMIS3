C+*********************************************************** comv-gr4.f
C@tno jcp 1996Mar29_15:47:44 new histogram calc routine
Ch***********************************************************************
C@empa aw 2000apr25 Call directly with the interval (dt) not with the actual time
CC        SUBROUTINE HistCalc(time)
        SUBROUTINE HistCalc(dt)
C pass parameter: time; uses common-Blocks for access
C***********************************************************************
C Purpose: Calculate the histogram for H-Output Option
C          called once per time loop in WriGrOut (if IsHOpt=TRUE)
C          And from Poltrans
C
C@tno jcp 1996Apr01_15:48:19
C Module : GrOut.F
C Changes:
C@empa aw 2000feb08 Handling of the occupant option is done here now and no longer in GetData
C                   If there is an occupant, we loop for all zones and weight the values with
C                   the number of occupants.
C                   Occupants are only allowed with histogram (H-option) now and no longer 
C                   with time series (S-option). 
C                   
C Limits :
C@tno jcp 1996Apr02_10:39:15 this HistCalc should use the average value for the
C just passed interval because the first  value at simulation start is not
C taken into account now (NotFirst=False then)
C
C the histogram starts at the element in HisotAr given by HistUsed(i,1), where
C i is the sequence number of individual histograms at &-PR-OUTPut
C start+0       =class 1                    |
C start+Nclass-1=class Nclass               |
C                                           | this is the normal histogram
C start+Nclass  =below range                |
C start+Nclass+1=above range                |

C start+Nclass+2=maximum found in any class | useful at plotting of this
C                of this histogram          | histogram

C start+Nclass+3=total of all summed values | for calculation of the average
C                stored in classes          |

C start+Nclass+4=total time of the summed   | used to normalise the histogram
C                time intervals             |

C start+Nclass+5=minimum value              | for advice on the range for
C start+Nclass+6=maximum value              | future runs / just info

C start+Nclass+7= free                      | for future use ?
C start+Nclass+8= free                      |  ,,

C start+Nclass+ 9=conversion offset         | for immediate conversion of all
C start+Nclass+10=conversion multiplier     | (SI)values that go into the
C                                           | histogram
C                                           | UserUnit=SI*multiplier+offset
C
C example: call HistCalc(time)
C call
C IO # Name	  unit		    description
C I  1 time	  [-]		    current timestep of the simulation
C
C USES:
Ch***********************************************************************

        IMPLICIT NONE

	  INCLUDE 'comv-inp.inc'
        INCLUDE 'comv-uni.inc'

        REAL GetData
C@NBI PGS 2000Aug02 - "time", "limit" and "rlimitval" unused so removed from decralations
CC      DOUBLE PRECISION time

        INTEGER i,start,iclass,type,nclass,dir
C@empa aw 2000feb08 
        INTEGER idx,StartIdx,EndIdx,ONum,nr,StPoint,point
        DOUBLE PRECISION DeltaT
C@empa aw 2000apr25 dt, OldValue
        REAL dt,OldValue
        REAL ActValue,lower,upper,width,CurValue,Max
        REAL Offset,Multiply,minimum,maximum
CC      REAL Rlimitval
C@empa aw 2000apr20 
CC        Logical NotFirstH
CC        Character*2 KeyWrd
        Character*4 KeyWrd

C ------------------------------------------------------------------------
c         write(cof,*) 'in histcalc, polqocc=',polqocc(1)

        i=1
C@empa aw 2000apr25 Call directly with the interval (dt), not with the actual time.
CC        if (OldH .eq. 0.0) then
CC          NotFirstH=.FALSE.
CCc          write(cof,*)'time=', time,'delta=',0
CC        else
CC          NotFirstH=.TRUE.
CC          DeltaT=ABS(time-OldH)
CCc          write(cof,*)'time=', time,'delta=',deltat
CC        endif
CC        OldH=time
CC
        DeltaT=dt/(86400.D0)

100     Continue
c        write(cof,*) 'Histogram',i
C         i=histogram nr
C              keyWrd sequence number
c           write(*,*) 'in histcalc, histogram',i
c          write(*,*) ' '
c          pause
          if ((IOOptH(1,i).eq.0).or.(i.gt.MaxOOH)) goto 110
c          write(cof,*) 'Histogram',i,'exists'
CC            if (NotFirstH) then

C             startpos in Hist,HistType
              start =HistUsed(i,1)

              type  =HistUsed(i,2)
              Nclass=HistoX(type,1)
              lower =HistoX(type,2)
              upper =HistoX(type,3)
              width =(upper-lower)/(nclass-1)

              Minimum =HistoAr(start+Nclass+ 5)
              Maximum =HistoAr(start+Nclass+ 6)

              Offset  =HistoAr(start+Nclass+ 9)
              Multiply=HistoAr(start+Nclass+10)

              dir=1
              keywrd=KeyW(IOOptH(1,i))(1:2)
C@empa aw 2000apr20 add '-H' to the Keyword
              if ((keywrd(1:1).eq.'C') .or.
     &           ((keywrd(1:1).eq.'F').and.
     &           (index('123456789',Keywrd(2:2)).ne.0)))then
                 keywrd(3:4)='-H'
              endif
c              call ho('voor act value in histcalc','')
C@empa aw 2000feb08 we divide here occupants or others
              nr=IOOptH(2,i)
C             pointer to the array ROOptH with values at the start of the intervall	
              StPoint=IOOptH(3,i)
              if (nr.gt.0) then
                StartIdx=nr
	          EndIdx=nr
              else
	          StartIdx=1
	          EndIdx=nz
              endif 
C@empa aw 2000feb08 loop for all zones if it is an occupant
C@empa DeltaT will be multiplide with ONum (Number of occupants) 
              do 200 idx=startIdx,EndIdx
                if (nr.lt.0) then
                   ONum=OccNum(-nr,idx)
                   point=StPoint+idx-1
                else 
                   ONum=1
	             point=StPoint
                endif     
                if ((nr.gt.0).or.((nr.lt.0).and.(ONum.gt.0))) then           


                ActValue=GetData(
     &            KeyWrd,
C                 keyword like C1 FZ EF etc
C@empa aw 2000feb08 take idx
CC                IOOptH(2,i)
     &            idx,
C                 zone, link number(positive) or occupant nr (as negative integer)
C@NBI PGS 2000Aug02 - (Hist)type is now a redundant argument after change by
C                     empa aw 2000jul03 (COMV-GR2.FOR), so can now be removed again.
CC   &            dir,
     &            dir)
C                 flowdirection (not used here)
CC   &            type)

C                 type of the histogram (necessary to get the roomsize and occupant
C                 dependency.
c                 write(*,*) 'HistCalc Keyword ',KeyWrd
c                 write(*,*) 'HistCalc actvalue',actvalue
c                 write(*,*) 'HistCalc offset, multiply ',offset,multiply

                  if (ActValue.lt.REALMax) then
                    ActValue=(offset+Multiply*ActValue)
                  end if

c                 write(cof,*) 'time',(time-int(time))*3600.*24.
c                 write(cof,*) 'occ act /num'
c                 write(cof,*) 'zone 1    zone 2 previous'
c                 write(cof,*) Loccact(1,1),Loccact(2,1)
c                 write(cof,*) Loccnum(1,1),Loccnum(2,1)
c                 write(cof,*) 'zone 1    zone 2 next'
c                 write(cof,*) occact(1,1),occact(2,1)
c                 write(cof,*) occnum(1,1),occnum(2,1)
c                 write(*,*) 'HistCalc start interval actvalue',ROoptH(i)
c                 write(*,*) 'HistCalc end   interval actvalue',actvalue
c                 call ho('na act value in histcalc','')

C                 we want to use the average value over this interval.
C                 the value at the start has been stored in ROoptH(i) (i=histogram sequence numb
C@empa aw 2000feb09 ROoptH has one value per histogram and zone, if we use occupant,
C@empa              therefore we need a new pointer 
CC                  ActValue=(ROoptH(i)+ActValue)/2
C@empa aw 2000apr25 Store ActValue in ROoptH for the use in the next interval
                  OldValue=ActValue
                  ActValue=(ROoptH(point)+ActValue)/2
                  ROoptH(point)=OldValue


c                 write(cof,*) 'HistCalc average actvalue',actvalue
c                 write(cof,*) 'Dt=',DeltaT*3600.*24.
c                 write(*,*) ' '
c                 pause

                  if (actValue.lt.Minimum) then
                    Minimum=actValue
                    HistoAr(start+Nclass+ 5)=Minimum
                  end if
                  if (actValue.gt.Maximum) then
                    Maximum=actValue
                    HistoAr(start+Nclass+ 6)=Maximum
                  end if
C                 get the limit value flow (for FZ, EF) or concentration (for Cn)
c                 call ho('voor RLimit in histcalc','')
C@empa aw 2000feb09 we don't evaluate excess values any more
CC                  Limit=RLimitVal(i)
c                 call ho('na RLimit in histcalc','')
CC                  If (KeyWrd(1:1).ne.'C') then
C                   not Cn: so limit is a flow rate
C                   sum too low flowrate (flow below LimitFlow)
CC                    if (actvalue.lt.Limit) then
CC                      HistoAr(start+Nclass+ 7)=HistoAr(start+Nclass+ 7)+
CC     &                DeltaT*ONum*(Limit-ActValue)
CC                    end if
CC                  else
C                   it is a concentration
C                   sum excess concentration
CC                    if (actvalue.gt.Limit) then
CC                      HistoAr(start+Nclass+ 7)=HistoAr(start+Nclass+ 7)+
CC     &                DeltaT*ONum*(ActValue-Limit)
CC                    end if
CC                  end if
c                 call ho('na excess value in histcalc','')

C                 could be wide over the range of an integer
                  if (ActValue.gt.(Upper+Width)) then
                    iclass=Nclass
                  else if(ActValue.lt.(Lower-Width)) then
                    iclass=-1
                  else
C                   close to the histogram no fear for overflow
                    iclass=(actvalue-lower)/width
                    iclass=INT(iclass)
                  end if

c                 write(cof,*) 'iclass=',iclass
c                 write(cof,*) 'conc=',actvalue
c                 write(cof,*) 'lower=',lower,' width=',width,'i=',iclass
C                 in range: iclass 0..Nclass-1; below:iclass<0 ; above:iclass>=Nclas

C                 below: store at start+Nclass
                  if (iclass.lt.0) then
                    iclass=Nclass
C                   above: store at start+Nclass+1
                  else if (iclass.ge.Nclass) then
                    iclass=Nclass+1
                  end if

C                 store in range and out of range values
                  iclass = iclass + start
                  HistoAr(iclass)=HistoAr(iclass)+DeltaT*ONum
c                 write(cof,*) 'iclass=',iclass

                  if (iclass.lt.(start+Nclass)) then
C                   this one was in range
C                   keep the Current summed value
                    CurValue=HistoAr(iclass)
C                   reuse iclass for start+nclass
                    iclass=start+nclass
C                   what is the maximum per class so far
C@tno jcp 1996Jul17_17:46:53 wrong
                    Max=HistoAr(iclass+2)
c                   write(cof,*)'Histogram',i,' Value=',ActValue,' iclass=',iclass
c                   write(cof,*)'MaxFreq=',Max,' Current max=',CurValue
c                   call ho('','')
C                   is the current value larger? then replace the maximum
                    if (Max.lt.CurValue) then
                      HistoAr(iclass+2)=CurValue
                    endif
                  else
C                   reuse iclass for start+nclass
                    iclass=start+nclass
                  end if

C                 total sum of DeltaT*Value (includes overflow values!)
                  HistoAr(iclass+3)=HistoAr(iclass+3)+
     &            DeltaT*ONum*ActValue
C                 total sum time passed since simulation start
                  HistoAr(iclass+4)=HistoAr(iclass+4)+deltaT*ONum
                endif                                                
200           continue 
C@empa aw 2000apr20 endif is obsolet with bugfix
CC            endif
CCC           ^end if of NotFirstH
          i=i+1
        goto 100

110     Continue

	RETURN
	END



Ch***********************************************************************
C@empa aw 2000mar08 RLimitVal is obsolete now, excess values are not explicitly
C                   evaluated any more. The user can read them from normal histogram
C                   output. (sum of freqencies of all classes above the limit)
C        REAL FUNCTION RLimitVal(ihist)
Ch***********************************************************************





Ch***********************************************************************
C@NBI PGS 2000Jul16 - This FUNCTION is retired, as it did exactly the same
CC                    thing as FUNCTION LenStr() !?!.  All calls to LastChar()
CC                    have now been redirected to LenStr().
CC      INTEGER FUNCTION LastChar(string)
Ch***********************************************************************





Ch***********************************************************************
	SUBROUTINE ClGrOut

C pass parameter: none; uses common-Blocks for access
C***********************************************************************
C Purpose: Closes the appropriate *.COS Files
C	   called once after the time loop in COMIS
C
C July 1990 wpk
C Module : GrOut.FOR
C Changes:
C Limits :
C
C
C example: call ClGrout
C call
C USES:
Ch***********************************************************************

        IMPLICIT NONE
	INCLUDE 'comv-inp.inc'
        INTEGER i

C ------------------------------------------------------------------------


          do 110 i=1,NumKeyW
            if (COSLine(i) .gt. 0) then
              CLOSE(COSlun(i))
            endif
110	  continue

	RETURN
        END


Ch***********************************************************************
	SUBROUTINE TOptCalc(time)

C pass parameter: time; uses common-Blocks for access
C***********************************************************************
C Purpose: Calculate the mean values for T-Output Option
C	   called once per time loop in WriGrOut
C
C Januar 1995 hf
C Module : GrOut.FOR
C Changes:
C Limits :
C@tno jcp 1996Apr02_09:34:22 this routine stores SI values (not user units)
C@NBI PGS 2000Aug02 - HistType is now a redundant argument after change by
C                     empa aw 2000jul03 (COMV-GR2.FOR), so can now be removed again.
C
C example: call TOptCalc(time)
C call
C IO # Name	  unit		    description
C I  1 time	  [-]		    current timestep of the simulation
C
C USES:
Ch***********************************************************************

        IMPLICIT NONE
        DOUBLE PRECISION time
        REAL GetData

	INCLUDE 'comv-inp.inc'
CC      INTEGER i,l,m,histtype
        INTEGER i,l,m
        DOUBLE PRECISION DeltaT
        REAL ActValue
        Logical NotFirstT

C ------------------------------------------------------------------------

        i=1
        if (OldT .eq. 0.0) then
          NotFirstT=.FALSE.
        else
          NotFirstT=.TRUE.
          DeltaT=ABS(time-OldT)
          SumDT=SumDT+DeltaT
        endif
        OldT=time

C@tno jcp 1996May24_09:37:04 this loop must be extended to work for option FW to
100     Continue
          if ((IOOptT(1,i).eq.0).or.(i.gt.MaxOOT)) goto 110
          if (KeyW(IOOptT(1,i))(1:2).eq.'FB') then
            if (NotFirstT) then
              do 10 l=0,Nz
                do 20 m=0,Nz
C sum deltaT weighted average during last DeltaT seconds into Qm
                  Qm(l,m)=Qm(l,m)+DeltaT*(Q(l,m)+Qold(l,m))/2
20              continue
10            continue
            endif
            do 30 l=0,Nz
              do 40 m=0,Nz
                Qold(l,m)=Q(l,m)
40            continue
30          continue
          else
C@tno jcp 1996Apr08_15:29:13 reset histtype for getdata (as this is no histogram
CC          histtype=0
            ActValue=GetData(KeyW(IOOptT(1,i))(1:2),IOOptT(2,i),
CC   &         IOOptT(3,i),HistType)
     &         IOOptT(3,i))
C@NBI PGS 1999Aug11 - Shouldn't really average two values of integrated 
C@NBI                 energy heat loss here, especially as the last timestep
C@NBI                 is of length 0 seconds.  But I have left it as it is
C@NBI                 for the time being
            if (NotFirstT) then
              ROOptT(1,i)=ROOptT(1,i)+(DeltaT*(ActValue+ROOptT(2,i))/2)
            endif
            ROOptT(2,i)=ActValue
          endif
          i=i+1
        goto 100

110     Continue

	RETURN
	END


Ch***********************************************************************
        INTEGER FUNCTION GetUnitNr(char)

C pass parameter: char
C***********************************************************************
C Purpose: Gets the number of user unit corresponding to the input character
C          which is the first character of output option keyword
C
C Januar 1995 hf
C Module : GrOut.FOR
C Changes:
C@empa aw 2000dec18 Number of user unit for concentration, source and sink is 
C@empa aw 2000dec18 individual per pollutant
C Limits :
C
C
C example:
C call
C IO # Name	  unit		    description
C I  1 char	  [-]		    First character of output option keyword
C  O F GetUnitNr  [-]		    Number of user unit
C USES:
Ch***********************************************************************
        IMPLICIT NONE
        include 'comv-par.inc'
C@empa aw 2000dec18 char has two characters now
        CHARACTER char*2
C@empa aw 2000dec18 np,eflag,intcon
        INTEGER i,np,eflag,intcon
C ------------------------------------------------------------------------

        if ((char(1:1).eq.'F').or.(char(1:1).eq.'I')) then
C         flow or infiltration
          i=UnitFma
        elseif (char(1:1).eq.'P') then
C         pressure
          i=UnitP
        elseif (char(1:1).eq.'T') then
C         temperature
          i=UnitTmul
        elseif (char(1:1).eq.'H') then
C         humidity
          i=UnitXh
        elseif (char(1:1).eq.'Q') then
C         pollutant source
          np=intcon(char(2:2),eflag)
          i=UnitPolSo+np
        elseif (char(1:1).eq.'S') then
C         pollutant sink
          np=intcon(char(2:2),eflag)
          i=UnitPolSi+np
        elseif (char(1:1).eq.'C') then
C         pollutant concentration
          np=intcon(char(2:2),eflag)
          i=UnitPolCo+np
        elseif (char(1:1).eq.'W') then
C         wind velocity
          i=UnitW
        elseif (char(1:1).eq.'A') then
C         airchange rate
          i=UnitRate
        elseif ((char(1:1).eq.'M').or.(char(1:1).eq.'N').or.
     &   (char(1:1).eq.'R')) then
C         mean age
          i=UnitAge
        elseif (char(1:1).eq.'L') then
C         Ventilation heatloss energy
          i=UnitE
        else
          i=NoUnit
        endif

        GetUnitNr=i

	RETURN

	END


Ch***********************************************************************
C@empa aw 2000apr04 FlowDir is now parameter of GetLiZoNa the differentiation is now
C                   made here
        CHARACTER*15 FUNCTION GetLiZoNa(KW,LiZoNr,FlowDir)

C pass parameter: KeyWStr
C***********************************************************************
C Purpose: Searches the link- or zone-name to the Link- or Zone-number
C
C
C Januar 1995 hf
C Module : GrOut.FOR
C Changes:
C@NBI PGS 2000Jul16 - Diplication. FUNCTION LastChar and LenStr do same thing.
C@NBI               - Converted couple of IF/ENDIF to in-line IFs
C Limits :
C
C
C example: GetLiZoNa('FL-',3)
C call
C IO # Name	  unit		    description
C I  1 KW   	  [-]		    Keyword for output option
C I  1 LiZoNr  	  [-]		    Sequence number of link or zone name
C  O F GetLiZoNa  [-]		    Name of link or zone
C
C USES:
Ch***********************************************************************
        IMPLICIT NONE
C@tno jcp 1996May09_18:22:11 increased LiZoNa to *20 as LiNa is *20 now
CC        CHARACTER KW*3, LiZoNa*15
        CHARACTER KW*3, LiZoNa*20,str*40
CC      INTEGER LiZoNr, lastchar, i ,lstr,FlowDir
        INTEGER LiZoNr, LenStr, i ,lstr,FlowDir

        INCLUDE 'comv-inp.inc'

C ------------------------------------------------------------------------

C search the Link-/Zonename for LiZoNa
        if (liZoNr.lt.0)  call intdis(-LiZoNr,str,lstr)
C@empa aw 2000feb01 if the second character is a number, then we have zones,
C@empa              that is true for C,Q,S,O,Y and F option 
        IF ((KW(2:2).EQ.'Z').OR.
CC     &     (INDEX('C Q S',KW(1:1)).NE.0)) THEN
     &     (INDEX('123456789',KW(2:2)).NE.0)) THEN
C@empa aw 2000apr04 FlowDir=3 indicates external nodes
          if (FlowDir.eq.3) then 
            LizoNa=ExNa(LiZoNr)
          else         
            if (lizonr.lt.0) then
              LiZoNa='OCC'//Str(1:Lstr)
            else
              LiZoNa=ZoNa(LiZoNr)
            endif
          endif 
        ELSEIF (KW(2:2).EQ.'L') THEN
          if (lizonr.lt.0) then
            LiZoNa='OCC'//Str(1:Lstr)
          else
C@empa aw 2000apr04 Differentiation FlowDir
CC          LiZoNa=LiNa(LiZoNr)
          if (FlowDir.eq.0) then
            LiZoNa=LiNa(LiZoNr)
          else
            if (FlowDir.eq.1) then
                  LiZoNa(1:1)='>'
            else if (FlowDir.eq.2) then
                  LiZoNa(1:1)='<'
            endif
            LiZoNa(2:15)=LiNa(LiZoNr)
          endif
C@empa aw 2000apr04 end patch
         
          endif
        ELSEIF (KW(2:2).EQ.'A') THEN
          LiZoNa='Ambient'
        ELSEIF (KW(2:2).EQ.'B') THEN
          LiZoNa='Building'
        ELSEIF (KW(1:2).EQ.'PE') THEN
          if (lizonr.lt.0) then
            LiZoNa='OCC'//Str(1:Lstr)
          else
            LiZoNa=ExNa(LiZoNr)
          endif
        ELSEIF (KW(1:2).EQ.'EF') THEN
          if (lizonr.lt.0) then
            LiZoNa='OCC'//Str(1:Lstr)
          else
            LiZoNa=ZoNa(LiZoNr)
          end if
        ELSE
          LiZoNa='Unknown'
        ENDIF

C@NBI PGS 2000Jul16 - Diplication. FUNCTION LastChar and LenStr do same thing.
CC      i=lastchar(LiZoNa)
        i=LenStr(LiZoNa)
        IF (i.le.0)  LiZoNa='?'
        GetLiZoNa=LiZoNa
	RETURN
	END


Ch***********************************************************************
	SUBROUTINE WriFlowMat(file,time,OutMean)

C***********************************************************************
C Purpose: Write flowmatrix to COS file every timestep or flowmatrix with
C          meanvalues at the last timestep
C Version: empa aw 1995feb27
C Module : GrOut.FOR
C Limits :
C
C
C
C IO # Name	  unit		    description
C I  1 file       [-]               file unit number for flow matrix output
C I  2 time	  [-]		    current timestep of the simulation
C I  3 OutMean    [-]               Flag to indicate output the meanvalues
C
C ERROR RETURN IF: none
Ch***********************************************************************

        IMPLICIT NONE
        DOUBLE PRECISION time

	INCLUDE 'comv-inp.inc'
        INTEGER LenStr

C local parameters
        INTEGER i,j,n,SepLen,Pos,Lstr
	character OutStr*(MCOSCol*16),SepStr1*10
C@tno jcp 1996May13_12:59:16 DTstring added
        character DTstring*31

        INTEGER file
        LOGICAL OutMean
C ------------------------------------------------------------------------

	    OutStr=' '
	    SepLen=Lenstr(SepStr)
	    if (SepLen.gt.2)then
	      SepStr1=Sepstr(2:Seplen-1)
	      SepLen=SepLen-2
	    else
	      SepLen=0
	    endif

           do 200 j=-1,Nz
           COutLine(1)=' '
            if (j.EQ.-1)then
C             first line
              if (OutMean) then
                write(COutLine(1),'(A15)')'Mean Values '
C@tno jcp 1996May13_13:21:18 here too write directly in the file
                write(file,'(A,A)',err=900) 'Mean Values',
     &                Ounit(UnitFma)
              else
C@tno jcp 1996May13_12:58:04 call TimeStr to convert to a readable date_time
                CALL TimeStr(Time,DTstring)
C@tno jcp 1996May13_13:10:50 fill up to 31 char
                lstr=lenstr(DTstring)
                if (lstr.lt.31) then
                  Call RptStr(lstr+1,31-Lstr,' ',DTstring)
                end if
C@tno jcp 1996May13_12:57:04 write DTstring in stead of time
CC               write(COutLine(1),'(E15.11E1)')time
C@tno jcp 1996May13_13:15:19 no more than 15 char !!!
CC                write(COutLine(1),'(A31)') DTstring
                write(COutLine(1),'(A15)') DTstring(1:15)
C@tno jcp 1996May13_13:17:11 just write a line in the file
                write(file,'(A,A,A)',err=900) DTstring,' ',
     &                Ounit(UnitFma)
              endif
              COutLine(2)='zone'
              COutLine(3)='ext'
              do 10  i=1,Nz
                COutLine(i+3)=ZoNa(i)
10            CONTINUE
            else if (j.GE.0) then
              if (j.EQ.0)then
C               second line
                COutLine(2)='ext'
              else
C               third line ... Nz'th line
                COutLine(2)=Zona(j)
              endif

              if (OutMean.and.(SumDT.gt.0.0)) then
                do 20  i=0,Nz
                  write (COutLine(i+3),'(E15.9)')
     &            Qm(j,i)/SumDt*ofact(UnitFma)
20              CONTINUE
              else
                do 30  i=0,Nz
                  write (COutLine(i+3),'(E15.9)')
     &            Q(j,i)*ofact(UnitFma)
30              CONTINUE
              endif
            endif
            Pos=1
C@tno jcp 1996May13_13:20:22 now we can omit the first column?
CC            do 40  n=1,Nz+3
            do 40  n=2,Nz+3
	      LStr=lenstr(CoutLine(n))
              if (Lstr.EQ.0) LStr=1
	      write(OutStr(Pos:Pos+14),'(A15)') COutLine(n)(1:LStr)
	      Pos=Pos+15
	      if (SepLen.gt.0)then
		OutStr(Pos:Pos+SepLen-1)=SepStr1(1:SepLen)
	      endif
	      Pos=Pos+SepLen
40 	    continue
	    write(file,'(A)',err=900) OutStr(1:Pos-1)
200        continue
           return

900	call inerr('&-PR-OUTPut: '//KeyW(i),
     &  'Error writing COS-file',.FALSE.,3)

        END
Ch***********************************************************************
        SUBROUTINE Wri2FlowMat(file,time,OutMean)

C***********************************************************************
C Purpose: Write flowmatrix to COS file every timestep or flowmatrix with
C          meanvalues at the last timestep
C          Flowmat= FmMat including external nodes as separate elements
C@tno jcp 1996May23_17:24:38
C Module : GrOut.FOR
C Limits :
C
C
C
C IO # Name	  unit		    description
C I  1 file       [-]               file unit number for flow matrix output
C I  2 time	  [-]		    current timestep of the simulation
C I  3 OutMean    [-]               Flag to indicate output the meanvalues
C
C ERROR RETURN IF: none
Ch***********************************************************************

        IMPLICIT NONE
        DOUBLE PRECISION time

	INCLUDE 'comv-inp.inc'
        INTEGER LenStr

C local parameters
        INTEGER i,j,n,SepLen,Pos,Lstr
	character OutStr*(MCOSCol*16),SepStr1*10
C@tno jcp 1996May13_12:59:16 DTstring added
        character DTstring*31

        INTEGER file
        LOGICAL OutMean
C ------------------------------------------------------------------------

        OutStr=' '
        SepLen=Lenstr(SepStr)
        if (SepLen.gt.2)then
          SepStr1=Sepstr(2:Seplen-1)
          SepLen=SepLen-2
        else
          SepLen=0
        endif

C-------header line with time-----------------------------------------
C call TimeStr to convert Time to a readable date_time in DTstring

        CALL TimeStr(Time,DTstring)


C fill DTstring up to 31 char
        lstr=lenstr(DTstring)
        if (lstr.lt.31) then
          Call RptStr(lstr+1,31-Lstr,' ',DTstring)
        end if

        if (OutMean) then
            write(file,'(4A)',err=900) 'Mean Values',
     &      DTstring(1:lenstr(DTstring)),' ',
     &      Ounit(UnitFma)(1:lenstr(Ounit(UnitFma))),
     &     ' No flows to Special pressures.'
        else
            write(file,'(4A)',err=900) DTstring(1:lenstr(DTstring)),' ',
     &      Ounit(UnitFma)(1:lenstr(Ounit(UnitFma))),
     &     ' No flows to Special pressures.'
        endif

C-------Column 0= zone/ext names --- Column 1..Nz..Nwind=flow rates
        do 200 j=0,Nz+Nwind
          if (j.EQ.0)then
              COutLine(1)='zoneID'
              do 10  i=1,Nz
                COutLine(i+1)=ZoNa(i)
10            CONTINUE
            do 11  i=1,Nwind
              COutLine(i+1+Nz)=ExNa(i)
11          CONTINUE

          else if (j.GE.1) then
            if (j.LE.Nz) then
C               2nd line ... Nz'th line
                COutLine(1)=ZoNa(j)
            else
                COutLine(1)=ExNa(j-Nz)
            end if

            if (OutMean.and.(SumDT.gt.0.0)) then
              do 20  i=1,Nz
                write (COutLine(i+1),'(1PE10.3)')
     &          FmMat(j,i)/SumDt*ofact(UnitFma)
20            CONTINUE
            else
              do 30  i=1,Nz+Nwind
                write (COutLine(i+1),'(1PE10.3)')
     &          FmMat(j,i)*ofact(UnitFma)
30            CONTINUE
            endif
          end if

          Pos=1
          do 40  n=1,Nz+1+Nwind
            LStr=lenstr(CoutLine(n))
            if (Lstr.EQ.0) LStr=1
            write(OutStr(Pos:Pos+9),'(A10)') COutLine(n)(1:LStr)
            Pos=Pos+10
            if (SepLen.gt.0)then
              OutStr(Pos:Pos+SepLen-1)=SepStr1(1:SepLen)
            endif
            Pos=Pos+SepLen
40        continue
	  write(file,'(A)',err=900) OutStr(1:Pos-1)
200     continue

        return

900	call inerr('&-PR-OUTPut: '//KeyW(i),
     &  'Error writing COS-file',.FALSE.,3)

        END
