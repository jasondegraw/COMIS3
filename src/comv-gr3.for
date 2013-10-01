C+*********************************************************** comv-gr3.f
CCCh***********************************************************************
C@NBI PGS 2000Jul20 - This subroutine wasn't used any more, so commented out
CC            SUBROUTINE GetOcc1Eflow(iocc,izone,Histtype,value)
CCC***********************************************************************
CCC Purpose: Get Effective flowrate for the occupant in zone izone
CCC          Called from GetData
CCC
CCC jcp 1996Apr06
CCC Module : Comv-Gr2.FOR
CCC Changes:
CCC Limits :
CCC
CCC
CCC example:call GetOcc1EFlow(2,5,value)
CCC value is effective flowrate for zone 2 accoring to the settings in Histogram 5
CCC call
CCC USES:
CCC
CCC IO # Name	  unit		    description
CCC I  1 izone	  [-]		    zone sequence number
CCC I  1 HistType   [-]               histogram type
CCC  O 2 Value	  [-]		    flowrate
CCC
CCCh***********************************************************************
CC
CC        IMPLICIT NONE
CC        include 'comv-inp.inc'
CC        include 'comv-uni.inc'
CC        INTEGER iocc,izone,HistType
CCC@tno jcp 1996Jul17_16:17:45 concentration difference
CC        REAL Value,weight,act1,num1,DC
CC        INTEGER OccuDep
CCC it is a zone
CC        if (HistType.eq.0) then
CCC this is not a histogram take the simple fixed fictive occu source flowrate
CC          DC=Cold(1,izone)-Cout(1)
CC          if (DC.gt.REALMin) then
CC            Value=PolQOcc(izone)/DC
CC          else
CCC C=0 then effective ventilation is infinite
CC            Value=REALMax
CC          end if
CC        else
CCC this IS a histogram take the fictive room source flowrate as indicated by
CCC the RoomDep parameter in HistoX(HistType,4)
CC          OccuDep=HistoX(HistType,5)
CC          DC=Cold(1,izone)-Cout(1)
CC          if (COld(1,izone).gt.REALMin) then
CC            if (histconc) then
CC              Act1=OccAct(iocc,izone)
CC              Num1=OccNum(iocc,izone)
CC            else
CC              Act1=LOccAct(iocc,izone)
CC              Num1=LOccNum(iocc,izone)
CC            end if
CC            if (OccuDep.eq.0)  then
CC              weight=1
CC            else if (OccuDep.eq.1)  then
CC              weight=act1/num1
CC            else if (OccuDep.eq.2)  then
CC              weight=num1
CC            else if (OccuDep.eq.3)  then
CC              weight=act1
CC            end if
CC            Value=PolQOcc(1)*weight/DC
CC          else
CCC C=0 then effective ventilation is infinite
CC            Value=REALMax
CC          end if
CC        end if
CC       return
CC       end
CC
CC
CC
Ch***********************************************************************
      SUBROUTINE SkipSpc(line,pointer)

C     pass parameter     1    2
C***********************************************************************
C Purpose: Sets pointer to next position in 'line' which is not a space;
C          sets pointer to LEN(line) if line contains only spaces from
C          pointer to the end of line
C
C July 1990 wpk
C Module : GrOut.FOR
C Changes:
C@empa aw 2000apr05 return if already end of line
C@NBI PGS 2000Jul16 - Rewritten & modified so that it also skips tabs
C Limits :
C
C example: call SkipSpc(line,1)
C call
C USES:
C
C IO  #  Name     Units  Description
C I   1  line     [-]    output file specification from *.CIF
C IO  2  pointer  [-]    pointer to the current character in line
C
C ERROR RETURN IF: none
Ch***********************************************************************

      IMPLICIT NONE
      character*(*) line
      INTEGER pointer
      DO WHILE( (pointer.lt.LEN(line))
     &    .AND. ( (line(pointer:pointer).eq.' ')
     &     .OR.   (line(pointer:pointer).eq.CHAR(9)) ) )
         pointer=pointer+1
      ENDDO
      return
      end


Ch***********************************************************************
      SUBROUTINE readNum(string,pointer,part,error,result,FlowDir)

C pass parameter # =	     1	    2	   3	 4     5      6
C***********************************************************************
C Purpose: Extracts an integer from 'string', starting at the position
C	   indicated by 'pointer';
C	   sets 'error' to .true. if no valid number could be extracted;
C
C July 1990 wpk
C Module : GrOut.FOR
C Changes:
C@empa hf/aw 1995mar15 New version with many changes:
C                       - Old markers deleted
C                       - New output parameter FlowDir added to indicate which
C                         flow (from-to or to-from) of the two way flow should
C                         be output.
C                       - Input line with link-, zone names and not sequence
C                         numbers any more.
C                       - Looking for names in the list of existing names with
C                         LookNam and give an error message if it does not
C                         exist.
C                       - Output parameter result is still the sequence number
C                         of the name
C                       - Logical function IsNum changed to IsNam to filter
C                         out the characters belonging to a name.
c@NBI PGS 1999May13 - Tidied up format (replaced tabs; no syntax changes)
C
C example: readNum(line,1,errFlag,number)
C call
C
C USES: IntCont (uti.f)
C
C IO # Name	  unit		    description
C I  1 string     [-]               input string that is supposed to contain INT
C IO 2 pointer	  [-]		    indicates current position within 'string'
C I  3 part       [-]               part of mkTab from which readnum is called
C                                     error messages only in part 0
C  O 4 error	  [-]		    error flag
C  O 5 result	  [-]		    result of conversion
C  O 6 FlowDir    [-]               direction of the flow in a link:
C                                     FlowDir=0  Netto Flow
C                                     FlowDir=1  From TO
C                                     FlowDir=2  To From
C
C ERROR RETURN IF: name does not exist
Ch***********************************************************************

        IMPLICIT NONE
        INCLUDE 'comv-inp.inc'
        INCLUDE 'comv-uni.inc'

        character string*160
C@tno jcp 1996Apr01_14:21:56 p1 added
        INTEGER pointer,p1,part
        logical error
C@tno jcp 1996Apr04_23:38:16 occnr added
C@empa aw 2000apr05 result2,key2 added
        INTEGER result, i, Key, OccNrH, Eflag,result2,Key2
        INTEGER FlowDir
        INTEGER IcIndex
        INTEGER Intcon
C@empa aw 1996jan08 K,L
        INTEGER K,L
C@empa aw 2000apr05 Lenstr
        INTEGER LenStr
        logical IsNam
C@empa aw 1996jan08 DumStr
CC      character*21 aNum
        character*21 aNum,DumStr

        result=0
        error=.false.
        aNum='      '
        Key=0
C@empa aw 26sep1995 initialize FlowDir
        FlowDir=0

        if (string(1:2).eq.'FL') then
          if (string(pointer:pointer).eq.'>') then
            FlowDir=1
            pointer=pointer+1
          else if (string(pointer:pointer).eq.'<') then
            FlowDir=2
            pointer=pointer+1
          else
           FlowDir=0
          endif
        endif
C@tno jcp 1996Apr01_14:23:11 this is the part before the range and use at error
        p1=pointer-1

        i=1
C@empa aw 2000apr05 return if end of line
        L=Len(string)
        
C@NBI PGS 2000Jul16 - Nested ..ELSE/IF/ENDIF/ENDIF -> ELSEIF bit more efficient
100     if (pointer.gt.L) then
           error=.true.
           pointer=L
           goto 10
CC      else 
CC          if (IsNam(string(pointer:pointer)) .and. (i .le. 20)) then
        ELSEIF (IsNam(string(pointer:pointer)) .and. (i .le. 20)) then
          aNum(i:i)=string(pointer:pointer)
          pointer=pointer+1
          i=i+1
          goto 100
CC      else
CC        if (i .eq. 1) then
        ELSEIF (i.eq.1) then
C           not a character in 1st pos. => skip
            pointer=pointer+1
            goto 100
CC        endif
CC      endif
C@NBI PGS 2000Jul16   (end)
        endif
C@empa aw 1996jan08 Call to GetWs because it may change the word to uppercase.
C@           As all names from CIF are read with GetWs, we have to do so
C@                  here to keep names comparable.
        DumStr=aNum
        K=0
        L=21
        call GetWs(DumStr,K,L,aNum,' ')
C@empa aw 1996jan08/end

        if (.not. error) then

          if ((i .gt. 20).and.(part.eq.0)) then
C           call ERROR2('Name too long  [&-PR-OUTPut]',string,3)
            error=.true.
          endif

C         Option is Zone or C1..C5
          if ( (string(2:2).eq.'Z')
     &    .or.(index('12345',string(2:2)).ne.0)) then
C@empa aw 2000apr03 EF is not a valid keyword any more
CC     &    .or.(String(1:2).eq.'EF')) then
c           write(cof,*) 'readnum detected zone/occ ',string
C@tno jcp 1996Apr01_11:49:58 add occupants names here in Histogram option
            if (ICindex(anum,'occ').eq.1) then
c             write(cof,*)'ReadNum occ=',anum
c             write(cof,*)'ReadNum occ=',anum
c             write(cof,*)'ReadNum occ=',anum
              OccNrH=Intcon(Anum(4:4),Eflag)
c             write(cof,*)'ReadNum occnr=',occnrH
              if (part.eq.0) then
                if ((OccNrH.gt.5).or.(OccNrH.lt.1)) then
C these errors come two times (part =0 and part=4 or 5)
C@empa aw 2000apr11
CC                  CALL InErr('&-PR-SIMU: '//
                  CALL InErr('&-PR-OUTPut: '//
     &            ' Occupant number: '//aNum(4:4)//' must be '//
     &            '1..5 !',' ',.TRUE.,2)
                end if
              end if
C code the occupants as numbers -1..-5
              result=-OccNrH
              if (OccNrH.gt.MaxOccNrH) MaxOccNrH=OccNrH
C             write(cof,*)'maxoccnrh=',maxoccnrh
            else
              CALL LookNam(Nz,aNum,ZoTree,ZoTreeN,
     &        result,ZoLarg,ZoSmal,key)
C@empa aw 2000apr04 enable also to look for external nodes
C             external node is marked with flowdir=3
              if (string(1:1).eq.'C') then
                if  (key.ne.0) then
                  CALL LookNam(Nwind,aNum,ExTree,ExTreeN,
     &            result,ExLarg,ExSmal,key)
                  flowdir=3
                  if ((key.ne.0).and.(part.eq.0)) then
C@NBI PGS 2000Jul16 - Grammar/clarity (no syntax change)
                    CALL InErr('&-PR-OUTPut: '//String(1:p1)//
     &              ': Zone name: '''//aNum(1:i)//''' does not exist '//
     &              'in &-NET-ZONes or &-NET-EXT !',' ',.TRUE.,2)
                  endif
                else
                  CALL LookNam(Nwind,aNum,ExTree,ExTreeN,
     &            result2,ExLarg,ExSmal,key2)
                  if ((key2.eq.0).and.(part.eq.0)) then
                    CALL InErr('&-PR-OUTPut: '//String(1:p1)//
     &              ': Zone name: '''//aNum(1:i-1)//''' exists '//
     &              'in both: &-NET-ZONes and &-NET-EXT.', 
     &              'Concentration output will be from zone: '''
     &               //aNum(1:i-1)//'''!',.TRUE.,1)
                  endif
                endif
              endif
            end if

          else if (string(2:2).eq.'E') then
            CALL LookNam(Nwind,aNum,ExTree,ExTreeN,
     &      result,ExLarg,ExSmal,key)
C@NBI PGS 2000Jul16 - Duplicated "IF(part.eq.0)" unncessary
CC          if (part.eq.0) then
            if ((key.ne.0).and.(part.eq.0)) then
              CALL InErr('&-PR-OUTPut: '//String(1:p1)//
     &        ': External zone name: '//aNum(1:i)//' does not exist'//
     &        ' in &-NET-EXT !',' ',.TRUE.,2)
            endif
CC          endif

          else if (string(2:2).eq.'L') then
            CALL LookNam(Nl,aNum,LiTree,LiTreeN,
     &      result,LiLarg,LiSmal,key)
C@NBI PGS 2000Jul16 - Duplicated "IF(part.eq.0)" unncessary
CC          if (part.eq.0) then
            if ((key.ne.0).and.(part.eq.0)) then
              CALL InErr('&-PR-OUTPut: '//String(1:p1)//
     &        ': Link name: '//aNum(1:i)//' does not exist '//
     &        'in &-NET-LINks !',' ',.TRUE.,2)
            endif
CC          endif
          else
            result=1
          endif

          if (Key .ne. 0) then
            error=.true.
          else
            error=.false.
          endif
        endif

C@empa aw 2000apr05 Label added
10      continue
        return
      end



Ch***********************************************************************
	logical function IsNam(charI)

C pass parameter # =		   1
C***********************************************************************
C Purpose: Checks whether or not 'char' is not TAB, ' ', ',' or '-'
C
C Version: empa aw 1995feb24
C Module : GrOut.FOR
C Changes:
C Limits :
C
C
C example: IF IsNam('9') GOTO 123
C call
C USES:
C
C IO # Name	  unit		    description
C I  1 charI	  [-]		    character that will be checked
C  O F IsNam	  [-]		    [function name] result of check
C
C ERROR RETURN IF: none
Ch***********************************************************************

        IMPLICIT NONE
	character charI


	if ((charI.eq.char(9)) .or. (charI.eq.char(44))
     &  .or. (charI.eq.char(45)).or.(charI.eq.char(32))) then
	  IsNam=.false.
	else
	  IsNam=.true.
	endif

	RETURN
	end


C@tno jcp 1996Mar29_15:47:44 store the values at start of an interval
Ch***********************************************************************
        SUBROUTINE ROoptHCalc

C uses common-Blocks for access
C***********************************************************************
C Purpose: ROoptHCalc is called just after the TimeStep, after NetSlv but
C          before Pollutant to store the values at the start of an interval.
C          The stored flowrates will not change during the interval (so start
C          and end value are the same) but for the concentrations there may be
C          a difference
C
C@tno jcp 1996Apr01_15:48:19
C Changes:
C@empa 2000feb09 Change for the occupant option: then we need an entry for each 
C                zone per histogramm
C
C example: call ROoptHCalc
C call
C IO # Name	  unit		    description
C
C USES:
Ch***********************************************************************


	INCLUDE 'comv-inp.inc'
        INCLUDE 'comv-uni.inc'

        INTEGER i,start,type,nclass,dir
C@empa aw 2000feb09
        INTEGER idx,StartIdx,EndIdx,nr,point,StPoint 
        REAL ActValue
        REAL Offset,Multiply
        REAL GetData

C ------------------------------------------------------------------------

        i=1
	  StPoint=1

100     Continue

C         i=histogram nr
C              keyWrd sequence number
c           write(*,*) 'in histcalc, histogram',i
c          write(*,*) ' '
c          pause
          if ((IOOptH(1,i).eq.0).or.(i.gt.MaxOOH)) goto 110

C             startpos in Hist,HistType
              start =HistUsed(i,1)

              type  =HistUsed(i,2)
              Nclass=HistoX(type,1)
c              lower =HistoX(type,2)
c              upper =HistoX(type,3)

              Offset  =HistoAr(start+Nclass+ 9)
              Multiply=HistoAr(start+Nclass+10)

C@empa aw 2000feb08 we divide here occupants or others
              nr=IOOptH(2,i)
C             pointer to the array ROOptH with values at the start of the intervall	
              IOOptH(3,i)=StPoint
              if (nr.gt.0) then
                StartIdx=nr
	          EndIdx=nr
              else
	          StartIdx=1
	          EndIdx=nz
              endif 
C@empa aw 2000feb08 loop for all zones if it is an occupant
              do 200 idx=startIdx,EndIdx
                if (nr.lt.0) then
                   point=StPoint+idx-1
                else 
	             point=StPoint
                endif     
                   if (point.gt.MaxOOH2)then
	                call inerr('Histograms need more memory. '//
     &                'use fewer histograms or increase MaxOOH ',
     &                '(in COMV-PAR.INC) and recompile.',.false.,3)
                   endif
              dir=1
              ActValue=GetData(
     &        KeyW(IOOptH(1,i))(1:2),
C             keyword like C1 FZ EF etc
C@empa aw 2000feb08 take idx
CC            IOOptH(2,i)
     &        idx,
C             zone, link number(positive) or occupant nr (as negative integer)
C@NBI PGS 2000Aug02 - (Hist)type is now a redundant argument after change by
C                     empa aw 2000jul03 (COMV-GR2.FOR), so can now be removed again.
CC   &        dir,
     &        dir)
C             flowdirection (not used here)
CC   &        type)

c             write(*,*) 'HistCalc actvalue',actvalue
c             write(*,*) 'HistCalc offset, multiply ',offset,multiply

              if (ActValue.lt.REALMax) then
                ActValue=(offset+Multiply*ActValue)
              end if

C@empa aw 2000feb09 ROoptH has one value per histogram and zone, if we use occupant,
C@empa              therefore we need a new pointer 
CC              ROoptH(i)=ActValue
              ROoptH(point)=ActValue
c              write(cof,*)'Roopth',i,ActValue
200           continue
              StPoint=point+1
          i=i+1
        goto 100

110     Continue

	RETURN
	END


