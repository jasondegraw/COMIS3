C+*********************************************************** comv-ch2.f
Ch***********************************************************************

        SUBROUTINE CheckFan()

C***********************************************************************
C Purpose: CheckFan checks the characteristic of the fan
C          The polynomial curve must correspond with the linear fix outside
C          the pressure range Pmin..Pmax
C@tno jcp 1996May23_11:16:36
C
C Pass parameters:
C
C A summary of the contents of Dat(*):
C Dat(1)=2 means this link is a fan
C Dat(2)= Flag is needed for COMIN and not for COMIS
C Dat(3)= highest power in the polynomial, n
C Dat(4)= airdensity belonging to the fandata which gave these coefficients
C Dat(5)= fanspeed belonging to the fandata which gave these coefficients
C Dat(6)= Cm if the fan is off
C Dat(7)= Expn if the fan is off
C Dat(8)= Pmin range where the polynomial is valid
C Dat(9)= Pmax	,,    ,,
C Dat(10)= slope for extrapolation
C Dat(11)= intercept for extrapolation
C Dat(12)= C0 polynomial coefficient
C Dat(13)= C1......
C Dat(12+Dat(3))=Cn

C@tno jcp 1997Jul03_14:40:22
C Dat(17)=C5
C Dat(18)=Number of data points say 4 here
C Dat(19)=P1
C Dat(20)=Q1
C Dat(21)=P2
C Dat(22)=Q2
C Dat(23)=P3
C Dat(24)=Q3
C Dat(25)=P4
C Dat(26)=Q4
C Dat(27)=Filter coef pol1
C Dat(28)=Filter coef pol2
C Dat(29)=Filter coef pol3
C Dat(30)=Filter coef pol4
C Dat(31)=Filter coef pol5
C
C
C IO # Name   unit          description
C ERROR routine Inerr is called
Ch***********************************************************************

        IMPLICIT NONE
	INCLUDE 'comv-inp.inc'
        INCLUDE 'comv-uni.inc'
C@tno jcp 1997Jul03_23:12:11 added fma3 dp2
        DOUBLE PRECISION Fma1,Fma2,fma3,Dp,Dp1,Dp2
C@tno jcp 1997Jul03_23:27:23 added slope and intercept
        REAL fratio,mp1,Pcor,Pmin,Pmax,Dfma,slope,intercept,RhoI
C@tno jcp 1997Jul03_22:32:30
        REAL C0

C@tno jcp 1997Jul03_23:32:14 more lstr and str added
        INTEGER I,lstr,lstr1,lstr2,lstr3,start
        character*40 Str,Str1,Str2,Str3
        INTEGER LenStr
        logical p1,p2
C@tno jcp 1997Jul24_14:42:49 npoints added
        integer npoints,j
        real pres1,pres2
C@lbl bvs 1997Oct20 declare pos
	integer pos

        Fratio=1.0
        Mp1=1.0
        Pcor=1.0
        do 200 i=1,NL
          start=pLiLDat(i)
C@empa aw 2000jun30 only if linktype has been defined
          if (start.ne.0) THEN
          if (Ldat(start).eq.2) then
c this is a fan
C@tno jcp 1997Jul28_10:55:21 changed fan flag1 and 2 use coef, 3 use datapairs
CC           if (Ldat(start+1).eq.1) then
           if (Ldat(start+1).lt. 2.5) then
c use coef

            p1=.false.
            p2=.false.
            C0=ldat(start+12-1)
            pmin=LDat(start+8-1)
            pmax=LDat(start+9-1)
C@empa aw 2005apr27 get RhoI
	      RhoI=Ldat(start+3)


C@empa aw 1998jul07 Double precision
CC            Dp1=-(Pmin+1E-8)
            Dp1=-(DBLE(Pmin)+1d-8)
C this is a pressure in the poly
c            write (cof,*) 'Pmin=',Pmin
c            write (cof,*) 'Dp1=',Dp1

            CALL Fan (Fma1,DFma,Dp1,LDAT(start),Fratio,Mp1*PCor)
c            write (cof,*) 'fan',i,' pmin=',Dp1,'fma =',fma1

C@empa aw 1998jul07 Double precision
CC            Dp=-(Pmin-1E-8)
            Dp=-(DBLE(Pmin)-1d-8)
c this is in the linear part
c            write (cof,*) 'Dp=',Dp
            CALL Fan (Fma3,DFma,Dp,LDAT(start),Fratio,Mp1*PCor)
c            write (cof,*) 'fan',i,' pmin=', Dp,'fma =',fma3
C@tno jcp 1997Jul03_22:33:30 
CC            if (ABS(Fma1-fma2).gt.2E-5) then
            if (ABS(Fma1-fma3).gt.ABS(2.0E-5*C0)) then
              Call RelDis(Pmin,4,Str,Lstr,0)
              Call RelDis((2.0E-5*C0),4,Str1,Lstr1,1)
              CALL INERR('Link='//LiNa(I)(1:lenstr(Lina(i)))//
     &        ', type='//LiTyNa(i)(1:lenstr(LiTyNa(i)))//
     &        '. Linear and Polynomial '//
     &        'part don''t match at Pmin '//Str(1:Lstr)//' Pa',
     &        'Deviation is more than '//Str1(1:Lstr1)//' kg/s'//
     &        ' Pmin, Pmax, Slope and Intercept will be updated.'
     &        ,.FALSE.,1)
              p1=.true.
C@empa aw 2005apr28 this belongs to the above message therefore it should go to 
C@empa aw 2005apr28 CRT and CER instead of COF
C@empa aw 2005apr28 print -dp as dp is negative but the input values for pmin are positive  
CC              write (cof,*) 'At ',dp1,' Pa fma =',fma1
CC              write (cof,*) 'At ', dp,' Pa fma =',fma3
              write (CRT,*) 'At ',-dp1,' Pa fma =',fma1
              write (CRT,*) 'At ', -dp,' Pa fma =',fma3
              write (CER,*) 'At ',-dp1,' Pa fma =',fma1
              write (CER,*) 'At ', -dp,' Pa fma =',fma3

            end if

C@lbl bvs 11Feb1997 testing smaller value
C@empa aw 1998jul07 Double precision
CC            Dp1=-(Pmax-1E-5)
CC            Dp2=-(Pmax-1E-8)
            Dp2=-(DBLE(Pmax)-1d-8)
C this is in the curve
c            write (cof,*) 'Pmax=',Pmax
c            write (cof,*) 'Dp1=',Dp1
c            write (cof,*) 'Mp1=',Mp1
c            write (cof,*) 'Pcor=',Pcor
c            write (cof,*) 'Ldat10=',Ldat(start+10-1)
c            write (cof,*) 'Ldat11=',Ldat(start+11-1)
c            write (cof,*) 'Ldat12=',Ldat(start+12-1)
c            write (cof,*) 'Ldat13=',Ldat(start+13-1)
c            write (cof,*) 'Ldat14=',Ldat(start+14-1)
            CALL Fan (Fma2,DFma,Dp2,LDAT(start),Fratio,Mp1*PCor)
c              write (cof,*) 'fan',i,' pmax=',Dp1,'fma =',fma2

C@lbl bvs 11Feb1997 testing smaller value
C@empa aw 1998jul07 Double precision
CC            Dp=-(Pmax+1E-5)
CC            Dp=-(Pmax+1E-8)
            Dp=-(DBLE(Pmax)+1d-8)
c this is in the linear part
c            write (cof,*) 'Dp=',Dp
            CALL Fan (Fma3,DFma,Dp,LDAT(start),Fratio,Mp1*PCor)
c              write (cof,*) 'fan',i,' pmax=', Dp,'fma =',fma3
C@tno jcp 1997Jul03_22:34:55
CC            if (ABS(Fma3-fma2).gt.1E-5) then
            if (ABS(Fma3-fma2).gt.ABS(2.0E-5*C0)) then
              Call RelDis(Pmax,4,Str,Lstr,0)
              Call RelDis((2.0E-5*C0),4,Str1,Lstr1,1)
              CALL INERR('Link='//LiNa(I)(1:lenstr(Lina(i)))//
     &        ', type='//LiTyNa(i)(1:lenstr(LiTyNa(i)))//
     &        ' Linear and Polynomial '//
     &        'part don''t match at Pmax '//Str(1:Lstr)//' Pa',
     &        'Deviation is more than '//Str1(1:Lstr1)//' kg/s.'//
     &        ' Pmin, Pmax, Slope and Intercept will be updated.'
     &        ,.FALSE.,1)
              p2=.TRUE.
C@empa aw 2005apr28 this belongs to the above message therefore it should go to 
C@empa aw 2005apr28 CRT and CER instead of COF
C@empa aw 2005apr28 print -dp as dp is negative but the input values for pmax are positive  
CC              write (cof,*) 'At ',dp2,' Pa fma =',fma2
CC              write (cof,*) 'At ', dp,' Pa fma =',fma3
              write (CRT,*) 'At ',-dp2,' Pa fma =',fma2
              write (CRT,*) 'At ', -dp,' Pa fma =',fma3
              write (CER,*) 'At ',-dp2,' Pa fma =',fma2
              write (CER,*) 'At ', -dp,' Pa fma =',fma3

            end if

c            write (cof,*) 'link',i,' is a fan'
             if (p1 .or. p2) then
c              pmin=dp1
C@ema aw 1998jul07 sign changed
C               ldat(start+8-1)=dp1
               ldat(start+8-1)=-dp1
c              pmax=dp2
C               ldat(start+9-1)=dp2
               ldat(start+9-1)=-dp2
              if (dp1.ne.dp2) then
C@empa aw 1998jul14 sign changed 
C                slope=(fma2-fma1)/(dp2-dp1)
                slope=-(fma2-fma1)/(dp2-dp1)
              else
              end if
              if (dp1.eq.0) then
                intercept=fma1
              else
                intercept=fma1-dp1*slope
              end if
              ldat(start+10-1)=slope
              ldat(start+11-1)=intercept
C@empa aw 2005apr27 give the values in input units, then the user may copy paste them into CIF
              slope=slope*ABS(ifact(UnitP))/
     &		(ifact(UnitFva)*RhoI**ifact(UnitFvFlag))
              intercept=intercept/
     &		(ifact(UnitFva)*RhoI**ifact(UnitFvFlag))
              dp1=dp1/ABS(ifact(UnitP))
              dp2=dp2/ABS(ifact(UnitP))
              Call RelDis(REAL(-dp1),7,Str,Lstr,1)
              Call RelDis(REAL(-dp2),7,Str1,Lstr1,1)
              Call RelDis(slope,10,Str2,Lstr2,1)
              Call RelDis(intercept,10,Str3,Lstr3,1)
              CALL INERR('Link='//LiNa(I)(1:lenstr(Lina(i)))//
     &       ', type='//LiTyNa(i)(1:lenstr(LiTyNa(i)))//
     &       ' updated:',
     &       ' pmin='//str(1:lstr)//' '//iunit(unitP)//
     &        'pmax='//str1(1:lstr1)//' '//iunit(unitP)
     &       //'; slope     = '//str2(1:lstr2)//' '//
     &       iunit(unitFva)(1:lenstr(iunit(unitFva)))//'/'//iunit(unitP)
     &       //'; intercept = '//str3(1:lstr3)//' '//
     &       iunit(unitFva),.FALSE.,0)


            end if
C@tno jcp 1997Jul24_14:43:06 else part for check of fan datapoints added
           else
C@tno jcp 1997Jul28_10:56:56
c flag>=3
C use datapoints and thus the catmul interpolation
C check if the pressure points are in rising order
            npoints=ldat(start+18-1)
            pres1=ldat(start+19-1)
            do 180 j=1,npoints
              pos=(j-1)*2+19
              pres2=ldat(start+pos-1)
              if (pres2.lt.pres1) then
                Call RelDis(pres1,7,Str,Lstr,1)
                Call RelDis(pres2,7,Str2,Lstr2,1)
                Call Inerr('Fan data points: '//str(1:lstr)//','
     &          //str2(1:lstr2)
     &        ,'Fan data points: Pressure must be in ascending order',
     &       .false.,2)

              end if
C@lbl bvs 1997Nov19 lastdp not used anymore
CC              lastdp=dp
 180        continue
           end if
C end if Ldat(start+1)=1 (use coef)

          end if
C end if Ldat(start)=2
C@empa aw 2000jun30
C end if Linktype has been defined
          endif
200    continue
       RETURN
       END
C end of routine CheckFan

Ch***********************************************************************

        INTEGER FUNCTION iCheckNet()

C***********************************************************************
C Purpose: iCheckNet checks the zones in the ventilation network.
C          There must be a path (via links) to all zones.
C          There can't be two ore more isolated networks.
C          There must be at least one link to a known pressure (an external
C          or special pressure)
C
C Approach:
C          A matrix of zones is made which contains a 1 if a link exists and
C          a 0 if no link exists
C
C          Two arrays are made: Zone(Nz), Look(Nz)
C
C          iIsland is a number 1,2,3.. which is given to the zones. It indicates
C          a group of zones that is connected.
C          If all is OK all zones are on iIsland=1 and are connected.
C          If more iIslands are used we have indepentent networks. The input
C          must be corrected and a link must be made between the one or more
C          zones of independent islands.
C
C          Starting at zone 1, all zones first get a zero assigned in Zone(*).
C          All reachable zones get the first island number if they are found
C          in &-NET-LINks. Zone(iZone)=iIsland.
C          If still more zones exist they will get the next island number and
C          so on.
C          Zones with Zone(*)=0 are not found in &-NET-LINk which is an error in
C          the input.
C          Zones with Zone(*)>1 are on an other island and must be linked to
C          island 1. This too is an error in the input.
C
C          Finally is checked whether there is at least one link from a zone to
C          a known pressure.
C
C          During the search, zones just reached for the first time are also
C          set in Look(iZone). A further search is made from those zones in
C          Look(*).
C
C Pass parameters:
C
C IO # Name   unit          description
C return value:
C 1= OK
C 0= Not OK
Ch***********************************************************************

        IMPLICIT NONE
	INCLUDE 'comv-uni.inc'
	INCLUDE 'comv-inp.inc'

        INTEGER Zone(MaxZ),Look(MaxZ),LM(MaxZ,MaxZ)
        INTEGER i,j,OK,From,To,iZone,Lst,NotExt,Iisland,
     &   Ilook,minimum

C@empa aw 1999nov29 initialize iCheckNet
        iCheckNet=0
        if (test.ge.1) then
          write(cof,*) '**********************************'
          write(cof,*) 'Checking the Links to all Zones  *'
          write(cof,*) '**********************************'
        end if


C zero all arrays
        do 1 i=1,NZ
           Zone(i)=-1
           Look(i)=0
           do 1 j=1,nz
             LM(i,j)=0
 1      continue

C Fill the matrix LM(Nz,Nz)
        DO 10 I=1,NL
c Check if there is an external connection to a known pressure
C Lstat is the link status:
C Lstat From     To
C  0    zone    zone
C  1     ext    zone
C  2    spec    zone
C  3    zone     ext
C  4     ext     ext
C  5    spec     ext
C  6    zone    spec
C  7     ext    spec
C  8    spec    spec
           Lst=Lstat(i)
          From=FromTo(1,i)
            To=FromTo(2,i)

C@empa aw 1999nov29 If a zone ID was not found in routine FRTO, From or To is zero
C@empa              here. In order not to produce a Fortran error we go out of this
C@empa              routine here. A Comis error message is already written in FRTO.
C@empa              And we get an other error message, because iCheckNet is not 1.
           IF ((From.EQ.0).OR.(To.EQ.0)) GOTO 920
c check the type of link
c
c a non zero crack, window is always ok
c a fan which is not a constant flowrate is ok
c a flowcontroller, not a constant flowrate, is (probably) ok
c a TD, not a constant flow rate, is ok
          OK=1
          if (Lstat(i).eq.0) then
            if (from.ne.to) then
              LM(from,to)=OK
              LM(to,from)=OK
            end if
          end if

          if ((Lst.eq.1) .or. (Lst.eq.2)) then
C Lstat=1 or 2 known pressure - zone
            LM(to,to)=1
          end if
          if ((Lst.eq.3) .or. (Lst.eq.6)) then
C Lstat=3 or 6 zone - known pressure
            LM(from,from)=1
          end if
 10     CONTINUE
c        write(*,*) LM(1,1),LM(1,2),LM(1,3),LM(1,4),LM(1,5),LM(1,6)
c        write(*,*) LM(2,1),LM(2,2),LM(2,3),LM(2,4),LM(2,5),LM(2,6)
c        write(*,*) LM(3,1),LM(3,2),LM(3,3),LM(3,4),LM(3,5),LM(3,6)
c        write(*,*) LM(4,1),LM(4,2),LM(4,3),LM(4,4),LM(4,5),LM(4,6)
c        write(*,*) LM(5,1),LM(5,2),LM(5,3),LM(5,4),LM(5,5),LM(5,6)
c        write(*,*) LM(6,1),LM(6,2),LM(6,3),LM(6,4),LM(6,5),LM(6,6)

        NotExt=0
        iIsland=0

C 20 is the start of the main loop through the islands
 20     Continue

        iIsland=iIsland+1
C        write(*,*) ' Island ', Iisland

        iZone=0
        ilook=0

        if (Nz.eq.0) goto 905

C look for a zone that is still zero (not reached)
 30     continue
          izone=izone+1
        if (iZone.gt.Nz) GOTO 900
        if (Zone(izone).ge.0) GOTO 30
C        write(*,*) zone(1),zone(2),zone(3),zone(4),zone(5),zone(6)
C        write(*,*) ' izone ', izone,' Zone(*)=',zone(izone)
C        write(*,*)
C        pause

 40     continue
C Look for zoneslinked to iZone and fill the island number in those linked
C zones: Zone(*)=iIsland where *=linked with iZone
        call connect(LM,Zone,Look,iZone,iLook,iIsland)
C at return iLook contains the last zone reached from iZone

        if (iLook.gt.0) then
C quickly take the just found zone with number iLook and look for its
C connections
          izone=ilook
          Look(ilook)=0
C          write(*,*) ' Ilook:izone ', izone,' Zone(*)=',zone(izone)
        goto 40
        end if

C See if there are other zonenumbers in Look to start a discovery to linked
C zones
        Call NotZero(Look,iLook)
        if (iLook.gt.0) then
C take this next zone with number iLook to look for its connections
          izone=ilook
          Look(ilook)=0
C          write(*,*) ' Notzero:ilook ', izone,' Zone(*)=',zone(izone)
        goto 40
        else
C no more zones to look at, that have not been reached yet
          goto 900
        end if

C if we didn't reach all zones loop again
 900    if (Minimum(Zone,Nz).eq.-1) GOTO 20


c Check if there is an external connection to a known pressure
        if (Nz.eq.0) goto 120
        DO 110 I=1,Nz
          if (LM(i,i).eq.1) then
            GOTO 120
          end if
 110    CONTINUE
C if we come here not a single OK link to a known pressure is found
          Write(COF,1001) ' NETWORK ERROR'
          if (Nz.gt.1) then
            Write(COF,1001) ' &-NET-ZONe contains',Nz,' zones.'
          else
            Write(COF,1001) ' &-NET-ZONe contains',Nz,' zone.'
          end if
          Write(COF,*) ' At least one of the zones should be '//
     & 'connected to a known pressure.'
          Write(COF,*) ' Two kinds of known pressures exist:'
          Write(COF,*) '  - external nodes. These have a negative'//
     & ' number in &-NET-LINks'
          Write(COF,*) '  - special pressures. These have a form '//
     & 'like nn.nPa (nn.n=a REAL number)'
          Write(COF,*) ' You have to add such a link to the network.'

C set this flag for use below
          NotExt=1

 120    CONTINUE

 905    CONTINUE
C        write(*,*)'iisland=',iisland,' NotExt=',NotExt

        if (iIsland.ne.1) then

          Write(COF,1001) ' NETWORK ERROR'
          Write(COF,1001) ' All zones should be connected, but here'//
     & ' are',iIsland,'isolated groups of zones.'
          Write(COF,*)' You have to add links, or delete not-used '//
     & 'zones from &-NET-ZONes.'
        if (iIsland.gt.2) then
          Write(COF,1001) ' You have to add at least',iIsland-1,
     & 'links between those groups.'
        else
          Write(COF,1001) ' You have to add at least',iIsland-1,
     & 'link between those groups.'
        end if
          Write(COF,*)' Zones in group 0 are not in &-NET-LIN or '//
     & 'are connected only to'
          Write(COF,*)' known pressure(s).'

          Write(COF,*)' Here follow the zones and the group they '//
     & 'are in.'
          Write(COF,*) ' Zone Group Zone name:'
          do 910 i=1,Nz
            Write(COF,1000) i,Zone(i),ZoNa(i)
 910      continue
        end if

        If (NotExt.gt.0) then
C Network-error because there is no external (known pressure) in the network.
C Set iIslands > 1 to signal an error in the network.
          iIsland=2
        end if

C        write(*,*)'iisland=',iisland,' NotExt=',NotExt
        iCheckNet=iIsland

1000    FORMAT (1X,2(I5,1X),2X,A)
1001    FORMAT (1X,A,I3,1X,A)

C@empa aw 1999nov29 CONTINUE
920   CONTINUE
	RETURN
	END





        SUBROUTINE Connect(LM,Zone,Look,iZone,iLook,iIsland)
Ch***********************************************************************
C       Connect is called from iCheckNet
C       Connect fills the array Zone(iZone) dependent on the linking given in LM
C       First Zone(izone)=0.
C       As soon as a new zone is reached for the first time (by a link indicated
C       in LM) it gets a 1 in Look(*).
C       If a connection to another zone has been found (iLook>0) then
C       Zone(izone)=iIsland.
C       The connections are indicated in the matrix LM(Nz,Nz) with 1. They
C       represent non-zero links.
C
C Pass parameters:
C
C IO # Name   unit       description
C I    LM     (Nz,Nz)    Link matrix contains a 1 for zones linked by a non
C                        zero link
C IO   Zone   (Nz)       Array for Zones -1 at start, 0 if handled, 1 if on
C                        island 1, n if on island n
C  O   Look   (Nz)       all zones with Look(*)=1 have to be taken as next start
C                        point for a further look to connected zones in the
C                        network. These zones are just reached for the first
C                        time
C I    iZone   -         Zone number from which a search has to be under taken.
C  O   iLook   -         Last newly visited zone
C I    iIsland -         All zones, within reach, get this number in Zone(*).
C                        This is the number incremented outside this routine
C                        every time all links of one island have been searched
C                        and still some zones remain out of reach.
C
Ch***********************************************************************

        IMPLICIT NONE
	INCLUDE 'comv-inp.inc'

        INTEGER LM(MaxZ,MaxZ),Zone(MaxZ),Look(MaxZ)
        INTEGER iZone,iLook,iIsland,i

        iLook=0

C set Zone(*) to 0 to signal that we have been here. A zone not linked in LM
C stays 0
        if (Zone(iZone).eq.-1) then
          Zone(iZone)=0
        end if

        do 40 i=1,Nz
          if (i.ne.izone) then
            if (LM(Izone,i).gt.0) then
C there is a connection from Zone(iZone) to Zone(i)
c See if Zone(i) had not been reached yet
              if(Zone(i).lt.1) then
C got one
                Zone(i)=iIsland
C               write(*,*)'LM(',izone,i,')>0 island=',iIsland
                Look(i)=1
                iLook=i
              end if
            end if
          end if
 40     continue

C         write(*,*)'in connect:ilook=',iLook
        if (iLook.gt.0) then
          Zone(iZone)=iIsland
        end if

        return
        end


        SUBROUTINE NotZero(Look,iLook)
Ch***********************************************************************
C       NotZero is called from iCheckNet
C       NotZero finds the array element in Look that is not zero
Ch***********************************************************************

        IMPLICIT NONE
	INCLUDE 'comv-inp.inc'

        INTEGER Look(MaxZ)
        INTEGER i,iLook

        iLook=0

        do 40 i=1,Nz
          if (Look(I).gt.0) then
              iLook=i
              Goto 900
          end if
 40     continue
 900    continue

        Return

        End




        INTEGER FUNCTION Minimum(Array,MaxEl)
Ch***********************************************************************
C       Minimum returns the lowest value in the INTEGER Array(1..Size)
Ch***********************************************************************


        IMPLICIT NONE
        INTEGER Array(*)
        INTEGER i,MaxEl,Mini

        mini=Array(1)

        do 40 i=1,MaxEl
          if (Array(I).lt.mini) then
              mini=Array(i)
          end if
 40     continue

        Minimum=mini

        Return

        End


C@NBI PGS 2000Jul20 - This subroutine was no longer used, so commented out
CC        SUBROUTINE CheckP1Q2(variable,P1,Q1,P2,Q2)
CCCh***********************************************************************
CCC check P1..2 Q1..2 is called from F1..F4
CCC checks values for zero or negative or
CCC Q1=Q2 or P1=P2
CCC P1,Q1 .. P2,Q2 is the part of the characteristic of a flow controller
CCC where it acts as a fixed opening
CCC
CCCh***********************************************************************
CC
CC        IMPLICIT NONE
CC        include 'comv-inp.inc'
CC        REAL P1,P2,Q1,Q2
CC        Character*(*) variable
CC
CC        if (P1.eq.0) then
CC          P1=REALMin
CC          Call Inerr(variable,'P1=zero, and replaced by 1E-37',
CC     &   .true.,1)
CC        end if
CC
CC        if (Q1.eq.0) then
CC          Q1=REALMin
CC          Call Inerr(variable,'F1=zero, and replaced by 1E-37',
CC     &    .true.,1)
CC        end if
CC
CC        if (P2.eq.0) then
CC          P2=REALMin
CC          Call Inerr(variable,'P2=zero, and replaced by 1E-37',
CC     &    .true.,1)
CC        end if
CC
CC        if (Q2.eq.0) then
CC          Q2=REALMin
CC          Call Inerr(variable,'F2=zero, and replaced by 1E-37',
CC     &     .true.,1)
CC        end if
CC
CC        if (P2.eq.P1) then
CC          Call Inerr(variable,'P1=P2 They should be different',
CC     &   .true.,2)
CC        end if
CC
CCc        if (Q2.eq.Q1) then
CCc          Call Inerr(variable,'F1=F2 They should be different',
CCc     &   .true.,2)
CCc        end if
CC
CC        if (P1.lt.0) then
CC          P1=REALMin
CC          Call Inerr(variable,'P1=zero, and replaced by 1E-37',
CC     &   .true.,2)
CC        end if
CC
CC        if (Q1.lt.0) then
CC          Q1=REALMin
CC          Call Inerr(variable,'F1=zero, and replaced by 1E-37',
CC     &   .true.,2)
CC        end if
CC
CC        if (P2.lt.0) then
CC          P2=REALMin
CC          Call Inerr(variable,'P2=zero, and replaced by 1E-37',
CC     &    .true.,2)
CC        end if
CC
CC        if (Q2.lt.0) then
CC          Q2=REALMin
CC          Call Inerr(variable,'F2=zero, and replaced by 1E-37',
CC     &   .true.,2)
CC        end if
CCc        write(*,*) p1,p2,q1,q2
CCc        call ho('in checkp1q2','')
CC
CC        Return
CC
CC        End

Ch***********************************************************************

        SUBROUTINE CheckNames(MSG,Name,n)

C***********************************************************************
C Purpose: CheckNames checks wether all usernames are unique
C@empa aw 1999nov23
C
      IMPLICIT NONE

      CHARACTER*(*) MSG,Name(*)

	INTEGER i,j,n,LStr,LENSTR

      DO 10   i=1, n
	  DO 20  j=i+1, n
	     IF (Name(i).EQ.Name(j)) THEN
	       Lstr=LENSTR(Name(i))
	       Call Inerr(MSG//Name(i)(1:LStr)//' is not unique!',
     &       '',.false.,2)
           ENDIF
20    CONTINUE
10    CONTINUE
      RETURN
	END

Ch***********************************************************************

        SUBROUTINE CheSchNam(Name,key,line)
C***********************************************************************
C@empa aw 2000jan20 
C Checks the first character of schedule names against 
C the convention in the User's Guide
C
C    IO Name   description
C
C     I Name   Schedule name
C     I key    Keyword number
C     I line   line in CIF beeing processed  
c
c  Keyword  Keyword   Schedule number
c  number 
c       23='SCH-MAI'  1  not implemented
c       24='SCH-LIN'  2  not implemented
c       25='SCH-WIN'  3
c       26='SCH-FAN'  4
c       27='SCH-TEM'  5
c       28='SCH-HUM'  6
c       29='SCH-SIN'  7
c       30='SCH-SOU'  8
c       31='SCH-OCC'  9
C***********************************************************************
      IMPLICIT NONE

      INCLUDE 'comv-inp.inc'
      CHARACTER Name*10, line*80,SchNam*10,FirstCh*1
	INTEGER key,p,p2,p3,lenstr

       SchNam='WFTHSQO'
       p=key-24
       p2=(key-1)*10+1
	 p3=p2+lenstr(Keys(p2:p2+9))
	 FirstCh=Name(2:2)
	 call upperc(FirstCh)
       if (FirstCh.ne.SchNam(p:p))then
	   call inerr('&-'//Keys(p2:p3)//':  Names of'//
     &   ' schedules must begin with *'// SchNam(p:p)//' !', 
     &   'Error in line:  '//line,.false.,2)
       endif
	RETURN
	END
 
