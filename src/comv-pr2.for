C+*********************************************************** comv-pr2.f
Ch*********************************************************************
       SUBROUTINE MatchConc(Name,Ipol,Nmatch)
C**********************************************************************
C Purpose    : MatchConc tries to match Name with the pollutant names Cname
C              (defined at &-POL-DES) and returns Ipol the pollutant sequence
C              number at &-POL-DES. Nmatch=0 none match found
C                                   Nmatch=1 one match found
C                                   Nmatch>1 more matches found (error)
C
C Module     : jcp 1996Apr07
C Changes:
C
C Pass parameters:
C
C IO  #	   Name		  Unit	      Description
C

C Common block parameters used in this routine:
C
C IO  #	   Name		  Unit	      Description
C
C
C Local parameters:
C
C Name	    Description
C

Ch*********************************************************************
        IMPLICIT NONE
        INTEGER ipol,Nmatch
        CHARACTER*(*) Name
C Declarations Global:
        INTEGER LenStr
        INTEGER IcIndex
C
	INCLUDE 'comv-uni.inc'
	INCLUDE 'comv-inp.inc'


              Nmatch=0
C              write(cof,*) 'Nconc=',Nconc
              Do 18 ipol=1,Nconc
C                write(cof,*) 'Conc=',ipol,cName(ipol)
C                write(cof,*) 'Lenstr(cname)=',lenstr(cName(ipol))
C                write(cof,*) 'Lenstr(SchPol)=',lenstr(name)


                if (ICindex(Cname(ipol),name).eq.1) then
                  IF (test.ge.1.and.iecho.ge.5) THEN
                    write(crt,*) Name(1:lenstr(Name)),
     &             ' matches ',Cname(ipol)
                  end if
                  Nmatch=Nmatch+1
                end if
18            continue

              return
              end



Ch*********************************************************************
         SUBROUTINE PreSch
C			    1
C**********************************************************************
C Purpose    : This routine checks all schedules that are invoked for
C              having a definition
C              There are Zone Schedules SchZ(1..Nz)(50char)
C                    and Link Schedules SoL(1..Nl)(5char)
C
C Module     : jcp 1996Feb07
C Changes:
C
C Pass parameters:
C
C IO  #	   Name		  Unit	      Description
C

C Common block parameters used in this routine:
C
C IO  #	   Name		  Unit	      Description
C
C
C Local parameters:
C
C Name	    Description
C

Ch*********************************************************************

        IMPLICIT NONE
C Declarations Global:
C
	INCLUDE 'comv-uni.inc'
	INCLUDE 'comv-inp.inc'

        INTEGER IcIndex

        INTEGER LenStr
        INTEGER iempty

C Declarations Local
        INTEGER iZone,k,l,imatch,nerrors,
     &  kName,Lname,lw,iLink
        Character*50 DUM,word,Name
        LOGICAL ZoneSch,LinkSch

        nerrors=0

        IF (test.ge.1.or.iecho.ge.1) THEN
	  Write (CRT,*)' '
C@NBI PGS 2000Jul16 - Better reading clarity
          Write (CRT,*)'****************************************'
          Write (CRT,*)'Checking if all used schedules defined *'
          Write (CRT,*)'****************************************'
	  Write (CRT,*)' '
        ENDIF

C@tno jcp 1996Apr04_11:28:34  are any schedules used?

        ZoneSch=.FALSE.
        Izone=0
12      Izone=Izone+1
        if ((iempty(SCHz(izone),1).eq.1).or.
     &        (SCHz(izone).eq.'-'        )     ) then
          if (Izone.lt.Nz) goto 12
        else
          ZoneSch=.TRUE.
        end if
        if ((test.ge.1.and.iecho.ge.5) .and. ZoneSch) then
          write(cof,*)'Zone Schedules have been used'
        end if

        LinkSch=.FALSE.
        Ilink=0
13      Ilink=Ilink+1
        if (iempty(SoL(ilink),1).eq.1) then
          if (Ilink.lt.Nl) goto 13
        else
          LinkSch=.TRUE.
        end if
        if ((test.ge.1.and.iecho.ge.5) .and. LinkSch) then
          write(cof,*)'Link Schedules have been used'
        end if

        if (ZoneSch .or. LinkSch) then
C zone or Link Schedules have been used: go and look in aName if they are
C defined

        lName=lenstr(aName)
        if (LName.eq.0) then
C aName string is empty

          if (ZoneSch .and. LinkSch) then

          CALL INERR('Zone and Link Schedules have been used but '//
     &    'none has been defined. Correct the input file.',
     &    'Use COMISman.Cif or look in the User Guide '//
     &    ' on how to input SCHedules.',.FALSE.,2)
          nerrors=nerrors+1

          else if (ZoneSch) then
            CALL INERR('Zone Schedules have been used but none '//
     &      'has been defined. Correct the input file.',
     &      'Use COMISman.Cif or look in the User Guide '//
     &      ' on how to input SCHedules.',.FALSE.,2)
            nerrors=nerrors+1

          else if (LinkSch) then
            CALL INERR('Link Schedules have been used but none '//
     &      'has been defined. Correct the input file.',
     &      'Use COMISman.Cif or look in the User Guide '//
     &      ' on how to input SCHedules.',.FALSE.,2)
            nerrors=nerrors+1

          else
C aName is empty but no Zone or Link schedules have been used. Nothing wrong.
          end if

        else
C a Name is not empty: check the names
        if (test.ge.1.and.iecho.ge.5) then
          write(crt,*)'all schedule names'
          call wrt80(crt,aName,WCrt)
        end if

        DO 20 Izone=1,Nz
          DUM=SCHz(izone)
          if (iempty(dum,1).eq.0) then
            if (DUM.ne.'-') then
              IF (test.ge.1.AND.iecho.ge.5) THEN
                write(crt,*)
                write(crt,*)'schedules ',DUM
              end if
            end if
            l=lenstr(DUM)

C loop 15 for all schedule names for this zone from SchZ(izone)

            k=0
15          continue
              CALL GETWRD(DUM,k,l,word)
              lw=lenstr(word)
C@tno jcp 1996Mar05_14:16:30 test if different from '-' (= not used schedule)
              if (word.ne.'-') then
              IF (test.ge.1.AND.iecho.ge.5) THEN
                write(crt,*)
                write(crt,*) 'used zone schedule= ',word
              end if
              imatch=0
C have to loop here through the names of aName
              kname=0

18            continue

              CALL GETWRD(aName,kname,lname,Name)
C@empa aw 2005oct06 the following line produces a compiler warning
CC              Name=Name//' '
              IF (test.ge.1.AND.iecho.ge.8) THEN
                write(crt,*) 'defined schedule= ',Name
                write(crt,*)' index=',ICindex(Name,word)
                write(crt,*)' Name(',lw+2,')=',Name(lw+2:lw+2)
              end if

              if (ICindex(Name,word).eq.2 .and.
     &            Name(lw+2:lw+2).eq.' ') then

                  IF (test.ge.1.AND.iecho.ge.5) THEN
                    write(crt,*) 'matches ',name
                  end if
                  imatch=imatch+1
                end if
C loop if we have not yet had all names from aName
              if (kname.lt.Lname) goto 18

              if (imatch.gt.1) then
                nerrors=nerrors+1
                call error2('The schedule name '//
     &          word(1:lenstr(word))//
     &         ' matches more than one defined schedule.',
     &         'Correct the input at &-NET-ZONe or &-SCH... and'//
     &         ' restart the program',2)
              end if
C end if imatch=1
              if (imatch.eq.0) then
                nerrors=nerrors+1
                call inerr('The schedule name '//
     &          word(1:lenstr(word))//
     &          ' matches none of defined schedules.',
     &         'Correct the input at &-NET-ZONe or &-POL-DES and'//
     &         ' restart the program',.false.,2)
C@tno jcp 1996Jul09_13:17:46 additional information if a reference starts with a
               if (word(1:1).eq.'*') then
             call lasterror2('The reference to the schedule '//
     &       word(1:lenstr(word))//' you used '//
     &       'should not start with an ''*''.','Asterisks are only '//
     &       'used to indicate the definition of a name.',2)
              end if
              end if
C end if imatch=0
            end if
C end if word<>'-'
            if (k.lt.l) goto 15
          end if
Cend if dum<>' '
20      continue

        DO 120 ILink=1,Nl
          word=Sol(Ilink)
C@tno jcp 1996Mar05_14:26:07 added lw for the length of the Link Schedule
          lw=lenstr(word)
          if (iempty(word,1).eq.0) then

            IF (test.ge.1.and.iecho.ge.5) THEN
              write(crt,*)
              write(crt,*)'used link schedule ',word

            end if

            imatch=0
C have to loop here through the names of aName
            kname=0

118         continue
            CALL GETWRD(aName,kname,lname,Name)
C@empa aw 2005oct06 the following line produces a compiler warning
CC            Name=Name//' '
            IF (test.ge.1.and.iecho.ge.8) THEN
              write(crt,*) 'defined schedule= ',Name
              write(crt,*)' index=',ICindex(Name,word)
              write(crt,*)' Name(',lw+2,')=',Name(lw+2:lw+2)
            end if


            if (ICindex(Name,word).eq.2 .and.
     &          Name(lw+2:lw+2).eq.' ') then

              IF (test.ge.1.and.iecho.ge.5) THEN
                write(crt,*) 'matches ',name
              end if
              imatch=imatch+1
            end if
C loop if we have not yet had all names from aName
            if (kname.lt.Lname) goto 118

            if (imatch.gt.1) then
              nerrors=nerrors+1
              call error2('The schedule name '//
     &        word(1:lenstr(word))//
     &       ' matches more than one defined schedule.',
     &       'Correct the input at &-NET-LINk or &-SCH... and'//
     &       ' restart the program',2)
            end if
C end if imatch=1
            if (imatch.eq.0) then
              nerrors=nerrors+1
              call error2('The schedule name '//
     &        word(1:lenstr(word))//
     &        ' matches none of defined schedules.',
     &       'Correct the input at &-NET-LINk or &-POL-DES and'//
     &       ' restart the program',2)
            end if
C end if imatch=0
          end if
C end if word<>' '
120     continue

C end if Lname>0
        end if

C end if ZoneSch or LinkSch
        end if

C stop here if there were errors
        if (nerrors.gt.0) then
          call error2('First correct the errors concerning the'//
     &   ' schedules',
     &   ' ',3)
        end if

        RETURN
	END

Ch*********************************************************************
         SUBROUTINE PreOCC
C			    1
C**********************************************************************
C Purpose    : This routine checks if all OCCupants used in Histograms (PR-
C SIMUare defined in &-OCCUPANt description
C
C Module     : jcp 1996apr07
C Changes:
C@empa aw 2000apr11 "&-PR-SIMU" changed to "&-PR-OUTP" in the messages 
C
C Pass parameters:
C
C IO  #	   Name		  Unit	      Description
C

C Common block parameters used in this routine:
C
C IO  #	   Name		  Unit	      Description
C
C
C Local parameters:
C
C Name	    Description
C

Ch*********************************************************************

        IMPLICIT NONE
C Declarations Global:
C
	INCLUDE 'comv-uni.inc'
	INCLUDE 'comv-inp.inc'


C Declarations Local
        INTEGER i,inr,Lstr
        Character*50 str
        INTEGER p,k,L,LenStr
        CHARACTER word*10


        if (maxOccNrh.gt.0) then
          IF (test.ge.1.or.iecho.ge.1) THEN
	    Write (CRT,*)' '
C@NBI PGS 2000Jul16 - Better reading clarity
            Write (CRT,*)'****************************************'
            Write (CRT,*)'Checking if all used occupants defined *'
            Write (CRT,*)'****************************************'
	    Write (CRT,*)' '
          ENDIF
        end if

        if (MaxOccNrH.gt.MaxOccNrD) then
              CALL INERR
C@empa aw 2000apr11 
CC     &       ('&-PR-SIMU and &-OCCUPANt:More Occupants are used in '//
     &       ('&-PR-OUTPut and &-OCCUPANt:More Occupants are used in '//
C@NBI PGS 1999Aug09 - Grammar
CC   &        'histograms then are defined.',' Correct the input. ',
     &        'histograms than are defined.',' Correct the input. ',
     &     .FALSE.,2)
        end if

C the occupant numbers may be in IOoptH(2,ih) as negative INTEGERs
C        write(cof,*)'MhisUs=',MhisUs
        Do 10 i=1,MHisUs
C          write(cof,*)'i=',i
          inr=IOoptH(2,i)
C          write(cof,*)'inr=',inr
          if (inr.lt.0) then
C this is an occupant. Check if it is in OccAge(OccNr)
            inr=-inr
C            write(cof,*) 'checking occupant', inr
            if (inr.le.MaxO) then
C the number fits within the parameter maximum
C              write(cof,*) 'occupants age=', OccAge(inr)
              if (OccAge(inr).gt.0) then
C we assume that a positive age means that this occupant has been defined
              else
	      call intdis(inr,str,LStr)
              CALL INERR
C@empa aw 2000apr11
CC     &       ('Occupant '//str(1:Lstr)//' used in &-PR-SIMU is not'//
     &       ('Occupant '//str(1:Lstr)//' used in &-PR-OUTPut is not'//
     & ' defined at &-OCCUPANt . ',
     &        'Define this occupant.',.FALSE.,2)
              end if
            else
	      call intdis(MaxO,str,LStr)
              CALL INERR
C@empa aw 2000apr11
CC     &       ('In &-PR-SIMU More Occupants are used than the allowed'//
     &       ('In &-PR-OUTPut More Occupants are used than the '//
     &        'allowed maximum of '//str(1:Lstr)//'.',
     &        ' Correct the input ',.FALSE.,2)

            end if

          end if
C end if Inr<0
10      continue

C@@@@@ now check all occupant schedules defined for match with occupants
C@empa aw 2000apr11 Ok, I will do that:
      k=1
      l=Lenstr(aName)
      if (l.gt.0) then
20    continue
        variab='look for occupant schedule'
        call GetWrd(aName,k,l,word)
        call lowerc(word)
        p=index(word,'*o')
        if (p.eq.1)then
C         found an occupant schedule name 
          Lstr=Lenstr(word)
          iNr=index('123456789',word(5:5))
          if ((inr.gt.0).and.(inr.le.MaxO).and.(lstr.eq.5).and.
     &    (index(word,'occ').eq.2)) then
C            we have a valid OCC name
            if (OccAge(inr).gt.0) then
C             we assume that a positive age means that this occupant has been defined
            else
              CALL INERR
     &         ('&-SCH-OCC: '//word(1:Lstr)//' is not'//
     &         ' defined at &-OCCUPANt . ',
     &         'Define this occupant.',.FALSE.,2)
            end if
          else
            CALL INERR
     &      ('&-SCH-OCC: '//word(1:Lstr)//' is not allowed '//
     &       'as occupant name. Valid names are: ;*OCC1 ... *OCC5.  ',
     &       ' ',.FALSE.,2)
          end if
        endif  
      if (k.lt.l)goto 20
      endif
C@@@@@ now check all used occupant schedules for being defined
C@empa aw 2000apr12 No, occupant schedules are used as soon as they are
C@empa aw           defined. They don't have to be referenced else where,
C@empa aw           which could be checked here.

      RETURN
	END


Ch*********************************************************************
         SUBROUTINE PreOCCPol
C			    1
C**********************************************************************
C Purpose    : PreOccPol checks if there are occupant generated pollutants
C              that have the same name as the pollutants used in this
C              simulation. If found, the flag OccAsSource=.TRUE.
C              and OccPolNr(ioccNr,1..2)=ipol <- the pollutant number
C              This aray is used in Poltrans to add the occupant sources
C
C Module     : jcp 1996apr22
C Changes:
C
C Pass parameters:
C
C IO  #	   Name		  Unit	      Description
C

C Common block parameters used in this routine:
C
C IO  #	   Name		  Unit	      Description
C
C
C Local parameters:
C
C Name	    Description
C

Ch*********************************************************************

        IMPLICIT NONE
C Declarations Global:
C
	INCLUDE 'comv-uni.inc'
	INCLUDE 'comv-inp.inc'

        INTEGER IcIndex

C Declarations Local
        INTEGER i,ipol,ioccnr


        if (maxOccNrD.gt.0) then
          IF (test.ge.1.or.iecho.ge.1) THEN
	    Write (CRT,*)' '
C@NBI PGS 2000Jul16 - Better reading clarity
            Write (CRT,*)'****************************************'
            Write (CRT,*)'Checking if occupants used as sources  *'
            Write (CRT,*)'****************************************'
	    Write (CRT,*)' '
          ENDIF
        end if
        Do 40 iPol=1,MaxC
          Do 30 iOccNr=1,MaxO
            Do 20 I=1,MaxC
c              write(*,*) cName(ipol),OccPoln(ioccnr,i)
              if (ICindex(CName(iPol),OccPolN(iOccNr,i)).eq.1) then
c                call ho('pol and occpol match','')
                OccAsSource=.TRUE.
C@tno jcp 1996Apr24_13:47:30 store the pollutant number that matches
                OccPolNr(ioccNr,i)=ipol
                if (test.ge.1 .and. iecho.ge.3 ) then









                  write(cof,*) 'Occupant',iOccNr,OccName(iOccNr),
     &            ' produces'
                  write(cof,*) OccPol(ioccNr,i),' which matches '//
     &            'pollutant ',iPol, Cname(iPol)

                end if
              end if
20          continue
30        continue
40      continue

        RETURN
	END

Ch*********************************************************************
         SUBROUTINE PreFicSPol
C			    1
C**********************************************************************
C Purpose    : Checks and assigns the pollutant names of fictive sources 
C              with the pollutant names in POL-DES 
C
C Module     : empa aw 2000feb01
C Changes:
C
Ch*********************************************************************
      IMPLICIT NONE

C Declarations Global:
C
	INCLUDE 'comv-uni.inc'
	INCLUDE 'comv-inp.inc'
C Declarations Local
      INTEGER   ipol,iFSo,Lstr,LenStr,ICindex
      CHARACTER Str*3

          Do 30 iFSo=1,MaxFSo
            if (LenStr(FSPolN(iFSo)).ne.0) then
              Do 20 ipol=1,MaxC
                if (ICindex(CName(ipol),FSPolN(iFSo)).eq.1) then
C                 store the pollutant number that matches
                  FSPolNr(iFSo)=ipol
                end if
	          if (FSPolNr(iFSo).eq.0)then
	             call intdis(iFSo,Str,3)
                   Lstr=LenStr(FSPolN(iFSo))
	             call inerr('&-POL-FICtive source Nr '//Str//
     &             'Pollutant '//FSPolN(iFSo)(1:Lstr)//
     &             ' is not defined in &-POL-DES!',' ',.false.,2)                    
                endif
20            continue 
             endif            
30        continue

        RETURN
	END


Ch*********************************************************************
         SUBROUTINE PreH2OPol
C			    1
C**********************************************************************
C Purpose    : PreH2OPol checks if H2O is a pollutant
C              if so  ipolH2O gets the pollutant number of H2O
C              if not ipolH2O=0
C              The flag UseMetH2O (use the meteo H2O values) is set, but reset
C              if the outdoor pollutant schedule defines H2O as values SCH-POL.
C              If this Schedule contains the word 'meteo' for the H2O column,
C              then the outdoor H2O is taken again from the meteo schedule.
C
C Module     : jcp 1996jun21
C Changes:
C
C Pass parameters:
C
C IO  #	   Name		  Unit	      Description
C

C Common block parameters used in this routine:
C
C IO  #	   Name		  Unit	      Description
C
C
C Local parameters:
C
C Name	    Description
C

Ch*********************************************************************

        IMPLICIT NONE
C Declarations Global:
        INCLUDE 'comv-uni.inc'
        INCLUDE 'comv-inp.inc'

        INTEGER IcIndex
        INTEGER iPol

C@NBI PGS 1999Aug09 - Shouldn't suppress output just because no occupants?
C@NBI                 However, should suppress check if there are no pollutants
CC      if (maxOccNrD.gt.0) then
        if (NconcPolDes.GT.0) then
          IF (test.ge.1.or.iecho.ge.1) THEN
            Write (CRT,*)' '
C@NBI PGS 2000Jul16 - Better reading clarity
            Write (CRT,*)'****************************************'
            Write (CRT,*)'Checking if H2O used as source         *'
            Write (CRT,*)'****************************************'
            Write (CRT,*)' '
          ENDIF
CC        end if
C@NBI PGS 1999Aug09   (end)
          ipolH2O=0
          useMetH2O=0
C@NBI PGS 1999Aug17 - Should be NconcPolDes, not MaxC
CC        Do 40 iPol=1,MaxC
          Do 40 iPol=1,NconcPolDes
c           write(*,*) cName(ipol),OccPoln(ioccnr,i)
            if (ICindex(CName(iPol),'H2O').eq.1) then
              ipolH2O=ipol
              UseMetH2O=1
c             write(*,*) ipolh2O
c             call ho('ipolH2O set','')
            end if
40        continue
C@NBI PGS 1999Aug09 - The answer to above question is
          IF(test.ge.1.or.iecho.ge.1)WRITE(CRT,*)'UseMetH2O=',UseMetH2O
        ENDIF
        RETURN
        END


Ch*********************************************************************
         SUBROUTINE PreHis
C			    1
C**********************************************************************
C Purpose    : This routine checks if all Histograms used in (PR-
C SIMU are defined in &-HISTO
C
C Module     : jcp 1996apr07
C Changes:
C
C Pass parameters:
C
C IO  #	   Name		  Unit	      Description
C

C Common block parameters used in this routine:
C
C IO  #	   Name		  Unit	      Description
C
C
C Local parameters:
C
C Name	    Description
C

Ch*********************************************************************

        IMPLICIT NONE
C Declarations Global:
C
	INCLUDE 'comv-uni.inc'
	INCLUDE 'comv-inp.inc'


C Declarations Local
        INTEGER i,
     &  type,Lstr,MaxHistTy,
     &  Lstr2
        Character*50 str,str2

        MaxHistTy=0
c        write(cof,*) 'in preHis, MHisUs=',mhisUs
        Do 1 i=1,MHisUs
c          write(cof,*)i,histused(i,2)
          type=HistUsed(i,2)
          if (type.gt.MaxHistTy) MaxHistTy=type
1       continue
c        write(cof,*) 'in preHis, MaxHistty=',MaxHistty



        if (MaxHistTy.gt.0) then
          IF (test.ge.1.or.iecho.ge.1) THEN
	    Write (CRT,*)' '
C@NBI PGS 2000Jul16 - Better reading clarity
            Write (CRT,*)'*****************************************'
            Write (CRT,*)'Checking if all used histograms defined *'
            Write (CRT,*)'*****************************************'
	    Write (CRT,*)' '
          ENDIF

        Do 2 i=1,MHisUs
          type=HistUsed(i,2)
          if (type.ne.0) then

          if (type.gt.MHisTy) then
C this type exceeds the maximum dimensioned size
              call intdis(type,str,Lstr)
              call intdis(MHisTy,str2,Lstr2)
              CALL INERR
C@empa aw 2000apr11
CC     &       ('&-PR-SIMU you use histogram '//Str(1:Lstr)//
     &       ('&-PR-OUTPut: you use histogram '//Str(1:Lstr)//
     &        ' while the maximum type is '//Str2(1:Lstr2),
     &        ' Correct the input. ',
     &     .FALSE.,2)
          else if (Histox(type,1).lt.2) then
C this histogram has less than 2 classes which means that is has not been define
              call intdis(type,str,Lstr)
              CALL INERR
C@empa aw 2000apr11
CC     &       ('&-PR-SIMU you use histogram '//Str(1:Lstr)//
     &       ('&-PR-OUTPut you use histogram '//Str(1:Lstr)//
     &        ' which is not defined at &-HISTO.',
     &        ' Correct the input. ',
     &     .FALSE.,2)
          end if
          end if
2       continue


        end if

        RETURN
	END
