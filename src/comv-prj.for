C+*********************************************************** comv-prj.f
Ch*********************************************************************
	 SUBROUTINE PREJUNC
C			    1
C**********************************************************************
C Purpose    : This routine determines Njunc and fills the array JuncNr
C	       The input is checked for:
C	       - Consistent RefLink and DuAngle data
C	       - Junction node having exactly three links and this
C		 three links being only DS or DU
C
C Module     : VD/AW 1991Aug27
C Changes:
C@empa aw 1993jun14 key for error return from looknam added,
C@empa              check key and report errors
C@empa aw 1993jun17 call inerr instead of ERROR2
C@empa aw 1993jun17 FErrIn canceled
C@empa aw 1992jun17 LStr added
C
C
C Pass parameters:
C
C IO  #	   Name		  Unit	      Description
C

C Common block parameters used in this routine:
C
C IO  #	   Name		  Unit	      Description
C
C I   1	   Nl		   -	      Number of links
C I   2	   JuncNR(MaxZ,5)  -	      JuncNr (..,1)= First main branch LiNr
C				      JuncNr (..,2)= Second main branch LiNr
C				      JuncNr (..,3)= Junction branch LiNr
C				      JuncNr (..,4)= DuAngle
C				      JuncNR (..,5)= ZoneNr of junction
C
C Local parameters:
C
C Name	    Description
C
C RefLFlg   Is used to determine weather a RefLink is stated not
C	    more than twice in the link input
C LNr	    Link Nr of reference link name

Ch*********************************************************************

        IMPLICIT NONE
C Declarations Global:
C
	INCLUDE 'comv-uni.inc'
	INCLUDE 'comv-inp.inc'
        INTEGER LenStr


C Declarations Local
        INTEGER RefLFlg,LNr,LStr,Junct,i,j,key,n

C Initialisations
C
	DO 10 I=1,MaxZ
	   DO 20 J=1,5
	       JuncNr(I,J)=0
20	   CONTINUE
10	CONTINUE
	NJunc=0
	RefLFLG=0

      IF (test.ge.1.or.iecho.ge.1) THEN
	    Write (CRT,*)' '
	    Write (CRT,*)'*******************************'
	    Write (CRT,*)'Checking HVAC T-Junction data *'
	    Write (CRT,*)'*******************************'
	    Write (CRT,*)' '
	ENDIF
C
C For Links having RefLink:   RefLink not = '     ':
	DO 100 I=1,Nl
C@empa aw 2000jun30 only if linktype has been defined
         IF (pLiLDat(i).ne.0) THEN 
C get the link type of link i
           Ltyp=LDat(pLiLDat(i))
C@@@ this has to be expanded to the number DU gets as soon as DU is implemented
           IF (ltyp.eq.3)  THEN
           IF (RefLink(I).NE.'     ')THEN
C Check: TO -node cannot be an external or fixed pressure node:
	      IF (Lstat(I).GT.2) THEN
                 CALL INERR('&-NET-LINks:  LinkNr: '//LiNa(I),
     &		 'TO-node of this link cannot be junction node! '//
     &           '(external or fixed pressure node)',
     &           .FALSE.,2)
C Exit loop 110
                 Junct=-FromTo(2,I)
	      ELSE
C Junction node = tonode of link (per definition)
		 Junct= FromTo(2,I)
	      ENDIF
C Check if Tonode is already Junction node:
	      DO 110 J=1,NJunc
		IF (Junct.EQ. JuncNr(J,5)) THEN
C The node is already defined as junction, but If Flagref=0 one additional
C branch can have a RefLink , but it must be the same RefLink!
		   IF (RefLFlg.EQ.0.AND.RefLink(I).EQ. RefLink(J)) THEN
			 RefLFlg =1
C Write Linknr of second main branch to Junc-Array
			 JuncNr(J,2)=I
		   ELSE
C Flagref=1 : This node has already two main branches defined
C	      Input Error message
                    IF (Junct.GE.0)THEN
                     CALL INERR('&-NET-LINks:  LinkNr: '
     &               //LiNa(I),
     &               'More than 2 main or 1 branch duct defined at '//
     &               'T-junction node: '//ZoNa(Junct),.FALSE.,2)
		   ENDIF
C exit loop 110 only if message is reported
		   GOTO 100
		ENDIF
		ENDIF
110	      CONTINUE
	      RefLFlg=0

C ToNode = T-Junction node , Write to JuncNr array
	      Njunc=NJunc+1
	      JuncNr(NJunc,1)=I
C Check RefLink is in LiNa resp. LiTree and find LiNr of RefLink
	      CALL LOOKNAM(Nl,Reflink(I),LiTree,LiTreeN,LNr,LiLarg,
     &			   LiSmal,key)
              IF (key.EQ.1) THEN
                 LStr=LENSTR(Reflink(I))
                 CALL INERR('&-NET-LINks:  LinkNr: '//LiNa(I),
     &           'Ref Link: '//Reflink(I)(1:LStr)//' does not exist '//
     &           'in &-NET-LINks !',.FALSE.,2)
              ELSE

C Check if RefLink From or To node is T-junct node, else Input error messg
	      IF  ((Lstat(LNr).EQ. 0).AND.
     &		  (FromTo(1,LNr).EQ.Junct.OR.FromTo(2,Lnr).EQ.Junct))
     &		  THEN
	          JuncNr(NJunc,3)=LNr
	      ELSE IF ((Lstat(LNr).EQ.3 .OR. Lstat(LNr).EQ.6).AND.
     &		  (FromTo(1,LNr).EQ.Junct)) THEN
	          JuncNr(NJunc,3)=LNr
	      ELSE IF ((Lstat(LNr).EQ.1 .OR. Lstat(LNr).EQ.2).AND.
     &		  (FromTo(2,Lnr).EQ.Junct)) THEN
	          JuncNr(NJunc,3)=LNr
	      ELSE
                LStr=LENSTR(Reflink(I))
                CALL INERR('&-NET-LINks:  LinkNr: '//LiNa(I),
     &          'Ref Link: '//Reflink(I)(1:LStr)//' is not connected '//
     &          'with your T-junction node !',.FALSE.,2)
	      ENDIF

              ENDIF
	      JuncNr(NJunc,4)=DuAngle(I)
	      JuncNr(NJunc,5)=Junct
           ENDIF
C ENDIF from RefLink<>'     '
           ENDIF
C ENDIF from Ltyp=3 (DS)
         ENDIF 
C@empa aw 2000jun30
C ENDIF from linktype has been defined
100	CONTINUE

C	For all T-junction nodes:
C	-  Write second main branch link nr to juncnr array if not already
C	   defined
C	-  Check if not more than 3 links are attached to this junction

	DO 200 I=1,Nl
	   DO 210  J=1,NJunc
	      IF ((Lstat(I).EQ.0.OR.Lstat(I).EQ.3.OR.Lstat(I).EQ.6)
     &	       .AND.(FromTo(1,I).EQ.JuncNr(J,5))
     &	       .OR.(Lstat(I).LT.3.AND.FromTo(2,I).EQ.JuncNr(J,5)))
     &	       THEN
	       IF ((I.NE.JuncNr(J,1)).AND.(I.NE.JuncNr(J,3))) THEN
		  IF (JuncNr(J,2).EQ.0) THEN
		     JuncNr(J,2)=I
		  ELSE IF (JuncNr(J,3).EQ.0) THEN
                     JuncNr(J,3)=I
		  ELSE
C    There are already three branches defined and this link must be an
C    additional one
                    IF (JuncNr(J,5).GE.0)THEN
                     CALL INERR('&-NET-LINks:  LinkNr: '//LiNa(I),
     &		     'More than 3 links at T-junction node: '//
     &               ZoNa(JuncNr(J,5)),.FALSE.,2)
		  ENDIF
	       ENDIF
	      ENDIF
	      ENDIF
210	   CONTINUE
200	CONTINUE

C    Check Links at this T-junction being only 3 and only  DS or DU:

	DO 300 I=1,NJunc
	  DO 310 J=2,3
	    N=JuncNr(I,J)
	    IF (N.EQ.0) THEN
               IF (JuncNr(I,5).GE.0)THEN
                CALL INERR('&-NET-LINks:  LinkNr: '//LiNa(JuncNr(I,1)),
     &		'Only 2 branches at T-junction node: '//
     &          ZoNa(JuncNr(I,5)),.FALSE., 2)
               ENDIF
	    ELSE IF (LiTyNa(N)(1:2).NE.'DS'.AND.LiTyNa(N)(1:2).NE.'DU')
     &		THEN
                CALL INERR('&-NET-LINks:  LinkNr: '//LiNa(N),
     &		'This junction link is not DS or DU:',.FALSE.,2)
	    ENDIF
310	  CONTINUE
300     CONTINUE
        RETURN
        END

Ch*********************************************************************
         SUBROUTINE PRERF
C			    1
C**********************************************************************
C Purpose    : This routine fills the array iRFlist
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
C I   2    iRFlist(MaxL)  -            iRFlist()= number of the Reference link
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

        INTEGER LenStr


C Declarations Local
        INTEGER i,Lnr,Key,Lstr

C Initialisations
C
        DO 10 I=1,MaxL
               iRFlist(I)=0
10	CONTINUE

      IF (test.ge.1.or.iecho.ge.1) THEN
	    Write (CRT,*)' '
	    Write (CRT,*)'*******************************'
          Write (CRT,*)'Looking for RF components     *'
	    Write (CRT,*)'*******************************'
	    Write (CRT,*)' '
	ENDIF
C
C For Links having RefLink:   RefLink not = '     ':
c        write(crt,*) 'i    ltyp    reflink'
        DO 100 I=1,Nl
C@empa aw 2000jun30 only if linktype has been defined
         IF (pLiLDat(i).ne.0) THEN 
C get the link type of link i
           Ltyp=LDat(pLiLDat(i))
c        Write(CRT,*) i,' ',ltyp,' ',reflink(i)
           if (Ltyp.eq.12) then
             IF (RefLink(I).NE.'     ')THEN
              CALL LOOKNAM(Nl,Reflink(I),LiTree,LiTreeN,LNr,LiLarg,
     &			   LiSmal,key)
              IF (key.EQ.1) THEN
                 LStr=LENSTR(Reflink(I))
                 CALL INERR('&-NET-LINks:  LinkNr: '//LiNa(I),
     &           'Ref Link: '//Reflink(I)(1:LStr)//' does not exist '//
     &           'in &-NET-LINks !',.FALSE.,2)
              ELSE
                iRFlist(i)=Lnr
c                write(CRT,*) 'gevonden, link nummer',Lnr,
c     &               'iRFList(i)=',iRFlist(i)
                if (lnr.ge.i) then
C is this following the sequence? or no longer by the sorting???
                 CALL INERR('&-NET-LINks:  LinkNr: '//LiNa(I),
     &          'Ref Link: '//Reflink(I)(1:LStr)//' must come before'//
     &           'i in &-NET-LINks !',.FALSE.,2)
                ENDIF
              endif
             ENDIF
           ENDIF
         ENDIF 
C@empa aw 2000jun30
C ENDIF from linktype has been defined

100     CONTINUE

        RETURN
	END


Ch*********************************************************************
         SUBROUTINE PreSchZ
C			    1
C**********************************************************************
C Purpose    : This routine checks the 1..Np pollutant names: Cnames
C              It checks the zone schedules starting with 'S'=sink and
C              'Q'=source. This must give a unique match for one of the Cnames
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


        INTEGER LenStr
C Declarations Local
        INTEGER i,Ic,iZone,ipol,k,l,imatch,iC1Pol,nerrors
        INTEGER iCol1ABC
        Character*50 DUM,word,word2

        nerrors=0

        IF (test.ge.1.OR.iecho.ge.1) THEN
	    Write (CRT,*)' '
	    Write (CRT,*)'*******************************'
          Write (CRT,*)'Looking for Pollutant names   *'
	    Write (CRT,*)'*******************************'
	    Write (CRT,*)' '
        ENDIF
        if (Nconc.gt.0) then
        DO 10 I=1,Nconc
          ic=Ichar(Cname(i)(1:1))
          IF (test.ge.1.and.iecho.ge.1) THEN
            write(crt,*)'pollutant ',cname(i)
          end if
          if (Ic.gt.47 .and. Ic.lt.58) then
            nerrors=nerrors+1
            call error2('Pollutant '//Cname(i)//' must not start'//
     &      ' with a number.',
     &     'Correct the input at &-POL-DES and restart the program',2)
          end if
C end if Ic in  47..58
10      continue

        IF (test.ge.1.OR.iecho.ge.1) THEN
	    Write (CRT,*)' '
	    Write (CRT,*)'*******************************'
          Write (CRT,*)'Check Schedule and Pollutants *'
	    Write (CRT,*)'*******************************'
	    Write (CRT,*)' '
        ENDIF

        DO 20 Izone=1,Nz
          DUM=SCHz(izone)
          IF (test.ge.1.and.iecho.ge.5) THEN
            if (dum.ne.'-') write(crt,*)
     &       'schedules for zone',Izone,'= ',DUM
          end if
          l=lenstr(DUM)

C loop 15 for all schedule names for this zone from SchZ(izone)

          k=0
15        continue
          CALL GETWRD(DUM,k,l,word)
          IF (test.ge.1.and.iecho.ge.5) THEN
            if (word.ne.'-') write(crt,*) 'schedule= ',word
          end if

C@tno jcp 1996Mar05_14:10:43 '-' signals a not used schedule
          if (word.ne.'-') then

            if (word(1:1).eq.'Q' .or. word(1:1).eq.'S') then
              IF (test.ge.1.and.iecho.ge.5) THEN
                write(crt,*) 'starts with Q or S'
              end if
              iC1Pol=iCol1ABC(2,word)
C@empa aw 1999dec13 check correct pollutant name
              if (iC1Pol.eq.0) then
                word2=' '
              else
                word2=word(iC1Pol:lenstr(word))
              endif 

              IF (test.ge.1.and.iecho.ge.5) THEN
                write(crt,*) 'pollutant name in schedule= ',word2
              end if

              CALL MatchConc(word2,Ipol,imatch)
              if (imatch.gt.1) then
                nerrors=nerrors+1
                call error2('The pollutant name '//
     &          word2(1:lenstr(word2))//' in schedule '//
     &          word(1:lenstr(word))//
     &         ' matches more than one pollutant.',
     &         'Correct the input at &-NET-ZONe or &-POL-DES and'//
     &         ' restart the program',2)
              end if
C end if imatch=1
              if (imatch.eq.0) then
                nerrors=nerrors+1
                call error2('The pollutant name '//
     &          word2(1:lenstr(word2))//' in schedule '//
     &          word(1:lenstr(word))//
     &          ' matches none of the pollutants.',
     &         'Correct the input at &-NET-ZONe or &-POL-DES and'//
     &         ' restart the program',2)
              end if
C end if imatch=0
            end if
C end if word(1:1) = Q or S
           end if
C end if word <>'-'
          if (k.lt.l) goto 15

20      continue
        if (nerrors.gt.1) then
          call error2('First correct the errors concerning the'//
     &    ' pollutants and schedules',' ',3)
        end if
        end if
C end if Nconc>0

        RETURN
	END

