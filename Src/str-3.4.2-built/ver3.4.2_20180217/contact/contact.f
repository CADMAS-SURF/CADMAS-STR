      SUBROUTINE CONTACT(ICONV2,ISLV,ISLVO,RSLV,IFRIC,FRIC,ISTK,NNOD
     &                  ,NBDY,NIELC,NINDC,NIGSF,IFMDL,INDG,ICBD,IELC
     &                  ,INDA,INDC,IEDA,IEDG,IELA,ICELA,ICEL,IBTE,ICEDA
     &                  ,ICED,ICFCA,ICFC,GELC,IELQ,IFCQ,IEDQ,IVRQ,ICTB
     &                  ,FRTB,EDML,EPSS,TOLA,EPSF,POSN,POSO,RFCI,SNRM
     &                  ,SNRMW,ISLVG,ISTEP,ITER2,MAXITER2,IDYN,ITO)
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION INDA(NBDY),INDC(NINDC),ISLV(2,NINDC),IEDG(6,*)
     &         ,IELC(3,NIELC),POSO(3,NNOD+NIGSF),POSN(3,NNOD+NIGSF)
     &         ,RSLV(3,NINDC),ICBD(NBDY),ICTB(NBDY,NBDY),IELA(3,NIELC)
     &         ,ISLVO(2,NINDC),RFCI(6,NNOD),RFC(3),ICELA(2,NNOD+NIGSF)
     &         ,ICEL(*),ICEDA(2,NNOD+NIGSF),ICED(*),ICFCA(2,NNOD+NIGSF)
     &         ,ICFC(*),GELC(NIELC),IBTE(4,*),IFRIC(10,NINDC)
     &         ,FRIC(10,NINDC),IEDA(NBDY),FRTB(3,NBDY,NBDY),EDML(NBDY)
     &         ,INDG(NNOD),SNRM(3,NINDC),SNRMW(3,NNOD+NIGSF)
     &         ,ISTK(NBDY,NBDY,2),ISLVG(3,NNOD),IELQ(4,*),IFCQ(NIELC)
     &         ,IEDQ(*),IVRQ(NNOD+NIGSF)
      DATA FTOL /0.D0/
C----&------------------------------------------------------------------
      RFC_MAX = 0.
C
      IF( ITER2 .EQ. 1 ) THEN
C
        DO I = 1, NINDC
          IND = INDC(I)
          IST = ISLV(1,I)
          IF( IST == 0 ) CYCLE
          CALL VECML2(RFCA,RFCI(1,IND),3)
          IF( RFCA > RFC_MAX ) RFC_MAX = RFCA
        ENDDO
C
        FTOL = RFC_MAX * 1.D-3
C
      ENDIF
C
      CALL SLVNRM(SNRM,SNRMW,NNOD,NIGSF,NIELC,NINDC,IELC,INDC,POSN)
C
      ISLVO(:,:) = ISLV(:,:)
      ISLV(:,:) = 0
      RSLV(:,:) = 0.
C
      DO 100 IBDY=1,NBDY
C
        IF(IBDY .EQ. 1) THEN
          IS=1
        ELSE
          IS=INDA(IBDY-1)+1
        ENDIF
        IE=INDA(IBDY)
C
        DO 110 I=IS,IE
C
          IND=INDC(I)
          IST=ISLVO(1,I)
          MASTER=ISLVO(2,I)
          CALL RMULT1(RFC,RFCI(1,IND),-1.D0,3)
C
          IF( IST .EQ. 1 ) THEN
            CALL VRTXCHK(ISLV(1,I),MASTER,RFC,SNRM(1,I),IEDG,IELC,ICELA
     &                  ,ICEL,IBTE,ICEDA,ICED,POSN,ITO)
          ELSEIF( IST .EQ. 2 .OR. IST .EQ. 4 ) THEN
            CALL EDGECHK(ISLV(1,I),POSN(1,IND),MASTER,RFC,SNRM(1,I),IELC
     &                  ,IEDG,POSN,ITO)
          ELSEIF( IST .EQ. 3 .OR. IST .EQ. 5 ) THEN
            CALL FACECHK(ISLV(1,I),RSLV(1,I),POSO(1,IND),POSN(1,IND)
     &                  ,MASTER,RFC,FTOL,IELC,IEDG,IELA,ICELA,ICEL,IBTE
     &                  ,ICFCA,ICFC,POSO,POSN,IFRIC(3,I),EPSF,ITO)
          ELSE
            CALL PENCHK(ISLV(1,I),RSLV(1,I),POSO(1,IND),POSN(1,IND)
     &                 ,SNRM(1,I),IBDY,NBDY,ICBD,IELC,IDUM,IEDG,IELA
     &                 ,ICELA,ICEL,IBTE,GELC,ICTB,POSO,POSN,TOLA,0)
          ENDIF
C
  110   CONTINUE
C
  100 CONTINUE
C
      CALL CIRCHK(ISLV,ISLVG,NNOD,NINDC,INDC,IEDG,IELC,IELQ,IFCQ,IEDQ
     &           ,IVRQ)
C
      POSO(:,:) = POSN(:,:)
C
      CALL ISTCHK(ICONV2,NNOD,NINDC,INDG,IELC,INDC,IEDG,ISLV,ISLVO,ISTEP
     &           ,ITER2,MAXITER2,ITO)
C
      IF( IFMDL > 0 )
     &  CALL FRICCHK(IFRIC,FRIC,NBDY,NINDC,ICBD,INDA,IEDA,ISLV,ISLVO
     &              ,FRTB,EDML,ITO)
C
      IF( IFMDL == 2 )
     &  CALL STICKCHK(IFRIC,ISTK,NBDY,NINDC,ICBD,INDA,ISLV,ISLVO,FRIC
     &               ,EPSS,ITER2,MAXITER2,ICONV2,IDYN,ITO)
C
      END
