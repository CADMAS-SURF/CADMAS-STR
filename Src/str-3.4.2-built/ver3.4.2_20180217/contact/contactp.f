      SUBROUTINE CONTACTP(ICONV2,ISLV0,ISLVP,RSLV0,PSLV,ISTICK,NNODI
     &                   ,NNOD,NNODC,NELM,NELMC,NBDY,NIGSF,NIGSFC,NINDC0
     &                   ,NINDC,IFMDL,IELM,NM,IBEL,IBTE,ICBD,IELC,IELCB
     &                   ,INDA0,IEDG,IELA,ICELA,ICEL,ICEDA,ICED,ICFCA
     &                   ,ICFC,GELC,ICTB,IFRIC,FRIC,U0,RL0,TOLA,EPSF
     &                   ,POSN,POSO,RFCI,ISTEP,ITER2,MAXITER2,IDYN,ITO)

      USE MPI_PARAM

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION ISLV0(2,NINDC0),ISLVP(NINDC0),RSLV0(3,NINDC0)
     &         ,PSLV(3,4,NINDC0),IBTE(4,*),ICBD(NBDY),IELC(3,*)
     &         ,INDA0(NBDY),IEDG(6,*),IELA(3,*)
     &         ,ICELA(2,NNOD+NNODC+NIGSF),ICEL(*)
     &         ,ICEDA(2,NNOD+NNODC+NIGSF),ICED(*)
     &         ,ICFCA(2,NNOD+NNODC+NIGSF),ICFC(*),GELC(*),IELCB(*)
     &         ,ICTB(NBDY,NBDY),IFRIC(10,NINDC),FRIC(10,NINDC)
     &         ,POSO(3,NNOD+NNODC+NIGSF+NIGSFC)
     &         ,POSN(3,NNOD+NNODC+NIGSF+NIGSFC),POSSO(3),POSSN(3),RFC(3)
     &         ,SNRM(3),IELM(NM,NELM+NELMC),IBEL(NELM+NELMC)
     &         ,RFCI(6,NNOD),U0(3,4,NINDC),RL0(3,NINDC),ISTICK(NINDC0)

      DATA FTOL /0.D0/

      IF( MYRANK == 1 ) CALL M_MPI_SEND_I(24,1,0)  ! SEND IOP=24 TO GLB_COMM

      CALL M_MPI_SEND_D(POSO,3*NNODI,0)
      CALL M_MPI_SEND_D(POSN,3*NNODI,0)
      CALL M_MPI_SEND_D(RFCI,6*NNODI,0)

      IF( NNODC > 0 ) THEN
        CALL M_MPI_RECV_D(POSN(1,NNOD+1),3*NNODC,0)
        CALL GSURF(POSN(1,NNOD+NNODC+NIGSF+1),POSN,IELM(1,NELM+1),NM
     &            ,NELMC,IBEL(NELM+1),3)
      ENDIF

      CALL M_MPI_BCAST_D(PSLV,12*NINDC0)

      DO I = 1, NINDC0
        IF( ISLV0(1,I) > 10 ) ISLV0(1,I) = ISLV0(1,I) - 10
      ENDDO

      IF( ITER2 == 1 ) THEN

        RFC_MAX = 0.

        DO I = 1, NINDC0
          IF( ISLV0(1,I) > 0 ) THEN
            CALL VECML2(RFCA,PSLV(1,3,I),3)
            IF( RFCA > RFC_MAX ) RFC_MAX = RFCA
          ENDIF
        ENDDO

        FTOL = RFC_MAX * 1.D-3

      ENDIF

      RSLV0(:,:) = 0.

      DO IBDY = 1, NBDY

        CALL ADDSET4(IS,IE,INDA0,IBDY)

        DO I = IS, IE

          IST = ISLV0(1,I)
          MASTER = ISLV0(2,I)
          IP = ISLVP(I)

          ISLV0(:,I) = 0
          ISLVP(I) = 0

          POSSO(:) = PSLV(:,1,I)
          POSSN(:) = PSLV(:,2,I)
          RFC(:) = PSLV(:,3,I)
          SNRM(:) = PSLV(:,4,I)

          IF( IST > 0 .AND. IP == MYRANK ) THEN
            SELECT CASE( IST )
            CASE( 1 )
              CALL VRTXCHK(ISLV0(1,I),MASTER,RFC,SNRM,IEDG,IELC,ICELA
     &                    ,ICEL,IBTE,ICEDA,ICED,POSN,ITO)
            CASE( 2, 4 )
              CALL EDGECHK(ISLV0(1,I),POSSN,MASTER,RFC,SNRM,IELC,IEDG
     &                    ,POSN,ITO)
            CASE( 3, 5 )
              CALL FACECHK(ISLV0(1,I),RSLV0(1,I),POSSO,POSSN,MASTER,RFC
     &                    ,FTOL,IELC,IEDG,IELA,ICELA,ICEL,IBTE,ICFCA
     &                    ,ICFC,POSO,POSN,ISTICK(I),EPSF,ITO)
            END SELECT
          ELSEIF( IST == 0 ) THEN
            CALL PENCHK(ISLV0(1,I),RSLV0(1,I),POSSO,POSSN,SNRM,IBDY,NBDY
     &                 ,ICBD,IELC,IELCB,IEDG,IELA,ICELA,ICEL,IBTE,GELC
     &                 ,ICTB,POSO,POSN,TOLA,1)
          ENDIF

          IF( ISLV0(1,I) > 0 ) ISLVP(I) = MYRANK

        ENDDO

      ENDDO

      POSO(:,:) = POSN(:,:)

      CALL M_MPI_SEND_I(ISLV0,2*NINDC0,0)
      CALL M_MPI_SEND_I(ISLVP,NINDC0,0)
      CALL M_MPI_SEND_D(RSLV0,3*NINDC0,0)

      CALL M_MPI_BCAST_I(ISLV0,2*NINDC0)
      CALL M_MPI_BCAST_I(ISLVP,NINDC0)
      CALL M_MPI_BCAST_D(RSLV0,3*NINDC0)

      IF( MYRANK == 1 ) THEN
        CALL M_MPI_SEND_I(ISTEP,1,0)
        CALL M_MPI_SEND_I(ITER2,1,0)
        CALL M_MPI_SEND_I(MAXITER2,1,0)
        CALL M_MPI_SEND_I(IDYN,1,0)
      ENDIF

      CALL M_MPI_BCAST_I(ICONV2,1)

      IF( IFMDL == 2 .AND. NINDC > 0 ) THEN
        CALL M_MPI_SEND_I(IFRIC,10*NINDC,0)
        CALL M_MPI_SEND_D(FRIC,10*NINDC,0)
        CALL M_MPI_SEND_D(U0,12*NINDC,0)
        CALL M_MPI_SEND_D(RL0,3*NINDC,0)
      ENDIF

      IF( IFMDL == 2 ) CALL M_MPI_BCAST_I(ISTICK,NINDC0)

      END