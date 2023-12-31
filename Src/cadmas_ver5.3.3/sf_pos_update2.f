      SUBROUTINE SF_POS_UPDATE2(POS,POS1,POS2,DVEL,DVEL1,DVEL2,POS0
     &                         ,POS10,POS20,DVEL10,DVEL20,SPC,IPND0
     &                         ,PRES0,DELZ0,IFIX0,GRDL,IGFC,IELM,AFC
     &                         ,IGNO,IENO,XX,YY,ZZ,DT)

      USE SF_TYPE

      IMPLICIT REAL*8(A-H,O-Z)

      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ANUMBI.h'
      INCLUDE 'VF_APARAI.h'
      INCLUDE 'VF_ATIMER.h'
      INCLUDE 'SF_STRUCT.h'

      TYPE(SPACE) :: SPC(NELM)

      DIMENSION POS(3,NNOD),POS1(3,NNOD),POS2(3,NNOD),DVEL(3,NELM)
     &         ,DVEL1(3,NELM),DVEL2(3,NELM),POS0(3,NNOD0),POS10(3,NNOD0)
     &         ,POS20(3,NNOD0),DVEL10(3,NELM0),DVEL20(3,NELM0)
     &         ,IGNO(NNOD),IENO(NELM),GRDL(NNOD),IGFC(NNOD)
     &         ,IELM(24,NELM),XX(MAXG1,NUMI),YY(MAXG1,NUMJ)
     &         ,ZZ(MAXG1,NUMK),AFC(NPFC0),IPND0(NNOD0),PRES0(NNOD0)
     &         ,DELZ0(NNOD0),IFIX0(NNOD0)

      REAL(8), POINTER :: POSN(:,:)

      TNEXT = TNOW + DT

      IF( TNEXT > TSTR2 ) THEN

        TSTR1 = TSTR2

        IF( MYRANK == 0 ) THEN

          POS10 = POS20
          DVEL10 = DVEL20

          CALL SF_C_MPI_RECV_D(POS20,NNOD0*3,IROOTS)
          IF( IGEO == 1 ) CALL SF_C_MPI_RECV_D(DVEL20,NELM0*3,IROOTS)

          IF( ICON == 1 ) THEN
            CALL SF_C_MPI_SEND_D(AFC,NPFC0,IROOTS)
            CALL SF_C_MPI_SEND_I(IPND0,NNOD0,IROOTS)
          ENDIF
          CALL SF_C_MPI_SEND_D(PRES0,NNOD0,IROOTS)
          CALL SF_C_MPI_SEND_D(TNEXT,1,IROOTS)

          CALL SF_C_MPI_RECV_D(TSTR2,1,IROOTS)

          IF( ISTM == 1 ) THEN
            CALL SF_C_MPI_SEND_D(DELZ0,NNOD0,IROOTS)
            CALL SF_C_MPI_SEND_I(IFIX0,NNOD0,IROOTS)
          ENDIF

        ENDIF

        IF( IPART == 0 ) THEN

          IF( MYRANK == 0 ) THEN
            POS1 = POS10
            POS2 = POS20
            DVEL1 = DVEL10
            DVEL2 = DVEL20
          ENDIF

          CALL VF_P1BCSD(POS1,NNOD*3,0)
          CALL VF_P1BCSD(POS2,NNOD*3,0)
          CALL VF_P1BCSD(DVEL1,NELM*3,0)
          CALL VF_P1BCSD(DVEL2,NELM*3,0)

        ELSEIF( IPART == 1 ) THEN

          CALL SF_BCAST_D(POS1,IGNO,NNOD,POS10,NNOD0,3)
          CALL SF_BCAST_D(POS2,IGNO,NNOD,POS20,NNOD0,3)
          CALL SF_BCAST_D(DVEL1,IENO,NELM,DVEL10,NELM0,3)
          CALL SF_BCAST_D(DVEL2,IENO,NELM,DVEL20,NELM0,3)

        ENDIF

        CALL VF_P1BCSD(TSTR2,1,0)

      ENDIF

      ALP = ( TNEXT - TSTR1 ) / ( TSTR2 - TSTR1 )

      IF( MYRANK == 0 )
     &  POS0(:,:) = ( 1.D0 - ALP ) * POS10(:,:) + ALP * POS20(:,:)

      ALLOCATE( POSN(3,NNOD) )

      POSN(:,:) = ( 1.D0 - ALP ) * POS1(:,:) + ALP * POS2(:,:)

      CALL SF_POS_UPDT(POS,SPC,GRDL,IGFC,IELM,POSN,XX,YY,ZZ)

      DEALLOCATE( POSN )

      DVEL(:,:) = ( 1.D0 - ALP ) * DVEL1(:,:) + ALP * DVEL2(:,:)

      END
