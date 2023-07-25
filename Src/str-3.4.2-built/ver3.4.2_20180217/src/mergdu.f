      SUBROUTINE MERGDU(UGP,VGP,UG,VG,DUG,DU,VG0,INDOF,INDMPC,MPCF,RMPC
     &                 ,POS,IELM,NM,IBEL,IELQ,IMPC,PG,DPG,INDOP,KK,ITER
     &                 ,ITER2,IGNL)

      USE MPI_PARAM

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION KK(*),UG(6,*),DUG(6,*),DU(*),INDOF(6,*),INDMPC(2,6,*)
     &         ,MPCF(2,*),RMPC(*),POS(3,*),IELM(NM,*),IBEL(*),VG(12,*)
     &         ,VG0(12,*),IELQ(4,*),UGP(6,*),VGP(12,*),PG(*),DPG(*)
     &         ,INDOP(*)

      NNOD = KK(8)
      NELM = KK(12)
      NBAR = KK(16)
      NNODC = KK(28)
      NNODX = KK(31)
      NIGSF = KK(94)
      NIELQ = KK(101)
      NIGSFX = KK(107)
      NIGSFC = KK(108)

      NN1 = NNOD
      NN2 = NNOD + NNODC
      NN3 = NNOD + NNODC + NIGSF + NIGSFC
      NN4 = NNOD + NNODC + NIGSF + NIGSFC + NNODX
      NN5 = NNOD + NNODC + NIGSF + NIGSFC + NNODX + NIGSFX

      UGP(:,1:NN5) = UG(:,1:NN5)
      VGP(:,1:NBAR) = VG(:,1:NBAR)

      DO I = 1, NN4
        IF( I > NN1 .AND. I <= NN3 ) CYCLE
        DO J = 1, 6
          IFR = INDOF(J,I)
          IF( IFR > 0 ) THEN
            DUG(J,I) = DU(IFR)
          ELSEIF(.NOT.(IFR == -1 .AND. ITER == 1 .AND. ITER2 == 1)) THEN
            DUG(J,I) = 0.
          ENDIF
        ENDDO
      ENDDO

      DO I = 1, NN4
        IF( I > NN1 .AND. I <= NN3 ) CYCLE
        DO J = 1, 6
          IFR = INDOF(J,I)
          IF( IFR == -2 ) THEN
            IS = INDMPC(1,J,I)
            IE = INDMPC(2,J,I)
            DO K = IS, IE
              II = MPCF(1,K)
              JJ = MPCF(2,K)
              DUG(J,I) = DUG(J,I) + RMPC(K)*DUG(JJ,II)
            ENDDO
          ENDIF
        ENDDO
      ENDDO

      UG(:,1:NN1) = UG(:,1:NN1) + DUG(:,1:NN1)
      POS(:,1:NN1) = POS(:,1:NN1) + DUG(1:3,1:NN1)

      IF( IMPC > 0 ) THEN
        CALL GSURF(UG(1,NN2+1),UG,IELM,NM,NELM,IBEL,6)
        CALL GSURF(POS(1,NN2+1),POS,IELM,NM,NELM,IBEL,3)
      ENDIF

      IF( MYRANK > 0 .AND. IMPC > 0 ) THEN

        UG(:,NN3+1:NN4) = UG(:,NN3+1:NN4) + DUG(:,NN3+1:NN4)
        POS(:,NN3+1:NN4) = POS(:,NN3+1:NN4) + DUG(1:3,NN3+1:NN4)

        CALL GSURFX(UG(1,NN4+1),6,NIGSFX,UG,IELQ(1,NIELQ+1))
        CALL GSURFX(POS(1,NN4+1),3,NIGSFX,POS,IELQ(1,NIELQ+1))

      ENDIF

      DO I = 1, NELM
        IF( IELM(2,I) == 4 ) THEN
          ID = IELM(7,I)
          CALL DIRUPDT(VG(1,ID),VG0(1,ID),IELM(8,I),DUG,IGNL)
        ENDIF
      ENDDO

      IF( KK(21) > 10 ) THEN

        DO I = 1, NN4
          IF( I > NN1 .AND. I <= NN3 ) CYCLE
          IFR = INDOP(I)
          IF( IFR > 0 ) THEN
            DPG(I) = DU(IFR)
          ELSEIF(.NOT.(IFR == -1 .AND. ITER == 1 .AND. ITER2 == 1)) THEN
            DPG(I) = 0.
          ENDIF
        ENDDO

        PG(1:NN1) = PG(1:NN1) + DPG(1:NN1)

        IF( IMPC > 0 )
     &    CALL GSURF(PG(NN2+1),PG,IELM,NM,NELM,IBEL,1)

        IF( MYRANK > 0 .AND. IMPC > 0 )
     &    PG(NN3+1:NN4) = PG(NN3+1:NN4) + DPG(NN3+1:NN4)

      ENDIF

      END
