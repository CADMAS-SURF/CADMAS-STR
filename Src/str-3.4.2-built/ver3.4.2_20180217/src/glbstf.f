      SUBROUTINE GLBSTF(STF,RHV,KK,NESTF,NCRMAX,NRAMAX,RR,GRID,IELM,NM
     &                 ,AMAT,RODA,BARD,BVEC,INDOF,INDMPC,MPCF,RMPC,IDSK
     &                 ,IDCG,DUG,UG,VG,D,S,MGP,DT1,DT2,ITER,ITER2,IMPC
     &                 ,IDYN,ITO)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION ESTF(NESTF),GRID(3,*),UG(6,*),IELM(NM,*),D(21*MGP,*)
     &         ,S(6*MGP,*),INDOF(6,*),INDMPC(2,6,*),MPCF(*),RMPC(*)
     &         ,RAMBDA(NRAMAX),WK(NRAMAX),IDCR(2,NCRMAX),RHV(*),DUG(6,*)
     &         ,STF(*),IDSK(*),IDCG(*),KK(*),RR(*),AMAT(33,*)
     &         ,EMASS(NESTF),VG(12,*),RODA(*),BARD(6,*),BVEC(3,*)

      NELM = KK(12)
      NELMC = KK(29)
      NELMX = KK(32)

      BETA = RR(4)
      W4 = RR(5)

      DT12 = .5D0 * ( DT1 + DT2 )

      STF(1:KK(20)) = 0.

      DO I = 1, NELM + NELMC + NELMX

        IF( I > NELM .AND. I <= NELM + NELMC ) CYCLE

        ITYP = IELM(2,I)
        IF( ITYP == 1 .OR. ITYP == 4 ) THEN
          NDF = 6
        ELSE
          NDF = 3
        ENDIF
        NNP = IELM(3,I)
        IMAT = IELM(4,I)
        I5 = IELM(5,I)
        I7 = IELM(7,I)

        N1 = NNP * NDF
        N2 = N1 * ( N1 + 1 ) / 2

        CALL ESTFMTX(ESTF,GRID,UG,VG(1,I7),ITYP,IELM(8,I),NNP,RODA(I5)
     &              ,BARD(1,I5),BVEC(1,I7),D(1,I),S(1,I),KK(2),ITO)

        IF( IDYN == 1 ) THEN

          CALL EMASSMTX(EMASS,GRID,ITYP,NDF,NNP,IELM(6,I),IELM(8,I)
     &                 ,AMAT(1,IMAT),RODA(I5),BARD(1,I5),KK(30),ITO)

          CM = AMAT(5,IMAT)
          GE = AMAT(4,IMAT) / W4

          ESTF(1:N2) = ( 1.D0/DT2 + .5D0*CM ) / DT12 * EMASS(1:N2)
     &               + ( .5D0/DT2*GE + BETA ) * ESTF(1:N2)

        ENDIF

        CALL MPCTRNS(IMPC,IELM(8,I),NNP,NDF,NNP*NDF,RAMBDA,NRAMAX,INDOF
     &              ,INDMPC,MPCF,RMPC,WK,ESTF,IDCR,NCR)

        IF( ITER == 1 .AND. ITER2 == 1 ) 
     &    CALL SPCDRHV(RHV,ESTF,IDCR,NCR,INDOF,DUG)

        CALL CGADM(STF,ESTF,IDCR,NCR,IDSK,IDCG,INDOF,KK(21),ITO)

      ENDDO

      END
