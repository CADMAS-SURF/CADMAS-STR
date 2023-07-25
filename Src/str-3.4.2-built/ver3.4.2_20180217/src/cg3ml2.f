      SUBROUTINE CG3ML2(Y,A,X,NEQ,IDSK,IDCG,NCGSPC,INDOF,NDF,IMPC,ITO)
C
      USE MPI_PARAM
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION Y(NEQ),A(NCGSPC),X(NEQ),IDSK(NEQ+1),IDCG(NCGSPC)
     *         ,INDOF(NDF,*)
C
      IF( NPROCS > 1 ) THEN
        CALL COMM_CG(X,INDOF,NDF)
        IF( IMPC > 0 ) CALL COMM_CGX(X,INDOF,NDF)
      ENDIF
C
      CALL CLEAR1(Y,NEQ)
C
      DO 100 I=1,NEQ
        IS=IDSK(I)
        NN=IDSK(I+1)-IDSK(I)
        CALL VECMID(Y(I),A(IS),X,IDCG(IS),NN)
        IF(NN.GE.2) CALL VECMID2(Y,A(IS+1),X(I),IDCG(IS+1),NN-1)
  100 CONTINUE
      RETURN
      END