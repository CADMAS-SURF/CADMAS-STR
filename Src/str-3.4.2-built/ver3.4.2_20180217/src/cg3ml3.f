      SUBROUTINE CG3ML3(Y,A,X,NEQ,IDSK,IDCG,INDOF,INDOP,IMPC,ITO)
C
      USE MPI_PARAM
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION Y(NEQ),A(*),X(*),IDSK(NEQ+1),IDCG(*),INDOF(6,*),INDOP(*)
C
      IF( NPROCS > 1 ) THEN
        CALL COMM_CG2(X,INDOF,INDOP)
        IF( IMPC > 0 ) CALL COMM_CG2X(X,INDOF,INDOP)
      ENDIF
C
      DO 100 I=1,NEQ
        IS=IDSK(I)
        NN=IDSK(I+1)-IDSK(I)
        CALL VECMID3(Y(I),A(IS),X,IDCG(IS),NN)
  100 CONTINUE
C
      END
