      SUBROUTINE SCALSY(NEQ,NEQEXT,P,A,B,IDSK,IDCG,INDOF,NDF,IMPC)
C
      USE MPI_PARAM
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION IDSK(NEQEXT+1),P(NEQEXT),A(*),IDCG(*),B(NEQ)
     &         ,INDOF(NDF,*)
C
      DO 200 I=1,NEQ
        IS=IDSK(I)
        P(I)=1.D0/DSQRT(A(IS))
  200 CONTINUE
C
      IF( NPROCS > 1 ) THEN
        CALL COMM_CG(P,INDOF,NDF)
        IF( IMPC > 0 ) CALL COMM_CGX(P,INDOF,NDF)
      ENDIF
C
      DO 100 I=1,NEQEXT
        IS=IDSK(I)
        IE=IDSK(I+1)-1
        DO 120 J=IS,IE
          K=IDCG(J)
          A(J)=A(J)*P(I)*P(K)
  120   CONTINUE
  100 CONTINUE
C
      DO 300 I=1,NEQ
        B(I)=B(I)*P(I)
  300 CONTINUE
C
      RETURN
      END
