      SUBROUTINE FNORM(FNRM,KK,INDOF,INDOP,FT,ISLV)
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION FNRM(3),KK(*),INDOF(6,*),INDOP(*),FT(*)
C
      NNOD=KK(26)
      CALL CLEAR1(FNRM,3)
      DO 300 I=1,NNOD
        DO 310 J=1,3
          IFR=INDOF(J,I)
          IF(IFR .GT. 0) FNRM(1)=FNRM(1)+FT(IFR)*FT(IFR)
  310   CONTINUE
        DO 320 J=4,6
          IFR=INDOF(J,I)
          IF(IFR .GT. 0) FNRM(2)=FNRM(2)+FT(IFR)*FT(IFR)
  320   CONTINUE
  300 CONTINUE
      IF( ISLV > 10 ) THEN
        DO 400 I=1,NNOD
          IFR=INDOP(I)
          IF(IFR .GT. 0) FNRM(3)=FNRM(3)+FT(IFR)*FT(IFR)
  400   CONTINUE
      ENDIF
C
      IF(FNRM(1) .GT. 0.) FNRM(1)=DSQRT( FNRM(1) )
      IF(FNRM(2) .GT. 0.) FNRM(2)=DSQRT( FNRM(2) )
      IF(FNRM(3) .GT. 0.) FNRM(3)=DSQRT( FNRM(3) )
C
      RETURN
      END
