      SUBROUTINE NODSET1(KN,NODOP,NOD,ITO)
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION KN(4),NOD(3)
C---------------------------------------------------------------------==
C        1         2         3         4         5         6         7**
C23456789+123456789+123456789+123456789+123456789+123456789+123456789+12
C---------------------------------------------------------------------==
C
      DO 100 I=1,4
        IF(KN(I).EQ.NODOP) GOTO(201,202,203,204) I
  100 CONTINUE
C
      WRITE(ITO,*) 'STOP IN SUB. NODSET1!'
      CALL ERRSTP(90,ITO)
C
  201 NOD( 1) = KN( 4)
      NOD( 2) = KN( 3)
      NOD( 3) = KN( 2)
      RETURN
C
  202 NOD( 1) = KN( 4)
      NOD( 2) = KN( 1)
      NOD( 3) = KN( 3)
      RETURN
C
  203 NOD( 1) = KN( 2)
      NOD( 2) = KN( 1)
      NOD( 3) = KN( 4)
      RETURN
C
  204 NOD( 1) = KN( 2)
      NOD( 2) = KN( 3)
      NOD( 3) = KN( 1)
      RETURN
C
      END
