      SUBROUTINE INV3(A,AINV,DET,ITO)
C
C     A    : IN  : [A]
C     AINV : OUT : [A]の逆行列
C     DET  : OUT : |A|
C
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION  A(3,3),AINV(3,3)
C
      DET =  A(1,1) * ( A(2,2)*A(3,3)-A(3,2)*A(2,3) )
     *     + A(2,1) * ( A(3,2)*A(1,3)-A(1,2)*A(3,3) )
     *     + A(3,1) * ( A(1,2)*A(2,3)-A(2,2)*A(1,3) )
C
C      IF( DABS(DET) .LT. 1.D-20 )  THEN
C          WRITE(ITO,*) 'JACOBIAN IS TOO SMALL. STOP IN SUB. INV3!'
C      ENDIF
C
      AINV(1,1) =  ( A(2,2)*A(3,3)-A(3,2)*A(2,3) ) / DET
      AINV(2,1) = -( A(2,1)*A(3,3)-A(3,1)*A(2,3) ) / DET
      AINV(3,1) =  ( A(2,1)*A(3,2)-A(3,1)*A(2,2) ) / DET
C
      AINV(1,2) = -( A(1,2)*A(3,3)-A(3,2)*A(1,3) ) / DET
      AINV(2,2) =  ( A(1,1)*A(3,3)-A(3,1)*A(1,3) ) / DET
      AINV(3,2) = -( A(1,1)*A(3,2)-A(3,1)*A(1,2) ) / DET
C
      AINV(1,3) =  ( A(1,2)*A(2,3)-A(2,2)*A(1,3) ) / DET
      AINV(2,3) = -( A(1,1)*A(2,3)-A(2,1)*A(1,3) ) / DET
      AINV(3,3) =  ( A(1,1)*A(2,2)-A(2,1)*A(1,2) ) / DET
C
C
      RETURN
      END
