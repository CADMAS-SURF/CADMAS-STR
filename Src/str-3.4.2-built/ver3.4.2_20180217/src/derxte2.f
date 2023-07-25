      SUBROUTINE DERXTE2(DNDX,DET,RL,XYZ,ITO)
C
C     DNDX : OUT : DNDX(i,j) = ∂Nj/∂xi
C     DET  : OUT : |J| ( Jij = ∂xj/∂ξi )
C     RL   : IN  : 体積座標
C     XYZ  : IN  : 節点座標
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION RL(4),P(3,10),XYZ(3,10),XJI(3,3),DNDX(3,10)
C
      CALL DERTE2(RL,P)
      CALL JACOB3(10,P,XYZ,XJI,DET,ITO)
      CALL AXB(DNDX,XJI,P,3,3,10)
C
      RETURN
      END
