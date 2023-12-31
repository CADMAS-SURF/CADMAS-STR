      SUBROUTINE DERTE2(RL,P)
C
C     RL : IN  : 体積座標
C     P  : OUT : P(i,j) = ∂Nj/∂ξi
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION P(3,10),RL(4)
C
      P( 1, 1) = 4.D0*RL(1)-1.D0
      P( 1, 2) = 0.D0
      P( 1, 3) = 0.D0
      P( 1, 4) = -(4.D0*RL(4)-1.D0)
      P( 1, 5) = 4.D0*RL(2)
      P( 1, 6) = 0.D0
      P( 1, 7) = 4.D0*RL(3)
      P( 1, 8) = 4.D0*(RL(4)-RL(1))
      P( 1, 9) = -4.D0*RL(2)
      P( 1,10) = -4.D0*RL(3)
C
      P( 3, 1) = 0.D0
      P( 3, 2) = 4.D0*RL(2)-1.D0
      P( 3, 3) = 0.D0
      P( 3, 4) = -(4.D0*RL(4)-1.D0)
      P( 3, 5) = 4.D0*RL(1)
      P( 3, 6) = 4.D0*RL(3)
      P( 3, 7) = 0.D0
      P( 3, 8) = -4.D0*RL(1)
      P( 3, 9) = 4.D0*(RL(4)-RL(2))
      P( 3,10) = -4.D0*RL(3)
C
      P( 2, 1) = 0.D0
      P( 2, 2) = 0.D0
      P( 2, 3) = 4.D0*RL(3)-1.D0
      P( 2, 4) = -(4.D0*RL(4)-1.D0)
      P( 2, 5) = 0.D0
      P( 2, 6) = 4.D0*RL(2)
      P( 2, 7) = 4.D0*RL(1)
      P( 2, 8) = -4.D0*RL(1)
      P( 2, 9) = -4.D0*RL(2)
      P( 2,10) = 4.D0*(RL(4)-RL(3))
C
      RETURN
      END
