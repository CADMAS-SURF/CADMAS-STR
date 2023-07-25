      SUBROUTINE SFNTR2(RL,H,ND)
C
C     RL : IN  : 面積座標
C     H  : OUT : H(i) = Ni
C     ND : IN  : 節点数
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION H(ND),RL(3)
C
      H(1) = RL(1)*(2.*RL(1)-1.) 
      H(2) = RL(2)*(2.*RL(2)-1.)
      H(3) = RL(3)*(2.*RL(3)-1.)
      H(4) = 4.*RL(1)*RL(2)
      H(5) = 4.*RL(2)*RL(3)
      H(6) = 4.*RL(3)*RL(1)
C
      IF( ND .EQ. 7 ) THEN
        H(7) = 27.*RL(1)*RL(2)*RL(3)
        DO I = 1, 3
          H(I) = H(I) + 1.D0/3.D0*H(7)
        ENDDO
        DO I = 4, 6
          H(I) = H(I) - 2.D0/3.D0*H(7)
        ENDDO
      ENDIF
C
      RETURN
      END
