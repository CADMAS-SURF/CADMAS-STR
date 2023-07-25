      SUBROUTINE BLMHX1(BL,DNDX,DUDX,IGNL)
C
C     BL   : OUT : [BL]
C                  BL(1:3,:) at (XG1,XG2,XG3)
C                  BL(4,:) at (0,0,XG3) (次数低減積分)
C                  BL(5,:) at (XG1,0,0) (次数低減積分)
C                  BL(6,:) at (0,XG2,0) (次数低減積分)
C     DNDX : IN  : DNDX(i,j,1) = ∂Nj/∂xi at (XG1,XG2,XG3)
C                  DNDX(i,j,2) = ∂Nj/∂xi at (0,0,XG3)
C                  DNDX(i,j,3) = ∂Nj/∂xi at (XG1,0,0)
C                  DNDX(i,j,4) = ∂Nj/∂xi at (0,XG2,0)
C     DUDX : IN  : DUDX(i,j,1) = ∂uj/∂xi at (XG1,XG2,XG3)
C                  DUDX(i,j,2) = ∂uj/∂xi at (0,0,XG3)
C                  DUDX(i,j,3) = ∂uj/∂xi at (XG1,0,0)
C                  DUDX(i,j,4) = ∂uj/∂xi at (0,XG2,0)
C     IGNL : IN  : =0:微小変形, =1:大変形
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION BL(6,3*8),DNDX(3,8,4),DUDX(3,3,4)
C
      CALL CLEAR1(BL,6*3*8)
C
      DO 100 J=1,8
C
        J0=3*(J-1)
        BL(1,J0+1)=DNDX(1,J,1)
        BL(2,J0+2)=DNDX(2,J,1)
        BL(3,J0+3)=DNDX(3,J,1)
        BL(4,J0+1)=DNDX(2,J,2)
        BL(4,J0+2)=DNDX(1,J,2)
        BL(5,J0+2)=DNDX(3,J,3)
        BL(5,J0+3)=DNDX(2,J,3)
        BL(6,J0+1)=DNDX(3,J,4)
        BL(6,J0+3)=DNDX(1,J,4)
C
  100 CONTINUE
C
      IF(IGNL .EQ. 0) RETURN
C
      DO 200 J=1,8
        J0=3*(J-1)
        DO 210 K=1,3
          DO 220 I=1,3
            BL(I,J0+K)=BL(I,J0+K)+DUDX(I,K,1)*DNDX(I,J,1)
  220     CONTINUE
          BL(4,J0+K)=BL(4,J0+K)+DUDX(1,K,2)*DNDX(2,J,2)
     &                         +DUDX(2,K,2)*DNDX(1,J,2)
          BL(5,J0+K)=BL(5,J0+K)+DUDX(2,K,3)*DNDX(3,J,3)
     &                         +DUDX(3,K,3)*DNDX(2,J,3)
          BL(6,J0+K)=BL(6,J0+K)+DUDX(3,K,4)*DNDX(1,J,4)
     &                         +DUDX(1,K,4)*DNDX(3,J,4)
  210   CONTINUE
  200 CONTINUE
C
      RETURN
      END
