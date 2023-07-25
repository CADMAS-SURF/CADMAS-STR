      SUBROUTINE ETAPN1(E,DNDX)

!     E    : OUT : [Eij]
!                  E(:,1) = [E11]
!                  E(:,2) = [E22]
!                  E(:,3) = [E33]
!                  E(:,4) = 2*[E12]
!                  E(:,5) = 2*[E23]
!                  E(:,6) = 2*[E31]
!     DNDX : IN  : DNDX(i,j) = ∂Nj/∂xi

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION F(3,18,3),DNDX(3,6),FTF(171,3,3),E(171,6)

      F(:,:,:) = 0.

      DO I = 1, 3
        DO J = 1, 6
          J0 = 3 * ( J - 1 )
          DO K = 1, 3
            F(K,J0+K,I) = DNDX(I,J)
          ENDDO
        ENDDO
      ENDDO

      DO I = 1, 3
        CALL MATML(FTF(1,I,I),1,F(1,1,I),3,F(1,1,I),2,18,18,3)
      ENDDO

      CALL MATML(FTF(1,1,2),1,F(1,1,1),3,F(1,1,2),2,18,18,3)
      CALL MATML(FTF(1,2,1),1,F(1,1,2),3,F(1,1,1),2,18,18,3)

      E(:,1) = FTF(:,1,1)
      E(:,2) = FTF(:,2,2)
      E(:,3) = FTF(:,3,3)
      E(:,4) = FTF(:,1,2) + FTF(:,2,1)

      END
