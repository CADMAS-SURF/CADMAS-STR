      SUBROUTINE ESTFPN2(ESTF,GRID,UG,NP,D,S,IGNL,ITO)

!     ESTF : OUT : 要素剛性行列(上三角,1次元配列)
!                  配列要素の並びは3×3の行列を例にとると以下の通り
!
!                    1 2 3
!                      4 5
!                        6
!
!     GRID : IN  : 節点座標
!     UG   : IN  : 節点変位
!     NP   : IN  : 要素の構成節点番号
!     D    : IN  : 構成則マトリックス
!     S    : IN  : 応力
!     IGNL : IN  : =0:微小変形, =1:大変形

      IMPLICIT REAL*8(A-H,O-Z)

      DIMENSION X0(3,15),U(3,15),GRID(3,*),UG(6,*),NP(15),D(21,7,3)
     &         ,DNDX(3,15),DUDX(3,3),BL(6,45),BLTD(45,6),BLTDBL(1035)
     &         ,E(1035,6),SE(1035),S(6,7,3),ESTF(1035)

      INCLUDE 'gauss_pn_73.h'

      X0(:,:) = GRID(:,NP(:))
      U(:,:) = UG(1:3,NP(:))

      ESTF(:) = 0.

      DO J = 1, 3
        DO I = 1, 7

          CALL DERXPN2(DNDX,DET,15,X0,RG(I),SG(I),TG(J),ITO)
          CALL MATML(DUDX,2,DNDX,2,U,3,3,3,15)

          CALL BLMTX(BL,DNDX,DUDX,15,IGNL)
          CALL MATML(BLTD,2,BL,3,D(1,I,J),1,45,6,6)
          CALL MATML(BLTDBL,1,BLTD,2,BL,2,45,45,6)

          IF( IGNL > 0 ) THEN
            CALL ETAMTX(E,DNDX,15,45,1035)
            CALL MATML(SE,2,E,2,S(1,I,J),2,1035,1,6)
          ELSE
            SE(:) = 0.
          ENDIF

          ESTF(:) = ESTF(:) + ( BLTDBL(:) + SE(:) ) * DET * WGI(I)
     &              * WGJ(J)

        ENDDO
      ENDDO

      END
