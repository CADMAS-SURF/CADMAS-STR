      SUBROUTINE ESTFTE1(ESTF,GRID,UG,NP,D,S,IGNL,ITO)

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

      DIMENSION X0(3,4),U(3,4),GRID(3,*),UG(6,*),NP(4),D(21)
     &         ,DNDX(3,4),DUDX(3,3),BL(6,12),BLTD(12,6),BLTDBL(78)
     &         ,E(78,6),SE(78),S(6),ESTF(78)

      X0(:,:) = GRID(:,NP(:))
      U(:,:) = UG(1:3,NP(:))

      CALL DERXTE1(DNDX,DET,X0,ITO)
      CALL MATML(DUDX,2,DNDX,2,U,3,3,3,4)

      CALL BLMTX(BL,DNDX,DUDX,4,IGNL)
      CALL MATML(BLTD,2,BL,3,D,1,12,6,6)
      CALL MATML(BLTDBL,1,BLTD,2,BL,2,12,12,6)

      IF( IGNL > 0 ) THEN
        CALL ETAMTX(E,DNDX,4,12,78)
        CALL MATML(SE,2,E,2,S,2,78,1,6)
      ELSE
        SE(:) = 0.
      ENDIF

      ESTF(:) = ( BLTDBL(:) + SE(:) ) * DET / 6.D0

      END
