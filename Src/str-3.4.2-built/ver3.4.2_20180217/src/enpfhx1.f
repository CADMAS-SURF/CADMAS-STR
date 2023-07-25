      SUBROUTINE ENPFHX1(FCG,EPS,S,GRID,UG,NP,D,IGNL,ITO)

!     FCG  : OUT : 等価節点力
!     EPS  : OUT : 歪
!     S    : OUT : 応力
!     GRID : IN  : 節点座標
!     UG   : IN  : 節点変位
!     NP   : IN  : 要素の構成節点番号
!     D    : IN  : 構成則マトリックス
!     IGNL : IN  : =0:微小変形, =1:大変形

      IMPLICIT REAL*8(A-H,O-Z)

      DIMENSION TE(3,3),X0(3,8),U(3,8),GRID(3,*),UG(6,*),NP(8)
     &         ,D(21,2,2,2),DNDX(3,8,4),DUDX(3,3,4),BL(6,24),EPS0(6)
     &         ,EPS(6,2,2,2),DEPS(6),DS(6),S(6,2,2,2),BLTS(3,8),FC(3,8)
     &         ,FCG(3,8)

      INCLUDE 'gauss_ln_3.h'

      CALL LCCHX1(TE,X0,U,GRID,UG,NP)

      FC(:,:) = 0.

      DO I = 1, 2
      DO J = 1, 2
      DO K = 1, 2

        XG1 = XG(I,2)
        XG2 = XG(J,2)
        XG3 = XG(K,2)
        WGT = WG(I,2)*WG(J,2)*WG(K,2)

        CALL DERXHX1(DNDX,DET,XG1,XG2,XG3,X0,ITO)
        DO L = 1, 4
          CALL MATML(DUDX(1,1,L),2,DNDX(1,1,L),2,U,3,3,3,8)
        ENDDO

        CALL BLMHX1(BL,DNDX,DUDX,IGNL)

        EPS0(:) = EPS(:,I,J,K)
        CALL GREEN_HX1(EPS(:,I,J,K),DUDX,IGNL)
        DEPS(:) = EPS(:,I,J,K) - EPS0(:)

        CALL MATML(DS,2,D(1,I,J,K),1,DEPS,2,6,1,6)
        S(:,I,J,K) = S(:,I,J,K) + DS(:)

        CALL MATML(BLTS,2,BL,3,S(1,I,J,K),2,24,1,6)

        FC(:,:) = FC(:,:) + BLTS(:,:) * DET * WGT

      ENDDO
      ENDDO
      ENDDO

      CALL MATML(FCG,2,TE,3,FC,2,3,8,3)

      END
