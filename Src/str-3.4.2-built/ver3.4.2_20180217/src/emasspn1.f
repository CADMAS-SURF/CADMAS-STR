      SUBROUTINE EMASSPN1(EMASS,AMAT,GRID,KN,LUMP,ITO)
C
C     EMASS : OUT : 要素質量行列
C     AMAT  : IN  : 物性テーブル
C     GRID  : IN  : 節点座標
C     KN    : IN  : 要素の構成節点番号
C     LUMP  : IN  : =0:lumped mass, =1:consistent mass
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION AMAT(*),EMASS(6,6),H(6),P(3,6),RG(3),SG(3),TG(2),X(3,6)
     &          ,GRID(3,*),KN(6),HTH(6,6)
C
      DATA RG / .5D0, .5D0, 0.D0 /
      DATA SG / 0.D0, .5D0, .5D0 /
      DATA TG / -.577350269189626D0, .577350269189626D0 /
C
      X(:,:) = GRID(:,KN(:))
C
      RHO = AMAT(3)
C
      EMASS(:,:) = 0.
C
      DO JG = 1, 2
        DO IG = 1, 3
C
          CALL SFNPN2(H,6,RG(IG),SG(IG),TG(JG))
C
          CALL DERPN2(P,6,RG(IG),SG(IG),TG(JG))
          CALL DET3(DET,6,P,X,ITO)
C
          IF( LUMP == 0 ) THEN
            EMASS(:,1) = EMASS(:,1) + H(:) * RHO * DET / 6.D0
          ELSE
            CALL AXB(HTH,H,H,6,1,6)
            EMASS(:,:) = EMASS(:,:) + HTH(:,:) * RHO * DET / 6.D0
          ENDIF
C
        ENDDO
      ENDDO
C
      END
