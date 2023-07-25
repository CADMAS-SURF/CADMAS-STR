      SUBROUTINE EVOLTE2(VOL,X,ITO)

      IMPLICIT REAL*8(A-H,O-Z)

      DIMENSION WG(5),RLG(4,5),X(3,10),P(3,10)

      DATA WG  / 4*0.075D0, -0.133333333333333D0 /
      DATA RLG /
     &    0.500000000000000D0, 0.166666666666667D0, 0.166666666666667D0
     &  , 0.166666666666667D0
     &  , 0.166666666666667D0, 0.500000000000000D0, 0.166666666666667D0
     &  , 0.166666666666667D0
     &  , 0.166666666666667D0, 0.166666666666667D0, 0.500000000000000D0
     &  , 0.166666666666667D0
     &  , 0.166666666666667D0, 0.166666666666667D0, 0.166666666666667D0
     &  , 0.500000000000000D0
     &  , 4*0.25D0 /

      VOL = 0.

      DO I = 1, 5

        CALL DERTE2(RLG(1,I),P)
        CALL DET3(DET,10,P,X,ITO)

        VOL = VOL + DET * WG(I)

      ENDDO

      END