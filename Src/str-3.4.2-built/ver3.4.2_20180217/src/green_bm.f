      SUBROUTINE GREEN_BM(EPS,UX,UY,UZ,IGNL)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION EPS(3),UX(3),UY(3),UZ(3)

      EPS(1) = UX(1)

      EPS(2) = UY(1) + UX(2)

      EPS(3) = UZ(1) + UX(3)

      IF( IGNL == 0 ) RETURN

      EPS(1) = EPS(1) + .5D0 * DOT_PRODUCT(UX,UX)

      EPS(2) = EPS(2) + DOT_PRODUCT(UX,UY)

      EPS(3) = EPS(3) + DOT_PRODUCT(UX,UZ)

      END
