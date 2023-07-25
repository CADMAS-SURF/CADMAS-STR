      SUBROUTINE DBEAM2(D,E,RNU,HD)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION D(6)

      H0D = E * HD / ( E + HD )

      G = .5D0 * E / ( 1.D0 + RNU )

      D(:) = 0.

      D(1) = H0D
      D(4) = G
      D(6) = G

      END