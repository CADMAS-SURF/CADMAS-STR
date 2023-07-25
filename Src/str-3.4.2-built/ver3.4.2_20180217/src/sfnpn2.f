      SUBROUTINE SFNPN2(H,ND,R,S,T)
C
C     H  : OUT : H(i) = Ni(ξ,η,ζ)
C     ND : IN  : 節点数
C     R  : IN  : ξ
C     S  : IN  : η
C     T  : IN  : ζ
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION H(ND),D(6)
C
      R2 = 2.D0 * R
C
      S2 = 2.D0 * S
C
      A  = 1.D0 - T
      A1 = .5D0 * A
      A2 = 2.D0 * A
C
      B  = 1.D0 + T
      B1 = .5D0 * B
      B2 = 2.D0 * B
C
      C  = 1.D0 - R - S
      C1 = .5D0 * C
      C2 = 2.D0 * C
C
      H(1) = A1 * C
      H(2) = A1 * R
      H(3) = A1 * S
      H(4) = B1 * C
      H(5) = B1 * R
      H(6) = B1 * S
C
      IF( ND == 6 ) RETURN
C
      D(1) = R2 + S2 + B
      D(2) = S2 + C2 + B
      D(3) = R2 + C2 + B
      D(4) = R2 + S2 + A
      D(5) = S2 + C2 + A
      D(6) = R2 + C2 + A
C
      H(1:6) = H(1:6) * ( 1.D0 - D(:) )
C
      H( 7) = A2 * C * R
      H( 8) = A2 * R * S
      H( 9) = A2 * C * S
      H(10) = A * B * C
      H(11) = A * B * R
      H(12) = A * B * S
      H(13) = B2 * C * R
      H(14) = B2 * R * S
      H(15) = B2 * C * S
C
      END
