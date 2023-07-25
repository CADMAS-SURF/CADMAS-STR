      SUBROUTINE FINDINT(IS,IE,FAC,X0,N,X)

      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION X0(N)

      DO I = 1, N

        IF( X <= X0(I) ) THEN

          IF( I == 1 ) THEN
            IS = 1
            IE = 1
            FAC = 1.D0
          ELSE
            IS = I - 1
            IE = I
            FAC = ( X - X0(I-1) ) / ( X0(I) - X0(I-1) )
          ENDIF

          RETURN

        ENDIF

      ENDDO

      IS = N
      IE = N
      FAC = 0.

      END
