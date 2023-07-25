      SUBROUTINE ENPFADD(FT,FC,NDF,ND,KN)
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION FT(6,*),KN(ND),FC(NDF,*)
C-----------------------------------------------------------------------
      FT(1:NDF,KN(:)) = FT(1:NDF,KN(:)) + FC(:,1:ND)

      END