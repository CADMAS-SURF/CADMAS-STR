      SUBROUTINE ENPFRCD(DF,ESTF,DUG,NP,NDF,NNP)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION DU(NDF,NNP),NP(NNP),DUG(6,*),DF(NDF,NNP),ESTF(*)

      DU(:,:) = DUG(1:NDF,NP(:))

      N = NDF * NNP

      CALL MATML(DF,2,ESTF,1,DU,2,N,1,N)

      END