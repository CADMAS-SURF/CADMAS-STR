      SUBROUTINE DERXBM0(BY,BZ,UY,UZ,VS,VT,AS,AT)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION BY(3,12),BZ(3,12),UY(3),UZ(3),VS(3,2),VT(3,2),AS(3,3,2)
     &         ,AT(3,3,2)

      BY(:,:) = 0.

      BY(:,4:6)   = .5D0 * AS(:,:,1)

      BY(:,10:12) = .5D0 * AS(:,:,2)

      BZ(:,:) = 0.

      BZ(:,4:6)   = .5D0 * AT(:,:,1)

      BZ(:,10:12) = .5D0 * AT(:,:,2)

      UY(:) = .5D0 * VS(:,1) + .5D0 * VS(:,2)

      UZ(:) = .5D0 * VT(:,1) + .5D0 * VT(:,2)

      END

      