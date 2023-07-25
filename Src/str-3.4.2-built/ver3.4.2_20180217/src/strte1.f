      SUBROUTINE STRTE1(EPS,SIG,EPSG,SIGG)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION EPSG(6),SIGG(6),EPS(6,4),SIG(6,4)

      DO I = 1, 4
        EPS(:,I) = EPSG(:)
        SIG(:,I) = SIGG(:)
      ENDDO

      END
