      SUBROUTINE STRPN2(EPS,SIG,EPSG,SIGG)
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION EPS(6,15),SIG(6,15),EPSG(6,7,3),SIGG(6,7,3)
C-----------------------------------------------------------------------
      CALL INTPLPN2(EPS,EPSG)
      CALL INTPLPN2(SIG,SIGG)
C
      END
