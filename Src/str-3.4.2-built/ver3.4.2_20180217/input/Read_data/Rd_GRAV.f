      SUBROUTINE RD_GRAV( IGRV, GRAV, CHAR )
C
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*80 CHAR
      DIMENSION GRAV(3)
C
      READ(CHAR,'(BN,8X,I8,8X,4F8.0)') IGRV, A, GRAV(:)
C
      GRAV(:) = A * GRAV(:)
C
      END
