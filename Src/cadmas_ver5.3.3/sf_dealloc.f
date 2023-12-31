      SUBROUTINE SF_DEALLOC()

      USE VF_A2ARRAY

      IMPLICIT REAL*8(A-H,O-Z)

      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ANUMBI.h'

      IERR = 0
      DEALLOCATE(GGV_0,STAT=IERR)
      DEALLOCATE(GGX_0,STAT=IERR)
      DEALLOCATE(GGY_0,STAT=IERR)
      DEALLOCATE(GGZ_0,STAT=IERR)
      DEALLOCATE(INDX0,STAT=IERR)
      DEALLOCATE(INDY0,STAT=IERR)
      DEALLOCATE(INDZ0,STAT=IERR)
      DEALLOCATE(BCU0,STAT=IERR)
      DEALLOCATE(BCV0,STAT=IERR)
      DEALLOCATE(BCW0,STAT=IERR)
      DEALLOCATE(BCP0,STAT=IERR)
      DEALLOCATE(BCF0,STAT=IERR)
      DEALLOCATE(BCVI0,STAT=IERR)
      IF (LEQK.NE.0) THEN
        DEALLOCATE(BCK0,STAT=IERR)
        DEALLOCATE(BCE0,STAT=IERR)
      ENDIF
      IF (LEQT.NE.0) THEN
        DEALLOCATE(BCT0,STAT=IERR)
        DEALLOCATE(BCTI0,STAT=IERR)
      ENDIF
      IF (LEQC.GT.0) THEN
        DEALLOCATE(BCC0,STAT=IERR)
        DEALLOCATE(BCCI0,STAT=IERR)
      ENDIF
      DEALLOCATE(INDB0,STAT=IERR)
      IF (LEQK.NE.0) THEN
        DEALLOCATE(INDBK0,STAT=IERR)
        DEALLOCATE(INDBE0,STAT=IERR)
      ENDIF
      IF (LEQT.NE.0) THEN
        DEALLOCATE(INDBT0,STAT=IERR)
      ENDIF
      IF (LEQC.GT.0) THEN
        DEALLOCATE(INDBC0,STAT=IERR)
      ENDIF
      IF (IERR.NE.0) CALL VF_A2ERR('SF_DEALLOC','CAN NOT DEALLOC.')

      END