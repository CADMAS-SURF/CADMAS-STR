      SUBROUTINE CG_MPI_IRECV_D(DRECV,N,ISRC,IREQ)

      USE MPI_PARAM

      IMPLICIT REAL*8(A-H,O-Z)

      INCLUDE 'mpif.h'

      DIMENSION DRECV(N)

      CALL MPI_IRECV(DRECV,N,MPI_DOUBLE_PRECISION,ISRC,0,CGWORLD,IREQ
     &              ,IERR)

      END
