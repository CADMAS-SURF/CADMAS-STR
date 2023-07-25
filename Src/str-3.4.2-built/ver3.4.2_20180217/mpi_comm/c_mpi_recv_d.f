      SUBROUTINE C_MPI_RECV_D(DRECV,N,ISRC)

      USE MPI_PARAM

      IMPLICIT REAL*8(A-H,O-Z)

      INCLUDE 'mpif.h'

      DIMENSION DRECV(N),ISTATUS(MPI_STATUS_SIZE)

      CALL MPI_RECV(DRECV,N,MPI_DOUBLE_PRECISION,ISRC,1,CPLWORLD
     &             ,ISTATUS,IERR)

      END