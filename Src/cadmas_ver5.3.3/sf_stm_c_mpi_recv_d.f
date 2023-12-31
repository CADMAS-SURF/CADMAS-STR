      SUBROUTINE SF_STM_C_MPI_RECV_D(DRECV,N,ISRC)

      USE MOD_COMM, ONLY: COMM_2FC_STM

      IMPLICIT REAL*8(A-H,O-Z)

      INCLUDE 'mpif.h'

      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ACPUTR.h'

      DIMENSION DRECV(N),ISTATUS(MPI_STATUS_SIZE)
!-----------------------------------------------------------------------
      CALL VF_A2CPUT(0,ICPUST,KCP9PL)

      CALL MPI_RECV(DRECV,N,MPI_DOUBLE_PRECISION,ISRC,1,COMM_2FC_STM
     &             ,ISTATUS,IERR)

      CALL VF_A2CPUT(0,ICPUEN,KCP9PL)

      END
