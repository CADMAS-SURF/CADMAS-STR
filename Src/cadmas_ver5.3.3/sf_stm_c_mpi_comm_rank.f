      SUBROUTINE SF_STM_C_MPI_COMM_RANK(MYRANK)

      USE MOD_COMM, ONLY: COMM_2FC_STM

      INCLUDE 'mpif.h'

      CALL MPI_COMM_RANK(COMM_2FC_STM,MYRANK,IERR)

      END
