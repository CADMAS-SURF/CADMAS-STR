      SUBROUTINE M_MPI_SEND_I(ISEND,N,IDEST)

      USE MPI_PARAM

      INCLUDE 'mpif.h'

      DIMENSION ISEND(N)

      CALL MPI_SEND(ISEND,N,MPI_INTEGER,IDEST,1,MYWORLD,IERR)

      END