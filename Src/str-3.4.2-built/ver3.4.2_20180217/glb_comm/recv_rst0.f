      SUBROUTINE RECV_RST0()

      USE MPI_PARAM

      CALL C_MPI_RECV_I(IRSTYP,1,IROOTC)
      CALL C_MPI_RECV_I(IRETYP,1,IROOTC)

      CALL M_MPI_BCAST_I(IRSTYP,1)
      CALL M_MPI_BCAST_I(IRETYP,1)

      END
