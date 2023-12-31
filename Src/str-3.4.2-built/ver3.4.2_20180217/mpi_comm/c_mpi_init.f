      SUBROUTINE C_MPI_INIT()

      USE MOD_COMM, ONLY: COMM_WORK_2FC_STR, COMM_MODEL
      USE MPI_PARAM

      INCLUDE 'mpif.h'

      CPLWORLD = COMM_WORK_2FC_STR

      CALL MPI_COMM_SIZE(CPLWORLD,NPROCS0,IERR)

      CALL MPI_COMM_RANK(CPLWORLD,MYRANK0,IERR)

      MYWORLD = COMM_MODEL

      CALL MPI_COMM_SIZE(MYWORLD,NPROCS,IERR)

      CALL MPI_COMM_RANK(MYWORLD,MYRANK,IERR)

      IF( MYRANK > 0 ) THEN
        CALL MPI_COMM_SPLIT(MYWORLD,0,0,CGWORLD,IERR)
      ELSE
!        CALL MPI_COMM_SPLIT(MYWORLD,MPI_UNDFINED,0,CGWORLD,IERR)
        CALL MPI_COMM_SPLIT(MYWORLD,1,0,CGWORLD,IERR)
      ENDIF

      ICPL = 0

      IF( NPROCS0 > NPROCS ) THEN

        ICPL = 2

!        IRANK = 0
!
!        CALL C_MPI_ALLREDUCE_I(IRANK,IROOTC,1)
!
!        IF( MYRANK == 0 ) THEN
!          IRANK = MYRANK0
!        ELSE
!          IRANK = 0
!        ENDIF
!
!        CALL C_MPI_ALLREDUCE_I(IRANK,IROOTS,1)

        IRANK = 0

        IF( MYRANK0 == 0 ) THEN
          IROOTC = IRANK
          DO I = 1, NPROCS0 - 1
            CALL C_MPI_RECV_I(IRANK,1,I)
            IROOTC = IROOTC + IRANK
          ENDDO
          DO I = 1, NPROCS0 - 1
            CALL C_MPI_SEND_I(IROOTC,1,I)
          ENDDO
        ELSE
          CALL C_MPI_SEND_I(IRANK,1,0)
          CALL C_MPI_RECV_I(IROOTC,1,0)
        ENDIF

        IF( MYRANK == 0 ) THEN
          IRANK = MYRANK0
        ELSE
          IRANK = 0
        ENDIF

        IF( MYRANK0 == 0 ) THEN
          IROOTS = IRANK
          DO I = 1, NPROCS0 - 1
            CALL C_MPI_RECV_I(IRANK,1,I)
            IROOTS = IROOTS + IRANK
          ENDDO
          DO I = 1, NPROCS0 - 1
            CALL C_MPI_SEND_I(IROOTS,1,I)
          ENDDO
        ELSE
          CALL C_MPI_SEND_I(IRANK,1,0)
          CALL C_MPI_RECV_I(IROOTS,1,0)
        ENDIF

      ENDIF

      IF( ICPL == 2 .AND. MYRANK == 0 ) CALL C_MPI_RECV_I(ISTM,1,IROOTC)

      CALL M_MPI_BCAST_I(ISTM,1)

      END
