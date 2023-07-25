      SUBROUTINE SEND_POS0(NNOD,NELM,IGEO)

      USE MPI_PARAM

      IMPLICIT REAL*8(A-H,O-Z)

      REAL(8), POINTER :: POS(:,:),FLUX(:,:)

      ALLOCATE( POS(3,NNOD) )
      CALL GATHER_NODAL_D(POS,3)
      CALL C_MPI_SEND_D(POS,3*NNOD,IROOTC)
      DEALLOCATE( POS )

      IF( IGEO > 0 ) THEN
        ALLOCATE( FLUX(3,NELM) )
        CALL C_MPI_SEND_D(FLUX,3*NELM,IROOTC)
        DEALLOCATE( FLUX )
      ENDIF

      END