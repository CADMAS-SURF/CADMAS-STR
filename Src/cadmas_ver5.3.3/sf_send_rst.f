      SUBROUTINE SF_SEND_RST()

      IMPLICIT REAL*8(A-H,O-Z)

      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_AFILEI.h'
      INCLUDE 'VF_APARAI.h'
      INCLUDE 'SF_STRUCT.h'

      IF( MYRANK == 0 ) THEN
        CALL SF_C_MPI_SEND_I(IRSTYP,1,IROOTS)
        CALL SF_C_MPI_SEND_I(IRETYP,1,IROOTS)
        IF( ISTM == 1 ) CALL SF_STM_C_MPI_SEND_I(IRETYP,1,IROOTSTM)
      ENDIF

      END
