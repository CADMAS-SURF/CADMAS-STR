      SUBROUTINE SF_PRSTRN(PRES0,PP,IPGRID,IGNO,POS,ZZ,NF)

      IMPLICIT REAL*8(A-H,O-Z)

      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_AFILEI.h'
      INCLUDE 'VF_ANUMBI.h'
      INCLUDE 'VF_APARAI.h'
      INCLUDE 'VF_ATIMER.h'
      INCLUDE 'SF_STRUCT.h'

      DIMENSION PRES0(NNOD0),PP(NUMI,NUMJ,NUMK),IPGRID(2,NNOD)
     &         ,POS(3,NNOD),ZZ(MAXG1,NUMK),NF(NUMI,NUMJ,NUMK),IGNO(NNOD)

      CALL SF_GPRES(PRES0,PP,IPGRID,IGNO,POS,ZZ,NF)

      IF( MYRANK == 0 ) THEN
        WRITE(ITPFIL,'(1PE15.7)') TNOW
        DO I = 1, NNOD0
          WRITE(ITPFIL,'(1PE15.7)') PRES0(I)
        ENDDO
      ENDIF

      END