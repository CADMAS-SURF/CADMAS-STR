      SUBROUTINE WT_RESTART(IFL,IFLT,KK,ISTEP,TIM3,DT1,ISEND)

      USE M_VAL
      USE MPI_PARAM

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION KK(*),NNOW(100),TNOW(100)

      DATA INIT / 0 /

      IF( ISTEP == KK(7) ) ISEND = 1

      N = 1

      IRW = 0

      IF( ICPL == 2 ) THEN
        IF( ISEND == 1 ) THEN
          CALL C_MPI_RECV_I(N,1,IROOTC)
          IF( N > 0 ) THEN
            CALL C_MPI_RECV_I(NNOW,N,IROOTC)
            CALL C_MPI_RECV_D(TNOW,N,IROOTC)
            IRW = 1
          ENDIF
        ENDIF
      ELSE
        IF( IROUT(ISTEP) > 0 ) IRW = 1
      ENDIF

      IF( IRW == 0 ) RETURN

      IF( ICPL == 2 .AND. INIT == 0 ) THEN
        INIT = 1
        WRITE(IFLT,'(A/A/)')
     &  '     [ C A D M A S ]          [ S  T  R ]',
     &  '    STEP        TIME       STEP        TIME'
      ENDIF

      WRITE(IFL) ISTEP,TIM3,DT1,NNOW(N)

      WRITE(IFL) UG1,UG2,UG3,POS,POSO,FTID,FCO,FCK,FCD,FCM,FCMD
     &          ,DMT,EPSG,SIGG

      IF( KK(16) > 0 ) WRITE(IFL) VG

      IF( KK(80) == 1 ) WRITE(IFL) IST,SIGY

      IF( KK(92) > 0 ) WRITE(IFL) ISLV,RSLV,U0,RL0,IFRIC,ISTK,FRIC

      IF( KK(25) > 0 ) WRITE(IFL) PG1,PG2,PG3,FCP

      IF( ISTM == 1 ) WRITE(IFL) GRID,SUMZ

      IF( ICPL == 2 ) THEN
        DO I = 1, N
          WRITE(IFLT,'(2(I8,F15.6))') NNOW(I),TNOW(I),ISTEP,TIM3
        ENDDO
      ELSE
        WRITE(IFLT,'(F15.6)') TIM3
      ENDIF

      END
