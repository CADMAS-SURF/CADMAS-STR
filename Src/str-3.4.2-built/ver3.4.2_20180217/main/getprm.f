      SUBROUTINE GETPRM(FLNAME,MEM,ISLV,IPRE,ICHK)
C
      IMPLICIT REAL*8(A-H,O-Z)
      CHARACTER*256 STRING,FLNAME
C
      FLNAME = 'data.bdf' ! INPUT DATA FILE
      MEM    = 8000       ! MEMORY : 8000(MB)
      ISLV   = 4          ! MATRIX SOLVER : MUMPS
      IPRE   = 1          ! CG PRE-CONDITIONING : INCOMPLETE CHOLESKY DECOMPOSITION
C
      IF( ICHK == 1 ) RETURN
C
      NARG=IARGC()
C
      DO 100 I=1,NARG,2
        CALL GETARG(I,STRING)
        IF(STRING(1:2) .EQ. '-F') THEN
          CALL GETARG(I+1,STRING)
          READ(STRING,'(A256)') FLNAME
        ELSEIF(STRING(1:2) .EQ. '-M') THEN
          CALL GETARG(I+1,STRING)
          READ(STRING,'(I6)') MEM
        ELSEIF(STRING(1:2) .EQ. '-S') THEN
          CALL GETARG(I+1,STRING)
          READ(STRING,'(I1)') ISLV
        ELSEIF(STRING(1:2) .EQ. '-P') THEN
          CALL GETARG(I+1,STRING)
          READ(STRING,'(I1)') IPRE
        ELSE
          WRITE(*,*) 'SOLVER PARAMETER IS WRONG, PROGRAM STOPPED!'
          STOP
        ENDIF
  100 CONTINUE
C
      END