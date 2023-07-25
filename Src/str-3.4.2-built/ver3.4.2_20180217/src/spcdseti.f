      SUBROUTINE SPCDSETI(ISID,INDOF,KK,ISPD,NSPD)
     &               
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION KK(*),ISPD(2,*),NSPD(7,*),INDOF(6,*)
C-----------------------------------------------------------------------
      NISPD = KK(38)
C
      DO 410 J=1,NISPD
        IF(ISPD(1,J) .EQ. ISID) THEN
          CALL ADDSET(J,ISPD,IS,NN)
          DO 411 K=1,NN
            IAD=IS-1+K
            NODFIX=NSPD(1,IAD)
            DO 412 L=1,6
              IF(NSPD(L+1,IAD) .EQ. 1) THEN
                INDOF(L,NODFIX)=-1
              ENDIF
  412       CONTINUE
  411     CONTINUE
          GOTO 500
        ENDIF
  410 CONTINUE
C
  500 RETURN
      END
