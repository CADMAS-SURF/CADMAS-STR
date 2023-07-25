      SUBROUTINE NODSET5(ND,KN,NODCOR,NODOP,NOD,ITO)
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION KN(*),NOD(ND)
C-----------------------------------------------------------------------
      DO 100 I=1,6
        IF(NODCOR.EQ.KN(I)) GOTO(201,202,203,204,205,206) I
  100 CONTINUE
C
      GOTO 900
C
  201 IF(NODOP.EQ.KN(5)) THEN
        NOD( 1) = KN( 1)
        NOD( 2) = KN( 4)
        NOD( 3) = KN( 5)
        NOD( 4) = KN( 2)
        IF(ND.EQ.8) THEN
          NOD( 5) = KN(10)
          NOD( 6) = KN(13)
          NOD( 7) = KN(11)
          NOD( 8) = KN( 7)
        ENDIF
      ELSEIF(NODOP.EQ.KN(6)) THEN
        NOD( 1) = KN( 1)
        NOD( 2) = KN( 3)
        NOD( 3) = KN( 6)
        NOD( 4) = KN( 4)
        IF(ND.EQ.8) THEN
          NOD( 5) = KN( 9)
          NOD( 6) = KN(12)
          NOD( 7) = KN(15)
          NOD( 8) = KN(10)
        ENDIF
      ELSEIF(NODOP.EQ.0) THEN
        NOD( 1) = KN( 1)
        NOD( 2) = KN( 2)
        NOD( 3) = KN( 3)
        IF(ND.EQ.6) THEN
          NOD( 4) = KN( 7)
          NOD( 5) = KN( 8)
          NOD( 6) = KN( 9)
        ENDIF
      ELSE
        GOTO 900
      ENDIF
      RETURN
C
  202 IF(NODOP.EQ.KN(6)) THEN
        NOD( 1) = KN( 2)
        NOD( 2) = KN( 5)
        NOD( 3) = KN( 6)
        NOD( 4) = KN( 3)
        IF(ND.EQ.8) THEN
          NOD( 5) = KN(11)
          NOD( 6) = KN(14)
          NOD( 7) = KN(12)
          NOD( 8) = KN( 8)
        ENDIF
      ELSEIF(NODOP.EQ.KN(4)) THEN
        NOD( 1) = KN( 2)
        NOD( 2) = KN( 1)
        NOD( 3) = KN( 4)
        NOD( 4) = KN( 5)
        IF(ND.EQ.8) THEN
          NOD( 5) = KN( 7)
          NOD( 6) = KN(10)
          NOD( 7) = KN(13)
          NOD( 8) = KN(11)
        ENDIF
      ELSEIF(NODOP.EQ.0) THEN
        NOD( 1) = KN( 2)
        NOD( 2) = KN( 3)
        NOD( 3) = KN( 1)
        IF(ND.EQ.6) THEN
          NOD( 4) = KN( 8)
          NOD( 5) = KN( 9)
          NOD( 6) = KN( 7)
        ENDIF
      ELSE
        GOTO 900
      ENDIF
      RETURN
C
  203 IF(NODOP.EQ.KN(4)) THEN
        NOD( 1) = KN( 3)
        NOD( 2) = KN( 6)
        NOD( 3) = KN( 4)
        NOD( 4) = KN( 1)
        IF(ND.EQ.8) THEN
          NOD( 5) = KN(12)
          NOD( 6) = KN(15)
          NOD( 7) = KN(10)
          NOD( 8) = KN( 9)
        ENDIF
      ELSEIF(NODOP.EQ.KN(5)) THEN
        NOD( 1) = KN( 3)
        NOD( 2) = KN( 2)
        NOD( 3) = KN( 5)
        NOD( 4) = KN( 6)
        IF(ND.EQ.8) THEN
          NOD( 5) = KN( 8)
          NOD( 6) = KN(11)
          NOD( 7) = KN(14)
          NOD( 8) = KN(12)
        ENDIF
      ELSEIF(NODOP.EQ.0) THEN
        NOD( 1) = KN( 3)
        NOD( 2) = KN( 1)
        NOD( 3) = KN( 2)
        IF(ND.EQ.6) THEN
          NOD( 4) = KN( 9)
          NOD( 5) = KN( 7)
          NOD( 6) = KN( 8)
        ENDIF
      ELSE
        GOTO 900
      ENDIF
      RETURN
C
  204 IF(NODOP.EQ.KN(2)) THEN
        NOD( 1) = KN( 4)
        NOD( 2) = KN( 5)
        NOD( 3) = KN( 2)
        NOD( 4) = KN( 1)
        IF(ND.EQ.8) THEN
          NOD( 5) = KN(13)
          NOD( 6) = KN(11)
          NOD( 7) = KN( 7)
          NOD( 8) = KN(10)
        ENDIF
      ELSEIF(NODOP.EQ.KN(3)) THEN
        NOD( 1) = KN( 4)
        NOD( 2) = KN( 1)
        NOD( 3) = KN( 3)
        NOD( 4) = KN( 6)
        IF(ND.EQ.8) THEN
          NOD( 5) = KN(10)
          NOD( 6) = KN( 9)
          NOD( 7) = KN(12)
          NOD( 8) = KN(15)
        ENDIF
      ELSEIF(NODOP.EQ.0) THEN
        NOD( 1) = KN( 4)
        NOD( 2) = KN( 6)
        NOD( 3) = KN( 5)
        IF(ND.EQ.6) THEN
          NOD( 4) = KN(15)
          NOD( 5) = KN(14)
          NOD( 6) = KN(13)
        ENDIF
      ELSE
        GOTO 900
      ENDIF
      RETURN
C
  205 IF(NODOP.EQ.KN(3)) THEN
        NOD( 1) = KN( 5)
        NOD( 2) = KN( 6)
        NOD( 3) = KN( 3)
        NOD( 4) = KN( 2)
        IF(ND.EQ.8) THEN
          NOD( 5) = KN(14)
          NOD( 6) = KN(12)
          NOD( 7) = KN( 8)
          NOD( 8) = KN(11)
        ENDIF
      ELSEIF(NODOP.EQ.KN(1)) THEN
        NOD( 1) = KN( 5)
        NOD( 2) = KN( 2)
        NOD( 3) = KN( 1)
        NOD( 4) = KN( 4)
        IF(ND.EQ.8) THEN
          NOD( 5) = KN(11)
          NOD( 6) = KN( 7)
          NOD( 7) = KN(10)
          NOD( 8) = KN(13)
        ENDIF
      ELSEIF(NODOP.EQ.0) THEN
        NOD( 1) = KN( 5)
        NOD( 2) = KN( 4)
        NOD( 3) = KN( 6)
        IF(ND.EQ.6) THEN
          NOD( 4) = KN(13)
          NOD( 5) = KN(15)
          NOD( 6) = KN(14)
        ENDIF
      ELSE
        GOTO 900
      ENDIF
      RETURN
C
  206 IF(NODOP.EQ.KN(1)) THEN
        NOD( 1) = KN( 6)
        NOD( 2) = KN( 4)
        NOD( 3) = KN( 1)
        NOD( 4) = KN( 3)
        IF(ND.EQ.8) THEN
          NOD( 5) = KN(15)
          NOD( 6) = KN(10)
          NOD( 7) = KN( 9)
          NOD( 8) = KN(12)
        ENDIF
      ELSEIF(NODOP.EQ.KN(2)) THEN
        NOD( 1) = KN( 6)
        NOD( 2) = KN( 3)
        NOD( 3) = KN( 2)
        NOD( 4) = KN( 5)
        IF(ND.EQ.8) THEN
          NOD( 5) = KN(12)
          NOD( 6) = KN( 8)
          NOD( 7) = KN(11)
          NOD( 8) = KN(14)
        ENDIF
      ELSEIF(NODOP.EQ.0) THEN
        NOD( 1) = KN( 6)
        NOD( 2) = KN( 5)
        NOD( 3) = KN( 4)
        IF(ND.EQ.6) THEN
          NOD( 4) = KN(14)
          NOD( 5) = KN(13)
          NOD( 6) = KN(15)
        ENDIF
      ELSE
        GOTO 900
      ENDIF
      RETURN
C
  900 WRITE(ITO,*) 'STOP IN SUB. NODSET4!'
      CALL ERRSTP(900,ITO)
C
      END
