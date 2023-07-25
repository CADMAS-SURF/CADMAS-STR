      SUBROUTINE LOADTR1(GRID,NOD,PRESS,DIR,ID,ICRD,TR,FC,ITO)
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION XYZ(3,3),GRID(3,*),NOD(3),FC(3,3),DIR(3),E(3)
     &         ,V12(3),V13(3),XYZG(3),TRG(3,3),TR(12,*),E0(3),ICRD(*)
     &         ,PRESS(3)
C-----------------------------------------------------------------------
      DO 100 I=1,3
        CALL SHIFT1(XYZ(1,I),GRID(1,NOD(I)),3)
  100 CONTINUE
C
      CALL SUBVEC(V12,XYZ(1,2),XYZ(1,1),3)
      CALL SUBVEC(V13,XYZ(1,3),XYZ(1,1),3)
      CALL CROSS1(V12,V13,S)
      S=S*0.5D0
C
      CALL VECML1(RNORM,DIR,DIR,3)
      IF(RNORM.NE.0.) THEN
        CALL DIRCOS(E,DIR,3)
        IF( ID .GT. 0 ) THEN
          DO 300 J=1,3
            XYZG(J)=( XYZ(J,1) + XYZ(J,2) + XYZ(J,3) ) / 3.
  300     CONTINUE
          CALL TRNSMTX6(TRG,ICRD(ID),XYZG,TR(1,ID),ITO)
          CALL AXB(E0,TRG,E,3,3,1)
        ELSE
          CALL SHIFT1(E0,E,3)
        ENDIF
      ELSE
        CALL CROSS2(V12,V13,E0)
      ENDIF
C
      PS0 = PRESS(1) + PRESS(2) + PRESS(3)
C
      DO 200 I=1,3
        PS = PS0 + PRESS(I)
        CALL RMULT1(FC(1,I),E0,S/12.D0*PS,3)
  200 CONTINUE
C
      RETURN
      END
