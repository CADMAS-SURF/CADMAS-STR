      SUBROUTINE EVOLHX2(VOL,X,NNP,ITO)

      IMPLICIT REAL*8(A-H,O-Z)

      DIMENSION XG(3,3),WG(3,3),X(3,NNP),P(3,20)

      DATA XG / 3*0.0D0
     &        , -.577350269189626D0, .577350269189626D0, 0.0D0
     &        , -.774596669241483D0, 0.0D0, .774596669241483D0 /
      DATA WG / 2.0D0, 2*0.0D0
     &        , 2*1.0D0, 0.0D0
     &        , 0.55555555555555556D0, 0.88888888888888889D0
     &        , 0.55555555555555556D0 /

      IF( NNP == 8 ) THEN
        NG = 2
      ELSE
        NG = 3
      ENDIF

      VOL = 0.

      DO I = 1, NG
      DO J = 1, NG
      DO K = 1, NG

        XG1 = XG(I,NG)
        XG2 = XG(J,NG)
        XG3 = XG(K,NG)
        WGT = WG(I,NG)*WG(J,NG)*WG(K,NG)

        CALL DERHX2(XG1,XG2,XG3,NNP,P)
        CALL DET3(DET,NNP,P,X,ITO)

        VOL = VOL + DET * WGT

      ENDDO
      ENDDO
      ENDDO

      END
