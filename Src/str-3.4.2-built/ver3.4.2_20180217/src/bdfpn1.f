      SUBROUTINE BDFPN1(FC,X,GRAV,R0,IGR,ITO)
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION P(3,6),RG(3),SG(3),TG(2),X(3,6),XJI(3,3),H(6),VEC(3)
     &         ,GRAV(3),R0(3),FC(3,6)
C
      DATA RG / .5D0, .5D0, 0.D0 /
      DATA SG / 0.D0, .5D0, .5D0 /
      DATA TG / -.577350269189626D0, .577350269189626D0 /
C-----------------------------------------------------------------------
      DO JG = 1, 2
        DO IG = 1, 3
C
          CALL DERPN2(P,6,RG(IG),SG(IG),TG(JG))
          CALL JACOB3(6,P,X,XJI,DET,ITO)
C
          CALL SFNPN2(H,6,RG(IG),SG(IG),TG(JG))
C
          DO I = 1, 6
C
            IF( IGR == 1 ) THEN
              VEC(:) = GRAV(:)
            ELSE
              VEC(:) = X(:,I) - R0(:)
            ENDIF
C
            FC(:,I) = FC(:,I) + H(I) * VEC(:) * DET / 6.D0
C
          ENDDO
C
        ENDDO
      ENDDO
C
      END
