      SUBROUTINE GMTXTE1_S(EKPP,ECPP,X,ITO)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION EKPP(10),ECPP(10),RN(4),X(3,4),DNDX(3,4)

      RN(:) = .25D0

      CALL DERXTE1(DNDX,DET,X,ITO)

      CALL MATML(EKPP,1,DNDX,3,DNDX,2,4,4,3)
      CALL MATML(ECPP,1,RN,3,RN,2,4,4,1)

      EKPP(:) = EKPP(:)*DET/6.D0
      ECPP(:) = ECPP(:)*DET/6.D0

      END