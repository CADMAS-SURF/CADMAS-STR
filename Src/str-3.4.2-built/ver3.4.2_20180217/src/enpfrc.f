      SUBROUTINE ENPFRC(FC,EPS,S,GRID,UG,VG,ITYP,NP,NNP,RODA,BARD,BVEC,D
     &                 ,IGNL,ITO)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION FC(*),EPS(*),S(*),GRID(3,*),UG(6,*),NP(NNP),D(*)
     &         ,VG(3,2,2),BARD(*),BVEC(3)

      SELECT CASE( ITYP )
      CASE( 2, 6 )
        SELECT CASE( NNP )
        CASE( 4 )
          CALL ENPFTE1(FC,EPS,S,GRID,UG,NP,D,IGNL,ITO)
        CASE( 10 )
          CALL ENPFTE2(FC,EPS,S,GRID,UG,NP,D,IGNL,ITO)
        CASE( 6 )
          CALL ENPFPN1(FC,EPS,S,GRID,UG,NP,D,IGNL,ITO)
        CASE( 15 )
          CALL ENPFPN2(FC,EPS,S,GRID,UG,NP,D,IGNL,ITO)
        CASE( 8 )
          CALL ENPFHX1(FC,EPS,S,GRID,UG,NP,D,IGNL,ITO)
        CASE( 20 )
          CALL ENPFHX2(FC,EPS,S,GRID,UG,NP,D,IGNL,ITO)
        END SELECT
      CASE( 3 )
        CALL ENPFTRS(FC,EPS,S,GRID,UG,NP,RODA,D,IGNL)
      CASE( 4 )
        CALL ENPFBM(FC,EPS,S,GRID,UG,VG,NP,BARD(2),BARD(1),BVEC,D,IGNL)
      END SELECT

      END
