      SUBROUTINE IST_SOL(IST,SY,S,D,NNP,E,ANU,ST,IYLD,HD,ALP,ITO)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION IST(*),SY(*),S(6,*),D(21,*)

      SELECT CASE( NNP )
      CASE( 4 )
        NGP = 1
      CASE( 10 )
        NGP = 5
      CASE( 6 )
        NGP = 6
      CASE( 15 )
        NGP = 21
      CASE( 8 )
        NGP = 8
      CASE( 20 )
        NGP = 27
      END SELECT

      DO J = 1, NGP
        SELECT CASE( IST(J) )
        CASE( 0 )
          CALL SOL0(IST(J),SY(J),S(1,J),D(1,J),E,ANU,ST,IYLD,HD,ALP,ITO)
        CASE( 2 )
          CALL SOL2(IST(J),SY(J),S(1,J),D(1,J),E,ANU,IYLD,HD,ALP)
        END SELECT
      ENDDO

      END