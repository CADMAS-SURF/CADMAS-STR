      SUBROUTINE CGINDX1(IDX,NSIZ,MIDX,NIDX,N,IPREV,NEXT,LAST,IDOF,NDOF
     &                  ,ISLV,ITO)
C
      DIMENSION IDX(NSIZ,MIDX),N(MIDX),IPREV(MIDX),NEXT(MIDX),LAST(*)
     &         ,IDOF(NDOF)
C
      DO I = 1, NDOF
C
        II = IDOF(I)
C
        IF( II <= 0 ) CYCLE
C
        DO J = 1, NDOF
C
          JJ = IDOF(J)
C
          SELECT CASE( ISLV )
          CASE( 1 )
            IF( JJ <= 0 .OR. JJ >= II ) CYCLE
          CASE( 2:4 )
            IF( JJ <= II ) CYCLE
          CASE( 11 )
            IF( JJ <= 0 .OR. JJ == II ) CYCLE
          CASE( 13,14 )
            IF( JJ <= 0 ) CYCLE
          END SELECT
C
          IROW = II
C
          DO
C
            NN = N(IROW)
C
            DO K = 1, NN
              IF( JJ < IDX(K,IROW) ) THEN
                CALL REARRANGE(IDX,NSIZ,NIDX,N,IPREV,NEXT,LAST,II,IROW,K
     &                        ,JJ)
                GOTO 10
              ELSEIF( JJ == IDX(K,IROW) ) THEN
                GOTO 10
              ENDIF
            ENDDO
C
            IF( NEXT(IROW) == 0 ) EXIT
C
            IROW = NEXT(IROW)
C
          ENDDO
C
          CALL REARRANGE(IDX,NSIZ,NIDX,N,IPREV,NEXT,LAST,II,IROW,NN+1
     &                  ,JJ)
C
   10     IF( NIDX == MIDX ) THEN
            WRITE(ITO,'(2A)') 'MORE MEMORY IS NECESSARY FOR NEXT '
     &                       ,'PROCESS OF MATRIX SOLVER.'
            CALL ERRSTP(20,ITO)
          ENDIF
C
        ENDDO
C
      ENDDO
C
      END