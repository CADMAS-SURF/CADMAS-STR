      SUBROUTINE EMASSMTX(EMASS,GRID,ITYP,NDF,ND,IBTYP,KN,AMAT,RODA,BARD
     &                   ,LUMP,ITO)
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION EMASS(*),AMAT(*),GRID(3,*),KN(ND),EMASR(400),BARD(*)
C-----------------------------------------------------------------------
      IF(ITYP.EQ.2 .OR. ITYP.EQ.6) THEN
        IF(ND.EQ.4) THEN
          CALL EMASSTE1(EMASR,AMAT,GRID,KN,LUMP,ITO)
        ELSEIF(ND.EQ.10) THEN
          CALL EMASSTE2(EMASR,AMAT,GRID,KN,LUMP,ITO)
        ELSEIF(ND.EQ.6) THEN
          CALL EMASSPN1(EMASR,AMAT,GRID,KN,LUMP,ITO)
        ELSEIF(ND.EQ.15) THEN
          CALL EMASSPN2(EMASR,AMAT,GRID,KN,LUMP,ITO)
        ELSE
          CALL EMASSHX2(EMASR,AMAT,GRID,KN,ND,LUMP,ITO)
        ENDIF
      ELSEIF(ITYP .EQ. 3) THEN
        CALL EMASSTRS(EMASR,AMAT,RODA,GRID,KN,LUMP,ITO)
      ELSEIF(ITYP.EQ.4) THEN
        CALL EMASSBM(EMASR,AMAT,BARD,GRID,IBTYP,KN,LUMP,ITO)
      ENDIF
C
      CALL TRNSEMASS(EMASS,EMASR,ND,NDF,LUMP)
C
      RETURN
      END
