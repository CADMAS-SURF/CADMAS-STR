      SUBROUTINE RD_MATT1( J_MATT1, CHAR )
C
      CHARACTER*80 CHAR
      DIMENSION J_MATT1(6)
C
      READ(CHAR,'(BN,8X,2I8,8X,3I8,8X,I8)') MID, JE, JNU, JRHO, JA, JGE
C
      J_MATT1(1) = MID
      J_MATT1(2) = JE
      J_MATT1(3) = JNU
      J_MATT1(4) = JRHO
      J_MATT1(5) = JGE
      J_MATT1(6) = JA
C
      END
