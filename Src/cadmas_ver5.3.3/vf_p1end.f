      SUBROUTINE VF_P1END()

CD=== 概要 ===========================================================

CDT   VF_P1END:並列環境を正常終了する

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C==== 実行 ===========================================================

C     -- 終了する --
      CALL VF_ZXMP_FINAL(IERR)

C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
