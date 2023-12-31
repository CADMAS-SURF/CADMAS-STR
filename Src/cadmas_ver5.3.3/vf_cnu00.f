      SUBROUTINE VF_CNU00(ANUT,ANU,NF,FF)

CD=== 概要 ===========================================================

CDT   VF_CNUT0:分子動粘性係数と渦動粘性係数の和を計算する

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ANUMBI.h'
      INCLUDE 'VF_APARAI.h'
      INCLUDE 'VF_APHYSR.h'

CD    -- 引数 --
CD    ANUT(@FOR-3D@) : IN  : R*8 : 渦動粘性係数νt
CD    ANU(@FOR-3D@)  : I/O : R*8 : 分子動粘性係数と渦動粘性係数の和
CD    NF(@FOR-3D@)   : IN  : I*4 : セルの状態を示すインデックス
CD    FF(@FOR-3D@)   : IN  : R*8 : VOF関数F
      DIMENSION ANUT(NUMI,NUMJ,NUMK),ANU(NUMI,NUMJ,NUMK)
      DIMENSION NF  (NUMI,NUMJ,NUMK)
      DIMENSION FF  (NUMI,NUMJ,NUMK)

C==== 実行 ===========================================================

CD    -- 並列時の範囲変更 --
      IA=1
      IB=NUMI
      JA=1
      JB=NUMJ
      IF (MYMIS.EQ.1) IA=2
      IF (MYMIE.EQ.1) IB=NUMI-1
      IF (MYMJS.EQ.1) JA=2
      IF (MYMJE.EQ.1) JB=NUMJ-1

CD    -- 乱流モデルを使用しない場合 --
      IF (LEQK.EQ.0) THEN
        DO 120 K=2,NUMK-1
          DO 110 J=JA,JB
            DO 100 I=IA,IB
              IF (NF(I,J,K).GT.-1) THEN
                ANU2=FF(I,J,K)*ANU0(1)+(1.D0-FF(I,J,K))*ANU0(2)
              ELSE
                ANU2=0.0D0
              ENDIF
              ANU(I,J,K)=ANU2
 100        CONTINUE
 110      CONTINUE
 120    CONTINUE

CD    -- 乱流モデルを使用する場合 --
      ELSE
        DO 220 K=2,NUMK-1
          DO 210 J=JA,JB
            DO 200 I=IA,IB
              IF (NF(I,J,K).GT.-1) THEN
                ANU2=FF(I,J,K)*ANU0(1)+(1.D0-FF(I,J,K))*ANU0(2)
              ELSE
                ANU2=0.0D0
              ENDIF
              ANU(I,J,K)=ANU2+ANUT(I,J,K)
 200        CONTINUE
 210      CONTINUE
 220    CONTINUE
      ENDIF

C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
