      SUBROUTINE VF_CSETUP(XX,YY,ZZ,BCU,BCV,BCW,BCP,BCF,BCVI,BCK,BCE,
     &                     NF,INDX,INDY,INDZ,INDB,INDBK,INDBE)

CD=== 概要 ===========================================================

CDT   VF_CSETUP:解析条件から各種情報を構築する

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_AFILEI.h'
      INCLUDE 'VF_AFILER.h'
      INCLUDE 'VF_ANUMBI.h'
      INCLUDE 'VF_APARAI.h'
      INCLUDE 'VF_APHYSI.h'
      INCLUDE 'VF_APHYSR.h'

CD    -- 引数 --
CD    XX(MAXG1,NUMI)   : IN  : R*8 : x方向格子座標等
CD    YY(MAXG1,NUMJ)   : IN  : R*8 : y方向格子座標等
CD    ZZ(MAXG1,NUMK)   : IN  : R*8 : z方向格子座標等
CD    BCU(NUMB,3)      : I/O : R*8 : x方向流速の境界値
CD    BCV(NUMB,3)      : I/O : R*8 : y方向流速の境界値
CD    BCW(NUMB,3)      : I/O : R*8 : z方向流速の境界値
CD    BCP(NUMB,3)      : I/O : R*8 : 圧力の境界値
CD    BCF(NUMB)        : I/O : R*8 : VOF関数Fの境界値
CD    BCVI(NUMB)       : I/O : R*8 : 流速の境界条件(壁面の粗さ)
CD    BCK(NUMB,3)      : I/O : R*8 : 乱流エネルギの境界値
CD    BCE(NUMB,3)      : I/O : R*8 : 乱流エネルギ散逸の境界値
CD    NF(@FOR-3D@)     : IN  : I*4 : セルの状態を示すインデックス
CD    INDX(@FOR-3D@)   : IN  : I*4 : x面の状態を示すインデックス
CD    INDY(@FOR-3D@)   : IN  : I*4 : y面の状態を示すインデックス
CD    INDZ(@FOR-3D@)   : IN  : I*4 : z面の状態を示すインデックス
CD    INDB(MAXB1,NUMB) : I/O : I*4 : 境界面のインデックス
CD    INDBK(MAXBK1,NUMB) : I/O : I*4 : 乱流エネルギの境界条件
CD    INDBE(MAXBE1,NUMB) : I/O : I*4 : 乱流エネルギ散逸の境界条件
      DIMENSION XX(MAXG1,NUMI),YY(MAXG1,NUMJ),ZZ(MAXG1,NUMK)
      DIMENSION BCU(NUMB,3),BCV(NUMB,3),BCW(NUMB,3),BCP(NUMB,3)
      DIMENSION BCF(NUMB),BCVI(NUMB),BCK(NUMB,3),BCE(NUMB,3)
      DIMENSION NF  (NUMI,NUMJ,NUMK),INDX(NUMI,NUMJ,NUMK)
      DIMENSION INDY(NUMI,NUMJ,NUMK),INDZ(NUMI,NUMJ,NUMK)
      DIMENSION INDB(MAXB1,NUMB),INDBK(MAXBK1,NUMB),INDBE(MAXBE1,NUMB)

C==== 実行 ===========================================================

CD    -- 並列時のシフト分 --
      IP=MYGIS-1
      JP=MYGJS-1

CD    -- 特殊境界の準備 --
      DO 140 JD=1,4

C       * 法線方向への造波境界の初期設定
        IF     (IBCTYP(1,JD).EQ.1) THEN
          N =IBCTYP(2,JD)
          L1=IBCTYP(3,JD)
          L2=IBCTYP(4,JD)
          D =BCTYP (1,JD)
          H =BCTYP (2,JD)
          T =BCTYP (3,JD)
          IF     (N.EQ.-3 .AND. MTBTYP.EQ.1) THEN
            IF (D.LT.ZERO)
     &        CALL VF_A2ERR('VF_CSETUP','(WAVE-BC DEPTH) NOT FOUND.')
            IF (T.LT.ZERO)
     &        CALL VF_A2ERR('VF_CSETUP','(WAVE-BC PERIOD) NOT FOUND.')
          ELSEIF (N.EQ.-3 .AND. MTBTYP.EQ.2) THEN
            IF (T.LT.ZERO)
     &        CALL VF_A2ERR('VF_CSETUP','(WAVE-BC PERIOD) NOT FOUND.')
          ELSEIF (N.EQ.-3 .AND. MTBTYP.EQ.3) THEN
            IF (T.LT.ZERO)
     &        CALL VF_A2ERR('VF_CSETUP','(WAVE-BC PERIOD) NOT FOUND.')
          ELSE
            IF (D.LT.ZERO)
     &        CALL VF_A2ERR('VF_CSETUP','(WAVE-BC DEPTH) NOT FOUND.')
            IF (H.LT.ZERO)
     &        CALL VF_A2ERR('VF_CSETUP','(WAVE-BC HEIGHT) NOT FOUND.')
            IF (T.LT.ZERO)
     &        CALL VF_A2ERR('VF_CSETUP','(WAVE-BC PERIOD) NOT FOUND.')
          ENDIF
          IF (N.EQ.-3) THEN
            IF (ABS(BCTYP(9,JD)).GT.ZERO)
     &        CALL VF_A2ERR('VF_CSETUP','(WAVE-BC ANGLE) CAN NOT USE.')
          ENDIF
C         * Ursell数を計算し、必要ならば25.0で切り替える
          WUS=0.0D0
          IF (D.GE.ZERO) WUS=GRZ0*H*T*T/D/D
          IF (N.EQ.0) THEN
            IF (WUS.LE.25.0D0) THEN
              N=-2
            ELSE
              N=-1
            ENDIF
            IBCTYP(2,JD)=N
          ENDIF
C         * 波長を計算する
          CALL VF_CWMAK0(D,T,H,WLN,N)
C         * 波高がゼロになるときの無次元位相を求める
          IF (N.NE.-3) THEN
            CALL VF_CWZERO(WT0,N)
          ELSE
            WT0=DMTBT0
          ENDIF
C         * 波長、Ursell数、無次元位相を格納する
          BCTYP(4,JD)=WLN
          BCTYP(5,JD)=WUS
          BCTYP(6,JD)=WT0

C       * 法線方向への開境界の初期設定
        ELSEIF (IBCTYP(1,JD).EQ.2) THEN
          N =IBCTYP(2,JD)
          L1=IBCTYP(3,JD)
          L2=IBCTYP(4,JD)
          D =BCTYP (1,JD)
          T =BCTYP (3,JD)
          IF (D.LT.ZERO)
     &        CALL VF_A2ERR('VF_CSETUP','(OPEN-BC DEPTH) NOT FOUND.')
          IF (T.LT.ZERO)
     &        CALL VF_A2ERR('VF_CSETUP','(OPEN-BC PERIOD) NOT FOUND.')
C         * 波長を計算する
          CALL VF_COMAK0(D,T,PI,GRZ0,WLN,N)
          BCTYP(2,JD)=0.0D0
          BCTYP(4,JD)=WLN
          BCTYP(5,JD)=0.0D0
          BCTYP(6,JD)=WLN/T
        ENDIF

C       * 特殊境界をインデックスに設定
        IF     (IBCTYP(1,JD).EQ.1) THEN
          IB=5
        ELSEIF (IBCTYP(1,JD).EQ.2) THEN
          IB=7
        ELSE
          IB=0
        ENDIF
        IF (IB.NE.0) THEN
          IF (JD.EQ.1 .OR. JD.EQ.2) THEN
            IF (JD.EQ.1) THEN
              I=2
            ELSE
              I=NUMI0
            ENDIF
            IF (L1.LE.0) THEN
              L1=2
              L2=NUMJ0-1
              IBCTYP(3,JD)=L1
              IBCTYP(4,JD)=L2
            ENDIF
            IF (MYGIS.LE.I .AND. I.LE.MYGIE) THEN
              DO 110 K=2,NUMK-1
                DO 100 J=L1,L2
                  IF (MYGJS.LE.J .AND. J.LE.MYGJE) THEN
                    L=INDX(I-IP,J-JP,K)
                    IF (L.GE.1) THEN
                      BCU (  L,1)=0.0D0
                      BCV (  L,1)=0.0D0
                      BCW (  L,1)=0.0D0
                      BCP (  L,1)=0.0D0
                      INDB(3,L)=IB
                      BCU (  L,2)=0.0D0
                      BCV (  L,2)=0.0D0
                      BCW (  L,2)=0.0D0
                      BCP (  L,2)=0.0D0
                      INDB(4,L)=IB
                      BCF (  L  )=0.0D0
                      BCVI(  L  )=0.0D0
C2F                   INDB(5,L)=IB
C2F                   BCU (  L,3)=0.0D0
C2F                   BCV (  L,3)=0.0D0
C2F                   BCW (  L,3)=0.0D0
C2F                   BCP (  L,3)=0.0D0
                    ENDIF
                  ENDIF
 100            CONTINUE
 110          CONTINUE
            ENDIF
          ELSE
            IF (JD.EQ.3) THEN
              J=2
            ELSE
              J=NUMJ0
            ENDIF
            IF (L1.LE.0) THEN
              L1=2
              L2=NUMI0-1
              IBCTYP(3,JD)=L1
              IBCTYP(4,JD)=L2
            ENDIF
            IF (MYGJS.LE.J .AND. J.LE.MYGJE) THEN
              DO 130 K=2,NUMK-1
                DO 120 I=L1,L2
                  IF (MYGIS.LE.I .AND. I.LE.MYGIE) THEN
                    L=INDY(I-IP,J-JP,K)
                    IF (L.GE.1) THEN
                      BCU (  L,1)=0.0D0
                      BCV (  L,1)=0.0D0
                      BCW (  L,1)=0.0D0
                      BCP (  L,1)=0.0D0
                      INDB(3,L)=IB
                      BCU (  L,2)=0.0D0
                      BCV (  L,2)=0.0D0
                      BCW (  L,2)=0.0D0
                      BCP (  L,2)=0.0D0
                      INDB(4,L)=IB
                      BCF (  L  )=0.0D0
                      BCVI(  L  )=0.0D0
C2F                   INDB(5,L)=IB
C2F                   BCU (  L,3)=0.0D0
C2F                   BCV (  L,3)=0.0D0
C2F                   BCW (  L,3)=0.0D0
C2F                   BCP (  L,3)=0.0D0
                    ENDIF
                  ENDIF
 120            CONTINUE
 130          CONTINUE
            ENDIF
          ENDIF
        ENDIF
 140  CONTINUE

CD    -- 減衰領域を使用する場合のチェック --
      DO 200 JD=1,4
        IF (IDAMP(JD).NE.-1) THEN
          N =IDAMP(  JD)
          W =DAMP (3,JD)
          D =DAMP (4,JD)
          IF (N.LT.0)
     &        CALL VF_A2ERR('VF_CSETUP','(DAMP DEGREE) NOT FOUND.')
          IF (W.LT.ZERO)
     &        CALL VF_A2ERR('VF_CSETUP','(DAMP WIDTH) NOT FOUND.')
          IF (D.LT.ZERO)
     &        CALL VF_A2ERR('VF_CSETUP','(DAMP DEPTH) NOT FOUND.')
        ENDIF
 200  CONTINUE

CD    -- 流速の対数則・粗面指定を乱流エネルギと乱流エネルギ散逸へ --
      IF (LEQK.NE.0) THEN
        DO 300 L=1,NUMB
          IF (INDB(3,L).EQ.6) THEN
            INDBK(1,L)=6
            BCK  (L,2)=0.0D0
            INDBE(1,L)=6
            BCE  (L,2)=0.0D0
          ELSEIF (INDB(3,L).EQ.8) THEN
            INDBK(1,L)=8
            BCK  (L,2)=0.0D0
            INDBE(1,L)=8
            BCE  (L,2)=0.0D0
          ENDIF
          IF (INDB(5,L).EQ.6) THEN
            INDBK(2,L)=6
            BCK  (L,3)=0.0D0
            INDBE(2,L)=6
            BCE  (L,3)=0.0D0
          ELSEIF (INDB(5,L).EQ.8) THEN
            INDBK(2,L)=8
            BCK  (L,3)=0.0D0
            INDBE(2,L)=8
            BCE  (L,3)=0.0D0
          ENDIF
 300    CONTINUE
      ENDIF

CAKIY @@@@@@@@@@@@@@@@@@@@@@
CD    -- 造波ソースの準備 --
CAKIY @@@@@@@@@@@@@@@@@@@@@@

C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
