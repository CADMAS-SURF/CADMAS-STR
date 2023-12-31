      SUBROUTINE VF_VMODIF(XX,YY,ZZ,UU,VV,WW,PP,FF,FX,FY,FZ,
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!   &                     GGV,GLV,BCU,BCV,BCW,BCF,DBUF,PT,
     &                     GGV,GLV,PPO,BCU,BCV,BCW,BCF,DBUF,PT,
     &                     RHOG,
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     &                     NF,INDX,INDY,INDZ,INDC,INDB)

CD=== 概要 ===========================================================

CDT   VF_VMODIF:流速・圧力を補正する

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_AFILEI.h'
      INCLUDE 'VF_ANUMBI.h'
      INCLUDE 'VF_APARAI.h'
      INCLUDE 'VF_APHYSI.h'
      INCLUDE 'VF_APHYSR.h'
      INCLUDE 'VF_ATIMER.h'

CD    -- 引数 --
CD    XX(MAXG1,NUMI)    : IN  : R*8 : x方向格子座標等
CD    YY(MAXG1,NUMJ)    : IN  : R*8 : y方向格子座標等
CD    ZZ(MAXG1,NUMK)    : IN  : R*8 : z方向格子座標等
CD    UU(@FOR-3D@)      : I/O : R*8 : x方向流速
CD    VV(@FOR-3D@)      : I/O : R*8 : y方向流速
CD    WW(@FOR-3D@)      : I/O : R*8 : z方向流速
CD    PP(@FOR-3D@)      : I/O : R*8 : 圧力
CD    FF(@FOR-3D@)      : IN  : R*8 : VOF関数F
CD    FX(@FOR-3D@)      : IN  : R*8 : x方向スタッガードセルでのVOF関数Fx
CD    FY(@FOR-3D@)      : IN  : R*8 : y方向スタッガードセルでのVOF関数Fy
CD    FZ(@FOR-3D@)      : IN  : R*8 : z方向スタッガードセルでのVOF関数Fz
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
CD    PPO(@FOR-3D@)     : I/O : R*8 : 前時刻の圧力
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
CD    GGV(@FOR-3D@)     : IN  : R*8 : 空隙率
CD    GLV(@FOR-3D@)     : IN  : R*8 : =GGV+(1-GGV)*CM
CD    BCU(NUMB)         : I/O : R*8 : x方向流速の境界値
CD    BCV(NUMB)         : I/O : R*8 : y方向流速の境界値
CD    BCW(NUMB)         : I/O : R*8 : z方向流速の境界値
CD    BCF(NUMB)         : IN  : R*8 : VOF関数Fの境界値
CD    DBUF(NUMBUF*MAXBUF) : OUT :R*8 : 並列用のバッファ
CD    PT(@FOR-3D@)      : IN  : R*8 : ポテンシャル関数
CD    NF(@FOR-3D@)      : IN  : I*4 : セルの状態を示すインデックス
CD    INDX(@FOR-3D@)    : IN  : I*4 : x面の状態を示すインデックス
CD    INDY(@FOR-3D@)    : IN  : I*4 : y面の状態を示すインデックス
CD    INDZ(@FOR-3D@)    : IN  : I*4 : z面の状態を示すインデックス
CD    INDC(@FOR-3D@)    : IN  : I*4 : セルの計算状態を示すインデックス
CD    INDB(MAXB1,NUMB)  : IN  : I*4 : 境界面のインデックス
      DIMENSION XX(MAXG1,NUMI),YY(MAXG1,NUMJ),ZZ(MAXG1,NUMK)
      DIMENSION UU  (NUMI,NUMJ,NUMK),VV  (NUMI,NUMJ,NUMK)
      DIMENSION WW  (NUMI,NUMJ,NUMK),PP  (NUMI,NUMJ,NUMK)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      DIMENSION PPO (NUMI,NUMJ,NUMK)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      DIMENSION FF  (NUMI,NUMJ,NUMK),FX  (NUMI,NUMJ,NUMK)
      DIMENSION FY  (NUMI,NUMJ,NUMK),FZ  (NUMI,NUMJ,NUMK)
      DIMENSION GGV (NUMI,NUMJ,NUMK),GLV (NUMI,NUMJ,NUMK)
      DIMENSION BCU(NUMB),BCV(NUMB),BCW(NUMB),BCF(NUMB)
      DIMENSION DBUF(NUMBUF*MAXBUF) ,PT  (NUMI,NUMJ,NUMK)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      DIMENSION RHOG(NUMI,NUMJ,NUMK)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      DIMENSION NF  (NUMI,NUMJ,NUMK),INDX(NUMI,NUMJ,NUMK)
      DIMENSION INDY(NUMI,NUMJ,NUMK),INDZ(NUMI,NUMJ,NUMK)
      DIMENSION INDC(NUMI,NUMJ,NUMK),INDB(MAXB1,NUMB)

C==== 実行 ===========================================================

CD    -- 並列時の範囲変更 --
      IA=MYIS
      IB=MYIE
      JA=MYJS
      JB=MYJE
      IF (MYMIE.EQ.1) IB=IB+1
      IF (MYMJE.EQ.1) JB=JB+1

CD    -- x方向流速の補正 --
      DO 120 K=2,NUMK-1
        DO 110 J=MYJS,MYJE
          DO 100 I=IA,IB
C2F         IF (NF(I-1,J,K)*NF(I,J,K).EQ.0) THEN
CSTR        IF (INDC(I-1,J,K)*INDC(I,J,K).EQ.0) THEN
CSTR          IS=INDX(I,J,K)
            IS=INDX(I,J,K)
            IF (IS.GE.0) THEN
              I1=I-1
CBUG              IF (NF(I,J,K).NE.8) THEN
              IF (MAX0(NF(I1,J,K),NF(I,J,K)) .NE. 8) THEN
                LGBF=3
              ELSE
                LGBF=5
              ENDIF
CD            * 通常処理
              IF (IS.EQ.0) THEN
                GV=XX(6,I)*( XX(2,I  )*GGV(I  ,J,K)/GLV(I  ,J,K)
     &                      +XX(2,I-1)*GGV(I-1,J,K)/GLV(I-1,J,K))
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!              RI=1.0D0/(FX(I,J,K)*RHO0(1)+(1.0D0-FX(I,J,K))*RHO0(2))
                RHO02 = XX(6,I)*( XX(2,I  )*RHOG(I  ,J,K)
     &                           +XX(2,I-1)*RHOG(I-1,J,K))
                RI=1.0D0/(FX(I,J,K)*RHO0(1)+(1.0D0-FX(I,J,K))*RHO02)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                UU(I,J,K)=UU(I,J,K)
     &                      +XX(5,I)*GV*(PT(I,J,K)-PT(I-1,J,K))*RI
CD            * 境界処理(フリー)
              ELSEIF (INDB(LGBF,IS).EQ.4) THEN
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!              RI=1.0D0/(BCF(IS)*RHO0(1)+(1.0D0-BCF(IS))*RHO0(2))
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
CD              * x方向負側に構造物
                IF (INDB(2,IS).EQ.1) THEN
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                  RHO02 = RHOG(I,J,K)
                  RI=1.0D0/(BCF(IS)*RHO0(1)+(1.0D0-BCF(IS))*RHO02)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                  UU(I,J,K)=UU(I,J,K)+2.0D0*XX(4,I  )
     &                        *GGV(I  ,J,K)/GLV(I  ,J,K)*PT(I  ,J,K)*RI
                  BCU(IS)=UU(I,J,K)
CD              * x方向正側に構造物
                ELSE
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                  RHO02 = RHOG(I-1,J,K)
                  RI=1.0D0/(BCF(IS)*RHO0(1)+(1.0D0-BCF(IS))*RHO02)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                  UU(I,J,K)=UU(I,J,K)-2.0D0*XX(4,I-1)
     &                        *GGV(I-1,J,K)/GLV(I-1,J,K)*PT(I-1,J,K)*RI
                  BCU(IS)=UU(I,J,K)
                ENDIF
CD            * 境界処理(造波境界&水位固定のみ)
              ELSEIF (INDB(LGBF,IS).EQ.5) THEN
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!              RI=1.0D0/(BCF(IS)*RHO0(1)+(1.0D0-BCF(IS))*RHO0(2))
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
CD              * x方向負側に構造物
                IF (INDB(2,IS).EQ.1) THEN
                  IF (I.EQ.2    .AND. IBCTYP(2,1).EQ.-3
     &                          .AND. MTBTYP     .EQ. 3) THEN
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                    RHO02 = RHOG(I,J,K)
                    RI=1.0D0/(BCF(IS)*RHO0(1)+(1.0D0-BCF(IS))*RHO02)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                    UU(I,J,K)=UU(I,J,K)+2.0D0*XX(4,I  )
     &                         *GGV(I  ,J,K)/GLV(I  ,J,K)*PT(I  ,J,K)*RI
                    BCU(IS)=UU(I,J,K)
                  ENDIF
CD              * x方向正側に構造物
                ELSE
                  IF (I.EQ.NUMI .AND. IBCTYP(2,2).EQ.-3
     &                          .AND. MTBTYP     .EQ. 3) THEN
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                    RHO02 = RHOG(I-1,J,K)
                    RI=1.0D0/(BCF(IS)*RHO0(1)+(1.0D0-BCF(IS))*RHO02)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                    UU(I,J,K)=UU(I,J,K)-2.0D0*XX(4,I-1)
     &                         *GGV(I-1,J,K)/GLV(I-1,J,K)*PT(I-1,J,K)*RI
                    BCU(IS)=UU(I,J,K)
                  ENDIF
                ENDIF
              ENDIF
            ENDIF
 100      CONTINUE
 110    CONTINUE
 120  CONTINUE

CD    -- y方向流速の補正 --
      DO 220 K=2,NUMK-1
        DO 210 J=JA,JB
          DO 200 I=MYIS,MYIE
C2F         IF (NF(I,J-1,K)*NF(I,J,K).EQ.0) THEN
CSTR        IF (INDC(I,J-1,K)*INDC(I,J,K).EQ.0) THEN
CSTR          IS=INDY(I,J,K)
            IS=INDY(I,J,K)
            IF (IS.GE.0) THEN
              J1=J-1
CBUG              IF (NF(I,J,K).NE.8) THEN
              IF (MAX0(NF(I,J1,K),NF(I,J,K)) .NE. 8) THEN
                LGBF=3
              ELSE
                LGBF=5
              ENDIF
CD            * 通常処理
              IF (IS.EQ.0) THEN
                GV=YY(6,J)*( YY(2,J  )*GGV(I,J  ,K)/GLV(I,J  ,K)
     &                      +YY(2,J-1)*GGV(I,J-1,K)/GLV(I,J-1,K))
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!              RI=1.0D0/(FY(I,J,K)*RHO0(1)+(1.0D0-FY(I,J,K))*RHO0(2))
                RHO02 = YY(6,J)*( YY(2,J  )*RHOG(I,J  ,K)
     &                           +YY(2,J-1)*RHOG(I,J-1,K))
                RI=1.0D0/(FY(I,J,K)*RHO0(1)+(1.0D0-FY(I,J,K))*RHO02)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                VV(I,J,K)=VV(I,J,K)
     &                      +YY(5,J)*GV*(PT(I,J,K)-PT(I,J-1,K))*RI
CD            * 境界処理(フリー)
              ELSEIF (INDB(LGBF,IS).EQ.4) THEN
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!              RI=1.0D0/(BCF(IS)*RHO0(1)+(1.0D0-BCF(IS))*RHO0(2))
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
CD              * y方向負側に構造物
                IF (INDB(2,IS).EQ.3) THEN
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                  RHO02 = RHOG(I,J,K)
                  RI=1.0D0/(BCF(IS)*RHO0(1)+(1.0D0-BCF(IS))*RHO02)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                  VV(I,J,K)=VV(I,J,K)+2.0D0*YY(4,J  )
     &                       *GGV(I,J  ,K)/GLV(I,J  ,K)*PT(I,J  ,K)*RI
                  BCV(IS)=VV(I,J,K)
CD              * y方向正側に構造物
                ELSE
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                  RHO02 = RHOG(I,J-1,K)
                  RI=1.0D0/(BCF(IS)*RHO0(1)+(1.0D0-BCF(IS))*RHO02)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                  VV(I,J,K)=VV(I,J,K)-2.0D0*YY(4,J-1)
     &                       *GGV(I,J-1,K)/GLV(I,J-1,K)*PT(I,J-1,K)*RI
                  BCV(IS)=VV(I,J,K)
                ENDIF
CD            * 境界処理(造波境界&水位固定のみ)
              ELSEIF (INDB(LGBF,IS).EQ.5) THEN
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!              RI=1.0D0/(BCF(IS)*RHO0(1)+(1.0D0-BCF(IS))*RHO0(2))
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
CD              * y方向負側に構造物
                IF (INDB(2,IS).EQ.3) THEN
                  IF (J.EQ.2    .AND. IBCTYP(2,3).EQ.-3
     &                          .AND. MTBTYP     .EQ. 3) THEN
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                    RHO02 = RHOG(I,J,K) 
                    RI=1.0D0/(BCF(IS)*RHO0(1)+(1.0D0-BCF(IS))*RHO02)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                    VV(I,J,K)=VV(I,J,K)+2.0D0*YY(4,J  )
     &                         *GGV(I,J  ,K)/GLV(I,J  ,K)*PT(I,J  ,K)*RI
                    BCV(IS)=VV(I,J,K)
                  ENDIF
CD              * y方向正側に構造物
                ELSE
                  IF (J.EQ.NUMJ .AND. IBCTYP(2,4).EQ.-3
     &                          .AND. MTBTYP     .EQ. 3) THEN
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                    RHO02 = RHOG(I,J-1,K) 
                    RI=1.0D0/(BCF(IS)*RHO0(1)+(1.0D0-BCF(IS))*RHO02)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                    VV(I,J,K)=VV(I,J,K)-2.0D0*YY(4,J-1)
     &                         *GGV(I,J-1,K)/GLV(I,J-1,K)*PT(I,J-1,K)*RI
                    BCV(IS)=VV(I,J,K)
                  ENDIF
                ENDIF
              ENDIF
            ENDIF
 200      CONTINUE
 210    CONTINUE
 220  CONTINUE

CD    -- z方向流速の補正 --
      DO 320 K=2,NUMK
        DO 310 J=MYJS,MYJE
          DO 300 I=MYIS,MYIE
C2F         IF (NF(I,J,K-1)*NF(I,J,K).EQ.0) THEN
CSTR        IF (INDC(I,J,K-1)*INDC(I,J,K).EQ.0) THEN
CSTR          IS=INDZ(I,J,K)
            IS=INDZ(I,J,K)
            IF (IS.GE.0) THEN
              K1=K-1
CBUG              IF (NF(I,J,K).NE.8) THEN
              IF (MAX0(NF(I,J,K1),NF(I,J,K)) .NE. 8) THEN
                LGBF=3
              ELSE
                LGBF=5
              ENDIF
CD            * 通常処理
              IF (IS.EQ.0) THEN
                GV=ZZ(6,K)*( ZZ(2,K  )*GGV(I,J,K  )/GLV(I,J,K  )
     &                      +ZZ(2,K-1)*GGV(I,J,K-1)/GLV(I,J,K-1))
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!              RI=1.0D0/(FZ(I,J,K)*RHO0(1)+(1.0D0-FZ(I,J,K))*RHO0(2))
                RHO02 = ZZ(6,K)*( ZZ(2,K  )*RHOG(I,J,K  )
     &                           +ZZ(2,K-1)*RHOG(I,J,K-1))
                RI=1.0D0/(FZ(I,J,K)*RHO0(1)+(1.0D0-FZ(I,J,K))*RHO02)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                WW(I,J,K)=WW(I,J,K)
     &                      +ZZ(5,K)*GV*(PT(I,J,K)-PT(I,J,K-1))*RI
CD            * 境界処理(フリー)
              ELSEIF (INDB(LGBF,IS).EQ.4) THEN
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!              RI=1.0D0/(BCF(IS)*RHO0(1)+(1.0D0-BCF(IS))*RHO0(2))
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
CD              * z方向負側に構造物
                IF (INDB(2,IS).EQ.5) THEN
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                  RHO02 = RHOG(I,J,K)
                  RI=1.0D0/(BCF(IS)*RHO0(1)+(1.0D0-BCF(IS))*RHO02)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                  WW(I,J,K)=WW(I,J,K)+2.0D0*ZZ(4,K  )
     &                       *GGV(I,J,K  )/GLV(I,J,K  )*PT(I,J,K  )*RI
                  BCW(IS)=WW(I,J,K)
CD              * z方向正側に構造物
                ELSE
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                  RHO02 = RHOG(I,J,K-1)
                  RI=1.0D0/(BCF(IS)*RHO0(1)+(1.0D0-BCF(IS))*RHO02)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                  WW(I,J,K)=WW(I,J,K)-2.0D0*ZZ(4,K-1)
     &                       *GGV(I,J,K-1)/GLV(I,J,K-1)*PT(I,J,K-1)*RI
                  BCW(IS)=WW(I,J,K)
                ENDIF
              ENDIF
            ENDIF
 300      CONTINUE
 310    CONTINUE
 320  CONTINUE

CD    -- 圧力の補正 --
C2F   RT=RHO0/DTNOW
      DO 420 K=2,NUMK-1
        DO 410 J=MYJS,MYJE
          DO 400 I=MYIS,MYIE
            IF (INDC(I,J,K).NE.-1) THEN
ccc              RT=(FF(I,J,K)*RHO0(1)+(1.0D0-FF(I,J,K))*RHO0(2))/DTNOW
ccc              PP(I,J,K)=PP(I,J,K)-RT*PT(I,J,K)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!            PP(I,J,K)=PP(I,J,K)-PT(I,J,K)/DTNOW
              PP(I,J,K)=PP(I,J,K)-PT(I,J,K)/DTNOW
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            ENDIF
 400      CONTINUE
 410    CONTINUE
 420  CONTINUE

      CALL VF_P3SRD2(PP,DBUF,0)
      CALL VF_P3SRD2(UU,DBUF,1)
      CALL VF_P3SRD2(VV,DBUF,2)
      CALL VF_P3SRD2(WW,DBUF,3)

C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
