      SUBROUTINE VF_PMGC2P(XX,YY,ZZ,UU,VV,WW,FF,
C----------------------------------------------------------2011.04 start
     &                     GGV,GGX,GGY,XPF,YPF,ZPF,IPF,JPF,KPF,
C----------------------------------------------------------2011.04 end
     &                     BCU,BCV,BCW,BCF,DBUF,NF,INDX,INDY,INDB)

CD=== 概要 ===========================================================

CDT   VF_PMGC2P:マルチグリッド環境の子の情報を親へ転送する

C==== 宣言 ===========================================================

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ANUMBI.h'
      INCLUDE 'VF_APARAI.h'

CD    -- 引数 --
CD    XX(MAXG1,NUMI)   : IN  : R*8 : x方向格子座標等
CD    YY(MAXG1,NUMJ)   : IN  : R*8 : y方向格子座標等
CD    ZZ(MAXG1,NUMK)   : IN  : R*8 : z方向格子座標等
CD    UU(@FOR-3D@)     : I/O : R*8 : x方向流速
CD    VV(@FOR-3D@)     : I/O : R*8 : y方向流速
CD    WW(@FOR-3D@)     : IN  : R*8 : z方向流速
CD    FF(@FOR-3D@)     : IN  : R*8 : VOF関数F
C----------------------------------------------------------2011.04 start
CD    XPF(NUMI)        : IN  : R*8 : x方向の親格子に対する補間係数
CD    YPF(NUMJ)        : IN  : R*8 : y方向の親格子に対する補間係数
CD    ZPF(NUMK)        : IN  : R*8 : z方向の親格子に対する補間係数
CD    IPF(MGPINF(1))   : IN  : I*4 : x方向の親格子1に対する格子の数
CD    JPF(MGPINF(2))   : IN  : I*4 : y方向の親格子1に対する格子の数
CD    KPF(MGPINF(3))   : IN  : I*4 : z方向の親格子1に対する格子の数
C----------------------------------------------------------2011.04 end
CD    BCU(NUMB,3)      : I/O : R*8 : x方向流速の境界値
CD    BCV(NUMB,3)      : I/O : R*8 : y方向流速の境界値
CD    BCW(NUMB,3)      : I/O : R*8 : z方向流速の境界値
CD    BCF(NUMB)        : I/O : R*8 : VOF関数Fの境界値
CD    DBUF(NUMBUF*MAXBUF) : OUT :R*8 : 並列用のバッファ
CD    NF(@FOR-3D@)     : IN  : I*4 : セルの状態を示すインデックス
CD    INDX(@FOR-3D@)   : IN  : I*4 : x面の状態を示すインデックス
CD    INDY(@FOR-3D@)   : IN  : I*4 : y面の状態を示すインデックス
CD    INDB(MAXB1,NUMB) : IN  : I*4 : 境界面のインデックス
      DIMENSION XX(MAXG1,NUMI),YY(MAXG1,NUMJ),ZZ(MAXG1,NUMK)
      DIMENSION UU  (NUMI,NUMJ,NUMK),VV  (NUMI,NUMJ,NUMK)
      DIMENSION WW  (NUMI,NUMJ,NUMK),FF  (NUMI,NUMJ,NUMK)
C----------------------------------------------------------2011.04 start
      DIMENSION GGV(NUMI,NUMJ,NUMK)
      DIMENSION GGX(NUMI,NUMJ,NUMK),GGY(NUMI,NUMJ,NUMK)
      DIMENSION XPF(NUMI),YPF(NUMJ),ZPF(NUMK)
      DIMENSION IPF(0:MGPINF(1)),JPF(0:MGPINF(2)),KPF(0:MGPINF(3))
C----------------------------------------------------------2011.04 end
      DIMENSION BCU (NUMB,3),BCV(NUMB,3),BCW(NUMB,3),BCF(NUMB)
      DIMENSION DBUF(NUMBUF*MAXBUF)
      DIMENSION NF  (NUMI,NUMJ,NUMK),INDX(NUMI,NUMJ,NUMK)
      DIMENSION INDY(NUMI,NUMJ,NUMK),INDB(MAXB1,NUMB)

C==== 実行 ===========================================================

CD    C@ CAKIY ID,JD,KD
C----------------------------------------------------------2011.04 start
C     デフォルト値
      IAMPARENT=0
C
C     水位接続を行う(=1)か否か(=0)
      ICONWL=0                                                  ! 要修正
C
C     親を領域分割するとき、以下の数値を設定（ハードコーディング）
C     (水位接続時に子供がデータを送る相手のRankで、実行時に### MPMD Subgroup Informationとして表示される値。左下から右に番号が増えていき、右端まできたら、一つ上に移動)
      IPARENT=4                                                 ! 要修正
C
      IF(MGRANK.EQ.IPARENT) IAMPARENT=1
C
C     接続比に応じて以下のID,JDを修正
      ID=2                                                      ! 要修正
      JD=2                                                      ! 要修正
      KD=1
      MGNV = 4
C----------------------------------------------------------2011.04 end

CD    -- 子の値を親に転送する --
      IF (MGPRNK.GE.0) THEN
C----------------------------------------------------------2011.04 start
C       DO 290 LL=1,4
C----------------------------------------------------------2011.04 end
          IS=1
          JS=1
          KS=1
          IE=MGPINF(1)
          JE=MGPINF(2)
          KE=MGPINF(3)
          IF (MGPINF(4).EQ.0) IS=IS+1
          IF (MGPINF(5).EQ.0) JS=JS+1
          IF (MGPINF(7).EQ.0) IE=IE-1
          IF (MGPINF(8).EQ.0) JE=JE-1
          NN=0
          IF (MGPINF(4).EQ.0) THEN
            DO 130 KK=KS,KE
              DO 120 JJ=JS,JE
C----------------------------------------------------------2011.04 start
C               NN=NN+1
C               VAL=0.0D0
C               K0=1+KD*(KK-1)
C               J0=1+JD*(JJ-1)
C               I0=2+ID*(IS-1)
C               DO 110 KL=1,KD
C                 K=K0+KL
C                 DO 100 JL=1,JD
C                   J=J0+JL
C                   IF (LL.EQ.1) THEN
C                     VAL=VAL+0.5D0*(FF(I0-1,J,K)+FF(I0,J,K))
C                   ENDIF
C                   IF (LL.EQ.2) THEN
C                     VAL=VAL+UU(I0,J,K)
C                   ENDIF
C                   IF (LL.EQ.3) THEN
C                     VAL=VAL+0.25D0*(VV(I0-1,J  ,K)+VV(I0,J  ,K)+
C     &                                VV(I0-1,J+1,K)+VV(I0,J+1,K))
C                   ENDIF
C                   IF (LL.EQ.4) THEN
C                     VAL=VAL+0.25D0*(WW(I0-1,J,K  )+WW(I0,J,K  )+
C    &                                WW(I0-1,J,K+1)+WW(I0,J,K+1))
C                   ENDIF
C100              CONTINUE
C110            CONTINUE
C               DBUF(NN)=VAL/DBLE(JD*KD)
                CALL VF_PMGC2P_CF(DBUF(NN+1),
     &                            XX,YY,ZZ,GGV,GGX,GGY,UU,VV,WW,FF,
     &                            XPF,YPF,ZPF,IPF,JPF,KPF,
     &                            BCU,BCV,BCW,BCF,NF,INDX,INDY,INDB,
     &                            IS,JJ,KK,-1,MGNV)
cdebug 20170512(s)
c                I=IS
c                X1=XX(1,IPF(I-1)+1)
c                Y1=0.5d0*(YY(1,JPF(JJ-1)+1)+YY(1,JPF(JJ)+1))
c                Z1=0.5d0*(ZZ(1,KPF(KK-1)+1)+ZZ(1,KPF(KK)+1))
c                write(100+mgrank,'(7f10.2)') X1,Y1,Z1,DBUF(NN+1:NN+4)
cdebug 20170512(e)
                NN = NN + MGNV
C----------------------------------------------------------2011.04 end
 120          CONTINUE
 130        CONTINUE
          ENDIF
          IF (MGPINF(7).EQ.0) THEN
            DO 180 KK=KS,KE
              DO 170 JJ=JS,JE
C----------------------------------------------------------2011.04 start
C               NN=NN+1
C               VAL=0.0D0
C               K0=1+KD*(KK-1)
C               J0=1+JD*(JJ-1)
C               I0=  ID*(IE+1)
C               DO 160 KL=1,KD
C                 K=K0+KL
C                 DO 150 JL=1,JD
C                   J=J0+JL
C                   IF (LL.EQ.1) THEN
C                     VAL=VAL+0.5D0*(FF(I0-1,J,K)+FF(I0,J,K))
C                   ENDIF
C                   IF (LL.EQ.2) THEN
C                     VAL=VAL+UU(I0,J,K)
C                   ENDIF
C                   IF (LL.EQ.3) THEN
C                     VAL=VAL+0.25D0*(VV(I0-1,J  ,K)+VV(I0,J  ,K)+
C    &                                VV(I0-1,J+1,K)+VV(I0,J+1,K))
C                   ENDIF
C                   IF (LL.EQ.4) THEN
C                     VAL=VAL+0.25D0*(WW(I0-1,J,K  )+WW(I0,J,K  )+
C    &                                WW(I0-1,J,K+1)+WW(I0,J,K+1))
C                   ENDIF
C150              CONTINUE
C160            CONTINUE
C               DBUF(NN)=VAL/DBLE(JD*KD)
                CALL VF_PMGC2P_CF(DBUF(NN+1),
     &                            XX,YY,ZZ,GGV,GGX,GGY,UU,VV,WW,FF,
     &                            XPF,YPF,ZPF,IPF,JPF,KPF,
     &                            BCU,BCV,BCW,BCF,NF,INDX,INDY,INDB,
     &                            IE+1,JJ,KK,+1,MGNV)
cdebug 20170512(s)
c                I=IE+1
c                X1=XX(1,IPF(I-1)+1)
c                Y1=0.5d0*(YY(1,JPF(JJ-1)+1)+YY(1,JPF(JJ)+1))
c                Z1=0.5d0*(ZZ(1,KPF(KK-1)+1)+ZZ(1,KPF(KK)+1))
c                write(100+mgrank,'(7f10.2)') X1,Y1,Z1,DBUF(NN+1:NN+4)
cdebug 20170512(e)
                NN = NN + MGNV
C----------------------------------------------------------2011.04 end
 170          CONTINUE
 180        CONTINUE
          ENDIF
          IF (MGPINF(5).EQ.0) THEN
            DO 230 KK=KS,KE
              DO 220 II=IS,IE
C----------------------------------------------------------2011.04 start
C               NN=NN+1
C               VAL=0.0D0
C               K0=1+KD*(KK-1)
C               J0=2+JD*(JS-1)
C               I0=1+ID*(II-1)
C               DO 210 KL=1,KD
C                 K=K0+KL
C                 DO 200 IL=1,ID
C                   I=I0+IL
C                   IF (LL.EQ.1) THEN
C                     VAL=VAL+0.5D0*(FF(I,J0-1,K)+FF(I,J0,K))
C                   ENDIF
C                   IF (LL.EQ.2) THEN
C                     VAL=VAL+0.25D0*(UU(I  ,J0-1,K)+UU(I  ,J0,K)+
C    &                                UU(I+1,J0-1,K)+UU(I+1,J0,K))
C                   ENDIF
C                   IF (LL.EQ.3) THEN
C                     VAL=VAL+VV(I,J0,K)
C                   ENDIF
C                   IF (LL.EQ.4) THEN
C                     VAL=VAL+0.25D0*(WW(I,J0-1,K  )+WW(I,J0,K  )+
C    &                                WW(I,J0-1,K+1)+WW(I,J0,K+1))
C                   ENDIF
C200              CONTINUE
C210            CONTINUE
C               DBUF(NN)=VAL/DBLE(ID*KD)
                CALL VF_PMGC2P_CF(DBUF(NN+1),
     &                            XX,YY,ZZ,GGV,GGX,GGY,UU,VV,WW,FF,
     &                            XPF,YPF,ZPF,IPF,JPF,KPF,
     &                            BCU,BCV,BCW,BCF,NF,INDX,INDY,INDB,
     &                            II,JS,KK,-2,MGNV)
cdebug 20170512(s)
c                J=JS
c                Y1=YY(1,JPF(J-1)+1)
c                X1=0.5d0*(XX(1,IPF(II-1)+1)+XX(1,IPF(II)+1))
c                Z1=0.5d0*(ZZ(1,KPF(KK-1)+1)+ZZ(1,KPF(KK)+1))
c                write(100+mgrank,'(7f10.2)') X1,Y1,Z1,DBUF(NN+1:NN+4)
cdebug 20170512(e)
                NN = NN + MGNV
C----------------------------------------------------------2011.04 end
 220          CONTINUE
 230        CONTINUE
          ENDIF
          IF (MGPINF(8).EQ.0) THEN
            DO 280 KK=KS,KE
              DO 270 II=IS,IE
C----------------------------------------------------------2011.04 start
C               NN=NN+1
C               VAL=0.0D0
C               K0=1+KD*(KK-1)
C               J0=  JD*(JE+1)
C               I0=1+ID*(II-1)
C               DO 260 KL=1,KD
C                 K=K0+KL
C                 DO 250 IL=1,ID
C                   I=I0+IL
C                   IF (LL.EQ.1) THEN
C                     VAL=VAL+0.5D0*(FF(I,J0-1,K)+FF(I,J0,K))
C                   ENDIF
C                   IF (LL.EQ.2) THEN
C                     VAL=VAL+0.25D0*(UU(I  ,J0-1,K)+UU(I  ,J0,K)+
C    &                                UU(I+1,J0-1,K)+UU(I+1,J0,K))
C                   ENDIF
C                   IF (LL.EQ.3) THEN
C                     VAL=VAL+VV(I,J0,K)
C                   ENDIF
C                   IF (LL.EQ.4) THEN
C                     VAL=VAL+0.25D0*(WW(I,J0-1,K  )+WW(I,J0,K  )+
C    &                                WW(I,J0-1,K+1)+WW(I,J0,K+1))
C                   ENDIF
C250              CONTINUE
C260            CONTINUE
C               DBUF(NN)=VAL/DBLE(ID*KD)
                CALL VF_PMGC2P_CF(DBUF(NN+1),
     &                            XX,YY,ZZ,GGV,GGX,GGY,UU,VV,WW,FF,
     &                            XPF,YPF,ZPF,IPF,JPF,KPF,
     &                            BCU,BCV,BCW,BCF,NF,INDX,INDY,INDB,
     &                            II,JE+1,KK,+2,MGNV)
cdebug 20170512(s)
c                J=JE+1
c                Y1=YY(1,JPF(J-1)+1)
c                X1=0.5d0*(XX(1,IPF(II-1)+1)+XX(1,IPF(II)+1))
c                Z1=0.5d0*(ZZ(1,KPF(KK-1)+1)+ZZ(1,KPF(KK)+1))
c                write(100+mgrank,'(7f10.2)') X1,Y1,Z1,DBUF(NN+1:NN+4)
cdebug 20170512(e)
                NN = NN + MGNV
C----------------------------------------------------------2011.04 end
 270          CONTINUE
 280        CONTINUE
          ENDIF
          CALL VF_ZXMG_ISENDD(DBUF,NN,MGPRNK,IREQ,IERR)
          CALL VF_ZXMG_WAIT(IREQ,IERR)
C----------------------------------------------------------2011.04 start
C290    CONTINUE
C----------------------------------------------------------2011.04 end
      ENDIF

CD    -- 子の値を親が受信する --
C
C 以下は2FCでは通らない
C
      DO 600 IC=1,MGCNUM
C----------------------------------------------------------2011.04 start
C       DO 590 LL=1,4
C----------------------------------------------------------2011.04 end
          IS=MGCPOS(1,IC)
          JS=MGCPOS(2,IC)
          KS=MGCPOS(3,IC)
          IE=MGCPOS(4,IC)
          JE=MGCPOS(5,IC)
          KE=MGCPOS(6,IC)
          IF (MGCINF(4,IC).EQ.0) IS=IS+1
          IF (MGCINF(5,IC).EQ.0) JS=JS+1
          IF (MGCINF(7,IC).EQ.0) IE=IE-1
          IF (MGCINF(8,IC).EQ.0) JE=JE-1
          NN=0
          IF (MGCINF(4,IC).EQ.0) NN=NN+(JE-JS+1)*(KE-KS+1)
          IF (MGCINF(7,IC).EQ.0) NN=NN+(JE-JS+1)*(KE-KS+1)
          IF (MGCINF(5,IC).EQ.0) NN=NN+(IE-IS+1)*(KE-KS+1)
          IF (MGCINF(8,IC).EQ.0) NN=NN+(IE-IS+1)*(KE-KS+1)
C----------------------------------------------------------2011.04 start
          NN = MGNV*NN
C----------------------------------------------------------2011.04 start
          CALL VF_ZXMG_IRECVD(DBUF,NN,MGCRNK(IC),IREQ,IERR)
          CALL VF_ZXMG_WAIT(IREQ,IERR)
          NN=0
          IF (MGCINF(4,IC).EQ.0) THEN
            DO 310 K=KS,KE
              DO 300 J=JS,JE
C----------------------------------------------------------2011.04 start
C               NN=NN+1
C               VAL=DBUF(NN)
C----------------------------------------------------------2011.04 end
                IF (INDX(IS,J,K).GE.1) THEN
                  L=INDX(IS,J,K)
C----------------------------------------------------------2011.04 start
C                 IF (LL.EQ.1) BCF(L)=VAL
C                 IF (LL.EQ.2) THEN
C                   BCU(L)=VAL
C                   UU(IS,J,K)=VAL
C                 ENDIF
C                 IF (LL.EQ.3) BCV(L)=VAL
C                 IF (LL.EQ.4) BCW(L)=VAL
                  BCF(L)=DBUF(NN+1)
                  BCU(L,:)=DBUF(NN+2)
                  BCV(L,:)=DBUF(NN+3)
                  BCW(L,:)=DBUF(NN+4)
                  UU(IS,J,K)=BCU(L,1)
                ENDIF
cdebug 20170512(s)
c                  I=IS
c                  X1=XX(1,I)
c                  Y1=0.5d0*(YY(1,J)+YY(1,J+1))
c                  Z1=0.5d0*(ZZ(1,K)+ZZ(1,K+1))
c                  write(100+mgrank,'(7f10.2)') X1,Y1,Z1,DBUF(NN+1:NN+4)
cdebug 20170512(e)
                  NN = NN + MGNV
C----------------------------------------------------------2011.04 end
C                ENDIF
 300          CONTINUE
 310        CONTINUE
          ENDIF
          IF (MGCINF(7,IC).EQ.0) THEN
            DO 360 K=KS,KE
              DO 350 J=JS,JE
C----------------------------------------------------------2011.04 start
C               NN=NN+1
C               VAL=DBUF(NN)
C----------------------------------------------------------2011.04 end
                IF (INDX(IE+1,J,K).GE.1) THEN
                  L=INDX(IE+1,J,K)
C----------------------------------------------------------2011.04 start
C                 IF (LL.EQ.1) BCF(L)=VAL
C                 IF (LL.EQ.2) THEN
C                   BCU(L)=VAL
C                   UU(IE+1,J,K)=VAL
C                 ENDIF
C                 IF (LL.EQ.3) BCV(L)=VAL
C                 IF (LL.EQ.4) BCW(L)=VAL
                  BCF(L)=DBUF(NN+1)
                  BCU(L,:)=DBUF(NN+2)
                  BCV(L,:)=DBUF(NN+3)
                  BCW(L,:)=DBUF(NN+4)
                  UU(IE+1,J,K)=BCU(L,1)
                ENDIF
cdebug 20170512(s)
c                  I=IE+1
c                  X1=XX(1,I)
c                  Y1=0.5d0*(YY(1,J)+YY(1,J+1))
c                  Z1=0.5d0*(ZZ(1,K)+ZZ(1,K+1))
c                  write(100+mgrank,'(7f10.2)') X1,Y1,Z1,DBUF(NN+1:NN+4)
cdebug 20170512(e)
                  NN = NN + MGNV
C----------------------------------------------------------2011.04 end
C                ENDIF
 350          CONTINUE
 360        CONTINUE
          ENDIF
          IF (MGCINF(5,IC).EQ.0) THEN
            DO 410 K=KS,KE
              DO 400 I=IS,IE
C----------------------------------------------------------2011.04 start
C               NN=NN+1
C               VAL=DBUF(NN)
C----------------------------------------------------------2011.04 end
                IF (INDY(I,JS,K).GE.1) THEN
                  L=INDY(I,JS,K)
C----------------------------------------------------------2011.04 start
C                 IF (LL.EQ.1) BCF(L)=VAL
C                 IF (LL.EQ.2) BCU(L)=VAL
C                 IF (LL.EQ.3) THEN
C                   BCV(L)=VAL
C                   VV(I,JS,K)=VAL
C                 ENDIF
C                 IF (LL.EQ.4) BCW(L)=VAL
                  BCF(L)=DBUF(NN+1)
                  BCU(L,:)=DBUF(NN+2)
                  BCV(L,:)=DBUF(NN+3)
                  BCW(L,:)=DBUF(NN+4)
                  VV(I,JS,K)=BCV(L,1)
                ENDIF
cdebug 20170512(s)
c                  J=JS
c                  Y1=YY(1,J)
c                  X1=0.5d0*(XX(1,I)+XX(1,I+1))
c                  Z1=0.5d0*(ZZ(1,K)+ZZ(1,K+1))
c                  write(100+mgrank,'(7f10.2)') X1,Y1,Z1,DBUF(NN+1:NN+4)
cdebug 20170512(e)
                  NN = NN + MGNV
C----------------------------------------------------------2011.04 end
C                ENDIF
 400          CONTINUE
 410        CONTINUE
          ENDIF
          IF (MGCINF(8,IC).EQ.0) THEN
            DO 460 K=KS,KE
              DO 450 I=IS,IE
C----------------------------------------------------------2011.04 start
C               NN=NN+1
C               VAL=DBUF(NN)
C----------------------------------------------------------2011.04 end
                IF (INDY(I,JE+1,K).GE.1) THEN
                  L=INDY(I,JE+1,K)
C----------------------------------------------------------2011.04 start
C                 IF (LL.EQ.1) BCF(L)=VAL
C                 IF (LL.EQ.2) BCU(L)=VAL
C                 IF (LL.EQ.3) THEN
C                   BCV(L)=VAL
C                   VV(I,JE+1,K)=VAL
C                 ENDIF
C                 IF (LL.EQ.4) BCW(L)=VAL
                  BCF(L)=DBUF(NN+1)
                  BCU(L,:)=DBUF(NN+2)
                  BCV(L,:)=DBUF(NN+3)
                  BCW(L,:)=DBUF(NN+4)
                  VV(I,JE+1,K)=BCV(L,1)
                ENDIF
cdebug 20170512(s)
c                  J=JE+1
c                  Y1=YY(1,J)
c                  X1=0.5d0*(XX(1,I)+XX(1,I+1))
c                  Z1=0.5d0*(ZZ(1,K)+ZZ(1,K+1))
c                  write(100+mgrank,'(7f10.2)') X1,Y1,Z1,DBUF(NN+1:NN+4)
cdebug 20170512(e)
                  NN = NN + MGNV
C----------------------------------------------------------2011.04 end
C                ENDIF
 450          CONTINUE
 460        CONTINUE
          ENDIF
C----------------------------------------------------------2011.04 start
C590    CONTINUE
C----------------------------------------------------------2011.04 end
 600  CONTINUE

      IF( ICONWL==0 ) GOTO 9999

CD    -- 子の値を親に転送する_水位 --
       IF (MGPRNK.GE.0) THEN
          IS=1
          JS=1
          KS=1
          IE=MGPINF(1)
          JE=MGPINF(2)
          KE=MGPINF(3)

C          write(*,*) 'SEND_NN2',' RANK',NN,mgrank
c          write(*,*) 'SEND_IS',' RANK',IS,mgrank
c          write(*,*) 'SEND_JS',' RANK',JS,mgrank
c          write(*,*) 'SEND_KS',' RANK',KS,mgrank
c          write(*,*) 'SEND_IE',' RANK',IE,mgrank
c          write(*,*) 'SEND_JE',' RANK',JE,mgrank
c          write(*,*) 'SEND_KE',' RANK',KE,mgrank

          NN=0
C-----西側境界------------------------2017.03 start.ohki
          IF (MGPINF(4).EQ.0) THEN
            IO=1+MYMIS              ! (IDxJDの範囲の左下のI)
            DO JJ=JS,JE
               JO=(JJ-1)*JD+MYMJS+1 ! (IDxJDの範囲の左下のJ)
C
               SF=0.0D0
               DO K=2,NUMK-1 ! 鉛直方向に積分する
                 DO J=JO,JO+JD-1  !  (IDxJDの範囲)
                 DO I=IO,IO+ID-1
                   IF(NF(I,J,K).LT.0) THEN
                    SF=SF+ZZ(2,K)*1.D0
                   ELSE 
                    SF=SF+ZZ(2,K)*FF(I,J,K)
                   END IF
                 ENDDO
                 ENDDO
               ENDDO
C
               SF=SF/DBLE(ID*JD)+ZZ(1,2) ! (IDxJDの範囲で平均化し、基準高さを付加）               
cdebug20170512(s)
c               X1=0.5D0*(XX(1,IO)+XX(1,IO+ID-1+1))
c               Y1=0.5D0*(YY(1,JO)+YY(1,JO+JD-1+1))
c               write(100+mgrank,'(a1,3f10.2)') 'w',X1,Y1,SF
c               NN=NN+1; DBUF(NN)=X1
c               NN=NN+1; DBUF(NN)=Y1
cdebug20170512(e)
               NN=NN+1
               DBUF(NN)=SF
            ENDDO
          ENDIF
C------東側境界-----------------------2017.03.start.ohki
          IF (MGPINF(7).EQ.0) THEN
            IO=(IE-1)*ID+MYMIS+1    ! (IDxJDの範囲の左下のI,=NUMI-2になるはず?)
            DO JJ=JS,JE
               JO=(JJ-1)*JD+MYMJS+1 ! (IDxJDの範囲の左下のJ)
C
               SF=0.0D0
               DO K=2,NUMK-1 ! 鉛直方向に積分する
                 DO J=JO,JO+JD-1  !  (IDxJDの範囲)
                 DO I=IO,IO+ID-1
                   IF(NF(I,J,K).LT.0) THEN
                    SF=SF+ZZ(2,K)*1.D0
                   ELSE 
                    SF=SF+ZZ(2,K)*FF(I,J,K)
                   END IF
                 ENDDO
                 ENDDO
               ENDDO
C
               SF=SF/DBLE(ID*JD)+ZZ(1,2) ! (IDxJDの範囲で平均化し、基準高さを付加）               
cdebug20170512(s)
c               X1=0.5D0*(XX(1,IO)+XX(1,IO+ID-1+1))
c               Y1=0.5D0*(YY(1,JO)+YY(1,JO+JD-1+1))
c               write(100+mgrank,'(a1,3f10.2)') 'e',X1,Y1,SF
c               NN=NN+1; DBUF(NN)=X1
c               NN=NN+1; DBUF(NN)=Y1
cdebug20170512(e)
               NN=NN+1
               DBUF(NN)=SF
            ENDDO
          ENDIF
C
C------南側境界-----------------------2017.03.start.ohki

          IF (MGPINF(5).EQ.0) THEN
            JO=1+MYMJS              ! (IDxJDの範囲の左下のJ)
            DO II=IS,IE
               IO=(II-1)*ID+MYMIS+1 ! (IDxJDの範囲の左下のJ)
C
               SF=0.0D0
               DO K=2,NUMK-1 ! 鉛直方向に積分する
                 DO J=JO,JO+JD-1  !  (IDxJDの範囲)
                 DO I=IO,IO+ID-1
                   IF(NF(I,J,K).LT.0) THEN
                    SF=SF+ZZ(2,K)*1.D0
                   ELSE 
                    SF=SF+ZZ(2,K)*FF(I,J,K)
                   END IF
                 ENDDO
                 ENDDO
               ENDDO
C
               SF=SF/DBLE(ID*JD)+ZZ(1,2) ! (IDxJDの範囲で平均化し、基準高さを付加）               
cdebug20170512(s)
c               X1=0.5D0*(XX(1,IO)+XX(1,IO+ID-1+1))
c               Y1=0.5D0*(YY(1,JO)+YY(1,JO+JD-1+1))
c               write(100+mgrank,'(a1,3f10.2)') 's',X1,Y1,SF
c               NN=NN+1; DBUF(NN)=X1
c               NN=NN+1; DBUF(NN)=Y1
cdebug20170512(e)
               NN=NN+1
               DBUF(NN)=SF
            ENDDO
          ENDIF
c          write(*,*) 'NNS',' RANK',NN,mgrank

C-------北側境界------------------------2017.03.start.ohki

          IF (MGPINF(8).EQ.0) THEN
            JO=(JE-1)*ID+MYMJS+1        ! (IDxJDの範囲の左下のJ,=NUMI-2になるはず?)
            DO II=IS,IE
               IO=(II-1)*ID+MYMIS+1 ! (IDxJDの範囲の左下のJ)
C
               SF=0.0D0
               DO K=2,NUMK-1 ! 鉛直方向に積分する
                 DO J=JO,JO+JD-1  !  (IDxJDの範囲)
                 DO I=IO,IO+ID-1
                   IF(NF(I,J,K).LT.0) THEN
                    SF=SF+ZZ(2,K)*1.D0
                   ELSE 
                    SF=SF+ZZ(2,K)*FF(I,J,K)
                   END IF
                 ENDDO
                 ENDDO
               ENDDO
C
               SF=SF/DBLE(ID*JD)+ZZ(1,2) ! (IDxJDの範囲で平均化し、基準高さを付加）               
cdebug20170512(s)
c               X1=0.5D0*(XX(1,IO)+XX(1,IO+ID-1+1))
c               Y1=0.5D0*(YY(1,JO)+YY(1,JO+JD-1+1))
c               write(100+mgrank,'(a1,3f10.2)') 'n',X1,Y1,SF
c               NN=NN+1; DBUF(NN)=X1
c               NN=NN+1; DBUF(NN)=Y1
cdebug20170512(e)
               NN=NN+1
               DBUF(NN)=SF
            ENDDO
          ENDIF
c      write(*,*) 'MGPRNK=',mgprank
cmod20170512          CALL VF_ZXMG_ISENDD(DBUF,NN,MGPRNK,IREQ,IERR)
          CALL VF_ZXMG_ISENDD(DBUF,NN,IPARENT,IREQ,IERR)
          CALL VF_ZXMG_WAIT(IREQ,IERR)
       ENDIF

c      write(*,*) 'NN1',' RANK',NN,mgrank

CD    -- 子の値を親が受信する --
cadd20170512(s)
      if( IAMPARENT.EQ.1 ) then
cadd20170512(e)
      DO 1110 IC=1,MGCNUM
          IS=MGCPOS(1,IC)
          JS=MGCPOS(2,IC)
          KS=MGCPOS(3,IC)
          IE=MGCPOS(4,IC)
          JE=MGCPOS(5,IC)
          KE=MGCPOS(6,IC)
c ..... ローカルインデックスに変換
          IS=IS-MYGIS+1
          JS=JS-MYGJS+1
          IE=IE-MYGIS+1
          JE=JE-MYGJS+1
          NN=0
cdebug20170512          IF (MGCINF(4,IC).EQ.0) NN=NN+(JE-JS+1)*3
cdebug20170512          IF (MGCINF(7,IC).EQ.0) NN=NN+(JE-JS+1)*3
cdebug20170512          IF (MGCINF(5,IC).EQ.0) NN=NN+(IE-IS+1)*3
cdebug20170512          IF (MGCINF(8,IC).EQ.0) NN=NN+(IE-IS+1)*3
          IF (MGCINF(4,IC).EQ.0) NN=NN+(JE-JS+1)
          IF (MGCINF(7,IC).EQ.0) NN=NN+(JE-JS+1)
          IF (MGCINF(5,IC).EQ.0) NN=NN+(IE-IS+1)
          IF (MGCINF(8,IC).EQ.0) NN=NN+(IE-IS+1)
c          write(*,*) 'NN2',' RANK',NN,mgrank
c          write(*,*) 'IS',' RANK',IS,mgrank
c          write(*,*) 'JS',' RANK',JS,mgrank
c          write(*,*) 'IE',' RANK',IE,mgrank
c          write(*,*) 'JE',' RANK',JE,mgrank
          CALL VF_ZXMG_IRECVD(DBUF,NN,MGCRNK(IC),IREQ,IERR)
          CALL VF_ZXMG_WAIT(IREQ,IERR)
          NN=0
C---------------西側境界＿受信--------------------2017.03.ohki
          IF (MGCINF(4,IC).EQ.0) THEN
              DO 780 J=JS,JE
cdebug20170512(s)
c               NN=NN+1; X1=DBUF(NN)
c               NN=NN+1; Y1=DBUF(NN)
cdebug20170512(e)
               NN=NN+1
               VAL=DBUF(NN)
               I=IS
cdebug20170512(s)
c               X2=0.5D0*(XX(1,I)+XX(1,I+1))
c               Y2=0.5D0*(YY(1,J)+YY(1,J+1))
c               write(100+mgrank,'(a1,3f10.2)') 'w',X2,Y2,VAL
cc               write(100+mgrank,'(a1,5f10.2)') 'w',X1,X2,Y1,Y2,VAL
cdebug20170512(e)
               DO 770 K=KS,KE
                IF(NF(I,J,K).LT.0) THEN
                 FF(I,J,K)=0.0D0
                ELSE 
                  IF(VAL.GT.ZZ(1,K+1)) THEN
                    FF(I,J,K)=1.D0
                  ELSE IF(VAL.LT.ZZ(1,K)) THEN
                    FF(I,J,K)=0.0D0
                  ELSE 
                    FF(I,J,K)=(VAL-ZZ(1,K))/ZZ(2,K)
                  ENDIF
                ENDIF
 770          CONTINUE
 780        CONTINUE
          ENDIF
C---------------東側境界＿受信--------------------2017.03.ohki
C          NN=0
          IF (MGCINF(7,IC).EQ.0) THEN
              DO 800 J=JS,JE
cdebug20170512(s)
c               NN=NN+1; X1=DBUF(NN)
c               NN=NN+1; Y1=DBUF(NN)
cdebug20170512(e)
               NN=NN+1
               VAL=DBUF(NN)
               I=IE
cdebug20170512(s)
c               X2=0.5D0*(XX(1,I)+XX(1,I+1))
c               Y2=0.5D0*(YY(1,J)+YY(1,J+1))
c               write(100+mgrank,'(a1,3f10.2)') 'e',X2,Y2,VAL
cc               write(100+mgrank,'(a1,5f10.2)') 'e',X1,X2,Y1,Y2,VAL
cdebug20170512(e)
               DO 790 K=KS,KE
                IF(NF(I,J,K).LT.0) THEN
                 FF(I,J,K)=0.0D0
                ELSE
                  IF(VAL.GT.ZZ(1,K+1)) THEN
                    FF(I,J,K)=1.D0
                  ELSE IF(VAL.LT.ZZ(1,K)) THEN
                    FF(I,J,K)=0.0D0
                  ELSE 
                    FF(I,J,K)=(VAL-ZZ(1,K))/ZZ(2,K)
                  ENDIF
                ENDIF
 790          CONTINUE
 800        CONTINUE
          ENDIF
C----------------南側境界＿受信--------------------2017.03.ohki
C          NN=0
          IF (MGCINF(5,IC).EQ.0) THEN
              DO 820 I=IS,IE
cdebug20170512(s)
c               NN=NN+1; X1=DBUF(NN)
c               NN=NN+1; Y1=DBUF(NN)
cdebug20170512(e)
               NN=NN+1
               VAL=DBUF(NN)
               J=JS
cdebug20170512(s)
c               X2=0.5D0*(XX(1,I)+XX(1,I+1))
c               Y2=0.5D0*(YY(1,J)+YY(1,J+1))
c               write(100+mgrank,'(a1,3f10.2)') 's',X2,Y2,VAL
cc               write(100+mgrank,'(a1,5f10.2)') 's',X1,X2,Y1,Y2,VAL
cdebug20170512(e)
               DO 810 K=KS,KE
                IF(NF(I,J,K).LT.0) THEN
                 FF(I,J,K)=0.0D0
                ELSE
                  IF(VAL.GT.ZZ(1,K+1)) THEN
                    FF(I,J,K)=1.D0
                  ELSE IF(VAL.LT.ZZ(1,K)) THEN
                    FF(I,J,K)=0.0D0
                  ELSE
                    FF(I,J,K)=(VAL-ZZ(1,K))/ZZ(2,K)
                  ENDIF
                ENDIF
 810          CONTINUE
 820        CONTINUE
          ENDIF
C-----------------北側境界＿受信--------------------2017.03.ohki
C          NN=0
          IF (MGCINF(8,IC).EQ.0) THEN
              DO 840 I=IS,IE
cdebug20170512(s)
c               NN=NN+1; X1=DBUF(NN)
c               NN=NN+1; Y1=DBUF(NN)
cdebug20170512(e)
               NN=NN+1
               VAL=DBUF(NN)
               J=JE
cdebug20170512(s)
c               X2=0.5D0*(XX(1,I)+XX(1,I+1))
c               Y2=0.5D0*(YY(1,J)+YY(1,J+1))
c               write(100+mgrank,'(a1,3f10.2)') 'n',X2,Y2,VAL
cc               write(100+mgrank,'(a1,5f10.2)') 'n',X1,X2,Y1,Y2,VAL
cdebug20170512(e)
               DO 830 K=KS,KE
                IF(NF(I,J,K).LT.0) THEN
                 FF(I,J,K)=0.0D0
                ELSE
                  IF(VAL.GT.ZZ(1,K+1)) THEN
                    FF(I,J,K)=1.D0
                  ELSE IF(VAL.LT.ZZ(1,K)) THEN
                    FF(I,J,K)=0.0D0
                  ELSE
                    FF(I,J,K)=(VAL-ZZ(1,K))/ZZ(2,K)
                  ENDIF
                ENDIF
 830          CONTINUE
 840        CONTINUE
          ENDIF
          
 1110  CONTINUE
cadd20170512(s)
      endif
cadd20170512(e)

c       call VF_ZXMG_BARRI(IERR)
c       call VF_ZXMG_ABORT(IERR)

C     -- 実行文の終了 --
      GOTO 9999

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
