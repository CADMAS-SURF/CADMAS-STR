      INTEGER MAX_CADMAS
      INTEGER MAX_NIST,MAX_NJST,MAX_NKST

      PARAMETER ( MAX_CADMAS = 256 )
      PARAMETER ( MAX_NIST = 500 )
      PARAMETER ( MAX_NJST = 500 )
      PARAMETER ( MAX_NKST = 100 )

      INTEGER IB_STOC
      INTEGER NB_CADMAS,LB_CADMAS,IB_CADMAS(MAX_CADMAS)
      INTEGER NB_SC
      INTEGER NIST,NJST,NKST,IWST,IEST,JSST,JNST
      INTEGER MIST(MAX_NIST+1),MJST(MAX_NJST+1),MKST(MAX_NKST+1)

      COMMON /VF_ASTOCI/IB_STOC,NB_CADMAS,LB_CADMAS,IB_CADMAS,NB_SC
     &                 ,NIST,NJST,NKST,IWST,IEST,JSST,JNST
     &                 ,MIST,MJST,MKST

CD=== 概要 ===========================================================

CDT   VF_ASTOCI.h:CADMAS-STOC連成関連:整数

C==== 内容 ===========================================================

CD    -- STOCとの通信に関する変数 --
CD    IB_STOC          : CNS : I*4 : comm_ic_mgにおけるSTOC-ICのランク
CD    NB_CADMAS        : CNS : I*4 : STOC-ICと接続しているCADMASのPE数
CD    LB_CADMAS        : CNS : I*4 : STOCと接続しているCADMASのPEの順番付け
CD                                   値は1～NB_CADMAS(STOCのPE及びSTOCと接続しないCADMASのPEでは0)
CD                                   IB_CADMASやIB_SC参照時に使用
CD    IB_CADMAS        : CNS : I*4 : STOCと接続しているCADMASのPEの、MPI_COMM_WORLDにおけるランク
CD       (NB_CADMAS)
CD    NB_SC            : CNS : I*4 : STOC-CADMAS接続に参加しているか
CD                                      (=0:参加していない、>0:参加している）

CD    -- STOCとCADMASのメッシュの対応関係に関する変数 --
CD    NIST             : CNS : I*4 : 自領域をSTOC側のメッシュで分割した場合のX方向分割数
CD    NJST             : CNS : I*4 : 自領域をSTOC側のメッシュで分割した場合のY方向分割数
CD    NKST             : CNS : I*4 : 自領域をSTOC側のメッシュで分割した場合のZ方向分割数
CD    MIST(NIST+1)     : CNS : I*4 : STOCのI番目の格子線位置が、CADMASの格子点インデックスのいくつに相当するか(X方向)
CD    MJST(NJST+1)     : CNS : I*4 : STOCのJ番目の格子線位置が、CADMASの格子点インデックスのいくつに相当するか(Y方向)
CD    MKST(NKST+1)     : CNS : I*4 : STOCのK番目の格子線位置が、CADMASの格子点インデックスのいくつに相当するか(Z方向)
CD    IWST             : CNS : I*4 : 西側境界でSTOCと接続するか否か(=1：接続する、=0：接続しない)
CD    IEST             : CNS : I*4 : 東側境界でSTOCと接続するか否か(=1：接続する、=0：接続しない)
CD    JSST             : CNS : I*4 : 南側境界でSTOCと接続するか否か(=1：接続する、=0：接続しない)
CD    JNST             : CNS : I*4 : 北側境界でSTOCと接続するか否か(=1：接続する、=0：接続しない)
