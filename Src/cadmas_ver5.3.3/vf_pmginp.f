      SUBROUTINE VF_PMGINP()

CD=== 概要 ===========================================================

CDT   VF_PMGINP:マルチグリッド環境ファイルを読み込む

C==== 宣言 ===========================================================

      use mod_comm

C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_AFILEI.h'
      INCLUDE 'VF_APARAI.h'
C     INCLUDE 'VF_ASTOCI.h'

C==== 実行 ===========================================================

C----------------------------------------------------for MG/2FC coupling
      MGNAME(:) = ' '
C----------------------------------------------------for MG/2FC coupling
CD    -- マルチグリッド環境ファイルの読み込み --
      IENFIL=0
      IF (MGRANK.EQ.0) THEN
        WRITE(*,9510)
        OPEN(MFILEN,FILE='data.env',
     &         STATUS='OLD',FORM='FORMATTED',IOSTAT=IERR )
C----------------------------------------------------for MG/2FC coupling
        IF(IERR/=0) THEN
          MGARAN = 1
          MGARAN_2FC = 1
          MGNAME(1) = 'data'
          MGNLEN(1) = len(trim(MGNAME(1)))
          MGNPIN(1) = MGPROC
          MGPARE(1) = 0
          N = MGPROC
          GOTO 6000
        END IF
C----------------------------------------------------for MG/2FC coupling
        IENFIL=MFILEN
        READ(IENFIL,*,END=9020,ERR=9020) MGARAN
        IF (MGARAN.GT.MAXPRO) GOTO 9030
        IF (MGARAN.LE.0     ) GOTO 9040
        N=0
        DO 120 I=1,MGARAN
          READ(IENFIL,*,END=9020,ERR=9020) MGNAME(I),MGNPIN(I),MGPARE(I)
C----------------------------------------------------for MG/2FC coupling
          MGNLEN(I)=0
          DO 100 L=MAXCHR,1,-1
            IF (MGNAME(I)(L:L).NE.' ') THEN
              MGNLEN(I)=L
              GOTO 110
            ENDIF
 100      CONTINUE
 110      CONTINUE
          IF (MGNLEN(I).LE.0     ) GOTO 9040
          IF (MGNPIN(I).LE.0     ) GOTO 9040
CCC IC-MG COUPLING    IF (MGPARE(I).LT.0    ) GOTO 9040
          IF (MGPARE(I).GT.MGARAN) GOTO 9040
          IF (MGPARE(I).EQ.I     ) GOTO 9040
          N=N+MGNPIN(I)
 120    CONTINUE
        CLOSE(IENFIL)
        IENFIL=0
        IF (N.GT.MAXPRO) GOTO 9030
        IF (N.NE.MGPROC) GOTO 9050
      ENDIF

CD    -- 環境データのパッシング --
      CALL VF_P0BCSI(MGARAN,     1,0)
      CALL VF_P0BCSI(MGNLEN,MGARAN,0)
      CALL VF_P0BCSI(MGNPIN,MGARAN,0)
      CALL VF_P0BCSI(MGPARE,MGARAN,0)
      DO 200 I=1,MGARAN
        CALL VF_P0BCSC(MGNAME(I),MGNLEN(I),0)
 200  CONTINUE

CD    -- 親子関係のループチェック --
      DO 320 I=1,MGARAN
        N=MGPARE(I)
        IF (N.LT.0) NB_SC = -N
CDEBUG        IF (N.GT.0) THEN
CDEBUG          DO 300 L=1,MGARAN-1
CDEBUG            N=MGPARE(N)
CDEBUG            IF (N.EQ.0) GOTO 310
CDEBUG 300      CONTINUE
CDEBUG 310      CONTINUE
CDEBUG          IF (N.NE.0) GOTO 9040
CDEBUG        ENDIF
 320  CONTINUE

CD    -- 領域名の重複チェック --
      DO 410 I=1,MGARAN-1
        DO 400 L=I+1,MGARAN
          IF (MGNAME(I).EQ.MGNAME(L)) GOTO 9040
 400    CONTINUE
 410  CONTINUE

C----------------------------------------------------for MG/2FC coupling
 6000 CONTINUE
C----------------------------------------------------for MG/2FC coupling
CD    -- 領域毎のデータをプロセス毎に展開する --
      N=MGPROC
      NPROCS=0
      MYRANK=-1
      DO 510 I=MGARAN,1,-1
        DO 500 L=1,MGNPIN(I)
          MGAREA(N)=I
          MGNAME(N)=MGNAME(I)
          MGNLEN(N)=MGNLEN(I)
          MGNPIN(N)=MGNPIN(I)
          MGPARE(N)=MGPARE(I)
          IF (N.EQ.MGRANK+1) THEN
            NPROCS=MGNPIN(N)
            MYRANK=NPROCS-L
          ENDIF
          N=N-1
 500    CONTINUE
 510  CONTINUE
      IF (NPROCS.LE.0) GOTO 9060
      IF (MYRANK.LT.0) GOTO 9060

CD    -- 領域毎のコミュニケータ作成 --
      MGCOMM=0
      CALL VF_ZXMG_SPLIT(MGAREA(MGRANK+1),MYRANK,MGCOMM,IERR)
      CALL VF_ZXMP_CSIZE(NP,IERR)
      CALL VF_ZXMP_CRANK(MY,IERR)
      IF (NPROCS.NE.NP) GOTO 9060
      IF (MYRANK.NE.MY) GOTO 9060

C     -- 実行文の終了 --
      GOTO 9999

C==== ファイル関連エラー処理 =========================================

 9010 CONTINUE
      WRITE(*,9520) 'VF_PMGINP','CAN NOT OPEN (data.env).'
      WRITE(*,9530)
      CALL VF_A2CLOS()
      CALL VF_P0ENDA()
      STOP
      GOTO 9999

 9020 CONTINUE
      WRITE(*,9520) 'VF_PMGINP','I/O ERROR.'
      WRITE(*,9530)
      CALL VF_A2CLOS()
      CALL VF_P0ENDA()
      STOP
      GOTO 9999

 9030 CONTINUE
      WRITE(*,9520) 'VF_PMGINP','AREA IS FULL.'
      WRITE(*,9530)
      CALL VF_A2CLOS()
      CALL VF_P0ENDA()
      STOP
      GOTO 9999

 9040 CONTINUE
      WRITE(*,9520) 'VF_PMGINP','INVALID VALUE.'
      WRITE(*,9530)
      CALL VF_A2CLOS()
      CALL VF_P0ENDA()
      STOP
      GOTO 9999

 9050 CONTINUE
      WRITE(*,9520) 'VF_PMGINP','MGPROC <> SUM(PE).'
      WRITE(*,9530)
      CALL VF_A2CLOS()
      CALL VF_P0ENDA()
      STOP
      GOTO 9999

 9060 CONTINUE
      WRITE(*,9520) 'VF_PMGINP','P.G ERROR.'
      WRITE(*,9530)
      CALL VF_A2CLOS()
      CALL VF_P0ENDA()
      STOP
      GOTO 9999

C==== フォーマット文 =================================================

 9510 FORMAT(/' ','>> FILE-ENV : IN : ALL')
 9520 FORMAT(/' ','>>>>> ERROR. [',A,'] : ',A)
 9530 FORMAT(/' ','##### ABNORMAL END. #########################'/)

C==== 終了 ===========================================================

 9999 CONTINUE
      RETURN
      END
