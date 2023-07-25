MODULE VF_A2ARRAY

      DOUBLE PRECISION, DIMENSION(:,:  ), ALLOCATABLE :: XX ,YY ,ZZ
      DOUBLE PRECISION, DIMENSION(:,:,:), ALLOCATABLE :: UU ,VV ,WW
      DOUBLE PRECISION, DIMENSION(:,:,:), ALLOCATABLE :: PP ,FF ,FX

      DOUBLE PRECISION, DIMENSION(:,:,:), ALLOCATABLE :: PPBK

      DOUBLE PRECISION, DIMENSION(:,:,:), ALLOCATABLE :: FY ,FZ ,ANU
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      DOUBLE PRECISION, DIMENSION(:,:,:), ALLOCATABLE :: RHOG
      DOUBLE PRECISION, DIMENSION(:,:,:), ALLOCATABLE :: DRHODP
      DOUBLE PRECISION, DIMENSION(:,:,:), ALLOCATABLE :: DRHODT
      DOUBLE PRECISION, DIMENSION(:,:,:), ALLOCATABLE :: RHOGO
      DOUBLE PRECISION, DIMENSION(:,:,:), ALLOCATABLE :: UUO
      DOUBLE PRECISION, DIMENSION(:,:,:), ALLOCATABLE :: VVO
      DOUBLE PRECISION, DIMENSION(:,:,:), ALLOCATABLE :: WWO
      DOUBLE PRECISION, DIMENSION(:,:,:), ALLOCATABLE :: PPO
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      DOUBLE PRECISION, DIMENSION(:,:,:), ALLOCATABLE :: CM0,CD0
      DOUBLE PRECISION, DIMENSION(:,:,:), ALLOCATABLE :: GGV,GGX,GGY,GGZ
      DOUBLE PRECISION, DIMENSION(:,:,:), ALLOCATABLE :: GLV,GLX,GLY,GLZ
      DOUBLE PRECISION, DIMENSION(:,:  ), ALLOCATABLE :: BCU,BCV,BCW,BCP
      DOUBLE PRECISION, DIMENSION(:    ), ALLOCATABLE :: BCF,BCVI
      DOUBLE PRECISION, DIMENSION(:,:  ), ALLOCATABLE :: BCU0,BCV0,BCW0
      DOUBLE PRECISION, DIMENSION(:,:  ), ALLOCATABLE :: BCP0
      DOUBLE PRECISION, DIMENSION(:    ), ALLOCATABLE :: BCF0,BCVI0
      DOUBLE PRECISION, DIMENSION(:    ), ALLOCATABLE :: TBUB
      DOUBLE PRECISION, DIMENSION(:,:,:), ALLOCATABLE :: DROPTX,DROPTY
      DOUBLE PRECISION, DIMENSION(:,:,:), ALLOCATABLE :: DROPTZ,DROPUU
      DOUBLE PRECISION, DIMENSION(:,:,:), ALLOCATABLE :: DROPVV,DROPWW
      DOUBLE PRECISION, DIMENSION(:    ), ALLOCATABLE :: GGVOLD,GGVNOW
      DOUBLE PRECISION, DIMENSION(:,:  ), ALLOCATABLE :: GGVEL
      DOUBLE PRECISION, DIMENSION(:,:  ), ALLOCATABLE :: GGVELO,GGVELN
      DOUBLE PRECISION, DIMENSION(:,:  ), ALLOCATABLE :: GGVLO,GGVLN
      DOUBLE PRECISION, DIMENSION(:,:,:), ALLOCATABLE :: GGV0,GLV0,GVL0
      DOUBLE PRECISION, DIMENSION(:,:,:), ALLOCATABLE :: ANUT,AK,AE
      DOUBLE PRECISION, DIMENSION(:,:  ), ALLOCATABLE :: BCK,BCE
      DOUBLE PRECISION, DIMENSION(:,:  ), ALLOCATABLE :: BCK0,BCE0
      DOUBLE PRECISION, DIMENSION(:,:,:), ALLOCATABLE :: TT,ALM
      DOUBLE PRECISION, DIMENSION(:    ), ALLOCATABLE :: BCT
      DOUBLE PRECISION, DIMENSION(:,:  ), ALLOCATABLE :: BCTI
      DOUBLE PRECISION, DIMENSION(:,:,:,:), ALLOCATABLE :: CC,DD
      DOUBLE PRECISION, DIMENSION(:    ,:), ALLOCATABLE :: BCC
      DOUBLE PRECISION, DIMENSION(:,:  ,:), ALLOCATABLE :: BCCI
      DOUBLE PRECISION, DIMENSION(:    ), ALLOCATABLE :: DMTBTT
      DOUBLE PRECISION, DIMENSION(:    ), ALLOCATABLE :: DMTBZZ
      DOUBLE PRECISION, DIMENSION(:    ), ALLOCATABLE :: DMTBHH
      DOUBLE PRECISION, DIMENSION(:,:  ), ALLOCATABLE :: DMTBUN
      DOUBLE PRECISION, DIMENSION(:,:  ), ALLOCATABLE :: DMTBUT
      DOUBLE PRECISION, DIMENSION(:    ), ALLOCATABLE :: DBUF
      DOUBLE PRECISION, DIMENSION(:    ), ALLOCATABLE :: RBUF
      DOUBLE PRECISION, DIMENSION(:,:,:), ALLOCATABLE :: PPPVC
      DOUBLE PRECISION, DIMENSION(:,:,:), ALLOCATABLE :: WK01,WK02,WK03
      DOUBLE PRECISION, DIMENSION(:,:,:), ALLOCATABLE :: WK04,WK05,WK06
      DOUBLE PRECISION, DIMENSION(:,:,:), ALLOCATABLE :: WK07,WK08,WK09
      DOUBLE PRECISION, DIMENSION(:,:,:), ALLOCATABLE :: WK10,WK11,WK12
      DOUBLE PRECISION, DIMENSION(:,:,:), ALLOCATABLE :: WK13,WK14,WK15
      DOUBLE PRECISION, DIMENSION(:,:,:), ALLOCATABLE :: WK16,WK17,WK18
      DOUBLE PRECISION, DIMENSION(:    ), ALLOCATABLE :: WKBC
      INTEGER         , DIMENSION(:,:,:), ALLOCATABLE :: NF  ,INDX
      INTEGER         , DIMENSION(:,:,:), ALLOCATABLE :: INDY,INDZ,INDC
      INTEGER         , DIMENSION(:,:  ), ALLOCATABLE :: INDB
      INTEGER         , DIMENSION(:    ), ALLOCATABLE :: INDS
      INTEGER         , DIMENSION(:,:  ), ALLOCATABLE :: INDBK,INDBE
      INTEGER         , DIMENSION(:    ), ALLOCATABLE :: INDBT
      INTEGER         , DIMENSION(:,:  ), ALLOCATABLE :: INDBC
      INTEGER         , DIMENSION(:,:,:), ALLOCATABLE :: INDX0,INDY0
      INTEGER         , DIMENSION(:,:,:), ALLOCATABLE :: INDZ0
      INTEGER         , DIMENSION(:,:  ), ALLOCATABLE :: INDB0
      INTEGER         , DIMENSION(:,:,:), ALLOCATABLE :: IPVC
      INTEGER         , DIMENSION(:    ), ALLOCATABLE :: IBUF
      INTEGER         , DIMENSION(:,:,:), ALLOCATABLE :: NWK1
      INTEGER         , DIMENSION(:    ), ALLOCATABLE :: NWKBC
!----------------------------------------------------for MG/2FC coupling
      DOUBLE PRECISION, DIMENSION(:    ), ALLOCATABLE :: XPF, YPF, ZPF
      INTEGER         , DIMENSION(:    ), ALLOCATABLE :: IPF, JPF, KPF
!----------------------------------------------------for MG/2FC coupling
!----------------------------------------------------for 2FC/STR coupling
      DOUBLE PRECISION, DIMENSION(:,:,:), ALLOCATABLE :: DGGV,DGLV
      DOUBLE PRECISION, DIMENSION(:,:,:), ALLOCATABLE :: GGV_0,GGX_0,GGY_0,GGZ_0
      DOUBLE PRECISION, DIMENSION(:    ), ALLOCATABLE :: BCT0
      DOUBLE PRECISION, DIMENSION(:,:  ), ALLOCATABLE :: BCTI0
      DOUBLE PRECISION, DIMENSION(:,:  ), ALLOCATABLE :: BCC0
      DOUBLE PRECISION, DIMENSION(:,:,:), ALLOCATABLE :: BCCI0
      INTEGER         , DIMENSION(:,:  ), ALLOCATABLE :: INDBK0,INDBE0
      INTEGER         , DIMENSION(:    ), ALLOCATABLE :: INDBT0
      INTEGER         , DIMENSION(:,:  ), ALLOCATABLE :: INDBC0
!----------------------------------------------------for 2FC/STR coupling
!----------------------------------------------------for 2FC/STM coupling
      DOUBLE PRECISION, DIMENSION(:,:,:), ALLOCATABLE :: GGW,GGR,FLFU,FLFV
      INTEGER         , DIMENSION(:,:  ), ALLOCATABLE :: LNDC,LNDC0,LNDC_0,KST
      DOUBLE PRECISION, DIMENSION(:,:  ), ALLOCATABLE :: DZ,SUMZ
      DOUBLE PRECISION, DIMENSION(:,:,:), ALLOCATABLE :: WK19,WK20
!----------------------------------------------------for 2FC/STM coupling

END MODULE VF_A2ARRAY
