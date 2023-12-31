      SUBROUTINE SF_STR1(DT)

      USE VF_A2ARRAY
      USE SF_ARRAY

      IMPLICIT REAL*8(A-H,O-Z)

      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_AFILEI.h'
      INCLUDE 'SF_STRUCT.h'

      IF( ICPL == 1 ) CALL SF_PRSINI(IPFACE0,AFC,IPGRID0,IPND0,ZZ)

      IENS=0
      IF( IENS == 1 ) CALL SF_ENS_GEOM(XX,YY,ZZ)

      IF( ISTM == 1 ) CALL SF_DPTH_INIT(XX,YY,ZZ,FF,GGW,GGR,NF)

      IF( IRETYP > 0 ) THEN
        IF( ICPL == 2 )
     &    CALL SF_SEND_SURF(IPND0,IPFACE0,AFC,IPGRID0,ZZ,IRFACE,IRGRID0)
        CALL SF_RD_RESTART(DT)
      ELSEIF( ICPL == 2 ) THEN
        CALL SF_POS_INIT(POS,POS2,DVEL,DVEL2,POS0,POS20,DVEL20,IPND0
     &                  ,PRES0,IPFACE0,AFC,IPGRID,IPGRID0,IRFACE,IRGRID0
     &                  ,IGNO,IENO,ZZ,NF,PP,DZ)
        CALL SF_PFACE1()
        CALL SF_PFACE3()
        CALL SF_STR_OBST(GGV,GGX,GGY,GGZ,GGW,GGR,GLV,GLX,GLY,GLZ,NF,INDC
     &                  ,INDX,INDY,INDZ,SPC,LNDC_0,LNDC0,LNDC,KST,GGV_0
     &                  ,GGX_0,GGY_0,GGZ_0,XX,YY,ZZ,CM0,SUMZ,IELM,POS
     &                  ,POR,1,WK01,WK02,WK03,WK04,WK05,WK06,WK07,WK08
     &                  ,WK09,WK10,WK11,WK12,WK13,DBUF,WK14,WK15,WK16
     &                  ,WK17,WK18,WK19,WK20)
        CALL SF_PFACE2()
        IF( ISTM == 1 ) CALL SF_RFACE2()
        CALL SF_ALLOC1()
        CALL SF_BC_RESET(INDB,INDBK,INDBE,INDBT,INDBC,BCU,BCV,BCW,BCP
     &                  ,BCF,BCVI,BCK,BCE,BCT,BCTI,BCC,BCCI,GGX,GGY
     &                  ,GGZ,GLX,GLY,GLZ,NF,INDX,INDY,INDZ,INDX0,INDY0
     &                  ,INDZ0,INDB0,INDBK0,INDBE0,INDBT0,INDBC0,BCU0
     &                  ,BCV0,BCW0,BCP0,BCF0,BCVI0,BCK0,BCE0,BCT0
     &                  ,BCTI0,BCC0,BCCI0,XX,YY,ZZ,IELM,POS,DVEL)
        CALL SF_CINIT(XX,YY,ZZ,UU,VV,WW,PP,FF,FX,FY,FZ,ANU,GGV,GGX,GGY
     &               ,GGZ,BCU,BCV,BCW,BCP,BCF,BCVI,DMTBTT,DMTBZZ,DMTBHH
     &               ,DMTBUN,DMTBUT,DBUF,WK01,WK02,WK03,NF,INDX,INDY
     &               ,INDZ,INDB,INDS,IBUF,WK14,WK15,WK16,WK17,WK18,WK19
     &               ,WK20)
      ENDIF

      END
