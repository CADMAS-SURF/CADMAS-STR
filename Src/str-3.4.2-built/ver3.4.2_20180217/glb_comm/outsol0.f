      SUBROUTINE OUTSOL0(NNOD,IFL)

      USE M_VAL

      DIMENSION IFL(*)

      REAL(8),POINTER :: SIGP(:,:),EPSP(:,:),PRNSIGP(:,:)

      ALLOCATE( SIGP(6,NNOD) )
      ALLOCATE( EPSP(6,NNOD) )
      ALLOCATE( PRNSIGP(3,NNOD) )

      CALL GATHER_NODAL_D(SIGP,6)
      CALL GATHER_NODAL_D(EPSP,6)
      CALL GATHER_NODAL_D(PRNSIGP,3)

      ICW = 0

      IPS = 1
      IPE = NP_ENS(2)

      CALL WTSOL(SIGP,EPSP,PRNSIGP,NNOD,IPS,IPE,NG_ENS,IG_ENS,IFL,ICW)

      IPS = NP_ENS(2) + NP_ENS(3) + NP_ENS(4) + 1
      IPE = NP_ENS(2) + NP_ENS(3) + NP_ENS(4) + NP_ENS(6)

      CALL WTSOL(SIGP,EPSP,PRNSIGP,NNOD,IPS,IPE,NG_ENS,IG_ENS,IFL,ICW)

      DEALLOCATE( SIGP )
      DEALLOCATE( EPSP )
      DEALLOCATE( PRNSIGP )

      END