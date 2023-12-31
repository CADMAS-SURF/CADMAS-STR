      SUBROUTINE SEND_TBL(KK,IP)

      USE M_VAL
      USE M_PART

      IMPLICIT REAL*8(A-H,O-Z)

      DIMENSION KK(*)

      INTEGER, POINTER :: LNOD(:),LELM(:),INDGW(:),IELMW(:,:)
      INTEGER, POINTER :: ISPDW(:,:),NSPDW(:,:),IFCW(:,:),NFCW(:,:)
      INTEGER, POINTER :: IPL4W(:,:),NPL4W(:,:),ISP1W(:,:),NSP1W(:,:)
      INTEGER, POINTER :: ISPCW(:,:),NSPCW(:,:),IBELW(:)
      INTEGER, POINTER :: IBTEW(:,:),ICBDW(:),IELCW(:,:),IELCBW(:)
      INTEGER, POINTER :: IEDAW(:),IEDGW(:,:),IEDGBW(:),LELC(:)
      INTEGER, POINTER :: IELQW(:,:),LELQ(:),IFCQW(:),IEDQW(:),IVRQW(:)
      INTEGER, POINTER :: IELGW(:),INDAW(:),INDCW(:)
      REAL(8), POINTER :: GRIDW(:,:),SPCDW(:,:),FCW(:,:)
      REAL(8), POINTER :: PLD4W(:,:),SPCW(:,:)

!     --- LNOD ---

      NNOD = KK(8)
      NIGSF = KK(94)

      ALLOCATE( LNOD(NNOD+NIGSF) )

      LNOD(:) = 0

      DO I = 1, NN_EXT(IP)
        LNOD( NOD(I,IP) ) = I
      ENDDO

      IF( KK(92) > 0 ) THEN

        DO I = 1, NN_EXTC(IP)
          LNOD( NODC(I,IP) ) = NN_EXT(IP) + I
        ENDDO

        DO I = 1, NG(IP)
          LNOD( NODG(I,IP) ) = NN_EXT(IP) + NN_EXTC(IP) + I
        ENDDO

        DO I = 1, NGC(IP)
          LNOD( NODGC(I,IP) ) = NN_EXT(IP) + NN_EXTC(IP) + NG(IP) + I
        ENDDO

      ENDIF

!     --- LELM ---

      NELM = KK(12)

      ALLOCATE( LELM(NELM) )

      LELM(:) = 0

      DO I = 1, NE(IP)
        LELM( IEL(I,IP) ) = I
      ENDDO

!     --- GRID ---

      NNODIW = NN_INT(IP)
      NNODW  = NN_EXT(IP)
      IF( KK(92) > 0 ) THEN
        NNODCW = NN_EXTC(IP)
        NIGSFW = NG(IP)
        NIGSFCW = NGC(IP)
      ELSE
        NNODCW = 0
        NIGSFW = 0
        NIGSFCW = 0
      ENDIF

      ALLOCATE( INDGW(NNODW+NNODCW) )
      ALLOCATE( GRIDW(3,NNODW+NNODCW) )

      INDGW(1:NNODW) = INDG(NOD(1:NNODW,IP))
      GRIDW(:,1:NNODW) = GRID(:,NOD(1:NNODW,IP))

      INDGW(NNODW+1:NNODW+NNODCW) = INDG(NODC(1:NNODCW,IP))
      GRIDW(:,NNODW+1:NNODW+NNODCW) = GRID(:,NODC(1:NNODCW,IP))

      CALL M_MPI_SEND_I(NNODIW,1,IP)
      CALL M_MPI_SEND_I(NNODW,1,IP)
      CALL M_MPI_SEND_I(NNODCW,1,IP)
      CALL M_MPI_SEND_I(NIGSFW,1,IP)
      CALL M_MPI_SEND_I(NIGSFCW,1,IP)

      CALL M_MPI_SEND_I(INDGW,NNODW+NNODCW,IP)
      CALL M_MPI_SEND_D(GRIDW,3*(NNODW+NNODCW),IP)

      DEALLOCATE( INDGW )
      DEALLOCATE( GRIDW )

!     --- ELEMENT ---

      NELMW = NE(IP)
      IF( KK(92) > 0 ) THEN
        NELMCW = NEC(IP)
      ELSE
        NELMCW = 0
      ENDIF
      NM = KK(37)

      ALLOCATE( IELMW(NM,NELMW+NELMCW) )

      IELMW(1:7,1:NELMW) = IELM(1:7,IEL(1:NELMW,IP))
      DO I = 1, NELMW
        N = IELMW(3,I)
        IELMW(8:7+N,I) = LNOD( IELM(8:7+N,IEL(I,IP)) )
      ENDDO

      IELMW(1:7,NELMW+1:NELMW+NELMCW) = IELM(1:7,IELMC(1:NELMCW,IP))
      DO I = 1, NELMCW
        IE = NELMW + I
        N = IELMW(3,IE)
        IELMW(8:7+N,IE) = LNOD( IELM(8:7+N,IELMC(I,IP)) )
      ENDDO

      CALL M_MPI_SEND_I(NELMW,1,IP)
      CALL M_MPI_SEND_I(NELMCW,1,IP)
      CALL M_MPI_SEND_I(IELMW,NM*(NELMW+NELMCW),IP)

      DEALLOCATE( IELMW )

!     --- SPCD ---

      IF( KK(38) > 0 ) THEN

        NISPD = KK(38)
        NNSPD = KK(39)

        ALLOCATE( ISPDW(2,NISPD) )
        ALLOCATE( NSPDW(7,NNSPD) )
        ALLOCATE( SPCDW(6,NNSPD) )

        CALL EX_SPC(NISPDW,NNSPDW,ISPDW,NSPDW,SPCDW,NISPD,ISPD,NSPD,SPCD
     &             ,LNOD,NNODW,7)

        CALL M_MPI_SEND_I(NISPDW,1,IP)
        CALL M_MPI_SEND_I(NNSPDW,1,IP)

        IF( NISPDW > 0 ) THEN
          CALL M_MPI_SEND_I(ISPDW,2*NISPDW,IP)
          CALL M_MPI_SEND_I(NSPDW,7*NNSPDW,IP)
          CALL M_MPI_SEND_D(SPCDW,6*NNSPDW,IP)
        ENDIF

        DEALLOCATE( ISPDW )
        DEALLOCATE( NSPDW )
        DEALLOCATE( SPCDW )

      ENDIF

!     --- FORCE ---

      IF( KK(42) > 0 ) THEN

        NIFC = KK(42)
        NNFC = KK(43)

        ALLOCATE( IFCW(2,NIFC) )
        ALLOCATE( NFCW(2,NNFC) )
        ALLOCATE( FCW(6,NNFC) )

        CALL EX_SPC(NIFCW,NNFCW,IFCW,NFCW,FCW,NIFC,IFC,NFC,FC
     &             ,LNOD,NNODW,2)

        CALL M_MPI_SEND_I(NIFCW,1,IP)
        CALL M_MPI_SEND_I(NNFCW,1,IP)

        IF( NIFCW > 0 ) THEN
          CALL M_MPI_SEND_I(IFCW,2*NIFCW,IP)
          CALL M_MPI_SEND_I(NFCW,2*NNFCW,IP)
          CALL M_MPI_SEND_D(FCW,6*NNFCW,IP)
        ENDIF

        DEALLOCATE( IFCW )
        DEALLOCATE( NFCW )
        DEALLOCATE( FCW )

      ENDIF

!     --- PLOAD4 ---

      IF( KK(44) > 0 ) THEN

        NIPL4 = KK(44)
        NNPL4 = KK(45)

        ALLOCATE( IPL4W(2,NIPL4) )
        ALLOCATE( NPL4W(4,NNPL4) )
        ALLOCATE( PLD4W(4,NNPL4) )

        CALL EX_PLOAD4(NIPL4W,NNPL4W,IPL4W,NPL4W,PLD4W,NIPL4,IPL4
     &                ,NPL4,PLD4,LELM,LNOD)

        CALL M_MPI_SEND_I(NIPL4W,1,IP)
        CALL M_MPI_SEND_I(NNPL4W,1,IP)

        IF( NIPL4W > 0 ) THEN
          CALL M_MPI_SEND_I(IPL4W,2*NIPL4W,IP)
          CALL M_MPI_SEND_I(NPL4W,4*NNPL4W,IP)
          CALL M_MPI_SEND_D(PLD4W,4*NNPL4W,IP)
        ENDIF

        DEALLOCATE( IPL4W )
        DEALLOCATE( NPL4W )
        DEALLOCATE( PLD4W )

      ENDIF

!     --- SPC1 ---

      IF( KK(48) > 0 ) THEN

        NISP1 = KK(48)
        NNSP1 = KK(49)

        ALLOCATE( ISP1W(2,NISP1) )
        ALLOCATE( NSP1W(7,NNSP1) )

        CALL EX_SPC1(NISP1W,NNSP1W,ISP1W,NSP1W,NISP1,ISP1,NSP1,LNOD
     &              ,NNODW)

        CALL M_MPI_SEND_I(NISP1W,1,IP)
        CALL M_MPI_SEND_I(NNSP1W,1,IP)

        IF( NISP1W > 0 ) THEN
          CALL M_MPI_SEND_I(ISP1W,2*NISP1W,IP)
          CALL M_MPI_SEND_I(NSP1W,7*NNSP1W,IP)
        ENDIF

        DEALLOCATE( ISP1W )
        DEALLOCATE( NSP1W )

      ENDIF

!     --- SPC ---

      IF( KK(56) > 0 ) THEN

        NISPC = KK(56)
        NNSPC = KK(57)

        ALLOCATE( ISPCW(2,NISPC) )
        ALLOCATE( NSPCW(7,NNSPC) )
        ALLOCATE( SPCW(6,NNSPC) )

        CALL EX_SPC(NISPCW,NNSPCW,ISPCW,NSPCW,SPCW,NISPC,ISPC,NSPC,SPC
     &             ,LNOD,NNODW,7)

        CALL M_MPI_SEND_I(NISPCW,1,IP)
        CALL M_MPI_SEND_I(NNSPCW,1,IP)

        IF( NISPCW > 0 ) THEN
          CALL M_MPI_SEND_I(ISPCW,2*NISPCW,IP)
          CALL M_MPI_SEND_I(NSPCW,7*NNSPCW,IP)
          CALL M_MPI_SEND_D(SPCW,6*NNSPCW,IP)
        ENDIF

        DEALLOCATE( ISPCW )
        DEALLOCATE( NSPCW )
        DEALLOCATE( SPCW )

      ENDIF

!     --- IMPORT & EXPORT NODE ---

      NODIMPW(1:NIMPW(IP),IP) = LNOD( NODIMPW(1:NIMPW(IP),IP) )
      NODEXPW(1:NEXPW(IP),IP) = LNOD( NODEXPW(1:NEXPW(IP),IP) )

      CALL M_MPI_SEND_I(NPEW(IP),1,IP)
      CALL M_MPI_SEND_I(NIMPW(IP),1,IP)
      CALL M_MPI_SEND_I(NEXPW(IP),1,IP)

      CALL M_MPI_SEND_I(IPEW(1,IP),NPEW(IP),IP)
      CALL M_MPI_SEND_I(IDXIMPW(1,1,IP),2*NPEW(IP),IP)
      CALL M_MPI_SEND_I(NODIMPW(1,IP),NIMPW(IP),IP)
      CALL M_MPI_SEND_I(IDXEXPW(1,1,IP),2*NPEW(IP),IP)
      CALL M_MPI_SEND_I(NODEXPW(1,IP),NEXPW(IP),IP)

      IF( KK(92) == 0 ) GOTO 10

!     --- IBEL ---

      ALLOCATE( IBELW(NELMW+NELMCW) )

      IBELW(1:NELMW) = IBEL( IEL(1:NELMW,IP) )
      IBELW(NELMW+1:NELMW+NELMCW) = IBEL( IELMC(1:NELMCW,IP) )

      CALL M_MPI_SEND_I(IBELW,NELMW+NELMCW,IP)

      DEALLOCATE( IBELW )

!     --- IBTE ---

      NIBTEW = NIBT(IP)

      ALLOCATE( IBTEW(4,NIBTEW) )

      DO I = 1, NIBTEW
        IBTEW(:,I) = LNOD( IBTE(:,IBT(I,IP)) )
      ENDDO

      CALL M_MPI_SEND_I(NIBTEW,1,IP)
      IF( NIBTEW > 0 ) CALL M_MPI_SEND_I(IBTEW,4*NIBTEW,IP)

      DEALLOCATE( IBTEW )

!     --- IELC ---

      NICRG = KK(92)
      NIELCW = NIEC(IP)

      ALLOCATE( ICBDW(NICRG) )
      ALLOCATE( IELCW(3,NIELCW) )

      CALL EX_ADD(ICBDW,ICBD,NICRG,IEC(1,IP),NIEC(IP))

      DO I = 1, NIELCW
        IELCW(:,I) = LNOD( IELC(:,IEC(I,IP)) )
      ENDDO

      CALL M_MPI_SEND_I(ICBDW,NICRG,IP)
      CALL M_MPI_SEND_I(NIELCW,1,IP)
      IF( NIELCW > 0 ) CALL M_MPI_SEND_I(IELCW,3*NIELCW,IP)

      DEALLOCATE( ICBDW )
      DEALLOCATE( IELCW )

!     --- IELCB ---

      ALLOCATE( IELCBW(NIELCW) )

      IELCBW(:) = 0

      DO I = 1, NIELCW
        IF( IELCP(1,IEC(I,IP)) == IP ) IELCBW(I) = 1
      ENDDO

      IF( NIELCW > 0 ) CALL M_MPI_SEND_I(IELCBW,NIELCW,IP)

      DEALLOCATE( IELCBW )

!     --- INDA0 ---

      CALL M_MPI_SEND_I(INDA0,NICRG,IP)

!     --- INDC ---

      NINDCW = NINC(IP)

      ALLOCATE( INDAW(NICRG) )
      ALLOCATE( INDCW(NINDCW) )

      CALL EX_ADD(INDAW,INDA0,NICRG,INC(1,IP),NINC(IP))

      INDCW(:) = LNOD( INDC0( INC(1:NINDCW,IP) ) )

      CALL M_MPI_SEND_I(INDAW,NICRG,IP)
      CALL M_MPI_SEND_I(NINDCW,1,IP)
      IF( NINDCW > 0 ) CALL M_MPI_SEND_I(INDCW,NINDCW,IP)

      DEALLOCATE( INDAW )
      DEALLOCATE( INDCW )

!     --- IEDG ---

      NIELC = KK(97)

      ALLOCATE( LELC(0:NIELC) )

      LELC(:) = 0

      DO I = 1, NIEC(IP)
        LELC( IEC(I,IP) ) = I
      ENDDO

      NIEDGW = NIEG(IP)

      ALLOCATE( IEDAW(NICRG) )
      ALLOCATE( IEDGW(6,NIEDGW) )

      CALL EX_ADD(IEDAW,IEDA,NICRG,IEG(1,IP),NIEG(IP))

      DO I = 1, NIEDGW
        IEDGW(1:2,I) = LNOD( IEDG(1:2,IEG(I,IP)) )
        IEDGW(3,I) = LELC( IEDG(3,IEG(I,IP)) )
        IEDGW(4,I) = IEDG(4,IEG(I,IP))
        IEDGW(5,I) = LELC( IEDG(5,IEG(I,IP)) )
        IEDGW(6,I) = IEDG(6,IEG(I,IP))
      ENDDO

      CALL M_MPI_SEND_I(IEDAW,NICRG,IP)
      CALL M_MPI_SEND_I(NIEDGW,1,IP)
      IF( NIEDGW > 0 ) CALL M_MPI_SEND_I(IEDGW,6*NIEDGW,IP)

      DEALLOCATE( IEDAW )
      DEALLOCATE( IEDGW )

!     --- IEDGB ---

      ALLOCATE( IEDGBW(NIEDGW) )

      IEDGBW(:) = 0

      DO I = 1, NIEDGW
        IF( IEDGP(1,IEG(I,IP)) == IP ) IEDGBW(I) = 1
      ENDDO

      IF( NIEDGW > 0 ) CALL M_MPI_SEND_I(IEDGBW,NIEDGW,IP)

      DEALLOCATE( IEDGBW )

!     --- IELG ---

      NIELG = KK(98)

      ALLOCATE( IELGW(NIELG) )

      IC = 0

      DO I = 1, NIELG
        LELG = LELC( IELG(I) )
        IF( LELG > 0 ) THEN
          IC = IC + 1
          IELGW(IC) = LELG
        ENDIF
      ENDDO

      NIELGW = IC

      CALL M_MPI_SEND_I(NIELGW,1,IP)
      IF( NIELGW > 0 ) CALL M_MPI_SEND_I(IELGW,NIELGW,IP)

      DEALLOCATE( LELC )

      DEALLOCATE( IELGW )

!     --- IELQ ---

      NIELQW = NIEQ(IP)

      ALLOCATE( IELQW(4,NIELQW) )

      DO I = 1, NIELQW
        IELQW(:,I) = LNOD( IELQ(:,IEQ(I,IP)) )
      ENDDO

      CALL M_MPI_SEND_I(NIELQW,1,IP)
      IF( NIELQW > 0 ) CALL M_MPI_SEND_I(IELQW,4*NIELQW,IP)

      DEALLOCATE( IELQW )

!     --- IFCQ, IEDQ, IVRQ ---

      NIELQ = KK(101)

      ALLOCATE( LELQ(0:NIELQ) )

      LELQ(:) = 0

      DO I = 1, NIEQ(IP)
        LELQ( IEQ(I,IP) ) = I
      ENDDO

      ALLOCATE( IFCQW(NIELCW) )
      ALLOCATE( IEDQW(NIEDGW) )
      ALLOCATE( IVRQW(NIGSFW) )

      IFCQW(:) = LELQ( IFCQ( IEC(1:NIELCW,IP) ) )
      IEDQW(:) = LELQ( IEDQ( IEG(1:NIEDGW,IP) ) )
      IVRQW(:) = LELQ( IVRQ( NODG(1:NIGSFW,IP) ) )

      IF( NIELCW > 0 ) CALL M_MPI_SEND_I(IFCQW,NIELCW,IP)
      IF( NIEDGW > 0 ) CALL M_MPI_SEND_I(IEDQW,NIEDGW,IP)
      IF( NIGSFW > 0 ) CALL M_MPI_SEND_I(IVRQW,NIGSFW,IP)

      DEALLOCATE( LELQ )
      DEALLOCATE( IFCQW )
      DEALLOCATE( IEDQW )
      DEALLOCATE( IVRQW )

!     --- DEALLOCATE LNOD, LELM ---

   10 DEALLOCATE( LNOD )
      DEALLOCATE( LELM )

      END
