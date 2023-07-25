      SUBROUTINE LCCBM(RL,TE,U,VST,VTT,AS,AT,VS,VT,GRID,UG,VG,NP,V,IGNL)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION E(3,3),GRID(3,*),NP(2),TE(3,3),U(3,2),UG(6,*),VG(3,2,2)
     &         ,VS0(3),VT0(3),VST(3,2),VTT(3,2),VS(3,2),VT(3,2),V(3)
     &         ,AS(3,3,2),AT(3,3,2)
      DATA VS0 / 0.D0, 1.D0, 0.D0 /
      DATA VT0 / 0.D0, 0.D0, 1.D0 /

      CALL LENGTH(RL,GRID(1,NP(1)),GRID(1,NP(2)),3)

      E(:,1) = GRID(:,NP(2)) - GRID(:,NP(1))
      CALL DIRCOS(E(1,1),E(1,1),3)
      CALL CROSS2(E(1,1),V,E(1,3))
      CALL CROSS(E(1,3),E(1,1),E(1,2))

      DO I = 1, 3
        DO J = 1, 3
          TE(I,J) = E(J,I)
        ENDDO
      ENDDO

      AS(:,:,:) = 0.D0
      AT(:,:,:) = 0.D0

      DO I = 1, 2

        CALL AXB(U(1,I),TE,UG(1,NP(I)),3,3,1)

        CALL AXB(VST(1,I),TE,VG(1,1,I),3,3,1)
        CALL AXB(VTT(1,I),TE,VG(1,2,I),3,3,1)

        VS(:,I) = VST(:,I) - VS0(:)
        VT(:,I) = VTT(:,I) - VT0(:)

        IF( IGNL > 0 ) THEN

          AS(1,2,I) =  VST(3,I)
          AS(1,3,I) = -VST(2,I)
          AS(2,1,I) = -VST(3,I)
          AS(2,3,I) =  VST(1,I)
          AS(3,1,I) =  VST(2,I)
          AS(3,2,I) = -VST(1,I)

          AT(1,2,I) =  VTT(3,I)
          AT(1,3,I) = -VTT(2,I)
          AT(2,1,I) = -VTT(3,I)
          AT(2,3,I) =  VTT(1,I)
          AT(3,1,I) =  VTT(2,I)
          AT(3,2,I) = -VTT(1,I)

        ELSE

          AS(1,2,I) =  VS0(3)
          AS(1,3,I) = -VS0(2)
          AS(2,1,I) = -VS0(3)
          AS(2,3,I) =  VS0(1)
          AS(3,1,I) =  VS0(2)
          AS(3,2,I) = -VS0(1)

          AT(1,2,I) =  VT0(3)
          AT(1,3,I) = -VT0(2)
          AT(2,1,I) = -VT0(3)
          AT(2,3,I) =  VT0(1)
          AT(3,1,I) =  VT0(2)
          AT(3,2,I) = -VT0(1)

        ENDIF

      ENDDO

      END

      