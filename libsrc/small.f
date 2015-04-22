!-----------------------------------------------------------------------
!
!     This section of DANLIB covers 'other' FE routines
!     that do not have a home elsewhere
!
! 
!   --- one element's worth matrices / vectors ----
!    FORM_KM          : Stiffness matrix - general 2d/3d FE
!    KM_ROD         : a header to 2d/3d 2nl rod element
!      GET_LINE_DCS : converts COORD into elem. len. and direction.
!      KM_ROD2D     : a 2d pin-jointed element
!      KM_ROD3D     : a 3d pin-jointed element
!      KM_BMCOL2    : a 2d Beam-column element
!    FORM_KP          : fluid 'stiffness' matrix
!    FORM_KE          : coupled soil-fluid matrix 
!    ASSEMBLE_KE      : explicit (KM+KP+C->KE) (obs)
!
!    FORM_MM          : the mass-matrix (for dynamics)
!    LUMP_MM_ROW      : to a diagonal vector (sum rows)
!    LUMP_MM_SPECIAL  : to a diagonal vector (Irons's weighted rows)
!
!    GET_COORD_GPS    : returns the global coords of the GP's for plotting
!    APPLY_GRAVITY_1EL: self-weight forces 
!    EXCAV_FORCES_1EL : boundary forces if this element is removed
!
! --- one element's worth of plasticity ---
!    PLAST_1EL_ROD
!    PLAST_1EL
!


!  --- DEE, KAY and BEE matrices ---
!    FORM_DEE         : stress-strain matrix : 2D/3D full/SRI
!    FORM_KAY         : fluid permeability matrix
!    FORM_ANY_BEE     : strain-displacment matrix : 2Dd/3D/Axisym
!    FORM_BEE_OLD     : older version of the above
!    FORM_BEE         : another old version
!    AMMEND_BEE_AXISYM: makes the  last line of BEE to be FUN/rad
!    FORM_BEE_3DPOLAR : full 3D matrix for cylindrical coords
!    FORM_VOL         : like BEE; for soil-fluid coupling

!--- Plasticity subroutines---
!    INVARIENTS       :  mean stress, deviatoric + lode angle
!    PRINCIPAL_STRESS : hence the 3 princiapl stresses
!    GET_F_ANY        : yield-function for *all* supported soil-models
!      GET_F_MC       : just Mohr-Coulomb
!      GET_F_DP       : just Drucker-Prager
!    VMPL             : DEE for vonMise (unused?)
!    MOCOUPL          : DEE for Mohr-Coulomb (unused?)
!    GET_DQ_DT        : visoplastic params for MC and vonMise
!    GET_FLOW         : assembles MM's into plastic FLOW matrix
!    MOCOUQ           : DQ's for M-C only
!    DP_Q             : DQ's for for Drucker-Prager
!    FORMM            : derivatives of the stress invarients - 2D
!    FORMM3           : derivatives of the stress invarients - 3D

!      Dan Kidger 1993, 1996
!-----------------------------------------------------------------------
!  revisions:
!
!     2-4-95 OK .. all the routines that act on a single element or
!            Gauss point
!    12-11-95  GET_COORD_GP  returns the global xyz coords of the Gauss Points
!    14- 7-96 above index written,

!-----------------------------------------------------------------------
      SUBROUTINE FORM_KM (KM,IKM, NDIME,NOD,ITYPE, 
!    &                   COORD,ICOORD,DEE,IDEE,NGP,IAXES)
     &           COORD,ICOORD, E,v,BW,Erate,H0,iop_sri  ,NGP,IAXES )
!
!      This forms the stiffness matrix of ANY element into KM
!      using NGP gauss-points
!      IAXES = 1 for cartesian, 2=axisym(polar), etc..
!
!      NOD   = # of nodes per element (eg.20)
!      NDIME = # of dimensions        (eg.3)
!      ITYPE = element type 1-9       (usually = 1)
!      DEE   = given 'D' matrix (for all the GP's!)
!
!     .. need to overload this with support for non-integrated KM's
!     .. eg a 2 node line element (where v=the cross-sectional area?)
!     (still of course we need to rotate to globals (cf PIPE)
!
      REAL KM(IKM,IKM) , COORD(ICOORD,*)

!----------------------- Workspace arrays ------------------------------
      PARAMETER (M_NOD =  32       !-- max # nodes per element
     &          ,M_NODOF= 4        !-- max # freedoms per node
     &          ,M_IH   = m_nodof*(m_nodof+1)/2     !-- max # stresses per GP
     &          ,MDOF   = M_NOD*M_NODOF  !-- max # freedoms per element
     &          ,ISMP  = 100       !-- max GP's per element
     &                      )
      PARAMETER ( IDER=M_NODOF,IDERIV=M_NODOF, IJAC=M_IH,IBEE=M_IH, 
     &            IDEE=M_IH)

      REAL    FUN (M_NOD)          !-- shape funs
     &       ,DER (IDER  ,M_NOD)   !-- derivs of shape funs
     &     ,DERIV (IDERIV,M_NOD)   !-- derivs of shape funs - global
     &       ,JAC (IJAC,IJAC)      !-- the 'Jacobian'
     &       ,BEE (IBEE,MDOF)      !-- the 'B' matrix
     &       ,DEE (IDEE,IDEE)      !-- the 'D' matrix
     &       ,SMP (ISMP,M_NODOF)   !-- Integration sampling points
     &       ,WTS (ISMP)           !-- Integration point 'weights'
     &        ,XY (M_NODOF)        !-- coords of a Gauss point


      IDOF = NOD * NDIME           !- or NODOF ?
      IH = NDIME*(NDIME+1)/2       !- AXISYM will IH++
!     CALL NULL2D   (KM,IKM,IDOF,IDOF)  !(done outside..hence 2 calls for SRI)
      CALL GET_ANY_GAUSS (SMP,ISMP,WTS,NDIME,NGP,NOD, area)

      DO IGP=1,NGP
         CALL GSF  (NDIME,NOD,ITYPE, DER,IDER,FUN,SMP,ISMP,IGP)
         CALL MTVMULT (COORD,ICOORD,FUN,NDIME,NOD,XY)
         CALL MAT_MULT (DER,IDER,COORD,ICOORD,JAC,IJAC,NDIME,NOD,NDIME)
         CALL INVERT_JAC (JAC,IJAC,DET,NDIME)
         CALL MAT_MULT (JAC,IJAC,DER,IDER,DERIV,IDERIV,NDIME,NDIME,NOD)
         CALL FORM_ANY_BEE (BEE,IBEE, DERIV,IDERIV, NOD,NDIME, 
     &     XY,FUN,IAXES,IH )
         IF (IAXES.eq.2) DET = DET * XY(1) !*2.*3.14159265
         WEIGHT=  DET * WTS(IGP) *area
      
!        IF (Erate .NE. 0) THEN     !- only do as required ?
          CALL FORM_DEE (DEE,IDEE,E+Erate*(h0-xy(2)),V,BW,NDIME,iop_sri)
!        ENDIF
         IF (E+Erate*(h0-xy(2)).LE.0.) CALL MYERROR (1,
     &    'negative or zero Young''s modulus detected')
         CALL FMBTDB (BEE,IBEE,DEE,IDEE,KM,IKM, WEIGHT, IH,IDOF)
      ENDDO 
      RETURN
      END

!-----------------------------------------------------------------------
      SUBROUTINE KM_ROD (KM,IKM, COORD,ICOORD, EA, NDIM)
!
!     This forms the stiffness matrix for a 2D/3D pin-jointed element
!        need a KM_BEAM too 
      REAL KM(IKM,IKM), COORD(ICOORD,*)
      IF (NDIM.EQ.2) THEN
        CALL KM_ROD2D (KM,IKM, COORD,ICOORD, EA)
      ELSEIF (NDIM.EQ.3) THEN
        CALL KM_ROD3D (KM,IKM, COORD,ICOORD, EA)
      ENDIF
      RETURN
      END

!-----------------------------------------------------------------------
      SUBROUTINE GET_LINE_DCS (COORD,ICOORD, NDIM, ELL, CX,CY,CZ)
!
!     This simply returns the direction cosines and length of a line-segment
!     used by KM_ROD2D, KM_ROD3D, AFRONT meshgen, etc
!       Dan Kidger 24-2-97
!     ? Should I handle 1d too (and 4d ?)
!
      REAL COORD(ICOORD,*), DX,DY,DZ, ELL
      INTEGER NDIM
      IF (NDIM.GE.1) DX = COORD(2,1)-COORD(1,1)
      IF (NDIM.GE.2) DY = COORD(2,2)-COORD(1,2)
      IF (NDIM.GE.3) DZ = COORD(2,3)-COORD(1,3)
      IF (NDIM.GE.4) DW = COORD(2,4)-COORD(1,4)
      IF (NDIM.EQ.1) THEN
        ELL = DX
      ELSEIF (NDIM.EQ.2) THEN
        ELL= SQRT(DX*DX+DY*DY)
        CZ = 0.
      ELSEIF (NDIM.EQ.3) THEN
        ELL= SQRT(DX*DX+DY*DY+DZ*DZ)
      ELSEIF (NDIM.EQ.4) THEN
        ELL= SQRT(DX*DX+DY*DY+DZ*DZ+DW*DW)
      ENDIF
      IF (NDIM.GE.1) CX = DX/ELL
      IF (NDIM.GE.2) CY = DY/ELL    
      IF (NDIM.GE.3) CZ = DZ/ELL    
      IF (NDIM.GE.4) CW = DW/ELL    
      RETURN
      END

!-----------------------------------------------------------------------
      SUBROUTINE KM_ROD2D (KM,IKM, COORD,ICOORD, EA)
!
!     This forms the stiffness matrix for a 2-D pin-jointed element
!     ... maybe overload with 2d/3d ?
!        IMS c.1983
!
      REAL KM(IKM,IKM), COORD(ICOORD,*), CX,CY,CZ, ELL

      CALL GET_LINE_DCS (COORD,ICOORD, 2, ELL, CX,CY,CZ)

      T1 = CX*CX *EA/ELL  !- the 3 different terms 
      T3 = CY*CY *EA/ELL
      T2 = CX*CY *EA/ELL

!     KM(1:4,1) = (/  T1,  T2, -T1, -T2/)     !- in F90
!     KM(1:4,2) = (/  T2,  T3, -T2, -T3/)
!     KM(1:4,3) = (/ -T1, -T2,  T1,  T2/)
!     KM(1:4,4) = (/ -T2, -T3, -T2, -T3/)
      KM(1,1) = T1                            !- in F77
      KM(1,2) = T2
      KM(1,3) = -T1
      KM(1,4) = -T2 

      KM(2,1) = T2 
      KM(2,2) = T3
      KM(2,3) = -T2 
      KM(2,4) = -T3

      KM(3,1) = -T1
      KM(3,2) = -T2 
      KM(3,3) = T1
      KM(3,4) = T2 

      KM(4,1) = -T2 
      KM(4,2) = -T3
      KM(4,3) = T2 
      KM(4,4) = T3

      RETURN
      END

!-----------------------------------------------------------------------
      SUBROUTINE KM_ROD3D (KM,IKM, COORD,ICOORD, EA)
!
!     This forms the stiffness matrix for a 3-D pin-jointed element
!        IMS c.1983
!
      REAL KM(IKM,IKM), COORD(ICOORD,*), CX,CY,CZ, ELL

      CALL GET_LINE_DCS (COORD,ICOORD, 3, ELL, CX,CY,CZ)
      A = CX*CX *EA/ELL            !- the 6 different terms 
      B = CY*CY *EA/ELL            ! (cf 6 stresses in 3d)
      C = CZ*CZ *EA/ELL
      D = CX*CY *EA/ELL
      E = CY*CZ *EA/ELL
      F = CZ*CX *EA/ELL

!     KM(1:6,1) = (/  A,  D,  F, -A, -D, -F /)     !- in F90
!     KM(1:6,2) = (/  D,  B,  E, -D, -B, -E /)
!     KM(1:6,3) = (/  F,  E,  C, -F, -E, -C /)
!     KM(1:6,4) = (/ -A, -D, -F,  A,  D,  F /)
!     KM(1:6,5) = (/ -D, -B, -E,  D,  B,  E /)
!     KM(1:6,6) = (/ -F, -E, -C,  F,  E,  C /)

!.. notes 1: 5+4+3+2+1 =15/36 terms are symmetricaly below the diagonal
!..       2: all 3x3 sub-matrices are the same
      KM(1,1) = A
      KM(1,2) = D
      KM(1,3) = F
      KM(1,4) = -A
      KM(1,5) = -D
      KM(1,6) = -F

      KM(2,1) = D
      KM(2,2) = B
      KM(2,3) = E
      KM(2,4) = -D
      KM(2,5) = -B
      KM(2,6) = -E

      KM(3,1) = F
      KM(3,2) = E
      KM(3,3) = C
      KM(3,4) = -F
      KM(3,5) = -E
      KM(3,6) = -C

      KM(4,1) = -A
      KM(4,2) = -D
      KM(4,3) = -F
      KM(4,4) = A
      KM(4,5) = D
      KM(4,6) = F

      KM(5,1) = -D
      KM(5,2) = -B
      KM(5,3) = -E
      KM(5,4) = D
      KM(5,5) = B
      KM(5,6) = E

      KM(6,1) = -F
      KM(6,2) = -E
      KM(6,3) = -C
      KM(6,4) = F
      KM(6,5) = E
      KM(6,6) = C

      RETURN
      END

!-----------------------------------------------------------------------
      SUBROUTINE KM_BMCOL2 (KM,IKM, COORD,ICOORD, EA,EI)
!
!      This forms the stiffness matrix of a 2-D Beam-column element
!        3 dof: x,y,rotation.
!        DJK 23-2-97 (from S+G 1988)
!  Notes:
!    1/ we do not need to NULL KM first?
!    2/ This subroutine would be much shorter in F90!
!    3/ See also my general 2d/3d beams in SAGE-PIPE (c.1994)

      REAL KM(IKM,IKM), COORD(ICOORD,*)

      CALL GET_LINE_DCS (COORD,ICOORD, 2, ELL, C,S,CZ)

      E1 = EA/ELL                 !- the 4-different terms.
      E2 = 12.*EI/(ELL*ELL*ELL)
      E3 = EI/ELL
      E4 = 6.*EI/(ELL*ELL)
!.. note that are only 6 (7) different values in KM. 
! upper 3x3
      A = C*C*E1+S*S*E2
      B = S*C*(E1-E2)
      C = -S*E4
      D = S*S*E1+C*C*E2
      E = C*E4
      F = 4.*E3
!     KM(1:6,1) = (/  A,  B,  C, -A, -B,  C /)     !- in F90 24-10-97
!     KM(1:6,2) = (/  B,  D,  E, -B, -D,  E /)     ! (needs checking)
!     KM(1:6,3) = (/  C,  E,  F, -C,  E, -F /)
!     KM(1:6,4) = (/ -A, -B, -C,  A,  B, -C /)
!     KM(1:6,5) = (/ -B, -D,  E,  B,  D, -E /)
!     KM(1:6,6) = (/  C,  E, -F, -C, -E,  F /)

      KM(1,1) = C*C*E1+S*S*E2
      KM(1,2) = S*C*(E1-E2)
      KM(1,3) = -S*E4

      KM(2,2) = S*S*E1+C*C*E2
      KM(2,3) = C*E4

      KM(3,3) = 4.*E3

!.. off-diagonal 3x3
      KM(1,4) = -KM(1,1)                      
      KM(1,5) = -KM(1,2)   !  S*C*(-E1+E2)
      KM(1,6) = KM(1,3)

      KM(2,4) = -KM(1,2)   ! KM(1,5)
      KM(2,5) = -KM(2,2)
      KM(2,6) = KM(2,3)

      KM(3,4) = -KM(1,3)    !S*E4
      KM(3,5) = -KM(2,3)    !-C*E4
      KM(3,6) = KM(3,3)/2.  !2.*E3

!.. lower 3x3  - as upper 3x3 but negate 2 terms
      KM(4,4) = KM(1,1)
      KM(4,5) = KM(1,2)
      KM(4,6) = -KM(1,3)  ! KM(3,4)

      KM(5,5) = KM(2,2)
      KM(5,6) = -KM(2,3)  ! KM(3,5)

      KM(6,6) = KM(3,3)

!... apply symmetry ...
      DO I=1,6
        DO J=I+1,6
          KM(J,I) = KM(I,J)
        ENDDO
      ENDDO
      RETURN
      END

!-----------------------------------------------------------------------
      SUBROUTINE FORM_KP (KP,IKP, NDIME,NOD,ITYPE 
     &                   ,COORD,ICOORD ,KAY,IKAY ,NGP, IAXES)
!
!      This forms the fluid-flow matrix of ANY element into KP
!      using NGP gauss-points
!
!      NOD   = # of nodes per element (eg.20)
!      NDIME = # of dimensions        (eg.3)
!      ITYPE = element type 1-9       (usually = 1)
!      KAY =  given 'D' matrix (for all the GP's!)
!
      REAL KP(IKP,IKP) , COORD(ICOORD,*), KAY(IKAY,*)

!----------------------- Workspace arrays ------------------------------
      PARAMETER (M_NOD =  32    !-- max # nodes per element
     &          ,M_NODOF= 4     !-- max # freedoms per node
     &          ,M_IH   = m_nodof*(m_nodof+1)/2     !-- max # stresses per GP
     &          ,ISMP  = 100     !-- max GP's per element
     &                      )
      PARAMETER ( IDER=M_NODOF,IDERIV=M_NODOF, IJAC=M_IH)  ! 'IH'?

      REAL    FUN (M_NOD)          !-- shape funs
     &       ,DER (IDER  ,M_NOD)   !-- derivs of shape funs
     &     ,DERIV (IDERIV,M_NOD)   !-- derivs of shape funs - global
     &       ,JAC (IJAC,IJAC)      !-- the 'Jacobian'
     &       ,SMP (ISMP,M_NODOF)   !-- Integration sampling points
     &       ,WTS (ISMP)           !-- Integration point 'weights'
     &        ,XY (M_NODOF)        !-- coords of a Gauss point

       IDOF = NOD*1      !- hmm this (truely) assumes NODOF=1.
!.. get_any_gauss should also be told what 'type' of element it is !
       CALL GET_ANY_GAUSS (SMP,ISMP,WTS,NDIME,NGP,NOD,area)

        DO IGP=1,NGP
          CALL GSF  (NDIME,NOD,ITYPE, DER,IDER,FUN,SMP,ISMP,IGP)
          CALL MTVMULT (COORD,ICOORD,FUN,NDIME,NOD,XY)
          CALL MAT_MULT (DER,IDER,COORD,ICOORD,JAC,IJAC,NDIME,NOD,NDIME)
          CALL INVERT_JAC (JAC,IJAC,DET,NDIME)
          CALL MAT_MULT (JAC,IJAC,DER,IDER,DERIV,IDERIV,NDIME,NDIME,NOD)
          IF (IAXES.eq.2) DET = DET * XY(1)  !*2.*3.14159265
          WEIGHT=  DET * WTS(IGP) * area
          CALL FMBTDB (DERIV,IDERIV,KAY,IKAY,KP,IKP, WEIGHT, NDIME,IDOF)
        ENDDO   !--- loop GP's ---
      RETURN
      END

!-----------------------------------------------------------------------
      SUBROUTINE FORM_KE (KE,IKE, NDIME,NOD,ITYPE 
!    &      , COORD,ICOORD, DEE,IDEE,KAY,IKAY
     &      , COORD,ICOORD, E,v,BW,Erate,H0,iop_sri ,KAY,IKAY
     &      , FACTOR,NGP,IAXES)
!
!        (unfinished?)
!      This forms the coupled fluid-soil stiffness matrix of ANY element 
!      into KE using NGP gauss-points
!         Dan Kidger 21-2-96
!      NOD   = # of nodes per element (eg.20)
!      NDIME = # of dimensions        (eg.3)
!      ITYPE = element type 1-9       (usually = 1)
!      DEE   = given 'D' matrix (for all the GP's!)
!      KAY   = given 'K' matrix
!      IAXES = 1 for cartesian, 2=axisym(polar), etc..
!
      REAL KE(IKE,IKE) , COORD(ICOORD,*), kay(IKAY)   !,DEE(IDEE,IDEE)
               
!----------------------- Workspace arrays ------------------------------
      PARAMETER (M_NOD =  32    !-- max # nodes per element
     &          ,M_NODOF= 4     !-- max # freedoms per node
     &          ,M_IH   = m_nodof*(m_nodof+1)/2     !-- max # stresses per GP
!    &          ,MDOF   = M_NOD*M_NODOF  !-- max # freedoms per element
     &          ,ISMP  = 100     !-- max GP's per element
     &          ,MDOF= M_NOD*M_NODOF+M_NOD     !- for BIOT ?
     &                      )
      PARAMETER ( IDER=M_NODOF,IDERIV=M_NODOF, IJAC=M_IH,IBEE=M_IH
     &      ,IKP=M_NOD ,IC=M_NOD 
     &           ,IDEE=M_IH)

      REAL    FUN (M_NOD)          !-- shape funs
     &       ,DER (IDER  ,M_NOD)   !-- derivs of shape funs
     &     ,DERIV (IDERIV,M_NOD)   !-- derivs of shape funs - global
     &       ,JAC (IJAC,IJAC)      !-- the 'Jacobian'
     &       ,BEE (IBEE,MDOF)      !-- the 'B' matrix
     &       ,DEE (IDEE,IDEE)      !-- the 'D' matrix
     &       ,SMP (ISMP,M_NODOF)   !-- Integration sampling points
     &       ,WTS (ISMP)           !-- Integration point 'weights'
!    &         ,C (IC,60)          !-- coupling matrix
     &        ,KP (IKP,IKP)        !-- fluid matrix
     &       ,VOL (MDOF)           !-- 'volumetric' matrix ** check all this !
     &        ,XY (M_NODOF)        !-- coords of a Gauss point
      INTEGER NUM2(20)  !- hmm

       IDOF = NOD * NDIME 
       IDOF_TOT = IDOF+ NOD        ! +NODF surely?
       IH = NDIME*(NDIME+1)/2      !- AXISYM will IH++
!      ---- get the daughter element -----
!     get its #nodes, thence 'map' the parent nodes to the daughter
!      should we use ITYPE=7 say for BIOT  (8+4)
!      (and perhaps ITYPE=8 for 8+8 Biot?)
       CALL GET_BIOT_DAUGHTER (NOD,NDIME,ITYPE, NODF,NUM2)

!---------------------------------------------
!      CALL NULL2D   (KM,IKM,IDOF,IDOF)  !(done outside..hence 2 calls for SRI)
!      .. but note that IDOF is bigger than just NOD*NDIME
       CALL GET_ANY_GAUSS (SMP,ISMP,WTS,NDIME,NGP,NOD, area)
!---------------------------------------------

!       ------- the soil part ------
        DO IGP=1,NGP
          CALL GSF  (NDIME,NOD,ITYPE, DER,IDER,FUN,SMP,ISMP,IGP)
          CALL MTVMULT (COORD,ICOORD,FUN,NDIME,NOD,XY)
          CALL MAT_MULT (DER,IDER,COORD,ICOORD,JAC,IJAC,NDIME,NOD,NDIME)
          CALL INVERT_JAC (JAC,IJAC,DET,NDIME)
          CALL MAT_MULT (JAC,IJAC,DER,IDER,DERIV,IDERIV,NDIME,NDIME,NOD)
!.. just pass XY rather than FUN and COORD to next?
          CALL FORM_ANY_BEE (BEE,IBEE, DERIV,IDERIV, NOD,NDIME,
     &     XY,FUN,IAXES,IH )
          IF (IAXES.eq.2) DET = DET * XY(1)  !*2.*3.14159265
          CALL FORM_VOL (DERIV,IDERIV,VOL,NOD,NODOF)  ! ** note this **
          WEIGHT=  DET * WTS(IGP) *area
!         IF (Erate .NE. 0) THEN     !- only do as required ?
          CALL FORM_DEE (DEE,IDEE,E+Erate*(h0-xy(2)),V,BW,NDIME,iop_sri)
!         ENDIF
          CALL FMBTDB (BEE,IBEE,DEE,IDEE,KE,IKE, WEIGHT, IH,IDOF)

!         ------- the fluid part ------
          CALL GSF  (NDIME,NODF,ITYPE, DER,IDER,FUN,SMP,ISMP,IGP)
!         CALL MTVMULT (COORD,ICOORD,FUN,NDIME,NOD,XY)
          CALL MAT_MULT (DER,IDER,COORD,ICOORD,JAC,IJAC,NDIME,NOD,NDIME)
          CALL INVERT_JAC (JAC,IJAC,DET,NDIME)
          CALL MAT_MULT (JAC,IJAC,DER,IDER,DERIV,IDERIV,NDIME,NDIME,NOD)
          WEIGHT=  DET * WTS(IGP) * area
          CALL FMBTDB (DERIV,IDERIV,KAY,IKAY,KP,IKP, WEIGHT, NDIME,IDOF)

!         ------- the coupling ------
          QUOT = 1. * DET !- is this correct ??
          DO I=1,IDOF
            DO J=1,NODF
              T = VOL(I) * FUN(J) * QUOT
!             C(I,J) = C(I,J) + T
              KE(IDOF+I,J) = KE(IDOF+I,J) + T
              KE(I,IDOF+J) = KE(I,IDOF+J) + T
            ENDDO
          ENDDO

        ENDDO   !--- loop GP's ---

        DO J=1,NOD              !---- add the fluid matrix at the bottom
          DO I=1,NOD                                            
            KE (IDOF+I,IDOF+J) = KE (IDOF+I,IDOF+J) + KP(I,J) * FACTOR 
          ENDDO                                            !(theta*dtime)
        ENDDO
! hmm what if we have freedoms: dx,dy,bending,pp ?
! I hope that this just affects G not KE ? (it does)

      RETURN
      END

!-----------------------------------------------------------------------
      SUBROUTINE FORM_MM (MM,IMM, NDIME,NOD,ITYPE, 
     &                    COORD,ICOORD,RHO,NGP)
!
!      This forms the mass-matrix of ANY element into MM
!      using NGP gauss-points
!
!      NOD   = # of nodes per element (eg.20)
!      NDIME = # of dimensions        (eg.3)
!      ITYPE = element type 1-9       (usually = 1)
!      RHO = the density of the material
!
!  Caveats:
!      If a rod or beam, we need to muliply by the sectional area
!
      REAL MM(IMM,IMM) , COORD(ICOORD,*)

!----------------------- Workspace arrays ------------------------------
      PARAMETER (M_NOD =  32    !-- max # nodes per element
     &          ,M_NODOF= 4     !-- max # freedoms per node
     &          ,M_IH   = m_nodof*(m_nodof+1)/2     !-- max # stresses per GP
     &          ,ISMP  = 100     !-- max GP's per element
     &                      )
      PARAMETER ( IDER=M_NODOF, IJAC=M_IH)

      REAL    FUN (M_NOD)          !-- shape funs
     &       ,DER (IDER  ,M_NOD)   !-- derivs of shape funs
     &       ,JAC (IJAC,IJAC)      !-- the 'Jacobian'
     &       ,SMP (ISMP,M_NODOF)   !-- Integration sampling points
     &       ,WTS (ISMP)           !-- Integration point 'weights'
     &        ,XY (M_NODOF)        !-- coords of a Gauss point

       IDOF = NOD * NDIME    !- or NODOF ?
       CALL NULL2D   (MM,IMM,IDOF,IDOF)

!.. get_any_gauss should also be told what 'type' of element it is !
       CALL GET_ANY_GAUSS (SMP,ISMP,WTS,NDIME,NGP,NOD,area)

        DO IGP=1,NGP
          CALL GSF  (NDIME,NOD,ITYPE, DER,IDER,FUN,SMP,ISMP,IGP)
          CALL MTVMULT (COORD,ICOORD,FUN,NDIME,NOD,XY)
          CALL MAT_MULT (DER,IDER,COORD,ICOORD,JAC,IJAC,NDIME,NOD,NDIME)
          CALL INVERT_JAC (JAC,IJAC,DET,NDIME)
          WEIGHT =  DET * WTS(IGP) *area * RHO
! if a beam: WEIGHT=WEIGHT*SECTION_AREA
! plates + shells simerlarly ?
! genericaly if this element is embedded in a greater dimension we
! must allow for this: rods in plane-strain, rods in 3d, plates in 3d, axisym

!........ loop MM and product-in FUN*FUN (for each DOF)
          DO I=1,NOD
            DO J=1,NOD
              VAL = FUN(I)*FUN(J) * WEIGHT
              DO K=1,NDIME
                MM ((I-1)*NDIME+K,(J-1)*NDIME+K) = 
     &          MM ((I-1)*NDIME+K,(J-1)*NDIME+K) + VAL
              ENDDO
            ENDDO
          ENDDO

        ENDDO   !--- loop GP's ---

!... hack to produce the MM for the 8nq as given in Book10.2
!.. cf the option to choose how we lump the MM (Hinton, etc.)
!      if (1.eq.0) then
!        CALL NULL2D (MM,IMM,IDOF,IDOF)
!        DO I=1,IDOF
!          MM(I,I) = 1. * 1.33333 * RHO * .2
!        ENDDO
!        DO I=1,13,4
!          MM(I,I) = MM(I,I) * .25
!        ENDDO
!        DO I=2,14,4
!          MM(I,I) = MM(I,I) * .25
!        ENDDO
!      endif
!... end of hack

      RETURN
      END

!-----------------------------------------------------------------------
      SUBROUTINE LUMP_MM_ROW (MM,IMM, IDOF)
!
!     This lumps the mass-matrix by simply summing the rows 
!     ... note: this may give -ve masses (eg the corner-nodes of the 8nq
!
      REAL MM (IMM,IMM)
      INTEGER IMM,IDOF, I,J
      DO I=1,IDOF
        VAL = 0.
        DO J=1,IDOF
          VAL = VAL + MM(I,J)
          MM(I,J) = 0.
        ENDDO
        MM(I,I) = VAL
      ENDDO
      RETURN
      END

!-----------------------------------------------------------------------
      SUBROUTINE LUMP_MM_SPECIAL (MM,IMM, IDOF)
!
!     This lumps the mass-matrix (Hinton et al. EE+SD, 1976)
!     .. it always produces positive masses by giving each diagonal
!     an amount of the total in proportion to its original value
!
      REAL MM (IMM,IMM)

      TOTMASS = 0.
      TOTDIAG = 0.
      DO I=1,IDOF
        TOTDIAG = TOTDIAG+MM(I,I)
        DO J=1,IDOF
          TOTMASS = TOTMASS + MM(I,J)
          IF (I.NE.J) MM(I,J) = 0.         !-- zap the off-diagonals
        ENDDO
      ENDDO
      DO I=1,IDOF
        MM(I,I) = MM(I,I) / TOTDIAG * TOTMASS
      ENDDO
      RETURN
      END

!-----------------------------------------------------------------------
      SUBROUTINE GET_COORD_GPS (COORD,ICOORD,NDIM, NDIME,NOD,ITYPE
     &              ,COORD_GPS,ICOORD_GPS,NGP)
!
!     This returns a table of the global coordinates of the Gauss-points
!     for any element.
!     (cf WTHATN that returns the nodal coords in Locals)
!     Note that COORD_GPS 's subscripts are the same as COORD as opposed
!     to the transpose (like DER)
!     This routine is typicaly used when plotting stresses at the GP's
!        Dan Kidger 12-11-95
!
      IMPLICIT NONE
      INTEGER ICOORD,NDIM, NDIME,NOD,ITYPE, ICOORD_GPS, NGP
      REAL      COORD (ICOORD, NDIM)  !-- nodal coords
     &     ,COORD_GPS (ICOORD_GPS,*)  !-- the resulting nodal forces

!----------------------- Workspace arrays ------------------------------
      INTEGER M_NOD,M_NDIM,ISMP,IDER
      PARAMETER (M_NOD = 32           !-- max # nodes per element
     &          ,M_NDIM= 4            !-- max # freedoms per node
     &          ,ISMP  = 27 )         !-- max GP's per element
      PARAMETER (IDER=M_NDIM)

      REAL    FUN (M_NOD)             !-- shape funs
     &       ,DER (IDER,M_NOD)        !-- derivs of shape funs
     &       ,SMP (ISMP,M_NDIM)       !-- Integration sampling points
     &       ,WTS (ISMP)              !-- Integration point 'weights'
      INTEGER IGP, J      !- locals
      REAL AREA, XY(M_NDIM)

      CALL GET_ANY_GAUSS (SMP,ISMP,WTS,NDIME,NGP,NOD,AREA)

!------------------------ loop the G.P.'s -----------------------------
      DO IGP=1,NGP
        CALL GSF (NDIME,NOD,ITYPE, DER,IDER,FUN,SMP,ISMP,IGP)
!       CALL MTVMULT (COORD,ICOORD,FUN,NDIME,NOD,XY)
        CALL MTVMULT (COORD,ICOORD,FUN,NDIM ,NOD,XY)
        DO J=1,NDIM
          COORD_GPS(J,IGP) = XY(J)
        ENDDO
      ENDDO   
      RETURN
      END

!-----------------------------------------------------------------------
      SUBROUTINE APPLY_GRAVITY_1EL (COORD,ICOORD, NOD,NDIM ,NDIME,itype
     &               ,RHO, GRAVLO,IGRAVLO, iaxes)
!
!     This forms the gavity loading 
!     from the 'Database' of elements in NUMS and GC  
!     (Material props in PRPTAB)
!     .. what about rods and beams ?
!
      REAL
     &          COORD (ICOORD, NDIM)  !-- nodal coords
     &        ,GRAVLO (IGRAVLO,NDIM)  !-- the resulting nodal forces
     &           ,RHO (NDIM)          !-- Gravity !

!----------------------- Workspace arrays ------------------------------
      PARAMETER (M_NOD = 32           !-- max # nodes per element
     &          ,M_NDIM= 4            !-- max # freedoms per node
     &          ,ISMP  = 27           !-- max GP's per element
     &          ,M_NODOF= 3 )      
      PARAMETER (IDER=M_NDIM,IJAC=M_NDIM)

      REAL    FUN (M_NOD)             !-- shape funs
     &       ,DER (IDER,M_NOD)        !-- derivs of shape funs
     &       ,JAC (IJAC,IJAC)         !-- the 'jacobian'
     &       ,SMP (ISMP,M_NDIM)       !-- Integration sampling points
     &       ,WTS (ISMP)              !-- Integration point 'weights'
     &        ,XY (M_NODOF)           !-- coords of a Gauss point

      CALL GET_DEFAULT_NGP (NOD,NDIME,NGP)  !- cf user-defined
      CALL GET_ANY_GAUSS   (SMP,ISMP,WTS,NDIME,NGP,NOD,AREA)
      CALL NULL2D (GRAVLO,IGRAVLO, NOD, NDIM)
!     if RHO()=0 then we can just return 'cos all is weightless

!------------------------ loop the G.P.'s -----------------------------
      DO IGP=1,NGP
        CALL GSF (NDIME,NOD,ITYPE, DER,IDER,FUN,SMP,ISMP,IGP)
        CALL MTVMULT (COORD,ICOORD,FUN,NDIM,NOD,XY)
        CALL MAT_MULT (DER,IDER,COORD,ICOORD,JAC,IJAC,NDIM,NOD,NDIM)
!       CALL INVERT_JAC (JAC,IJAC,DET,NDIM)
        CALL INVERT_JAC (JAC,IJAC,DET,NDIME)          ! 25-2-98
        IF (IAXES.eq.2) DET = DET *XY(1)  !*2.*3.14159265            ! 20-8-97

        WEIGHT= DET * WTS(IGP) * AREA  ! AREA<1. if a triangle 'axisym too?'
        DO J=1,NDIM           !- explicit VVMULT
          DO I=1,NOD
            GRAVLO(I,J) = GRAVLO(I,J) + WEIGHT * FUN(I) * RHO(J)
          ENDDO
        ENDDO
      ENDDO   
      RETURN
      END

!-----------------------------------------------------------------------
      SUBROUTINE EXCAV_FORCES_1EL (COORD,ICOORD, NOD,NDIM ,NDIME, ITYPE
     &               ,STRESSES, PORES, EX_LO,IEX_LO, IAXES)
!
!     This returns the 'Excavation Forces' EX_LO for an element
!     as simply =[B]T*SIGMA.
!       Dan Kidger c.1995
! Notes:
!   1/ 24-10-97 Fixed bug for triangle elements
!   2/ 25- 2-98 What if we excavate a rod -element eg. a prop?
!   3/ 12- 5-99 if STRESSES hold the effective (not total) stresses then
!               we must add the pore pressures too. (we can do this 
!               outside this routine)
!
      REAL
     &          COORD (ICOORD, NDIM)  !- nodal coords
     &        ,EX_LO (IEX_LO,NDIM)    !- the resulting nodal forces
     &       ,STRESSES (*)            !- all stresses in this elem. 
     &       ,PORES (*)               !- GP Pore Pressures

!----------------------- Workspace arrays ------------------------------
      PARAMETER (M_NOD = 32           !- max # nodes per element
     &          ,M_NDIM= 4            !- max # freedoms per node
     &          ,M_IH   = 6           !- max # stresses per GP
     &          ,ISMP  = 27           !- max GP's per element
     &          ,MDOF   = M_NOD*M_NDIM )  !-- max # freedoms per element
      PARAMETER (IDER=M_NDIM, IJAC=M_NDIM, IDERIV=M_NDIM, IBEE=M_IH
     &          ,M_NODOF= 3 )      

      REAL    FUN (M_NOD)             !- shape funs
     &       ,DER (IDER,M_NOD)        !- derivs of shape funs
     &       ,JAC (IJAC,IJAC)         !- the 'jacobian'
     &     ,DERIV (IDERIV,M_NOD)      !- derivs of shape funs - global
     &       ,BEE (IBEE,MDOF)         !- the 'B' matrix
     &       ,SMP (ISMP,M_NDIM)       !-- Integration sampling points
     &       ,WTS (ISMP)              !-- Integration point 'weights'
     &        ,XY (M_NODOF)           !-- coords of a Gauss point
      LOGICAL DIRECT                  !- if sx,sy or sz (ie not shear)

      IH = NDIME * (NDIME+1)/2        !- = 1 in 1D, 3 in 2D, 6 in 3D, 10 in 4D
      IF (NDIME.EQ.2) IH = 4          !- include the z term ?

!     NGP = 2**NDIME                  !-- to match the outside world ! (arrgh)
!     -- hmm so always 2x2 or 2x2x2 GPs  ** This will break triangles!
!     < Kara noticed this bug in Aug. 1997 >
      CALL GET_DEFAULT_NGP (NOD,NDIME,NGP)  !- cf user-defined

      CALL GET_ANY_GAUSS   (SMP,ISMP,WTS,NDIME,NGP,NOD,AREA)
      CALL NULL2D (BEE,IBEE, IH,NOD*NDIME)  !- only needed for the LAST row
      CALL NULL2D (EX_LO,IEX_LO, NOD, NDIM)

!------------------------ loop the G.P.'s -----------------------------
      DO IGP=1,NGP
        CALL GSF (NDIME,NOD,ITYPE, DER,IDER,FUN,SMP,ISMP,IGP)
        CALL MTVMULT (COORD,ICOORD,FUN,NDIM,NOD,XY)
        CALL MAT_MULT (DER,IDER,COORD,ICOORD,JAC,IJAC,NDIM,NOD,NDIM)
        CALL INVERT_JAC (JAC,IJAC,DET,NDIM)
        CALL MAT_MULT (JAC,IJAC,DER,IDER,DERIV,IDERIV,NDIM,NDIM,NOD)
        CALL FORM_ANY_BEE (BEE,IBEE, DERIV,IDERIV, NOD,NDIME,
     &     XY,FUN,IAXES,IH )
        IF (IAXES.eq.2) DET = DET * XY(1) !*2.*3.14159265
        WEIGHT =  DET * WTS(IGP) *AREA

        DO K=1,IH                 
          DIRECT=(k.eq.1.or.k.eq.2.or.
     &     (k.eq.4.and.ndime.eq.2).or.(k.eq.3.and.ndime.eq.3))
          IPT = (IGP-1)*IH + K
          T = STRESSES(IPT) * WEIGHT      !- STRESS = cummulative stress
!----- add pore pressure (to the direct stresses only) ------
          if (direct) T = T + pores(igp) *WEIGHT

          IC = 0                      !--- hence the reaction forces ---
          DO I=1,NOD
!           II = NUM(I)         !-/ cf direct into global
            II =     I          !-\      or just a local
            DO J=1,NDIM
              IC = IC + 1
              EX_LO (II,J) = EX_LO (ii,J) + BEE(K,IC) * T
            ENDDO
          ENDDO
        ENDDO      !- the 4 (or 6) streses
      ENDDO   
      RETURN
      END

!-----------------------------------------------------------------------
!        > > > > >    'Plasticity for one element' routines    < < < < <
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
      SUBROUTINE PLAST_1EL_ROD (COORD,ICOORD, NDIM, ITYPE,ELD,
     &    PROP,STRESS, STRAIN, CONVERGED)
!
!     Handles the Plasticity (non-linearity) of rod and beam elements
!        Dan Kidger 24-2-97
!
      REAL COORD(ICOORD,*), ELD(*), PROP(*), STRESS(*), STRAIN(*)
      INTEGER NDIM,ITYPE,ICOORD 
      LOGICAL CONVERGED

      CALL GET_LINE_DCS (COORD,ICOORD, ndim, ELL, CX,CY,CZ)
      E = PROP(1)      !- do I like this style of abstraction?
      A = PROP(23)          

      IF (ITYPE.EQ.1) THEN    !- type 1 = rods
        IF (NDIM.EQ.1) THEN
          EAX = ELD(2) -ELD(1)         ! (CX=1. here)
        ELSEIF (NDIM.EQ.2) THEN
          EAX = (ELD(3)*CX + ELD(4)*CY) -(ELD(1)*CX + ELD(2)*CY)
        ELSEIF (NDIM.EQ.3) THEN        !- oops was =2!
          EAX = ELD(4) * CX + ELD(5)*CY + ELD(6)*CZ 
     &       -( ELD(1) * CX + ELD(2)*CY + ELD(3)*CZ) 
        ENDIF     
!.. hey! what about dividing by the element length too?
        EAX = EAX / ELL

        SAX = EAX * E        !- axial stress (bug fixed 20-5-97)

        IF (CONVERGED) THEN
          STRAIN(1) = EAX    !- elastic or plastic strain ?
          STRESS(1) = SAX    !- and store (what about the other '2d' stresses?
        ELSE
!.. hmm.. what do we do here ?
!   So clip STRESS to the max allowable
!  and return the XS stress.

!.. cf SAGE-PIPE and Ramberg_Osgood ?
        ENDIF

!     CALL KM_ROD (KM,IKM, COORD,ICOORD, E*A, NDIM)
!     ELSEIF (ITYPE.EQ.2) THEN    !- type 2 = beam-columns
!.. we dont know how to handle 3dof in 2d space yet?
!       R_I = PRPTAB(24,IMAT)
!       CALL KM_BMCOL2 (KM,IKM, COORD,ICOORD, E*A,E*R_I)
!     ELSEIF (ITYPE.EQ.3) THEN    !- type 3 = plus axial-force 
      ENDIF

      RETURN
      END

!-----------------------------------------------------------------------
      SUBROUTINE PLAST_1EL (ELD,    NDIME,NOD,ITYPE ,COORD,ICOORD
!    &         ,COORD,ICOORD,DEE,IDEE, E,V,C,PHI,PSI
     &     ,E0,V,BW,Erate,H0,iop_sri, C0,Crate,PHI0,PSI0
     &     ,NGP,IAXES, NUM, NDIM, IDOF, NODOF
     &     ,STRESSES,PORES, STRAINS, BDYLDS_N,IBDYLDS_N
     &     ,CONVERGED, ngp_failed1, F_MSS1, IOP_VM   ) 
!
!     This takes an elements' disp. increment ELD, and the existing
!     stress state STRESSES and produces the XS bodyforces FORCES
!     IOP_VM codes for the plasticity model (0=elastic, 1=VM, ..)
!
!    9-9-97 E,Cu = f(depth) ?
!   hmm what if E and Cu are not constant but a linear function of depth.
!   in this case we must form_DEE within this routine.
!   perhaps I should just pass C and Crate to here (and E and Erate and Yo)
!   to here and do all the work here?

!.. Make this a subroutine = PLAST_1EL (MC+VM), MONOT will be seperate?
!.. from COORD,ELD,(EVPT) --> to XS Forces (& update stress at convergence)
!.. (BT*sigma at convergence too so reaction forces)

!   14-3-99 for undrained analyses, we must update the PP too at convergence
!
      INTEGER     NUM (*)               !- element's nodes (obs?)
     &     ,NDIME,NOD,ITYPE             !- element type
     &     ,NGP, IAXES, ngp_failed1

      REAL        ELD (*)               !- displacement increment
     &         ,COORD (ICOORD,*)        !- element's coords
!    &        ,FORCES (IFORCES,*)       !- produced bodyforces
     &        ,STRESSES (*)             !- all stresses (+ hard) of this elem
     &        ,PORES    (*)             !- set of GP Pore Pressures
     &        ,STRAINS  (*)             !- all (visco)strains of this elem
     &        ,BDYLDS_N (IBDYLDS_N,*)

      LOGICAL CONVERGED

!----------------------- Workspace arrays ------------------------------
      PARAMETER (M_NOD = 32           !- max # nodes per element
     &          ,M_NODOF= 3           !- max # freedoms per node
     &          ,M_IH   = 6           !- max # stresses per GP
     &          ,MDOF   = M_NOD*M_NODOF  !-- max # freedoms per element
     &          ,ISMP  = 27           !-- max GP's per element
     &                      )
      PARAMETER (IBEE= M_IH ,IJAC=M_IH, IDER=M_NODOF,IDERIV=M_NODOF
     &     ,IFLOW=M_IH, IDEE=M_IH)

      REAL
     &        SMP (ISMP,M_NODOF)   !- Integration sampling points
     &       ,WTS (ISMP)           !- Integration point 'weights'
     &       ,FUN (M_NOD)          !- shape funs
     &       ,DER (IDER  ,M_NOD)   !- derivs of shape funs
     &     ,DERIV (IDERIV,M_NOD)   !- derivs of shape funs - global
     &       ,JAC (IJAC,IJAC)      !- the 'Jacobian'
     &       ,DEE (IDEE,IDEE)      !- the 'D' matrix
     &       ,BEE (IBEE,MDOF)      !- the 'B' matrix

     &       ,EPS_TOT (M_IH)       !- tot strain inc.
     &       ,EPS (M_IH)           !- tot strain inc. less EVPT
     &    ,STRESS (M_IH)           !- STRESS increment (DEE*EPS)
     &     ,SIGMA (M_IH)           !- total STRESS   (TOTAL+STRESS)
     &      ,FLOW (IFLOW,IFLOW)    !- plastic 'flow' matrix (cf DEE)
     &       ,EVP (M_IH)           !- plastic 'flow' strain 
     &    ,EVP_NT (M_IH)           !- no-tension plastic 'flow' strain 
     & ,STRESS_FL (M_IH)           !- plastic 'flow' stress 
     &        ,XY (M_NODOF)        !-- coords of a Gauss point

!     LOGICAL VISCO
      LOGICAL YIELDED


      F_MSS1 = -9.99                !- most yielded point
      ngp_failed1=0                 !- and count them

!.. really IH is abstracted from the stress array
      IH = NDIME * (NDIME+1)/2      !- = 1 in 1D, 3 in 2D, 6 in 3D, 10 in 4D
      IF (NDIME.EQ.2) IH = 4        !- include the z term ?

!.. really we shoudl get NGP from the stress record.
      CALL GET_ANY_GAUSS (SMP,ISMP,WTS,NDIME,NGP,NOD, AREA)

      DO IGP=1,NGP 
        CALL GSF (NDIME,NOD,ITYPE, DER,IDER,FUN,SMP,ISMP,IGP)
        CALL MTVMULT (COORD,ICOORD,FUN,NDIM,NOD,XY)
        CALL MAT_MULT (DER,IDER,COORD,ICOORD,JAC,IJAC,NDIME,NOD,NDIME)
        CALL INVERT_JAC (JAC,IJAC,DET,NODOF)
        CALL MAT_MULT (JAC,IJAC,DER,IDER,DERIV,IDERIV,NDIM,NDIM,NOD)
        CALL FORM_ANY_BEE (BEE,IBEE, DERIV,IDERIV, NOD,NDIME,
     &     XY,FUN,IAXES, IH )
        IF (IAXES.eq.2) DET = DET * XY(1) !*2.*3.14159265
        WEIGHT =  DET * WTS(IGP) *AREA

        CALL MVMULT (BEE,IBEE,ELD,IH,IDOF,EPS_TOT) !- strain incrment
        DO J=1,IH                              !- take a copy 9-7-99
          EPS(J) = EPS_TOT(J)                  !- (cos we will change EPS)
        ENDDO

!... call a PLASTICITY_1GP routine ? .....

!------------------ Subtract the 'viscoplastic' strains ----------------
!.. if 'inital stress' or 'elastic' only .. skip EVPT cos n/a
!.. note STRAINS is just a slice of EVPT()
!       VISCO = (IOP_VISCO.eq.1)
        IF (IOP_VM.GE.1.AND.IOP_VM.LE.9) THEN
!    &      .and. VISCO ) THEN
          DO J=1,IH                              !- remove the plastic strain
            IPT = (IGP-1)*IH + J
            EPS(J) = EPS(J) - STRAINS(IPT)   !- hmm in 2d Ez is a dummy!
          ENDDO
        ENDIF

!---------- Get the stress increment hence total 'new' stress ----------
!.. be careful with the extraction of the old stresses.. the *IH is 
!   really the *total length* of a stress record (inc. hardening params etc.)
!   hmm does Crate get FOS too ?

        C = C0 + Crate*(h0-XY(2))
        E = E0 + Erate*(h0-xy(2))
        phi = phi0                      !(may get reset if no-tension)
        psi = psi0                      !(may get reset if no-tension)
!       BW1=BW
! 11-3-99 dont use Water Bulk Modulus for effective stress calcs.
        BW1=0.
        CALL FORM_DEE (DEE,IDEE,E,V,BW1,NODOF,1)   !
        CALL MVMULT (DEE,IDEE,EPS,IH,IH,SIGMA)   !- stress increment
        DO J=1,IH
          IPT = (IGP-1)*IH + J
          STRESS(J) = STRESSES(IPT) + SIGMA(J)   !- total stress
        ENDDO

!.. so now, we have the old stress, stress increment (elastic)
! and the strain increment too.

!-------------------------- If we have converged -----------------------
!.. elastic-only also counts as 'converged'?
!.. here we:
!      1/ Update the stresses (+strains)
!      2/ (update any hardening parameters)
!      3/ (compute the pore pressure increment)
!      4/ Calculate the reaction forces 
!         (can do seperately? .. cf  excav_elem)
!
!    .. also cf an approach where we use XS stress rather than disp as a 
!    .. measure of convergency
!
      IF (CONVERGED) THEN

!---------------------- update the total stress ------------------------
!.. cf an approach where we *ADD* SIGMA to STRESSES (every time?)
!.. also if we are doing Init. Stress we have ELSO here too.
! 14-3-99 we must update the PP too as += BW*strain.inc.

        DVOLSTRAIN= EPS_TOT(1)+EPS_TOT(2)
        IF (NDIME.eq.2) DVOLSTRAIN = DVOLSTRAIN + EPS_TOT(4) !(=0.)
        IF (NDIME.eq.3) DVOLSTRAIN = DVOLSTRAIN + EPS_TOT(3) 
        DPORE =  BW * DVOLSTRAIN
        PORES(IGP) = PORES(IGP) + DPORE

        DO K=1,IH     
          IPT = (IGP-1)*IH + K
          STRESSES(IPT) = STRESS(K)                    !both
!         STRESSES(IPT) = STRESSES(IPT) + SIGMA(K)     !the same
!  put PP term at IH+1 perhaps?

          STRAINS(IPT) = EPS(K)       !really it is += ?
        ENDDO

!------------------------ compute the reaction forces ------------------
!.. there is no need to do this!
!.. be careful about whether we want the total reactions or just the increment
!.. I think I prefer to calculate just one elem's reactions
!.. then sum outside PLAST_1EL
        DO K=1,IH                 
!         T = STRESS(K) * WEIGHT      !- STRESS = cummulative stress
          T = SIGMA (K) * WEIGHT      !- SIGMA = stress increment (be careful!)
          IC = 0                      !--- hence the reaction forces ---
          DO I=1,NOD
            II = NUM(I)
            DO J=1,NODOF
              IC = IC + 1
              BDYLDS_N (J,II) = BDYLDS_N (J,II) + BEE(K,IC) * T
            ENDDO
          ENDDO
        ENDDO      !- the 4 (or 6) streses

        GOTO 11       !-'EXIT' direct to the next Gauss-point
      ENDIF         !- if converged

!--------- Calc the Body-Loads if not converged and yielded ------------
!.. here the code will split according to 
!    1/  whether Initial stress or Visoplastic
!    2/  the constitutive model that we are using.
!
!    * ie. the next is identical to a call to MOHRPL (in MONOT.F)
!       (but give it the old stress & the increment.)
!       returns the 'new' stress

!.. if No-Tension Mohr-Coulomb, then perhaps test for tension first
!.. then if test for normal c,phi if first is OK
        CALL INVARIENTS (STRESS,NODOF,SIGM,DSBAR,THETA)
        CALL GET_F_ANY (PHI,C, SIGM,DSBAR,THETA ,F,F_MSS, IOP_VM)
        F_MSS1 = MAX (F_MSS,F_MSS1)  !- remember the max 'F' factor
!                                          !- (just for print-out)
        YIELDED = (F.GT.0.)

!------- 'no-Tension' correction --------
!.. 18-4-98 For 'No-tension' we need to correct to a phi=90, c=0. too.
!.. ie. test for both every time ?
        IF (IOP_VM.EQ.5) THEN
          CALL GET_F_ANY (90.,0., SIGM,DSBAR,THETA ,F_NT,F_MSS, IOP_VM)
          F_MSS1 = MAX (F_MSS,F_MSS1)       !- watch this one too.
          IF (F_NT.GT.0) YIELDED=.TRUE.     ! flag if gone into tension
        ENDIF
!-----------------------------------------------------------------------
!--- maybe split here instead if initial stress / viscoplastic
!.. If initial stress:
!    use old stress to get F0
!    use F0 and F to get stress on the yield surface itself
!    call MOCOPL/VMPL/(Monot) to get Dplastic
!    so ELSO = PL * EPS (inc. strain) * FAC
!    forces = BT * ELSO
!    (if converged: tot_stress += SIGMA-ELSO)
!    < so we need to calc ELSO at convergence :-(  >

!.. If viscoplastic:
!     use 'new' stress (SIGMA) to get MM(,),DQ1,DQ2,DQ3 hence FLOW(,)
!     EVP = FLOW*SIGMA *DT
!     STRESS_FLOW = DEE * EVP
!     EVPT += VP
!     forces = BT * STRESS_FLOW
!     (if converged: tot_stress += SIGMA

!------------------- yielded so get the XS stress ----------------------
!.. ie build the FLOW (cf.DEE^-1) matrix from PSI,XS-STRESS etc.
!.. hence plastic strains hence plastic stress (hence *BEE -> BDYLDS)
        IF (yielded) THEN                  !- yielded ?

        ngp_failed1=ngp_failed1+1          !- count them
        IF (IOP_VM.eq.0) THEN         !------- linear-elastic -------
          DO I=1,IH
            STRESS_FL(I) = 0.         !- never yields
          ENDDO

        ELSEIF (IOP_VM.EQ.5) THEN     !---------- MC-NT ----------
!.. this algorithm always treats tension if it exists.
!.. perhaps an alternative would be to do both ?
!.. or even average the two FLOW matrices ?

!      .. build the two components
          IF (F.GT.0.) THEN
            CALL GET_DQ_DT (SIGM,DSBAR,THETA ,E,V,C,PHI,PSI
     &        ,DQ1,DQ2,DQ3,DT ,IOP_VM)
            CALL GET_FLOW (STRESS,IH, DQ1,DQ2,DQ3, DT*F, FLOW,IFLOW)
            CALL MVMULT (FLOW,IFLOW,STRESS,IH,IH,EVP)       !- flow 'strain'
          ENDIF
          IF (F_NT.GT.0.) THEN
            CALL GET_DQ_DT (SIGM,DSBAR,THETA ,E,V,0.,90.,90.
     &        ,DQ1,DQ2,DQ3,DT ,IOP_VM)
            CALL GET_FLOW (STRESS,IH, DQ1,DQ2,DQ3, DT*F_NT, FLOW,IFLOW)
            CALL MVMULT (FLOW,IFLOW,STRESS,IH,IH,EVP_NT)   !- flow 'strain'
          ENDIF

          IF (F_NT.GT.0..and.F.lt.0.) then       !- just NT
              do i=1,ih            !- use just the no-tension pl.strains
                evp(i)=evp_nt(i)
              enddo
          ELSEIF (F_NT.GT.0..and.F.gt.0.) then
             alpha=0.5        !- method 1: use half of each
             beta =0.5
!             alpha=1.        !- method 2: use all of each
!             beta =1.
!             alpha=0.        !- method 3: use just the no-tension
!             beta =1.
!             alpha=0.5*get_random()   !- method 4: use a random proportion?
!             beta =1.-alpha
             do i=1,ih
               evp(i)=alpha*evp(i)+ beta*evp_nt(i)
             enddo
          ENDIF

          CALL MVMULT (DEE,IDEE  ,EVP  ,IH,IH,STRESS_FL)  !- flow 'stress'
          DO J=1,IH
            IPT = (IGP-1)*IH + J
            STRAINS (IPT) = STRAINS (IPT) + EVP(J)  !-- update pl. strains
          ENDDO

!------------------------------------------------
!.. 18-11-97 note in next E is now a f(depth)
!... if VisoPl we need to update the table of Viscoplastic strains
        ELSEIF (IOP_VM.ge.1.and.IOP_VM.le.9) THEN    !- MC/VM
          CALL GET_DQ_DT (SIGM,DSBAR,THETA ,E,V,C,PHI,PSI
     &      ,DQ1,DQ2,DQ3,DT ,IOP_VM)
          CALL GET_FLOW (STRESS,IH, DQ1,DQ2,DQ3, DT*F,  FLOW,IFLOW)
          CALL MVMULT (FLOW,IFLOW,STRESS,IH,IH,EVP)       !- flow 'strain'
          CALL MVMULT (DEE,IDEE  ,EVP  ,IH,IH,STRESS_FL)  !- flow 'stress'
          DO J=1,IH
            IPT = (IGP-1)*IH + J
            STRAINS (IPT) = STRAINS (IPT) + EVP(J)  !-- update pl. strains
          ENDDO

        ELSEIF (IOP_VM.eq.10) THEN        !------ 10=Monot -------

        ENDIF


!------------------ calculate the excess Body-forces ------------------
!.. here direct to global .. (via BLOAD if we want a simple ELEMENT
!.. based subroutine (ge FORM_KM, GRAV_LOAD_1EL, etc.)
!       WEIGHT =  DET * WTS(IGP) *AREA  !- 11-8-97
!       WEIGHT =  DET * WTS(IGP) *1.     !- 11-8-97
        DO K=1,IH     !-- loop stress-components
          T = STRESS_FL(K) * WEIGHT     !- be careful about SIGMA 
          IC = 0
          DO I=1,NOD
            II = NUM(I)
            DO J=1,NODOF
              IC = IC + 1
              BDYLDS_N (J,II) = BDYLDS_N (J,II) + BEE (K,IC) * T
            ENDDO
          ENDDO
        ENDDO

!----------------------------------------------------------------------
        ENDIF        !- only if a failed GP (F>=0)

   11 CONTINUE        !- jump to point if 'converged' (F90 EXIT)
      ENDDO   !-- GP's ---

!  end of subroutine so  return one elements worth of FORCES
!  (either XS bodyforces or total bodyforces (if converged)
!-----------------------------------------------------------------------
! sum these element bodyforces into the global table
! (cf my old way which summed *direct* into the global table
      RETURN
      END

!-----------------------------------------------------------------------
!        > > > > >      DEE and BEE  Subroutines    < < < < <
!-----------------------------------------------------------------------
      SUBROUTINE FORM_DEE (DEE,IDEE,E,V,BW,NODOF,IOP)
!
!     This forms most stress/strain matrices in 2D and 3D
!        Dan Kidger   c. 1989
!
!     Set IOP = (1)NORM, (2)K, (3)G, 4(LAME), 5(G),6(PLANE-STRESS)
!     BW = modulus of the water ( =0.0 if none)
!     -- Axisym is automatic if there is room in DEE (>=4,4)
!    
      REAL DEE(IDEE,*), FACT(10)
      INTEGER M(3,6)
      DATA M/5,4,2, 3,3,1, 7,8,2, 4,4,1, 6,1,2,  9,10,2/

      CALL NULL2D (DEE,IDEE,IDEE,IDEE)

!----- fact= (1)0.,(2)G,(3)K,(4)LAME,(5)L2,(6)2G,(7)4G/3,(8)=2G/3 ------

      FACT(1) = 0.0
      FACT(2) = E/2./(1.+V)
      FACT(3) = E/3./(1.-2.*V)              + BW
      FACT(4) =     V *E/(1.+V)/(1.-2.*V)   + BW
      FACT(5) = (1.-V)*E/(1.+V)/(1.-2.*V)   + BW
      FACT(6) =   2.*FACT(2)                  
      FACT(7) =   4.*FACT(2)/3.
      FACT(8) = - 2.*FACT(2)/3.
      FACT(9) =     E/(1.-V*V)              
      FACT(10)=   V*E/(1.-V*V)

      DO I=1,NODOF                     !----- direct terms -----
        DEE(I,I) = FACT(M(1,IOP))
        DO J=1,NODOF
          IF (I.NE.J) DEE(I,J) = FACT(M(2,IOP))
        ENDDO
      ENDDO
      DO I=NODOF+1, NODOF*(NODOF+1)/2  !----- shear terms ------
        DEE(I,I) = FACT(M(3,IOP))
      ENDDO

      IF (NODOF.EQ.2.AND.IDEE.GE.4) THEN  !---- Sz terms if 2D -----
        DEE (1,4) = DEE (1,2)
        DEE (2,4) = DEE (1,2)
        DEE (3,4) = 0.
        DEE (4,4) = DEE (1,1)

        DO I=1,4                                      ! 3?
          DEE(4,I) = DEE(I,4)   !- symmetry
        ENDDO
      ENDIF
      RETURN
      END

!-----------------------------------------------------------------------
      SUBROUTINE FORM_KAY (KAY,IKAY,KX,KY,KZ,NDIME,IOP)
!
!     This simply forms the fluid K matrix (cf DEE)
!      IOP = 1 : Normal matrix
!      IOP = 2 : Inverse matrix (for calculating flow-lines)
!
      REAL KAY(IKAY,IKAY), KX,KY,KZ

      CALL NULL2D (KAY,IKAY,NDIME,NDIME)
      IF (IOP.EQ.1) THEN
        IF (NDIME.GE.1) KAY(1,1) = KX
        IF (NDIME.GE.2) KAY(2,2) = KY
        IF (NDIME.GE.3) KAY(3,3) = KZ
      ELSEIF (IOP.EQ.2) THEN
        IF (NDIME.GE.1) KAY(1,1) = KX
        IF (NDIME.GE.2) KAY(2,2) = KY
        IF (NDIME.GE.3) KAY(3,3) = KZ
      ELSE
        CALL MYERROR (2,'Unknown option in FORM_KAY')
      ENDIF
      IF (IOP.EQ.2)  CALL INVERT_JAC (KAY,IKAY,DET,NDIME)
      RETURN
      END

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!     Routines for Building BEE : the strain-displacement matrix.
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
      SUBROUTINE FORM_ANY_BEE (BEE,IBEE, DERIV,IDERIV, NOD,NDIME,
     &     XY,FUN, IAXES,IH )
!
!     This forms any BEE matrix 
!     NDIME = the #dimensions of the problem (2,3,any)
!     IAXES = 1 for Cartesian, 2= axisymetry about the x-axis (3D too)
!         DJK 7-8-95
!     (note that AREA is modified accordingly?)
!
      REAL BEE(IBEE,*), DERIV(IDERIV,*), XY(*),FUN(*)
      INTEGER NOD,NDIME,IAXES

      IF (IAXES.EQ.1) THEN
        CALL FORM_BEE (BEE,IBEE,DERIV,IDERIV,NOD,NDIME,IH)
      ELSEIF (IAXES.EQ.2) THEN
        IF (NDIME.EQ.2) THEN
          IH = 4                 !- (I think it is already=3?)
          CALL FORM_BEE (BEE,IBEE,DERIV,IDERIV,NOD,NDIME,IH)
          CALL AMMEND_BEE_AXISYM (BEE,IBEE, FUN,NOD,NDIME, XY(1))
        ELSEIF (NDIME.EQ.3) THEN     !- can 'ammend' the 3D BEE too
          CALL FORM_BEE_3DPOLAR (BEE,IBEE,DERIV,IDERIV,FUN,RAD,NOD)
        ENDIF
      ELSE
        PRINT*,'>< Iaxes = ',Iaxes
        CALL MYERROR (2,'Unknown axes type (FORM_ANY_BEE)')
      ENDIF
      END

!-----------------------------------------------------------------------
!.. need a FORM_BEE that adds the axisymetric terms into the bottom row
!      also a BEE for cylindrical coordinates (see Dr. Ning Su)
!-----------------------------------------------------------------------
      SUBROUTINE FORM_BEE_OLD (BEE,IBEE,DERIV,IDERIV,NOD,NODOF)
!     (*obsolete*)
!     This forms the strain-displacement matrix for 1D,2D & 3D etc.
!      7-4-95 see 'new' version which will vectorise better
!
      REAL BEE(IBEE,*), DERIV(IDERIV,*)

      IH = NODOF*(NODOF+1)/2                 != 1 (1D), 4 (2D), 6 (3D)
      CALL NULL2D (BEE,IBEE,IH ,NOD*NODOF)

      IB = 0
      DO I=1,NOD
        IV = NODOF 
        DO J=1,NODOF
          BEE (J,IB + J) = DERIV(J,I)        !- direct terms
          DO K=J+1,NODOF
            IV = IV + 1
            BEE(IV,IB + J) = DERIV(K,I)      !- shear terms
            BEE(IV,IB + K) = DERIV(J,I)
          ENDDO
        ENDDO
        IB = IB + NODOF    !- update the 'base' pointer
      ENDDO
      END            

!-----------------------------------------------------------------------
      SUBROUTINE FORM_BEE (BEE,IBEE,DERIV,IDERIV,NOD,NODOF,IH)
!
!     This forms the strain-displacement matrix for 1D,2D & 3D etc.
!     7-4-95 Vectorisable version
!     .. careful of FMBRAD.. if we want the Z-term.
!
      REAL BEE(IBEE,*), DERIV(IDERIV,*)

!     CALL NULL2D (BEE,IBEE,IH ,NOD*NODOF)
!     IH = NODOF*(NODOF+1)/2                 != 1 (1D), 4? (2D), 6 (3D)


!cdir$ unroll 4
      do i=1,ih                        !-- explicit NULL here ?
!cdir$ unroll 8
        do j=1,nod*nodof
          BEE(I,J) = 0.
        enddo
      enddo
!      call lsetv(BEE,1,IBEE*NOD*NODOF,0.)

      IV = NODOF         !- row # to put the shear-terms
      DO J=1,NODOF
        DO I=1,NOD
          BEE (J,(I-1)*NODOF+J) = DERIV(J,I)        !- direct terms
        ENDDO
        DO K=J+1,NODOF
          IV = IV + 1
!dir$ unroll 4
          DO I=1,NOD
            BEE(IV,(I-1)*NODOF + J) = DERIV(K,I)      !- shear terms
            BEE(IV,(I-1)*NODOF + K) = DERIV(J,I)
          ENDDO
        ENDDO
      ENDDO
      END            

!---------------------------------------------------------------------
      SUBROUTINE AMMEND_BEE_AXISYM (BEE,IBEE, FUN,NOD,NODOF, radius)
!
!     This adds the last-row to BEE for 2D axisymmetry
!     .. note this is only Y=AXIS OF SYMMETRY (not X as for a flywheel)
!     .. compare and contrast this with BEE for 3D cylindricals
!      DJK 7-4-95
!
      IMPLICIT NONE
      INTEGER IBEE,NOD,NODOF,I
      REAL BEE(IBEE,*), FUN(*), RADIUS

      IF (NODOF.NE.2) CALL MYERROR (3,'attempt to to non-2d Axisym')
      DO I=1,NOD
        BEE(4,2*I) = 0.                  !- x=0.
        BEE(4,2*I-1) = FUN(I)/ radius    !- y= fun/r
      ENDDO
      END

!-----------------------------------------------------------------------
       SUBROUTINE FORM_BEE_3DPOLAR (BEE,IBEE,DERIV,IDERIV,FUN,RAD,NOD)
!
!     This forms the strain/displacement matrix for polar coordinates
!       .. inspired by Dr. Ning's Su version
!    (methinks he has z up, x radialy and y circumferentially)
!    (so I re-wrote this y up, x across (radius) and z around the circum.
!    -- I guess 'z' must be in radians?
!     ** This routine has not yet been verified **
!        Dan Kidger  5-8-95
!
      INTEGER NOD,IBEE,IDERIV
      REAL BEE(IBEE,*) ,DERIV(IDERIV,*), FUN(*),RAD
      INTEGER I,IB
      REAL X,Y,Z

      DO I=1,NOD
        IB = 3*(I-1) 
        X = DERIV(1,I)
        Y = DERIV(2,I)
        Z = DERIV(3,I)     !(deg to radians ??)
        BEE(1,IB+1) = X               !     X     .     .
        BEE(2,IB+2) = Y               !     .     Y     .
        BEE(3,IB+3) = Z/RAD           !     F/r   .     Z/r
        BEE(3,IB+1) = FUN(I)/RAD

!       BEE(4,IB+1) = Y/RAD           !     Y/r   X-F/r .
!       BEE(4,IB+2) = X-FUN(I)/RAD    !     Z     .     X
!       BEE(5,IB+1) = Z               !     .     Z     Y/r
!       BEE(5,IB+3) = X
!       BEE(6,IB+2) = Z
!       BEE(6,IB+3) = Y/RAD

        BEE(4,IB+1) = Y               !     Y     X     .          
        BEE(4,IB+2) = X               !     Z/r   .     X-F/r      
        BEE(5,IB+1) = Z/RAD           !     .     Z/r   Y          
        BEE(5,IB+3) = X-FUN(I)/RAD                            
        BEE(6,IB+2) = Z/RAD
        BEE(6,IB+3) = Y
      ENDDO
      END

!-----------------------------------------------------------------------
      SUBROUTINE FORM_VOL (DERIV,IDERIV,VOL,NOD,NODOF)
!
!     This forms a vector containing the derivatives of the shape funs ?
!        ie. the sum of the first NODOF rows of BEE
!     .. is it better to do this straight from DERIV or BEE ??
!          (eg. if axisymmetry)
!
      REAL DERIV(IDERIV,*),VOL(*)
      IB = 0
      DO I=1,NOD
        DO J=1,NODOF
          VOL(IB) = DERIV (J,I)
          IB = IB + 1
        ENDDO
      ENDDO
      END

!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!           > > > > > >   Plasticity  Subroutines < < < < < <
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------


!-----------------------------------------------------------------------
      SUBROUTINE INVARIENTS (STRESS,NODOF,SIGM,DSBAR,THETA)
!
!     This forms the stress invariants from the stress components 2D/3D
!       in 1D does SIGM= SX, DSBAR=THETA=0. ?
!
!    15-7-97 note when we need the plastic shear strain DEBARP
!            = sqrt (ex^2+ey^2+ez^2 + 2*exy^2) / sqrt(3.)
!
      REAL STRESS(*)
      SQ3=SQRT(3.)
      SX=STRESS(1)       !--- this block of code to abstact SX,SY,..
      SY=STRESS(2)       !-- is used elsewhere too
      IF (NODOF.EQ.1) THEN      !- only axial stress?
        SY = 0.
        SZ = 0.
        S4 = 0.
        S5 = 0.
        S6 = 0.
      ELSEIF (NODOF.EQ.2) THEN
        SZ=STRESS(4)      !- sigma-z
        S4=STRESS(3)      !- shear term
        S5 = 0.
        S6 = 0.
      ELSEIF (NODOF.EQ.3) THEN
        SZ=STRESS(3)
        S4=STRESS(4)
        S5=STRESS(5)
        S6=STRESS(6)
      ENDIF

      SIGM = (SX+SY+SZ)/3.
      D2   = ((SX-SY)**2+(SY-SZ)**2+(SZ-SX)**2)/6. +S4**2 +S5**2 +S6**2

      DSX = SX-SIGM    !- only used once so can collapse out?
      DSY = SY-SIGM    !- DSY ==(2*SY - SX-SZ)/3.
      DSZ = SZ-SIGM
!.. cf XJ3 for the next
      D3 = DSX*DSY*DSZ - DSX*S5**2 - DSY*S6**2 - DSZ*S4**2 + 2.*S4*S5*S6
!.. do some authors use DSBAR=SQRT(D2)/root2
      DSBAR = SQ3*SQRT(D2)
      IF (ABS(DSBAR).LT.1.E-10) THEN      !- ie no shear
        THETA=0.
      ELSE
        SINE=-3.*SQ3*D3/(2.*SQRT(D2)**3)
        SINE = MIN(MAX(-1.,SINE),1.)       !-- clip the lode angle
        THETA=ASIN(SINE)/3.          !- (in radians)
      ENDIF
      RETURN
      END

!-----------------------------------------------------------------------
      SUBROUTINE PRINCIPAL_STRESS (SIGM,DSBAR,THETA, S1, S2, S3)
!
!     This calculates the principal stresses from the Stress-Invarients
!     cf an eigenvalue method direct from the SX,SY,.. stresses
!      DJK 7-4-95
!
      REAL SIGM,DSBAR,THETA, S1,S2,S3

      PI3 = ACOS(-1.)/3.
      STM1 = SIGM + 2./3.*DSBAR* SIN (THETA-2.*PI3)
      STM2 = SIGM + 2./3.*DSBAR* SIN (THETA       )
      STM3 = SIGM + 2./3.*DSBAR* SIN (THETA+2.*PI3)
      S1 = MAX (STM1,STM2,STM3)
      S3 = MIN (STM1,STM2,STM3)
      S2 = STM1+STM2+STM3 -S1 -S3
!     TMAX=(S1-S3)/2.
      END

!-----------------------------------------------------------------------
      SUBROUTINE GET_F_ANY (PHI,C, SIGM,DSBAR,THETA ,F,F_MSS, IOP_VM)
!
!     returns the 'F' failure factor for any soil model
!     .. be careful of MONOT etc. that need more params
!        DJK 13-9-95
!
!      15-11-96 added Drucker-Prager.
!
      INTEGER IOP_VM              !- 0=elastic, 1=mc,2=vm,3=vm*,10=monot
      REAL     PHI,C              !- soil strnegth params
     &        ,SIGM,DSBAR,THETA   !- stress invarients
     &        ,F,F_MSS            !- the returned factors


      IF (IOP_VM.EQ.0) THEN              !- Linear elastic
        F = -1.          !- no yield
        F_MSS = 0.       !- nowhere near failure surface

      ELSEIF (IOP_VM.EQ.1) THEN          !- Mohr-Coulomb
        CALL GET_F_MC (PHI,C,SIGM,DSBAR,THETA,F, F_MSS)
      ELSEIF (IOP_VM.EQ.5) THEN          !- Mohr-Coulomb No-Tension
        CALL GET_F_MC (PHI,C,SIGM,DSBAR,THETA,F, F_MSS)
!       CALL GET_F_MC (90.,0.,SIGM,DSBAR,THETA,F_nt, F_MSS)
      ELSEIF (IOP_VM.EQ.2) THEN          !- Von Mise: plane-strain
        F      =DSBAR - C*SQRT(3.)   
        F_MSS = DSBAR /(C*SQRT(3.))

      ELSEIF (IOP_VM.EQ.3) THEN          !- Von Mise: triaxial
        F     = DSBAR - C*2.  
        F_MSS = DSBAR /(C*2.)

      ELSEIF (IOP_VM.EQ.4) THEN          !- Drucker-Prager
        CALL GET_F_DP (PHI,C,SIGM,DSBAR,THETA,F, F_MSS)

!     ELSEIF (IOP_VM.EQ.10) THEN         !- Monot: has F too

      ELSE                               !- other yield surfaces ?

      ENDIF                     
      END

!-----------------------------------------------------------------------
      SUBROUTINE GET_F_MC (PHI,C,SIGM,DSBAR,THETA,F, F_MSS)
!
!        (was MOCOUF)
!     This calculates the yield function for a Mohr-Coulomb material 
!      cf Von-Mise which is much simpler
!            (phi in degrees, theta in radians)
!       --- ie. < 0. is not failed; > 0. is ??
!     --> 16-6-94 also returns F_MSS = 'Mobilsed Shear-strength Factor'
!
      PHIR = PHI*4.*ATAN(1.)/180.
      SNPH = SIN (PHIR)
      CSPH = COS (PHIR)
      SNTH = SIN (THETA)
      CSTH = COS (THETA)
      A = DSBAR*(CSTH/SQRT(3.)-SNTH*SNPH/3.)    ! stress*cone geometry 
      B = SNPH*(-SIGM) +   C*CSPH               ! v. strength
      F = A - B
      if (abs(b).gt.1.e-12)      ! be careful of 's1=s2=s3=0 when c=0'
     &F_MSS = A / B
      RETURN
      END

!-----------------------------------------------------------------------
      SUBROUTINE GET_F_DP (PHI,C,SIGM,DSBAR,THETA,F, F_MSS)
!
!        (was GET_F_MC)
!     This calculates the yield function  for a Drucker-Prager material 
!            (phi in degrees, theta in radians)
!       --- ie. < 0. is not failed; > 0. is ??
!     .. inspired from B+G (1992)
!         DJK  15-11-96
!
!      PHIR = PHI*4.*ATAN(1.)/180.
!      SNPH = SIN (PHIR)
!      CSPH = COS (PHIR)
!      SNTH = SIN (THETA)
!      CSTH = COS (THETA)
!      A = DSBAR*(CSTH/SQRT(3.)-SNTH*SNPH/3.)
!      B = -SNPH*SIGM +   C*CSPH
!      F = A - B
!      F_MSS = A / B              ! be careful of 's1=s2=s3=0 when c=0'

!-----------------------------------------------------------------------
!     DRUCKER PRAGER (OUTSCRIBING CIRCLE)
!-----------------------------------------------------------------------
!c     C = COS(PHI)
!      S = SIN(PHI)
!      ALPHA = 2.*S/(SQRT(3.)*(3.-S))
!      C1 = -3.*ALPHA
!          ==> -6.*sin(phi)    / (sqrt3* (3-sin(phi)))
!      C2 = 1.
!      C3 = 0.

!-----------------------------------------------------------------------
!     MOHR COULOMB
!-----------------------------------------------------------------------
!      S = SIN(PHI)
!      SRT3 = SQRT(3.)
!      C1 = -S
!      C2 = COS(TH)*(1.+TAN(TH)*TAN(3.*TH)-(S/SRT3)*(TAN(3.*TH)-TAN(TH)))
!      C3 = (SRT3*SIN(TH)-COS(TH)*S)/(2.*SBAR*SBAR*COS(3.*TH))

      RETURN
      END

!-----------------------------------------------------------------------
!     Plastic DEE matrices for Initial Stress
!-----------------------------------------------------------------------
      SUBROUTINE VMPL (E,V,STRESS,PL,IPL)
!
!     This forms the plastic DEE matrix for a Von-Mises material in 2D
!     .. need a general 2D/3D version, so pass NODOF and call MYERROR
!
      REAL STRESS (*)           !- input stress componenets
     &        ,PL (IPL,IPL)     !- output Plastic DEE matrix
     &        ,E,v              !- input elastic parameters
      REAL TERM(4),SX,SY,SZ,TXY,TYZ,TZX,DSBAR,EE    !- workspaces
      INTEGER I,J
      IH = 4

!.. maybe can put this into a subroutine = get/put stress terms
!.. alternatively just leave in place (call S() for briefness)
!   giving:
!     DSBAR = 1./SQRT(2). * SQRT (
!    &       (S(1)-S(2))**2 + (S(2)-S(3))**2  +(S(3)-S(1))**2
!    &  +6.* (S(4)**2       + S(5)**2         + S(6)**2 ) )
!.. in F90 we could be super-clever as in SUM(S(4:6)**2) ?? !

      SX = STRESS(1)
      SY = STRESS(2)
      IF (IH.eq.4) THEN
        TXY= STRESS(3)
        SZ = STRESS(4)
        TYZ= 0.
        TZX= 0.
      ELSEIF (IH.eq.6) THEN
        SZ = STRESS(3)
        TXY= STRESS(4)
        TYZ= STRESS(4)
        TZX= STRESS(4)
      ELSE 
!       .. error message here
      ENDIF

      DSBAR = SQRT((SX-SY)**2+(SY-SZ)**2+(SZ-SX)**2+6.*TXY**2)/SQRT(2.)
!     best to do SIGM too ?
      EE = 1.5*E/((1.+V)*DSBAR*DSBAR)  !- cf DQ1,2,3 = 0.,root3*(2dsbar).0.
      TERM(1) = (2.*SX-SY-SZ)/3.       ! ie = SX- SIGM 
      TERM(2) = (2.*SY-SZ-SX)/3.
      TERM(3) = TXY                    !- note 3D terms
      TERM(4) = (2.*SZ-SX-SY)/3.

!.... in F90 the inner loop becomes PL(:,J) = TERM(:) * TERM(J)*EE
!.... we could even make TERM = TERM / SQRT(EE) for super-speed :-)
      DO I = 1,IH                       !=IH really
        DO J = I,IH
          PL(I,J) = TERM(I)*TERM(J)*EE
          PL(J,I) = PL(I,J)            !- symmetry (why bother!)
        ENDDO
      ENDDO
      RETURN
      END

!-----------------------------------------------------------------------
      SUBROUTINE MOCOPL (PHI,PSI,E,V, STRESS, PL,IPL)
!
!     This forms the plastic stress/strain matrix (DEE) *in 2D*
!     for a Mohr-Coulomb material  (phi,psi in degrees)
!     This is used by Initial Stress algorithms (not visco-p)
!
!     .. need a general 2D/3D version, so pass NODOF and call MYERROR
!     hmm 3D is fairly easy as 'uncoupled' :
!     just abstract more of the stresses and loop more of ROW and COL
!
      REAL STRESS(*)            !- input stress
     &   ,PL(IPL,IPL)           !- output plastic DEE matrix
     &        ,E,v              !- input elastic parameters
     &   ,ROW(6) ,COL(6)        !- workspace (cf VM where ROW==COL

      SX = STRESS(1)
      SY = STRESS(2)
      TXY= STRESS(3)            ! TZX,TYZ too in 3D
      SZ = STRESS(4)

      PI = 4.*ATAN(1.)          ! cf RTD or even 'internal' RADIAN values
      PHIR = PHI*PI/180.
      PSIR = PSI*PI/180.
      SNPH = SIN(PHIR)
      SNPS = SIN(PSIR)
      SQ3 = SQRT(3.)
      CC = 1.-2.*V
      DX = (2.*SX-SY-SZ)/3.            !- cf VMPL and DTPARD
      DY = (2.*SY-SZ-SX)/3.            !- really = SX- I1/3. (SIGM?)
      DZ = (2.*SZ-SX-SY)/3.
      D2 = SQRT (-DX*DY -DY*DZ -DZ*DX +TXY*TXY)     !- +3d shears
      D3 = DX*DY*DZ - DZ*TXY*TXY    !D3==J3?        !- + 2 more terms in 3D
      TH = MIN (MAX (-1.,-3.*SQ3*D3/(2.*D2**3)),1. )   !- clip +/-30deg?

!      IF (TH.GT. 1.) TH = 1.   !- old style
!      IF (TH.LT.-1.) TH = -1.
!      TH = ASIN(TH)/3.         !- cf DTPARD which uses ATAN2(,)

      SNTH = SIN(ASIN(TH)/3.)
      IF (ABS(SNTH).GT..49) THEN     !-- close to the 'Kink'
        SIG = -1.
        IF (SNTH.LT.0.) SIG = 1.
        RPH = SNPH*(1.+V)/3.
        RPS = SNPS*(1.+V)/3.
        CPS = .25*SQ3/D2*(1.+SIG*SNPS/3.)
        CPH = .25*SQ3/D2*(1.+SIG*SNPH/3.)
        COL(1) = RPH+CPH*((1.-V)*DX+V*(DY+DZ))
        COL(2) = RPH+CPH*((1.-V)*DY+V*(DZ+DX))
!       COL(3) = RPH+CPH*((1.-V)*DZ+V*(DX+DY))
        COL(3) = CPH*CC*TXY
!       COL(5) = CPH*CC*TYZ
!       COL(6) = CPH*CC*TZX
        COL(4) = RPH+CPH*((1.-V)*DZ+V*(DX+DY))

        ROW(1) = RPS+CPS*((1.-V)*DX+V*(DY+DZ))
        ROW(2) = RPS+CPS*((1.-V)*DY+V*(DZ+DX))
        ROW(3) = CPS*CC*TXY
        ROW(4) = RPS+CPS*((1.-V)*DZ+V*(DX+DY))
        EE = E/((1.+V)*CC*(RPH*SNPS+2.*CPH*CPS*D2*D2*CC))
      ELSE                       !------ 'normal' state of affairs
        ALP = ATAN(ABS((SX-SY)/(2.*TXY)))   ! so what is à? =p/q??
        CA = COS(ALP)
        SA = SIN(ALP)
        DD = CC*SA
        S1 = 1.                      ! == SIGN(1,SX-SY)
        S2 = 1.                      ! == SIGN(1, TXY )
        IF ((SX-SY).LT.0.) S1 = -1.
        IF ( TXY.   LT.0.) S2 = -1.
        COL(1) = SNPH +S1*DD         !(DD==CC*SA)
        COL(2) = SNPH -S1*DD
        COL(3) =       S2*CC*CA      !- 3D has 3 of there S2's
        COL(4) = SNPH *2.*V          ! note CC is almost a common factor
        ROW(1) = SNPS +S1*DD         
        ROW(2) = SNPS -S1*DD         ! ROW==COL but use PSI not PHI
        ROW(3) =       S2*CC*CA
        ROW(4) = SNPS *2.*V
        EE = E/(2.*(1.+V)*CC*(SNPH*SNPS+CC))
      END IF
      DO I = 1,4               ! = 6 in 3D
        DO J = 1,4
          PL(I,J) = EE*ROW(I)*COL(J)
        ENDDO
      ENDDO
      RETURN
      END

!-----------------------------------------------------------------------
!     Plastic Flow matrices for Viscoplasticity
!-----------------------------------------------------------------------
      SUBROUTINE GET_DQ_DT (SIGM,DSBAR,THETA ,E,V,C,PHI,PSI
     &                      ,DQ1,DQ2,DQ3,DT ,IOP_VM)
!
!    This calculates the 'flow' params  (viscoplastic material)
!     if IOP_VM = 1  : for Mohr-Coulomb
!     if IOP_VM = 2  : for Von Mises' model
!
!     (isn't DSBAR,THETA a f(STRESS) ? .. yes but we already have them at hand
!     .. maybe we should do DQ1,DQ2,DQ3,DT outside
!     .. so ViscoPl calls a pair:  get_DT_DQs, and GET_FLOW
!
!     REAL

      DTR = 3.14159265 / 180.   !-- factor for 'degress to radians'

!----------- Form the DQ1,DQ2,DQ3, DT ----------
!.. if elastic, are DQ1,DQ2,DQ3 all = 0. ?
! 0=elastic, 1=MC,2=vmi.3=vmo?,4=dp,5=mcnt
      IF (IOP_VM.EQ.1) THEN              !- Mohr-Coulomb
        CALL MOCOUQ (PSI,DSBAR,THETA,DQ1,DQ2,DQ3)  
        DT  =  4.*(1.+V)*(1.-2.*V)  / (E*(1.-2.*V+SIN(PHI*DTR)**2 ))
      ELSEIF (IOP_VM.EQ.5) THEN          !- Mohr-Coulomb no-tension
!.. so when we call this routine, sometimes we set psi=phi=90.
        CALL MOCOUQ (PSI,DSBAR,THETA,DQ1,DQ2,DQ3)  
        DT  =  4.*(1.+V)*(1.-2.*V)  / (E*(1.-2.*V+SIN(PHI*DTR)**2 ))
      ELSEIF (IOP_VM.EQ.2.or.IOP_VM.eq.3) THEN      !(arrgh was =1 30-8-95)
!..     maybe should be a subroutine for compatability
        DQ1 = 0.
        DQ2 = 1.5/DSBAR    
        DQ3 = 0.
        DT  =  4.*(1.+V) / (3.*E)  !- Von Mise
      ELSEIF (IOP_VM.EQ.4) THEN      ! Drucker-Prager
        CALL DP_Q (PSI,DSBAR,THETA,DQ1,DQ2,DQ3)  
!??       DT  =  4.*(1.+V)*(1.-2.*V)  / (E*(1.-2.*V+SIN(PHI*DTR)**2 ))
      ELSE
        CALL MYERROR (2,'Unknown Von Mise/Mohr-Coulomb rule')
      ENDIF

!-------------------- Hence form the flow matrix ---------------
!.. beter to call externally
!      CALL GET_FLOW (STRESS,IH, DQ1,DQ2,DQ3, FACT,  FLOW,IFLOW) 
      RETURN
      END

!-----------------------------------------------------------------------
      SUBROUTINE GET_FLOW (STRESS,IH, DQ1,DQ2,DQ3, FACT,  FLOW,IFLOW)
!
!     Forms the MM matrixes hence the FLOW matrix, based on STRESS
!     DQ1,DQ2,DQ3 are the input params
!     FACT = a scalar muliplier (= DT*F is viscoplastic)
!
      IMPLICIT NONE
      INTEGER IH,IFLOW
      REAL   STRESS (IH)          !- input stresses
     &     ,DQ1,DQ2,DQ3           !- input stress derivatives 'DQ's
     &     ,FLOW(IFLOW,IFLOW)     !- output flow matrix
     &     ,FACT                  !- overall scale factor (eg F*DT in Visco-p)

      INTEGER IM,I,J
      PARAMETER (IM = 6)    !- max # of stress terms
      REAL M1(IM,IM), M2(IM,IM), M3(IM,IM)  !-  componenent matrices

      IF (IH.eq.4) THEN
        CALL FORMM  (STRESS,M1,M2,M3,IM)
      ELSEIF (IH.EQ.6) THEN
        CALL FORMM3 (STRESS,M1,M2,M3,IM)
      ELSE
        STOP 'MC_FLOW - NOT 2D OR 3D'       !- how about 1d ??
      ENDIF

      DO J=1,IH
        DO I=1,IH   
!         FLOW(I,J) = F*DT*  (M1(I,J)*DQ1 +M2(I,J)*DQ2 +M3(I,J)*DQ3)
!         FLOW(I,J) = FLOW(I,J) +
          FLOW(I,J) = 
     &      FACT*  (M1(I,J)*DQ1 +M2(I,J)*DQ2 +M3(I,J)*DQ3)
        ENDDO
      ENDDO

      RETURN
      END

!-----------------------------------------------------------------------
!     CONTAINS    !- in F90 GET_FLOW has MOCOUQ,FORM_MM,FORM_MM3 as internals
!.. but MOCOUQ is Mohr-Coulomb specific,
!-----------------------------------------------------------------------
      SUBROUTINE MOCOUQ (PSI,DSBAR,THETA,DQ1,DQ2,DQ3)
!
!     This forms the derivatives of a Mohr-Coulomb potential function 
!     with respect to the three invariants     (psi in degrees) 
!     Daughter of GET_FLOW q.v.
!
      PSIR = PSI*4.*ATAN(1.)/180.
      SNTH = SIN(THETA)
      SNPS = SIN(PSIR)
      SQ3 = SQRT(3.)
      DQ1 = SNPS
      IF (ABS(SNTH).GT..49) THEN       !- close to the 'cusp' ?
        C1 = 1.
        IF (SNTH.LT.0.) C1 = -1.
        DQ2 = (SQ3*.5-C1*SNPS*.5/SQ3)*SQ3*.5/DSBAR
        DQ3 = 0.                                                      

!  cf CRISP :  " MOHR COULOMB (NEAR A SINGULARITY)"
!     dq2 = 0.25      *  (3ñsin(phi))  / DSBAR
!      c2 = 0.5/root3 *  (3ñsin(phi))
!     so c2 is sort-of dq2 * DSBAR
!c     S = SIN(PHI)
!c     SRT3 = SQRT(3.)
!      C1 = -S
!      TERM = 0.5*(3.+S)/SRT3
!      IF(TH.LT.ZERO)TERM = 0.5*(3.-S)/SRT3
!      C2 = TERM
!      C3 = 0.

      ELSE
        CSTH = COS(THETA)
        CS3TH = COS(3.*THETA)
        TN3TH = TAN(3.*THETA)
        TNTH = SNTH/CSTH
!       DQ1 = SIN(PSIR)    !- done above
        DQ2 = SQ3*CSTH/DSBAR*((1.+TNTH*TN3TH) +SNPS*(TN3TH-TNTH)/SQ3)*.5
!       DQ2 = SQ3/2. * (1./dsbar) 
!    &    * CSTH*((1.+TNTH*TN3TH) +SNPS/SQ3*(TN3TH-TNTH))
        DQ3 = 1.5*(SQ3*SNTH+SNPS*CSTH) / (CS3TH*DSBAR*DSBAR)
!       DQ3 = 3.*(SQ3*SNTH+CSTH*SNPS) / (2.*DSBAR*DSBAR*CS3TH)
      ENDIF
! cf CRISP : for "MOHR COULOMB"
!      notes : some signs are reversed .
!            : C2 has no DSBAR in it!
!            : DQ2 is sqrt(3)/2.*(1./DSBAR)*C2 ?
!            : C3 has a '2' on the bot.line so DSBAR = sqrt(2)*SBAR ?
!              a -ve sign on top line, 
!              also DQ3 is 3* C3 !
!         ** why in DQ3 v. C3 is the sign of Cos(theta) reversed?
!         * ans: because theta=+/- pi/6. so cos is always +ve !
!      S = SIN(PHI)
!      SRT3 = SQRT(3.)
!      C1 = -S
!      C2 = COS(TH)*(1.+TAN(TH)*TAN(3.*TH)-(S/SRT3)*(TAN(3.*TH)-TAN(TH)))
!      C3 = (SRT3*SIN(TH)-COS(TH)*S)/(2.*SBAR*SBAR*COS(3.*TH))

      RETURN
      END

!-----------------------------------------------------------------------
      SUBROUTINE DP_Q (PSI,DSBAR,THETA,DQ1,DQ2,DQ3)
!
!     This forms the derivatives of a Drucker-Prager potential function 
!     DRUCKER PRAGER (OUTSCRIBING CIRCLE)
!     with respect to the three invariants     (psi in degrees) 
!     Daughter of GET_FLOW q.v.
!     modifed from MOCOUQ .. inspired by B+G (so be careful of signs etc.)
!
      PSIR = PSI*4.*ATAN(1.)/180.
      SNPS = SIN(PSIR)
      SQ3 = SQRT(3.)
!.. DP is a perfect cone so surface is not a function of THETA
!     SNTH = SIN(THETA)

!.. note from CRISP...
!    for VonMise DQ1..3  = 0., root3, 0.
!    for Tresca  DQ1..3  = 0., 2cosé(1+tanétan3é), root3siní/(sbarýcosé)
!    for DP(out) DQ1..3  = -6/root3*siní/(3-siní), 1. ,0.)      
!    for DP(in)  DQ1..3  = -3*siní/û(9-3sinýí), 1. ,0.)      
! ie. these need muliplying by  1.,dsbar,sigm  or something like that.
!--- outscribing circle ----
      DQ1 = 3. * 2 * snps / (sq3*(3.-snps))
      DQ2 = 1.
      DQ3 = 0.

!--- inscribing circle ----
!      DQ1 = 3. * snps / (sqrt(9.+3.*snps))
!      DQ2 = 1.
!      DQ3 = 0.

!.. now turn these parameters into S+G style
!     I think DQ3==C3 .. and CONS3 is a red-herring (purple-mackarel?)
!.. *but* this still does seem quite right.
!      CONS1 = C1/3.
!      CONS2 = C2/(2.*SBAR)
!      CONS3 = C3*SBAR*SBAR/3.   !(unused)

!-- now build the 'stress'? vector
!    (note ESN = stress-mean_stress, hence SBAR and TH)
!     A(x) = DQ1 + DQ2*sx + C3*sy*sz  
!     A(xy) = DQ2*2Txy + C3*(-2SzTxy)
! .. so is this somehow the same as DQ1*MM1+ etc. ?
!      A(1) = CONS1+ESN(1)*CONS2+CONS3+C3*ESN(2)*ESN(3)
!      A(2) = CONS1+ESN(2)*CONS2+CONS3+C3*ESN(1)*ESN(3)
!      A(3) = CONS1+ESN(3)*CONS2+CONS3+C3*(ESN(1)*ESN(2)-ESN(4)*ESN(4))
!      A(4) = 2.*ESN(4)*CONS2+C3*(-2.*ESN(3)*ESN(4))
!      IF (NDIM.EQ.2) RETURN
!      A(1) = A(1)-C3*ESN(5)*ESN(5)
!      A(2) = A(2)-C3*ESN(6)*ESN(6)
!      A(4) = A(4)+C3*(2.0*ESN(5)*ESN(6))
!      A(5) = 2.0*ESN(5)*CONS2+2.0*C3*(ESN(4)*ESN(6)-ESN(1)*ESN(5))
!      A(6) = 2.*ESN(6)*CONS2+2.0*C3*(ESN(4)*ESN(5)-ESN(2)*ESN(6))
!.. hence can get the 'tangent DEE matrix' ?
! .. CRISP has 2 routines like this .. one returns a 2d DD .. the other 
!     just the raw terms
!     form_elastic_DEE
!     then explicit DBAR =  DEE_terms * A()    {DBAR}=[DEE]{A}
!     then   BBAR = dot_product(DBAR,A)
!     so     DEE = DEE - VVMULT(B,B)/A.B; where B=MVMULT(DEE,A)
!       compare with VMPL ?
      RETURN
      END

!-----------------------------------------------------------------------
      SUBROUTINE FORMM (STRESS,M1,M2,M3,IM)
!
!     This forms the derivatives of the invariants  
!     with respect to the stresses  - 2D
!     in F90  this will be a 'pretty' set of array constructors
!     Daughter of GET_FLOW q.v.
!
      REAL STRESS(*),M1(IM,IM), M2(IM,IM), M3(IM,IM)

      CALL NULL2D (M1,IM,IM,IM)
      CALL NULL2D (M2,IM,IM,IM)
      CALL NULL2D (M3,IM,IM,IM)

      SX = STRESS(1)
      SY = STRESS(2)
      TXY= STRESS(3)
      SZ = STRESS(4)
      DX = (2.*SX-SY-SZ)/3.
      DY = (2.*SY-SZ-SX)/3.
      DZ = (2.*SZ-SX-SY)/3.
      SIGM = ( SX+SY+SZ)/3.

!     M1(:,1) = (/1.,1.,0.,1./)
!     M1(:,2) = (/1.,1.,0.,1./)
!     M1(:,3) = (/0.,0.,0.,0./)
!     M1(:,4) = (/1.,1.,0.,1./)
      M1(1,1) = 1.       !-- hmm could really set all to 1.
      M1(1,2) = 1.       !-- then null the 3rd row & col
      M1(2,1) = 1.
      M1(1,4) = 1.
      M1(4,1) = 1.
      M1(2,2) = 1.
      M1(2,4) = 1.
      M1(4,2) = 1.       !- note: only 7/16 zeros 
      M1(4,4) = 1.
      DO I = 1,4
        DO J = 1,4
          M1(I,J) = M1(I,J)/(9.*SIGM)
        ENDDO
      ENDDO
!     M2(:,1) = (/ 2.,-1.,0.,-1./)
!     M2(:,1) = (/-1., 2.,0.,-1./)
!     M2(:,1) = (/-1.,-1.,6.,-1./)         !(all /3.)
!     M2(:,1) = (/-1.,-1.,0., 2./)
      M2(1,1) = .6666666666666666     !- diagonal    (FORMM3 does /3.
      M2(2,2) = .6666666666666666                 !   at the end :-)
      M2(4,4) = .6666666666666666
      M2(3,3) = 2.                    !- shear term
      M2(2,4) = -.3333333333333333
      M2(4,2) = -.3333333333333333    !- off-diagonals
      M2(1,2) = -.3333333333333333
      M2(2,1) = -.3333333333333333
      M2(1,4) = -.3333333333333333    !- note: only 6/16 zeros !
      M2(4,1) = -.3333333333333333

      M3(1,1) = DX/3.
      M3(2,4) = DX/3.
      M3(4,2) = DX/3.
      M3(2,2) = DY/3.
      M3(1,4) = DY/3.
      M3(4,1) = DY/3.
      M3(4,4) = DZ/3.
      M3(1,2) = DZ/3.
      M3(2,1) = DZ/3.
      M3(3,3) =-DZ
      M3(3,4) = TXY*(-2./3.)            !- bracketsd added 18-3-93
      M3(4,3) = TXY*(-2./3.)
      M3(1,3) = TXY/3.
      M3(3,1) = TXY/3.
      M3(2,3) = TXY/3.                 !- note: only 6/16 zeros
      M3(3,2) = TXY/3.
      RETURN
      END

!-----------------------------------------------------------------------
      SUBROUTINE FORMM3 (STRESS,M1,M2,M3,IM)
!
!     This forms the derivatives of the invariants
!     with respect to the stresses  - 3D
!     Daughter of GET_FLOW q.v.
!
!  ---> OK for 2d/3d... replace '3' with NODOF (careful with axisym)
!
      REAL STRESS(*), M1(IM,IM), M2(IM,IM), M3(IM,IM)

      CALL NULL2D (M1,IM,IM,IM)
      CALL NULL2D (M2,IM,IM,IM)
      CALL NULL2D (M3,IM,IM,IM)

      SX  = STRESS(1)
      SY  = STRESS(2)
      SZ  = STRESS(3)
      TXY = STRESS(4)
      TYZ = STRESS(5)
      TZX = STRESS(6)
      SIGM= (SX+SY+SZ)/3.
      DX = SX-SIGM         !--- hmm only the deviatorics are relevant
      DY = SY-SIGM
      DZ = SZ-SIGM

      DO I = 1,3
        DO J = 1,3         !-- only J=I:3 is needed (symmetry)
          M1(I,J) = 1./(3.*SIGM)   !- direct-stres terms
        ENDDO
      ENDDO
      DO I = 1,3
        M2(I  ,I  ) = 2.           !- direct-stress terms
        M2(I+3,I+3) = 6.           !- shear -stress terms
      ENDDO
      M2(1,2) = -1.                !- off-diagonal terms
      M2(1,3) = -1.
      M2(2,3) = -1.

!     M3(1,:)=(/ DX, DZ, DY,    TXY,-2.*TYZ,    TZX/)
!     M3(2,:)=(/ DZ, DY, DX,    TXY,    TYZ,-2.*TZX/)
!     M3(3,:)=(/ DY, DX, DZ,-2.*TXY,    TYZ,    TZX/)
!     M3(4,:)=(/TZX
!     M3(5,:)=(/
!     M3(6,:)=(/
      M3(1,1) = DX                 !- loop ?
      M3(1,2) = DZ
      M3(1,3) = DY
      M3(1,4) = TXY
      M3(1,5) = -2.*TYZ
      M3(1,6) = TZX                !- or use IF round the 3d bits
      M3(2,2) = DY
      M3(2,3) = DX
      M3(2,4) = TXY
      M3(2,5) = TYZ
      M3(2,6) = -2.*TZX
      M3(3,3) = DZ
      M3(3,4) = -2.*TXY
      M3(3,5) = TYZ
      M3(3,6) = TZX
      M3(4,4) = -3.*DZ
      M3(4,5) =  3.*TZX
      M3(4,6) =  3.*TYZ
      M3(5,5) = -3.*DX
      M3(5,6) =  3.*TXY
      M3(6,6) = -3.*DY
      DO I = 1,6
        DO J = I,6
          M1(I,J) = M1(I,J)/3.
          M1(J,I) = M1(I,J)       !-- symmetry
          M2(I,J) = M2(I,J)/3.    !-- and scaling 
          M2(J,I) = M2(I,J)       
          M3(I,J) = M3(I,J)/3.
          M3(J,I) = M3(I,J)       
        ENDDO
      ENDDO
      RETURN
      END
!-----------------------------------------------------------------------
           

!-----------------------------------------------------------------------
      SUBROUTINE GET_EL_DET (GC,IGC,NDIM,NN,NUMS,INUMS,NEL,IEL,DET)
!
!     This simply returns the DETERMINANT of the JACOBIAN at the
!     centre of an element. (hence can ZAP or flip if DET<=0.)
!     Contrast this with a routine that returns the actual element volume
!         DJK 1994
!     .... note that this module is one of the very few (cf DANBLOCKS)
!          that need Shape functions (shape.f) and Gauss-points (gauss.f)
!
      REAL GC(IGC,*)
      INTEGER NUMS(INUMS,*)

!----------------------- Workspace arrays ------------------------------
      PARAMETER (M_NOD =  32    !-- max # nodes per element
     &          ,M_NODOF= 4     !-- max # freedoms per node
     &          ,ISMP  = 1      !-- max GP's per element
     &                      )
      PARAMETER ( IDER=M_NODOF, IJAC=4, ICOORD=M_NOD)

      REAL    FUN (M_NOD)          !-- shape funs
     &       ,DER (IDER  ,M_NOD)   !-- derivs of shape funs
     &       ,JAC (IJAC,IJAC)      !-- the 'Jacobian'
     &       ,SMP (ISMP,M_NODOF)   !-- Integration sampling points
     &       ,WTS (ISMP)           !-- Integration point 'weights'
      INTEGER NUM (M_NOD)          !-- element node numbers
      REAL  COORD(ICOORD,M_NODOF)


!.. OK could use 'Danplot' area method
!... or I could use Shape funs.. hence.. get DET
      CALL GET_ELEMENT (NUMS,INUMS,IEL, NUM, NOD,NDIME,ITYPE
     &  ,IMAT,IUSER1,IUSER2,IUSER3)
      CALL GET_COORD (NUM,NOD,NDIM,GC,IGC,COORD,ICOORD)    !- coords

      CALL GET_ANY_GAUSS (SMP,ISMP,WTS,NDIME,1,NOD,AREA)
      CALL GSF  (NDIME,NOD,ITYPE, DER,IDER,FUN,SMP,ISMP,1)
!.. hmm maybe do MAT_MULT explicitly as it is the ONLY occurence
      CALL MAT_MULT (DER,IDER,COORD,ICOORD,JAC,IJAC,NDIME,NOD,NDIME)
!     CALL INVERT_JAC (JAC,IJAC,DET,NDIME)
      IF (NDIME.EQ.1) THEN
        DET = JAC(1,1)
      ELSEIF (NDIME.EQ.2) THEN
        DET = JAC(1,1)*JAC(2,2)-JAC(1,2)*JAC(2,1)
      ELSEIF (NDIME.EQ.3) THEN
        DET =  JAC(1,1) * (JAC(2,2)*JAC(3,3)-JAC(3,2)*JAC(2,3))
     &       - JAC(1,2) * (JAC(2,1)*JAC(3,3)-JAC(3,1)*JAC(2,3))
     &       + JAC(1,3) * (JAC(2,1)*JAC(3,2)-JAC(3,1)*JAC(2,2))
      ELSE
        CALL MYERROR (3,'Cannot calc DET for this element')
      ENDIF
!     print*,' det=',det

      RETURN
      END

!-----------------------------------------------------------------------
      SUBROUTINE CALC_IH (NODOF,IH)
!
!     simply returns the 'usual' number of stress/strain terms
!     given the world's IH
!         Dan Kidger  12-5-98
!     Really IH = f(NDIME) rather than the global NODOF ?
!
      IF (NODOF.EQ.1) IH = 1      !
      IF (NODOF.EQ.2) IH = 4      !- include the z term here
      IF (NODOF.EQ.3) IH = 6      ! 
      IF (NODOF.EQ.4) IH = 10     ! 
      return
      end

!-----------------------------------------------------------------------
