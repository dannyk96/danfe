C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C     > > > > > > >     Stiffness Matrix Routines   < < < < < < < < <
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

c -- 1: global matrix assemble --
c     FORM_KM_ANY_GLOBAL : forms any global 'stiffness' matrix
c     FORM_KM_GLOBAL : forms global stiffness matrix
c     FORM_KP_GLOBAL : forms global fluid-flow (laplace) matrix
c     FORM_MM_GLOBAL : forms the global (diagonal) mass matrix

c -- 2: global storage requirements ---
c     GET_NGP_MAX    : returns largest #Gauss point in the mesh
c     GET_KV_STATS   : finds the bandwidth, each freedom needs
c     PRINT_KV_STATS : write memory sizes for Sparin, PCG, etc

c -- 3: gravity, equilbrium, excavation ---
c     APPLY_GRAVITY  : adds loads for the self-weight of the elements
c     EXCAV_FORCES   : forms the equilibrium forces for a removed block
c     APPLY_CONSOL   : initial stress directly from overburden and self-weight

c -- 4: Material non-linearity & stresses ---
c     PLASTICITY     : finds the excess bodyforces due to material yielding
c     PLAST_1EL      : XS forces for one element
c  (   WRITE_STRESSES_OLD : writes the current stress state ) -> moved
c        to ELEMENTS.F  7-12-96)

c -- 5: freedom handling --
c   ( moved to K_BC.F 17-1-98)
c     ON_FREEDOMS    : switches on freedomsat all 'present' nodes
c     CORRECT_NF     : as ON_FREEDOMS but preserves existing 'zeros'
c     SORTNF         : simply resequences non-zero freedoms
c     FORM_G         : returns the 'G' vector for the given element (NUM2G)
c     EXTEND_G_BIOT  : added the '4' BIOT values at the end of G.
c
c  revisions:
c
c

C-----------------------------------------------------------------------
      SUBROUTINE FORM_KM_ANY_GLOBAL
     &     (GC,IGC,NN,NDIM,NUMS,INUMS,NEL,PRPTAB,IPRPTAB
     & ,NF,INF,NODOF,N,ILOADS,KV,IKV,IR,KDIAG,IKDIAG
     & ,GROUPS,IGROUPS ,IOPS,IPR_EL,KM_CODE,DTIM)
c
c... should test to see if the NOD_element is > current maximum handled
C
C     This forms the global stiffness matrix KV
C     from the 'Database' of elements in NUMS and GC
C     (Material props in PRPTAB)
C     IOP_PCG = 0 for SPARIN,
C             = 1 for conj. gradients
C             = 2 for a full NxN matrix (9-6-94)
C
C     17- 6-94 pushed out (NF stuff) and (BW stuff) to sub-routines
c     18- 7-96 overloaded to handle KP,KPI, KE too
c             (note that in general, we might have ALL in the same analysis?

c     Pore pressures and BIOT freedoms:
c       (eq for the 8nq; add a 4nq*1dof = 4 freedoms => IDOF = 20)
c       so FORM_KP too and the 'C' too = VOL*FUN., maybe all in one subr.
c       then assemble twice : one KC 20x20, one just KP 4x4 ?
C       but KD is 20x20 too. (but not used in Gabbie's version)
c
c       so global KP matrix is the same as for flow-nets, but we have
c       a seperate NF (&G). We only matrix*vector it so CG storage is best.
c
c       Each load-step:
C          Abstract fluid 'disps' from DISPS_TOT, *KP =>  'loads'
C          reassemble into the PP column of the applied forces
C          .. and apply every iteration
C          (what about applied pore pressures (eg drawdown)
C          .. But at drainage boundaries, must zap this (real BC's ?)
C          rest is standard, but at convergence, can get GP's PP by FUN*ELD
C          (or get anytime, eg. when post-processing)


C   * Note on Freedoms *
C   The concept of freedoms is ONLY relevant to the matrix solution
C   so this routine must also return the KDIAG pointers to KV and
C   resequence NF (?) to point to these freedoms, so that applied FORCES
C   may be packed into LOADS, and the resulting DISPLACEMENTS extracted
C

      REAL      GC (IGC,NN)     !-- the coordinates of all the nodes
     &     ,PRPTAB (IPRPTAB,*)  !-- the properties of each material type
     &         ,KV (IKV)        !-- the resulting stiffnes matrix

      INTEGER NUMS (INUMS,NEL)  !-- the topology of the elements
     &      ,KDIAG (IKDIAG)     !-- pointers to the rows of KV
     &         ,NF (INF,NN)     !-- node freedom array
     &       ,IOPS (*)          !-- various control options
     &     ,GROUPS (0:IGROUPS)  !-- pointers to KV entrypoints for LEGO
c                                   PCG storage (workspace)
      CHARACTER KM_CODE*4       !-- If KP,KM,KE

C----------------------- Workspace arrays ------------------------------
      PARAMETER (M_NOD =  32    !-- max # nodes per element
     &          ,M_NODOF= 3     !-- max # freedoms per node
     &          ,M_IH   = m_nodof*(m_nodof+1)/2     !-- max # stresses per GP
     &          ,MDOF   = M_NOD*M_NODOF  !-- max # freedoms per element
     &                      )
      PARAMETER (IKM=MDOF  ,ICOORD=M_NOD,IKAY=M_IH) !IDEE=M_IH)

      REAL
     &         KM (IKM,IKM)        !-- element stiffness matrix
     &     ,COORD (ICOORD,M_NODOF) !-- nodal coords
c    &       ,DEE (IDEE,IDEE)      !-- DEE matrix
     &       ,KAY (IKAY,IKAY)      !-- 'DEE' matrix

       INTEGER  G (MDOF)           !-- freedom steering of an element
     &       ,NUM (M_NOD)          !-- node numbers of an element
      INTEGER IPOS                 !-- flag to pass to 'assemble_kv'
      integer hits                 !-- # of times this material has
c                                       appeared in *MAT_PROPS
      REAL KX,KY,KZ
c

C--- an attempt at IMPLICIT NONE for tidyness
      INTEGER NDIME,ITYPE,IMAT,IEL,IDOF,NGP

c-----------------0: control options --------------------
      IOP_PCG = IOPS(3)       ! 0=sparin,1=pcg,2=NxN (-1 = auto?)
      IOP_LEGO= IOPS(11)      ! 1 if NUMS has == elements flagged
      IOP_NGP = IOPS(10)      ! -1=autonatic, else explicit (eg 27)
      IOP_NGP2= IOPS(13)      ! 0 = no SRI, else the K_bit (IOP_NGP=G_bit)
      IAXES   = IOPS(14)      ! 1= usual cartesian, 2=axisym..

c-------------------------0: initilize KV ------------------------------
c  (and sets KDIAG for PCG) .. for upper-tri (iop_pcg=5) sets flag
c  shoudl also re-ALLOCATE (malloc) KV
      CALL INIT_KV (KV,IKV,IR,KDIAG,IKDIAG, N,IOP_PCG)

C------------------1: loop elements & build KM -------------------------
      IPERCENT_OLD = 0
      NEL_used = 0
      IGRP_OLD=0
      DO IEL=1,NEL
c       IF (IPR_EL.GE.5) CALL PR_EL_COUNT ('Elem',IEL,NEL,IPR_EL)
        IPERCENT = nint(100.*REAL(IEL)/REAL(NEL))
        IF (IPR_EL.ge.5 .and.IPERCENT.NE.IPERCENT_OLD)
     &  CALL BAR_LINE (IPERCENT,IPERCENT_OLD, 79)

        CALL GET_ELEMENT (NUMS,INUMS,IEL, NUM, NOD,NDIME,ITYPE
     &      ,IMAT,IGRP,IUSER2,IUSER3)
        IF (IMAT.GT.0) THEN              !- skip 'missing' materials (cf CYCLE)
          NEL_used = NEL_used + 1        !- #elements that are active

c----------- 5: build IDOF, NUM,NUM2 and G & G2 --------------
c note we must build G() *before* we let LEGO only form one matrix
c  if BIOT:  IDOF = NOD*NDIM + nod2*1  = 8*2+4=20 for an 8nq

c          CALL NUM2G (NF,INF,NOD,NUM,NODOF,G)  !- freedoms, return IDOF too?
c          IDOF = NOD * NODOF            !- but BIOT ??
          CALL FORM_G (NF,INF,NOD,NUM,NODOF,G,IDOF)  !- calc freedoms and IDOF
c         IF (BIOT) GET_BIOT_DAUGHTER (NOD,NDIME,ITYPE, NODF, NUM2)
c         IF (BIOT) CALL EXTEND_G_BIOT (NF,INF,NOD,NUM,G,IDOF)

c---- 2: the 'LEGO' algorithm ----
        IPOS = 0               !- default = just 'calc_n_store'
        IF (IOP_LEGO.GE.1) THEN     !- so not-done = 0 so flags 'next-store'
          IPOS = GROUPS(IGRP)
          IF (IOP_PCG.NE.1.and.IOP_PCG.NE.6) THEN
            IF (IEL.EQ.1.OR.IGRP.ne.igrp_old) ipos=0    !- hack off?
          ENDIF
        ENDIF

c-----3: ------------------------------------------------------
        IF (IPOS.EQ.0) THEN           !- a 'new' KM (LEGO)
          IGRP_OLD=IGRP     !- remember for next time?

c----- 4: get nodal coords and Gauss-points ready ------
C  (stresses too ?, for E as f(stress)  )
        CALL GET_COORD (NUM,NOD,NDIM,GC,IGC,COORD,ICOORD)    !- coords
        IF (IOP_NGP.lt.0) THEN          !.. hmm could pass IOP_NGP too?
          CALL GET_DEFAULT_NGP (NOD,NDIME,NGP)
        ELSE
          NGP= IOP_NGP
        ENDIF

        IF (IOP_NGP2.lt.0) THEN  !-- the SRI NGP too
          CALL GET_DEFAULT_NGP (NOD,NDIME,NGP2)
        ELSE
          NGP2= IOP_NGP2
        ENDIF

c-----------------------------------------------------------------------
c    Form each of the KP,KM,KE etc.
c-----------------------------------------------------------------------
c.. where do we calculate the size of 'KM' ??
c.. ok now pick up the icode  =1 for disps, 4+1 for biot, 8 for temp. etc.
c use this as the descriminator


        CALL NULL2D (KM,IKM,IDOF,IDOF)   !- zero the element matrix

c----------- 6: now assemble the element matrix -------------
c     IF (icode.eq.16) THEN
      IF (KM_CODE.EQ.'KP  '.or. KM_CODE.eq.'KPI ') THEN
C-- get material properties, hence form KAY ---
        permx    = PRPTAB (15,IMAT)        !- cf over-writing E,v (,c)
        permy    = PRPTAB (16,IMAT)
        permz    = PRPTAB (17,IMAT)
        IF (KM_CODE.EQ.'KP  ')
     &  CALL FORM_KAY (KAY,IKAY,PERMX,PERMY,PERMZ,NDIME,1)
        IF (KM_CODE.EQ.'KPI ')
     &  CALL FORM_KAY (KAY,IKAY,PERMX,PERMY,PERMZ,NDIME,2)

        CALL NULL2D (KM,IKM,IDOF,IDOF)            !- cf an SRI approach
        CALL FORM_KP (KM,IKM, NDIME,NOD,ITYPE, COORD,ICOORD
     &                  ,KAY,IKAY,NGP, IAXES)

      ELSEIF (KM_CODE.EQ.'KM  ') THEN

C---- get material properties, hence form DEE ----
c... 6-4-95 hmm need to determine *here* if this is plane-strain/plane-stress
c... 0= 1d/pane-strain/3d/4d,  1= plane-stress, 2=axisym x, 3-axisym-y ?

c-- 15-7-96 or extract PERMX,PERMY for fluid-flow, or both for BIOT
c           & hardening params too (MONOT etc.)
c            E may be a f(depth) so one DEE per GP?

c       call get_elastic_params (E,Erate,Ho,v,BW, C,Crate,PHI,PSI)
        E    = PRPTAB (1,IMAT)
        Erate= PRPTAB (26,IMAT)              !- E = Es+Erate*(depth-H0)
        H0   = PRPTAB (14,IMAT)              !-
        V    = PRPTAB (2,IMAT)
        BW   = PRPTAB (6,IMAT)               ! bulk mod. = a bit obsolete ?
        HITS = nint(PRPTAB (21,imat))

        if (E.eq.0) call myerror
     &   (1,'zero Young''s Modulus found')
        if (E.lt.0) call myerror
     &   (1,'negative Young''s Modulus found! ')
c        if (hits.eq.0) call myerror
c    &   (1,'This material has never been defined (FORM_KM_GLOBAL)')
c   < 23-10-97 cf overloading E =f(ITYPE) for Femur models >
c   < 18- 5-98 .. also 'soft-kill' for shape optimisation >

c     SRI :   IOP_NGP can code for this
c             so extract  *two* NGP's   (1000*?) .. hence call FORM_KM twice

c--- and build... -----

c*** for explcit KM's - eg 2nl 2d/3d

      IF (NDIME.eq.1) THEN
        A = PRPTAB(23,IMAT)
        IF (ITYPE.EQ.1) THEN    !- type 1 = rods
          CALL KM_ROD (KM,IKM, COORD,ICOORD, E*A, NDIM)
        ELSEIF (ITYPE.EQ.2) THEN    !- type 2 = beam-columns
c.. we dont know how to handle 3dof in 2d space yet?
          R_I = PRPTAB(24,IMAT)
c         CALL KM_BMCOL2 (KM,IKM, COORD,ICOORD, E*A,E*R_I)
c       ELSEIF (ITYPE.EQ.3) THEN    !- type 3 = plus axial-force
        ENDIF
        GOTO 88   ! cycle
      ENDIF

c.. here we can do the KM: KP: KE split
c.. note that DEE may vary over the element.
c*** if plane-stress there is a different DEE again.
c so 1/ determine code (for DEE) from NGP2, but Plane-stress too.
c       (say if thickness is /=0)
c       call FORM_KM once
C       if code is an SRI one, then call FORM_KM again

        IF (NGP2.eq.0) THEN   !---- standard Integration ----
c         CALL FORM_DEE (DEE,IDEE,E,V,BW,NODOF,1)           ! 'full' DEE
          CALL FORM_KM  (KM,IKM, NDIME,NOD,ITYPE,
     &           COORD,ICOORD, E,v,BW,Erate,H0,1  ,NGP,IAXES )
c         GOTO 88   ! cycle

        ELSE                  !--- use 'Selective Reduced Integration' ---
c            .. this is the K/G split: Lame/G split is DEE:4/5
c          CALL FORM_DEE (DEE,IDEE,E,V,BW,NODOF,3)           ! 'G' bit
c          CALL FORM_KM  (KM,IKM, NDIME,NOD,ITYPE,
c     &                   COORD,ICOORD,DEE,IDEE,NGP,IAXES)
c          CALL FORM_DEE (DEE,IDEE,E,V,BW,NODOF,2)           ! 'K' bit
c          CALL FORM_KM  (KM,IKM, NDIME,NOD,ITYPE,
c     &                   COORD,ICOORD,DEE,IDEE,NGP2,IAXES)
          CALL FORM_KM  (KM,IKM, NDIME,NOD,ITYPE,
     &           COORD,ICOORD, E,v,BW,Erate,H0,3  ,NGP,IAXES )
          CALL FORM_KM  (KM,IKM, NDIME,NOD,ITYPE,
     &           COORD,ICOORD, E,v,BW,Erate,H0,2  ,NGP2,IAXES )
c         GOTO 88   ! cycle
        ENDIF             !- F.I. v. SRI.

c     ELSEIF (ICODE.EQ.1+4) THEN
      ELSEIF (KM_CODE.EQ.'KE  ') THEN
C.. coupled BIOT problems
c.. hmm what about some elements having PP and some not (+beam elements)
        E    = PRPTAB (1,IMAT)
        Erate= PRPTAB (26,IMAT)              !- E = Es+Erate*(depth-H0)
        H0   = PRPTAB (14,IMAT)              !-
        V    = PRPTAB (2,IMAT)
        BW   = PRPTAB (6,IMAT)               ! bulk mod. = a bit obsolete ?
        Kx   = PRPTAB (15,IMAT)        !- cf over-writing E,v (,c)
        Ky   = PRPTAB (16,IMAT)
        Kz   = PRPTAB (17,IMAT)
c       DTIM = 1.
        THETA = 0.5            !- or as a *control option?
        CALL FORM_KAY (KAY,IKAY,KX,KY,KZ,NDIME,1)
c       CALL FORM_DEE (DEE,IDEE,E,V,BW,NODOF,1)           ! 'full' DEE
        CALL FORM_KE  (KM,IKM, NDIME,NOD,ITYPE,
     &         COORD,ICOORD, E,v,BW,Erate,H0,2 ,KAY,IKAY
     &        ,FACTOR,NGP,IAXES)

      ENDIF   !- KP,KM,KE split


   88     CONTINUE          !- jump-to point once we have built KM
        ENDIF             !- LEGO algorithm speed-up hack

c----- and store in global arrays ------
      CALL ASSEMBLE_KV (KV,IKV,IR, KDIAG,IKDIAG,N, KM,IKM,G,IDOF
     &   ,IOP_PCG, ipos)
      IF (IOP_LEGO.GE.1) THEN     !-- store the IPOS for reference
        GROUPS(IGRP) = IPOS      !SPARIN etc. just return '1'  (?)
      ENDIF
c-----------------------------------------------------------------------
        ENDIF   !-- skip of 'missing' elements (cf CYCLE)
      ENDDO   !-- elements ---

C-----------------------------------------------------------------------
      END

C-----------------------------------------------------------------------
      SUBROUTINE FORM_KM_GLOBAL
     &     (GC,IGC,NN,NDIM,NUMS,INUMS,NEL,PRPTAB,IPRPTAB
     & ,NF,INF,NODOF,N,ILOADS,KV,IKV,IR,KDIAG,IKDIAG
     & ,GROUPS,IGROUPS ,IOPS,IPR_EL)

c      *OBSOLETE*
c... should test to see if the NOD_element is > current maximum handled
C
C     This forms the global stiffness matrix KV
C     from the 'Database' of elements in NUMS and GC
C     (Material props in PRPTAB)
C     IOP_PCG = 0 for SPARIN,
C             = 1 for conj. gradients
C             = 2 for a full NxN matrix (9-6-94)
C
C     17-6-94 pushed out (NF stuff) and (BW stuff) to sub-routines

c     Pore pressures and BIOT freedoms:
c       (eq for the 8nq; add a 4nq*1dof = 4 freedoms => IDOF = 20)
c       so FORM_KP too and the 'C' too = VOL*FUN., maybe all in one subr.
c       then assemble twice : one KC 20x20, one just KP 4x4 ?
C       but KD is 20x20 too. (but not used in Gabbie's version)
c
c       so global KP matrix is the same as for flow-nets, but we have
c       a seperate NF (&G). We only matrix*vector it so CG storage is best.
c
c       Each load-step:
C          Abstract fluid 'disps' from DISPS_TOT, *KP =>  'loads'
C          reassemble into the PP column of the applied forces
C          .. and apply every iteration
C          (what about applied pore pressures (eg drawdown)
C          .. But at drainage boundaries, must zap this (real BC's ?)
C          rest is standard, but at convergence, can get GP's PP by FUN*ELD
C          (or get anytime, eg. when post-processing)


C   * Note on Freedoms *
C   The concept of freedoms is ONLY relevant to the matrix solution
C   so this routine must also return the KDIAG pointers to KV and
C   resequence NF (?) to point to these freedoms, so that applied FORCES
C   may be packed into LOADS, and the resulting DISPLACEMENTS extracted
C


      REAL      GC (IGC,NN)     !-- the coordinates of all the nodes
     &     ,PRPTAB (IPRPTAB,*)  !-- the properties of each material type
     &         ,KV (IKV)        !-- the resulting stiffnes matrix

      INTEGER NUMS (INUMS,NEL)  !-- the topology of the elements
     &      ,KDIAG (IKDIAG)     !-- pointers to the rows of KV
     &         ,NF (INF,NN)     !-- node freedom array
     &       ,IOPS (*)          !-- various control options
     &     ,GROUPS (0:IGROUPS)  !-- pointers to KV entrypoints for LEGO
c                                   PCG storage (workspace)
C----------------------- Workspace arrays ------------------------------
      PARAMETER (M_NOD =  32    !-- max # nodes per element
     &          ,M_NODOF= 3     !-- max # freedoms per node
     &          ,M_IH   = m_nodof*(m_nodof+1)/2     !-- max # stresses per GP
     &          ,MDOF   = M_NOD*M_NODOF  !-- max # freedoms per element
     &                      )
      PARAMETER (IKM=MDOF  ,ICOORD=M_NOD, IDEE=M_IH)

      REAL     KM (IKM,IKM)        !-- element stiffness matrix
     &     ,COORD (ICOORD,M_NODOF) !-- nodal coords
     &       ,DEE (IDEE,IDEE)      !-- DEE matrix

       INTEGER  G (MDOF)           !-- freedom steering of an element
     &       ,NUM (M_NOD)          !-- node numbers of an element
      INTEGER IPOS                 !-- flag to pass to 'assemble_kv'
c

C--------- an attempt at IMPLICIT NONE for tidyness -----------
      INTEGER NDIME,ITYPE,IMAT,IEL,IDOF,NGP

c------------------- control options --------------------
      IOP_PCG = IOPS(3)       ! 0=sparin,1=pcg,2=NxN (-1 = auto?)
      IOP_LEGO= IOPS(11)      ! 1 if NUMS has == elements flagged
      IOP_NGP = IOPS(10)      ! -1=autonatic, else explicit (eg 27)
      IOP_NGP2= IOPS(13)      ! 0 = no SRI, else the K_bit (IOP_NGP=G_bit)
      IAXES   = IOPS(14)      ! 1= usual cartesian, 2=axisym..

c--------------------------- initilize KV ------------------------------
c  (and sets KDIAG for PCG) .. for upper-tri (iop_pcg=5) sets flag
      CALL INIT_KV (KV,IKV,IR,KDIAG,IKDIAG, N,IOP_PCG)

C-------------------- loop elements & build KM -------------------------
      NEL_used = 0
      DO IEL=1,NEL
        IF (IPR_EL.GE.5) CALL PR_EL_COUNT ('Elem',IEL,NEL,IPR_EL)
        CALL GET_ELEMENT (NUMS,INUMS,IEL, NUM, NOD,NDIME,ITYPE
     &      ,IMAT,IGRP,IUSER2,IUSER3)
        IF (IMAT.GT.0) THEN              !- skip 'missing' materials (cf CYCLE)
          NEL_used = NEL_used + 1        !- #elements that are active

c          CALL NUM2G (NF,INF,NOD,NUM,NODOF,G)  !- freedoms, return IDOF too?
c          IDOF = NOD * NODOF         !- but BIOT ??

          CALL FORM_G (NF,INF,NOD,NUM,NODOF,G,IDOF)  !- calc freedoms and IDOF
c         IF (BIOT) CALL EXTEND_G_BIOT (NF,INF,NOD,NUM, G,IDOF)

c---- the 'LEGO' algorithm ----
        IF (IOP_LEGO.GE.1) THEN     !- so not-done = 0 so flags 'next-store'
          IPOS = GROUPS(IGRP)
        ELSE
          IPOS = 0               !- default = just 'calc_n_store'
        ENDIF
        IF (IPOS.EQ.0) THEN           !- a 'new' KM (LEGO)

c---- get nodal coords and Gauss-points ready ----
C  (stresses too ?, for E as f(stress)  )
        CALL GET_COORD (NUM,NOD,NDIM,GC,IGC,COORD,ICOORD)    !- coords
        IF (IOP_NGP.lt.0) THEN          !.. hmm could pass IOP_NGP too?
          CALL GET_DEFAULT_NGP (NOD,NDIME,NGP)
        ELSE
          NGP= IOP_NGP
        ENDIF

        IF (IOP_NGP2.lt.0) THEN  !-- teh SRI NGP too
          CALL GET_DEFAULT_NGP (NOD,NDIME,NGP2)
        ELSE
          NGP2= IOP_NGP2
        ENDIF

C---- get material properties, hence form DEE ----
c... 6-4-95 hmm need to determine *here* if this is plane-strain/plane-stress
c... 0= 1d/pane-strain/3d/4d,  1= plane-stress, 2=axisym x, 3-axisym-y ?

c-- 15-7-96 or extract PERMX,PERMY for fluid-flow, or both for BIOT
c           & hardening params too (MONOT etc.)
c            E may be a f(depth) so one DEE per GP?

        E    = PRPTAB (1,IMAT)
        V    = PRPTAB (2,IMAT)
        BW   = PRPTAB (6,IMAT)               ! bulk mod. = a bit obsolete ?

c     SRI :   IOP_NGP can code for this
c             so extract  *two* NGP's   (1000*?) .. hence call FORM_KM twice

c---- zero the element matrix -----
        CALL NULL2D    (KM,IKM,IDOF,IDOF)

c--- and build... -----

c.. here we can do the KM: KP: KE split
        IF (NGP2.eq.0) THEN   !---- standard Integration ----
          CALL FORM_DEE (DEE,IDEE,E,V,BW,NODOF,1)           ! 'full' DEE
c          CALL FORM_KM  (KM,IKM, NDIME,NOD,ITYPE,
c     &                   COORD,ICOORD,DEE,IDEE,NGP,IAXES)

        ELSE                  !--- use 'Selective Reduced Integration' ---
c            .. this is the K/G split: Lame/G split is DEE:4/5
          CALL FORM_DEE (DEE,IDEE,E,V,BW,NODOF,3)           ! 'G' bit
c          CALL FORM_KM  (KM,IKM, NDIME,NOD,ITYPE,
c     &                   COORD,ICOORD,DEE,IDEE,NGP,IAXES)
          CALL FORM_DEE (DEE,IDEE,E,V,BW,NODOF,2)           ! 'K' bit
c          CALL FORM_KM  (KM,IKM, NDIME,NOD,ITYPE,
c     &                   COORD,ICOORD,DEE,IDEE,NGP2,IAXES)
        ENDIF             !- F.I. v. SRI.

        ENDIF             !- LEGO algorithm speed-up hack

c----- and store in global arrays ------
      CALL ASSEMBLE_KV (KV,IKV,IR, KDIAG,IKDIAG,N, KM,IKM,G,IDOF
     &   ,IOP_PCG, ipos)
      IF (IOP_LEGO.GE.1) THEN     !-- store the IPOS for reference
        GROUPS(IGRP) = IPOS      !SPARIN etc. just return '1'  (?)
      ENDIF
c-----------------------------------------------------------------------
        ENDIF   !-- skip of 'missing' elements (cf CYCLE)
      ENDDO   !-- elements ---

C-----------------------------------------------------------------------
      END

C-----------------------------------------------------------------------
      SUBROUTINE FORM_KP_GLOBAL
     &     (GC,IGC,NN,NDIM,NUMS,INUMS,NEL,PRPTAB,IPRPTAB
     & ,NF,INF,NODOF,N,ILOADS,KV,IKV,IR,KDIAG,IKDIAG
     & ,GROUPS,IGROUPS ,IOPS,IOP_K,IPR_EL)

c .. note that is pretry well IDENTICAL to FORM_KV_GLOBAL
c    only NODOF and 'DEE' are different
C
C     This forms the global fluid 'stiffness' matrix KV
C     from the 'Database' of elements in NUMS and GC
C     PRPTAB (1) is Kx
C     PRPTAB (2) is Ky
C
C     IOP_K = 1 for the 'equi_p-KAY', = 2 for flow-KAY (the inverse prob)

      REAL      GC (IGC,NN)     !-- the coordinates of all the nodes
     +     ,PRPTAB (IPRPTAB,*)  !-- the properties of each material type
     +         ,KV (IKV)        !-- the resulting stiffnes matrix

      INTEGER NUMS (INUMS,NEL)  !-- the topology of the elements
     +      ,KDIAG (IKDIAG)     !-- pointers to the rows of KV
     +         ,NF (INF,NN)     !-- node freedom array
     +       ,IOPS (*)          !-- various control options
     +     ,GROUPS (0:IGROUPS)  !-- pointers to KV entrypoints for LEGO
c                                   PCG storage (workspace)

C----------------------- Workspace arrays ------------------------------
      PARAMETER (M_NOD =  32    !-- max # nodes per element
     +          ,M_NODOF= 3     !-- max # freedoms per node
     +          ,M_IH   = m_nodof*(m_nodof+1)/2     !-- max # stresses per GP
     +          ,MDOF   = M_NOD*M_NODOF  !-- max # freedoms per element
     +                      )
      PARAMETER (IKM=MDOF  ,ICOORD=M_NOD, IKAY=M_IH)

      REAL     KM (IKM,IKM)        !-- element stiffness matrix
     +     ,COORD (ICOORD,M_NODOF) !-- nodal coords
     +       ,KAY (IKAY,IKAY)      !-- 'DEE' matrix
     +       ,KX,KY,KZ             !- permeabilities (NOT integers!)
      INTEGER   G (MDOF)           !-- freedom steering of an element
     +       ,NUM (M_NOD)          !-- node numbers of an element
      INTEGER IPOS                 !-- flag to pass to 'assemble_kv'

c----------- control options ----------------------
      IOP_PCG = IOPS(3)       ! 0=sparin,1=pcg,2=NxN (-1 = auto?)
      IOP_LEGO= IOPS(11)      ! 1 if NUMS has == elements flagged
      IOP_NGP = IOPS(10)      ! -1=autonatic, else explicit (eg 27)
      IOP_NGP2= IOPS(13)      ! 0 = no SRI, else the K_bit (IOP_NGP=G_bit)
      IAXES   = IOPS(14)      ! 1= usual cartesian, 2=axisym..
c... init GROUPS ??

c--------------------------- initilize KV ------------------------------
c  (and sets KDAG for PCG) .. for upper-tri (iop_pcg=5) sets flag
      CALL INIT_KV (KV,IKV,IR,KDIAG,IKDIAG, N,IOP_PCG)

C-------------------- loop elements & build KM -------------------------
      NEL_used = 0
      DO IEL=1,NEL
        IF (IPR_EL.GE.5) CALL PR_EL_COUNT ('Elem',IEL,NEL,IPR_EL)
        CALL GET_ELEMENT (NUMS,INUMS,IEL, NUM, NOD,NDIME,ITYPE
     &      ,IMAT,IGRP,IUSER2,IUSER3)
        IF (IMAT.GT.0) THEN              !- skip 'missing' materials (cf CYCLE)
          NEL_used = NEL_used + 1        !- #elements that are active
c          CALL NUM2G (NF,INF,NOD,NUM,NODOF,G)       !- freedoms
          IDOF = NOD * NODOF
c.. how come I need IDOF

c.. hmm is LEGO *always* only a sub-option of PCG ?
          IF (IOP_LEGO.GE.1) THEN     !- so not-done = 0 so flags 'next-store'
            IPOS = GROUPS(IGRP)
          ELSE
            IPOS = 0               !- default = just 'calc_n_store'
          ENDIF

        IF (IPOS.EQ.0) THEN           !- a 'new' KM (LEG0)

        CALL GET_COORD (NUM,NOD,NDIM,GC,IGC,COORD,ICOORD)    !- coords
c        CALL NUM2G     (NF,INF,NOD,NUM,NODOF,G)              !- freedoms
c        IDOF = NOD * NODOF  !- no already got G and IDOF

        IF (IOP_NGP.lt.0) THEN
          CALL GET_DEFAULT_NGP (NOD,NDIME,NGP)
        ELSE
          NGP= IOP_NGP
        ENDIF

C----------- get material properties, hence form KAY -------------------
c.. 29-9-95 Kx, Ky,Kz moved
c        Kx    = PRPTAB (1,IMAT)
c        Ky    = PRPTAB (2,IMAT)
        Kx    = PRPTAB (15,IMAT)        !- cf over-writing E,v (,c)
        Ky    = PRPTAB (16,IMAT)
        Kz    = PRPTAB (17,IMAT)
c       Kz    = 0.     ! (dummy)
        CALL FORM_KAY (KAY,IKAY,KX,KY,KZ,NDIME,IOP_K)

          CALL NULL2D (KM,IKM,IDOF,IDOF)            !- cf an SRI approach
          CALL FORM_KP (KM,IKM, NDIME,NOD,ITYPE, COORD,ICOORD
     &                  ,KAY,IKAY,NGP, IAXES)
        ENDIF             !- LEGO algorithm speed-up hack

c--------------- store in the 'global stiffnes matrix ------------------
      CALL ASSEMBLE_KV (KV,IKV,IR, KDIAG,IKDIAG,N, KM,IKM,G,IDOF
     &   ,IOP_PCG, ipos)
      IF (IOP_LEGO.GE.1) THEN     !-- store the IPOS for reference
        GROUPS(IGRP) = IPOS      !SPARIN etc. just return '1'
      ENDIF
c-----------------------------------------------------------------------
        ENDIF   !-- skip of 'missing' elements
      ENDDO   !-- elements ---

      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE FORM_MM_GLOBAL
     &     (GC,IGC,NN,NDIM,NUMS,INUMS,NEL,PRPTAB,IPRPTAB
     &         ,NF,INF,NODOF,N,MB,IMB,IR_MB,IOPS, IPR_EL)
c
C
C     This forms the global consistent mass matrix MM  <-NO "Lumped"
C     from the 'Database' of elements in NUMS and GC
C     (Material props in PRPTAB)
C     IOP_PCG must = 2 (full NxN) I guess
C         DJK c. 1995
C
      REAL      GC (IGC,NN)     !-- the coordinates of all the nodes
     &     ,PRPTAB (IPRPTAB,*)  !-- the properties of each material type
     &         ,MB (IMB)        !-- the resulting global mass matrix


      INTEGER NUMS (INUMS,NEL)  !-- the topology of the elements
     &         ,NF (INF,NN)     !-- node freedom array
     &       ,IOPS (*)          !-- various control options
C----------------------- Workspace arrays ------------------------------
      PARAMETER (M_NOD =  32    !-- max # nodes per element
     &          ,M_NODOF= 3     !-- max # freedoms per node
     &          ,MDOF   = M_NOD*M_NODOF  !-- max # freedoms per element
     &                      )
      PARAMETER (IMM=MDOF  ,ICOORD=M_NOD)

      REAL     MM (IMM,IMM)        !-- element stiffness matrix
     &     ,COORD (ICOORD,M_NODOF) !-- nodal coords

       INTEGER  G (MDOF)           !-- freedom steering of an element
     &       ,NUM (M_NOD)          !-- node numbers of an element

C-----------------------------------------------------------------------
c     IOP_PCG = IOPS(3)       ! 0=sparin,1=pcg,2=NxN
c     IOP_1EL = IOPS(9)       ! assume all Els are the same :-)
      IOP_NGP = IOPS(10)
      IOP_LUMP= IOPS(16)      ! 0 = lumped mass (Hinton), 1=simplex, (2=full)

C-------------------- loop elements & build MM -------------------------

      IF (IOP_LUMP.EQ.0.or.IOP_LUMP.EQ.1) THEN
        IR_MB = N                   !- The used-size of the mass matrix
      ELSE       !.. eg. a consistent mass CHOLIN-style in BK10_3
        CALL MYERROR (2,'Unknown Lumped-Mass method')
      ENDIF
c     if (allocated(mb)) deallocate (mb)
c     allocate (mb,ir_mb)

      CALL NULVEC (MB,IR_MB)      !- zap the mass-matrix vector

      IPERCENT_OLD = 0
      NEL_used = 0
!     IGRP_OLD=0
      DO IEL=1,NEL
c       IF (IPR_EL.GE.5) CALL PR_EL_COUNT ('Elem',IEL,NEL,IPR_EL)
        IPERCENT = nint(100.*REAL(IEL)/REAL(NEL))
        IF (IPR_EL.ge.5 .and.IPERCENT.NE.IPERCENT_OLD)
     &  CALL BAR_LINE (IPERCENT,IPERCENT_OLD, 79)

        CALL GET_ELEMENT (NUMS,INUMS,IEL, NUM, NOD,NDIME,ITYPE
     &      ,IMAT,IUSER1,IUSER2,IUSER3)
        IF (IMAT.GT.0) THEN                !- skip 'missing' materials
          NEL_used = NEL_used + 1        !- #elements that are active

        CALL GET_COORD (NUM,NOD,NDIM,GC,IGC,COORD,ICOORD)    !- coords
c       IDOF = NOD * NODOF
c       call calc_idof (itype, (imat), idof)
        IDOF = NOD * NDIM                            ! be careful!
c       CALL NUM2G     (NF,INF,NOD,NUM,NODOF,G)              !- freedoms
        CALL FORM_G    (NF,INF,NOD,NUM,NODOF, G,IDOF)        !- freedoms &IDOF
c.. do we have a 'mass-matrix' for the fluid freedoms ?

c       rho  = PRPTAB (10,IMAT)        !- check this value (10)!
        rho  = PRPTAB (12,IMAT)        !- valid if we use 'RHO=123.'

c.. maybe I want a different order of Gauss-rule ..eg Nodal Integration ?
        IF (IOP_NGP.lt.0) THEN
          CALL GET_DEFAULT_NGP (NOD,NDIME,NGP)
        ELSE
          NGP= IOP_NGP
        ENDIF
c.. IOP_1EL killed .. cf a LEGO type approach
c         IF (.not.(IOP_1EL.EQ.1.AND.IEL.GT.1)) THEN   !- skip-option
      CALL NULL2D  (MM,IMM,IDOF,IDOF)    !- cf an SRI approach
      if (ndime.eq.1) then           !- rods and beams
c .. weight=ELL*area*rho, hence 1/2 at each node?
        CALL GET_LINE_DCS (COORD,ICOORD, ndim, ELL, CX,CY,CZ)
        area  = PRPTAB (23,IMAT)
        mm(1,1) = ELL*area*rho /2.
        mm(2,2) = ELL*area*rho /2.
      else
      CALL FORM_MM (MM,IMM, NDIME,NOD,ITYPE, COORD,ICOORD,RHO,NGP)
      endif
c         ENDIF     !- hack for speed-up

c-------- now 'lump' the mass matrix -------------
c.. eventually we will have an option to keep a full (banded) mass-matrix
      IF (IOP_LUMP.EQ.0) THEN
        CALL LUMP_MM_SPECIAL (MM,IMM,IDOF)    !- Hinton's method
      ELSEIF (IOP_LUMP.EQ.1) THEN
        CALL LUMP_MM_ROW (MM,IMM,IDOF)        !- simplex
      ELSE
        CALL MYERROR (2,'Unknown Lumped-Mass method')
      ENDIF

c---- store in the 'global mass-matrix vector -----
c... note the several methods of storage: DIAG, SPARIN, CHOLIN, BANDRD
c.. therefore add 'diag-only' to my set of ASSEMBLE_KV ?
c-- do
        DO I=1,IDOF
          IF (G(I).NE.0) THEN
            MB (G(I)) = MB (G(I)) + MM(I,I)
          ENDIF
        ENDDO

c---> note treatment of MB as if it where a 1d vector /2d array
c     CALL FORMKB (MB,N, MM,IMM, G,IW,IDOF  N,IW, IDOF)

c-----------------------------------------------------------------------
        ENDIF   !-- skip of 'missing' elements
      ENDDO   !-- elements ---

      RETURN
      END

c-----------------------------------------------------------------------
c      SUBROUTINE GET_NF_MAX (NUMS,INUMS,NEL, NEL_USED,NGP_MAX, NODOF)
c
c     This returns NODOF_MAX -the max # of degrees of freeom at any node
c      - hence the 'width' that NF needs to be allocated.
c        Dan Kidger 25-6-97

c     Notes:
c     1. give each freedom a bitmask number:
c          P() is a INTEGER*4 = 32bit number
c          cf ABAQUS's list of I beleive
c           bit0: dx
c           bit1: dy
c           bit2: dz
c           bit3: dw   (for 4d problems)
c           bit4: pp   - pore pressure (head)
c           bit5: T    - temperature
c           bit6:      - Stream-function (+pp for flow-nets)
c           bit7:      -
c
c     2. Stream-func etc. is only known when tell it '*analyse_flowlines'
c      Not when we are doing *NULL_ARRAYS
c
c     3. also we will need *MAT_PROPS before this ?
c      so maybe freedom type is a f(el_code) ? - as is the case in Abaqus.
c
c
c      RETURN
c      END

c-----------------------------------------------------------------------
      SUBROUTINE GET_NGP_MAX (NUMS,INUMS,NEL, NEL_USED,NGP_MAX, IOP_NGP)
c
c     This simply returns the biggest NGP used in the mesh
c     used to dimension the stress arrays appropriately (NEL_USED too)
c     *though really I want to allocate this element by element*
c
c     .. this is also a model for a routine to return the # of different
C     ..  element types and the # of each type
C        (nice for a 'mesh stats table' too)
c
      INTEGER NUMS (INUMS,NEL)  !-- the topology of the elements
      INTEGER  NUM (32)         !-  (be careful of NUM and G sizes)

      NEL_USED = 0
      NGP_MAX = 0
      DO IEL =1,NEL                        !- zap all *present* groups
        CALL GET_ELEMENT (NUMS,INUMS,IEL, NUM, NOD,NDIME,ITYPE
     &      ,IMAT,IUSER1,IUSER2,IUSER3)
        IF (IMAT.GT.0) THEN              !- skip 'missing' materials (cf CYCLE)
          NEL_used = NEL_used + 1        !- #elements that are active
          IF (IOP_NGP.lt.0) THEN
            CALL GET_DEFAULT_NGP (NOD,NDIME,NGP)
          ELSE
            NGP= IOP_NGP
          ENDIF
          NGP_MAX = MAX (NGP_MAX, NGP)
        ENDIF
      ENDDO
      END

c-----------------------------------------------------------------------
      SUBROUTINE GET_KV_STATS (NUMS,INUMS,NEL, NN,NODOF,NF,INF, N,iloads
     &  ,IKV, IR, KDIAG,IKDIAG, IRK
     &   ,GROUPS,IGROUPS,IOP_PCG,IOP_LEGO,ipr)
c
c     This forms the KDIAG pointers (used by SPARIN: if used)
c     and returns the sizes IR (KV) and IRK (KDIAG) that
c     will be needed.
c
c     It also prints a table of memory requirements for various
c     different solvers.
C     .. nice to set up the LEGO algorithm for CG
c     .. nice to auto choose CG if too big for SPARIN
c
      INTEGER NUMS (INUMS,NEL)  !-- the topology of the elements
     &         ,NF (INF,NN)     !-- node freedom array
     &      ,KDIAG (IKDIAG)     !-- pointers to the rows of KV
     &     ,GROUPS (0:IGROUPS)  !-- pointers to KV entrypoints for LEGO
c                                   PCG storage (workspace)

      INTEGER  NUM (99)         !-  (be careful of max NUM and G sizes)
     &          ,G (99)         !

      PARAMETER (nmeth = 6)     !---- statistics table
      REAL TABLE (5,-1:NMETH)    !- 1=IR, 2=IKDIAG,  3=%IR,4=IKDIAG, 5=Mb


C------- count the number of groups ------------
c... really =the max group number.
c.. or try insert_this_IGRP into a list?
      IF (IOP_LEGO.GE.1) THEN
        NGROUPS = 0                          !- zap all *present* groups
        DO IEL =1,NEL
          CALL GET_EL_IMAT (NUMS,INUMS,IEL, IMAT)
          IF (IMAT.GT.0) THEN                !- skip 'missing' materials
            CALL GET_EL_IGRP (NUMS,INUMS,IEL,IGRP)
            NGROUPS = MAX (NGROUPS,IGRP)
          ENDIF
        ENDDO
        GROUPS (0) = NGROUPS     !- so a 'data' structure
      ELSE
        NGROUPS = NEL            !- otherwise treat all as 'different'
      ENDIF

C     IF (ALLOCATED(GROUPS)) DEALLOCATE (GROUPS)
C     ALLOCATE (GROUPS(NGROUPS))
      DO I=1,NGROUPS     !- hence create the empty datastructure
        GROUPS(I) = 0  !flag when this element type has been done
      ENDDO

c----- 0: first check that we have enough workspace -----
C     IF (IOP_PCG.EQ.0.AND.N.GT.IKDIAG)
      IF (N.GT.IKDIAG)
     &   CALL MYERROR (3,' KDIAG is too small')
c     IF (ALLOCATED(KDIAG)) DEALLOCATE (KDIAG)
c     ALLOCATE (KDIAG(n))
      DO I=1,N
        KDIAG(I) = 0   !- count the max bandwidth of each freedom
      ENDDO            !(check that we have enough room first?)

c------- 1: Other initialisation -------
      IR_CG  = 0       !-really have an array (2,5) to hold ALL the counters
      IR_CGT = 0       ! t=triangle storage
      IRK_CG = 0
      DO J=1,5         !- 5 columns per entry
        DO I=0,NMETH        !- assume all = 0
          TABLE(J,I) = 0
        ENDDO
      ENDDO
      TABLE(1,-1)  = IKDIAG     !- put max_vals at the column tops.
      TABLE(2,-1)  = IKV        !- so good for printing
      TABLE(2,1) = 20           !- PCG base storage
      TABLE(2,6) = 20           !- up-tri PCg is the same

C--------------------- Loop and calc. all the sizes --------------------
c .. also calcs the #elements per group
c .. nice to calc the min/max & mean_bw here (not from KDIAG!)

      DO IEL=1,NEL
        CALL GET_ELEMENT (NUMS,INUMS,IEL, NUM, NOD,NDIME,ITYPE
     &    ,IMAT,IUSER1,IUSER2,IUSER3)
!TODO CYCLE
        IF (IMAT.GT.0) THEN                    !- skip 'missing' materials
          CALL GET_EL_IGRP (NUMS,INUMS,IEL,IGRP)

c.. hmm for each 'freedom' I want to know how many nodes this element
c is giving to it: hence IDOF =sum()
C         IDOF = NOD * NODOF     !** hey! watch for BIOT **
c         call get_IDOF (NOD,NDIME,ITYPE, NODOF,IBIOT, IDOF)
          CALL FORM_G (NF,INF,NOD,NUM,NODOF, G,IDOF)

c         BIOT = (ITYPE.eq.5)
c         IF (BIOT) CALL EXTEND_G_BIOT (NF,INF,NOD,NUM, G,IDOF)

          CALL FORM_KDIAG (KDIAG,G,IDOF)             !- for SPARIN etc.
          TABLE(2,1) = TABLE(2,1) + 2 + IDOF         !- (+20 for basis)
          TABLE(2,6) = TABLE(2,6) + 2 + IDOF         !- (+20 for basis)
          IF (IOP_LEGO.eq.0.or.GROUPS(IGRP).eq.0) THEN
            IR_CG  = IR_CG  + idof*idof
            IR_CGT = IR_CGT + (IDOF * (IDOF+1))/2 
!           TABLE(1,1) = TABLE(1,1) + IDOF * IDOF         !- for CG
!           TABLE(1,6) = TABLE(1,6) +(IDOF * (IDOF+1))/2  !- for CG-upper-tri
            IF (IOP_LEGO.ne.0) GROUPS(IGRP) = GROUPS(IGRP)+1  ! flag as done
          ENDIF

        ENDIF  !- skip -ve mats
      ENDDO   !- loop elems

c... note here that *P*CG methods require even more storage
c... so separately compute the Precon's storage ? (or assume =N reals)
c... also big_springs will increase storage (one loc for each)

c.. dont like this ... do ibw_min myself!
      CALL RESEQ_KDIAG (KDIAG,N,IR_SP,IBW_MIN,IBW_MAX)
      IF (N.GT.0) THEN            !- avoid a divide by zero.
        IBW_MEAN = int(REAL(IR_SP) / REAL(N))
      ELSE
        IBW_MEAN = 0
      ENDIF

      IR_BAN = N*(IBW_MAX+1)        !- +1 added 11-3-01
      TABLE(1,0) = IR_SP           ! SPARIN
      TABLE(1,1) = IR_CG           ! PCG
      TABLE(1,2) = N*N             !- for a full NxN matrix
      TABLE(1,3) = IR_BAN          ! BANRED
      TABLE(1,4) = IR_BAN          ! CHOLIN

      TABLE(1,6) = IR_CGT          ! PCG-tri

      TABLE(2,0) = N               ! ikdaig;SPARIN (rest are zero or PCG)

c------ hence get the percent of total space and total Mb of storage
c... note that this assumes REAL*8 and INTEGER*4 !
      DO I=0,NMETH
        TABLE (3,I) = TABLE(1,I) / IKV           ! reals
        TABLE (4,I) = TABLE(2,I) / IKDIAG        ! integers
        TABLE (5,I) = (8*TABLE(1,I) + 4*TABLE(2,I)) / 1024./1024.  ! MB if DP.
      ENDDO

c---------- write statistics table ------------
      IF (IPR.GE.3) CALL PRINT_KV_STATS
     &     (N,ILOADS, TABLE, NMETH, IOP_PCG,
     &      IBW_MIN,IBW_MAX,IBW_MEAN, NEL,NGROUPS ,IPR )

!TODO must allow extra space for any BIG_SPRINGS 
!   this is in kdiag as <+n entries and 1 entry in KV
! note PRECON which is N entries in KV
      IR  = nint(TABLE(1,iop_pcg))       !** return total storage required
      IRK = nint(TABLE(2,iop_pcg))       ! (oops was 1,.. 25-2-96

c---------- check some errors here -----------
      IF (N.eq.0) CALL MYERROR
     &    (1,' There are no freedoms in the mesh!')
      IF (IR.GT.IKV) CALL MYERROR
     &    (1,' The stiffnes matrix is too small')
      IF (IRK.GT.IKDIAG) CALL MYERROR
     &    (1,' KDIAG is too small')
      IF (IBW_MIN.eq.0) CALL MYERROR
     & (3,'A zero bandwidth was found : ie. freedoms with no elements')

c----- reset GROUPS .. better to do every time we build KV
      DO I=1,NGROUPS
        GROUPS(I) = 0
      ENDDO

      RETURN
      END

c-----------------------------------------------------------------------
      SUBROUTINE PRINT_KV_STATS (N,ILOADS, TABLE, NMETH, IOP_PCG,
     &      IBW_MIN,IBW_MAX,IBW_MEAN, NEL,NGROUPS ,IPR)
c
c     Prints the table of memory requirements as calculated by CALC_KV_STATS()
c      ** not yet finished **
c      -- not all the parameters have been passed across.
c
      PARAMETER (nmeth1 = 6)       ! (cos nmeth is an argument)
      REAL TABLE (5,-1:NMETH1)     !- 1=IR, 2=IKDIAG,  3=%IR,4=IKDIAG, 5=Mb

      CHARACTER NAMES (0:NMETH1)*8
!     CHARACTER LL*6
      DATA  NAMES/ 'Sparin  ','PCG     ','Full NxN'
     &            ,'Banred  ','Cholin  ','        ','PCG-tri '/

c---- statement functions ------           ! just for print out. (subr?)
C      S1 (ITYPE) = 100. * ITYPE / IKV              !- statement function
C      S2 (ITYPE) = 100. * ITYPE / IKDIAG           !- statement function
!       LL (i) = char(27)//'[;3'//char(ICHAR('5') -  !- ansi code :-)
!     &          2*sign(1,abs(i-iop_pcg)-1) ) //'m'

      IKDIAG = nint(TABLE(1,-1))     !- get max_vals from the column tops.
      IKV    = nint(TABLE(2,-1))     !-

c----- & #freedoms, #els, #groups ----
        IF (IPR.GE.3)
     &   WRITE(*,'(A,I8,A,I8,A,F6.1,A)')
     &   ' no. of freedoms =',N,'/',ILOADS,
     &   '(',100.*N/REAL(ILOADS) ,'%)'

!---------- element groups (for Lego PCG) -------
        IF (IPR.GE.4)
     &  WRITE (*,'(A,I8,A,I8,A,F6.1,A)')
     &    'NEL =',NEL,' Ngroups =',ngroups,' =',100.*NGROUPS/NEL,'%'

c---------------- bandwidths ----------------
c.. also count the number of non-zero terms in each row ??
c.. and/or express BW as a % of N
      IR_SP =  nint(TABLE(1,0))           ! SPARIN
      IR_BAN = nint(TABLE(1,4))          ! CHOLIN

      IF (IPR.GE.3)
     &WRITE (*,'(6A12/   ,T4, I8,3I12,F12.1,A,F12.1,A)')
     &   'Bwidths : N ','MIN','MAX','MEAN', ' Efficiency ','%N'
     &  ,N, IBW_MIN, IBW_MAX, IBW_MEAN
     &  ,100.*IR_SP/IR_BAN,' %',  100.*IBW_MAX/N,' %'

c--------------- storage requirements ----------------
c . nice to sort in the order of Mb required ?
c     WRITE (*,'(5A10)') '             ','KV','KDIAG', '100%','100%'
c    &     ,'TOTAL'   ,IKV,100.,          IKDIAG,100.
      IF (IPR.GE.3) THEN
        DO I=0,NMETH
c  select which row(s) to print
          IF (I.eq.IOP_PCG.or.IPR.GE.4) THEN
            WRITE (*,'(A, 2F19.0,F19.3 ,A)')
     &      NAMES(I) , (TABLE (J,I),J=1,2),TABLE(5,I)
     &     ,'Mb'
          ENDIF
        ENDDO
      ENDIF

      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE APPLY_GRAVITY (GC,IGC,NN,NDIM,NUMS,INUMS,NEL
     &        ,PRPTAB,IPRPTAB,GRAV_FORCES,IGF,NODOF, JMAT, FAC
     &   ,IPR_EL, iaxes)
C
C     This forms the gavity loading
C     from the 'Database' of elements in NUMS and GC
C     to material JMAT (or wildcard), factored by FAC
C     (Material props in PRPTAB)
C
      INTEGER    NUMS (INUMS,NEL) !-- the topology of the elements
      REAL         GC (IGC,NN)    !-- the coordinates of all the nodes
     &        ,PRPTAB (IPRPTAB,*) !-- the properties of each material type
     &   ,GRAV_FORCES (IGF,*)     !-- the resulting nodal forces

C----------------------- Workspace arrays ------------------------------
      PARAMETER (M_NOD = 32        !-- max # nodes per element
     &          ,M_NDIM= 4 )       !-- max # freedoms per node
      PARAMETER (ICOORD = M_NOD,IGRAVLO = M_NOD)
      REAL
     &      COORD (ICOORD,M_NDIM)  !-- nodal coords
     &    ,GRAVLO (IGRAVLO,M_NDIM) !-- nodal forces
     &       ,RHO (M_NDIM)         !-- Gravity !
      INTEGER NUM (M_NOD)          !-- node numbers of an element

C--------------- loop through the elements ---------------------------
      IPERCENT_OLD = 0
      DO IEL=1,NEL
        CALL GET_EL_IMAT (NUMS,INUMS,IEL, IMAT)
        IF (IMAT.GT.0) THEN                !- skip 'missing' materials
        IF (IMAT.eq.JMAT .or.JMAT.eq.230964) THEN   !- wildcard ?

        IPERCENT = nint(100.*REAL(IEL)/REAL(NEL))
        IF (IPR_EL.ge.5 .and.IPERCENT.NE.IPERCENT_OLD)
     &  CALL BAR_LINE (IPERCENT,IPERCENT_OLD, 79)

        CALL GET_ELEMENT (NUMS,INUMS,IEL, NUM, NOD,NDIME,ITYPE
     &      ,IMAT,IUSER1,IUSER2,IUSER3)
        CALL GET_COORD (NUM,NOD,NDIM,GC,IGC,COORD,ICOORD)    !- coords
        DO J=1,NDIM
          RHO(J)  = PRPTAB (6+J,IMAT)      !- ie columns 7,8,9
        ENDDO
c       iaxes=1    !- 12-11-97 fudge to never axisymmetry !!
c    ... what about beam elements - need to do rho*area?
        CALL APPLY_GRAVITY_1EL (COORD,ICOORD, NOD,NDIM ,NDIME,itype
     &               ,RHO, GRAVLO,IGRAVLO,iaxes)
        DO J=1,NDIM         !-- add into the global array
          DO I=1,NOD          !- cf NODOF (=NDIM+1 if BIOT?)
            GRAV_FORCES (J,NUM(I)) = GRAV_FORCES(J,NUM(I))
     &                   + GRAVLO(I,J)*FAC
          ENDDO
        ENDDO
        ENDIF     !-- only a particular material
        ENDIF   !-- only if element is 'present'
      ENDDO   !-- elements

      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE EXCAV_FORCES (GC,IGC,NN,NDIM,NUMS,INUMS,NEL
     &        ,EVSTRS,IEVSTRS, PORE,IPORE
     &  ,EX_FORCES,IGF,NODOF, JMAT,IPR_EL, IAXES)
C
C   This forms the 'excavation' forces due   ie  F = B å
C   really these are the 'equlibrium' forces eg. for print-out
C
c   14-3-99 for effective stress analyses, we must add the PP here.
c
      INTEGER    NUMS (INUMS,NEL)    !-- the topology of the elements
      REAL         GC (IGC,NN)       !-- the coordinates of all the nodes
     &        ,EVSTRS (IEVSTRS,NEL)  !- total GP stresses
     &          ,PORE (IPORE,NEL)    !- pore pressures (only if BK/=0)
     &     ,EX_FORCES (IGF,*)        !-- the resulting nodal forces

C----------------------- Workspace arrays ------------------------------
      PARAMETER (M_NOD = 32           !-- max # nodes per element
     &          ,M_NDIM= 4 )          !-- max # freedoms per node
      PARAMETER (ICOORD = M_NOD,IEX_LO = M_NOD)
      REAL
     &       COORD (ICOORD,M_NDIM)    !-- nodal coords
     &      ,EX_LO (IEX_LO,M_NDIM)    !-- nodal forces
      INTEGER NUM (M_NOD)             !-- node numbers of an element

C--------------- loop through the elements ---------------------------
      DO IEL=1,NEL
        CALL GET_EL_IMAT (NUMS,INUMS,IEL, IMAT)
        IF (IMAT.GT.0) THEN                !- skip 'missing' materials
        IF (IMAT.eq.JMAT .or.JMAT.eq.230964) THEN   !- wildcard ?
        IF (IPR_EL.GE.5) CALL PR_EL_COUNT ('Elem',IEL,NEL,IPR_EL)
        CALL GET_ELEMENT (NUMS,INUMS,IEL, NUM, NOD,NDIME,ITYPE
     &      ,IMAT,IUSER1,IUSER2,IUSER3)
        CALL GET_COORD (NUM,NOD,NDIM,GC,IGC,COORD,ICOORD)    !- coords
        CALL EXCAV_FORCES_1EL (COORD,ICOORD, NOD,NDIM ,NDIME, ITYPE
     &            ,EVSTRS(1,IEL), PORE(1,IEL), EX_LO,IEX_LO, IAXES)
        DO J=1,NDIM         !-- add into the global array
          DO I=1,NOD
            EX_FORCES(J,NUM(I)) = EX_FORCES(J,NUM(I))+EX_LO (I,J)
          ENDDO
        ENDDO
        ENDIF     !-- only a particular material
        ENDIF   !-- only if element is 'present'
      ENDDO   !-- elements

      RETURN
      END

C-----------------------------------------------------------------------
      FUNCTION PRINT_TIME (TARGET_TIME, DT)
c
c     This returns .true. when the given target_time is reached
c     (and if so, the target_time is updated to the next checkpoint)
c       Dan Kidger 13-9-99
c Notes:
c   1/ On first call set target_time to -1.0 - it will generate a .true.
c      event and set a real time for the next event.
c   2/ have a built-in value of DT (via *DEBUG?)
c      if <0. use ÝDTÝ if <argument (so -999.= always use default)
c      if =0. then always return .true.  'cos
c      if >0. use this value instead of passed argument (so 999. = never?)
c   3/ What if we have no system clock?
c   4/ or.. if DT<0. then use as a 'set_dt' therefore avoid COMMON
c      (note here that if parallel processing then we need to distribute DT)
c
c
      LOGICAL PRINT_TIME
      real target_time, dt, time_now
c     common /times/dt_global/

c      if (dt_globaleq.0) then     !- cookie to always print
c        print_time=.true.         !- cf .0001
c        return   !- not yet
c      endif

      call get_secs (time_now)
      if (time_now.lt.target_time) then
        print_time=.false.
        return   !- not yet
      endif
      print_time=.true.

      if (target_time.lt.0.) then    !- flagged as 'first pass'
        ttarget = time_now+dt          ! initialise
      else
        ttarget = time_now +dt - mod (time_now-target_time,dt)
      endif
      RETURN
      END
C-----------------------------------------------------------------------
      SUBROUTINE APPLY_CONSOL (GC,IGC,NN,NDIM,NUMS,INUMS,NEL
     &         ,PRPTAB,IPRPTAB,EVSTRS,IEVSTRS,NODOF, code,IPR_EL)
C
C     This forms the Initial Stresses in the Material due to both
C     self-weight (GAMA) and overburden (CONS)
C
C     from the 'Database' of elements in NUMS and GC
C     (Material props in PRPTAB)
C

      REAL     GC (IGC,NN)      !-- the coordinates of all the nodes
     &    ,PRPTAB (IPRPTAB,*)   !-- the properties of each material type
     &    ,EVSTRS (IEVSTRS,*)   !-- the Gauss-point stresses
      INTEGER NUMS (INUMS,NEL)  !-- the topology of the elements
C----------------------- Workspace arrays ------------------------------
      PARAMETER (M_NOD = 32     !-- max # nodes per element
     &          ,M_NODOF= 3     !-- max # freedoms per node
     &          ,M_NS   = 6     !-- max # stresses per GP
     &          ,ISMP  = 27 )   !-- max GP's per element
      PARAMETER (IDER=M_NODOF, ICOORD = M_NOD)

      REAL    FUN (M_NOD)          !-- shape funs
     &       ,DER (IDER,M_NOD)     !-- shape fun derivs
     &     ,COORD (ICOORD,M_NODOF) !-- nodal coords
     &       ,SMP (ISMP,M_NODOF)   !-- Integration sampling points
     &       ,WTS (ISMP)           !-- Integration point 'weights'
     &        ,XY (M_NODOF)        !-- coords of a Gauss point
     &    ,STRESS (M_NS)           !-- GP stress

      INTEGER NUM (M_NOD)          !-- node numbers of an element
      CHARACTER CODE*2             !- 'xy' or 'xz'
      logical print_time
C--------------- loop through the elements ---------------------------
      ipercent_old = 0
      target_time=-1.
      DO IEL=1,NEL
        IPERCENT = nint(100.*REAL(IEL)/REAL(NEL))
        IF (IPR_EL.ge.5 .and.IPERCENT.NE.IPERCENT_OLD)  THEN
          IF (print_TIME(TARGET_TIME,0.2))
     &    CALL BAR_LINE (IPERCENT,IPERCENT_OLD, 79)
        ENDIF
        CALL GET_ELEMENT (NUMS,INUMS,IEL, NUM, NOD,NDIME,ITYPE
     &      ,IMAT,IUSER1,IUSER2,IUSER3)

c...... hmm ..need to only do those elements that are 'new' !
c.. take an optional argument jmat ? (=230964 for all elements)
        IF (IMAT.GT.0) THEN                !- skip 'missing' materials

        CALL GET_COORD (NUM,NOD,NDIM,GC,IGC,COORD,ICOORD)    !- coords
        IH = NDIME * (NDIME+1)/2     !- = 1 in 1D, 3 in 2D, 6 in 3D, 10 in 4D
        IF (NDIME.EQ.2) IH = 4       !- include the z term ?
c       .. probally pass Y0 and EPK0 as arguments
c       Y0   = PRPTAB (11,IMAT)      !- 'zero'-height (usu=0.)
        GAMA = PRPTAB ( 8,IMAT)      !- hack out the Y-value  (?)
        CONS = PRPTAB (10,IMAT)      !- the consolidation (2D ?)
        EPK0 = PRPTAB (11,IMAT)      !- coeff. of earth pressure (usu = 1.)
c       EPK0 = 1
        IF (EPK0.LT. .0001) CALL MYERROR (1,'EPK0 is zero !')
C------------------------ loop the G.P.'s -----------------------------
c.. 9-3-98 Won't the next fail for triangles?
c..  < Perhaps Kara first observed this -summer '97 ? >
c       NGP = 2**NDIME        !-- why always R.I. ??
        CALL GET_DEFAULT_NGP (NOD,NDIME,NGP)    ! 9-3-98 (but explicit NGP ?)

        CALL GET_ANY_GAUSS (SMP,ISMP,WTS,NDIME,NGP,NOD,AREA)
        DO IGP=1,NGP
          CALL GSF (NDIME,NOD,ITYPE, DER,IDER,FUN,SMP,ISMP,IGP)
          CALL MTVMULT (COORD,ICOORD,FUN,NDIM,NOD,XY)



c.. 17-9-96 now does Ko times both GAMA *and* CONS.

          if (code.eq.'xy') then
            Sy = (CONS - XY(2) * GAMA)    !- -ve 'cos downwards
            Sx = sy * EPK0
            sz = sy * EPK0

          elseif (code.eq.'xy') then
c.. 27-8-98  What if model is in the xy plane? cf. I. Williams's pile+slot
c  simple: sz=cons; sx=sy=sz*epk0
            Sz = CONS
            Sx = sz * EPK0
            sy = sz * EPK0

          else
            call myerror (3,'Unknown xy/xz plane (APPLY_CONSOL)')
          endif

          STRESS (1) = sx
          STRESS (2) = sy
          STRESS (3) = 0.  !txy
          STRESS (4) = sz
          IF (NODOF.EQ.3) THEN
            STRESS(3) = sz
            STRESS(4) = 0. !txy
            STRESS(5) = 0. !tyz
            STRESS(6) = 0. !tzx
          ENDIF
          DO K=1,IH
            IPT = (IGP-1)*IH + K
            EVSTRS(IPT,IEL) = EVSTRS(IPT,IEL) + STRESS(K)
          ENDDO
        ENDDO      !-- GP's
        ENDIF   !-- only if element is 'present'
      ENDDO   !-- elements

      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE R_MATS_BY_STRESS (IO, GC,IGC,NN,NDIM,NUMS,INUMS,NEL, P
     &              ,PRPTAB,IPRPTAB,EVSTRS,IEVSTRS,NODOF, IPR_EL)
C
C     This can change an elements material number depending upon its
c     vonMise stress level
c         Dan Kidger 9-3-98
C
C     This used for shape optimisation - ie. delete elements whose
c     stress is below a certain threshold so we are left with the
c     minimum weight structure
C  (as inspired by the Swansea paper in Engineering Compuations - March 1998)
C      * note also the soft-kill method whereby E is factored down if
C      the stress is 'low'
c
c   Data:
c     IMAT, IMAT_NEW,  IOP, FACT, RANDFACT
c   Where
c     IMAT     = Material to gnaw at. (230964=wildcard)
c     IMAT_NEW = The new material number to give them (=0 or -ve to zap)
c                =3 say to change to IMAT=3 with a 'pseudo' stiffness.
c     IOP = option:
c            =1  Fact=cutoff fraction of max stress
c            =2  Fact=fraction of total elements to remove
c            =3  Fact=fraction of 'current' elements to remove
c            =4  Fact=explicit no. of elements to remove.
c     FACT = cut-off point as a fraction of the *peak* vonMises stress
c                  typically between 1 and 5 percent (0.01-0.05)
c     RANDFACT = chance of this element being changed,
C                 If <1.00 then we can introduce some 'fuzzyness' into
c                 the process :-)
c  also SOFTKILL= 0 hardkill, 10= divide into 10 levels of E ?
c
c
      REAL     GC (IGC,NN)        !- the coordinates of all the nodes
     &    ,PRPTAB (IPRPTAB,*)     !- the properties of each material type
     &    ,EVSTRS (IEVSTRS,*)     !- the Gauss-point stresses

      INTEGER NUMS (INUMS,NEL)    !- the topology of the elements
     &          ,P (*)            !- pointers for sorting
      PARAMETER (IWORKSPACE= 20000) !- workspace
c     PARAMETER (IWORKSPACE=125000) !- workspace
c      integer :: iworkspace=nel
      REAL VALUES (IWORKSPACE)         !- peak stresses of all elements
      INTEGER  P2 (IWORKSPACE)          !- and their element numbers

C----------------------- Workspace arrays ------------------------------
      PARAMETER (M_NOD = 32       !- max # nodes per element
     &          ,M_NS   = 6 )     !- max # stresses per GP
      REAL STRESS (M_NS)          !- GP stress
      INTEGER NUM (M_NOD)         !- node numbers of an element
      REAL GET_RANDOM
      EXTERNAL GET_RANDOM

C---------------------- 0: Initialise -----------------------------
      if (nel.gt.iworkspace) call myerror(3,
     &    'Workspace too small (R_MATS_BY_STRESS)')

C---------------------- 1: Read paramaters -----------------------------
c  as tokens: set defaults of
C      JMAT=1, IMAT_NEW=0 IOP=3 FACT=0.10 RANDFACT=1.00
c  (count tokens), loop tokens, pick up token and value pair
c    if..elseif each.  (cf enumerated types in *MAT_PROPS)
c
c  Therefore a function: read_next_token
c    has its own 256 byte buffer
c    If list is not empty, pick the next pair (return as pointers ifr:ito?)
c    If list is empty, read next line: check line(1:1) to see if a
c      comment or the next keyword. so return as an 'ifail'.
c
c
      DO ILOOP = 1,9999
    1   READ (IO,*,IOSTAT=IOS) JMAT,IMAT_NEW, IOP,FACT,  RANDFACT
        CALL IN_TEST (IO,IOS,*1,*999)

C----------- 2: Find the vonMises stresses, hence the peak -------------
c     allocate (p2(nel),values(nel))         !- NEL is the max possible
c     allocate (p2(nlist),values(nlist))     !- min size
      Nlist = 0
      DO IEL=1,NEL
        CALL GET_EL_IMAT (NUMS,INUMS,IEL, IMAT)
        IF (IMAT.LE.0) GOTO 11       !- skip 'missing' materials
        IF (IMAT.NE.JMAT.AND.IMAT.NE.230964) GOTO 11  !- wrong material
        CALL GET_ELEMENT (NUMS,INUMS,IEL, NUM, NOD,NDIME,ITYPE
     &      ,IMAT,IUSER1,IUSER2,IUSER3)
        CALL GET_DEFAULT_NGP (NOD,NDIME,NGP)
c       .. realy IH is stored in the data structure
        IH = NDIME * (NDIME+1)/2     !- = 1 in 1D, 3 in 2D, 6 in 3D, 10 in 4D
        IF (NDIME.EQ.2) IH = 4       !- include the z term ?
c       CALL GET_STRESS (STRESSES,IBASE, STR_EL,NGP,IH )
c       CALL GET_STRESS_TERM (STR_EL,NGP,IH,NODOF, C,PHI,IOP_VM
c     & ,GPVALS, CC )

        stressval = 0.       !--- get max vonMise stress of this element ---
        DO IGP=1,NGP
          DO K=1,IH
            IPT = (IGP-1)*IH + K
            stress(k) = EVSTRS(IPT,IEL)
          ENDDO
          CALL INVARIENTS (STRESS,NDIME,SIGM,DSBAR,THETA)
c         stressval = max(stressval,dsbar)         !- max of this element
          stressval = stressval+dsbar/real(ngp)    !- mean of this element
        ENDDO      !-- GP's
        nlist=nlist+1
        p2(nlist)     = iel
        values(nlist) = stressval
   11   CONTINUE      !- cycle-to point
      ENDDO

C----------------------- 3: rank the stresses --------------------------
      CALL INDEX_REALS (values,nlist, P)
      SMIN = values(p(1))
      SMAX = values(p(nlist))
      write(*,'(A,g14.4,a,g14.4,a)')
     & 'vonMise Stress range= (',smin,' ->',smax,' )'

C----------------- 4: switch-off the target elements -------------------
      ic = 0
      DO ILIST=1,NLIST
        IEL = p2(P(ILIST))      !- pick elements in ascending stress order.
        stressval=values(p(ilist))

c-------- Criteria --------
        chance = get_random()*randfact       !--- eg perhaps 80% is good?
        IF (chance.ge.randfact) goto 12

        IF (IOP.EQ.1) THEN          !-- Fraction of max stress --
          IF (stressval.GT.FACT*SMAX) goto 12
        ELSEIF (IOP.EQ.2) THEN      !-- Fraction of total #elements --
          IF (ILIST.GT.FACT*NEL) goto 12
        ELSEIF (IOP.EQ.3) THEN      !-- Fraction of remaining #elements --
          IF (ILIST.GT.FACT*NLIST) goto 12
        ELSEIF (IOP.EQ.4) THEN      !-- Explicit #elements --
          IF (ILIST.GT.nint(FACT)) goto 12
        ENDIF

c--- switch-off this element ---
c.. or soft-kill which factors down 'E'
        ic = ic + 1
        CALL PUT_EL_IMAT (NUMS,INUMS,IEL, IMAT_new)

   12   CONTINUE    !-- cycle-to point
      ENDDO
c     if (ipr.ge.2)
      write(*,'(A,i6,a,i6,a,i6,a, a,i3)')
     & '<>', ic,' out of',nlist, ' (',nel,')',
     & ' elements changed to imat=',imat_new

      ENDDO    !- outer loop
  999 continue
c     deallocate (p2,values)
      RETURN
      END

C-----------------------------------------------------------------------
C     Plasticity routines
C-----------------------------------------------------------------------

C-----------------------------------------------------------------------
      SUBROUTINE PLASTICITY (GC,IGC,NN,NDIM,NUMS,INUMS,NEL
     &    ,PRPTAB,IPRPTAB , IOP_NGP, NODOF
c    &    ,NF,INF,NODOF, DISPS,N     !- old* freedom based style --
     &   ,DISPS_INC,idisps_inc

     &   ,EVSTRS,IEVSTRS, PORE,IPORE, EVPT,IEVPT, BDYLDS_N,IBDYLDS_N
     &   ,CONVERGED, IOP_VM,FOS_C,FOS_PHI
     &   ,ngp_failed, F_MSS_MAX, IPR_EL, IAXES)
C
C     This finds the excess body-forces in a mesh
C      ie. loop elements & Gauss points:
C      disp inc.-> strain inc.-> stress inc. -> tot. stress
C           check stress , if too high :
C    stress->(flow)-> flow strain-> flow stress-> flow forces->bdylds
C         if CONVERGED:
C         can update tot. stresses & do tot stress->react. forces instead
C         if IOP_VM =1 use Mohr-Coulomb; =2 use Von Mise

C     14-6-94 Returns F_MAX, the max 'F' factor
c     15-7-98 need to avoid calls to PLAST_1EL if only seepage
c     17-3-99 added Pore Pressures (only /=0 if BK/=0)

c------------------------ passed arrays --------------------------------
      REAL      GC (IGC,NN)        !- the coordinates of all the nodes
     &     ,PRPTAB (IPRPTAB,*)     !- the properties of each material type

     &  ,DISPS_INC (IDISPS_INC,NN) !- the nodal displacements
     &     ,EVSTRS (IEVSTRS,NEL)   !- total GP stresses
     &       ,PORE (IPORE,NEL)     !- GP pore pressures
     &       ,EVPT (IEVPT,NEL)     !- visocplastic strains (not needed for IS.)
     &   ,BDYLDS_N (IBDYLDS_N,*)   !- output nodal forces (bdylds/reactions)

      INTEGER NUMS (INUMS,NEL)     !- the topology of the elements

      LOGICAL CONVERGED         !- flag if converged (so only update arrays)

C----------------------- Workspace arrays ------------------------------
      PARAMETER (M_NOD =  32          !-- max # nodes per element
     &          ,M_NODOF= 3           !-- max # freedoms per node
     &          ,M_IH   = m_nodof*(m_nodof+1)/2     !-- max # stresses per GP
     &          ,MDOF   = M_NOD*M_NODOF  !-- max # freedoms per element
     &                      )
      PARAMETER (ICOORD=M_NOD)
      INTEGER NUM (M_NOD)             !-- node numbers of an element
      REAL
     &      COORD (ICOORD,M_NODOF)    !- nodal coords
     &       ,ELD (MDOF)              !- displacement increment
c    &        ,XY (5)                 !- coords of a GP.

C--------- an attempt at IMPLICIT NONE for tidyness -----------
      INTEGER NDIME,ITYPE,IMAT,IEL,IDOF,NGP
c     DATA NGP_FAILED/0/
c... if Initial-stress we null BDYLDS everytime
c... so need a three way flag: 0=viscoplastic,1=Init Stress,2=disabled
c.. cf IOP_VM which now becomes a material property:
C          0=MC, 1=VM, 2=Lin.Elastic, 3=Monot!, etc.

      IF (CONVERGED) CALL NULL2D (BDYLDS_N,IBDYLDS_N, NODOF,NN) ! hence reactions
      IF (.not.CONVERGED) F_MSS_MAX = -9.99
      IF (.not.CONVERGED) ngp_failed=0     !- recount each time.

C--------------------- loop through the elements -----------------------
      IPERCENT_OLD = 0
      DO IEL=1,NEL

C       IF (IPR_EL.GE.5) CALL PR_EL_COUNT ('Elem',IEL,NEL,IPR_EL)
        IPERCENT = nint(100.*REAL(IEL)/REAL(NEL))
        IF (IPR_EL.ge.5 .and.IPERCENT.NE.IPERCENT_OLD)
     &  CALL BAR_LINE (IPERCENT,IPERCENT_OLD, 79)

        CALL GET_ELEMENT (NUMS,INUMS,IEL, NUM, NOD,NDIME,ITYPE
     &      ,IMAT,IUSER1,IUSER2,IUSER3)

        IF (IMAT.LE.0) GOTO 88      !-CYCLE  !- skip 'missing' materials

        CALL GET_COORD (NUM,NOD,NDIM,GC,IGC,COORD,ICOORD)    !- coords
        IH = NDIME * (NDIME+1)/2      !- = 1 in 1D, 3 in 2D, 6 in 3D, 10 in 4D
        IF (NDIME.EQ.2) IH = 4        !- include the z term ?

c.. if BIOT (as a Material property), get the daughter element type NODF
c.. so IDOF += NODF

C------------------ abstract the element's displacements ---------------
c    if BIOT, should loop as (1:NOD) *NDIM for DISPS then (1:NODF)
c    ie. if BIOT add (4) more terms to ELD
C    cf. here the re-interpolation of the mid-side nodes
C   .. Hang on folks!, I dont think BIOT comes in here at all!
c     the only thing that happens in Gabbie's code is an interpolation
C     of the inc. nodal PP to the GP's

        DO J=1,NODOF          !- explicit abstraction - bypasses G()
          DO I=1,NOD
            ELD(J+(I-1)*NODOF) = DISPS_INC(J,NUM(I))
          ENDDO
        ENDDO
        IDOF = NOD * NDIM
c.. IDOF is realy the length of ELD; we also have a subset of ELD and
c   IDOF for just the structural freedoms.

       IMODEL=itype        !- is this struct, PP, or BIOT, etc.
       IF (MOD(IMODEL,2).NE.1) GOTO 88   !- No structural freedosm here.

c------ get the No. of Stress sampling (Gauss) points ---
c.. cf later the # of terms we have at each sampling point
c.. do internally?, if IOP_NGP>0 explicit, else auto (SRI = -123 too?)
        IF (IOP_NGP.lt.0) THEN
          CALL GET_DEFAULT_NGP (NOD,NDIME,NGP)
        ELSE
          NGP= IOP_NGP
        ENDIF

C----------- get material properties, hence form DEE -------------------
c.. cf the hybrid SRI approach to stress sampling (Yuk!)
c--> for Monot (cos of the 21 params), nicer to abstract to a COMMON block
c    (in F90 a derived type (via a module) is better)

c.. maybe as a subroutine  GET_PARAMS_MC_VM (PRPTAB -> c,phi, ..)
c.. so follow by FORM_DEE (E,v,BW)  (cf elastic v. tangent)
c.. if MONOT  call GET_PARAMS_MONOT, hence call form_DEE_monot

c < note here, we also pick up a pointer to the list of state variables
c   for this element as NGP records:   (+ any 'whole' element params?)
C    1/ Elastic            : none,  or just stresses
C    2/ Von Mise (+Tresca) : stresses
C    3/ Mohr-Coulomb (+ DP): stresses
C    4/ Monot              : stresses + 2 hard + 1 code
C
C    All may have strains on/off (only affects print-out ?)
C    may also add a pore-pressure
C    If ViscoPl, add (visco)strains
C
c.. cf the # of non-linear params
c.. depends on model, lin-least needs none, VM: just Cu, Monot needs 14+!

        IOP_VM = NINT(PRPTAB(18,IMAT))
c.. maybe turn IOP_VM into a string: 'EL',  'VM','MC','DP', 'MT' etc.
c.. this will greatly aid readability.

c------------------------ Elastic /VM / M-C ----------------------------
c.. realy I want a more powerful GET_PARAM
c  In F90 we can optional arguements to return just those params that we need.
c  - but if C=f(depth) we need to do this for every GP.
c  (should we limit C to a max value?)

        IF (IOP_VM.GE.0.AND.IOP_VM.le.9) THEN    !- VM,TRESCA,MC,DP

c.. we can use the next wherever we watn to (FORM_KM, GRAVITY,etc.)
c.. note if ERATE=CRATE=0. then we only need to call this on the first
c.. pass of a loop_GP structure.
c         CALL GET_EP_PARAMS (PRPTAB,IPRPTAB, E,V,BW, C,PHI,PSI,
C    &       ERATE,CRATE, XY,
C    &       FOS_C, FOS_PHI)
          E0   = PRPTAB (1,IMAT)
          Erate= PRPTAB (26,IMAT)              !- E = Es+Erate*(depth-H0)
          H0   = PRPTAB (14,IMAT)              !-
          V    = PRPTAB (2,IMAT)
          BW   = PRPTAB (6,IMAT)               !    BW = a bit obsolete ?

          C0   = PRPTAB (3,IMAT)
          Crate= PRPTAB(13,imat)               ! C = Cs+Crate*(depth-H0)
          PHI  = PRPTAB (4,IMAT)
          PSI  = PRPTAB (5,IMAT)
c.. (note the next calls GAUSS internally)- if in a GP loop it is explicit
c.. 9-9-97 oops! IGP is undefined here.
c          IGP = 1 !(say)
c          CALL GET_GAUSS_XY (IGP,NGP,COORD,ICOORD, NDIME,NOD,NDIM,XY)
c       ----- patch to make C a f(depth) --------
c          if (crate.ne.0.) then
c            C = C0 + Crate*(h0-XY(2))
c          ENDIF

c       .. if MONOT I guess we can still form a DEE here if we wanted to.
c       .. but in MONOT, DEE=f(stress) and so is different for each G.P.
c       .. even here, as long as we use a tangent approach , E+=f(SIGM)
c         CALL FORM_DEE (DEE,IDEE,E,V,BW,NODOF,1)

c       ----- patch to allow them to be 'factored down' ---
c  (within the GP loop eg. if C=f(depth)?
        IF (IOP_VM.NE.0) THEN         !- no point for elastic!
          DTR  = 3.1415926535 / 180.  !-- factor for 'degress to radians'
          C0   = C0    /FOS_C
          Crate= Crate /FOS_C
          PHI  = ATAN (TAN(PHI*DTR) /FOS_PHI) /DTR
          PSI  = ATAN (TAN(PSI*DTR) /FOS_PHI) /DTR    !- FOS this too ?
        ENDIF

c----------------------------- M o n o t -------------------------------
        ELSEIF (IOP_VM.EQ.10) THEN         !- (use 11 for a varient?)
c         CALL GET_MONOT_PARAMS (,IMAT)
        ELSE
c       .. should crash-out on any errors here
          CALL MYERROR(1,'Unknown plasticity model (PLASTICITY)')
        ENDIF


c-----------------------------------------------
c.. for rod/beam elements - yielding is based on max moment /axial stress
c..so FORM_KM, ACTIONS=KM*ELD,  so check peak moments.
C.. note also the use of Hermite shape functions to interpolate the
C   disps etc.

c.. even if elastic, we still want to update:
c       BDYLDS_N : the nodal reaction forces
c       EVSTRS   : the 'stresses'  - say Sx + bending fiber stress ?
      IF (NDIME.eq.1) THEN
        CALL PLAST_1EL_ROD (COORD,ICOORD, NDIM, ITYPE, ELD,
     &     PRPTAB(1,IMAT),EVSTRS(1,IEL),EVPT(1,IEL), CONVERGED )

        CYCLE ! GOTO 88   ! cycle
      ENDIF

c... now call a handling routine depending on whether:
C     1/ Initial stress v. Visoplasticity
C     2/ Mohr-Coulomb (+VM) v. Monot (v. Cam-Clay)
C
c..note that this routine uses the global strees, viso-strains and bodyloads
c      arrays directly.rather than abstract just one element's worth.
c      .. note parallelism cos each PLAST_1EL should be independant

c.. hmm in general DEE, C/PHI may be different at each Gauss point.
c.. also Dee may be a f(STRESS)

         CALL PLAST_1EL (ELD, NDIME,NOD,ITYPE
c    &         ,COORD,ICOORD,DEE,IDEE, E,V,C,PHI,PSI
     &         ,COORD,ICOORD, E0,V,BW,Erate,H0,1, C0,Crate,PHI,PSI
     &         ,NGP,IAXES, NUM, NDIM, IDOF, NODOF
     &         ,EVSTRS(1,IEL), PORE(1,IEL), EVPT(1,IEL)
     &         ,BDYLDS_N,IBDYLDS_N
     &         ,CONVERGED, ngp_failed1, F_MSS1,IOP_VM   )
        F_MSS_MAX = MAX (F_MSS1,F_MSS_MAX)  !- remember the max 'F' factor
        ngp_failed=ngp_failed+ngp_failed1

  88     CONTINUE          !- jump-to point once we have handled this elem

      ENDDO   !-- elements ---

      RETURN
      END
c-----------------------------------------------------------------------

