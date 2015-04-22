C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C    >>>> 'Self-Contained' routines for reading various mesh file types
C          . Dan Kidger   Started: Sept'93 
c
c   note the various sections:
c     eq. 1/ basic meshes
c         2/ read/write whole meshes
c         3/ material types etc.
c         4/ DANBLOCKS et al
c         5/ morphing (circles and squares etc.)
c         6/ misc (obsolete etc.)
c      *  7/ NF routines
c      *  8/ applied DISPS and LOADS
c
c   * = not present in this module  (see K_BC.F)
c
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
c.. moved QUICKPLOT to here 16-2-94
c.. 9-3-94 added K_MESH and K_MMUNG together to here
c   13-9-96 moved quickplot to K_MESHIO.F with all the other I/O
c   17-5-97 added UNBISECT_ELEMENTS
c   19-8-97 added GREEDY
c    4-9-97 added MESH_QUALITY
c  12-12-97 added *TUNNEL_A_SQUARE for 3-arc tunnels (Baha)
c  22- 2-98 *PROJECT_MESH for 3d->2d etc.
c  13- 3-98 added wr_GROUPS to calc and write inter-connections
c  12- 5-98 a bit on merging 2 meshes.
C-----------------------------------------------------------------------
c
c   what about KEY_MESH_MORPH - those routines that do not change
c   either NN or NEL - eg tunnels, scaling, groups, etc.
C-----------------------------------------------------------------------
      SUBROUTINE KEY_MESH_READ
     &      (FOUND,IPR,KEYWORD,IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL,P)
C
C     A Keyword handler for reading/writing the basic mesh
C     DJK 24-5-93
C     24-8-94   all KEY_MESH_MUNG added too (therefore P too)
C      4-4-95   IPR>=2 for  status output  (1=keywords themselves)
C
      REAL GC (IGC,*)
      INTEGER NUMS(INUMS,*), IO, P(*), IPR
      LOGICAL FOUND   !, OPENED
      CHARACTER KEYWORD*(*) !,FILE*70    !,FILE2*70
      character method*3             !-for smoothing
      DATA IO2/80/                   !- this is for file writes I think
c              .. nice to have a 'get next free unit after IBASE' cf CBS)
      FOUND = .TRUE.

c     print*,'<> keyword =',keyword
c--------------------------- General -----------------------------------
c.... nDIM modules are relevent when READING *NODES, etc.
c... also maybe if 2D and then *THREE_DIM. then :
c      DO I=1,nn, DO j=ndim_old+1, ndim ; GC(J,I) = 0.   (cf DANBLOCKS)
c    (but in DANFE we have NODOF and NDIM as 2 seperate entities)
c    .. really  if DANFE is to mimic PAFEC say, then NODOF is produced
c    once we have a table of elements, 
C      ie. standard x,y. 
C          Add BIOT if any elements have Biot freedoms.
C          Add Rotation if any Beam elements.
C       hence allocate x,y, etc total #=nodof

      IF (KEYWORD.EQ.'*ONE_DIMENSIONAL') THEN
        CALL CHANGE_NDIM (GC,IGC,NDIM,NN,1)       !- updated 8-2-98

      ELSEIF (KEYWORD.EQ.'*ZERO_DIMENSIONAL') THEN
        CALL CHANGE_NDIM (GC,IGC,NDIM,NN,0)          ! wow!

      ELSEIF (KEYWORD.EQ.'*TWO_DIMENSIONAL') THEN
        CALL CHANGE_NDIM (GC,IGC,NDIM,NN,2)

      ELSEIF (KEYWORD.EQ.'*THREE_DIMENSIONAL') THEN
        CALL CHANGE_NDIM (GC,IGC,NDIM,NN,3)

      ELSEIF (KEYWORD.EQ.'*FOUR_DIMENSIONAL') THEN
        CALL CHANGE_NDIM (GC,IGC,NDIM,NN,4)

      ELSEIF (KEYWORD.EQ.'*NEWMESH') THEN        !- reset the mesh
        NN  =0                             !(cf disp, stresses etc.)
        NEL =0
c.. F90 may deallocate the arrays too?
c- what about generic chopping - eg. chop 35271 element to 500 say (or 0)

      ELSEIF (KEYWORD.EQ.'*ONE_ELEMENT') THEN        !- create an element!
        CALL R_ONE_ELEMENT (IO,IPR,GC,IGC,NDIM,NN,NUMS,INUMS,NEL)

C------------------ complete mesh descriptions -------------------------

      ELSEIF (KEYWORD.EQ.'*SUPERBLOCK') THEN    !- simple cuboid
        CALL R_SUPERBLOCK (IO,IPR,GC,IGC,NDIM,NN,NUMS,INUMS,NEL)

      ELSEIF (KEYWORD.EQ.'*SLOPE_MESH') THEN    !- 'old' style
        CALL R_SLOPE_MESH (IO,IPR,GC,IGC,NDIM,NN,NUMS,INUMS,NEL)

      ELSEIF (KEYWORD.EQ.'*ELEMENT_TYPES') THEN   !- 'old' style
        CALL R_GETELT (IO,NUMS,INUMS,NEL,NDIM)

      ELSEIF (KEYWORD.EQ.'*LAYERED_SLOPE') THEN         
        CALL R_LAY_SLOPE (IO,IPR,GC,IGC,NDIM,NN,NUMS,INUMS,NEL)

      ELSEIF (KEYWORD.EQ.'*BASIC_BLOCK') THEN     !- just 1 4nq (any nDIM)      
        CALL R_BASIC_BLOCK (IO,ipr,GC,IGC,NDIM,NN,NUMS,INUMS,NEL)

      ELSEIF (KEYWORD.EQ.'*BASIC_BLOCK_4NQ') THEN       
c        IF (NDIM.EQ.1)
c     &  CALL R_BASIC_1NL (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL)
        IF (NDIM.EQ.2)
     &  CALL R_BASIC_4NQ (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL)
        IF (NDIM.EQ.3)
     &  CALL R_BASIC_8NB (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL)
c        IF (NDIM.EQ.4)
c     &  CALL R_BASIC_16NS (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL)

c----------------- explicit read of NODES and ELEMENTS -----------------
c.. really a part of I/O of meshes 
c.. hmm. how do I *merge* 2 datasets together .. ie. can *NODES
c..   add node *after* NN, and if so how does *ELEMENTS know this offset

      ELSEIF (KEYWORD.EQ.'*NODES') THEN               !- nodal coords
c       ibase=nn
c       if (foo) ibase=0     !- overwrite //append
        CALL R_NODES (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL)

      ELSEIF (KEYWORD.EQ.'*ELEMENTS') THEN            !- elem steering
c       jbase=nel
c       if (foo) jbase=0       !- overwrite // append
c       if (bar) ibase=0       !- NUM()+=ibase
c    realy we only +=base for nodes >=ibase ?? but if 6+6: node 4=??
        CALL R_ELEMENTS (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL)
c 21-9-98 try the next as the default? 
        CALL BUILD_MIDSIDE_NODES (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL,P)

      ELSEIF (KEYWORD.EQ.'*ELEMENTS_2') THEN          !- alternative
c       note here a totaly general approach
        CALL R_ELEMENTS_2 (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL,p)

c------------------- interpolation within elements ---------------------

      ELSEIF (KEYWORD.EQ.'*DANBLOCKS') THEN
        CALL R_DANBLOCKS (IO,IPR,GC,IGC,NDIM,NN,NUMS,INUMS,NEL,P)

      ELSEIF (KEYWORD.EQ.'*DISECT_MESH') THEN   
         call myerror (1,'Please use *BISECT_ELEMENTS instead')
c        call myerror (2,'Please use *QUADRILATERALISE_MESH instead')

      ELSEIF (KEYWORD.EQ.'*QUADRILATERALISE_MESH') THEN     ! 4nq <-> 3nt  :-)
        call myerror (1,'Please use *BISECT_ELEMENTS instead')
c        CALL QUADRILATERALISE_MESH 
c     &       (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL,P)      !(IO is dummy)  

      ELSEIF (KEYWORD.EQ.'*TRIANGULARISE_MESH') THEN !- general disection
        call myerror (1,'Please use *BISECT_ELEMENTS instead')
c        CALL R_TRIANGULARISE_MESH
c     &       (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL,P)

      ELSEIF (KEYWORD.EQ.'*BISECT_ELEMENTS') THEN !*** general disection ***
        CALL R_BISECT_ELEMENTS
     &       (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL,P)


      ELSEIF (KEYWORD.EQ.'*UNBISECT_ELEMENTS') THEN !*** glue together! ***
        CALL R_UNBISECT_ELEMENTS
     &       (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL,P)

      ELSEIF (KEYWORD.EQ.'*BUILD_MIDSIDE_NODES') THEN  !- auto create
        CALL BUILD_MIDSIDE_NODES (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL,P)

C---------------------- removing elements etc. -------------------------

      ELSEIF (KEYWORD.EQ.'*DELETE_ZERO_AREA_ELEMENTS') THEN
        CALL DEL_ZERO_AREA (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL,P)

      ELSEIF (KEYWORD.EQ.'*DELETE_NUL_ELEMENTS') THEN
        CALL DEL_NUL_ELEMENTS (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL,P)

      ELSEIF (KEYWORD.EQ.'*DELETE_ORPHAN_NODES') THEN
        CALL DEL_O_NODES (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL,P)
                                                
      ELSEIF (KEYWORD.EQ.'*MERGE_NODES') THEN
        CALL MERGE_NODES (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL,P)
c       CALL DEL_COIN_NODES (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL,P)
      ELSEIF (KEYWORD.EQ.'*JOIN_COINCIDENT_NODES') THEN
         call myerror (1,'Slow!: Please use *MERGE_NODES in future')
         CALL DEL_COIN_NODES (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL,P)
      ELSEIF (KEYWORD.EQ.'*DELETE_COINCIDENT_NODES') THEN
         call myerror (2,'Please use *JOIN_COINCIDENT_NODES instead')

      ELSEIF (KEYWORD.EQ.'*JOIN_COINCIDENT_NODES_BY_BOX') THEN
        CALL R_DEL_COIN_NODES_BY_BOX 
     &        (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL,P)

c------------------ sorting the nodes/elements -------------------------
c.. sort elements from a point too ?
      ELSEIF (KEYWORD.EQ.'*SORT_NODES_FROM_POINT') THEN
c       CALL SORT_NODES_POINT (IO,GC,IGC,NDIM,NN,P)
        CALL R_SORT_NODES_POINT (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL,P)

      ELSEIF (KEYWORD.EQ.'*SORT_ELEMENTS_FROM_POINT') THEN
        CALL R_SORT_ELEMENTS_POINT (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL,P)

      ELSEIF (KEYWORD.EQ.'*SORT_ELEMENTS_IMAT') THEN
        CALL SORT_ELEMENTS_IMAT (GC,IGC,NDIM,NN,NUMS,INUMS,NEL,P)

       ELSEIF (KEYWORD.EQ.'*SORT_NODES_RANDOM') THEN   ! jumble!
         CALL SORT_NODES_RANDOM (GC,IGC,NDIM,NN,NUMS,INUMS,NEL,P)

       ELSEIF (KEYWORD.EQ.'*Q_ELEMENTS_AT_NODE') THEN
         CALL ELEMS_AT_NODES (NN,NEL,NUMS,INUMS,P, IBW_OPT)
         CALL ELEMS_AT_NODES (NN,NEL,NUMS,INUMS,P, IBW_OPT)

       ELSEIF (KEYWORD.EQ.'*SORT_NODES_CUTHILL') THEN
         CALL CUTHILL (NN,NEL,NUMS,INUMS,P, IBW_OPT)
         CALL UPDATE_GC   (GC,IGC,NDIM,NN, P)
         CALL UPDATE_NUMS (NUMS,INUMS,NEL, P)


c------- Element Groups, LEGO and Message Passing ----------
c  These routines all use the IGRP column of NUMS
c    (split off mostly in an attempt to reduce the size of the Fortran
c    source file?)
c

      ELSEIF (KEYWORD.EQ.'*GROUP_CONNECTIVITY') THEN
c       CALL GEOMETRY_IGRP (GC,IGC,NDIM,NN,NUMS,INUMS,NEL,P)
        CALL GROUP_CONNECTIVITY (GC,IGC,NDIM,NN,NUMS,INUMS,NEL,P)
      ELSEIF (KEYWORD.EQ.'*GROUP_GREEDY') THEN
        CALL R_GROUP_GREEDY (IO,IPR,GC,IGC,NDIM,NN,NUMS,INUMS,NEL,P)

      ELSEIF (KEYWORD.EQ.'*WRITE_GROUPS') THEN    !- write
        CALL wr_GROUPs (IO,GC,IGC,NDIM,NN,NUMS,INUMS, NEL, P)

      ELSEIF (KEYWORD.EQ.'*CONNECTION') THEN      !- and read
        CALL R_CONNECTION (IO,GC,IGC,NDIM,NN,NUMS,INUMS, NEL, P)

      ELSEIF (KEYWORD.EQ.'*SET_LEGO_ALLDIFF') THEN
c        IOPS(11) = 1          ! for testing: result should be == to 
        DO IEL= 1,NEL         ! that without using IGROUP
          CALL PUT_EL_IGRP (NUMS,INUMS,IEL,IEL)
        ENDDO
      ELSEIF (KEYWORD.EQ.'*SET_LEGO_ALLSAME') THEN
c        IOPS(11) = 1          ! as used my MEMSIZ_20, etc.
        DO IEL= 1,NEL
          CALL PUT_EL_IGRP (NUMS,INUMS,IEL,1)
        ENDDO
      ELSEIF (KEYWORD.EQ.'*SET_LEGO_IMAT') THEN   ! 2-2-97
c        IOPS(11) = 1          ! 
        DO IEL= 1,NEL
          CALL GET_EL_IMAT (NUMS,INUMS,IEL,IMAT)
          CALL PUT_EL_IGRP (NUMS,INUMS,IEL,IMAT)
        ENDDO


c------------------------ material properties --------------------------
c.... It would be nice to have MATS_BY_BOX_BY_IMAT .. to only 
c.... affect a certain original material (eg. soil around piles)

      ELSEIF (KEYWORD.EQ.'*ELEMENT_SET') THEN      !- cf. Abaqus
        CALL R_ELEMENT_SET (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL,P)

      ELSEIF (KEYWORD.EQ.'*ELEMENT_GROUP') THEN      !- cf. Abaqus
        CALL R_ELEMENT_GROUP (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL,P)

      ELSEIF (KEYWORD.EQ.'*MATERIALS_BY_BOX') THEN
        CALL R_MATS_BY_BOX (IO,IPR,GC,IGC,NDIM,NN,NUMS,INUMS,NEL)
      ELSEIF (KEYWORD.EQ.'*MATERIALS_BY_BOX_IMAT') THEN
        CALL R_MATS_BY_BOX_IMAT (IO,IPR,GC,IGC,NDIM,NN,NUMS,INUMS,NEL)

      ELSEIF (KEYWORD.EQ.'*MATERIALS_BY_IEL') THEN
        CALL R_MATS_BY_IEL (IO,IPR,GC,IGC,NDIM,NN,NUMS,INUMS,NEL)
      ELSEIF (KEYWORD.EQ.'*WRITE_MATERIALS') THEN
        CALL WR_MATS_BY_IEL (IO2,GC,IGC,NDIM,NN,NUMS,INUMS,NEL,P)

      ELSEIF (KEYWORD.EQ.'*DELETE_MATERIALS') THEN
        CALL R_DEL_MATS   (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL,P)

      ELSEIF (KEYWORD.EQ.'*DELETE_INVIS_MATERIALS') THEN
        CALL DEL_INVIS_MATS (GC,IGC,NDIM,NN,NUMS,INUMS,NEL,P)

      ELSEIF (KEYWORD.EQ.'*SWAP_IMAT_IGRP') THEN
        CALL SWAP_IMAT_IGRP (GC,IGC,NDIM,NN,NUMS,INUMS,NEL)

      ELSEIF (KEYWORD.EQ.'*CHANGE_MATERIALS') THEN
        CALL R_CHANGE_MATS (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL)

c... both of these use the same notation +2, -4 etc.
c... nice to do this using the mouse in DANPLOT
      ELSEIF (KEYWORD.EQ.'*ADD_ELEMENTS') THEN
c        CALL R_ADD_REM_ELEMS (IO,NUMS,INUMS,NEL)
        call myerror (2,'Please use *CHANGE_MATERIALS instead')
      ELSEIF (KEYWORD.EQ.'*REMOVE_ELEMENTS') THEN
        call myerror (2,'Please use *CHANGE_MATERIALS instead')
c        CALL R_ADD_REM_ELEMS (IO,NUMS,INUMS,NEL)

C----------------- duplicating and mirroring the mesh ------------------

      ELSEIF (KEYWORD.EQ.'*COPY_MESH') THEN
        CALL R_COPY_MESH (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL)
      
      ELSEIF (KEYWORD.EQ.'*COPY_MESH_ARC') THEN
        CALL R_COPY_MESH_ARC (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL)
      
      ELSEIF (KEYWORD.EQ.'*MIRROR_MESH') THEN
        CALL R_MIRROR_MESH (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL)

c-------------- changing the 'handedness' of the mesh ------------------
c.. so -DET -> +ve, or vice versa or 'both' simultaneously
c.. ok try *FLIP_ELEMENTS, default is 'UP', but can have 'DOWN, 'OVER'
      ELSEIF (KEYWORD.EQ.'*TURN_UP_ELEMENTS') THEN
        CALL FLIP_DET (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL,1)
      ELSEIF (KEYWORD.EQ.'*TURN_DOWN_ELEMENTS') THEN
        CALL FLIP_DET (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL,2)
      ELSEIF (KEYWORD.EQ.'*TURN_OVER_ELEMENTS') THEN
        CALL FLIP_DET (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL,3)
      ELSEIF (KEYWORD.EQ.'*FLIP_OVER_ELEMENTS') THEN
        call myerror (2,'Please use *TURN_OVER_ELEMENTS instead')
c      ELSEIF (KEYWORD.EQ.'*MIRROR_MESH') THEN
c        CALL R_MIRROR_MESH (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL)

C----------------------- modifing elements -----------------------------

c----------------------- moving the mesh -------------------------------
c.. some like BARYCENTRIC are concerned with improving the mesh quality.
c.. and 'Morphing' too
c.. These only affect the COORDINATES *not* the element topology.

      ELSEIF (KEYWORD.EQ.'*BARYCENTRIC') THEN     !- added 12-3-97
c. 2-7-97 .. oops RINGS is not available within this module :-(
c       CALL MARK_RING_NODES (RINGS, P,NN)  !- find unmovable nodes.
        max_iters=500
        fact=0.85      !- underrelaxation factor
        tol=1.e-5
        method='A  '; alpha=0.
        CALL BARYCENTRIC  (GC,IGC,NDIM,NN,NUMS,INUMS,NEL,P
     &   ,max_iters, fact,tol, method,alpha,ipr)

      ELSEIF (KEYWORD.EQ.'*SMOOTH_MESH') THEN     !- added 8-7-98
c                       eg.  1.e-4, 100, 0.8, 'ISO', 0.65
        read(io,*,iostat=ios) tol,max_iters, fact, method,alpha
        CALL BARYCENTRIC  (GC,IGC,NDIM,NN,NUMS,INUMS,NEL,P
     &   ,max_iters, fact,tol, method,alpha,ipr)

      ELSEIF (KEYWORD.EQ.'*MOVE_NODES') THEN
        CALL R_NODMOV (IO,GC,IGC,NDIM,NN)

      ELSEIF (KEYWORD.EQ.'*SHIFT_MESH') THEN
        CALL R_SHIFT_MESH (IO,GC,IGC,NDIM,NN)
      ELSEIF (KEYWORD.EQ.'*SCALE_MESH') THEN
        CALL R_SCALE_MESH (IO,GC,IGC,NDIM,NN)
      ELSEIF (KEYWORD.EQ.'*ROTATE_MESH') THEN
        CALL R_ROTATE_MESH (IO,GC,IGC,NDIM,NN)

      ELSEIF (KEYWORD.EQ.'*PROJECT_MESH') THEN        !- collapses 'z' or 4D
        CALL PROJECT_MESH (GC,IGC,NDIM,NN)

      ELSEIF (KEYWORD.EQ.'*X_TO_Y_TO_Z') THEN
        CALL X2Y2Z  (GC,IGC,NDIM,NN)
      ELSEIF (KEYWORD.EQ.'*X_TO_Y') THEN
        CALL X2Y    (GC,IGC,NDIM,NN)
 
      ELSEIF (KEYWORD.EQ.'*WRAP_AROUND_X') THEN       !- cart->cyl
        CALL WRAP_X (GC,IGC,NDIM,NN)
      ELSEIF (KEYWORD.EQ.'*WRAP_AROUND_Y') THEN       !- cart->cyl
        CALL WRAP_Y (GC,IGC,NDIM,NN)
      ELSEIF (KEYWORD.EQ.'*WRAP_AROUND_Z') THEN       !- cart->cyl
        CALL WRAP_Z (GC,IGC,NDIM,NN)

      ELSEIF (KEYWORD.EQ.'*CIRCLE_A_SQUARE') THEN     !- just one 
        CALL R_WRAP_CIRCLE_SQUARE (IO,GC,IGC,NDIM,NN) !- circlar-arc

      ELSEIF (KEYWORD.EQ.'*TUNNEL_A_SQUARE') THEN     !- just one 12-12-97 
        CALL R_WRAP_TUNNEL_SQUARE (IO,GC,IGC,NDIM,NN) !- 3-arc 'tunnel'

      ELSEIF (KEYWORD.EQ.'*WRAP_CIRCLE_Y') THEN       
        CALL WRAP_CIRCLE_Y (GC,IGC,NDIM,NN)           !- all to circlar
      ELSEIF (KEYWORD.EQ.'*WRAP_SQUARE_Y') THEN
        CALL WRAP_SQUARE_Y (GC,IGC,NDIM,NN)           !- all to square
      ELSEIF (KEYWORD.EQ.'*WRAP_SQUARE_Y_RANGE') THEN
        CALL R_WRAP_SQUARE_Y_RANGE (IO,IPR,GC,IGC,NDIM,NN)           !-  selected radii to square

      ELSEIF (KEYWORD.EQ.'*WRAP_SPHERE') THEN         !- cube to a balloon
        CALL WRAP_SPHERE (GC,IGC,NDIM,NN)

      ELSEIF (KEYWORD.EQ.'*WRAP_TWIST_Y') THEN        !- lets do the twist !
        CALL R_WRAP_TWIST_Y (IO,GC,IGC,NDIM,NN)


c-------------------------- obsolete modules ---------------------------

      ELSEIF (KEYWORD.EQ.'*TO_QUADS') THEN   !-- eg for 'teapot.g' 
        CALL M_TO_QUADS  (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL,P)  !*obs*

c     ELSEIF (KEYWORD.EQ.'*4_3NTS_TO_4NQS') THEN   !-- eg for 'teapot.g' 
c       CALL M_3NT_4NQ  (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL,P)  !*obs*

c     ELSEIF (KEYWORD.EQ.'*4NQS_TO_3NTS') THEN    !- slice each square '/'
c       CALL M_4NQ_3NT  (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL,P)    !*obs*

c      ELSEIF (KEYWORD.EQ.'*MIRROR_EVEN_ELEMENTS') THEN  ! eg for 'faces.2d'
c        CALL MIRROR_EVENS (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL,P)  !*obs*

C------------------------- else unknown --------------------------------
      ELSE
        FOUND = .FALSE.
      ENDIF
      RETURN
      END

C-----------------------------------------------------------------------







C-----------------------------------------------------------------------
c.. ( R_DANPLOT/WR_DANPLOT were here 6 July 93)
c... now returned here 10-9-93 and 'other' file formats added too
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C-----------------------------------------------------------------------
      SUBROUTINE R_LAY_SLOPE (IO,IPR,GC,IGC,NDIM,NN,NUMS,INUMS,NEL)
C
C     This reads in a mesh as a set of material layers (from READGM) 
C     written particular for 'IMS Italian sllope' example (cf Woody)
C     c. Dan Kidger 4-10-93
C     DATA as   : NLAYERS
C     then      : a table of NLAYERS lines where each line is the 
C               : X-coord then the set of y-values up this ordinate
C
      REAL GC(IGC,*)
      REAL    X(30)                     !- Max = 30 layers :-)
      INTEGER NUM(32), NUMS(INUMS,*)

      NDIM = 2
      NN  = 0    !- node numbers
      NEL = 0      !- element numbers

   1  READ (IO,*,IOSTAT=IOS) NLAY
      CALL IN_TEST(IO,IOS,*1,*999)

      DO IX=0,9999
   2  READ (IO,*,IOSTAT=IOS) (X(I),I=1,NLAY+2)
        CALL IN_TEST(IO,IOS,*2,*999)

C----------------- first create the new coordinates -----------------
        NN_BASE = NN
        DO I=1,NLAY+1
          NN = NN + 1
          GC (1,NN) = X(1)
          GC (2,NN) = X(1+I)
        ENDDO

C------------------- then create the new elements -------------------
        IF (IX.GE.1) then   !- only on subsequent passes
          DO I=1,NLAY
            NEL = NEL + 1         
            NUM(1) = NN_BASE+I    - (NLAY+1)
            NUM(2) = NN_BASE+I +1 - (NLAY+1)
            NUM(3) = NN_BASE+I +1 
            NUM(4) = NN_BASE+I    

            IMAT  = I
            NDIME = NDIM 
            ITYPE = 1
            NOD   = 4

            CALL PUT_ELEMENT 
     &      (NUMS,INUMS,NEL, NUM, NOD,NDIME,ITYPE, IMAT, 1,1,1)
          ENDDO
        endif
      ENDDO    !- loop the new strips

  999 CONTINUE
      IF (IPR.GE.2) WRITE (*,'(A,I7,A,I7,   a,2i7,a)') 
     &    '>< NN=',NN, ' NEL=',NEL, '(Nlayers,NXE=', nlay,ix-1,' )'
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE R_BASIC_4NQ (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL)

C.... just a simple way of producing NxM meshes of 4nq's
C  ....FOR 4-NODE QUADS COUNTING IN Y-DIRECTION
C... nice to generalise this to 2/3/4D, GC is easy, NUM is specific :-(

      REAL GC(IGC,*)
      INTEGER NUM(20), NUMS(INUMS,*)

   2  READ (IO,*,IOSTAT=IOS) NXE,NYE, AA,BB      !- assume NDIM ?
        CALL IN_TEST(IO,IOS,*2,*999)             !- ASSUME imat ?

c---------------------- store the nodal coords -------------------------
c.. in general I would use a set of 'toggles' and a single loop
      NN = 0
      DO I= 0,NXE
        DO J= NYE,0,-1                !- so #1 is top-left
          NN = NN + 1
          GC (1,NN ) = AA * I
          GC (2,NN ) = BB * J
        ENDDO
      ENDDO
c---------------------- store the element steering ---------------------
      NEL = 0
      DO IQ= 1,NYE
        DO IP= 1,NXE
          NEL = NEL + 1
          NUM(1)=(IP-1)*(NYE+1)+IQ+1
          NUM(2)=NUM(1)-1
          NUM(3)=IP*(NYE+1)+IQ
          NUM(4)=NUM(3)+1
          IMAT  = 1
          NDIME = NDIM 
          ITYPE = 1
          NOD   = 4
          CALL PUT_ELEMENT 
     &       (NUMS,INUMS,NEL, NUM, NOD,NDIME,ITYPE, IMAT, 1,1,1)
        ENDDO
      ENDDO

  999 WRITE (*,'(A,I7,A,I7,   a,2i7,a)') 
     +    '>< NN=',NN, ' NEL=',NEL
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE R_BASIC_8NB (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL)
C
C     Just a simple way of producing NxMxP meshes of 8nb's
C       DJK 2-2-97
c  Notes:
c     Jasper's numbers X then Y then Z, so he should enter NYP-1,NXP-1,NZP-1
C
      REAL GC(IGC,*)
      INTEGER NUM(20), NUMS(INUMS,*)

   2  READ (IO,*,IOSTAT=IOS) NXE,NYE,NZE, AA,BB,CC      !- assume NDIM ?
        CALL IN_TEST(IO,IOS,*2,*999)

c---------------------- store the nodal coords -------------------------
c.. in general I would use a set of 'toggles' and a single loop
      NN = 0        !- why reset to zero ?
      DO K= 0,NZE
        DO I= 0,NXE
          DO J= NYE,0,-1                !- so #1 is top-left
            NN = NN + 1
            GC (1,NN ) = AA * I
            GC (2,NN ) = BB * J
            GC (3,NN ) = CC * K
          ENDDO
        ENDDO
      ENDDO

c---------------------- store the element steering ---------------------
      NEL = 0
      DO IQ= 1,NYE
        DO IP= 1,NXE
          DO IS= 1,NZE               !- was NXE 21-9-98
            NEL = NEL + 1
            NUM(1) = (IQ-1)*(NXE+1)*(NZE+1)+IS*(NXE+1)+IP
            NUM(2) = NUM(1)-NXE-1
            NUM(3) = NUM(2)+1
            NUM(4) = NUM(1)+1
            NUM(5) = NUM(1)+(NXE+1)*(NZE+1)
            NUM(6) = NUM(5)-NXE-1
            NUM(7) = NUM(6)+1
            NUM(8) = NUM(5)+1

            NOD   = 8
            NDIME = NDIM 
            ITYPE = 1
            IMAT  = 1
            CALL PUT_ELEMENT 
     &       (NUMS,INUMS,NEL, NUM, NOD,NDIME,ITYPE, IMAT, 1,1,1)
          ENDDO
        ENDDO
      ENDDO

  999 WRITE (*,'(A,I7,A,I7,   a,2i7,a)') 
     &    '>< NN=',NN, ' NEL=',NEL
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE R_BASIC_BLOCK (IO,ipr,GC,IGC,NDIM,NN,NUMS,INUMS,NEL)
C
C     This creates a basic-building-block for meshes
C     in the form of a 4nq (8nb in 3D) of the given Material type
c     This will also do a single 6nt etc. ?
C        Dan Kidger 9-4-94
C     DATA as   : IMAT
C     then      : lines of x-from, x-width
C               :          y-from, y-width (+Z for 3D)
C
      PARAMETER (ICOORD=32)
      REAL GC(IGC,*), START(5),WIDTH(5)     !- box-size
     &    ,COORD_L (ICOORD,3)               !- local coords of the box
     &    ,xyz(5)                           !- a point
      INTEGER NUM(icoord), NUMS(INUMS,*)

      DO JJJ = 1,999
c---- read-in the material type and the box-size -------
c.. note that we do not read in NDIME but imply it ??
   1  READ (IO,*,IOSTAT=IOS) IMAT
      CALL IN_TEST(IO,IOS,*1,*999)

      NDIME = NDIM
      DO J=1,NDIME
   2    READ (IO,*,IOSTAT=IOS) START(J),WIDTH(J)
        CALL IN_TEST(IO,IOS,*2,*999)
      ENDDO
c--------------------- first do the coordinates --------------------------
c If I could read in NDIME,NOD,ITYPE then I could create *any* element
      NOD = 2**NDIME          !- ie 2nl, 4nq, 8nb...
      ITYPE = 1
      CALL WTHATN (NOD,NDIME,ITYPE,COORD_L,ICOORD)

      DO I=1,NOD
        INODE = NN+I
        DO J=1,NDIM            !- note use of NDIM rather than NDIME ?
c         GC(J,I+NN) = START(J) + WIDTH(J) * (COORD_L(i,j)+1.)/2.
          xyz(J) = START(J) + WIDTH(J) * (COORD_L(i,j)+1.)/2.
        ENDDO
        CALL FIND_OR_ADD_NODE (GC,IGC,NN,NDIM,XYZ,1,NN,JNODE)
        NUM(I)=JNODE
      ENDDO

c--------- now build NUMS ----
      NEL = NEL + 1
      CALL PUT_ELEMENT (NUMS,INUMS,NEL, NUM, 
     &                   NOD,NDIME,ITYPE, IMAT, 1,1,1)

C------ to the next 'box' ----
      ENDDO

  999 nboxes = jjj-1

      IF (NBOXES.EQ.0) THEN
        CALL MYERROR(2, 'Invalid *BASIC_BLOCK format')
      ELSEIF (NBOXES.EQ.1) THEN
        if (ipr.ge.2)
     &  WRITE (*,'(A,I7,A)')  '>< ', nboxes,' Box created' 
      ELSE
        if (ipr.ge.2)
     &  WRITE (*,'(A,I7,A)')  '>< ', nboxes,' Boxes created' 
      ENDIF
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE R_NODES (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL)
C
C     This reads in the nodal coordinates (cf.'Danplot format')
C       25-4-94
c     hmm this is explicit - It wont let you sum two meshes together  
C
      REAL GC(IGC,*)
      INTEGER NUMS(INUMS,*)

      nn_old=nn
      nn_old=0    !- reset ?
      DO NN_NEW = 1,99999
   1    READ (IO,*,IOSTAT=IOS) INODE,(GC(J,nn_old+INODE),J=1,NDIM)
        CALL IN_TEST (IO,IOS,*1,*999)
        NN = MAX (NN,INODE+nn_old)             !- update max node number
      ENDDO
  999 WRITE(*,'(A,I7,A)')  '>< ',NN_NEW-1,' nodes read'
      END

C-----------------------------------------------------------------------
      SUBROUTINE R_ELEMENTS (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL)
C
C     This reads in the element steering (cf.'Danplot format')
C      but NOD as 3,20,1 for complete-definition
C      25-4-94

c   8-2-98 ABSTRACTION OF CONSTANT-VALUES
c       If all elements are 20nb's then good to set as 'etype=3 20 1'
c       simerlarly for IMAT.
C       hence extend for IGRP too (any others ?)
c       This column therefore is skipped in reading
c       doing 'etype=' resets to an explicit definition per element
c
c  1: WRITING                             
c       Check ahead how many are the same. If > min (say(5)), invoke 
c       this 'etype=3 8 1' line. If a change, again scan ahead to see
c       whether we should use an 'etype' line or revert to explicit.
c       ? maybe always use the etype format ?
c         cf. the abaqus style.
c
c  2: READING
c       Always read as a char string
c       If first is not a number - treat as a token.
c       Have a knowledge of which values are currently being held in tokens
c       so we know hany many data items to expect
c
c

      REAL GC(IGC,*)
      INTEGER NUMS(INUMS,*), NUM(32)

c.. I need a flag, about wether to use the IEL explicitly
C.. or to start from 1 again OR to add to the existing set
c.. good to record how R_NODES got on wrt re-labeling the nodes
c    hence add this as an offset
c.. ok what about having .KEY, .DP and .OUT as valid (-d) input formats
c   then we can parse them for *one* *NODES and *one* elements ?
c   (but be careful of calling K_MESHIO recursively)


      DO iel2 = 1,99999
    1   READ (IO,*,iostat=ios) IEL,  NDIME,NOD,ITYPE, 
     &                         (NUM(J),J=1,NOD),IMAT
        CALL IN_TEST (IO,IOS,*1,*999)
c        do j=1,nod                    !- add offsets
c          num(j) = num(j)+nn_old
c        enddo
c       NEL = MAX (NEL,IEL2)       ! 10-7-96 changed to respect existing
        NEL = NEL + 1              ! elements (but still use new el. numbers)
        CALL PUT_ELEMENT 
c    &  (NUMS,INUMS,IEL2, NUM, NOD,NDIME,ITYPE, IMAT,iel,1,1)
     &  (NUMS,INUMS,NEL, NUM, NOD,NDIME,ITYPE, IMAT,iel,1,1)
      ENDDO
  999 WRITE(*,'(A,I7,A)')  '>< ',iel2-1,' elements read'
      END
                 
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
c  8-2-98 AUTOMATIC PLACEMENT OF MID-SIDE NODES
C      nice to have pseudo node numbers (eg use -1 ?). 
C      Later a *MIDSIDE_NODES module will replace these with newly
c      generated nodes, using WTHATN and sampling within a parent 4nq
c      (or a triangle, 8nb, etc. as appropriate)
c      - or Method 2:
c       for any element with a node number=0:
c          loop its (6) facets
c            loop around this facet, any zero is set as a linear interpolation
c            of the corner (positive) nodes either side.
c       Best is to sample using a line element along this edge?
C       - what about mid-face nodes (eg of a 14nb, or 17nq) ?
C
C-----------------------------------------------------------------------
      SUBROUTINE BUILD_MIDSIDE_NODES 
     & (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL,P)
C
C     This creates real midside nodes where there were only 
C       placeholders (node#=0) in *ELEMENTS
C        Dan Kidger  8-2-98
C
C    Caveats:
c      So far we only support the 8-node quad (6nq too)
c      12nq will *not* work.
c      realy we should use loop the (6) FACETS for 3d elements
c

      REAL GC(IGC,*), XYZ(5)
      INTEGER NUMS(INUMS,*), P(*), NUM(32)

      DO IEL=1,NEL
        CALL GET_ELEMENT
     &  (NUMS,INUMS,IEL, NUM, NOD,NDIME,ITYPE, IMAT,I1,i2,i3)
C--- test for mid-siders
        NZEROS=0
        DO I=1,NOD
          IF (NUM(I).EQ.0) NZEROS=NZEROS+1
        ENDDO
        IF (NZEROS.eq.0) goto 1    ! CYCLE  cos element was OK

c-- loop and fix them
c.. need to look for a zero, thence scan for how many in this run
c for now lets just assume there is only one zero at a time

        DO I=1,NOD
          IF (NUM(I).EQ.0) THEN
            ilast=i-1
            if (ilast.eq.0) ilast=nod
            inext=i+1
            if (inext.gt.nod) inext=1
c           IF (NOD(INEXT).EQ.0) then
c             - keep looking to get ic = the #of consecutives.
c
c           ENDIF

c           DO II=ilast+1, inext-1
              DO J=1,NDIM
                XYZ(J) = (GC(j,NUM(ILAST)) + GC(j,NUM(INEXT)) )/2.
              ENDDO
              CALL FIND_OR_ADD_NODE (GC,IGC,NN,NDIM,XYZ,1,NN,JNODE)
              NUM(I) = JNODE   !- record the (new) node
c           ENDDO

C.. was the last a zero too?
c          ELSEIF (NUM(I).ne.0) THEN
c.. If the last was a zero then we now nvoke the method
          ENDIF
        ENDDO

c... finally save this now-complete element
        CALL PUT_ELEMENT
     &  (NUMS,INUMS,IEL, NUM, NOD,NDIME,ITYPE, IMAT,I1,i2,i3)


    1   CONTINUE 
      ENDDO   !- loop elements

      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE R_ELEMENTS_2 (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL,P)
C
C     This reads in the element steering (cf.'Danplot format')
C        Alternative format: 
C      1st line = element type   (eg. 3,20,1)
C      2nd line = material type  (eg. 4)
C      subsequent lines are just IEL and a line of NUM()
C      .. also cf a PAFEC like style where *some* information can be 
C      abstracted
C        10-3-95
C  
      REAL GC(IGC,*)
      INTEGER NUMS(INUMS,*), P(*), NUM(32), num0(1)
      data num0(1)/0/

  11  READ (IO,*,IOSTAT=IOS) NDIME, NEN, ITYPE
      CALL IN_TEST(IO,IOS,*11,*999)

  12  READ (IO,*,IOSTAT=IOS) IMAT
      CALL IN_TEST(IO,IOS,*12,*999)

      DO ii = 1,99999
    1   READ (IO,*,iostat=ios) iel,(NUM(J),J=1,NEN)
        CALL IN_TEST (IO,IOS,*1,*999)
c............... fill-in any gaps ................
c contrast with just storing seqentialy
        do iel2 =nel+1, iel-1  
          CALL PUT_ELEMENT 
     &    (NUMS,INUMS,IEL2,   num0, 0, 0, 0,     0,0,0,0)
        enddo
c.............. add the current element ...............
        CALL PUT_ELEMENT 
     &  (NUMS,INUMS,IEL, NUM, NEN,NDIME,ITYPE, IMAT,iel,1,1)
        NEL = MAX (NEL,IEL)
      ENDDO
  999 WRITE(*,'(A,I7,A)')  '>< ',ii,' elements read'

      END

C-----------------------------------------------------------------------
      SUBROUTINE R_SLOPE_MESH (IO,IPR,GC,IGC,NDIM,NN,NUMS,INUMS,NEL)
C
C     This routine reads the element geometry puts it into 'long' form
C     for *BASIC_SLOPE_MESH
C     19-1-93 moved from L1A.FOR to MESH1.FOR for mesh generation
C
      REAL    GC(IGC,*)
      INTEGER NUMS(INUMS,*)
      REAL    TOP(90), BOT(90), DEPTH(90), BRDTH(90)

      NZE = 1
   1  READ (IO,*,IOSTAT=IOS) NDIME, NOD, ITYPE
        CALL IN_TEST(IO,IOS,*1,*999)
   2  READ (IO,*,IOSTAT=IOS) NXE,NXS 
        CALL IN_TEST(IO,IOS,*2,*999)
   3  READ (IO,*,IOSTAT=IOS) (TOP(I),I=1,NXS+1)
        CALL IN_TEST(IO,IOS,*3,*999)
   4  READ (IO,*,IOSTAT=IOS) (BOT(I),I=1,NXE+1)
        CALL IN_TEST(IO,IOS,*4,*999)
   5  READ (IO,*,IOSTAT=IOS) NYE,NYS 
        CALL IN_TEST(IO,IOS,*5,*999)
   6  READ (IO,*,IOSTAT=IOS) (DEPTH(I),I=1,NYE+1)
        CALL IN_TEST(IO,IOS,*6,*999)

      IF (NDIME.EQ.3) THEN
   7    READ (IO,*,IOSTAT=IOS) NZE
          CALL IN_TEST(IO,IOS,*7,*999)
   8    READ (IO,*,IOSTAT=IOS) (BRDTH(I),I=1,NZE+1)
          CALL IN_TEST(IO,IOS,*8,*999)
      ENDIF

      IMAT=1    !- default material type
      CALL SLOPE_MESH (IPR,GC,IGC,NDIM,NN,NUMS,INUMS,NEL,
     &  NXE,NXS, NYE,NYS, NZE, TOP,BOT,DEPTH,BRDTH, NDIME,NOD,ITYPE
     & ,IMAT)
      RETURN
  999 CALL MYERROR(2, 'mesh reading failed')
      RETURN
      END 

C-----------------------------------------------------------------------
      SUBROUTINE R_ONE_ELEMENT (IO,IPR,GC,IGC,NDIM,NN,NUMS,INUMS,NEL)
c
c     just WTHATN creates a single element
c
      PARAMETER (ICOORD=32)
      INTEGER NUMS(INUMS,NEL), NUM(ICOORD)
      REAL GC(IGC,NN), COORD(ICOORD,3)

      READ (IO,*,IOSTAT=IOS)  NOD,NDIME,ITYPE
      CALL CHANGE_NDIM (GC,IGC,NDIM,NN,NDIME)

      CALL WTHATN (NOD,NDIME,ITYPE,COORD,ICOORD)

      DO I=1,NOD
        NN=NN+1
        NUM(I)=NN
        DO J=1,NDIME
          GC(J,NN) = COORD(I,J)
        ENDDO
      ENDDO

      NEL=NEL+1
      CALL PUT_ELEMENT 
     &  (NUMS,INUMS,NEL, NUM, NOD,NDIME,ITYPE, IMAT, 1,1,1)

      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE R_SUPERBLOCK (IO,IPR,GC,IGC,NDIM,NN,NUMS,INUMS,NEL)
C
C     This routine can create a large cuboid meshes of 8,14,20 node bricks
C     (8nq in 2d) via a call to SLOPE_MESH (which calls my old SLOGET)
C      cf. R_BASIC_4NQ, R_BASIC_8NB that explicitly node create
C
C     This routine is good for 'mega' 50x50x50 20nb meshes as used to 
C     put the Cray T3D through its paces !
C        DJK 29-3-96 
C     16-2-97 ** note new data format: **
c          IMAT,  NDIME,NOD.ITYPE  (eg   7   2 8 1 )
c           NXE,  xstart, xwidth        50   -25.  50.
c           NYE,  ystart, ywidth        10   -20.  20.
c         < NZE,  zstart, zwidth >       5     0.  30.
C
      REAL    GC(IGC,*)
      INTEGER NUMS(INUMS,*)
      REAL    TOP(256), BOT(256), DEPTH(256), BRDTH(256)
      integer nx(5)          !- NXE,NYE,(NZE)
c     real    wid(5)         !- AA,BB,(CC)
      real    xstart(5),xwidth(5) !- start and size of the blovk

c.. hmm I would like to give both start and width
c.. so change format to NX(), XSTART(), XWDITH() ?

c.. why can I not read in an IMAT (maybe store in common ?)

   1  READ (IO,*,IOSTAT=IOS) IMAT,  NDIME,NOD,ITYPE
        CALL IN_TEST(IO,IOS,*1,*999)
c  2  READ (IO,*,IOSTAT=IOS) (nx(j),wid(j),j=1,ndime)
   2  READ (IO,*,IOSTAT=IOS) (nx(j),xstart(j),xwidth(j),j=1,ndime)
        CALL IN_TEST(IO,IOS,*2,*999)

      NXE = NX(1)
      NXS = NXE
      NYE = NX(2)
      NYS = NYE
      NZE = 1
      IF (NDIME.EQ.3) NZE = NX(3)

      IF (IPR.GE.2) WRITE (*,'(3(A,I7))') 
     &  'Nxe=', nxe, ' Nye=', nye,' Nze=', nze

      DO I=1,NXE+1
c       TOP(I) = (I-1) * WID(1)
        TOP(I) = XSTART(1) + real(I-1)/real(nxe) * XWIDTH(1)
        BOT(I) = TOP(I)
      ENDDO
      DO I=1,NYE+1
c       DEPTH(I) = (NYE+1-I) * WID(2)     !- note the reverse order
c       DEPTH(I) = -(I-1) * WID(2)        !- note the reverse order
        DEPTH(I) = XSTART(2) + real(nye+1-I)/real(nye) * XWIDTH(2)
      ENDDO

      IF (NDIME.EQ.3) THEN
        DO I=1,NZE+1
c         BRDTH(I) = (NZE+1-I) * WID(3)   !- note the reverse order
c         BRDTH(I) = -(I-1) * WID(3)      !- note the reverse order
          BRDTH(I) = XSTART(3) + real(nze+1-I)/real(nze) * XWIDTH(3)
        ENDDO
      ENDIF

      CALL SLOPE_MESH (IPR,GC,IGC,NDIM,NN,NUMS,INUMS,NEL,
     &  NXE,NXS, NYE,NYS, NZE, TOP,BOT,DEPTH,BRDTH, NDIME,NOD,ITYPE
     & ,IMAT)
      RETURN
  999 CALL MYERROR(2, 'mesh reading failed')
      RETURN
      END 

C-----------------------------------------------------------------------
      SUBROUTINE SLOPE_MESH (IPR,GC,IGC,NDIM,NN,NUMS,INUMS,NEL,
     &  NXE,NXS, NYE,NYS, NZE, TOP,BOT,DEPTH,BRDTH, NDIME,NOD,ITYPE
     &  ,IMAT)
C
C    Forms the coordinates and element steering for 'slope-like' meshes
C    (This family includes all the basic rectangular meshes)
C      29-3-96 DJK - cut out of R_SLOPE_MESH
C      25-5-04 added IMAT as an arg
      REAL    GC(IGC,*)
      INTEGER NUMS(INUMS,*)
      REAL    TOP(*), BOT(*), DEPTH(*), BRDTH(*)
      INTEGER NUM(20)
      REAL COORD(20,3)

!     IMAT  = 1       !- why can I not pass this in ?
c                     !- cf where I want IMAT = IY for layered soil.

C---------------- form the global co-ord array GCOORD ------------
      NDIM = NDIME    !--- ie 3D elements in '3D'
      NN = NN
c     NN  = 0           !- why can I not 'append'?
c     NN_OLD = 0
      NN_OLD = NN       !- or build on an old mesh ?
c     NEL_OLD = 0
      NEL_OLD = NEL
      IEL = NEL_OLD
      DO IP=1,NXE
        DO IQ=1+NYS*MIN(MAX(IP-NXS,0),1),NYE
          DO IS=1,NZE
            IEL = IEL + 1

c....... hmm I could even have 1d elements !
          IF (NDIM.EQ.2) THEN
            CALL SLOGE2(IP,IQ,NXS,NYS,NYE,TOP,BOT,DEPTH,COORD,20,NUM)
          ELSEIF (NDIM.EQ.3) THEN           !-- 8nq only
            CALL SLOGET(IP,IQ,IS,NXS,NYS,NYE,NZE,TOP,BOT,DEPTH,BRDTH,
     &      COORD,20,NUM,NOD)      !-- 8nb/14nb/20nb
          ELSE 
            CALL MYERROR(2,'NDIM not valid (not 2 or 3)')  
          ENDIF
        NDIME = NDIM  !------------- update the element table ----------
        ITYPE = 1
        DO I=1,NOD
          NUM(I) = NUM(I) + NN_OLD   !- add the offset
        ENDDO
        CALL PUT_ELEMENT 
     &  (NUMS,INUMS,IEL, NUM, NOD,NDIME,ITYPE, IMAT,IP,IQ,IS)

         DO I=1,NOD        !----- update the coordinate table --------- 
           INODE = NUM(I)
           NN = MAX (INODE,NN)
           DO J=1,NDIM
             GC (J,INODE) = COORD(I,J)
           ENDDO
         ENDDO

          ENDDO         ! loop       -z
        ENDDO          ! the        -y
      ENDDO           ! elements   -x
      NEL = IEL
      IF (IPR.GE.2) WRITE (*,'(3(A,I7))')   
     &  '>< NN=',NN, ' NEL=',NEL,' NOD=',nod
      RETURN
      END
C-----------------------------------------------------------------------
C   > > > > > > 'OLD' style basic mesh forming routines < < < < < < <
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C-----------------------------------------------------------------------
      SUBROUTINE SLOGET (IP,IQ,IS,NXS,NYS,NYE,NZE,TOP,BOT,DEPTH,BRDTH
     +       ,COORD,ICOORD,NUM,NEN)
C
C    header to SLOGE3 (20nb), SLOG14 (14nb) and SLOG08 (8nb/11nb)
c       Dan Kidger  c. 1988
C
C    .. needs fixing so that it can call SLOGE2 also 
C
      REAL TOP(*),BOT(*),DEPTH(*),BRDTH(*),COORD(ICOORD,*)
      INTEGER NUM(*)
      IF (NEN.EQ.20) THEN
        CALL SLOGE3(IP,IQ,IS,NXS,NYS,NYE,NZE,TOP,BOT,DEPTH,BRDTH
     +       ,COORD,ICOORD,NUM)
      ELSE IF (NEN.EQ.14) THEN
        CALL SLOG14(IP,IQ,IS,NXS,NYS,NYE,NZE,TOP,BOT,DEPTH,BRDTH
     +       ,COORD,ICOORD,NUM)
      ELSE IF (NEN.EQ. 8 .OR. NEN.EQ.11) THEN
        CALL SLOG08(IP,IQ,IS,NXS,NYS,NYE,NZE,TOP,BOT,DEPTH,BRDTH
     +       ,COORD,ICOORD,NUM)
        IF(NEN.EQ.11)THEN
          DO I=9,11
          NUM(I)=I
          ENDDO
        ENDIF
      ELSE
        PRINT*,'  OOPS NOT A 8,11,14 OR 20 NODE BRICK'
        STOP
      ENDIF
      RETURN
      END
C********************************************************************

      SUBROUTINE SLOGE2 (IP,IQ,NXS,NYS,NYE,TOP,BOT,DEPTH,COORD,ICOORD
     +,NUM)
C
C      THIS  FORMS THE COORDINATES AND STEERING VECTOR
C      FOR 8-NODE QUADRILATERALS IN A 'SLOPE' GEOMETRY
C      (NUMBERING IN THE Y-DIRECTION)
C      ---> corrected 23/6/88 by DJK to get the X-values right!   8-)
C      ...... ammended 7-9-88 by DJK
C
      REAL TOP(*),BOT(*),COORD(ICOORD,*),DEPTH(*)
      INTEGER NUM(*)
      NUM(1)=(IP-1)*(3*NYE+2)+2*IQ+1
      NUM(4)=(IP-1)*(3*NYE+2)+2*NYE+IQ+1
      NUM(5)=IP*(3*NYE+2)+2*IQ-1
      IF(IP.GT.NXS)THEN
      NUM(1)=NUM(1)-(((IP-1-NXS)*3)+0)*NYS
      NUM(4)=NUM(4)-(((IP-1-NXS)*3)+1)*NYS
      NUM(5)=NUM(5)-(((IP-1-NXS)*3)+3)*NYS
      ENDIF
      NUM(2)=NUM(1)-1
      NUM(3)=NUM(1)-2
      NUM(6)=NUM(5)+1
      NUM(7)=NUM(5)+2
      NUM(8)=NUM(4)+1
c ... lines to form 'G' deleteed
      IF(IQ.LE.NYS)THEN
      FAC1=(BOT(IP  )-TOP(IP  ))/ (DEPTH(NYS+1)-DEPTH(1))
      FAC2=(BOT(IP+1)-TOP(IP+1))/ (DEPTH(NYS+1)-DEPTH(1))
      COORD(1,1)=TOP(IP  )+(DEPTH(IQ+1)-DEPTH(1))*FAC1
      COORD(3,1)=TOP(IP  )+(DEPTH(IQ  )-DEPTH(1))*FAC1
      COORD(5,1)=TOP(IP+1)+(DEPTH(IQ  )-DEPTH(1))*FAC2
      COORD(7,1)=TOP(IP+1)+(DEPTH(IQ+1)-DEPTH(1))*FAC2
      ELSE
      COORD(1,1)=BOT(IP)
      COORD(3,1)=BOT(IP)
      COORD(5,1)=BOT(IP+1)
      COORD(7,1)=BOT(IP+1)
      ENDIF
      COORD(2,1)=.5*(COORD(1,1)+COORD(3,1))
      COORD(6,1)=.5*(COORD(5,1)+COORD(7,1))
      COORD(4,1)=.5*(COORD(3,1)+COORD(5,1))
      COORD(8,1)=.5*(COORD(7,1)+COORD(1,1))
      COORD(1,2)=DEPTH(IQ+1)
      COORD(8,2)=DEPTH(IQ+1)
      COORD(7,2)=DEPTH(IQ+1)
      COORD(3,2)=DEPTH(IQ)
      COORD(4,2)=DEPTH(IQ)
      COORD(5,2)=DEPTH(IQ)
      COORD(2,2)=.5*(COORD(1,2)+COORD(3,2))
      COORD(6,2)=.5*(COORD(5,2)+COORD(7,2))
      RETURN
      END
C************************************************************************
      SUBROUTINE SLOGE3 (IP,IQ,IS,NXS,NYS,NYE,NZE,TOP,BOT,DEPTH,BRDTH
     +       ,COORD,ICOORD,NUM)
C
C      THIS FORMS THE COORDINATES AND STEERING VECTOR
C      FOR 20-NODE BRICKS IN A 'SLOPE' GEOMETRY
C      (NUMBERING IN THE Z-Y-X -DIRECTIONS)
C      ---> corrected 23/6/88 by DJK to get the X-values right!   8-)
C      ...... ammended 7-9-88 by DJK for slopes on a base 8->
C      -----> extension of SLOGE2 by DJK ....  10-2-89
C
!     IMPLICIT NONE
      REAL TOP(*),BOT(*),COORD(ICOORD,*),DEPTH(*),BRDTH(*)
      INTEGER NUM(*),IFROM(12),ITO(12),IE(3,12), BASE, I,J
      DATA IFROM/15,16,17,10,11, 3, 4, 5,14,18, 2, 6/
      DATA ITO  /14,20,18, 9,12, 2, 8, 6,13,19, 1, 7/
      DATA IE   / 1, 2, 3, 3, 4, 5, 5, 6, 7, 7, 8, 1,
     +           13,14,15,15,16,17,17,18,19,19,20,13,
     +            1, 9,13, 3,10,15, 5,11,17, 7,12,19/
      IF(IQ.LE.NYS.AND.IP.GT.NXS)THEN
      PRINT*,'OUT OF MESH RANGE..... ie this element aint ''ere'
      STOP
      ENDIF
      IF(IP.LE.NXS)THEN
      BASE=(IP-1)*(4*NYE*NZE+3*(NYE+NZE)+2)
      NYEH=NYE
      IQ1=IQ
      ELSE
      NYEH=NYE-NYS
      IQ1=IQ-NYS
      BASE = 4*NXS*NYE*NZE+3*(NXS*(NYE+NZE)+NYE*NZE)+2*(NXS+NYE+NZE)+1
     + - 3*NZE*NYEH -2*(NZE+NYEH) -1
     + + (IP-NXS-1)*(4*NYEH*NZE+3*(NYEH+NZE)+2)
      ENDIF
      N1=3*NZE*NYEH+2*(NZE+NYEH)+1
      N2=N1+(NZE+1)*(NYEH+1)
      NUM(15)=BASE+ (3*NZE+2)*(IQ1-1)+ 2*(IS-1)+ 1
      NUM(10)=NUM(15)+ (2*NZE+1)- (IS-1)
      NUM( 3)=NUM(15)+ (3*NZE+2)
      NUM(16)=BASE+N1+ (NZE+1)*(IQ1-1)+  (IS-1)+ 1
      NUM( 4)=NUM(16)+ (NZE+1)
      NUM(17)=NUM(15)+ N2
      NUM(11)=NUM(10)+ N2
      NUM( 5)=NUM( 3)+ N2
      DO I=1,12
        NUM(ITO(I))=NUM(IFROM(I)) + 1
      ENDDO
C     WRITE(*,'(20I4)')(NUM(I),I=1,20)
      IF(IQ.LE.NYS)THEN
      FAC1=(BOT(IP  )-TOP(IP  ))/ (DEPTH(NYS+1)-DEPTH(1))
      FAC2=(BOT(IP+1)-TOP(IP+1))/ (DEPTH(NYS+1)-DEPTH(1))
      COORD( 3,1)=TOP(IP  )+(DEPTH(IQ+1)-DEPTH(1))*FAC1
      COORD(15,1)=TOP(IP  )+(DEPTH(IQ  )-DEPTH(1))*FAC1
      COORD(17,1)=TOP(IP+1)+(DEPTH(IQ  )-DEPTH(1))*FAC2
      COORD( 5,1)=TOP(IP+1)+(DEPTH(IQ+1)-DEPTH(1))*FAC2
      ELSE
      COORD( 3,1)=BOT(IP)
      COORD(15,1)=BOT(IP)
      COORD(17,1)=BOT(IP+1)
      COORD( 5,1)=BOT(IP+1)
      ENDIF
      COORD( 3,2)=DEPTH(IQ+1)
      COORD(15,2)=DEPTH(IQ)
      COORD(15,3)=BRDTH(IS  )
      COORD(13,3)=BRDTH(IS+1)
      COORD(13,1)=COORD(15,1)
      COORD(19,1)=COORD(17,1)
      COORD( 1,1)=COORD( 3,1)
      COORD( 7,1)=COORD( 5,1)
      COORD(17,2)=COORD(15,2)
      COORD(13,2)=COORD(15,2)
      COORD(19,2)=COORD(15,2)
      COORD( 5,2)=COORD( 3,2)
      COORD( 1,2)=COORD( 3,2)
      COORD( 7,2)=COORD( 3,2)
      COORD(17,3)=COORD(15,3)
      COORD( 3,3)=COORD(15,3)
      COORD( 5,3)=COORD(15,3)
      COORD(19,3)=COORD(13,3)
      COORD( 1,3)=COORD(13,3)
      COORD( 7,3)=COORD(13,3)
      DO I=1,12
        DO J=1,3
          COORD(IE(2,I),J) = .5*(COORD(IE(1,I),J)+COORD(IE(3,I),J))
        ENDDO
      ENDDO
      RETURN
      END
C************************************************************************
      SUBROUTINE SLOG08 (IP,IQ,IS,NXS,NYS,NYE,NZE,TOP,BOT,DEPTH,BRDTH
     +       ,COORD,ICOORD,NUM)
C
C      THIS FORMS THE COORDINATES AND STEERING VECTOR
C      FOR  8-NODE BRICKS IN A 'SLOPE' GEOMETRY
C      (NUMBERING IN THE Z-Y-X -DIRECTIONS)
C
C     --> based on SLOG14 (cf)   . . . . . . .   11-Aug-90  by DJK
C
      REAL TOP(*),BOT(*),COORD(ICOORD,*),DEPTH(*),BRDTH(*)
      INTEGER NUM(*)
     + ,BASE,F1,F2
      IF(IQ.LE.NYS.AND.IP.GT.NXS)THEN
        PRINT*,'OUT OF MESH RANGE..... ie this element aint ''ere'
        STOP
      ENDIF
      IF(IP.LE.NXS)NYEH=NYE
      IF(IP.GT.NXS)NYEH=NYE-NYS
      F1 = (NYEH+1) * (NZE+1)
      F2 = (NYE +1) * (NZE+1)
      IF(IP.LE.NXS)THEN
        IQ1=IQ
      BASE= (IP-1) * F2
      ELSE
        IQ1=IQ-NYS
        BASE = NXS* F2  + (IP-1-NXS) * F1
      ENDIF
C   ------- simply step around the element,to number it-------
C     PRINT*,BASE,F1,F2
      NUM( 6)=BASE+ (NZE+1) * (IQ-1) + IS
      NUM( 5)=NUM( 6)+ 1
      NUM( 2)=NUM( 5)+ NZE
      NUM( 1)=NUM( 2)+ 1
      NUM( 7)=NUM( 6)+ F1
      NUM( 8)=NUM( 7)+ 1
      NUM( 3)=NUM( 8)+ NZE
      NUM( 4)=NUM( 3)+ 1
C     WRITE(*,'(8I4)')(NUM(I),I=1,8)
C -------- set the 4 x-values of the 'trapezoidal' faces -----
      IF(IQ.LE.NYS)THEN
        FAC1=(BOT(IP  )-TOP(IP  ))/ (DEPTH(NYS+1)-DEPTH(1))
        FAC2=(BOT(IP+1)-TOP(IP+1))/ (DEPTH(NYS+1)-DEPTH(1))
        COORD( 1,1)=TOP(IP  )+(DEPTH(IQ+1)-DEPTH(1))*FAC1
        COORD( 5,1)=TOP(IP  )+(DEPTH(IQ  )-DEPTH(1))*FAC1
        COORD( 8,1)=TOP(IP+1)+(DEPTH(IQ  )-DEPTH(1))*FAC2
        COORD( 4,1)=TOP(IP+1)+(DEPTH(IQ+1)-DEPTH(1))*FAC2
      ELSE
        COORD( 1,1)=BOT(IP)
        COORD( 5,1)=BOT(IP)
        COORD( 8,1)=BOT(IP+1)
        COORD( 4,1)=BOT(IP+1)
      ENDIF
      COORD( 1,2)=DEPTH(IQ+1)
      COORD( 5,2)=DEPTH(IQ  )
      COORD( 1,3)=BRDTH(IS+1)
      COORD( 2,3)=BRDTH(IS  )
C --------  copy  to nodes of the same x,y,z ------
      DO I=1,5,4
        COORD( 8-I,1)=COORD(9-I,1)
        COORD( 1+I,1)=COORD(I  ,1)
      ENDDO
      DO I=2,4
        COORD(I  ,2)=COORD(1,2)
        COORD(I+4,2)=COORD(5,2)
      ENDDO
      COORD(4,3)=COORD(1,3)
      COORD(5,3)=COORD(1,3)
      COORD(8,3)=COORD(1,3)
      COORD(3,3)=COORD(2,3)
      COORD(6,3)=COORD(2,3)
      COORD(7,3)=COORD(2,3)
      RETURN
      END
C************************************************************************
      SUBROUTINE SLOG14 (IP,IQ,IS,NXS,NYS,NYE,NZE,TOP,BOT,DEPTH,BRDTH
     +       ,COORD,ICOORD,NUM)
C
C      THIS FORMS THE COORDINATES AND STEERING VECTOR
C      FOR 14-NODE BRICKS IN A 'SLOPE' GEOMETRY
C      (NUMBERING IN THE Z-Y-X -DIRECTIONS)
C
C     --> based on SLOGE3 (cf)   . . . . . . .   31-July-89  by DJK
C
      REAL TOP(*),BOT(*),COORD(ICOORD,*),DEPTH(*),BRDTH(*)
      INTEGER NUM(*) ,IFN(6),IF(6,4)
     + ,BASE,F1,F2
      DATA IFN /  9, 10, 11, 12, 13, 14/
     +    ,IF  /  1,  5,  1,  2,  1,  3,
     +            2,  6,  3,  4,  2,  4,
     +            3,  7,  5,  6,  5,  7,
     +            4,  8,  7,  8,  6,  8/
      IF(IQ.LE.NYS.AND.IP.GT.NXS)THEN
        PRINT*,'OUT OF MESH RANGE..... ie this element aint ''ere'
        STOP
      ENDIF
      IF(IP.LE.NXS)NYEH=NYE
      IF(IP.GT.NXS)NYEH=NYE-NYS
C     PRINT*,NYE,NZE
      F1=2*NYEH*NZE + NYEH+ NZE
      F2=2*NYE *NZE + NYE + NZE
      IF(IP.LE.NXS)THEN
        IQ1=IQ
        BASE=(IP-1)*(2*F2+1)
      ELSE
        IQ1=IQ-NYS
        BASE = NXS* (2*F2+1) +NYS* (2*NZE+1) + (IP-1-NXS) * (2*F1+1)
      ENDIF
C---------- simply step around the element to number it ----------------
C     PRINT*,BASE,F1,F2
      NUM( 6)=BASE+ (2*NZE+1)*(IQ1-1)+ IS
      NUM( 2)=NUM( 6)+ 1
      NUM(13)=NUM( 2)+ NZE
      NUM( 5)=NUM(13)+ NZE
      NUM( 1)=NUM( 5)+ 1
      NUM(12)=NUM( 2)+ F1
      NUM(10)=NUM(12)+ NZE
      NUM( 9)=NUM(10)+ 1
      NUM(11)=NUM( 9)+ NZE
      NUM( 8)=NUM(12)+ F1
      NUM( 4)=NUM( 8)+ 1
      NUM(14)=NUM( 4)+ NZE
      NUM( 7)=NUM(14)+ NZE
      NUM( 3)=NUM( 7)+ 1
C -------- set the 4 x-values of the 'trapezoidal' faces -----
      IF(IQ.LE.NYS)THEN
        FAC1=(BOT(IP  )-TOP(IP  ))/ (DEPTH(NYS+1)-DEPTH(1))
        FAC2=(BOT(IP+1)-TOP(IP+1))/ (DEPTH(NYS+1)-DEPTH(1))
        COORD( 2,1)=TOP(IP  )+(DEPTH(IQ+1)-DEPTH(1))*FAC1
        COORD( 1,1)=TOP(IP  )+(DEPTH(IQ  )-DEPTH(1))*FAC1
        COORD( 4,1)=TOP(IP+1)+(DEPTH(IQ  )-DEPTH(1))*FAC2
        COORD( 3,1)=TOP(IP+1)+(DEPTH(IQ+1)-DEPTH(1))*FAC2
      ELSE
        COORD( 2,1)=BOT(IP)
        COORD( 1,1)=BOT(IP)
        COORD( 4,1)=BOT(IP+1)
        COORD( 3,1)=BOT(IP+1)
      ENDIF
      COORD( 1,2)=DEPTH(IQ+1)
      COORD( 2,2)=DEPTH(IQ  )
      COORD( 1,3)=BRDTH(IS+1)
      COORD( 5,3)=BRDTH(IS  )
C --------  copy  to nodes of the same x,y,z ------
      DO I=5,8
        COORD( I,1)=COORD(I-4,1)
      ENDDO
      COORD( 3,2)=COORD( 1,2)
      COORD( 5,2)=COORD( 1,2)
      COORD( 7,2)=COORD( 1,2)
      COORD( 4,2)=COORD( 2,2)
      COORD( 6,2)=COORD( 2,2)
      COORD( 8,2)=COORD( 2,2)
      COORD( 2,3)=COORD( 1,3)
      COORD( 3,3)=COORD( 1,3)
      COORD( 4,3)=COORD( 1,3)
      COORD( 6,3)=COORD( 5,3)
      COORD( 7,3)=COORD( 5,3)
      COORD( 8,3)=COORD( 5,3)
C--------------- put face nodes at the 'mid-face' ---------------
      DO I=1,6
        DO J=1,3
          SUM=0
          DO K=1,4
            SUM = SUM+COORD(IF(I,K),J)
          ENDDO
          COORD(IFN(I),J) = SUM/4.
        ENDDO
      ENDDO
      RETURN
      END
c-----------------------------------------------------------------------


C-----------------------------------------------------------------------
C **** This is the BIG one ! ******
C-----------------------------------------------------------------------
      SUBROUTINE R_DANBLOCKS (IO,IPR,GC,IGC,NDIM,NN,NUMS,INUMS,NEL,P)
C
C     This is the KEYWORD-driven entry-point to DANBLOCKS (q.v.)
C     10-3-94 .. split-off form DANBLOCKS itself
C
      PARAMETER (Iwidths=5)       !- Max DOF = 5d (!)

      REAL      GC (IGC,*)        !- Nodal coord database
     &     ,WIDTHS (IWIDTHS,0:99) !- input sub-element widths

      INTEGER  NUMS (INUMS,*)     !- Element database
     &           ,P (*)           !- node pointers (unused)
     &         ,NXE (IWIDTHS)     !- number of sub-elements in each direction

      DO ILOOP = 1,9999
    1   READ (IO,*,IOSTAT=IOS) JMAT , NDIME_NEW,NOD_NEW,ITYPE_new  
        CALL IN_TEST (IO,IOS,*1,*999)

        DO J=1,NDIME_NEW          !- read the edge spacings
    2     READ (IO,*,IOSTAT=IOS)   NXE(J),(WIDTHS(J,I),I=1,NXE(J))
          CALL IN_TEST (IO,IOS,*2,*999)   
        ENDDO            

c-------- special case for logarithmic distributions --------
c  15-6-97 if any WIDTHS(J,1) is -ve (perhaps) then we treat this as
c    the ratio of the first to last element: so typicaly -2. will make 
C    the last half the width of the first.
c     -- finished and tested 31-1-98

        DO J=1,NDIME_NEW
          IF (WIDTHS(J,1).LT.0.) THEN
            NPTS = NXE(J) !-1
            FACT = abs(WIDTHS(J,1)) 
            RATIO =  exp ( LOG(FACT) /REAL(NPTS-1)  )
            print*,'<> Applying a logarithmic ratio=',ratio
            wid= 1.
            DO I=1,NPTS
              WIDTHS(J,I) = wid
              wid = wid*ratio
            ENDDO
          ENDIF
        ENDDO

c------ translate to *cummulative* widths -------
        DO J=1,NDIME_NEW  
          WIDTHS(J,0) = 0.    !- set the first coord in the list to zero.
          DO I=1,NXE(J)    
            WIDTHS(J,I) = WIDTHS(J,I) + WIDTHS(J,I-1)
          ENDDO
        ENDDO

c-------------- perform the subdivisions -----------------
        NN_OLD  = NN
        NEL_OLD = NEL
        CALL DANBLOCKS_old (IPR,GC,IGC,NDIM,NN,NUMS,INUMS,NEL, 
     &     JMAT,NDIME_NEW,NOD_NEW,ITYPE_new,  NXE,WIDTHS,IWIDTHS )
c .. cf the newer more modular DANBLOCKS_NEW

        IF (IPR.GE.2)
     &  WRITE(*,'(I4,A,I7,A,I7,A)')
     &   ILOOP,' : ', NEL-NEL_OLD,' elements and'
     &     , NN-NN_OLD,' nodes created'
       ENDDO    
  999 CONTINUE
      RETURN
      END            

c-----------------------------------------------------------------------
      SUBROUTINE CHANGE_NDIM (GC,IGC,NDIM,NN,NDIM_NEW)
c
c     This sets the number of dimensions of the model
c        Dan Kidger 8-2-98
c     If any nodes already exist then they are promoted (eg set their z=0.)
c     This of course can also be used to squash back to NDIM-- too.
c
      INTEGER IGC, NDIM,NN,NDIM_NEW
      REAL GC (IGC,*)

      IF (NDIM_NEW.GT.NDIM) THEN
        DO I=1,NN
          DO J=NDIM+1,NDIM_NEW
            GC(J,I) = 0.
          ENDDO
        ENDDO
      ENDIF
      NDIM = NDIM_NEW  
      RETURN
      END

c-----------------------------------------------------------------------
      SUBROUTINE DANBLOCKS_OLD (IPR,GC,IGC,NDIM,NN,NUMS,INUMS,NEL, 
     &     JMAT,NDIME_NEW,NOD_NEW,ITYPE_new,  NXE,WIDTHS,IWIDTHS )
C
C     This will 'condense' all nodes that share the same position
C     using FIND_NODE to get the node number of a node
C     3-4-93  now can fill 8nq's with 20nb !!  :-)
C     ... however first we need to set the z-dimensions to zero
C     ... then upadte NDIM to 3
C---> really there should be a 'daughter' routine where we just
C---- handle one element at a time
c... note that WIDTHS should be cummulative (but no need to normalize)
C
C    Input as :  the usual mesh (GC and NUMS)
C              and
C      JMAT : only do this material-type (230964=wildcard)
C      NDIME_NEW,NOD_NEW,ITYPE_new  = the new element type (8nq = 2,8,1)
C      NXE() hold the number of new elements in each NDIME_NEW direction
C      WIDTHS(,) holds the actual spacings of NXE()

c     10-7-96 I need to resolve a problem when we have 2d plane-elements
c             embedded in a 3d-space (as aero guy's turbine-mesh)
c             here NDIM is already 3 so no morph_to_3d.
c     10-7-96 It is time I could do quads->2*triangles here (as Yau can do)

      PARAMETER (MAX_NOD = 32,MDIM=5)
      PARAMETER (ICOORD= MAX_NOD,IDER=MDIM)

      REAL      GC (IGC,*)        !- Nodal coord database
     &     ,WIDTHS (iwidths,0:99) !- input sub-element widths
     &  ,COORD_OLD (ICOORD,MDIM)  !- coords of the parent element
     &    ,COORD_L (ICOORD,MDIM)  !-  local coords of the daughter elements
     &    ,COORD_G (MDIM)         !- global coords of a daughter node
     &       ,SAMP (1,MDIM)       !- sampling point of the 'new' nodes
     &        ,FUN (MAX_NOD)      !- shape functions of the parent
     &        ,DER (IDER,MAX_NOD) !- sf derivs of the parent (unused)
     &     ,NORMAL (MDIM)         !- normal vector (when extruding)
     &        ,JAC (MDIM,MDIM)    !- set of direction vectors

      INTEGER  NUMS (INUMS,*)     !- Element database
     &     ,NUM_OLD (MAX_NOD)     !- node numbers of the parent elem
     &         ,NUM (MAX_NOD)     !- node numbers of the new elements
     &           ,L (MDIM)        !- toggles for the sub-directions
     &         ,NXE (iwidths)     !- number of sub-elements in each direction

      LOGICAL L_TRI_MUM !, L_TRI_KID

c----------- 1: Go into '3d'  if we were only in '2d' before :-) ----------
c < 2d elems in 3-space are unaffected >
c .. this should be a seperate subroutine
c  ... we just set the 'z' of all old nodes to zero.
        NDIM_OLD = NDIM              !- remember if we are changing 2D->3D
c  .. we remember NDIM_OLD so that we distinguish betweennew coord is 
c     local (for interpolation) or a new global coord directly.

      IF (NDIME_NEW.GT.NDIM)
     &CALL CHANGE_NDIM (GC,IGC,NDIM,NN,NDIME_NEW)
c          DO I=1,NN
c            DO J=NDIM+1,NDIME_NEW
c              GC(J,I) = 0.
c            ENDDO
c          ENDDO
c          NDIM = NDIME_NEW           !- update (ie. if a 20nb pushes us into 3D)
c        ENDIF

c---------- 2: Get the local coords of the new element type ------------
C.. and normalise to 0.->1. so easier to handle later (cf quads+tris)
c.. as a daughter routine is nicest - just return COORD_L
      CALL WTHATN (NOD_NEW,NDIME_NEW,ITYPE_new,COORD_L,ICOORD)
      DO J=1,NDIME_NEW
        CMAX = COORD_L(1,J)
        CMIN = COORD_L(1,J)
        DO I=2,NOD_NEW                      ! get min/max
          CMAX = MAX (CMAX,COORD_L(I,J))
          CMIN = MIN (CMIN,COORD_L(I,J))
        ENDDO
        DO I=1,NOD_NEW                      ! scale to 0.->1.
          COORD_L(I,J) = (COORD_L(I,J)-CMIN)/ (CMAX-CMIN)
        ENDDO
      ENDDO
c      do i=1,nod_new
c        write(*,'(i4, 3f8.4)') i,  (coord_l(i,j),j=1,ndime_new)
c      enddo
c.... note if all CMIN are =0. then we have a triangle :-)

C----------------- 3: loop the 'original' elements ---------------------
      NEL_NEW  = NEL      !- new element numbers (incremented)
      NN_NEW   = NN       !- new node numbers (updated)
      IC = 0
      DO IEL = 1,NEL
        IF (IPR.GE.3) CALL PR_EL_COUNT ('Elem',IEL,NEL,IPR)
        CALL GET_ELEMENT (NUMS,INUMS,IEL, NUM_OLD, 
     &          NOD_OLD,NDIME_OLD,ITYPE_OLD,IMAT,IUSER1,IUSER2,IUSER3)

        IF ((IMAT.le.0).or.(JMAT.ne.IMAT.and.JMAT.ne.230964)) GOTO 44   ! CYCLE
c       IF (JMAT.ne.IMAT.and.JMAT.ne.230964) GOTO 44    ! CYCLE
c.. now CALL a subdivide this element?
c   pass it: NUMS and GC : to put the new elems in.
c     COORD_L(,) : the 0.-1. local coords of the daughters
c    NXE(),WIDTHS() : the Number and sizes of the daughters
c

c---------- Automatic Choice of New Element Type ---------
c  14-9-98  for example a mixed mesh of 3nt and 4nqs - can I :
c    1: do 4nq->8mq but 3nt->6nt
c or 2: do 4nq->8nb and 3nt ->6np  if NOD_NEW = 0
c
      if (nod_new.eq.0) then
c.. so every element 'knows' what is next in the promotion change 4-8-12-17
c  (demotion likewise) also 3d equivalents 4->8, 8-> 14 (or 20?)

      endif

c---------------- Ok now sub-divide this parent element ----------------
          IC = IC + 1 !- count of new elements formed
          CALL PUT_ELEMENT (NUMS,INUMS,IEL, NUM_OLD, NOD_OLD,NDIME_OLD
     &      ,ITYPE_OLD  ,-1,IUSER1,IUSER2,IUSER3)    !- just kill the parent

c----------- 1: Get the coords of the parent ------------
          CALL GET_COORD (NUM_OLD,NOD_OLD,NDIM,GC,IGC,COORD_OLD,ICOORD)

c--------- 2: set up the indeces to loop ALL daughter elements ---------
c... hmm. note that triangle subdivision is not quite the same!
c.. because all NXE(:) should be the same, this is still valid
          N_SUB_EL = 1
          DO IPOS_L=1,NDIME_NEW
            L(IPOS_L) = 0                      !-- null the pointers
            N_SUB_EL = N_SUB_EL * NXE(IPOS_L)  !-- total # new-elems
          ENDDO

c < loop the in-plane basic squares >
c   < loop the 2 triangles within a 4nq  (cf 5 tets in an 8NB>
c     < loop the 3d-projections >
c      (note N_SUB_EL already has N_SUB_EL allowed for

c------ if quads->tri's then mung N_SUB_EL --------
c.. if 4nq->3nt then effectively have 2* as many to do
c.. if 8nb->4ntet then have 5* as many to do
c   so for each need to loop the munged 'mirror's of the daughters
c   IF (QUAD->TRI) N_SUB_EL = N_SUB_EL *2
c   16-2-97 maybe we should 'pretend that we are doing a quad then split later?


C------------- 3: Loop the in-plane daughter elements ------------------
c ( then inside loop the 'new' dimension )
          DO I_SUB_EL = 1,N_SUB_EL
      
c----------------- 4: Loop the nodes of this daughter ------------------
c..  Now get the local (hence global) coords of the new nodes                   
c.. Note: if the parent and child are of the same family (eg 4nq and 8nq)
c..    then it doesn't matter if WIDTHS is set to -1->1 or 0.->1 !
c..    .. now try and explain why ??!!
          DO INODE = 1,NOD_NEW

c OK ... SAMPLING-PONT in locals =
c       the start-point (its fractional posn.) 0.->1. hence -1-> +1
C       plus the frac. width of this piece 0.->1. hence -1-> +1
c       times the 'local' posn within this piece -1->+1 hence 0.->1.

C NO! need to break down. .. if 4nq->3nt then morph the 2 daughters
c ie. by munging COORD_L into its 'up' and 'down' forms.
c     if 3nt->tri's need to use the code as in DANPLOT's sub-facets

c              SAMP(1,J) =     
c     &          WIDTHS(J,L(J))/WIDTHS(J,NXE(J))    !- base 
c     &   +  (WIDTHS(J,L(J)+1)-WIDTHS(J,L(J)) )     !- width
c     &           / WIDTHS(J,NXE(J))
c     &       * COORD_L(INODE,J)                    !- * offset

c--- form the local x,y,z of this new node. ---

c------------- Parent a Quadrilateral? Subdivision ---------------
            DO J=1,NDIME_NEW
              xbase = WIDTHS(J,L(J))/WIDTHS(J,NXE(J)) 
              deltax= (WIDTHS(J,L(J)+1)-WIDTHS(J,L(J)) ) 
     &                 /    WIDTHS(J,NXE(J))
              xlocal= coord_l(inode,j)
              SAMP(1,J) = xbase + deltax * xlocal
            ENDDO

c  < 16-2-97 if morphing quads-> tri's it is here that we sub-loop (2)
c     and for each: transform the 0>1. for an 'up' and a down' (geddit?) 
c  if 2d then just:
c          samp(1,J) = 1.-SAMP(1,J)
c          samp(2,J) = 1.-SAMP(2,J)  ! ie. 2 reflections.
c    for the 2nd (fisrt is unchanged)
c   >>> 8nb->4tets is:
C           1* natural; 3* double reflects + 1* ??
c

c.. morph local to -1>+1 (only for direction in the parent that are  -1>1
c.. > so sampling in a 6nw x,y are 0.>1, but 'z' in -1>+1.
            DO J=1,NDIME_NEW
              SAMP(1,J) = SAMP(1,J)  * 2. - 1.     !*NOT* for triangle-parents
            ENDDO

c-------------- Parent a triangle? Subdivision ------------------
c.. patch (does this work always?)
c.. note that for tri's nxe must = nye (but spacings may differ)
c.. 16-2-97 What about tetrahedra?

c.. 16-2-97 if 6nw's then i_sub_el is not true! should be just the
c    in-plane sub-el # .. ie. mod (i_sub_el,nze)+1 out of nxe*nye
c-- best to get the element code and use that?
      L_TRI_MUM = (ndime_old.eq.2). and. (nod_old.eq.3.or.nod_old.eq.6
     &   .or. nod_old.eq.10 .or. nod_old.eq.15) 
      if (L_TRI_MUM) then
        nx = nxe(1)                             
        ix = int(sqrt(real(i_sub_el-1)+ .01) +1)     !- as 0 -> (nxe-1)
c       iy = nx - (ix*ix-i_sub_el)/2 +1
        iy = nx - (ix*ix-i_sub_el)/2 
        ix = nx+1-ix      !- invert so ix=1, iy=1 is the bottom LH corner
        iy = nx+1-iy

c        if (inode.eq.1)     !- debug
c     &  print*,'i_sub_el=',i_Sub_el, ' ix,iy=',ix,iy
        xbase = WIDTHS(1,ix-1) /WIDTHS(1,nx)   !- NXE(1) must = NXE(2) here
        ybase = WIDTHS(2,iy-1) /WIDTHS(2,nx) 
        deltax= (WIDTHS(1,ix)-WIDTHS(1,ix-1) ) / WIDTHS(1,NX)
        deltay= (WIDTHS(2,iy)-WIDTHS(2,iy-1) ) / WIDTHS(2,NX)
        xlocal= coord_l(inode,1)
        ylocal= coord_l(inode,2)

        IF (MOD(I_SUB_EL,2).EQ.MOD(nx+1-IX,2)) THEN   ! an 'up' pointing triangle
          SAMP(1,1) =  xbase + deltax*(xlocal) 
          SAMP(1,2) =  ybase + deltay*ylocal 
        ELSE
          SAMP(1,1) =  xbase + deltax*(1.-xlocal)  
          SAMP(1,2) =  ybase + deltay*(1.-ylocal) 
        ENDIF

c        SAMP(1,1) = 1.- SAMP(1,1)    !- invert 'cos LH node has local
c        SAMP(1,2) = 1.- SAMP(1,2)    !- coord=1 (cf -1 to +1 of a quad)
c        SAMP(1,2) = 1.- SAMP(1,2)-samp(1,1)

      ENDIF    !- triangles

c--------- 5: So sample to get the 'Global' cord of this node ----------
c            write(*,'(i4,a,3f8.3,a)')
c     &   inode, ' samp=(', samp(1,1),samp(1,2),samp(1,3), ' )'
c--- 5a: sample in the plane of the parent element ---
            CALL GSF (NDIME_OLD,NOD_OLD,ITYPE_OLD ,DER,IDER,FUN
     &          ,SAMP,1,1)
            DO J=1,NDIM
              COORD_G(J) = 0.         !- ie. MTV_MULT  :-)
              DO K=1,NOD_OLD
                COORD_G(J) = COORD_G(J) + FUN(K) * COORD_OLD(K,J)
              ENDDO   !- nodes
            ENDDO   !- the dimensions of the new element

C----------- 5b: extrude into '3d' if desired ------------------
C if the new element has more dimensions than the parent.
            IF (NDIME_NEW.GT.NDIME_OLD) THEN
              IF (NDIME_NEW.gt.NDIM_OLD) THEN 
c               -- simply project the 'z'-coord
c hmm what if I going directly from 1d to 3d
c                do j=1,ndim
c                  normal(j) =0.
c                enddo
c                normal(ndim)=1.
                DO J=NDIM_OLD+1,NDIM
                  COORD_G(J) = 
     &            WIDTHS(J,L(J))                           !- base 
     &        + ( WIDTHS(J,L(J)+1) - WIDTHS(J,L(J)) )      !- width
     &          * COORD_L(INODE,J)                         !- * offset
                ENDDO

              ELSE
c--------- 5c: if projecting within the current space. ---
c.. here if - adding tunnel lining (1d->2d in 2d space)
c     or    - thickening plates (2d->3d in 3d space)
c-- NDIME_NEW=NDIM_OLD=NDIM_NEW, just that NDIME_NEW>NDIME_OLD
c  Method:
c    1: get the vector along the line (2 vectors in a plane)
c    2: hence get the outward normal vector
c    3: to a unit vector
c    4: so get 'elevation'  vector
c    5: add this to GOORD_G()

      HEIGHT =    WIDTHS(NDIM,L(NDIM))                           !- base 
     &        + ( WIDTHS(NDIM,L(NDIM)+1) - WIDTHS(NDIM,L(NDIM)) )      !- width
     &          * COORD_L(INODE,NDIM)                         !- * offset

c----- get the vectors in the direction(s) of the parent -----
c.. hmm shouldn't we init JAC to a unit vector?
        DO I=1,NDIME_OLD
          DO J=1,NDIM
            X=0.0               ! explcit MATMUL
            DO K=1,NOD_OLD
              X=X+DER(I,K)*COORD_OLD(K,J)
            ENDDO
            JAC(I,J)=X
          ENDDO
        ENDDO

c----- get the normal vector -----
c.. 31-1-98 How on earth do I make this general ?
        if (ndim.eq.2) then
          normal(1) = -jac(1,2)
          normal(2) = jac(1,1)
        elseif (ndim.eq.3) then
          normal(1) = JAC(1,2)*JAC(2,3) - JAC(2,2)*JAC(1,3)
          normal(2) = JAC(1,3)*JAC(2,1) - JAC(2,3)*JAC(1,1)
          normal(3) = JAC(1,1)*JAC(2,2) - JAC(2,1)*JAC(1,2)
        elseif (ndim.eq.4) then     ! ???
        endif
c---- scale to a unit vector ----
        zzz=0.
        do j=1, ndim 
          zzz= zzz+ normal(j)**2
        enddo
        zzz=sqrt(zzz)
        do j=1, ndim 
          normal(j)=normal(j) / zzz
        enddo
C.. add this height
         DO J=1,NDIM
           COORD_G(J) = COORD_G(J) + NORMAL(J) * HEIGHT
         ENDDO 

              ENDIF

            ENDIF

               
c.. maybe now I can return to the calling routine the COORD of the daughter?

c----------------- build the nodes of the new element ------------------ 
c... 18-10-95 Just calling FIND_OR_ADD_NODE is very wasteful.
c..  better to try the parent element first , *then* try rest_of_mesh    
c..  (eg if 20nb->8nb all will be found this way)

c.. so scan local coords of parent to each new node. 
c  (( really no reason why I can't use globals I guess ))
c.. if a hit, we have JNODE.. if no hit try FIND_OR_ADD_NODE

      JNODE = 0
      DO I=1,NOD_OLD         !- loop the parent
c       DO J=1,NDIME_NEW
        DO J=1,NDIM
          IF (ABS(COORD_G(J)-COORD_OLD(I,J)).GT.0.0001) GOTO 91    != F90 CYCLE
        ENDDO
        JNODE = NUM_OLD(I)
        goto 92            !-- OK we have found the right node
   91   CONTINUE   !-- carry on looking
      ENDDO
C---- failed to find so just go to FIND_OR_ADD_NODE now

            CALL FIND_OR_ADD_NODE (GC,IGC,NN_new,NDIM,COORD_G,
     &        1,NN_NEW,JNODE)

   92       NUM(INODE) = JNODE

c-----------------------------------------------------------------------
          ENDDO     !-- (loop nodes of the new element)

c--------------------- Add-in the new element ---------------------------
          IF (I_SUB_EL.EQ.1) THEN
            IEL_NEW = IEL                !- so replace the old one first
          ELSE
            NEL_NEW = NEL_NEW + 1        !- then build the new elements
            IEL_NEW = NEL_NEW            !- at the end of the list
          ENDIF

          CALL PUT_ELEMENT 
     &    (NUMS,INUMS,IEL_NEW, NUM, NOD_NEW,NDIME_NEW,ITYPE_NEW, IMAT,
     &     IUSER1,IUSER2,IUSER3)

C------------- end of this new element .. increment pointers -----------
c.. If I am looping the 2 tris in a 4nq (5tets in a 8nb) then just loop back 

          IPOS_L = NDIME_NEW
  123     L(IPOS_L) = L(IPOS_L) + 1               !-- increment
          IF (L(IPOS_L) .GE.NXE(IPOS_L)) THEN     !-- the pointers
            L(IPOS_L) = 0
            IPOS_L = IPOS_L -1
            IF (IPOS_L.GT.0) GOTO 123             !-- next toggle
          ENDIF
          ENDDO           !-- the new elements

c   now RETURN to the driving routine and do the next element.
c------------------------------------------------------------------
  44    CONTINUE       !-- only this material
      ENDDO         !-- original element loop
      NEL = NEL_NEW
      NN  = NN_NEW
c .. now print a summary?
      RETURN
      END
C-------------------- end of DANBLOCKS routine -------------------------

c-----------------------------------------------------------------------
      SUBROUTINE DANBLOCKS (IPR,GC,IGC,NDIM,NN,NUMS,INUMS,NEL, 
     &     JMAT,NDIME_NEW,NOD_NEW,ITYPE,  NXE,WIDTHS,IWIDTHS )
C
C     This will 'condense' all nodes that share the same position
C     using FIND_NODE to get the node number of a node
C     3-4-93  now can fill 8nq's with 20nb !!  :-)
C     ... however first we need to set the z-dimensions to zero
C     ... then upadte NDIM to 3
C---> really there should be a 'daughter' routine where we just
C---- handle one element at a time
c... note that WIDTHS should be cummulative (but no need to normalize)
C
C    Input as :  the usual mesh (GC and NUMS)
C              and
C      JMAT : only do this material-type (230964=wildcard)
C      NDIME_NEW,NOD_NEW,ITYPE  = the new element type (8nq = 2,8,1)
C      NXE() hold the number of new elements in each NDIME_NEW direction
C      WIDTHS(,) holds the actual spacings of NXE()

c     10-7-96 I need to resolve a problem when we have 2d plane-elements
c             embedded in a 3d-space (as aero guy's turbine-mesh)
c             here NDIM is already 3 so no morph_to_3d.
c     10-7-96 It is time I could do quads->2*triangles here (as Yau can do)


      PARAMETER (MAX_NOD = 32,MDIM=4)
      PARAMETER (ICOORD= MAX_NOD)
      REAL      GC (IGC,*)        !- Nodal coord database
     &     ,WIDTHS (iwidths,0:99) !- input sub-element widths
c    &    ,COORD_L (ICOORD,3)     !-  local coords of the daughter elements

      INTEGER  NUMS (INUMS,*)     !- Element database
     &     ,NUM_OLD (MAX_NOD)     !- node numbers of the parent elem
     &         ,NXE (iwidths)     !- number of sub-elements in each direction

      LOGICAL L_TRI_MUM !, L_TRI_KID

c----------- 1: Go into '3D'  if we were only in '2D' before :-) ----------
c < 2d elems in 3-space are unaffected >
c .. this should be a seperate subroutine
c  ... we just set the 'z' of all old nodes to zero.
c  .. we remember NDIM_OLD so that we distinguish betweennew coord is 
c     local (for interpolation) or a new global coord directly.

c = make at least n-dimensions (by padding with zeros)
        NDIM_OLD = NDIM              !- remember if we are changing 2D->3D
        NDIM = NDIME_NEW             !- update (ie. if a 20nb pushes us into 3D)

        IF (NDIME_NEW.GT.NDIM) THEN
          DO I=1,NN
            DO J=NDIM+1,NDIME_NEW
              GC(J,I) = 0.
            ENDDO
          ENDDO
        ENDIF


C-------------------- 3: loop the parent elements ----------------------
      NEL_NEW  = NEL      !- new element numbers (incremented)
      NEL_OLD  = NEL      !- original #
      NN_NEW   = NN       !- new node numbers (updated)
      NN_OLD   = NN       !- original #
      IC = 0
      DO IEL=1,NEL_OLD 
        IF (IPR.GE.3) CALL PR_EL_COUNT ('Elem',IEL,NEL,IPR)
        CALL GET_ELEMENT (NUMS,INUMS,IEL, NUM_OLD, 
     &          NOD_OLD,NDIME_OLD,ITYPE,IMAT,IUSER1,IUSER2,IUSER3)

        IF (JMAT.EQ.IMAT.or.JMAT.eq.230964) THEN      !-- found a material

c.. now CALL a subdivide this element?
c   pass it: NUMS and GC : to put the new elemes in.
c     COORD_L(,) : the 0.-1. local coords of the daughters
c    NXE(),WIDTHS() : the Number and sizes of the daughters
c
c---------------- Ok now sub-divide this parent element ----------------
          IC = IC + 1 !- count of new elements formed
          CALL PUT_ELEMENT (NUMS,INUMS,IEL, NUM_OLD, NOD_OLD,NDIME_OLD
     &      ,ITYPE  ,-1,IUSER1,IUSER2,IUSER3)    !- just kill the parent
 
      L_TRI_MUM = (ndime_old.eq.2). and. (nod_old.eq.3.or.nod_old.eq.6
     &   .or. nod_old.eq.10 .or. nod_old.eq.15) 

          CALL DANBLOCK_1EL (IPR,GC,IGC,NDIM,NN,NUMS,INUMS,NEL,IEL, 
     &      JMAT,NDIME_NEW,NOD_NEW,ITYPE,  NXE,WIDTHS,IWIDTHS,
     &      L_TRI_MUM )
        ENDIF          !-- only this material
      ENDDO         !-- original element loop
      NEL = NEL_NEW
      NN  = NN_NEW
c .. now print a summary?
      RETURN
      END
C-------------------- end of DANBLOCKS routine -------------------------


c-----------------------------------------------------------------------
      SUBROUTINE DANBLOCK_1EL (IPR,GC,IGC,NDIM,NN,NUMS,INUMS,NEL,IEL, 
     &     JMAT,NDIME_NEW,NOD_NEW,ITYPE,  NXE,WIDTHS,IWIDTHS, 
     &     L_TRI_MUM )
c
c     This replaces an element with any number of smaller elements.
c     The daughter elements may be of a differnet type to the parent
c     < contrast this with h-refinement >
c        Dan Kidger ..15-6-97

      PARAMETER (MAX_NOD = 32,MDIM=4)
      PARAMETER (ICOORD= MAX_NOD,IDER=MDIM)

      REAL      GC (IGC,*)        !- Nodal coord database
     &     ,WIDTHS (iwidths,0:99) !- input sub-element widths
     &  ,COORD_OLD (ICOORD,MDIM)  !- coords of the parent element
     &    ,COORD_L (ICOORD,3)     !-  local coords of the daughter elements
     &    ,COORD_G (MDIM)         !- global coords of a daughter node
     &       ,SAMP (1,MDIM)       !- sampling point of the 'new' nodes
     &        ,FUN (MAX_NOD)      !- shape functions of the parent
     &        ,DER (IDER,MAX_NOD) !- sf derivs of the parent (unused)

      INTEGER  NUMS (INUMS,*)     !- Element database
     &     ,NUM_OLD (MAX_NOD)     !- node numbers of the parent elem
     &         ,NUM (MAX_NOD)     !- node numbers of the new elements
     &           ,L (MDIM)        !- toggles for the sub-directions
     &         ,NXE (iwidths)     !- number of sub-elements in each direction

      LOGICAL L_TRI_MUM !, L_TRI_KID

c-------- 1: Get the coords of the parent ------------
          CALL GET_COORD (NUM_OLD,NOD_OLD,NDIM,GC,IGC,COORD_OLD,ICOORD)

c-------- 2: Get the local coords of the new element type -----
C.. and normalise to 0.->1. so easier to handle later (cf quads+tris)
c.. maybe do this for each parent : remember that WTHATN carries its own cache.
      CALL WTHATN (NOD_NEW,NDIME_NEW,ITYPE,COORD_L,ICOORD)
      DO J=1,NDIME_NEW
        CMAX = COORD_L(1,J)
        CMIN = COORD_L(1,J)
        DO I=2,NOD_NEW                      ! get min/max
          CMAX = MAX (CMAX,COORD_L(I,J))
          CMIN = MIN (CMIN,COORD_L(I,J))
        ENDDO
        DO I=1,NOD_NEW                      ! scale to 0.->1.
          COORD_L(I,J) = (COORD_L(I,J)-CMIN)/ (CMAX-CMIN)
        ENDDO
      ENDDO
c      do i=1,nod_new
c        write(*,'(i4, 3f8.4)') i,  (coord_l(i,j),j=1,ndime_new)
c      enddo
c.... note if all CMIN are =0. then we had a triangle :-)

c------ 3: set up the indeces to loop ALL daughter elements ---------
c... hmm. note that triangle subdivision is not quite the same!
c.. because all NXE(:) should be the same, this is still valid
c.. contast this with init-ing at the end of the first pass, then
c.. looping back until done. hence handling quads->tris is maybe easier.
c   (but sometimes we might want to know the #daughters a-priori.)
          N_SUB_EL = 1
          DO IPOS_L=1,NDIME_NEW
            L(IPOS_L) = 0                      !-- null the pointers
            N_SUB_EL = N_SUB_EL * NXE(IPOS_L)  !-- total # new-elems
          ENDDO

c < loop the in-plane basic squares >
c   < loop the 2 triangles within a 4nq  (cf 5 tets in an 8NB>
c     < loop the 3d-projections >
c      (note N_SUB_EL already has N_SUB_EL allowed for

c------ if quads->tri's then mung N_SUB_EL --------
c.. if 4nq->3nt then effectively have 2* as many to do
c.. if 8nb->4ntet then have 5* as many to do
c   so for each need to loop the munged 'mirror's of the daughters
c   IF (QUAD->TRI) N_SUB_EL = N_SUB_EL *2
c   16-2-97 maybe we should 'pretend that we are doing a quad then split later?

C------------- 3: Loop the in-plane daughter elements ------------------
c ( then inside loop the 'new' dimension )
          DO I_SUB_EL = 1,N_SUB_EL
      
c----------------- 4: Loop the nodes of this daughter ------------------
c..  Now get the local (hence global) coords of the new nodes                   
c.. Note: if the parent and child are of the same family (eg 4nq and 8nq)
c..    then it doesn't matter if WIDTHS is set to -1->1 or 0.->1 !
c..    .. now try and explain why ??!!
          DO INODE = 1,NOD_NEW

c OK ... SAMPLING-PONT in locals =
c       the start-point (its fractional posn.) 0.->1. hence -1-> +1
C       plus the frac. width of this piece 0.->1. hence -1-> +1
c       times the 'local' posn within this piece -1->+1 hence 0.->1.

C NO! need to break down. .. if 4nq->3nt then morph the 2 daughters
c ie. by munging COORD_L into its 'up' and 'down' forms.
c     if 3nt->tri's need to use the code as in DANPLOT's sub-facets

c              SAMP(1,J) =     
c     &          WIDTHS(J,L(J))/WIDTHS(J,NXE(J))    !- base 
c     &   +  (WIDTHS(J,L(J)+1)-WIDTHS(J,L(J)) )     !- width
c     &           / WIDTHS(J,NXE(J))
c     &       * COORD_L(INODE,J)                    !- * offset

c--- form the local x,y,z of this new node. ---

c-------------- Parent a triangle? Subdivision ------------------
c.. patch (does this work always?)
c.. note that for tri's nxe must = nye (but spacings may differ)
c.. 16-2-97 What about tetrahedra?

c.. 16-2-97 if 6nw's then i_sub_el is not true! should be just the
c    in-plane sub-el # .. ie. mod (i_sub_el,nze)+1 out of nxe*nye

      IF (L_TRI_MUM) then              !-- triangle subdivion --
        CALL SUB_TRI (NXE(1), I_SUB_EL, WIDTHS, iwidths,SAMP,
     &  coord_l,icoord,inode )
      ELSE                             !-- Quadrilateral Subdivision --
        DO J=1,NDIME_NEW
          xbase = WIDTHS(J,L(J))/WIDTHS(J,NXE(J)) 
          deltax= (WIDTHS(J,L(J)+1)-WIDTHS(J,L(J)) ) 
     &                 /    WIDTHS(J,NXE(J))
          xlocal= coord_l(inode,j)
          SAMP(1,J) = xbase + deltax * xlocal
        ENDDO
c--- morph local to -1>+1 ----
c  (only for direction in the parent that are  -1>1
c    so sampling in a 6nw x,y are 0.>1, but 'z' in -1>+1.
        DO J=1,NDIME_NEW
          SAMP(1,J) = SAMP(1,J)  * 2. - 1.     !*NOT* for triangle-parents
        ENDDO

      ENDIF  !- local coords for diffetn parent types: tris,quads,wedges,...
c            write(*,'(i4,a,3f8.3,a)')
c     &   inode, ' samp=(', samp(1,1),samp(1,2),samp(1,3), ' )'



c--------- 5: So sample to get the 'Global' cord of this node ----------
c.. get shape fun here - hence interpolate the global coords of the

c
c   Notes on 3d-projections of shells:  14-6-97
c    - ie if NDIME_daughter > NDIME_parent
c       < dimension *reduction* would be very stranger?? >
c    - we always interpolate in the plane of the parent.
c    - but if a 3d-daughter of a 4nq - we must project at 90deg.
c      so we need the normal vector at each sampling point
c      - if NDIM=2 (=NDIM_NEW) then this is explicit?
c      - else we can use GET_NORMAL via the cross-product of 2 DER's
c      - But think about 1d elems->2d eg in creating a tunnel-lining
c

            CALL GSF (NDIME_OLD,NOD_OLD,ITYPE ,DER,IDER,FUN ,SAMP,1,1)
c.. be careful : what do we mean if we are dividing 3d shells
            DO J=1,NDIM
              IF (J.LE.NDIME_OLD) THEN
c             IF (J.LE.NDIM_OLD) THEN     !- 12-2-97 is this correct?
c------ first the 'in-plane' coordinates (eg X, Y) -------
                COORD_G(J) = 0.           !- ie. MTV_MULT  :-)
                DO K=1,NOD_OLD
                  COORD_G(J) = COORD_G(J) + FUN(K) * COORD_OLD(K,J)
                ENDDO   !- nodes
              ELSE
c----- then the 'out-of-plane' coordinates (eg Z) --------
c.. hmm for now we just get the 'local' coords output
c.. Ok now we can get the ACTUAL widths input originally
C               COORD_G(J) = SAMP(1,J)   !- but widths ?? 
                COORD_G(J) = 
     &          WIDTHS(J,L(J))                           !- base 
     &   +   ( WIDTHS(J,L(J)+1) - WIDTHS(J,L(J)) )       !- width
     &       * COORD_L(INODE,J)                          !- * offset
              ENDIF

C----------------------------------------------------------
            ENDDO   !- the dimensions of the new element
               
c.. maybe now I can return to the calling routine the COORD of the daughter?

c----------------- build the nodes of the new element ------------------ 
c... 18-10-95 Just calling FIND_OR_ADD_NODE is very wasteful.
c..  better to try the parent element first , *then* try rest_of_mesh    
c..  (eg if 20nb->8nb all will be found this way)

c.. so scan local coords of parent to each new node. 
c  (( really no reason why I can't use globals I guess ))
c.. if a hit, we have JNODE.. if no hit try FIND_OR_ADD_NODE

      JNODE = 0
      DO I=1,NOD_OLD         !- loop the parent
c       DO J=1,NDIME_NEW
        DO J=1,NDIM
          IF (ABS(COORD_G(J)-COORD_OLD(I,J)).GT.0.0001) GOTO 91    != F90 CYCLE
        ENDDO
        JNODE = NUM_OLD(I)
        goto 92            !-- OK we have found the right node
   91   CONTINUE   !-- carry on looking
      ENDDO
C---- failed to find so just go to FIND_OR_ADD_NODE now

            CALL FIND_OR_ADD_NODE (GC,IGC,NN,NDIM,COORD_G,
     &        1,NN_NEW,JNODE)

   92       NUM(INODE) = JNODE

c-----------------------------------------------------------------------
          ENDDO     !-- (loop nodes of the new element)

c--------------------- Add-in the new element ---------------------------
          IF (I_SUB_EL.EQ.1) THEN
            IEL_NEW = IEL                !- so replace the old one first
          ELSE
            NEL = NEL + 1        !- then build the new elements
            IEL_NEW = NEL            !- at the end of the list
          ENDIF

          CALL PUT_ELEMENT 
     &    (NUMS,INUMS,IEL_NEW, NUM, NOD_NEW,NDIME_NEW,ITYPE, IMAT,
     &     IUSER1,IUSER2,IUSER3)

C------------- end of this new element .. increment pointers -----------
c.. If I am looping the 2 tris in a 4nq (5tets in a 8nb) then just loop back 

          IPOS_L = NDIME_NEW
  123     L(IPOS_L) = L(IPOS_L) + 1               !-- increment
          IF (L(IPOS_L) .GE.NXE(IPOS_L)) THEN     !-- the pointers
            L(IPOS_L) = 0
            IPOS_L = IPOS_L -1
            IF (IPOS_L.GT.0) GOTO 123             !-- next toggle
          ENDIF
        ENDDO           !-- the new elements

      RETURN
      END


C-----------------------------------------------------------------------
c.. Notes on morphing quads-> tri's  16-6-97
c  < 16-2-97 if so it is here that we sub-loop (2)
c     and for each: transform the 0>1. for an 'up' and a down' (geddit?) 
c  if 2d then just:
c          samp(1,J) = 1.-SAMP(1,J)
c          samp(2,J) = 1.-SAMP(2,J)  ! ie. 2 reflections.
c    for the 2nd (fisrt is unchanged)
c   >>> 8nb->4tets is:
C           1* natural; 3* double reflects + 1* ??
C-----------------------------------------------------------------------
      SUBROUTINE SUB_TRI (NX, I_SUB_EL, WIDTHS, iwidths, SAMP
     & ,coord_L, icoord,inode  )
C
C     This returns the local coords for a sub-element within the pyramid
C     'pyramid' of the parent.
C        c. Dan Kidger 14-6-97
c
c  Notes:
C   - We assume (rightly) that the all three edges have the same #subdivisons
c   - if the 'x' and 'y' spacing are not equal - it can look a little strnage
c   19-11-97 coord_l added as an argument (FTN90/win32 found it)
C
      INTEGER NX
      REAL WIDTHS(iwidths,0:99), SAMP(1,*)
      real coord_l (icoord,*)       ! 19-11-97

c     nx = nxe(1)         !- 19-11-97 nxe is an array?                    
      ix = int(sqrt(real(i_sub_el-1)+ .01)) +1     !- as 0 -> (nxe-1)
      iy = nx - (ix*ix-i_sub_el)/2 
      ix = nx+1-ix      !- invert so ix=1, iy=1 is the bottom LH corner
      iy = nx+1-iy

c        if (inode.eq.1)     !- debug
c     &  print*,'i_sub_el=',i_Sub_el, ' ix,iy=',ix,iy
       xbase = WIDTHS(1,ix-1) /WIDTHS(1,nx)   !- NXE(1) must = NXE(2) here
       ybase = WIDTHS(2,iy-1) /WIDTHS(2,nx) 
       deltax= (WIDTHS(1,ix)-WIDTHS(1,ix-1) ) / WIDTHS(1,NX)
       deltay= (WIDTHS(2,iy)-WIDTHS(2,iy-1) ) / WIDTHS(2,NX)
       xlocal= coord_l(inode,1)
       ylocal= coord_l(inode,2)

       IF (MOD(I_SUB_EL,2).EQ.MOD(nx+1-IX,2)) THEN   ! an 'up' pointing triangle
         SAMP(1,1) =  xbase + deltax*(xlocal) 
         SAMP(1,2) =  ybase + deltay*ylocal 
       ELSE
         SAMP(1,1) =  xbase + deltax*(1.-xlocal)  
         SAMP(1,2) =  ybase + deltay*(1.-ylocal) 
       ENDIF

      RETURN 
      END

C-----------------------------------------------------------------------
      SUBROUTINE R_BISECT_ELEMENTS 
     &      (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL,P)
c
c     header to BISECT_ELEMENTS  .. just read OPCODE (IMAT too ?)
c     24-8-96 ?
c
      REAL GC(IGC,*)
      INTEGER NUMS(INUMS,*), P(*)
      do kk=1,999
    1   READ(IO,*,IOSTAT=IOS) JMAT,IOP          !- get the opcodes
        CALL IN_TEST (IO,IOS,*1,*999)
        NEL_OLD=NEL
        CALL BISECT_ELEMENTS (GC,IGC,NDIM,NN,NUMS,INUMS,NEL,P, IOP,JMAT)
        print*,nel-nel_old,' new elements formed'
      enddo
      RETURN
 999  if (kk.eq.1)
     &CALL MYERROR(1,'Missing opcodes (JMAT,IOP) in *BISECT_MESH')
      END

C-----------------------------------------------------------------------
      SUBROUTINE BISECT_ELEMENTS (GC,IGC,NDIM,NN,NUMS,INUMS,NEL,P
     &   ,IOP, JMAT)             
c
c     This just loops the elements and makes a call to bisect each.
c     eg. quads get turned in 4*3nt's etc. 
c       Dan Kidger c.1996
c
c     Note the methods: (2-7-97)
C       A/ add no more nodes - cut into (2) 3nt's
C       B/ add a centre node - cut to (4) 3nt's
C       C/ add centre and edge nodes - cut to (4) 4nq's
c     so IOP = A: 'left', 'right', 
c                 'random', 'auto'
c                 'blacks', 'whites'     !- aka a chessboard
c              B: 'centre'
c              C: 'edges'
c   also what if we let IOP = base node (1:nod) so bisect pentagons etc.?
c
      REAL GC(IGC,*)
      INTEGER NUMS(INUMS,*), P(*)

      NN_OLD  = NN           !- maybe remember this too ? (for print-out)
      NEL_OLD = NEL          !- only disect the 'original' set

      DO IEL=1,NEL_OLD
        CALL GET_EL_IMAT (NUMS,INUMS,IEL, IMAT)
        IF (JMAT.EQ.IMAT.OR.JMAT.EQ.230964) THEN  !-- If a 'good' element
          CALL BISECT_ELEMENT (GC,IGC,NDIM,NN,NUMS,INUMS,NEL,P,IOP, IEL)
        ENDIF
      ENDDO
c     print*, 'Now', nel_old,' elements and',nn_new,' nodes'
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE BISECT_ELEMENT 
     &           (GC,IGC,NDIM,NN,NUMS,INUMS,NEL,P,IOP,IEL)
C
C      < was TRAINGULARISE_MESH >
C     This will take a 4-node quad and replace it with a pair
C     of 3 node triangles (sloped like / rather than \), the first side 
C     of each element is along the sloping face              DJK 7-4-94
C
C     4-7-94 extended to 6 methods; also 8nq's->6*6nt's and 8nb->5*tet's
C     where IOP = 1:   |/|
C               = 2:   |\|
C               = 3:   alternate 1 and 2
C               = 4:   alternate 2 and 1
C               = 5:   random 1 or 2
C               = 6:   'best-fit' 1 or 2
C               = 7:   4nq to 4*3nt's    (etc.) = add central node
C                 8    (=same as 7)
c      19-5-97  = 11:  divide a n-gon into n-triangles (+centre node)
c      19-5-97  = 12:  divide a n-gon into n-quads  (+centre+edge nodes)
c      19-5-97  = 13:  to quads: special case for 6nt, 8nq's. ?
c
c      (         = 17    special cxase for 4nq's perhaps? )
c               = 21   to triangles - by adding a centre node
c               = 22   to quads     - by adding a centre node
c
c   Think about disecting 8nb's -> 6*5n-pyramids ; 5n-pyramids->2*4ntets
c                               -> 8*8nb's
c                               -> 5 (or 6) 4ntets.
c

      REAL GC(IGC,*)
      INTEGER NUMS(INUMS,*), P(*)
      REAL GET_RANDOM
      EXTERNAL GET_RANDOM

      IF (IOP.EQ.1) THEN           !-- As |/|
c... 14-7-96 I really want to just use a list of 'marked' elements
c... either by its IMAT or a pointer list (via 'mark_by_imat' or 'mark_by_box'
c       IF (NOD.ne.3) return
        CALL MUNG_3NT_LEFT (NUMS,INUMS, NEL,IEL)

      ELSEIF (IOP.EQ.2) THEN       !-- As |\|
        CALL MUNG_3NT_RIGHT (NUMS,INUMS, NEL,IEL)

      ELSEIF (IOP.EQ.3) THEN       !-- Alternate |\| and |/|
        IF (MOD(IEL,2).eq.0) THEN    
          CALL MUNG_3NT_LEFT (NUMS,INUMS, NEL,IEL)
        ELSE
          CALL MUNG_3NT_RIGHT (NUMS,INUMS, NEL,IEL)
        ENDIF

      ELSEIF (IOP.EQ.4) THEN       !-- Alternate |/| and |\|
        IF (MOD(IEL,2).eq.1) THEN     !(was .EQ.0 14-7-96)
          CALL MUNG_3NT_LEFT (NUMS,INUMS, NEL,IEL)
        ELSE
          CALL MUNG_3NT_RIGHT (NUMS,INUMS, NEL,IEL)
        ENDIF

      ELSEIF (IOP.EQ.5) THEN       !-- Random  |\| or |/|
        IF (GET_RANDOM().GT.0.5) THEN
          CALL MUNG_3NT_LEFT (NUMS,INUMS, NEL,IEL)
        ELSE
          CALL MUNG_3NT_RIGHT (NUMS,INUMS, NEL,IEL)
        ENDIF

      ELSEIF (IOP.EQ.6) THEN       !-- 'best-fit' |\| or |/|
c... get this element, hence its coords, and so do some maths to get
c... ?the 2 diagonals?, then choose the min. to give either LH and RH split
        CALL MUNG_3NT_LEFT (NUMS,INUMS, NEL,IEL)

      ELSEIF (IOP.EQ.7) THEN       !-- 4 tri's around a new central node  
        CALL MUNG_3NT_CENTRE (GC,IGC,NDIM,NN,NUMS,INUMS,IEL,NEL,P)

c.. here we add a centre node and also edge nodes.
      ELSEIF (IOP.EQ.17) THEN       !-- 4 4nq's around a new central node  
c       CALL MUNG_3NT_CENTRE (GC,IGC,NDIM,NN,NUMS,INUMS,IEL,NEL,P)

      ELSEIF (IOP.EQ.8) THEN       !-- turn both 4nq's and 3nt's to sub-quads
c        if (nod.eq.3) then
          CALL MUNG_3NT_CENTRE (GC,IGC,NDIM,NN,NUMS,INUMS,IEL,NEL,P)
c        else
c         same as DANBLOCKS for a 4nq but explicit.
c         CALL MUNG_4NQ_CENTRE (GC,IGC,NDIM,NN,NUMS,INUMS,IEL,NEL,P)
c        endif

      ELSEIF (IOP.EQ.21) THEN     
        CALL SPLIT_TO_TRIS (GC,IGC,NDIM,NN,NUMS,INUMS,IEL,NEL,P)

      ELSEIF (IOP.EQ.22) THEN     
        CALL SPLIT_TO_QUADS (GC,IGC,NDIM,NN,NUMS,INUMS,IEL,NEL,P)


      ELSE
       CALL MYERROR (1,'unknown opcode in BISECT_ELEMENT')

      ENDIF

      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE SPLIT_TO_TRIS (GC,IGC,NDIM,NN,NUMS,INUMS,IEL,NEL,P)
c
c     Divides elements into 'triangles' (pyramids in 3d) by 
c     building triangles from the edges to a new centroid node.
c      cf. SPLIT_TO_QUADS
c       c. Dan Kidger 2-7-97
c

      REAL GC(IGC,*) !, XYZ(5)
      INTEGER NUMS (INUMS,*)      ! elements
     &          ,P (*)            ! # of elems per node (workspace)
     &        ,NUM (32)           ! nodes of an element
     &       ,NUM2 (32)           ! nodes on a facet- hence daughter elem

      CALL GET_ELEMENT (NUMS,INUMS,IEL, NUM, NOD,NDIME,ITYPE
     &      ,IMAT,IUSER1,IUSER2,IUSER3)

c----- 1: Form the new centroidal node -----
      NN = NN + 1     
      DO J=1,NDIM
        X = 0.
        DO I=1,NOD                   !- create the new node
          X = X + GC (J,NUM(I))
        ENDDO
        GC(J,NN) = X / real(nod)     !- store the mean coord
      ENDDO
c     NUM(NOD+1) = NN    !- add its new node ???


c----- 2: 1D elements -----
c   simply form 2 lines? (parent should be a 3nl tho!)
      IF (NDIME.EQ.1) THEN
        DO I=1,2
          NUM2(1) = NUM(I)
          NUM2(2) = NN          !- the new centre node.
          NOD2 = 2
          IEL2 = NEXT_ELEM (I,IEL,NEL)
          CALL PUT_ELEMENT (NUMS,INUMS,IEL2, NUM2,NOD2,NDIME,ITYPE 
     &      ,IMAT,IUSER1,IUSER2,IUSER3)
        ENDDO

c----- 2: 2D elements -----
c... loop edges 
      ELSEIF (NDIME.EQ.2) THEN
        DO i=1,nod
          J = mod(I,NOD)+1
          NUM2(1) = NUM(I)
          NUM2(2) = NUM(J)
          NUM2(3) = NN          !- the new centre node.
          NOD2 = 3
          IEL2 = NEXT_ELEM (I,IEL,NEL)
          CALL PUT_ELEMENT (NUMS,INUMS,IEL2, NUM2,NOD2,NDIME,ITYPE 
     &      ,IMAT,IUSER1,IUSER2,IUSER3)
        ENDDO

      ELSEIF (NDIME.EQ.3) THEN
c----- 3: Count and hence Loop the new faces ------
      CALL GET_FACE (NUM,NOD,NDIME,ITYPE
     &                ,NFACES,NUM2,NN_F,NN_FT, 1 )     !- just NFACES
      DO IFACE=1,NFACES         ! loop this element's faces

        CALL GET_FACE (NUM, NOD,NDIME,ITYPE, IFACE,NUM2,NN_F,NN_FT, 2)
        NOD2 = NN_F+1
        NUM2(NOD2) = NN         !- the 'extra' node.
        IEL2 = NEXT_ELEM (IFACE,IEL,NEL)
        CALL PUT_ELEMENT (NUMS,INUMS,IEL2, NUM2,NOD2,NDIME,ITYPE 
     &    ,IMAT,IUSER1,IUSER2,IUSER3)
      ENDDO

      ENDIF    !- 2d/3d elements
c-------------------

      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE SPLIT_TO_QUADS (GC,IGC,NDIM,NN,NUMS,INUMS,IEL,NEL,P)
c
c     Divides elements into 4nq's (? in 3d) by 
c     building quads from the corner nodes to a new centroid node.
c     via 2 new mid edge nodes.
c     cf. SPLIT_TO_TRIS
c        c. Dan Kidger 2-7-97
c
      REAL GC(IGC,*), XYZ(5)
      INTEGER NUMS (INUMS,*)      ! elements
     &          ,P (*)            ! # of elems per node (workspace)
     &        ,NUM (32)           ! nodes of an element
     &       ,NUM2 (32)           ! nodes on a facet- hence daughter elem
     &       ,NUM3 (32)           ! the new mid-edge nodes

      CALL GET_ELEMENT (NUMS,INUMS,IEL, NUM, NOD,NDIME,ITYPE
     &      ,IMAT,IUSER1,IUSER2,IUSER3)
      IF (NDIME.NE.2) RETURN  !- only valid for planar elements

c----- 1: Form the new centroidal node -----
      NN_BASE = NN
      NN = NN + 1     
      DO J=1,NDIM
        X = 0.
        DO I=1,NOD                   !- create the new node
          X = X + GC (J,NUM(I))
        ENDDO
        GC(J,NN) = X / real(nod)     !- store the mean coord
      ENDDO


c----- 2: Form the new mid-edge nodes -----
c.. hmm what about mid-face nods in 3d ?
      DO i=1,nod
        i2 = mod(i,nod) + 1        !- the next corner node.
        DO J=1,NDIM
          XYZ(J) = ( GC(J,NUM(I)) + GC(J,NUM(I2)) )/2.
        ENDDO
        CALL FIND_OR_ADD_NODE (GC,IGC,NN,NDIM,XYZ,1,NN,JNODE)
        NUM3(I) = JNODE
      ENDDO


c----- 3: loop and form the NOD daughter 4nq's -----
      DO i=1,nod
        J = i-1
        IF (J.EQ.0) J = NOD

        NUM2(1) = NUM(I)          !- corner node
        NUM2(2) = NUM3(I)         !- 'front' edge
        NUM2(3) = NN_BASE+1       !- the new centre node.
        NUM2(4) = NUM3(J)         !- 'back' edge
        NOD2 = 4
        IEL2 = NEXT_ELEM (I,IEL,NEL)
        CALL PUT_ELEMENT (NUMS,INUMS,IEL2, NUM2,NOD2,NDIME,ITYPE 
     &    ,IMAT,IUSER1,IUSER2,IUSER3)
      ENDDO

c-------------------

      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE MUNG_3NT_CENTRE (GC,IGC,NDIM,NN,NUMS,INUMS,IEL,NEL,P)
C
c     < cf the proposed SPLIT_TO_TRIS, SPLIT_TO_QUADS>
c     Disects an element by the creation of a central node:
c     < was QUADRILATERALISE_MESH >
c          3nt -> 3* 4nq's (also creates 3 new edge nodes)
c          6nt -> 3* 4nq's
c          4nq -> 4* 3nt's
c       (  4np -> 4* 8nb's is also possible ! )
c       (  4np -> 4* 4np's too )
C     4-7-94   will also turn 4nq's into 4* 3nq's
C              also will expand 3nts->6nts
c..   6-9-95 the no-op 'IO' added to argument list for generality
c..   6-9-95 ,IOP removed from the argument list
c.. * 6-6-97 8nb->4* 5np

c   really the 3nt->3*4nq does not quite belong here.
c     ie contrast the 2 cases:
c         1: turn *any* polygon into 3nt's by adding a central node
c         2: turn *any* polygon into 4nq's by adding edge nodes (+centre)
c     hence make a pair of SPLIT_TO_TRIS, SPLIT_TO_QUADS
c        a/ each creates a central node
c        b/ TO_QUADS loops the edges and adds midside nodes. 
c        c/ loop the n-children, form the list of nodes and PUT_ELEMENT
c
c   SPECIAL CASES:
c      For non-polygons (eg 6nt, 8nq) we may want to *use* the existing
c        mid-edge nodes when turning into quads.
c

      REAL GC(IGC,*), XYZ(5)
      INTEGER NUMS (INUMS,*)      ! elements
     &          ,P (*)            ! # of elems per node (workspace)
     &        ,NUM (32)           ! nodes of an element
     &       ,NUM2 (32)           ! nodes on a facet- hence daughter elem

      INTEGER LIST_3nt(4,3), LIST_4nq(3,4)  !, LIST_8nb(5,6)
      DATA LIST_3nt/ 1,6,7,2,   6,5,4,7,   2,7,4,3 /  !- the 3 4nq's
c     DATA LIST_4nq/ 2,1,5,  3,2,5,  4,3,5,  1,4,5 /  !- the 4 3nt's -LH
      DATA LIST_4nq/ 2,5,1,  3,5,2,  4,5,3,  1,5,4 /  !- the 4 3nt's -RH

c.. cf loop GET_FACE then add a 'fifth' centre node.
c.. this would work for any polyhedron.. daughters will be 'pyramids'
c.. with a base = #nodes on this face (ceg a disection of the 6n-wedge?)
c     DATA LIST_8nb/ / 1,2,3,4, 9,  
c
      CALL GET_ELEMENT (NUMS,INUMS,IEL, NUM, NOD,NDIME,ITYPE
     &      ,IMAT,IUSER1,IUSER2,IUSER3)
c      IF ((NDIME.EQ.2).AND.(NOD.EQ.3.OR.NOD.EQ.6.OR.NOD.EQ.4)) THEN
c       else we just exit.

c---------------- create mid-edge nodes if necessary -------------------
c.. for 3nt->3*3nts (via a 6nt), and 4np->3*8nbs (via a 10np)
c (I could make this a seperate subroutine ?)
        IF (NOD.EQ.3) THEN 
          NUM(5) = NUM(3)
          NUM(3) = NUM(2)
          NUM(7) = NUM(1)  !- hack node 1 to make the loop nicer
          DO I=2,6,2       !- loop the midside nodes
            DO J=1,NDIM
              XYZ(J) = (GC (J,NUM(I-1)) + GC (J,NUM(I+1)) )/2.
            ENDDO
            CALL FIND_OR_ADD_NODE (GC,IGC,NN,NDIM,XYZ,1,NN,JNODE)
            NUM(I) = JNODE   !- record the (new) node
          ENDDO
          NOD = 6     !- now a 6nt.
        ENDIF

c------------------- form the new centroidal node ----------------------
c.. yes we always create a new centre-node
c.. sometimes we create mid-edge nodes too.
        NN = NN + 1     
        DO J=1,NDIM
          X = 0.
          DO I=1,NOD                   !- create the new node
            X = X + GC (J,NUM(I))
          ENDDO
          GC(J,NN) = X / real(nod)     !- store the mean coord
        ENDDO

        NUM(NOD+1) = NN    !- add its new node
c.. be careful with the next 
c.. here I will cheat by saving it first as a: 
c       3nt->6nt->7nt, 6nt->7nt,   4nq->5nq, 8nq->9nq  (8nb->9nb?)

        CALL PUT_ELEMENT (NUMS,INUMS,IEL, NUM, NOD+1,NDIME,ITYPE
     &      ,IMAT,IUSER1,IUSER2,IUSER3)

c-------------- perform the disection --------------
        IF (NDIME.EQ.1) THEN
c         hmm nothing to do here - cf just bisecting into 2 lines ?
        ELSEIF (NDIME.EQ.2) THEN
          IF (NOD.EQ.6)       !- also if parent was a 3nt too.
     &    CALL CREATE_SUB_ELEMS (NUMS,INUMS,NEL,IEL, LIST_3nt ,4,3)
          IF (NOD.EQ.4)       !- to 4*3nt's
     &    CALL CREATE_SUB_ELEMS (NUMS,INUMS,NEL,IEL, LIST_4nq ,3,4)

        ELSEIF (NDIME.EQ.3) THEN
c.. eg. use this to turn an 8nb into 6* 5node-pyramids.
c..    < a 6nw will turn into 3* 5n-pyramids and 2* 4ntets !
c.. loop - GET_FACE - add a centre node - to global NUM & put_element

        CALL GET_FACE (NUM,NOD,NDIME,ITYPE
     &                ,NFACES,NUM2,NN_F,NN_FT, 1 )     !- just NFACES
        DO IFACE=1,NFACES         ! loop this element's faces
          CALL GET_FACE (NUM, NOD,NDIME,ITYPE, IFACE,NUM2,NN_F,NN_FT, 2)
          NOD2 = NN_F+1
          NUM2(NOD2) = NN         !- the 'extra' node.
          IEL2 = NEXT_ELEM (IFACE,IEL,NEL)
          CALL PUT_ELEMENT (NUMS,INUMS,IEL2, NUM2,NOD2,NDIME,ITYPE 
     &      ,IMAT,IUSER1,IUSER2,IUSER3)
         ENDDO

      ENDIF       !- only those types I know how to bisect

      RETURN
      END

C-----------------------------------------------------------------------
c    Notes on other 5np-generation:  9-6-97
c
c   - Try taking a 'flat' mesh of 4nq's and turning each into a 5np
C    - eg as a special case of *DANBLOCKS
c   < hence use *DANBLOCKS as an engine in its own right ?>
c      special cases for daughter =5np etc.?
c       - find centre of the 4 nodes - project up as sqrt(area)
c       - so get normal vector - hence area too.
c

C-----------------------------------------------------------------------
      SUBROUTINE MUNG_3NT_LEFT (NUMS,INUMS,NEL,Iel)
C
C     This replaces a 4nq with a pair of 3nt's     '|/|' style
C      DJK 7-4-94
C
      INTEGER NUMS(INUMS,*), NEL,IEL, NUM(50)

      INTEGER LIST_4NQ(3,2), LIST_8NQ(3,6), LIST_8NB(4,5)
c     DATA LIST_4NQ / 1,3,2,  3,1,4/            !- the 2 triangles -lh
      DATA LIST_4NQ / 1,2,3,  3,4,1/            !- the 2 triangles -rh
c     DATA LIST_8NQ / 2,6,4,  6,2,8,            !- 2 central tri's -lh
c    &        8,2,1,  2,4,3,  4,6,5,  8,7,6/    !-  + corner tri's     
      DATA LIST_8NQ / 2,4,6,  6,8,2,            !- 2 central tri's -rh
     &        8,1,2,  2,3,4,  4,5,6,  8,6,7/    !-  + corner tri's
      DATA LIST_8NB / 2,7,4,5,                  !- central tetrahedron
     &   3,2,7,4,  6,7,2,5, 8,4,7,5, 1,5,2,4 /  !- + 4 corner tets

      CALL GET_ELEMENT (NUMS,INUMS,Iel, NUM, 
     &     NOD,NDIME,ITYPE,IMAT,IUSER1,IUSER2,IUSER3)

      IF (NDIME.EQ.2.AND.NOD.EQ.4) THEN      !------ 4nq's to 2 3nt's ------
        CALL CREATE_SUB_ELEMS (NUMS,INUMS,NEL,Iel, LIST_4NQ, 3,2)
      ELSEIF (NDIME.EQ.2.AND.NOD.EQ.8) THEN  !------ 8nq's to 6 3nt's ------
        CALL CREATE_SUB_ELEMS (NUMS,INUMS,NEL,Iel, LIST_8NQ, 3,6)
      ELSEIF (NDIME.EQ.3.AND.NOD.EQ.8) THEN  !------ 8nb's to 5 4np's ------
        CALL CREATE_SUB_ELEMS (NUMS,INUMS,NEL,Iel, LIST_8NB, 4,5)
      ELSE
        RETURN    !- no method known for dividing this sort of element
      ENDIF
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE MUNG_3NT_RIGHT (NUMS,INUMS,NEL,Iel)
C
C     This replaces a 4nq with a pair of 3nt's     '|\|' style
C      DJK 7-4-94
C      19-2-97 changed polarity of triangles
      INTEGER NUMS(INUMS,*), NEL,IEL, NUM(50)

      INTEGER LIST_4NQ(3,2), LIST_8NQ(3,6), LIST_8NB(4,5)
c     DATA LIST_4NQ / 4,2,1,  2,4,3/            !- the 2 triangles -lh
      DATA LIST_4NQ / 4,1,2,  2,3,4/            !- the 2 triangles -rh
c     DATA LIST_8NQ / 8,4,2,  4,8,6,            !- 2 central tri's -lh
c    &        8,2,1,  2,4,3,  4,6,5,  8,7,6/    !-  +corner tri's
      DATA LIST_8NQ / 8,2,4,  4,6,8,            !- 2 central tri's -rh
     &        8,1,2,  2,3,4,  4,5,6,  8,6,7/    !-  +corner tri's
      DATA LIST_8NB / 3,8,1,6,                  !- central tetrahedron
     &   4,3,8,1,  7,8,3,6, 5,1,8,6, 2,6,3,1 /  !-  +corner tets

      CALL GET_ELEMENT (NUMS,INUMS,Iel, NUM,       !- only to 
     &     NOD,NDIME,ITYPE,IMAT,IUSER1,IUSER2,IUSER3)  !- get NOD

      IF (NDIME.EQ.2.AND.NOD.EQ.4) THEN      !------ 4nq's to 2 3nt's ------
        CALL CREATE_SUB_ELEMS (NUMS,INUMS,NEL,Iel, LIST_4NQ, 3,2)
      ELSEIF (NDIME.EQ.2.AND.NOD.EQ.8) THEN  !------ 8nq's to 6 3nt's ------
        CALL CREATE_SUB_ELEMS (NUMS,INUMS,NEL,Iel, LIST_8NQ, 3,6)
      ELSEIF (NDIME.EQ.3.AND.NOD.EQ.8) THEN  !------ 8nb's to 5 4np's ------
        CALL CREATE_SUB_ELEMS (NUMS,INUMS,NEL,Iel, LIST_8NB, 4,5)
      ELSE
        RETURN    !- no method known for dividing this sort of element
      ENDIF
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE CREATE_SUB_ELEMS (NUMS,INUMS,NEL,IEL_old,
     &         LIST,nod2,nsub)
C
C     This breaks the parent element into a number of daughter elements
C     ..called by MUNG_3NT_LEFT and MUNG_3NT_RIGHT
C
C    ( maybe I can use this when sub-dividing a 4nq into 4 3nt's 
C      by creation of a new central node 
C     .. also maybe used from within DANBLOCKS .. when children are tri's
C     .. or when doing 3 4nq's from a 3nt.  )
C
      INTEGER NUMS(INUMS,*), NUM(50), NUM2(50), list(nod2,nsub)

      CALL GET_ELEMENT (NUMS,INUMS,IEL_old, NUM, 
     &     NOD,NDIME,ITYPE,IMAT,IUSER1,IUSER2,IUSER3)

      DO I=1,NSUB
        DO J=1,NOD2
          NUM2(J) = NUM( LIST(J,I))
        ENDDO
        IEL2 = NEXT_ELEM (I,IEL_OLD,NEL)
        CALL PUT_ELEMENT (NUMS,INUMS,IEL2, NUM2, 
     &      NOD2,NDIME,ITYPE,IMAT,IUSER1,IUSER2,IUSER3)
      ENDDO
      RETURN
      END

C-----------------------------------------------------------------------
      FUNCTION NEXT_ELEM (I,IEL_OLD,NEL)
c
c     This returns the position of the next free node/elem
c     where I is =1 we use the old pos'n
c           else we extend after NEL
c      < ie returns arg #2 if I=1 else returns ++arg3
c
      INTEGER NEXT_ELEM, I, IEL_OLD, NEL
      IF (I.EQ.1) THEN
         NEXT_ELEM =  iel_old
      ELSE
         NEL = NEL + 1
         NEXT_ELEM = NEL
      ENDIF
      RETURN
      END
c................ end of QAUD_MESH and TRIANG_MESH (phew!)

C-----------------------------------------------------------------------
      SUBROUTINE R_UNBISECT_ELEMENTS 
     &      (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL,P)
c
c     header to UNBISECT_ELEMENTS  .. just read OPCODE (IMAT too ?)
c     24-8-96 ?
c
      REAL GC(IGC,*)
      INTEGER NUMS(INUMS,*), P(*)
      do kk=1,999
    1   READ(IO,*,IOSTAT=IOS) JMAT  !,IOP          !- get the opcodes
        CALL IN_TEST (IO,IOS,*1,*999)
c       JMAT = 230964                      !- wildcard
        CALL UNBISECT_ELEMENTS (GC,IGC,NDIM,NN,NUMS,INUMS,NEL,P 
     &   ,IOP,JMAT)
        CALL DEL_INVIS_MATS (GC,IGC,NDIM,NN,NUMS,INUMS,NEL,P)
      enddo
      RETURN
 999  if (kk.eq.1)
     &CALL MYERROR(1,'Missing opcodes (JMAT,IOP) in *UNBISECT_MESH')
      END

C-----------------------------------------------------------------------
      SUBROUTINE UNBISECT_ELEMENTS (GC,IGC,NDIM,NN,NUMS,INUMS,NEL,P
     &   ,IOP, JMAT)             
c
c     This glues adjacent elements together eg. pairs of triangles from 
c     an advancing-front mesh to form quads.
c          Dan Kidger  16-5-97
c
c   Algotithms : #2 is the best?
c     Method 1: loop elems and join conseq pairs of tris if they share 
c               a common edge. (then zap_nul_elements)
c     Method 2: as 1. but we keep the 'spare' tris on a stack and test 
C               those too (before using new elements I guess.)
c
c     Method 7: join 4 tris to form a quad. (cos after joining 2 we have 
c               a 180deg edge?)
c
c     Method 3: as 2. but we do a quality test on the new quads. eg.
c     3-8-97    the max. new angle. if > 179deg. say. then skip.
c
c
      REAL GC(IGC,*)
      INTEGER NUMS(INUMS,*), P(*), NUM1(99), NUM2(99), NUM(32)
c      PARAMETER (M_NOD=32, M_NODOF=4, ICOORD=M_NOD)   !- as #include ?
c      REAL  COORD(ICOORD,M_NODOF)
      REAL ANGLES(32)             !- the set of internal angles of an element.
      LOGICAL FROM_STACK          !- if test element was on the stack

c     NN_OLD  = NN           !- maybe remember this too ? (for print-out)
c                            ! (but of course it doesn't change.
      NEL_OLD = NEL          !- only disect the 'original' set

c     Method 2 (and 1 too?):
c       1: Get min/mid/max node # of this elem.
c       2: loop rest of elems (realy just the next is needed in Method 2?)
c       3: 
c       4:

      LSTACK = 0              !- start with no 'spare' triangles.
      n_quads =0

c----------- 2: loop all the current elements -------------
      DO IEL=1,NEL_OLD
        CALL GET_EL_IMAT (NUMS,INUMS,IEL, IMAT)
C       IF (JMAT.ne.IMAT.AND.JMAT.ne.230964) GOTO 33  !- not this material
        if (IMAT.le.0) GOTO 33                  !- CYCLE if a n/a elem.

c  -- get this 'trial' element
        CALL GET_ELEMENT (NUMS,INUMS,IEL, NUM1, NOD1,NDIME,ITYPE
     &  ,IMAT,IUSER1,IUSER2,IUSER3)

c------ 3: is this a trinagle ? ------
c.. we currently only supprt 3nt + 3nt -> 4nq
c.. maybe I *am* allowed to to join general polygonal elements together?
c.. maybe I should 'mark' all the elements that I have done with -ve
c   IMAT say so that I do not do them recursively
        IF (NOD1.NE.3) GOTO 33              ! CYCLE if not a triangle.
        IF (NDIME.NE.2) GOTO 33            ! CYCLE if not 2d ?
c.. simplify ie. put lowest node # as node #1 ?
c       INODE_MIN = MIN (NUM1(1),NUM1(2),NUM1(3))

c------ 4: Loop the trial elements to test against --------
c  -- first test the list of 'odd and sods' ---
c ? 1: maybe hack IEL+1 to the end of the stack to simplify the looping ?
c   2: probaly faster to loop the stack in reverse order.
c   3: hence put IEL+1 in position zero?
c    - or just fudge to get IEL2 -> loop down stack then up IEL

        nmore = min(1, nel-iel)         !- =0 if the last elem.
        DO ITST = 1, LSTACK + nmore     !- so the next 5 elems too?

c------ 5: Test against the stack of odd triangles --------
          FROM_STACK = ITST.LE.LSTACK
          IF (FROM_STACK) THEN    !--- choose the trial element ---
            ISTACK = LSTACK+1 - ITST      !- which one. (reversed)
            IEL2 = P(ISTACK)

c------ 6: Test against the other elements --------
c it is quickest to just test the *next* (IEL+1) element
c (because of the way the advancing front method works)
          ELSE
            IEL2 = IEL + (ITST-LSTACK)    !- so usu. just IEL+1
          ENDIF

          CALL GET_ELEMENT (NUMS,INUMS,IEL2, NUM2, NOD2,NDIME2,ITYPE2
     &    ,IMAT2,IUSER1,IUSER2,IUSER3)
          IF (IMAT2.LE.0)    GOTO 22     !- a nul element (eg a hole)
          IF (IMAT2.NE.IMAT) GOTO 22     !- not the same 
          IF (NOD2.ne.3)     GOTO 22     !- not a triangle
          IF (NDIME2.ne.2)   GOTO 22     !- not 2d

c------- 7: crash detect these 2 triangles --------
c** compare with the FACET_STRIP crash detector ? **
          CALL COMMON_EDGES (NUM1,NOD1, NUM2,NOD2, IBASE,IBASE2,NEDGES)
          if (nedges.eq.1)      !- try again :-(
     &    CALL COMMON_EDGES2 (NUM1,NOD1, NUM2,NOD2, IBASE,IBASE2,NEDGES)
          IF (NEDGES.ne.2) GOTO 22        !- they do not touch edge:edge

c------- 8: form the new element ------
c.. ie simply return the new NUM from the old NUM and NUM2
C  13-8-97 hmm we MUST NOT overwrite NUM or NOD in case we reject this glueing
          NOD  = NOD1
          DO I=1,NOD          !- take a copy to overwrite into.
            NUM(I) = NUM1(I)   
          ENDDO
          CALL GLUE_TRI_PAIR (NUM,NOD, NUM2,NOD2, IBASE,IBASE2,NEDGES)

c------- 9: Quality-test: get max/min internal angles. -----
c.. get the min and max internal angles then tests against a quality 
c   criterion
c         CALL GET_COORD (NUM4,NOD4,NDIM,GC,IGC,COORD,ICOORD)    !- coords
          CALL INT_ANGLES_1EL (GC,IGC,NDIM,  NUM,NOD,NDIME, angles)
c.. now test the 4 angles (or maybe just the 2 new ones?)
c= nod(1) = is in tri #1 ?
c         nod = 4  !- hmm GLUE_TRI_PAIR seems to mung this to 7 sometimes
          angle_big=-999.
          angle_sum =0.
          do i=1,nod
            angle_big=max(angle_big,angles(i))
            angle_sum = angle_sum + angles(i)
          enddo
          if (angle_big.gt.170.) then       !- try 170deg as a cut-off
c           WRITE (*,'(i4,99f8.2)') iel, (angles(i),i=1,nod), angle_sum
            goto 22       !- so abort cos angle is too big.
          endif 
c------- 10: Store the new element in one hole and zap the other hole --
          n_quads = n_quads+1
          CALL PUT_ELEMENT (NUMS,INUMS,IEL, NUM, NOD,NDIME,ITYPE
     &    ,IMAT,IUSER1,IUSER2,IUSER3)
          CALL PUT_EL_IMAT (NUMS,INUMS,IEL2, 0)    !- zap the other triangle

c         if the match was off the stack we need to close the stack up.
c         IF (ITST.ge.1.and.itst.le.lstack) THEN      !- if off the stack.
          IF (FROM_STACK) THEN      !- if off the stack.
c           DO I=ITST,LSTACK-1     !- close up the gap.
            DO I=ISTACK,LSTACK-1
              P(I) = P(I+1)
            ENDDO
            P(LSTACK) = 0         !- for tidyness.
            LSTACK = LSTACK-1    
          ENDIF
          GOTO 33          !- jump to the next base triangles

   22      CONTINUE   !- cycle
          ENDDO !- loop trials
c.. only here if no hit - so place this elem onto the stack of 'spares'
          lstack = lstack+1
          p(lstack) = iel

   33   CONTINUE
      ENDDO

      write(*,'(a,i7,a,i7,a)') '<> ', n_quads, ' quads created;' 
     &                        ,lstack,' triangles left over'
      RETURN
      END

c-----------------------------------------------------------------------
      SUBROUTINE INT_ANGLES_1EL (GC,IGC,NDIM,  NUM,NOD,NDIME,
     &   angles)
C
C     This calculates the internal angles for the given element.
c     This data can then be used for mesh quality purposes.
C     eg. to reject new elements that have >=180deg angles, or to flag
C     all elements with an angle less than 20deg. say.
c          c. Dan Kidger 4-8-97
c     Notes:
c     1/ cf. ANGLE_STATS() in my AFRONT.F90
C     2/ Maybe then rank so angles(1) is the smallest, angles(NOD) is 
c         the largest. 
c     3/ How do we hangle polygons in 3d space?
c     4/ If I use ACOS: 181deg==179deg :-(
c
      REAL GC(IGC,*)     !- global cooords
      INTEGER num(*)     !- nodes of an element.
      REAL ANGLES(*)     !- the angles.

      do i1=1,nod            !-- loop around this element
        i0 = i1-1
        if (i0.eq.0) i0=nod
        i2 = i1+1
        if (i2.gt.nod) i2=1
        n0= num(i0)
        n1= num(i1)       !- the three consecutive nodes.
        n2= num(i2)
        dx1 = gc(1,n2) - gc(1,n1)  !- RH vector
        dy1 = gc(2,n2) - gc(2,n1)
        dx2 = gc(1,n0) - gc(1,n1)  !- LH vector
        dy2 = gc(2,n0) - gc(2,n1)

c       .. now use the dot-product of the 2 vectors ?
        dist1=sqrt(dx1**2+dy1**2)
        dist2=sqrt(dx2**2+dy2**2)
c       dot = (dx1*dx2 + dy1*dy2)/(dist1*dist2)
c       theta = acos(dot) * 180./3.14159265     !- in degrees.

c       .. now use the cross-product of the 2 vectors ?
c        xx= 0.  !dy1 - dy2
c        yy= 0.  !dx2 - dx1
c        zz= -(dx1*dy2 - dx2*dy1)
c       sintheta = sqrt (xx**2 + yy**2 + zz**2) /(dist1*dist2)
c        sintheta = (dx1*dy2 - dx2*dy1) / (dist1*dist2)
c       print*,'sintheta=',sintheta
c        if (sintheta.gt.1.1) stop 'oops - (INT_ANGLES_1EL)'
c        if (sintheta.gt.1.) sintheta=1.
c        theta = asin (sintheta) * 180./3.14159265

c.. use 2 whole circle bearings...
        theta1= atan2(dy1,dx1) * 180./3.14159265
        theta2= atan2(dy2,dx2) * 180./3.14159265
        theta = theta1-theta2
        if (theta.lt.0.) theta = theta + 360.
c       print*,'theta1,2,d=',theta1,theta2, theta

c.. rotate the second to 'North'
c       vlen = sqrt(dx1*dx1 + dy1*dy1)
c       cc = dx1/vlen
c       ss = dy1/vlen
c        dx3 = dx2 * cc + dy2* ss
c        dy3 =-dx2 * ss + dy2* cc
c        theta = atan2(dy3,dx3)

        angles(i1) = theta
      enddo

c----- 3: now sort the list into ascending order.
c.. so that we can easily pick up the smallest and largest
c.. hmm forget this . do externally if we need to.

      RETURN
      END

c-----------------------------------------------------------------------
      SUBROUTINE COMMON_EDGES (NUM,NOD, NUM2,NOD2, IBASE,IBASE2,NEDGES)
C
C    This Routine returns which edge EDGE(1,2,3) of polygon NUM that touches
C    edge IEDGE2 of element NUM2.
c     Dan Kidger 17-5-97
c
c  -  This is called by UNBISECT_ELEMENTS when we are gluing triangles together
C  -  returns IBASE of NUM
C            IBASE2 of NUM2
C        and NEDGES = the count of edges (nodes-1) that are common.
c
c   NOTES
C     'edge 5' is the edge that runs from node '5' to node '6'.
c    CAVEATS:
C      What do we do if some triangles are clockwise and some aniclockwise?
C
c      so maybe the general method is to return the list of common points
c      we only want the case where npoints=2

      INTEGER NUM(NOD), NUM2(NOD2)            
     &       ,IEDGE                   !- the 2 matching edges

      IBASE  = 0                 !- default is no hits
      IBASE2 = 0
      NEDGES = 0                 !- so no edge segments either

c---------------- 1/ find IBASE --------------------
      DO I=1,NOD              !- try forwards (so lowest node#)
c     DO I=NOD,1, -1          !- try backwards
        INODE = NUM(I)
        DO J=1, NOD2
          IF (NUM2(J).EQ.INODE) THEN
            IBASE = I
            IBASE2= J
            GOTO 11
          ENDIF
        ENDDO
      ENDDO
   11 CONTINUE

      IF (IBASE.EQ.0) RETURN       !- we never got a matching pair.

c----------- 2/ count the common edge points ----------
c we loop one polygon clockwise the other anticlock.
c note that it is rare that we will ever want anything other than 
c           NOD==NOD2==3
c cf. 'Greedy' algorithms where we are advancing a lasso around elements ??
c  - util NOD beomes = NEL/nprocs for load-balancing. :-)

      inode  = ibase
      inode2 = ibase2
      DO iedge = 1, MAX(NOD,NOD2)         !- or MIN ??
        inode  = mod (inode,nod)+1        !- next clock
        inode2 = inode2-1                 !- next anti-clock
        if (inode2.eq.0) inode2 = nod2
        IF (NUM(INODE).NE.NUM2(INODE2) ) GOTO 22   !- lost contact
      ENDDO
   22 NEDGES = iedge         !- usu. NEDGES is 2

c      IF (NEDGES.EQ.1) THEN    !-- try looping the other way too?
c        inode  = ibase
c        inode2 = ibase2
c        DO iedge = 1, MAX(NOD,NOD2)       !- or MIN ??
c          inode2  = mod (inode2,nod2)+1   !- next clock
c          inode = inode-1                 !- next anti-clock
c          if (inode.eq.0) inode = nod
c          IF (NUM(INODE).NE.NUM2(INODE2) ) GOTO 33   !- lost contact
c        ENDDO
c   33   NEDGES = iedge         !- usu. NEDGES is 2
c      ENDIF

      RETURN

      end !SUBROUTINE COMMON_EDGES 

c-----------------------------------------------------------------------
      SUBROUTINE COMMON_EDGES2 (NUM,NOD, NUM2,NOD2, IBASE,IBASE2,NEDGES)
C
C    This Routine returns which edge EDGE(1,2,3) of polygon NUM that touches
C    edge IEDGE2 of element NUM2.
C    return IBASE of NUM
C           IBASE2 of NUM2
C        and NEDGES = the count of edges (nodes-1) that are common.
c     Dan Kidger 17-5-97
c
c    NOTES
C     'edge 5' is the edge that runs from node '5' to node '6'.
c    CAVEATS:
C      What do we do if some triangles are clockwise and some aniclockwise?
C
c      so maybe the general method is to return the list of common points
c      we only want the case where npoints=2

      INTEGER NUM(NOD), NUM2(NOD2)            
     &       ,IEDGE                   !- the 2 matching edges

      IBASE  = 0                 !- default is no hits
      IBASE2 = 0
      NEDGES = 0                 !- so no edge segments either
c---------------- 1/ find IBASE --------------------
c     DO I=1,NOD              !- try forwards (so lowest node#)
      DO I=NOD,1, -1          !- try backwards
        INODE = NUM(I)
        DO J=1, NOD2
          IF (NUM2(J).EQ.INODE) THEN
            IBASE = I
            IBASE2= J
            GOTO 11
          ENDIF
        ENDDO
      ENDDO
   11 CONTINUE

      IF (IBASE.EQ.0) RETURN       !- we never got a matching pair.

c----------- 2/ count the common edge points ----------
c we loop one polygon clockwise the other anticlock.
c note that it is rare that we will ever want anything other than 
c           NOD==NOD2==3
c cf. 'Greedy' algorithms where we are advancing a lasso around elements ??
c  - util NOD beomes = NEL/nprocs for load-balancing. :-)

      inode  = ibase
      inode2 = ibase2
      DO iedge = 1, MAX(NOD,NOD2)         !- or MIN ??
        inode  = mod (inode,nod)+1        !- next clock
        inode2 = inode2-1                 !- next anti-clock
        if (inode2.eq.0) inode2 = nod2
        IF (NUM(INODE).NE.NUM2(INODE2) ) GOTO 22   !- lost contact
      ENDDO
   22 NEDGES = iedge         !- usu. NEDGES is 2

c      IF (NEDGES.EQ.1) THEN    !-- try looping the other way too?
c        inode  = ibase
c        inode2 = ibase2
c        DO iedge = 1, MAX(NOD,NOD2)       !- or MIN ??
c          inode2  = mod (inode2,nod2)+1   !- next clock
c          inode = inode-1                 !- next anti-clock
c          if (inode.eq.0) inode = nod
c          IF (NUM(INODE).NE.NUM2(INODE2) ) GOTO 33   !- lost contact
c        ENDDO
c   33   NEDGES = iedge         !- usu. NEDGES is 2
c      ENDIF

      RETURN

      end !SUBROUTINE COMMON_EDGES2 

c-----------------------------------------------------------------------
      SUBROUTINE GLUE_TRI_PAIR (NUM,NOD,NUM2,NOD2, IBASE,IBASE2,NEDGES)
c
c     This replaces the given pair of elements with a single element
c     by removing the common edge(s)
c        Dan Kidger 17-5-97
c     METHOD:
c      1. build edge from: node 1 to IEDGE (elem #1)
c      2. build edge from: node IEDGE to IEDGE2 (elem #2)
c      3. build edge from: node IEDGE2 to 1 (elem #1)
c      4. PUT_ELEMENT (usu a 4nq now)
c      5. zap the hole - just put as IMAT=0 I guess, then clean up later
c
c      Maybe I should use GET_ELEMENT again so that we get IUSER1..3 etc.?

c     as input idealy we would like to know
c       1/ the start node of the common edge(s) in *both* elements
c       2/ the end node of the common edge(s) in *both* elements
c          usu 2. = inode+1 in elem1 and = inode-1 in elem2
c          so maybe the *length* of the common edge is more usuful (usu.=1)

      INTEGER NUM(NOD), NUM2(NOD2)

c     NOD3 = NOD + NOD2 - 2*NEDGES    !- size of the resulting element
      NOD3 = NOD + NOD2 -   NEDGES    !- size of the resulting element

c   maybe re-build into NUM ...
c       so move from IBASE+1 to NOD to the end of the list.
c       then simply copy NUM2 into place in reverse order
      NREM = NOD-IBASE+1     !- #nodes to move
      DO I=1, NREM
        NUM(nod3+1-I) = NUM(nod+1-I)
      ENDDO

      DO I=1,NOD2-NEDGES     !- #nodes to copy
        I2 = IBASE2+I
        if (i2.gt.nod2) i2 = i2-nod2
        NUM(IBASE+I) = NUM2(I2)
      ENDDO
      NOD = NOD3        !- now a longer polygon.

      END   !subroutine GLUE_TRI_PAIR
C-----------------------------------------------------------------------

C-----------------------------------------------------------------------
      SUBROUTINE M_TO_QUADS (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL,P)
C
C         < *semi-obsolete* >
C   Thie adds a fourth node to 3 node triangles so that they are valid 
c   as quads (hence can DANBLOCK into 3D)  (for Crisp's polutry.scd')
C     Dan Kidger  14-9-98
C
      REAL GC(IGC,*)
      INTEGER NUMS(INUMS,*), P(*), num(32)
      IC = 0
      DO IEL=1,NEL
        CALL GET_ELEMENT
     &  (NUMS,INUMS,IEL, NUM, NOD,NDIME,ITYPE, IMAT,I1,i2,i3)
        IF (NDIME.EQ.2.AND.NOD.EQ.3) THEN
          ic = ic + 1
          NUM(4) = NUM(3)        !- or duplicte the node with the
          nod=4                  !- smallest internal angle?
          CALL PUT_ELEMENT
     &    (NUMS,INUMS,IEL, NUM, NOD,NDIME,ITYPE, IMAT,I1,i2,i3)
        endif
      ENDDO
      WRITE(*,'(A,I7,A)') '<> ', IC,' elements converted'
      RETURN
      END
C-----------------------------------------------------------------------
      SUBROUTINE M_3NT_4NQ (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL,P)
C
C         < *obsolete* >
C     This glues 4 consecutive 3nt's into a 4nq (eg. TEAPOT.G)
C      ( however cf. *UNBISECT_ELEMENTS )
C     .. WHAT ABOUT GET/PUT_ELEMENT ?
C
      REAL GC(IGC,*)
      INTEGER NUMS(INUMS,*), P(*)
      IC = 0
      DO IEL=1,NEL,4
        IC = IC +1
        NUMS (1,IC) = NUMS(1,IEL  )
        NUMS (2,IC) = NUMS(2,IEL  )
        NUMS (3,IC) = NUMS(1,IEL+2)
        NUMS (4,IC) = NUMS(2,IEL+2)
        NUMS (INUMS,IC) = 4
      ENDDO
      NEL = IC
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE R_DEL_MATS (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL,P)
C
C     this will delete any elements of a given material type
C     **** MUST CHANGE TO USE GET/PUT_ELEMENT ! ****
C
      REAL GC(IGC,*)
      INTEGER NUMS(INUMS,*), P(*)

      DO KK = 1,999
    1   READ (IO,*,IOSTAT=IOS) IMAT_
        CALL IN_TEST (IO,IOS,*1,*999)
        NEL1 = NEL    
        IC = 0
        DO IEL=1,NEL
c         NOD  = NUMS (INUMS  ,IEL)
          IMAT = NUMS (INUMS-3,IEL)
          IF (IMAT.NE.IMAT_) THEN               !-- If a 'good' element ..
            IC = IC + 1 
            DO J = 1,INUMS
              NUMS(J,IC)=NUMS(J,IEL)            !-- store it at IC
            ENDDO
          ENDIF
        ENDDO
        NEL = IC
        WRITE(*,'(A,I4,A,I7,A)')  '>< ',KK,':',
     +               NEL1-NEL,' elements deleted'
      ENDDO
  999 RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE DEL_INVIS_MATS (GC,IGC,NDIM,NN,NUMS,INUMS,NEL,P)
C
C     This will delete any elements of a given material type
C     **** MUST CHANGE TO USE GET/PUT_ELEMENT ! ****
c .... ? for speed get_IMAT only .. if to keep then get NUM too ??
C       cf. DEL_MATS
C
      REAL GC(IGC,*)
      INTEGER NUMS(INUMS,*), P(*)

      NEL1 = NEL    
      IC = 0
      DO IEL=1,NEL
        IMAT = NUMS (INUMS-3,IEL)
        IF (IMAT.GE.1) THEN               !-- If a 'good' element ..
          IC = IC + 1 
          DO J = 1,INUMS
            NUMS(J,IC)=NUMS(J,IEL)            !-- store it at IC
          ENDDO
        ENDIF
      ENDDO
      NEL = IC
      KK = 0
      WRITE(*,'(A,I4,A,I7,A)')  '>< ',KK,':',
     +               NEL1-NEL,' elements deleted'
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE DEL_NUL_ELEMENTS (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL,P)
C
C     This will delete any elements that have no nodes
C     **** MUST CHANGE TO USE GET/PUT_ELEMENT ! ****
c .... ? for speed get_IMAT only .. if to keep then get NUM too ??
C       cf. DEL_MATS
C   .. 6-09-95 IO added to argument list for generality
      REAL GC(IGC,*)
      INTEGER NUMS(INUMS,*), P(*)

c      NEL1 = NEL    
c      IC = 0
c      DO IEL=1,NEL
c        IMAT = NUMS (INUMS-3,IEL)
c        IF (IMAT.GE.1) THEN               !-- If a 'good' element ..
c          IC = IC + 1 
c          DO J = 1,INUMS
c            NUMS(J,IC)=NUMS(J,IEL)            !-- store it at IC
c          ENDDO
c        ENDIF
c      ENDDO
c      NEL = IC
c      KK = 0
c      WRITE(*,'(A,I4,A,I7,A)')  '>< ',KK,':',
c     +               NEL1-NEL,' elements deleted'
c      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE DEL_ZERO_AREA (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL,P)
C
C     This will delete any elements which have 'zero' Area
C     compare with FLIP_DET which 'flips-over' element with -ve DETs
C         DJK 1994
C     29-3-96 pushed the getting the 'DET' to a subroutine
C
      REAL GC(IGC,*)
      INTEGER NUMS(INUMS,*), P(*)

      NEL1 = NEL    
      IC = 0
      DO IEL=1,NEL

C.. Get the element 'area' - 2 methods?
c    1/ use 'Danplot' area method
c    2/ use Shape funs.. hence.. get DET
        CALL GET_EL_DET (GC,IGC,NDIM,NN,NUMS,INUMS,NEL,IEL,DET)
c       print*,' iel=',iel,' det=',det

c       IF (abs(DET).GE. 0.0001) THEN          !-- If a 'good' element ..
        IF (DET.GE. 0.0001) THEN               !-- If a 'good' element ..
          IC = IC + 1 
          DO J = 1,INUMS
            NUMS(J,IC)=NUMS(J,IEL)             !-- store it at IC
          ENDDO
        ENDIF
       ENDDO
       NEL = IC
       WRITE(*,'(A,I4,A,I7,A)')  '>< ',1,':',
     +               NEL1-NEL,' elements deleted'
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE DEL_O_NODES (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL,P)
C
C     this will delete any 'unreferenced' orphan nodes
C
c     cf CALL GET_NODAL_VALENCY : then zap any zeros?
c
      REAL GC(IGC,*)
      INTEGER NUMS(INUMS,*), P(*)
      INTEGER NUM(99)

      DO I=1,NN
        P(I) = 0          !-- null the pointer list
      ENDDO

      DO IEL=1,NEL  !-------- light-up ALL valid nodes -----------------
        CALL GET_ELEMENT (NUMS,INUMS,IEL, NUM, 
     &          NOD,NDIME,ITYPE,IMAT,IUSER1,IUSER2,IUSER3)
        DO J = 1,NOD           
          IF (IMAT.NE.0) P(NUMS(J,IEL)) = 1   !- or count them ?
        ENDDO
      ENDDO
      CALL RESEQ_P (P,NN,NN_NEW)  

      CALL UPDATE_GC   (GC,IGC,NDIM,NN, P)
      CALL UPDATE_NUMS (NUMS,INUMS,NEL, P)

      WRITE(*,'(A,I7,A)')  '>< ',NN-NN_NEW,' nodes deleted'
      NN = NN_NEW
      END

C-----------------------------------------------------------------------
      SUBROUTINE MERGE_NODES (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL,P)
c
c     This will 'condense' all nodes that share the same position
c     For speed we pre-sort the nodes into a ascending x-value
c          Dan Kidger  17-4-98 
c
C Methods: (16-2-97)
c    1: SIMPLEX (Sledgehammer)
c      For each node: search the remaining nodes and record the *lowest* 
c      node number at this location.
c      Hence renumber all the node# in NUMS via UPDATE_NUMS and 
c      and delete any orphan nodes vua DEL_O_NODES
c    2: X-SORT (a)
c      Rank the nodes wrt. their x-coordinate.
c      Then loop *this* list and only search forwards where 'x' is the same.
c      flag the Min. node number at each location then resort as before
c    3: X-SORT (b)
c      As (a) but for each 'x' scan forward for nodes with this 'x' and 
c      thence sort the sub-lists wrt 'y' .. maybe z too, hence left with a
c      'perfect'  list of nodes as coincident nodes are consecutive.
c    4- XYZ-sort
c      Similar to (a) and (b) above but first call *SORT_NODES_FROM_POINT
c      Hence just loop this new order: scan forwards and all just test 
C      all within +TOL of the first node .. Not quite true . I still need
c      the list of distances from the 'Origin' or I will have to keep retesting
c      the 'radius'.
c    5- hash-sort
c      Instead of sorting wrt x/y/z individualy - multiply x by one 'random'
c      number, y by another; z too and hence sort this list, the 3 factors
c      just need to be 'irrational' I guess
c    6- Octrees
c      Loop all nodes and insert them into a data structure, then balance
c      the tree. Thence recurse the tree to find 'neighbouring' nodes.
c    7- Integer partioning (?)
c      Give each node an integer x,y,z coord (say 8-bits each), hence a 24 bit 
c      code. So integer sort - but need to check 'adjacent' cells? 

      REAL GC(IGC,*)
      INTEGER P(*), NUMS(INUMS,*)

      REAL DEPTHS(nn)           !- workspace
      integer p2(nn)
      character method*1
      real fact(5)
      data fact/.1876, .3238, .2893, .4101, .2321/

c----- 0: Initialisation --------
      tol =0.0001     ! tolerance ..or as a f(diagonal) ?

c----- 1: pre-sort nodes =o(NlnN) ----
      method='x'          !- try other direction instead ?
      do i=1,nn
        if (method.eq.'x') then
          XYZ = gc(1,i)    !- 
        elseif (method.eq.'y') then
          XYZ = gc(2,i)    !- 
        elseif (method.eq.'z') then
          XYZ = gc(3,i)    !- 
        elseif (method.eq.'r') then    !- an arbitrary plane :-)
          xyz=0.
          do j=1,ndim
            XYZ = xyz + GC(j,I)*fact(j)
          enddo
        endif
        DEPTHS(I) = XYZ
      enddo

      CALL INDEX_REALS (DEPTHS,NN, P2)

c------ 2: Loop nodes in this order and test following nodes -----------
      do i=1, nn
        p(i) = i    
      enddo
      DO II=1,NN
        INODE_base=P2(ii)
        depth_base = depths(inode_base)
c       p(inode_base)=inode_base   !- assume it is unique

c       DO JJ=II+1,NN          !- scan backward -- 
        DO JJ=II+1,NN          !- scan forwards -- 
          INODE = P2(JJ)
          if (abs(depths(inode)-depth_base).gt. tol) goto 21  !-cannot be common
          do j=ndim,1,-1                !- skip if not common
            if (abs(gc(j,inode)-gc(j,inode_base)).gt..0001) goto 22
          enddo
c----         3:  - Ok got a hit ------
          
c         p(inode)=min(p(inode),inode_base)   !- re-mark as the base node.
          if (p(inode).eq.inode)    !- if not already re-assigned
     &    p(inode)=inode_base   !- re-mark as the base node.

   22   continue                !- jump-to 
        ENDDO   !- loop trials
   21   continue                !- jump-to if a miss
      enddo   !- loop all nodes


c----------- 4: renumber the nodes apropriately -------------
      CALL UPDATE_NUMS (NUMS,INUMS,NEL, P)  !- collapse nums, THEN del orphans
      CALL DEL_O_NODES (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL,P)

      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE DEL_COIN_NODES (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL,P)
C
C     This will 'condense' all nodes that share the same position
C     using FIND_NODE to get the node number of a node
C      ** this one of the modules that is order n^2 :-(  **
c
      REAL GC(IGC,*)
      INTEGER P(*), NUMS(INUMS,*)

C---------------- 1: index the node to their 'first occurence' ------------
c.. We only need to scan from node 1 to node 'I'
      IPERCENT_OLD = 0
      DO I=1,NN,1    !-- reversed for 'style'??  :-) 

        IPERCENT = nint(100.*REAL(I)/REAL(NN))
        IF (IPERCENT.NE.IPERCENT_OLD) 
     &  CALL BAR_LINE (IPERCENT,IPERCENT_OLD, 79)

        CALL FIND_NODE (GC,IGC,NN,NDIM,GC(1,I),1,I,INODE)   
        P(I) = INODE      !-- the 'lowest' Node # at this point
      ENDDO

c----------- 2: renumber the nodes apropriately -------------
      CALL UPDATE_NUMS (NUMS,INUMS,NEL, P)  !- collapse nums, THEN del orphans
      CALL DEL_O_NODES (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL,P)

      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE R_DEL_COIN_NODES_BY_BOX (IO,GC,IGC,NDIM,NN,NUMS,INUMS
     &   ,NEL,P)
C
c     'stitches' meshes within a given zone.
c     The same as DEL_COIN_NODES but can be significantly faster.
c        written for the BAe slabs  analysis - Feb 97 ?
c
      REAL GC(IGC,*)
      INTEGER P(*), NUMS(INUMS,*)
      REAL XMIN(5),XMAX(5), XY(5)
c#ifdef F90 then
c     INTEGER P2(NN)
c#else
      integer iworkspace
c     parameter (iworkspace=20000)
      parameter (iworkspace=125000)
      INTEGER P2(iworkspace)
c#endif

c---- 0: INIT ----
      TOL=  1.E-20

      DO ILOOP = 1,99999
    1   READ (IO,*,IOSTAT=IOS) (XMIN(J),XMAX(J),J=1,NDIM)
        CALL IN_TEST (IO,IOS,*1,*999)

c--------- get the list of relevant nodes ----------
      NN_BOX = 0
      DO I=1, NN
        DO J=1,NDIM
          IF (gc(j,i).LT.XMIN(J)-TOL) GOTO 2    !- skip to next element
          IF (gc(j,i).GT.XMAX(J)+TOL) GOTO 2    ! (cf CYCLE)
        ENDDO
        NN_BOX = NN_BOX + 1
        P2(NN_BOX) = I         !- store this point
    2   CONTINUE             !- cycle
      ENDDO
      WRITE (*,*) '<>', nn_box
     &   ,' nodes out of',nn,' nodes to sort..'

c.. hmm so juggle these nodes within the box to the end of the list?

c-- ? now move these to a consecutive sequence.
c.. hence strip as normal ?

c.. problem : I have a 'short' list of nodes to handle
c    but I wanted to end up with a set of old node :new node pairs
c   ie a contradiction.
c   so maybe 'float' the possible intersecting nodes to the *end* of the
c   list (UPDATE_NUMS) then       

C---------------- index the node to their 'first occurence' ------------
      DO I=1,NN
        P(I) = I     !--- assume no change
      ENDDO

      IHIT = 0
      IPERCENT_OLD = 0
      DO I=1,NN_BOX         !- loop these points.

        IPERCENT = nint(100.*REAL(I)/REAL(NN_BOX))
        IF (IPERCENT.NE.IPERCENT_OLD) 
     &  CALL BAR_LINE (IPERCENT,IPERCENT_OLD, 79)

        IBASE = P2(I)
        DO J=1, NDIM                     !- target point
          XY(J) = GC(J,IBASE)
        ENDDO
        INODE = IBASE         !- assume only find myself.
        DO II=1,I-1           !- only look through the 'previously' done.
          IOLD = P2(II)
          DO J=1,NDIM
            IF (ABS(XY(J)-GC(J,IOLD)).GT.TOL) GOTO 11    !-- give-up !
          ENDDO
          INODE = IOLD
          ihit = ihit + 1
          GOTO 12
   11     CONTINUE
        ENDDO                      !- end of node-search loop
   12   P(IBASE) = INODE        !- store the 'new' number

      ENDDO

        CALL UPDATE_NUMS (NUMS,INUMS,NEL, P)  !- collapse nums, THEN del orphans
        CALL DEL_O_NODES (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL,P)

        WRITE (*,'(A,i6,A,i6)') '<>',nn_box-ihit,
     &    ' noded joined out of',nn_box
      ENDDO  !- loop boxes
c---------------------------------------------
 999  IF (ILOOP.eq.1) CALL MYERROR (2,'Missing bounding box coords')
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE FIND_OR_ADD_NODE (GC,IGC,NN,NDIM,XY,IFROM,ITO,INODE)
C
C     This calls FIND_NODE to get the node number at a given coordinate
C     if no node was found then it is added to the end of the list
c
c      18-10-94 now search *backwards* because the node we are looking 
C      for is most likely to be one of the last added to the mesh
c      10-9-97 This routine is a major slow-down of DANBLOCKS - we
c         need some sort of NlogN sorting.
c
      REAL GC(IGC,*), XY(*)
c- note reverse for speed - since new node is more likely to be at the end.
        CALL FIND_NODE (GC,IGC,NN,NDIM,XY,ITO,IFROM,INODE)  
        IF (INODE.LE.0) THEN       !- if not found
          ITO = ITO + 1
          DO J=1,NDIM               !- create a new node
            GC(J,ITO) = XY(J)
          ENDDO
          INODE = ITO
        ENDIF
      END

C-----------------------------------------------------------------------
      SUBROUTINE RESEQ_P (P,NN, IC)
C
C     This resequences the non-zero pointers in P()
C     from 1 to (up to) NN
C     -- this is called from DEL_ORPHAN_NODES
C
      INTEGER P(*), NN, IC, I
      IC = 0
      DO I=1,NN
        IF (P(I).GT.0) THEN
          IC = IC + 1
          P(I) = IC
        ENDIF
      ENDDO
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE IS_ASCENDING (P,N,ASCEND)
C
C     checks whether the list in P() is in ascending order.
C     (not counting zeros which *are* allowed)
C
      INTEGER P(*), N
      LOGICAL ASCEND

      ASCEND = .FALSE.
      DO I=2,N
        IF (P(I).NE.0.AND. P(I).LT.P(I-1)) RETURN
      ENDDO
      ASCEND = .TRUE.
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE RESEQ_NUMS (NUMS,INUMS,NEL, P)
C
C     This re-orders the element order in NUMS based on the NEW 
C     *rank* held in P
C      eg. after SORT_ELEMENTS_FROM_POINT
C     -- if P(I) = 0 then this element is removed  (rarely used)
C       DJK 29-8-96
C
      INTEGER NUMS(INUMS,*), P(*)
      IC = 0
      OPEN (97,STATUS='scratch',FORM='unformatted')
      DO IEL=1,NEL
        WRITE(97) (NUMS(J,IEL),J=1,INUMS)
      ENDDO
      REWIND(97)
      DO IEL=1,NEL
        IF (P(IEL).NE.0) THEN
          IC = IC + 1
          READ(97) (NUMS(J,P(IEL)),J=1,INUMS)
        ELSE
          READ(97) (JUNK,J=1,INUMS)           !- zap this node
        ENDIF
      ENDDO
      CLOSE(97)
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE UPDATE_GC (GC,IGC,NDIM,NN, P)
C
C     This re-orders the nodal co-ordinates in GC based on the NEW 
C     *rank* held in P
C       eg. after SORT_NODES_FROM_POINT
C     -- if P(I) = 0 then this node is removed
C     BUT if just DEL_O_NODES there is no need to save & re-read !
c     ie. If P() is ascending order, we just need to 'copy down'
C         Else is a 'convolution' must write and read. 
c         (note only need to write the 'non-null' nodes.)!
c     Note, we can also use this routine to re-order DISPS as well.
c
c     10-3-97 Check if any need to write out then re-read.
c     28-5-97 cf PERMUT_GC (from TOMS) which needs no temp. storage.
c
      REAL GC(IGC,*)
      INTEGER P(*)

      LOGICAL ASCEND

c.. Check if consecutive - if so can do explicitly.
      CALL IS_ASCENDING (P,NN, ASCEND)

      IF (ASCEND) THEN
        DO I=1,NN
          IF (P(I).NE.0) THEN
            DO J=1,NDIM
              GC(J,P(I)) = GC(J,I)
            ENDDO
          ENDIF
        ENDDO

      ELSE
        OPEN (97,STATUS='scratch',FORM='unformatted')
        DO I=1,NN
          WRITE(97) (GC(J,I),J=1,NDIM)
        ENDDO
        REWIND(97)
        IC = 0
        DO I=1,NN
          IF (P(I).NE.0) THEN
            IC = IC + 1           !- count the number of remaining nodes.
            READ(97) (GC(J,P(I)),J=1,NDIM)
          ELSE
            READ(97) (RJUNK,J=1,NDIM)           !- zap this node
          ENDIF
        ENDDO
        CLOSE(97)
      ENDIF
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE PERMUT_GC (NN,IP, A )
c
c     This re-orders an ARRAY into the order indicated by IP
c     - cf UPDATE_GC which has to write and re-read the mesh from a 
c      scratch-file
c         Dan Kidger 28-5-97
c     *   This algorithm is from TOMS624.F77 written by  *
c     * ROBERT RENKA OAK RIDGE NATL. LAB. (615) 576-5139 *
c     Method:
c      o loop down the list looking for a start-node.
c      o daisy-chain the permutations down the list
c      o  flag  nodes that we have done as -ve in IP()
c      o put the head into the last-link in the chain.
c      o for DERIVED TYPES - I need a temp storage loc of this TYPE
c
      INTEGER NN, IP(NN)              !- new order
      REAL    A(NN)                   !- array to be sorted
      REAL    TEMP                    !- temp holder for a slice of A()
      INTEGER N, K, J, IPJ

C LOCAL PARAMETERS -
C     N   = local copy of NN  (why? - to save indirect addressing ?)
      N = NN
      IF (N .LT. 2) RETURN     !- nothing to do.

c------------------------ Loop the Permutations ------------------------
c  realy this is simply a:
c   DO K=1,NN
c     IF IP(K)<0 then CYCLE

      K = 1
    1 CONTINUE

      J = K              ! J= working pointer

c----------  apply permutation to A ---------
      TEMP = A(J)        !- store away to place at the end of the chain.
    2 IPJ = IP(J)        !- location to move a node *from*
        IP(J) = -IPJ              ! Mark as this node has now been done
        IF (IPJ .EQ. K) THEN      ! Found the start-node again.
          A(J) = TEMP             !  so complete the ring.
          GOTO 3                  ! and EXIT
        ENDIF
        A(J) = A(IPJ)             ! move a node
        J = IPJ                   ! the next pointer.
      GO TO 2                     ! ? count the length of the ring for stats?
    3 CONTINUE

C---------------- search for an unmarked element of IP -----------------
c.. this could be done *before* we shuffle a ring ?
    4 K = K + 1
        IF (K .GT. N) GO TO 5        !- EXIT if list is exhausted
        IF (IP(K) .GT. 0) GO TO 1    !- got a 'hit' so CYCLE
      GO TO 4
    5 CONTINUE

C ALL PERMUTATIONS HAVE BEEN APPLIED.  UNMARK IP.
c.. cf. flaging all at the beginning as -ve (=to be moved), then 
c   unmarking as they get moved.
      DO K = 1,N
        IP(K) = -IP(K)       !- or use ABS() to make sure?
      ENDDO
      RETURN
      END


C-----------------------------------------------------------------------
      SUBROUTINE UPDATE_NUMS (NUMS,INUMS,NEL, P)
C
C     This re-orders the node-steering in NUMS based on the NEW order
C     held in P  
c     -- This is normally called followinfg a call to to UPDATE_GC
c     (cf re-ordering the elements themselves)
C     if IOP=1 (default) then P() is the NEW number
C     if IOP=2  then P() is the position to put this number in (ie. inverse)
c
c   * Called from DEL_ORPHAN_NODES, SORT_NODES_POINT, FACETS_TO_ELEMENTS,
c     DEL_COIN_NODES
C
      INTEGER NUMS(INUMS,*), P(*), NUM(99), NUM2(99)
      IOP = 1
      DO IEL=1,NEL
        CALL GET_ELEMENT (NUMS,INUMS,IEL, NUM, 
     &          NOD,NDIME,ITYPE,IMAT,IUSER1,IUSER2,IUSER3)
        IF (IOP.EQ.1) THEN
          DO J=1,NOD
            NUM2(J) = P(NUM(J))       !-- if ZERO delete elem ?
          ENDDO
        ELSEIF (IOP.EQ.2) THEN
          DO J=1,NOD
            NUM2(J) = P(NUM(J))   
          ENDDO
        ELSE
          STOP '*OOPS* in UPDATE_NUMS'
        ENDIF
        CALL PUT_ELEMENT (NUMS,INUMS,IEL, NUM2, 
     &          NOD,NDIME,ITYPE,IMAT,IUSER1,IUSER2,IUSER3)
      ENDDO
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE PROJECT_MESH (GC,IGC,NDIM,NN)
C
C     'Divide by Z' : projects a mesh into one less dimension
C        Dan Kidger 22-2-98
c     Written so that I can actualy view 4D meshes!
c     Might be useful for 3d->2d.
c     Before use, use *SHIFT_MESH to put z=0 through the middle of the mesh
c     and also arrange the z to go from z=1. to z=2. say (or z=10. to 20. etc.)
c
c     More generaly I might set the eye point (or vanishing point)
C       for projection (Hence map any 2d image plane cf. Foley, et al.)
C
      REAL GC(IGC,NN) , DEPTH

      DO I=1,NN
        DEPTH = GC (NDIM,I)
        DO J=1,NDIM-1
          IF (abs(DEPTH) .gt.1.e-20)       !- avoid a divide by zero.
     &    GC(J,I) = GC(J,I) / DEPTH
        ENDDO
      ENDDO
      NDIM = NDIM - 1
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE R_CONNECTION (IO,GC,IGC,NDIM,NN,NUMS,INUMS, NEL, P)
c
c     This reads in an inter-processor communication link
c       Dan Kidger 15-4-98
c     Data as:
c        IPROC
c        NN_connection
c        < list of nodes >
c
      REAL GC(IGC,NN)
      INTEGER  NUMS (INUMS,NEL)      !- element steerings
     &           ,P (*)              !- data structure

      save
      DATA ICALL,ibase/0,2/
c------ 0: initialise ------
      ICALL = ICALL+1
      IF (ICALL.EQ.1) then
         NCONNECTIONS=0     !- initialy none
         ibase=2            ! length of data structure
         p(1) = -123        ! to store the total #connections
      endif

c------ 1: read data ------
    1 READ (IO,*,IOSTAT=IOS) IPROC
      CALL IN_TEST (IO,IOS,*1,*999)
      READ (IO,*,IOSTAT=IOS) NN_CONNECTION
      p(ibase+1)= iproc
      p(ibase+2)= nn_connection
      CALL IN_TEST (IO,IOS,*1,*999)
      READ (IO,*,IOSTAT=IOS) (P(ibase+2+i),I=1,NN_CONNECTION)
      ibase=ibase + 2+nn_connection

      nconnections=nconnections+1
      p(1) =nconnections

c.. hence can quickly parse this to get:
c  The list of other porcessors
c  The max node # to send to each (hence earliest possible send)
c  can count the #comms each node makes (hence factor down dot_products)

c  Hence a send_values_mpi function: given a set of freedoms
c  uses NF and this P() to build and so send a message
c     if (ipr.ge.2)
      write (*,'(i3,a,i3, a,i7,a,i7)')
     & icall,' : call to', iproc, 
     & ' .. n=',nn_connection, ' l=',ibase
  999 continue
      return
      end

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
c     SUBROUTINE CONSTRUCT_VORONOI ()
c
c     This constructs the Voronoi tessalation around the given NN nodes.
c     A further NNV nodes are constructed and NELV polygons are constructed . 
c        Dan Kidger 21-8-97
c
c   1. This is 'fun' to do after a Delaunay triangulation to contrast.
c   2. The external Voronoi edges give semi-polygons - just store as rods ?
c   3. (Simplex algorithm is to take each node and use all other nodes to 
C      construct their bisectors and 'accumulate' into a polygon. - then
C      just loop and 'find_or_add' each node and save the polygon.)
c   4. Contrast edge 'C's with real boundary elements (BEM)?
c
c  Better method:    26-8-97
C     Note that after Delauney we known which edges to bisect, so:
c   1. For each node (or do simultaneously)
c   2.  Loop mesh and find those elements that touch this node,
c   3.  just consder the 'clockwise' of the 2 touching edges.
c   4.  Construct the mid-edge point and hence the normal (clockise) vector
c   5.  Then we need to intersect these together ??
c      - ? a/ maybe initially just store the set of nodes around 'my' node.
c            - we can use NUMS for this - incrementaly build
c      - now for each of these individually...
c      -   b/ sort these into clockwise order
c          c/ take the first: build its normal vector
c          d/ take the next and intersect to give a new point
c          e/ take the third and test against both? 
c
c   ie. need an algorithm to return the set of 'closest' intersection points
c    of a set of lines (normal bisectors).
c
c
c
c     RETURN
c     END

C-----------------------------------------------------------------------
      SUBROUTINE R_COPY_MESH (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL)
c
c     Header to COPY_MESH to make n copies of a mesh
c
      REAL       GC (IGC,*) ,XS(5)   !-
      INTEGER  NUMS (INUMS,*)        !-
    1 READ (IO,*,IOSTAT=IOS) ncopies, (XS(J),J=1,ndim)
      CALL IN_TEST (IO,IOS,*1,*999)
      CALL COPY_MESH (GC,IGC,NDIM,NN,NUMS,INUMS,NEL, ncopies,XS)
      RETURN
 999  CALL MYERROR (2,'Missing opcode in *COPY_MESH')
      END

C-----------------------------------------------------------------------
      SUBROUTINE COPY_MESH (GC,IGC,NDIM,NN,NUMS,INUMS,NEL, ncopies,XS)
C
C     This makes 'ncopies' of the given mesh
C     each copy is displaceed dx,dy,dz from the previous
c     Note no attempt is made to stitch-up the pieces. cf. *MERGE_NODES
C
      REAL       GC (IGC,*) ,XS(5)   !-
      INTEGER  NUMS (INUMS,*)        !-

      NEL_DUP = NEL    !- # to duplicate
      NN_DUP  = NN     !- # to duplicate
      DO ICOPY = 1,NCOPIES

        DO I=1,NN_DUP      !-------- shift the nodes
          DO J=1,NDIM
            X = GC(J,I) + XS(J)*ICOPY
            GC (J,NN+I) = X
          ENDDO     
        ENDDO

c       CALL DUP_ELEMENTS (NUMS,INUMS,NEL,NEL, NN ,1) !- another 'copy'
        CALL DUP_ELEMENTS (NUMS,INUMS,NEL,NEL, NN ,0) !- 0=no mirror 
        NN     = NN  +NN_DUP
        NEL    = NEL +NEL_DUP    !- update NN and NEL
      ENDDO
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE R_COPY_MESH_ARC (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL)
c
c     Header to COPY_MESH_ARC
c
      REAL       GC (IGC,*)
      INTEGER  NUMS (INUMS,*)
    1 READ (IO,*,IOSTAT=IOS) ncopies, IDIR,THETA
      CALL IN_TEST (IO,IOS,*1,*999)
      CALL COPY_MESH_ARC (GC,IGC,NDIM,NN,NUMS,INUMS,NEL,
     &     ncopies,IDIR,THETA)
      RETURN
 999  CALL MYERROR (2,'Missing opcode in *COPY_MESH_ARC')
      END

C-----------------------------------------------------------------------
      SUBROUTINE COPY_MESH_ARC
     &       (GC,IGC,NDIM,NN,NUMS,INUMS,NEL, ncopies, IDIR,THETA)
C
C     This makes 'ncopies' of the given mesh
C     each copy is rotated THETA about the 'y' axis
C       ... hmm a given axis is better cos 2D should be about the 'z' !
C
      REAL       GC (IGC,*)
      INTEGER  NUMS (INUMS,*)

      NEL_DUP = NEL    !- # to duplicate
      NN_DUP  = NN     !- # to duplicate
      PI = 4.*ATAN(1.)
      DO ICOPY = 1,NCOPIES

        ANG= THETA * PI/180. * ICOPY
        print*,'ang=',ang
        DO I=1,NN_DUP         
          CALL ROTATE_POINT( GC(1,I), GC(1,NN+I), NDIM,  IDIR, ANG)
        ENDDO

        CALL DUP_ELEMENTS (NUMS,INUMS,NEL,NEL_DUP, NN_DUP*ICOPY ,0) !- another 'copy'
        NN     = NN  + NN_DUP
        NEL    = NEL + NEL_DUP    !- update NN and NEL
      ENDDO
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE ROTATE_POINT (P1,P2, NDIM,  IDIR, ANG)
C
C     This rotates the point P1 about the origin to P2 by ANG degrees
C     IDIR is the axis 3=z-axis (=x-y plane), etc. 
c      (all clockwise when looking towards the origin down an axis)
C      ... rotation about an arbitary axis will be by a MV_MULT
C         .. daughter of COPY_MESH_ARC etc.
C         DJK 2-9-94
C
      REAL P1(NDIM), P2(NDIM), ANG

      CA = COS(ANG)
      SA = SIN(ANG)
      IF (IDIR.EQ.3.OR.NDIM.EQ.2) THEN     !- 2D *MUST* be in plane !
        P2(1) =   P1(1) * CA + P1(2) * SA
        P2(2) = - P1(1) * SA + P1(2) * CA
        IF (NDIM.GE.3) P2(3) = P1(3)
      ELSEIF (IDIR.EQ.1) THEN
        P2(2) =   P1(2) * CA + P1(3) * SA
        P2(3) = - P1(2) * SA + P1(3) * CA
        P2(1) = P1(1)
      ELSEIF (IDIR.EQ.2) THEN
        P2(3) =   P1(3) * CA + P1(1) * SA
        P2(1) = - P1(3) * SA + P1(1) * CA
        P2(2) = P1(2)
      ELSE
        STOP 'Unknown IDIR for Rotating coords'
      ENDIF
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE R_MIRROR_MESH (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL)
c
c     Just a Header to MIRROR_MESH so we can read in the control params
c
      REAL       GC (IGC,NDIM)         !-
      INTEGER  NUMS (INUMS,*)          !-
    1 READ (IO,*,IOSTAT=IOS) IDIR, VAL
      CALL IN_TEST (IO,IOS,*1,*999)
      CALL MIRROR_MESH (GC,IGC,NDIM,NN,NUMS,INUMS,NEL, IDIR,VAL)
      RETURN
 999  CALL MYERROR (2,'Missing opcode in *MIRROR_MESH')
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE MIRROR_MESH (GC,IGC,NDIM,NN,NUMS,INUMS,NEL, IDIR,VAL)
C
C       This mirrors a mesh about a given plane
C        ie. where IVAL (x/y/z) = VAL
C     *note be careful that some mirrored elements will have negative DETs :-(
c..... also be careful about the size of the mesh as we mirror it ...
C
      REAL       GC (IGC,*)          !-
      INTEGER  NUMS (INUMS,*)        !-

      DO I=1,NN          !--------- Mirror the nodes ------------
        DO J=1,NDIM
          X = GC(J,I)
          IF (J.EQ.IDIR) X = 2*VAL - X     ! reflect a coord
          GC (J,NN+I) = X
        ENDDO     
      ENDDO

      CALL DUP_ELEMENTS (NUMS,INUMS,NEL,NEL, NN ,1) !- another 'copy'

      NN     = NN  * 2     !- so 
      NEL    = NEL * 2
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE DUP_ELEMENTS (NUMS,INUMS,NEL,NEL_DUP ,NN,IMIRROR)
C
C     This duplicates NEL_DUP elements in NUMS to *after* NEL 
C     --- this is a daughter of MIRROR_MESH .. and DUP_MESH_ARC etc.
C
C     where IMIRROR =
C                 0 : simply copy elements
C                 1 :'mirror' each element in copying
C
      INTEGER  NUMS (INUMS,*)        !- element steering
     +         ,NUM (32)             !- nodes of one element

      DO IEL=1,NEL_DUP
        CALL GET_ELEMENT 
     &      (NUMS,INUMS,IEL, NUM, NOD,NDIME,ITYPE, IMAT,IU1,IU2,IU3)
        DO J=1,NOD
          NUM(J) = NUM(J) + NN        !- add the offset
        ENDDO

        IF (IMIRROR.EQ.1)         !- mung NUM if mirroring 
     &    call mirror_element (num, ndime,nod,itype) 

        CALL PUT_ELEMENT 
     &    (NUMS,INUMS,NEL+IEL, NUM, NOD,NDIME,ITYPE, IMAT,IU1,IU2,IU3)
      ENDDO
      RETURN
      END

c-----------------------------------------------------------------------
      SUBROUTINE MIRROR_ELEMENT (num, ndime,nod,itype) 
c
c      Algorithm for munging NUM into its mirror image
c      .. used by MIRROR_MESH and FLIP_NEGATIVE_DETs
c      This does an explcit flip for elements it knows about, otherwise
c      it just reverses the NUM as if it was a simple closed polygon
c      (put this routine into SHAPE.F ?)
c
C     .. The mirroring needs to be made more powerful (using WTHATN)
C     .. to properly invert 20nb's (say)
c.. *** I really need some code to LOCALLY invert NUM
c.. do we get the Jacobian without being negated 
c
      INTEGER NUM(*), NUM2(32), NDIME,NOD,ITYPE

      DO I=1,NOD            !-- copy the original
        NUM2(I) = NUM(I)
      ENDDO
      NUM(1) = 0   !- just a flag
C     ICODE = NDIME*100 + NOD + max(ITYPE,1)*1000

      IF (NDIME.EQ.2) then
c.. for 5,9,17 then last node is in the centre
c.. must check that this is ok ?
c.. It actualy makes more sense to use the FACETS information.
c for 15nt the 2 of the 3 centre nodes need swapping
        IF (NOD.EQ.5.or.NOD.eq.9.or.NOD.eq.17.or.NOD.eq.10) THEN 
          NUM(1) = num2(1)
          DO J=2,(NOD-1) /2
            NUM(J)       = NUM2((NOD-1)+2-J)
            NUM((NOD-1)+2-J) = NUM2(J)
          ENDDO
c          do i=1,nod-1
c            num(i) = num2(nod-i)
c          enddo
        ENDIF

      ELSEIF (NDIME.EQ.3) then
        IF (NOD.EQ.8) THEN
          DO J=2,(NOD-1) /2
            NUM(J)       = NUM2((NOD-1)+2-J)
            NUM((NOD-1)+2-J) = NUM2(J)
          ENDDO
          do i=1,4
            num(i)   = num2(i+4)
            num(i+4) = num2(i)
          enddo    
        ELSEIF (NOD.EQ.20) THEN
          do i=1,8
            num(i)    = num2(i+12)
            num(i+12) = num2(i)
          enddo       !(note nodes 9,10,11,12 remain unchanged)
c        ELSEIF (NOD.EQ.14) THEN   !-- need more work on this one
c          do i=1,8
c            num(i)    = num2(i+12)
c            num(i+12) = num2(i)
c          enddo  
         ENDIF
      ENDIF

C---------------- otherwise just 'flip' as if a polygon ---------- 
      IF (NUM(1).EQ.0) THEN     !-  no explicit rule was found
        NUM(1) = num2(1)
        DO J=2,NOD /2 +1          ! +1 added DJK 29-3-96
          NUM(J)       = NUM2(NOD+2-J)
          NUM(NOD+2-J) = NUM2(J)
        ENDDO
c        do i=1,nod
c          num(i) = num2(nod+1-i)
c        enddo
      ENDIF
      RETURN
      END

c-----------------------------------------------------------------------

C-----------------------------------------------------------------------
C   SECTION 2:     Toolbox routines 
c
c   ... ie. not MODULES in their own right
C-----------------------------------------------------------------------

      SUBROUTINE GET_NORMALS (GC,IGC,NDIM,NN,NUMS,INUMS,NEL, 
     &                           NORMALS,MDF,P)
c
c... note that GET_NORMALS uses DISPS too ! 
c     This find the mean normal direction at each node
c     and stores it in the DISPS table (as the first load-case)
c     This is only really useful for 3D polygonal meshes (eg. OFF format)
c     because we only use the FIRST facet of each element
c         Dan Kidger   18-11-93
c
C ... I *could* avoid P() by using the 3rd column of GDISPS ??
C... note the use of GET_FACE from DANPLOT to get the nodes an one of 
c... (6) faces of an element

      INTEGER NUMS (INUMS,*)      ! elements
     &          ,P (*)            ! # of elems per node (workspace)
     &        ,NUM (32)           ! nodes of an element
     &         ,FS (32)           ! nodes on a facet

      REAL      GC (IGC,*)        ! nodal coords
     &      ,NORMALS (MDF,*)        ! BIG table of displacements

c------------------------- null the arrays ----------------------------
c  call NULL2D ?
      DO I=1,NN
        DO J=1,NDIM
          normals(J,I) = 0.
        ENDDO
      ENDDO
      DO I=1,NN
        P(I) = 0       !- to count the number of facets at a node
      ENDDO

c----------------- loop the elements and build the facets --------------
      DO IEL=1,NEL
        IFACE = 1          !.. cos always only 1 facet per elem
        CALL GET_ELEMENT   (NUMS,INUMS,IEL, NUM, NOD,NDIME,ITYPE,
     &                                            IMAT,IU1,IU2,IU3)
        CALL GET_FACE (NUM,NOD,NDIME,ITYPE,IFACE,FS,NN_F,NN_FT, 2)
c........ here FS() is usu. just 1,2,3,4..NOD ! ('cos of .OFF)
c.. now loop the nodes and find the direction-cosines of this face 
        C1 = 0.
        C2 = 0.
        C3 = 0.
        DO I=1,NN_FT
          K  = FS (I)                  !- node #
          J  = FS ( MOD(I,NN_F) + 1)   !- node #+1
          C1 = C1 + (GC(3,K)+GC(3,J)) * (GC(2,J)-GC(2,K)) /2. 
          C2 = C2 + (GC(1,K)+GC(1,J)) * (GC(3,J)-GC(3,K)) /2.
          C3 = C3 + (GC(2,K)+GC(2,J)) * (GC(1,J)-GC(1,K)) /2.
        ENDDO
        C4 = ABS (C1*C1 + C2*C2 + C3*C3 )
        IF (C4.GT.1.E-12) THEN      !- skip Zero-area-elems :-)
          C4 = SQRT(C4)
          DO I=1,NN_FT
            K = FS(I)               !- the actual node number
            normals(1,K) = normals(1,K) + C1 /C4
            normals(2,K) = normals(2,K) + C2 /C4
            normals(3,K) = normals(3,K) + C3 /C4
            P(K) = P(K) + 1      !- increment # of faces at this node
          ENDDO
        ENDIF
      ENDDO !- loop elements

C-------------------- now average the values --------------------------
c.. and normalise too ??
      DO I=1,NN
        IF (P(I).GT.0) THEN        !- skip 'orphan' nodes
          DO J=1,3
            normals(J,I) = normals(J,I) / real (P(I))
          ENDDO
        ENDIF
      ENDDO
      RETURN
      END

c-----------------------------------------------------------------------
      SUBROUTINE CALC_VOLUMES (GC,IGC,NDIM,NN,NUMS,INUMS,NEL, 
     &                           DISPS,MDF,nodof,nbins)
c
c     This takes a mesh and its displacements and calculates the 
c     volume (area in 2d) of the mesh that has a mean displacemnt 
c     greater than a given value (in steps of 10% maximum)
c

      INTEGER NUMS (INUMS,*)      ! elements
     &         ,n_el_bin (0:50)     ! element count
     &         ,n_el_bin_cum (0:50)     ! element count - cummulative

      REAL      GC (IGC,*)        ! nodal coords
     &      ,DISPS (MDF,*)        ! BIG table of displacements
     &       ,area_bin (0:50)       ! total areas
     &       ,area_bin_cum (0:50)       ! total areas - cummulative

      PARAMETER (M_NOD =  32       !-- max # nodes per element
     &          ,M_NDIM= 4            !-- max # freedoms per node
     &          ,M_NODOF= 4        !-- max # freedoms per node
     &          ,ISMP  = 27        !-- max GP's per element
     &          ,IDER=M_NDIM  )
      integer     NUM (32)           ! nodes of an element

      real        SMP (ISMP,M_NODOF)   !-- Integration sampling points
     &           ,WTS (ISMP)           !-- Integration point 'weights'
     &       ,FUN (M_NOD)             !-- shape funs
     &       ,DER (IDER,M_NOD)        !-- derivs of shape funs

c---- 0: initialise ----
c     nbins=10    !11 or try 20
      do ibin=0,nbins
        n_el_bin(ibin) = 0      !- count elements
        area_bin(ibin) = 0.     !- sum areas
        n_el_bin_cum(ibin) = 0      !- count elements - cummulative
        area_bin_cum(ibin) = 0.     !- sum areas- cummulative
      enddo

c---- 1: calculate the maximum displacemnt ----
      D_Max  = 1.e-25
      DO I=1,NN
        DISM1 = 0.
        DO J=1,ndim  !NODOF
          DISM1 = DISM1 + DISPS(J,I)**2
        ENDDO
        D_Max = MAX(D_max,DISM1)
      ENDDO
      D_Max = SQRT(D_max)

c---- 2: loop the elements and compute mean dispalcement ---------
c-- note a more accurate method is to loop gauss points and sample the 
C   DISP here.
c   This is of course good for shear strains too.

      DO IEL=1,NEL
        CALL GET_ELEMENT   (NUMS,INUMS,IEL, NUM, NOD,NDIME,ITYPE,
     &                                            IMAT,IU1,IU2,IU3)
c--- get the area of the element..
c   ie. sample at the centre, then muliply by area in local coords
        CALL GET_EL_DET (GC,IGC,NDIM,NN,NUMS,INUMS,NEL,IEL,DET)
      
c... hmm this seem quite complicated. :-(
c        CALL WTHATN (NOD,NDIME,ITYPE,COORD_L,ICOORD)
c        volume_local=1.
c        do 
c        CALL GET_ANY_GAUSS (SMP,ISMP,WTS,NDIME,NGP,NOD, AREA)
      area=-0.12345
      if (ndim.eq.1) area=2.   !- arbitary hack
      if (ndim.eq.2) area=4.   !- arbitary hack
      if (ndim.eq.3) area=8.   ! only valid for quads and bricks

c---------- loop gauss points ----------
        CALL GET_DEFAULT_NGP (NOD,NDIME,NGP)  !- cf user-defined
        ngp=1
        wts(1) = 1.     !- default
        CALL GET_ANY_GAUSS   (SMP,ISMP,WTS,NDIME,NGP,NOD,AREA)

        do igp=1,ngp
          CALL GSF  (NDIME,NOD,ITYPE, DER,IDER,FUN,SMP,ISMP,igp)

        d_mean=0.
        DO I=1,NOD
          DISM1 = 0.
          DO J=1,ndim  !NODOF
c.. for sub-sampling, we need to 'sample' at this point
            dis = DISPS(J,num(I)) *FUN(i)  *WTS(igp) 
            DISM1 = DISM1 + DIS**2
          ENDDO
          dism1=sqrt(dism1)         !- rms

          d_mean=d_mean+dism1       !- mean movement of this element
        ENDDO
        d_mean=d_mean/real(nod)

        volume = area*det *WTS(IGP)
c--- now find its bin 0-> nbins
        ibin= int(d_mean/d_max * nbins)
        ibin=min(ibin,nbins)                        !- clip
        n_el_bin(ibin) = n_el_bin(ibin)+1           !- count elements
        area_bin(ibin) = area_bin(ibin) + volume    !- sum areas

        ENDDO !- loop Gauss points      !(actualy only one)
      ENDDO !- loop elements

c---- convert to cummulative -----
c.. note reverse order so bin #0 is always 100%
      nel_tot = 0
      area_tot = 0.

      write (*,'(a)') 'ibin :  %, nel, area, änel, äarea'
      do ibin=nbins,0,-1
        nel_tot=nel_tot+n_el_bin(ibin)
        area_tot=area_tot+area_bin(ibin)
        n_el_bin_cum(ibin) = nel_tot
        area_bin_cum(ibin) = area_tot
      enddo

      do ibin=0,nbins

        write (*,'(i4,a,f6.1, i6,g15.4,i6,g15.4)') 
     &      ibin,' :', 100.*ibin/real(nbins)
     &     ,n_el_bin(ibin), area_bin(ibin) 
     &     ,n_el_bin_cum(ibin), area_bin_cum(ibin)
      enddo
      RETURN
      END

c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
c  K_MESH_MUNG .. those routines that do NOT change NEL or NN (or NDIM)
c
c   DJK 17-4-98
c+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

c-----------------------------------------------------------------------
c      SUBROUTINE KEY_MESH_MORPH
c     &      (FOUND,IPR,KEYWORD,IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL,P)
c-----------------------------------------------------------------------

C-----------------------------------------------------------------------
      SUBROUTINE WR_DANPLOT_KEY (IO,IPR,GC,IGC,NDIM,NN,NUMS,INUMS,NEL)
C
C     This writes out the mesh in 'Danplot' format .. new *Keyword style
c     or just call *WRITE_MESH genericaly ?
C
      REAL GC(IGC,*)
      INTEGER NUMS(INUMS,*), IO, NUM(32)

      call wr_danplot_ndim (io,ndim)
      call wr_danplot_snodes (io,nn)
      DO I=1,NN
         WRITE (IO,'(I6,5G14.6)') I,(GC(J,I),J=1,NDIM)
      ENDDO
      IF (IPR.GE.2)
     &WRITE(*,'(A,I7,A)')  '>< ',NN,' nodes written'
      call wr_danplot_selements (io,nel)
      DO IEL=1,NEL
        CALL GET_ELEMENT
     &  (NUMS,INUMS,iEL, NUM, NOD,NDIME,ITYPE, IMAT,I1,i2,i3)
         WRITE (IO,'(i6,i2,i3,i2, 99i8)')
     &   IEL,NDIME,NOD,ITYPE, (NUM(I),I=1,NOD),IMAT
      ENDDO
      IF (IPR.GE.2)
     &WRITE(*,'(A,I7,A)')  '>< ',NEL,' elements written'

      RETURN
      END  

c-----------------------------------------------------------------------
      subroutine wr_danplot_ndim (io,ndim)

      WRITE (IO,'(A)') '#',
     & '# A Keyword based output file suitable for plotting by the',
     & '# DANPLOT package,   by Dr. Dan Kidger   d.kidger@man.ac.uk',
     & '#'
      IF (NDIM.EQ.1) WRITE (IO,'(A)') '*ONE_DIMENSIONAL'
      IF (NDIM.EQ.2) WRITE (IO,'(A)') '*TWO_DIMENSIONAL'
      IF (NDIM.EQ.3) WRITE (IO,'(A)') '*THREE_DIMENSIONAL'
      IF (NDIM.EQ.4) WRITE (IO,'(A)') '*FOUR_DIMENSIONAL'
      return
      end

c-----------------------------------------------------------------------
      subroutine wr_danplot_snodes (io,nn)

      WRITE (IO,'(A)') '*NODES'
     & ,'#    Table of Nodal Coordinates'
     & ,'# Data as: node number and its x,y,(z) position'
      end

c-----------------------------------------------------------------------
      subroutine wr_danplot_selements (io,nel)

      WRITE (IO,'(A)') '*ELEMENTS'
     & ,'#      Table of element toplogy and type'
     & ,'# Data as: elem.#, elem. code, list_of_nodes, material number'
     & ,'#   where an elem. code of "2 8 1" indicates a "2-dimensional,'
     & ,'#   8-noded element of type 1=standard type"'
      end

c-----------------------------------------------------------------------
      SUBROUTINE R_GETELT (IO,NUMS,INUMS,NEL,NDIM) 
C                
C     This reads in the element material types in integer x,y,z from/to
C        Dan Kidger  19-1-93  .. moved to MESH1.FOR from L1A.FOR
c
C     Data Format: <mat type, xfrom,xto, yfrom,yto,(zfrom,zto) >
C
      INTEGER NUMS(INUMS,*), XF(5),XT(5)

      DO KK=1,9999
    1   READ(IO,*,IOSTAT=IOS) MATYPE, (XF(I),XT(I),I=1,NDIM)
        CALL IN_TEST(IO,IOS,*1,*999)
        IC = 0
        DO IEL=1,NEL
          NOD = NUMS (INUMS,IEL)  
          IFLAG = 1
          DO J=1,NDIM
            IPQR = NUMS(INUMS-3-J,IEL)
            IF (IPQR.LT.XF(J).OR.IPQR.GT.XT(J) ) IFLAG = 0   !-Not OK
          ENDDO
          IF (IFLAG.EQ.1) THEN
            IC=IC+1
            NUMS (INUMS-3,IEL) = MATYPE    
          ENDIF
        ENDDO   !---- loop elements
        WRITE(*,'(A,I4,A,I7,A,I4)')
     &  '>< ',KK,' :',IC,' Elements of type',MATYPE
      ENDDO
  999 RETURN
      END

C-----------------------------------------------------------------------
c      SUBROUTINE CONSTRUCT_DCEL (GC,IGC,NDIM,NN,NUMS,INUMS,NEL, 
c     &  wing,iwing,nedges,P)
      SUBROUTINE TO_WINGED_EDGE (GC,IGC,NDIM,NN,NUMS,INUMS,NEL, 
     &  wing,iwing,nedges,P)
C
C     This converts an existing F.E. mesh into a edge-based database.
c     This makes scanning the mesh much faster: O(N) rather than O(Ný) ?
C      started: 6-5-97 D.Kidger
c
c     Format as 1: Winged edge - pointers to both elements, both nodes
C                    and next/last edge of both elements
c               2: Doubly-connected edge list - as 1: but just store the 
c                    clockwise edges
c     Aditional pointers:
C      a/ one from every element to its 'base' edge
c      b/ one from every node to 'any' of its connected edges.
C      so total (integer) storage = NEL + NN + Nedges*6 
C           cf NUMS which is NEL*NOD)
C
C     Some other advantages:
c      1/ the maximum NOD per element is not limited
c      2/ in mixed meshes the 3nt's do not waste space.
c      3/ Boundary edges are simply those with edges with a nul element 
C         on one side (orphan edges have 2 nul elements)
c      4/ hence boundary nodes are those where at least one of its edges
c         have a nul element
c      5/ boundary polygons are found explicitly by tracing from any node
c         (loop edge to find 'seed' edges)
c         - cf material polygons. - where at each node we scan round it
c           *anticlockwise* to find the next hetero-mat edge
c
c
c Notes on WINGED_EDGES:
c    1/ As a table of 8 integers per edge. (+ nodal coords)
c    2/ We can extract NUM from it (so NUMS is unnecessary execpt for 9nq's)
c    3/ The 8 vals. are: - the 2 nodes at either end
c                        - the 2 faces on the LH and RH
c                        - the next and previous edge of the 2 faces.
c    4/ We also need GC to point to *one* of the edges at a node.   
c         - cf pointing to one of the elements in NUMS ?
c    5/ We also need to point to *one of the edges from NUMS too
c       hence we can pick the list of all elements that touch this one say.

c  19-8-97 Compare with the DCEL = 'Doubly-Connected-Edge-List'
c    here we only hold 6 integers - ie. only the 'clockwise' following
c    edge from either end.
c       - the upshot is that we can only follow polygons clockwise.
c

c   TO BUILD A WINGED_EDGE STRUCTURE
c     1/ loop elems - for edge store.
c       - for each *first* edge, mark this element to point to it
c       - for each edge mark both nodes to point at it.
c     (need to check wether this edge has been done yet)
c     (easy! - done if both its nodes have already been done, both
c        of these point to (an) edge - so scan round this node until we 
C        find 'our' edge
c
c  or...
c     2/ rank the (double) list
c     3/ crash detect the edges - for each pair store in W_E, and remove 
c        from the stack
c     4/ for each edge - for each side:
c         - get NUM hence find the 2 nodes on the fore and aft edges
c         - search the whole list for this edge and add double-pointers. :-(
c
c  -> or store directly - so we can store the next/last 3edge as we go 
c      along

c    ADDITIONAL ARRAYS
c    - for every element, we need to point to its *first* edge
c    - for every node, we need to point to *any* of its connected edges. 

c     Some abstractions from the database.
c     I think theere are *9* basic abstractions:
c   eg. A> ABSTRACT_NUM
c        - NUMS(IEL) points to WING(IEDGE)
c        - pick the LH/RH side of the EDGE (which side points to IEL)
c        - remember this edge, set nod=0 
c        - step to next edge and store a node (nod++)
c        - if edge /= base edge CYCLE

c      B> ABSTRACT_TOUCHERS - the lists of elems the element is adjacent to
c        - NUMS(IEL) points to WING(IEDGE)
c        - as ABSTRACT_NUM but pick up the other_elem from each edge.

c      C> SWEEP_THE_WHOLE_MESH - eg for the greedy algorithm. 
c
c      D> RETURN TOUCHING ELEMENTS
c         loop edges and build the list <cf ABSTRACT_NUM>
c         - note that for 8nq's etc. we should collapse out consecutive
c           occurences of the the same toucher (across mid-side nodes)

c  < 19-8-97 note that I can mung WING to that every element has its
c    first edge at position IEL - this elimnates additional 
c    pointers. However 3nt's have more elements that edges? :-(

      INTEGER WING (8,IWING)     !- the data structure
c     type winged_edge
c        integer :: n1,n2        ! the 2 nodes at either end
c        integer :: iel1,iel2    ! the left &right hand elements
c        integer :: e1,e2        ! the 'next' edge of el1 and el2
c        integer :: e3,e4        ! the 'previous' edge of el1 and el2
c     end type
c     type (winged_edge),allocatable :: wing
c     allocate (wing(iwing))
c.. perhaps we need some sort of re-allocate too?
c.. as a fibonaci series ?                                      
      
c------- 1: Initialise ------
c-- set all node-pointers to zero
c-- set all element-pointers to zero
c-- set #edges = 0

c------- 2: Loop and add elements -------
       
c------- 3: Loop and add edges -------
c.. via get_face

c------- 4: Is this edge new? -------
c.. yes if one of its two ends is as yet unmarked
c.. so simply just add

c------- 5: Not new so find in the database
c.. once found we mark its other side.

c------- 5:
c------- 5:
c------- 5:


      END !SUBROUTINE TO_WINGED_EDGE ()

C-----------------------------------------------------------------------
      SUBROUTINE FLIP_DET (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL,iop)
C
C     This 'Flips' over any elemnt that is upside-down (ie -ve area)
C     compare with DEL_ZERO_AREA which zap any 'odd' elements.
C        Dan Kidger  29-3-96
C
C     IOP=1 make all DETs +ve
C     IOP=2 make all DETs -ve
C     IOP=3 invert all elements
C
      REAL GC(IGC,*)
      INTEGER NUMS(INUMS,*), NUM(99)

      IC = 0
      print*,'iop=',iop
      DO IEL=1,NEL
        CALL GET_EL_DET (GC,IGC,NDIM,NN,NUMS,INUMS,NEL,IEL,DET)

c---- should we flip it over ? ---
        IF ( (IOP.EQ.1.AND.DET.LT.0.) .OR.
     &       (IOP.EQ.2.AND.DET.GT.0.) .OR. IOP.EQ.3) THEN

          IC = IC + 1 
          CALL GET_ELEMENT (NUMS,INUMS,IEL, NUM, NOD,NDIME,ITYPE
     &    ,IMAT,IUSER1,IUSER2,IUSER3)
          CALL mirror_element (num, ndime,nod,itype) 
          CALL PUT_ELEMENT (NUMS,INUMS,IEL, NUM, NOD,NDIME,ITYPE
     &    ,IMAT,IUSER1,IUSER2,IUSER3)
        ENDIF
      ENDDO
      WRITE(*,'(A,I7,A)')  '>< ',IC,' elements flipped over'
      RETURN
      END

C-----------------------------------------------------------------------
c     Material setting routines
C-----------------------------------------------------------------------
      SUBROUTINE R_CHANGE_MATS (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL)
C
C     This will change any elements of a given material type
C     to another material type
C
      IMPLICIT NONE
      INTEGER IO,IGC,NN,NDIM,INUMS,NEL,nchange
      REAL GC(IGC,NN)
      INTEGER NUMS(INUMS,NEL)
      INTEGER  KK,IOS,JMAT1,JMAT2  !- local vars

      DO KK = 1,999
    1   READ (IO,*,IOSTAT=IOS) JMAT1, JMAT2
        CALL IN_TEST (IO,IOS,*1,*999)
        CALL CHANGE_MATS (NUMS,INUMS,NEL, JMAT1, JMAT2, nchange)
      WRITE(*,'(A,I7,A,I4,A,i4)')  '>< ',nchange
     &  ,' elements changed from' ,JMAT1,' to',JMAT2
      ENDDO
  999 RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE CHANGE_MATS (NUMS,INUMS,NEL, JMAT1, JMAT2,nchange)
C
C     This will change any elements of a given material type
C     to another material type
C     - called from R_CHANGE_MATS 
C     - and also *EXCAVATE_LAYER and *BUILD_LAYER too
C
      IMPLICIT NONE
      INTEGER INUMS,NEL,NUMS(INUMS,NEL), JMAT1,JMAT2,nchange
      INTEGER IC,IEL,IMAT   !- local vars.
      IC = 0
      DO IEL=1,NEL
        CALL GET_EL_IMAT (NUMS,INUMS,IEL, IMAT)
        IF (IMAT.EQ.JMAT1) THEN               !-- If a 'good' element ..
          IC = IC + 1 
          IMAT = JMAT2
          CALL PUT_EL_IMAT (NUMS,INUMS,IEL, IMAT)
        ENDIF
      ENDDO
      nchange = ic
      RETURN
      END !subroutine change_mats

C-----------------------------------------------------------------------
      SUBROUTINE R_ELEMENT_SET (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL,P)
C
C     This read in a set of element #s and sets them all to the given
C     material #
C     .. Abaqus-inspired .. nice to have 1:23, 25, 27, 30:87 style :-)
C             DJK 11-3-95
C
      REAL GC(IGC,*)
      INTEGER NUMS(INUMS,*), P(NN)

      DO I=1,NN     !- zap all for tidyness
        P(I) = 0
      ENDDO
    2 READ (IO,*,IOSTAT=IOS) JMAT
      CALL IN_TEST (IO,IOS,*2,*999)

      READ (IO,*,IOSTAT=IOS) (P(I),I=1,NN+1)     !- why plus one?
c     CALL IN_TEST (IO,IOS,*1,*999)
      IC = I-1
      DO II=1,IC
        IEL = P(II) 
        CALL PUT_EL_IMAT (NUMS,INUMS,IEL, JMAT)
      ENDDO
      WRITE(*,'(A,I7,A,I4)')
     &   '>< ', IC,' elements set to mat type', jmat

  999 RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE R_MATS_BY_BOX (IO,IPR,GC,IGC,NDIM,NN,NUMS,INUMS,NEL)
C
C     this reads in a 'material group' based on  
C     'X-from, X-to, Y-from, Y-to, Z-from, Z-to' co-ords
C      where GC contains all the nodal co-ordinates     <-- CHECK
C
C     21-9-93 from L1A (READNF) to here as R_BC_BOX
C        .. really needs to be split into a daughter routine
C
      PARAMETER (ICOORD=32)
      INTEGER NUMS(INUMS,NEL), NUM(ICOORD)
      REAL GC(IGC,NN), XMIN(5),XMAX(5),XCEN(5), COORD(ICOORD,3)
c     CHARACTER FMT*25

      TOL=  1.E-20

      DO ILOOP = 1,99999
    1   READ (IO,*,IOSTAT=IOS) IMAT2, (XMIN(J),XMAX(J),J=1,NDIM)
        CALL IN_TEST (IO,IOS,*1,*999)

        IC = 0
        DO IEL=1,NEL
          CALL GET_ELEMENT (NUMS,INUMS,IEL, NUM, NOD,NDIME,ITYPE
     &    ,IMAT,IUSER1,IUSER2,IUSER3)
          IF (NOD.eq.0) goto 2    !- a 'nul' element
          CALL GET_COORD (NUM,NOD,NDIM,GC,IGC,COORD,ICOORD)    !- coords

          DO J=1,NDIM
            XCEN(J) = 0.
            DO I=1,NOD
              XCEN(J) = XCEN(J) + COORD(I,J)
            ENDDO
            XCEN(J) = XCEN(J) / REAL (NOD)
            IF (XCEN(J).LT.XMIN(J)-TOL) GOTO 2    !- skip to next element
            IF (XCEN(J).GT.XMAX(J)+TOL) GOTO 2    ! (cf CYCLE)
          ENDDO
          IC = IC + 1
          CALL PUT_EL_IMAT (NUMS,INUMS,IEL, IMAT2)
    2     CONTINUE                    !-  skip-to-point
        ENDDO
        IF (IPR.GE.2)
     &  WRITE(*,*) ILOOP,' :', IC, ' Elements set to imat=', imat2 
        IF (IC.EQ.0) CALL MYERROR (1,'no elements were found')
      ENDDO

 999  CONTINUE       !- next keyword found so exit the subroutine
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE R_MATS_BY_BOX_imat (IO,IPR,GC,IGC,NDIM,NN,
     &     NUMS,INUMS,NEL)
C
C     this reads in a 'material group' based on  
C     'X-from, X-to, Y-from, Y-to, Z-from, Z-to' co-ords
C      as R_MATS_BY_BOX but only affects the given material
C       28-8-98 Dan Kidger
C
      PARAMETER (ICOORD=32)
      INTEGER NUMS(INUMS,NEL), NUM(ICOORD)
      REAL GC(IGC,NN), XMIN(5),XMAX(5),XCEN(5), COORD(ICOORD,3)
c     CHARACTER FMT*25

      TOL=  1.E-20

      DO ILOOP = 1,99999
    1   READ (IO,*,IOSTAT=IOS) IMAT1,IMAT2, (XMIN(J),XMAX(J),J=1,NDIM)
        CALL IN_TEST (IO,IOS,*1,*999)

        IC = 0
        DO IEL=1,NEL
          CALL GET_ELEMENT (NUMS,INUMS,IEL, NUM, NOD,NDIME,ITYPE
     &    ,IMAT,IUSER1,IUSER2,IUSER3)
          IF (IMAT.ne.imat1.and.imat1.ne.230964) goto 2
          IF (NOD.eq.0) goto 2    !- a 'nul' element
          CALL GET_COORD (NUM,NOD,NDIM,GC,IGC,COORD,ICOORD)    !- coords

          DO J=1,NDIM
            XCEN(J) = 0.
            DO I=1,NOD
              XCEN(J) = XCEN(J) + COORD(I,J)
            ENDDO
            XCEN(J) = XCEN(J) / REAL (NOD)
            IF (XCEN(J).LT.XMIN(J)-TOL) GOTO 2    !- skip to next element
            IF (XCEN(J).GT.XMAX(J)+TOL) GOTO 2    ! (cf CYCLE)
          ENDDO
          IC = IC + 1
          CALL PUT_EL_IMAT (NUMS,INUMS,IEL, IMAT2)
    2     CONTINUE                    !-  skip-to-point
        ENDDO
        IF (IPR.GE.2)
     &  WRITE(*,*) ILOOP,' :', IC, ' Elements set to imat=', imat2 
        IF (IC.EQ.0) CALL MYERROR (1,'no elements were found')
      ENDDO

 999  CONTINUE       !- next keyword found so exit the subroutine
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE R_MATS_BY_IEL (IO,IPR,GC,IGC,NDIM,NN,NUMS,INUMS,NEL)
C
C     This reads in a set of element material numbers based on  
C      // IMAT, IEL_from, IEL_to //
C      (first used in the Tierra Armee 'arch')
C          DJK 1-9-95
C
      IMPLICIT NONE
      INTEGER IO,IPR,IGC,NDIM,NN,INUMS,NEL
      REAL GC(IGC,NN)
      INTEGER NUMS(INUMS,NEL)

      INTEGER ILOOP,IEL,IEL_FROM,IEL_TO,IC,IMAT, IOS

      DO ILOOP = 1,99999
    1   READ (IO,*,IOSTAT=IOS) IMAT, IEL_from,IEL_TO
        CALL IN_TEST (IO,IOS,*1,*999)

        IC=0
        DO IEL= MAX(1,IEL_FROM), MIN(IEL_TO,NEL)   !- clip :-)
          CALL PUT_EL_IMAT (NUMS,INUMS,IEL, IMAT)
          IC = IC + 1
        ENDDO
        IF (IPR.GE.3)
     &  WRITE(*,*) ILOOP,' :', IC, ' Elements set to imat=', imat 
        IF (IC.EQ.0) CALL MYERROR (1,'no elements were found')
      ENDDO

 999  CONTINUE       !- next keyword found so exit the subroutine
      END

C-----------------------------------------------------------------------
      SUBROUTINE WR_MATS_BY_IEL (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL,P)
C
C     This writes out the element material numbers based on  
C      // IMAT, IEL_from, IEL_to //
C      (first used in the Tierra Armee 'arch')
C          DJK 9-3-98

c     Algorithm:
c       1: loop elements 
c       2:  if first pick up its IMAT1 and IEL_from
c       3:  if imat/=imat1 .or iel=nel then..
c       4:     write line
c       5:  enddo

c     IMPLICIT NONE
      INTEGER IO,IGC,NDIM,NN,INUMS,NEL
      REAL GC(IGC,NN)
      INTEGER NUMS(INUMS,NEL), P(*)

      INTEGER IEL,IEL_FROM,IEL_TO,IMAT1,imat2

      WRITE (IO,'(A)') '*MATERIALS_BY_IEL' 

c    1   READ (IO,*,IOSTAT=IOS) IMAT, IEL_from,IEL_TO
c        CALL IN_TEST (IO,IOS,*1,*999)
      DO IEL=1,NEL
        IF (Iel.EQ.1) then
          iel_from=iel
          CALL GET_EL_IMAT (NUMS,INUMS,IEL, IMAT1)
        endif
        CALL GET_EL_IMAT (NUMS,INUMS,IEL, IMAT2)
        IF (IMAT2.NE.IMAT1) THEN
          IEL_TO=IEL-1      !- last good one
          WRITE (IO,'(I3,2I8)') IMAT1, IEL_FROM,IEL_TO
          iel_from=iel
          imat1=imat2
        ENDIF
      ENDDO
      WRITE (IO,'(I3,2I8)') IMAT1, IEL_FROM,NEL    !- finish list
      END

C-----------------------------------------------------------------------
c     Group number setting routines
c    
c     Groups are used for 3 purposes:
c       1:  LEGO algorithm for reducing storage
c       2:  Domain decomposition for parallel processing
c       3:  DD based on disconnected sets.
c
C-----------------------------------------------------------------------
      SUBROUTINE R_ELEMENT_GROUP (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL,P)
C
C     This read in a set of element #s and sets them all to the given
C     group #
C     .. Abaqus-inspired .. nice to have 1:23, 25, 27, 30:87 style :-)
C             DJK 11-3-95
c     data as: IGRP, (El#,1..nel)
C
      REAL GC(IGC,*)
      INTEGER NUMS(INUMS,*),P(NN)

      DO I=1,NN     !- zap all for tidyness
        P(I) = 0
      ENDDO
    2 READ (IO,*,IOSTAT=IOS) JGRP
      CALL IN_TEST (IO,IOS,*2,*999)

      READ (IO,*,IOSTAT=IOS) (P(I),I=1,NEL+1)
c     CALL IN_TEST (IO,IOS,*1,*999)
      IC = I-1
      DO Ii=1, IC
        iel = P(II)
        CALL PUT_EL_IGRP (NUMS,INUMS,IEL, JGRP)
      ENDDO
      WRITE(*,'(A,I7,A,I4)')
     &   '>< ', IC,' elements set to group type', jgrp

  999 RETURN
      END

c-----------------------------------------------------------------------
      SUBROUTINE SWEEP_IGRP (NUMS,INUMS, NEL,NN, P, IEL_SEED, IGRP 
     &   ,nel_todo, nel_group, NSWEEPS)      
c 
c     This marks sets of adjacent elements with IGRP 
C      by recursively sweeping the elements 
c       c. Dan Kidger 21-8-97 (orig 1994) 
c    Notes: 
c     Used by both *GROUP_GREEDY and *GROUP_CONNECTIVITY 
c     It terminates when either - a sweep found nothing new (exhasuted) 
c                            or - the target #elems had been done 
C     This would be much simpler with a DSEL edge structure! 
c 
c   where: 
c    P is set to /=0 for unvisited nodes 
c    so when P(I) changes we have found a boundary. 
c    NEL_TODO is the maximum that we want to be set to this group. 
c    NEL_GROUP is how many we actualy set (<=NEL_TODO) 
c    NSWEEPS is the returned: 'how many sweeps were necessary' 
c.. hmm should iterate this until nothing further changes 

      INTEGER  NUMS (INUMS,NEL)      !- element steerings 
     &         ,NUM (32)             !- one elements steering 
     &           ,P (NN)             !- workspace 
      LOGICAL DEFER 

       DEFER= .TRUE.    !- so we only build on 'old' nodes 
c      DEFER= .FALSE. 

      NSWEEPS = 0                    !- no sweeps yet. 
c----- 1: Mark the seed element -------
c.. should we validate the status of the seed element (its IGRP should=0) 
      CALL GET_EL_IGRP (NUMS,INUMS,IEL_SEED, IGRP1) 
      IF (igrp1.ne.0) call myerror(3,'Wrong Seed was used') 
      CALL PUT_EL_IGRP (NUMS,INUMS,IEL_SEED, IGRP) 
      CALL GET_ELEMENT (NUMS,INUMS,IEL_SEED, NUM, 
     &     NOD_A,NDIME,ITYPE, IMAT,IGRP1,IU2,IU3) 
c ** note here a varient where we only allow elements that share the 
c    same IMAT as the parent - this can be used to count disjoint 
c    block of the same material (cf my RINGS() structure which also 
c    achieves this ** 
c.. 29-1-98 We might wish to use this to get statisitics about how many 
c      materials there are and in how many disjointed pieces each is in. 

      DO J=1,NOD_a               !- set its nodes so we can build on them. 
        P(NUM(J)) = IGRP
      ENDDO
      NEL_GROUP=1  !-  mark the first element. 
      IF (NEL_GROUP.ge.NEL_TODO)  goto 1025  !- RETURN 

      NEL_DONE = 0               !- count how many elements were done. 

c----- 2: perform the sweeps ----------
      DO NSWEEPS= 1, NEL      !-  we will never need more than NEL-1 sweeps 
        idone = 0             !- flag how many were done in this pass 

c----- 3: sweep the whole mesh -------
c       DO IEL=IEL2+1,NEL              !- Loop the rest 
        DO IEL=1,NEL                   !- Loop the whole mesh 
          CALL GET_EL_IGRP (NUMS,INUMS,IEL, IGRP1) 
          IF (IGRP1.ge.1) goto 22     !- skip cos already been done 

            CALL GET_ELEMENT  (NUMS,INUMS,IEL, NUM, 
     &        NOD,NDIME,ITYPE, IMAT,IGRP1,IU2,IU3) 

c--- 4: test if we should add this element ---
c.. note that we want to check for: 
C     1: any one node marked 
C     2: any edge marked (eg. as 2 conseq. nodes) 
C     3: any facet marked (as all nodes on the face) 
c     - contrast simply testing the nodes as marked, or testing that 
c       they all touch the *same* other element (to avoid 'bridges') 
C 
c           N_C = 1   !- any node will do. 
            N_C=2    !.. to just build across edges, we need 2 consecutive matches.. 
            IC = 0 
            DO JJ=1,NOD+1            
              J = MOD(JJ,NOD)+1        !- cycle (watch 9nq, 15nt etc. ! 
c             .. realy we should be looping the around the *facets* 
              IF (P(NUM(J)).eq.IGRP) THEN 
                IC = IC + 1          !- found another consecutive one 
              ELSE 
                IC = 0 
              ENDIF                  !- reset my counter 
              IF (IC.GE.N_C) GOTO 21   !- found an edge 
            ENDDO                    
            GOTO 22    !- CYCLE cos this element is no good 
   21       CONTINUE 
c .. found an element to add. 
c .. DEFERING: for convexity I want the newly marked nodes to not be 
C    built upon until the end of the pass. 

            DO J=1,NOD              !- mark all of its nodes 
              IF (P(NUM(J)).EQ.IGRP) THEN  !- already current 
c               no-op 
              ELSE 
                IF (P(NUM(J)).ne.0) THEN    !- on another boundary 
c               .. but also record that this is a 'boundary' node? 
c               ?maybe I can prove that we do not need this as a seed? 
                ENDIF 
                IF (DEFER) THEN 
                  P(NUM(J)) = -123           !- defer 
                ELSE 
                  P(NUM(J)) = IGRP           !- immediate 
                ENDIF 
              ENDIF                   !- a fresh node? 
            ENDDO                  !- loop (8) nodes 

            CALL PUT_EL_IGRP (NUMS,INUMS,IEL, IGRP) 
            nel_group = nel_group+1 
            IF (NEL_GROUP.ge.NEL_TODO)  goto 1025  !- RETURN 
            idone = idone+1      !- flag that an 'hit' has happened 

   22       CONTINUE    !- CYCLE-to point if not a useful element. 
        ENDDO     !- loop the mesh 

        IF (DEFER) THEN     !- re-mark the defered nodes. 
          DO I=1,NN    
            IF (P(I).eq.-123) P(I) = IGRP 
          ENDDO 
        ENDIF 

        IF (idone.eq.0) goto 1025       !- EXIT (cos no more found) 
      ENDDO   !- loop sweeps 

 1025 CONTINUE 
        IF (DEFER) THEN     !- re-mark the defered nodes. 
          DO I=1,NN        !- or we could just zap them ?? 
            IF (P(I).eq.-123) P(I) = IGRP 
          ENDDO 
        ENDIF 
c     .. finished - so can print NEL_GROUP, NSWEEPS, etc. 

      RETURN 
      END 

C-----------------------------------------------------------------------
c     SUBROUITNE GROUP_BALANCE () 
c 
c     This attempts to optimise a 'Group'ing (eg. after Greedy) 
c     by swapping elements between groups on the Boundaries. 
c        Dan Kidger  21-8-97 
c 
c   The targets are: 
c      1/ An equal number of elements in each group 
c      2/ The minimum boundary lengths (hence message length) 
c      3/ The minimum number of zones, a zone is in contact with 
c      4/ no more than 3 (?) groups to meet at a node 
c      5/ contiguous regions (no isolated islands) 
c 
c     RETURN 
c     END 

C-----------------------------------------------------------------------
c     SUBROUITNE GROUP_SORT () 
c 
c     This simply arranges the elements in ascending group number 
c     - hence makes it easier to split onto multiple processors. 
c        Dan Kidger  22-8-97 
c 
c     RETURN 
c     END 

C-----------------------------------------------------------------------
c     We also create lists of which nodes are on the boundary and which 
c     processor to communicate them to. 
c      i.e node x pn my proc. is equal to node y on processor z 
c      hence if a far proc. decodes to renumber its nodes we simply 
C      update this link to. 
c 
c     notes: 
C       If we number the boundary nodes *first* then we can send them on to 
c       the next processor as early as possible? 
c 
c     Algorithm: 
c       treat each IGROUP in turn: 
c        find the boundary nodes 
c        ?for each boundary node broadcast a message to all other procs: 
C         'do you want this node?' - reply Y/N 
c         - ie. a message comes back with the list of nodes it wants. 
c         - so simply compile a list 
c        for each do DELETE_ORPHAN_NODES ( P() tells me how they have moved) 
c 
c      or. 
c        for every node complie a list of all the segments it is in 
c        0 as a list of values and pointer to next (0=nul pointer) 
c        a double -list of NN integers : set all second column to -1 
c        loop elements: 
c          for each of its nodes: 
c            call mark_node_igroup 
c          enddo 
c        enddo 
c        where mark_node_igroup is: 
c          is pointer = -1   if so add value here & set pointer=0; return 
c   iterate: is value here =IGROUP -> if yes return 
c        is pointer /=0 -> if yes recurse 
c        so 'add new igroup': 
c          get next free stack position ('malloc') 
c          make last point to this 
c          as new: insert value=igroup, pointer =0 
c       < note converse unmark_node_igroup  so it is dyanmic :-) > 
c      Now can: 
c         1/ show how many groups each is attached to, 
c         2/ for any 2 groups: in O(NN) show which nodes are in common 
c            (in ascending order too). 

c 
c     Now address the problem of coherence between processors 
c     really each node could carry its original node# as a label 
c     But the real issue is for each processor to carry a table 
c     that lists for each *other* processor which nodes to pass 
c     and also what that node is called on the other processor. 
c     data: 
c      jproc, nn_common, (old,new, i=1,nn_common)  // for each other proc. 
c     then we send a message to that processor, with DISPS say for 
c     these nodes, along with the list of nodes (that it calls them) 
c     (The latter is unnessary cos the receiver already nows which nodes 
C     they will be, cos it knows which iproc sent them. 
c 
c     INVERSION: 
c        For borders we want the nodes to appear in the same order for 
c     each processor; hence a processor does not actualy need to know 
c     what the other processor chooses to call it! 
c 

c     Another way to look at it is to have a table where there is a row 
c     for each *boundary* node (a node which is shared) 
c     then a row for each processor that this patch touches. 
c      eg   processor:    4    6   7   9  15 
c             node  5    34    0   0  27  27 
c     There will be lots of zeros :-) 

c     Ok stages: 
c     1. null P(NN), loop elements; for this IGRP: 
c         marks all its nodes in P 
c         thence rank P - this gives for each old, the new node numbers 
c         hence just write the *NODES for just these 
c         and the *ELEMENTS likewise  
c 
c     2. Loop all processors (but skip own) 
c        re-mark (=1) all P() for proc1 
c        mark (=2) all P() for this proc2 (that are =1) 
c        thnce count all '2's = NN to transfer 
c        simply write this integer list as *COMINICATE_NODES 
c        dat as proc1, proc2, NN_com 
c               << list of nodes >> 

C-----------------------------------------------------------------------
      SUBROUTINE wr_GROUPs (IO,GC,IGC,NDIM,NN,NUMS,INUMS, NEL, P) 
c 
c     This writes the mesh out to muliple files such that each file 
c      contains only elements that share the same group number 
c        Dan Kidger  22-8-97 
c     Such that each processor just sees a *local* node and element 
C     numbering. 
c 
c  17-2-98 What about a routine that just writes the mesh into NPROC files. 
c     - for each IGRP: 
c       null P(NN), loop elements; for this IGRP: 
c         marks all its nodes in P 
c       thence rank P - this gives for each old, the new node numbers 
c       hence just write the *NODES for just these 
c       and the *ELEMENTS likewise  
c       simple :-) 

      REAL GC(IGC,NN) 
      INTEGER  NUMS (INUMS,NEL)      !- element steerings 
     &           ,P (NN)             !- workspace 
      CHARACTER FILE*20              !- output filename 

c---------------------------- 0: initialise ---------------------------      
      ngroups=0 
      DO IEL=1,NEL 
        CALL GET_EL_IGRP (NUMS,INUMS,IEL, IGRP) 
        ngroups=max(ngroups,igrp)            !- how many groups altogether 
      ENDDO 

c------------------ 1: loop groups -------------------------
c.. open output file 
c... call dump_this_igrp 
c.. zap these elements ? - yes but I need them later to construct cross 
c      pointers 
      DO JGRP=1,NGROUPS 
         IO2= IO+10 
         print*,'io2=',io2 
         WRITE (FILE,'(A,I4.4,A)')  'grp',JGRP,'.d' 
         OPEN (IO2,FILE=FILE) 
         CALL DUMP_THIS_GRP (IO2,GC,IGC,NDIM,NN,NUMS,INUMS, NEL, P 
     &    ,JGRP) 
         CLOSE (IO2) 
      ENDDO 
      RETURN 
      END 

C-----------------------------------------------------------------------
      SUBROUTINE DUMP_THIS_GRP (IO,GC,IGC,NDIM,NN,NUMS,INUMS, NEL,P 
     &   ,JGRP) 
c 
c     This writes out elements of a given igrp/imat to a file 
c     It also writes out the table of interconnections *CONNECTIONS 
c        Dan Kidger  13-3-98 
c 
c     ?And then deletes them from the mesh? 
c     so occasionaly we can dump a mesh feature and get it back later? 

      REAL GC(IGC,NN) 
      INTEGER  NUMS (INUMS,NEL)      !- element steerings 
     &           ,P (NN)             !- workspace 
      INTEGER NUM(99), BUFFER (20) 

c---------------------------- 0: initialise ---------------------------      

c---- 2: switch off all nodes '-1' ------
      DO I=1,NN 
        P(I) = 0 
      ENDDO 

c---- 3: flag 'my' nodes as '-1' ---
      ngroups=0                   !- find the largest group number too. 
      DO IEL=1,NEL 
        CALL GET_EL_IGRP (NUMS,INUMS,IEL, IGRP) 
        ngroups=max(ngroups,igrp) 
c       CALL GET_EL_IMAT (NUMS,INUMS,IEL, IMAT) 
        IF (IGRP.EQ.JGRP) THEN 
          CALL GET_ELEMENT 
     &    (NUMS,INUMS,iEL, NUM, NOD,NDIME,ITYPE, IMAT,IGRP,i2,i3) 
          DO J=1,NOD 
            P(num(J))=-1 
          ENDDO 
        ENDIF 
      ENDDO 

c-- 4: flag boundary nodes as '-2' ---
      DO IEL=1,NEL 
        CALL GET_EL_IGRP (NUMS,INUMS,IEL, IGRP) 
c       CALL GET_EL_IMAT (NUMS,INUMS,IEL, IMAT) 
        IF (IGRP.NE.JGRP) THEN 
          CALL GET_ELEMENT 
     &    (NUMS,INUMS,IEL, NUM, NOD,NDIME,ITYPE, IMAT,IGRP,i2,i3) 
          DO J=1,NOD 
            IF (P(NUM(j)).eq.-1) P(NUM(J))=-2 
          ENDDO 
        ENDIF 
      ENDDO 

c.. note that at this stage, we can get a list of which other IGRPs 
c   are involved. so later only need to loop these. 

c------- 5: resequence -------
c. on first pass, number the boundary nodes 
c. on second pass, number the internal nodes 
      IC = 0 
      DO I=1,NN 
        IF (P(I).EQ.-2) THEN 
          IC = IC+1 
          P(I) = IC 
        ENDIF 
      ENDDO 
      NN_boundary = IC 
      DO I=1,NN 
        IF (P(I).EQ.-1) THEN 
          IC = IC+1 
          P(I) = IC 
        ENDIF 
      ENDDO 
      NN_group = IC 

c----------------------- write *NODES --------------------
c.. note that this is in the original order rather than the new. 
      call wr_danplot_ndim (io,ndim) 
      call wr_danplot_snodes (io,nn) 
c     WRITE (IO,'(A)') '*NODES' 
      WRITE (IO,'(A,I9)') '#  NN=',nn_group 
      WRITE (IO,'(A,I9)') '#  NN_boundary=',nn_boundary 
      DO I=1,NN        
        IF (P(I).GT.0) 
     &  WRITE (IO,'(I6,5G14.6)') P(I),(GC(J,I),J=1,NDIM)
      ENDDO 

c---------------------- write *ELEMENTS --------------------
      call wr_danplot_selements (io,nel) 
c     WRITE (IO,'(A)') '*ELEMENTS' 
      nel_group=0 
      DO IEL=1,NEL 
        CALL GET_EL_IGRP (NUMS,INUMS,IEL, IGRP) 
        IF (IGRP.EQ.JGRP) THEN 
          CALL GET_ELEMENT 
     &    (NUMS,INUMS,iEL, NUM, NOD,NDIME,ITYPE, IMAT,IGRP,i2,i3) 
          nel_group=nel_group+1 
          WRITE (IO,'(i6,i2,i3,i2, 99i7)') 
     &    nel_group,NDIME,NOD,ITYPE, (P(NUM(I)),I=1,NOD),IMAT 
        ENDIF 
      ENDDO 

c--------------------- 5: Connectivity ---------------------
c.. or call this as a daughter routine ? 
      nn_message_total=0     !- count total size 
      nconnections=0         !- how many neighbours 
      do jgrp2=1,ngroups 
        if (jgrp2.eq.jgrp) goto 22  !- skip myself 

c------- 9: reset the boundary nodes as -ve -----
      do i=1,nn 
        inode = abs(p(i)) 
        if (inode.le.nn_boundary) then 
          p(i) = -inode 
        else 
          p(i) = 0 
        endif 
      enddo 

c------ 10: flag the boundaries as +ve 
      DO IEL=1,NEL 
        CALL GET_EL_IGRP (NUMS,INUMS,IEL, IGRP) 
c       CALL GET_EL_IMAT (NUMS,INUMS,IEL, IMAT) 
        IF (IGRP.eq.JGRP2) THEN 
          CALL GET_ELEMENT 
     &    (NUMS,INUMS,IEL, NUM, NOD,NDIME,ITYPE, IMAT,IGRP,i2,i3) 
          DO J=1,NOD         !- turn -ves to +ve 
            IF (P(NUM(j)).lt.0) P(NUM(J))=abs(P(NUM(J))) 
          ENDDO 
        ENDIF 
      ENDDO 

c----- 11: count them ---------
      NN_MESSAGE=0 
      DO I=1,NN 
        IF (P(I).GT.0) NN_MESSAGE=NN_MESSAGE+1 
      ENDDO 

c-------- write *CONNECTIVITY -----
c- the list can be compressed as ifrom:ito particularly since we force the 
c- lowest numbered nodes to be on the boundary? 
      if (nn_message.eq.0) goto 22    !- nothing to do. 
c... maybe report some statistics on the total mesage sizes? 
      nn_message_total = nn_message_total + nn_message 
      nconnections = nconnections+1 

      WRITE (IO,'(A)') '*CONNECTION' 
      WRITE (IO,'(i6,A)') JGRP2,      '    !- the oher processor id' 
      WRITE (IO,'(i6,A)') NN_MESSAGE, '    !- how many nodes' 
      LBUF=0 
      DO I=1,NN 
        IF (P(I).GT.0) THEN 
          LBUF=LBUF+1           !- buffer as 10 numbers per line. 
          BUFFER(LBUF)=P(I) 
          IF (LBUF.EQ.10) THEN 
            WRITE (IO,'(i7,99i8)') (buffer(j),j=1,LBUF)  !- the list 
            lbuf=0 
          ENDIF 
        ENDIF 
      ENDDO 
      IF (LBUF.gt.0) 
     & WRITE (IO,'(i7,99i8)') (buffer(j),j=1,LBUF)  !- the last line of numbers 


c--------------------------------
   22   continue 
      enddo  !- loop other processors 

c--- summary table 
c     if (ipr.ge.2) 
      write(*,'(i5, a,i8,a,i8, a,i5,a,i8)') 
     &    jgrp,': nel=',nel_group, ' nn=',nn_group, 
     &   '  ncon=',nconnections,' total size=',nn_message_total 

      RETURN 
      END 


C-----------------------------------------------------------------------
c     Node (and Element) re-sequencing
C-----------------------------------------------------------------------
      SUBROUTINE R_SORT_NODES_POINT (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL,P)
C
C     This will sort the nodes wrt distance from a point
C
      REAL GC(IGC,*), CENTRE(5)
      INTEGER NUMS(INUMS,*), P(*)

    1 READ (IO,*,IOSTAT=IOS)  (CENTRE(J),J=1,NDIM)
      CALL IN_TEST (IO,IOS,*1,*999)
      CALL SORT_NODES_POINT (GC,IGC,NDIM,NN,NUMS,INUMS,NEL,P,CENTRE)
      RETURN
 999  CALL MYERROR(2,'Missing opcode in *SORT_NODES_POINT')
      END

C-----------------------------------------------------------------------
      SUBROUTINE SORT_NODES_POINT 
     &           (GC,IGC,NDIM,NN,NUMS,INUMS,NEL,P,CENTRE)
C
C     This will sort the nodes wrt distance from a point
C      *Note: that this routine holds DEPTHS() as a temp buffer :-(
C
      REAL GC(IGC,*), CENTRE(5)
      INTEGER NUMS(INUMS,*), P(*)

c#ifdef F90 then
c     REAL DEPTHS(NN)
c#else
      integer iworkspace
c     parameter (iworkspace=20000)
      parameter (iworkspace=125000)
      real DEPTHS(iworkspace)          ! fixed array
c#endif

c#ifndef F90 then
      IF (NN.GT.iworkspace) CALL MYERROR (3, 
     &  'Too many nodes (SORT_NODES_POINT)')
c#endif

      DO I=1,NN
        D = 0.
        DO J=1,NDIM
          D = D + (GC(J,I) - CENTRE(J))**2
        ENDDO
        DEPTHS(I) = D
      ENDDO

      CALL INDEX_REALS  (DEPTHS,NN, P)
c     CALL GET_SORT (P,DEPTHS,NN)

c... we need to flip P() from indexing (as in depth_sort)
C... to a ranking (cf 'value' ie. the inverse)
c........ isn't this next bit the same as 'JUGGLE_P' ?? ....
c... cf as a subroutine like in "NR in F77"
       OPEN (97,STATUS='SCRATCH',FORM='UNFORMATTED')
       DO I=1,NN
         WRITE(97) P(I)
       ENDDO
       REWIND(97)
       DO I=1,NN
       READ(97) J
         P(J) = I
       ENDDO
       CLOSE(97)

      CALL UPDATE_GC   (GC,IGC,NDIM,NN, P)
      CALL UPDATE_NUMS (NUMS,INUMS,NEL, P)

      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE SORT_NODES_RANDOM (GC,IGC,NDIM,NN,NUMS,INUMS,NEL,P)
c
c     This juggles the nodes into a random order
c     - prehaps a good starting point for a Bandwidth optimier?
c           Dan Kidger   3-2-98
c
      REAL GC(IGC,*)
      INTEGER NUMS(INUMS,*), P(*)

      call shuffle_p (p,nn)
      CALL UPDATE_GC   (GC,IGC,NDIM,NN, P)
      CALL UPDATE_NUMS (NUMS,INUMS,NEL, P)
      RETURN
      END


C-----------------------------------------------------------------------
      subroutine shuffle_p (p,ncards)
c
c     This returns a list of NCARDS random integers
c     for example as used to shuffle a pack of cards.
C         Dan Kidger 3-2-98
C
      integer ncards,p(*)
      integer i,ipos,ival, nleft
      real get_random
      external get_random

      DO I=1,Ncards
        P(I)=I
      ENDDO
c.. Yes I do like the following shuffling algorithm: O(n) and no workspace!
      nleft=ncards
      DO I=1,Ncards
        ipos=I+int(nleft*GET_RANDOM())
          ival=p(i)               !- the next random number
          p(i)=p(ipos)
          p(ipos)=ival            ! fill the hole with this
        nleft=nleft-1
      ENDDO
      RETURN
      END

c-----------------------------------------------------------------------
      SUBROUTINE ELEMS_AT_NODES (NN,NEL,NUMS,INUMS,P, IBW_OPT)
c
c     This routines finds which elements are attached to each node
c     ie. the inverse relationship to that which NUMS holds.
c         Dan Kidger 19-10-98
c  Notes:
c   1. This information is need by 'Barycentric', 'Cuthill-Mcgee',
c      'Gouraud shading', but princiaply for 'Superconvergent Patch 
c       Recovery' for stress smoothing.
c       Often we can build up, what we need by sweeping all elements, 
c       then summing the contribution at each node.
c   2. The list of elements at a node is not well bounded - (some nodes 
c      in a triangulations may have 15+ elements), tetrahedra is worse.
c      so a fixed width table is not attractive (unlike NUMS?)
c   3. The sledgehammer approach is O(n^2) = loop nodes, then loop elements
c      and loop NUM, and find hits. Slightly better is to prefind for 
c      each node, what the min and max element no. here is.
c   4.:-)  For every element, around a node, this node may be the:
c         a) Highest numbered
c         b) lowest numbered
c         c) intermediate numbered
c      Triangles clearly only have one of type (c), mixed meshes will 
c      have more.
c   5. Thus we have solved the problem if we can find the 3 tables a,b 
c      and c - each a vector of length NEL. This table is analagous
c      to NUMS in size and shape.
c   6. To parse these tables, we need to find for each node#, the 
c      (contiguous) set of this node number in each column. Some may be 
c      empty - for example, node 1 will never be the *highest* numbered 
c      for any element!
c   7. Method 4. will fail if 2 nodes are collapsed together?
c   8. To build the tables, it seems likely that we will first want to 
c      take NUMS and for each element sort NUM into ascending order.
c   9. Then we sort verticaly, so that all the references to node 1 
c      will float to the top of column 1. But how do we remember which 
c      element it came from ? 
c  10. So perhaps we are realy ranking the columns, so we have a second 
c      matching table of columns of pointers to rows of NUMS_2.
c  11. Instead of NUMS_2, we can sequentialy pick off the next highest 
c      (or lowest) node #, by doing a maxval, but then setting this node 
c      to a -ve, so it wont be used next time. Put this node number 
c      into P(1:NEL), then rank and store the resulting pointer (to an 
c      element #) in NUMS_2. 
c  12. For 3nts in a 4nq mesh, then the last 'column' of P, will contain 
c      nulls, so after sorting NUMS_2 should be 'shorter' ie. pad with 
c      zeros at the end (or malloc the columns in F90).   
c
c *13: OK lets start with a simpler problem: CALL COUNT_ELEMENTS(NUMS,P)
c      - returns in P(1:NN), the # of elements attached here.
c      - in each sweep find the next largest element# attached to each node
c      method:
c        1. set all P to NEL+1
c        2. loop 1:999 (sweep)
c        2a.  P2()=0
c        3. loop 1 to NEL - extract and loop NUM
c        4.  - for each node INODE in NUM(1:NOD), compare its IEL , with the
c               stored value in P(INODE),
c        5. only if larger, then: 
c        6.    P2(INODE) =  min (P2(INODE,
c
c        6. if larger - skip
c

c      REAL GC(IGC,*)
      INTEGER NUMS(INUMS,*), P(*)  , NUM(99)

      NOD_MAX=3
      DO KK=1, NOD_MAX
        DO IEL=1,NEL
          CALL GET_ELEMENT 
     &    (NUMS,INUMS,IEL, NUM, NOD,NDIME,ITYPE, IMAT,IGRP,i2,i3) 
c---- find the next 'smallest' node # in NUM
          DO J=1,NOD
            INODE = NUM(J)
          ENDDO
c          idepths(IEL) = INODE
        ENDDO
c        CALL INDEX_INTEGERS (idepths,NEL, P)
c---- now store P as a column of NUMS_2 ---

      ENDDO

c---- to proove it works - lets parse NUMS and print the elements
c  around each node (and so check on a plot)


      END

c-----------------------------------------------------------------------
      SUBROUTINE CUTHILL (NN,NEL,NUMS,INUMS,P, IBW_OPT)
C
C     This routine optimises the bandwidth by renumbering the nodes
C     uisng the well-known Cuthill-Mcgee algorithm
C          Dan Kidger 1-3-98 
C      (based on an Original by R. Bougharou 1994)
C
      integer nums(inums,*), p(*)
      integer num(32), first,last,temp,iel
      logical sorted

c#ifdef F90 then
c     integer points(nn,ipoints),
c    & ,relate(nn), ordered(nn)
c#else
      integer iworkspace
      parameter (iworkspace=2000)     !20000x100=2M x4 =8Mb :-(
c     parameter (iworkspace=125000)
      parameter (ipoints=6)            !- for workspace :-(
c     parameter (ipoints=100)          !- for workspace :-(
c... hmm how do I know If I exceed the array bounds?
c     parameter (inn=iworkspace)
      integer points(iworkspace,ipoints)
     &       ,relats(iworkspace)       ! nodes attached a point
     &      ,ordered(iworkspace)       ! the nodes in ascending valency order
c#endif

c  Algorithm:
C  consider each node of the mesh and identify nodes connected to it.
C  store connected nodes in POINTS, and their number in RELATS.
C  also find the original band of the matrix ibw1.

c--------- 0: allocate workspace --------------
c#ifndef F90 then
      if (nn.gt.iworkspace) call myerror 
     &    (3,'Out of workspace (Cuthill)')
c#endif

c--------- 1: build list of other nodes attached to each node ----------
c contrast this with finding the list of elements around each node
c  ie. the 'true inverse' of NUMS.
      do i = 1,nn
        relats(i)=0                     ! null the count for each node.
      end do
      ibw_orig=0
      do iel = 1,nel                         !- loop elements
          CALL GET_ELEMENT (NUMS,INUMS,IEL, NUM, NOD,NDIME,ITYPE
     &    ,IMAT,IUSER1,IUSER2,IUSER3)
        do i = 1,nod
          node = num(i)                      !- pick a node
          do j = 1,nod
            if (i.eq.j) goto 11              !- no need to add myself. (CYCLE)
            node2=num(j)
            do k = 1,relats(node)
              if (node2.eq.points(node,k)) goto 11  !- already got it so CYCLE
            end do
            relats(node) = relats(node)+1            !- add this node
           if (relats(node).gt.ipoints) call myerror 
     &         (3,'Out of workspace (Cuthill)')
            points(node,relats(node)) = node2
            ibw_orig = max (ibw_orig, abs(node-node2))       !- compute bandwidth
c           -- abs cos both upper and lower bw
   11       continue
          end do          ! jnode
        end do         ! inode
      end do        ! elements

      write(*,'(a,i7,a,i6,a)')
     &  'Original matrix band is ..',ibw_orig, ' (',ibw_orig*2-1,')'

c--------- 2: Calculate the current bandwidth ----------
      do i=1,nn
        p(i) = i          !- the the oroginal ordering
      enddo
      call calc_bw (relats, points,inn,nn,p,ibw)

c--------- 4: Sort the nodes attached to each node ----------
C  Bubble sort horizontally so the first has the least other touchers

      do i=1,nn                                  ! for this node
        sorted= .false.
        first = 1                                !
        last= relats(i)-1                        ! the range
        do while (.not.sorted)
          sorted=.true.
          do k=first,last
            if (relats(points(i,k)).gt.relats(points(i,k+1))) then
              temp = points(i,k)
              points(i,k) = points(i,k+1)
              points(i,k+1) = temp
c             first = k           ! position of the first misorder
              sorted = .false.
            end if
          end do
c hmm for bubble sort we can increment 'first' too.
          last=last-1             ! last place is now the biggest
        end do
      end do

c--------- 5: Sort RELATS/POINTS ----------
c  so the first has the least other touchers
c.. note we *only* do this so we can try the more obvious root nodes first.

      call index_integers (relats,nn, ordered)

c----------- 6: Optimisation -----------
C   attempt to minimise the band by renumbering the nodes.
C   The new renembering scheme is held in array P. The 
C   locations of the new nodes numbers in  P is equal to their
C   old numbers. The reduced band is IBW_OPT

c--------- 6: Sweep the mesh renumbering ----------

c     do iroot=1,1
      do iroot=1,min(50,nn)       !- loop the possible starting nodes
c     do iroot=1,nn               !- loop the possible starting nodes
        jroot=ordered(iroot) 
        call sweep_this_root (relats, points,inn,nn, jroot, p,ibw)
        if (ibw.lt.ibw_opt.or. iroot.eq.1) then   ! best so far?
          ibw_opt=ibw               !- new best
          jroot_best=jroot          ! and its root
          write (*,'(i6,a,i6,a,i3,a, a,4i5)') 
     & iroot,': from', ordered(iroot),' (',relats (ordered(iroot)),')'
     & ,' ibw=',ibw, ibw_opt,jroot_best  !, ipos_duff
        endif

      enddo
      if (ibw_opt.lt.ibw_orig) then
        call sweep_this_root (relats, points,inn,nn, jroot_best, p,ibw)
        call calc_bw (relats, points,inn,nn,p,ibw)
      else
        print*,'<> No improvement could be found'
        do i=1,nn
          p(i)=i                           !- Null solution.
        enddo
      endif
      return
      end  !SUBROUTINE CUTHILL

c-----------------------------------------------------------------------
      subroutine calc_bw (relats, points,inn,nn,p,ibw)
c
c   simply loop nodes
c   and take the difference between the min and max node# that is comnnected
c   BW is the max of these. (also mean and min too)
c          DJK 1-3-98 
c
      integer relats(*), points(inn,*), p(*)
      integer ibw_max, inode,jnode,i,j
      real bw_mean

      IBW_MAX=0
      BW_MEAN=0
      DO I=1,NN                    !--- loop nodes ----
        inode = p(i)
        DO J=1,RELATS(I)
          jnode = p(points(i,j))
          imin=inode
          if (j.eq.1) then                !- first time
            imax=jnode
          endif
          imax = max (imax,jnode)
        ENDDO
        IBW=IMAX-IMIN
        bw_mean= bw_mean+ibw
        ibw_max = max(ibw,ibw_max)
      ENDDO
      bw_mean = bw_mean/nn
c     write(*,*) 'NN=',NN
      write(*,'(a,i9, a,f6.1,a)')
     &   ' Max bandwidth (Banred) is:',ibw_max,
     &   ' =',100.*ibw_max/real(nn)  ,'%'
      write(*,'(a,f9.1, a,f6.1,a)')
     &   'Mean bandwidth (Sparin) is:', bw_mean,
     &   ' =',100.*bw_mean/real(nn)  ,'%'
      return
      end  !subroutine calc_bw

c-----------------------------------------------------------------------
      subroutine sweep_this_root (relats, points,inn,nn, jroot, p,ibw)
c
c     This sweeps the whole mesh, renumbering all the nodes
c     given a root node: jroot
c     Returns p() so that p(1) is the new number for node 1
c     called by group_greedy,etc.  I guess
c          DJK 1-3-98 
c
      integer relats(*), points(inn,*), p(*)
c#ifdef F90 then
c     integer newjt(nn)
c#else
      integer iworkspace
c     parameter (iworkspace=20000)     !
      parameter (iworkspace=125000)
      integer newjt(iworkspace)              !- workspace :-(
c.. need to check array bounds eg. for the trabecular bone cube!
c#endif

c  At any step:
c     pick the next base node
c     loop around it and renumber the attached nodes
c     exit when all nodes have been marked (not necesserily used as bases)
c < does this make this a 'greedy' algorithm? >

c
c  hmm why can we not use P() also as the list of base nodes?
c
      do i=1,nn
        p(i) = 0                      !- mark all nodes as 'free'
      end do
      newjt(1) = jroot                !- start from this node
      p(jroot) = 1                    !   and flag
      k = 1                           !- 1:nn for added nodes
      ibw = 0                         !- the largest bandwidth so far


c----- loop the nodes around this point ----
c  note: newjt(i) = ordered() ???
c  This algorithm is not truly cuthill-mcgee since the base points
c  are not necesserily used in order of increasing valency
c  (merely there are added in order only for each base node used)

c   only a range of newjt is needed - as it is a lifo stack 
c   we add at the top and read from the bottom.
c   better to have a pair of POP, PUSH subroutines
c   POP always takes the base and shuffles down
c   PUSH uses bisection to find where to put this entry
c   < perhaps this is elegant with pointers? >
c     methinks that it is a binary tree?

      do i=1,nn                       !-- loop 'sweeping' points --
        do jj=1,relats(newjt(i))         !- loop all nodes attached here
          inode=points(newjt(i),jj)      !- its node number
          if (p(inode).gt.0) goto 21     !- CYCLE if already done
          k=k+1                          !- a 'new' node
          p(inode)=k                     !- give it a node #
          newjt(k)=inode                 !- put on the stack as a new root
          ibw = max (ibw, abs(i-k))      !- compute bandwidth
          if (k.ge.nn) goto 22           !- EXIT outer loop
   21     continue
        enddo
      enddo 
   22 continue     
      return
      end  !subroutine sweep_this_root

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
c     SUBROUTINE CUTHILL_MCGEE (GC,IGC,NDIM,NN,NUMS,INUMS,NEL,P)
C
C      * ABANDONED *
C     This optimises the node-numbering using the algorithm by
C      Cuthill and Mcgee.
C     (based on CONNECTIVITY_IGRP)   Dan Kidger 12-3-96
C
C      Note too .. REVERSE_CUTHILL_MCGEE .. where we insert onto the 
C      stack in the opposite order (so take off max connectivity first)
C      then at the end simply reverse the numbers in P()
C
C      Once done it is nice to compute the (nodal) bandwidth, both max and mean
C      For the optimum mesh , it is best to repeat for the other 3 'corners' 
C      hence pick the best one.
C
C
C   Algorithm:
C     1/ null count: loop elems, hence get #elems at each node.
C     2/ start at the start-node (find its elem #)
C     3/ give start_node #=1, 
C     4/ get this elems NUM, sort wrt 'valency'
C     5/ loop these and number 2,3,4..8, also flag these as 'current front'
C     6/  (eg put as -ve in P(), also 'zero' node 1 as all_done.
C     7/ now 'move' to these new nodes and advance ..?
C
C   Alternative scheme:
C     1/ null count: loop elems, hence get #elems at each node. (cf KDIAG)
C     2/ loop elems, and assemble a list of nodes that each node connects to
C         .. ie. the inverse of NUMS .. hence dont need NUMS anymore
C     3/ start with the first node
C     4/
C     5/
C     6/
C     7/
C
C    or.. 
C      maintain a 'stack' of the current front.. best as a linked 
C     list. So delete a node by closing up: add a node by scanning 
C     for an insertion point.
C       So get (and remove) bottom node from the stack, and number P(NN++)
C      loop all the other nodes that it is connected too (>1 element?)
C      and (if not already numbered) insert onto the stack (*after* 
C      other nodes of the same valency.)
C
C     ..Hard bits..
C       having a list of which nodes are connected to which nodes
C      (if I just store the elem #s then I can loop these, hence get nodes)
C      .. so like KDIAG, get the min and max node# attached to a node.
C         NO! .. loop elems, loop NUM, (if already 'done' CYCLE) ..??
C       .. or loop mesh and for every node write out all the pairs :-(
C          hence index this list  (too laborious?)
C       simplex algorithm: create a 2d array T(10,NN) .. loop elems & 
C      loop NUM, for each loop rest_of_NUM and record in T() by direct 
C      insertion in ascending node number order.
C
C      or even just dont bother? .. when we get a new node .. loop all elems, 
C      loop NUM, if a hit: loop rest_of_NUM and add each (in ascending order?) 
C      . Then at end just count the number of entries. :-)
C      .. this can be speeded up quite a bit, if for each node we record
C      the first element# it is found in (last too?), so only need to 
C      search these
C
C      compare with a routine that for counts the # of occurences of each 
C      node: P()=0, loop elems&loop NUM, so P(NUM(:)++
C
C      then we will know when there are no more elemnts to check.
C
C
c      REAL       GC (IGC,NN)         !- global coords
c      INTEGER  NUMS (INUMS,NEL)      !- element steerings
c     &           ,P (NN)             !- workspace
c      RETURN
c      END

C-----------------------------------------------------------------------
      SUBROUTINE SORT_ELEMENTS_RANDOM (NUMS,INUMS,NEL,P)
c
c     This juggles the elements into a random order
c        Dan Kidger   17-4-98
c
      INTEGER NUMS(INUMS,*), P(*)

      call shuffle_p (p,nel)
      CALL RESEQ_NUMS (NUMS,INUMS,NEL, P)
      RETURN
      END
C-----------------------------------------------------------------------
      SUBROUTINE R_SORT_ELEMENTS_POINT (IO,GC,IGC,NDIM,NN,NUMS,INUMS,
     &              NEL,P)
C
C     This will sort the elements wrt distance from a point
C
      REAL GC(IGC,*), CENTRE(5)
      INTEGER NUMS(INUMS,*), P(*)

    1 READ (IO,*,IOSTAT=IOS)  (CENTRE(J),J=1,NDIM)
      CALL IN_TEST (IO,IOS,*1,*999)
      CALL SORT_ELEMENTS_POINT (GC,IGC,NDIM,NN,NUMS,INUMS,NEL,P,CENTRE)
      RETURN
 999  CALL MYERROR(2,'Missing opcode in *SORT_NODES_POINT')
      END

C-----------------------------------------------------------------------
      SUBROUTINE SORT_ELEMENTS_POINT 
     &           (GC,IGC,NDIM,NN,NUMS,INUMS,NEL,P,CENTRE)
C
C     This will sort the elements wrt distance from a point
C      *Note: that this routine holds DEPTHS() as a temp buffer :-(
C
      REAL GC(IGC,*), CENTRE(5)
      INTEGER NUMS(INUMS,*), P(*)  , NUM(99)

c#ifdef F90 then
c     real DEPTHS(NEL)
c#else
      integer iworkspace
c     parameter (iworkspace= 20000)
      parameter (iworkspace=125000)
      real DEPTHS(iworkspace)
c      INTEGER NUMS(INUMS,*), P(*)  , NUM(99)
c#endif

c#ifndef F90 then
      IF (NEL.GT.iworkspace) CALL MYERROR (3, 
     &  'Too many nodes (SORT_ELEMENTS_POINT)')
c#endif

      DO IEL=1,NEL
        CALL GET_ELEMENT (NUMS,INUMS,IEL, NUM, 
     &          NOD,NDIME,ITYPE,IMAT,IUSER1,IUSER2,IUSER3)
        D = 0.
        DO J=1,NDIM
          DD= 0.            !- find the mean x,y,z coord of this element
          DO I=1,NOD
            DD = DD + GC(J,NUM(I))
          ENDDO
          DD = DD / REAL (NOD)
          D = D + (DD - CENTRE(J))**2
        ENDDO
        DEPTHS(IEL) = D
      ENDDO

      CALL INDEX_REALS  (DEPTHS,NEL, P)

c... we need to flip P() from indexing (as in depth_sort)
C... to a ranking (cf 'value' ie. the inverse)
c........ isn't this next bit the same as 'jUGGLE_P' ?? ....
c... cf as a subroutine like in "NR in F77"
       OPEN (97,STATUS='SCRATCH',FORM='UNFORMATTED')
       DO IEL=1,NEL
         WRITE(97) P(IEL)
       ENDDO
       REWIND(97)
       DO IEL=1,NEL
       READ(97) J
         P(J) = IEL
       ENDDO
       CLOSE(97)

      CALL RESEQ_NUMS  (NUMS,INUMS,NEL, P)

      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE SORT_ELEMENTS_IMAT
     &           (GC,IGC,NDIM,NN,NUMS,INUMS,NEL,P)
C
C     This will sort the elements in ascending order of material number
C      *Note: that this routine holds IDEPTHS() as a temp buffer :-(
C     this should make better use of Cache for Lego-PCG
C
      REAL GC(IGC,*)
      INTEGER NUMS(INUMS,*), P(*)

c#ifdef F90 then
c     integer IDEPTHS(NEL)
c#else
      integer iworkspace
c     parameter (iworkspace=20000)
      parameter (iworkspace=125000)
      INTEGER IDEPTHS(iworkspace)           !- workspace
c#endif

      DO IEL=1,NEL
        CALL GET_EL_IMAT (NUMS,INUMS,IEL, IMAT)
        IDEPTHS(IEL) = IMAT
      ENDDO

      CALL INDEX_INTEGERS (IDEPTHS,NEL, P)

c... we need to flip P() from indexing (as in depth_sort)
C... to a ranking (cf 'value' ie. the inverse)
c........ isn't this next bit the same as 'JUGGLE_P' ?? ....
c... cf as a subroutine like in "NR in F77"
      OPEN (97,STATUS='SCRATCH',FORM='UNFORMATTED')
      DO I=1,NN
        WRITE(97) P(I)
      ENDDO
      REWIND(97)
      DO I=1,NN
        READ(97) J
        P(J) = I
      ENDDO
      CLOSE(97)

      CALL RESEQ_NUMS (NUMS,INUMS,NEL, P)

      RETURN
      END

C-----------------------------------------------------------------------
c       Coordinate changing routines
C-----------------------------------------------------------------------
      SUBROUTINE R_NODMOV (IO,GC,IGC,NDIM,NN)
C
C     This moves nodes based on their 'old' and 'new' coords
C     .. should change to use FIND_NODE directly so no need for a workspace
c       Dan Kidger  c.1991?
c
c     PARAMETER (MAX_NODES = 500)
      REAL GC(IGC,*), COLD(3), CNEW(3)
c     INTEGER NUMLST(MAX_NODES) 
      tol =1.e-4

      WILD = 230964.
    1 DO K=1 , 9999
        READ(IO,*,IOSTAT=IOS) (COLD(I),I=1,NDIM) ,(CNEW(I),I=1,NDIM)
        CALL IN_TEST(IO,IOS,*1,*999)
c       CALL FIND_NODES (GC,IGC,NDIM,NN,COLD,NUMLST,MAX_NODES,NNOD)

        IFROM = 1
        ITO = NN
        do jj=1,nn     !- 'infinite' loop
          CALL FIND_NODE (GC,IGC,NN,NDIM,COLD,IFROM,ITO,INODE)
          IF (INODE.GT.0) THEN         !-- node found
            DO J=1,NDIM
              IF (ABS(COLD(J)-WILD)/WILD.GT.tol) 
     &        GC (J,inode) = CNEW(J)
            ENDDO
          ELSE
            goto 21   !exit inner loop
          ENDIF
        enddo
   21   continue

c        DO I=1,NNOD
c          DO J=1,NDIM
c            IF (ABS(COLD(J)-WILD)/WILD.GT.tol) 
c     &      GC (J,NUMLST(I)) = CNEW(J)
c          ENDDO
c        ENDDO
        WRITE(*,'(3(A,I7))')   '<> ',k,':',jj-1,' Node(s) moved'
      ENDDO
  999 RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE R_SCALE_MESH (IO,GC,IGC,NDIM,NN)
C
C     Header to SCALE_MESH
C
      REAL GC(IGC,NN) , XS(5)
    1 READ (IO,*,IOSTAT=IOS) (XS(J),J=1,NDIM)
      CALL IN_TEST (IO,IOS,*1,*999)
      CALL SCALE_MESH (GC,IGC,NDIM,NN ,XS)
      RETURN
 999  CALL MYERROR (2,'Missing opcode in *SCALE_MESH')
      END

C-----------------------------------------------------------------------
      SUBROUTINE SCALE_MESH (GC,IGC,NDIM,NN ,XS)
C
C     Scale all the coordinates  in  x,y,(z)
C
      REAL GC(IGC,NN) , XS(5)

      DO I=1,NN
        DO J=1,NDIM
          GC(J,I) = GC(J,I) * XS(J)
        ENDDO
      ENDDO
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE R_SHIFT_MESH (IO,GC,IGC,NDIM,NN)
C
C     Header to SHIFT_MESH
C
      REAL GC(IGC,NN) ,XS(5)
    1 READ (IO,*,IOSTAT=IOS) (XS(J),J=1,NDIM)
      CALL IN_TEST (IO,IOS,*1,*999)
      CALL SHIFT_MESH (GC,IGC,NDIM,NN ,XS)
      RETURN
 999  CALL MYERROR (2,'Missing opcode in *SHIFT_MESH')
      END

C-----------------------------------------------------------------------
      SUBROUTINE SHIFT_MESH (GC,IGC,NDIM,NN,XS)
C
C     Shift all the coordinates by a read-in offset  (hi-lighting?)
C
      REAL GC(IGC,NN) ,XS(5)

      DO I=1,NN
        DO J=1,NDIM
          GC(J,I) = GC(J,I) + XS(J)
        ENDDO
      ENDDO
      END

C-----------------------------------------------------------------------
      SUBROUTINE R_ROTATE_MESH (IO,GC,IGC,NDIM,NN)
C
C     Header to ROTATE_MESH
C
      REAL GC(IGC,NN)
    1 READ (IO,*,IOSTAT=IOS) IDIR,THETA
      CALL IN_TEST (IO,IOS,*1,*999)
      CALL ROTATE_MESH (GC,IGC,NDIM,NN ,IDIR,THETA)
      RETURN
 999  CALL MYERROR (2,'Missing opcode in *ROTATE_MESH')
      END

C-----------------------------------------------------------------------
      SUBROUTINE ROTATE_MESH (GC,IGC,NDIM,NN,IDIR,THETA)
C
C     Rotate all the coordinates about a given axis
C
      REAL GC(IGC,NN), THETA, P(5)

      PI=4.*atan(1.)
      ANG= THETA  * PI/180.
      DO I=1,NN
        DO J=1,NDIM
          P(J) = GC(J,I)
        ENDDO
        CALL ROTATE_POINT( P, GC(1,I), NDIM,  IDIR, ANG)
      ENDDO
      END

C-----------------------------------------------------------------------
      SUBROUTINE X2Y2Z (GC,IGC,NDIM,NN)
C
C     This simply mungs the x coordinate to y, y to z and z to x
C
      REAL GC(IGC,NN)
      DO I=1,NN
        X = GC(1,I)
        Y = GC(2,I)
        Z = GC(3,I)
        GC(1,I) = Z
        GC(2,I) = X
        GC(3,I) = Y
      ENDDO
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE X2Y (GC,IGC,NDIM,NN)
C
C     This simply mungs the x coordinate to y, y to -x
C      ie. a rotation about the z-axis of 90deg.
C
      REAL GC(IGC,NN)
      DO I=1,NN
        X = GC(1,I)
        Y = GC(2,I)
        GC(1,I) =  Y
        GC(2,I) = -X
      ENDDO
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE WRAP_X (GC,IGC,NDIM,NN)
C
C     This MUNGs the co-ordinates from a 'cylindrical' to 'cartesian'
C      ( THIS WAS called 'WRAP_Y' originally !)   eg. my SPUDcan mesh
C        where : x= axis (unaffected)
C                y = radius
C                z = angle
C     ** Hmm no call to this from the Keyword handler
C
      REAL GC(IGC,NN)
      PI=4.*ATAN(1.)
      DO I=1,NN
        RAD = -GC(2,I)               !--- Radii are all -ve :-)
        ANG =  GC(3,I) * PI/180.
        GC(2,I) = RAD * SIN(ANG)     !---- apply cylindical transf.
        GC(3,I) = RAD * COS(ANG)
      ENDDO
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE WRAP_Y (GC,IGC,NDIM,NN)
C
C     This MUNGs the co-ordinates from a 'cylindrical' to 'cartesian'
C        where : y= axis (unaffected)     (eg. my 'book60 in 3D' mesh)
C                x = radius
C                z = angle
      REAL GC(IGC,NN)
      PI=4.*ATAN(1.)
      DO I=1,NN
        RAD =  GC(1,I)           
        ANG =  GC(3,I) * PI/180.
        GC(1,I) = RAD * COS(ANG)     !---- apply cylindical transf.
        GC(3,I) =-RAD * SIN(ANG)
      ENDDO
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE WRAP_Z (GC,IGC,NDIM,NN)
C
C     This MUNGs the co-ordinates from a 'cylindrical' to 'cartesian'
C     about the Z axis
C        where : z= axis (unaffected)
C                y = radius
C                z = angle
C
      REAL GC(IGC,NN)
      PI=4.*ATAN(1.)
      DO I=1,NN
        RAD =  GC(2,I)               !--- Radii are all -ve :-)
        ANG =  GC(1,I) * PI/180.
        GC(1,I) = RAD * SIN(ANG)     !---- apply cylindical transf.
        GC(2,I) = RAD * COS(ANG)
      ENDDO
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE R_WRAP_TWIST_Y (IO,GC,IGC,NDIM,NN)
C
C     This Twists the x-z coordinates up the y-axis
C       ... TWIST_LEN should be 0.->1. surely (so use SCALE_MESH too)
      REAL GC(IGC,NN), TWIST_LEN

    1 READ (IO,*,IOSTAT=IOS) TWIST_LEN
      CALL IN_TEST (IO,IOS,*1,*999)
      CALL WRAP_TWIST_Y (GC,IGC,NDIM,NN, TWIST_LEN) !(note no 'IO')
      RETURN
 999  CALL MYERROR (2,'Missing opcode "TWIST_LEN" in *WRAP_TWIST_Y')
      END

C-----------------------------------------------------------------------
      SUBROUTINE WRAP_TWIST_Y (GC,IGC,NDIM,NN, TWIST_LEN)
C
C     This Twists the x-z coordinates up the y-axis
C      .. in general about any axis ?
C
      REAL GC(IGC,NN)
      PI=4.*ATAN(1.)
      DO I=1,NN
        ANG= GC(2,I) * TWIST_LEN  * PI/180.
        XO = GC(1,I)
        ZO = GC(3,I)
        GC(1,I) =   XO * COS(ANG) + ZO * SIN(ANG)
        GC(3,I) = - XO * SIN(ANG) + ZO * COS(ANG)
      ENDDO
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE WRAP_CIRCLE_Y (GC,IGC,NDIM,NN)
C
C     This MUNGs the co-ordinates into a circle (from a square)
C     This is the INVERSE of WRAP_SQUARE_Y
C     points on the x/z axes are unaffected .. the rest get expanded
C        DJK 14-6-94
C
      REAL GC(IGC,NN)

      DO I=1,NN
        XO = GC(1,I)
        ZO = GC(3,I)
        RADBX = MAX (ABS(XO),ABS(ZO))
        RADSP = SQRT (XO**2 + ZO**2 + 1.E-10)
        GC(1,I) = XO * RADBX / RADSP   !- hmm used to be * 2. ??
        GC(3,I) = ZO * RADBX / RADSP 
      ENDDO
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE R_WRAP_SQUARE_Y_RANGE (IO,IPR,GC,IGC,NDIM,NN)
!
!     wrapper to WRAP_SQUARE_Y_RANGE
!     DJK 25-5-04
!
      REAL GC(IGC,NN)
      real r1,r2

   1  READ (IO,*,IOSTAT=IOS) R1,R2
        CALL IN_TEST(IO,IOS,*1,*999)
      CALL WRAP_SQUARE_Y_RANGE (GC,IGC,NDIM,NN,R1,R2,ipr)
      RETURN
 999  CALL MYERROR (2,'Missing r1,r2 in *WRAP_SQUARE_Y_RANGE')
      END

C-----------------------------------------------------------------------
      SUBROUTINE WRAP_SQUARE_Y_RANGE (GC,IGC,NDIM,NN,r1,r2,ipr)
C
C     This MUNGs the co-ordinates into a square (from a circle)
C     This is the INVERSE of WRAP_CIRCLE_Y
C     points on the x/z axes are unaffected .. the rest get expanded
C        DJK 14-6-94
C
c     11/04/03  r1,r2 deifine the lo.hi radius to affect
c     - also perhaps an expansion/contraction factor
      REAL GC(IGC,NN)
      real r1,r2

      nn_changed=0
      DO I=1,NN
        XO = GC(1,I)
        ZO = GC(3,I)
        RADBX = MAX (ABS(XO),ABS(ZO)) + 1.E-10
        RADSP = SQRT (XO**2 + ZO**2   + 1.E-10)
!       IF (abs(RADSP-2.9625).lt. 0.0001) then
        if (radsp>r1 .and. radsp<r2) then
        nn_changed= nn_changed+1
        GC(1,I) = XO * RADSP / RADBX      !- why was it *2 ??
        GC(3,I) = ZO * RADSP / RADBX
        ENDIF
      ENDDO
      if (IPR>=2) write(*,'(I7,a)') nn_changed,' Nodes moved'
      RETURN
      END


C-----------------------------------------------------------------------
      SUBROUTINE WRAP_SQUARE_Y (GC,IGC,NDIM,NN)
C
C     This MUNGs the co-ordinates into a square (from a circle)
C     This is the INVERSE of WRAP_CIRCLE_Y
C     points on the x/z axes are unaffected .. the rest get expanded
C        DJK 14-6-94
C
c - 11/04/03  consider a filter of a given radius (radii) to affect
c     - also perhaps an expansion/contraction factor
      REAL GC(IGC,NN)

      DO I=1,NN
        XO = GC(1,I)
        ZO = GC(3,I)
        RADBX = MAX (ABS(XO),ABS(ZO)) + 1.E-10
        RADSP = SQRT (XO**2 + ZO**2   + 1.E-10)
!c Hack for B38
!        IF (abs(RADSP-2.9625).lt. 0.0001) then
        GC(1,I) = XO * RADSP / RADBX      !- why was it *2 ??
        GC(3,I) = ZO * RADSP / RADBX 
!        ENDIF
      ENDDO
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE WRAP_CIRCLE_Z (GC,IGC,NDIM,NN)
C
C     This MUNGs the co-ordinates into circle in the x-y planes
C     points on the x/z axes are unaffected .. the rest get shrunk!
C
      REAL GC(IGC,NN)

      DO I=1,NN
        XO = GC(1,I)
        ZO = GC(3,I)
        RADBX = MAX (ABS(XO),ABS(ZO))
        RADSP = SQRT (XO**2 + ZO**2 + 1.E-10)
        GC(1,I) = XO * RADBX / RADSP 
        GC(3,I) = ZO * RADBX / RADSP
      ENDDO
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE R_WRAP_CIRCLE_SQUARE (IO,GC,IGC,NDIM,NN)
C
C     HEADER routine
c  perhaps simplify - assume centre is (0.,0.) use *SHIFT_MESH if otherwise
C
      REAL GC(IGC,NN), XC,YC,R_SQ,R_C

c     DO ILOOP = 1,9999     !- do I really want multiple options ??
c ok lets overload 

    1 READ (IO,*,IOSTAT=IOS) XC,YC, R_SQ,R_C
      CALL IN_TEST (IO,IOS,*1,*999)
      CALL WRAP_CIRCLE_SQUARE (GC,IGC,NDIM,NN,XC,YC,  R_SQ,R_C)
      RETURN
 999  CALL MYERROR (2,'Missing opcode in *CIRCLE_A_SQUARE')
      END

C-----------------------------------------------------------------------
      SUBROUTINE WRAP_CIRCLE_SQUARE (GC,IGC,NDIM,NN,XC,YC,  R_SQ,R_C)
C
C     This MUNGs the co-ordinates lying on a given square into a circle
C
      REAL GC(IGC,NN), XC,YC

      IC = 0
      DO I=1,NN
        XP = GC(1,I)
        YP = GC(2,I)
        XR = XP - XC      !- the 2 'radii'
        YR = YP - YC 
        R_HI = MAX ( ABS(XR),ABS(YR) )     !- must = R_SQ

        IF ( ABS(R_HI-R_SQ).LT.1.E-4 ) THEN
          IC = IC + 1
          RAD = SQRT (XR**2 + YR**2)
          GC(1,I) = XC + R_C * XR/RAD      ! (note Dir. cos's)
          GC(2,I) = YC + R_C * YR/RAD 
        ENDIF
      ENDDO    !- the nodes

      WRITE(*,'(A,I7,A)') '<> ', IC,' nodes moved'
      RETURN
      END
C-----------------------------------------------------------------------
      SUBROUTINE WRAP_SQUARE_CIRCLE (GC,IGC,NDIM,NN,XC,YC,  R_SQ,R_C)
C
C     This MUNGs the co-ordinates lying on a given circle into a square
C      (fix me)
C
      REAL GC(IGC,NN), XC,YC

      IC = 0
      DO I=1,NN
        XP = GC(1,I)
        YP = GC(2,I)
        XR = XP - XC      !- the 2 'radii'
        YR = YP - YC
        R_HI = MAX ( ABS(XR),ABS(YR) )     !- must = R_SQ

        IF ( ABS(R_HI-R_SQ).LT.1.E-4 ) THEN
          IC = IC + 1
          RAD = SQRT (XR**2 + YR**2)
          GC(1,I) = XC + R_C * XR/RAD      ! (note Dir. cos's)
          GC(2,I) = YC + R_C * YR/RAD
        ENDIF
      ENDDO    !- the nodes

      WRITE(*,'(A,I7,A)') '<> ', IC,' nodes moved'
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE R_WRAP_TUNNEL_SQUARE (IO,GC,IGC,NDIM,NN)
C
C     HEADER routine
C     R1 is the top radius, R3 the bottom, R2 is the transition.
C       DJK / Baha    12-12-97
c
c   Should I do sanity-checks for r1,r2,r3
C
      REAL GC(IGC,NN), XC,yc, r1,r2,r3

c     DO ILOOP = 1,9999     !- do I really want multiple options ??

    1 READ (IO,*,IOSTAT=IOS) XC,YC, R_SQ, R1,r2,r3
      CALL IN_TEST (IO,IOS,*1,*999)
      CALL WRAP_TUNNEL_SQUARE (GC,IGC,NDIM,NN,XC,YC,  R_SQ,r1,r2,r3)
      RETURN
 999  CALL MYERROR (2,'Missing opcode in *TUNNEL_A_SQUARE')
      END

C-----------------------------------------------------------------------
      SUBROUTINE WRAP_TUNNEL_SQUARE (GC,IGC,NDIM,NN,XC,YC,  R_SQ
     & ,r1,r2,r3)
C
C     This MUNGs the co-ordinates lying on a given square into a 
c     tunnel made of 3 circular arcs
C        Baha Ismail / Dan Kidger 12-12-97
C
      REAL GC(IGC,NN), XC,yc, r1,r2,r3, e,psi,r
      pi=3.1315926535

c------- 0:calc basic parameters -------
      theta = asin((r1-r2)/(r3-r2))    !- arc for R3
c       print*,'<> Theta=',theta
      t = r3*tan(theta)                !- 
c       print*,'<> t=',t
      psi = pi-atan(t/r2)              !- transition from R2 to R3
c       print*,'<> Psi=',psi
      e = (r3-r2)*cos(theta)           !- elvation of the cventre of R3
c       print*,'<> elevation of R3 centre (e)=',e


c------- 1: scan all nodes in the mesh for ones one the base square. -------
      IC = 0
      DO I=1,NN
        XP = GC(1,I)
        YP = GC(2,I)
        XR = XP - XC      !- the 2 'radii'
        YR = YP - YC 
        R_HI = MAX ( ABS(XR),ABS(YR) )     !- must = R_SQ

        IF ( ABS(R_HI-R_SQ).gt.1.E-4 ) goto 22   ! cycle
          IC = IC + 1
          RAD = SQRT (XR**2 + YR**2)
c         if (rad .eq.0.000) goto 22
c------------------- baha's bit -------
          alpha=atan2(xr,yr)
c           print*,'alpha,rad,xr,yr =', rad, theta, xr,yr
          IF (alpha.gt.(-pi/2.) .and. ALPHA.lt.PI/2. ) THEN       !--- 'Top' arc---
c             XP = 0. + (r1*sin(alpha))
c             YP = 0. + (r1*cos(alpha))
              r=r1

c        (need to check the next bit if >180 deg.)
          ELSEIF (alpha.gt.(-psi) .and. ALPHA.lt.PSI) THEN     !-- fillet arcs ---
             flip = sign (1.,alpha)
             phi1= asin (  (r1-r2)/r2 * sin(abs(alpha)-pi/2.)   )
             gama= abs(alpha)+phi1-(pi/2.)
             xp  = (r1-r2) + r2*cos(gama)
             yp  =   0.    - r2*sin(gama)
             r  = SQRT (XP**2+YP**2)    ! 'radius' of the tunnel  at this point

          ELSE                                          !-- 'Base' arc ---
            phi2 = asin(e*sin(alpha)/r3)
            phi3 = pi-alpha-phi2
            xp   = 0.  + r3*sin(phi3)
            yp   = e   - r3*cos(phi3)
            r  = SQRT (XP**2+YP**2)    ! 'radius' of the tunnel  at this point
          ENDIF

c-------------------------------------

          GC(1,I) = XC + R * xr/rad      ! (note Dir. cos's)
          GC(2,I) = YC + R * yr/rad 

  22    continue
      ENDDO    !- the nodes

      WRITE(*,'(A,I7,A)') '<> ', IC,' nodes moved'
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE WRAP_SPHERE (GC,IGC,NDIM,NN)
C
C     This MUNGs the co-ordinates into a sphere
C     points on the x/y/z axes are unaffected .. the rest get shrunk!
C
      REAL GC(IGC,NN)

      DO I=1,NN
        XO = GC(1,I)
        YO = GC(2,I)
        ZO = GC(3,I)
        RADBX = MAX(ABS(XO),ABS(YO),ABS(ZO))
        RADSP = SQRT (XO**2 + YO**2 + ZO**2 + 1.E-10)
        GC(1,I) = XO * RADBX / RADSP * 2.     !(why *2. ??)
        GC(2,I) = YO * RADBX / RADSP * 2.
        GC(3,I) = ZO * RADBX / RADSP * 2.
      ENDDO
      RETURN
      END

c-----------------------------------------------------------------------
      SUBROUTINE MESH_QUALITY (IPR,GC,IGC,NDIM,NN,NUMS,INUMS,NEL,P)
c
c     This computes data on the 'quality' of the current mesh, for example
c     the min/man area, min/max internal angles, size of circumcircle
c         Dan Kidger 4-9-97
c
c     Notes:
c      1/ As well as maxima and minima, it is good to report the range 
c         as a set of (say) 20 'bins'/
c      2/ For 8nq's I guess we always expect beta_max=180deg.
c      3/ For triangles I want to report the circumcircle radius/area
c         non-dimensionalise to the:
C          a/ element area /circum_area
c       or b/ longest edge /circum_radius
c         Both scaled such that an equilateral-triangle returns '1.'
c      4/ for 3D things are less clear. :-(
c
c
      REAL       GC (IGC,NN)         !- global coords
      INTEGER  NUMS (INUMS,NEL)      !- element steerings
     &         ,NUM (32)             !- one elements steering
     &           ,P (NN)             !- workspace
      PARAMETER (ICOORD=32)
      REAL COORD(ICOORD,3)

c------------- 2: loop and find the 'equilateralness' ------------
      DO IEL=1,NEL
        CALL GET_ELEMENT (NUMS,INUMS,IEL, NUM, 
     &          NOD,NDIME,ITYPE,IMAT,IUSER1,IUSER2,IUSER3)
          IF (NOD.ne.3) goto 2      !- not a triangle
c         IF (NDIME.ne.2) goto 2    !- not planar
          CALL GET_COORD (NUM,NOD,NDIM,GC,IGC,COORD,ICOORD)    !- coords

          DET11 = COORD(1,1) - COORD(3,1)                  ! dx
          DET12 = COORD(1,2) - COORD(3,2)                  ! dy
          DET13 = DET11 * (COORD(1,1)+COORD(3,1)) /2.      ! x^2 + y^2
     &          + DET12 * (COORD(1,2)+COORD(3,2)) /2.
          DET21 = COORD(2,1) - COORD(3,1)                  ! dx
          DET22 = COORD(2,2) - COORD(3,2)                  ! dy
          DET23 = DET21 * (COORD(2,1)+COORD(3,1)) /2. 
     &          + DET22 * (COORD(2,2)+COORD(3,2)) /2.

          DD = DET11*DET22 - DET12*DET21                 !- determinant
c hmm is DD then the (twice?) area/volume of the triangle ?
          xc = (DET13*DET22 - DET23*DET12) /DD
          yc = (DET11*DET23 - DET21*DET13) /DD
          rc2 = (COORD(1,1) - xc)**2 + (COORD(1,2) - yc)**2
          area_circle = 3.14159265 * rc2
c.. now calc the triangle area = either jacobian or an explicit det
                          
   2    CONTINUE
      ENDDO
      print*,'<> '
C---------------------------------------
      RETURN
      END

c-----------------------------------------------------------------------
      SUBROUTINE BARYCENTRIC (GC,IGC,NDIM,NN,NUMS,INUMS,NEL,P
     &   ,max_iters, fact,tol,method,alpha,ipr)
c
c     This adjusts the mesh spacing so that each node moves towards 
c     the centroid of its 'attached' nodes. 
c  On entry:
c     P() is non-zero for those nodes to be clamped.
c     max_iters is the max number of passes to do (say 100)
c     fact is a relaxation factor - try say 0.8 or 1.
c     tol is the convergence tolerance - say 1.e-4
c     method='A  '  (coudl try'ISO' for 4nq's
c     alpha= nagative weight for diagonal node (for 'ISO' only)
c        Dan Kidger 20-3-96
c
c   Note 'attached nodes' may mean:
c       1/ all nodes of attached elements (ie. like the 'bandwidth')
c       2/ only the '2' corner' nodes of attached elems
c       3/ just the '2' adjacent nodes along an edge of each elem.
c
c     12-3-97 Moved to K_MESH.F from AFRONT.F90
c      2-7-97 Attatched nodes now resolved = those attached by an edge
c      2-7-97 Need an 'NF' like way of clamping nodes. ..cf -ve P()?
c      9-8-98 Added alpha 0.>1.; 0=Laplace, 1=Isoparametric (quads only)
c 
c  Notes:
c     1/ This would be much simpler if we knew explicitly which nodes, 
c        a given node is connected to.
c     2/ An alternative method might try and make elements more 'equilateral'
c      eg. assume each is a deformed equilateral 'triangle' hence 'bodyforces'
c      then just solve a standard FE type analysi?. :-)
c     3/ 19-8-97 note also that we can smooth in any (or all) desired 
c        directions. 
c
c---- Normaly the fixed B.C's are the nodes on the boundary 
c     so either: 1/ strip the edges to find them
c                2/ get the sum of the internal angles.
c                3/ (hack) for 4nqs if the nodal valency is <4
c  19-8-97 But in 3D it is all nodes on all facets that do not touch the
c        same IMAT
c
      REAL       GC (IGC,NN)         !- global coords
      INTEGER  NUMS (INUMS,NEL)      !- element steerings
     &         ,NUM (99)             !- one elements steering
     &          ,FS (32)             !- one element facet's steering
     &           ,P (NN)             !- workspace

      real    wts (nn)               !
      real    gc2 (igc,nn)             ! workspace
      integer nf (ndim,nn)
!     integer nf (3,100000)
      REAL GC_MIN(5),GC_MAX(5)       !

!     integer stats (20)
      integer list (32)              ! points to use
      real weights(32)               ! ..and their weights
      real dx(5),dx_max(5)           !- a point's movement
      character method*3             ! Laplace or Isometric weighting

c--- 0: initialisation ----
c--- 0a: Algorithm Control Parameters ---
c      METHOD = 'A  '         !- A=Laplace
c      METHOD = 'ISO'         !- ISO=isoparametric (for quads)
c      alpha=1.
c       if ISO, we use alpha = 0->1. (say 0.8) which is the weighting factor
c     IMETHOD = 1               !- The 'standard' list of connected nodes.
c     tol = 1.e-4               !- Really should be a f(Diag Boundng Box)
c     max_iters = 5             !- Or maybe larger eg. 1000 ?

      do i=1,nn
        do j=1,ndim;nf(j,i)=1; enddo
      enddo
      ic = 0
      DO I=1,NN                 !--- mark nodes that are to be fixed
        IF (P(I).NE.0) THEN        ! if on a boundary.
          ic = ic + 1
          P(I) = -1                ! flag (free nodes are all zero)
          do j=1,ndim; nf(j,i)=0; enddo
        ENDIF
      ENDDO 
      print*,ic,' nodes marked as fixed position'
      IF (IC.EQ.0) 
     & CALL MYERROR(2,'Missing *MARK_BOUNDARY (BARYCENTRIC)')

c---------------------------- loop iterations -------------------------
      do iter = 1, max_iters

c-------- 1: null the new coords ---------
c.. note here that in a sweeping method, we only need to buffer these
c.. until all nodes have been sampled (ie. less than NN)
c.. Also question whether immediate update is valid.
        do i=1,nn
          do j=1,ndim
            gc2(j,i) = 0.                !- new centroid of
          enddo                          !- surrounding points.
          wts(i)=0.                      !- its weight
        enddo

c---- 2: count of each nodes 'connectivty' (a.k.a 'nodal valency') ----
c  this doesn't change so maybe simplest to pre-calculate?
        do i=1,nn
          if (p(i).gt.0) p(i) = 0                       
        enddo
c--- 2a get the mesh extents --
      CALL GET_MESH_RANGE (GC,IGC, NN,NDIM,  GC_MIN,GC_MAX,DIAG)


c----- 3: loop elements and do each of its nodes -----
      do iel = 1, nel
        CALL GET_ELEMENT (NUMS,INUMS,IEL, NUM, NOD,NDIME,ITYPE
     &     ,IMAT,IU1,IU2,IU3)

c--- ** use the facets ----
c because it 'knows' about which nodes are adjacent to the given.
        CALL GET_FACE (NUM,NOD,NDIME,ITYPE, NFACES,FS,NN_F,NN_FT, 1 )  
        DO IFACE=1,NFACES              !- loop this element's faces
          CALL GET_FACE (NUM, NOD,NDIME,ITYPE, IFACE,FS,NN_F,NN_FT, 2)

c------ 4 : for each node in this element ------
c.. we create a list of nodes to use at a weight of each
c.. therefore assume that we use all, and set the weights for the ones
c   we dont want to zero :-)
c   so usu. weights = 0,0,0,1,*0*,1,0,0 for node #5
c   ie the LH and RH toucher only
c   for isoparametric 4nqs we would have -1,0,1,0,*0*,0,1,0
c   cos we only want to use corner nodes (not midside)
c   It would be more elegant of mid-edge nodes had weight from more than
c   just the two adjoining corners so that we get nicer 'curved'  meshes
c   (consider the case of a curved member - 2 elements thick)
c   perhaps then use 2 corners and the 2 opposite mid-edges (all=1.)

          npts=nn_f
          DO I=1, npts

            do j=1,npts
              list(j)    = fs(j)           !- full list of (8) nodes
              weights(j) = 0.              !- assume all are 'off'
            enddo
            inode = fs(i)                  !- my node

c-------- Method A: the 2 nodes on either side ----------
            IF (METHOD.eq.'A  ') THEN
            ip1 = mod(i,nn_f)+1            !- the next node
            im1 = i-1                      !- the previous node
            IF (im1.eq.0) im1 = nn_f
            weights(im1)=1.                !- Just 2 nodes on either side
            weights(ip1)=1.                !-

c-------- Method ISO: all 3 other nodes of a 4nq ----------
            ELSEIF (METHOD.eq.'ISO') THEN
c ..if a corner node:
c   seek the other '3' corners..
c     adjacents have weight=1., 'far' has weight=-alpha
c ..if an edge node:
c     a/ just use 2 adjacent corner nodes as before
c  or b/ use 2 oppsosite edge nodes as well (maybe with lessor weights?)
            ip1 = mod(i,nn_f)+1            !- the next node
            ip2 = mod(i+1,nn_f)+1          !- the next node after that
            im1 = i-1                      !- the previous node
            IF (im1.eq.0) im1 = nn_f
            weights(im1)=1.                !- 
            weights(ip1)=1.                !-
            weights(ip2)=-alpha            !-

c-----------------------------------------------------------
            ELSE
              call myerror(3,'Unknown smoothing style (BARYCENTRIC)')
            ENDIF
c--- 4: add in these cordinates (thence average) -----
c.. note that some references will weight each point wrt to the
C   target density function here.

c.. also note that 'isoparametric' smoothing for quads introduces a 
C  negative weight for the diagonaly connected nodes
            do K=1,npts            !- the (2) nodes connected to this point
              DO j=1,NDIM
                gc2(j,inode) = gc2(j,inode) + weights(k)*gc(j,list(k))
              ENDDO
              wts(inode) = wts(inode) + weights(k)
            enddo

            if (p(inode).ge.0) p(inode) = p(inode) + 1
          enddo      !- do each of the nodes of this facet
        enddo      !- loop (6) facets
      enddo    !- loop all elements.

c--------------- 6: update the new coordinates -----------------
c.. We 'mask' out the boundary (unmovable) nodes using NF(j,i)/=0
        do j=1,ndim                    !- Track the largest movement
          dx_max(j) = 0.               !- for convergence testing
        enddo                          !- RMS too?

        do i=1,nn           
          do j=1,ndim
            if (nf(j,i).ne.0) then
!           if (p(i).gt.0) then
c             dx(j) = fact* ( gc2(j,i) / p(i) - gc(j,i) )   ! S.O.R.
              dx(j) = fact* ( gc2(j,i) / wts(i) - gc(j,i) ) ! S.O.R.
              dx_max(j) = max(dx_max(j),abs(dx(j)))
c             dx_max(j) = dx_max(j) +dx(j)**2              ! rms
            else
              dx(j) = 0.
            endif
          enddo

c.. other stratagies? *obsolete*
c 3: boundary has less elements at this node.
c           if (p(i).le.4*2) dx(j) = 0.             ! hack for 4nqs

          do j=1,ndim   !- shift this node.
!           if (p(i).gt.0)
! next line causes a segv / address error in a later subroutine !
            if (nf(j,i).ne.0) 
     &      gc(j,i) = gc(j,i) + dx(j)
          enddo
        enddo    !- loop nodes.

!       -- monitor convergence --
!       do j=1,ndim; dx_max(j) = sqrt(dx_max(j));enddo   ! if RMS
        dx_maxx= 0.
        do j=1,ndim; dx_maxx= max(dx_maxx,dx_max(j)); enddo

        if(ipr>=3) then     !- monitor output to the console
          if(ipr>=4 .or. mod(iter,10)==0)
     &     write(*,'(i5,a,5f12.7)') iter,' :',(dx_max(j),j=1,ndim)
        endif

c----- check convergence -----
c.. as a fraction of the mesh 'diagonal'
        if (dx_maxx/diag.lt.tol)  exit
      enddo     !-- loop iterations

      if(ipr>=1) WRITE (*,'(A,I5,A)')  
     & '<> Mesh smoothed in', iter-1, ' Iterations'
      return
      end

C-----------------------------------------------------------------------
      SUBROUTINE SWAP_IMAT_IGRP (GC,IGC,NDIM,NN,NUMS,INUMS,NEL)
c
c     This simply swaps the material and group numbers for all elements
c     This is a useful 'dodge' when testing GREEDY and GROUP_LEGO_GEOM
c        Dan Kidger 21-8-97
C
      REAL       GC (IGC,NN)         !- global coords
      INTEGER  NUMS (INUMS,NEL)      !- element steerings

      DO IEL = 1, NEL
        CALL GET_EL_IGRP (NUMS,INUMS,IEL, IGRP)
        CALL GET_EL_IMAT (NUMS,INUMS,IEL, IMAT)
        CALL PUT_EL_IGRP (NUMS,INUMS,IEL, IMAT)
        CALL PUT_EL_IMAT (NUMS,INUMS,IEL, IGRP)
c       print*,iel, imat,'<->',igrp
      ENDDO
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE R_GROUP_GREEDY (IO,IPR,GC,IGC,NDIM,NN,NUMS,INUMS,NEL,P)
c
c     Header to GROUP_GREEDY: the 'Greedy' algorithm for domain decomp.
c
      REAL       GC (IGC,NN)         !- global coords
      INTEGER  NUMS (INUMS,NEL)      !- element steerings
      INTEGER P(*)

    1 READ (IO,*,IOSTAT=IOS) NGROUPS, Iop_Seed  !- also IMETHOD?
      CALL IN_TEST (IO,IOS,*1,*999)
      CALL GROUP_GREEDY (IPR,GC,IGC,NDIM,NN,NUMS,INUMS,NEL,P, 
     & NGROUPS,iop_seed)
  999 RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE GROUP_GREEDY (IPR,GC,IGC,NDIM,NN,NUMS,INUMS,NEL,P
     & ,NGROUPS,iop_seed)
C
c     This partitions the mesh into NGROUPS sections 
C     (as part of domain decomposition for parallel processing)
c          Dan Kidger 21-8-97
c     Notes:
c     - based on the mesh-sweping method of GEOMETRY_IGRP
C     - on return the IGRP column of NUMS is altered, P is workspace
C
      REAL       GC (IGC,NN)         !- global coords
      INTEGER  NUMS (INUMS,NEL)      !- element steerings
c    &         ,NUM (32)             !- one elements steering
     &           ,P (NN)             !- workspace
      INTEGER LIST(99)               !- for printing

C------------- 1: Initialise ---------------
c.. realy just need to get a unique IGRP to use and so no need
C.. to zero all elements first.
      DO I=1,NN
        P(I) = 0       !- zap all nodes' codes
      ENDDO
      DO IEL=1,NEL
        CALL PUT_EL_IGRP (NUMS,INUMS,IEL, 0)  !- zap all Groups
      ENDDO

C------------- 2: Loop the new Groups ---------------
c.......... loop and implement
c.. pick the lowest numbered element as the 'seed' of each set.
      NEL_DONE = 0
      DO IGRP = 1,NGROUPS
        NEL_GETTO = NINT (NEL * REAL(IGRP)/REAL(NGROUPS))
        NEL_TODO = NEL_GETTO-NEL_DONE    !- Target
C       NEL_TODO = NEL / NGROUPS         !- (simplex version)
c Using NEL_SOFAR will acheive better load balancing so 5,4,5,4 rather 
c  than 4,4,4,6
c.. we then repeatedaly call the sweep until we get NEL_GRP
c  (or until we reach NEL)

C------------- 3: Choose the new 'seed' element ---------------
c.. or just always use the next 'free' element
c  For Greedy it is probably best if we could choose a seed that has  lots of 
c  free elements all around it so we naturally produce convex subdomains
c

c     IOP_SEED = 1    !-  default = scan forwards

c------- 4: call the sweeps multiply -----------
      NEL_GROUP = 0
      nsweeps_tot = 0
      DO NZONES = 1, NEL
        CALL GET_NEW_SEED (NUMS,INUMS,NEL, iel_seed, IOP_SEED)
        if (iel_seed.le.0) RETURN           !- abandon
        CALL SWEEP_IGRP (NUMS,INUMS, NEL,NN,P, IEL_SEED, IGRP
     &   ,nel_todo, nel_zone, NSWEEPS)      
        IF (NEL_ZONE.LE.0) CALL MYERROR (3,'No elemnts found (GREEDY)')
        nsweeps_tot = nsweeps_tot+nsweeps
        list(nzones) = nel_zone
        NEL_GROUP = NEL_GROUP + NEL_ZONE    !- now done this many here
        NEL_TODO  = NEL_TODO  - NEL_ZONE    !- less to do
        IF (NEL_TODO.EQ.0) GOTO 21   !- EXIT   !- complete.
      ENDDO   !- loop disconnected zones.

   21 CONTINUE
      NEL_DONE = NEL_DONE + NEL_GROUP      !- total so far (oops 18-10-99)
      IF (IPR.GE.2) THEN
        IF (IGRP.EQ.1) WRITE (*,'(A)')
     &     '  igrp  nsweeps nel_done  nzones,nel_zones()'
        write (*,'(2i4,i6,99i6)') igrp,nsweeps_tot, nel_done, 
     &    nzones, (list(i),i=1,nzones)
      ENDIF
      ENDDO      !- loop IGRP's

      RETURN
      END

c-----------------------------------------------------------------------
      SUBROUTINE GET_NEW_SEED (NUMS,INUMS,NEL, iel_seed, IOP_SEED)
c
c     This picks a new element to use as the base ('seed')  for growing
c     elements onto (eg. for the GREEDY algorithm)
c
c  Notes:
c      IEL_SEED is returned as the new seed (or =0 if failed)
c     'Choice of NEW SEED'  (IOP_SEED=) ...
c   1: Scan forwards from the last seed to find a new base 
c   2: Scan backwards
C   3: Random (of the remaining free elements)
C   4: Choose the 'best' element on the surface of the 'done' zone.
C   5: Other heuristic.

      INTEGER  NUMS (INUMS,NEL)      !- element steerings
      REAL GET_RANDOM                !- random number function.

      IF (IOP_seed.eq.1) then      !------ Method 1: Scan forwards ------
        DO IEL_SEED=1,NEL
          CALL GET_EL_IGRP (NUMS,INUMS,IEL_SEED, IGRP1)
          IF (IGRP1.EQ.0) GOTO 22  !- only if this not yet done (cf /CYCLE)
        ENDDO
      ELSEIF (IOP_seed.eq.2) then  !------ Method 2: Scan backwards ------
        DO IEL_SEED=NEL,1,-1
          CALL GET_EL_IGRP (NUMS,INUMS,IEL_SEED, IGRP1)
          IF (IGRP1.EQ.0) GOTO 22  !- only if this not yet done (cf /CYCLE)
        ENDDO
      ELSEIF (IOP_seed.eq.3) then  !------ Method 3: Random ------
c  we need to assemble all the free elements then pick a random one of them
c  (NOT a random hit or miss - may take a long time to find the last ones?)
        DO I=1,99*NEL
          IEL_SEED = int(GET_RANDOM()*NEL)
          CALL GET_EL_IGRP (NUMS,INUMS,IEL_SEED, IGRP1)
          IF (IGRP1.EQ.0) GOTO 22  !- only if this not yet done (cf /CYCLE)
        ENDDO
      ELSEIF (IOP_seed.eq.4) then  !------ Method 4: 'Best' surface ------
c    .. here we scan to find an element that lies on the boundary of
c       last IGRP - ie. with a node marked as IGRP-1. There may be none so
C       we have to use another method (eg #1)
c     .. maybe the one with the least free edges is best ironically - so we 
c       are less likely to leave isolated holes?
      ENDIF

      CALL MYERROR (1,'No new seed element could be found')
      IEL_SEED = 0

   22 CONTINUE        !- jup-yo when we find a valid seed
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE GROUP_CONNECTIVITY (GC,IGC,NDIM,NN,NUMS,INUMS,NEL,P)
c
C     This will find all elements that are interconnected and give them
C     the same group number.
c        Dan Kidger 21-8-97 
c
c   Method:
c     init all nodes and elements
c     loop:
c       get_next_seed: EXIT if all done
c       sweep from this seed (max=inf.)
c     endloop.
c
c  Caveats:
c     SWEEP_IGRP sweeps to elements connected at least by 2 conseq. nodes
c     but we only need one node for 'connectivity' 
C       - better to pass to SWEEP_IGRP the type of connectivity we want 
C         to check for: eg: point, edge, face.
c
      REAL       GC (IGC,NN)         !- global coords
      INTEGER  NUMS (INUMS,NEL)      !- element steerings
     &           ,P (NN)             !- workspace

C------------- 1: Initialise ---------------
      DO I=1,NN
        P(I) = 0       !- zap all nodes' codes
      ENDDO
      DO IEL=1,NEL
        CALL PUT_EL_IGRP (NUMS,INUMS,IEL, 0)  !- zap all Groups
      ENDDO

      IOP_SEED = 1   !- use the lowest numbered free element

C------------- 2: Loop the new Groups ---------------
c     IGRP = 0          
      NEL_DONE = 0
      NGROUPS = NEL      !- 10-9-97 max possible number ?
      DO IGRP = 1,NGROUPS
        NEL_TODO = NEL+99     !- ie. 'all' of them

        CALL GET_NEW_SEED (NUMS,INUMS,NEL, iel_seed, IOP_SEED)
        if (iel_seed.le.0) RETURN           !- abandon
        CALL SWEEP_IGRP (NUMS,INUMS, NEL,NN,P, IEL_SEED, IGRP
     &   ,nel_todo, nel_zone, NSWEEPS)      
        IF (NEL_ZONE.LE.0) CALL MYERROR (3,'No elements found (GREEDY)')

      ENDDO    !- loop groups
      print*,' There are', IGRP,' disjointed regions on the mesh.'
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE CONNECTIVITY_IGRP (GC,IGC,NDIM,NN,NUMS,INUMS,NEL,P)
C
C      *OBSOLETE* ? -> see GROUP_CONNECTIVITY
C     This will find all elements that are interconnected and give them
C     the same group number.
C     on return the IGRP column of NUMS is altered, P is workspace but 
C     also returns the *node* group numbers (hence orphans = -1)
c       c. Dan Kidger 1994?
C     13-3-95 now 'group' number rather than material #
C     19-8-97 we can do GREEDY like this - just stop IGRP when it is >=
C      the target value = (NEL/NPROCS), hence IGRP++ and continue
C
      REAL       GC (IGC,NN)         !- global coords
      INTEGER  NUMS (INUMS,NEL)      !- element steerings
     &         ,NUM (32)             !- one elements steering
     &           ,P (NN)             !- workspace
                                  
C.............. initialise ................
c.. realy just need to get a unique IGRP to use and so no need
C.. to zero all elements first.
      DO I=1,NN
        P(I) = -1       !- zap all nodes' codes
      ENDDO
      DO IEL=1,NEL
        CALL PUT_EL_IGRP (NUMS,INUMS,IEL, -1)  !- zap all Groups
      ENDDO
                                                    
c.......... loop and impliment
c.. pick the lowest numbered element as the 'seed' of each set.
      IGRP = 0          
      DO IEL2=1,NEL
        CALL GET_ELEMENT  (NUMS,INUMS,IEL2, NUM, 
     &     NOD_A,NDIME,ITYPE,IGRP_a,IU1,IU2,IU3)
        IF (IGRP_a.EQ.-1) THEN    !- only if this not yet done
          igrp = igrp + 1                  
          print*, 'doing Group #', igrp
          CALL PUT_EL_IGRP (NUMS,INUMS,IEL2, IGRP)
          DO J=1,NOD_a               !- set all these nodes as 'done'
            P(NUM(J)) = IGRP        
          ENDDO                     
c..................
c.. hmm should iterate this until nothing further changes
 1024     idone = 0
          DO IEL=IEL2+1,NEL              !- Loop the rest
            CALL GET_EL_IGRP (NUMS,INUMS,IEL, IGRP_B)
            IF (IGRP_B.eq.-1) then     !- only if not yet done ?

              CALL GET_ELEMENT  (NUMS,INUMS,IEL, NUM, 
     &          NOD,NDIME,ITYPE, IGRP_B,IU1,IU2,IU3)
              DO J=1,NOD               !- check if 'any' hit
                IF (P(NUM(J)).eq.IGRP) then  
                  CALL PUT_EL_IGRP (NUMS,INUMS,IEL, IGRP)
                  DO JJ=1,NOD              !- and all its nodes
                    P(NUM(JJ)) = IGRP      !- 'on' too
                  ENDDO                    !-
                  idone = 1      !- flag that an 'hit' has happened
                  GOTO 1023
                ENDIF
              ENDDO
 1023         CONTINUE    !- jump out
            ENDIF
          ENDDO     !- loop the rest
          IF (idone.eq.1) goto 1024       !- re-iterate
c    .. here if none were found on this coomplete pass - so no more can 
c    .. possibly be found - we need to pick a new 'seed' element.

        ENDIF   !- skip already 'done' elements
      ENDDO
      print*,' There are ',igrp,' different zones in the mesh!'
      RETURN
      END

C-----------------------------------------------------------------------
c     SUBROUTINE GROUP_LEGO (GC,IGC,NDIM,NN,NUMS,INUMS,NEL,P)
      SUBROUTINE GEOMETRY_IGRP (GC,IGC,NDIM,NN,NUMS,INUMS,NEL,P)
C
C     This will find all elements that are have same *SHAPE* and IMAT
C     and give them the same group number. (or IMAT?)
c           ** Not yet completed **

c     nice to use KV and/or KDIAG as workspace to store COORD, IMAT etc.
c
c     Possible algorithms:
c       1/ (faster) loop all and sort wrt IMAT  (overload with 1000*NOD too)
c            loop IMATS and get x-width, depth-sort
c              loop each piece, and sort wrt y-width
c        (mark when we get 'uniqueness' .. move to the head of the list?)
c         continue until all have been resolved
c
c       2/ (simplex) loop all elems.. extract COORD
C            compare with the base list. .. if found copy ref.
c            if not extend the base list with this elem.
c       3/ try to recursively rank the list - dealy with =subsets:
c          a/ sort elements wrt imat
c          for each imat:
c          b/ sort elements wrt DX of the BB 
c          for each of those:
c          b/ sort elements wrt DY of the BB (DZ too)
c          for each of those:
c          take the first and fully check dcoord of each
c           if a mismatch elemA is unique (?) so set elemA<-elemB
c
c
c  28-1-98 Create a set of tool subroutines that sort a given set of
c     elements wrt a criteria. 
C     Hence loop levels; loop strings,  call SORT wrt criteria #n
c   - The idea is to sort first wrt which will sort the list the 
c     fastest. ie. create the most sub-lists.
c   - The ultimate is to sort wrt. the Eigenvalue spectrum, but this is 
c     perhaps slow since we need to form the KM of every element
C
      REAL       GC (IGC,NN)         !- global coords
      INTEGER  NUMS (INUMS,NEL)      !- element steerings
     &         ,NUM (32)             !- one elements steering
     &           ,P (NN)             !- workspace
                                  
C---------------- 0: initialise --------------------
      DO IEL=1,NEL
        P(IEL) = IEL                   !- list of elements
      ENDDO
      DO IEL4=1,NEL
        IEL = P(IEL4)
        CALL PUT_EL_IGRP (NUMS,INUMS,IEL, 1)    !- assume all same
      ENDDO

C---------------- 1: loop trial discrimators --------------------

C---------------- 2: treat each sublist --------------------
c hmm once an element is unique, we can flag its IGRP as -ve to
c  speed up later searches: this element can always be skipped.
      ibeg=1
c     do ngroups=1, nel !99999
c.. search for iend = the last element of the run that shares IGRP
      CALL GET_EL_IGRP (NUMS,INUMS,p(ibeg), IGRP_base)
      iend=ibeg    !- assume this is a unique element

      DO IEL3=IBEG, IEND
          CALL GET_EL_IGRP (NUMS,INUMS,P(IEL3), IGRP)
          if (igrp.ne.igrp_base) goto 21     !*EXIT*
          IEND = IEL3      !- this is OK so continue
      ENDDO
  21  CONTINUE

c... type 1: IMAT >>>>>>>>>>>>>>>>>>
c   not perfect as we *are* allowed different IMATS that are the same
c   element type - for example in 3d tunneling
c   perhaps we should call this routine *before* *BORE_MATERIALS
c
c  Method:
c   1. Loop elements in this 'group'; pick its IMAT, store in VALUES(), then SORT
c   2. Jiggle the elements into this new order
c     3. as the IMAT changes, NGROUPS++, and store the rest as this
c
c

      DO IEL2=1,NEL
        CALL GET_ELEMENT  (NUMS,INUMS,IEL2, NUM, 
     &     NOD_A,NDIME,ITYPE, IGRP_a,IU1,IU2,IU3)
        IF (IGRP_a.EQ.-1) THEN    !- only if this not yet done
          igrp = igrp + 1                  
          print*, 'doing Group #', igrp
          CALL PUT_EL_IGRP (NUMS,INUMS,IEL2, IGRP)
          DO J=1,NOD_a               !- set all these nodes as 'done'
            P(NUM(J)) = IGRP        
          ENDDO                     
c..................
c.. hmm should iterate this until nothing further changes

 1024     idone = 0
          DO IEL=IEL2+1,NEL              !- Loop the rest
            CALL GET_ELEMENT  (NUMS,INUMS,IEL, NUM, 
     &        NOD,NDIME,ITYPE, IGRP_B,IU1,IU2,IU3)
c           NOD = NUMS(1,IEL)
            IF (IGRP_B.eq.-1) then     !- only if not yet done ?
              DO J=1,NOD               !- check if 'any' hit
                IF (P(NUM(J)).eq.IGRP) then  
                  CALL PUT_EL_IGRP (NUMS,INUMS,IEL, IGRP)
                  DO JJ=1,NOD              !- and all its nodes
                    P(NUM(JJ)) = IGRP    !- 'on' too
                  ENDDO                    !-
                  idone = 1      !- flag that an 'hit' has happened
                  GOTO 1023
                ENDIF
              ENDDO
 1023         CONTINUE    !- jump out
            ENDIF
          ENDDO     !- loop the rest
          IF (idone.eq.1) goto 1024       !- re-iterate
        ENDIF   !- skip already 'done' elements
      ENDDO
      print*,' There are ',igrp,' different zones in the mesh!'
c.. really I need to return IGRP - cos if >1 then we should stop the program?
      RETURN
      END
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C
C  Statistics:
C     Report how many are of each type - in order of decreasing number 
C     hence eg. 14.2% of elements are 'masters', of which  7.1% are 'uniques'
C     and 5.3% are of the largest group.
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      SUBROUTINE R_ADD_REM_ELEMS (IO,NUMS,INUMS,NEL)
C
C     This simply switches on or off the given elements
C
      INTEGER NUMS (INUMS,NEL)    !- ie. >=NEL :-)

      DO ILOOP=1,9999
    1   READ (IO,IOSTAT=IOS) JMAT
        CALL IN_TEST (IO,IOS,*1,*999)

        IC = 0
        DO IEL=1,NEL
          CALL GET_EL_IMAT (NUMS,INUMS,IEL, IMAT)
          IF (IMAT.EQ.ABS(JMAT)) THEN
            IC = IC + 1
            CALL PUT_EL_IMAT (NUMS,INUMS,IEL, JMAT)
          ENDIF
        ENDDO
        PRINT*, ILOOP,' :',JMAT,' : elements found =',IC
      ENDDO

 999  RETURN
      END

c-----------------------------------------------------------------------
      SUBROUTINE MESH_STATISTICS (GC,IGC,NDIM,NN,NUMS,INUMS,NEL,P)
c
c     This computes varrious statistics about the composition of the mesh
c
c     Perhaps give options for what infirmation we are after
c
c     1: Materials: how many, what they are and how many of each
c     2: El types:  how many, what they are and how many of each
c     3: corrolation table of materials and elements

c  list_mats : returns how many types and a list of them
c  count_imat : a function that counts how many of the given imat
c  list_eltypes : returns how many types and a list of them
c  count_eltype : a function that returns how many of the given eltype
c * Danplot could produce a bar chart of this.

c LIST_MATS:
c   create a 'bucket' of elements in P()
c A: pick an element (it will be new)
c    record its imat (so maintain an ordered list)
c   scan 1:bucket_size
c     If it matches, remove from the bucket and IC++ (so close list) 
c   enddo
c   if bucket /= empty goto A:
c << in simplex form, forget the bucket; scan ordered list, then scan 
c      all 1:nel >>
c << genericaly I can use IGRP, IMAT, IU1,IU2,IU3, eltype, the node# >>
c    also num(1), etc to crash detect for IMS's parallel PCG
c - LIST_MATS, effectively calls COUNT_MATS for the head of the list as 
c     the bucket shrinks
c - perhaps we are realy just sorting the bucket into ascending order
c  (so nlogn), thence count the different entries

c   Corrolation table:
c     - List the materials down the page (the el types across)
c     - each 'box' stores how many there are of this
c     - Pivoting: so 'diagonaly dominant'? (skip for now)
c     - order: ascending (nice) or order of first occurence?
c     can loop all mats, then count each - if =0 skip printing
c
c    Loop Imats
c      create a bucket of this Imat
c      loop eltypes & count from this bucket (so bucket gets smaller)
c      so print each
c       .. the format should be auto chosen to fit ..

c potentialy I will need plenty of temporary arrays - Ok in F90, but annoying 
c in F77
      END
c-----------------------------------------------------------------------
c-----------------------------------------------------------------------








C-----------------------------------------------------------------------
      SUBROUTINE FIND_NODES (GC,IGC,NDIM,NN,POINT,NUMLIST,INUMLIST,IC)
C
C        .. child of R_NODMOV
C      *OBSOLETE as routines can multiply call FIND_NODE
C     given a co-ordinate, this returns its node number(s) in NUMLIST
C
      REAL GC(IGC,*),POINT(*)
      INTEGER NUMLIST(INUMLIST)

      IC = 0
      IFROM = 1
      ITO = NN
      DO I=1,INUMLIST
        CALL FIND_NODE (GC,IGC,NN,NDIM,POINT,IFROM,ITO,INODE)
        IF (INODE.GT.0) THEN         !-- node found
          IC = IC + 1
          NUMLIST(IC) = INODE
          IFROM = INODE + 1                   
        ELSE
          RETURN                     !-- No more nodes to find
        ENDIF
      ENDDO
      CALL MYERROR (1,'Too many nodes found in FIND_NODES')
      RETURN
      END

