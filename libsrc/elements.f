C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C    > > > > > > > >     Element Database routines < < < < < < < < <
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
c.. need to decide why GET_FACE is here but WTHATN is in 'shape.f'

c---------- the Keyword entry points ----------
c      KEY_FACETS    : strip/read/write facets/rings

c------- the NUMS database 'class' ------
c      PUT_ELEMENT   : saves an element into the database
c      GET_ELEMENT   : returns an element from the database
c      PUT_EL_IMAT   : saves just the MAT # into the database
c      GET_EL_IMAT   : returns just the MAT # from the database
c      PUT_EL_IGRP   : saves just the GROUP # into the database
c      GET_EL_IGRP   : returns just the GROUP # from the database
c      PUT_ELEMENT_ALL : stores a whole element qith new node #s

c------ misc ----
c
c      GET_COORD     : simply turns NUM() into COORD from GC()
c      FIND_NODE     : returns the node # at a given coord
c      GET_MESH_RANGE: returns the bounding box of the mesh

c----- FACETs of Elements ----
c.. maybe it is time I had a K_FACETS.F ; it could handle RINGS too.
c      GET_FAC       : the returns the (local) coords of a given sub-facet
c      GET_FACE      : returns the nodes on one face of a 3D element
c      GET_FACE2     : returns the nodes on one face of a 3D element
c        GET_FACE_TABLE : for one elem type
c        COPY_TABLE     : just copies an array
c        GFACE_obs      : early version of GET_FACE (slower?)
c      FSTRIP2          : returns a table of every facet in the mesh (& touchers)
c      DEL_INT_FACETS   : reduces FACETS to leave only 'externl' ones
c      DEL_INT_MAT_FACETS   : reduces FACETS to leave only 'externl' ones
c      FACETS_TO_ELEMENTS : turns FACETS into independant elements (eg -> .OFF)

c----- EDGEs of Elements ----- 
c.. note that we can also use a RING as a start for ADVANCING_FRONT
c      STRIP_EDGES      : reduces FACETS into its component polygons
c        FACETS_TO_LINES  : abstract edge segments from the facets
C        LINES_TO_RINGS   : daisy-chains these edges into polygon 'rings'
C          GET_DC           : given a pair of nodes, return their unit vector
C          GET_DC_NORMAL    : given a facet, return its normal vector
C      PLOT_MESH_BOUNDARY : plots the RINGS (eg plot the DISPS on top)
c

c----- STRESSES of Elements ----
c.. 
c  <   NUL_STRESSES  : does the whole data structure ? >
c      R_STRESSES    : reads a table (& MALLOCs as needed)
c  <   WR_STRESSES   : writes table (for plotting/restart) >
c      PUT_STRESS    : stores an elements NGP by IH stress(+strain?) terms
c      GET_STRESS    : returns an elements NGP by IH stress terms
c      GET_STRESS_TERM: given 'Sx' or 'J2', calcs all GP's of an element.
c      GP_TO_NODES   : extrapolates a set of GP values to the element's nodes
c        GET_GP_MATRIX : calcs the GP-> nodes rectangular TR matrix
c
c

C-----------------------------------------------------------------------
C   1: handling the ELEMENT database (NUMS)
c
c  Entrypoints:
c     PUT_ELEMENT, GET_ELEMENT
C     PUT_EL_IMAT, GET_EL_IMAT
C     PUT_EL_IGRP, GET_EL_IGRP
C     PUT_ELEMENT_ALL

c  Global coordinates (GC) handling:
C     GET_COORD
c     FIND_NODE
c     GET_MESH_RANGE

c     GET_FAC

C-----------------------------------------------------------------------

C-----------------------------------------------------------------------
      SUBROUTINE PUT_ELEMENT (NUMS,INUMS,IEL, NUM, NOD,NDIME,ITYPE
     &      ,IMAT,IUSER1,IUSER2,IUSER3)
C
C     This adds an element reference to the NUMS database
C     : an element of NOD nodes in NDIME dimensions (eg 2D) of 
C       element 'type' ITYPE,
C       IUSER(1-3) are user-defined properties (eg. element groups.)
C
C... must test to see wether there is enough space for this element !

      INTEGER NUMS(INUMS,*) , NUM(*)

      DO J=1,NOD
        NUMS(J,IEL) = NUM(J)
      ENDDO

      NUMS (INUMS  ,IEL) = NOD      
      NUMS (INUMS-1,IEL) = NDIME    
      NUMS (INUMS-2,IEL) = ITYPE    
      NUMS (INUMS-3,IEL) = IMAT     
      NUMS (INUMS-4,IEL) = IUSER1   
      NUMS (INUMS-5,IEL) = IUSER2   
      NUMS (INUMS-6,IEL) = IUSER3  

      DO J=NOD+1,INUMS-7
        NUMS(J,IEL) = 0    !- null any other columns
      ENDDO
      END !SUBROUTINE PUT_ELEMENT


C-----------------------------------------------------------------------
      SUBROUTINE GET_ELEMENT (NUMS,INUMS,IEL, NUM, NOD,NDIME,ITYPE
     &      ,IMAT,IGROUP,IUSER2,IUSER3)
C
C     This extratcs an element reference to the NUMS database
C     : an element of NOD nodes in NDIME dimensions (eg 2D) of 
C       element 'type' ITYPE,
C       IUSER(1-3) are user-defined properties (eg. element groups.)
C
      INTEGER NUMS(INUMS,*) , NUM(*)

      NOD     = NUMS (INUMS  ,IEL) 
      NDIME   = NUMS (INUMS-1,IEL) 
      ITYPE   = NUMS (INUMS-2,IEL) 
      IMAT    = NUMS (INUMS-3,IEL) 
      IGROUP  = NUMS (INUMS-4,IEL) 
      IUSER2  = NUMS (INUMS-5,IEL) 
      IUSER3  = NUMS (INUMS-6,IEL) 

      DO J=1,NOD
        NUM(J) = NUMS(J,IEL)
      ENDDO
      END !SUBROUTINE GET_ELEMENT

C-----------------------------------------------------------------------
      SUBROUTINE PUT_EL_IMAT (NUMS,INUMS,IEL, IMAT)
C     simply sets the material number of a given element (cf PUT_ELEMENT)
      IMPLICIT NONE
      INTEGER INUMS,IEL,IMAT,NUMS(INUMS,*)
      NUMS (INUMS-3,IEL) = IMAT
      END

C-----------------------------------------------------------------------
      SUBROUTINE GET_EL_IMAT (NUMS,INUMS,IEL, IMAT)
C     simply returns the material number of a given element (cf GET_ELEMENT)
      IMPLICIT NONE
      INTEGER INUMS,IEL,IMAT,NUMS(INUMS,*)
      IMAT = NUMS (INUMS-3,IEL) 
      END

C-----------------------------------------------------------------------
      SUBROUTINE PUT_EL_IGRP (NUMS,INUMS,IEL, IGRP)
C     simply sets the 'group' number of a given element 
      IMPLICIT NONE
      INTEGER INUMS,IEL,IGRP,NUMS(INUMS,*)
      NUMS (INUMS-4,IEL) = IGRP
      END

C-----------------------------------------------------------------------
      SUBROUTINE GET_EL_IGRP (NUMS,INUMS,IEL, IGRP)
C     simply returns the 'group' number of a given element
      IMPLICIT NONE
      INTEGER INUMS,IEL,IGRP,NUMS(INUMS,*)
      IGRP = NUMS (INUMS-4,IEL) 
      END

C-----------------------------------------------------------------------
      SUBROUTINE PUT_ELEMENT_ALL (NUMS,INUMS,NEL,GC,IGC,NDIM,NN 
     &       ,COORD,ICOORD ,NOD,NDIME,ITYPE, IMAT,IU1,IU2,IU3)
c
c     This adds ALL an element to GC and NUMS
c     by creating new node numbers for all points.
c         Dan Kidger 18-8-94
c     This is called from READ_NFF, READ_DXF_**, etc.
c     Also would be used for Isosurfaces using 'Marching Cubes' ?
c     A call to *MERGE_NODES afterwards is helpful.
c              
      INTEGER NUMS(INUMS,*), NUM(50), IMAT
      REAL GC (IGC,*)
      REAL COORD(ICOORD,*)

      DO I=1,NOD
        NN =NN + 1
        NUM(I) = NN
        DO J=1,NDIM
          GC(J,NN) = COORD(I,J)
        ENDDO
      ENDDO
      NEL = NEL + 1
      CALL PUT_ELEMENT 
     &    (NUMS,INUMS,NEL, NUM, NOD,NDIME,ITYPE, IMAT,IU1,IU2,IU3)
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE GET_COORD (NUM,NOD,NDIM,GC,IGC,COORD,ICOORD)    !- coords
C
C     This simply extracts the nodal coordinates from the GC table
C         Dan Kidger c.1992?
C
      IMPLICIT NONE
      INTEGER NUM(*), NOD,NDIM, IGC,ICOORD
      REAL GC(IGC,*),COORD(ICOORD,*)
      INTEGER I,J
      DO I=1,NOD
        DO J=1,NDIM
          COORD(I,J) = GC(J,NUM(I))
        ENDDO
      ENDDO
      END

C-----------------------------------------------------------------------
      SUBROUTINE FIND_NODE (GC,IGC,NN,NDIM,XY,IFROM,ITO,INODE)
C
C     This searches the node-list GC (IFROM to ITO) to find the node
C     number for a given coordinate (INODE)
C     - allows 230964. as a wildcard
C       DJK 1993
C
C     18-10-95 now supports a reverse search if ITO is smaller than IFROM
c       hmm be careful then of 'bounce' calls
c     10-9-97 NN added to the arg list.
c   < ie. if IFROM>NN then clearly we should abort ? - generaly abort
c     if either ITo,IFROM is outside 1->NN
      IMPLICIT NONE
      INTEGER IGC,NDIM,IFROM,ITO,INODE, NN
      REAL GC(igc,*), XY(*)
      INTEGER I,J,ISTEP
      REAL WILD, TOL

      WILD = 230964.
      TOL = 1.E-4
      INODE = 0                        !- assume not found
      istep = 1
      if (ito.lt.ifrom) istep =-1      !- need a negative step
c     if (ifrom.gt.nn) return
      if (max(ito,ifrom).gt.nn) return     !- 10-9-97
      if (min(ito,ifrom).lt.1) return
      DO I=IFROM,ITO,ISTEP
        DO J=1,NDIM
          IF (ABS(XY(J)-  WILD ).GT.TOL .AND.
     &        ABS(XY(J)-GC(J,I)).GT.TOL) GOTO 1    !-- give-up !
        ENDDO
        INODE = I
        RETURN        !-- OK we have found the right node
    1   CONTINUE   !-- carry on looking
      ENDDO

      INODE = 0       !-- The node was NOT found
      END


!-----------------------------------------------------------------------
      SUBROUTINE GET_MESH_RANGE (GC,IGC, NN,NDIM, 
     &     GC_MIN,GC_MAX,DIAG)
!
!     this calculates range of the data in GCOORD
!     and hence DIAG, the length of the bounding cube's diagonal
!     cf using MINVAL/MAXVAL of F90
!
      REAL GC (IGC,NN), GC_MAX(5),GC_MIN(5), DIAG
      INTEGER I,J

      DO J=1,5
        GC_MIN(J) = 0.              !- Null Z sizes 
        GC_MAX(J) = 0.              !-(eg. if a 2D mesh
      ENDDO

C------------- loop and find the MAX,MIN & CENTROID --------------
      DO J=1,NDIM
        GC_MIN(J) = GC(J,1)     !- initialise the max and min
        GC_MAX(J) = GC(J,1)     !- to the first node
        DO I=1,NN
          GC_MIN(J) = MIN (GC_MIN(J),GC(J,I))
          GC_MAX(J) = MAX (GC_MAX(J),GC(J,I))
        ENDDO
      ENDDO

C---------- find the 'chararistic model length' = major daigonal -------
      DIAG = 0.
      DO J=1,NDIM
        DIAG = DIAG + (GC_MAX(J)-GC_MIN(J)) **2 
      ENDDO
      DIAG = SQRT (DIAG)
      RETURN
      END

!-----------------------------------------------------------------------
      SUBROUTINE GET_MESH_STATS_IMAT (NUMS,INUMS,NEL,GC,IGC,NDIM,NN
     &   ,nel_imat, nel_imat_invis ,gc_min,gc_max, volume
     &   ,JMAT,IPR_EL, IAXES)
!
!     This calculates various statistics about a zone of the mesh:
!     - no. of elements
!     - how many are invisible
!     - smallest / largest volume
!     - bounding box
!
!     Consider using an F90 derived type here
!     JMAT=230964 will give the whole mesh (cf. GET_MESH_RANGE)
!     Dan Kidger 1/6/04

!   consider splitting into:
!     topology stats: element code, #els, #invis
!        touching imats (cf.  DUMP_THIS_GRP), nels on surface, ..
!     geometry stats: volume, bounding box

      INTEGER    NUMS (INUMS,NEL)    !-- the topology of the elements
      REAL         GC (IGC,NN)       !-- the coordinates of all the nodes

      PARAMETER (M_NOD = 32           !-- max # nodes per element
     &          ,M_NDIM= 4 )          !-- max # freedoms per node
      PARAMETER (ICOORD = M_NOD)
      REAL      COORD (ICOORD,M_NDIM)    !-- nodal coords

      real  GC_MAX(5),GC_MIN(5), DIAG
      character string*8

      INTEGER I,J, NUM(32)

      gc_min(1:M_NDIM) = 0.;   gc_max(1:M_NDIM) = 0.
      DO j=1,ndim  !- initalise (careful if node1 is not in this zone)
        gc_min(j)=gc(j,1)  ;   gc_max(j)=gc(j,1)
      enddo
      volume=0.

C--------------- loop through the elements ---------------------------
      nel_imat=0 
      nel_imat_invis=0      
      DO IEL=1,NEL
        CALL GET_EL_IMAT (NUMS,INUMS,IEL, IMAT2)
        IMAT=ABS(IMAT2)
        IF (IMAT.eq.JMAT .or.JMAT.eq.230964) THEN   !- wildcard 
        nel_imat= nel_imat+1
        if (imat<0) nel_imat_invis = nel_imat_invis + 1
!       - what if imat=0 ?

!-- get element type
        CALL NUM_TO_CODE (NDIME,NOD,ITYPE, STRING)

        CALL GET_ELEMENT (NUMS,INUMS,IEL, NUM, NOD,NDIME,ITYPE
     &      ,IMAT,IUSER1,IUSER2,IUSER3)
        CALL GET_COORD (NUM,NOD,NDIM,GC,IGC,COORD,ICOORD)    !- coords

!--- accumulate the bounding box --
        DO J=1,NDIM
          DO I=1,NOD
            gc_min(j) = min(gc_min(j),coord(i,j))
            gc_max(j) = max(gc_max(j),coord(i,j))
          ENDDO
        ENDDO

!--- accumulate the material volume --
        call GET_DEFAULT_NGP (NOD,NDIME,NGP)
        IAXIS=1   !- assume *not* axisymmetric !
        volume_el=0.
! ** FIX ME **
!       call GET_ELEMENT_AREA (NDIME,NOD,ITYPE,COORD,ICOORD
!    &               ,NGP,IAXES,VOLUME_EL)
         volume=volume+volume_el


!-- do I want to calculate the diagonal of the bounding box also ?

        ENDIF     !-- only a particular material
      ENDDO   !-- elements


C---------- find the 'chararistic model length' = major daigonal -------
!     DIAG = 0.
!     DO J=1,NDIM
!       DIAG = DIAG + (GC_MAX(J)-GC_MIN(J)) **2
!     ENDDO
!     DIAG = SQRT (DIAG)
      RETURN
      END


C-----------------------------------------------------------------------
      SUBROUTINE GET_FAC (LC_SF,ILC_SF, I_SF, N_SF, NN_SF, NN_FT)
C
C     this returns the I_SF'th sub-facet of NN_SF points in LC_SF
C     of a N_SF*N_SF patch 
C     for quadrilaterals and triangles 
C         DJK 1993

c    hmm: maybe DANBLOCKS should use this concept for triangles

C.... really the looping of the sub-facet nodes should be in the main
c.... program.. a sub-routine could return them (eg. 4 node or 8 node
C....                                             or even sub-triangles) 
C.... ¨ or have 2 interfaces to SAMPLE .. one will do all nodes
C....  the other just a given (by #?) point

      REAL LC_SF (ILC_SF,*)
      INTEGER LC4 (2,4)
      DATA LC4/-1,-1, -1,0,  0,0, 0,-1/  ! changed to clockwise 5-6-92 !
c--> 4nq (nice to have an 8nq also?)  .. cf DANBLOCKS

      K = NN_FT
      deltax = 1./real(n_sf)                ! width of a sub-element
      deltay = deltax
C-------------------- quadrilateral subdivision ------------------------
      IF (K.eq. 4.or.K.eq. 5.or.K.eq. 8.or.     !-select case surely ?
     &    K.eq. 9.or.K.eq.12.or.K.eq.17     ) THEN 
        NN_SF = 4     !-------- 4-node quadrilateral sub-division
        IY = 1 +    (I_SF-1)/    N_SF
        IX = 1+ MOD (I_SF-1+N_SF,N_SF)
        DO I=1,NN_SF
          LC_SF(I,1) = -1. + 2. * REAL (IX+LC4(1,I)) * deltax
          LC_SF(I,2) = -1. + 2. * REAL (IY+LC4(2,I)) * deltay
        ENDDO

C----------------------- triangle subdivision --------------------------
C --> wow ! what a nice piece of code (even if I do say so myself)!
      ELSEIF (K.eq.3.or.K.eq.6.or.K.eq.10.or.K.eq.15) THEN
        NN_SF = 3
        ix = int(sqrt(real(i_sf-1)+ .01) +1)
        iy = n_sf - (ix*ix-i_sf)/2   +1
        LC_SF(1,1) = 1.- deltax * (ix-1)      ! x1
        LC_SF(1,2) = 1.- deltay * (iy-1)      !    y1
        IF (MOD(I_SF,2).EQ.MOD(IX,2))THEN     ! an 'up' pointing triangle
          LC_SF(2,1) = LC_SF(1,1) - deltax    ! x2
          LC_SF(2,2) = LC_SF(1,2) + deltay    !    y2
          LC_SF(3,1) = LC_SF(1,1) - deltax    ! x3
          LC_SF(3,2) = LC_SF(1,2)             !    y3
        ELSE
          LC_SF(2,1) = LC_SF(1,1)             ! x2
          LC_SF(2,2) = LC_SF(1,2) + deltay    !    y2
          LC_SF(3,1) = LC_SF(1,1) - deltax    ! x3
          LC_SF(3,2) = LC_SF(1,2) + deltay    !    y3
        ENDIF
      ELSE
        PRINT*,'*** WARNING: NEN=',NN_FT,' not known in GET_FAC ***'
      ENDIF
      RETURN
      END

C-----------------------------------------------------------------------
C   2: handling the Element surfaces database (FACETS and RINGS)
C-----------------------------------------------------------------------
*      MODULE M_FACETS
c
c     The data structures and handling routines for the surfaces
c     of the finite element mesh: used particularly for plotting and smoothing
c        Dan Kidger  6-3-98
c     Really these arrays should be ALLOCATABLE, with perhaps an initial 
c     size. This then needs to be re-allocated larger as needed. *sqrt(2)
c
c
*      INTEGER,PARAMETER ::          
c     &          MELS =60000,        !-- Max # of elements (change as needed)
c     &          IFCETS = MELS*6,    ! Max # of facets
*     &          IFCETS = 300000,     ! Max # of facets
*     &          IRINGS  = 2000       ! Max # of boundary edge nodes
*      integer::  FACETS (3,IFCETS)   !- table of ALL the 3D faces (rev)
*      integer::  RINGS (irings)      !- polygons of the mesh boundary
cc     integer,allocatable :: facets(:,:),rings(:)
*      integer:: nfcets=0             ! initialy none

*      CONTAINS
C-----------------------------------------------------------------------

      SUBROUTINE KEY_FACETS
     &      (FOUND,IPR,KEYWORD,IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL,P,
     &       FACETS,IFCETS,NFCETS, RINGS,IRINGS) 
C
C     A Keyword handler for handling the facets and edges of a mesh.
C       Dan Kidger  26-2-97
C
C     This is used by DANMESH and DANPLOT but not DANFE (it is now! 11/97)
C
      REAL GC (IGC,*)
      INTEGER NUMS(INUMS,*), IO, P(*), IPR
     &  ,FACETS (3,IFCETS)       !- 3->7 so can store edge info too? (No)
     &   ,RINGS (IRINGS)         !- polygons edges
      LOGICAL FOUND   !, OPENED
      CHARACTER KEYWORD*(*) !, FILE*70 !,FILE2*70
c     DATA IO2/80/            !- this is for file writes I think
c              . nice to have a 'get next free unit after IBASE' cf CBS)
      FOUND = .TRUE.

      IF (KEYWORD.EQ.'*STRIP_FACETS') THEN
        CALL FSTRIP3 (NUMS,INUMS,NEL,NN,FACETS,IFCETS,NFCETS,P) !* super-fast *

      ELSEIF (KEYWORD.EQ.'*QUERY_FACETS') THEN
        CALL Q_FACETS (NUMS,INUMS,NEL,NN,FACETS,IFCETS, NFCETS)
        CALL Q_RINGS (RINGS,IRINGS)

      ELSEIF (KEYWORD.EQ.'*DELETE_INTERNAL_FACETS') THEN
        CALL DEL_INT_FACETS (FACETS, NFCETS)        ! old
      ELSEIF (KEYWORD.EQ.'*DELETE_FACETS') THEN     ! 13-7-99 
         CALL R_DEL_FACETS (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL,P,
     &       FACETS,IFCETS,NFCETS)

      ELSEIF (KEYWORD.EQ.'*FACETS_TO_ELEMENTS') THEN
        CALL FACETS_TO_ELEMENTS (GC,IGC,NDIM,NN,NUMS,INUMS,NEL,P
     &             , facets,nfcets)
      
      ELSEIF (KEYWORD.EQ.'*STRIP_EDGES') THEN
c.. nicer to have a default option, but allow it to be modified.
        READ(IO,*) IOP1, IOP2          !- (1,0) is best)
        CALL STRIP_EDGES (GC,IGC,NDIM,NN,NUMS,INUMS,NEL
     &      ,FACETS,IFCETS,NFCETS,P,RINGS,IRINGS,iop1, iop2)
c        CALL R_STRIP_EDGES (GC,IGC,NDIM,NN,NUMS,INUMS,NEL
c     &      ,FACETS,IFCETS,NFCETS,P,RINGS,IRINGS)

c.. Ok note read_a_filename : as a subroutine but supply the default 
c   string. cf. giving names to load steps etc.

      ELSEIF (KEYWORD.EQ.'*WRITE_RINGS') THEN
c       .. hence we can use this to do advancing front ?
        open (81,file='rings.dat')   !- or read a filename
        CALL WRITE_RINGS (81,GC,IGC,NDIM,NN,RINGS)
        close (81)


      ELSEIF (KEYWORD.EQ.'*BORE_MATERIALS') THEN
         CALL R_BORE_MATS (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL,P,
     &       FACETS,IFCETS,NFCETS)

      ELSEIF (KEYWORD.EQ.'*DRAW') THEN
c        CALL DRAW_ANYTHING (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL,P,
c     &       FACETS,IFCETS,NFCETS, RINGS,IRINGS)

      ELSEIF (KEYWORD.EQ.'*PLOT_MESH_BOUNDARY') THEN
        CALL PLOT_MESH_BOUNDARY (GC,IGC,NDIM,NN,RINGS)


c     ELSEIF (KEYWORD.EQ.'*MARK_RING_NODES') THEN
c       .. This lets as fix these nodes when 'barycentric smoothing'.
c       CALL MARK_RING_NODES (RINGS, P,NN)  !- find unmovable nodes.

      ELSEIF (KEYWORD.EQ.'*MARK_BOUNDARY') THEN
c.. overload to all 'external', different imat, RINGS only, etc.?
       CALL MARK_BOUNDARIES (GC,IGC,NDIM,NN,NUMS,INUMS,NEL,
     &      FACETS,IFCETS,NFCETS,P,RINGS,IRINGS)

c---------------- generic plotting -------------
c... to plot *any* mesh features: elems, fill, node#, boundary, etc.
c..  It should plot by layers. (but 3d ?)
c   ( calc scale factors first)
      ELSEIF (KEYWORD.EQ.'*DRAW') THEN
        call draw()

C------------------------- else unknown --------------------------------
      ELSE
        FOUND = .FALSE.
      ENDIF
      RETURN
      END !SUBROUTINE KEY_FACETS

C-----------------------------------------------------------------------


C-----------------------------------------------------------------------
      SUBROUTINE FSTRIP_NUL (NUMS,INUMS,NEL,FACETS,IFCETS,NFCETS,P)
C
C     THIS turns NUMS into FACETS-a list of element face pointers
C     Data as:  Element #, Facet # (1-6), 'touching' facet #
C     - This is the simplex method :
C       it only creates the list of elements and the (1..6) facets
C       ** the 'toucher' column is (0) left blank! **
C          c. DJK 4-11-95
C
      INTEGER INUMS, NEL, IFCETS, NFCETS
      INTEGER
     &     NUMS (INUMS,*)        !- The element steerings
     &  ,FACETS (3,IFCETS)       !- 3->7 so can store edge info too? (NO)
     &       ,P (IFCETS)         !- workspace only (unused)
     &     ,NUM (32)             !- steering for one element
     &     ,FS1 (19)             !- nodes on the first/second facet

c-------------- 1: determine the total size ----------
      NFCETS=0
      DO IEL=1,NEL
        CALL GET_ELEMENT     
     &  (NUMS,INUMS,IEL, NUM, NOD1,NDIME,ITYPE, IMAT1,IU1,IU2,IU3)
        CALL GET_FACE (NUM,NOD1,NDIME,ITYPE,NFACES,FS1,NN_F1,NN_FT1, 1)
        NFCETS = NFCETS + NFACES
      ENDDO
c-------------- 2: allocate storage -------------
C     IFCETS= NFCETS
c     if (allocated(facets)) dealocate(facets)
c     ALLOCATE (FACETS(3,NFCETS))
c-------------- 3: store the data ----------
      I = 0
      DO IEL=1,NEL
        CALL GET_ELEMENT     
     &  (NUMS,INUMS,IEL, NUM, NOD1,NDIME,ITYPE, IMAT1,IU1,IU2,IU3)
        CALL GET_FACE (NUM,NOD1,NDIME,ITYPE,NFACES,FS1,NN_F1,NN_FT1, 1)
        DO IFACE=1,NFACES         ! loop this element's faces
          CALL GET_FACE (NUM,NOD1,NDIME,ITYPE,IFACE,FS1,NN_F1,NN_FT1,2) 
          I = I + 1
          FACETS(1,I) = IEL        !--> = the element number
          FACETS(2,I) = IFACE      !--> only one face (so face # == 1)
          FACETS(3,I) = 0          !--> all facets on the 'outside'(no pointer)
        ENDDO
      ENDDO
      END !SUBROUTINE FSTRIP_NUL

C-----------------------------------------------------------------------
      SUBROUTINE FSTRIP_VERIFY (NUMS,INUMS,NEL,FACETS,IFCETS,NFCETS,PL2)
C
C     * NOT IMPLEMENTED YET *
C     This verifies that the current FACETS() does accurately cross-
c     reference itself.
C     Data as:  Element #, Facet # (1-6), 'touching' facet #
C     - This only tests internal facets are OK, not ones reported as 
C        external; so no good after calling FSTRIP_NUL :-(
C          c. DJK 4-11-95
C
      INTEGER
     &     NUMS (INUMS,*)        !- The element steerings
     &  ,FACETS (3,IFCETS)       !- 3->7 so can store edge info too? (NO)
     &     ,PL2 (IFCETS)         !- workspace only (unused)
c     &     ,NUM (32)             !- steering for one element
c     &     ,FS1 (19)             !- nodes on the first/second facet

c... not yet done !
      END !SUBROUTINE FSTRIP_VERIFY

C-----------------------------------------------------------------------
      SUBROUTINE FSTRIP3 (NUMS,INUMS,NEL,NN
     &   ,FACETS,IFCETS,NFCETS,PL2)
C
C     THIS turns NUMS into FACETS-a list of element face pointers
C     Data as:  Element #, Facet # (1-6), 'touching' facet #, (4)
C     .. the sequel to FSTRIP2, now use a FSTRIP_NUL first then calcs 
C      the min node # (and max) of each facet, then loop in this order
C      the # hits max is then 12 for each facet, not nfcets^2 !

C.. revisions:
C 19-11-93 v2 hmm. could make 'touching' # -ve if of a different IMAT ??
C  4-11-95 v3 should be much much faster now! almost order n (was n log n)

      INTEGER
     &     NUMS (INUMS,*)        !- The element steerings
     &  ,FACETS (3,IFCETS)       !- 3->7 so can store edge info too? (NO)
     &     ,PL2 (IFCETS)         !- workspace only
     &     ,NUM (32)  ,NUM2(32)  !- steering for one element
     &     ,FS1 (19)  ,FS2 (19)  !- nodes on the first/second facet

c     INTEGER IWORK
      PARAMETER (IWORKspace=210 000*6)
c     PARAMETER (IWORKspace=20000*6)
c     integer,allocatable :: inode_lo(:)
      INTEGER INODE_LO (iworkspace)   !- workspace for min node #s :-(

      N_INT = 0     !- total # of 'internal' facets hetero/homo-imat
      N_EXT = 0     !- total # of 'external' facets

c--------------- 0: start with a blank set of facets -------------------
c.. hmm what if I want my own 'special' set of facets to work with, eg.
c  after *DELETE_FACETS to remove much of the data.
      CALL FSTRIP_NUL (NUMS,INUMS,NEL,FACETS,IFCETS,NFCETS,PL2)
cF90     allocate (inode_lo(nfcets))
cF77.... check that we have enough memory to continue ....
C.. (maybe I can  resort to FSTRIP2 if this fails?)
      IF (NFCETS.GT.iworkspace) THEN
        PRINT*, '<> Not enough storage for FSTRIP3 !'
        RETURN
      ENDIF

C--------------- 1: compute the min node # of every facet ---------------
      DO IFC1 = 1,NFCETS
        IEL1  = FACETS(1,IFC1)
        IFACE1= FACETS(2,IFC1)
        CALL GET_ELEMENT         
     &    (NUMS,INUMS,IEL1, NUM, NOD,NDIME,ITYPE, IMAT1,IU1,IU2,IU3)
        CALL GET_FACE (NUM ,NOD,NDIME,ITYPE,IFACE1,FS1,NN_F,NN_FT, 2 )

c-------- get the min/max nodes of this facet ---------
        N_MIN= FS1(1)        !( just use MINVAL and maxval in F90)
        N_MAX= FS1(1)
        DO I=2,NN_F
          N_MIN= MIN (N_MIN,FS1(I))
          N_MAX= MAX (N_MAX,FS1(I))
        ENDDO
        INODE_LO(IFC1) =  N_MIN ! *1000 000     !  & this facets' MIN
c       INODE_LO(IFC1) =  N_MAX          !  & this facets' MAX
c       INODE_LO(IFC1) =  N_MIN*NFCETS+N_MAX !  l-o-n-g code
c       INODE_LO(IFC1) =  (N_MIN-1)*NN +(N_MAX-1) !  l-o-n-g code

c       FACETS(3,IFC1) = -N_MAX          !  & this facets' MAX
c       FACETS(4,IFC1) = -N_MAX          !   (and MIN)  . node numbers
      ENDDO

C----------- sort the list into ascending first node number --------
      CALL INDEX_INTEGERS (INODE_LO,NFCETS, PL2)

c--- hack  <- why?
      DO I=1,NFCETS
        FACETS(3,I) = -1
      ENDDO

c------- 2: Loop facets and compare test a 'hit' with all others --------
C Algorithm:
C     loop all facets in order of their lowest node number
C     for each get its details .. skip if already been 'hit'
C       loop following facets 
c          if min node # is /=   ..mark as 'external' and skip out.
c          get its details 
c
      DO IFC_B1 = 1,NFCETS
        IFC1  = PL2(IFC_B1)          !- get its 'actual' facet #
        IEL1  = FACETS(1,IFC1)
        IFACE1= FACETS(2,IFC1)
        ITCH1 = FACETS(3,IFC1)
        IF (ITCH1.gt.0) goto 44      !- exit 'cos already been hit
c       .. no cos we work *down* thru the list we wont get any back-references

        CALL GET_ELEMENT 
     &   (NUMS,INUMS,IEL1, NUM, NOD1,NDIME,ITYPE, IMAT1,IU1,IU2,IU3)

c.. If this facet is a 2d element then assume external, directly ?
c.. or even make 2d elements double sided?
c     so surface 4nq on 8nb just leaves the upper 4nq surface?
c ( but what if I start with the 8nb and find the 4nq touching it?)
c       IF (NDIME.EQ.2) THEN
c           IFC_HIT = 0   
c           GOTO 22
c       ENDIF

        CALL GET_FACE 
     &   (NUM,NOD1,NDIME,ITYPE, IFACE1,FS1,NN_F1,NN_FT1 ,2) 
        ILO  = INODE_LO(IFC1)      !- min node # of this facet
        IBOT = FS1(1)              !- base node of this facet (any will do)


c----------------- check rest_of_facets for a hit-----------------------
c... now try and find a hit to these nodes in FS1() 
        IFC_HIT = 0                 !- assume touching facet doen't exist

        DO IFC_B2 = IFC_B1+1,NFCETS     !- loop other facets (usu only 12ish)
          IFC2  = PL2(IFC_B2)           !- get its 'actual' facet #
          IF (INODE_LO(IFC2).NE.ILO) GOTO 22     !- now beyond range so *EXIT*

c... hack to see if min/max nodes == is enough 
c          IFC2_HIT = IFC2
c          goto 22

          IEL2  = FACETS(1,IFC2)
          IF (IEL1.eq.IEL2) goto 1   !- *CYCLE*: an element cannot touch itself
          IFACE2= FACETS(2,IFC2)
          ITCH2 = FACETS(3,IFC2)
          IF (ITCH2.gt.0) goto 1        !- *CYCLE* cos already been hit.
c.. note here that a tunnel lining *may* give 3 co-planar facets.

c 15-2-98 hmm, surface patches of 4nq's on 8nb meshes should survive
        
          CALL GET_ELEMENT
     &     (NUMS,INUMS,IEL2, NUM2, NOD2,NDIME,ITYPE, IMAT2,IU1,IU2,IU3)
          CALL GET_FACE 
     &     (NUM2,NOD2,NDIME,ITYPE, IFACE2,FS2,NN_F2,NN_FT2 ,2)
          IF (NN_FT1.NE.NN_FT2) GOTO 1      !- skip if incompatible ?

c--------- find the base node (1:8) in the second facet ---------
          DO IBASE=1,NN_F1
            IF (FS2(IBASE) .EQ. IBOT ) GOTO 3  !- found it
            ENDDO
          GOTO 1               ! no match so *CYCLE* to the next facet

    3     CONTINUE

c--------- check all node #'s match around the 2 facets ---------
          DO I1=2,NN_F1 
            I2 = 1+MOD (NN_F1+IBASE -I1, NN_F1)     
            IF (FS1(I1).ne.FS2(I2) ) GOTO 1     !- no match so *CYCLE*
          ENDDO
          IFC_HIT = IFC2
          GOTO 22              !- full hit so exit mark the pair

    1     CONTINUE   !- only here if they dont match (CYCLE)
        ENDDO       !-- loop the trial facets

  22    CONTINUE    ! (just an EXIT)


c---------- 3: now record whether it was internal/external -------------
c.. other possibilities:
c.. if a 4nq touches an 8nb, can 8nb be 'internal' but 4nq external ??

        IF (IFC_HIT.EQ.0) THEN         !---- an external facet ----
          N_EXT = N_EXT + 1
c          print*,ifc1,' is external'
          FACETS (3,IFC1) = 0
        ELSE                           !---- an internal pair ----
          N_INT = N_INT + 1
c          print*,ifc1,' <->',ifc_hit,' are a pair'
c          IFLIP = 1
c          IF (IMAT1.EQ.IMAT2) IFLIP = -1
          FACETS (3,IFC1) = IFC_hit        !- point at each other
          FACETS (3,IFC_HIT) = IFC1
c         .. maybe zap the INODE_LO(IFC_HIT) column too. ?
c 1-9-96 hang on folks; does facets(3,*) point to a facet or an element ?
c  what about FACETS(*,3) =  0  if external
c                           -ve if homogeneous
c                           +ve if heterogeneus (only draw these)
c note that I can also overload the IEL & IFACE with a +/- too.
c
        ENDIF
   44   CONTINUE       !-- CYCLE if this facet already has a toucher.
      ENDDO       !-- loop every facet

c      WRITE (*,*) '# external facets=', N_EXT
c      WRITE (*,*) '# internal facets=', N_INT

c     deallocate inode_lo(nfcets)
      RETURN
      END !SUBROUTINE FSTRIP3


C-----------------------------------------------------------------------
      SUBROUTINE DEL_INT_FACETS (FACETS, NFCETS) 
c     
c     This will contract the FACETS list to leave only those which 
c     have no 'toucher' (ie the 3rd number is zero) 
C     FACETS Data as:  Element #, Facet # (1-6), 'touching' facet #
c        DJK 26-8-94 
c
c     - cf a routine that pushes the internals to the top of the list
C       and the externals to the bottom.
c     1-9-96 note there are several levels of deletion:
c        none : nothing changes
c        all  : zap all facets
c        internals: all 'internal' facets; leaves just the outer shell
c        mat_internals: just the facets that touch others of the same IMAT
c        back-faces   : those that face away from the viewer
c        zero_area    : those reduced to just a line or a point.
c 
        INTEGER FACETS(3,*) 
        IC = 0 
        DO I=1,NFCETS 
          IF (FACETS(3,I).EQ.0) THEN 
            IC = IC + 1 
            DO J=1,3 
              FACETS(J,IC) = FACETS(J,I) 
            ENDDO 
          ENDIF 
        ENDDO 
        PRINT*,'>>',NFCETS,' FACETS reduced down to only', IC 
        NFCETS = IC 
        RETURN 
        END !SUBROUTINE DEL_INT_FACETS

C-----------------------------------------------------------------------
      SUBROUTINE Q_FACETS (NUMS,INUMS,NEL,NN,FACETS,IFCETS, NFCETS)
C
C     This returns info about the facets:
C..       Dan Kidger  15-2-98 
c   how many : total; external, iso-mat, hetero-mat
c    given GC I can calc the total surface area (cf areas of RINGS)
c.. can use for statistics eg surface:volume ratio for trabeculae
c   (cf ring info - eg normal vectors - hence apply BC based on this.)
c   - also note facets that are visible from a point .. eg .'FIXED_BASE'
c   * also report inter-group boundaries too.
C
      INTEGER NUMS(INUMS,*), FACETS(3,IFCETS)
      INTEGER cc(6,6), cc_max

      DO I=1,6
        DO J=1,6
          CC(I,J) = 0   !- face:face pairs
        ENDDO
      ENDDO
      N_EXT=0           !- external facets
      N_ISO=0           !- iso-material
      N_HETERO=0        !- hetero-material
      N_BROKEN=0        !- broken (non-paired) facets (even #?)
      IFACE_MAX=0       !- max (=1 in 2d, =6 in 3d)
      CC_MAX=0          !- max in the table
      DO IFC=1,NFCETS
        IEL1  = FACETS(1,IFC)
        IFACE1= FACETS(2,IFC)
        IFACE_MAX=max(iface_max,iface1)
        IFC2  = FACETS(3,IFC)
        IF (IFC2.EQ.0) THEN
          N_EXT=N_EXT+1
        ELSE
          IEL2= FACETS(1,IFC2)
          CALL GET_EL_IMAT (NUMS,INUMS,IEL1, IMAT1)
          CALL GET_EL_IMAT (NUMS,INUMS,IEL2, IMAT2)
          IF (IMAT1.EQ.IMAT2) THEN
            N_ISO=N_ISO+1
          ELSE
            N_HETERO = N_HETERO+1
          ENDIF
C------ corrolation ------
          IFACE2= FACETS(2,IFC2)
          if (iface1.le.6.and.iface2.le.6) then
            cc (iface1,iface2) = cc(iface1,iface2) +1
            cc_max=max(cc_max,cc(iface1,iface2))
          endif
C------ validity ------
          IFC3= FACETS(3,IFC2)
          IF (IFC3.NE.IFC) N_BROKEN=N_BROKEN+1
        ENDIF

      ENDDO

C------ write summary ------
      WRITE(*,'(i7,A,i7,A,2i7,a,f6.2,a)') 
     & nfcets,' Facets: ', n_ext,' External and',
     & n_iso,n_hetero,' ==,/= materials'
     &, real (n_ext)/real(nfcets)*100.,'%'
      IF (N_BROKEN.GT.0) 
     & WRITE(*,'(A,I7,A)') '**',n_broken,' facets are mis-matched!'
C------ corrolation ------
      DO I=1,IFACE_MAX
        WRITE (*,'(6i7)')   (CC(I,J), J=1,I)
      ENDDO

      RETURN
      END !SUBROUTINE Q_FACETS

C-----------------------------------------------------------------------
      SUBROUTINE Q_RINGS (RINGS,IRINGS)
C
C     This writes statistics of the RINGS structure
C       Dan Kidger 26-2-98
C     for example: total, max&min lengths, 
C      # of each material; if >1 then disjoint (or 3d)
C
      INTEGER RINGS(*)
      RETURN
      END !SUBROUTINE Q_RINGS


C-----------------------------------------------------------------------
      SUBROUTINE R_DEL_FACETS (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL,P,
     &       FACETS,IFCETS,NFCETS)
c
c       Header to DEL_FACETS: removes facets from the 'draw-list' 
c       according to various conditions.
c          Dan Kidger 12-7-99
c       eg:
c         *DELETE_FACETS
c         ">="
c
c
c     Where:
c      CODE = one or more of: (one per line)
c         "ext" deletes all facets that have no toucher ie. 'external'.
c    *    "=="  deletes facet pairs where IMAT=JMAT
c         ">"   deletes IMAT if IMAT>JMAT, ie. leaves the lower numbered
c                of the pair
c    *    "<"   deletes IMAT if IMAT<JMAT, ie. leaves the higher numbered
c                of the pair (good for removing all the 'soil'
c         "/="  (not useful)?
c   .. hence <= that will remove imat if it =jmat, or if it is less than JMAT?
c
      INTEGER NUMS (INUMS,*)      ! elements
     &     ,FACETS (3,*), P(*)
      REAL      GC (IGC,*)        ! nodal coords
      character code*8
      IC = 0
c     DO KK=1, 999 999

c       print*,'in r_del_facets'
    1   READ (IO,*,IOSTAT=IOS)  code
c       print*,'code=',code,' ios=',ios
        CALL IN_TEST (IO,IOS,*1,*999)
        CALL DEL_FACETS (GC,IGC,NDIM,NN,NUMS,INUMS,NEL,P,
     &       FACETS,IFCETS,NFCETS, code)

c     ENDDO
  999 continue
      END

C-----------------------------------------------------------------------
      SUBROUTINE DEL_FACETS (GC,IGC,NDIM,NN,NUMS,INUMS,NEL,P,
     &       FACETS,IFCETS,NFCETS, code)
c
c
c
      INTEGER NUMS (INUMS,*)      ! elements
     &     ,FACETS (3,*), P(*)
      REAL      GC (IGC,*)        ! nodal coords
      character code*8

      DO I=1,NFCETS 
c--- 1: pick up IMAT and JMAT ---
        IEL = FACETS(1,I)
        CALL GET_EL_IMAT (NUMS,INUMS,IEL, IMAT)
        ITOUCH = FACETS(3,I)      !- -ve if toucher is the same IMAT
        IF (ITOUCH.le.0) THEN
          jmat = -12345
        ELSE
          IEL2   = FACETS(1,ITOUCH)   
          CALL GET_EL_IMAT (NUMS,INUMS,IEL2, JMAT)
        ENDIF      

c--- 2: apply the criteria ---
      IF (    (CODE.EQ.'=='.AND. IMAT.EQ.JMAT)
     &  .OR.  (CODE.EQ.'>='.AND. IMAT.GE.JMAT)
     &  .OR.  (CODE.EQ.'<='.AND. IMAT.LE.JMAT)
     &  .OR.  (CODE.EQ.'<' .AND. IMAT.LT.JMAT)
     &  .OR.  (CODE.EQ.'>' .AND. IMAT.GT.JMAT)
     &  .OR.  (CODE.EQ.'/='.AND. IMAT.NE.JMAT) ) THEN
c     also 'always' and 'never' but these are not useful?

c as we traverse the whole structure we must be able to still decode the
c first half of a killed pair as we look at the second.
c so set the face number (1:6) to zero ?

         FACETS(2,i) = 0

        ENDIF 
      ENDDO   !- loop all the facets
C--- 3: clean up the new structure ---
c. do we not also have to clean up column 3 ?
      IC = 0 
      DO I=1,NFCETS
        IF (FACETS(2,I).NE.0) THEN
          IC = IC + 1 
          DO J=1,3 
            FACETS(J,IC) = FACETS(J,I) 
          ENDDO 
        ENDIF
      ENDDO 

      PRINT*,'>>',NFCETS,' FACETS reduced down to only', IC 
      NFCETS = IC 
      RETURN 
      END !SUBROUTINE DEL_FACETS

C-----------------------------------------------------------------------
      SUBROUTINE DEL_INT_MAT_FACETS (FACETS, NFCETS) 
c     
c     This will contract the FACETS list to leave only which 
c     touch facets of another material,or are external 
C     FACETS Data as:  Element #, Facet # (1-6), 'touching' facet #
c     .. note that FACETS(*,3) in F90 would be a pointer so dont 
c      overload too much. However FACETS(*,2) only uses 3 bits, hence
C      use the other 13 bits to flag the 'nature' of the facet: 
c      int/ext,  imat/=,  'to_draw', 'marked' (for munging)
c         DJK (orig.26-8-94) 1-9-96
c
        INTEGER FACETS(3,*) 
        IC = 0 
        DO I=1,NFCETS 
          ITOUCH = FACETS(3,I)      !- -ve if toucher is the same IMAT
c         IF (ITOUCH.ne.0) THEN              
          IF (ITOUCH.ge.0) THEN       ! external are 0; different IMAT are +ve

c         IEL2   = FACETS(1,ITOUCH)   
c         CALL GET_EL_IMAT (NUMS,INUMS,IEL2, IMAT2)
c         IF (FACETS(3,I).EQ.0) THEN 
c         IF (IMAT2.ne.IMAT) THEN 
            IC = IC + 1 
            DO J=1,3 
              FACETS(J,IC) = FACETS(J,I) 
            ENDDO 
          ENDIF 
        ENDDO 
        PRINT*,'>>',NFCETS,' FACETS reduced down to only', IC 
        NFCETS = IC 
        RETURN 
        END !SUBROUTINE DEL_INT_MAT_FACETS

C-----------------------------------------------------------------------
      SUBROUTINE R_BORE_MATS (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL,P,
     &       FACETS,IFCETS,NFCETS)
C
c       Header to BORE_MATS: gives a row of elements incremental 
c       element numbers
c          Dan Kidger 9-9-97
c
C     Where:
C      IMAT = Which elements to build on.
C      IDIR = which side (1->6)   ?should there be 1->4 in 2d ?)
C      IMAT_DELTA = the *increment* of IMAT, typicaly= +1
c    ( JMAT = the allowable material to bore into (usu.=230964), can be 
c      set to avoid boring into retaining walls etc.
c      cf a CHANGE_MATERIALS before and after to disable some ?
c
      INTEGER NUMS (INUMS,*)      ! elements
     &     ,FACETS (3,*), P(*)
      REAL      GC (IGC,*)        ! nodal coords

      IC = 0
      DO KK=1, 999 999

    1   READ (IO,*,IOSTAT=IOS)  nbands, IMAT,IDIR,IMAT_DELTA !, JMAT  
        CALL IN_TEST (IO,IOS,*1,*999)

        IC = IC + 1
        JMAT = 230964    !- do all elements
        N_NEW_TOT=0
        DO I=1,NBANDS
          CALL BORE_MATS (GC,IGC,NDIM,NN,NUMS,INUMS,NEL,P,
     &       FACETS,IFCETS,NFCETS, IMAT,IDIR,IMAT_DELTA,JMAT, N_NEW)
          IMAT = IMAT + IMAT_DELTA     !- ready for next time.
          N_NEW_TOT = N_NEW_TOT + N_NEW
        ENDDO
        PRINT*,'<>',IC,' :',N_NEW_TOT,' elements marked in'
     &    ,NBANDS,' sweeps'
      ENDDO
  999   CONTINUE
      RETURN
      END !SUBROUTINE R_BORE_MATS

C-----------------------------------------------------------------------
      SUBROUTINE BORE_MATS (GC,IGC,NDIM,NN,NUMS,INUMS,NEL,P,
     &       FACETS,IFCETS,NFCETS,  IMAT,IDIR,IMAT_DELTA,JMAT, N_NEW )
C
C     For a given material, this gives adjacent materials (in a 
c     particular direction), a new material number. Eg when creating 
c     meshes for 3D tunnelling.
c         Dan Kidger 9-9-97
c    Notes:
c      1. 3D only as we use FACETS :-(
c      2. cf my R_PRESSURE that also uses FACETS.
c      3. I could explicitly avoid FACETS, but this would make this O(n^2) ?
c          ie. for a given element, get the desired facet, then scan the 
C          rest of the mesh looking for it.
c          (as it is as have to search all FACETS looking for IMAT :-(
c      4.
c
      INTEGER NUMS (INUMS,*)      ! elements
     &          ,P (*)            ! (workspace)
     &        ,NUM (32)           ! nodes of an element
c    &         ,FS (32)           ! nodes on a facet
     &     ,FACETS (3,*)
      REAL      GC (IGC,*)        ! nodal coords (unused here)

      N_bases = 0    !- count the # of bases
      N_NEW = 0      !- the count the # of 'new' elements'

c------------ 0: cache the element IMATs ---------- 
c.. so we can be safe against 'propagating updates'
      DO IEL =1,NEL
        CALL GET_EL_IMAT (NUMS,INUMS,IEL,P(IEL)) 
      ENDDO

c------------- 0.1: Make sure we have got the facets stripped -------------
      IF (NFCETS.EQ.0) 
     &CALL FSTRIP3 (NUMS,INUMS,NEL,NN,FACETS,IFCETS,NFCETS,P) 

c------------ 1: loop all facets ---------- 
c.. in 2D we should have a table like FACETS but based on EDGES

      DO IFC=1,NFCETS
        IEL = FACETS(1,IFC)
c       CALL GET_EL_IMAT (NUMS,INUMS,IEL,IMAT2)    ! **1**
        IMAT2= P(IEL)                              ! **2**
        IF (IMAT2.NE.IMAT) GOTO 21    !- cos the wrong material

c------------ 2: pick which side to build on ---------- 
c  maybe be : a) Explicit (1..6)
c             b) Random 1..6
c             c) All '+Y facing' facets  (xXzZ too)
c             d) The 'Most +Y facing' facet  (xXzZ too)

        IFACE= IDIR
c---- patch to overload IDIR
c       if (IDIR.eq.97) then        !-- random direction :-) ---
C        IFACE = NINT (6.*RANDOM())    !- but '4' for tetrahedra
c       elseif (idir.gt.100.and.idir.le.109) then  !- 'xXyYzZ'
c         - loop the '6' facets, get its unit normal vector
c         - pick the one whose 'dot' product with <1,0,0> is the greatest
cc       if (IDIR.eq.120) then        !-- all directions.
Cc         IFACE = 230964             !
cc       endif

        IFACE2 = FACETS(2,IFC)
        IF (IDIR.ne.230964) THEN          !- wildcard builds on all 6 sides
         IF (IFACE2.ne.IFACE) GOTO 21  !- cos the wrong side
        ENDIF
        ITOUCH= FACETS(3,IFC)
        IF (ITOUCH.eq.0) GOTO 21      !- cos on the outer boundary

        CALL GET_ELEMENT   (NUMS,INUMS,IEL, NUM, NOD,NDIME,ITYPE,
     &                                            IMAT,IU1,IU2,IU3)
c       CALL GET_FACE (NUM,NOD,NDIME,ITYPE1,IFACE,FS,NN_F,NN_FT, 2)

c------------ 4: pick up its toucher -------
c.. and skip if we do not like its IMAT
        IEL2 = FACETS(1,ITOUCH)
        CALL GET_EL_IMAT (NUMS,INUMS,IEL2,IMAT3) 
        IF (IMAT3.EQ.IMAT2) GOTO 21    !- nothing to do

c------------ 5: mark this element with the new IMAT -------
c... or defer if IMAT_DELTA=0 to avoid fast propogation?
        N_NEW= N_NEW+1
        CALL PUT_EL_IMAT (NUMS,INUMS,IEL2,IMAT+IMAT_DELTA) 

   21   CONTINUE
      ENDDO
      PRINT*,'><',n_new,' Elements marked'
      RETURN
      END !SUBROUTINE BORE_MATS

C-----------------------------------------------------------------------
      SUBROUTINE FACETS_TO_ELEMENTS
     &             (GC,IGC,NDIM,NN,NUMS,INUMS,NEL,P, facets,nfcets)
C
C     This reduces the element database to just a set of elements that
C     comprise the external surfaces of the original mesh
C     Algorithm :
c     *  Loop and write facets to scratchfile and flag all 'used' nodes.
c     *  Shuffle down the coords, hence Reseq node list
c     *  Read and store the facet-list as elements (do node #s as we go along)
c     *
C
      INTEGER NUMS (INUMS,*)      ! elements
     &          ,P (*)            ! (workspace)
     &        ,NUM (32)           ! nodes of an element
     &         ,FS (32)           ! nodes on a facet
     &     ,FACETS (3,*)

      REAL      GC (IGC,*)        ! nodal coords

c------------------- get the reduced list of 'present' nodes -----------
c.. also  write out the set of 'new' elements 
c... hmm: we can't copy (like GC) 'cos they are not necessirily in 
c..  ascending order :-(

      OPEN (97,STATUS='SCRATCH',FORM='UNFORMATTED') !- to dump polygons
C      DO I=1,NN
C        P(I) = 0   !- mark all nodes as 'unfound'
C      ENDDO
      DO I=1,NFCETS
        IEL  = FACETS(1,I) 
        IFACE= FACETS(2,I) 
        CALL GET_ELEMENT   (NUMS,INUMS,IEL, NUM, NOD,NDIME,ITYPE,
     &                                            IMAT,IU1,IU2,IU3)
        CALL GET_FACE (NUM,NOD,NDIME,ITYPE1,IFACE,FS,NN_F,NN_FT, 2)
C        DO J=1,NN_F
C          P(FS(J)) = P(FS(J)) + 1    !- count occurences
C        ENDDO                        !- 'cos nice for debug
        ITYPE = 9    !- default = a 'general' (non-FE) polygon
        IF (NOD.EQ.3.OR.NOD.EQ.4.OR.NOD.EQ.6.OR.NOD.EQ.8.OR.NOD.EQ.9)
     &  ITYPE = 1    !- valid as a FE 
        WRITE (97)  2,NN_F,itype, (FS(J),J=1,NN_F),IMAT,IU1,IU2,IU3
      ENDDO

c------------------ now rebuild my table of elements -------------------
c.. won't they ALL be valid as 2D FE's ??
c.. this could easily be a subroutine 'READ_NUMS_UNFORMATTED' (but note P)
c.. note that now we do not remove the orphan nodes here (breaks disps)
C   - if needed do explicitly afterwards.
      REWIND(97)
      DO IEL=1,NFCETS
        READ (97) ndime,NOD,itype, (NUM(I),I=1,NOD),IMAT,iu1,iu2,iu3
        CALL PUT_ELEMENT (NUMS,INUMS,IEL, NUM, NOD,NDIME,ITYPE,
     &                      IMAT,IU1,IU2,IU3)
      ENDDO
      CLOSE(97)

c    optional step: Renumber the remianing nodes
c     CALL  DEL_O_NODES (IO,GC,IGC,NDIM,NN_NEW,NUMS,INUMS,NFECTS,P)

c.. option to control output (ie quiet option as in K_MESH.F)??
      WRITE(*,'(A,I5,A,I5)')
     &   '>> ',  NEL,' elements to ', NFCETS
c    &  ,'>> ',  NN, ' nodes to only ', NN_NEW
      NEL = NFCETS
c     NN = NN_NEW
      RETURN
      END !SUBROUTINE FACETS_TO_ELEMENTS

C-----------------------------------------------------------------------
      SUBROUTINE STRIP_EDGES (GC,IGC,NDIM,NN,NUMS,INUMS,NEL,
     &      FACETS,IFCETS,NFCETS,P,RINGS,IRINGS, IOP1,iop2)  
C
C  * * This strips the facets into a set of edge-polygons * *
C       Dan Kidger c.Sept 1994
C
C    Notes:
c     use IOP,IOP2= 1,1 normaly, but 0,1 before BARYCENTRIC
c      1-10-97  maybe I can call STRIP_FACETS if I haven't done it yet
c               (if NFCETS=0)

c    DATA formats:
C     FACETS Data as:  Element #, Facet # (1-6), 'touching' facet #
c      if IOP1 = 0 then all edge nodes are used
c      if IOP1 = 1 then co-linear nodes on an edge are removed  (nice)
c
c      if IOP2 = 0 then only the external boundary is used.
c      if IOP2 = 1 then *internal* mat:mat facets are used as well (usual)
c
C.... note STRUCTURE OF 'RINGS' :
c        1.  total length of data
c        2.  # rings
c        3.      # points in this ring  (maybe -ve if un-closed?)
c        4.      IMAT of this ring
c        5.      IPLANE of this ring
c        6.++    nodes on this ring
c        7.  ring #2 , (3.->6.) etc....
C
      REAL GC(IGC,NN)            !- coords of the nodes
      INTEGER IWORK
      PARAMETER (IWORK=500000)    !- for workspace  *10 to 5.e5 10-8-00
      PARAMETER (ILINES=IWORK)   ! upper limit is nfacets*4 ?
      INTEGER
     &     NUMS (INUMS,*)        !- The element steerings
     &  ,FACETS (3,IFCETS)       !- 3->7 so can store edge info too? (no)
     &   ,RINGS (IRINGS)         !- edge polygons (output)
     &   ,LINES (4,ILINES)       !- workspace for the edges :-(
     &       ,P (NFCETS)         !- workspace only: to flag 'done' facets
      LOGICAL MINIMAL, INTERNAL

c----------------- 0: initiation ---------------------
      MINIMAL  = (IOP1.EQ.1)
      INTERNAL = (IOP2.EQ.1)
      RINGS(1) = 2   !- just these 2 headers so far
      RINGS(2) = 0   !- No rings yet found

c------------- 1: Make sure we have got the facets stripped -------------
      IF (NFCETS.EQ.0) 
     &CALL FSTRIP3 (NUMS,INUMS,NEL,NN,FACETS,IFCETS,NFCETS,P) !* super-fast *

c------------------ 2: loop and identify each 'plane' -------------------
c.. just do 1 for now (?)
C.. Note, *only* handle facets that have a +ve IMAT
c.. cf. juggling FACETS to put the active at the top (so avoid P)
      IPLANE = 1    
      NP = 0
      DO I=1,NFCETS
        IF (FACETS(3,I).EQ.0 .OR. INTERNAL) THEN  !- just externals? 
          IEL   = FACETS(1,I)
          CALL GET_EL_IMAT (NUMS,INUMS,IEL,IMAT) 
          IF (IMAT.GE.1) THEN         !- only 'visible' (+ve) materials
            NP = NP + 1                      !(INTERNAL will override)
            P(NP) = I      
          ENDIF
        ENDIF
      ENDDO

c-----------3: get the edges (2 nodes+imat) then daisy-chain -----------
c     allocate (lines (4,ilines))    !- do here ?
      DO WHILE (NP.GT.0)
        CALL FACETS_TO_LINES  (GC,IGC,NDIM,NN,NUMS,INUMS,NEL
     &      ,FACETS,IFCETS,NFCETS,P,NP,LINES,ILINES, NLINES)
        DO WHILE (NLINES.gt.0)
          CALL LINES_TO_RINGS  (GC,IGC,NDIM,NN,LINES, ILINES,NLINES
     &    ,RINGS,IRINGS ,iplane,minimal)
        ENDDO
      ENDDO

c... just exit .. cf looping of all the 'planes'
      RETURN
      END !SUBROUTINE STRIP_EDGES 

C-----------------------------------------------------------------------
      SUBROUTINE FACETS_TO_LINES (GC,IGC,NDIM,NN,NUMS,INUMS,NEL
     &      ,FACETS,IFCETS,NFCETS,P,NP,LINES,ILINES, NLINES)
C
C    This extracts a set of lines into LINES from the facets in FACETS 
C    pointed to by P().
C       DJK 19-9-94
C  NOTES:
C     1. Only the first IMAT found is handled, therefore multiple
C        calls to this routine are needed to whittle away all of P().
c        ..perhaps sort wrt IMAT first ?
C     2. Maybe need an OPCODE to do ALL facets or just a certain IMAT
C     3. 22-9-94 now only uses facets that share the same normal :-)
C
      SAVE     !(so faster v mem size)
      PARAMETER (ICOORD=32)      !- max # nodes per element
      REAL   GC (IGC,NN)         !- coords of the nodes
     &   ,COORD (ICOORD,5)       !- coords of once elemnt
     &   ,DC0(5), DC(5)          !- normal vectors of base/test facets
      LOGICAL COPLANAR           !- if facets are co-planar (normals ==)
      INTEGER
     &     NUMS (INUMS,*)        !- The element steerings
     &  ,FACETS (3,IFCETS)       !- each facet's el#, side#, and 'toucher'
     &   ,LINES (4,ILINES)       !- workspace for the edges :-(
     &     ,NUM (ICOORD)         !- steering for one element
     &      ,FS (ICOORD)         !- nodes on a facet
     &       ,P (NP)             !- workspace only: to flag 'done' facets

      TOL = COS (15. * 3.14159265/180. )    ! coplanar tolerance (or 2 deg ?)

      NLINES = 0               !- no lines stored yet
      IMAT_MY = 0              !- no material found yet

c.. 15-2-98 In a large mesh I want to eliminate as much as possible early on
c   ie. prestore the normal vectors of each facet
c   and perhaps sort wrt this ?

c--------------------- 1: loop the facets ------------------------------
      DO I = 1,NP
        IFC = P(I)
        IEL   = FACETS(1,IFC)
        IFACE = ABS(FACETS(2,IFC))            ! (what was -ve for?)
        ITOUCH= FACETS(3,IFC)
        IF (ITOUCH.GT.0) GOTO 100             !- *cycle* if not external
        CALL GET_EL_IMAT (NUMS,INUMS,IEL,IMAT)     !- 'tis faster this way
        IF (IMAT_MY.EQ.0) IMAT_MY = IMAT      !- mark first time
        IF (IMAT.NE.IMAT_MY) GOTO 100         !- *cycle* if wrong IMAT

c---------- 2: Get this element's NUMS and turn into EDGES -------------
        CALL GET_ELEMENT (NUMS,INUMS,IEL, NUM, NOD,NDIME,ITYPE
     &     ,IMAT,IU1,IU2,IU3)
        CALL GET_FACE (NUM,NOD,NDIME,ITYPE,IFACE,FS,NN_F,NN_FT, 2) 

c--------- 3: Now test its normal vector and skip if >5 deg out --------
        IF (NDIM.LT.3) THEN    !- speed-up cos 2d is always coplanar
          COPLANAR = .TRUE.
        ELSE
        CALL GET_COORD (FS,NN_F,NDIM,GC,IGC,COORD,ICOORD)    !- coords
        CALL GET_DC_NORMAL (COORD,ICOORD,NN_F,NDIM, DC )     !- normal vector

        IF (NLINES.EQ.0) THEN      !- ie. the very first facet
          DO K=1,NDIM
            DC0(K) = DC(K)      !- remember the very first facet
          ENDDO
          COPLANAR = .TRUE.     !- ie. we must store the first facet
        ELSE           !-----  for subsequent edges test if co-planar
          t = 0
          DO J=1,NDIM
            T = T + DC(J) * DC0(J)       !- dot-product = cos(theta)
          ENDDO
          COPLANAR= (T .GT. TOL)         !- if perfect then t=1.
        ENDIF
        ENDIF    !-- speed-up for 2D meshes
        IF (.NOT.COPLANAR) GOTO 100      !- irrelevant facet

c--------------------- loop the edges of the facet ---------------------
        DO IBIT = 1,NN_F
          IFR = FS (IBIT)                !- a pair of
          ITO = FS (MOD(IBIT,NN_F)+1)    !- edge nodes

C-------------------- test all edges for a hit -------------------------
c.. what about maintaining an ordered list, then binary chop?
          DO J = 1,NLINES
            IF (IFR .EQ.LINES(2,J)) THEN     !(note swap of 1 and 2)
            IF (ITO .EQ.LINES(1,J)) THEN       
            IF (IMAT.EQ.LINES(3,J)) THEN       
            IF (IEL .NE.LINES(4,J)) THEN         !- got a 'hit'
              DO K=1,4
                LINES (K,J) =  LINES (K,NLINES)  !- shrink the list
              ENDDO
              NLINES = NLINES - 1                
              GOTO 123              !- jump out to the next edge-piece
            ENDIF
            ENDIF
            ENDIF
            ENDIF
          ENDDO
          NLINES = NLINES + 1      !-------- no 'hit' so add  ------------
          IF (NLINES.gt.ILINES) CALL MYERROR (3,
     &    'not enough bufferspace in FACETS_TO_LINES')
          LINES (1,NLINES) = IFR    !(to the end of the table)
          LINES (2,NLINES) = ITO
          LINES (3,NLINES) = IMAT
          LINES (4,NLINES) = IEL
  123     CONTINUE  !- 'jump out' after a 'hit'
        ENDDO     !-- loop the edge-bits of each facet

        P(I) = 0            !- mark as now done
 100    CONTINUE         !- 'cycle' if irrelevant Facet
      ENDDO           !- loop FACETS

c--------------- remove the facets that we have dealt with -----------
      NP_ORIG = NP
      NP = 0
      DO I = 1,NP_ORIG
        IF (P(I).NE.0) THEN
          NP = NP+1
          P(NP) = P(I)   !- copy down
        ENDIF
      ENDDO
      RETURN
      END !SUBROUTINE FACETS_TO_LINES

C-----------------------------------------------------------------------
      SUBROUTINE LINES_TO_RINGS  (GC,IGC,NDIM,NN,LINES, ILINES,NLINES
     &   ,RINGS,IRINGS ,iplane, minimal)                
c
c     This simply (!) glues the line segments in LINES together to make
c     polygons that are stored in RINGS
c          DJK 20-9-94
c  NOTES: 
c     1. RINGS(1) = total length; RINGS(2) = # rings
c     2. IPLANE is this PLANE # (eg.1) that is stored with each ring
c     3. 20-9-94 now uses GC to collapse out co-linear nodes
c     4. We take the direction of the FIRST edge in a 'run' and
c        add lines until it deviates by > .3 deg
c
      REAL GC(IGC,NN)            !- coords of the nodes
     &  ,dc0(5),dc1(5), dc2(5)   !- direction cosines of segments
      LOGICAL COLINEAR           !- if the new edge is colinear with the last
     &         ,MINIMAL          !- flag to remove 'co-linear' nodes
      INTEGER
     &    LINES (4,ILINES)       !- workspace for the edges :-(
     &   ,RINGS (IRINGS)         !- edge polygons (output)

      TOL = COS (15. * 3.14159265/180. )    ! colinear tolerance = 15 deg 

C------- 1: get the properties of the first edge in this ring ----------
      IFR  = LINES (1,1)     !- the first node
      ITO  = IFR             !-  .. so force us to find it straight away
      IMAT = LINES (3,1)     !- the material type of this ring
      IEL  = LINES (3,1)     !- element # of this line (unused)
      NPTS = 0               !- # points in this ring 
      IC = RINGS(1) +3       !- start loc. to save the chain after

c-------- 2: Loop back point after we find each edge segment -----------
  100 CONTINUE    
      DO I=1,NLINES
        IF (LINES(1,I).EQ.ITO ) THEN       
        IF (LINES(3,I).EQ.IMAT) THEN       !- found a seqment

c-------------------- 3: Found an edge segment -------------------------
          ITO = LINES(2,I)          !- next we will look for point 'B'

          CALL GET_DC (GC(1,lines(1,i))             !- get direction of
     &               , GC(1,lines(2,i)),NDIM,dc2 )  !- this segment

          IF (NPTS.EQ.0) THEN      !- ie, the very first edge piece
            DO K=1,NDIM
              DC0(K) = DC2(K)      !- remember the very first edge  
            ENDDO
            COLINEAR = .FALSE.     !- ie. we must store the first edge
          ELSE           !-----  for subsequent edges test if co-linear
            t = 0
            DO J=1,NDIM
              T = T + DC1(J) * DC2(J)       !- dot-product = cos(theta)
            ENDDO
            COLINEAR= (T .GT. TOL)          !- if perfect then t=1.
          ENDIF
          IF (MINIMAL.AND.COLINEAR) THEN  !------------- co-linear -------------
c           ... Nothing to do here .. we just update the 'look-for' 
c           point (see 10 lines back) .. and keep going .. 
          ELSE      !------------ if inclined to the last ------------
            NPTS = NPTS + 1                  !- # of points (in this poly)
            IC = IC + 1                      !- & store address both++
            RINGS(IC) = LINES(1,I)           !- store point 'A'
            DO K=1,NDIM
              DC1(K) = DC2(K)                !- update the direction
            ENDDO
          ENDIF
          DO K=1,4           !- always shrink the list after a 'hit'
            LINES (K,I) =  LINES (K,NLINES)  
          ENDDO
          NLINES = NLINES - 1

          IF (ITO.EQ.IFR ) GOTO 99     !- 'EXIT' 'cos now closed
          GOTO 100   !- scan the whole list for the next point
        ENDIF
        ENDIF
      ENDDO      !- loop the remaining edge pieces
      PRINT*,'** internal error: RING did not close !'
      STOP

c---------------- 5: finish-off this closed ring -----------------------
c... hmm if we are now 'closed' then should test against the 1st line
c... in the polygon.. if co-linear then the 1st point should be replaced
c... with rings(ic) hence ic-- & npts--
c... at this point dc1=dc2 = dc of the last edge
c... dc0 = dc of the first edge

   99 CONTINUE

C------- 6: collapse out the first node if co-linear with last ---------
      t = 0
      DO J=1,NDIM
        T = T + DC2(J) * DC0(J)       !- dot-product = cos(theta)
      ENDDO
      COLINEAR= (T .GT. TOL)          !- if perfect then t=1.
      IF (MINIMAL.AND.COLINEAR) THEN
        RINGS (RINGS(1)+4) = RINGS(IC)    !- move last to first point:-)
        IC = IC - 1
        NPTS = NPTS - 1
      ENDIF

c------------ 7: Update the = rings, total length, etc. ----------------
      RINGS(RINGS(1)+1) = NPTS      !- # points in this ring
      RINGS(RINGS(1)+2) = IMAT      !- material type of this ring
      RINGS(RINGS(1)+3) = IPLANE    !- 'plane' # of this ring

      RINGS(1) = IC                 !- total length of the array
      RINGS(2) = RINGS(2) + 1       !- # rings
      RETURN
      END !SUBROUTINE LINES_TO_RINGS 

c-----------------------------------------------------------------------
      SUBROUTINE GET_DC (PNT1, PNT2, NDIM, DC ) 
c
c     Given the two points PNT1() and PNT2(), this simply returns
c     the direction cosines of the vector between them
c          DJK 20-9-94
c
      REAL PNT1(NDIM), PNT2(NDIM), DC(NDIM)

      TOTAL = 0.
      DO I=1,NDIM
        DC (I) = PNT2(I) - PNT1(I)
        TOTAL = TOTAL + DC(I)**2
      ENDDO
      TOTAL = SQRT(TOTAL)
      IF (TOTAL .LE. .00001) THEN  !---- if nodes are coincident
        DC (1) = 1.               !- just make an arbitary direction
      ELSE                        ! (cos all DC's =0)
        DO I=1,NDIM
          DC (I) = DC(I) / TOTAL
        ENDDO
      ENDIF
      RETURN
      END !SUBROUTINE GET_DC

c-----------------------------------------------------------------------
      SUBROUTINE GET_DC_NORMAL (C,ICOORD,NOD, NDIM, DC ) 
c
c     Given the nodes of an element in COORD, this simply returns
c     the direction cosines of the 'best-fit' normal vector
c     .. hence used to calc the shade of a facet.
c     .. or used to determine which facets lie on the same plane (strip_edges)
c        DJK 22-9-94
c
      REAL C(ICOORD,ndim), dc(ndim)

      TOTAL = 0.
      DO I=1,NDIM
        DC (I) = 0.       !- zap first
      ENDDO
      IF (NDIM.NE.3) THEN
        DC(1) = 1.           !- force a dummy direction for non-3D
        RETURN
      ENDIF  

      DO I=1,NOD
          J  = MOD (I,NOD) + 1    !(cyclic I+1)
          DC(1) = DC(1) + (C(I,3)+C(J,3)) * (C(J,2)-C(I,2)) /2.
          DC(2) = DC(2) + (C(I,1)+C(J,1)) * (C(J,3)-C(I,3)) /2.
          DC(3) = DC(3) + (C(I,2)+C(J,2)) * (C(J,1)-C(I,1)) /2.
      ENDDO
      TOTAL = ABS (DC(1)**2 + DC(2)**2 + DC(3)**2)   !(3d only ??)
      TOTAL = SQRT(TOTAL)

      IF (TOTAL .LE. .00001) THEN  !---- if nodes are coincident
        DC (1) = 1.               !- just make an arbitary direction
      ELSE                        ! (cos all DC's =0)
        DO I=1,NDIM
          DC (I) = DC(I) / TOTAL
        ENDDO
      ENDIF
      RETURN
      END !SUBROUTINE GET_DC_NORMAL

C-----------------------------------------------------------------------
c------------------------------------------------------
c   GENERALISED MESH PLOTTING:
c      12-9-97
c   Notes:
c     1/ Parse like *MAT_PROPS
c     2/ 2d v.3D : I would like to be able to do 3D; so default view
c        is down z, but can modify, by say VIEW=45.,30.
c     3/ Hidden-line: need to depth sort the facets (like Danplot)
c        so loop and plot these (rather than loop elements)
c     4/ proceedure:
c          CALL FSTRIP (always?)
c          GET_MESH_RANGE hence scale factor & centroid
c          CAMERA to get rotation matrices
c          CALL DEPTH_SORT_FACETS
c          draw preambles:
c             picture background, anything else?
c          loop facets_to_draw:
c            loop options: 
c               draw each.
c            endloop
c          endloop
c          draw additionals:  
c            outer boundary if selected (so STRIP_EDGES too?)
c            contour legend, title, window frame, etc.
c
c     5/ 
c

c... to plot *any* mesh features: elems, fill, node#, boundary, etc.
c..  It should plot by layers. (but 3d ?)
c   ( calc scale factors first)
c     ELSEIF (KEYWORD.EQ.'*DRAW') THEN
c         CALL GET_KEYWORD_ARGS ()   !- try this?
c         -- returns the rest of the line after the comma,
c            eg *DRAW,BOUNDARY=1,contour=sx    (cf ABAQUS POST)
c.. eg:
c  *DRAW
c    dest=ps
c    elements=red
c    Boundary=WHITE
c    nodes=0x7fefef
c    node#=6
c
C-----------------------------------------------------------------------
      SUBROUTINE DRAW ()
C
C     This draws (almost) anything
C       Dan Kidger 19-5-98 
c    Notes:
c     1/ Best in F90 as most of the arguments can be OPTIONAL
c
c
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE PLOT_MESH_BOUNDARY (GC,IGC,NDIM,NN,RINGS)
C
C     This will do a 2D plot of the mesh boundary (*STRIP_EDGES)
c     based on PLOT_MESH for just the (2d) elements of a mesh.
C     -> for full plotting, see the DANPLOT package <-
C
C     22-7-93 Added wait for a keypress for 20 secs.. else exit.
C     .. time I had a DISPL_MESH_PLOT routine too ?? :-)
c     .. also *PLOT_FACETS ? (with/without bpc, hlr, & cameras.)
c     I would prefer a *PLOT, BOUNDARY,MESH,NODES, DISPS
C      so 'PLOT' goes into graphics (& calcs TR[]), then loop
c      and call up daughter routines (in order) to draw
c      - this is a bit like ANSYS plot ?
C       can set colours too?
C
      INTEGER RINGS(*)
      REAL GC(IGC,*)
      real gc_min(5),gc_max(5), diag
c------------------------- statement functions -------------------------
c or just construct TR[]
      fx(i) =           IRESX * (FACT+R*(GC(1,I)-XMIN) /DATAX)
      fy(i) =   IRESY - IRESX * (FACT+R*(GC(2,I)-YMIN) /DATAX) 

c------------------------ set graphics mode size -----------------------
c     print*,'press <CR> to plot the mesh (then <CR> again to continue)'
c     read*
      ICOL = 0
      CALL INTO_VGA (ICOL,IRESX,IRESY)  !-or other device
      AR = REAL(IRESX) / REAL(IRESY)

C--------------------- maxima and minima -------------------------------
      CALL GET_MESH_RANGE (GC,IGC, NN,NDIM, GC_MIN,GC_MAX,DIAG)
      xmin=gc_min(1)
      ymin=gc_min(2)
C---------------------- normalize the data -----------------------------
      FACT = 0.05         !- shrink factor
      R = (1.-2.*FACT)    !- resulting size
c     DATAX = MAX ((XMAX - XMIN),(YMAX - YMIN)*AR)   !- scale factor
      DATAX = MAX (GC_MAX(1) - gc_MIN(1),
     &            (GC_MAX(2) - GC_MIN(1))*ar )        !- scale factor

C----------------------- plot the mesh-rings ---------------------------
c.. also maybe 'colour-in' the elements themselves in their material 
c.. colours ?  .. or restore text/grahics screen on exit ?

      ic = 3
      DO IRING = 1,rings(2)
        NEN    = RINGS(ic)
        IMAT   = RINGS(IC+1)
        IPLANE = RINGS(IC+2)
        IC = IC + 2
c.. contast the FX(),FY() approach to a TR[] approach
c.. note I can also FILL them as well
        DO J=1,NEN
          K = MOD(J,NEN)+1
          I1 = RINGS(IC+J)
          I2 = RINGS(IC+K)
          CALL DRAW_A_LINE ( FX(i1),FY(I1),FX(I2),FY(I2),IMAT+0)
        ENDDO
c       print*,' imat=', imat,' #points=', nen
        IC = IC + NEN+1   !- update to the next polygon ring
      ENDDO
C------------------- plot the nodes ------------------------
      DO I=1,NN
        CALL DRAW_A_LINE (FX(I)-1,FY(I)+1,FX(I)+1,FY(I)-1,1)
      ENDDO

c..... <press any key to continue>
c     read*
      CALL INTO_TEXT()
      RETURN
      END !SUBROUTINE PLOT_MESH_BOUNDARY


C-----------------------------------------------------------------------
      SUBROUTINE MARK_BOUNDARIES (GC,IGC,NDIM,NN,NUMS,INUMS,NEL,
     &      FACETS,IFCETS,NFCETS,P,RINGS,IRINGS)
c
c     This flags in P those nodes that are on a 'boundary'.
c     This is the count of how many times a node appears in FACETS(3d), or RINGS(2d)
c     This is usualy called just before *BARYCENTRIC
c        Dan Kidger 13-9-97
c  Notes:
c     1. In 2d we need to STRIP_EDGES and mark those instead
c     2. Do we want to differentiate between internal and exteranl boundaries? - eg for plotting?
!     3. We could do more to differentiate the different cases
!       e.g.  mark mat:mat nodes as code=2 (touching facet is different imat)
!            outer faces as code=1 (where no touching facte)
!     to do this, we need some workspace
c
      REAL GC(IGC,NN)            !- coords of the nodes
      INTEGER
     &     NUMS (INUMS,*)        !- The element steerings
     &  ,FACETS (3,IFCETS)       !- 3->7 so can store edge info too? (no)
     &   ,RINGS (IRINGS)         !- edge polygons (output)
     &       ,P (NN)             !- output 
     &     ,NUM (99)             !- steering for one element
     &     ,FS  (19)             !- nodes on an element's facet

      IF (NDIM.EQ.1) THEN
c       Is this just xmin and xmax?
C       No. because we might have multiple disjointed sets.
        call myerror (2,'Cannot do 1d yet (MARK_BOUNDARIES)')

      ELSEIF (NDIM.EQ.2) THEN
        IOP1 = 0       !- 0= do all (including colinear) nodes
        IOP2 = 1       !- 1= do mat:mat boundaries too.
        CALL STRIP_EDGES (GC,IGC,NDIM,NN,NUMS,INUMS,NEL
     &      ,FACETS,IFCETS,NFCETS,P,RINGS,IRINGS,iop1, iop2)
        CALL MARK_RING_NODES (RINGS, P,NN)

c------- do 3d here ? ------
      ELSEIF (NDIM.EQ.3) THEN

C----- 1: zero the counts ------
        DO I=1, NN
          P(I) = 0
        ENDDO

c----- 2: loop the facets and mark -----
        DO IFC = 1,NFCETS
          iflavour=0             !- assume internal node
          IEL   = FACETS(1,IFC)
          IFACE = FACETS(2,IFC)
          ITOUCH= FACETS(3,IFC)
          IF (ITOUCH.eq.0) THEN    !- check internal boundaries too.
            iflavour=1               !- a node on the outer boundary
          else
            IEL2= FACETS(1,ITOUCH)
            CALL GET_EL_IMAT (NUMS,INUMS,IEL1,IMAT1)  
            CALL GET_EL_IMAT (NUMS,INUMS,IEL2,IMAT2)  
            IF (IMAT1.NE.IMAT2) goto 21  !- CYCLE cos not a boundary
!  whyis it not EQ in the line above ?
            iflavour=2               ! on a mat:mat boundary ?
! note need to AND flavour cos might be mat:mat and on the surface
! may consider normals too?
! cf strip_edges too and loop and AND those bits also.
! so 4 = on a outer 'sharp' edge (on 2 rings)
!    8= on a corner (on 3+ rings) ?
!  hence a flavour=5 do not allow to be smoothed (edge), but =1 (coplanar)should be
!   
          ENDIF
          CALL GET_ELEMENT (NUMS,INUMS,IEL, NUM, NOD,NDIME,ITYPE
     &     ,IMAT,IU1,IU2,IU3)
          CALL GET_FACE (NUM,NOD,NDIME,ITYPE,IFACE,FS,NN_F,NN_FT, 2) 
          DO J=1,NN_FT
            P(FS(J)) = P(FS(J)) + 1    !- mark occurencies
          ENDDO  

   21     CONTINUE
        ENDDO
c--------------------------------------------------------
      ELSEIF (NDIM.GE.4) THEN
        call myerror (2,'Cannot handle NDIM>3 yet (MARK_BOUNDARIES)')
      ENDIF

      RETURN
      END !SUBROUTINE MARK_BOUNDARIES

C-----------------------------------------------------------------------
      SUBROUTINE MARK_RING_NODES (RINGS, P,NN)
C
C     This flags which nodes are on mesh boundaries
c      Daughter of MARK_BOUNDARIES
c     This is done by simply counting the number of times a node appears
c     in the RINGS structure.
C       c. Dan Kidger 2-7-97
C
C     Notes:
C       For Internal nodes:    P() = 0
C       For Outer boundaries:  P() = 1 
C       For mat:mat boundaries P() = 2 
C       For mat:mat corners    P() = 4 (or 3)
C       If P(3) then the internal nodes is probably at a 'T'
C
      INTEGER RINGS(*), P(*)

C----- 1: zero the counts ------
      DO I=1, NN
        P(I) = 0
      ENDDO

c----- 2: loop the rings -----
      ic = 3               !- first ring starts here
      DO IRING = 1,rings(2)
        NEN    = RINGS(ic)
c       IMAT   = RINGS(IC+1)
c       IPLANE = RINGS(IC+2)
        IC = IC + 2
        DO J=1,NEN
          INODE = RINGS(IC+J)
          P(INODE) = P(INODE) + 1
        ENDDO
        IC = IC + NEN+1   !- update to the next polygon ring
      ENDDO
      RETURN
      END !SUBROUTINE MARK_RING_NODES

C-----------------------------------------------------------------------
      SUBROUTINE WRITE_RINGS (IO,GC,IGC,NDIM,NN,RINGS)
C
C     This writes out the mesh bounding 'rings'
C     - currently the format is in 'DANSYS-ADVANCE' format 
C     - DXF and others are also possible.
C      Dan Kidger 24-2-97
C
      INTEGER RINGS(*)
      REAL GC(IGC,*)

      ic = 3               !- first ring starts here
      WRITE (IO,'(I3/,f12.4/,f8.4)') 0, 1., 6.
      WRITE (IO,'(I3,A)') rings(2),'   !- number of objects'
      DO IRING = 1,rings(2)
        NEN    = RINGS(ic)
        IMAT   = RINGS(IC+1)
        IPLANE = RINGS(IC+2)
        IC = IC + 2
        WRITE (IO,'(I3,I5,A)') 1, IMAT,'  !- #sections, IMAT'
        WRITE (IO,'(A7,i5,20g12.4/,(t13,20g12.4)  )') 
     &   '''3nl''', NEN+1 
     &   ,((GC(K,RINGS(IC+J)),K=1,ndim),J=1,NEN)
     &   , (GC(K,RINGS(IC+1)),K=1,ndim)

        IC = IC + NEN+1   !- update to the next polygon ring
      ENDDO
      RETURN
      END !SUBROUTINE WRITE_RINGS

c-----------------------------------------------------------------------
c-----------------------------------------------------------------------
*      END MODULE M_FACETS

c-----------------------------------------------------------------------
c   3: Routines for reading in Displacements
c-----------------------------------------------------------------------
      SUBROUTINE R_HEIGHTS (IO,DISPS,IDISPS,nodof,nn)
c
c     This reads a set of spot heights (eg. BNFL data)
c     DJK 22-1-98
c
c     It is assumed that these are in ascending node order, normaly 1:NN
c     heights stored as (0., 0., height)
c
      real disps (idisps,nn)
      CHARACTER LINE*255

c     print*,' reading spot height values..'
      NODOF = 3     !- force this here ?
      DO I=1,NN
        DO J=1,NODOF        !- make all disps zero , except those 
          DISPS(J,I) = 0.   !- defined below.
        ENDDO
      ENDDO

c------------ read the 'z' disp of each point ---------------
      INODE = 0
      DO Iline=1, 99999
c      print*,'line..#',iline
    1   READ(IO,'(a)',iostat=ios) LINE
        CALL IN_TEST (IO,IOS,*1,*999)
        CALL COUNT_TOKENS (LINE, NTOK)
        if (line(1:1).eq.'*') then
          backspace(io)    !- oops
          goto 999    !- all done
        endif
        READ (line,*) (disps(3,ii),ii=inode+1,inode+ntok)
        inode = inode + ntok
      ENDDO

  999 continue
      hmin = 1.e22
      hmax = -1.e22
      DO I=1,NN
        hmin = min(hmin,disps(3,i))
        hmax = max(hmax,disps(3,i))
      ENDDO
      print*,'>>', inode,' nodes read in', iline-1,
     & ' lines, min,max=', hmin,hmax

      RETURN
      END !subroutine r_heights

c-----------------------------------------------------------------------
c   4: Routines for handling the Stress (and Strain) database.
c-----------------------------------------------------------------------
c        S T R E S S E S   - 
c
c    This module handles all I/O and 'database operations on the 
c    Stresses (and strains) of the FE mesh. 
c
c    Data Structure:
c       In Fortran 77:a long-vector for storage, with an integer vector
c    that points to the start of the stress-record for each element.
c    In F90 we can have an array of f90-pointers each then as a record
c    malloc'ed to it (cf a daisy-chain of records)
c
c     R_STRESSES  -  Keyword=based reading - flags for #GP's etc.
c     WR_STRESSES -  Writes records as IEL,NGP,(code), stresses...#
c                     code = record type (stress/strain/ sx/s1/sigm..)
c
c     GET_STRESS  -  abstract an elements stresses
c     PUT_STRESS  -  re-store a stress record.
c
c     INIT_STRESSES  - zap all the pointers
c     CLOSE_STRESSES - (f90 only) deallocate the whole table

c-----------------------------------------------------------------------
c      SUBROUTINE NUL_STRESSES (STRESSES,MSTRESSES,lstress, PSTRESS, NEL)
cC
c      REAL STRESSES (MSTRESSES)
c      INTEGER PSTRESS (*)
c      INTEGER IEL,KK,I,  NGP,IH 
cc
cc     DO IEL =1,NEL)
c
c-----------------------------------------------------------------------
      SUBROUTINE R_STRESSES (IO,STRESSES,MSTRESSES,lstress, PSTRESS,NEL)
C
C     This reads the stresses from the datafile
C     It allocates storage for each element (if not yet allocated)
C     ,extends the End_of_structure pointer
C     and stores the record of stresses 
C     .. for now assume that all records are NGP*IH long
C
C     Caveats:
C        Stores *all* of the stress record (ie. Xg,Yg, S,T, S1,S2,S3, etc.
C        - this is not necessarily compatable with DANFE
C      (tho we could decode ICODE hence abstract what we desire?)
C
      REAL STRESSES (MSTRESSES)
      INTEGER PSTRESS (*)

      REAL STR_EL (27 * (6+6+1))           !- element stresses
      INTEGER NEL,IEL,KK,I,  NGP,IH        !- NEL is unused

      DO KK=1, 999 999

    1   READ (IO,*,IOSTAT=IOS)  IEL, NGP,IH, icode
     &          ,(STR_EL(I),I=1,NGP*IH)

c.. now strip STR_EL to give just the IH raw stresses
c (is IH the original (6) or the  extended (eg.23) ? )

        CALL IN_TEST (IO,IOS,*1,*999)
        IF (PSTRESS(iel).EQ.0) THEN    !--- allocate storage ----
          PSTRESS(iel) = LSTRESS + 1
          LSTRESS = LSTRESS + IH*NGP + 2 !  (hmm ?)
        ELSE                   !--- already have storage -----
c              ... maybe compare old and new records to check that they
c              ... are compatable, else (F90-only) dealloc-realloc.
        ENDIF
        CALL PUT_STRESS (STRESSES,PSTRESS(IEL), STR_EL,NGP,IH )

      ENDDO   !-- loop lines in the table

  999 WRITE(*,'(I6,A)') KK,' elements'' stresses read in'
      WRITE(*,'(A,i3,a,i3,a)') 'Last elem read has',
     &     NGP,' GP''s and', IH,' stresses per point'

c     WRITE (*,'(A,(8i7))') 'pointers=', (PSTRESS(IEL),IEL=1,NEL)

      RETURN
      END


c-----------------------------------------------------------------------
      SUBROUTINE R_DS_BOVIDA (IO,DISPS_TOT,MDF,IDISPS, NODOF,
     & STRESSES,MSTRESSES,lstress, PSTRESS,NEL)
C
C     This read in a results file in BOVIDA format.
C     ie. a set of Nodal disps, and a set of GP stresses.
C        DJK 7-12-96
C     File Format:
C       NEL_  , NN_   (ie the reverse of the 2 tables!)
C       <INODE, DX,DY>, 1:NN_
C       <IGP, IGX,IGY,S1,S2,alpha, (hence no Sz!) > 1:NEL_ (so no IEL!)
C
C     Caveats:
C     - Should we use the individual DISPS_TOT, or the database GDISPS here
C       ie. Is the concept of multi-loadsteps a part of the library or 
C       just DANPLOT? (A: probably the latter)
C     - Does the S1,S2 -> sx,sy,Txy work?
C     - Sz is undefined - just set so mean stress ?
C
      REAL DISPS_TOT(MDF,IDISPS)
      REAL STRESSES (MSTRESSES)
      INTEGER PSTRESS (*)

      READ (IO,*) NEL_, NN_
c.. should I zap the other DISPS (NN_+1:NN) to avoid oddities?

      CALL R_DISPS (IO,DISPS_TOT,MDF,IDISPS, NODOF,NN_)
c---- null all the existing pointers ----
      LSTRESS=0
      DO I=1,NEL
        PSTRESS(I) = 0
      ENDDO
      CALL R_STRESSES_BOVIDA (IO,STRESSES,MSTRESSES,lstress,
     &   PSTRESS,NEL_)
      RETURN
      END

c-----------------------------------------------------------------------
      SUBROUTINE R_STRESSES_BOVIDA (IO,STRESSES,MSTRESSES,lstress,
     &   PSTRESS,NEL)                  
C
C     This reads the stresses from the datafile
C     ** In 'BOVIDA' format: iel IEL,IGP, IGX,IGY,sx,sy,Txy, (sz)
C     .. else same as R_STRESSES q.v.
C       DJK 7-12-96
C
C     Notes:
C        BOVIDA file only has 2d 4-GP element (usu.=8nq's)
C        BOVIDA file has no SZ, so we just set to zero
C        must supply NEL (really = NEL_with_stresses)
C     Caveats:
C        I have not verified that the GP's are in the right order
C        - probaly *NOT* right cos it was based on FE5LIB.
C        It guesses that NGP=4 always. might be 9 sometimes?
C
      REAL STRESSES (MSTRESSES)
      INTEGER PSTRESS (*)

      REAL STR_EL (27 * (6+6+1))           !- element stresses
      INTEGER NEL,IEL,  NGP,IH 
      INTEGER POS(4)
      DATA POS/4,3,2,1/     !- order of the GP's
      RTD = 3.14159265/180.

      IH = 4
      NGP= 4         !- it is possible that NGP may be 9 too ?
      DO IEL=1,NEL
        DO IGP = 1,NGP
    1     READ (IO,*,IOSTAT=IOS)  IGP2, X_GP,Y_GP,S1_,S2_,alpha_  !,SZ
          CALL IN_TEST (IO,IOS,*1,*999)
c.. it would be clever if we WTHATNed X_GP and Y_GP to make sure we
c   get it right!
          IBASE = (POS(IGP)-1)* IH
c            s1 = max (s1,s2)     !- so s1 = most +ve ?do we need to do this?
c            s2 = min (s1,s2)     !     s2 = most -ve

c             IF (S1_ .GT. S2_) THEN   !(code copied from dtdxf.c )
c               S1 = S1_
c               S2 = S2_
c             ELSE
c               IF (S2_ .LT.0.) ALPHA = ALPHA + 90.
c               S1 = S2_
c               S2 = S1_
c               ALPHA = ALPHA+90.
c             ENDIF
             S1 = S1_
             S2 = S2_

c           s1 = -s1      !- I think that I want compresion to be +ve?
c           s2 = -s2

c          Sx = S1         !- hack back ? 
c          Sy = S2
c          TXY=0.

          alpha = alpha_
c         if (s2_.lt.0) alpha = alpha + 180.
c         if (s1-s2.lt.0) alpha = alpha + 180.    !- wow a hack!
          if (alpha.lt.0) alpha = alpha + 90.    !- wow a hack!
          Smean = (S1+S2)/2.
          Tmax  = abs(S1-S2)/2.      !- this is max shear  (always +ve)

c.. OK Sx and Sy appear to be correct , but sometimes they  are the 
c   wrong way around.
          Sx  = Smean - Tmax * COS (2.*alpha*rtd)    !- check signs?
          Sy  = Smean + Tmax * COS (2.*alpha*rtd)
          Txy = Tmax * SIN (-2.*alpha*rtd)            !- Ok ?
c         Sz = 0.
          Sz = (SX+SY)/2.

c.. or try this (based on s1,s2 as eigenvalues)
          ct = cos(alpha_*rtd)
          st = sin(alpha_*rtd)
          sx =  ct*ct*s1 + st*st*s2
          sy =  st*st*s1 + ct*ct*s2
          txy=  st*ct*s1 - ct*st*s2



          if (iel.eq.1.and.igp.eq.1) open (77,file='dan_bov.log')
          if (igp.eq.1) write(77,'(A,i5)')' Element no',iel
          write (77,'(2f7.3,99f14.8)') 
     &          x_gp,y_gp, sx,sy,txy, 0.00, s1_,s2_, alpha, st,ct
          if (igp.eq.ngp) write(77,'(A)')

          STR_EL(IBASE+1) = SX     !- simple to stuff in a different order
          STR_EL(IBASE+2) = SY
          STR_EL(IBASE+3) = TXY
          STR_EL(IBASE+4) = SZ     ! Sz is just a guess.
        ENDDO

        IF (PSTRESS(iel).EQ.0) THEN    !--- allocate storage ----
          PSTRESS(iel) = LSTRESS + 1
          LSTRESS = LSTRESS + IH*NGP + 2 !  (hmm ?)
        ELSE                   !--- already have storage -----
c              ... maybe compare old and new records to check that they
c              ... are compatable, else (F90-only) dealloc-realloc.
        ENDIF
        CALL PUT_STRESS (STRESSES,PSTRESS(IEL), STR_EL,NGP,IH )

      ENDDO   !-- loop lines in the table

  999 WRITE(*,'(I6,A)') IEL,' elements'' stresses read in'
      WRITE(*,'(A,i3,a,i3,a)') 'Last elem read has',
     &     NGP,' GP''s and', IH,' stresses per point'

c     WRITE (*,'(A,(8i7))') 'pointers=', (PSTRESS(IEL),IEL=1,NEL)

      RETURN
      END

c-----------------------------------------------------------------------
      SUBROUTINE WR_STRESSES (IO,STRESSES,PSTRESS,lstress)
C
C     This writes the stresses to the output file
C     My style is to have a mat_prop that determines if we write for
C     this element or not (and also what *type* of date we write.)
C     .. so here do we write ALL stresses or just one elements worth?
C
C     See WR_STRESSES_OLD which uses the old 'fixed size' stress array
C     . one day I will simply update the old code
      REAL STRESSES (*)
      INTEGER PSTRESS (*)
c     INTEGER IEL,KK,I,  NGP,IH 
      CALL MYERROR (3,'Can''t write stresses from this array')

      END

C-----------------------------------------------------------------------
      SUBROUTINE WRITE_STRESSES_OLD (IO, GC,IGC,NN,NDIM,NUMS,INUMS,NEL
     &    ,PRPTAB,IPRPTAB, STRESSES,IEVSTRS, pore,ipore  ,NODOF 
     &     ,IPR,L_STRAIN,IOP_NGP)

C
C     This writes the current stress vector to the outfile IO
C     .. eventually will have an option for principal stresses etc.
C       ( it does now )
C  -  L_STRAIN is a flag if we pass strains so '*STRAINS' header and shear /=2
c  -  Old style 'cos STRESSES is 2D, (new style is 1D with a vector of 
c       pointers to each element's record.)
C     Date=1995?
c
c    7-12-96 hmm I could overload this routine to write out the data in 
c     RESBO format = single table of IEL,IGP, IGX,IGY, SX,SY,TXY, (SZ?)
c     .. this is nice for simple postprocessing
C
      INTEGER    NUMS (INUMS,NEL)    !-- the topology of the elements
      REAL         GC (IGC,NN)       !-- the coordinates of all the nodes
     &      ,STRESSES (IEVSTRS,NEL)  !- total GP stresses
     &        ,PORE (IPORE,NEL)      !- pore pressures for all gauss points
     &        ,PRPTAB (IPRPTAB,*)    !- the properties of each material type
      LOGICAL L_STRAIN               !- true is data is STRAIN (not stress)
C----------------------- Workspace arrays ------------------------------
      PARAMETER (M_NOD = 20     !-- max # nodes per element
     &          ,M_NODOF= 3     !-- max # freedoms per node
     &          ,M_NS   = 6     !-- max # stresses per GP
     &          ,ISMP  = 27 )   !-- max GP's per element
      PARAMETER (IDER=M_NODOF, ICOORD = M_NOD)
      PARAMETER (MAXVEC=30)   !- max no. of terms written per line.

      INTEGER NUM (M_NOD)              !-- node numbers of an element
      REAL STRESS (10)                 !-- stresses at a point
     &    ,longvec(maxvec)             !-- list as we build it
     &     ,totvec(maxvec)             !-- elem. average values. (optional)

      CHARACTER LABELS(maxvec)*13      !- labels for each column

      REAL
c     &        FUN (M_NOD)          !-- shape funs
c     &       ,DER (IDER,M_NOD)     !-- shape fun derivs
     &      COORD (ICOORD,M_NODOF) !-- nodal coords
c     &       ,SMP (ISMP,M_NODOF)   !-- Integration sampling points
c     &       ,WTS (ISMP)           !-- Integration point 'weights'
     &        ,XY (M_NODOF)        !-- coords of a Gauss point
      LOGICAL L_DIRECT, L_INVAR, L_PRINC, L_MSS, L_GC, L_AVE, L_PORE,
     &   HETERO

      ILABEL=1     !- flag so we will write the column headers
      RTD = 180./3.14159265            !-- factor for 'degress to radians'
c     rtd = 1.                         ! (just for the lode angle)

c--------------------- find stats of what to print -------------------
      HETERO = .FALSE.
      NEL_print = 0
      IPSTRESS_old = 0
      DO IEL=1,NEL
         IF (IPSTRESS_old.eq.0) GOTO 20      !- none found yet
         CALL GET_EL_IMAT (NUMS,INUMS,IEL, IMAT)
         IF (IMAT.EQ.0) GOTO 20              !- skip 'missing' materials
         IF (.NOT.L_STRAIN) THEN
            IPSTRESS = nint(PRPTAB (19,IMAT))      !- cf strain option = (20)
         ELSE
            IPSTRESS = nint(PRPTAB (20,IMAT))      !- cf stress option = (19)
         ENDIF
         IF (IPSTRESS.EQ.0) GOTO 20          !- skip 'no print' elements
         NEL_PRINT=NEL_PRINT+1               ! count of elements to print
         IF (IPSTRESS.ne.IPSTRESS_OLD) THEN  ! 2 differing codes found
            HETERO = .TRUE.                  ! flag as 'heterogenous'
            GOTO 21                          ! and 'EXIT'
         ENDIF
  20     CONTINUE    !- 'cycle'
      ENDDO
   21 CONTINUE


c.. hmm option to skip the titles if 'RESBO' style??
      IF (NEL_PRINT.ge.0) THEN                !- not yet tested ?
        IF (     L_STRAIN) WRITE (IO,'(A)')   '*STRAINS'
        IF (.NOT.L_STRAIN) WRITE (IO,'(A)')   '*STRESSES'
        WRITE (IO,'(A,i7)') '# NEL_PRINT=',NEL_PRINT
      ENDIF

C-------------------- loop through the elements ------------------------
c.. only really need NGP (via NOD), maybe can store in NUMS so direct ?
c   better is to store NGP in the STRESSES database.
      DO IEL=1,NEL
        CALL GET_EL_IMAT (NUMS,INUMS,IEL, IMAT)
        IF (IMAT.GT.0) THEN                 !- skip 'missing' materials
        IF (.NOT.L_STRAIN) THEN
          IPSTRESS = nint(PRPTAB (19,IMAT))       !- cf strain option = (20)
        ELSE
          IPSTRESS = nint(PRPTAB (20,IMAT))       !- cf stress option = (19)
        ENDIF
        IF (IPSTRESS.NE.0) THEN             !- skip 'no print' elements

c... 26-6-97 cf if C= f(depth)
        IOP_VM = NINT(PRPTAB (18,IMAT))
          C    = PRPTAB (3,IMAT)    
          PHI  = PRPTAB (4,IMAT)

        CALL GET_ELEMENT (NUMS,INUMS,IEL, NUM, NOD,NDIME,ITYPE
     &      ,IMAT,IUSER1,IUSER2,IUSER3)

c------ get the No. of Stress sampling (Gauss) points ---
c.. cf later the # of terms we have at each sampling point
c..  Note that NGP should really be abstracted from the stress database
c    rather than re-calculated here (may not be the same!)

        IF (IOP_NGP.lt.0) THEN
          CALL GET_DEFAULT_NGP (NOD,NDIME,NGP)
        ELSE
          NGP= IOP_NGP
        ENDIF

c----------- get the # of stress terms per Gauss point ----------
c.. better if a database held the #of stress-points in *this* element
        IH = NDIME * (NDIME+1)/2      !- = 1 in 1D, 3 in 2D, 6 in 3D, 10 in 4D
        IF (NDIME.EQ.2) IH = 4        !- include the z term ?

C---- decode IPSTRESS into what we should write to the outfile ------
        J=IPSTRESS
        L_AVE= (J.LT.0)                     !- to just average each elem
          J=ABS(J)
        L_DIRECT= MOD(J,2).EQ.1            ! *1*
          J=J/2
        L_INVAR = MOD(J,2).EQ.1            ! *2*
          J=J/2
        L_PRINC = MOD(J,2).EQ.1            ! *4*
          J=J/2
        L_MSS   = MOD(J,2).EQ.1            ! *8*
          J=J/2
        L_GC    = MOD(J,2).EQ.1            ! *16*
          J=J/2
        L_PORE  = MOD(J,2).EQ.1            ! *32*

c--------------------- the Table Headers ------------------------
c.. as a SUBROUTINE: (NDIM,L_* -> LABELS, ILONG)
c.. Hmm if all elements have the *same* non-zero IPSTRESS then we
c   should only write out this title line at the top of the whole table

c   ie. loop all elems . get code, if other code /=, EXIT 'FLAG=HETERO'
c      if HETERO .or. IEL=1 then write line
c .. also get count of nel_to_print .. if =zero skip *STRESSES writing!

          IF (ILABEL.EQ.1) THEN     !- if we want column labels.
           CALL WR_STRESS_HEADERS (NDIM,IH, LABELS,MAXVEC, ILONG
     &      ,L_DIRECT, L_INVAR, L_PRINC, L_MSS, l_GC, L_PORE) 
            WRITE (IO,'(A,99A13)') '# ',(LABELS(J),J=1,ILONG)
            ILABEL=0        !- so inhibit next time
          ENDIF

C----------- loop and process each Gauss-Point ------------


        DO IGP=1,NGP

c.. < hmm this seems to ignore my  malloc'ed data structure >
c.. yes; this is the only place where the new and old structures differ.
          DO I=1,IH             !--  get the stresses at this point
            STRESS (I) = STRESSES((IGP-1)*IH+I,IEL)
          ENDDO

          IF (L_STRAIN) THEN      !-- if strains, must mung the shears
            IF (NDIM.EQ.2) THEN
              STRESS(3) = STRESS(3) /2.
            ELSEIF (NDIM.EQ.3) THEN
              DO I=4,6
                STRESS(I) = STRESS(I) /2.
              ENDDO
            ENDIF
          ENDIF

c-- get the coords of this Gauss point --
c   IGP,NGP, ,COORD,NDIME,NOD,NDIM -> XY()

          IF (L_GC) THEN       !- if we will need the GP coords
            CALL GET_COORD (NUM,NOD,NDIM,GC,IGC,COORD,ICOORD)
            CALL GET_GAUSS_XY (IGP,NGP,COORD,ICOORD, NDIME,NOD,NDIM,XY) 
          ENDIF


c.. hmm if strains then the shears need to be halved.
c         CALL INVARIENTS (STRESS,NODOF,SIGM,DSBAR,THETA)
          CALL INVARIENTS (STRESS,NDIME,SIGM,DSBAR,THETA)   ! 23-2-97
          CALL PRINCIPAL_STRESS (SIGM,DSBAR,THETA, S1,S2,S3)
          IF (L_MSS)     !- be careful if we are doing strains
     &    CALL GET_F_ANY (PHI,C, SIGM,DSBAR,THETA ,F,F_MSS, IOP_VM)

c--------------------- assemble the line to print out ------------------
c.. cf above where we create the list of column headers of what's what.
c.. if RESBO style then we could just explicitly write the line out here.
          ilong=0            !- current line length
          IF (L_GC) then
            DO I=1,NDIM
              ilong = ilong + 1
              LONGVEC(ilong) = XY(i)
            ENDDO
          ENDIF
          IF (L_DIRECT) then
            DO I=1,IH
              ilong = ilong + 1
              LONGVEC(ilong) = stress(i)
            ENDDO
          ENDIF

          IF (L_PORE) then
            ilong = ilong + 1
            LONGVEC(ilong) = PORE(igp,iel)   !Pore Pressures 
          ENDIF

          IF (L_INVAR) then
            ilong = ilong + 1
            LONGVEC(ilong) = SIGM           !  (or as T,S,theta ?)
            ilong = ilong + 1
            LONGVEC(ilong) = DSBAR
            ilong = ilong + 1
            LONGVEC(ilong) = THETA *RTD     !- so lode angle in degrees?
          ENDIF
          IF (L_MSS) then
            ilong = ilong + 1
            LONGVEC(ilong) = F
            ilong = ilong + 1
            LONGVEC(ilong) =F_MSS
          ENDIF
          IF (L_PRINC) then
            ilong = ilong + 1          !/* if only this was in 'C :-) */
            LONGVEC(ilong) = S1        
            ilong = ilong + 1
            LONGVEC(ilong) = S2
            ilong = ilong + 1
            LONGVEC(ilong) = S3
          ENDIF

c--- write the length of this element's record
c.. code for the 'type' of each record too (=IPSTRESS)?
c.. some file styles will not have a line like this.
c.. *caveat* should it be IH or the size of LONGVEC here?
c.. OOPS! 23-03-01 next line was 'I' not 'IGP'
c  henc DANPLOT couldnt read the stresses back
        IF (IGP.eq.1.and..NOT.L_AVE)
c    &  WRITE (IO,'(I7,2I3,I3)') IEL, NGP,IH, IPSTRESS
     &  WRITE (IO,'(I7,2I3,I3)') IEL, NGP,ILONG,IPSTRESS

          IF (.NOT.l_ave) THEN
            WRITE (IO,'(99G13.4)') (LONGVEC(J),J=1,ILONG)
          ELSE
            IF (IGP.EQ.1) THEN
              DO I=1,ILONG
                TOTVEC(I) = LONGVEC(I)
              ENDDO
            ELSE
              DO I=1,ILONG
                TOTVEC(I) = TOTVEC(I) + LONGVEC(I)
              ENDDO
            ENDIF       !- sum over each element
          ENDIF     !- if option to form the average value.
        ENDDO  !- loop GP's

        IF (L_AVE) 
     &  WRITE (IO,'(99G13.4)') (TOTVEC(J)/REAL(NGP),J=1,ILONG)

      ENDIF   !-- only if this material 'print stress' option is on
      ENDIF   !-- only if element is 'present'  
      ENDDO   !-- elements

      RETURN
      END
c-----------------------------------------------------------------------
      SUBROUTINE WR_STRESS_HEADERS (NDIM,IH, LABELS,ILABELS,ILONG
     &  ,L_DIRECT, L_INVAR, L_PRINC, L_MSS, l_GC, L_PORE) 
c
c     Writes out the column headers for the table of stresses.
c       Dan Kidger 26-6-97
C
      LOGICAL L_DIRECT, L_INVAR, L_PRINC, L_MSS, L_GC, L_PORE
      CHARACTER LABELS(ILABELS)*13, STR*30   !- labels for each column 

      ilong = 0      !- create the table for each GP
      IF (L_GC) then
        DO I=1,NDIM
          ilong = ilong + 1
          str = 'XYZW'
          LABELS(ilong) = str(i:i)
        ENDDO
      ENDIF
      IF (L_DIRECT) then
        DO I=1,IH
          ilong = ilong + 1
          ipos = (I-1)*3
c         IF (NODOF.EQ.2)THEN
          IF (IH.EQ.4)THEN
            STR = 'Sx Sy TxySz'
            LABELS(ilong) = STR(ipos+1:ipos+3)
c         ELSEIF (NODOF.EQ.3) THEN
          ELSEIF (IH.eq.6) THEN
            STR = 'Sx Sy Sz TxyTyzTzx'
            LABELS(ilong) = STR(ipos+1:ipos+3)
          ENDIF
        ENDDO
      ENDIF
      IF (L_PORE) then                   ! 26-3-99
        ilong = ilong + 1
        LABELS(ilong) = 'Pore P'           !  
      ENDIF
      IF (L_INVAR) then
        ilong = ilong + 1
        LABELS(ilong) = 'Sigm'           !  (or as T,S,theta ?)
        ilong = ilong + 1
        LABELS(ilong) = 'DSbar'
        ilong = ilong + 1
        LABELS(ilong) = 'Lode A'    !- so lode angle in degrees?
      ENDIF
      IF (L_MSS) then
        ilong = ilong + 1
        LABELS(ilong) = 'Yield Func'
        ilong = ilong + 1
        LABELS(ilong) ='Mob.S.Str'
      ENDIF
      IF (L_PRINC) then
        ilong = ilong + 1
        LABELS(ilong) = 'S1'
        ilong = ilong + 1
        LABELS(ilong) = 'S2'
        ilong = ilong + 1
        LABELS(ilong) = 'S3'
      ENDIF
      RETURN
      END

c-----------------------------------------------------------------------
      SUBROUTINE GET_GAUSS_XY (IGP,NGP, COORD,ICOORD, NDIME,NOD,NDIM,XY)
c
c     This simply returns the XY coords of a Gauss-point, given the
c     Coords of the parent and the GP number.
c        c. Dan Kidger 26-9-97 
c

      REAL COORD (icoord,NDIM), XY(NDIM)
      PARAMETER (M_NOD = 20        !-- max # nodes per element
     &          ,M_NODOF= 3        !-- max # freedoms per node
     &          ,ISMP  = 27 )      !-- max GP's per element
      PARAMETER (IDER=M_NODOF)
      REAL    FUN (M_NOD)          !-- shape funs
     &       ,DER (IDER,M_NOD)     !-- shape fun derivs
     &       ,SMP (ISMP,M_NODOF)   !-- Integration sampling points
     &       ,WTS (ISMP)           !-- Integration point 'weights'

      CALL GET_ANY_GAUSS (SMP,ISMP,WTS,NDIME,NGP,NOD,AREA)
      CALL GSF (NDIME,NOD,ITYPE, DER,IDER,FUN,SMP,ISMP,IGP)
c     CALL GET_COORD (NUM,NOD,NDIM,GC,IGC,COORD,ICOORD)
      CALL MTVMULT (COORD,ICOORD,FUN,NDIM,NOD,XY)

      RETURN
      END

C-----------------------------------------------------------------------

c-----------------------------------------------------------------------
      SUBROUTINE PUT_STRESS (STRESSES,IBASE, STR_EL,NGP,IH )
c
c     This simply stores a element's stress record
c     (eg called from a Plasticity routine)
c     - do we store NGP here ? (stored as a mat prop ???)
C     - do we store IH here ? (implied from NDIME in NUMS)
c
      REAL STRESSES (*), STR_EL (NGP*IH)
      IP = IBASE
      STRESSES (IP) = NGP
      IP = IP +1
      STRESSES (IP) = IH     !(actualy the length of the record eg. +PP)
      IP = IP +1
      DO I=1,NGP*IH
        STRESSES (IP) = STR_EL (I)
        IP = IP +1
      ENDDO
      END

c-----------------------------------------------------------------------
      SUBROUTINE GET_STRESS (STRESSES,IBASE, STR_EL,NGP,IH )
c
c     This simply retrives an element's stress record
c     (eg called from a Plastcity routine)
c     Note:
C       here we return NGP and IH - should be the same as calculated.
      REAL STRESSES (*), STR_EL (NGP*IH)

      IP = IBASE
      NGP = NINT (STRESSES(IP))
      IP = IP +1
      IH = NINT (STRESSES(IP))
      IP = IP +1
      DO I=1,NGP*IH
        STR_EL(I) = STRESSES (IP)
        IP = IP +1
      ENDDO
      END

c-----------------------------------------------------------------------
      SUBROUTINE GET_STRESS_TERM (STR_EL,NGP,IH,NODOF, C,PHI,IOP_VM
     & ,GPVALS, CC )
C
c     This returnd a list of derived stress-terms GPVALS() for *all* the GP's 
c     based on the code: CC = 'Sx',etc for what we want.
C     What about the Abstraction of Strains, and Pore Pressure, etc.?
c          DJK  2-12-95
C     Notes:
c       1-9-96 nicer to chose just one GP / all GPs ?
c       This repeats code in WR_STRESSES_OLD :-(

c     Available codes:
c         'Sx'  'Sy'  'Sz'  'Txy'  'Tyz'  'Tzx'
c         'SIGM'   'DSBAR'  'THETA'  (LODE?)   'J2'
c         'S1'  'S2'  'S3'  'S'    'T'
c         'F'      'F_MSS'  'FOS'    (=1/F_MSS)
c
      REAL GPVALS (NGP)
      REAL STR_EL (NGP*IH)           !- one element's stress/strain record
c    &      ,LIST (NGP)
      CHARACTER CC*4
      REAL  STRESS (120)            !- a full stress record

      DO IGP=1,NGP
        I = IGP
        IPT = (IGP-1)*IH +1

c------------------- direct-stress terms ---------------------
c.. hmm macro-if so we can group better?
      IF (index('sx  sy  sz  txy tyz tzx ',CC(1:4)).gt.0) THEN

        DO J=1,IH
          STRESS(J) = STR_EL (IPT + J-1)
        ENDDO  
        IF (CC.eq.'sx') THEN
          GPVALS(I) = STRESS (1)
        ELSEIF (CC.eq.'sy') THEN
          GPVALS(I) = STRESS (2)
        ELSEIF (CC.eq.'sz') THEN
          IF (NODOF.eq.2) GPVALS(I) = STRESS (4)
          IF (NODOF.eq.3) GPVALS(I) = STRESS (3)
        ELSEIF (CC.eq.'txy') THEN
          IF (NODOF.eq.2) GPVALS(I) = STRESS (3)
          IF (NODOF.eq.3) GPVALS(I) = STRESS (4)
        ELSEIF (CC.eq.'tyz') THEN
          IF (NODOF.eq.2) GPVALS(I) = 0.
          IF (NODOF.eq.3) GPVALS(I) = STRESS (5)
        ELSEIF (CC.eq.'tzx') THEN
          IF (NODOF.eq.2) GPVALS(I) = 0.
          IF (NODOF.eq.3) GPVALS(I) = STRESS (6)
        ENDIF

c------------------- direct-stress terms ---------------------
c.. hmm macro-if so we can group better?
      ELSEIF (index('s1  s2  s3  ',CC(1:4)).gt.0) THEN
        CALL INVARIENTS (STR_el(ipt),NODOF,SIGM,DSBAR,THETA)
        CALL PRINCIPAL_STRESS (SIGM,DSBAR,THETA, S1, S2, S3)
        IF (CC.EQ.'s1') THEN         !- 7-12-96 was S1 in CAPS !
          GPVALS(I) = S1
        ELSEIF (CC.EQ.'s2') THEN
          GPVALS(I) = S2
        ELSEIF (CC.EQ.'s3') THEN
          GPVALS(I) = S3
        ENDIF

c------------------- deviatoric-stress terms ---------------------
      ELSEIF (CC.EQ.'sigm') THEN
        CALL INVARIENTS (STR_el(ipt),NODOF,SIGM,DSBAR,THETA)
        GPVALS(I) = Sigm
      ELSEIF (CC.EQ.'dsb') THEN
        CALL INVARIENTS (STR_el(ipt),NODOF,SIGM,DSBAR,THETA)
        GPVALS(I) = dsbar
      ELSEIF (CC.EQ.'lode') THEN
        CALL INVARIENTS (STR_el(ipt),NODOF,SIGM,DSBAR,THETA)
        GPVALS(I) = theta * 180./3.14159265      !-  is degress nicer?

c------------------- Mobilsied shear-strength etc. -------------------

      ELSEIF (CC.EQ.'f   ') THEN
        CALL INVARIENTS (STR_el(ipt),NODOF,SIGM,DSBAR,THETA)
        CALL GET_F_ANY (PHI,C, SIGM,DSBAR,THETA ,F,F_MSS, IOP_VM)
        GPVALS(I) = F
      ELSEIF (CC.EQ.'mss ') THEN
        CALL INVARIENTS (STR_el(ipt),NODOF,SIGM,DSBAR,THETA)
        CALL GET_F_ANY (PHI,C, SIGM,DSBAR,THETA ,F,F_MSS, IOP_VM)
        GPVALS(I) = F_MSS
      
      ELSEIF (CC.EQ.'fos ') THEN
        CALL INVARIENTS (STR_el(ipt),NODOF,SIGM,DSBAR,THETA)
        CALL GET_F_ANY (PHI,C, SIGM,DSBAR,THETA ,F,F_MSS, IOP_VM)
        GPVALS(I) = 1./F_MSS

C-------------------------------------------------------------
      ELSE
        PRINT*,'Stress-term', C,' is unknown !' 
        CALL MYERROR (3,'Unknown stress term (GET_STRESS_TERM)') 
      ENDIF

      ENDDO    !-- loop the Gauss-points
      RETURN
      END


c-----------------------------------------------------------------------
      SUBROUTINE GP_TO_NODES (GPVALS,NGP,IH, NDIME,NOD,ITYPE, NODALS )
c
c     This takes a (set?) of GP values and 'extrapolates' them out to 
c     the nodes. We do this by constructing a set of 'weighting' for 
c     each GP to give a node's value
c        DJK 2-12-95

c      ** also think about low-order smoothing = a set of factors
c         that fits a 4nq through the '9nq' of the data (needs values?)
c
c     To do smoothing do:
c         loop elements : get_num, get_coord, get_ngp, get_stress
c      call get_stress_term to get just the term (eg DSBAR) that we want
c      then call this routine to get the nodal values.
c      then either print-out (cf FEMVIEW); contour-directly or global-average
C      at all the nodes.

      SAVE
      REAL GPVALS (NGP)        ! input values at the Gauss points
     &     ,NODALS (NOD)        ! output values at the Nodes.

      PARAMETER (IPM=32)        !- max NOD  (also max NGP)
      REAL PM(IPM,IPM)          ! The transformation matrix

      INTEGER NOD,    NDIME,    ITYPE
     &       ,NOD_OLD,NDIME_OLD,ITYPE_OLD
      DATA    NOD_OLD,NDIME_OLD,ITYPE_OLD/3*-99/

      IF (     NOD.NE.NOD_OLD  
     &    .OR. NDIME.NE.NDIME_OLD 
     &    .OR. ITYPE.NE.ITYPE_OLD) THEN   !- a 'new' element type
        CALL GET_GP_MATRIX (NDIME,NOD,ITYPE, NGP, PM,IPM )
        NOD_OLD   = NOD
        NDIME_OLD = NDIME
        ITYPE_OLD = ITYPE
      ENDIF

      DO I=1,NOD                !- an explicit MVMULT
        T = 0.                  !  hmm if NOD>NGP 'tis better
        DO IGP=1,NGP              !  to reverse these loops
          T = T + PM (IGP,I) * GPVALS(IGP)
        ENDDO
        NODALS(I) = T
      ENDDO
      RETURN
      END

c-----------------------------------------------------------------------
      SUBROUTINE GET_GP_MATRIX (NDIME,NOD,ITYPE, NGP, PM,IPM )
C
C     This returns the GP->NODES transformation matrix: PM(,) .
C     ie. a MVMULT of this by a set of GP values gives a set of nodal 
C     values.
C        c. DJK  2-12-95
c
c  Method.
C      1. WTHATN to get the nodal coords in locals
c      2. 'construct' a '4nq' of the (4) GP's
c      3. Call GET_GAUSS to get the GP coord -> min/max values
c      4. so multiply all the nodal-local-coods by 1./maxval.
c      5. loop the nodes and call GSF() to get FUN(4 values)  
c      6. assemble this into a 4x8 'morph' matrix.
c    .. note we can store the 'last' elem. so avoiding repeating work.

      REAL PM (IPM,*)

C----------------------- Workspace arrays ------------------------------
      PARAMETER (M_NOD =  32    !-- max # nodes per element
     &          ,M_NODOF= 4     !-- max # freedoms per node
c     &          ,M_IH   = m_nodof*(m_nodof+1)/2     !-- max # stresses per GP
c     &          ,MDOF   = M_NOD*M_NODOF  !-- max # freedoms per element
     &          ,ISMP  = 100     !-- max GP's per element
     &                      )
      PARAMETER ( IDER=M_NODOF,ILCOORD=M_NOD )

      REAL    FUN (M_NOD)          !-- shape funs
     &       ,DER (IDER  ,M_NOD)   !-- derivs of shape funs
     &    ,LCOORD (ILCOORD,3)      !-- local coords of the GPs
c    &     ,COORD (ICOORD,3)       !-- Gp coords as a 4nq (etc.)
     &       ,SMP (ISMP,M_NODOF)   !-- Integration sampling points
     &       ,WTS (ISMP)           !-- Integration point 'weights'
      INTEGER NGP2

      INTEGER L_4NQ(4),L_8NB(8)
      DATA L_4NQ/1,3,4,2/, L_8NB /1,5,6,2, 3,7,8,4/  !- check?

c.. if only one GP then smooth to all nodes equaly ..
      IF (NGP.EQ.1) THEN  
        DO I=1,NOD
          PM(1,I) = 1.
        ENDDO
        RETURN
      ENDIF
 
c.. otherwise find the 'inner element'
      CALL WTHATN (NOD,NDIME,ITYPE,LCOORD,ILCOORD)
      CALL GET_ANY_GAUSS (SMP,ISMP,WTS,NDIME,NGP,NOD, area)
      XMAX_GP=ABS(SMP(1,1))        !- really MAXVAL but first GP 
c                                  !   is always in a 'corner'

C--- extrapolate to get the local-coords of the (8) nodes ---
      DO I=1,NOD
        DO J=1,NDIME
          LCOORD(I,J) = LCOORD(I,J) * 1./XMAX_GP
        ENDDO
      ENDDO


C--------- sample to get the shape-functions at each real node.
c.. make sure that an NGP-noded element exists (ngp=4 for tri's, Irons rules..
      ngp2=ngp
      if (ngp2.eq.14) ngp2=8       !19-03-98 hacked to skip mid-face points
      DO I=1,NOD
        CALL GSF  (NDIME,NGP2,1, DER,IDER,FUN, LCOORD,ILCOORD, I)

        IF (NGP2.EQ.4) THEN
          DO IGP=1,NGP2
            PM(L_4NQ(IGP),I) = FUN(IGP)       !- Is this the correct way round ?
          ENDDO
        ELSEIF (NGP2.EQ.8) THEN
          DO IGP=1,NGP2
            PM(L_8NB(IGP),I) = FUN(IGP)       !- Is this the correct way round ?
          ENDDO
        ELSE
C         -- just hope for the best ??
          DO IGP=1,NGP2
            PM(IGP,I) = FUN(IGP)       !- Is this the correct way round ?
          ENDDO
        ENDIF
      ENDDO
      RETURN
      END
c-----------------------------------------------------------------------


