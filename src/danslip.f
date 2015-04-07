
      PROGRAM DANS_SLIP_SURFACE_ANALYSER

      PARAMETER ( MELS =8000 )   !-- Max # of elements (change as needed)
      CHARACTER  VERSION*15
      PARAMETER (VERSION =             'Version 1.0'                   )

c------------------------------------------------------------------------c
      COMMON/COM_DISPS/DISPS(3,5000)    !-- in COMMON for SAMPL3 :-)
c-----------------------------------------------------------------------

      PARAMETER ( MNN = MELS     ! = 2 for 2d (=3 for 2d/3d)
     +           ,INF = 3        ! = 2 for 2d (=3 for 2d/3d)
     +           ,IGC = INF      !
     +           ,INUMS=28   )   ! = max nodes per element

      REAL  GC  (IGC,MELS)       !- co-ordiantes of all the nodes
      INTEGER NUMS (INUMS,MNN)   !- the elements' nodes etc.
     +          ,P (MNN)         !- a 'workspace' of pointers

      CHARACTER KEYWORD*70   ! data input keyword 'token'
     +            ,FILE*100  ! input data file name


      INTEGER U0             ! .INP : the original data file
     +       ,U1             !  --- : the current data file being read

      LOGICAL FOUND          !- a keyword was found

c------------------ DATA stataments ------------------------------------
      DATA    U0/40/   ! ,U7/10/
      DATA NEL,NN/2*0/            !-- initially NO elements etc.
      DATA NDIM/0/                !-- default is 0-dimensions !

C=======================================================================
      WRITE (*,'(A)') '-------- Dan''s PhD Slip Surface Program ------'
     +               ,'Version: '//VERSION

      FILE = ' '
      CALL GET_DATA_FILE_NAME (FILE)
      open (u0,file=FILE, status='old')

C----------------- Parse the data file (s) -----------------------------
      U1 = U0                         !-- start from the base file = U0
      DO IKEYWORD = 1, 999999
        CALL GET_KEYWORD (U1,U0,KEYWORD)
        CALL PRINT_KEYWORD (IKEYWORD,KEYWORD,.false.)

c------------------------ find the '*keyword' --------------------------
      FOUND = .TRUE.

      IF (KEYWORD.EQ.'*EOF') THEN
          read*
          CALL GINEND ()
          GOTO 888                          !-- exit the program nicely
c     ELSEIF (KEYWORD.EQ.'*CONTROL') THEN

      ELSE
        FOUND=.FALSE.
      ENDIF
C----------------------------------------------------------------------

      IF (.NOT.FOUND) CALL KEY_GINO_PHD
     &    (FOUND,KEYWORD,U1,GC,IGC,NDIM,NN,NUMS,INUMS,NEL,P, DISPS)

      IF (.NOT.FOUND) CALL KEY_MESH_READ
     &          (FOUND,99,KEYWORD,U1,GC,IGC,NDIM,NN,NUMS,INUMS,NEL,P)
      IF (.NOT.FOUND) CALL KEY_MESH_IO
     &          (FOUND,99,KEYWORD,U1,GC,IGC,NDIM,NN,NUMS,INUMS,NEL)

      IF (.NOT.FOUND) THEN
        CALL MYERROR(2,'unknown keyword:'//KEYWORD)
      ENDIF  

C----------------------------------------------------------------------
      ENDDO
C------------------------- end of program ----------------------------
  888 STOP '> > >       Program completed OK  :-)       < < <' 
      END

C-----------------------------------------------------------------------
      
c... Ok main-prog is like DANMESH butput DISPS (and GC?) in COMMON
C    (tho' we will have to 'carry' this size of DISPS in the SUBS :-(
c
c   so read-in mesh + disps the try a write_as_danplot
c   if that works move on to DR_MESH .. eg. nodes and edges etc.
c   then try DR_MODEL3D
C   and hence onto DR_VECTORS as a random set
c     .. note work on CAMERA and hence move/draw .. maybe to VGA for now ?
c

c-----------------------------------------------------------------------
c  LIB2.FOR
c
c
c   Note that we can use this library without producing any graphical
c   output .. just to produce the coords of points on the failure surface
c    Library parts :
c
c     1/  DATA file reading to build the COMMON blocks and D()
c     2/  GRAPHICS entry : windowing and titling
c     3/  MESH-drawing : elements, nodes, wireframe etc.
c     4/  REGRIDDED vectors, mesh, trajectories
c     5/  RAY-casting to find the failure plane
c     6/  CONTOURING of disp/strain
c App.A/ 'old' obsolete routines
c
c App.B/ 'new' GINO-like interface for Postscript DRAWing
c
c      Revison History
c
c  1986-1989 Original code developed for my Ph.D
c  1990 Some post-PhD tidying-up 
c  1990 adapted to handle Spud-cans and their 'cyclindrical' geometry
c  2-4-94 Cleaned Fortran to DO-ENDDo, in-line comments etc.
c  3-4-94 LIB2A : towards DANPLOT style meshes : 
c
c
c-----------------------------------------------------------------------

C----------------------------------------------------------------------
      SUBROUTINE KEY_GINO_PHD (FOUND,KEYWORD,IO,GC,IGC,NDIM,NN
     &        ,NUMS,INUMS,NEL,P,   DISPS)
C
C     This Keyword based Routine handles the SLOPE SLIP FINDING
C     methods of my PhD. days.
C     .  Some of the modules are pure-GINO interfaces
C     .  Some handle windowing (eg. PAGER3)
C     .  The rest handle the SLIP circles, etc. (eg. DR_VECTORS)
C           DJK May'94 (from work 1986-1990)
C
C     note: SLOPE_MESH is duplicated from K_MESH (to put data in COMMON)
C     also: DISPS will need to be in COMMON for DR_DISP etc.
C
      SAVE         !- we need to save D(,), XYZL(), etc.

      REAL           GC (IGC,*)        ! Nodal coords
     &           ,DISPS (3,*)          ! Nodal Displacements
      INTEGER NUMS(INUMS,*), IO
      integer :: p(*)                  ! not actually used
      CHARACTER KEYWORD*(*)
     +            ,TITLES*90 !
     +            ,FILE_PCX*20    !- PCX save filename
      LOGICAL       FOUND              !- if this KEYWORD was handled here
     &             ,EXISTS             !- the PCX file already exists

!     INTEGER*2   IFAIL_2      !- for PCX saving
!     INTEGER*4   BUFFER       !- for PCX saving 

      REAL D(6,7),DD(2)        ! * Model Shape *
     &           ,B(4)         !  window boudaries (not really used)
     &           ,XYZL(7)      !  the slip-circle intescetions


c     DATA IO2/80/            !- this is for file writes I think
      DATA XYZL/7*0./

      FOUND = .TRUE.

c------------------------ Date File Reading ---------------------------
      IF (KEYWORD.EQ.'*SLOPE_MESH') THEN     !- cf. in DANLIB
        CALL R_SLOPE_MESH_1 (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL)
        CALL BOUND   (D,DD)     !- record the mesh-shape

      ELSEIF (KEYWORD.EQ.'*DISPLACEMENTS') THEN     !- cf. in DANLIB
        NODOF = NDIM
        CALL R_NODES (IO,DISPS,3,NODOF,NN,NUMS,INUMS,NEL)
        ZPMAX = 0.
        DO I =1,NN
          DO J=1,NODOF
            ZPMAX = MAX (ZPMAX,ABS(DISPS(J,I)))
          ENDDO
        ENDDO

c------------------------- GINO and Windowing --------------------------
      ELSEIF (KEYWORD.EQ.'*GINOIN') THEN        !- in (goto VGA@ etc.)
        READ (IO,*) IOP    !- 1= VGA etc.
        CALL GINOIN (IOP)
         
      ELSEIF (KEYWORD.EQ.'*SHIFHE2') THEN        !- windowing
        READ (IO,*) XL,XR,XT,XB             !- screen borders
        READ (IO,*) TITLES    !- dummy the character-string
        CALL SHIFHE2 (XL,XR,XT,XB, TITLES)

      ELSEIF (KEYWORD.EQ.'*PAGER3') THEN        !- windowing
        READ (IO,*) NQX,NQY,NG              !- page numbers
        READ (IO,*) XL,XR,XT,XB             !- screen borders
        CALL PAGER3 (NQX,NQY,NG,XL,XR,XT,XB, B)

      ELSEIF (KEYWORD.EQ.'*SUVIEW') THEN        !- clear the screen
        READ (IO,*) IV,SF                       !- eg 4, 1.
        CALL SUVIEW    (D,DD,IV,SF)

      ELSEIF (KEYWORD.EQ.'*PICCLE') THEN        !- clear the screen
c       CALL CLEAR_SCREEN@()
      ELSEIF (KEYWORD.EQ.'*PENSEL') THEN        !- choose the pen-number
        READ (IO,*) IPEN            !(read pen # directly)
        CALL PENSEL (IPEN, 1.,1.)

      ELSEIF (KEYWORD.EQ.'*GINEND') THEN        !- exit
        CALL GINEND ()


c------------------------ 'extra' keywords -----------------------------
      ELSEIF (KEYWORD.EQ.'*SET_COLOURS') THEN        !- exit
        READ (IO,*) IPAL,IOP2, NCOLS
        CALL SET_COLOURS (IPAL,IOP2, NCOLS)

      ELSEIF (KEYWORD.EQ.'*DUMP_PCX') THEN        !- exit
c
c     This saves the current VGA page to a PCX file
c
        DO IPIC=1,999
          WRITE (FILE_PCX,'(A,i3.3,a)') 'dump',ipic,'.pcx'
          INQUIRE (file=FILE_PCX, exist=EXISTS)
          IF (.NOT.EXISTS) GOTO 91
        ENDDO
   91 CONTINUE
c       CALL GET_SCREEN_BLOCK@ (0,0,639,479,BUFFER)
c       CALL SCREEN_BLOCK_TO_PCX@ (FILE_PCX,BUFFER,IFAIL_2)
c       CALL RETURN_STORAGE@ (BUFFER)
cc       CALL PCX_FIX (PICFILE,PAL)           !- do we still need to do this ?
        CALL PICCLE ()                       !- & force a new page :-)

c---------------------------- output -----------------------------------
      ELSEIF (KEYWORD.EQ.'*WRITE_D_MESH') THEN 
        CALL WR_D_AS_DANPLOT  (D,DD)

      ELSEIF (KEYWORD.EQ.'*DRAW_MODEL') THEN 
        READ (IO,*) IOP     !(read option directly)
        CALL DR_MODEL  (D, IOP,GC,IGC,NN)
      ELSEIF (KEYWORD.EQ.'*DRAW_MODEL_3D') THEN 
        CALL DR_MODEL_3D  (D,DD, GC,IGC,NN)

      ELSEIF (KEYWORD.EQ.'*RAYS') THEN    !- ray casting !
        READ (IO,*) XC,YC, IOP, NRAYS
        CALL RAY3 (XC, YC, D, IOP, NRAYS)

      ELSEIF (KEYWORD.EQ.'*DR_VECTORS') THEN    !- disp vectors
c       CALL SET_SEED@(1.)       !- ie. a repeatable sequence
        READ (IO,*) nvects, fact, iop
        CALL DR_VECT (ZPMAX,D,NVECTS, iop, FACT,  GC,IGC,NN)

      ELSEIF (KEYWORD.EQ.'*DR_DISP') THEN    !- disp. boundary 
        READ (IO,*) steplen, fact              !(eg. 1.,1.)
        DF = 0.
        DT = d(4,5)    !- location of the toe
        CALL DR_DISP (DF,DT,D,steplen, ZPMAX, FACT)

      ELSEIF (KEYWORD.EQ.'*DR_FLOWNET') THEN    !- flow-net :-)
        READ (IO,*) XC, YC
        CALL TRACE (D,ZPMAX,XC, YC)

      ELSEIF (KEYWORD.EQ.'*VECTOR_CENTRE') THEN 
        READ (IO,*) nvects,fpercent
        CALL DISPCEN (ZPMAX,D, FPERCENT, NVECTS,  XC,YC,RC)

      ELSEIF (KEYWORD.EQ.'*DR_CIRCLE') THEN 
        READ (IO,*) iop           !- eg try '4'
        CALL DRCIRC (XC,YC,RC, D,XYZL,iop,npts)

      ELSE
        FOUND = .FALSE.
      ENDIF
      RETURN
      END
C-----------------------------------------------------------------------
      

