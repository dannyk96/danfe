       

      PROGRAM     DANPLOT

c    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C          This is Dan Kidger's general purpose plotting package
c                   http://www.man.ac.uk/~mbgasdk/dansoft
c                           d.kidger@man.ac.uk
c
c    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

      CHARACTER VERSION*7
      PARAMETER (VERSION='08Jun00')
      COMMON/BIG_ARRAYS/GC,NUMS,GDISPS, DISPS,STRESSES

      PARAMETER
c     &          ( MNN = 71 000            ! Max # of nodes
c     &          ,MELS = 71 000            ! Max # of elements
c     &       ,IGDISPS = 71 000            ! Max # of displaced nodes
c     &        ,IRINGS = 200 000            ! Max # of nodes in 'polygons'
!- These should be used to ALLOCATE arrays: hence can adjust to suit.

     &          ( MNN = 100 000            ! Max # of nodes
     &          ,MELS = 100 000            ! Max # of elements
     &       ,IGDISPS = 20 000 000          ! Max # of displaced nodes
     &        ,IRINGS = 500 000            ! Max # of nodes in 'polygons'

     &       ,MSTRESS = MELS *6*8          !-  max size of stress table
     &        ,IFCETS = MELS*6       )     ! Max # of facets

c.... NEW history: (these comments shouldn't be here)
c
c  18- 6-93 : move command-line parsing into the 'main-block'
c   5- 6-93 : * * New menus and Makefiles * *

C--------------------- revision history ------------------------------

C     -- Version 2              23/9/91                Dr. Dan Kidger
C     -- was PL2.FOR  ... now auto -cycling added 27/9/91
C     -- light-source shading added  and  'c=5' (?)
C     -- palete changing ','
C     -- ver 4.0 read all load steps at once
C     -- screen border toggle '*'
C     -- polygon decompostion
C  22-10-91 DO-loops changed to DO..ENDDO structures 
C  23-10-91 4 node quad in '3D' added, + color backface facets 
C     'cursor keys' redraw bug fixed/ mouse action changed
C      color map '~' option/ '.' and ',' joined
C  24-10-91 screen based menu started.
C  30-10-91 on-screen time per frame  '"'.
C   8-11-91 2d 'flat', more menuing...
C  20-11-91 PCX fixing.. esp. palette !
C  20-11-91 dialog from top of the screen
C  25-11-91 check mouse event BEFORE key event, add abort mid-picture
C  25-11-91 sub-facet division (via shape funcs) started...
C  27-11-91 ..continued
C   5-12-91    ..continued
C   9-12-91       .. completed!
C  12-12-91 ** Gouraud shader added for disp. contouring
C  19-12-91  'G' to 'glass' a material property 
C  19-12-91  'M' to Mirror the model.
C  19-12-91  Strain-contouring begun
C   7- 1-92  Command line arguements for filename and macros added
C   8- 1-92  <<PL7>> major menu restructuring
C  25- 3-92  PL7A .. strain contouring/ 'palette' menu/ PL7A.MNU etc.
C  21- 4-92  PL7C .. GFACE restructuring.
C  28- 4-92  add comments to ALL initial data ... / .CFG config files :-)
C  30- 4-92  (push eye posn, light posn.), picture # to .() ?
C   4- 5-92  multiplle windows !
C  22- 5-92  'PL7E' FSTRIP changes for speed!
C  26- 5-92  transform and sort only 'visible' facets
C   1- 6-92  multiple plots.. link load#/view# to pic#
C   5- 6-92  shading fixed/ 'zebra' contours/ 'material look-up table'
C   9- 6-92  -ve 'I' options moved to (+ve) J :-)
C   9- 6-92  output options on 'O' <including laser-printer:-) >
C  22- 6-92  draw'n'save animation (via PCX/memory)
C  23- 6-92  ']' to get 'cut-out models patch.. -ve mats. = <ld+1
C            '[' to get built up models patch.. +ve mats  = >ld
C   2- 7-92  'NFF' file-input support added !
C  14 -7-92  FTN77 spurious-line-diagonal BUG fixed !

C   4-12-92 WOW.. what a long time ... Postscript Output :-)

C   30-3-93 PL8C_M  'extra lines' if '*.PL2'
C   31-3-93 properly mended GOURAUD for 'clipped' contours
C    1-4-93 reversed the default colours to black on white (and back!)
C   10-5-93 PL9.  attempt to add *KEYWORDS ('-k' on command-line)
c     ...... now cut into multiple source files ! ......
C    2-8-93 Made all large-array sizes INTEGER*4
C           reversed NUMS subscripts,  FACETS now (7,*)
C    6-8-93 Added logo and date to PS output, added ARCs to PS

C   1-11-93? *Regridding* added :-)
C   9-11-93 'V' to put mean nodal vector normals into GDISPS
C  16-11-93 moved FSTRIP etc. to drawing section (=streamline data reading)
C  16-11-93 changed all NUMS to the new style.
C  16-11-93 hence extend to parse keyword-files from the command-line
C  22-11-93 GCOORD renamed as GC
C  17- 2-94 SET_PAL now called SET_COLOURS and extended + several other mods
C  24- 2-94 GFACE->GET_FACE (now faster and returns FS() as Global #s)
C           CONT_VALS() list of nodal values to contour (tbc...)
C  28- 2-94 Added shifting and scaling to 'R', overload XME for presets
C           Windows and Animation now as DO-loops
C           MAJOR changes to Animation (now in CV)
C           MAJOR revamp of printer selection
C  16- 3-94 IOP_BUF now CV(38), fixed print bug
C  24- 4-94 added *NODES, *ELEMENTS, etc.
C  22- 8-94 added 'default.cfg' file, added scale-calc to *keyword data
C  26- 8-94 added 'h' to toggle cursor keys action (pan/rotate/light)
c
c  25- 1-95 SHADE does HLS :-) .. now 'use_vesa_mode@' <- vesa is bad!
c   7- 2-95 all MOUSE hamdled pushed out to SUBs
c           Hence support for SVGA w menus
c
c  12- 7-95 LH mouse marks box, so EDIT to mark nodes.. hence change IMAT
c           pop-up 'wait' boxes for Fstrip, Printing, etc. (not yet OK!)
c  11-11-95 STRESS support -part 1
c  13- 7-96 added xyz labels to axes
c  18-12-96 fixed /2. bug in stress-vector calc. | fixed 20=PCX bug.
c  22- 1-97 updated 'w' to include borders. Fixed buffering IRESCP
c   ?- 1-98 -r800 otion to set screen resolution
c   5- 8-98 Thoughts on other-than facets in the 'to draw' list., eg isosurfaces

C-------------------- Max problem sized Arrays -------------------------

c     PARAMETER (   MN = 32        !-  Max element size
      PARAMETER (   MN = 20        !-  Max element size
c     PARAMETER (   MN = 8         !-  Max element size
     &             ,MDF = 3          !- /
     &            ,M_CV = 90         !- max # of CV entries
     &         ,MIMAGES = 1024)      !- max # of stored images
                                                
      PARAMETER (INUMS = MN+7        !- # data items per element
     &             ,IGC = 3          !- max # coords per node
     &          ,IDISPS = IGC        !- # of disps per node (+Pore?)
c    &         ,MSTRESS = * MEL* (2 + MGP*MS)  !-  max size of stress table
     &   )

c----------------------- The *Big* Arrays ------------------------------
c.. hmm IGDISPS is only used to define and in READ_LOADS
c.. try a much smaller GDISPS .. when full write to disk (pointers)
c.. any more load-cases to be procssed 
c---> can do the same trick for stresses .. maybe use common storage to GDISPS
C cos I might just want to animate the stress changes
C .. also at the point consider that I may want *NODAL_VALUES with maybe six 
C    Smoothed stresses at each node. (or disps+pp)
c.. so define a one line listing what variable are in that table. 'sx', etc.

      REAL    GC (IGC,MNN)           !- Nodal coordinates
     &        ,DISPS (IDISPS,MNN)    !- Nodal disps (DataBase)
     &       ,GDISPS (MDF,IGDISPS)   !- Nodal disps (every loadcase)
     &        ,NORMS (IDISPS,MNN)    !- Normal vectors at the nodes (?)
     &     ,STRESSES (MSTRESS)       !- * Gauss-point stresses *
     &    ,CONT_VALS (MNN)           !- nodal values to contour
c    &    ,CONT_LEVS (255)           !- pre-defined contour levels
      
      INTEGER   NUMS (INUMS,MELS)    !- element steering 
     &       ,FACETS (3,IFCETS)      !- table of ALL the 3D faces
     &        ,RINGS (IRINGS)        !- poygons around each material
     &         ,PSTR (MELS)          !- pointer to each element's stress record
     &       ,ifl_nodes (MNN)        !- flags for 'hilighted' nodes (elems too?)
c    &       ,ifl_elems (MELS)       !- element flags
c    &       ,ifl_facets(IFCETS)     !- flagged facets


      REAL     ZBUF (IFCETS)         !- the 'depths' of the facets
      INTEGER   PL1 (IFCETS)         !- RSORTed order of facets/objects(local)
      INTEGER   PL2 (IFCETS)         !- list of object-points (to draw)
      INTEGER   PL3 (IFCETS)         !- list of object types

c-- Iosurfaces
      PARAMETER (IGC2=3,INUMS2=3)
      REAL GC2(IGC2,8000)            !- say for 10000 triangles max
      INTEGER NUMS2(INUMS2,10000)
c---------------------- 'Finite Element' arrays ------------------------
       PARAMETER (MGP = 27        !- max. # of GP's per element
     &          ,M_IH = 13        !- Max # of stress terms per GP (+stain+pp)
     &         ,ICOORD= MN        !- # of coords per elem
     &          ,IPRPTAB = 20     !-- Max. # of props per material
     &          ,MMATS = 199      !-- max # of materials
     &                       )
       REAL  
     &           COORD (ICOORD,3) !- coords of an element's nodes. (cf C_F)
     &      ,COORD_GPS (3,MGP)    !- coords of an element's GPs. (cf C_F)
     &         ,STR_EL (MGP*M_IH) !- complete stress record for an elem.
     &         ,DISPS_EL (ICOORD,3) !- displacements of an element
     &         ,PRPTAB (IPRPTAB,MMATS)  !-- the material properties 
     &        ,GPVALS (MGP)       !- a set of elem. GP values.
     &        ,VALS_ELEM (ICOORD) !- a set of elem. nadal values (cf  shade)
     &        ,VALS_FACET(ICOORD) !- a set of facet nodal values (cf  shade)

c---------------------------- other arrays -----------------------------
      REAL    X (20) ,Y (20)      !- lines.. eg. window-frame
     &       ,XTRA_L (4,300)       !- for 'added' lines on the drawing (MAH)
c-- need three sets of variables:
c    1: Control, eg CV
c    2: Menuing only - MENU_STRING
c    3: and mostly Rendering only, eg. C_F, C_SF, SC_SF, DI_F, etc.

C---------- the following need their subscripts reversing ! ------------
c.... if reversed then can pass 'slices' to subroutines

      PARAMETER (ISC = MN)         !- max number of nodes per facet
      REAL     C_F (ISC,3)         !- model-cords of a facet (cf COORD)
     &       ,C_SF (ISC,3)         !- model-cords of a sub-facet
     &       ,SC_F (ISC,3)         !- screen-coords of a facet
     &      ,SC_SF (ISC,3)         !- screen-coords of a sub-facet
     &       ,DI_F (ISC,3)         !- the disps of a facet
     &      ,C_F_D (ISC,3)         !- the disp'd coords of a facet
     &      ,LC_SF (ISC,3)         !- local coords of a sub-facet
     &       ,ACOL (ISC)           !- colours at the nodes ob a s-f 
     &        ,VEC (3)             !- general vector
     &      ,GC_MIN (3)            !-  min and max
     &      ,GC_MAX (3)            !-  coordinates of the mesh
     &        ,C_R (10)            !- contour limits actual/chosen + stress

c... the next is really only 1 part of the table of information needed
c... by the device drivers.. also image x,y size, etc.
      COMMON /CV/CV                !- in common only cos its used alot
      INTEGER CV (M_CV)            !- **all the colour 'tokens' **
      COMMON /MENU_STRINGS/MENU_STRINGS    !- to MENUS
      CHARACTER MENU_STRINGS(2)*90 !-for datafilename in menus etc.


      INTEGER
     &         U0,U1,U3            !- file unit numbers
     &            ,NUM (INUMS)     !- nodes of an element    
     &             ,FS (ISC)       !- node numbers of this facet
     &             ,FS6(ISC)       !- local node numbers of this facet
     &        ,IC_MATS (99)        !- colours for each material type (obs)
     &          ,CODES (20)        !- opcodes (generated by menus)
      LOGICAL 
     &     KEY_WAITING             !- keyboard has been pressed (func)
      EXTERNAL KEY_WAITING
      LOGICAL
     &             EXIT            !- to quit image-drawing
     &     ,INTERP_FILL            !- if shaded facet rather than solid IMAT
     &       ,L_OVERLAP            !- if drawn FACETS overlap the menu
     &   ,CMN_EXHAUSTED            !- any tokens left on the command-line ?
     &   ,KWF_EXHAUSTED            !- any keywords left in the input file ?
     &         ,KWFOUND            !- if a keyword is known
     &        ,VIS,VIS2            !- facet visibility flags
     &        ,vesa                !- if graphics card is VESA compatable.

C-------------- viewing parameters, etc. -------------------------------
      COMMON /PALETTE/PAL          !(why in common ?.. init, and *PALETTE
      INTEGER   PAL (3,0:255)      !- 'current' colour palette
     &     ,ID_FACE (6)            !- 0/1/2 to force draw a facet type

      REAL     EYE (3)             !- coord of 'eye'
     &        ,COA (3)             !- coord of 'centre-of-attension'
     &         ,UP (3)             !- unit vector pointing 'up'
     &      ,LIGHT (3)             !- unit vector of light direction
     &         ,PM (4,4)           !- tranformation matrix - perspect
     &         ,TS (4,4)           !- tranformation matrix - windowing
c    &         ,TT (4,4)           !- tranformation matrix - combined
     &         ,GPT (3)            !- global coord of a point
     &         ,GPT2(3)            !- global coord of a point
     &         ,COM (3)            !- global coord of element centre
     &         ,SPT (3)            !- screen coord of a point
     &        ,SPT2 (3)            !- s.c. of another point
     &        ,MBOX (4)            !- 'marked box' for zoom/select_nodes
     &         ,DC (3)             !- normal vector of a surface
     &         ,LD                 !- the load-step
c    &         ,LDD                !-  and 'anim' increment
      REAL
     &         DCOA (3)            !- Animation- move model increment

C-----------------------------------------------------------------------
      CHARACTER 
     &           CKEY *1           !- ** character for event-parsing **
     &           ,EXT *3           !- general DOS filename extension
     &          ,LINE *132         !- general line of text
     &       ,FILE_IN *80          !- ** Any sort of input file **
     &       ,FILE_OUT*80          !- ** Any sort of output file **
     &       ,PICFILE *30          !- for PCX/print output
     &     ,FILE_ROOT *30          !- for animation PCX files (cf PCX)
     &       ,KEYWORD *80          !- *NEW* current keyword
c    &          ,NTXT *4           !- 'small' string for labels
c    &           ,C_T *6           !- == ANSI CODE for 'top-left screen'
     &          ,TYPE *35          !- PS page size, eg 'A4 ' or 'A3L'
     &      ,TITLES(10)*79         !- The main titles
     &        ,TITLE_LS*79         !- A load step title
     &        ,S_term*4            !- stress-term eg 'sx','txy','mss',..

       CHARACTER
     &         CMNAM *60          !- the command-line function

c     integer*4  free_space_available@
      real free_memory               ! function (int_*.f)
      integer istr_beg,istr_end               !
      external istr_beg, istr_end, free_memory, post_menus
c     external cmnam@, free_memory   !, free_space_available@
!--------  variables for parsing the command line ---------
      CHARACTER :: filenames*1024=' ',    !- stack of input filenames
     &                  FILENAME*60 =' ', !- input datfile name
     &                  basename*60       !- base for output filenames
      integer  :: nfilenames = 0              !- count of files (only for info)
      integer :: iverb=2           !  verbosity  so can increment or decrement from here.
     &          ,igraph=0          !- 0=no graphics, 1= ..
      character :: arg*80, arg1*1, arg2*1
      integer :: ikeyword=0
      integer :: nargs, danfe_argc
      logical :: debug = .false.,     !- cf. iverb (maybe cpp this?)
     &           colorise=.true.       !- toggle if use ANSI colours




C-----------------------------------------------------------------------
C------ explicit 'short-integers' variable types for ftn77 calls
C... I should really aim to remove all of these into 'interface'
C... subroutine to avoid 'cluttering' up the main prog

      REAL  TIME (20), TIM       !- for status info

      INTEGER      !( INT*4 is now the default )
     &          BUFFER             !- memory loc. of an image
     &         ,IMAGE (MIMAGES)    !- memory locs. of stored frames
c    &         ,VSCREEN            !- memory loc. 

      INTEGER
     &         IFAIL_2            !- if a subroutine 'fails'
c    &         ,WIN1               !- HANDLE of a text window

c     &          IXM_S_2,IYM_S_2    !- mouse position 
c     &         ,IBUT_S_2           !- mouse button status
c     &         ,KEY_2              !- GET_KEY@ keycode

      INTEGER   
c    &          IRESCP             !- # bit-planes of colour (for VSCREEN)
c    &          IBITPL             !- # of col-planes in image (PCX) (obs)
     &          RES (6)            !- store of current screen sizes (obs?)
     &         ,res_wx,res_wy      !- viewport size (zero is full window)

C-----------------------------------------------------------------------
C---------------- some initial data values -----------------------------
      DATA U0/40/ ,U3/13/ ,EXIT/.FALSE./   ,iout /88/ !- output files
      DATA N_X_L /0/          !- The number of extra-lines on the mesh
      DATA TIME /20*0./       !- zap all timings
      DATA RES_wx,RES_wy/0,0/ !- viewport is full window sized.

C------------------- nul initial data sets -----------------------------
c.. maybe put these in the executables, cf zapping RINGS
      DATA
     +     NEL,NN,NDIM /0,0,0/      ! initially no elements or nodes 
     +   , NN_OLD  /-1/             ! no known nodes hence FSTRIP
     +   , NEL_OLD /-1/             ! no known elements hence FSTRIP
     +   , NLDS    /0/              ! no loadsteps present      (in CV?)
     +   , LD      /0./             ! current load-step number  (in CV?)
     +   , FACT    /0./             ! disp scale factor = as defined 
     +   , FACT1   /0./             ! disp scale factor - as used 
c    +   , FACT2   /0./             ! disp scale factor - for contouring
     +   , NFCETS  /0/              ! # facets altogether
     +   , NFCETS_D/0/              ! # facets actualy drawn

C------------------- viewing parameters --------------------------------
c.. nice to wrap up into a 'camera' model, 'lighting' model
c.. hence'objects' for setting by menu (and keyword), save/load etc.
C.. animation should allow all to be interpolated /incremented
c.. also aniso scales etc.?
      DATA
     &     AR      /-1./            ! aspect ratio for this device
     &   , FEYE    /13./            ! viewing angle          
     &   , DEYE    /-4./            ! viewing distance       + Integers ?
     &   , XME,YME /  0.,  0./      ! angle of the eye       / 
     &   , XML,YML / -30.,  15./     ! angle of light vector  +
     &   , UP      /0.,-1.,0./      ! 'up' vector = y-axis   
     &   , SSC     /1./             ! image scale factor
     &   , IROT    /0/              ! portrait/landscape
c    &   , XDS,YDS,ZDS /1.,1.,1./   ! anisotropic disp. scale factors
     &   , C_R /0.,0.,0.,1.,1.,-1.  ! contour scale factors 
     &       ,1.e6,1.e6             !  + stress vec limits too.
     &       ,.5, 0.  /             !  0.->1. for isosurface, dummy
     &   , DCOA /3*0./              ! anim move object
     &   , ITEM2C  /1/              ! - token for a CV() entry :-)
     &   , DIAG /1./                ! model diameter 21-2-98
c-----------------------------
      DATA
     &     ID_FACE  /6*0/           ! facet# to draw (0='selective')
     &   , IMAGE    /MIMAGES*-1/    ! saved images (switch off)
     &   , IMAGEN   /0/             ! # of auto-save images

      DATA
     &     CMN_EXHAUSTED /.FALSE./  ! command-line arguments ?
     &   , KWF_EXHAUSTED /.TRUE./   ! no more keywords ?
     &   , L_OVERLAP     /.FALSE./  ! if Facets overlap the menu

c-----------------------------------------------------------------------
C---------------------- Initial constants ------------------------------
c-----------------------------------------------------------------------
c     OPEN (19,file='debug')     !- switch on ONLY when necessary !
c     C_T =  CHAR(27)//'[2;2H'   !- position top-left of screen 
c     IDEST = 1                  !- to VGA screen              (in COMMON)
      FILE_IN = 'unknown'
      CALL SET_IDEST(0)         !- to graphics loaded yet
      DO I=1,2
        MENU_STRINGS(I) = ' n/a' !- = application title line / status-line
      ENDDO
      N_TITLES=0
      TITLES(1) = ' '
      TITLES(2) = ' '
      TITLE_LS = ' '


      RINGS(1) = 0
      RINGS(2) = 0      !- default is NO known RINGS around the facets

c---- set the colour palettes ----
c (note if no screen graphics then just keep in PAL)
      PAL(1:3,0:255) = 70   !- dark gray

c.. move next to plotter driver ? perhaps not
c     idev_x=800                    !- default is 640x480x356
c     idev_y=600
      idev_x=640                    !- default is 640x480x256
      idev_y=480
      vesa=.true.                   !- default is to use VESA ?
!      call put_app_s-- mateize (idev_x,idev_y)   !- and store
!tter driver ? perhaps not
      call put_app_size (idev_x,idev_y)   !- and store

      CALL SET_CV (999,999,0)       !- reset CV

c--- materials --
      DO I=1,99
        IC_MATS(I) = I              !- set material-cols to be 'themselves'
      ENDDO                         !- (currently never changed !) cf wrap_around
      PRPTAB(1:IPRPTAB,1:MMATS) = 0.  

c--- prescan the command line  --
c look for:
C     -ng  no-graphics :-)
c     -r800 set screen x-resolution n (cf the size of a X-window?)dcs
c       also cf device=ps say ? ie. user-interface screen  v. output device.
c     - screen options : resolution , #bitplanes, vesa interface
c     - help text
c       
!? start graphics here ? --
c-----------------------------------------------------------------------
!--------------- Parse the command line ------------------
c-----------------------------------------------------------------------
c..  good to support Quadrics like regexps eg hopper.o[1-23] :-)
c    hence a function get_next_name()
c       returns the next file. It is allowed to modify the list
c       hence it write hopper.o[8-23] if it wishes.:-)
      nargs=iargc()
      !nargs= danfe_argc()
      iskip=0
      if (nargs==0) then
        call usage()
        stop
      endif
      do i=1,nargs
        if (iskip>=1) then
           iskip=iskip-1
           cycle
        endif
        call danfe_getarg(i,arg)
        arg1 = arg(1:1)
        arg2 = arg(2:2)
        if (arg1 /= '-') then       !- not a prefixed option
!  hmm perhaps instead check that no options *follow* the first filename found?
          if (filenames/=' ') arg=','//trim(arg)
          filenames=trim(filenames)//trim(arg)
!         nfilenames = nfilenames+1
        else                       !----- parse options begining with '-' or '--' -----
          if (arg2=='-') then           !--- '--' =Gnu style options----
            if (arg=='--usage') then
              call usage() ; stop
            elseif (arg=='--help') then
               call help(); stop
            else
            call error('unknown argument:'//arg); stop
            endif
          else                         !-- single '-'
!           now loop if we have say '-vvv'
            do j=2,len_trim(arg)
              arg2=arg(j:j)
              if (arg2 ==' ') then
                call error ('option missing')   !(or = stdin ?)
              elseif (arg2=='h') then
                call help(); stop
              elseif (arg2=='v') then    !---- verbosity level ----
                 iverb = iverb+1
              elseif (arg2=='s') then      !silent
                 iverb = 0
              elseif (arg2=='g') then    !---- graphics option
                 igraph = igraph+1          !or read option?

              elseif (arg2=='b') then    !---- toggle colourisation
                 colorise = .not.colorise
              elseif (arg2=='t') then      !- output file type (default is no file!)
                 if (i==nargs) then
                   call usage(); stop
                 endif
                 call danfe_getarg(i+1,arg); iskip=1
!                outfile_type=arg(1:len(outfile_type))
!                select (arg)                 !-  check it is valid type
!                case ('pl'.'dp',..)          !-
              elseif (arg2.eq.'p') then   !-- palette number --
                 call danfe_getarg(i+1,arg); iskip=1
c.. or think about a contour-palatte name eg. '-pHOTIRON'
                 READ (arg,'(I5)') ICOL
                 CALL SET_COLOURS ( ICOL ,-99,CV(18))   !- cv(18)= # of cols to use
              else
                call error('unknown option:"'//arg2//'"'); stop
              endif
            enddo
          endif
        endif
        if (debug) write(*,'(i8," :",a)') i,arg
      enddo
      if (filenames ==" ") then
        call usage (); stop
      endif
      ipr=iverb          ! synonym

!      ELSEIF (LINE(1:2).EQ.'-r') THEN  !--- choose screen resolution 
!                perhaps I should support 'odd' aspect ratios ?
!                        cf. full +h+w-X-y  X-style ?
!        READ (LINE,'(2X,I5)') idev_x
!        call get_idev_y (idev_x,idev_y)    !- so deduce y
!        call put_app_size (idev_x,idev_y)   !- and store

c others.
c   explict name of the danplot.cfg file

c--------------- look for a 'default.cfg keyword file ------------------
c.. should also look in the DANPLOT home-directory too :-)
c.. and prehaps the base directory of the date file?
c.. I think that we should *prepend* this to the list of files
c   - if if append then we can set view region, zoom, etc.
c  Ok so functions push_filelist(file,iop),pop_filelist(file)
c  in push_filelist(filei,iop)
c  iop=1 = append, iop=0 prepend, iop=2 expand if foo[1-3]
c  in pop_filelist(filei,iop)
c   return empty string if all exhausted
c  iop=2  return next but dont pop
c  iop=3  return previous (cached) perhaps (cf get_curretn)
c  iop=4  return count of files ?

      arg='danplot.cfg'
      OPEN (U0, FILE=arg, STATUS='OLD', IOSTAT=IOSTAT)
      CLOSE (U0,iostat=iostat2)
      IF (IOSTAT.EQ.0) THEN     !- if it exists , *prepend* to the list
        if (filenames/=' ') arg=trim(arg)//','
        filenames=trim(arg)//trim(filenames)
        CMN_EXHAUSTED=.false.
      ENDIF

C-------------------------- into graphics ------------------------------
c... and 'other' initialisation ..
c.. Be careful of going into graphics if the keyword-file says "no-graphics"
c.. so first assimilate the command-line. scan for -nographics

      CALL graphics_start (vesa)
      IDEST=1                         !- why do this ?
c     so draw a splash screen while we are initialiising ?
c     call danplot_splash_screen()
      
c-------------- set up the initial colour palette(s) ----------------
c need to initiate pgplot first ?
      CALL SET_COLOURS (0,-99,13)         !- start from standard VGA
      N_CONTS = CV(18)
      CALL SET_COLOURS (11,-99,13)        !- my 'materials' palette (DACS too)
      CALL SET_COLOURS (27,-99,N_CONTS)    !- & shaded materials
      CALL SET_COLOURS (10,-99,13)        !- default menu colours
      CALL SET_COLOURS (12,-99,N_CONTS)    !- & 'rainbow' for contours

      CALL STANDARD_MENU_FONT (-1)        !- reset the font ?
      L_OVERLAP = .true.                  !- so we post the menus

c.. maybe only show the logo if we have no data file (or if NEL=0  !!)
c.. nice to have an animated logo .. or a logo *before* we post the menus
      CALL RESET_SIZES  (RES)             !- ie x,y sizes of the window
      mbox(1) = res(4)
      mbox(2) = res(5)                    !- zoom-box == screen window
      mbox(3) = res(4) + res(1)
      mbox(4) = res(5) + res(1)
c     CALL DANPLOT_LOGO (RES,version)     !- and fill with some info

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C        S T A R T    O F    T H E    M A I N    L O O P
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

 1110 CONTINUE       !- return-point *after* a redraw/ event
 1111 CONTINUE       !- return-point if no event was found
      call set_idest(1)           !- back to the screen ?
      call reset_sizes(res)       !- just in case it got munged 

c------------------- create the status line etc. -----------------------
c... either write to screen explicitly
c... or pass to POST_MENUS as a COMMON BLOCK :-(
c also: ?
c    #materials, #load steps, #stress tables, NDIM
c  # of commands-issued
c  # of keywords so far :-)

      WRITE (MENU_STRINGS(1), '(t12,5a)') 
     &  'Danplot ',VERSION,' -', FILE_IN(1:istr_end(FILE_IN))
c      now centre this string ? 
      WRITE (MENU_STRINGS(2), 
     &       '(I1,A, I6,A,I6,A  ,I2,A   ,i3,a
     &       ,I5,A,I5           
     &       ,I3,A,I3,   F7.2,A  
     &      , F6.2,A   )')            
c     &   'NN=' , NN , ' (',NINT(100.*NN /MNN ),'%)'
c     &  ,'NEL=', NEL, ' (',NINT(100.*NEL/MELS),'%)'
     +   ndim,'D',NN,'n',NEL,'e'
     +   ,nlds,'L', ikeyword,'K'
     + , nfcets_d,'/',nfcets  !  , ' <',CKEY,'>'
     + , nint(XME),'-',nint(YME)     !- light too?

     + ,FREE_MEMORY(1), 'Mb'
     + ,TIME(5)-TIME(1),'s'                    !- image draw time
c    + ,'   ','RPL'(cv(30)-1:cv(30)-1)         !- panning mode

c------------------------- post up the menus ---------------------------
c1111 CONTINUE       !- return-point if no event was found
!- L_OVERLAP is set if say an object is drawn bigger than the window
      IF (L_OVERLAP) THEN
        CALL POST_MENUS (1, 0,0,Codes) !- if obscured
        L_OVERLAP = .false.
      ENDIF

c      PRINT*,C_T       !- to the origin of the screen :-(
c       call set_cursor_pos@ (15,3)
c..... First we should test to see if we have an 'event' in the 
c..... currently opened file
c..... if NOT then see if there is another file in the command line
c     CALL SLEEP@(.1)              !- just a little rest :-)

c-----------------------------------------------------------------------
c-------           Pickup the next data filename            ------------
c-----------------------------------------------------------------------
c.. if a keyword-file is open OR command-line args still waiting
c.. if .NN_OLwiting .. handle this first ?? ( so can draw as we read load-steps)

! do next if current read file is empty (cf nesting)
      IF (KWF_EXHAUSTED.and. .not.CMN_EXHAUSTED) THEN
! so why is the next block here ?
       if (filenames==' ') then        !- no more files to process
!        CMN_EXHAUSTED = .TRUE.        ! flag as such
         GOTO 901                      !- so redraw image ?
       endif
        ipos = index(filenames,',')
        if (ipos>=2) then
          filename  = filenames(:ipos-1)
          filenames = filenames(ipos+1:)
        else                            !- last time 
          filename = trim(filenames)
          filenames=' '
          CMN_EXHAUSTED = .TRUE.        ! flag as such
        endif
!-- now can handles extra flags like say '-d' ?
        if (iverb >=2)
     &  print*,"Processing '",trim(filename),"'"
        file_in=filename

!--- hmm allow for tokens embedded here too ? - eg -d -------
!cf munged when we read the command line above ?
          OPEN (U0,FILE=FILE_IN,STATUS='OLD',iostat=iostat)
          IF (IOSTAT.NE.0) THEN    !------- Invalid file name ---------
            WRITE (line,'(A)') 
     & '**** "'//trim(FILE_IN)//'" not found <CR>'
            if (iverb>=2) print*,trim(line)
            CALL POST_POP_UP (LINE)
c.. pop-ups need an 'OK' button (or <CR>) , or even CANCEL <esc>
c.. even a title line for them would be nice. (cf CLEARWIN of course)
c           CALL SOUND@ (512,16)
            READ*
            CALL POST_POP_UP (' ')
            GOTO 1110                !- loop back   (n/a) ?
          endif

! now get the file type, consider using:
!   - the filename extension
!   - a #! or other cookie  (cf cf. Postscript)
!       CALL GET_MESHFILE_TYPE (filename, EXT)          ! 6-12-96

        U1 = U0                    !- Start from the base file = U0
!   if a keyword file then ...
        KWF_EXHAUSTED = .FALSE.    !- now have a file to read from
        IKEYWORD=0
!   else set flag for other date file types.
!- no need to loop back - just continue below
          GOTO 1111                  ! loop-back (so handle)

      ENDIF   !- block of code for getting a new data file

c-----------------------------------------------------------------------
c----------------- Parse a Fixed format data file ----------------------
c-----------------------------------------------------------------------
c-  if KWF_EXhausted is true but the file format isnt danfe keywords .. 

c- I could consider some of these formats to have either one kwyword 
c for the whole file, or one keyword per section (hence loop around)
c  latter give bme greater control over say multiple loadsteps.
        ext=" "
        if (ext.ne." ") then
!- pre-read the extra lines 
! (cf putting these in a seperate file ?)
!  hence allow multiple files for various objects ?
        IF (EXT.eq.'PL2') THEN    !- patch to read 'extra lines' :-(
          PRINT*,'reading Extra lines.....'
          READ (U1,*) N_X_L , ((XTRA_L(J,I),J=1,4),I=1,N_X_L)
        ENDIF

c------------------ read a 'fixed format' meshfile ---------------------
c.. hmm BOVIDA's RESBOxx files will pass through here too .
C.. so read disps and stresses (a bit like if .PL)
        NLDS = 0         !- reset
        FACT = 0.
c ipr=0 to suppress output onto the graphics screen
        CALL R_MESHFILE (U1,iverb,GC,IGC,NDIM,NN,NUMS,INUMS,NEL,PL2,EXT)
        NODOF = NDIM

c----------------------- handle special cases --------------------------
        IF (EXT.eq.'pl'.or.EXT.eq.'pl2') THEN  !- read disps 
          CALL GET_MESH_RANGE (GC,IGC ,NN,NDIM, GC_MIN,GC_MAX,DIAG)
          CALL READ_LOADS (U1,1,NLDS,GDISPS,MDF,IGDISPS,NODOF,NN)
          CALL GET_DEFAULT_DISP_SCALE (GDISPS,MDF,NN,NODOF
     &                                         ,NLDS,DIAG,FACT)
          LD = NLDS                     !/ show the LAST load-step /!

!--  obsolete but consider here as an example
        ELSEIF (EXT.eq.'P59'.or. EXT.eq.'P68') THEN    !- S+G's Book5.9 (Jan 98)
          CALL GET_MESH_RANGE (GC,IGC ,NN,NDIM, GC_MIN,GC_MAX,DIAG)
          CALL R_DISPS (U1,GDISPS(1,NLDS*NN+1),MDF,IDISPS, NODOF,NN)  !- generic
c.. now should loop back and so read the next load step?
          NLDS = NLDS + 1
          CALL GET_DEFAULT_DISP_SCALE (GDISPS,MDF,NN,NODOF
     &                                         ,NLDS,DIAG,FACT)
          LD = NLDS                         !/ show the LAST load-step /!
c.. now should read the stresses
c.. but note that the order is no longer the same :-(

        ELSEIF (EXT.eq.'geom'.or.EXT.EQ.'geo'.or.EXT.EQ.'g') THEN 
C------------------ pre-build vector normals for OFF files -------------
c.. maybe can do for ALL elements (cf NFF,OFF,etc.)
c.. really if we have ITYPE=9 then we can build the vector-normals
c.. just for those sort of elements ??
C  << Note: this is for command-line OFF files : *not* IMPORT_FILE :-(

          NLDS = 1               !- we now have some "displacements" :-)
          LD   = 1.
!- steo in 'norms' or in GDISPS ??
          CALL GET_NORMALS (GC,IGC,NDIM,NN,NUMS,INUMS,NEL,NORMS,MDF,PL2)
          FACT = 0.

!-- obsolete really?
        ELSEIF (EXT.eq.'BO2') THEN       !- BOVIDA's 'RESBOxx' 
c.. next is only so we have DIAG ready for the DISP scale calc.
          CALL GET_MESH_RANGE (GC,IGC ,NN,NDIM, GC_MIN,GC_MAX,DIAG)
c.. handle multi-steps, but note only one STRESS table is stored.
c.. where do we check for overfull_gdisps?
          CALL R_DS_BOVIDA (U1,GDISPS(1,NLDS*NN+1),MDF,NN,NODOF,
     &     STRESSES,MSTRESS,lstresses, PSTR,NEL)
          NLDS = NLDS + 1
          CALL GET_DEFAULT_DISP_SCALE (GDISPS,MDF,NN,NODOF
     &                                         ,NLDS,DIAG,FACT)
          LD = NLDS                         !/ show the LAST load-step /!

        ELSEIF (EXT.eq.'UCD') THEN
        ENDIF         !- special cases for.PL,.GEO, .BO2 files

        KWF_EXHAUSTED = .TRUE.       !- all done
        CLOSE (U0)
        goto 901      !- straight to re-draw ?
      ENDIF         !- special cases for.PL,.GEO, .BO2 files
  


c-----------------------------------------------------------------------
C------------------ Parse a Keyword data file --------------------------
c-----------------------------------------------------------------------
c.... get the next keyword: may skip out  at EOF or return keyword
c.... should we handle *QUICKPLOT ??

      IF (.NOT.KWF_EXHAUSTED) THEN     ! gotta file to read
        IKEYWORD = IKEYWORD + 1
        CALL GET_KEYWORD (U1,U0,KEYWORD)
        if (iverb>=3)          !- or use colourisation
     &   CALL PRINT_KEYWORD(IKEYWORD,KEYWORD,.FALSE.)
!     !  WRITE (   *,'(I3,A,A)') IKEYWORD, ' : ',KEYWORD
        if (iverb>=2) then          !- or use colourisation
          WRITE (LINE,'(I3,A,A)') IKEYWORD, ' : ',KEYWORD
          CALL POST_POP_UP (LINE)
        endif

c------------------------ find the '*keyword' --------------------------
c.. at this stage, its nice to report the # of keywords found.
c.. if = 0 then must be a 'duff' keyword file
      KWFOUND = .TRUE.                !- assume that we will find it
      IF (KEYWORD.EQ.'*EOF') THEN
          KWF_EXHAUSTED = .TRUE.       !- all done
          CLOSE (U0)
          IF (IKEYWORD.LE.1) THEN
            CALL POST_POP_UP 
     &      ('** The input file did not contain any keywords ! <CR>')
            READ*
          ENDIF
          goto 901     !- fire a redraw at the end of each datafile read
C-----------------------------------------------------------------------
c hmm be careful of *CONTROL if different packages have different options.
      ELSEIF (KEYWORD.EQ.'*CONTROL') THEN
        CALL R_OPTIONS (U1,CV)                  !- read options

      ELSEIF (KEYWORD.EQ.'*PALETTE') THEN    
        CALL R_PALETTE (U1) !,PAL)                 ! PAL is a dummy
c       CALL SET_ALL_DACS ()
      ELSEIF (KEYWORD.EQ.'*PALETTE_HLS') THEN  
        CALL R_PALETTE_HLS (U1) !,PAL)
c       CALL SET_ALL_DACS ()

c--------------------------- titles, etc. ------------------------------
      ELSEIF (KEYWORD.EQ.'*JOB_TITLES') THEN  
! No! - call a subroutine
!     - and check for opening quotes on each lines (or at least line(1:1)=' '
        DO I=1,10
          READ (U1,*,iostat=ios) TITLES(I)
          IF (ios.ne.0) GOTO 9123
        ENDDO
 9123   IF(I.NE.11) BACKSPACE (U1)   !-- point back to the next *KEYWORD
        N_TITLES = I-1
        WRITE(*,'(A)') (char(27)//'[1;33m'//
     &                 TITLES(I)//char(27)//'[0m',I=1,N_TITLES)
      ELSEIF (KEYWORD.EQ.'*LOAD_STEP_TITLE') THEN  
        READ (U1,'(A)') TITLE_LS
        IF (TITLE_LS.EQ.' ') TITLE_LS = ' --- No title was given --- '     
        WRITE(*,'(A)')  char(27)//'[1;33m'//TITLE_LS//char(27)//'[0m'
      ELSEIF (KEYWORD.EQ.'*LOAD_CASE_NUMBER') THEN  
        READ (U1,*) LC_number         !- not actualy used by Danplot
      ELSEIF (KEYWORD.EQ.'*LOAD_STEP_NUMBER') THEN  
        READ (U1,*) LS_number         !- not actualy used by Danplot

c      ELSEIF (KEYWORD.EQ.'*VIEW_') THEN  
c        READ (U1,*) xme,yme
c.. avoid COA as it is not general ?
      ELSEIF (KEYWORD.EQ.'*VIEW_CAMARA') THEN    !- cf STARBASE order.
        READ (U1,*) xme,yme, FEYE, up(1:3)
      ELSEIF (KEYWORD.EQ.'*VIEW_WINDOW') THEN   
c       READ (U1,*) win_xo, win_yo,win_xw, win_yw, ssc
        READ (U1,*) res_wx,res_wy
      ELSEIF (KEYWORD.EQ.'*VIEW_LIGHT') THEN  
        READ (U1,*) xml,yml
      ELSEIF (KEYWORD.EQ.'*VIEW_ANIMATION') THEN  
        READ (U1,*) DCOA(1:3)        ! (much of the rest are CV integers)

      ELSEIF (KEYWORD.EQ.'*CONTOUR_LIMITS') THEN  
        READ (U1,*) c_r(3),c_r(4),c_r(7)  !- contour limits + vecs.

C-------------------------- Stresses & Strains -------------------------
c.. 11-11-95 : initial support : I will probaly change the format several
c.. times until I get the 'final' style.
c   * Urgent needs * : to plot 1/ tensile zones, 2/ Mob Sh-Str 
c     then S1,S2 lines, smooth contours ,'zone' a GP marker to a 1/3 elem.
C .. for now only store one stress table

      ELSEIF (KEYWORD.EQ.'*MAT_PROPS') THEN
        CALL R_PROPS_TOKEN (U1, PRPTAB,IPRPTAB,99, NMATS, 0)

      ELSEIF (KEYWORD.EQ.'*STRAINS') THEN  
c       .. currently ignored .. cf direct strains from the nodal disps.

      ELSEIF (KEYWORD.EQ.'*STRESSES') THEN  
c.. where do we zap this table if we don't have any stresses (eg an OFF file)
        do iel=1,nel           !- first kill the data structure ?
          pstr(iel) = 0
        enddo
        lstresses = 0
        CALL R_STRESSES (U1,STRESSES, mstress,lstresses, pstr,nel)
c       .. now maybe extract the min/max of each component ?

C-----------------------------------------------------------------------
      ELSEIF (KEYWORD.EQ.'*PLOT_MESH') THEN   !- within DANPLOT
        GOTO 901                              !- do internally
        
      ELSEIF (KEYWORD.EQ.'*DRAW') THEN        !- so cause a redraw
        GOTO 901

      ELSEIF (KEYWORD.EQ.'*PRINT') THEN      
c       IDEST= -1                             !- select 'printer' ?
        IDEST= cv(47)                         !- 
        GOTO 901

      ELSEIF (KEYWORD.EQ.'*QUIT') THEN        !- exit from the program
        GOTO 999

C------------------------ Image Reading --------------------------------
C.. a PCX image .. I will have to read the VGA palette by hand !
c.. nice to read to the centre of the screen (clip?)
c   then save as a 'normal' image background (but cf SVGA background)
c   .. so can 'centre', 'tile', etc. 
c   .. careful of 256 col palettes ... cut to 100 (say) and shift to 
c      the range 101->200, so contouring etc are unaffected
c.. nice to save the PCX palette with the image .. = 100x3 = only 300 bytes

      ELSEIF (KEYWORD.EQ.'*READ_PCX') THEN       !- read an image
        READ (U1,*) I,PICFILE
        print*,' Not supported under PGPLOT'
c       CALL PCX_TO_SCREEN_BLOCK@ (PICFILE, IMAGE(I), LINE, ifail_2)
!       IF (IMAGE(I).EQ.-1) THEN
!         CALL MYERROR (1,'invalid PCX file')
!       ELSE
!         CALL READ_PCX_PALETTE (PICFILE,PAL, IX_PCX,IY_PCX)
!         CALL RESTORE_SCREEN_BLOCK@ 
!    &       (RES(4)+ (RES(1)-IX_PCX)/2     !- to screen centre
!    &      , RES(5)+ (RES(2)-IY_PCX)/2 ,IMAGE(I),0,IFAIL_2)
!         CALL SET_ALL_DACS ()
!         read*
!       ENDIF

C-----------------------------------------------------------------------
c... for now this just uses the *NODES style DANPLOT format
c... cf *ALL_DISPS, (what would this be ?)
C     *DISPLACEMENTS : NN lines of NODOF data each
C     *NODAL_VALUES  : has column headers)
C     *EIGENMODES    : each line is all the freedoms in the mesh (+eval)

      ELSEIF (KEYWORD.EQ.'*LOADS') THEN     
        NLDS = NLDS+1
          WRITE (LINE,'(A,i4)')  'Reading Applied loads', NLDS
          call post_pop_up (line)
        NODOF = NDIM            !- be careful here
        CALL R_NODES (U1,DISPS,IDISPS,NODOF,NN,NUMS,INUMS,NEL)

c.... 11-11-95 now store this table into our database ...
C (there is no need for GDISPS to be 2-dim, a single vector is better)
c (hence a pointer list for the entry for each load step)
C   .. store NODOF etc with the table ?
        DO J=1,NODOF
          DO I=1,NN
            GDISPS(J,(NLDS-1)*NN+I) = DISPS(J,I)
          ENDDO
        ENDDO
        LD = NLDS                     !/ show the LAST load-step /!

      ELSEIF (KEYWORD.EQ.'*NF') THEN     
        NLDS = NLDS+1
          WRITE (LINE,'(A,i4)')  'Reading Boundary fixities', NLDS
          call post_pop_up (line)
        NODOF = NDIM            !- be careful here
        CALL R_NODES (U1,DISPS,IDISPS,NODOF,NN,NUMS,INUMS,NEL)

c.... 11-11-95 now store this table into our database ...
C (there is no need for GDISPS to be 2-dim, a single vector is better)
c (hence a pointer list for the entry for each load step)
C   .. store NODOF etc with the table ?

        DO J=1,NODOF        !----- invert so only the BCs are non-zero.
          DO I=1,NN
            DISPS(J,I) = 1.-DISPS(J,I)
          ENDDO
        ENDDO

        DO J=1,NODOF
          DO I=1,NN
            GDISPS(J,(NLDS-1)*NN+I) = DISPS(J,I)
          ENDDO
        ENDDO
        LD = NLDS                     !/ show the LAST load-step /!

      ELSEIF (KEYWORD.EQ.'*DISPLACEMENTS') THEN     
        NLDS = NLDS+1
          WRITE (LINE,'(A,i4)')  'Reading Load step', NLDS
          call post_pop_up (line)
        NODOF = NDIM
        CALL R_NODES (U1,DISPS,IDISPS,NODOF,NN,NUMS,INUMS,NEL)
c.. the next lines were somehow missing - fixed 22-1-98
        DO J=1,NODOF
          DO I=1,NN
            GDISPS(J,(NLDS-1)*NN+I) = DISPS(J,I)
          ENDDO
        ENDDO
        LD = NLDS                     !/ show the LAST load-step /!
        CALL GET_DEFAULT_DISP_SCALE (GDISPS,MDF,NN,NODOF,NLDS,DIAG,FACT)

      ELSEIF (KEYWORD.EQ.'*DISPS_RAW') THEN     
c 29-9-98 hmm.  generically I need to be able to read disps and 
c  stresses, etc. from within another module, and then have Danplot 
c  recognise that this has happened and so maintain its concept of a 
c  database of multiple load steps (disps AND stresses).  
c
        NLDS = NLDS+1
        WRITE (LINE,'(A,i4)')  'Reading Load step', NLDS
        call post_pop_up (line)
c       NODOF = NDIM
        CALL R_DATA_JASPER (U1,DISPS,IDISPS,NODOF,NN,NUMS,INUMS,NEL)
c       CALL R_DATA_JASPER (U1,DISPS,IDISPS,NODOF,NN)
        DO J=1,NODOF
          DO I=1,NN
            GDISPS(J,(NLDS-1)*NN+I) = DISPS(J,I)
          ENDDO
        ENDDO
        LD = NLDS                     !/ show the LAST load-step /!
        CALL GET_DEFAULT_DISP_SCALE (GDISPS,MDF,NN,NODOF,NLDS,DIAG,FACT)

      ELSEIF (KEYWORD.EQ.'*SPOT_HEIGHTS') THEN  !- BNFL?   
        NLDS = NLDS+1
          WRITE (LINE,'(A,i4)')  'Reading Load step', NLDS
          call post_pop_up (line)
        NODOF = NDIM         !(just in case)? 
        CALL R_HEIGHTS (U1,DISPS,IDISPS,NODOF,NN)
        LD = NLDS                     !/ show the LAST load-step /!

c ?? what about letting ANALYSE do the storing so can just about 
c  parse a complete DANFE input file?

c.... 11-11-95 now store this table into our database ...
C (there is no need for GDISPS to be 2-dim, a single vector is better)
c (hence a pointer list for the entry for each load step)
C   .. store NODOF etc with the table ?
        DO J=1,NODOF
          DO I=1,NN
            GDISPS(J,(NLDS-1)*NN+I) = DISPS(J,I)
          ENDDO
        ENDDO
        LD = NLDS                     !/ show the LAST load-step /!
        CALL GET_MESH_RANGE (GC,IGC ,NN,NDIM, GC_MIN,GC_MAX,DIAG)
        CALL GET_DEFAULT_DISP_SCALE (GDISPS,MDF,NN,NODOF,NLDS,DIAG,FACT)

c-----------------------------------------------------------------------
c.. maybe read DISPS then copy to the GDISPS data structure.
      ELSEIF (KEYWORD.EQ.'*EIGENMODES') THEN     
        call post_pop_up ('Reading Eigenmode data')
        NODOF = NDIM
        DO KK = 1, NN*NDIM    !--- assume all eigenmodes are here --
          READ (U1,*) II, EVAL, ((GDISPS( J ,(KK-1)*NN+I)
     &                ,j=1,NODOF),I=1,NN)
          NLDS = KK
        ENDDO
        LD = NLDS

c... 14-10-94 before we had no DIAG for disp_scale calc !
        CALL GET_MESH_RANGE (GC,IGC ,NN,NDIM, GC_MIN,GC_MAX,DIAG)
        CALL GET_DEFAULT_DISP_SCALE (GDISPS,MDF,NN,NODOF,NLDS,DIAG,FACT)

c.. read the palette as col=, then RGB as (0->255) ?
c..... nice to handle FRACTINT style .MAP files too
C. we will also have CONTROL as "NXE=..", etc. ?

C-----------------------------------------------------------------------
c.. hmm palette is an INTEGER table 
c     ELSEIF (KEYWORD.EQ.'*PALETTE') THEN
c       CALL R_PALETTE (U1,PAL)                  !- read colour-entries

C-----------------------------------------------------------------------
      ELSEIF (KEYWORD.EQ.'*SLEEP') THEN
        READ (U1,*) TIM                         !- just a 'pause'
c       CALL SLEEP@ (TIM)
C-----------------------------------------------------------------------
c... this is as in K_MESH but we reset eye params too.
c perhapsbetter to do this if we 'spot' that NDIm has changed.
c      ELSEIF (KEYWORD.EQ.'*TWO_DIMENSIONAL') THEN
c        NDIM = 2
c        XME    = 0.   !- x-eye position
c        YME    = 0.   !- y-eye position

C-----------------------------------------------------------------------
c      ELSEIF (KEYWORD.EQ.'*THREE_DIMENSIONAL') THEN
c        NDIM = 3
c        XME    =-30.  !- x-eye position
c        YME    = 15.  !- y-eye position

C-----------------------------------------------------------------------
c... not needed if material bounds are needed as these are now automatic.
c   but can also be used for adjacent objects, loading, container etc.
      ELSEIF (KEYWORD.EQ.'*EXTRA_LINES') THEN      !- colour ?
c       .. really should append to the current list rather than replace
        READ (U1,*) N_X_L , ((XTRA_L(J,I),J=1,4),I=1,N_X_L)

C---------------------- user-supplied keywords -------------------------
      ELSE
        KWFOUND = .FALSE.
      ENDIF

C--------------------- 'standard' keywords -----------------------------
      IF (.NOT.KWFOUND) CALL KEY_MESH_READ
     &        (KWFOUND,0,KEYWORD,U1,GC,IGC,NDIM,NN,NUMS,INUMS,NEL,PL2)

      IF (.NOT.KWFOUND) CALL KEY_MESH_IO
     &        (KWFOUND,0,KEYWORD,U1,GC,IGC,NDIM,NN,NUMS,INUMS,NEL,PL2)

      IF (.NOT.KWFOUND) CALL KEY_FACETS
     &        (KWFOUND,0,KEYWORD,U1,GC,IGC,NDIM,NN,NUMS,INUMS,NEL,PL2,
     &         FACETS,IFCETS,NFCETS, RINGS,IRINGS)

C-----------------------------------------------------------------------
c  hmm keep the mesh size up to date ? (so DIAG is ok for DISP scale
c really do this if NN has changed by any means
      IF (KEYWORD.EQ.'*NODES') THEN       !- OR IF nn /= nn_old
        CALL GET_MESH_RANGE (GC,IGC ,NN,NDIM, GC_MIN,GC_MAX,DIAG)
      ENDIF
c  IF (NDIM.NE.NDIM_OLD) THEN
C  NDIM_OLD=NDIM
C  ENDIF
C.. note here that in DANMESH if we remember the last 5 NNs and NELs, 
c      then it is simple to select any newly added mesh parts 
c      eg. to SHIFT them. :-)

C-----------------------------------------------------------------------
      IF (.NOT.KWFOUND) THEN
        PRINT*,'KEYWORD :',KEYWORD,'was not found !'
      ENDIF
C-----------------------------------------------------------------------
        call post_pop_up (' ')      !- kill any status pop-up
        goto 1111  !- back up to read next keword ? (NO .. its OK?)
      ENDIF    !- block-if wrapper for '.NOT.KWF_EXHAUSTED'


c-----------------------------------------------------------------------
c----------------- Keyboard or mouse_on_menu Evemt ---------------------
c-----------------------------------------------------------------------
c ... hence support for cursor-keys for the cross-hairs ?
c.. hence if so call 'menu' to get the KEY you wanted
c    ( MENU should be able to return a long string or integer array)
c    .. drag 'n drop events are harder!

      CALL GET_MOUSE_OR_KEY_EVENT (IXM,IYM, CODES,POST_MENUS)

      IF (CODES(1).EQ.0) THEN      !------ no key or menu-hit ------
        GOTO 1111                    ! loop-back

      ELSEIF (CODES(1).LT.0) THEN  !----------- keyboard -----------
        CODES(1) = ABS (CODES(1))           !- back to +ve
        CALL MUNG_KEY_CODES (codes)         !- Mung keypress to opcodes

      ELSEIF (CODES(1).GT.0) THEN  !---------- mouse/menu ----------
c.. Q: Dont we already have the all the info in CODES ?
c.. A: Not quite.. eg event may be a post_menu...
        if (iverb>=4) 
     &  print*, 'menu hit=',codes(1)
        CALL POST_MENUS (2,IXM,IYM,Codes)    !- get the opcode ?
      ENDIF

      CKEY = CHAR (CODES(1))     !- a character is nicer to handle
      KEY2 = CODES(2)            !- 2nd opcode too

c-----------------------------------------------------------------------
c.. OK I may be here if I generate CKEY from the command-line ?
      IF (CKEY.EQ.' ') GOTO 1111    !--- try again   .. why ?

C-----------------------------------------------------------------------
C------------------------ key event handling ---------------------------
C-----------------------------------------------------------------------

! **OBSOLETE** 29/03/15 This label never used.
! 1112 CONTINUE      !- got an event from MOUSE or KEYBOARD

      KEY = ichar (ckey)  !- ascii-value of the 'KEY'
      if (iverb>=3) 
     &WRITE (*,'(A,A,i3,i4,A,$)') '<',ckey,key,key2,'>'   !- debug
 
c-----------------------------------------------------------------------
      IF (KEY.eq.0) THEN            !- no-op

!- next is probably obsolete 
!- if we did what it - then I guess we should push the filename onto filenames()
!  this avoids data file opeinign in multiple places inthe code
      ELSEIF (KEY.ne.13.and.KEY.GE.1.AND.KEY.LE.26) THEN     !- control-A ..etc.
        arg='alt_'//char(key+ichar('a')-1)//' .key'
        if (filenames/=' ') arg=','//arg(1:len_trim(arg))
        filenames=trim(filenames)//arg(1:len(arg))
        CMN_EXHAUSTED=.false.

      ELSEIF (CKEY.eq.'z') THEN    !- no-op

      ELSEIF (CKEY.eq.'_') THEN    !- draw image    ' ' aliases to '_'
        GOTO 901         !----- Jump straight to    * Image_Draw *

C------------------ DRAW or PRINT or SVGA re-draw ----------------------
C.. 0= 'Normal VGA screen'(1) (cf 2 for non-interupt)
C.. 1= 'SVGA redraw'     (10) .. actual mode will come from CV
C.. 2= 'Print'           (-1) .. actual device will overload this
c---> how does this fit in with CHOOSING the output destination ?
      ELSEIF (CKEY.EQ.'P')THEN
        IDEST = KEY2
        if (idest.eq.999.or.idest.eq.-99) then  !- use the pre-defined output device
          idest = cv(47)        !- hmm does MENU always return this?
        endif
        GOTO 901         !----- Jump straight to    * Image_Draw *

C------------------- output destination choice -------------------------
c-- really at this stage, all we are choosing is the output destination.
c-- some need secondary options: eg. dpi for HPLJ, port/land for PS
c... note I could also specify the SVGA mode this way too ?
c... or even to the VGA with an image size

c-- * push to a subroutine , hence call from 'other' graphics programs
c   = 'select driver'.. need menu version urgently
C   contrast 'select printer', with 'configure printer'
c.. eg latter for PS file/lpt1: and image size, filename etc.

c.. so really here we are :
c.. 1: choosing the current printer (default = PS)
C.. 2: for selected can change the default settings
c         - image size/resolution
c         - destination: laser=lpt1, ps=file.ps, pcx=file.pcx, ..
c         - orientation (ps only?)
c.. some settings are unchangable, eg aspect ratio, #cols for lasers

c.. so when we hit the 'print' button, just call up the 'select printer'
C.. subroutine (that holds its own parameters?).
c   At the end call 'close_printer', and revert back to the VGA driver?


      ELSEIF (CKEY.EQ.'O')THEN
        IF (KEY2.eq.-99) then
          PRINT*,'Output destination ? (30)'
          PRINT*,'(20=PCX, 30=PS, 40=HP-LJ, 50=HP-GL, 60=dot-'
     &     //' matrix, 70=DXF, 80=WMF)'
          READ*,key2
        ENDIF       

      IF (KEY2.EQ.20) THEN    !----------- PCX output --------------
c       .. I could allow a menu to pass KEY2=31 (say) to give 640x480
c.. Ok 30=640x480, 21=800x600, .. 29=user-defined (# colours?)
        CV(47) = KEY2            ! 18-12-96 .. this line was missing
        PRINT*,' Enter the desired PCX image size (eg. 800x600)'
        READ*, CV(48),CV(49)
        PRINT*,' as 16 or 256 colour file ? (16,256)'
        READ*,CV(50)
c       CV(51) =-1 *100    !(aspect ratio)    !--do I invert y ?
        CV(51) = 1 *100    !(aspect ratio)    !--do I invert y ?

      ELSEIF (KEY2.EQ.30) THEN    !----------- Postscript output -------
        CV(47) = KEY2
        CV(48) = 2560        !- what are these sizes ??
        CV(49) = 6360
        CV(50) = 256         ! # of colours  (really 'tis infinite)
        CV(51) = 1 *100      ! Aspect Ratio

      ELSEIF (KEY2.EQ.40) THEN    ! ---> laser-printer output

c       READ*,IPRES     !-- hmm maybe force to 150 dpi ?
c.... maybe just call SEL_PCL_P here to get the image size ?
c.. or have a 'set' of PCL codes at different resolutions
c.. also paper size (A3/A4) implies # of pixels (PCL and HPGL)

        CV(47) = KEY2
        CV(48) =  8 * 150    ! check these sizes
        CV(49) = 11 * 150    !
        CV(50) =  2           ! # of colours ? (or dpi instead ?)
        CV(51) = -1 * 100     ! Aspect Ratio

      ELSEIF (KEY2.eq.50) THEN         ! ---> HP7550 plot output
c       call set_dev_size (50,  'A4', 10870/10, 7600/10, 8, 100%)
c ... also filename etc. 
c ... cf get_device_capabilities
        CV(47) =    KEY2
        CV(48) = 10870  /10          ! HP-GL is 40 dots-p-mm?
        CV(49) =  7600  /10
        CV(50) =     8   
        CV(51) =   100    !%
      ELSEIF (KEY2.EQ.60) THEN    ! ---> dot matrix printer
        CV(47) = KEY2
        CV(48) = 960         ! but what dpi is this 75?
        CV(49) = 576
        CV(50) = 2           ! # of colours ?
        CV(51) = -130        !  Y = 130 % of X 
      ELSEIF (KEY2.EQ.70) THEN    !------ DXF output ------
        CV(47) = KEY2
        CV(48) = 2560        !-- hmm in points like PS ? --
        CV(49) = 6360
        CV(50) = 256         ! # of colours (no limit?)
        CV(51) = 1 *100      ! Aspect Ratio = 1:1
      ELSEIF (KEY2.EQ.80) THEN    !------ WMF output ------
        CV(47) = KEY2
c       CV(48) = 2560 *50      
c        CV(48) = 6360 *50     
c        CV(49) = 6360 *50       !- mung to 50*larger ?
c.. but note that SELECT_WMF_PRINTER returns its own sizes
        CV(48) = int(32760. /SQRT(2.))      !- biggest integer*2 number 
        CV(49) = 32760       !- cf unsigned = 65536

        CV(50) = 256          ! # of colours (no limit?)
        CV(51) = -1 *100      ! Aspect Ratio = 1:1
      ENDIF  ! end of ouput devices


c=======================================================================
C                      READ / WRITE THE MESH 
c=======================================================================
c.... this block really needs hacking out into a subroutine !
c.... (cf. with the 'R' mirror options, which also include shifting,
c.... scaling, connectivity, *hardening* , etc.
      ELSEIF (CKEY.EQ.'#')THEN
C... key2 = 0 new data, =1 re-read = 2, append data etc. ??

c    0 = ZAP all Elements and Nodes
c    1 = read a  keyword file

c   20 = Import a .PL/.PL2/.G/.NFF format file
c   21 = as 20 but already have the filename in FILE_IN

c  100++ = Import .PL .OFF etc.
c  200++ = Export .PL .OFF etc.

c   99 = Next in a sequence ??

c========================== kill all data ==============================
      IF (KEY2.EQ.0) THEN      !- = "NEW"
        NN  = 0    
        NEL = 0

c======================== Import a Keyword File ========================
c.. I would rather do my own file browser (or use a MOTIF-style one)
c-- as a subroutine - 'get file', mask=*.key, istatus=0 if OK
c.. note SELECT_FILE is obsoleted in Salford for Win95
      ELSEIF (KEY2.EQ.1) THEN      
        print*,' Enter data filename ?'
        read*,arg
        if (filenames/=' ') arg=','//arg(1:len_trim(arg))
        filenames=trim(filenames)//arg(1:len(arg))
        CMN_EXHAUSTED=.false.

C====================== Import a mesh file =============================
c.. also need an 'auto' option.. cf GET_IDT() .. 
c... NO! .. just get the EXT from the given filename, then just R_MESHFILE

c.. IF .PL (etc.) then need to read the DISPS .. (then calc scale-fac?)
c     (if .PL2 .. read extra lines too)

      ELSEIF (KEY2.GE.100.and.KEY2.le.199) THEN      
        ITYPE = KEY2-100
c  no I would rather the menu subsystem could return the 3 characters 
c   directly eg.by overloading the 4byte integer?
c either way I should convert ITYPE 
        IF (ITYPE.eq.0) THEN     !---- auto-detect the import-file-type
           CALL GET_MESHFILE_TYPE (FILE_IN, EXT)          ! 6-12-96
        ELSE                     !---- choose a file of this EXT
           CALL GET_MESHFILE_EXT (ITYPE, EXT)
        ENDIF

        print*,' Enter data filename ?'
        read*,arg
        if (filenames/=' ') arg=','//arg(1:len_trim(arg))
        filenames=trim(filenames)//arg(1:len(arg))
        CMN_EXHAUSTED=.false.


c-----------------------------------------------------------------------
C====================== Export a mesh file =============================
c-----------------------------------------------------------------------
c... I would rather use a set of KEY2 values for the various file formats
c... hence also add the ability to EXPORT meshes too.

      ELSEIF (KEY2.GE.200.and.KEY2.le.299) THEN      
        ITYPE = KEY2-200
        CALL GET_MESHFILE_EXT (ITYPE, EXT)      ! ITYPE->EXT
c.. really we should call a subroutine to do this - as sometimes we want
c to modify the root name too (eg.BOVIDA)
        OPEN (IOUT,FILE=FILE_IN(1:INDEX(FILE_IN,'.'))//EXT)
        CALL WR_MESHFILE (IOUT,GC,IGC,NDIM,NN,NUMS,INUMS,NEL,PL2, EXT)

c------------- Output of Disps and Stresses --------------
c-- 29-9-98 Option here to write out disps and stresses too, eg if .UCD or .PL
c     also could save viewing paramaters if to a rayshader.
c-- I guess that we should write out the current (interpolated) disps.
c-- I should make this compatable with DANMUNG if possible.

        IF (NLDS.gt.0) THEN
      IF (EXT.EQ.'UCD')
     &CALL WR_DISPS_UCD (IOUT,DISPS,IDISPS,NODOF,NN)
c---------
c     IF (EXT.EQ.'DP')
c     WRITE (U3,'(A,/,A,I3,A,G14.5)')
c     &  '*DISPLACEMENTS',
c     &  '!  Load-step No.',IY,' Total Load=',9999. 
c      DO I=1,NN
c        WRITE (U3,'(I7,9E13.4)')  I,(DISPS_TOT(J,I),J=1,NODOF)
c      ENDDO
c---------
        ENDIF
        CLOSE(IOUT)

C====================== import a mesh file (old method) ================
c < stuff deleted 29-9-98 >


      ELSE
        Print*,' unknown KEY2 option in <#> '
      ENDIF   !--- of KEY2=1 = 'read a data file'




C------------------------- clear screen --------------------------------
      ELSE IF (CKEY.EQ.'!') THEN     !- whole VGA screen 
      call get_app_size (idev_x,idev_y)
      CALL FILL_RECTANGLE (0,0,idev_x,idev_y, 0)

c  .. killed 'm' = 'post_menu'
c  .. killed '/' = 'SVGA preview

c----------------------- exit/DOS shell --------------------------------
c... I Don't need two of these !
      ELSEIF (CKEY.EQ.'q') THEN
        GOTO 999    !- exit
!     ELSE IF (CKEY.EQ.'d') THEN             !- DOS shell 
! **OBSOLETE** 29/03/15
!       PAUSE '--PAUSE--'

C-------------------------- s t a t u s --------------------------------
c.. maybe I should do the status on the bottom-line ?

c      ELSEIF (CKEY.EQ.'Q') THEN  !'Q=Query'
c        CALL RESET_SIZES (RES)
c       CALL DR_BOX (1.*RES(4),1.*RES(5),1.*RES(1),1.*RES(2),0)     ! blank
c        CALL MAKE_BOX (1.*RES(4),1.*RES(5),1.*RES(1),1.*RES(2),X,Y) ! blank
c        CALL DR_PRIM (LINE,X,Y,4,0,2)                               ! out ?
c        CALL SHOW_STATUS (TIME,NN,NEL,NLDS,NFCETS_D,NFCETS,FILE_IN)

c-------------------------- 'zoom'-box ---------------------------------
c. this is currently broken :-(
c or rather it correctly draws the box, but fails to set the view.
      ELSEIF (CKEY.EQ.'B') THEN
        mbox(1) =     codes(3)
        mbox(2) = 480-codes(4)   !- store the box coords
        mbox(3) =     codes(5)   ! (note codes in INT, mbox is REAL
        mbox(4) = 480-codes(6)
c       print*, 'zoom..', codes(3),codes(4),codes(5),codes(6)
        CALL MAKE_BOX (mbox(1),mbox(2) ,mbox(3)-mbox(1)
     &                ,mbox(4)-mbox(2) ,X,Y)
        CALL DR_PRIM (LINE,X,Y,4,15,1)     !- now draw

c-----------------------------------------------------------------------
C---------------------------- Animation --------------------------------
c-----------------------------------------------------------------------
c.. 28-2-94 revamped to separate and use CV not IOP_ANIM
c .. a fill-in form (with default values is much nicer
C    maybe I should try a form in HTML , so parse & write a macro???

c     ELSEIF (CKEY.EQ.'A') THEN
      ELSEIF (CKEY.EQ.'Z') THEN     !- since PGPLOT uses 'A' for the mouse
        IF (KEY2.eq.-99) THEN
          PRINT*,'Which Animation parameter?'
          PRINT*,'0=reset,1=GO,2=#frames,3=dest,4=filename'
          PRINT*,'5=disp.code, 6=๋eye,7=๋coa'
          PRINT*,'8=store memory to PCX''s'
          READ*,KEY2
        ENDIF           

        IF (KEY2.eq.0) THEN     !- reset all parameters ??
          DO I=53,60               !- default not ncessirily zero tho!
            CV(I) = 0
          ENDDO           !-- better to use SET_CV to recreate !
          CV(54)=20     
        ELSEIF (KEY2.eq.1) THEN
          CV(53) = 1    !- activate
          GOTO 901      !- redraw
        ELSEIF (KEY2.eq.2) THEN
          PRINT*,'How many frames ? (eg.20)'
          READ*,CV(54)
        ELSEIF (KEY2.eq.3) THEN    ! (zero = none) (-ve .. use SVGA?)
          PRINT*,'Enter the destination code (0 for no store,'//
     &    ' 1 for MEMORY,2 for PCX (-ve to use the SVGA mode))'
          READ*,CV(55)
c            ELSEIF (KEY2.eq.4) THEN  ! (what is this ?)
          IF (ABS(CV(55)).EQ.2) THEN
            PRINT*,'What Animation root-file name ? (eg.''fred'')'
            READ(*,'(A)') FILE_ROOT
          ENDIF

        ELSEIF (KEY2.eq.5) THEN
          PRINT*,'Enter the disp. code (1 for disps, 2 for user-def',
     &            ' 3 for eigenmodes)'
          READ*,CV(60)
          IF (CV(60).EQ.2) THEN
            PRINT*,'Enter the load-steps FROM / TO to animate'
            READ*, AN_LS1, AN_LS2
          ENDIF
        ELSEIF (KEY2.eq.6) THEN
          PRINT*,'Enter the increments for the EYE (10,0)'
          READ*,CV(56),CV(57)
        ELSEIF (KEY2.eq.7) THEN
c         or the light angle ?
          PRINT*,'Enter the increments for the COA (0,10,0)'
          READ*,dcoa(1),dcoa(2),dcoa(3)
        ELSEIF (KEY2.eq.8) THEN
          print*,"This isn't yet supported under PGPLOT"
          PRINT*,'What Animation root-file name ? (eg.''fred'')'
          READ(*,'(A)') FILE_ROOT                     
          DO IPIC = 1,NPICS        !- save current to disk
            II = istr_end(FILE_ROOT)
            WRITE (PICFILE,'(i8.8,a)') IPIC,'.PCX'
            PICFILE(1:II) = FILE_ROOT(1:II)
c           CALL SCREEN_BLOCK_TO_PCX@ (PICFILE,IMAGE(IPIC),IFAIL_2)
c           CALL DOSERR@(IFAIL_2)    !- shouldn't fail tho
          ENDDO
c       ELSEIF (KEY2.eq.9) THEN    !----flag to use SVGA screen
        ELSE
          PRINT*,'** INTERNAL : unknown animation opcode'
        ENDIF
                                                       
C----------------------- animation replay ------------------------------
c.. should really have a *choice* of enviroments:
c.. eg. with menus, black screen, centered (to file?)
c.. or even a complete enviroment with VCR style controls ?
c.. there need a set of 'play' codes (extend 'A' ??)
c.. also need an option to save frames to PCX files  <---
c -- maybe even animation playback direct from disk ?
C.. ie. 
C     'A' :20  = 1 read-from-disk, 2=write-to-disk,3=play,4=bounce
C         :21  = set a,b,c frame pause parameters
C           (hence macros to set 'fast' or 'pause at ends')
C         :22  = replay destination : in window/full-screen/special screen/
C .. playpack timing control ? 
C
C .. options : forwards, backwards, bounce, # cycles (infinite)
C .. save to disk (respect timings as multiple identical frames ?)
C .. pause, > so fast-forward, rewind, jump to frame #, step up/down
      ELSEIF (CKEY.eq.'}'.or.CKEY.eq.'{') THEN   
        CALL RESET_SIZES (RES)               !- in the 'normal' window
        IC  = 0                                 ! start-frame minus 1
        IC2 = 1                                 ! & increment
        DO I=1,9999
          IC = IC + IC2
          IREPEAT = CV(68)                      !- no. of frames
          IF (IC.EQ.1) IREPEAT = CV(67)
          IF (IC.EQ.NPICS) IREPEAT = CV(69)
          if (cv(60).eq.3) irepeat =cv(68)   !- no extra-pause for eigenmodes
          DO J=1,IREPEAT
c         CALL RESTORE_SCREEN_BLOCK@(RES(4),20+RES(5)    ! +20 cos of topmenus
c    &      ,IMAGE(IC),0,IFAIL_2)
          ENDDO
          IF (KEY_WAITING()) GOTO 1111      !- ie. loop back
          IF (CKEY.EQ.'}') THEN
            IF (IC.GE.NPICS)  IC = 0         !- wrap (cos all done)
          ELSEIF (CKEY.EQ.'{') THEN
            IF (IC.GE.NPICS)  IC2 = -1       !- reverse at last (ie auto-#)
            IF (IC.LE.1)      IC2 =  1       !- reverse at first
          ENDIF
        ENDDO

C------------- 'facet# to always/never draw' ---------------------------
c.. again I can fill in a table off the menu  0,1,2 for each of the 6
c.. must inform the user of which face is which
c.. even show on the menu itself
      ELSEIF (CKEY.EQ.'E')THEN           !-- was '|' , & then was แ  !!
        write(*,'(a,6i2)')'Facet codes(1..6)?=',(ID_FACE(i),i=1,6)
        write(*,*) '0 = selective(default), 1=never, 2=always'
        read*,(ID_FACE(i),i=1,6)

C------------------------------ input macro ----------------------------
c... obsolete ! ... but interesting as an example
c      ELSEIF (CKEY.eq.'k') THEN    ! (rarely used !)
c        PRINT*,'Enter macro string (_=space,~=CR)'
c        READ(*,'(a)') LINE
c        DO I=1,istr_end (LINE)
c          IF (LINE(I:I).eq.'~') THEN
c            CALL FEED_KEYBOARD@ (13, ifail)
c          ELSEIF (LINE(I:I).eq.'_') THEN
c            CALL FEED_KEYBOARD@ ., ifail)
c          ELSE
c            CALL FEED_KEYBOARD@ (ints(ICHAR(LINE(I:I))), ifail)
c          ENDIF
c        ENDDO

C***********************************************************************
C------------------> set an item's color -------------------------------
C----------- (or take other appropriate action)
C***********************************************************************
C... first give the code of the entity that is going to be changed'
c   F,I,J and C all just need CV ?

      ELSEIF (CKEY.EQ.'T') THEN            !-- Triplet Setting
        print*,'enter title:'
        read(*,'(A)') titles(key2)
        n_titles=max(n_titles,key2)
      ELSEIF (CKEY.EQ.'F') THEN            !-- Triplet Setting
        CV (KEY2/1000) =MOD (KEY2,1000) 

      ELSEIF (CKEY.EQ.'I'.or.CKEY.eq.'J') THEN
        IF (KEY2.eq.-99) THEN
          IF (CKEY.EQ.'I') then
             PRINT*,'Item Number to colour ?'       !- via dialog
          ELSEIF (CKEY.EQ.'J') then
             PRINT*,'Item Number to set size ?'     !- via dialog
          ENDIF
             READ*,KEY2
             CALL CV_GET_LABEL (key2,LINE)
             WRITE(*,'(I3,A,A)') key2,'=',LINE
c            show current value?
          ENDIF           

c        CALL CV_GET_LABEL (I,LINE)
c        WRITE(*,'((I3,I5,A,A))') I,IVAL,'    !-  ', LINE
        CALL SET_CV (KEY2,999, 2)        !- save this number (cf ITEM2C)

      ELSEIF (CKEY.EQ.'C') THEN
        IF (KEY2.eq.-99) THEN     
c          CALL CV_GET_LABEL (key2,LINE)
c          WRITE(*,'(I3,A,A)') key2,'=',LINE
          CALL SET_CV (ITEM2C,999, 6)        !- get ITEM2C ??
          WRITE(*,'(A,I3,A)')
     &      'colour value (', CV(ITEM2C),') ?'      !- dialog
          READ*,KEY2
        ELSEIF (KEY2.eq.-10) THEN         !(why are there 2 of these?)
          PRINT*,'value ?'             !- dialog
          READ*,KEY2
        ENDIF

c-------------- increment / decrement an item # or colour --------------
c.. fix limits to 1->50 ??
        IF (KEY2.EQ.-11) THEN            ! ITEM--
          CALL SET_CV (ITEM,IVAL,6)
          ITEM = MAX (1,ITEM - 1)
          CALL SET_CV (ITEM,IVAL,2)
          L_OVERLAP = .true.                    !- so repost marbles
        ELSEIF (KEY2.EQ.-12) THEN        ! ITEM++
          CALL SET_CV (ITEM,IVAL,6)
          ITEM = MIN (50,ITEM + 1)       !- why hard coded to 50 ?
          CALL SET_CV (ITEM,IVAL,2)
          L_OVERLAP = .true.           !-- so force a re-post

        ELSEIF (KEY2.EQ.-13) THEN        ! IVAL--
          CALL SET_CV (ITEM,IVAL,6)         !- get item
          CALL SET_CV (ITEM,IVAL,5)         !- so get its IVAL
          IVAL = MAX (-1,IVAL - 1)
          CALL SET_CV (ITEM,IVAL,1)
          L_OVERLAP = .true.
        ELSEIF (KEY2.EQ.-14) THEN        ! IVAL++
          CALL SET_CV (ITEM,IVAL,6)
          CALL SET_CV (ITEM,IVAL,5)
          IVAL = MIN (255,IVAL + 1)
          CALL SET_CV (ITEM,IVAL,1)
          L_OVERLAP = .true.
        ELSE
          CALL SET_CV (ITEM2C,999, 6)            !- get ITEM2C ??
        IF (ITEM2C.GT.0.AND.ITEM2C.LE.M_CV) THEN  !--- if within CV() ---
          CALL SET_CV (999, KEY2, 3)             !- change a CV() entry

c.. special cases - not realy anything to do with CV
C------------- shade palette based on an existing colour ---------------
        ELSEIF (ITEM2C.EQ.201) THEN
          Icol2=KEY2
          CALL SET_COLOURS ( 6,icol2,CV(18))

C----------------- RGB definition of a single colour -------------------
        ELSEIF (ITEM2C.EQ.202) THEN 
          icol2 = key2
          CALL SET_COLOURS ( 8 ,icol2,CV(18))

C--------------- random RGB of a single colour -------------------------
      ELSEIF (ITEM2C.EQ.203) then
          icol2 = key2
          CALL SET_COLOURS ( 7 ,icol2,CV(18))
C------------------------- overscan colour -----------------------------
      ELSEIF (ITEM2C.EQ.204) then           !- only works for 16 col modes  
          icol2 = key2
          CALL SET_COLOURS ( 7 ,icol2,CV(18))
C------------------------ load step # patch ----------------------------
        ELSEIF (ITEM2C.EQ.250) then
          LD = KEY2

C------------------------- save image to memory ------------------------
c... need an option to clear ALL saved images (RETURN_STORAGE@() )
c... * BEWARE *  of 'inverted' RES for GET_SCREEN_BLOCK
!-- need a PGPLOT/generic interfaceY
c
        ELSEIF (ITEM2C.EQ.301) then
          CALL RESET_SIZES(RES)      !- ie get the 'window' area ?
c         IF (IMAGE(KEY2).NE.-1)
c    &      CALL RETURN_STORAGE@ (IMAGE(KEY2))   !- zap old image
c         CALL GET_SCREEN_BLOCK@ (
c    &        RES(4),RES(5),RES(4)+RES(1),RES(5)+RES(2),IMAGE(KEY2))
            IF (IMAGE(KEY2).EQ.-1) THEN
c             CALL SOUND@( 256,2)
              PRINT*,'****ERROR: IMAGE',KEY2,' out of memory' 
            ENDIF
C------------------------- load iamge from memory ------------------------
!-- need a PGPLOT/generic interfaceY
        ELSEIF (ITEM2C.EQ.302) then
          IF (IMAGE(KEY2).ne.-1) THEN   !- if saved
            CALL RESET_SIZES (RES)           !- in the 'normal' window
c           CALL RESTORE_SCREEN_BLOCK@ 
c    +      (RES(4),RES(5),IMAGE(KEY2),0,IFAIL_2)
c           CALL DOSERR@(IFAIL_2)
          ELSE
c           CALL SOUND@( 256,2)
            PRINT*,'****ERROR: IMAGE',KEY2,' was not saved' 
          ENDIF
C---------------------- auto save image to memory ----------------------
c.. hmm should really just be saving the 'image' part of the screen !
        ELSEIF (ITEM2C.EQ.-9) then  !- ie. in the next available space
          IMAGEN = IMAGEN+1
c         CALL GET_SCREEN_BLOCK@ (0,0, 639,479,IMAGE(IMAGEN))
          IF (IMAGE(IMAGEN).EQ.-1) THEN
c           CALL SOUND@( 256,2)
            PRINT*,'****ERROR: IMAGE',KEY2,' out of memory' 
          ENDIF
C----------------------- glassing materials ---------------------------
c... so need to glass a material and also unglass it
c... and so Glass_all, and Unglass_all
c... and the best method 'Glass within a box' so can cut. a portion out
c... This should also take a mat # ? to stop Piles etc. disapperaing!
c---------------------- glass this material ----------------------------
        ELSEIF (ITEM2C.EQ.501) THEN
          DO IEL=1,NEL
            CALL GET_EL_IMAT (NUMS,INUMS,IEL, IMAT)
            IMAT = ABS(IMAT)
            IF (IMAT.eq.KEY2) CALL PUT_EL_IMAT (NUMS,INUMS,IEL, -IMAT)
          ENDDO
c------------------- unglass (restore) this material -------------------
        ELSEIF (ITEM2C.EQ.502) THEN
          DO IEL=1,NEL
            CALL GET_EL_IMAT (NUMS,INUMS,IEL, IMAT)
            IMAT = ABS(IMAT)
            IF (IMAT.eq.KEY2) CALL PUT_EL_IMAT (NUMS,INUMS,IEL, IMAT)
          ENDDO

c------------------- set material for 'marked' elements ----------------
        ELSEIF (ITEM2C.EQ.503) THEN
          DO IEL=1,NEL
            CALL GET_ELEMENT 
     &      (NUMS,INUMS,IEL, NUM, NOD,NDIME,ITYPE, IMAT,IU1,IU2,IU3)
            II = 0
            DO I=1,NOD
              IF (IFL_NODES(NUM(I)).NE.0) II=II+1
            ENDDO
            IF (II.EQ.NOD)     !- if *all* nodes are selected
     &          CALL PUT_EL_IMAT (NUMS,INUMS,IEL, KEY2)
          ENDDO

c-----------------------------------------------------------------------
        ELSE
          PRINT*,' ITEM2c=',item2c,' not understood'
        ENDIF
        ENDIF       !- if just inc/dec or extended options

C------- individual color specification & pre-def palettes ? -----------
c ** This is the MAJOR way in which SET_COLOURS is called
c.. eg. 'rainbow','spectrum','gray' 
c.. hmm note that 'C' also handles cases where 1 colour is chosen from 
c.. the menu and so calls SET_COLOURS from there
      ELSEIF (CKEY.EQ.'.') THEN
        ICOL = KEY2 
        CALL SET_COLOURS (ICOL,icol2,CV(18))  

C------------------- palette cycling -----------------------------------
c... hmm  just spin the table ! .. (careful of the menu-colors !)
c.. spin both ways ?  .. just  spin the contour table ? (20++)
c.. hmm was 'P'    *obsolete* surely
c.. for PGplot etc. this would require a replot
      ELSEIF (CKEY.EQ.'W') THEN
        DO I=1,5*3
          CALL SET_COLOURS (41,-99,CV(18))     !- opcode = 41 ?
        ENDDO

C----------------- *** Contour type selection *** ----------------------
      ELSEIF (CKEY.EQ.'K') THEN
        IF (KEY2.EQ.-99) THEN      ! menu pointer to contour type
          PRINT*,'Enter contour type(',CV(13),')' 
          READ*,KEY2
        ENDIF
        CV(13) = KEY2
C.. now if we have a contour type >1000 then we loop ALL elements
c.. do WTHATN and loop NOD and sample to get value
C.. store in CONT_VALS(I) and use P() to average
c................ build the contour tables here ........................
c.. make sure we have the current 'disps' to get values from. 
c.. ?disps as a 'straight vector' - then partition into 2 dims
c.. keep GDISPS as pure 'backing store' 
C  -- careful of 'mirroring' cos it 'creates' GDISPS. :-(
c .. cf mirroring as merely creating 'shadow' elements. via a TR matrix)
C.. Eg. try this for the shear-strain.
C-- 2-12-95 Can I do the stresses here too?
      IF (CV(13).GE.1000) THEN
C       CV(13) = CV(13) - 1000
        DO I=1,NN
          PL2(I) = 0   !- Zap count of elements per node
        ENDDO
        DO IEL=1,NEL
          CALL GET_ELEMENT 
     &    (NUMS,INUMS,IEL, NUM, NOD,NDIME,ITYPE, IMAT,IU1,IU2,IU3)
          IF (IMAT.GT.0) THEN
            CALL WTHATN (NN_F,2,1,LC_SF,ISC)
            DO I=1,NOD
c.. hmm DI_F is undefined here
              CALL SAMPLE (C_F,ISC,DI_F,ISC,  NN_FT,2,3,
     &                           VAL,VEC, LC_SF,ISC,I,10)   !10 ??
              J = NUM(I)
              PL2(J) = PL2(J) + 1
              CONT_VALS(J) = CONT_VALS(J) + VAL
            ENDDO   !- element's nodes
          ENDIF   !- skip -ve IMATs
        ENDDO    !- elements
c.. so now average at nodes ?
        DO I=1,NN
          IF (PL2(I).NE.0)
     &    CONT_VALS(I) = CONT_VALS(I) / PL2(I)
        ENDDO
      ENDIF   !- if CV() > 1000 = 'pre-form contour table'

C-----------------------------------------------------------------------
C----------------------- contour options -------------------------------
C-----------------------------------------------------------------------
c... an easy block to push out to a subroutine !
c... an *obvious* candiadate for form-filling !
c... so create a menu/form with all this in. & let the user 'edit' it
c...... also nicer to split into ../ # conts / Min,Max / Log.power /
c.. can also choose contour type too ?
      ELSEIF (CKEY.EQ.'^') THEN
        IF (KEY2.eq.1) THEN    !---------- auto set range ----------
          C_R(3) = c_r(1)
          C_R(4) = c_r(2)
          C_R(5) = 1.          !- log
          C_R(6) =-1.          !- abs
          IF (abs(C_R(2)-C_R(1)).lt.1.e-26 )
     &      C_R(4) = C_R(3) + 1.E-5              !- avoid ==
        ELSEIF (KEY2.eq.2) THEN
           write(*,'(4(A,G10.3),A)')
     &        'Contour range = ?  (current =',C_R(3),' to', C_R(4)
     &       ,' from limits of',C_R(1),' to', C_R(2) ,' )'
           READ*, c_r(3),C_R(4)       !- nice to 'fill-in' boxes
        ELSEIF (KEY2.eq.3) THEN
           PRINT*,'Number of contours = ? (current =', CV(18),' )'
           READ*, cv(18)
        ELSEIF (KEY2.eq.4) THEN
           PRINT*,'Log Factor = ?  (current =',C_R(5),' )' 
           READ*, c_r(5)
        ELSEIF (KEY2.eq.5) THEN
           PRINT*,'Take Absolutes (if=-1)?  (current =',C_R(6),' )' 
           READ*, c_r(6)

        ELSEIF (KEY2.eq.7) THEN       !- auto minimax
          C_R(4) = max ( abs(c_r(1)),abs(c_r(2)) )
          C_R(3) = -c_r(4)

c       ------ stress vector scaling ------
c       ... note: maybe it should change dynamically as we zoom in ?
        ELSEIF (KEY2.eq.10) THEN
           C_R(7) = C_R(8)
        ELSEIF (KEY2.eq.11) THEN
           PRINT*,'Stress level that is 5% model size?'
           PRINT*,'Current=',C_R(7),' auto=', C_R(8)
           READ*, c_r(7)

c       ------ isosurface level ------
        ELSEIF (KEY2.eq.20) THEN
           PRINT*,
     &    'Iso-surface level as a fraction 0.->1.of the contour range'
           PRINT*,'Current=',C_R(9)
           READ*, c_r(9)


        ELSEIF (KEY2.eq.-1) THEN    !-- manualy set the contour range ----

        write(*,'(A,G12.4/A,G12.4,A,G12.4)')
     &      'Data range    =', C_R(1),'<---->',C_R(2)
     &     ,'Current range =', C_R(3),'<---->',C_R(4)
     &     ,' power=',C_R(5)
c          PRINT*,'Enter new Min, Max, & Power, (-ve for abs)'
c          WRITE (LINE,'(2G12.3,F5.2)') C_R(3),C_R(4),C_R(5)
c          CALL READ_EDITED_LINE@ (LINE,0s,1S,15S,IFAIL)   !- 'esc' to quit
c           read(LINE,*) C_R(3), C_R(4), C_R(5)
           READ(*,*) C_R(3), C_R(4), C_R(5)
           C_R(6) = SIGN (1.,C_R(5))       !- just +1 or -1
           C_R(5) =  ABS    (C_R(5))       !- lose the sign :-)
        ENDIF

c-----------------------------------------------------------------------
c-------------- misc. keycodes that just affect CV ---------------------
c-----------------------------------------------------------------------
      
C----------------------- backface polygon cull -------------------------
c  (Obs?) .... since the menu does this directly
      ELSEIF (CKEY.EQ.char(92))THEN         !- a backslash
        IF (KEY2.ge.0) THEN
          IF (KEY2.LE.3) CV(32) = KEY2            ! b_p_c
          IF (KEY2.LE.6) CV(17) = KEY2-3          ! depth sort (obs here)
        ELSE 
          CV(32) = MOD(CV(32)+1,3) ! just toggle if keyb.
        ENDIF 

C-------------------------- facet sub-division -------------------------
c.. (obs) as done directly from the mneu into CV(19)
      ELSEIF (CKEY.EQ.'@')THEN
        PRINT*,'How many sub-facets per facet (',CV(19),' ) ?'
        READ*,CV(19)
        CKEY = '_'

C------------------- auto-glassing of materials ------------------------
c.. really need a more general method !
c... how do I reset it tho ! (better JUST from the menu
c-- things like this are maybe better from the menu only
c-- as  just a triplet : set_cv, #, and value
c-- so on viewing menu = auto-build / excavate / normal          
c.. a more general method is where each load case records which elements 
c   are coming or going, hence a list of 'active' elements
      ELSEIF (CKEY.EQ.'|') then    !------ reset ------
        CV(52) = 0
      ELSEIF (CKEY.EQ.'[') then    !------ built-up models------
        CV(52) = 1
      ELSEIF (CKEY.EQ.']') then   !------ excavation models-------
        CV(52) = 2

C-------------------------- element shrinking --------------------------
c.. (obs) again this can be done direct from the menu? (also +/-1??) 
c.. push to all the 'image bits'. eg image scale factor ?
c--> put on the 'mesh' menu then triplet set. or 'J','C' ?
      ELSEIF (CKEY.EQ.'S') THEN
        print*,'ishrink=', cv(31),' ?'    !typicaly=30%
        read*,cv(31)

C-------------------- buffered screen output ---------------------------
c.. via generic triplet surely?  (but a toggle ?)
      ELSEIF (CKEY.EQ.'b') THEN    
        CV(38) = MOD(CV(38)+1,2)

C--------------------- dump the full screen to a PCX -------------------
c.. compare and contrast this with using PCX as a print file
c.. also full-page v. image-window
c.. maybe more 'logical' as a cntrl-key /F-key (or PrtScn ?)
c.. must push to DRAW2.F as a general 'tool'  (auto-filename ?)

      ELSEIF (CKEY.eq.'"') THEN           !---(PCX screen dump) --
c       CALL GET_SCREEN_BLOCK@ (0,0,639,479,BUFFER)
        PRINT*,'PCX screen dump File name: ?'    
        READ(*,'(A)') PICFILE
c       CALL SCREEN_BLOCK_TO_PCX@ (PICFILE,BUFFER,IFAIL_2)
c       CALL RETURN_STORAGE@ (BUFFER)     !- zap this image page
c       CALL PCX_FIX (PICFILE,PAL)

C---------------- screen window size and position ----------------------
c... I think that this is completely obsolete (because of RESET_SIZES)
c... however I could use a printer-driver to get a smaller window
c... on the screen ??
c-- Ok yes. I think that we need to store this sub-window in a new array
c  this is the VIEWPORT
      ELSEIF (CKEY.EQ.'w') THEN
        PRINT*,'enter window size, x,y (460,460)'
         READ*,RES_wx, RES_wy
c        READ*,RES(1),RES(2)
c        RES(4) = (639 -RES(1)) / 2     !- force it to the 
c        RES(5) = (479 -RES(2)) / 2     !- centre_of_the_screen

C-------------------- save current status---------------------------
c... maybe we cannot only save those that have been changed ?
c.. it is a pity that I can't use keywords or comments.
c.. ie every CV() as a cv_string() to go with it. (so read & write)
c.. overload 'Y' with over 'Danplot' options too?
c.. save contour range, view parameters, etc ?
c.. 18-7-95 added PAL too.

      ELSEIF (CKEY.EQ.'Y') THEN   
        PRINT*,' Filename to save current status ? (danplot.cfg)'
        READ(*,'(A)') FILE_OUT
        IF (FILE_OUT.EQ.' ') FILE_OUT='danplot.cfg'
        OPEN (88,FILE=FILE_OUT)
          WRITE(88,'(A)') 
     &    '#','# DANPLOT saved status variables','#'
          WRITE(88,'(A)') '[CONTROL]'
          DO I=1,M_CV
            CALL CV_GET (I,IVAL)
            CALL CV_GET_LABEL (I,LINE)
            WRITE(88,'((I3,I5,A,A))') I,IVAL,'    !-  ', LINE
          ENDDO
          WRITE(88,'(A/,(I3,3I4))')       !(use a GET/PUT_PALETTE?
     &   '[PALETTE]',
     &       (I,(PAL(J,I),J=1,3),I=0,255)

c.. maybe all contour options : #conts, log-factor, variable name (x,y,z)
c    < some of these are held in CV() anyway >
c..  nicer to use 'MinContour = 1.23 etc.'

          WRITE(88,'(A/,(2G15.5))')       !(use a GET/PUT_PALETTE?
     &   '[CONTOUR_LIMITS]',
     &       c_r(3),c_r(4), c_r(7)        

c..... cf. POVRAY: 'camera {', location, direction,look_at, ..
c..... maybe light sources too? so >1, coloured, ...
c          WRITE(88,'(A/,(2G15.5))')    
c     &   '*VIEW_PARAMETERS'       

        CLOSE(88)

c-----------------------------------------------------------------------
c---------------------- Image Scaling and Moving -----------------------
c-----------------------------------------------------------------------
C --> menu-driven  eye-angle/eye-pan/light-source changing etc. ?
C.. can make a subroutine .. nicer if we hold ALL these real numbers in
C.. a 'viewing-array' :
      ELSEIF (INDEX ('Vh€+t-<s>%',CKEY).ge.1) THEN
        CALL CHANGE_VIEW_PARAMS (CKEY,CODES, SSC,FACT, COA, DEYE, IROT
     &            ,XME,YME, XML,YML, CV)


c--------------- now re-create the viewing transformations -------------
c.. and show the wire-frame
c.. note method for creating and using TR
c.. need a mode switch for:   uodate wire-frame v. recreate whole view.
c       if (cv(??).eq.1) goto 901        !- just redraw full image

        CALL XM2EYE (XME,YME,EYE  )      ! convert to a unit vector
        T = SIGN (1.,ABS(YME)-90.001)    ! 'fly -over-the-top' patch
        DO I=1,3
          UP(I) = SIGN( UP(I),T )
        ENDDO
        CALL CAMERA_2 (EYE,COA,UP, DEYE*DIAG/2.,FEYE, 0.,1.,PM)
        win_xo=res(4)
        win_yo=res(5)
        win_xw=res(1)
        win_yw=res(2)
        CALL CAM_WINDOW (win_xo, win_yo, win_xo+win_xw, win_yo+win_yw
     &    ,(1.+CV(44)/100.)/2.,(1.+CV(45)/100.)/2., SSC, 0.,AR, TS )
        CALL PREPEND_TR(PM,TS)

        CALL MAKE_BOX (WIN_XO,WIN_YO,WIN_XW,WIN_YW,X,Y)  !- blank screen
        CALL DR_PRIM (LINE,X,Y,4,  0  ,2)                !- (0='black')
c.. cf use of the material colours here ? - or even a 'buffered' metafile.
          if (iverb>=3) print*,' drawing the rings'
        CALL DRAW_RINGS (RINGS,0,GC,IGC,DISPS,IDISPS,FACT  !- as deformed.
     &    , 'line',15 ,PM)
          if (iverb>=3) print*,'done'

c.. nice to draw: light vector, scales, ...
c------- draw the 3 axes -------
        DO I=1,3                ! loop the 3 axes
          DO J=1,3
            GPT(J) = COA(J)
            IF (I.EQ.J) GPT(J) = GPT(J) + DIAG*.4  ! get the axis
          ENDDO
          CALL TRANSFORM2 (COA,SPT ,PM)
          CALL TRANSFORM2 (GPT,SPT2,PM)
          CALL DR_ARROW  (SPT,SPT2,5,15,13)   
            LINE = CHAR (ICHAR('w') + I )  ! ie 'x','y','z'
            X(1) = SPT2(1)
            Y(1) = SPT2(2)
            CALL DR_PRIM (LINE,X,Y,1, 14,20)    !- colour #14
        ENDDO

c------- draw the light vector -------
c.. it would help if I drew its x-z plane preojection too.
        CALL XM2EYE (XML,YML,LIGHT)      !- to unit ector
        DO I=1,3
          GPT(I) = COA(I)
        ENDDO
        CALL TRANSFORM2 (GPT,SPT ,PM)    
        GPT(1) = GPT(1) - LIGHT(1) * DIAG/4.   !(note -ve.)
        GPT(2) = GPT(2) - LIGHT(2) * DIAG/4.   !(note -ve.)
        GPT(3) = GPT(3) - LIGHT(3) * DIAG/4.   !(note -ve.)
        CALL TRANSFORM2 (GPT,SPT2 ,PM) 
        CALL DR_ARROW  (SPT,SPT2,10,15,  14)   !- colour #14


c-----------------------------------------------------------------------
c-----------------------------------------------------------------------
C-------------------- Mirroring and Munging the model ------------------
c-----------------------------------------------------------------------
c-----------------------------------------------------------------------
C---> push the work to a subroutine to save space ? :-)

c.. SUBROUTINE would need NUMS,GC,GDISPS,IDIR,VAL (xyz limits?)
C       IDIR = 1,2,3  : Mirror in low x,y,z
C              4,5,6  : Mirror in the high x,y,z
C              7,8,9  : Un-mirror (7=half,8=1/4,9=1/8th)
C               50    : Set Mats to 'connectivity'
C               51    : 'Harden' geom to geom+disps
C               52    : Sum load-steps
C               53    : Difference load-steps
C               54    : put face normals into disps table (cf .OFF)
C               55    : prompt for xs,ys,zs then scale mesh
C               56    : prompt for xs,ys,zs then shift mesh
C            ..  etc. etc.
c
      ELSEIF (CKEY.EQ.'R') THEN
        IF (KEY2.le.0) THEN
          PRINT*, 'Enter the Mirror/Mesh_Mung opcode (1=mirr-x etc.)' 
          READ*,IDIR
        ELSE
          IDIR = KEY2
        ENDIF

c.. shouldn't the next line be U3 not IO ?
        CALL MUNG_MESH (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL,PL2 
     &   ,GDISPS,IGDISPS,MDF,NLDS,LD
     &   ,FACETS,IFCETS,NFCETS, RINGS,IRINGS,  NORMS, IDISPS
     &   ,IDIR)

c------------------ Marking the mesh -------------------------
c.. ? select by mouse, and also select_more_nodes too? (eg piles)
c.. also 'cummulative'? so only select 
c.. must move back out to DANPLOT surely.

      ELSEIF (CKEY.EQ.'T') THEN
        IDIR = key2
        IF (IDIR.EQ.40) THEN  !- use mouse zoom-box
          DO I=1,NN
            IFL_NODES (I) = 0               !- unmark all first ?
c           IF (IFL_NODES(I).NE.0) THEN     !- so only use 'current' nodes
              do j=1,3
                GPT(J) = GC(J,I) + FACT1 * DISPS(J,I)    !(Is disps up_to_date ?)
              enddo
              CALL TRANSFORM2 (GPT,SPT ,PM)            !(Is PM up to date)
              IF ((SPT(1)-mbox(1))*(SPT(1)-mbox(3)).lt.0) THEN        ! X
                IF ((SPT(2)-mbox(2))*(SPT(2)-mbox(4)).lt.0) THEN      ! Y
                  IFL_NODES (I) = 1
                ENDIF
              ENDIF
c           ENDIF
          ENDDO
          GOTO 901     !- force a redraw to show the marked nodes / elements ?

        ELSEIF (IDIR.EQ.41) THEN  !- refine selected
c         DO I=1,NN
c           IFL_NODES (I) = 0    !- unmark all first ?
c         ENDDO
          DO I=1,NN
            IF (IFL_NODES(I).NE.0) THEN     !- so only use 'current' nodes
              IFL_NODES (I) = 0               !- unmark first ?
              do j=1,3
                GPT(J) = GC(J,I) + FACT1 * DISPS(J,I)    !(Is disps up_to_date ?)
              enddo
              CALL TRANSFORM2 (GPT,SPT ,PM)            !(Is PM up to date)
              IF ((SPT(1)-mbox(1))*(SPT(1)-mbox(3)).lt.0) THEN        ! X
                IF ((SPT(2)-mbox(2))*(SPT(2)-mbox(4)).lt.0) THEN      ! Y
                  IFL_NODES (I) = 1
                ENDIF
              ENDIF
            ENDIF
          ENDDO
          GOTO 901     !- force a redraw to show the marked nodes / elements ?

c.. Or just a 'mini-draw' ..
c.. Use the last Painters-list, loop, get facets, draw facets with marked nodes
c   in 'yellow'? rest in blue ? .. nodes in white?
c
        ELSEIF (IDIR.EQ.42) THEN !  Mark all
          DO I=1,NN
            IFL_NODES (I) = 1
          ENDDO
        ELSEIF (IDIR.EQ.43) THEN !  Un-Mark all
          DO I=1,NN
            IFL_NODES (I) = 0
          ENDDO
        ELSEIF (IDIR.EQ.44) THEN !- Flip all
          DO I=1,NN
            IF (IFL_NODES(I).EQ.0) THEN
              IFL_NODES(I) = 1
            ELSE
              IFL_NODES(I) = 0
            ENDIF
          ENDDO
        ENDIF

C-----------------------------------------
C-- linkage of load step # to picture # --
C   There are 3 possibilities :
C       -1 = NO linking
C        1 = set load step <= pic #
C        2 = set view dirn <= pic #     .. more options ?
c.. see triplet setting     


C------------------- x,y,z up-direction toppling -----------------------
C..   better as a subroutine to save space ??
C      ( for Ian Williams et al. who think lop-sided :-)
C...  we can also do this by simply adjusting the mapping of the model 
c..   'xyz' onto the 3d screen model xyz ?
C     There are 2 things to do : COORDS, DISPS (strain?) and the COA
c.. need to move to 'R' above.
      ELSEIF (CKEY.EQ.'a') THEN
        DO I=1,NN  
               T  = GC(1,I)  
          GC(1,I) = GC(2,I)
          GC(2,I) = GC(3,I)
          GC(3,I) = T
        ENDDO
        DO IBASE = 0,NLDS*NN-1,NN     !- step thru load cases
          DO I = 1,NN
                           T  = GDISPS(1,IBASE+I)  
            GDISPS(1,IBASE+I) = GDISPS(2,IBASE+I)
            GDISPS(2,IBASE+I) = GDISPS(3,IBASE+I)
            GDISPS(3,IBASE+I) = T
          ENDDO
        ENDDO
c... now recalc ranges (could just topple too)
        CALL GET_MESH_RANGE (GC,IGC ,NN,NDIM, GC_MIN,GC_MAX,DIAG)
        DO J=1,3
          COA(J) = (GC_MAX(J) + GC_MIN(J) ) /2.
        ENDDO

C------------------------- load step number ----------------------------
c.. (obs)?) either +1, -1, or typed .. so do from the 'number-menu' only ?
c.. hence 'all' typed values would be prompted by their title :-)
c.. where do we do load step +1 and Load step -1  ?
      ELSEIF (CKEY.EQ.'l') THEN
        IF (KEY2.eq.1) THEN               !  +1
          LD = MAX (0.,LD-1.)
        ELSEIF (KEY2.eq.2)then            !  -1
          LD = MIN (REAL(NLDS),LD+1.)
        ELSE                              ! typed
          PRINT*,'enter load step 1.-->',NLDS,'(',LD,')'
          READ*,LD
        ENDIF

C-----------------------------------------------------------------------
C-----------------> else invalid key-press detected <-------------------
C-----------------------------------------------------------------------
      ELSE
        if (iverb>=3)
     &  WRITE (*,'(A,$)') '.?.'    !  or just 'beep' ?
      ENDIF

C-----------------> if CKEY='_' then we generate a refresh  
C (note. KEY=13 was a HARD redraw (no interupts alllowed) )
c     IF ( IBUT2.ne.0) CKEY='_'                    ! mouse-redraw
c     IF ( IBUT1.ne.0.or.KEY_WAITING@()) CKEY=' '  ! don't redraw ??
c     IF ( .not.(CKEY.eq.'_'.or.KEY.eq.13))  GOTO 1111   ! go back

      GOTO 1111               !-- back again !

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------




























C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C----- OK either we have moved or a new load case .. so draw -----------
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
c.. we jump here both forwards and backwards ! (*NOT TRUE* !)
c.. ie. jump straight to draw after data reading. *DRAW, print, etc. 
  901 CONTINUE

c... Only need to do if the table of elements has changed
c... eg. mesh_read, mirror, etc?
c... Hmm careful if we push to 3D cos the mesh changes but NEL doesn't
c---- also we must get COA for plotting (and ranges for the 'box')
c---- however this MAY kill manual COA setting ?

c... overall this is 'what we need to do when we have a NEW mesh'

c.. as a subroutine (GC,NUMS -> xme,yme,coa,range, facets, rings)
      IF (NEL.NE.NEL_OLD.or.NN.NE.NN_OLD) THEN
!  be carefull how we detect unmirroring
        DO I=1,NN
          IFL_NODES (I) = 1        !- mark all nodes back to 'to-draw'
        ENDDO

c............... set the initial view direction .................
c... maybe just use DIAG as a directional vector ?
        IF (NEL_OLD.le.0) THEN  !------ if no mesh to start with ------
          IF (NDIM.EQ.2) THEN            
            XME    = 0.   !- x-eye
            YME    = 0.   !- y-eye
          ELSEIF (NDIM.EQ.3) THEN      !- an inclined view for 3D  
!-- hmm note here some heuristic for deciding which is the 'front' of the model
!   like view from the quadrant that opposite teh one most of the mesh is in.
            XME    =-30.  !- x-eye
            YME    = 15.  !- y-eye
          ENDIF
        ENDIF

        NN_OLD = NN
        NEL_OLD = NEL


!--- find the bounding box hence centre on screen-----
        call post_pop_up ( 'Finding the Mesh extents')
        CALL GET_SECS (TIME(11))       
        CALL GET_MESH_RANGE (GC,IGC ,NN,NDIM, GC_MIN,GC_MAX,DIAG)
        CALL GET_DEFAULT_DISP_SCALE (GDISPS,MDF,NN,NODOF,NLDS,DIAG,FACT)
        DO J=1,3                         ! get 'look-at' point
          COA(J) = (GC_MAX(J) + GC_MIN(J) )/2.
        ENDDO
        IF (NDIM.LT.3) THEN  !-------- zap Z values of 2d meshes
!                              cf doing this when we read the mesh
          DO J=NDIM+1,3
            DO I=1,NN          ! (this makes drawing easier)
              GC(J,I) = 0.
            ENDDO
          ENDDO
        ENDIF

C-----------------------------------------------------------------------
C--------- turn the NUMS into a list of external facets 'FACETS' -------
C-----------------------------------------------------------------------
        CALL POST_POP_UP ( 'Determining internal/External Facets')
        CALL FSTRIP3 (NUMS,INUMS,NEL, NN,FACETS,IFCETS,NFCETS,PL2) !*very fast*
c... the next should be OK .. but 3D will 'wire-frame'
c     .. so need to BPC and depth-sort 
C        (cf 'backplanes' and other image artifacts. )
c .. Ok for now only do 2D explitly (3D only from the menu)
        call post_pop_up ( 'Computing Boundary polygons')
          CALL STRIP_EDGES (GC,IGC,NDIM,NN,NUMS,INUMS,NEL
     &      ,FACETS,IFCETS,NFCETS,PL2,RINGS,IRINGS,     0,0) !- all nodes
        CALL GET_SECS (TIME(12))
        call post_pop_up ( ' ' )      !- nul will close last pop-up
!       CALL POST_MENUS (1, 0,0,Codes)   !- repost all  (nice to drop pop-up)
      ENDIF
C-----------------------------------------------------------------------

      CALL GET_SECS (TIME(1))       !--- start of drawing --

C------------------ Find how many frames to draw -----------------------
c.... ie. usu. as the ANIMATION loop.
      IF (CV(53).EQ.0) THEN
        IPIC_LO = 1              !- usu. just 1 pic
        IPIC_HI = 1              !- to draw
      ELSE
        IPIC_LO = 1
        IPIC_HI = CV(54)
        IF (CV(55).LT.0)  IDEST = 10   !- flag to animate full screen
      ENDIF              

C-----------------------------------------------------------------------
C---------------------- Loop the multiple frames -----------------------
C-----------------------------------------------------------------------
c.. cf 'current camera params' and a set of key-views
c.. so animation does NOT affect the overall view

c.. also compare the use of a MACRO to 'fly' the camera through a scene
c  .. and note the use of 'save_this_camera', etc. camera(0) is the 
c      default? .. so animation is of the form t=.01, camera=(2) to ..
c    should really be able to linerarly interolate *any* params:
c    (if >2 cameras then maybe we can use splines)
c
c   1-  eye pos x,y,z (or alpha,theta)
c   2-  light pos x,y,z (or alpha,theta) - intensity too?
c   3-  view distance  (and perspective angle - log?))
c   4-  eye 'up' vector
c   5-  sinusoidal 'fact' - theta
c   6-  linear fact (maybe even combined with the sinusoidal)
c   7-  load step number eg with 4 'cameras'   t=  0.  .2    .7   1.0
c                                              ls= 0.  0.    12.  12.
c                      so has a start and and end 'rest' period.

c... so 
C    CALL  GET_VIEW_PARAMS (CAMERAS, .. ,  xme,coa,ld,fact1, ...)
c ... returns the camera(0) that we will use (ld,fact too)

      FACT1 = FACT                    !(default dsiplacement scale)
      DO IPIC = IPIC_LO, IPIC_HI
        IF (CV(53).GE.1) THEN         !-  Animation updates
          XME = XME + CV(56)
          YME = YME + CV(57)          !- rotate the camera
          XML = XML + CV(58)
          YML = YML + CV(59)
          coa (1) = coa(1) + dcoa(1)    !- move model (eg tunnels)
          coa (2) = coa(2) + dcoa(2)
          coa (3) = coa(3) + dcoa(3)

          IF (CV(60).eq.1) THEN           !- start at load step=0
            LD = (IPIC-1)/REAL(IPIC_HI-1) *NLDS   
          ELSEIF (CV(60).eq.2) THEN       !- User-defined range
            LD = an_ls1 + (IPIC-1)/REAL(IPIC_HI-1) * (an_ls2-an_ls1) 
          ELSEIF (CV(60).eq.3) THEN       !- Sinusoidal FACT for Eigenmodes ?  
            FACT1 = FACT * SIN(REAL(IPIC)/
     +              REAL(IPIC_HI) *2.*3.14159265)
          ENDIF
        ENDIF    !- various Animation opcodes
C.... also need to create the PCX (/PS) filenames if we are writing.

c-----------------------------------------------------------------------
C-------------------- Set up the Output Destination --------------------
c-----------------------------------------------------------------------
c  ** lets have this as a subroutine **
c     hence I can use from DANFE (usu. to PS maybe to X11 too)
c
c     given IDSET  -> open the graphics device and sets the default 
c
c      scaling  (ie. return the image size in pixels?)
c      CV(46) = SVGA mode etc.
c        (47) = printer type ? PS/LJ/..
c     (48),(49) = image size in pixels (pre-calculated !)
c  hence get the picture size parameters 
c  Plotfiles need their filename prompting for
c  .. maybe I might have >1 page in a PS file?
c  * double bufering is only for the screen (ignore for now ?)
c
c... IDEST = -1 force Printer IDEST mode from CV()
c... IDEST = 10 SVGA mode held in CV hence image size
c            11 for 256 colour modes
c            12 for 15bit colour     etc. ..?
c
c... set all the properties of the device.. # colours etc.
      CALL SET_IDEST (IDEST)
      CALL SET_FONT (101,.8,1.,1.)    !-reset font (why do here ?)

c.. cf window or full-screen eg. when animating
c-- if a window (VIEWPORT) then mung RES() to the smaller size.
c-- & perhaps fill in the 'gap' with a coloured frame?
c  (can use -ve size to 'turn off' (or be a border width ?). so zero is no-op
      IF (IDEST.ge.1.and.idest.le.2) THEN  !------- ordinary image draw ------
        CALL RESET_SIZES (RES)      
        if (idest.eq.2) CALL RESET_SIZES_2 (RES) !- hack to use the full VGA  
        IRESC  = 256   
        AR     = 1.                        ! changed to +1. 25-1-95
        CALL STANDARD_MENU_FONT(-1)        !- use ordinary VGA font

c------ VIEWPORTing ------
c DJK 21-1-97  - this is probably obsolete
        if (RES_WX.gt.0 .or.RES_wy.gt.0) then       !- 'window'ing
          RES(4) = res(4) + (RES(1)-RES_WX)/2
          RES(5) = res(5) + (RES(2)-RES_WY)/2
          RES(1) = RES_WX
          RES(2) = RES_WY
        elseif (RES_WX.lt.0 .or.RES_wy.lt.0) then   !- 'border'ing
          RES(1) = RES(1)- (-RES_WX)*2
          RES(2) = RES(2)- (-RES_WY)*2
          RES(4) = RES(4) + (-RES_WX)
          RES(5) = RES(5) + (-RES_WY)
        endif
 

c--------------------- Full-screen -------------------------
! probably not currently supproted under PGPLOT ?
c   from IMODE (CV(46), get x,y,ncols, .. so set mode, IDEST codes nbitplanes
      ELSEIF (IDEST.ge.10.and.idest.le.19) THEN 
c       CALL GET_GR_MODE (CV(46), RES, IRESC,AR2)
        IRESCP = NINT (LOG(REAL(IRESC))/LOG(2.))
        IF (IRESCP.EQ. 4) IDEST = 10    !- 16 colour: so dither / clip
        IF (IRESCP.EQ. 8) IDEST = 11    !- 256 cols 'standard'
        IF (IRESCP.EQ.15) IDEST = 12    !- 'hicolor' 
        IF (IRESCP.EQ.16) IDEST = 13    !- 'hicolor' (unsupported)
        IF (IRESCP.EQ.24) IDEST = 14    !- 'trucolor' 
c       IF (IRESCP.LT.15) CALL SET_ALL_DACS()   !- need to reset my DAC colours
c                                               !- but 'hicolor' bypasses them
c-- hmm cf draw_pgplto which also calls set_device()
        CALL SET_DEVICE           !- reset
     &  (IDEST,1.*res(1),1.*res(2),1.*iresc, 200.,220., 1.)

      ELSEIF (IDEST.ge.20) THEN   !--------- Printer/File output ------------  
c       IDEST  = CV(47)           !- pick up printer type
        RES(1) = CV(48)           ! Xwidth
        RES(2) = CV(49)           ! Ywidth
        RES(4) = 0                ! all printers will
        RES(5) = 0                ! have Zero offsets   (except PS tho !
        IRESC =  CV(50)           ! # of colours available
        AR    =  CV(51) / 100.    !('cos its stored as a %)
        CALL SET_DEVICE           !- reset
     &  (IDEST,1.*res(1),1.*res(2),1.*iresc, 200.,220., 1.)

!        IF (IDEST.EQ.20) THEN     !------------ PCX output -------------
!          PRINT*,'PCX File name: ? '     !(or defer until later?)
!          READ(*,'(A)') PICFILE
!          IRESCP = NINT (LOG(REAL(IRESC))/LOG(2.))
!c         CALL CREATE_SCREEN_BLOCK@ (RES(1),RES(2),IRESCP,VSCREEN)
!c         CALL OPEN_VSCREEN@ (VSCREEN,IFAIL_2)

      IF (IDEST.ge.30.and. IDEST.le.39) THEN !--------- Postscript output ---------
!    Lets not support direct printing - so always produce a file
!      so ' ' would auto generate a unique filename
!      - either do here or outside - hence we can animate to Postscript files
!    Lets support say PDF by calling a helper routine 
!       maybe if filenname ends in '.pdf'
!       cf gzipping if filename ends in .gz ?
          PRINT*,'PostScript File name: ? <CR> for LPT1'
          READ (*,'(A)') PICFILE
          IF (PICFILE.EQ.' ') PICFILE = 'PRINT'   !- so will print it
!       NO - set the page size string as part of 'setup-printer'
!       here we set A4,A3,.., Landscape/portrait - also logo/border.. 
          print*,' enter the page size, <CR=A4>'
          read(*,'(a)') type
          CALL SELECT_PS_PRINTER (TYPE, PICFILE,RES, LINE, version) 
          IF (TYPE.eq.'PHD') THEN    !- special case
            LINE = titles(1)
            IF (LINE.EQ.' ') LINE = FILE_IN
            CALL DR_PHD_LOGO (RES, LINE)
          ELSE
!           should have a option to skip the filename/datestamp, etc.
!           note we write the logo after the image
            CALL WRITE_PS_LOGOS (55,RES,FILE_IN,VERSION)    !- logo etc.
          ENDIF
c         print*,'res=', res(1),res(2),res(4),res(5),ar,iresc
        ELSEIF (IDEST.eq.9930) THEN !--------- GIF output ---------
!    Ok here we want to use PGPLOT to write to a bitmap file - say GIF
!     then can mung on the fly to say JPG etc. by calling a Linux tool

!        ELSEIF (IDEST.eq.40) THEN !----------- HP-LJ output ------------
!          PRINT*,'HP-LaserJet File name: ? <CR> for LPT1'
!          READ (*,'(A)') PICFILE
!          IF (PICFILE.EQ.' ') PICFILE = 'PRINT'   !- so will print it
!c         CALL OPEN_GPRINT_FILE@     (1,IFAIL_2)
!c         CALL SELECT_PCL_PRINTER@ (0,'A4',IPRES,RES(1),RES(2))

!        ELSEIF (IDEST.eq.50) THEN !----------- HP-GL output ------------
!          PRINT*,'HP7550 File name: <CR> for COM1'
!          READ(*,'(A)') PICFILE
!          IF (PICFILE.eq.' ') PICFILE = 'PRINT'
!c         CALL OPEN_PLOT_FILE@ (PICFILE, ifail_2)

!        ELSEIF (IDEST.eq.60) THEN !------ dot-matrix output (yuk!) -----
!c        CALL OPEN_GPRINT_DEVICE@     (1,IFAIL_2)
!c        CALL SELECT_DOT_MATRIX@ (0,RES(1),RES(2))

!- these two: DXF/ WMF use my own drivers to output the 2D primitves
        ELSEIF (IDEST.eq.70) THEN !------ DXF output -----
          PRINT*,'DXF File name: ?'
          READ (*,'(A)') PICFILE
          TYPE='A4'
          CALL SELECT_DXF_PRINTER (TYPE, PICFILE,RES, LINE, version) 

        ELSEIF (IDEST.eq.80) THEN !------ WMF output -----
          PRINT*,'WMF File name: ?'
          READ (*,'(A)') PICFILE
          TYPE='A4'
c         CALL SELECT_WMF_PRINTER (TYPE, PICFILE,RES, LINE, version) 

        ENDIF  !- printer types
        CALL POST_POP_UP ('Printing ... ') 
      ENDIF    !- output destinations


C-------- Find which windows we are going to draw on the page ----------
      IF (CV(37).EQ.-1) THEN
        I_WIN_LO = CV(41)        !- usu. just 1 pic
        I_WIN_HI = CV(41)        !- to draw
      ELSEIF (CV(37).EQ.1) THEN
        I_WIN_LO = 1             !- draw ALL load-steps
        I_WIN_HI = NLDS          !- clip to max #windows on screen ?
        I_WIN_HI=MIN(I_WIN_HI,cv(42)*cv(43))   !- clip
      ELSEIF (CV(37).EQ.2) THEN
        I_WIN_LO = 1
        I_WIN_HI = 4             !- draw the 4 view directions
      ENDIF              

c-----------------------------------------------------------------------
C------------------ Loop the multiple drawings on a page ---------------
c-----------------------------------------------------------------------
c cf an exit when all are done.
      DO I_WIN = I_WIN_LO, I_WIN_HI
      IF (CV(37).EQ.1) THEN         !---- tie load-step to pic # -------
        LD = I_WIN
      ELSEIF (CV(37).EQ.2) THEN     !---- view dirn linking to pic # ---
        IF (I_WIN.EQ.1) XME = 1001.      !- xz
        IF (I_WIN.EQ.2) XME = 1003.      !- orthogo.
        IF (I_WIN.EQ.3) XME = 1002.      !- xy
        IF (I_WIN.EQ.4) XME = 1004.      !- zx
      ENDIF

C-------------- create the drawing window area ------------------------- 
c.. ie multi-picture # --> x/y --> hilo of the window
c. rem. cv(41) = #, 42=nxpics,43=nypics, RES = image width/height
c.. note that this uses RES(1..5) so.. need to call-up
c.. the info for the current device .. eg if SVGA
c.. * IF AR < 0 then the pic_origin moves AND iwin_yw inverts
c.. * also 'tied' numbers move too ? (eg. pic #)

      iwinx =  mod(i_win-1  , cv(42) )     ! window -ix
      iwiny =  cv(43)-1 -  (i_win-1) / cv(42)       !        -iy

      win_xw = res(1)/ cv(42)             ! the window widths
      win_yw = res(2)/ cv(43)             !
      win_xo = res(4) + win_xw * iwinx    ! the bot-left   
      win_yo = res(5) + win_yw * iwiny    !   of the window

c------------- set the image->window transformation factors ------------ 
c.. SC_X/Y/Z are (ugly #) screen scale factors 'hacked' onto 
c.. subr. TRANSFORM to adjust image size AND aspect ratio
c.. X/Ycen are also hacks for the centre of the 'viewable' window
c.. CV(44,45) are the % shift of the centre (-50% to 50% ?)
C-- I don't really like PANNING .. better to properly move the COA
c-- these is simply done by just using the roatation bit of TR ?

C      SC_X = SSC * win_xw         ! x,y,z scaling factors (screen)
C      SC_Y = SSC * win_xw * AR
C      SC_Z = SSC * win_xw
C      XCEN = win_xo + win_xw/2. + win_xw/2.*CV(44)/100.    ! image-centre
C      YCEN = win_yo + win_yw/2. + win_xw/2.*CV(45)/100.    ! + panning (yuk?)
C      xf = -1.+2.*cv(44)/100.
C      yf = -1.+2.*cv(45)/100.

C----------------- create transformation matrices ----------------------
c... this is the 3d transform

      CALL XM2EYE (XME,YME,EYE  )      ! convert to a (unit) vector
      CALL XM2EYE (XML,YML,LIGHT)      !    "    "  "   "     "
      T = SIGN (1.,ABS(YME)-90.001)    ! 'fly -over-the-top' patch
      DO I=1,3
        UP(I) = SIGN( UP(I),T )
      ENDDO
 
c     CALL CAMERA (EYE,COA,UP, DEYE*DIAG,FEYE, PM)   !--- create PM() ---
      CALL CAMERA_2 (EYE,COA,UP, DEYE*DIAG/2.,FEYE, 0.,1.,PM)   !- create PM()

c.. cv(44..45) are the 'panning' offsets. (cg OpenGL)
      CALL CAM_WINDOW (win_xo, win_yo, win_xo+win_xw, win_yo+win_yw
     &  ,(1.+cv(44)/100.)/2.,(1.+cv(45)/100.)/2., SSC, 0.,AR, TS )

c--------------------------- combine view - matrices -------------------
c.. cf anisotropic scaling of the object here = POSTPEND_TR(xs,ys,zs)
      CALL PREPEND_TR(PM,TS)

C--------- extract displacemnts for this loadstep LD ---------

C     CALL EXTRACT_DISPS (GDISPS,MDF,IGDISPS, NODOF,NN,NLDS,
c    &  LD,DISPS,IDISPS)
c  **  as a subroutine , given LD (&NLDS) return DISPS() **
c.. ILD1,2 are the lo,hi 'adjacent' load steps  
c   (maybe do externaly ?) <- but would 'break' Eigenmode plots !
c  << unless I hack eigenmodes to a 'set' of elements !! >>

      ILD2 = MAX(1,(MIN(INT(LD+1),NLDS))) !- following load-step
      ILD1 = ILD2 - 1                     !- preceding load-step
      FAC2 = LD - REAL(ILD1)              ! hence the 2
      FAC1 =  1. - FAC2                   ! factors
      IF (ILD1.EQ.0) THEN                 !- patch
        ILD1 = 1                       !- why does this work ??
        FAC1 = 0.                      !- cos we may have NO load-steps !
      ENDIF
      IB1  = NN * (ILD1-1)       !- pointers to the table
      IB2  = NN * (ILD2-1)     
c.. an-iso factors .. in general we have 3 factors and 3 indeces 
c.. within GDISPS to use (cos may have PP first etc.)
c      XDS1 = XDS * FACT * FAC1     !- anisotropic scale factors
c      YDS1 = YDS * FACT * FAC1     !- >> maybe do this AND 'juggling'
c      ZDS1 = ZDS * FACT * FAC1     !- via a 'fudge' matrix (3x3) !
c      XDS2 = XDS * FACT * FAC2     !- therefore do on the 'outside'
c      YDS2 = YDS * FACT * FAC2     !- (cf the 'contour' vector)
c      ZDS2 = ZDS * FACT * FAC2
c...hmm I should only re-build disps if I am in the process of doing
c.. an animation ? (or else will kill the NORMALS for shading .OFF
c... ie. if CV(53) .GT.0 ??

      IF (NLDS.GT.0) THEN       !- build the DISPS table
        DO J=1,3                !- note that we later *FACT1
          DO I=1,NN             !- for actual plotting   
            DISPS(J,I) = 
     &      (FAC1 *GDISPS(J,IB1+I) +FAC2 *GDISPS(J,IB2+I) )
          ENDDO              ! IB1, IB2 not used again
        ENDDO
      ELSE            ! no disps present
        DO J=1,3                !- kill disps
          DO I=1,NN             !- (but beware of vector-normals)
            DISPS(J,I) = 0.     !- held in DISPS ? )
          ENDDO
        ENDDO
      ENDIF


c ( 12-7-95 Auto-glassing moved to sort_facets )

C------------- find out how many nodes are 'selected' ------------------
c .. used for selective drawing (eg if ringed with a zoom boz)
      NN_SELECTED = 0
      DO I=1,NN
        IF (IFL_NODES(I).NE.0) NN_SELECTED = NN_SELECTED + 1
      ENDDO
c     IF (NN_SELECTED.EQ.0) NN_SELECTED = NN  !- ie. 'none' resets to 'all'


c-- on visibility.
c   for Excavation / Construction, mouse-box-selection
c   It is neater to be able to flag FACETS() directly as:
c      available to draw / non-visible (cos excavated, etc)
c   Contrast this with -IMATs that totaly disables elements

c-----------------------------------------------------------------------
C---------------- loop the objects to get the 'draw-list' ---------------
c-----------------------------------------------------------------------
c      gives  PL1(1:NOBJECTS_D) = ordered list of facets

      NFCETS_D = 0
      NOBJECTS_D= 0                  ! total no. of drawable objects
      L_OVERLAP = .false.      !/* Assume no facets are partly visible */

c---------------- first insert the facets to draw ---------------
c      CALL GET_DRAW_LIST (GC,IGC,NDIM,NN,NUMS,INUMS,NEL
c     &      ,FACETS,IFCETS,NFCETS ,PM)
c      SUBROUTINE GET_DRAW_LIST (GC,IGC,NDIM,NN,NUMS,INUMS,NEL
c     &      ,FACETS,IFCETS,NFCETS ,PM)
c
c     This returns the list of FACETS to draw in order from back to front
c            Dan Kidger 27-2-98
c
c     The operations:
c       1:  Loop facets and get its IFACE, IMAT, toucher, etc.
c       1a: If always vis, skip next few tests.
c       2:   CYCLE if invis IMAT (<=0)
c       3:   CYCLE if build/excav and this IMAT is invis
c       4:   CYCLE if 'never draw' this IFACE (1:6)
c       5:   find if toucher is vis. (as above)
c       6:      CYCLE if it is!

c       7:  Get the nodes (+coords) of this facet, hence screen coords too
c       8:   CYCLE if some of its nodes are 'not-selected'
c       9:   CYCLE if all of its nodes are off-screen  win_xo,win_xw,.. 
c            (and count the # of facets that are partly off-screen)
c      10:   Get facet normal, CYCLE if backfacing (&culling)
c          ( if cull both onlt rods will survive)
c          (NDIME=2 should be double-sided and so not culled)
c      11: calc its min.max/mean Z
c      12: store one-of (both if we use  Evan's algorithm?
c          (fudge rods and plates so that 1d is 'closer' than 2d > 3d
c      13: depth-sort, and so return list of NFCETS_D and their global facet#
 
c   .. maybe do in 2 passes .. first abstract a subset of FACETS after
c      zapping internals and 'never-draws'
c     = 1/ clip off_Screen.
c       2/ Z_BUFFER

c.. I could mark the IFACE column as -ve if as -ve if we are not to 
c     consider this facet (eg. if excavated)
c

c .. I could mark the 'touch' column as -ve if the adjoining material 
c    is of a different material type?--> so can 'always' draw?
c--> could be a subroutine : from FACETS and GC to 'list-to-draw'
c  >> note the effects of B_P_C and 'Force_draw'

c.. nice to get stats on the total # of facets
c.. and the # killed for each reason: invis, never draw, BPC,off-screen

c--- *** I may have more 'artifacts' to depth-sort than just facets
c        eg. boundary polygon's  .. 'back-planes'

c     INTEGER NUMS(INUMS,*), FACETS(3,*)
c     REAL GC(IGC,*)
c     REAL PM(4,4)
C     LOGICAL L_OVERLAP
c     INTEGER ID_FACE(6)

c   Controls:
c      PM(4,4) transformation matrix
c      load-step# controlled excav. :-(
c      ID_FACE(6) controlled force draw, force nodraw, normal
c      IFL_NODES (NN) for node subset selection
c      win_xo,win_xw, win_yo,win_yw window clipping
c      (win_zo, win_zw for front/back clipping?)
c      culling: 00,01,10,11  front/back/neither
c      depth:  far,near,mid
c      flag to not depth-sort (never for NDIME=2)


c.. really should call via my device driver
c- move until later ?
c     call PGBBUF ()  !- enter buffered state (DJK 27-3-01)
      DO IFC=1,NFCETS
        IEL   =     FACETS(1,IFC)
        IFACE = ABS(FACETS(2,IFC))     !(what are -ve flags for ?)
        ITOUCH=     FACETS(3,IFC)

        CALL GET_EL_IMAT (NUMS,INUMS,IEL, IMAT)

c------ Auto-vis-if BUILD/EXCAV. --------
c-- invis facets : imat<=0, not yet built / excavated
c (this probably no longer is relavant)
        vis = .true.
        IF (IMAT.LE.0) THEN            ! 'skip' (as 'invis')
          vis =.false.
        ELSEIF (CV(52).EQ.1.AND.IMAT.NE.1.and.IMAT.GT.LD+1.5 ) THEN
          vis =.false.
        ELSEIF (CV(52).EQ.2.AND.IMAT.NE.1.and.IMAT.LT.LD+1.51) THEN
          vis =.false.

c-- this facet direction flagged for 'never draw'
        ELSEIF (ID_FACE(IFACE).eq.1) THEN
          VIS =.FALSE.                      ! never  'draw' this side ?

c        IF (ITOUCH.eq.0) THEN               !-- draw exterior faces
c          GOTO 777                   
c        ELSE                        

        ELSEIF (ITOUCH.ne.0) THEN                !-- check vis of its toucher
          IEL2 = FACETS(1,ITOUCH)   
          CALL GET_EL_IMAT (NUMS,INUMS,IEL2, IMAT2)
          vis2 = .true.
          IF (IMAT2.le.0) THEN
            vis2=.false.   
          ELSEIF (CV(31).ne.0) THEN    !- shrunk elements allow visibility
            vis2=.false.               
          ELSEIF (CV(52).EQ.1.AND.IMAT2.NE.1.and.IMAT2.GT.LD+1.5 ) THEN
            vis2=.false.               
          ELSEIF (CV(52).EQ.2.AND.IMAT2.NE.1.and.IMAT2.LT.LD+1.51) THEN
            vis2=.false.               
          ENDIF
          IF (vis2) vis=.false.
        ENDIF

        IF (ID_FACE(IFACE).eq.2) GOTO 777   ! always 'draw' this side ?
        IF (.not.VIS) GOTO 701            !- skip invisble facets


c-------------- now get & transform this facets' nodes -----------------
c.. maybe have a list of 'flagged' elements to so can check whether to
C.. draw them in the previous section
c  ? be careful of 1d rod elements here
  777 CONTINUE
      CALL GET_ELEMENT 
     &   (NUMS,INUMS,IEL, NUM, NOD,NDIME,ITYPE, IMAT,IU1,IU2,IU3)
      CALL GET_FACE (NUM,NOD,NDIME,ITYPE,IFACE,FS,NN_F,NN_FT, 2 )

c-------- check if this facet is current 'not-selected' ----
c.. contrast 'facet selected' if at least one node is selected
C.. or if all must be selected (default)
      IF (NN_SELECTED.LT.NN) THEN     !- if we are using a subset
        II = 0
        DO I=1, NN_FT
          IF (IFL_NODES(FS(I)) .NE.0) II = II+1
        ENDDO
c       IF (II.eq.0) GOTO 701         !- skip cos no nodes are marked
        IF (II.LT.NN_FT) GOTO 701     !- skip cos some nodes are unmarked
      ENDIF

c------------------ transform points to screen coords ------------------
C      Z_MIN,Z_MAX,Z_MEAN ???
      N_VISIBLE = 0           ! # of nodes of this facet that are on-screen
      DO I=1,NN_FT
        II = FS(I)
        DO J=1,3
          GPT(J) = GC(J,II) + cv(2)*FACT1 * DISPS(J,II)  !- include disps ?
        ENDDO
        CALL TRANSFORM2 (GPT,SPT ,PM)
C       CALL TRANSFORM (GPT,SPT, PM, IROT, XCEN,YCEN,SC_X,SC_Y,SC_Z)
        DO J=1,3
          SC_F (I,J) = SPT(J)  !- store the screen coords  
        ENDDO  

C---------------------- now test for 'off' screen' ---------------------
c.... this should be 'optional' .. or at least fixed for 'buffering'
c.. cos then win_yw may be -ve it is better to test that the
c.. produce is -ve (ie. SPT is on the INSIDE of the box

        IF ((SPT(1)-win_xo)*(SPT(1)-win_xo-win_xw).lt.0) THEN
          IF ((SPT(2)-win_yo)*(SPT(2)-win_yo-win_yw).lt.0) THEN
            N_VISIBLE = N_VISIBLE + 1
          ENDIF
        ENDIF
      ENDDO            !-- end of loop_a_facets_nodes

c.. 21-4-98 for WMF files we want to skip if *any* are off-screen
c (because we are not able to do our own clipping)
      IF (IDEST.eq.80.and.N_VISIBLE.LT.NN_FT) GOTO 701

      IF (N_VISIBLE.eq.0) GOTO 701   !-- skip as no_nodes are on_screen
c.. next is only for on-screen drawing NOT for PS , etc. !
      L_OVERLAP = L_OVERLAP.OR.(N_VISIBLE.LT.NN_FT) ! record 'menu overlaps'

C------------------ back-face polygon culling --------------------------
c.. only cull for 3D meshes
c.. but then don't cull for 1d entities
c.. also perhaps dont cull plates and shells
c     IF (NDIM.EQ.3.and.ndime.ge.2) THEN      ! only cull for '3D' meshes
      IF (NDIM.EQ.3.and.ndime.ge.3) THEN      ! only cull for '3D' meshes
        C3 = 0.
        DO I=1,NN_F
          IP1 = MOD (I,NN_F) + 1
          C3 = C3 + (SC_F(I,2)+SC_F(IP1,2)) * (SC_F(IP1,1)-SC_F(I,1))/2.
        ENDDO
        C3 = -C3 * AR                         ! weight wrt. the screen topology
        IF (CV(32).EQ.1.AND.C3.GT.0) GOTO 701 ! cull 'fronts'
        IF (CV(32).EQ.2.AND.C3.LT.0) GOTO 701 ! cull 'backs'
      ENDIF

C--------- OK this facet IS to be drawn.. so get its 'z'-value ---------
       IF (NDIM.eq.3.and.CV(17).NE.0 ) THEN      !- only if we ARE depth-sorting
         ZVAL = SC_F(1,3)
         IF (CV(17).EQ.2) THEN                    ! furthest point
           DO I=1,NN_FT
             ZVAL = MIN(ZVAL,SC_F (I,3))
           ENDDO
         ELSEIF (CV(17).EQ.1) THEN                ! nearest point   ?
           DO I=1,NN_FT
             ZVAL = MAX(ZVAL,SC_F (I,3))
           ENDDO
         ELSEIF (CV(17).EQ.3) THEN                ! centroid point 
           DO I=1,NN_FT
             ZVAL = ZVAL + SC_F (I,3)
           ENDDO
           ZVAL = ZVAL / REAL(NN_FT)
         ENDIF
         IF (ZVAL.lt. 0.)   GOTO 701         !- near xy-clipping-plane
c        IF (ZVAL.gt.9999.) GOTO 701         !- far  xy-clipping-plane

       ELSE
         ZVAL = 1.                          !- dummy depth for 2d ?
       ENDIF                           !-- only if we are sorting facets

c--- 17-2-98 patch to make 2d plates lie over 3d bricks (eg newspine.mrc)
       IF (NDIME.eq.2) ZVAL=ZVAL*1.00001
       IF (NDIME.eq.1) ZVAL=ZVAL*1.00002

c--- 5-8-98 Notes on 'other' objects in the draw list
c After the facets, I can loop and add any objects that I like.
c eg. 1.The bounding cube back-walls (cf a graph-graticle)
c     2.Isosurfaces - from marching cubes (only for brick elements)
c     3.The RINGS - bpc'd  
c       for RINGs if we edge we want to draw them slightly in front, 
c      faces slightly behind, hence RINGS generate *2* polygons each?
c
c  For each we store 2 pieces of data:
c     a. A code for the type '0=facet,10=box,20=isosurface,30=RINGS,40=..'
c     b. A pointer to the record:-
c        1. Facets : its number in FACETS(3,*) cf el# overloaded with sf_number
c        2. Bounding cube: just a number 1-6; but note surface rulings?
c        3. Isosurface: The element # overloaded with the (1-5) sf_number
c        4. RINGS: pointer to the start of its structure within RINGS :-)
c
c

c--- store this z-depth  --
       NFCETS_D = NFCETS_D + 1           !  # of Facets
       NOBJECTS_D = NOBJECTS_D + 1       ! '# of Onjects to draw'
       ZBUF (NOBJECTS_D) = ZVAL          ! store depth
       PL3  (NOBJECTS_D) = 0             ! a normal facet
       PL2  (NOBJECTS_D) = IFC           ! and this facets actaul number

  701 CONTINUE !.......... end-of 'skip-to' (='ignore this facet')
      ENDDO  !............ end of the facet loop (IFC)

c-----------b: Add Faces of the Bounding box (10) ---------------
c.. simply create an 8nb using the BB
      if (cv(25).ge.0.or.cv(26).ge.0) then
      CALL WTHATN (8,3,1, coord,icoord)
      zmin=123.
      zmax=123.
      do i=1,8
        do j=1,3
          gpt(j) = (gc_max(j)+gc_min(j))/2. +
     &            ((gc_max(j)-gc_min(j))/2. + diag*100.*cv(23))
     &                 * coord(i,j)
        enddo
        CALL TRANSFORM2 (GPT,SPT,PM)
        DO J=1,3
          SC_F (I,J) = SPT(J)  !- store the screen coords  
        ENDDO                  !- ie the depth SC_F(:,3) of each.
        if (i.eq.1.or.spt(3).lt.zmin)then
            zmin=spt(3)
            imin=i
        endif
        if (i.eq.1.or.spt(3).gt.zmax)then
            zmax=spt(3)
            imax=i
        endif
      enddo
      DO IFACE=1,6
        CALL GET_FACE2 (8,3,1,IFACE,FS,NN_F,NN_FT, 2) 
        nn_f=4 !- to make sure?
        C3 = 0.
        DO I=1,NN_F
          j = fs(i)
          IP1 = fs(MOD (I,NN_F) + 1)
          C3 = C3 + (SC_F(j,2)+SC_F(IP1,2)) * (SC_F(IP1,1)-SC_F(j,1))/2.
        ENDDO
        C3 = -C3 * AR                         ! weight wrt. the screen topology

c.. now find whether this facet contains the far node or the near node
c.. assume it is the far node
        zval=zmin
        ifront=-1
c       do j=1,4
c         if (fs(j).eq.imax) then
          if (c3.lt.0.) then
            ifront=1  !- at front, so clip ?
            zval=zmax
          endif 
c       enddo
c       if (ifront.eq.-1) then
        if (1.eq.1) then
          NOBJECTS_D = NOBJECTS_D + 1       ! = '# of Facets to draw'
          ZBUF (NOBJECTS_D) = ZVAL          ! store depth
          PL3  (NOBJECTS_D) = 10            ! This is a bounding box
          PL2  (NOBJECTS_D) = IFACE*ifront    ! overload with front/back
        endif
      enddo
      endif

c-----------c: Add Element based symbols (99) ---------------
c ..So here add a cue to the centre of all elements (imat>0), by 
c   simply computing either the average displaced nodal position or (better)
c   to sample at the centre of the element..
c
c When drawing:
c    1/ draw a glyph (dot) coloured wrt 
c
c
c    2/ draw element:element (facet) connections
c    3/
c    4/
      if (cv(81).ge.0) then
        do iel=1,nel
          CALL GET_ELEMENT 
     &    (NUMS,INUMS,IEL, NUM, NOD,NDIME,ITYPE, IMAT,IU1,IU2,IU3)
          if (imat.ge.0) then
            CALL GET_COORD (NUM,NOD,NDIM,GC,IGC,COORD,ICOORD)    !- coords
            do j=1,ndim            !- find the centroid
              gpt(j) = 0.
              do i=1,nod
                gpt(j)=gpt(j) + coord(i,j)
              enddo
                gpt(j)=gpt(j)/real(nod)
            enddo
            CALL TRANSFORM2 (GPT,SPT,PM)
            zval=spt(3)

            NOBJECTS_D = NOBJECTS_D + 1       ! = '# of Facets to draw'
            ZBUF (NOBJECTS_D) = ZVAL          ! store depth
            PL3  (NOBJECTS_D) = 50            ! This is an element centre
            PL2  (NOBJECTS_D) = iel           ! overload with front/back
          endif   !- visible materials
        enddo   !- loop elements
      endif

c-----------d: Add Isosurface (20) ---------------
      if (cv(83).ge.0) then
        nn_iso=0
        nel_iso=0
        do iel=1,nel
          CALL GET_ELEMENT 
     &     (NUMS,INUMS,IEL, NUM, NOD,NDIME,ITYPE, IMAT,IU1,IU2,IU3)
c.. skip if not an 8nb
c.. for 14/20nb's need to get a list (via wthatn?) of the 8 corners
c ?I suppose sub-faceting might be allowed?
          if (imat.gt.0.and.ndime.eq.3) then
c.. need to morph NUM and NOD  20nb->8nb here
c         IF (NOD.NE.8) CALL MORPH_NUM (NUM, NOD,NDIME,ITYPE)

          CALL WTHATN (NOD,NDIME,ITYPE,LC_SF,ISC)
          CALL GET_COORD (NUM,NOD,NDIM,GC,IGC,COORD,ICOORD)    !- coords
          DO I=1,NOD
            DO J=1,NDIM
              II = NUM(I)
              DISPS_EL (I,J) = DISPS(J,II)
            ENDDO
          ENDDO
c.. assemble VALS_ELEM(1:no) with the 8 nodal values
          do i=1,nod
c.. use the generic? (via wthatn?)
            IOP_CONT = CV(13)            !- ie. What are we contouring?
c.. need to set LC_SF(isc) to the local coords of this node
c... likewise sample for the coordinates? (cf subfaceting)
             CALL SAMPLE (COORD,ICOORD,DISPS_EL,ICOORD,NOD,ndime,ndim,
     &                 VAL,VEC,LC_SF,ISC,I,IOP_CONT)
c           val= disps (2,num(i))    !- hack to the y-displacement for now
            vals_elem (i) = val
            c_r(1) = min(c_r(1),val)      !- update range
            c_r(2) = max(c_r(2),val)
          enddo

c... perhaps it is sufficient to only depth-sort element centres rather 
c   than the isosurface polygons themselves?
c.. hence mini-depth-sort each of the (5) iso-facets as we draw them

c---- loop the isosurface levels
        do ilevel=1,1
c       r_isolevel=(c_r(3)+c_r(4))/2.     !- hack to mean value.
        r_isolevel=c_r(3) + (c_r(4)-c_r(3))*c_r(9)  !- factored 0.->1.
c----- invoke the algorithm ----
c.. perhaps init npts,ntris so we can create a 'new' mesh, hence MERGE_NODES
c  so we can smooth globaly and get normal vectors?
c
       ntris=0
       nel_iso_old=nel_iso
c       call mcubesmp_Polygonise (coord,icoord,vals_elem,r_isolevel, 
c    &     gc2,igc2,nn_iso, nums2,inums2,nel_iso) 

       ntris=nel_iso-ntris
c      do itri=1,ntris       !--- loop the isosurface triangles
       do itri=nel_iso_old+1, nel_iso    !--- loop the (new) isosurface triangles

       nn_f=3
       do ipt=1,nn_f 
         II = nums2(ipt,Itri)
         DO J=1,3
           GPT(J) = GC2(J,II) 
         ENDDO
         CALL TRANSFORM2 (GPT,SPT ,PM)
         DO J=1,3
           SC_F (Ipt,J) = SPT(J)  
         ENDDO  
       enddo

c.. no reason to bpc isosurfaces cos double-sided
c.. extract near-point of this triangle ZVAL (cf CV(17)==1)
c 1. to draw:   put points into x() and y() and flat_shade
c.. or at least pick up the colour shade from the known range
c 2. to store: get near-point and centroid and store.

c ? overload '20' with the contour number - hence its colour?

c- should clip to screen here too.
c        N_VISIBLE = 0
c        DO I=1,3
c        IF ((SPT(1)-win_xo)*(SPT(1)-win_xo-win_xw).lt.0) THEN
c          IF ((SPT(2)-win_yo)*(SPT(2)-win_yo-win_yw).lt.0) THEN
c            N_VISIBLE = N_VISIBLE + 1
c          ENDIF
c        ENDIF
c        ENDDO

            zval=max(SC_F(1,3),SC_F(2,3),SC_F(3,3))   !- nearest point
            NOBJECTS_D = NOBJECTS_D + 1       ! = '# of Facets to draw'
            ZBUF (NOBJECTS_D) = ZVAL          ! store depth
            PL3  (NOBJECTS_D) = 20            ! This is an isosurface
c           PL2  (NOBJECTS_D)  = iel+nel*(itri-1) ! overload with tri #
            PL2  (NOBJECTS_D)  = itri ! overload with tri #
            enddo     !- isosurface-patches for this element (max=5)
            enddo   !- loop different surfaces
          endif   !- visible materials
        enddo   !- loop elements
      endif



c-----------e: Add ring-edges where wanted. (30) -------------
      IF (cv(62).GE.0.or.cv(64).GE.0.or.cv(66).GE.0) THEN ! any edges to draw?
        CALL SORT_RINGS (RINGS,0,GC,IGC,ndim,DISPS,IDISPS
     &           ,ZBUF,PL2,PL3,IFCETS,NOBJECTS_D, FACT,CV(32),AR,PM)
      ENDIF



c-----------------------------------------------------------------------
C---------------- depth sort the 'drawable' facets ---------------------
c-----------------------------------------------------------------------
      CALL GET_SECS (TIME(2))  !--  time to end of transformation
      IF (NDIM.eq.3.and.CV(17).NE.0) THEN
        CALL INDEX_REALS (ZBUF,NOBJECTS_D, PL1)     !- in MATRIX.F :-)

c------ remember the closest and furthest (so can colour code) ---------
c.. or calc afterwards since we know which 2 facets they are.
        Z_CLOSEST = ZBUF(PL1(1))
        Z_FURTHEST= ZBUF(PL1(NFCETS_D))
c        DO I=1,NOBJECTS_D         !- ..so just number sequentially
c          PL1 (I) = pl2(pl1(i))  !- pick up the actual facet numbers
c        ENDDO
      ELSE                      !- no depth-sort 
        DO I=1,NOBJECTS_D         !- ..so just number sequentially
c         PL1 (I) = pl2(I)
          PL1 (I) = I
        ENDDO
      ENDIF
c     RETURN
C     END


c----------------- find the frontmost facet for each node --------------
c 25-2-98 It is wasteful to draw a node# (nodal dot, etc.) 4 times
c  so store for each node which, of its (4) facets is closest to the viewer
c  This is quite simple:
c    1: null P()
c    2: loop facets from *front* to back.
c    3: loop its nodes; set P(inode)=ifc if P(inode)==0
c    4: done!
c    DO I=1,NN
c      PL2(I)=0
c    ENDDO
c    DO I_F=1, NFCETS_D,1,-1         ! loop front to back
C      IFC = PL1(I_F)
C      IEL  = FACETS (1,IFC)         ! hence : el #
C      IFACE= FACETS (2,IFC)         !       : &face dirn.
C      CALL GET_ELEMENT (NUMS,INUMS,IEL, NUM,
C   &                   NOD,NDIME,ITYPE, IMAT,IU1,IU2,IU3)
C      CALL GET_FACE (NUM,NOD,NDIME,ITYPE,IFACE,FS,NN_F,NN_FT, 2)
C      DO I=1,NN_FT
C        IF (PL2(FS(I).eq.0) PL2(FS(I)) = IFC  !- store if not yet used
C      ENDDO
c    ENDDO
c    can now count how many nodes are actualy going to be drawn?

      CALL GET_SECS (TIME(3))  !-- time to (after) depth-sorting the facets 

C-----------------------------------------------------------------------

c... Ok so now we have an ordered list of facets to draw
c... so open any graphics devices.. draw any 'furniture'
C... then loop facets and draw .. and finally draw any other info (titles...)

C=======================================================================
C========================  draw the new image ==========================
C=======================================================================

c-----------------------------------------------------------------------
c  Note the 'layers'
c     ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออป
c     บ                Layers in the Image                    บ
c     ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออน
c     บ   -0- screen background                               บ
c     บ   -0- axes & 'backplanes'                             บ
c     บ    1: material colour                                 บ
c     บ    2:   light-shading (either/or do with material ?)  บ
c     บ    3:   disp. contours (etc.)                         บ
c     บ    4:   flow-net lines                                บ
c     บ    5:   re-gridded surface lines                      บ
c     บ    5a:  displacement vectors                          บ
c     บ    6:   sub-facet edges                               บ
c     บ    7: facet edges                                     บ
c     บ    8: node symbols / numbers                          บ
c     บ    9: element # / material #                          บ
c     บ   -0- contour legend                                  บ
c     บ   -0- Titles                                          บ
c     ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
c-----------------------------------------------------------------------

c.. really should call via my device driver
c..   should use CV(38) option to control buffering
      if (cv(38).ne.0)
     & call PGBBUF ()  !- enter buffered state (DJK 27-3-01)
C------------------------ clip to the window ----------------------------
C.. currently only Postscript can do this
c.. we need 41=unclip too (in PS this is just 'grestore'?)
!     IF (IDEST.EQ.30) THEN
         CALL MAKE_BOX (WIN_XO,WIN_YO,WIN_XW,WIN_YW,X,Y)
         CALL DR_PRIM (LINE,X,Y,4,0,40)     !- 40 = clip
!     ENDIF
C------------------------ blank window area ----------------------------
      
C... an alternative to this is to use a given PCX file as a backdrop
C... or a 'function' (eg. variable tints from top to bottom)
C... This MUST use the 'primitive' draw option !
c... hmmm .. really I want to use my *OWN* fill_rectangle routine !

c.... if to screen make sure that we first clear the screen.
      IF ( (IDEST.EQ.1.or.IDEST.eq.10.or.IDEST.eq.11)
     &   .and.cv(20).eq.-1) THEN
        CALL MAKE_BOX (WIN_XO,WIN_YO,WIN_XW,WIN_YW,X,Y)
c       CALL DR_PRIM (LINE,X,Y,4, 254, 2)     !- 0='black', 254 = dk. blue. 
        CALL DR_PRIM (LINE,X,Y,4, 95, 2)     !- 0='black', 95 = dk. blue. 

      ELSEIF (CV(20).NE.-1) THEN
        IF (IDEST.NE.50) THEN             !--- NOT pen-plotters!
          IF (CV(20).LT.1000) THEN          !-- solid colour
            CALL MAKE_BOX (WIN_XO,WIN_YO,WIN_XW,WIN_YW,X,Y)
            CALL DR_PRIM (LINE,X,Y,4,cv(20),2)     !- filled polygon

C         .....  include code here for patterned backdrops
c         .....  eg. graded, grided,PCX-file, fractal? 
          ELSEIF (CV(20).lt.2000) then
           i = cv(20) - 1000
c          CALL RESTORE_SCREEN_BLOCK@ (RES(4),RES(5),IMAGE(I),0,IFAIL_2)

          ENDIF
        ENDIF
      ENDIF

C-------------- draw a graticule over the background -------------------
C---------------- anything else to draw at the begining ? --------------
C ... nice to draw the 'back-planes too' :-)
C ... ie. loop the '6' sides of the surrounding cuboid
C ... then b_p_c each, then loop each's 'squares' & draw wire-frame/fill
C ... to the nearest 10**size.. ie. square size =1mm/5m/20km etc.
C... could even create as a real element ?
C... but just loop all six faces (GET_FACE of 8nb) . getface normal
C... if 'forwards ' then draw.
c---> push this to a subroutine too !

C------------------------- plot the axes -------------------------------
c.. cf as an icon too. -eg. in the top-left of the window
c.. draw screen centered when rotating view
c.. draw in the corner (after the main image) when printing
      IF (CV(24).NE.-1) THEN

c     CALL CAMERA_2 (EYE,COA,UP, DEYE*DIAG/2.,FEYE, 0.,1.,PM)   !- create PM()
c      CALL CAM_WINDOW (win_xo, win_yo, win_xo+win_xw, win_yo+win_yw
c     &  ,1.,1., 4.*DIAG., 0.,AR, TS )
c     CALL PREPEND_TR(PM,TS)

      DO I=1,3                ! loop the 3 axes
        DO J=1,3
          GPT(J) = COA(J)
          IF (I.EQ.J) GPT(J) = GPT(J) + DIAG/2.  ! get the axis
        ENDDO
        CALL TRANSFORM2 (COA,SPT ,PM)
        CALL TRANSFORM2 (GPT,SPT2,PM)
        CALL DR_ARROW  (SPT,SPT2,10,15,CV(24))   !- cf DR_PRIM
        IF (CV(27).NE.-1) THEN   ! label the axis
          LINE(1:) = CHAR (ICHAR('w') + I )  ! ie 'x','y','z'
          X(1) = SPT2(1)               !.. in top LH corner
          Y(1) = SPT2(2) 
          CALL DR_PRIM (LINE,X,Y,1,CV(27),20)
        ENDIF
      ENDDO
      ENDIF

C--------------------- Fill the 'RING' boundaries ----------------------
c.. need to add these to the depth-sorted list - as *behind* the facets 
c    that lie above it (ie depth-srt based on (backmost node?)
c
c      CALL RINGS (RINGS,0,GC,IGC,DISPS,IDISPS,0.  , 'fill',CV(63) !-- undef.
c     &                ,PM)
c      CALL DRAW_RINGS (RINGS,0,GC,IGC,DISPS,IDISPS,FACT, 'fill',CV(65) !- deform.
c     &                ,PM)

C---------------- set min/max contour values etc. ----------------------
c.. only so we can 'spot' the min/max values found
c.. (so just initiate if this is the very first sub-facet ?
      C_R(1) =  1.e20  ! set the max/min contour values
      C_R(2) = -1.e20  ! to very high/low numbers
      C_R(8) = 1.e-20  !  max stress vector size. found

C-----------------------------------------------------------------------
C------------------- loop objects and plot them ------------------------
C-----------------------------------------------------------------------

      DO I_F = 1, NOBJECTS_D  ! =just the NFCETS that are to be drawn!

        IOBJECT_TYPE = PL3(pl1(I_F))
        IFC  = pl2(PL1(I_F))              !- the facet's number

C       IF (IOBJECT_TYPE.EQ. 0) THEN ...     !--- A Facet ---
        IF (IOBJECT_TYPE.EQ.10) THEN         !--- A box-face ---
          IFACE = abs(IFC)
          CALL GET_FACE2 (8,3,1,IFACE,FS,NN_F,NN_FT, 2) 
          CALL WTHATN (8,3,1, coord,icoord)
          do i=1,nn_f
            do j=1,3
              gpt(j) = (gc_max(j)+gc_min(j))/2. +
     &            ((gc_max(j)-gc_min(j))/2. + diag*100.*cv(23))
     &                     * coord(fs(i),j)
            enddo
            CALL TRANSFORM2 (GPT,SPT,PM)
            DO J=1,3
              SC_F (I,J) = SPT(J)  !- store the screen coords  
            ENDDO                  !- ie the depth SC_F(:,3) of each.
          ENDDO
          if (ifc.lt.0)  !- not front faces
     &    CALL DRAW_POLY (SC_F,ISC,4,cv(26),2)    ! fill
          CALL DRAW_POLY (SC_F,ISC,4,cv(25),1)    ! edges
          GOTO 1991       !- skip to the next object
        ENDIF
        IF (IOBJECT_TYPE.EQ.50) THEN         !--- Element centered value --
          iel=ifc
          CALL GET_ELEMENT 
     &    (NUMS,INUMS,IEL, NUM, NOD,NDIME,ITYPE, IMAT,IU1,IU2,IU3)
          CALL GET_COORD (NUM,NOD,NDIM,GC,IGC,COORD,ICOORD)    !- coords
          do j=1,ndim            !- find the centroid
            gpt(j) = 0.
            do i=1,nod
              gpt(j)=gpt(j) + coord(i,j)
            enddo
              gpt(j)=gpt(j)/real(nod)
          enddo
          CALL TRANSFORM2 (GPT,SPT,PM)
          x(1) = spt(1)
          y(1) = spt(2)
          WRITE (LINE,'(I7)') IEL
          LINE = LINE(ISTR_BEG(LINE):)           ! ie. left-justify ?
          x(2) = 4.                              !- point size
          icol=15   !- hack for now
          CALL DR_PRIM (LINE,X,Y,1,icol,21)


          GOTO 1991       !- skip to the next object
        ENDIF


c------------------------ Isosurface drawing -----------------------
        IF (IOBJECT_TYPE.EQ.20) THEN    !--- Isosurface ---
c       IEL  = FACETS (1,IFC)           ! el #
          iel=ifc
          DO I=1,3
            DO J=1,3
              GPT(J)=GC2(J,NUMS2(I,IEL))
            ENDDO
            CALL TRANSFORM2 (GPT,SPT ,PM)
            DO J=1,3
              c_sf(i,j)=spt(j)
            ENDDO
            x(i)=spt(1)
            y(i)=spt(2)
          ENDDO
          CALL GET_DC_NORMAL (C_SF,ISC,3, 3, DC ) 
          SHADE = (LIGHT(1)*dc(1)+LIGHT(2)*dC(2)+LIGHT(3)*DC(3)) !- dot prod.
          SHADE = -SHADE      !-hack ?
          SHADE = MIN(MAX(0.001,SHADE),.9999) !- clips -ve's too :-)
          ICOL = 21 + int(SHADE+1)*CV(18)            ! CV(18) = # cols =13 usu.
          ICOL2= 21 + int((.5+1)*CV(18))               ! mid-range value
          if (cv(83).ge.2)                        !- bit mask: 0=lines,1=fill
     &    CALL DR_PRIM (LINE,X,Y,3,icol,2)        !- filled polygon
          if (mod(cv(83),2).eq.1)
     &    CALL DR_PRIM (LINE,X,Y,3,icol2,1)           !- edges
c.. we need to calculate the shade here too
c.. also we might even contour over this surface - from data in an 'extra'
c   column of GC2 ?

c  so we know it is an isosurface patch - 
c    we need to get its 3 coords, so.. 
c   < for smoothed normals, we need to create and MERGE_NODES, thence
c     I guess we store the NUMS too, hence it becomes  just like any 
c     other element too? >
c
c  1 Get element and patch # from IFC (by overloading) 
c  2 Get this element's nodal coords and nodal 'values' by SAMPLing
c  3 Call ISOSURFACE to get the n triangles, and pick the i'th
c  4 get normal vector and call DR_PRIM.
c   OR:
c  2 get element directly from the stored database GC2, NUMS2
c    so no need to call ISOSURFACE again
c    But we need its 'level' to give it a colour (cf IMAT)
c    Also maybe need to be able to contour on this surface,
c    (for example cutting planes), so..
c     - at the time of creating the triangles, we need to abstract one
c       value per node and then we can contour this :-)
c       ie. we have, for each 8nb, both 8 values that will give the 
c       curvilinear plane, and the 8 values of the target surface 
c       function - these may be the same

c  < note here that Polygonise could work in locals, so that we can 
c    externaly use FUN to sample anything (eg vectors) over the surface. >
c
c
c -  steps 1-3 can be wrapped together as 'get list of triangles with the 
c      given element IEL, and given contour value
c - I should colour the surface directly from GOURAUD I guess, to make 
c      sure we use the right colour. (cf line contours in 2d), hence no 
c      reason, why the user could not ask for several isosurfaces at a 
C      time and so get 'onion rings' (just would be slow)
c
c
c
        ENDIF


c--------------------- Wirefarme /fill drawing -----------------------

        IF (IOBJECT_TYPE.EQ.30) THEN         !--- A Ring ---
c         if (cv(64).ge.0) ...
          CALL DRAW_RINGS (RINGS,IFC,GC,IGC,DISPS,IDISPS,0.     !- undef
     &                , 'line',CV(64),PM)
          CALL DRAW_RINGS (RINGS,IFC,GC,IGC,DISPS,IDISPS,0.     !-- undef.
     &                , 'fill',CV(65),PM)
          CALL DRAW_RINGS (RINGS,IFC,GC,IGC,DISPS,IDISPS,FACT   !- deformed.
     &                , 'line',CV(62),PM)
          CALL DRAW_RINGS (RINGS,IFC,GC,IGC,DISPS,IDISPS,FACT   !- deform.
     &                , 'fill',CV(63),PM)
          CALL DRAW_RINGS (RINGS,IFC,GC,IGC,DISPS,IDISPS,FACT   !- imat (on def)
     &                , 'imat',CV(66),PM)
          GOTO 1991       !- skip to the next object
        ENDIF

!- next is catch all to skip round the main object loop
        IF (IOBJECT_TYPE.ne. 0) goto 1991    !--- Ignore non-facets? ---
c-----------------------------------------------------------------------
c----------------------------- Facet drawing ---------------------------   
c-----------------------------------------------------------------------
C       IF (IOBJECT_TYPE.EQ. 0) THEN ...     !--- A Facet ---
c       IFC  = PL2 (PL1(I_F))         !- the facet's number
        IEL  = FACETS (1,IFC)         ! hence : el #
        IFACE= FACETS (2,IFC)         !       : &face dirn.
c.. we do not use IFC again and so we could have overloaded IFC to
c    carry (IFACE-1)*NEL+IEL and so we did not need FACETS() here

        CALL GET_ELEMENT (NUMS,INUMS,IEL, NUM,
     &                   NOD,NDIME,ITYPE, IMAT,IU1,IU2,IU3)
        CALL GET_FACE  (NUM,NOD,NDIME,ITYPE,IFACE,FS,NN_F,NN_FT, 2)
        CALL GET_FACE2 (NOD,NDIME,ITYPE,IFACE,FS6,NN_F,NN_FT, 2) 

c       .. get coord of *all* nodes of this elem ?
c       .. whatabout the displaced COORDS ??
        CALL GET_COORD (NUM,NOD,NDIM,GC,IGC,COORD,ICOORD)    !- coords
        IF (NDIM.EQ.2) THEN
          DO I=1,NOD              !- make sure 2D's have a nul Z-coord?
            COORD(I,3) =0.
          ENDDO
        ENDIF

C------------- get element's displacements (and PP, etc.) ----
        DO I=1,NOD
          DO J=1,NDIM
            II = NUM(I)
            DISPS_EL (I,J) = DISPS(J,II)
          ENDDO
        ENDDO

c.. get deformed coords ?? for plotting stress tensors I guess?
c.. yes but this mungs strain contouring via JAC=DER*COORD
        DO I=1,NOD
          DO J=1,3
            COORD(I,J) = COORD(I,J) + cv(2)*FACT1 * DISPS(J,II)
          ENDDO
        ENDDO

C------------- get screen positions of the nodes of a facet ------------
c.. careful if the data has NO disps ??
c.. I *could* take GPT from COORD (+DISPS)
c   I will need *all* disps of an elem. for accurate strain plots
C   .. ie need to build ELD() too.

c.. realy I would prefer to get COORD and ELD for the whole element, then
c   never refer to GC or DISPS again.
      DO I=1,NN_FT
        II = FS(I)
        DO J=1,3
c         GPT(J) = GC(J,II) + FACT1 * DISPS(J,II)
          GPT(J) = GC(J,II) + CV(2)*FACT1* DISPS(J,II)
        ENDDO

        CALL TRANSFORM2 (GPT,SPT ,PM)
c       CALL TRANSFORM (GPT,SPT, PM, IROT, XCEN,YCEN,SC_X,SC_Y,SC_Z)
        DO J=1,3
          SC_F (I,J) = SPT(J)  !- store the screen coords  
        ENDDO  
      ENDDO

C-------------------------- 'shrink' algorithm -------------------------
C... here we find the global (hence screen centroid) of the facet
C... then shrink all nodes towards this
c.. It must be better to shrink in the Model coord system ???
c So record the centroid of the parent element (deformed?)
c thence loop and move points along 90% of this line
c shrink
      IF (CV(31).ne.0) THEN    !- shrink factor
      DO J=1,3    ! no need for screen 'z' ?
        COM(J) = 0.
        DO I=1,NOD
           COM(J) = COM(J) + GC (J,NUM(I))  !- why No disps ?
           COM(J) = COM(J) + CV(2)*FACT1*DISPS(J,NUM(I))
c          COM(J) = COM(J) + COORD(I,J)
        ENDDO
        COM(J) = COM(J) / REAL (NOD)     !- the element 'centroid'
      ENDDO

      t = cv(31)/100.
      DO I=1,NN_FT
        II = FS(I)
        DO J=1,3 
          GPT(J) = GC(J,II) + CV(2)*FACT1* DISPS(J,II)
c         GPT(J) = COORD(J,fs(I))    !- the same as the previous ?
          GPT(J) = (1.-t)*gpt(J) + t*com(j)
        ENDDO
        CALL TRANSFORM2 (GPT,SPT ,PM)
        DO J=1,3 
          SC_F(I,J) = SPT(J)
        ENDDO
      ENDDO

c      DO J=1,2                           !- no need for screen 'z' ?
c        IF (CV(31).GT.0) then            !- as %-age shrinking?
c          DO I=1,NN_FT
c            SC_F(I,J) = SC_F(I,J) - (SC_F(I,J)-SPT(J)) * cv(31)/100.
c          ENDDO
c        ELSEIF (CV(31).lt.0) THEN        !- as # of pixels to shrink by
c          DO I=1,NN_FT
c            SC_F(I,J) = SC_F(I,J) - SIGN (REAL(CV(31)),SC_F(I,J)-SPT(J))
c          ENDDO
c        ENDIF
c      ENDDO
      ENDIF     ! if shrink

C------------------- get coord and disps of this facet -----------------
c.. cf getting the whole element, then abstract down to the facet.
c.. need coords for shrinking
c.. need disps for calculating the strains

      DO I=1,NN_FT  
        II  = FS(I)
        DO J=1,3
          C_F (I,J) = GC   (J,II)   !- C_F is *UNDEFORMED coords (cf SC_F)
          DI_F(I,J) = DISPS(J,II)   !- don't forget *FACT1 later
          C_F_D(I,J)= C_F(I,J) + FACT1*DI_F(I,J)  !- deformed pos.
        ENDDO
      ENDDO

C-------------------- edge the 'undeformed' facet ----------------------
c.. really the 'other' one .. if BASE is un-deformed then do deformed 
c.. here and vice-versa
c------ careful where I put this block of code
      IF (CV(16).NE.-1) THEN       !- this is the edges.. fill ?
        DO I=1,NN_F 
          II = FS(I)
          CALL TRANSFORM2 (GC(1,II),SPT ,PM)
          DO J=1,3
            SC_SF(I,J) = SPT(J)          !SC_F() surely ??
          ENDDO                     !(realy it is a 'tempory' polygon
        ENDDO
        CALL DRAW_POLY (SC_SF,ISC,NN_F,CV(16),1) !  :-)
      ENDIF                        

C-----------------------------------------------------------------------
C------------------------ sub-faceting ---------------------------------
C-----------------------------------------------------------------------
C... should call a subroutine to set 'up' this sub-facet
C... and another within the loop to return the local co-ords of one
C... of the points (given a 'patch' of what it looks like)

c.. surely if I am sub-faceting then I must calculate C1,C2,C3 for each !
c.. also colouring sub-facets' # etc. too.

      N_SF = CV(19)                   !- # along each edge (maybe= -1)
c     DO I_SF = 1, N_SF * N_SF        !- hence total #
      DO I_SF =    N_SF * N_SF,1,-1   !- hence total #

c...................................................................
C ... skip to main menu if a key pressed  ** mouse press ?? **
c .. this test could go almost anywhere
      EXIT = (KEY.ne.13.and.KEY_WAITING())  !(use INT_DBOS.F ?)
      IF (EXIT) GOTO 1119
c.......................................................................

      IOP_CONT = CV(13)            !- ie. What are we contouring?
      IF (IMAT.EQ.CV(80)) IOP_CONT = -1 !- but not for this material 17-2-97

C---------------------- no sub-faceting --------------------------------
c----- get the local co-ords of this facets' nodes using WTHATN
c.. ! note I am forcing 2D ! (am I really ??)
c... avoid WTHATN if 'OFF' style polygons ?? !
      IF (N_SF.eq.-1) THEN   
c----- Skip if OFF/NFF or if no contouring /disp. vectors/ regridding  -----
c  flow-nets too :-)
c.. this is just for speed to 'miss out' calculating LC_SF
c.. which is only used in SAMPLE for strains, etc.
        IF (ITYPE.ne.9.AND.  
     &   (IOP_CONT.GE.1.or.CV(12).ge.0.or.cv(33).ge.0.or.cv(35).ge.0)) 
     &   THEN
c           IF (NDIME.EQ.1) THEN   !- hack cos no 2d facet for a 2nl
c             CALL WTHATN (NN_F,1,1,LC_SF,ISC)
c           ELSE
             CALL WTHATN (NN_F,2,1,LC_SF,ISC)
c           ENDIF
         ENDIF

         NN_SF = NN_F
         DO I=1, NN_SF
           DO J=1,3                           ! (ie. get LC_SF and SC_SF)
              C_SF(I,J) =  C_F(I,J)   
             SC_SF(I,J) = SC_F(I,J)   
           ENDDO
         ENDDO

C--------------------------- sub-faceting ------------------------------
c.. so get local coords of this sub-facet (respect triangles)
c.. hence screen coords as FUN * SC_F -> SC_SF

c.. for sub-facets, we need to get the nodal DISPLACED coords
c.. then SAMPLE and transform to screen-coords
c----- OK use SAMPLE to interpolate the coords, then SAMPLE to interp
C----- the disps at a point, then add and transform

        ELSE
          CALL GET_FAC (LC_SF,ISC, I_SF,N_SF, NN_SF, NN_FT)  ! get LC_SF
          DO I=1,NN_SF              !- loop the nodes of a sub-facet
c           CALL SAMPLE (C_F,ISC,DI_F,ISC,  NN_F,2,3,
            CALL SAMPLE (C_F,ISC,DI_F,ISC,  NN_FT,2,3,
     &                            VAL,VEC, LC_SF,ISC,I,21)
            DO J=1,3
              C_SF(I,J)= VEC(J)       !- hence use to shrink /shade
            ENDDO
            CALL SAMPLE (C_F,ISC,DI_F,ISC,  NN_FT,2,3,
c           CALL SAMPLE (C_F,ISC,DI_F,ISC,  NN_F,2,3,
     &                            VAL,VEC, LC_SF,ISC,I,31)
            DO J=1,3
c             GPT (J)  = C_SF(I,J)+FACT1*VEC(J)    !- coord+disp
              GPT (J)  = C_SF(I,J)+cv(2)*FACT1*VEC(J)    !- coord+disp?
c.. need to shrink too (towards the element centroid)
              t = cv(31)/100.
              GPT(J) = (1.-t)*gpt(J) + t*com(j)
            ENDDO

            CALL TRANSFORM2 (GPT,SPT ,PM)
C           CALL TRANSFORM (GPT,SPT, PM, IROT, XCEN,YCEN,SC_X,SC_Y,SC_Z)
            DO J=1,3
              SC_SF(I,J) = SPT(J)     !- to screen coord
            ENDDO
          ENDDO
        ENDIF

      DO J=1,3                            !- 'close' the polygon (why?)
        SC_SF (NN_SF+1,J) = SC_SF (1,J)
      ENDDO

C--------------------- get the 'flat' shade value ----------------------
c.. this gets SHADE =  0. -> 1. 
c.. I could use the 'original' (object) cords instead ?
c.. also C3 = the area in pixels of the sub-facet. 
c.. I could use C3 to choose 'auto-sub-faceting'

C---- could use the following (as used in STRIP_EDGES) (C1,C2,C3 = DC(1..3)
c.. hey! - but C_SF doesn't include the disps :-(  26-6-98
      CALL GET_DC_NORMAL (C_SF,ISC,NN_SF, 3, DC ) 
      SHADE = (LIGHT(1)*dc(1)+LIGHT(2)*dC(2)+LIGHT(3)*DC(3)) !- dot prod.
      SHADE = -SHADE      !-hack ?
      SHADE = MIN(MAX(0.001,SHADE),.9999) !- clips -ve's too :-)
c        C1 = 0.
c        C2 = 0.
c        C3 = 0.
c        DO I=1,NN_SF
c          J  = MOD (I,NN_SF) + 1
c          C1 = C1 +(C_SF(I,3)+C_SF(J,3)) *(C_SF(J,2)-C_SF(I,2)) /2.
c          C2 = C2 +(C_SF(I,1)+C_SF(J,1)) *(C_SF(J,3)-C_SF(I,3)) /2.
c          C3 = C3 +(C_SF(I,2)+C_SF(J,2)) *(C_SF(J,1)-C_SF(I,1)) /2.
c        ENDDO
c        C4 = ABS (C1*C1 + C2*C2 + C3*C3 )
c        IF (C4.GT.1.E-12) THEN
cc         .. why do I need B ? if the light is in 'locals'
c          C4 = SQRT(C4)
cc         B = (                            (1.)*C3) /C4  ! eye
cc         SHADE = SIGN(A,A*B)    ! ie. on the 'same' side as the light
c          SHADE = ABS(SHADE)
c        ELSE
c          SHADE = 0.         ! zero-area face so skip colouring it
c        ENDIF

C-----------------------------------------------------------------------
C------------------- select face/edge color options  -------------------
C-----------------------------------------------------------------------
c... maybe loop around this twice .. for 2 'layers' eg. conts+mats
c... careful with mean NN contours , contour NEN ?
c.. also chessboard MOD 3 or 4 ?? and nodal BW ?
c-- isn't this needed for every sub-facet ? 'cept outer edge ?

c... use cols 0-255 as solid 500+ as 'opcodes'

      ICMAT= IC_MATS(MOD(IMAT-1,99)+1)  !- material colour 
      INTERP_FILL = .false.             !- assume 'solid'
      DO IPASS = 1,2
        IF (IPASS.EQ.1) IBASE_TYP = CV(1)    !- also >1000 value ?
        IF (IPASS.EQ.2) IBASE_TYP = CV(3)
        ICOL = -1      !- defualt is 'invis ???

        IF (IBASE_TYP.EQ.-1) THEN                    ! See-thru
          ICOL = -1 
        ELSEIF (IBASE_TYP.LT.500) THEN                 ! Solid colours
          ICOL = IBASE_TYP                              !- explicit
c         IF (IPASS.EQ.2) ICOL = CV(4)

        ELSEIF (IBASE_TYP.EQ.501) THEN          ! Material # (indexed)
          ICOL = ICMAT 
c        ELSEIF (IBASE_TYP.EQ.502) THEN          ! 'Group' eg. connectivity
c          ICOL = IU1
        ELSEIF (IBASE_TYP.EQ.503) THEN          ! 'Group2' eg. == els.
          ICOL = IU2
        ELSEIF (IBASE_TYP.EQ.504) THEN          ! 'Group3' eg. built-up
          ICOL = IU3

         ELSEIF (IBASE_TYP.EQ.502) THEN          ! Facet direction
           ICOL = ABS(IFACE) 

c        ELSEIF (IBASE_TYP.EQ.503) THEN          ! Inside/Outside
c          IF (DC(3).LT.0) ICOL =  2
c          IF (DC(3).GT.0) ICOL =  1

c        ELSEIF (IBASE_TYP.EQ.504) THEN          ! Glass front :-)
c          IF (C3.LT.0) ICOL = -1                 ! only if no b_p_c ?
c          IF (C3.GT.0) ICOL =  1

        ELSEIF (IBASE_TYP.EQ.505) THEN             ! Element #
          ICOL = INT(IEL / REAL(NEL) *CV(18)) + 20
        ELSEIF (IBASE_TYP.EQ.506) THEN             ! Mean Node #
          COL = 0
          DO I=1,NN_FT
            COL = COL + FS(I)
          ENDDO      
          ICOL = int(COL /REAL(NN_FT) /REAL(NN) * CV(18)) + 20
        ELSEIF (IBASE_TYP.EQ.507) THEN                 ! sub-facet #
          ICOL = I_SF                                     !(junk)
        ELSEIF (IBASE_TYP.EQ.508) THEN                 ! Chessb.  sub-facets 
          ICOL = MOD(I_SF,2) 
        ELSEIF (IBASE_TYP.EQ.509) THEN                 ! Chessboard elements
          ICOL = MOD (IEL,2) 
        ELSEIF (IBASE_TYP.EQ.510) THEN        !------- flat shading ---------
          ICOL = 21 + int(SHADE*CV(18))            ! CV(18) = # cols =13 usu.
        ELSEIF (IBASE_TYP.EQ.511) THEN        !--- flat shading 'striped'
          ICOL = 21 + int((SHADE*CV(18)) +(IMAT-1)*cv(18))
        ELSEIF (IBASE_TYP.EQ.512) THEN        !--- z-buffer depth 17-2-98
c         or maybe simplest to calc the mean depth here.
c.. be careful that zmin,zmax are the extremes not just of the 'front corners'
          Z=0.
          DO I=1,NN_SF              !- loop the nodes of a sub-facet
            Z=Z+SC_SF(I,3)
          ENDDO
          Z=Z/real(NN_SF)
c         Z= ZBUF(pl1(i_f))
          Z= (Z-Z_CLOSEST) / (Z_FURTHEST-Z_CLOSEST)
          Z=ABS(Z)                 !- not needed?
          z=min(max(0.,z),.999)    !- not needed?
          ICOL = 21 + int(z*CV(18))


C----------------------- * Light Source Shading * ----------------------
c.. This used to be part of CONTOURing .. 29-2-95 now seperate hence
c.. shear strain line-contours over a shaded surface
c  ( maybe have a seperate palette for shading and contouring ? )
c... hmm light-source-shading should use the 'displaced' co-ords
c...  coord/disp/strains will use the 'original' geometry !

c    11-7-95 normals (.OFF files) now held in NORMS(,) so separately
C    call SAMPLE is so ??
      ELSEIF (IBASE_TYP.GE.520 .and.IBASE_TYP.le.529) THEN    

        INTERP_FILL = .true.         !- cos we have one colour per node
        N_CONTS = cv(18)             !- (hack to 20 colour levels ??)
        DO I=1,NN_SF   !----- loop the nodes of a sub-facet (NN_SFT ?)
          IF (IBASE_TYP.EQ.520.or.IBASE_TYP.eq.521) THEN
C           .. should never be 'sub-faceted' .. why not ??
            DO J=1,3                          !- From a table
              VEC(J) = DISPS(J,FS(I))         !- of stored
            ENDDO                             !- normal vectrors
          ELSEIF (IBASE_TYP.EQ.522.OR.IBASE_TYP.EQ.523) THEN 
c... here we use C_F_D which holds the deformed position of the nodes
c... on the surface of the element                      !- form cross-product
           CALL SAMPLE (C_F_D,ISC,DI_F,ISC,NN_FT,2,3,   !- of JAC rows
c     &                VAL,VEC,LC_SF,ISC,I,IOP_CONT)     !- (NOT IOP_CONT)
     &                VAL,VEC,LC_SF,ISC,I,14)     !-  14= normal
          ELSE
            PRINT*,'** INTERNAL: unknown shading option (520-525)'
          ENDIF

          T = SQRT(VEC(1)**2+VEC(2)**2+VEC(3)**2) ! noramalise face normal
          IF (ABS(T).LT..001) THEN             !- skip if point has zero-area
            SHADE = 0.                         ! dummy shade col?
          ELSE
            A = (LIGHT(1)*VEC(1)+LIGHT(2)*VEC(2)+LIGHT(3)*VEC(3)) /T
            B = (                                    (1.)*VEC(3)) /T  !eye
            SHADE = A
c           SHADE = SIGN(A,A*B)    !- ie. on the 'same' side as the light
            SHADE = ABS(A)
c           SHADE = MIN(MAX(0.001,SHADE),.9999) !- avoid exactly 0 or 1 ?
          ENDIF
          COL = SHADE*N_CONTS      !- to a colour

c... the next could maybe appear 'in general': if the 'light switch' is on
c... then ALL entities will shaded.
        IF (IBASE_TYP.EQ.521.or. IBASE_TYP.EQ.523)    !- stripe with
     &  COL = COL + (IMAT-1) * N_CONTS            !- the mat. colour ??
        ACOL(I) = COL       !- palette-offset will be added in GOURAUD
      ENDDO               !- loop sub-facet nodes

        ENDIF          !- end of facet flat-colour options

        IF (IPASS.EQ.1) ICOL_FACET = ICOL
        IF (IPASS.EQ.2) ICOL_EDGES = ICOL
      ENDDO

C------------------'flat'-colour in the sub-facet ----------------------
c.. the edges are down later after contouring etc.

      IF (INTERP_FILL) THEN                          !- an interpolated fill
        CALL GOURAUD (SC_SF,ISC,NN_SF,ACOL, N_CONTS*10,1002,'fill')
c       CALL GOURAUD (SC_SF,ISC,NN_SF,ACOL, N_CONTS,1002,'fill')
      ELSE
        IF (ICOL_FACET.NE.-1) 
     &  CALL DRAW_POLY (SC_SF,ISC,NN_SF,ICOL_FACET,2) 
      ENDIF


C-----------------------------------------------------------------------
C------------------ contouring of strains, disps, etc. -----------------
C-----------------------------------------------------------------------
c... hmm contour Node Number also ?
c... also the value held in CONT_VALS() directly (know max&min directly)
c... I should allow SHADE to shift to darker palettes too
c.. speed-ups include soild-fill based on the *first* node or
c      by averaging ACOL()
c
c   2-12-95 Really I want to pull Stress contouring here too, plus the
c          smoothed values calculated above (eg shear strain)
c      so put the variables we want into DI_F before calling SAMPLE
c      (eg a set of stress-values), so just use SAMPLE for sub-facets etc.
c
c---------------------------------
c          E    = PRPTAB (1,IMAT)
c          V    = PRPTAB (2,IMAT)
c          BW   = PRPTAB (6,IMAT)               !    BW = a bit obsolete ?
c          C    = PRPTAB (3,IMAT)    
c          PHI  = PRPTAB (4,IMAT)
c          PSI  = PRPTAB (5,IMAT)  


      IOP_CONT  = cv(13)             !- what sort of contours
      IF (IMAT.EQ.CV(80)) IOP_CONT = -1 !- but not for this material 17-2-97

      N_CONTS   = CV(18)             !- and how many
      IF (IOP_CONT.GE.1) THEN        !- if we are contouring...
        s_term = '....'
        DO I=1,NN_SF   !----- loop the nodes of a sub-facet (NN_SFT ?)

c  if strain .. get nodal values: then sample here, feeding the vector
c  if pre-calculated (smoothed) strain, again feed a vector.
c  else just use SAMPLE fed with Coords and disps
c ** cf using the parent elements shape funs or just the 'surface' facet
c.. if derived strain, we need the parent else surface elem is sufficient.

c--------------- pre-formed nodal values ----------------------
c.. build a facets-worth from global,then FUN*VAL
      IF (IOP_CONT.GE.1000) THEN

c--------------------- Stresses ----------------------
c.. build a facets-worth from the stress-table.
c ** I should GET_STRESS only once per element
c ** I should GET_STRESS_TERM only once per element
c    hence get one facet's worth of these nodal values.
C .. thence just loop and sample at each subfacets' node.                   

      ELSEIF (IOP_CONT.GE.200) THEN
        IF (I.EQ.1) THEN
        CALL GET_STRESS (STRESSES,PSTR(IEL), STR_EL,NGP,IH)  !- all GP's
        IF (NGP.le.0) GOTO 951    !- just jump-out if we have no data?
c          .. what can I do with the elastic params? plastic strain?   
c.. yes DER*COORD-> elastic strian, hence diff = pl. strain. ?
c          E    = PRPTAB (1,IMAT)
c          V    = PRPTAB (2,IMAT)
c          BW   = PRPTAB (6,IMAT)               !    BW = a bit obsolete ?
         IOP_VM = NINT(PRPTAB(18,IMAT))
           C    = PRPTAB (3,IMAT)    
           PHI  = PRPTAB (4,IMAT)
c          PSI  = PRPTAB (5,IMAT)  

c.. hmm need a numeric to string converter !!?
c.. as a subroutine : do coord, disps, strains also
c   and so add to the contour legend
c       call get_s_term(iop_cont,s_term)
        IF (iop_cont.eq. 201) s_term= 'sx  '
        IF (iop_cont.eq. 202) s_term= 'sy  '
        IF (iop_cont.eq. 203) s_term= 'sz  '
        IF (iop_cont.eq. 204) s_term= 'txy '
        IF (iop_cont.eq. 205) s_term= 'tyz '
        IF (iop_cont.eq. 206) s_term= 'tzx '

        IF (iop_cont.eq. 211) s_term= 's1  '
        IF (iop_cont.eq. 212) s_term= 's2  '
        IF (iop_cont.eq. 213) s_term= 's3  '
        IF (iop_cont.eq. 214) s_term= 'sigm'
        IF (iop_cont.eq. 215) s_term= 'dsb '
        IF (iop_cont.eq. 216) s_term= 'lode'
        IF (iop_cont.eq. 217) s_term= 'f   '
        IF (iop_cont.eq. 218) s_term= 'mss '
        IF (iop_cont.eq. 219) s_term= 'fos '
        if (s_term.eq.'....') call myerror(3,'unknown stress term')
        CALL GET_STRESS_TERM (STR_EL,NGP,IH,NODOF, C,PHI,IOP_VM
     & ,GPVALS, s_term)
        CALL GP_TO_NODES (GPVALS,NGP,IH, NDIME,NOD,ITYPE, VALS_ELEM )
        ENDIF     !- only extract the table on the first pass.

c.. hmm .. oh dear .. if I 'sample' just over a facet I will need
C      to pull-out VALS_ELEM to just the FACET's set. ..cf FS(I)
C      (else .. mung LC_SF back to full 3D, using IFACE)
c       CALL SAMPLE_1 (VALS_ELEM,NOD,NDIME,NDIM,VAL, LC_SF,ISC,I,0)
        DO J=1,NN_FT
          VALS_FACET (J) = VALS_ELEM(FS6(J))
        ENDDO
        CALL SAMPLE_1 (VALS_FACET,NN_FT,2,2,VAL, LC_SF,ISC,I,0)
c.. now got VAL :-)      

c--------------- strains using the parent element ---------------
c.. here we still use SAMPLE but feed it the full element
C.. therefore SAMPLE needs to handle full 3D BEE's etc. 
      ELSEIF (IOP_CONT.GE.40.and.iop_cont.lt.59) THEN
           CALL SAMPLE (COORD,ICOORD,DISPS_EL,ICOORD,NOD,ndime,ndim,
     &                 VAL,VEC,LC_SF,ISC,I,IOP_CONT)

c---------- standard coords and disps (+strain?) ----------------
c.. need to form s-term='dx','dy', etc.
      ELSE
          CALL SAMPLE (C_F,ISC,DI_F,ISC,NN_FT,2,3,
     &                 VAL,VEC,LC_SF,ISC,I,IOP_CONT)
      ENDIF
          C_R (1) = MIN (C_R(1),VAL)         !  min contour value found
          C_R (2) = MAX (C_R(2),VAL)         !  max contour value found

c.. scale so 'good' values lie between 0. to 1.
c..    values to clipped will be -ve or > 1. 
c.. so where to they get 'clipped' off ?
c 2-12-95 ** I don't like this! .. I would rather pass the raw values to
C  'Gouraud' and also a list of contour levels.

c------------------ take absolute value -----------------------
          IF (C_R(6).GT.0.) VAL = ABS(VAL)  

c-------------- Negate (eg. for settlements)-------------------
c         IF (C_R(7).GT.0.) VAL = -VAL

c--------------- Scale from 0. to 1. -----------------
        VAL = (VAL-C_R(3))/(C_R(4)-C_R(3)+1.e-20) 

c--------------- Logarithic scaling -------------------
c.... (skip if 'log factor' == 1 for speed)
        IF (INT(C_R(5)*10).NE.10)  VAL = VAL ** (1./C_R(5)) 

c--------------- clip extreme values --------------------
c         VAL = MIN(MAX( -1., VAL),  2.)     !- just avoids int*2 overflow
          VAL = MIN(MAX( -99., VAL),  99.)   !- just avoids int*2 overflow

c-- expand so valid values to contour are 0. --> N_CONTS
          ACOL(I) = VAL * N_CONTS            !- offset is added in GOURAUD
        ENDDO       

        CALL GOURAUD (SC_SF,ISC,NN_SF,ACOL, N_CONTS,CV(15),'fill')  !-- filled
        CALL GOURAUD (SC_SF,ISC,NN_SF,ACOL, N_CONTS,CV(14),'line')  !-- +lines

      ENDIF   ! end of contouring disps/ strains etc.
  951 CONTINUE   !- skip to point if we avoid contouting (eg. no stresses here)
C---------------------------- * FLOW-NETS * ----------------------------
c. this assumes that the 'x' disps contain the 'heads' and 'y' the stream-func.
c     also both values should be scaled from '0' to '1' beforehand
c  < can do 'tricks' like coloured flow channels with equipotential lines >

      IF (CV(35).GE.0) THEN
        N_C = CV(36)      !- the # of 'contour' lines
        DO J=1,2          !- 2 cases : iso-pots/flow-lines
          DO I=1,NN_SF   
            CALL SAMPLE (C_F,ISC, DI_F,ISC, NN_FT,2,3,    
     &               VAL,VEC, LC_SF,ISC, I,30)            
            VAL = VEC(J)
            ACOL(I) = VAL * N_C            !- expand to 0.--> nlines
          ENDDO       
          CALL GOURAUD (SC_SF,ISC,NN_SF,ACOL, N_C,CV(35),'line') 
        ENDDO    ! loop iso-pot/flow-lines
      ENDIF  ! if flow-nets

C------------------------- * REGRIDDED LINES * -------------------------
c.... this is the same as contouring of x/y/z coordinates
      IF (CV(33).GE.0) THEN
        N_C = 2**CV(34)      !- the # of 'contour' lines
        DIAG2 = MAX (GC_MAX(1)-GC_MIN(1),GC_MAX(1)-GC_MIN(1),
     &               GC_MAX(1)-GC_MIN(1) )
        DO J=1,NDIM
          DO I=1,NN_SF   !----- loop the nodes of a sub-facet (NN_SFT ?)
            CALL SAMPLE (C_F,ISC, DI_F,ISC, NN_FT,2,3,     !- get coord
     &               VAL,VEC, LC_SF,ISC, I,20)            
            VAL = VEC(J)
            VAL = VAL - GC_MIN(J)      !- push to 0.--> tot.width
            VAL = VAL / DIAG2          !- scale  to 0. -> 1.
            ACOL(I) = VAL* N_C         !- expand to 0.--> nlines
          ENDDO       
          CALL GOURAUD (SC_SF,ISC,NN_SF,ACOL, N_C,CV(33),'line')  !-- +lines
        ENDDO    ! loop x->y->z
      ENDIF  ! end of regridding

C-----------------------------------------------------------------------
C------------------------ nodal glyphs ---------------------------------
C-----------------------------------------------------------------------
c  various symbols that can be drawn at the nodes
c  Need to be able to draw markers for the BC's
c  perhaps node numbers are also of this class
c

C-----------------------------------------------------------------------
C-------------------- displacement vectors -----------------------------
C-----------------------------------------------------------------------
c   Here we transform BOTH the start and end-points ..
c   however by (sub-)faceting we already know the end-point :-)
c.. careful of these anisotropic scale factors (ignore!)
c.. by this token strain-vectors may also be drawn (tensors?)
c.. nice to be able to sample at the Gauss-points too.

cc      CALL DR_STRESS_VECTORS (STR_EL(IPT), NODOF,IH, COORD_GPS(1,IGP) 
cc     &  , PM, C_R(8),C_R(7), DIAG*0.05
cc     &  ,CV(71),cv(72),cv(73),cv(74),cv(75),cv(76) )
      IF (CV(12).NE.-1) THEN
        alpha=    cv(4)/10.    ! 0.5=centered,1=forwards,0=before
        DO I=1,NN_SF
          CALL SAMPLE (C_F_D,ISC, DI_F,ISC, NN_FT,2,3,     !- get coord
     &               VAL,GPT2, LC_SF,ISC, I,20)
          CALL SAMPLE (C_F,ISC, DI_F,ISC, NN_FT,2,3,     !- get disp
     &               VAL,VEC, LC_SF,ISC, I,30)

c.. if centered need to -= 1/2*vec(disps) ?
c.. SAMPLE C_F to get global coords of point
c.. and SAMPLE to get global disp, then *=FACT1             
c.. do GPT1() and GPT2() as point๑ 0.5*VEC
c-----------------------------
c.. I *could* get both coord and disps at once :-)
          DO J=1,3
            GPT(J) = GPT2(J) - (1.-ALPHA)* VEC(J) * FACT1
          ENDDO
          CALL TRANSFORM2 (GPT,SPT ,PM)
          DO J=1,3
            GPT(J) = GPT2(J) + ALPHA* VEC(J) * FACT1
          ENDDO
          CALL TRANSFORM2 (GPT,SPT2 ,PM)
c-----------------------------
c  also can colour wrt a component (eg. z=out_of_plane)
c ok so do as 2-passes? - first get limits; second re-draw?
          IF (CV(12).EQ.1000) THEN
            val = sqrt(vec(1)**2 +vec(2)**2 +vec(3)**2)
            icol = 20+ int(n_conts * max(0.,min(val/fact,1.)))
          ELSE
            icol = cv(12)         !- explicit ICOL
          ENDIF
c... hmm sometimes we may swap spt and spt2?
          CALL DR_ARROW  (SPT,SPT2,10,20,icol)
        ENDDO
      ENDIF
c... This is the end of the face-filling so maybe do all together here ?      

C-------------------- edge the sub-facet -------------------------------
c.. dont I want to use ICOL_EDGES (eg. if shaded)
      ICOL = CV(5)                ! =sub-facet edge colour
      IF (ICOL.NE.-1) CALL DRAW_POLY (SC_SF,ISC,NN_SF,ICOL,1) ! edges

C.. OK have a CV(6) (say) for edging the 'exterior' only ?
c.. This would have the effect of drawing 'smooth' element edges
c.. if quads need the NXE/NYE of the sub facet
c.. so draw LH edge if NXE=1.. etc., etc. (easy)
c.. in triangle it is a little harder BUT if 1-> NSF, RH if 1,3,5 ???  

C------------------ end of the sub-faceting ----------------------------
      ENDDO

c------------------------------- STRESSES ------------------------------
c .. note I will need MAT_PROPS to do MSS, etc.
c .. what will I do if the GP's also carry a pore-pressure?
c .. note if this elem has no stress terms then NGP will be returned as zero
C    (so we nicely will skip the loop that handles each one in tern)

      IF (max(CV(71),cv(72),cv(73)).GE.0 .OR. 
     & max(CV(74),cv(75)).ge.0) then       !- any stresses to do?

c   Why not GET_STRESS for EVERY facet's parent element? 
c

        CALL GET_STRESS (STRESSES,PSTR(IEL), STR_EL,NGP,IH)  !- all GP's

        IF (NGP.gt.0) then        !- just jump-over if we have no data


C    .. get coords of all the GP's  
C    .. only needed if we want to plot vectors at them. (or 'tension dots')
        CALL GET_COORD_GPS (COORD,ICOORD,3, NDIME,NOD,ITYPE
     &          ,COORD_GPS, 3, NGP )

c      c= 20.       !- some dummy soil-parameters
c      phi=0.       !
c      iop_vm= 1    ! 1=Mohr-Coulomb
c      CALL GET_STRESS_TERM (STR_EL,NGP,IH,NODOF, C,PHI,IOP_VM
c     & ,GPVALS, 'mss ')
c     CALL GP_TO_NODES (GPVALS,NGP,IH, NDIME,NOD,ITYPE, VALS_ELEM )
c       ----------- loop and do each Gauss-point in tern ----------
c.. *or* do each stress-term independantly .ie plot the S3<0 for all 
c      GP's, then all the S1/S3 vectors, then Contour F_MSS
c  .. in this case I also need a function to return the DIRECTION_COSINES
c  of S1, S2,S3, in-order to plot them.
C       

        DO IGP = 1,NGP

      IPT = (IGP-1)*IH +1
      CALL DR_STRESS_VECTORS (STR_EL(IPT), NODOF,IH, COORD_GPS(1,IGP) 
     &  , PM, C_R(8),C_R(7), DIAG*0.05
     &  ,CV(71),cv(72),cv(73),cv(74),cv(75),cv(76) )

c-----------------------------------------------------------------------
        ENDDO     !- loop GP's
        ENDIF   !- only elements with stress records
      ENDIF   !- stress processing

C---------------------- edge facet -------------------------------------
C... need to sub-sample for 'smooth' edges 
C...  -also identify the geometry/material 'edges'
c... If I have done the 'smooth' edges of a sub-facet then there is no
c... need to do any more ?
c... BUT I could have a second N_SUB_FAC & loop and sample along the edges
C... to produce a l-o-n-g polyline to draw :-)

c.. hmm this is 'easy' ! .. just draw edge #1 if LHS of element etc. :-)
c.. so need a CV code to edge all of a sub-facet or just the boundary
c.. or better: a second sub-facet edge colour code for 'boundary' only

      IF (ICOL_EDGES.NE.-1) 
     &     CALL DRAW_POLY (SC_F,ISC,NN_F,ICOL_EDGES,1) ! edges

c--------------------- 'coded' facet edgeing ---------------------------
c.. Loop facet edges (3 or 4)
c.. Extract its code from FACETS (4-5-6-7)
c.. If code = 1 (material boundary) etc.
c.. Extract its colour code from CV(50+) then draw ths edge in
c--( Also there will be a code to use sub-sampling to get smoother edges)
c--( This will use the shape funs of a single edge (3nl element ?)
c    < not yet implemented >

C----------------------- draw nodes ------------------------------------
c 24-1-98  overload to show BC's (eg. NF or applied loads)
c  eg if cv(9)==1001,2,3 then only dot if x,y,z fixity (disps/=1)
c   if cv(9)=1010 colourcode dot so free=invis, fixed=black, other=coloured
c-- Contrast drawing nodes as circles or as little ticks or squares.
c-- Seems broken for postscipt circle drawing
      IF (CV(9).NE.-1) THEN
        IF (CV(21).LE.0.OR.CV(21).EQ.IEL) THEN   !- only for 1 elem ?
          DO I=1,NN_FT
            X(1) = SC_F(I,1)
            Y(1) = SC_F(I,2)
            X(2) = NINT (CV(10)*RES(1)/640.)     !- radius
            Y(2) = X(2)                          !- let y-radius=x-radius
            icol = cv(9)
            CALL DR_PRIM (LINE,X,Y,2,icol,10)   !- (code 10=circle)
          ENDDO
        ENDIF
      ENDIF

C-------------------- node numbering -----------------------------------
C.... need an option to draw 'centred' on the node?

      IF (CV(11).NE.-1) THEN
        DO I=1,NN_FT       ! ie. 'all' the nodes on this facet
          WRITE (LINE,'(I7)') FS(I)
c          CALL TRIM (LINE)           
          LINE = LINE(ISTR_BEG(LINE):)           ! ie. left-justify ?
          X(1) = SC_F(I,1)
          Y(1) = SC_F(I,2)
          x(2) = 4.                             !- point size
          CALL DR_PRIM (LINE,X,Y,1,CV(11),20)   !- (code 20=text-string)
        ENDDO
      ENDIF

C------------------element numbering -----------------------------------
      IF (CV(8).NE.-1) THEN
        WRITE (LINE,'(I7)') IEL
        LINE = LINE(ISTR_BEG(LINE):)           ! ie. left-justify ?
        X(1) = 0.
        Y(1) = 0.                   ! find the centroid of the face
        DO J=1,NN_F                 ! (or NN_FT ?)
          X(1) = X(1) + SC_F(J,1)
          Y(1) = Y(1) + SC_F(J,2)
        ENDDO
        X(1) = X(1) /REAL(NN_F)
        Y(1) = Y(1) /REAL(NN_F)
        x(2) = 4.                             !- point size
        CALL DR_PRIM (LINE,X,Y,1,CV(8),21)
      ENDIF

C------------------ material numbering -----------------------------------
c cf. element numbering
      IF (CV(7).NE.-1) THEN
        WRITE (LINE,'(I4)') IMAT
        CALL L_JUST (LINE)            !       right-justify (or centre?)
! F90     LINE=adjustl(line)
        X(1) = 0.
        Y(1) = 0.                   ! find the centroid of the face
        DO J=1,NN_F                 ! (or NN_FT ?)
          X(1) = X(1) + SC_F(J,1)
          Y(1) = Y(1) + SC_F(J,2)
        ENDDO
        X(1) = X(1) /REAL(NN_F)
        Y(1) = Y(1) /REAL(NN_F)
        x(2) = 4.                             !- point size
        CALL DRAW_POLY (SC_SF,ISC,NN_F,CV(16),1) !  :-)
        CALL DR_PRIM (LINE,X,Y,1,CV(7),21)
        CALL DR_PRIM (LINE,X,Y,1,CV(7),21)
      ENDIF

C------------------- end of this facet/object --------------------------
 1991  CONTINUE
      ENDDO    

c--------------------- if no mesh to draw  -----------------------------
c... large font ??
      IF (NEL.EQ.0) THEN
        LINE = 'NO MESH'
        X(1) = WIN_XO + WIN_XW /2
        Y(1) = WIN_YO + WIN_YW /2
c       x(2) = 30.                             !- point size
        x(2) = 70.                             !- point size
        CALL DR_PRIM (LINE,X,Y,1,14,20)
      ENDIF

C--------- Draw the 'extra-lines (eg. model boundaries) ----------------
c... really a subroutine .. maybe can do as a list of nodes as well
c... 'cos' this is easier to store (hence flag end-of chains with -1 ?
C.. needs a colour
C.. cf. DRAW_RINGS for the model boundary
C
c. note there is no attempt to autoscale tthe window to include these
      IF (CV(61).GE.0) THEN
        DO I=1,N_X_L
          GPT(1) = XTRA_L(1,I)
          GPT(2) = XTRA_L(2,I)
          GPT(3) = 0.
          CALL TRANSFORM2 (GPT,SPT ,PM)
C         CALL TRANSFORM (GPT,SPT, PM, IROT, XCEN,YCEN,SC_X,SC_Y,SC_Z)
          X(1) = SPT(1)
          Y(1) = SPT(2)

          GPT(1) = XTRA_L(3,I)
          GPT(2) = XTRA_L(4,I)
          GPT(3) = 0.
          CALL TRANSFORM2 (GPT,SPT ,PM)
C         CALL TRANSFORM (GPT,SPT, PM, IROT, XCEN,YCEN,SC_X,SC_Y,SC_Z)
          X(2) = SPT(1)
          Y(2) = SPT(2)

          CALL DR_PRIM (LINE,X,Y,2,CV(61),1)       !--- draw_a_line
        ENDDO
      ENDIF

c----------------------- draw the contour legend -----------------------
c  push this to a subroutine
c.. Disable if we have no contours or frame is 'invisible'
c.. Mark zero too ?
c.. need to make sure it appears at the *bottom* of the page
c.. I guess that this means using CAM_WINDOW and transforming wrt TS
c ie. model as 0.->1. then TRANSFORM
C       CALL TRANSFORM2 (GPT,SPT ,TS)
c.. perhaps I should have the option to place the legend at any of the 4 
c      sides of the picture . cf. the common RHS placement

      IF ( CV(13).GE.0 .and. CV(22).GE.0) THEN
        NN_SF = 4
        SC_SF (1,1) = WIN_XO + win_xw  * .08        !- 8% margin
        SC_SF (2,1) = SC_SF (1,1)
        SC_SF (3,1) = WIN_XO + win_xw  * .92        !- 8% margin
        SC_SF (4,1) = SC_SF (3,1)
        SC_SF (1,2) = WIN_YO + win_yw  * .03  
        if (idest.eq.80)            !- hmm yo is at the *top* of WMF
     &  SC_SF (1,2) = WIN_YO + win_yw  * (1.-.03)    
        SC_SF (2,2) = SC_SF (1,2)   + win_yw  * .03   ! 3% of the height
        SC_SF (3,2) = SC_SF (2,2)   
        SC_SF (4,2) = SC_Sf (1,2)
        ACOL (1) = 0.
        ACOL (2) = 0.
        ACOL (3) = N_CONTS
        ACOL (4) = N_CONTS

        CALL GOURAUD (SC_SF,ISC,NN_SF,ACOL, N_CONTS,CV(15),'fill')  !-- filled
        CALL GOURAUD (SC_SF,ISC,NN_SF,ACOL, N_CONTS,CV(14),'line')  !-- +lines
        CALL DRAW_POLY (SC_SF,ISC,NN_SF,CV(22),1)   ! edges

        CALL SET_FONT (101,.8,1.,1.)    !-try a larger font ?

        LINE = '0.'
        F = (0.-C_R(3)) / (C_R(4)-C_R(3))
c       X(1) = F* SC_SF(2,1) + (1.-F)* SC_SF(3,1)
        X(1) = (1.-F)* SC_SF(2,1) + F* SC_SF(3,1)   !- fixed 15-8-98
        Y(1) = SC_SF(1,2) -5
        x(2) = 6.                             !- point size
        IF (f.gt.0. .and. f.lt.1.-.0) THEN    !- is ZERO within the range?
          CALL DR_PRIM (LINE,X,Y,1,CV(22),20)
        ENDIF

        WRITE (LINE,'(G13.4)') c_r(3)
        CALL L_JUST (LINE)                          !left-justify?
        if (abs(c_r(3)).lt.1.e-24) line='0.'      !(prettier)
        X(1) = SC_SF(2,1) 
        Y(1) = SC_SF(2,2) +5
        x(2) = 7.                             !- point size
        CALL DR_PRIM (LINE,X,Y,1,CV(22),20)

        WRITE (LINE,'(G13.4)') c_r(4)
        if (abs(c_r(4)).lt.1.e-30) line='0.'      !(prettier)
        X(1) = SC_SF(3,1) !-50
        Y(1) = SC_SF(3,2) +5
        x(2) = 7.                             !- point size
        CALL DR_PRIM (LINE,X,Y,1,CV(22),22)

c.. now write *under* the legend the 'sigma-1', 'pore-pressure', etc !
c.. ie. I need a dual look-up of contour code <-> name
        WRITE (LINE,'(i4)') iop_cont       !- contour type
        X(1) = (SC_SF(2,1)+SC_SF(3,1))/2. !-50
        Y(1) = SC_SF(2,2) +10
        x(2) = 4.                             !- point size
        CALL DR_PRIM (LINE,X,Y,1,CV(22),21)
      ENDIF

C--------------- finished model so draw border etc.----------------------
c...  Should really have *Two* borders : one around each image
c...  and one (maybe thicker ?) around the whole picture
c
c----------------------- Draw the Main Titles  -------------------------
c.. disable if we have no title  or titles are 'invisible'
c.. really need to centre AND increase the font size.
c  ? what if many muliple images ?
c subtitle= TITLES(2) - so draw underneath 
      IF ( TITLES(1).ne.' '.and. CV(29).GE.0) THEN
        CALL SET_FONT (101,1.2,1.,1.)   !-try a larger font ?
        LINE = TITLES(1)                !- trim extra spaces ?
        X(1) = WIN_XO + win_xw /2.
        Y(1) = WIN_YO + win_yw * .94    !- 94% up 
        x(2) = 16.                            !- point size (was 20.)
        CALL DR_PRIM (LINE,X,Y,1,CV(29),21)   !-21=centered
      ENDIF
      IF ( TITLES(2).ne.' '.and. CV(29).GE.0) THEN
        CALL SET_FONT (101,1.2*.7,1.,1.)   !-try a larger font ?
        LINE = TITLES(2)                !- trim extra spaces ?
        X(1) = WIN_XO + win_xw /2.
        Y(1) = WIN_YO + win_yw * .90    !- 90% up 
        x(2) = 16.*.7                         !- point size (was 20.)
        CALL DR_PRIM (LINE,X,Y,1,CV(29),21)   !-21=centered
      ENDIF

c-------------------- show the load step number ------------------------
      IF (CV(39).GT.-1) THEN
        WRITE (LINE,'(A,F8.2)') 'LS =',LD
        X(1) = RES(4)               !.. in top LH corner
        Y(1) = RES(5) + 20.         !- careful with ABSOLUTE offsets
        x(2) = 7.                             !- point size
        CALL DR_PRIM (LINE,X,Y,1,CV(39),20)
      ENDIF

c-------------- show the (animation) image number ----------------------
c.. window number too ?
      IF (CV(40).GT.-1) THEN
        WRITE (LINE,'(A,I4)') 'PIC=',IPIC
        X(1) = RES(4)               !.. in top LH corner
        Y(1) = RES(5) + 40.  
        x(2) = 7.                             !- point size
        CALL DR_PRIM (LINE,X,Y,1,CV(40),20)
      ENDIF

C-------------- set clipping back to the whole drawing area ------------
! can only my postscript driver do this ?
      CALL DR_PRIM (LINE,X,Y,0,0,41)     !- 41 = un-clip

C-------------- draw border around the current window ------------------
c.. but not if only one windows on the page (cf picture frame)
      IF (CV(28).GE.0) THEN
c       IF (I_WIN_HI.GT.I_WIN_LO) THEN    !- multi-pics
          CALL MAKE_BOX (WIN_XO,WIN_YO,WIN_XW-2,WIN_YW-2,X,Y)
          CALL DR_PRIM (LINE,X,Y,4,CV(28),1)     !--- as a closed polygon
c       ENDIF
      ENDIF


C-----------------------------------------------------------------------
C----------------- Make picture current (if double buffered) -----------
C-----------------------------------------------------------------------
c.. really should call via my dvice driver
      if (cv(38).ne.0)
     &call PGEBUF ()  !- exit buffered state (DJK 27-3-01)

C-----------------------------------------------------------------------
C----------------- loop the next picture on the page -------------------
C-----------------------------------------------------------------------
      ENDDO    !- loop I_WIN

C-----------------------------------------------------------------------
c... also jump-to point if we are 'aborting' a picture-draw ??

 1119 CONTINUE

C-----------------------------------------------------------------------
C--------------------- end of image drawing ----------------------------
C-----------------------------------------------------------------------
c.. now close any output devices and return to VGA mode (or 'base' mode)

      CALL GET_SECS (TIME(4))    !- timings 

C-------------  dump image to printer/plotter file etc.-----------------

      IF (IDEST.EQ.1) THEN    !---- if VGA ------
        IF (CV(38).EQ.1) THEN        !---- double buffering ------
          CALL RESET_SIZES (RES)         ! get offsets again?
cc        CALL VSCREEN_TO_SCREEN@ (RES(4),RES(5),0,IFAIL_2) 
c         CALL VSCREEN_TO_SCREEN@ (RES(4),41,0,IFAIL_2) 
c         IF (IFAIL_2.NE.0) PRINT*,'Ifail=',IFAIL_2,' VS_2_S' 
c         CALL CLOSE_VSCREEN@()      
c         CALL RETURN_STORAGE@ (VSCREEN)  
c         print*,'res(4,5)=',res(4),res(5)
        ELSE !- if NOT buffered
c         IF (L_OVERLAP) THEN
C            CALL POST_MENUS (1, 0,0,Codes) !- if obscured
C            L_OVERLAP = .false.  !- since we have just reposted
c         endif
        ENDIF
      ELSEIF (IDEST.ge.10.and.idest.le.19) THEN  !------- SVGA prevew -------
        IF (IPIC.eq.IPIC_HI) THEN   !- Only if it is the 'last' page
          READ*                       !- wait for a keypress (or mouse)
          CALL GRAPHICS_START (vesa)  !- back to normal
          L_OVERLAP = .TRUE.          !- flag so menus will be redrawn (or do here ?)
        ENDIF

c----------------- the rest are 'printers' --------------------
c.. need a generic "close_printer" (via DR_PRIM surely?)
c.. if auto-copy to device, then issue "print file" or "remsh menhpa .." or 
c     "nprint -server=man-eng-ps1 .." etc.

      ELSE 
c.. hmm the next line gets sent to the output device *not* the VGA
c       call post_pop_up ('Outputing to Disk/Device ... ') 

      IF (IDEST.EQ.20) THEN     !-------- PCX output to file -------
c.. hmm does this invert the image?
c         CALL VSCREEN_TO_PCX@ (PICFILE,IFAIL_2)
c         CALL PCX_FIX (PICFILE,PAL) 
c         CALL CLOSE_VSCREEN@ ()
c         CALL RETURN_STORAGE@ (VSCREEN)  !- why ?

      ELSEIF (IDEST.GE.30.and.IDEST.le.39) THEN     !----------- PostScript ------------
!       need option to skip this perhaps?
        CALL WRITE_PS_DANPLOT_LOGO (55,RES,VERSION)    !- on the image

        L_OVERLAP = .TRUE.      !- flag so menus will be redrawn (or do here ?)
        CALL CLOSE_PS_PRINTER ()     !(just grestore and showpage)
        IF (INDEX(PICFILE,'.PDF').GT.0) THEN
c        call system ('ps2pdf'//PICFILE) 
!        and can delete the postscript file.
        ELSE IF (INDEX(PICFILE,'.EPS').GT.0) THEN
c           --  nothing else to do if .EPS?
        ELSE
          IF (PICFILE.eq.'PRINT') THEN            ! auto-print 
c           CALL CISSUE@ ('copy print prn',ifail_2)     ! copy to printer
c           CALL CISSUE@ ('erase print',ifail_2)        ! then delete
c           ... also nice to have an option to send to the CGU printers?
          ENDIF
        ENDIF   !- various PS options

!      ELSEIF (IDEST.EQ.40) THEN     !--------- HP-LJ to file ----------
!        L_OVERLAP = .TRUE.          !- flag so menus will be redrawn (or do here ?)
!c       CALL CLOSE_GRAPHICS_PRINTER@ () 
!        IF (PICFILE.eq.'LASER') THEN          ! auto print to HP-UX :-)
!c         CALL CISSUE@ ('pr_hpux',ifail_2)    !( sends & deletes 'LASER')
!        ELSEIF (PICFILE.eq.'PRINT') THEN     
!c         CALL CISSUE@ ('copy/b print prn',ifail_2) ! prints
!        ENDIF
!
!      ELSEIF (IDEST.EQ.50) THEN        !------- HP7550 to file ---------
!        L_OVERLAP = .TRUE.         !- flag so menus will be redrawn (or do here ?)
!c       CALL CLOSE_PLOTTER@ ()         ! & auto-print ? 
!        IF (PICFILE.eq.'PRINT') THEN            ! auto-print 
!c         CALL CISSUE@ ('copy print com1',ifail_2)     ! copy to printer
!        ENDIF
!
!      ELSEIF (IDEST.EQ.60) THEN        !------ pro-printer to LPT1 -----
!c       CALL CLOSE_GRAPHICS_PRINTER@() ! & auto-print ?

      ELSEIF (IDEST.EQ.70) THEN     !----------- DXF file ------------
        L_OVERLAP = .TRUE.      !- flag so menus will be redrawn (or do here ?)
        CALL CLOSE_DXF_PRINTER ()     !(just EOF)
      ELSEIF (IDEST.EQ.80) THEN     !----------- WMF file ------------
        L_OVERLAP = .TRUE.      !- flag so menus will be redrawn (or do here ?)
c       CALL CLOSE_WMF_PRINTER ()     !(just EOF)
      ENDIF   !- of printers
      call post_pop_up (' ')         !- un-post

      ENDIF   !- of output destiantions

      IF (EXIT) GOTO 1110   ! jump back to the top of the program

c-----------------------------------------------------------------------
c------------ loop to the next image to draw (Animation) ---------------
c-----------------------------------------------------------------------
C.. here we may save the current page to memory or PCX
c.. (PCX is maybe better handled as a PRINT option?)
c.. hmm put in say postscript - we could have all farmes as individual pages in the one file?

      IF (CV(53).GE.1) THEN    !- if animating

        IF (CV(55).ne.0) then   !- save the current pic
c         CALL GET_SCREEN_BLOCK@  (RES(4),20+RES(5),
c    &       RES(4) + RES(1)-1, 20+(RES(5)+RES(2)-1), BUFFER)
            buffer=-1   !- hack
            IF (BUFFER.EQ.-1) THEN
c             CALL SOUND@( 256,2)    !- need a standard error-box +<OK> button
              PRINT*,'****ERROR: IMAGE',KEY2,' out of memory' 
              CV(53) = 0     !- kill animation mode
              GOTO 991       !- abort the loop
            ENDIF
            NPICS = IPIC     !- number of stored frames

          IF (abs(CV(55)).EQ.1) THEN   !---- save current window to memory ----
c           IF (IMAGE(IPIC).NE.-1) 
c    &      CALL RETURN_STORAGE@ (IMAGE(IPIC))   !- zap any old image
            IF (IPIC>MIMAGES) then
              print*,' too many images - skipping'
            else
              IMAGE (IPIC) = BUFFER                !- remember this frame
            endif 
 
          ELSEIF (abs(CV(55)).EQ.2) THEN !---- save current window to PCX -----
!- under X11 / PGPLTO - There is probably a way to capture an XxYy screen rectangle and
! save as a bitmap
             WRITE (PICFILE,'(i8.8,a,t1,a)')     !- note *backwards* tabbing
     &        IPIC,'.PCX', FILE_ROOT(1:istr_end(FILE_ROOT))

c           CALL SCREEN_BLOCK_TO_PCX@ (PICFILE,BUFFER,IFAIL_2)
c           CALL DOSERR@(IFAIL_2)    !- shouldn't fail tho
c           CALL RETURN_STORAGE@ (BUFFER)      !- garbage collection
c           CALL PCX_FIX (PICFILE,PAL)    !- pre 2.76 DBOS needs this Subr.
          ENDIF      !- save to PCX
c         CALL RETURN_STORAGE@ (BUFFER)   !- zap any old image
        ENDIF      !- save image
      ENDIF      !- if ANIM


      ENDDO    !- to the next frame : IPIC
  991 CV(53) = 0     !- switch of animation after it has been done 

c-----------------------------------------------------------------------
c-------------- END of ALL windows of ALL frames -----------------------
c-----------------------------------------------------------------------
c.. 
      CALL GET_SECS (TIME(5))   !- time at the end of ALL drawing

      IDEST = 1     !- so future draws will go to the VGAscreen
      iresc = 256
      call reset_sizes(res)
      call get_app_size (idev_x,idev_y)
      CALL SET_DEVICE           !- reset back to the VGA ? (but resolution?)
c    &  (IDEST,640.,480.,256.,   12.,9.,  1.)
     &  (IDEST,idev_x*1.,idev_y*1.,256.,   12.,9.,  1.)
      GOTO 1110     !- back up to the menu & parsing 

c-----------------------------------------------------------------------
C--------------------> E X I T   P R O G R A M <------------------------
c-----------------------------------------------------------------------
c....... eg. from various errors or if event = 'q' ?
  999 CONTINUE  
c.. note that INTO_TEXT waits for <enter> to be pressed.
      CALL INTO_TEXT ()           !- sub-calls TEXT_MODE@()
      END                         !- The end of the Main program

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

c-----------------------------------------------------------------------
      SUBROUTINE SET_CV (ITEM,IVAL,IOP)
C
C     This database holds the values of CV .. the 'ITEM COLOURS' etc.
C     WOW ! This is fully object-orientated
C
c     for O-O, probally best to store the database in a single subroutine
c     that has a complete set of opcodes to get/put/restore any item
c     (in  this case a token's value, its string, the last referenced, etc.
c     user-callable routines are:
c       CV_GET        - get a value (-1 = get table length?)
c       CV_SET        - set a value (-1 = use last referenced='current')
c       CV_RESET      - reset a value/label (-1 = whole table?)
c       CV_GET_LABEL  - get string eg. for *WRITE_CV
c       CV_SET_LABEL  - only useful for user-defined labels
c
c
c
C     if IOP = 0  : all values are initialised
C        IOP = 1  : CV(ITEM) is set to IVAL
C        IOP = 2  : the current ITEM is rememberd
C        IOP = 3  : CV(rem.ITEM) is set to IVAL
C  (     IOP = 4  : CV(rem.ITEM  is re-initialised ? )
C
C        IOP = 5  : CV(ITEM) is returned in IVAL
C        IOP = 6  :  The remembered item is returned
C        IOP = .. :  any other options ?
C
c.. how about a pseudo-module.. CV_MOD
C    common CV across and call the access functions:
C       CV_INIT  : All to initial values, CV_INIT_ONE for just one ?
C                  NO, val=-1 should reset all
C    ?  CV_SET_CURRENT :  stores 'item' 
C    ?  CV_GET_CURRENT :  returns current
C                 ( or maybe CV_GET returns a specific, if -1 returns 
c                        'last' accessed one (both ITEM and CV() )
C      CV_PUT    : set a value (-1 = last accessed)
C      CV_GET    : get a value (-1 = last accessed)
C also..
C      CV_GET_NAME : returns the CV values 'name' so write init_file
C                    and in menu dialogs (long v. short names?)
C    ? CV_FIND_NAME : searches list for name and so returns value
C                     eg. EdgeColor = 23 (red?) in parsing

C---------------- colour info vector -----------------------------------
      SAVE
      COMMON /CV/CV               !- a ftn90 module surely :-)
      COMMON /CV_ALL/CVT,icurrent !- put CV here too and so make PRIVATE

      PARAMETER (   M_CV = 90)
      INTEGER  CV  (M_CV),CV1 (M_CV)    !- current table and initial

      CHARACTER CVT (M_CV)*20     !- descriptive strings (short?)
     &         ,CVT1(M_CV)*20     !- inital copy (cf 'block data')

      DATA ITEM_STORE/1/          !- default stored 'item'

      DATA (CV1(I),CVT1(I),I=1,10) /
c            1234+6789+1234+6789+
     & 501 ,'Facet Colour        '! 1  1001=mats, 2=facet dirn, etc.
     &,  1 ,'Show Displaced Mesh '! 2  displacement switch
     &, 13 ,'Edge Colour         '! 3  ( <1000 = solid olour)
     &,  0 ,'Vector position     '! 4  eg '5' (0.5) would centre on node
     &, -1 ,'Sub Edge colour     '! 5  
     &, -1 ,'n/a                 '! 6  ( material edges ) ?
     &, -1 ,'Material # colour   '! 7  col: material number
     &, -1 ,'Element # colour    '! 8  col: element number
     &, -1 ,'Node colour         '! 9  col:  nodes
     &,  2 ,'Node Size           '!10 pixels or 1/640ths. of image x ?)
     & /       

      DATA (CV1(I),CVT1(I),I=11,20) /
     &  -1 ,'Node # Colour       '! 11  
     &, -1 ,'Disp. vector colour '! 12  (code too?)
     &, -1 ,'Contour variable    '! 13   31= disp-x, etc.
     &, -1 ,'Contour line type   '! 14  -1,0,1,(1002=coloured)
     &,1002,'Contour fill type   '! 15  -1,0,1,(1002=col,7/8=zebra)
     &, -1 ,'Undef Elem. colour  '! 16  
     &,  2 ,'Depth sort code     '! 17  (0=off,1=nearest,2=furthest,3=central)
     &, 20 ,'# contours          '! 18  
     &, -1 ,'# sub-facets        '! 19  per element side
     &, 20 ,'Paper colour        '! 20  -1 = 'see-thru' = 'natural'
!    &, -1 ,'Paper colour        '! 20  -1 = 'see-thru' = 'natural'
     & /       

      DATA (CV1(I),CVT1(I),I=21,30) /
     &  -1 ,'Noded Element #     '! 21  (was gratic)  (if<1 then ALL)
     &,  0 ,'Legend colour       '! 22  (was gratic)  contour legend
     &,  0 ,'BBox-%oversize      '! 23  BB- % oversize
     &, -1 ,'XYZ axes colour     '! 24  
     &, -1 ,'BBox-edges          '! 25  BB- edges  (see-thru front-face?)
     &, -1 ,'BBox-fill           '! 26  BB-fill (also % oversize?)
     &, -1 ,'axes label colour   '! 27  x,y,z
     &,  0 ,'Window frame colour '! 28  
     &,  0 ,'Title colour        '! 29     is black or white best ?
     &,  0 ,'Cursor Key mode     '! 30   0=rotate,1=pan,2=light
     & /       

      DATA (CV1(I),CVT1(I),I=31,40) /
     &   0 ,'Element Shrink %    '! 31  +ve=%-age,-ve = in pixels
     &,  1 ,'Backface cull code  '! 32  0=none,1=back,2=front
     &, -1 ,'Regrid line colour  '! 33  
     &,  4 ,'Regrid Line #       '! 34  = 2**(n=4) 
     &, -1 ,'Flow net colour     '! 35  
     &, 30 ,'Flow net # lines    '! 36  in x dirn hence calc # in Y
     &, -1 ,'Multi-window Type   '! 37  -1=none ,1=load#(eg Eigens) ,2=view
     &,  1 ,'Double Buffer       '! 38  0=off, 1=on
     &, -1 ,'Load step # colour  '! 39  
     &, -1 ,'Image # colour      '! 40  eg.when animating
     & /       

c.. 27-7-98 I need to overload printers, so dest='prn', 'lpt2','file','autofile'
c first 2 are direct, third prompts for filename, fourth picks its own name
      DATA (CV1(I),CVT1(I),I=41,50) /      !----- windowing /printers ------ 
     &   1 ,'Window #            '! 41  picture #
     &,  1 ,'# windows across    '! 42  
     &,  1 ,'# windows down      '! 43  
     &,  0 ,'panning - x (%)     '! 44   (OLD-style shifting of COA
     &,  0 ,'Panning - y (%)     '! 45    as a +/1% of window size (-50%=RHS
     &,  9 ,'SVGA mode #         '! 46   eg 1024x768x256
     &, 30 ,'Printer type code   '! 47   30=PS, 40=HP-LJ etc.
     &,4560,'Printer x size      '! 48   .. should be OO as each printer 
     &,6360,'Printer y size      '! 49        has its own x,y, #colours
     &,256 ,'Printer # colours   '! 50    
     & /       
                         
      DATA (CV1(I),CVT1(I),I=51,60) /              !--- Animation etc. ----
     & 100 ,'Printer aspect ratio'! 51   (%)
     &,  0 ,'Auto-excavate code  '! 52   1= built-up, 2= excav 
     &,  0 ,'Animation GO        '! 53   = 1 if active
     &, 50 ,'Anim - # frames     '! 54   cf. NLDS
     &,  0 ,'Anim - dest code    '! 55   0=no store, 1=Memory, 2=PCX 
     &,  0 ,'Anim - dx-eye       '! 56  
     &,  0 ,'Anim - dy-eye       '! 57   .. ๋ COA for tunnels too
     &,  0 ,'Anim - dx-light     '! 58   < load step from/to ?? >
     &,  0 ,'Anim - dy-light     '! 59  
     &,  1 ,'Anim - disp code    '! 60   0=off, 1=disps, 3 for SIN (eg.Eigen)
     & /       

      DATA (CV1(I),CVT1(I),I=61,70) /      !-----  RING colours etc.----
     &   0 ,'User-lines colour   '! 61  'extra lines on the page'
     &, -1 ,'Ring edge colour    '! 62  1001++ = IMAT etc.
     &, -1 ,'Ring fill colour    '! 63  
     &, -1 ,'Ring def edge colour'! 64   - deformed
     &, -1 ,'Ring def fill colour'! 65   - deformed
     &, -1 ,'Ring def IMAT       '! 66  
     &, 10 ,'Anim delay -first   '! 67  1st frame   !--- replay parameters ---
     &,  2 ,'Anim delay -inter.  '! 68  normal frames  (cf a replayer prog)
     &, 10 ,'Anim delay -last    '! 69  last frame
     &, -1 ,'n/a                 '! 70      
     & /       


      DATA (CV1(I),CVT1(I),I=71,80) /      !-----  STRESSES -------
     &  -1 ,'sigma1              '! 71 vectors: most comp. stress tensor 
     &, -1 ,'sigma2              '! 72  & intermediate
     &, -1 ,'sigma3              '! 73 & most tensile  (iop for - or = ?)
     &, -1 ,'Tmax                '! 74 (max shear stain as 'X' (or / or bs)
     &, -1 ,'Tensile zones       '! 75 show -ve ๅ3 as a dot /vector
     &, -1 ,'Compr. zones        '! 76 show +ve ๅ1 as a dot /vector
c                                      overrides 71,72,73 if tensile
     &, -1 ,'                    '! 77
     &, -1 ,'                    '! 78
     &, -1 ,'                    '! 79
     &, -1 ,'no-contour IMAT     '! 80 These elements will *not* be contoured
     & /       

      DATA (CV1(I),CVT1(I),I=81,90) /      !-- Isosurfaces, etc. --
     &  -1 ,'                    '! 81 flag to 'dot contour' element centres
     &, -1 ,'                    '! 82
     &, -1 ,'                    '! 83 Isosurface flag (cf value?)
     &, -1 ,'                    '! 84
     &, -1 ,'                    '! 85
     &, -1 ,'                    '! 86
     &, -1 ,'                    '! 87
     &, -1 ,'                    '! 88
     &, -1 ,'                    '! 89
     &, -1 ,'                    '! 90      
     & /       

c-----------------------------------------------------------------------

      IF (IOP.EQ.0) THEN           !-------- reset data ---------
        DO I=1,M_CV
          CV(I) = CV1(I)
          CVT(I) = CVT1(I)
        ENDDO

c.. rest can now be handled by explicit cv_get/set
      ELSEIF (IOP.EQ.1) THEN       !-------- set data -----------
        CV (ITEM) = IVAL
      ELSEIF (IOP.EQ.2) THEN
        ITEM_STORE = ITEM
      ELSEIF (IOP.EQ.3) THEN
        CV (ITEM_STORE) = IVAL
      ELSEIF (IOP.EQ.4) THEN
        CV (ITEM_STORE) = CV1(ITEM_STORE)

      ELSEIF (IOP.EQ.5) THEN       !------- extract data --------
        IVAL = CV (ITEM)

      ELSEIF (IOP.EQ.6) THEN       !--- return 'remembered'
        ITEM = ITEM_STORE
      ELSE                         !---------- error ------------
        CALL MYERROR (2,'Unknown SET_CV option')
      ENDIF

      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE CV_RESET (ITEM)
c
c     Resets a CV value (-1 = all)
c
      COMMON /CV/CV               !- a ftn90 module surely :-)
      COMMON /CV_ALL/CVT,icurrent !- put CV here too and so make PRIVATE
      PARAMETER (   M_CV = 90)
      INTEGER  CV  (M_CV)  !,CV1 (M_CV)    !- current table and initial
      CHARACTER CVT (M_CV)*20     !- descriptive strings (short?)

      icurrent = 1    ! so always gets initialised to something ??
      IF (ITEM.GE.0.AND.ITEM.LE.M_CV) then
c       CV(ITEM) = CV1 (ITEM)
        call set_cv (999,999,0)
        icurrent = ITEM
      ELSEIF (ITEM.EQ.-1) THEN
        DO I=1,M_CV
c         CV(I) = CV1(I)
        ENDDO
        call set_cv (999,999,0)
      ELSE
        CALL MYERROR (2,'(CV_RESET) value out of range')
      ENDIF
      END

C-----------------------------------------------------------------------
      SUBROUTINE CV_SET (ITEM,IVAL)
c
c     Sets a CV value (-1 = 'current'
c.. maybe use 'include' files for the type declarations (functions too)
c.. or just take the plunge and use Fortran90 now!
C
      PARAMETER (   M_CV = 90)
      COMMON /CV/CV                !- a ftn90 module surely :-)
      COMMON /CV_ALL/CVT,icurrent  !- put CV here too and so make PRIVATE
      CHARACTER CVT (M_CV)*20      !- descriptive strings (short?)
      INTEGER  CV  (M_CV)          !- current table and initial

      CALL CV_GET_ITEM (ITEM,ICURRENT)
      CV (ICURRENT) = IVAL
      END

C-----------------------------------------------------------------------
      SUBROUTINE CV_GET (ITEM,IVAL)
c
c     Returns a CV value (-1 = 'current')
c
      PARAMETER (   M_CV = 90)
      COMMON /CV/CV                !- a ftn90 module surely :-)
      COMMON /CV_ALL/CVT,icurrent  !- put CV here too and so make PRIVATE
      CHARACTER CVT (M_CV)*20      !- descriptive strings (short?)
      INTEGER   CV  (M_CV)         !- current table and initial
 
      CALL CV_GET_ITEM (ITEM,ICURRENT)
      IVAL = CV (ICURRENT)
      END

C-----------------------------------------------------------------------
      SUBROUTINE CV_GET_ITEM (ITEM,ICURRENT)
C
C     (PRIVATE) Returns the 'current value'
C        Turns ITEM into the last value (ICURRENT) if ITEM = -1
C        if 0>ITEM>60 then no-op , else calls error-handler
C
      PARAMETER (   M_CV = 90)
c      COMMON /CV/CV                !- a ftn90 module surely :-)
c      COMMON /CV_ALL/CVT,icurrent  !- put CV here too and so make PRIVATE
c      CHARACTER CVT (M_CV)*20      !- descriptive strings (short?)
c      INTEGER   CV  (M_CV)         !- current table and initial

      IF (ITEM.GE.0.AND.ITEM.LE.M_CV) then   !(SELECT CASE)
        icurrent = item
      ELSEIF (ITEM.EQ.-1) THEN     ! a no-op (use 'old' icurrent)

      ELSE
        CALL MYERROR (3,'CV item value out of range')
      ENDIF
      END

C-----------------------------------------------------------------------
      SUBROUTINE CV_GET_LABEL (ITEM,LABEL)
c
c     Returns a CV value (-1 = 'current')
c
      PARAMETER (M_CV = 90)
      COMMON /CV/CV               !- a ftn90 module surely :-)
      COMMON /CV_ALL/CVT,icurrent          !- put CV here too and so make PRIVATE
      INTEGER  CV (M_CV)         !- current table and initial
      CHARACTER CVT (M_CV)*20     !- descriptive strings (short?)
      CHARACTER*(*) LABEL

      CALL CV_GET_ITEM (ITEM,ICURRENT)
      LABEL = CVT (ICURRENT)
      END

C-----------------------------------------------------------------------
      SUBROUTINE R_PALETTE (IO) !,PAL)
C
c     Reads in a palette table
C       as ICOL, IR,IG,IB <0-255>, cf a ICOL,R,G,B <0.-1.>
c       and store the palette in COMMON
C      DJK 21-1-97
C
      IMPLICIT NONE
c     INTEGER PAL (3,0:255)      !- 'current' colour palette
      INTEGER IO, IOSTAT,I, ICOL,IR,IG,IB

      DO I=1,9999
    1   READ(IO,*,IOSTAT=IOSTAT) ICOL, IR,IG,IB
        CALL IN_TEST (IO,IOSTAT,*1,*999)
        CALL SET_RGB_VALUE (ICOL,IR/255.,IG/255.,IB/255.)
      ENDDO
      CALL MYERROR (3,'-- should never get to this line') !-code = 3 ?
  999 RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE R_PALETTE_HLS (IO) !, PAL)
C
c     Reads in a palette table
C       as ICOL, IR,IG,IB <0-255>, cf a ICOL,R,G,B <0.-1.>
c       and store the palette in COMMON
C      DJK 21-1-97
C
      IMPLICIT NONE
c     INTEGER PAL (3,0:255)      !- 'current' colour palette
      INTEGER IO, IOSTAT,I, ICOL
      REAL H,L,S

      DO I=1,9999
    1   READ(IO,*,IOSTAT=IOSTAT) ICOL, H,L,S
        CALL IN_TEST (IO,IOSTAT,*1,*999)
        CALL SET_HLS_VALUE (ICOL,H,L,S)
      ENDDO
      CALL MYERROR (3,'-- should never get to this line') !-code = 3 ?
  999 RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE GET_PRINC_STRESS (STRESS,NODOF,IH,  SSS, DDD)
c
c     Turns a set of stresses into the principal values and their directions
c       Dan Kidger 17-2-97
c
      INTEGER NODOF,IH,IFAIL
      REAL STRESS(*)
      REAL xxx(3,3)             !- the stresses as a 3x3 matrix
      REAL SSS(3), DDD(3,3)     !- principal stresses and directions
      REAL WORK(3)              !- workspace
c
c----- 1: abstract the stresses ------
      SX=STRESS(1)       !--- this block of code to abstact SX,SY,..
      SY=STRESS(2)       !-- is used elsewhere too
      IF (NODOF.EQ.2) THEN
c     IF (IH.EQ.4) THEN
        SZ=STRESS(4)      !- sigma-z
        S4=STRESS(3)      !- shear term
        S5 = 0.
        S6 = 0.
      ELSEIF (NODOF.EQ.3) THEN
C     ELSEIF (IH.EQ.6) THEN
        SZ=STRESS(3)
        S4=STRESS(4)
        S5=STRESS(5)
        S6=STRESS(6)
      ENDIF

c----- 2: store in the 3x3 stress tensor ------
      xxx(1,1) = sx
      xxx(2,2) = sy
      xxx(3,3) = sz

      xxx(1,2) = s4
      xxx(2,3) = s5
      xxx(1,3) = s6

      xxx(2,1) = s4
      xxx(3,2) = s5
      xxx(3,1) = s6

c----- 3: calculate the eigenvalues (principal stresses) -------
c            and eigenvectors (directions) 
c .. these routines are from EISPACK (1970's) ?
c .. methinks a more explicit solution for a 3x3 system would be better
      CALL TRED2 (3,3,XXX,SSS,WORK,DDD,3,  0)
      CALL TQL2  (3,3,    SSS,WORK,DDD,3, IFAIL)
      RETURN
      END

c-----------------------------------------------------------------------
      SUBROUTINE DR_STRESS_VECTORS (STRESS, NODOF,IH, XYZ
     &   ,PM,SMAX, SMAX_SC, DIAG1
     &   ,ICOL_S1,ICOL_S2,ICOL_S3,ICOL_TMAX,ICOL_TENS,ICOL_COMPR)
c
c   This draws the three principal stress 'vectors' on the screen
c   for the given point XYZ.
c     Tensile zones are marked in 'RED' if desired.
c        Dan Kidger 26-2-97

C Hmm overload ICOL_TENS : if no S3 to draw then use a DOT
c                        : if S3 then colour that!
c .. for completenes we aught to have ICOL_COMPR that overloads ICOL_S1
c         --- get the principal stressees

      REAL STRESS(IH), XYZ(3)      !- stress and it coord.
     &   ,PM(4,4)                  !- plotting transformation matrix
     &   ,SMAX                     !- max. stress value found.
     &   ,DIAG1                    !- 5% of the model size'
      INTEGER ICOL_S1,ICOL_S2,ICOL_S3      ! colour: the 3 components
     &    ,ICOL_TMAX,ICOL_TENS,ICOL_COMPR   ! tens/compr override
     &    ,NODOF,IH
      REAL     SSS (3)             !- 3 principal stresses
     &        ,DDD (3,3)           !- 3 othogonal D.C's (eg stresses)
     &        ,VEC (3)             !- each vector
     &        ,SPT2(4)             !- the screen point
     &        ,x(2),y(2)           !- a line
      CHARACTER LINE*10
      LOGICAL DOUBLE               !- if double lines

      CALL GET_PRINC_STRESS (STRESS,NODOF,IH, SSS, DDD)
c.. where SSS = vector of 3 terms, and DDD is 3x3 set of D.C's
c.. hence SSS*DDD is a set of 3 vectors (x,y,z of each)

c---- loop the 3 stress directions
      DO IDIR=1,3
        IF (IDIR.EQ.1) ICOL = ICOL_S1
        IF (IDIR.EQ.2) ICOL = ICOL_S2
        IF (IDIR.EQ.3) ICOL = ICOL_S3

        VAL = SSS(IDIR)
        VEC(1) = ddd(1,IDIR)
        VEC(2) = ddd(2,IDIR)
        VEC(3) = ddd(3,IDIR)  

        IF (ICOL.ge.0) THEN
          SMAX = MAX( SMAX,ABS(VAL) )

          DOUBLE = (VAL.GT.0)               !- 2 lines if tensile
c         DOUBLE = (VAL.LT.0)               !- 2 lines if compressive
c.. but also if triggered must zap the double line hypothesis
          if (VAL.lt.0..and.ICOL_COMPR.ge.0) then
            icol = icol_compr
            double = .false.
          endif
          if (VAL.gt.0..and.ICOL_TENS .ge.0) then
            icol = icol_tens
            double = .false.
          endif

          CALL DR_TENSOR (XYZ,VEC, VAL/SMAX_SC *DIAG1 
     &      ,double, 0.10, PM, icol)    !- note flag for 'doubles'
        ENDIF  !- only if visible

      ENDDO  !-- loop the 3 principal directions

c----------------- tensile (+ve) stress detected -----------------------
c . Draw a DOT at the Gauss point if any stresses are tensile
c This is inhibited if we have already drawn the vector for this component
      IF (ICOL_TENS.GE.0.AND. 
     &  max(sss(1),sss(2),sss(3)).gt.0 .and. ICOL_S3.lt.0) THEN
        CALL TRANSFORM2 (XYZ,SPT2 ,PM)
        x(1) = spt2(1)
        y(1) = spt2(2)
c       X(2) = NINT (CV(10)*RES(1)/640.)     !- radius (as nodal size)
        X(2) = 2
        Y(2) = X(2)                          !- 
        CALL DR_PRIM (LINE,X,Y,2,icol_tens,10)  !- (code 10=circle)
      ENDIF

c---------------- compressive (-ve) stress detected --------------------
      IF (ICOL_COMPR.GE.0.AND.
     &  min(sss(1),sss(2),sss(3)).lt.0 .and. ICOL_S1.lt.0) THEN
        CALL TRANSFORM2 (XYZ,SPT2 ,PM)
        x(1) = spt2(1)
        y(1) = spt2(2)
c       X(2) = NINT (CV(10)*RES(1)/640.)     !- radius (as nodal size)
        X(2) = 2
        Y(2) = X(2)                          !- 
        CALL DR_PRIM (LINE,X,Y,2,icol_compr,10)  !- (code 10=circle)
      ENDIF

      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE graphics_start (vesa)
C
C   This routine puts the display in or out of graphics mode
c     Dan Kidger 7-2-98
c
c   We use this for the main application (DANPLOT)
c    so default is 640x480x256
C    we need to be able to use (say): 800x600, 24bit colour, novesa
c    Best if object orientated - so common entrypoint for setting and querying

c      save
       integer idev_x, idev_y, idev_ncols
       integer PGOPEN
       external PGOPEN
       logical vesa
       real*4  :: x1,y1,x2,y2

      idev_ncols=256
      call get_app_size (idev_x,idev_y)
c     if (vesa) call use_vesa_interface@()
c     CALL SELECT_SVGA_MODE (idev_x,idev_y,idev_ncols, IFAIL)
      IF (PGOPEN("/XWIN").le.0) then
        print*,'failed to open PGPLOT''s X11 driver, exiting..'
        stop
      ENDIF
      x1=0.;y1=0.;x2=1.;y2=1.
      call PGSVP (x1,x2,y1,y2)     !- set full viewport
      x1=0.;y1=0.;x2=640.;y2=480.
      call PGSWIN (x1,x2,y1,y2)  !- and set internals to 800x600

      call SET_IDEST(1)     !- ie. on-screen graphics
      RETURN
      END

C-----------------------------------------------------------------------
      subroutine put_app_size (idev_x,idev_y)
c
c     This saves the application window width and depth in common
c        cf get_app_size
c
      common /app_size/iidev_x,iidev_y, xsc
      iidev_x= idev_x
      iidev_y= idev_y
      xsc = idev_x/640.  !- relative scale-up
      return
      end

C-----------------------------------------------------------------------
      subroutine get_app_size (idev_x,idev_y)
c
c     This saves the application window width and depth in common
c        cf put_app_size
c
      common /app_size/iidev_x,iidev_y, xsc
      idev_x= iidev_x
      idev_y= iidev_y
      return
      end

C-----------------------------------------------------------------------
      subroutine get_idev_y (idev_x,idev_y)
c
c     simply returns the VGA screen height for a given x width
c     Overloaded to return the x width=640 too if it was given as -ve
c      perhaps we also should be able to unpack codes like 400300 ?
c       Dan Kidger 7-2-98
c
!      select case (idev_x)
!      case 
      if (idev_x.le.0) then
        idev_x=640
      endif
      if (idev_x.eq.320) then
        idev_y=200
      elseif (idev_x.eq.640) then
        idev_y=480
      elseif (idev_x.eq.800) then
        idev_y=600
      elseif (idev_x.eq.1024) then
        idev_y=768
      elseif (idev_x.eq.1280) then
        idev_y=1024
      else
        call myerror (2,'Unknown graphics resolution')
      endif
      return
      end

C-----------------------------------------------------------------------
      LOGICAL FUNCTION KEY_WAITING()
c
c     checks (but does not wait for) a key-press
c     returns true if there is at least one character to be read
c

c here a no-op
c but with ifc we have peekcharqq()
      KEY_WAITING=.FALSE.
      END

c-----------------------------------------------------------------------
      function cmnam ()
c
c  pretends to return command-line arguments
c
      character :: cmnam*60, arg*60
      integer   :: ipass=0, nargs=0
      integer, external :: danfe_argc
 
      ipass=ipass+1
      if (ipass==1) then
!       open (89,file='FILE')
        nargs= danfe_argc()
      endif
      if (ipass<=nargs) then
        call danfe_getarg(ipass,arg)
!        read (89,'(a)',iostat=ios) arg
!        if (ios.ne.0) arg=" "
        cmnam=arg
      else 
        cmnam=" "
      endif
      end
c-----------------------------------------------------------------------
c-----------------------------------------------------------------------
!----------------------------------------------------------------------------------------
      subroutine usage()
!
      write (*,'(a)')
     &"Usage:  danplot [-vh] [-g graphics] [input file(s)]"

      end
!----------------------------------------------------------------------------------------
      subroutine help()
!
      write (*,'(a)')
     & "Usage:  danplot [OPTIONS] file(s)" , 
     & "-v  verbose (may be repeated)" ,
     & "(more options need to be added here)"

      end

!----------------------------------------------------------------------------------------
      subroutine error(string)
!
      character string*(*)
      write (*,'(a)') "error:", string
      end

