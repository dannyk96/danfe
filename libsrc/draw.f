C-----------------------------------------------------------------------
c
c     This is Dan Kidger's Graphics Primitive library
c
c     It is called directly from DANPLOT or from my GINO emulator
c       (Therefore from the DANSLIP slip surface visualiser
c          and from the GINO demenstrations)
c
c     The core routine is DR_PRIM that handles ALL Output
c     This calls Salford drivers or my Postscript driver               
C
c included here:
c       1/ all 3D transformations  CAMERA etc.
c       2/ all Palette adjusting   SET_COLOURS ...
c       3/ all Primitive Drawing   DR_PRIM ..
c       4/ the GOURAUD interpolated shader :-)
c
c      Dan Kidger 15-6-93
C-----------------------------------------------------------------------
c  revisions:
C     20?- 4-95  Split off DRAW_SAL.F for Salford Specifics
c      25- 5-95  General TR's : ROTATE_3, SHIFT_3, etc.
c      12-11-95  DR_TENSOR added. - call twice for both components: s1,s3
c       8- 2-96  DR_DXF DXF driver added
c
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
c    0:  Routines for Initialising various Output devices eg. SVGA
C-----------------------------------------------------------------------
c-----------------------------------------------------------------------

c-----------------------------------------------------------------------
      SUBROUTINE SET_DEVICE 
     &  (IDEV,DEV_Xp,DEV_Yp,DEV_NC,DEV_XL,DEV_YL,DEV_AR)
C
C     This simply sets the output destiantion held in COMMON 
C      (used in DR_PRIM *only* ?)
C      .. be careful of the PALETTE also held in COMMON
C      .. really need to set all output device parameters 
C          eg. Xres, Yres, scale (to mm), AR, Ncols, etc.
C
      COMMON /DEST/IDV, DV_xp,DV_yp,DV_nc,DV_xl,DV_yl ,DV_ar
      IDV   = IDEV      !- device code
      DV_xp = DEV_Xp     !- # of 'device units' in x
      DV_yp = DEV_Yp     !                          y
      DV_nc = DEV_NC    ! # of different colours supported by the device 
      DV_xp = DEV_XP    !- physical width of paper/screen (mm)
      DV_yp = DEV_YP    !- physical height of paper/screen (mm)
      DV_ar = DEV_AR    !- aspect ratio of y pixels to x pixels

c      DV_XL = DEV_XL    !- physical size ?
c      DV_YL = DEV_YL
      RETURN
      END

c-----------------------------------------------------------------------
      SUBROUTINE GET_DEVICE 
     &  (IDEV,DEV_Xp,DEV_Yp,DEV_NC,DEV_XL,DEV_YL,DEV_AR)
C
C     This simply returns the output destiantion held in COMMON 
C      (used in DR_PRIM *only* ?)
C      .. be careful of the PALETTE also held in COMMON
C      .. really need to set all output device parameters 
C          eg. Xres, Yres, scale (to mm), AR, Ncols, etc.
C
      COMMON /DEST/IDV, DV_xp,DV_yp,DV_nc,DV_xl,DV_yl ,DV_ar
      IDEV   = IDV      !- device code
      DEV_xp = DV_Xp    !- # of 'device units' in x
      DEV_yp = DV_Yp    !                          y
      DEV_nc = DV_NC    ! # of different colours supported by the device 
      DEV_xp = DV_XP    !- physical width of paper/screen (mm)
      DEV_yp = DV_YP    !- physical height of paper/screen (mm)
      DEV_ar = DV_AR    !- aspect ratio of y pixels to x pixels

c      DEV_XL = DV_XL    !- physical size ?
c      DEV_YL = DV_YL
      RETURN
      END

c-----------------------------------------------------------------------
      SUBROUTINE SET_IDEST (IDEV)
C
C     This simply sets the output destiantion held in COMMON 
C      (used in DR_PRIM *only* ?)
C      .. be careful of the PALETTE also held in COMMON
C      .. really need to set all output device parameters 
C          eg. Xres, Yres, scale (to mm), AR, Ncols, etc.
C    --> probaly never call this ..only SET_DEVICE
c
c
      COMMON /DEST/IDV, DV_xp,DV_yp,DV_ncol,DV_xl,DV_yl ,DV_ar
      IDV = IDEV
      END

c-----------------------------------------------------------------------
      SUBROUTINE GET_IDEST (IDEV)
C
C     This simply returns the output destiantion held in COMMON 
C      (used in DR_PRIM *only* ?)
C      .. be careful of the PALETTE also held in COMMON
C      .. really need to set all output device parameters 
C          eg. Xres, Yres, scale (to mm), AR, Ncols, etc.
C
      COMMON /DEST/IDV, DV_xp,DV_yp,DV_ncol,DV_xl,DV_yl ,DV_ar
      IDEV = IDV
      END

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
c     1: The actual Drawing Routines 
C-----------------------------------------------------------------------
c-----------------------------------------------------------------------

C----------------------------------------------------------------------
        SUBROUTINE DR_PRIM (TEXT,X,Y,N,ICOL,IOP)
C
C  ******** This is the Main primitive drawing interface *********
C
C       X() ,Y() = array of N coordinates to draw (z too?)
C       TEXT     = (optional) Text string for output (embedded codes?)
C       ICOL     = colour index(es?) to the stored PAL(,) RGB values
C       IOP      = Primitive type (see below)
C       
C   the OUTPUT device code (and sizes) are held in COMMON
C
c---------------------------------------------
C      IOP=   1 = for closed edges
c             2 = for filled
c             3 = unclosed edge ,etc.
c            10 = filled circle (eg. nodes)
C            20 = text string (eg node numbers) -- normal font. 
C    etc. ?

      COMMON /DEST/IDV, DV_xp,DV_yp,DV_ncol,DV_xl,DV_yl ,DV_ar
c...     IDEST = the output Device, where
c...             1  = VGA
c...            10  = SVGA
c...            20+ = PCX
c...            30+ = Post-Script
c...            40+ = HP-laserjet   (PCL)
C...            50+ = HP7550        (HP-GL)     <--- NEW
c...            60+ = Epson dot-matrix   (check these)
c...            70+ = DXF driver ?
c...  17-7-97   80+ = WMF driver 

      REAL X(*),Y(*)        !--- x,y,(z) coords of the line/polygon
      CHARACTER TEXT*(*)

      IDEST = IDV
      IF (ICOL.lt.0) RETURN                 ! ie. if color = -1 exit !

c-----------------------------------------------------------------------
c.. note if SVGA device is >256 cols, then we must GET_RGB(icol)
c.. then icol <= 32*32*r + 32*g + b  (so no need for DACS anymore)

      IF (IDEST.EQ.-1) THEN                     !- no device nominated 
        PRINT*,'No Output device has been defined (DR_PRIM)'
        STOP
      ELSEIF (IDEST.EQ.0) THEN                  !- dummy device 

      ELSEIF (IDEST.LE.19) THEN                 !- to VGA/SVGA/SCREEN-BLOCK
c         CALL DR_VGA  (TEXT,X,Y,N,ICOL, IOP)
          CALL DR_PGPLOT (TEXT,X,Y,N,ICOL,IOP)
      ELSEIF (IDEST.LE.29) THEN                 !- to PCX
c         CALL DR_SALFORD (TEXT,X,Y,N,ICOL, IOP)
c         CALL DR_VGA (TEXT,X,Y,N,ICOL, IOP)          !27-7-98
      ELSEIF (IDEST.LE.39) THEN                 !- to Postcript
          CALL DR_PS   (TEXT,X,Y,N,ICOL, IOP)
      ELSEIF (IDEST.LE.49) THEN                 !- to HP-Laserjet
c         CALL DR_SALFORD (TEXT,X,Y,N,ICOL, IOP)
      ELSEIF (IDEST.LE.59) THEN                 !- to HPGL file
c         CALL DR_HPGL (TEXT,X,Y,N,ICOL, IOP)
      ELSEIF (IDEST.LE.69) THEN                 !- to Dot-matrix printers
c         CALL DR_SALFORD (TEXT,X,Y,N,ICOL, IOP)
      ELSEIF (IDEST.LE.79) THEN                 !- to DXF file ?
          CALL DR_DXF  (TEXT,X,Y,N,ICOL, IOP)
      ELSEIF (IDEST.LE.89) THEN                 !- to WMF file ?
c         CALL DR_WMF  (TEXT,X,Y,N,ICOL, IOP)
      ELSE                    !- other Salford (HP-Laser/ EPSON only)
c         CALL DR_SALFORD (TEXT,X,Y,N,ICOL, IOP)
          call myerror(3,' Unknown printer driver (DR_PRIM)')
      ENDIF

      END

C----------------------------------------------------------------------
      SUBROUTINE DR_ARROW (pt_fr,pt_to,iangle,iscale,icol)
C
C     To draw an arrow vector (from L2.for)    c. Dan Kidger
C       eg. angle=10. (deg) , scale = 10 (%)
C
c.... nicer as a sub-function of DR_PRIM ..extends X and Y for the head! 
c.... ie type 7= default arrow
c....    type 8= general arrow (head size and angle in X(3),Y(3)
c.... also could have 'other' arrows eg. filled_head (cf a PS macro)
c... 7-2-96 need an 'itype' for different arrows / glyphs 
C            (cf Crisp's set using data-statements)
c           so create glyph along the x-axis, then scale, and then rotate
c
      REAL PT_FR(*),PT_TO(*),  x(6),y(6)
      CHARACTER T*5

      XL=pt_to(1) - pt_fr(1)
      YL=pt_to(2) - pt_fr(2)

      IF (ISCALE.GT.0) THEN           ! skip if no arrow head
        A  =  3.14159265/180.  * IANGLE 
        SA = -SIN(A) * ISCALE/100.
        CA = -COS(A) * ISCALE/100.

        x(1) = pt_fr(1)    !- base
        y(1) = pt_fr(2)
        x(2) = pt_to(1)    !- top
        y(2) = pt_to(2)

        x(3) = x(2) + XL*CA + YL*SA     !- LH tip
        y(3) = y(2) - XL*SA + YL*CA
        x(4) = x(2)                 !- top again
        y(4) = y(2)
        x(5) = x(2) + XL*CA - YL*SA     !- RH tip
        y(5) = y(2) + XL*SA + YL*CA

        CALL DR_PRIM (T,X,Y,5,ICOL,3)    !----- as line-draw (filled ?)

      ENDIF
      RETURN
      END

C----------------------------------------------------------------------
      SUBROUTINE DR_TENSOR (pt_fr,dxyz, scale,double,sep,PM,icol)
C
C     To draw an tensor vector, centered on the given point.
C     if (double), then a double-line is drawn? (spaced at SEP * length?)
C     - note that this takes world coords then transforms them here.
c       unlike DR_ARROW which is in screen-coords
C       c. Dan Kidger 12-11-95
C 
C     ? use angle, to rotate in-plane
C     ? should this be 2D or 3D 
c     ? option to draw arrow heads 
C
C     pt_fr = anchor point
C     dxyz  = vector length (total or positive only?)
C     scale = arbitary scale factor (eg just=1.) 
C     double = logical flag to draw double lines
C     sep = seperation between double lines (eg .05 = 5% of vector length)
C     icol = colour index (cf a 24-bit definition)
C
c   - Maybe call with dxyz as the direction cosines of the s1 (or s3)
C     and put the magnitude of s1 in scale (* by the screen scale factor)
c   - really this is a glyph just like DR_ARROW
c
      REAL PT_FR(3) ,Dxyz(3),  SEP, PM(4,4)
      LOGICAL DOUBLE

      XL=dxyz(1) * scale /2.      !- half-length of the vector
      YL=dxyz(2) * scale /2.
      ZL=dxyz(3) * SCALE /2.      !- z-component (unused?)
C                          
      XS =  YL *SEP            !- clockwise seperation
      YS = -XL *SEP
      ZS = 0.

      x1 = pt_FR(1) - XL       !- a standard line
      y1 = pt_FR(2) - YL
      Z1 = pt_FR(3) - ZL

      x2 = pt_FR(1) + XL
      y2 = pt_FR(2) + YL
      Z2 = pt_FR(3) + ZL

      IF (.NOT. DOUBLE) THEN
        CALL DRAW_LINE_G (X1,Y1,Z1,  X2,Y2,Z2, ICOL,PM)
      ELSE
        CALL DRAW_LINE_G (X1+XS,Y1+YS,Z1+ZS, X2+XS,Y2+YS,Z2+ZS, ICOL,PM)
        CALL DRAW_LINE_G (X1-XS,Y1-YS,Z1-ZS, X2-XS,Y2-YS,Z2-ZS, ICOL,PM)
      ENDIF

      RETURN
      END

C----------------------------------------------------------------------
      SUBROUTINE DRAW_LINE_G (X1,Y1,Z1, X2,Y2,Z2, ICOL,PM)
C
C    Transforms and draws a line between the given 2 points
C    - called by DR_TENSOR
C    (cf my GINO interface)
C
      IMPLICIT NONE
      REAL X1,Y1,Z1, X2,Y2,Z2, PM(4,4), GPT(3),SPT(3), X(2),Y(2)
      INTEGER ICOL
      CHARACTER T*5

      GPT(1) = X1                        !- the first point
      GPT(2) = Y1
      GPT(3) = Z1
      CALL TRANSFORM2 (GPT, SPT, PM)
      X(1) = SPT(1)
      Y(1) = SPT(2)
 
      GPT(1) = X2                        !- the second point
      GPT(2) = Y2
      GPT(3) = Z2
      CALL TRANSFORM2 (GPT, SPT, PM)
      X(2) = SPT(1)
      Y(2) = SPT(2)
 
      CALL DR_PRIM (T,X,Y,2,ICOL,3)    ! line-draw
      RETURN
      END

C----------------------------------------------------------------------
      SUBROUTINE DRAW_POLY (SC,ISC,NPTS,ICOL,IPRIM)
C
C     This is simply a header to DR_PRIM 
C
C     it unpacks a 2D list of coords into X() and Y() then calls DR_PRIM
C     .. if SC (i,j) is reversed then we could call DR_PRIM directly :-)
C     IPRIM = 1 for edges, = 2 for filled, etc....
C     ** This is probally used for drawing facets and their edges
C
      IMPLICIT NONE
      SAVE                  !/ does this make it faster ?
      INTEGER ISC,NPTS,ICOL,IPRIM
      REAL SC(ISC,*)        ! x,y,(z) coords of the line/polygon
      REAL X(50), Y(50)
      CHARACTER TEXT*5
      CALL EXTRACT_POLY (SC,ISC,NPTS, X,Y)
      CALL DR_PRIM (TEXT,X,Y,NPTS,ICOL, IPRIM)   
      RETURN
      END

C----------------------------------------------------------------------
      SUBROUTINE EXTRACT_POLY (SC,ISC,NPTS, X,Y)
C
C     like DRAW_PRIM but just exatracts the polygon as 2 lists
C     it unpacks a 2D list of coords into X() and Y() 
C       .. called from DRAW_POLY and GOURAUD
C     SAVE                  !/ does this make it faster ?
      IMPLICIT NONE
      INTEGER ISC,NPTS,I
      REAL SC(ISC,*)        ! x,y,(z) coords of the line/polygon
      REAL X(*), Y(*)
      DO I=1,NPTS
        X(I) = SC(I,1)
        Y(I) = SC(I,2)
      ENDDO
      RETURN
      END

C----------------------------------------------------------------------
      SUBROUTINE GRATIC (IRESX,IRESY,NX,NY,ICOL)
C
C     draws a graticule of RESX by RESY boxes
C .. should really draw within the current window ??
C     15-6-93   ... actually this is a bit obsolete ! 

      INTEGER IRESX,IRESY,JRESX,JRESY, ICOL,I
      CHARACTER T*5
      REAL X(2),Y(2)
      JRESX = IRESX-1         !-- so that the far side is visible
      JRESY = IRESY-1         !--  "    "   " "   "
      DO I=0,NX                        !--- horizontal rulings
        X(1) = 0.
        X(2) = JRESX
        Y(1) = REAL(JRESY)*I / REAL(NX)
        Y(2) = Y(1)
        CALL DR_PRIM (T,X,Y,2,ICOL,3)    !----- edges
      ENDDO

      DO I=0,NY                         !--- vertical rulings       
        X(1) = REAL(JRESX)*I / REAL(NY)
        X(2) = X(1)
        Y(1) = 0.
        Y(2) = JRESY
        CALL DR_PRIM (T,X,Y,2,ICOL,3)    !----- edges
      ENDDO
      RETURN
      END

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
c     3: My Gouraud Shading routines   
C            .. eg. for colour filled contours
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C----------------------------------------------------------------------
      SUBROUTINE GOURAUD (SC,ISC,NPTS, COLS, N_CONTS,IOP,type)
C
C     .. should really be called GOURAUD_POLYGON ?
C     this shades a facet  
C     -- decomposing into triangles anchored at the first node
C        8nq's are a special case and are split into 6 tri's nicely
C
C     SC(ISC,*) are the (screen) coords of the 2D polygon of NPTS points
C     COLS(*)   are the values (colour-numbers) at each node 
C     N_CONTS   is used for 'cliping' the range
C     IOP      = the colour code (overloaded to give 'rainbow', 'zebra' etc.
C     TYPE      = 'fill' or 'line' = contour type
c
c     7-2-95  now if colour is constant, dont need to subdivide
c    -- better (and simpler) to add a centre node to *all* non-triangles
C     8-2-96  also nice to 'glue' all constant shade triangles into a
C             block to speed plotting and reduce PS file sizes.
c    28-12-96 GOURAUD still has bugs - also I would rather pass a list
C             of REAL countour values (so variable spacing?)

      REAL SC(ISC,*)  ,COLS(*)        !- given facet and nodal colours
     &  ,XP(3) ,YP(3) ,COL(3)         !- the abstracted triangles
      REAL X(50),Y(50)                !- for direct drawing
      CHARACTER TYPE*4

c      INTEGER T8NQ(3,6)
c      DATA T8NQ/1,2,8, 7,6,8, 6,8,2, 6,2,4, 2,3,4, 4,5,6/  !- 8nq :-)


c-------------- find the 'range' of the whole polygon -----------------
c ICOL_RANGE is the number of contour lines that cross the polygon.

c.. cf for REALS finding the max and min VALUES, hence loop up the INDEXes
c   of the lines that will cross this polygon.
c   If no-crossings then can fill in as a solid colour: 
c     If below Min. then either skip or fill in in the smallest colour
c     If above Max. then either skip or fill in in the highest colour

      COL_MIN = COLS(1)
      COL_MAX = COLS(1)
      DO I=2, NPTS
         COL_MIN = MIN (COL_MIN, COLS(I))
         COL_MAX = MAX (COL_MAX, COLS(I))
      ENDDO
      ICOL_MIN = INT(COL_MIN)
      IF (COL_MIN.LT.0) ICOL_MIN = ICOL_MIN - 1     !- FLOOR()
      ICOL_MAX = INT(COL_MAX)
      IF (COL_MAX.LT.0) ICOL_MAX = ICOL_MAX - 1     !- FLOOR()
      ICOL_RANGE = ICOL_MAX - ICOL_MIN

      IF (COL_MAX.Lt.0) RETURN          !- all below min.  28-12-96
      IF (COL_MIN.GE.N_CONTS) RETURN    !- all above max.

c------- If no lines cross we can just fill the whole polygon ------
c... errm but only if COL_MIN is between 0 and N_CONTS surely.
      IF (ICOL_RANGE.LE.0) THEN
        ICOL = int(COL_MIN + 1)                      !(why add 1?)
c       IF (ICOL.LT.0  .OR. ICOL. GT. N_CONTS) RETURN ! hack 28-12-96
        IF (type.NE.'fill') RETURN              !- nothing to draw
C       CALL DRAW_POLY    (SC,ISC,NPTS,ICOL,2)  !- a header to DR_PRIM
        CALL EXTRACT_POLY (SC,ISC,NPTS, X,Y)
        CALL GOURAUD_draw (X,Y,NPTS,ICOL, IOP,TYPE)
        RETURN
      ENDIF

c---------------- Process different 'options' -------------------
      IF (IOP.eq.-1) THEN      !- if 'off' skip
c     .. no-op ...

c---------- Average every element to the average value ----------
c- (note for an 8nq the nodes have different weights (1:4:1) so the
c- the mean element value should *not* be just the mean of 8 numbers
      ELSEIF (IOP.eq.-2) THEN  !- if 'average' : colour as the mean value
        CC = 0.
        DO I=1,NPTS
          CC = CC + COLS(I)
        ENDDO
        ICOL = nint(CC/NPTS)

c       CALL DRAW_POLY (SC,ISC,NPTS,ICOL,2)  !- a header to DR_PRIM
        CALL EXTRACT_POLY (SC,ISC,NPTS, X,Y)
        CALL GOURAUD_draw (X,Y,NPTS,ICOL, IOP,TYPE)
        RETURN

      ELSE                     !-  ie '0'= 'black' etc. etc.

C-------------------- loop the sub-triangles --------------------
c.. Maybe check each for no-draw, or constant-fill 
c.. If a  triangle is constant-fill then maybe try the next tri. too
c.. so lump together (beware convex polygons?)
        IF (NPTS.EQ.3) THEN   !- do trinagles directly
          DO I=1,3                
            XP (I) = SC (I,1)     !- just abstract as 3 lists
            YP (I) = SC (I,2)   
            COL(I) = COLS (I)
          ENDDO
          CALL GOURAUD_TRI (XP,YP, COL, N_CONTS,IOP,TYPE)
          RETURN
        ENDIF

c----------- method 2 : create a centre-node
        xp_ = 0.
        yp_ = 0.      !-  find the coord & colour of the centre.
        col_ = 0.

        DO I=1,npts
          xp_ = xp_ + sc(i,1)
          yp_ = yp_ + sc(i,2)
          col_ = col_ + cols(i)
        enddo

        XP (3) = xp_  /real(npts)    !- point #3 is always the centre
        YP (3) = yp_  /real(npts)  
        COL(3) = col_ /real(npts)
        DO IT = 1,NPTS               ! ie. NPTS sub-triangles
            i1 = it
            XP (1) =   SC (i1,1)     !- go clockwise :-)
            YP (1) =   SC (i1,2)   
            COL(1) = COLS (i1)
            i2 = mod(it,npts)+1         ! the 'next point' -- wrap back to 1
            XP (2) =   SC (i2,1)     !- go clockwise :-)
            YP (2) =   SC (i2,2)   
            COL(2) = COLS (i2)
          CALL GOURAUD_TRI (XP,YP, COL, N_CONTS,IOP,TYPE)
        ENDDO
        RETURN

c----------- method 1 : minimum number of triangles ----
C       NTRI = NPTS - 2               !-   # triangles  
c        DO IT = 0,NPTS-3
c          IF (NPTS.eq.8) THEN   ! if 8nq then dissect explicitely (9nq?)
c            DO I=1,3                  !- 5nq for 14nb's ? <- sub_facet
c              II = T8NQ(I,IT+1)       !- etc. ??
c              XP (I) =   SC (II,1)
c              YP (I) =   SC (II,2)   
c              COL(I) = COLS (II)
c            ENDDO

c          ELSE       !-- default is a set of triangles from node #1 -----
c            XP (1) =   SC (1,1)    !-cf creation of a central node.
c            YP (1) =   SC (1,2)
c            COL(1) = COLS (1)
c            DO I=2,3
c              XP (I)  =   SC (IT+I,1)
c              YP (I)  =   SC (IT+I,2)
c              COL(I)  = COLS (IT+I)
c            ENDDO
c          ENDIF  !- of 8nq/others

c          CALL GOURAUD_TRI (XP,YP, COL, N_CONTS,IOP,TYPE)
c        ENDDO    !- next triangle

      ENDIF   !- if not 'skipped' or 'averaged'
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE GOURAUD_TRI (XP,YP,COLS, N_CONTS,IOP,TYPE)
C
C     ** This contours the given triangle (filled/lines) **
C     usu. called from GOURAUD
C     given the triangle in XP,YP with cols COLS
C       TYPE ='fill' or 'line'
C       IOP is just passed through to select the colours
C  ?  values in COLS are skipped outside  0.->N_CONTS  <- check this
C
      REAL XP(3), YP(3), COLS(3) !- given triangle and nodal values
      REAL X(6),Y(6)             !- the polygon to draw
      INTEGER A,B,C              !- labels of the 3 points
      CHARACTER TYPE*4           !- ='fill' or 'line'
c     --- statement functions ---
      BIS   (VAL,E,F) = MIN(MAX(0., (VAL-E)/(F-E+t)) ,1.)
c     BIS   (VAL,E,F) = (VAL-E) / (F-E+t)
      RATIO (VAL,G,H) = G + (H-G) * VAL

      T = 1.E-12                    !-   = a 'small' number :-(

      A = 1                         !-  find :  'A' the largest value
      B = 2                         !           'B' the middle value
      C = 3                         !           'C' the smallest value

c.. maybe forget A B and C and just make X(1),Y(1),COLS(1) the *lowest*
c.. hence X(2).. and X(3)..
c.. really there are 6 possibilities, but note that I would like to 
c   differentiate between clockwise and anti-clockwise triangles       

      IF (COLS(A).LT.COLS(C) ) CALL ISWAP (C,A)  !- bubble-sort
      IF (COLS(B).LT.COLS(C) ) CALL ISWAP (B,C)
      IF (COLS(A).LT.COLS(B) ) CALL ISWAP (A,B)

      if (cols(a).lt.cols(b).or.cols(b).lt.cols(c)) 
     & STOP '*** oops wrong order in Gouraud'

      IF (COLS(a).Lt.0) RETURN                 !- all below min.  28-12-96
      IF (COLS(c).GE.N_CONTS) RETURN           !-contour N_CONTS is never filled

      icol_a = int (cols(a))
      if (cols(a).lt.0) icol_a = icol_a - 1    !- this is floor() - the 
      icol_b = int (cols(b))                   !- number *lower* than it
      if (cols(b).lt.0) icol_b = icol_b - 1
      icol_c = int (cols(c))
      if (cols(c).lt.0) icol_c = icol_c - 1

c.. I do not like the next 2 lines!
      I_LO = MAX (icol_C,  0)            !- the lowest contour 
      I_HI = MIN (icol_a+1,N_CONTS)       !- the highest contour

c better method:
c   never 'clip' the values at A,B,C ? 
c   -- be sure what happens for polygons triangles that contours that 
c      cross it. 

C--------------- loop from the MIN contour to the MAX contour ----------
c hmm maybe I should be looping ALL the contour values?
c (start-1 to finish+1 to make sure we catch ALL the fills
c cf NCONTS with CI.. ie one causes a calc of the other?

c.. hmm if all this elements values are above (N_CONTS) then we must do 
c   nothing -- not even 'fill in the last polygon'
      N = 0                      !- count points per polygon
c.. really we are looping the 'zones' 
      DO I=I_LO,I_HI
        RI = REAL (I)
        IF (I.EQ.icol_c) THEN
           N = N + 1
           X(N) =  XP(C)              !- record the first point
           Y(N) =  YP(C)              ! NO! it might be clipped off!
        ENDIF

        IF (I-1.EQ.ICOL_B) THEN    !- add middle node if necessary
          N = N + 1
          X(N) = XP(B)
          Y(N) = YP(B)
        ENDIF
c                                   (  be careful if COLS(B)=COLS(C) !)
c  1: maybe swap the next 2 over if we need *clockwise* polygons.
c  2: 
        N = N + 1
        IF (RI.LT.COLS(B)) THEN                  !- edge C -> B
          FAC  = BIS   (RI, COLS(C),COLS(B))
          X(N) = RATIO (FAC, XP(C), XP(B))
          Y(N) = RATIO (FAC, YP(C), YP(B))
        ELSE                                     !- edge B -> A 
          FAC  = BIS   (RI, COLS(B),COLS(A))
          X(N) = RATIO (FAC, XP(B), XP(A))
          Y(N) = RATIO (FAC, YP(B), YP(A))
        ENDIF

        N = N + 1                                !- 'long' edge A->C
        FAC  = BIS   (RI, COLS(C),COLS(A))
        X(N) = RATIO (FAC, XP(C), XP(A))
        Y(N) = RATIO (FAC, YP(C), YP(A))

c--------------- 'fill' and 'edge' the contour bands -------------------
c.. fills: on the first pass; if icol at A is less than the minimum.. then
c     this is the 'below' min band
c.. lines are always drawn between the 'last' two points

        IF (TYPE.EQ.'fill'.and. i.gt.0) then                 
          CALL GOURAUD_draw (X,Y,N,I,IOP, TYPE)
        ENDIF

        IF (TYPE.EQ.'line') THEN    !- skip last time?
c       ELSEIF (TYPE.EQ.'line'.and.i.lt.icol_a) THEN    !- skip last time?
          IF (n.ge.2) then 
            CALL GOURAUD_draw (X(n-1),Y(n-1),2,I,IOP, TYPE) 
          endif
        ENDIF
c    ---- update the 'back-edge' ----
        X(1) = X(N)       !- copy last 2 nodes to list base
        Y(1) = Y(N)       !- ready for the next contour band
        X(2) = X(N-1)
        Y(2) = Y(N-1)
        N = 2
      ENDDO

c---------------- finally finish off the last polygon ------------------
c.. hmm but if the do-loop was a no-op then should we do this ?
      N = N + 1
      X(N) =  XP(A)          !- record the first point
      Y(N) =  YP(A)
      IF (TYPE.EQ.'fill') then                 
        IF (cols(c).gt.N_CONTS) then
           CALL GOURAUD_draw (X,Y,N,I,IOP, TYPE) 
        ELSE
c          CALL GOURAUD_draw (X,Y,N,-1,IOP, TYPE) !- 'outside'
        ENDIF
      ENDIF
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE ISWAP (I,J)
c
c     this just swaps the 2 integer arguements
c        .. daughter of GOURAUD
c
        K = I
        I = J
        J = K
      END

C-----------------------------------------------------------------------
      SUBROUTINE GOURAUD_draw (X,Y,N,ICOL, IOP,TYPE)
C
C     This is a sub-part of 'GOURAUD' which simply draws the shapes
C     X(),Y() is the polgon of N points
C     ICOL is the colour code
C     IOP chooses which method to use (solid/zebra etc.)
C     IOP is the 'style' : black/coloured/zebra etc.
C     ITYPE =1 for filled and =2 for just the lines
C     hence ILF=2 for filled, 3 for just lines.
C
C     The options I want are:
C      -1 = invis (pre-filtered out)   
C      0->15 solid colours (0->256?)   .. eg. RED contour lines
C            'standard' colours (eg. spectrum)
C            b+w zebras   ..both types.. fixed to cols 1+2 ?
C            also zebras with invis inbetween
C            coloured zebras .. with b,w, or invis ? (therefore 2 codes?)
C    -- therefore code -1 to 15 are solid, 1000 is coloured
C    
C    2000+m indicates that we are zebra-ing?
C     hence we need a secondary code for the intermediate bands.
C     (therefore call Gouraud TWICE with intermediate bands as -1 ?
C      or just 2 opcodes (simpler) .. so if opcode 1 is 'flagged' 
C      then use opcode 2 for the other bands (default=black)
C      or even just if the secondary is not-zero then it is used and 
c      so 'overides'the primary value (and so doubles its spacing)
C                                                               
C    -- should I use NCONTS to MOD the values ?
C    .. maybe I should just produce a long-table of colour codes to use
C       and take the work away from my GOURAUD routine.
C
c     maybe only offer the following: ?
c        0-15 = solid colours     (for lines only?)
c        1000 = rainbow coloured (for vectors too?)
c        1001 = zebra coloured (others invis: use mat col to change bg.
c        1002 = zebra b+w (1003 = complimentary?)

c      14-8-92 now IOP<1000 = solid, >1000 = 'multi' (eg +2 = all cols)

      REAL X(*), Y(*)
      CHARACTER TYPE*4      !- ='fill' or 'line'
      CHARACTER T*5        !- dummy for DR_PRIM
                      

      if (type.eq.'fill'.and.n.le.2) return   !- if a zero-area polygon


!     IB = 2                !- offset into the colour table
!     IB = 16  -1           !- hacked to after the 'standard' colours
      IB = 20    !  -1      !- 20++ leaves me more room

      IF (TYPE.EQ.'line') IPRIM = 3    !- prim type 3 = polyline
      IF (TYPE.EQ.'fill') IPRIM = 2    !- prim type 2 = filled
                       
      J = ICOL/2            !- Ok half the colour
      K = ICOL-2*J          !- either 1=odd, or 2=even
c... hmm so fill will never pick a colour outside 1->16
c     ICOL2 = IB + MOD (ICOL,13)    !- offset into the colour table
      ICOL2 = IB + ICOL             !- offset into the colour table
      IOP2 = IOP-1000
      IF (IOP2.lt.0) then         !- solid colours
        CALL DR_PRIM (T,X,Y,N,IOP,IPRIM)
      ELSEIF (IOP2.eq.0) THEN                        !- black (obs)
        CALL DR_PRIM (T,X,Y,N,0,IPRIM)
      ELSEIF (IOP2.eq.1) THEN                        !- white    (obs)
        CALL DR_PRIM (T,X,Y,N,1,IPRIM)
      ELSEIF (IOP2.eq.2) THEN                        !- coloured
        CALL DR_PRIM (T,X,Y,N,ICOL2,IPRIM)
      ELSEIF (IOP2.eq.3) THEN                        !- zebra evens
        CALL DR_PRIM (T,X,Y,N,K,IPRIM)
      ELSEIF (IOP2.eq.4) THEN                        !- zebra odds
        CALL DR_PRIM (T,X,Y,N,1-K,IPRIM)                             
      ELSEIF (IOP2.eq.5) THEN                        !- stripe with invis (-1) 
        IF (K.EQ.0) CALL DR_PRIM (T,X,Y,N,ICOL2,IPRIM)
      ELSEIF (IOP2.eq.6) THEN                        !- stripe with 0
        IF (K.EQ.0) CALL DR_PRIM (T,X,Y,N,J+IB,IPRIM)
        IF (K.EQ.1) CALL DR_PRIM (T,X,Y,N,0,IPRIM)                              
      ELSEIF (IOP2.eq.7) THEN                        !- stripe with 1 
        IF (K.EQ.0) CALL DR_PRIM (T,X,Y,N,J+IB,IPRIM)          
        IF (K.EQ.1) CALL DR_PRIM (T,X,Y,N,1,IPRIM)                              
c     ELSEIF (IOP2.eq.8) THEN                        !- mat colour .. why ?     
c       CALL DR_PRIM (T,X,Y,N,ICMAT,IPRIM)                                      
c     ELSEIF (IOP.eq.9) THEN                        !- zebra with mat col    
c       IF (K.EQ.0) CALL DR_PRIM (T,X,Y,N,ICMAT,IPRIM)      ! (obs)             
c       IF (K.EQ.1) CALL DR_PRIM (T,X,Y,N,1,IPRIM)                              
c     ELSEIF (IOP.eq.10) THEN                       !- shade material ?        
c       ICOL3= (15-ICOL)*16 +ICMAT+1                !- (obsolete?)             
c       CALL DR_PRIM (T,X,Y,N,ICOL3,IPRIM)                                      
      ENDIF
      END

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
c     4: My 3D Volume-rendering routines
C           eg. MARCHING CUBES for isosurfaces
c             DJK 8-2-96
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

c 8-2-98 Notes on 'Marching Cubes'
c    - In 3d, we can draw iso-surfaces
c    - These are the 3d analogues of contours in 2d
c    - But can only rely see one isosurface at a time (unless transparaent)
c    - They need depth-sorting (or az z-buffer)
c    - Can be drawn flat - or average the normal vectors at the verteces
c    - So depth-sort the elements that have iso-surface in them, then
c      loop and sub-volume (sub-facet) each and build each's triangles,
c      depth-sort and draw (average normals at this level ?)
c
c  For any brick element:
c    1. Sub-facet into NxNxN sub-cubes (perhaps 1x1x1 for a 20nb?)
c        hence the xyz of the 8 nodes (C_SF) and the 8 values ()
c    2. If CV is <all or >all; then nothing to do - skip
c    3. Call Marching_Cubes to get a set of (1-6) triangles xyz.
c    4. (smooth normals?) - or use parent shape-functions?
c    5. Backface polygon cull.
c    6. store triangles - so can depth sort.
c    7. Depth sort
c    8. Draw.

c   Notes on iso-surfaces with other 'objects'.
c    - Perhaps we should depth-sort those elements that will be isosurfaced
c        together with the normal facets. 
c    - I guess we would want to front-face cull ? (or cull all?)
c    - We still would like to contour on cut faces?
c        This is the same as standard contouring with:
c          1. only one level
c          2. option set for 'clear below'
c    - I guess that the iso-surface colour should be interpolated from 
c       the contour colours? - but then we need to shade this too.
c       This leads us towards 24-bit colour
c    - I *can* have mulitple isosurfaces - we will see them where they 
c       meet an outer boundary.
c       (so set #isosurface bands: usu=2 so we *only* see the interface?
c       = 3 would show both the 1:2 and the 2:3 interfaces. 

c   Notes on F77/F90 implimentation of MARCHING_CUBES
c    - Original is fairly vanila C.
c    - Has 2 look-up tables both with 256 entries:
c       + First is 256 12-bit codes: each bit represents a cube-edge
c       + Second is 256 of; up to 5 3-integers: each repesenting the 
c         edge on which the three corners of the (5) triangles lie.
c   Algorithm:
c       1: Find how many nodes are above the threshold - 256 combinations   
c       2: So pick up which of the 12 edges:loop and linearly interpolate 
c          along each to give the xyzs
c       3: Pick up the set of predefined triangles: loop and store these.
c       4: done
c


C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
c    5: Routines for 2D/3D transformation with perspective
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C-----------------------------------------------------------------------
      SUBROUTINE GET_TR (TR)
C
C     This simply returns the current transformation matrix
C       (unused?)
C      IMPLICIT NONE
      SAVE
      INTEGER I,J
      real tr(4,4),tr_save(4,4)
      DO J=1,4
        DO I=1,4
          TR_save(I,J) = TR(I,J)
        ENDDO
      ENDDO
      END

C-----------------------------------------------------------------------
      SUBROUTINE PUT_TR (TR)
C
C     This (restores) the previously saved Transformation Matrix
C     this needs access to the MODULE that contains the stack
C     ie. PUSHPOP_TR  (1=push,2=POP) + errors if full or empty
C       (unused?)
C
      IMPLICIT NONE
      SAVE
       real tr(4,4) !,tr_save(4,4)
c      INTEGER I,J
c      DO J=1,4
c        DO I=1,4
c          TR(I,J) = TR_save(I,J)
c        ENDDO
c      ENDDO
      END

C-----------------------------------------------------------------------
      SUBROUTINE TRANSFORM (GPT,SPT, PM, IROT, XCEN,YCEN,SC_X,SC_Y,SC_Z)
C
C        *OBSOLETE* surely (cf just a MAT_MULT)
C     this transforms a point in GPT ('object' co-ordinates)
C                           into SPT ('screen' co-ordionates)
C     PM() is the homogeneous transformation matrix (from CAMERA q.v.)
C     if IROT = 1 then the X and Y screen axes are swapped (ie. X=up)
C     XCEN,YCEN = screen postion of the centre of the object 
C     SC_X,SC_Y,SC_Z are screen scaling factors 
C      (all the same unless the screen is anisotropic) 
C 
C.... this is too slow ... should build the IROT, etc. bits directly into PM
c
c     cf TRANSFORM in GINO.F

      REAL GPT(*), SPT(*), PM(4,4)

c....................... transform the 3D position .....................
      SPT4 =  GPT(1)*PM(4,1)+GPT(2)*PM(4,2)+GPT(3)*PM(4,3)+PM(4,4)
      SPT1 = (GPT(1)*PM(1,1)+GPT(2)*PM(1,2)+GPT(3)*PM(1,3)+PM(1,4))/SPT4
      SPT2 = (GPT(1)*PM(2,1)+GPT(2)*PM(2,2)+GPT(3)*PM(2,3)+PM(2,4))/SPT4
      SPT3 = (GPT(1)*PM(3,1)+GPT(2)*PM(3,2)+GPT(3)*PM(3,3)+PM(3,4))/SPT4
c....................... flip X and Y if desired........................
      IF (IROT.EQ.1) THEN
        T      = SPT1
        SPT1   =-SPT2
        SPT2   = T
      ENDIF
C-------------------- compute the screen coords ------------------------
      SPT(1) = XCEN + SPT1 * SC_X
      SPT(2) = YCEN + SPT2 * SC_Y
      SPT(3) =        SPT3 * SC_Z
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE TRANSFORM2 (X,SPT, PM)
C
C     this transforms a point in X ('object' co-ordinates)
C                           into SPT ('screen' co-ordionates)
C     PM() is the homogeneous transformation matrix (from CAMERA q.v.)

      REAL X(*), SPT(*), PM(4,4)

c....................... transform the 3D position .....................
      SPT4 = X(1)*PM(4,1)+X(2)*PM(4,2)+X(3)*PM(4,3)+PM(4,4)
      SPT(1) = (X(1)*PM(1,1) +X(2)*PM(1,2) +X(3)*PM(1,3) +PM(1,4))/SPT4
      SPT(2) = (X(1)*PM(2,1) +X(2)*PM(2,2) +X(3)*PM(2,3) +PM(2,4))/SPT4
      SPT(3) = (X(1)*PM(3,1) +X(2)*PM(3,2) +X(3)*PM(3,3) +PM(3,4))/SPT4
      END

C-----------------------------------------------------------------------
C     SUBROUTINE CAM_SCREEN (SCX,SCY,SCZ,  XC,YC, THETA,  TS)
      SUBROUTINE CAM_WINDOW (x1,y1,x2,y2, XF,YF,scale, theta,ar, PM )
C
C     This produces the *screen* scaling matrix
C       where:
C           THETA        : the angle to rotate the image by (=90 for landscape)
C           PM           : the returned 4x4 matrix
C           XF,YF        : relative (0.->1.) position of the origin in the window
C
      REAL PM(4,4)
      DTR = 3.14159265/180.

      XC = (1.-Xf)*X1 + Xf*X2           !- shift the centroid
      YC = (1.-Yf)*y1 + Yf*Y2       
      SC = SCALE * MIN (ABS(X2-X1),ABS(Y2-Y1) )/2. !- to fill the window

      DO I=1,4              !- null first ? (9/16 terms will be non-zero)
        DO J=1,4
          PM(I,J) = 0.
        ENDDO
      ENDDO

      SA = SIN (THETA*DTR)             !- in-plane spin (0 or 90 deg)
      CA = COS (THETA*DTR)
c     PM(1,1:4)=(/SC * CA,  -SC * SA, 0.,    XC  /)
c     PM(2,1:4)=(/SC*SA*AR, SC*CA*AR, 0.,    YC  /)
c     PM(3,1:4)=(/0.        0.,       SC,    0.  /)
c     PM(4,1:4)=(/0.        0.        0.,    1.  /)
      PM(1,1) =  SC * CA               !- scale to the window size
      PM(1,2) = -SC * SA 
      PM(2,1) =  SC * SA * AR
      PM(2,2) =  SC * CA * AR         
      PM(3,3) = SC   
      PM(4,4) = 1.   

      PM(1,4) = XC                       !- shift to the window centre
      PM(2,4) = YC   
      PM(3,4) = 0.   
      RETURN
      END

C----------------------------------------------------------------------
      SUBROUTINE CAMERA (EYE,COA,UP, DEYE,FEYE,PM)
C 
c     * OBSOLETE * .. CAMERA_2 is much neater.
C     sets up the camera transformation matrix
C
C     based on line_of_sight EYE (unit vector)  eg. 1 1 1 (cf eye coord)
C              camera_y_axis UP  (unit vector)  eg  0 1 0
C              centre_of_attention COA          eg 123. 87. -20.
C     DEYE = eye distance, FEYE = eye field of view (eg. 40.)
C     M = returned homogenous 4x4 transformation matrix
C
C     .. need an orthotropic version (no DEYE or FEYE (D) used)
c     9-2-96 I would like to move to an EYE x,y,z,(not COA) based system.
C
      REAL EYE(*), COA(*), UP(*)  
      REAL PM(4,4),   RM(4,4), TM(4,4), MP(4,4), MM(4,4), RX(3)

C ----- make sure are EYE,UP unit vectors ? -----
      CALL UNITV (EYE,3)
      CALL UNITV (UP, 3)

C ----- first appply the translation matrix 'TM' -----
c     CALL TRANS_3 (TM, -COA(1),-COA(2),-COA(3))
      CALL UNITM (TM,4,4)
      DO I=1,3
        TM(I,4) = - COA(I) 
      ENDDO
      CALL PREPEND_TR (PM, TM)

C ----- next apply the rotation matrix 'RM' -----
      CALL UNITM (RM,4,4)
      RX(1) = UP(2)*EYE(3) - UP(3)*EYE(2)    !- first the 'right-hand' vector
      RX(2) = UP(3)*EYE(1) - UP(1)*EYE(3)
      RX(3) = UP(1)*EYE(2) - UP(2)*EYE(1)
      CALL UNITV (RX,3)
      DO I=1,3
        RM(1,I) = RX(I)
      ENDDO
      RM(2,1) = EYE(2)*RX(3) - EYE(3)*RX(2)   !- next the 'up' vector'
      RM(2,2) = EYE(3)*RX(1) - EYE(1)*RX(3)  
      RM(2,3) = EYE(1)*RX(2) - EYE(2)*RX(1)  
      DO I=1,3
        RM(3,I) = EYE(I)                      !- third the eye-line itself
      ENDDO

C ----- next apply the  perspective matrix 'PM' ------
C .. this is based on the viewing distance DEYE
C ..           and the subtended eye angle FEYE

      D =  TAN( (90.-FEYE)/2. *3.14159265/180.)
      CALL UNITM (MP,4,4)
      MP(3,3)  = 0.
      MP(4,3)  = 1./D
      MP(3,4)  = DEYE
      MP(4,4)  = DEYE/D

C     CALL MATMUL(PM,4,TMP,4, M ,4, 4,4,4)
C -- do a 4x4 MATMUL explicitly      
      DO I=1,4
        DO J=1,4
          MM(I,J) = RM(I,1)*TM(1,J) + RM(I,2)*TM(2,J)
     +             +RM(I,3)*TM(3,J) + RM(I,4)*TM(4,J)
        ENDDO
      ENDDO
C -- apply perspective to PM explicitly
      DO I=1,4
        DO J=1,4
          PM(I,J)= MP(I,1)*MM(1,J) + MP(I,2)*MM(2,J)
     +            +MP(I,3)*MM(3,J) + MP(I,4)*MM(4,J)
        ENDDO
      ENDDO
      RETURN
      END

C----------------------------------------------------------------------
      SUBROUTINE CAMERA_2 (EYE,COA,UP, DEYE,FEYE,TWIST,SCALE, PM)
C 
C     Sets up the camera transformation matrix
C       New version, uses standard matrices vis SHIFT_3 etc.
C
C     based on line_of_sight EYE (unit vector)  eg. 1 1 1 (cf eye coord)
C              camera_y_axis UP  (unit vector)  eg  0 1 0
C              centre_of_attention COA          eg 123. 87. -20.
C     DEYE = eye distance, FEYE = eye field of view (eg. 40.)
C     PM = returned homogenous 4x4 transformation matrix
C
C     .. need an orthotropic version (no DEYE or FEYE (D) used)
C
      REAL  EYE(*)            ! unit vector *out* from the object to eye
     &     ,COA(*)            ! 'look at' point
     &     ,UP(*)             ! Up-vector (usu =0,1,0 for y-axis)
     &     ,DEYE ,FEYE        ! eye distance, field of view (eg 30 deg)
     &     ,TWIST             ! twist up-vector (roll) 
     &     ,PM(4,4)           ! Output homogeneos matrix
     &     ,TR(4,4)           ! workspace for each sub-matrix in the stack


      CALL UNITV (EYE,3)        !- make sure are EYE,UP unit vectors ? --
      CALL UNITV (UP, 3)
      CALL UNITM (PM,4,4)       !- re- initialise the view stack

      CALL TR_TRANS_3 (TR, -COA(1),-COA(2),-COA(3))  !- translate obj to origin
      CALL PREPEND_TR (PM, TR)

      CALL TR_EYE (TR, UP,EYE)            !- align along the eye-vector
      CALL PREPEND_TR (PM, TR)

      CALL TR_ROTATE_3 (TR, 1,TWIST)      !- twist about the eye-line
      CALL PREPEND_TR (PM, TR)

c.. cf perpective which returns a 'unit' sized view
c.. and parallel which scales to the current window
      if (abs(deye).lt. .00001) then    !- is parallel
        CALL TR_scale_3 (TR, 1./scale,1./scale,1./scale) 
        CALL PREPEND_TR (PM, TR)
      else                              !---- perspective ------
        CALL TR_PERSPECT (TR, DEYE,FEYE)    !- based on eye distance
        CALL PREPEND_TR (PM, TR)
      endif
      END

C-----------------------------------------------------------------------
      SUBROUTINE XM2EYE (XM,YM,EYE)
C
C     to convert latitude/longitude (XM,YM) to vector EYE (or LIGHT)
C      - and also resets the XM to -180 -> +180,  YM = -180 -> +180
C     28-2-94 : now overload XM; if 1001-1004 gives a preset view
C
      REAL EYE(*),XM,YM
      INTEGER XMI

      XMI = NINT (XM)
      IF (XMI.LT.1000) THEN
        XM = MOD (XM+180.+360.,360.) -180.    !- to range +/- 180.
        YM = MOD (YM+180.+360.,360.) -180. 
        DTR = 3.14159265/180.
        EYE(1) = - SIN (XM*DTR)            !- first rotate in the
        EYE(3) = - COS (XM*DTR)            !- x-y plane
        EYE(2) = - SIN (YM*DTR)            !- next the vertical value
        EYE(1) = EYE(1) * COS(YM*DTR)     !- and so need to rescale
        EYE(3) = EYE(3) * COS(YM*DTR)     !- the x and z
      ELSEIF (XMI.EQ.1001) THEN         !- down 'z'-axis = x-y plane
        EYE (1) = 0.
        EYE (2) = 0.
        EYE (3) = -1.
      ELSEIF (XMI.eq.1002) THEN         !- down 'y' axis = x-z plane
        EYE (1) = 0.
        EYE (2) =  -.999     ! (take care if we X-product EYE and UP(0,1,0)
        EYE (3) = -0.001
      ELSEIF (XMI.eq.1003) THEN         !- down 'x' axis = y-z plane
        EYE (1) = -1.
        EYE (2) = 0.
        EYE (3) = 0.
      ELSEIF (XMI.eq.1004) THEN         !- fixed 'isometric' view
        EYE (1) = -1.      
        EYE (2) = -1.      !- 1,1,1 is OK as long as we rescale :-)
        EYE (3) = -1. 
        CALL UNITV (EYE,3)
      ELSE     !--- ERROR .. probably we tried moving a 'fixed' view ----
        XME = 0.       !- so just reset for next time ?
      ENDIF
c        XME = -30.      ! maybe try this view too ?
c        YME =  15.
      RETURN
      END

C----------------------------------------------------------------------
      SUBROUTINE UNITV (V,N)
C
C     This scales a vector V(1:N) into a unit vector
C
      REAL V(*)
      VL = 0.
      DO I=1,N
        VL = VL + V(I)**2
      ENDDO
      VL = SQRT(VL)
      DO I=1,N
        V(I) = V(I) / VL
      ENDDO
      RETURN
      END

C----------------------------------------------------------------------
      SUBROUTINE UNITM (A,IA,N)
C
C     Makes the top NxN of A into a unit matrix
C
      REAL A(IA,*)
      DO I=1,N
        DO J=1,N
          A(I,J) = 0.
        ENDDO
        A(I,I) = 1.
      ENDDO
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE TR_TRANS_3 (TM, dx,dy,dz)
c
c     Returns a 4x4 matrix that shifts the view by dx,dy,dz
c     (need an option to pre-pend or post-pend this matrix onto the stack
c         DJK 25-5-95
c
      IMPLICIT NONE
      REAL TM(4,4), DX,DY,DZ
      CALL UNITM (TM,4,4)
      TM(1,4) = dx
      TM(2,4) = dy
      TM(3,4) = dz
      END

C-----------------------------------------------------------------------
      SUBROUTINE TR_SCALE_3 (TM, sx,sy,sz)
c
c     Returns a 4x4 matrix that scales the view by sx,sy,sz
c     (need an option to pre-pend or post-pend this matrix onto the stack
c         DJK 25-5-95
c
      IMPLICIT NONE
      REAL TM(4,4), SX,SY,SZ
      INTEGER I,J
      DO I=1,4                   !== just TM=0. in F90
        DO J=1,4                 ! (or array constructors)
          TM(I,J) = 0.
        ENDDO
      ENDDO
      TM(1,1) = sx
      TM(2,2) = sy
      TM(3,3) = sz
      TM(4,4) = 1
      END

C-----------------------------------------------------------------------
      SUBROUTINE TR_SHEAR_3 (TM, iax1,iax2,value)
c
c     Returns a 4x4 matrix that shears the view by VALUE
c     (need an option to pre-pend or post-pend this matrix onto the stack
c         DJK 25-5-95
c
      IMPLICIT NONE
      REAL TM(4,4), VALUE
      INTEGER  iax1,iax2, IAX3
      CALL UNITM (TM,4,4)
      IAX3 = 6-IAX1-IAX2
c     TM (IAX1,IAX2) = VALUE    !- hmm got problems with this
c     TM (IAX2,IAX1) = VALUE
      END

C-----------------------------------------------------------------------
      SUBROUTINE TR_ROTATE_3 (TM, Iaxis, THETA)
c
c     Returns a 4x4 matrix that rotates the view by THETA about axis #iaxis
c     (need an option to pre-pend or post-pend this matrix onto the stack
C     (styled from p145 of "Advanced Graphics with the Sinclair ZX Spectrum"
C     (I.O.Angell and B.J.Jones, MAcmillan, 1983)
c         DJK 25-5-95
c
      IMPLICIT NONE
      REAL TM(4,4), THETA,C,S
      INTEGER IAXIS,I,J, iax1,iax2
      DO I=1,4                   !== just TM=0. in F90
        DO J=1,4          
          TM(I,J) = 0.           !- why can I not just use UNITM ()?
        ENDDO
      ENDDO
      TM (4,4) = 1
      TM(IAXIS,IAXIS) = 1
      IAX1 = MOD(IAXIS,3)+1
      IAX2 = MOD(IAX1 ,3)+1
      S = SIN (THETA)
      C = COS (THETA)
      TM (IAX1,IAX1) = C
      TM (IAX1,IAX2) = -S
      TM (IAX2,IAX1) = S
      TM (IAX2,IAX2) = C
      END

C-----------------------------------------------------------------------
      SUBROUTINE TR_EYE (TM, UP,EYE2)
c
c     Returns a 4x4 matrix that rotates the view along to the eye vector
c      UP is the upwards vector so an axis (usu y) remains vertical
c     (need an option to pre-pend or post-pend this matrix onto the stack
c         DJK 25-5-95
c
      IMPLICIT NONE
      REAL TM(4,4), UP(3), EYE(3), RX(3), eye2(3)
      INTEGER I

      DO I=1,3              !- the fringes
        TM(I,4) = 0.
        TM(4,I) = 0.
      ENDDO
      TM(4,4) = 1
      eye(1) = -eye2(1)
      eye(2) = -eye2(2)
      eye(3) = -eye2(3)

      RX(1) = UP(2)*EYE(3) - UP(3)*EYE(2)    !- first the 'right-hand' vector
      RX(2) = UP(3)*EYE(1) - UP(1)*EYE(3)
      RX(3) = UP(1)*EYE(2) - UP(2)*EYE(1)
      CALL UNITV (RX,3)
c      rx(1) = - rx(1)
c      rx(2) = - rx(2)   !- 14-6-95 hack cos gino inverts L<->R
c      rx(3) = - rx(3)     !(because of the direction of EYE()
      DO I=1,3
        TM(1,I) = RX(I)
      ENDDO
c      rx(1) = - rx(1)
c      rx(2) = - rx(2)   !- 14-6-95 hack cos gino inverts L<->R
c      rx(3) = - rx(3)     !(because of the direction of EYE()
      TM(2,1) = EYE(2)*RX(3) - EYE(3)*RX(2)   !- next the 'up' vector'
      TM(2,2) = EYE(3)*RX(1) - EYE(1)*RX(3)  
      TM(2,3) = EYE(1)*RX(2) - EYE(2)*RX(1)  
c      tm(2,1) = - tm(2,1)
c      tm(2,2) = - tm(2,2)
c      tm(2,3) = - tm(2,3)
      DO I=1,3
        TM(3,I) = EYE(I)                      !- third the eye-line itself
      ENDDO
      END

C-----------------------------------------------------------------------
      SUBROUTINE TR_PERSPECT (TM, DEYE,FEYE)
c
c     Returns a 4x4 matrix that applies perspective
c     FEYE is the angle subtended at the eye (eg. 30 deg.)
c     DEYE is the distance of the eye to the viewplane (?)
c     ? why is this not simply 'divide' by 'Z' ?
c         DJK 25-5-95
c
      IMPLICIT NONE
      REAL TM(4,4), DEYE,FEYE,D
      CALL UNITM (TM,4,4)
c... so D <1.   = height of an object 1 unit away from the eye
c     D =  TAN( (90.-FEYE)/2.    *3.14159265/180.)   ! (yuk)
      D =  TAN( (90.-FEYE) *3.14159265/180.)   ! (yuk)
      TM(3,3)  = 0.
      TM(4,3)  = 1./D
      TM(3,4)  = DEYE
      TM(4,4)  = DEYE/D
      END

C-----------------------------------------------------------------------
      SUBROUTINE PREPEND_TR (TM, A)
C
C     Simply pre-multiplies the 4x4 transformation matrix PM by A
C      (this is the usual case cf. POSTPEND_TR)
C       DJK 25-5-95
C
      IMPLICIT NONE
      REAL TM(4,4), A(4,4),T(4,4)
      INTEGER I,J
      DO I=1,4
        DO J=1,4
          T(I,J) = A(I,1)*TM(1,J) + A(I,2)*TM(2,J)
     &            +A(I,3)*TM(3,J) + A(I,4)*TM(4,J)
        ENDDO
      ENDDO
      DO I=1,4
        DO J=1,4
          TM(I,J)= T(I,J)
        ENDDO
      ENDDO
      END

C-----------------------------------------------------------------------
      SUBROUTINE POSTPEND_TR (TM, A)
C
C     Simply post-multiplies the 4x4 transformation matrix PM by A
C       DJK 25-5-95
C
      IMPLICIT NONE
      REAL TM(4,4), A(4,4),T(4,4)
      INTEGER I,J
      DO I=1,4
        DO J=1,4
          T(I,J) = TM(I,1)*A(1,J) + TM(I,2)*A(2,J)
     &            +TM(I,3)*A(3,J) + TM(I,4)*A(4,J)
        ENDDO
      ENDDO
      DO I=1,4
        DO J=1,4
          TM(I,J)= T(I,J)
        ENDDO
      ENDDO
      END

C----------------------------------------------------------------------
C----------------------------------------------------------------------
C         5: COLOUR PALETTES
C----------------------------------------------------------------------
C----------------------------------------------------------------------

C-----------------------------------------------------------------------
      SUBROUTINE SET_RGB_VALUE (ICOL, R,G,B)
C
C     Sets the colour index ICOL to the given RGB colour 
C       DJK 24-1-95
C
      IMPLICIT NONE
      COMMON /PALETTE/PAL            !(be careful of the *scope* of COMMON)
      INTEGER ICOL
      REAL R,G,B
      INTEGER PAL (3,0:255)
      PAL (1,ICOL) = min(max(0,nint( R *255)),255)    !- clip for
      PAL (2,ICOL) = min(max(0,nint( G *255)),255)    !- good measure
      PAL (3,ICOL) = min(max(0,nint( B *255)),255)
c     CALL SET_DACS (icol,icol)      !- just does one
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE GET_RGB_VALUE (ICOL, R,G,B)
C
C     Returns the RGB colour for a given pen number
C       DJK 24-1-95
C
      IMPLICIT NONE
      COMMON /PALETTE/PAL
      INTEGER PAL (3,0:255), ICOL
      REAL R,G,B
      R = PAL (1,ICOL) /255.     !- as 0. -> 1.
      G = PAL (2,ICOL) /255.
      B = PAL (3,ICOL) /255.
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE SET_HLS_VALUE (ICOL, H,L,S)
C
C      *obsolete* just call the 2 daughter routines directly
C     Simply the HLS colour of a given colour index
C       DJK 24-1-95
C
      INTEGER ICOL
      REAL H,L,S, R,G,B
      CALL HLS_TO_RGB   (H,L,S, R,G,B)
      CALL SET_RGB_VALUE (ICOL, R,G,B)
      RETURN
      END

C----------------------------------------------------------------------
      SUBROUTINE SET_COLOURS (IOP,ICOL2,ncol)
C
C     set the colour palette colour(s)  (ne‚ SET_PAL)
C      0 = * reset to standard VGA *
C      2 = reverses the sequence of colours (2-15)
C      3 = inverts the RGB of all the colours
C      4 = swaps the fg/bg of the menus (colours 0 and 1)
C      5 = swaps alternate colours ('evens')
C      6 = shades the whole pallete block from black to the given colour #
C      7 = sets a colour to a random value
C      8 = sets a colour to a prompted for value
C      9 = cycle the menu base colour (eg. grey->pink->grenn ...)
C     10 = just set the  menu fg & bg to black & white
C     11 = 'material' set
C     12 = rainbow   (orange->blue)
C     13 = Hot-iron  (after Haines)
C     14 = spectrum  (red-> magenta)
C     15 = grayscale
C     16 = Random !
C     17 = Print the current set
C     18 = set 0 to white; all others to black (eg for laser-printers)
C     19 = 0=gray,1=black,2-7 = roygbiv (rainbow), 8-15 = greyscale "GINO"

C     21 = Patran set .. 16 cols,  blues,greens,pinks,yel->red.
C   (  21 = red->green )
C     22 = blue-> yellow
C     23 = red->black->cyan
C     24 = user-defined (prompted for)
C     25 = grey
C     26 = prompts for 2 colour numbers
C
C     41 = rotate palette up
C     42 = rotate palette down
C
C    revision history:
C    16-2-94 added IOP=16 for a RANDOM set 
C    17-2-94 added IOP=17 to print the current set
C            added 6,7,8 & changed name to SET_COLOURS
C     2-6-97 added 27 = shade the first 10 materials numbers (as if contours)

      COMMON /PALETTE/PAL
      save imenu_back, ipass
c     INTEGER*2 IFAIL                    !- for read_edited_line ?
      INTEGER PAL (3,0:255)
      INTEGER PAL_DEFAULT (3,20),        !- colours for lines and materials
     +        PAL_GINO    (3,0:15),      !- the colours as used by GINO
c    +        PAL_RAINBOW (3,15),        !- a preset rainbow 
     +        PAL_PATRAN  (3,16),        !- reverse-engineered PATRAN contour scale - interpolate?
     +        PAL_MENUPANE(3,7)          !- some background colourdea  
      REAL R,G,B, H,L,S
      INTEGER PAL1 (3,0:255)           !- just for reseting to VGA
      REAL GET_RANDOM
      EXTERNAL GET_RANDOM

c     CHARACTER LINE*80 
      REAL PI
      DATA PAL_DEFAULT/
     +     0, 90, 10,   138, 75, 39,    83,128, 57,    90,  0,190,
     +   123, 45, 76,
     +   194, 49, 56,   252, 78,207,   253,202, 90,     0,255,  0, 
     +    40, 40,255,   169,133, 23,   255,  0,  0,   255,127,  0,
     +    255,255, 0,   255,255,255,
     +     123,254,12,  243,200,123,   93,23,56,      83,200,205,
     +     12,254,123/

      DATA PAL_GINO/   ! 0=gray,1=black,2-8=red,ora,yel,grn,cyan,blu,mag
     +   127,127,127,     0,  0,  0,   !- 9=brown,10=white        
     +   255,  0,  0,   255,127,  0,   255,255,  0,  !- 11..=?
     +     0,255,  0,     0,255,255,     0,  0,255,   255,  0,255, 
     +   152, 76,  0,   255,255,255,    84, 84,252,    84,252, 84,
     +    84,252,252,   252, 84, 84,   252, 84,252 /

c     DATA PAL_RAINBOW/ 
c    +   40,120, 90,     1,177,204,     1,204,177,     9,227,147, 
c    +   23,243,116,    45,253, 85,    71,255, 57,   100,249, 33,  
c    +  131,236, 15,   162,216,  4,   191,191,  0,   216,162,  4,
c    +  236,131, 15,   123, 45, 76,   255,255,255/  !(last 2 are dummy)   

      DATA PAL_PATRAN/
     &   48, 48, 51,     4, 18, 48,     3, 14, 42,     2,  1, 12,
     &   12, 11, 34,    21, 40, 32,     4, 16, 14,     5,  9, 11,
     &   62, 52, 60,    52, 20, 38,    39,  1, 11,    63, 63, 10,
     &   62, 52, 11,    53, 28,  9,    38,  1,  4,    63, 63, 63 /

      DATA imenu_back / 1 /, ipass/0/
      DATA PAL_Menupane/  127,127,160, 
     +      160, 80, 80,   80,160, 80,  80, 80,160,  
     +      160,160, 80,   80,160,160, 160, 80,160 /
 
      DATA PI/3.14159265/
      JM = 0    !(was 2)    !- #menu colours: so COLORS  0,1,2  are reserved
      IBCONT = 20           !- where the contours start
      ipass = ipass+1
      if (ipass.eq.1) then   !- initialise
        do i=0,255
          pal(1,i) = i/2     !- a set of dark reds
          pal(2,i) = 0
          pal(3,i) = 0
        enddo
      endif

C--------- rest all colours to 'standard' VGA ------------
c.. need a 'daughter' routine as this is not unix-portable
      IF (IOP.EQ.0) THEN
c       CALL LOAD_STANDARD_COLOURS@ ()     !- hmm this changes the screen!
c       CALL GET_VIDEO_DAC_BLOCK@ (0,256,PAL1)
        DO I=0,255- 5     !less 5 to avoid my menu colours :-)
          DO J=1,3
c           PAL (J,I) = PAL1 (J,I) * 4
            PAL1(J,I) = I     !- try just a gray scale for now
            PAL (J,I) = I     !- try just a gray scale for now
          ENDDO
        ENDDO

C-------------- reverse palette ---------------
      ELSEIF (IOP.EQ.2) THEN
        jm =ibcont  !- so after the 'material colours'
        DO I=1,(ncol+1)/2 
          DO J=1,3                        !-- be careful of ISWAP !
            CALL ISWAP (PAL(J,I+jm),PAL(J,ncol+jm+1-I) )
          ENDDO
        ENDDO

c------------------- invert all the contour colours --------------------
      ELSEIF (IOP.EQ.3) THEN    
        jm = ibcont !- so after the 'material colours'
        DO I=1,ncol
          DO J=1,3
            PAL(J,jm+I) = 255 - PAL(J,jm+I)
          ENDDO
        ENDDO

C------------- reverse black and white --------------
      ELSEIF (IOP.EQ.4) THEN
        DO J=1,3
          CALL ISWAP (PAL(J,0),PAL(J,15) )
        ENDDO

C----------- reverse alternate colours --------------
      ELSE IF(IOP.EQ.5) THEN                                       
        jm = ibcont !- so after the 'material colours'
        DO I=1,ncol-1,2               !- == 'evens'
          DO J=1,3
c           CALL ISWAP (PAL(J,I+jm+1),PAL(J,I+jm+2) )
            CALL ISWAP (PAL(J,I+jm),PAL(J,I+jm+1) )
          ENDDO
        ENDDO

C---------------------- Shade the current RGB --------------------------
c... need a varient that does a HLS set (so black->colour->white)
c... so do RGB_to_HLS to get the hue .. then do L=0.->1. hence RGB
      ELSEIF (IOP.EQ.6666) THEN   !- temporarily zapped
        jm = ibcont !- so after the 'material colours'
        CALL GET_RGB_VALUE (ICOL2, R, G, B)
        CALL INTERP_RGB (0.,0.,0., R,G,B, 1+jm, NCOL+jm)

c        IOFF= 6         !- weights the shades to not start at full black
c        IR = PAL(1,ICOL2)    !- get the target colour
c        IG = PAL(2,ICOL2)    !-
c        IB = PAL(3,ICOL2)    !-
c        DO I=1,ncol
c          PAL(1,I+JM) = IR * (I+IOFF) /(NCOL+IOFF)
c          PAL(2,I+JM) = IG * (I+IOFF) /(NCOL+IOFF)
c          PAL(3,I+JM) = IB * (I+IOFF) /(NCOL+IOFF)
c        ENDDO

C---------------------- Shade the current HLS -------------------------
      ELSEIF (IOP.EQ.6) THEN
        jm = ibcont !- so after the 'material colours'
        CALL GET_RGB_VALUE (ICOL2, R, G, B)
        CALL RGB_TO_HLS (R,G,B, H,L,S)
        CALL INTERP_HLS (H,0.,S , H,1.,S , 1+jm, nCOL+jm)
C       CALL INTERP_HLS (H,L ,0., H,L ,1., 1+jm, nCOL+jm)

C------------------ set a single colour to a random value --------------
      ELSEIF (IOP.EQ.7) THEN   
        r = get_random()
        g = get_random()
        b = get_random()
        call set_rgb_value (icol2,r,g,b)

C--------------- set a single colour to an input value -----------------
      ELSEIF (IOP.EQ.8) THEN
        print*,'Enter the RGB (0->255) for colour #',ICOL2
        read*,IR,IG,IB
        PAL (1,ICOL2) = IR
        PAL (2,ICOL2) = IG
        PAL (3,ICOL2) = IB

C----------------- cycle the bg colour of the menus --------------------
C..... I could even copy one of the other colours to here ?
      ELSEIF (IOP.EQ.9) THEN
       imenu_back = mod (imenu_back, 7) + 1        !- cycle
       ij = 250     !- offset into menu -colours
       ij=16
       DO J=1,3
         PAL (J,ij+2) = PAL_Menupane (J,imenu_back)   !- and set.
       ENDDO

C--------------------- standard 'menu' colours -------------------------
c... nicer to do HLS of a 'theme' eg Blues (like motif)
c... so provide 5 colours   white, -, col, -, black  ??
      ELSEIF (IOP.EQ.10) THEN
       ij = 250     !- offset into menu -colours
       ij=16
       PAL (1,ij+0) =  25             !- 'black'
       PAL (2,ij+0) =  25             !- 
       PAL (3,ij+0) =  70             !- 
       PAL (1,ij+1) = 210             !- 'white'
       PAL (2,ij+1) = 210             !- 
       PAL (3,ij+1) = 255             !- 
       PAL (1,ij+2) = PAL_Menupane (1,imenu_back)
       PAL (2,ij+2) = PAL_Menupane (2,imenu_back)
       PAL (3,ij+2) = PAL_Menupane (3,imenu_back)

       PAL (1,ij+4) = nint(40.   *1.5)   !- what is this for?
       PAL (2,ij+4) = nint(40.   *1.5)
       PAL (3,ij+4) = nint(80.   *1.5)

c--------------------- standard 'material' colours ---------------------
      ELSEIF (IOP.EQ.11) THEN
        JM=0
        DO I=1, 20 
          DO J=1,3
            PAL (J,I+JM) = PAL_DEFAULT (J,I)
          ENDDO
        ENDDO

C------------------ 'rainbow' colours for contouring -------------------
      ELSEIF (IOP.EQ.12) THEN
        jm = ibcont !- so after the 'material colours' ??
        DO I=1,ncol
c          DO J=1,3
c            PAL(J,I+jm) = PAL_RAINBOW(J,I)
c          ENDDO
           S = 1.                       !- fully saturated colours !
           V = 1.                       !- and full 'brightness'
           H1 = (NCOL-I) / REAL (NCOL)    !- Hue as 0.->.999
           CALL RAINBOW (H1, S, V, R, G, B)
           CALL SET_RGB_VALUE (i+JM, R,G,B)
        ENDDO

C------------------- 'Hot-Iron' colours for contouring -----------------
      ELSEIF (IOP.EQ.13) THEN          != black->red->yellow->white
        jm = ibcont !- so after the 'material colours'
        DO I=1,ncol
          hue = real(I-1.) / real(ncol)           ! hue = 0.-->1.
          R = min(max(0.,3.*(hue+.03    )),1.)    ! note use of clipping
          G = min(max(0.,3.*(hue-.333333)),1.)   
          B = min(max(0.,3.*(hue-.666667)),1.)
          CALL SET_RGB_VALUE (i+JM, R,G,B)
        ENDDO

c----------------- 'spectrum' colours for contouring -------------------
c.. why not use HLS, and spin H?
      ELSEIF (IOP.EQ.14) THEN                                       
        jm = ibcont !- so after the 'material colours'
        DO I=1,NCOL
c         H = (I-1)/real(ncol-1)
c         L = .5
c         S = 1.
c         CALL SET_HLS_VALUE (I+jm, H,L,S)
          RED = REAL(I-1) /REAL(NCOL-1) * 2 * PI
          R = .5 + .5 * COS (RED)
          G = .5 + .5 * COS (RED + 2.* PI/3.)
          B = .5 + .5 * COS (RED - 2.* PI/3 )
          CALL SET_RGB_VALUE (i+JM, R,G,B)
        ENDDO

C------------------ 'greyscale' colours for contouring -----------------
      ELSEIF (IOP.EQ.15) THEN            !- gray
        jm = ibcont !- so after the 'material colours'
        from = .3
        to = 1.
        DO I=1,ncol
          val = from + (to-from) * real(i-1)/real(ncol-1)
          CALL SET_RGB_VALUE (i+JM, val,val,val)
        ENDDO

C-------------------------- 'random' colours ---------------------------
      ELSEIF (IOP.EQ.16) THEN   
        jm = ibcont !- so after the 'material colours'
        DO I=1,ncol
          r = get_random()
          g = get_random()
          b = get_random()
          CALL SET_RGB_VALUE (i+JM, r,g,b)
        ENDDO

C----------------------- print the current set -------------------------
      ELSEIF (IOP.EQ.17) THEN   
        DO I=1,NCOL
          WRITE(*,'(I4,3I4 )') I, (PAL(J,JM+I),J=1,3)
        ENDDO

c----------------------- monochrome -------------------------------
      ELSEIF (IOP.EQ.18) THEN               !- yuk !
       call set_rgb_value (0,  1.,1.,1.)
        DO I=1,15
          call set_rgb_value (0,  0.,0.,0.)
        ENDDO

C------------------ 'the GINO' colours for contouring -------------------
      ELSEIF (IOP.EQ.19) THEN
c       jm = 15     !- so after the 'material colours'?
        DO I=0,15
          DO J=1,3
            PAL(J,I) = PAL_GINO(J,I)
          ENDDO
        ENDDO

c------------------ 'linear' palettes ----------------------------------
c.. generalise to linear RGB / linear HLS and multi-lists (eg hot-iron)

      ELSEIF (IOP.EQ.21) THEN                   !--- PATRAN colours
        jm = ibcont !- so after the 'material colours'
c       CALL INTERP_RGB (0.,.6,0.,  1.,0.,0., 16, 16+ncol)
c   so what about colours 17,18,19,20 ?
        DO I=1,16  
          DO J=1,3
            PAL (J,I+JM) = PAL_PATRAN (J,I) * 4
          ENDDO
        ENDDO

      ELSEIF (IOP.EQ.22) THEN                   !--- blue--> yellow ---
        jm = ibcont !- so after the 'material colours'
        CALL INTERP_RGB (0.,0.,.8,  1.,1.,0., jm, jm+ncol)
      ELSEIF (IOP.EQ.23) THEN                   !-- blue->black->red --
c.. wont HLS interpolation be nicer?
        jm = ibcont !- so after the 'material colours'
c        CALL INTERP_RGB (.5,.8,1.,  0.1,0.1,0.1, jm, jm+ncol/2)
c        CALL INTERP_RGB (0.1,0.1,0.1,  1.,.7,.5, jm+ncol/2,jm+ncol)
        CALL INTERP_HLS (0.666,.75,1., 0.666,0.,0., jm, jm+ncol/2)
        CALL INTERP_HLS (0.   ,0.,0.,  0.   ,.75,1., jm+ncol/2,jm+ncol)

      ELSEIF (IOP.EQ.24) THEN                        ! User-defined
c.. lets scrap READ_EDITED_LINE !
c        LINE = '3 15 1.     0 0 .7    1 1 0 '
c        CALL READ_EDITED_LINE@ (LINE,0,1,33,IFAIL)   !- 'esc' to quit
c        IF (IFAIL.EQ.0) CALL SET_SHADE (LINE)     

      ELSEIF (IOP.EQ.25) THEN                        !- a grayscale 
        CALL INTERP_RGB (.1,.1,.1,  1.,1.,1., jm+1, jm+ncol)

      ELSEIF (IOP.EQ.26) THEN                        ! User-defined
        jm = ibcont !- so after the 'material colours'
c       .. interp in hls is probally nicer !
        print*,' Shade between which 2 colour numbers ?'
        READ*, IFROM,ITO
        CALL GET_RGB_VALUE (IFROM, R1,G1,B1)
        CALL GET_RGB_VALUE (Ito,   R2,G2,B2)
        CALL INTERP_RGB (R1,G1,B1, R2,G2,B2, jm,jm+ncol)

      ELSEIF (IOP.EQ.27) THEN      !- set contour palette for material shading
c       .. interp in hls is probally nicer !
c     .. can I adjust ncol here  (not necesserily the same as #contours)
c   .. or just put *after* contours?
c       ibcont2=ncol
        NMAT= nint((255-20-10) / real (Ncol))    !- skip base cols and menu cols.
        NMAT = min (nmat,19)     !- so dont overrun the contour col table
        jm = ibcont !+ibcont2 !- so after the 'mat. colours' (and the contours)
        do i=1,NMAT
           CALL GET_RGB_VALUE (I, R, G, B)
           CALL RGB_TO_HLS (R,G,B, H,L,S)
           CALL INTERP_HLS (H,0.2,S , H,0.8,S , 1+jm, nCOL+jm)
           jm = jm + ncol
        enddo
 

C------------------- palette 'animations' etc. -------------------------
C----------- step left -------------    (*obsolete*)
      ELSEIF (IOP.EQ.41) THEN
        DO J=1,3
          DO I=15,jm+1,-1 
            PAL(J,I) = PAL(J,I-1)
          ENDDO
            PAL(J,jm+1) = PAL(J,15)
        ENDDO

C------------------------------ step right -----------------------------
c.. hmm the material cols, or the contour cols?
      ELSEIF (IOP.EQ.42) THEN
        DO J=1,3
          DO I=jm+1,15-1 
            PAL(J,I) = PAL(J,I+1)
          ENDDO
            PAL(J,15) = PAL(J,2)
        ENDDO
      ELSE
        STOP '** UNKNOWN SET_COLOUR option !'

c-----------------------------------------------------------------------
      ENDIF     !-- end-of-all options

c      IF (NCOL.EQ.13) THEN   !-- shade down the other colors :-)
c        DO I=0,15            !-- if we do this then 256 colour
c          DO K=0,15          !-- modes will show the lighting-shading
c            I2 = K*16 + I
c            DO J=1,3   
c              PAL(J,I2) = PAL(J,I) * (16+5-K) / (16.+5.)
c            ENDDO
c          ENDDO
c        ENDDO
c      ENDIF  

c... only need to update any PALs that have changed
      CALL UPDATE_DACS ()      !- only if to VGA?
      END

C-----------------------------------------------------------------------
      SUBROUTINE UPDATE_DACS ()
C
C     Conditionaly update the VGA colour look-up tables
C     (only if VGA,SVGA or a PCX file)
C       DJK 25-5-95
c     8-2-98  SET_DACS now makes this decision
       IMPLICIT NONE
c      INTEGER IDEST
c      CALL GET_IDEST (IDEST)
c      IF (IDEST.LT.39) CALL SET_DACS (0,255)
       CALL SET_DACS (0,255)           !- SET_DACS now makes this decision
       
      END

C-----------------------------------------------------------------------
      SUBROUTINE RAINBOW (H1, S, V, R, G, B)
C
C     Taken from 'rainbow.c' from Netlib (Eric Grosse,1991)
C
c
c  /*  rainbow(double h, double s, double v, double *r, double *g, double *b)
c   This routine computes colors suitable for use in color level plots.
c   Typically s=v=1 and h varies from 0 (red) to 1 (blue) in
c   equally spaced steps.  (h=.5 gives green; 1<h<1.5 gives magenta.)
c   To convert for frame buffer, use   R = floor(255.999*pow(*r,1/gamma))  etc.
c   To get tables calibrated for other devices or to report complaints,
c   contact ehg@research.att.com.
c  */
c
c/*
c  The author of this software is Eric Grosse.  Copyright (c) 1986,1991 by AT&T.
c  Permission to use, copy, modify, and distribute this software for any
c  purpose without fee is hereby granted, provided that this entire notice
c  is included in all copies of any software which is or includes a copy
c  or modification of this software and in all copies of the supporting
c  documentation for such software.
c */

      IMPLICIT NONE
      INTEGER NTAB
      PARAMETER (NTAB = 60)
      REAL   H1, H,S,V,   R,G,B
      INTEGER I
      REAL HUETAB (0:NTAB)            !- table of equi-spaced HUEs

c hmm too much green - not enough yellow?
c        /*   computed from the FMC-1 color difference formula */
c        /* Barco monitor, max(r,g,b)=1, n=61 magenta,  2 Jan 1986 */
      DATA HUETAB /     .0000,  .0062,  .0130,  .0202,  .0280,  .0365, 
     &  .0457,  .0559,  .0671,  .0796,  .0936,  .1095,  .1275,  .1482, 
     &  .1806,  .2113,  .2393,  .2652,  .2892,  .3119,  .3333,  .3556, 
     &  .3815,  .4129,  .4526,  .5060,  .5296,  .5501,  .5679,  .5834, 
     &  .5970,  .6088,  .6191,  .6281,  .6361,  .6430,  .6490,  .6544, 
     &  .6590,  .6631,  .6667,  .6713,  .6763,  .6815,  .6873,  .6937, 
     &  .7009,  .7092,  .7190,  .7308,  .7452,  .7631,  .7856,  .8142, 
     &  .8621,  .9029,  .9344,  .9580,  .9755,  .9889, 1.0000 /


c---------------- Look up the Hue from the table -----------------------
c.. note H really is up to 1.5 (1.->1.5) is via magenta to complete the cycle
      H = H1/1.5 * NTAB                     !- real posn in the table
      I = INT(H)                            !- hence 'col-from'
      H = HUETAB(I) + (HUETAB(I+1) - HUETAB(I)) * (H-I)
      CALL DHSV2RGB (H, S, V, R, G, B)
      END

c-----------------------------------------------------------------------
      SUBROUTINE DHSV2RGB (h2, s, v, r, g, b)   
c
c     Turns a HSV colour into its RGB components
C         < cf my HLS_to_RGB from Foley, Van Damm >
c      Daughter of RAINBOW () by Eric Grosse
c
c    hexcone model...      all variables in range [0,1]
c    here, h=.667 gives blue, h=0 or 1 gives red. 
c    see Alvy Ray Smith, Color Gamut Transform Pairs, SIGGRAPH '78
c
      IMPLICIT NONE
      REAL H2,  H,S,V,  R,G,B,  M,N,K, F
      INTEGER I

      h = 6. * h2                      !- so 0.->6.
      i = int(h)                       !- base #
      f = h  - i                       !- fraction: 0.> f >1.
      m = 1. - s                         !
      n = 1. - s * f                     !  The 3 Coefficients
      k = 1. - (s * (1. - f))            !

      IF (I.EQ.0) THEN
        r = 1. 
        g = k 
        b = m 
      ELSEIF (I.EQ.1) THEN
        r = n 
        g = 1. 
        b = m 
      ELSEIF (I.EQ.2) THEN
        r = m 
        g = 1. 
        b = k 
      ELSEIF (I.EQ.3) THEN
        r = m 
        g = n 
        b = 1. 
      ELSEIF (I.EQ.4) THEN
        r = k 
        g = m 
        b = 1. 
      ELSEIF (I.EQ.5) THEN
        r = 1. 
        g = m 
        b = n 
      ELSE
        PRINT*,'** bad i: ', h, i 
        stop
      ENDIF

c-------------- Factor down the RGB values to 0.->1. -------------------
      F = V / MAX (R,G,B)  
      r = r * f
      g = g * f
      b = b * f
      END

C-----------------------------------------------------------------------
      SUBROUTINE RGB_TO_HLS (R,G,B, H,L,S)
C
C     Converts a colour defined by its hue, saturation and value
C       into its red,green,blue components
C     Algorithm from p.595 Foley & Van Dam (2nd ed.)
C
c     IMPLICIT NONE
      REAL H,L,S, R,G,B
      rgbmin = min(r,g,b)
      rgbmax = max(r,g,b)
      L = (rgbmin+rgbmax)/2.     ! lightness = median RGB

      IF (abs(RGBMIN-RGBMAX).lt. .001) THEN  !- R=G=B so no colour
        S = 0.
        H = .1666    !- yellow..actually hue can be anything 'cos its 'gray'
        RETURN
      ENDIF
      DELTA = RGBMAX-RGBMIN       !- colour range
      IF (L.LE.0.5) THEN              !- darker
        S = DELTA / (RGBMAX+RGBMIN)
      ELSE                            !- lighter
        S = DELTA / (2.-RGBMAX-RGBMIN)
      ENDIF
      IF (abs(R-RGBMAX).lt. .0001) THEN        !- between yellow and magenta
        H = (G-B) /DELTA
      ELSEIF (abs(G-RGBMAX).lt. .0001) THEN    !- betwwen cyan and yellow
        H = (B-R) /DELTA +2.
      ELSEIF (abs(B-RGBMAX).lt. .0001) THEN    !- between magenta and cyan
        H = (R-G) /DELTA +4.
      ENDIF                        
c     H = H*60.                    !- to degrees
c     IF (H.LT.0.) H = H + 360.      !- make sure 0 >= hue >= 360
      H = H / 6.
      IF (H.LT.0.) H = H + 1.        !- make sure 0 >= hue >= 1

      END

C-----------------------------------------------------------------------
      SUBROUTINE HLS_TO_RGB (H,L,S, R,G,B)
C
C     Converts a colour defined by its 
C     Hue [0,1], Saturation [0,1] and Value [0,1]
C     into its red,green,blue components [all 0,1]
C     Algorithm from p.596 Foley & Van Dam (2nd ed.)
C
      IMPLICIT NONE
      REAL H,L,S, R,G,B, V1,V2, RGB_VALUE
      EXTERNAL RGB_VALUE

      IF (L.LE.0.5) THEN     !- darker than pure hue
        V2 = L * (1+S)
      ELSE                   !- lighter than pure hue
        V2 = L + S - L*S
      ENDIF
      V1 = 2*L - V2
      IF (S.lt. .0001) THEN    !- if no saturation (=gray) do explicitly
        R = L
        G = L
        B = L
      ELSE
        R = RGB_VALUE (V1, V2, H+.3333)
        G = RGB_VALUE (V1, V2, H     )
        B = RGB_VALUE (V1, V2, H-.3333)
      ENDIF
      END

C-----------------------------------------------------------------------
      FUNCTION RGB_VALUE (v1,v2, HUE1)
c
c     Daughter function of HLS_TO_RGB
c
      IMPLICIT NONE
      REAL V1,V2,HUE,HUE1, RGB_VALUE

      HUE = HUE1 *360.             !- patch from a HUE of 0.-> 1.
      HUE = MOD (HUE+360.,360.)     !- so in range 0 -> 360.

      IF (HUE.LT. 60.) THEN
        RGB_VALUE = V1 + (V2-V1) * HUE / 60.
      ELSEIF (HUE.LT.180.) THEN
        RGB_VALUE = V2
      ELSEIF (HUE.LT.240.) THEN
        RGB_VALUE = V1 + (V2-1.) *(240.-HUE) / 60.
      ELSE
        RGB_VALUE = V1
      ENDIF
      END

c-----------------------------------------------------------------------
      SUBROUTINE INTERP_RGB (R1,G1,B1, R2,G2,B2, ICOL1, ICOL2)
C
C     This creates a smooth set of colours (eg. for contouring)
C     between 2 given colours by linear interpolation in RGB space
C      where RGB lie 0.->1.
C       Dan Kidger  24-1-95
C
      IMPLICIT NONE
      INTEGER ICOL1,ICOL2, I,NCOLS
      REAL    R,G,B, R1,G1,B1,  R2,G2,B2, FACT
      EXTERNAL SET_RGB_VALUE
      NCOLS = ICOL2-ICOL1
      DO I = 0,NCOLS
        FACT = REAL(I) / REAL (NCOLS)       !-    0. >= fact >= 1.
        R = R2 * FACT + R1 * (1.-FACT)
        G = G2 * FACT + G1 * (1.-FACT)
        B = B2 * FACT + B1 * (1.-FACT)
        CALL SET_RGB_VALUE (ICOL1+I, R,G,B)
      ENDDO
      END

c-----------------------------------------------------------------------
      SUBROUTINE INTERP_HLS (H1,L1,S1, H2,L2,S2, ICOL1, ICOL2)
C
C     This creates a smooth set of colours (eg. for contouring)
C     between 2 given colours by linear interpolation in HLS space
C       Dan Kidger  24-1-95
C
      implicit none
      REAL H,L,S, H1,L1,S1, H2,L2,S2 , FACT, R,G,B
      INTEGER I,ICOL,ICOL1,ICOL2,NCOLS
      NCOLS = ICOL2-ICOL1
      DO I = 0,NCOLS
        ICOL = I + icol1
        FACT = REAL(I) / REAL (NCOLS)       !-    0. >= fact >= 1.
        H = H2 * FACT + H1 * (1.-FACT)
        L = L2 * FACT + L1 * (1.-FACT)
        S = S2 * FACT + S1 * (1.-FACT)
        CALL HLS_TO_RGB   (H,L,S, R,G,B)    !- to an RGB value
        CALL SET_RGB_VALUE (ICOL, R,G,B)
      ENDDO
      END

c-----------------------------------------------------------------------
c      SUBROUTINE SET_SHADE (LINE)
C
C         * OBSOLETE *
C     Creates a colour map that varies linearly in RGB space
C     between 2 given colours 
C     Data is held in a CHARACTER string for compactness
C       ( gama correction not yet implimented )
C         Dan Kidger 1993
C
C     24-1-95  interpolation abstracted to INTERP_RGB
C
c      CHARACTER LINE*(*)
c
c      READ (LINE,*) ICol1,ICol2,GAMA, R1,G1,B1,  R2,G2,B2
c      END

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C      6: Miscilaneousy
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

C-----------------------------------------------------------------------
      SUBROUTINE DR_PHD_LOGO (RES, TITLE)
C
C     This recreates my PHD style titles by raising the base of the
C     picture by 10mm then creating a box under this and filling it 
C      via. DR_PRIM .. cf. my GINO.FOR method too.
C      --> hmm  10mm/25mm borders ?   A3/A4 format ?
C          DJK 17-8-94
C
      CHARACTER*(*) TITLE
      INTEGER RES(6)
      REAL X(5),Y(5)
      ctp = 72./25.4     !- centimetres to points
      H_box  = 10. * ctp
      H_text =  7. * ctp

      CALL MAKE_BOX (1.*res(4),1.*res(5), 1.*res(1),H_box,X,Y)
      CALL DR_PRIM  (TITLE,x,y,4,15,1)       !-- the box --

      x(1) = res(4) + res(1)/2.
      y(1) = res(5) + (H_box-H_text)   !/2.
      x(2) = H_text
      CALL DR_PRIM (TITLE,x,y,1,15,21)       !-- the title --

      res(2) = res(2) - nint(H_box)     !- drawing area reduces
      res(5) = res(5) + nint(H_box)     !- y-offset increases

      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE MAKE_BOX (Xo,Yo,XW,YW,X,Y)
C
C     simply stores xlo/xhi info as a polygon (so draw_a_box)
C     ... OR we could store in COORD() style ?
C       .. why do we subtract '1' ??
C
      REAL X(4),Y(4)
      X(1) = Xo
      X(2) = Xo + XW  !-1
      X(3) = Xo + XW  !-1
      X(4) = Xo

      Y(1) = Yo
      Y(2) = Yo
      Y(3) = Yo + YW  !-1
      Y(4) = Yo + YW  !-1
      RETURN
      END

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

