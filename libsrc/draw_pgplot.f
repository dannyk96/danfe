
C-----------------------------------------------------------------------
c    These are the Graphics routine that call the PGPLOT library
c    
c     Dan Kidger  7-6-00
C-----------------------------------------------------------------------

C-----------------------------------------------------------------------
      SUBROUTINE INTO_GRAPHICS ()
C
C     This opens an X-window
C     Then sets the desired palette  ( black,white,gray, colours..)
C     and then enables the mouse (be careful if there is no mouse ?)
C      .. just reset VGA and then set_all_dacs ??
C
C     ... * Trash this routine *, better to just call SELECT_SVGA_MODE
c      & set-overscan?, mouse bounds is done anyhow
c
c     LOGICAL LMOUSE
      INTEGER IRESX,IRESY

!     IRESx = 640                    !- store these !
      IRESx = 800                    !- store these !
      IRESy = 480
      NCOLS = 256          !.. cf. a 16-colour option

      if (PGOPEN("/XWIN").le.0) then
        print*,'failed to open PGPLOT''s X11 driver, exiting..'
        grstate=0
        return
      endif

c     IRESx = 800      
c     IRESy = 600
c     call vga@()
c     CALL SET_DEVICE (10, IRESX*1.,IRESY*1., ncols*1., 12.,9.,1.)
c     CALL SET_ALL_DACS ()     !- reset colours (call VGA@ mungs these)

C------------------ set the palette to be the video DACs ---------------
c-----------------------------------------------------------------------
c     RETURN    !- so skip all the mouse stuff
C----------------------- reset mouse -----------------------------------
c.. use generic_int_to int*2 ??
c      CALL INITIALISE_MOUSE@ ()        !
c      CALL MOUSE_SOFT_RESET@ (LMOUSE)  ! =.false. if mouse is not present
c      CALL SET_MOUSE_BOUNDS@ (0,0,IRESx-1,IRESy-1)
c      CALL DISPLAY_MOUSE_CURSOR@()     !
c      CALL SET_MOUSE_POSITION@ ( 121 + 10, 41+ 10 )
c-----------------------------------------------------------------------

      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE SELECT_SVGA_MODE (IXRES,IYRES,NCOLS, IFAIL)
C
C     Goes into a given SVGA graphics mode
C       and set the colours ?
C     * This sub-calls a store of all the mode info (as used by Danplot)
C
      IFAIL = 0                      !- assume all is OK
c     CALL USE_VESA_INTERFACE@ ()    !- VESA is nicer ?

c.. check that the VGA card can do this mode (configs card too)
c     CALL SET_DEVICE (10, IXRES*1.,IYRES*1., ncols*1., 35.,20.,1.)
c     CALL INIT_MOUSE (IXRES,IYRES)         !- set mouse limits etc.


c.... hmm I don't like the next, as it requires PAL (held in COMMON)
c (also cf a fade_up_from_black style init)
c     CALL SET_DACS (0,ncols-1)     !- reset colours (call VGA@ mungs these)
      END

C----------------------------------------------------------------------
c     PGPLOT  drawing routine  - eg. for X-windows
C----------------------------------------------------------------------
      SUBROUTINE DR_PGPLOT (TEXT,X,Y,N,ICOL,IOP)
C
C       IOP   1 = for closed edges
c             2 = for filled
c             3 = unclosed edge ,etc.
c            10 = filled circle (eg. nodes)
C            20..22 = text string (eg node numbers) -- normal font. 
c
C --> here we go into INT*2 for DBOS drawing routines

      COMMON /DEST/IDV, DV_xp,DV_yp,DV_ncol,DV_xl,DV_yl ,DV_ar
      COMMON /PALETTE/PAL
      INTEGER PAL(3,0:255)
      integer istr_end
      external istr_end

      SAVE           !(so XI_2, YI_2 becomae static for speed
      REAL X(*),Y(*)          !--- x,y,(z) coords of the line/polygon
      real*4 :: x4(N+1), y4(N+1),XI 
      CHARACTER TEXT*(*)
      INTEGER NP1    

c      integer  ICOL_2    !- hack

c---------- copy real coords to integer*2 x,y lists --------------------
c  *** invert ALL the y-values ???

      Iresy = dv_yp     !- ***just a hack for no .. beware SVGA
c     IF (IDV.eq.10) iresy = 600 !768    !- hack to max. resolution for now

! debug
!      write(*,'(a,i3,a,i3,2f14.3)') 
!     & 'draw_pgplot', iop,' :', n,x(1),y(1)

C------------------------- closed polygon draw -------------------------
      IF (IOP.EQ.1) THEN  
        CALL PGSCI (icol)
        x4(1:n)= x(1:n) ; y4(1:n)=y(1:n)
        np1=n+1;  x4(np1)=x(1); y4(np1)=y(1)
        CALL PGLINE (NP1,X4,Y4)
  

c-------------------------- filled polygon draw ------------------------
      ELSEIF (IOP.EQ.2) THEN 
        CALL PGSCI (icol)
        x4(1:n)= x(1:n) ; y4(1:n)=y(1:n)
        CALL PGPOLY (N,X4,Y4)

c--------------------------- polyline draw -----------------------------
      ELSEIF (IOP.EQ.3) THEN       
c        CALL POLYLINE@ (XI_2,YI_2, N_2, ICOL_2)
         CALL PGSCI (icol)
         x4(1:n)= x(1:n) ; y4(1:n)=y(1:n)
         CALL PGLINE (N,X4,Y4)

c-------------------------- filled circle draw -------------------------
c.. where nodes' radius are held in XP(2)
      ELSEIF (IOP.EQ.10) THEN   
        CALL PGSCI (icol)
        x4(1:n)= x(1:n) ; y4(1:n)=y(1:n)
        CALL PGCIRC (X4(1),Y4(1), x4(2))

c------------------------- Text string drawing -------------------------
c.. hmm font select ?  .. x(2) is supposed to be the point-size
c.. also centered text etc. 
c      .. get point size (as given) and adjust (but true string length?)
c      20 = LH, 21=centre,  22=RH justification !
      ELSEIF (IOP.ge.20.and.iop.le.29) THEN 
        x4(1:n)= x(1:n) ; y4(1:n)=y(1:n)
        ixoff=0
        if (iop.eq.21) ixoff = istr_end(text)*X(2)/2     ! centre  25-3-98
        if (iop.eq.22) ixoff = istr_end(text)*X(2)       ! RH just
        XI=x(1)-ixoff           ! set text anchor point
c       CALL DRAW_TEXT@ (TEXT,XI_2(1),YI_2(1),ICOL_2)
        CALL PGSCI (icol)
        CALL PGTEXT (XI,Y4(1),TEXT)

      ELSEIF (IOP.EQ.40) THEN   !/* clip-to-polygon */
c cf  call PGQCLP(STATE) to query the state.
c  next sets clippign to teh current viewport - this may well not be what we want
       call pgsclp(1)
      ELSEIF (IOP.EQ.41) THEN   !/* un-clip (back to full image) */
       call pgsclp(0)
c---------------------------- unknown option ---------------------------
      ELSE
        PRINT*,'IOP=',IOP,' unknown in DR_PGPLOT'
        STOP
      ENDIF
!-- note pgplot sub-functions
!      SUBROUTINE VIDRIV (IFUNC, RBUF, NBUF, CHR, LCHR)
!      INTEGER IFUNC, NBUF, LCHR
!      REAL    RBUF(*)
!      CHARACTER*(*) CHR

C--- IFUNC = 1, Return device name -------------------------------------
C--- IFUNC = 2, Return physical min and max for plot device, and range
C--- IFUNC = 3, Return device resolution -------------------------------
C--- IFUNC = 4, Return misc device info --------------------------------
!  40 CHR = 'HNNANNNNNN'
C--- IFUNC = 5, Return default file name -------------------------------
C--- IFUNC = 6, Return default physical size of plot -------------------
C--- IFUNC = 7, Return misc defaults -----------------------------------
!  70 RBUF(1) = 8.0
C--- IFUNC = 8, Select plot --------------------------------------------
C--- IFUNC = 9, Open workstation ---------------------------------------
!     RBUF(1) = UNIT
!     RBUF(2) = 1
C--- IFUNC=10, Close workstation ---------------------------------------
C--- IFUNC=11, Begin picture -------------------------------------------
C--- IFUNC=12, Draw line -----------------------------------------------
C--- IFUNC=13, Draw dot ------------------------------------------------
C--- IFUNC=14, End picture ---------------------------------------------
C--- IFUNC=15, Select color index --------------------------------------
C--- IFUNC=16, Flush buffer. -------------------------------------------
C--- IFUNC=17, Read cursor. --------------------------------------------
C--- IFUNC=18, Erase alpha screen. -------------------------------------
C--- IFUNC=19, Set line style. -----------------------------------------
C--- IFUNC=20, Polygon fill. -------------------------------------------
C--- IFUNC=21, Set color representation. -------------------------------
C--- IFUNC=22, Set line width. -----------------------------------------
C--- IFUNC=23, Escape --------------------------------------------------

      END

C----------------------------------------------------------------------
      SUBROUTINE SET_ALL_DACS()
C
C     Writes the whole current palette to the VGA Hardware
C       Dan Kidger 1993
C
      CALL SET_DACS (0,255)      ! 8-2-98
      END

C----------------------------------------------------------------------
      SUBROUTINE SET_DACS (ifr,ito)
C
C     Writes a subset of the current palette to the VGA Hardware
C   ? if  VGA only set 16 cols
C     if SVGA set 256 cols
C     if to HP-GL / Postscript  do nothing (or messes up the screen?)
c     so test 
C       Dan Kidger 1993
c     should see what the current output device is - if not VGA do 
c     nothing
C
      COMMON /PALETTE/PAL
      INTEGER PAL(3,0:255)
      real*4 :: r,g,b

C--- patch to avoid setting dacs unless we are writing to screen ---
c     CALL GET_IDEST(IDEST)
c     print*,'idest=',idest
c     IF (IDEST.LE.0) RETURN      ! -1=off, 0=dummydriver
c     IF (IDEST.GE.20) RETURN     ! 20->29=PCX, but I manualy mung the 
c                                   palette, after writing the file
c     IF (IDEST.GE.30) RETURN     ! 30+ are printers (+WMF+DXF)

      DO I=max(ifr,0),min(ito,255)    !- clip just in case!
        r=pal(1,i)/255.1
        g=pal(2,i)/255.1
        b=pal(3,i)/255.1
        if (max(r,g,b).gt.1. .or. min(r,g,b).lt.0.) then
         print*, i,r,g,b
c         stop
        endif
c       CALL SET_VIDEO_DAC_BLOCK@(ifr,ito-ifr+1, PAL1(1,ifr) )
      
c        print*, i,r,g,b
        call PGSCR (i,r,g,b)
      ENDDO
 
      END

C-----------------------------------------------------------------------
C     Mouse Handling Routines
C-----------------------------------------------------------------------

C-----------------------------------------------------------------------
      SUBROUTINE INIT_MOUSE (ixres,iyres)
C
C     Initialises the mouse using Salford Routines
C     .. cf: my mouse driver v. Salford (DOS's) arrow cursor (not SVGA)
C     .. cf the mask (32768) for XOR drawing a mouse cursor
C
c     CALL INITIALISE_MOUSE@()                    !- do here ?
c     CALL SET_MOUSE_BOUNDS@ (3,3, ixres-3-10, iyres-3 )
c     CALL SET_MOUSE_MOVEMENT_RATIO@( 3*8,1*8)    !- aniso hmm. why?
c     CALL SET_MOUSE_POSITION@ (ixres/3,iyres/3)  !- start pos
      END

C-----------------------------------------------------------------------
c-----------------------------------------------------------------------
c     Routines for handling the Mouse cursor / cursor-keys
c     +  Mouse interupt subroutines (from Salford Manual Example)
c-----------------------------------------------------------------------

c-----------------------------------------------------------------------
      SUBROUTINE GIN (X,Y,itype, x1,y1,x2,y2, x3,y3)
c
c     Simply draws the 'current' mouse cursor at the INTEGER screen posn, X,Y
c
c     Type 1  = full cross-hairs (default)
c          2  = 'window' clipped cross-hairs
c          3  = small cross
c          4  = a 'fat' cross ?
c          5  = a standard mouse arrow (as a set of lines)
c         10  = a rubber-band box (so store the 'anchor-point') eg. zoom
c         11  = a resizable circle .. for fill ?
c         20  = a dragged box frame (need xyXY of orig. & ëxy from 'anchor'
c                (eg. to move an element like the contour legend )
c
c     x1,y1,x2,y2 = the screen/window limits (eg 0,0,639.439)
c          ,x3,y3 = 'anchor-point' for zoom-box / ellipses etc.
c    < Maybe these are better put in an array ?)
c
      IMPLICIT NONE
      INTEGER X,Y, IX,IY, ITYPE, ITH,I,ILN
     &       ,X1,Y1, X2,Y2, X3,Y3   !- clip-limits & anchor-point
      INTEGER*4 NADD,ICOL
      EXTERNAL DRAW_LINE           !- necessary or just tidier ?

c      IX = (INTL(XMAX)*INTL(iX1))/640      !-  ix1,iy1 based on a
c      IY = (INTL(YMAX)*INTL(iY1))/400      !-  640 by 400 screen ?
      IX = X                     !( why to IX,IY ?)
      IY = Y

c... hmm the value to add for XOR, I think is f(#bitplanes)
      nadd = 32768                !- 2^16 for flag XOR (or 2*n_col_res
      icol = 15 + nadd            !- cursor colour   .. NADD causes XOR?

c................ type 1 = full screen cross-hairs .................
c... needs the xyXY of the whole screen ... hack to 800x600?
      if (itype.eq.1) then       !- = SELECT CASE really
c        Xmax = x2 
c        Ymax = y2 
c       CALL DRAW_LINE@ (IX,y1 ,IX,y2  ,icol)
c       CALL DRAW_LINE@ (x1,IY ,x2,IY  ,icol)

c................ type 2 = window clipped cross-hairs ...............
c.. same as type 1 for now .. we will need the xyXY of the current window
c.. let the driver routine pass with either the screen or window bounds
c   ( maybe both & GIN() decides which to use ? )

      ELSEIF (ITYPE.EQ.2) THEN      
c       CALL DRAW_LINE@ (IX,x1 ,IX,y2  ,icol)
c       CALL DRAW_LINE@ (x1,IY ,x2,IY  ,icol)

c.................. type 3 = small cross-hairs ....................
      elseif (itype.eq.3) then
        ith = 20
c       CALL DRAW_LINE@ (IX     ,iy-ith,IX    ,IY+ith,icol)
c       CALL DRAW_LINE@ (ix-ith ,IY    ,IX+ith,IY    ,icol)

c.................. type 4 = small fat cross  .....................
      elseif (itype.eq.4) then
        do i=-1,1       !- so 3 lines thick
          ith = 10
c         CALL DRAW_LINE@ (IX+i   ,iy-ith,IX+i  ,IY+ith,icol)
c         CALL DRAW_LINE@ (ix-ith ,IY+i  ,IX+ith,IY+i  ,icol)
        enddo

c.................. type 5 = arrow  .....................
      elseif (itype.eq.5) then
        ith = 10      !- head size   *Adjustable*
        iln = 2*ith   !- tail length
c       CALL DRAW_LINE@ (IX  ,IY  ,IX+ith  ,IY+  0,icol)   !- 1st x
c       CALL DRAW_LINE@ (IX+1,IY+1,IX+ith  ,IY+  0,icol)   !- 2nd x
c       CALL DRAW_LINE@ (IX  ,IY+1,IX+  0  ,IY+ith,icol)   !-  1st y
c       CALL DRAW_LINE@ (IX+1,IY+2,IX+  0  ,IY+ith,icol)   !-  2nd y
c       CALL DRAW_LINE@ (IX+2,IY+2,IX+iln  ,IY+iln,icol)   !-  1st tail
c       CALL DRAW_LINE@ (IX+2,IY+3,IX+iln  ,IY+iln,icol)   !-  2nd tail

c.................. type 6 = Zoom box #1 .....................
c.. try just the top & LH lines (then do base & RH below ?)
      elseif (itype.eq.6) then
c       CALL DRAW_LINE@ (X3 ,Y3   ,x3 ,Y2  ,icol)   !- TOP
c       CALL DRAW_LINE@ (X3 ,Y3+1 ,x2 ,Y3  ,icol)   !- LH

c.................. type 7 = Zoom box #2 .....................
c.. The full box :-)    .. cf POLYLINE@()
      elseif (itype.eq.7) then
c       CALL DRAW_LINE@ (X3  ,Y3   ,IX   ,Y3   ,icol)   !- TOP
c       CALL DRAW_LINE@ (X3  ,Y3+1 ,X3   ,IY   ,icol)   !- LH
c       CALL DRAW_LINE@ (IX  ,IY   ,X3+1 ,IY   ,icol)   !- Bottom
c       CALL DRAW_LINE@ (IX  ,IY-1 ,IX   ,Y3+1 ,icol)   !- RH

c...................................................................
      ELSE      !- error if unknown ?
      ENDIF
      RETURN
      END                 

c-----------------------------------------------------------------------
c     Routines for handling the Mouse cursor / cursor-keys
c     +  Mouse interupt subroutines (from Salford Manual Example)
c-----------------------------------------------------------------------
c-----------------------------------------------------------------------
      SUBROUTINE GET_MOUSE_OR_KEY_EVENT (IXM,IYM, codes, Q_FUNC)
C
C     ( cf CURSOR_GINO)
C     Lets the user move the cross-hairs around with the mouse
C     returns the screen-coord of the hit
C     (and which mouse button was pressed)
C     If the keyboard is pressed, the scancode /ascii value is returned
C      .. in codes(1)
c      .. a mouse hit returns codes(1) = opcode and codes(2..5) as data
c         from the POST_MENUS subcall
C         Dan Kidger 6-2-95
C

c.. nice to define the cursor-shape, or even let it change in different
C   areas of the screen (even 'flash; buttons as we pass over)

      INTEGER KEY
      CHARACTER CKEY*1
      REAL*4 ::  X,Y
      integer codes(10)     !- return menu box coords
      integer ipass
c--------------- Interface to Salford's Interupt handler ---------------
c     EXTERNAL M_TRAP,B_TRAP        !- mouse trap/ <cntl-break> trap
      COMPLEX*16 LAB                !- jump-to label if mouse-press
c     INTEGER*4 Q                   !- (dummy store of old trap)
c     INTEGER*2      KEY_2          !- key-code   
c    &          ,MX_2,MY_2          !- mouse position
c    &          ,ic_2               !- button press count

      EXTERNAL Q_FUNC
      integer PGCURS                ! returns cursor position
      COMMON /mouse_status/ LAB,MX_2,MY_2    !- talks whit interupt sub.
      COMMON /C2/ X,Y,XMAX,YMAX            !- +screen size ?
      logical zoom, over_button
c     integer x1,y1,x2,y2,x3,y3
c     data x1,y1,x2,y2,x3,y3/ 0,0,639,439,  100,100/
      data ipass/0/
      
      data igin/5/      !- default cursor .. cf F10 to change on the fly
      igin_old = -1    !- 'last' cusror shape
c     igin = 3    !- 1=full cross-hairs,4=fat cross
c     igin = 5    !- 5 = arrow cursor

      ipass=ipass+1
      if (ipass.eq.1) then
        x=100
        y=100
      endif 
      zoom = .false.         !- not yet zooming   (or 3 states: off, 1st,2nd?)
      over_button = .false.  !- not yet 'flashed' a button
      do i=1,10          !- null the menu codes for good measure
        codes(i)= 0
      enddo
      
!---- wait until a mouse (or keyboard) event occurs ---
!-- should also test for a key press (ifc's peekcharqq())

      ifail=PGCURS (x,y,ckey)

      ixm = nint(x)
      iym = nint(y)
c     iym = 600-nint(y)
      key=ichar(ckey)
c     print*,' you pressed "',ckey,'" (ascii=',key,') at x,y=',x,y
c     CALL GIN (X,Y,igin, x1,y1,x2,y2,x3,y3)   !- show the cross-hairs

c.......... wait until something happens .......
c1     continue                      !- (a F90 endless DO..ENDDO)

        IF (CKEY.eq.'A') THEN   !- got LH mouse
          GOTO 22
        ELSEIF (CKEY.eq.'Z') THEN   !- got RH mouse (cf. zoom box)
          GOTO 22
        ELSEIF (KEY.ne.0) THEN                !- keyboard was pressed -!
          codes(1) = -key      !- so return the (-ve) opcode (mung outside)
          RETURN
        ENDIF

!------------------ Query and hi-light any menus/polygons ------------
c-- eg a list of selectable object such as menu buttons
c  (as a pair of diagonals) .. nicer to shade-in ?
        call Q_FUNC (4,x,y, codes)
c       call post_menus (4,x,y, codes)
        if (codes(1).gt.0) then    !- if a 'hit'
          if (.not.over_button) then
            over_button = .true.
            ICOL = 32768 + 14
            ICOL = 32768 + 255
c         CALL DRAW_LINE@( CODES(5), CODES(6), CODES(7), CODES(6), ICOL)
c         CALL DRAW_LINE@( CODES(7), CODES(6), CODES(7), CODES(8), ICOL)
c         CALL DRAW_LINE@( CODES(7), CODES(8), CODES(5), CODES(8), ICOL)
c         CALL DRAW_LINE@( CODES(5), CODES(8), CODES(5), CODES(6), ICOL)
          endif
        endif
c       CALL GIN (X,Y,igin, x1,y1,x2,y2,x3,y3)       !-- draw new cursor
c     GOTO 1      !-- re-cycle

c--------------- ON LH button, return coords (and codes too?) ----------
  22  continue
c     test if the mouse was over a menu button
c     if so return in codes(1) a posiitive number
      call Q_FUNC (4,ixm,iym, codes)
      RETURN

c--------------- ON RH button, initiate zoom box ? ---------------------
  33  continue
      call Q_FUNC (4,x,y, codes)          !- get opcodes
      if (.not.zoom) then  !- first corner
        zoom = .true.
c       x3 = x             !-  save the mouse position
c       y3 = y
        igin_old = igin
        igin = 7                             ! now do the full zomm-box GIN
c       CALL GIN (X,Y,igin, x1,y1,x2,y2,x3,y3)   !- show the new cursor-box
c       goto 1             !- back into the loop
      else                 !- second corner

c... OK lets not bother .. just return a box
C     so draw, 
c       KEY = -110                  !- and flag event = -110 = Zoom box
        codes (1) = -ichar ('B')     !- a view type operation
c       codes (2) = 101             !- 101 = 'zoom into this box'
c       codes (3) = min (x,x3)
c       codes (4) = min (y,y3)      !- return the xyXY of the zoom box
c       codes (5) = max (x,x3)
c       codes (6) = max (y,y3)
        if (igin_old.ge.0) igin = igin_old    !- restore the cursor
        if (igin_old.ge.0) igin = igin_old    !- restore the cursor
        RETURN
      endif
      END

c-----------------------------------------------------------------------
      subroutine buffer_graphics(iop)
c
c     controls buffering of graphical output 
c
      select case(iop)
      case (1); call PGBBUF ()      !- enter buffered state
      case (0); call PGEBUF ()      !- exit buffered state
      case (2); call PGUPDT ()      !- make_picture_current
      end select        
      end

c-----------------------------------------------------------------------
c-----------------------------------------------------------------------

