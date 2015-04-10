
C-----------------------------------------------------------------------
c 8-11-95  need to add drivers for WINDOWS and CLEARWIN+

C-----------------------------------------------------------------------
c    These are the Graphics routine that call the Salford DBOS library
c    directly (so *dont* call from UN*X or MS-Windows, etc.
c
C-----------------------------------------------------------------------

C-----------------------------------------------------------------------
      SUBROUTINE INTO_GRAPHICS ()
C
C     This goes into standard VGA mode 
C     Then sets the desired palette  ( black,white,gray, colours..)
C     and then enables the mouse (be careful if there is no mouse ?)
C      .. just reset VGA and then set_all_dacs ??
C
C     ... * Trash this routine *, better to just call SELECT_SVGA_MODE
c      & set-overscan?, mouse bounds is done anyhow
c
c     LOGICAL LMOUSE
      INTEGER IRESX,IRESY, IFAIL, ipos,icol

      IRESx = 640                    !- store these !
      IRESy = 480
      NCOLS = 256          !.. cf. a 16-colour option

c      IRESx = 800      
c      IRESy = 600
c     call vga@()
      call SELECT_SVGA_MODE (IRESX,IRESY,NCOLS, IFAIL)
C     CALL GRAPHICS_MODE_SET@( iresx, iresy, NCOLS, IFAIL_2)
c     CALL GRAPHICS_MODE_SET@( 640, 480, 32768L, IFAIL_2)  !- even this!
c     CALL SET_DEVICE (10, IRESX*1.,IRESY*1., ncols*1., 12.,9.,1.)
c     CALL SET_ALL_DACS ()     !- reset colours (call VGA@ mungs these)

C------------------ set the palette to be the video DACs ---------------
c.. maybe as a sub-routine as SVGA needs this too ?
C      DO I=1,15      
C        IPOS_S = I
C        ICOL_S = I
C        CALL SET_PALETTE@ (IPOS_S,ICOL_S)
c     ENDDO
      IPOS = 17
      ICOL = 2
      CALL SET_PALETTE@ (IPOS,ICOL)       !- overscan
c-----------------------------------------------------------------------
c     RETURN    !- so skip all the mouse stuff
C----------------------- reset mouse -----------------------------------
c.. use generic_int_to int*2 ??
c      CALL INITIALISE_MOUSE@ ()        !
c      CALL MOUSE_SOFT_RESET@ (LMOUSE)  ! =.false. if mouse is not present
       CALL SET_MOUSE_BOUNDS@ (0,0,IRESx-1,IRESy-1)
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
      INTEGER*2 IFAIL_2
      IFAIL = 0                      !- assume all is OK
c     CALL USE_VESA_INTERFACE@ ()    !- VESA is nicer ?

c.. check that the VGA card can do this mode (configs card too)
      CALL VGASIZE_TO_MODE (IXRES,IYRES,NCOLS, IMODE, 1)
      IF (IMODE.LE.0) THEN
        STOP                   !- failed to set mode
      ENDIF

      CALL GRAPHICS_MODE_SET@ (IXRES,IYRES,NCOLS, IFAIL_2)
      IF (IFAIL_2.NE.0) CALL MYERROR (3,
     &  'Failed to set VGA mode (but DBOS said I could)' )

      DO I=1,15
        CALL SET_PALETTE@(I,I)              !- so indeces are alligned
      ENDDO
      CALL SET_DEVICE (10, IXRES*1.,IYRES*1., ncols*1., 35.,20.,1.)

      CALL INIT_MOUSE (IXRES,IYRES)         !- set mouse limits etc.


c.... hmm I don't like the next, as it requires PAL (held in COMMON)
c (also cf a fade_up_from_black style init)
      CALL SET_DACS (0,ncols-1)     !- reset colours (call VGA@ mungs these)
      END

C-----------------------------------------------------------------------
      SUBROUTINE VGASIZE_TO_MODE (IXRES,IYRES,NCOLS, IMODE, IOP)
C
C     Simply return the mode # (IMODE) for the given SVGA size (IXRES,..)
C     returns -1 of this mode is not found
C     option to print-out available modes:
C     0 = just report available modes
C     1 = (standard) hard-fail if error (so report available modes)
C     2 = soft-error (just return IMODE = -1)
C     (cf an IOP to find the IXRES,.. for the given IMODE)
C
      INTEGER*2 IGX(40), IGY(40)                     !- x,y resolutin
     &        , IGM(40) !, ifail_2                   !- BIOS mode #
      INTEGER*4 IGC(40)                              !- ncols
      LOGICAL*2 IGB(40)                              !- if banked

c---------------- form the look-up table -----------------
      CALL GET_GRAPHICS_MODES@ ( IGX,IGY,IGC,IGM,IGB ) 
      DO I=1,40
        IF (IGX(I).EQ.-1) GOTO 1
      ENDDO
    1 NMODES= I-1

      IF (IOP.EQ.1.OR.IOP.EQ.2) THEN
c--------------- search for the mode --------------
        DO I=1,NMODES
          IF (IXRES.eq.IGX(I) .and.
     &        IYRES.eq.IGY(I) .and.
     &        NCOLS.eq.IGC(I) ) THEN    !- a hit so return IMODE
            IMODE = I
            RETURN
          ENDIF
        ENDDO
        IF (IOP.eq.1) THEN        !--- hard fail
          PRINT*, '*** Given Graphics mode is unavailable ***'
        ELSEIF (IOP.eq.2) THEN    !--- soft-fail
          IMODE = -1
          RETURN
        ENDIF
      ENDIF        !- only if searching

c---------- write the table of available modes -------
      PRINT*,'Available graphics modes are:'
      DO I=1,NMODES
        WRITE (*,'(I4,a,I4,a,i4, i3,a,  a,i3)')
     &   I,' : ',IGX(I),' by ',IGY(I)
     &  ,nint (log(real(igc(i)))/ log(2.) ),' bits'
     &  ,'      Mode =',IGM(i)
      ENDDO
      IF (IOP.eq.1) STOP       !--- hard fail
      END

C-----------------------------------------------------------------------
      SUBROUTINE GET_GR_MODE (IOP,RES,IRESC,AR)
C
C     This simply sets a SVGA mode (given my its number in IOP)
C     and returns the screen size in RES,IRESC,AR (as used by Danplot)
C     Note:
C         This re-calls GET_GRAPHICS_MODES@() to find the mode that this 
C         number represents
C
C     cf. a routine that given a BIOS SVGA mode # finds it's x,y, ncols ...
C
C     cf. a routine that given the mode (x,y,ncols) returns whether it 'exists'
C    so call 'check_out_this_mode' before we try and set it.
C    if not valid, we can exit with an error message
C    (note side effect of configuring the graphics card for us.)
C
C    if error (or a 'null' code) we can pretty_print the available modes)
C    .. via a daughter-rotuine surely
C
C          DJK 1994
C

      INTEGER RES(6)
      INTEGER*2 IGX(40), IGY(40)                     !- x,y resolutin
     &        , IGM(40), ifail_2                     !- BIOS mode #
      INTEGER*4 IGC(40)                              !- ncols
      LOGICAL*2 IGB(40)                              !- if banked
      integer ifail
      ifail=0 ! assume all is OK ?
      PRINT*,'IOP=',IOP
      IF (IOP.LE.0) STOP ' SVGA mode # =< 0 !'
      CALL GET_GRAPHICS_MODES@ ( IGX,IGY,IGC,IGM,IGB ) 
      DO I=1,40
        IF (IGX(I).EQ.-1) GOTO 1
      ENDDO
    1 NMODES= I-1
      IMODE = MIN (IOP,NMODES)   !- ie '99' will always give the max mode

        RES(1) = IGX(imode)
        RES(2) = IGY(imode)
        RES(4) = 0
        RES(5) = 0
        IRESC = IGC(IMODE)
        AR = -1. 

c    ..? USE_VESA_INTERFACE .. is probally nice
c       CALL SET_VGA_MODE    (IGX(imode),IGY(imode),IGC(imode), IFAIL)
C        CALL GRAPHICS_MODE_SET@ (IGX(imode),IGY(imode)
C     &                          ,IGC(imode), IFAIL_2)
        CALL SELECT_SVGA_MODE  (1*IGX(imode),1*IGY(imode)
     &                          ,1*IGC(imode), IFAIL)
        ifail_2=ifail
        CALL DOSERR@ (IFAIL_2)     !- check that all is OK ?
      END

C----------------------------------------------------------------------
C     Primitve drawing routines - 1: HP-GL plotters
C----------------------------------------------------------------------

C----------------------------------------------------------------------
      SUBROUTINE DR_HPGL  (TEXT,X,Y,N,ICOL,IOP)
C
c     This uses the Salford Driver. However there is no reason why
c     I cannot do it all myself using write() statments.
c        DJK 22-1-95
C
c     COMMON /DEST/IDEST        !- eg. 5=postscript?
      COMMON /DEST/IDV, DV_xp,DV_yp,DV_nc,DV_xl,DV_yl ,DV_ar

      SAVE                      !- so XI_2, YI_2 becomae static for speed
      REAL X(N),Y(N)            !- x,y,(z) coords of the line/polygon
      CHARACTER TEXT*(*)
      INTEGER*2 XI_2(500), YI_2(500), N_2,  NP1_2     !- For Salford DBOS
      integer*4 icol_2
c      ,IPOLY,IFAIL     !- sub-routine calls        

c---------- copy real coords to integer*2 x,y lists --------------------
c.. probably best to work in mm and so scale *40 here.
      ICOL_2 = ICOL   !- Salford devices will need INT*2
      FACT = 10.      !- mm to device units
      N_2  = N
      DO I=1,N      
        XI_2 (I) = min(max(NINT (X(I)  * fact),0),32767)   !- and clip?
        YI_2 (I) = min(max(NINT (Y(I)  * fact),0),32767)
      ENDDO

C------------------------- closed polline draw -------------------------
      IF (IOP.EQ.1) THEN  
          NP1_2 = N_2 + 1                !- close the polygon
          XI_2 (NP1_2) = XI_2 (1)        !- with a copy of
          YI_2 (NP1_2) = YI_2 (1)        !- the first point
          CALL POLYLINE@       (XI_2,YI_2, NP1_2, ICOL_2)

c-------------------------- filled polygon draw ------------------------
c... this has been zapped for HPGL files 
c   (because Salford draws them as a family of parallel lines
c   ..  maybe substitute an edge draw or HP-GL/2's area fill
      ELSEIF (IOP.EQ.2 ) THEN 

c--------------------------- polyline draw -----------------------------
      ELSEIF (IOP.EQ.3) THEN       
        CALL POLYLINE@ (XI_2,YI_2, N_2, ICOL_2)

c-------------------------- filled circle draw -------------------------
c.. where nodes' radius are held in XP(2)
c.. hacked to do hollow circles for plotters :
      ELSEIF (IOP.EQ.10) THEN   
        CALL ELLIPSE@ (XI_2(1),YI_2(1),XI_2(2),XI_2(2),ICOL_2)

c------------------------- Text string drawing -------------------------
c.. hmm font select ?  .. x(2) is supposed to be the point-size
      ELSEIF (IOP.ge.20.and.iop.le.29) THEN 
        CALL DRAW_TEXT@ (TEXT,XI_2(1),YI_2(1),ICOL_2)

c---------------------------- unknown option ---------------------------
      ELSE
        PRINT*,'** primitive type ',IOP,' unknown in DR_HPGL'
      ENDIF
      END

C----------------------------------------------------------------------
c     VGA drawing routine (cf DR_SALFORD)
C----------------------------------------------------------------------
      SUBROUTINE DR_VGA (TEXT,X,Y,N,ICOL,IOP)
C
C      For VGA, SVGA and Screen_Blocks
C      Inverts the Y-axis so origin becomes the top-left for drawing
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
      REAL X(*),Y(*)        !--- x,y,(z) coords of the line/polygon
      CHARACTER TEXT*(*)
c     CHARACTER TEXT*80
      INTEGER*2 XI_2(500), YI_2(500), N_2  !- For Salford DBOS
     &  ,IYMIN,IYMAX
     &  ,NP1_2, IPOLY,IFAIL     !- sub-routine calls        
c    &   ,ICOL_2
       integer*4  ICOL_2    !- hack

c---------- copy real coords to integer*2 x,y lists --------------------
c  *** invert ALL the y-values ???

      Iresy = dv_yp     !- ***just a hack for no .. beware SVGA
c     IF (IDV.eq.10) iresy = 600 !768    !- hack to max. resolution for now

      ICOL_2 = ICOL   !- Salford devices will need INT*2
      N_2  = N
      DO I=1,N      
        XI_2 (I) =          NINT (X(I))  
        YI_2 (I) =iresy-1 - NINT (Y(I))
      ENDDO

      IF (IDV.EQ.12 ) THEN   !- 'hicolor' needs explicit RGB value
        IR = PAL(1,ICOL) /8
        IG = PAL(2,ICOL) /8         !- this is the function for 15bit colour
        IB = PAL(3,ICOL) /8
        ICOL_2= IR*64*32 + IG*64 + IB
      ENDIF

c--------------------------- polyline draw -----------------------------
      IF (IOP.EQ.3) THEN       
         CALL POLYLINE@ (XI_2,YI_2, N_2, ICOL_2)

C------------------------- closed polygon draw -------------------------
c... should do this here too
      ELSEIF (IOP.EQ.1) THEN  
          NP1_2 = N_2 + 1                !- close the polygon
          XI_2 (NP1_2) = XI_2 (1)        !- with a copy of
          YI_2 (NP1_2) = YI_2 (1)        !- the first point
          CALL POLYLINE@       (XI_2,YI_2, NP1_2, ICOL_2)

c-------------------------- filled polygon draw ------------------------
C.. To cure a BUG in Salford's  FILL_POLYGON@() routine that causes
C.. some polygons to have a horizontal 'tail' (going the 'leading' diagonal)
C... this algorithm will find the total height of the polygon 
C... and skip-out if has zero-height  :   Dan Kidger 14-07-92
C        (cf my Postscript driver which works perfectly !)
c... possibly I only need this for SVGA and VSCREEN (eg. PCX) ?
c... Note: at one time I had the code collapsing pairs of nodes
c... that had the same y-value

      ELSEIF (IOP.EQ.2 ) THEN 

C--------- patch to remove 'zero-height' polygons 14-07-92 -------------
c.. I thought the error was just co-incident nodes ??
        ICOLS = ICOL
        IYMIN = YI_2(1)                ! find the 
        IYMAX = YI_2(1)                ! max and min y-values
        DO I=2,N                       ! of this
          IYMIN = MIN(IYMIN,YI_2(I))   ! polygon
          IYMAX = MAX(IYMAX,YI_2(I))   !
        ENDDO
        IF (IYMIN.EQ.IYMAX) RETURN  !- abort if zero-height
        CALL CREATE_POLYGON@ (XI_2,YI_2,N_2,IPOLY,IFAIL) ! create
        CALL DOSERR@                            (IFAIL)  ! the 
        CALL FILL_POLYGON@         (IPOLY,ICOL_2,IFAIL)  ! polygon
        CALL DOSERR@                            (IFAIL)  ! then fill
        CALL DELETE_POLYGON_DEFINITION@  (IPOLY, IFAIL)  ! and 
        CALL DOSERR@                            (IFAIL)  ! reset

c-------------------------- filled circle draw -------------------------
c.. where nodes' radius are held in XP(2)
      ELSEIF (IOP.EQ.10) THEN   
        CALL FILL_ELLIPSE@ (XI_2(1),YI_2(1),XI_2(2),XI_2(2),ICOL_2)

c------------------------- Text string drawing -------------------------
c.. hmm font select ?  .. x(2) is supposed to be the point-size
c.. also centered text etc. 
c      .. get point size (as given) and adjust (but true string length?)
c      20 = LH, 21=centre,  22=RH justification !
      ELSEIF (IOP.ge.20.and.iop.le.29) THEN 
        ixoff=0
c        if (iop.eq.21) ixoff = leng(text)*X(2)/2     ! centre  25-3-98
c        if (iop.eq.22) ixoff = leng(text)*X(2)       ! RH just
        if (iop.eq.21) ixoff = istr_end(text)*X(2)/2     ! centre  25-3-98
        if (iop.eq.22) ixoff = istr_end(text)*X(2)       ! RH just
        XI_2(1)=xi_2(1)-ixoff           ! set text anchor point
        CALL DRAW_TEXT@ (TEXT,XI_2(1),YI_2(1),ICOL_2)

c---------------------------- unknown option ---------------------------
      ELSE
        PRINT*,'IOP=',IOP,' unknown in DR_VGA'
        STOP
      ENDIF
      END

C----------------------------------------------------------------------
      SUBROUTINE DR_SALFORD (TEXT,X,Y,N,ICOL,IOP)
C
C     Handles Salford output (VGA and HPGL are handled elsewhere)
C     so just PCL and EPSON Printers I guess.
C
      COMMON /DEST/IDV, DV_xp,DV_yp,DV_ncol,DV_xl,DV_yl ,DV_ar
      SAVE           !(so XI_2, YI_2 becomae static for speed
      REAL X(*),Y(*)        !--- x,y,(z) coords of the line/polygon
      CHARACTER TEXT*(*)
c     CHARACTER TEXT*80
      INTEGER*2 XI_2(500), YI_2(500), N_2   !- For Salford DBOS
     &      ,NP1_2 ,IPOLY,IFAIL       !- sub-routine calls        
     &      ,iymin,iymax
      integer*4 icol_2
c---------- copy real coords to integer*2 x,y lists --------------------
c  *** invert ALL the y-values ???
c     Iresy = 480     !- ***just a hack for no .. beware SVGA
       Iresy = dv_yp     !- ***just a hack for no .. beware SVGA
      ICOL_2 = ICOL   !- Salford devices will need INT*2
      N_2  = N
      DO I=1,N      
        XI_2 (I) =        NINT (X(I))  
c       YI_2 (I) =iresy - NINT (Y(I))
        YI_2 (I) =        NINT (Y(I))
      ENDDO

c--------------------------- polyline draw -----------------------------
      IF (IOP.EQ.3) THEN       
         CALL POLYLINE@ (XI_2,YI_2, N_2, ICOL_2)

C------------------------- closed polygon draw -------------------------
c... should do this here too
      ELSEIF (IOP.EQ.1) THEN  
          NP1_2 = N_2 + 1                !- close the polygon
          XI_2 (NP1_2) = XI_2 (1)        !- with a copy of
          YI_2 (NP1_2) = YI_2 (1)        !- the first point
          CALL POLYLINE@       (XI_2,YI_2, NP1_2, ICOL_2)

c-------------------------- filled polygon draw ------------------------
C.. To cure a BUG in Salford's  FILL_POLYGON@() routine that causes
C.. some polygons to have a horizontal 'tail' (going the 'leading' diagonal)
C... this algorithm will find the total height of the polygon 
C... and skip-out if has zero-height  :   Dan Kidger 14-07-92
C        (cf my Postscript driver which works perfectly !)
c... possibly I only need this for SVGA and VSCREEN (eg. PCX) ?
c... Note: at one time I had the code collapsing pairs of nodes
c... that had the same y-value

      ELSEIF (IOP.EQ.2 ) THEN 

C--------- patch to remove 'zero-height' polygons 14-07-92 -------------
c.. I thought the error was just co-incident nodes ??
        ICOLS = ICOL
        IYMIN = YI_2(1)                ! find the 
        IYMAX = YI_2(1)                ! max and min y-values
        DO I=2,N                       ! of this
          IYMIN = MIN(IYMIN,YI_2(I))   ! polygon
          IYMAX = MAX(IYMAX,YI_2(I))   !
        ENDDO
        IF (IYMIN.EQ.IYMAX) RETURN  !- abort if zero-height
        CALL CREATE_POLYGON@ (XI_2,YI_2,N_2,IPOLY, IFAIL)  ! create
        CALL DOSERR@                            (IFAIL)  ! the 
        CALL FILL  _POLYGON@       (IPOLY,ICOL_2,IFAIL)  ! polygon
        CALL DOSERR@                            (IFAIL)  ! then fill
        CALL DELETE_POLYGON_DEFINITION@  (IPOLY, IFAIL)  ! and 
        CALL DOSERR@                            (IFAIL)  ! reset

c-------------------------- filled circle draw -------------------------
c.. where nodes' radius are held in XP(2)
      ELSEIF (IOP.EQ.10) THEN   
        CALL FILL_ELLIPSE@ (XI_2(1),YI_2(1),XI_2(2),XI_2(2),ICOL_2)

c------------------------- Text string drawing -------------------------
c.. hmm font select ?  .. x(2) is supposed to be the point-size
      ELSEIF (IOP.ge.20.and.iop.le.29) THEN 
          CALL DRAW_TEXT@ (TEXT,XI_2(1),YI_2(1),ICOL_2)

c---------------------------- unknown option ---------------------------
      ELSE
        PRINT*,'IOP=',IOP,' unknown in DR_SALFORD'
        STOP
      ENDIF
      END

C-----------------------------------------------------------------------
      SUBROUTINE PCX_FIX (FILE_PCX,PAL)
C
C     This 'corrects' the palette in a PCX file to be the same as
C     on the screen
C     (This may be done automatically in newer versions of DBOS)
C
C       --> also handles 256 colour PCX files
C        ( PAL of course does not have to come from COMMON !)
C     however even so, only 63 levels of R/G/B are stored in the VGA 
C     registers so this is technically better quality cos we use my
C     internal 8-bit colours rather than the VGA DAC's only 5-bits.
C
      INTEGER  PAL(3,0:255)
      CHARACTER FILE_PCX*(*)

      CHARACTER DUMMY*(16), bitpl*1
      integer*2 ierror
      INTEGER*4  NBYTES_R, EOF, NEW_POS, POS, nb

      CALL OPENRW@ (FILE_PCX,IO,IERROR)

      nb = 16
      CALL READF@ (dummy,IO,nb,NBYTES_R,IERROR)   !- (need DUMMY for codes)
      bitpl = dummy(4:4)
      DO I=0,15              !- the first copy of the palette
        nb = 1
        CALL WRITEF@ (CHAR(PAL(1,I)),IO,nb,IERROR)
        CALL WRITEF@ (CHAR(PAL(2,I)),IO,nb,IERROR)
        CALL WRITEF@ (CHAR(PAL(3,I)),IO,nb,IERROR)
      ENDDO

      IF (ICHAR(bitpl).eq.8) then           ! 8 bit planes .. so palette 
c                                           ! is at the end too !
        POS = 1024
        POS = POS * POS                     !(this avoids int*2 problems)
        CALL FPOS@ (IO,POS,NEW_POS,IERROR)  !- to EOF
        EOF = NEW_POS
        POS = EOF - 769
        CALL FPOS@ (IO,POS,NEW_POS,IERROR)  !- start of the 256 col palette
        nb = 1
        call READF@ (DUMMY,IO,nb,NBYTES_R,IERROR)
        IF (ICHAR(DUMMY(1:1)).ne.12) THEN    !- magic code
          PRINT*,' *** Palette at the end of the PCX file not found'
          RETURN
        ENDIF
        DO I=0,255          !(can't I write 3 bytes at once?
          nb = 1
          CALL WRITEF@ (CHAR(PAL(1,I)),IO,nb,IERROR)
          CALL WRITEF@ (CHAR(PAL(2,I)),IO,nb,IERROR)
          CALL WRITEF@ (CHAR(PAL(3,I)),IO,nb,IERROR)
        ENDDO
      ENDIF
      CALL CLOSEF@ (IO,IFAIL)         !---- OOPS ! 6-11-93
      END

C-----------------------------------------------------------------------
      SUBROUTINE READ_PCX_PALETTE (FILE_PCX,PAL,ix_pcx,iy_pcx)
C
C     This reads the 256 RGB palette in a PCX file
C       because Salford can only read the 16 colour palette :-(
C       .. also returned is the image x,y size
C         Dan Kidger 7-2-95
C
      INTEGER  PAL(3,0:255)
      CHARACTER FILE_PCX*(*)

      CHARACTER dummy *(16), val*1
c     character cols(3)
      integer*2 ierror   !, sizes(2)
      INTEGER*4  NBYTES_R, EOF, NEW_POS, POS, nb, nbr

      CALL OPENRW@ (FILE_PCX,IO,IERROR)

      nb = 12
      CALL READF@ (dummy,IO,nb,NBYTES_R,IERROR)   !- (need DUMMY for codes)

      nb = 1
      CALL READF@ (val,IO,nb,NBYTES_R,IERROR)  
      ix_pcx = ichar(val)
      CALL READF@ (val,IO,nb,NBYTES_R,IERROR)  
      ix_pcx = ix_pcx + 256*ichar(val)
      CALL READF@ (val,IO,nb,NBYTES_R,IERROR)  
      iy_pcx = ichar(val)
      CALL READF@ (val,IO,nb,NBYTES_R,IERROR)  
      iy_pcx = iy_pcx + 256*ichar(val)

c     iy_pcx = sizes(2)

      DO I=0,15              !- the first copy of the palette
        nb = 1
        do j=1,3
          CALL READF@ (val,IO,nb,nbr,IERROR)
          pal (j,i) = ichar(val)
        enddo
      ENDDO

      IF (ICHAR(DUMMY(4:4)).eq.8) then      ! 8 bit planes .. so paletee 
c                                           ! is at the end too !
        POS = 1024
        POS = POS * POS                     !(this avoids int*2 problems)
        CALL FPOS@ (IO,POS,NEW_POS,IERROR)  !- to EOF
        EOF = NEW_POS
        POS = EOF - 769
        CALL FPOS@ (IO,POS,NEW_POS,IERROR)  !- start of the 256 col palette
        nb = 1
        call READF@ (DUMMY,IO,nb,NBYTES_R,IERROR)
        IF (ICHAR(DUMMY(1:1)).ne.12) THEN    !- magic code
          PRINT*,' *** Palette at the end of the PCX file not found'
          RETURN
        ENDIF
        DO I=0,255    
          nb = 1
          do j=1,3
            CALL READF@ (val,IO,nb,nbr,IERROR)
            pal (j,i) = ichar(val)
          enddo
        ENDDO
      ENDIF
      CALL CLOSEF@ (IO,IFAIL)         !---- OOPS ! 6-11-93
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
c     should see what th ecurrent output device is - if not VGA do 
c     nothing
C
      COMMON /PALETTE/PAL
      INTEGER PAL(3,0:255)
      INTEGER*1 PAL1 (3,0:255)

C--- patch to avoid setting dacs unless we are writing to screen ---
      CALL GET_IDEST(IDEST)
c     print*,'idest=',idest
      IF (IDEST.LE.0) RETURN      ! -1=off, 0=dummydriver
      IF (IDEST.GE.20) RETURN     ! 20->29=PCX, but I manualy mung the 
c                                   palette, after writing the file
c     IF (IDEST.GE.30) RETURN     ! 30+ are printers (+WMF+DXF)

      DO J=1,3   
        DO I=max(ifr,0),min(ito,255)    !- clip just in case!
          PAL1(J,I) = PAL(J,I)/4 
        ENDDO
      ENDDO
      CALL SET_VIDEO_DAC_BLOCK@(ifr,ito-ifr+1, PAL1(1,ifr) )
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
      CALL INITIALISE_MOUSE@()                    !- do here ?
      CALL SET_MOUSE_BOUNDS@ (3,3, ixres-3-10, iyres-3 )
      CALL SET_MOUSE_MOVEMENT_RATIO@( 3*8,1*8)    !- aniso hmm. why?
      CALL SET_MOUSE_POSITION@ (ixres/3,iyres/3)  !- start pos
      END

C-----------------------------------------------------------------------
c     SUBROUTINE DEBOUNCE_BUTTONS
C
C     This routine will absorb all mouse button presses
C     until there is some change in the button status.
C
c     INTEGER*2 OLD_STATUS,IH,IV,STATUS
c     CALL GET_MOUSE_POS@ (IH,IV,OLD_STATUS)
c     STATUS=OLD_STATUS
c     WHILE (STATUS.EQ.OLD_STATUS.AND.STATUS.NE.0) DO
c        CALL GET_MOUSE_POS@(IH,IV,STATUS)
c     ENDWHILE
c     END

C-----------------------------------------------------------------------
c-----------------------------------------------------------------------
c     Routines for handling the Mouse cursor / cursor-keys
c     +  Mouse interupt subroutines (from Salford Manual Example)
c-----------------------------------------------------------------------
      SUBROUTINE CURSOR_GINO (IXM,IYM, codes)
C
C     Lets the user move the cross-hairs around with the mouse
C     returns the screen-coord of the hit
C     (and which mouse button was pressed)
c
c     Simplex (GINO) style .. waits here until a mouse_press/ key_press
C      <no sub-call to 'MENUS'>
c
C     If the keyboard is pressed, the scancode /ascii value is returned
C      .. in codes(1)
c      .. a mouse hit returns codes(1) = opcode and codes(2..5) as data
c         from the POST_MENUS subcall
C         Dan Kidger 6-2-95
C

c.. nice to define the cursor-shape, or even let it change in different
C   areas of the screen (even 'flash; buttons as we pass over)

      INTEGER X,Y, KEY
      integer codes(10)     !- return menu box coords

c--------------- Interface to Salford's Interupt handler ---------------
c     EXTERNAL M_TRAP,B_TRAP        !- mouse trap/ <cntl-break> trap
      COMPLEX*16 LAB                !- jump-to label if mouse-press
c     INTEGER*4 Q                   !- (dummy store of old trap)
      INTEGER*2      KEY_2          !- key-code   
     &          ,MX_2,MY_2          !- mouse position
     &          ,ic_2               !- button press count

      COMMON /mouse_status/ LAB,MX_2,MY_2    !- talks with interupt sub.
      COMMON /C2/ X,Y,XMAX,YMAX            !- +screen size ?
      logical zoom, over_button
      integer x1,y1,x2,y2,x3,y3
      data x1,y1,x2,y2,x3,y3/ 0,0,639,439,  100,100/   !- window size!

      data igin/5/    !- default is an arrow cursor (cf F10)
c     igin = 3    !- 1=full cross-hairs,4=fat cross
c     igin = 5    !- 5 = arrow cursor
c     igin = 1    !- 1 = full cross hairs

c     CALL LABEL@    (LAB,*2)              !- ie LAB == statement labeled '2'
c     CALL SET_TRAP@ (M_TRAP,Q,4)          !- 4=if mouse-event
c     CALL SET_MOUSE_INTERRUPT_MASK@ (1+2) !- bit0=move, bit1=left-but-'in'
c     CALL SET_MOUSE_INTERRUPT_MASK@ (  2) !- bit0=move, bit1=left-but-'in'
c     CALL SET_MOUSE_POSITION@ (400,200)    !- can set start point?

      zoom = .false.         !- not yet zooming   (or 3 states: off, 1st,2nd?)
      over_button = .false.  !- not yet 'flashed' a button
      do i=1,10          !- null the menu codes for good measure
        codes(i)= 0
      enddo

      CALL RESTORE_GRAPHICS_BANK@()
      CALL GET_MOUSE_POS@ (MX_2,MY_2)  !- starting pos
      X=MX_2                    !- maybe I can use my own start-posn. ?
      Y=MY_2                    !- so X,Y = Int*2 coord
      CALL GIN (X,Y,igin, x1,y1,x2,y2,x3,y3)   !- show the cross-hairs

c.......... wait until something happens .......
1     continue                      !- (a F90 endless DO..ENDDO)
        CALL GET_KEY1@ (KEY_2)           !- ASCII value
        KEY= KEY_2                       
        IF (KEY.ne.0) THEN          !- keyboard was pressed -!
          codes(1) = key            !- so return the opcode (mung outside)
          RETURN
        ENDIF
        CALL GET_MOUSE_BUTTON_PRESS_COUNT@(0,IC_2)
          IF (IC_2.GE.1) goto 22                !- got a LH mouse press
        CALL GET_MOUSE_BUTTON_PRESS_COUNT@(1,IC_2)
          IF (IC_2.GE.1) goto 33                !- got a RH mouse press
        CALL GET_MOUSE_POS@ (MX_2,MY_2) 
        IF ((X.EQ.MX_2).AND.(Y.EQ.MY_2)) GOTO 1    !-- wait for a movement --

        CALL GIN (X,Y,igin, x1,y1,x2,y2,x3,y3)     !-- erase old cursor
        X=MX_2             
        Y=MY_2                !- update mouse coord

c..  patch to mark any button that we cross
c  (as a pair of diagonals) .. nicer to shade-in ?
c        call post_menus (4,x,y, codes)
c        if (codes(1).gt.0) then    !- if a 'hit'
c          if (.not.over_button) then
c            over_button = .true.
c            ICOL = 32768 + 14
c            ICOL = 32768 + 255
c          CALL DRAW_LINE@( CODES(5), CODES(6), CODES(7), CODES(6), ICOL)
c          CALL DRAW_LINE@( CODES(7), CODES(6), CODES(7), CODES(8), ICOL)
c          CALL DRAW_LINE@( CODES(7), CODES(8), CODES(5), CODES(8), ICOL)
c          CALL DRAW_LINE@( CODES(5), CODES(8), CODES(5), CODES(6), ICOL)
c          endif
c        endif
        CALL GIN (X,Y,igin, x1,y1,x2,y2,x3,y3)       !-- draw new cursor
      GOTO 1      !-- re-cycle

c--------------- ON LH button, return coords (and codes too?) ----------
  22  CALL GIN (X,Y,igin, x1,y1,x2,y2,x3,y3)      !-- erase cursor
      ixm = x                  !- copy int*2 to 'generic'
      iym = y
c     KEY = -100               !- and flag event = -100 = LH mouse
      KEY = ichar ('L')           !- GINO style
      codes(1) = key
      RETURN

c--------------- ON LH button, return coords (and codes too?) ----------
  33  CALL GIN (X,Y,igin, x1,y1,x2,y2,x3,y3)      !-- erase cursor
      ixm = x                  !- copy int*2 to 'generic'
      iym = y
c     KEY = -100               !- and flag event = -100 = LH mouse
      KEY = ichar ('R')           !- GINO style
      codes(1) = key
      RETURN

c--------------- ON RH button, initiate zoom box ? ---------------------
 133  CALL GIN (X,Y,igin, x1,y1,x2,y2,x3,y3)      !-- erase cursor
      if (.not.zoom) then  !- first corner
        zoom = .true.
        x3 = x             !-  save the mouse position
        y3 = y
        igin = 7                             ! now do the full zomm-box GIN
        CALL GIN (X,Y,igin, x1,y1,x2,y2,x3,y3)   !- show the new cursor-box
        goto 1             !- back into the loop
      else                 !- second corner

        codes (1) = ichar ('V')     !- a view type operation
        codes (2) = 101             !- 101 = 'zoom into this box'
        codes (5) = min (x,x3)
        codes (6) = min (y,y3)      !- return the xyXY of the zoom box
        codes (7) = max (x,x3)
        codes (8) = max (y,y3)
c       KEY = -110                  !- and flag event = -110 = Zoom box
        RETURN
      endif
      END

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
      EXTERNAL DRAW_LINE@           !- necessary or just tidier ?

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
        CALL DRAW_LINE@ (IX,y1 ,IX,y2  ,icol)
        CALL DRAW_LINE@ (x1,IY ,x2,IY  ,icol)

c................ type 2 = window clipped cross-hairs ...............
c.. same as type 1 for now .. we will need the xyXY of the current window
c.. let the driver routine pass with either the screen or window bounds
c   ( maybe both & GIN() decides which to use ? )

      ELSEIF (ITYPE.EQ.2) THEN      
        CALL DRAW_LINE@ (IX,x1 ,IX,y2  ,icol)
        CALL DRAW_LINE@ (x1,IY ,x2,IY  ,icol)

c.................. type 3 = small cross-hairs ....................
      elseif (itype.eq.3) then
        ith = 20
        CALL DRAW_LINE@ (IX     ,iy-ith,IX    ,IY+ith,icol)
        CALL DRAW_LINE@ (ix-ith ,IY    ,IX+ith,IY    ,icol)

c.................. type 4 = small fat cross  .....................
      elseif (itype.eq.4) then
        do i=-1,1       !- so 3 lines thick
          ith = 10
          CALL DRAW_LINE@ (IX+i   ,iy-ith,IX+i  ,IY+ith,icol)
          CALL DRAW_LINE@ (ix-ith ,IY+i  ,IX+ith,IY+i  ,icol)
        enddo

c.................. type 5 = arrow  .....................
      elseif (itype.eq.5) then
        ith = 10      !- head size   *Adjustable*
        iln = 2*ith   !- tail length
        CALL DRAW_LINE@ (IX  ,IY  ,IX+ith  ,IY+  0,icol)   !- 1st x
        CALL DRAW_LINE@ (IX+1,IY+1,IX+ith  ,IY+  0,icol)   !- 2nd x
        CALL DRAW_LINE@ (IX  ,IY+1,IX+  0  ,IY+ith,icol)   !-  1st y
        CALL DRAW_LINE@ (IX+1,IY+2,IX+  0  ,IY+ith,icol)   !-  2nd y
        CALL DRAW_LINE@ (IX+2,IY+2,IX+iln  ,IY+iln,icol)   !-  1st tail
        CALL DRAW_LINE@ (IX+2,IY+3,IX+iln  ,IY+iln,icol)   !-  2nd tail

c.................. type 6 = Zoom box #1 .....................
c.. try just the top & LH lines (then do base & RH below ?)
      elseif (itype.eq.6) then
        CALL DRAW_LINE@ (X3 ,Y3   ,x3 ,Y2  ,icol)   !- TOP
        CALL DRAW_LINE@ (X3 ,Y3+1 ,x2 ,Y3  ,icol)   !- LH

c.................. type 7 = Zoom box #2 .....................
c.. The full box :-)    .. cf POLYLINE@()
      elseif (itype.eq.7) then
        CALL DRAW_LINE@ (X3  ,Y3   ,IX   ,Y3   ,icol)   !- TOP
        CALL DRAW_LINE@ (X3  ,Y3+1 ,X3   ,IY   ,icol)   !- LH
        CALL DRAW_LINE@ (IX  ,IY   ,X3+1 ,IY   ,icol)   !- Bottom
        CALL DRAW_LINE@ (IX  ,IY-1 ,IX   ,Y3+1 ,icol)   !- RH

c...................................................................
      ELSE      !- error if unknown ?
      ENDIF
      RETURN
      END                 

c-----------------------------------------------------------------------
c     INTERRUPT SUBROUTINE M_TRAP
      SUBROUTINE M_TRAP              !- FTN90 doesn't like this :-(
c
c     Routine 'tripped in' if any mouse evemt - unused ?
c
c      .. nice to trap all mouse events
c      .. Double-click: mark time of first press, wait
c          if a second press occurs, jump to 'double-click' exit
c          elseif more movement, maybe do a 'drag' (eg. menu frames)
c               .. also rubber-banding for Zoom
c
      INTEGER*2 MX_2,MY_2,MASK
      COMPLEX*16 LAB               !- a 'far' pointer 
      COMMON /mouse_status/ LAB,MX_2,MY_2

      CALL GET_MOUSE_EVENT_MASK@ (MASK)   !- what happened?
c     if (MASK.EQ.1) CALL GET_MOUSE_POS@(MX_2,MY_2)   !- update coord
      if (MASK.eq.2) CALL JUMP@(LAB)              !- R button-pressed
c     if (MASK.eq.4) CALL JUMP@(LAB)              !- L button-pressed
      END
               
c-----------------------------------------------------------------------
c      INTERRUPT SUBROUTINE B_TRAP
c
c     Trap for control-break  - unused ?
c       Example only .. not currently used by me :-)
c      .. can close files, release memory, reset interupts
C      .. ask for confirmation, etc. then exit the program.
c
c      STOP
c      END
c-----------------------------------------------------------------------
c-----------------------------------------------------------------------
C-----------------------------------------------------------------------


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

      INTEGER X,Y, KEY
      integer codes(10)     !- return menu box coords

c--------------- Interface to Salford's Interupt handler ---------------
c     EXTERNAL M_TRAP,B_TRAP        !- mouse trap/ <cntl-break> trap
      COMPLEX*16 LAB                !- jump-to label if mouse-press
c     INTEGER*4 Q                   !- (dummy store of old trap)
      INTEGER*2      KEY_2          !- key-code   
     &          ,MX_2,MY_2          !- mouse position
     &          ,ic_2               !- button press count

      EXTERNAL Q_FUNC
      COMMON /mouse_status/ LAB,MX_2,MY_2    !- talks whit interupt sub.
      COMMON /C2/ X,Y,XMAX,YMAX            !- +screen size ?
      logical zoom, over_button
      integer x1,y1,x2,y2,x3,y3
      data x1,y1,x2,y2,x3,y3/ 0,0,639,439,  100,100/
      
      data igin/5/      !- default cursor .. cf F10 to change on the fly
      igin_old = -1    !- 'last' cusror shape
c     igin = 3    !- 1=full cross-hairs,4=fat cross
c     igin = 5    !- 5 = arrow cursor

c     CALL LABEL@    (LAB,*2)              !- ie LAB == statement labeled '2'
c     CALL SET_TRAP@ (M_TRAP,Q,4)          !- 4=if mouse-event
c     CALL SET_MOUSE_INTERRUPT_MASK@ (1+2) !- bit0=move, bit1=left-but-'in'
c     CALL SET_MOUSE_INTERRUPT_MASK@ (  2) !- bit0=move, bit1=left-but-'in'
c     CALL SET_MOUSE_POSITION@ (400,200)    !- can set start point?

      zoom = .false.         !- not yet zooming   (or 3 states: off, 1st,2nd?)
      over_button = .false.  !- not yet 'flashed' a button
      do i=1,10          !- null the menu codes for good measure
        codes(i)= 0
      enddo

      CALL RESTORE_GRAPHICS_BANK@()
c     call init_mouse ?  .. so button_counts are zero?
      CALL GET_MOUSE_BUTTON_PRESS_COUNT@(0,IC_2)     !- so button counts
      CALL GET_MOUSE_BUTTON_PRESS_COUNT@(1,IC_2)     !- are zeroed
      CALL GET_MOUSE_POS@ (MX_2,MY_2)  !- starting pos
      X=MX_2                    !- maybe I can use my own start-posn. ?
      Y=MY_2                    !- so X,Y = Int*2 coord
      CALL GIN (X,Y,igin, x1,y1,x2,y2,x3,y3)   !- show the cross-hairs

c.......... wait until something happens .......
1     continue                      !- (a F90 endless DO..ENDDO)
        CALL GET_KEY1@ (KEY_2)            !- ASCII value
        KEY= KEY_2                       
        IF (KEY.eq.324) THEN              !- F10 to change the cursor shape
          CALL GIN (X,Y,igin, x1,y1,x2,y2,x3,y3)   !- erase
          igin = igin + 1             
          if (igin.gt.5) igin = 1         !- not 'zoom boxes'!
          CALL GIN (X,Y,igin, x1,y1,x2,y2,x3,y3)   !- re-show 
c         goto 1    !- cycle ??
        ELSEIF (KEY.ne.0) THEN                !- keyboard was pressed -!
          codes(1) = -key      !- so return the (-ve) opcode (mung outside)
          RETURN
        ENDIF
        CALL GET_MOUSE_BUTTON_PRESS_COUNT@(0,IC_2)
          IF (IC_2.GE.1) goto 22                !- got a LH mouse press
        CALL GET_MOUSE_BUTTON_PRESS_COUNT@(1,IC_2)
          IF (IC_2.GE.1) goto 33                !- got a RH mouse press
        CALL GET_MOUSE_POS@ (MX_2,MY_2) 
        IF ((X.EQ.MX_2).AND.(Y.EQ.MY_2)) GOTO 1    !-- wait for a movement --

        CALL GIN (X,Y,igin, x1,y1,x2,y2,x3,y3)     !-- erase old cursor
        X=MX_2             
        Y=MY_2                !- update mouse coord

c------------------ Query and hi-light any menus/polygons ------------
c-- eg a list of selectable object such as menu buttons
c  (as a pair of diagonals) .. nicer to shade-in ?
        call Q_FUNC (4,x,y, codes)
c       call post_menus (4,x,y, codes)
        if (codes(1).gt.0) then    !- if a 'hit'
          if (.not.over_button) then
            over_button = .true.
            ICOL = 32768 + 14
            ICOL = 32768 + 255
          CALL DRAW_LINE@( CODES(5), CODES(6), CODES(7), CODES(6), ICOL)
          CALL DRAW_LINE@( CODES(7), CODES(6), CODES(7), CODES(8), ICOL)
          CALL DRAW_LINE@( CODES(7), CODES(8), CODES(5), CODES(8), ICOL)
          CALL DRAW_LINE@( CODES(5), CODES(8), CODES(5), CODES(6), ICOL)
          endif
        endif
        CALL GIN (X,Y,igin, x1,y1,x2,y2,x3,y3)       !-- draw new cursor
      GOTO 1      !-- re-cycle

c--------------- ON LH button, return coords (and codes too?) ----------
  22  continue
      call Q_FUNC (4,x,y, codes)
      CALL GIN (X,Y,igin, x1,y1,x2,y2,x3,y3)      !-- erase cursor 
      ixm = x                  !- copy int*2 to 'generic'
      iym = y
c     codes(1) = 5             !- and flag event (5='mark-box')
      if (igin_old.ge.0) igin = igin_old    !- restore the cursor
      RETURN

c--------------- ON RH button, initiate zoom box ? ---------------------
  33  continue
      call Q_FUNC (4,x,y, codes)          !- get opcodes
      CALL GIN (X,Y,igin, x1,y1,x2,y2,x3,y3)      !-- erase cursor
      if (.not.zoom) then  !- first corner
        zoom = .true.
        x3 = x             !-  save the mouse position
        y3 = y
        igin_old = igin
        igin = 7                             ! now do the full zomm-box GIN
        CALL GIN (X,Y,igin, x1,y1,x2,y2,x3,y3)   !- show the new cursor-box
        goto 1             !- back into the loop
      else                 !- second corner

c... OK lets not bother .. just return a box
C     so draw, 
c       KEY = -110                  !- and flag event = -110 = Zoom box
        codes (1) = -ichar ('B')     !- a view type operation
c       codes (2) = 101             !- 101 = 'zoom into this box'
        codes (3) = min (x,x3)
        codes (4) = min (y,y3)      !- return the xyXY of the zoom box
        codes (5) = max (x,x3)
        codes (6) = max (y,y3)
        if (igin_old.ge.0) igin = igin_old    !- restore the cursor
        if (igin_old.ge.0) igin = igin_old    !- restore the cursor
        RETURN
      endif
      END

c-----------------------------------------------------------------------
      SUBROUTINE Q_GINO_NUL (IQTYPE,X,Y,CODES)
C
C     A bit of Object-Oriented code :-)
C     Passed through GET_MOUSE_OR_KEY_EVENT   
c     Returns in CODE() the integer data about which picture segment 
C     we are within   CODE(1) = 0 if no hit
C     cf 'POST_MENUS' in DANPLOT
C     IQTYPE = ?   (whether we are 'posting' or 'querying'
C     X,Y  = the input screen location of the cursor
C
      INTEGER IQTYPE, X,Y, CODES(20)

      CODES (1) = 0    !- no event found
      END
c-----------------------------------------------------------------------

