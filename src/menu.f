C----------------------------------------------------------------------
C  ** The menuing subroutines for DANPLOT v2.00
C
C  * There are ONLY 2 entrypoints
C    POST_MENUS : this is used for BOTH drawing all the menus 
C                  and returning which button has been pressed.
C    RESET_SIZES : returns the sizes of the screen-area left over for 
C                  actually drawing the model
C
C         c. Dan Kidger   1993
C
C   Notes:
C      need 'transient menus' eg. initial danplot logo.
C
C
C   22-3-95    CHAR amd IVAL now CODES() = string of integer (eg 16)
c   18-12-96   now a submenu for contour sx,sy,sz, s1, etc.
C
C
C-----------------------------------------------------------------------

      SUBROUTINE POST_MENUS (IOP,ixm,iym,codes)
C
C    This is the *ONLY* Entrypoint to all menu handeling
C     set IOP = 
C         = 1 : all the 'current' menus are re-drawn
C         = 2 : the menus are searched for a given button-press
C         =10 : un-post (and hence recover dynamic memory) of ALL transients
C
C   ?     = 3 : un-post this (transient) menu
C               
C   ?     = 4 : the screen coords of the button that the mouse (ixm,iym) 
C               is in is returned (plus codes ?)
C
      parameter   (n_menus=30+1)      !- Total # of menu panes
      integer post(n_menus)         !- 'Posted' status of each menu
     &       ,codes (*)             !- Return opcodes to the main program
     &       ,ixm, iym              !- mouse position (input)
      character char1*1
C       Note codes : -ve if invis, +ve if visible
C              1 = PERMANENT
C              2 = Normal LH side menus                          
C              3 = transient menus (over the draw-window)
C        4 = Priority .. needs 'EXIT' to release
      data (post(i),i=1,10)   
     + / 1      !1= 'TITLE-line'                  | Permanent
     + , 1      !2= 'MENU-line'                   | Menus
     + , 1      !3= '*IMAGE* itself               |
     + , 1      !4= 'STATUS-LINE' (at bottom)     |
     + , 1      !5= 'Toolbar' (was number-line)

     + ,-2      !6=  'File'                       | The
     + ,-2      !7=  'Edit'                       | 7 title-line
     + ,-2      !8=  'View'                       | menus
     + ,-2      !9=  'Contour'                    |
     + ,-2 /    !10= 'Mesh'                       |

      data (post(i),i=11,20)   
     + /-2      !11= 'Animation'                  | + 'vectors' + 'Axes'
     + ,-2      !12= 'Help'                       |
      
     + , 2      !13=  'About' = 'Danplot logo'  ? | Sub-menus
     + ,-3      !14=  'Config' (general)          |
     + ,-3      !15=  'SVGA' modes'               | possible as
     + ,-2      !16=  'palettes'                  | 'transients' ?
     + ,-3      !17=  'Faces + Edges'             |
                                                  
     + ,-3      !18=  'Edit: Mirror               |
     + ,-3      !19=  'Edit: Shift 'n Scale       |
     + ,-3 /    !20=  'Edit: Nodes 'n Elements    |

      data (post(i),i=21,n_menus)   
     + /-3      !21=  'Edit: Loads 'n Disps       |
     + ,-3      !22=  'Edit: Miscilaneous         |
     + ,-3      !23=  'Edits + Edges'             |

     + ,-3      !24=  'Import mesh'               |
     + ,-3      !25=  'Export Mesh'               |
     + ,-3      !26=  'old NUMBERS (16)'          |

     + ,-2      !27=  'Vectors'                   |
     + ,-2      !28=  'Axes'                      |
     + ,-4      !29=  'New 256 colour Pane'       |
     + ,-3      !30=  'New contour-terms'         |


     + ,0       !XX=  '?'                         |
     + /

c--------------------- Post all current menus --------------------------
      IF (IOP.EQ.1) THEN
        CALL RESTORE_GRAPHICS_BANK ()    !- just in case :-)
c.. if any are status=4 .. just do that & exit
c Q: do I need to do this (I dont think that I ever get here?)
        DO I=1,N_MENUS
          IF (POST(I).EQ.4) THEN
            CALL POST_A_MENU (I,1,IXM,IYM,CODES)
            RETURN
          ENDIF
        ENDDO
        DO I=1,N_MENUS     !- normal case of doing all.
          IF (POST(I).GT.0) CALL POST_A_MENU (I,1,IXM,IYM,CODES)
        ENDDO

c----------------------- Unpost all transients -------------------------
c.. eg just before an image-draw
      ELSEIF (IOP.EQ.10) THEN
        CALL CLEAR_TRANSIENT_MENUS (POST,N_MENUS)

c------------------------- search menus only ---------------------------
      ELSEIF (IOP.eq.4) THEN
        DO I=1,10
          CODES(I) = 0      !- space for the xyXY of a button
        ENDDO
        DO IMENU=N_MENUS,1,-1           !- *reversed cos some overlay others*
          IF (POST(IMENU).GT.0) CALL POST_A_MENU (IMENU,2,IXM,IYM,CODES)
          IF (CODES(1).NE.0) RETURN    !- cos mouse is over a button
        ENDDO
        RETURN      !-- exit cos mouse is over nothing

c------------------- Process a possible button-hit----------------------
      ELSEIF (IOP.EQ.2) THEN
c       IVAL = -2                       !- flag as 'unfound'
c       CHAR1 = ' '                      !- unfound ?
        DO I=1,10
          CODES(I) = 0
        ENDDO

c.. maybe abstract a list (ordered) of 'current' menus)
c.. if any ='4' then list is only 1-long
        DO IMENU=N_MENUS,1,-1       !- *reversed cos some overlay others*
          IF (POST(IMENU).GT.0) CALL POST_A_MENU (IMENU,2,IXM,IYM,CODES)
          IF (CODES(1).NE.0) THEN                     !- found the option
            CALL DRAW_BUTTON_YELLOW_2 (CODES)         !- 'beep' too ?
            IF (imenu.eq.29)
     &      CALL POST_A_MENU (IMENU,1,IXM,IYM,CODES)  !- repost :-) ?
c           CALL POST_A_MENU (IMENU,1,IXM,IYM,CODES)  !- repost :-) ?
            GOTO 2                                    !--> to handler
          ENDIF
          IF (POST(IMENU).EQ.4) THEN  !- here if outside a skicky panel.
            CALL SOUND (256,4)     !- cos an error 
            RETURN              !- disallow any others
          ENDIF
        ENDDO
c.. can beep  'cos we are outside the menuing area.
        RETURN      !-- not found so exit (should always HIT !)

c--------- If we have a 'duff' option (from the main prog) --------
      ELSE   
        STOP '*** unknown option (POST_MENUS)' 
      ENDIF
      RETURN      !- return for all but a mouse-press


C-----------------------------------------------------------------------
c------------------- Handle A BUTTON -HIT  -----------------------------
C-----------------------------------------------------------------------
c   in all cases need to unpost all transients ?
C
c   eg. if hit = 'post-menu'
c           or = 'set a CV value' so post 'numbers' transient
c
c... if we have a (non post-transient) event then un-post my transients
c
    2   CONTINUE
        CHAR1 = char (CODES(1))     !- nicer as an ASCII character

c----------------- Post colour-bar as a 'transient' --------------------
c.. this really belongs to a class of 'application specific' functions
c.. (is 'm'='post_menu' specific or general?)
        IF (CHAR1.EQ.'I'.or.CHAR1.EQ.'J') THEN 
c         imenu2 = 26               !- menu 26 = 16-colour-bar
          IF (CHAR1.EQ.'J') imenu2 = 26     !- menu 26 = 16-colour-pane
          IF (CHAR1.EQ.'I') imenu2 = 29     !- menu 29 = 256-colour-pane
          POST (IMENU2) = ABS(POST(IMENU2))          !- Enable this menu
          CALL POST_A_MENU (IMENU2,1,IXM,IYM,CODES)  !- and post it
c         CALL SET_CV (IVAL,999,2)         !- 'sets current'  ??
c         CALL CV_SET (IVAL,999,2)         ! (no need to do here)
c         CODES(1) = 0                !- 'cos all done here
          RETURN

c------------------------ Any non-menu-post ----------------------------
c.. just return the opcodes to the calling program

        ELSEIF (CHAR1.NE.'m') THEN     !- general-hit (not a post-menu)
          IF (POST(IMENU).ne.4)           !- leave panel's *up* 
     &     CALL CLEAR_TRANSIENT_MENUS (POST,N_MENUS)
c          CALL SOUND (1024,2)      !- a 'chirrup'   (cf a sound-card?)
c          CALL SOUND (768,2)    
          RETURN       !- all done

c-----------------------------------------------------------------------
c------------------------ handle 'post-menu(s)' ------------------------
c-----------------------------------------------------------------------
        ELSEIF (CHAR1.EQ.'m') THEN   
          codes (1) = 0               !- nul opcode cos handled here
          IMENU2 = codes(2)           !- get menu #


c------------------------- a system error ! ---------------------------
c.. maybe just do this in post_a_menu()
        IF (IMENU2.LT.-2.OR.IMENU2.GT.N_MENUS) THEN
          STOP '*** Menu not found! (POST_MENUS)' 

c----------------- clear the fill-in panel (and any 'strays'?)
c.. I if we hit an 'OK' button. 
C.. this sould be the *only* way that we can dismiss a pane (cf <esc> key)
        ELSEIF (IMENU2.eq.-2) THEN
          CALL CLEAR_TRANSIENT_MENUS (POST,N_MENUS)
          DO I=1,N_MENUS              !- now repost all others
            IF (POST(I).GT.0) 
     &      CALL POST_A_MENU (I,1,IXM,IYM,CODES)
          ENDDO
          CALL SOUND (1024,1)      !- a 'chirrup'   (cf a sound-card?)
          CALL SOUND (768,1)    
          RETURN
c-------------------------- refresh ALL menus  -------------------------
c.. (eg. from the application title-line)
        ELSEIF (IMENU2.EQ.-1) THEN
          CALL CLEAR_TRANSIENT_MENUS (POST,N_MENUS)
          DO I=1,N_MENUS              !- now repost all others
            IF (POST(I).GT.0) 
     &      CALL POST_A_MENU (I,1,IXM,IYM,CODES)
          ENDDO
          CODES(1) = 0      !-  done option so 'switch off'
          RETURN
        ENDIF

c------------------otherwise post a 'new' menu ----------------------------
c... if a 'sticky' then clear all stickies (and transients?)
c... if a transient just post
c--- maybe clear all transients (except the current = new's 'parent')
        CALL CLEAR_TRANSIENT_MENUS (POST,N_MENUS)
        IF (ABS(POST(IMENU2)).eq.2) THEN      !- new is 'sticky' (RHS)
           DO I=1,N_MENUS    !-- clear 2=='RHS stickies' too.
             IF (POST(I).GE.2) POST (I) = -abs(POST(I))
           ENDDO
        ENDIF
         POST (IMENU2) = ABS(POST(IMENU2))           !- Enable this menu
         CALL POST_A_MENU (IMENU2,1,IXM,IYM,CODES)   !- and post it
         CODES(1) =  0      !- cos we have just handled this event
         RETURN
c-----------------------------------------------------------------------
      ENDIF   !- various opcodes ('m' etc.)
      END

C-----------------------------------------------------------------------
c.....             Menu posting/inquiring routines
C-----------------------------------------------------------------------
      SUBROUTINE CLEAR_TRANSIENT_MENUS (POST,N_MENUS)
C
C     Simply clears all posted menus that are flagged as 'transient'
C       DJK 22-5-95
C
      IMPLICIT NONE
      INTEGER  N_MENUS,POST (N_MENUS)      !- status of all menus
     &          ,I,IXM,IYM, CODES(10)      !- rest are all dummies

      DO I=N_MENUS,1,-1
        IF (POST(I).EQ.3)THEN                !- if a transient menu
           CALL POST_A_MENU (I,3,IXM,IYM,CODES)   !- unpost
           POST(I)=-ABS(POST(I))                  !- now switch off
        ELSEIF (POST(I).EQ.4)THEN            !- if a sticky
          CALL POST_A_MENU (I,3,IXM,IYM,CODES)   !- unpost
          POST(I)=-ABS(POST(I))                  !- now switch off
        ENDIF
      ENDDO
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE UNPOST_MENU (Q,BUFFER)
C    
C    This simply restores the image that was underneath a menu
C     (cf. simply doing a GET_SCREEN_BLOCK to save it)
C  
c.. 4-1-01 or PGPLOT, we need to fix this 
c  - the simplest is to paste a black rectangle over the menu
c   
      INTEGER Q(4)
      INTEGER IFAIL_2
      INTEGER BUFFER

      IF (BUFFER.NE.-1) THEN
        CALL RESTORE_SCREEN_BLOCK (Q(1),Q(2),BUFFER,0,IFAIL_2)
c       CALL RETURN_STORAGE@ (BUFFER)
      ENDIF
      BUFFER = -1
      RETURN
      END

c-----------------------------------------------------------------------
      SUBROUTINE RESET_SIZES (RES)
C
C     This simply resets the image size  to the 'drawing' window
C     ... 15-9-94 resy now inverted  (so origin is bot-left)
C         *cancelled* .. just invert in the main prog
C
      INTEGER RES(*)

      call get_app_size (idev_x,idev_y)

c.. then subtract the menu metrics 
c     RES(1) = 518-20  !  -hacked to allow for the colour-number here
      RES(1) = idev_x-120-20  !  =idev_x- LH(120) - RH(20)
c     RES(2) = 419     !- image height   !- hmm ?  (was 413)
      RES(2) = idev_y -20-20-20
      RES(4) = 120+1   !- offsets -x = width of LH pane
c     RES(5) = 41      !          -y    !- original
      RES(5) = 20+1    ! or measure from the base?
      END

c-----------------------------------------------------------------------
      SUBROUTINE RESET_SIZES_2 (RES)
C
C     This 'alternative' to RESET_SIZES .. returns the full VGA
C
      INTEGER RES(*)

      call get_app_size (idev_x,idev_y)

c     RES(1) = 639
      RES(1) = idev_x-1
c     RES(2) = 479
      RES(2) = idev_y-1
      RES(4) = 0
      RES(5) = 0
      END

C-----------------------------------------------------------------------
      SUBROUTINE POST_TW_2 (q,p,TEXT,txt2)
C
C     A header to 'POST_TEXT_WIDGET' which auto-colours the text of the button 
C     if a title (txt2=' ') :  (ie. *NO* op-code)
C        text in *black* with no button box
C     else
C        text in *white* within a button box, with b+w shadow of 1 pixel
C
C   .. I could also have options to: disable colour, hi-light sub-menus,etc.
C
      IMPLICIT NONE
      INTEGER Q(4),P(4)
      CHARACTER TEXT*(*),TXT2*1

      IF (TXT2.EQ.' ') THEN                                !* A title *!
        CALL POST_TEXT_WIDGET (Q,P, 2,0, 1,0, 0,text)
      ELSE                                                 !* An option *!
        CALL POST_TEXT_WIDGET (Q,P, 2,1, 1,0, 1,text)
      ENDIF
      RETURN
      END

c-----------------------------------------------------------------------
      SUBROUTINE POST_TEXT_WIDGET (q,p,
     &                   ICOLB,ICOLT,ICOL1,ICOL2,ITHK,TEXT)
c                                                        
c     This posts a menu item 'text' at position 'pos' with the given colour
c                                              Dan Kidger  29-11-91
c
c     This is (almost) the only routine that draws any primitives
c     ... uses Salford directly (use my DR_PRIM instead??)

c--> need to extend to include 'control' characters eg. colours, font,
c    'dials', 'sliders', etc. !
c.. also nice to optionaly switch off the shadowing of the buttons

c  7-2-98 Multi-line text, loop and scan string for '/', write each
c         and advance in y (but note vertical centering ?)
c
      COMMON /MENU_FONT/ IFONT

      INTEGER Q(4),P(4)
      CHARACTER TEXT*(*)
      integer ibig
      integer istr_end

      IF (P(4).EQ.0) RETURN   !- dummy skip of invisible buttons
      call get_button_pos (q,p, ixf,iyf, ixt,iyt)

      ij = 16                !- palette start for button colours (0-6)
      DO J=0,ithk -1                   !- 'thick' borders
        CALL DRAW_LINE (ibig(IXT-J) ,ibig(IYT-J),
     &                   ibig(IXF+J) ,ibig(IYT-J), ij+ICOL1)  ! top
        CALL DRAW_LINE (ibig(IXF+J) ,ibig(IYF+J),
     &                   ibig(IXT-J) ,ibig(IYF+J), ij+ICOL2)  ! bottom
        CALL DRAW_LINE (ibig(IXT-J) ,ibig(IYF+J),
     &                   ibig(IXT-J) ,ibig(IYT-J), ij+ICOL1)  ! left
        CALL DRAW_LINE (ibig(IXF+J) ,ibig(IYT-J),
     &                   ibig(IXF+J) ,ibig(IYF+J), ij+ICOL2)  ! right
      ENDDO
c       IF (ICOLB.NE.-1)
c.. synonym for 'fill_rectangle' ?
        CALL FILL_RECTANGLE (IXF+ithk, IYF+ithk
     &   , IXT-ithk, IYT-ithk, ij+ICOLB)

      IBLACK = 0                             !- ('black' is col #0)

      IF (ISTR_END(TEXT).GT.0) THEN              !- no need to do for 'blanks'
ccc   IF (LENG(TEXT).GT.0) THEN              !- no need to do for 'blanks'
c     IF (TEXT=' ') THEN                     !- no need to do for 'blanks'
        IF (IFONT.EQ.1) THEN
          IOFF = -10
        ELSE         ! set 'origin' for the text
          IOFF = 0
        ENDIF
        IOFF=-10   ! for PGPLOT
          IOFF = -9    !- try one pixel higher
        IBDR = (IYT - IYF - 14 ) /2  + ithk-1   !- thickness of the 'border' ??

c.. win32 problem 25-2-99
c       do ii=1,istr_end(text) ! ouch - fix hershey problems with underscores
c         if (index('_\~',text(ii:ii)).gt.0) text(ii:ii)=' '
c       enddo

        IF (ICOLT.NE.IBLACK) THEN        !- shadow non-black letters
          IF (ifont.eq.1)                !( only if 'standard VGA font' ?)
     &    CALL DRAW_TEXT (TEXT(1:ISTR_END(TEXT)),
     &    (IXF+ithk+1), (IYT-ibdr-4+IOFF),ij+IBLACK)
        ENDIF
c--- now draw in the text string
        CALL DRAW_TEXT (TEXT(1:ISTR_END(TEXT)),
     &  ibig(IXF+ithk+2), ibig(IYT-ibdr-3+IOFF), ij+ICOLT)
      ENDIF
      END

c-----------------------------------------------------------------------
      function ibig (ix)
      common /app_size/iidev_x,iidev_y, xsc
      if (iidev_x.eq.640) then
        ibig=ix 
      else
c       ibig = nint (xsc*ix)
        ibig=ix 
      endif
      return
      end

c-----------------------------------------------------------------------
      SUBROUTINE get_button_pos (q,p, ixf,iyf, ixt,iyt)
c
c     Abstracts the screen xyXY of a button from its offsets
c      (maybe be -ve if RH aligned)
c        DJK 9-2-98 
c
c      q is the x,y, dx,dy of the menu pane
c      p is the x,y, dx,dy of the button itself
c     Maybe extend for centered buttons?
c
      INTEGER Q(4),P(4), ixf,iyf,ixt,iyt
      if (p(1).ge.0) then
        IXF = q(1) + p(1)          !- pane base +xo,yo of button
      else
        IXF = q(3) - p(1)-p(3)     !- pane_xmax -xo-button_width
      endif

      if (p(2).ge.0) then
        IYF = q(2) + p(2)           
      else
        IYF = q(2) - p(2)-p(4)     !- pane_ymax -yo-button_height
      endif

! hmm 480 is rather arbitrary. 
      iyf = 480- iyf-p(4)      !- hack for PGPLOT -> origin at lower-left
      IXT = IXF  + p(3)          !- button width and depth
      IYT = IYF  + p(4)
      return
      end

c-----------------------------------------------------------------------
      SUBROUTINE POST_MARBLE (YES, Q, P )
c                                                        
c     This adds a 'marble' at the *end* of a (pre-posted) button
c     to indicate its status
c                                              Dan Kidger  9-6-93

      INTEGER Q(4),P(4)
      LOGICAL YES

      IF (P(4).EQ.0) RETURN        !- skip invisible buttons

      call get_button_pos (q,p, ixf,iyf, ixt,iyt)

      IXC = IXT - 10            !- move 10 pixels out
      IYC = (IYF+IYT)/2         !- y-centre-line
      ij = 16       !- menu colours start (are #13,14,15?)
      
      IF (YES) THEN                                     !- On
        CALL FILL_ELLIPSE (IXC, IYC  , 4,4, ij+1)
        CALL FILL_ELLIPSE (IXC, IYC  , 2,2, ij+0)
      ELSE                                              !- Off
        CALL FILL_ELLIPSE (IXC, IYC  , 4,4, ij+2)      !- 2=gray-out
        CALL FILL_ELLIPSE (IXC, IYC  , 1,1, ij+0)
      ENDIF
      RETURN
      END

c-----------------------------------------------------------------------
      SUBROUTINE POST_COLOUR_DOT (ICOL, Q, P )
c                                                        
c     This adds a coloured dot at the *LH* of a (pre-posted) button
c     to indicate its status eg. for element # / node colour etc.
c                                              Dan Kidger  9-6-93

      INTEGER Q(4),P(4)

      IF (P(4).EQ.0) RETURN        !- skip invisible buttons
      IF (icol.eq.-1) RETURN       !- skip 'off' buttons
      call get_button_pos (q,p, ixf,iyf, ixt,iyt)
      IXC = IXT - 10
      Ixc = ixf + 6
      IYC = (IYF+IYT)/2
      
      IF (ICOL.ge.0) THEN                                     !- On
        CALL FILL_ELLIPSE (IXC, IYC  , 5,5, icol)
      ENDIF
      RETURN
      END

c-----------------------------------------------------------------------
      SUBROUTINE Q_MENU (IXM,IYM, Q,P,N_MENU, ITEM,XM,YM)
c
c     this returns the menu item (and sub-postion) of a selection
c     ih,iv = mouse position (screen)
c     xm,ym = fractional position within a menu item
c
c... the table will be just one menu so no need for 'menup()'

      integer q(4), p(4,N_MENU)
      item=  -1                   !- assume no hit
      xm  = .5                    !- assume at button centre?
      ym  = .5                    !-

      IH = IXM - Q(1)             !- relative to this pane
      IV = IYM - Q(2)
c  for PGPLOT we need the next line to invert the y-axis
c       call get_button_pos (q,p(1,i), ixf,iyf, ixt,iyt)
c     IF (IXM.LT.Q(1)) RETURN     !- exit *FAST* if the point 
c     IF (IYM.LT.Q(2)) RETURN     !- is outside the whole menu
c     IF ( IH.GT.Q(3)) RETURN
c     IF ( IV.GT.Q(4)) RETURN

      do i = n_menu,1,-1      !- use reverse search (overlaps?)
        IF ( P(4,i).eq.0)        goto 1   !- 'invisible' button (no depth)
        call get_button_pos (q,p(1,i), ixf,iyf, ixt,iyt)
        if (ixm.lt.ixf .or. ixm.gt.ixt) goto 1
        if (iym.lt.iyf .or. iym.gt.iyt) goto 1
           item = i
           xm = real(ih-p(1,i)) / real(p(3,i))   !- relative position
           ym = real(iv-p(2,i)) / real(p(4,i))
c          CALL DRAW_BUTTON_YELLOW (P,Q,I)    !- just a frame
c          CALL FLASH_BUTTON (P,Q,I)          !- flash the whole button
           RETURN    !- cos we have a 'button hit'
    1  CONTINUE
      ENDDO

      RETURN             !- only here if we hit the menu pane itself.
c... so need a 'special' key-code .. that 'post-menus' will handle
c    that causes just this menu to be reposted (kill all 'other' 
c    transients (must keep this menu!)
c   < hence this is a good way to un-post any 'daughter' menus.>

      END

C-----------------------------------------------------------------------
      SUBROUTINE DRAW_BUTTON_YELLOW (P,Q,I)
C
C     (ugly) routine to draw a (yellow) frame around the selected button
C     .. eg when a button on a menu is pressed
c     just so the user get a 'feel' for which button was hit.
C
      INTEGER Q(4),P(4,*)
C     INTEGER*2 II_2
      icol =15
      call get_button_pos (q,p(1,i), ixf,iyf, ixt,iyt)
      call draw_line (ixf,iyf,ixt,iyf,icol)
      call draw_line (ixt,iyf,ixt,iyt,icol)
      call draw_line (ixt,iyt,ixf,iyt,icol)
      call draw_line (ixf,iyt,ixf,iyf,icol)

c     CALL DRAW_LINE (q(1)+p(1,i)       , q(2)+p(2,i)       ,
c    +                 q(1)+p(1,i)+p(3,i), q(2)+p(2,i)       ,15) 
c     CALL DRAW_LINE (q(1)+p(1,i)+p(3,i), q(2)+p(2,i)       ,
c    +                 q(1)+p(1,i)+p(3,i), q(2)+p(2,i)+p(4,1), 15) 
c     CALL DRAW_LINE (q(1)+p(1,i)+p(3,i), q(2)+p(2,i)+p(4,1),
c    +                 q(1)+p(1,i)       , q(2)+p(2,i)+p(4,1), 15) 
c     CALL DRAW_LINE (q(1)+p(1,i)       , q(2)+p(2,i)+p(4,1),
c    +                 q(1)+p(1,i)       , q(2)+p(2,i)       , 15) 

      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE DRAW_BUTTON_YELLOW_2 (CODES)
C
C     Routine to draw a (yellow) frame around the selected button
C     .. eg when a button on a menu is pressed
C     just so the user get a 'feel' for which button was hit.
C     This is called after Q_MENU returns its position
C
      INTEGER P(4),CODES(10)
      P(1) = CODES(5)
      P(2) = CODES(6)
      P(3) = CODES(7)
      P(4) = CODES(8)

      CALL DRAW_LINE (p(1), p(2), p(3), p(2),  14) 
      CALL DRAW_LINE (p(3), p(2), p(3), p(4),  14) 
      CALL DRAW_LINE (p(3), p(4), p(1), p(4),  14) 
      CALL DRAW_LINE (p(1), p(4), p(1), p(2),  14) 
      END

C-----------------------------------------------------------------------
      SUBROUTINE FLASH_BUTTON (P,Q,I)
C
C     (ugly) routine to momentarily 'flash' an area on the VGA screen
C     .. eg when a button on a menu is pressed
c
c    Not really very good at all
c   .. hey but I could add 32768 to ICOl & just draw ?
C
C... only for screen areas *less* than   100 by 100 pixels (wot):-)
c... eg. to avoid the whole 'image' being flashed ?'
c
      INTEGER Q(4),P(4,*)
c     INTEGER*2 II_2
      ibcol =15
c... patch to avoid over-large areas being flashed (eg non-buttons)
      IF (REAL(P(3,I)) * REAL(P(4,I)).GT.100.*100.) RETURN 
C     ii_2 = 3
c     CALL GRAPHICS_WRITE_MODE@ (ii_2)
      call get_button_pos (q,p(1,i), ixf,iyf, ixt,iyt)
      CALL FILL_RECTANGLE (ixf,iyf, ixt,iyt, ibcol)
c     CALL CLEAR_SCREEN_AREA
c    &   (q(1)+p(1,i),        q(2)+p(2,i)
c    &   ,q(1)+p(1,i)+p(3,i), q(2)+p(2,i)+p(4,1), 15)     !- White-out

c     call sleep@(.1)        !- be careful if /DREAL
c      A = 1.
c      DO II=1,2000
c        A = A + SQRT(real(II))**2.     !-  a 'pause' (oh dear)
c      ENDDO

      CALL FILL_RECTANGLE(ixf,iyf, ixt,iyt, ibcol)
c      CALL CLEAR_SCREEN_AREA
c     +   (q(1)+p(1,i),        q(2)+p(2,i)
c     +  ,q(1)+p(1,i)+p(3,i), q(2)+p(2,i)+p(4,1), 1S)     !- White-out

c     ii_2 = 0
c     CALL GRAPHICS_WRITE_MODE (II_2)
c     CALL DISPLAY_MOUSE_CURSOR@()

      RETURN
      END

c-----------------------------------------------------------------------

C-----------------------------------------------------------------------
      SUBROUTINE STANDARD_MENU_FONT (IOP)
C
C     Just a common entry-point for the 'standard' menu-font
C     .. can also change the standard font (and its size ?)
C
      SAVE
      COMMON /MENU_FONT/ IFONT
      IF (IOP.EQ.-1) THEN 
        IFONT = 101                ! = VGA font (line drawn ?)
        SIZE  = 1.                 ! at normal size
        IOFF  = 0                  ! with no vertical shift
      ELSEIF (IOP.EQ.0) THEN 
        CALL SET_FONT ( IFONT,  SIZE, 0.,0.)
      ELSEIF (IOP.EQ.1) THEN
        PRINT*,'Which font do you want (1,100-112) ?'
        READ*,IFONT
        IF (IFONT.EQ.1) THEN
          IOFF = -8
        ELSE         ! set 'origin' for the text
          IOFF = 0
        ENDIF
      ELSEIF (IOP.EQ.2) THEN
        PRINT*,'Which size of font do you want (1.) ?'
        READ*,SIZE
      ENDIF
      RETURN
      END

c-----------------------------------------------------------------------
      SUBROUTINE GET_CODES_BOX (CODES, OPT, OPC, Q, P)
C
C     Simply puts the menu coords into CODES(1..4)
C    ? .. the fractional mouse positon is put into codes(5..6) ?
c
C     * BETTER STRUCTURE :
C           1 = menu #, 2= button #
C           3,4,5,6 = xyXY of the button ('pixel coords ?')
c           .. also store the mouse x,y too?
c           7,8,(9..) = opcode(s)  (or 10++)
c
C    9-2-98 What if a button is aligned to the LH edge?
      IMPLICIT NONE
      INTEGER  CODES(*),  OPC, Q(4), P(4)    !(combine P and Q ?)
      CHARACTER OPT*1
c     REAL XM,YM
      INTEGER IXF,IYF,IXT,IYT

      call get_button_pos (q,p, ixf,iyf, ixt,iyt)
      CODES(1) = ICHAR(OPT)
      CODES(2) =       OPC
      CODES(5) = ixf              ! x-lo
      CODES(7) = ixt       !   x-hi
      CODES(6) = iyf             ! y-lo
      CODES(8) = iyt       !   y-hi
c      CODES(5) =       ! x-lo
c      CODES(7) = q(1) + p(1) + p(3)       !   x-hi
c      CODES(6) = q(2) + p(2)              ! y-lo
c      CODES(8) = q(2) + p(2) + p(4)       !   y-hi
      RETURN
      END

c-----------------------------------------------------------------------
      SUBROUTINE POST_MENU_PANE  (PANE)  !- menu pane
C
C     Simply posts the menu-pane itself.
C     Standard = gray with 2 pixel wideblack/gray shamfer
C            DJK 21-6-95
C
      integer pane(4)
      integer q1(4)
      data q1/0,0,0,0/
      CALL POST_TEXT_WIDGET (Q1,PANE, 2,1,1,0, 2,' ' )  !- menu pane
      END

C-----------------------------------------------------------------------
      SUBROUTINE GET_LH_MENU_POS (Q,Q1)
c
c     This simply returns the Q and Q1 positions for the 
c     standard-menu panes that appear on the LHS
c     .. so if a non 640x480 screen can set the margins appropriately
c  .. in PGPLOT, it is better to leave everything as 640x480, but scale the
c  *whole* application
      IMPLICIT NONE
      INTEGER Q(4), Q1(4), idev_x,idev_y

      call get_app_size (idev_x,idev_y)
      Q (1) =   0         !- top-right hand corner -x
      Q (2) =   0+ 20+20  !-                       -y
      Q (3) =   120     !- menu width
c     Q (4) =   420     !- menu height    (was 415)
      Q (4) =   idev_y -20-20-20
c.. hmm height= idev_y - title(20) -toolbar(20) -status line(20)
      Q1 (1) = 0            !
      Q1 (2) = 0            ! All zero 
      Q1 (3) = 0            ! (only used to post the menu-pane itself ?)
      Q1 (4) = 0            !  (therefore obsolete ?)
      END

C-----------------------------------------------------------------------
      SUBROUTINE POST_A_MENU (I,IOP,IXM,IYM,CODES)
C
C     This simply invokes a menu by number 
C       (This is the *only* way any individual menu is called)
C
C     IOP  :   1 = post this menu
C              2 = query this menu
C                  IXM,IYM = input mouse pos.
C                  codes = returned opcodes
C              3 = un-post (for transient menus)
C
      IMPLICIT NONE
      INTEGER    I                    !- The menu # to post in invoke
     &          ,IOP                  !- the ACTION (1=post, 2=query, etc) 
     &          ,IXM,IYM              !- input mouse coords
     &          ,CODES(*)             !- returned opcodes

c... if ordinary VGA mouse .. need to hide whilst we draw
c     call hide_mouse_cursor@()          !- hide whilest we draw
      call standard_menu_font(0)         !(only if *drawing* ?)
      call buffer_graphics(1)
      if (i.eq.1) then         !----- 'SELECT CASE' surely ! -------
         call post_menu_1  (i,IOP,ixm,iym,codes)      
      elseif (i.eq.2) then
         call post_menu_2  (i,IOP,ixm,iym,codes)
      elseif (i.eq.3) then
         call post_menu_3  (i,IOP,ixm,iym,codes)
      elseif (i.eq.4) then
         call post_menu_4  (i,IOP,ixm,iym,codes)
      elseif (i.eq.5) then
         call post_menu_5  (i,IOP,ixm,iym,codes)
      elseif (i.eq.6) then
         call post_menu_6  (i,IOP,ixm,iym,codes)
      elseif (i.eq.7) then
         call post_menu_7  (i,IOP,ixm,iym,codes)
      elseif (i.eq.8) then
         call post_menu_8  (i,IOP,ixm,iym,codes)
      elseif (i.eq.9) then
         call post_menu_9  (i,IOP,ixm,iym,codes)
      elseif (i.eq.10) then
         call post_menu_10 (i,IOP,ixm,iym,codes)
      elseif (i.eq.11) then
         call post_menu_11 (i,IOP,ixm,iym,codes)
      elseif (i.eq.12) then
         call post_menu_12 (i,IOP,ixm,iym,codes)
      elseif (i.eq.13) then
         call post_menu_13 (i,IOP,ixm,iym,codes)
      elseif (i.eq.14) then
         call post_menu_14 (i,IOP,ixm,iym,codes)
      elseif (i.eq.15) then
         call post_menu_15 (i,IOP,ixm,iym,codes)
      elseif (i.eq.16) then
         call post_menu_16 (i,IOP,ixm,iym,codes)
      elseif (i.eq.17) then
         call post_menu_17 (i,IOP,ixm,iym,codes)
      elseif (i.eq.18) then
         call post_menu_18 (i,IOP,ixm,iym,codes)
      elseif (i.eq.19) then
         call post_menu_19 (i,IOP,ixm,iym,codes)
      elseif (i.eq.20) then
         call post_menu_20 (i,IOP,ixm,iym,codes)
      elseif (i.eq.21) then
         call post_menu_21 (i,IOP,ixm,iym,codes)
      elseif (i.eq.22) then
         call post_menu_22 (i,IOP,ixm,iym,codes)
      elseif (i.eq.24) then
         call post_menu_24 (i,IOP,ixm,iym,codes)
      elseif (i.eq.25) then
         call post_menu_25 (i,IOP,ixm,iym,codes)
      elseif (i.eq.26) then
         call post_menu_26 (i,IOP,ixm,iym,codes)
      elseif (i.eq.27) then
         call post_menu_27 (i,IOP,ixm,iym,codes)
      elseif (i.eq.28) then
         call post_menu_28 (i,IOP,ixm,iym,codes)
      elseif (i.eq.29) then
         call post_menu_29 (i,IOP,ixm,iym,codes)
      elseif (i.eq.30) then
         call post_menu_30 (i,IOP,ixm,iym,codes)
      endif
      call buffer_graphics(0)

c     call display_mouse_cursor@()          !- restore mouse
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE POST_MENU_1 (imnu,IOP,IXM,IYM,CODES)
c
c     This is menu #1 ... the title-bar (plus 'exit' button etc)
c
c     if IOP = 1 then the menu is posted
c     if IOP = 2 then the op-codes for a 'hit' on one of this menus 
c     items is returned (maybe 'nul' if no 'hit')
c
      parameter (nbut=3)
      integer q(4),q1(4),p(4,nbut), opc(nbut),codes(*)
      character  text(nbut)*90, opt(nbut)*1
      COMMON /MENU_STRINGS/MENU_STRINGS
      CHARACTER MENU_STRINGS(2)*90
      data ((p(j,i),j=1,4),text(i),opt(i),opc(i),i=1,nbut)  
     + / 
     +    20,  0,  600, 20,    'title', 'm' ,-1     !- all menus ? 
     +  ,  0,  0,   20, 20,    '-',     'q' ,0      !- exit
     +  , -1,  0,   20, 20,    'x',     'P' ,10     !- SVGA redraw
     + /      
      DATA    Q /0, 0, 639, 20/      !- the x,y,w,h of this menu
     +      , Q1/0, 0,   0,  0/      !- 'offset' of the menu (always=0.)

      call get_app_size (idev_x,idev_y)
      Q(3)=idev_x              !- total menu width (since default is 640 pixels
      p(3,1)=idev_x-20-20      !- the title line
      TEXT(1) = MENU_STRINGS(1)     !- ie. 'Danplot, <ver>, <filename>'
      q1(1) = q1(1)    !- just a dummy
c----------------------------------------------------------------------
      IF (IOP.EQ.1) THEN
c     CALL POST_TEXT_WIDGET (Q1,Q, 1, 1, 1,0, 2,' ' )    !- menu pane

c.. hmm better to be able to set the title from the program !
      CALL SET_FONT ( 107,  1.2, 0.,0.)       !- for title

        i = 1
        CALL POST_TEXT_WIDGET   (Q,P(1,I), 7,1,1,0, 2,TEXT(I) ) 
        DO I = 2,3
          CALL POST_TEXT_WIDGET (Q,P(1,I), 2,1,1,0, 2,TEXT(I) )
        ENDDO

c--------------------------- search menus ------------------------------
      ELSEIF (IOP.EQ.2) THEN  
        CALL Q_MENU (IXM,IYM,Q,P,NBUT,ITEM,XM,YM)
        IF (ITEM.LE.0) RETURN   !- no hit 
        CALL GET_CODES_BOX (CODES, OPT(ITEM),OPC(ITEM), Q, P(1,ITEM))
      ENDIF
      END

c-----------------------------------------------------------------------
      SUBROUTINE POST_MENU_2 (imnu,iop,ixm,iym,codes)
c
c     This is menu #2 ...   the main horizontal menubar
c
c     if IOP = 1 then the menu is posted
c     if IOP = 2 then the op-codes for a 'hit' on one of this menus 
c     items is returned (maybe 'nul' if no 'hit')
c
      parameter (nbut=10)
      integer q(4),q1(4),p(4,nbut) ,opc(nbut), codes(*)
      character  text(nbut)*12, opt(nbut)*1
      data ((p(j,i),j=1,4),text(i),opt(i),opc(i),i=1,nbut)  
     +  /
     +    10, 2, 45,16,    'File'         ,'m', 6
     +  , 50, 2, 45,16,    'Edit'         ,'m', 7     ! +Mesh-generate ?
     +  , 90, 2, 45,16,    'Mesh'         ,'m',10 
     +  ,145, 2, 65,16,    'Contour'      ,'m', 9
     +  ,210, 2, 65,16,    'Vector'       ,'m',27
     +  ,275, 2, 45,16,    'View'         ,'m', 8
     +  ,320, 2, 45,16,    'Axes'         ,'m',28
     +  ,365, 2, 70,16,    'Animation'    ,'m',11 
     +  ,430, 2, 70,16,    'Colours'      ,'m',16 
     +  ,600, 2, 60,16,    'Help'         ,'m',12  !- RH justified
c    +  , -1, 2, 60,16,    'Help'         ,'m',12  !- RH justified
     +  /
c.. hmm if a button as a -ve xo or yo then we build from the RH edge
c  (like X-windows)
      DATA Q/0,20,639,20/, Q1/0,0,0,0/
      call get_app_size (idev_x,idev_y)
      Q(3)=idev_x

c------------------------ post menus -----------------------------------
!  consider here getting teh text size and so automatically setting the
!  position of each button
      IF (IOP.EQ.1) THEN 
        CALL POST_TEXT_WIDGET (Q1,Q, 2, 1, 1,0, 2,' ' )    !- menu pane
        DO I=1,NBUT   
c         CALL POST_TEXT_WIDGET (Q,P(1,I), -1, 0, 0,2, 0,TEXT(I) )  
          CALL POST_TEXT_WIDGET (Q,P(1,I),  2, 1, 1,0, 0,TEXT(I) )
        ENDDO

c--------------------------- search menus ------------------------------
      ELSEIF (IOP.EQ.2) THEN  
        CALL Q_MENU (IXM,IYM,Q,P,NBUT,ITEM,XM,YM)
        IF (ITEM.LE.0) RETURN   !- no hit 
        CALL GET_CODES_BOX (CODES, OPT(ITEM),OPC(ITEM), Q, P(1,ITEM))
      ENDIF
      END

C-----------------------------------------------------------------------
      SUBROUTINE POST_MENU_3 (imnu,iop,ixm,iym,codes)
c
c     This is menu #3 ...   the  PICTURE itself !
c
      SAVE
      PARAMETER (NBUT=1)
      INTEGER Q(4), P(4,NBUT), codes(*)

c      DATA P/  0, 0,519,415/          !- window ?
c      DATA Q/120,40,519,415/          !- menu pane itself
      DATA P/  0, 0,499,415/          !- window ?
      DATA Q/120,40,499,415/          !- menu pane itself

c---------------------------- post menus -------------------------------
      IF (IOP.EQ.1) THEN
c........ nothing to draw !  .. picture frame ??

c--------------------------- search menus ------------------------------
c... get the mouse pos. hence the sub-picture # 
c... return the position within a picture too ?
      ELSEIF (IOP.EQ.2) THEN  
        CALL Q_MENU (IXM,IYM,Q,P,NBUT,ITEM,XM,YM)
        IF (ITEM.LE.0) RETURN   !- no hit 
c       CALL GET_CODES_BOX (CODES, OPT(ITEM),OPC(ITEM), Q, P(1,ITEM))
        CALL GET_CODES_BOX (CODES, '_',0,               Q, P(1,ITEM))

c        IF (ITEM.GT.0) THEN    !- A 'HIT' on this menu
c          call sound( 400,1)   !- debug
c          CALL SET_CV (42,NXE,5)       !- NXE
c          CALL SET_CV (43,NYE,5)       !- NYE
c          IPIC = NXE * INT (NYE * YM) + INT (NXE * XM) + 1
c          CALL SET_CV (41,IPIC,1)       !- Picture number
c          codes(1) = iCHAR('_')         !- redraw :-)
c        ENDIF

      ENDIF
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE POST_MENU_4 (imnu,iop,IXM,IYM,codes)
C
C     This is Menu #4 -------------- the STATUS-LINE
C
C     24-1-94 This used to be the colour-line :-)
C
C     if IOP = 1 then the menu is posted
C     if IOP = 2 then the op-codes for a 'hit' on one of this menus 
C     items is returned (maybe 'nul' if no 'hit')
C
      SAVE
      PARAMETER (NBUT=1)
      INTEGER Q(4),Q1(4),P(4,NBUT) ,   OPC(NBUT), codes(*)
      CHARACTER  TEXT(NBUT)*90, OPT(NBUT)*1
      COMMON /MENU_STRINGS/MENU_STRINGS
      CHARACTER MENU_STRINGS(2)*90

      data ((p(j,i),j=1,4),text(i),opt(i),opc(i),i=1,nbut)  
     +  /
     +    20,  0,  600, 20,    'title', 'm' ,-1     !- all menus ? 
     +  /
c.. really want to pick up the screen width (ires_x)
c   the screen depth (hence subract the pane height)
      DATA Q/0,460,639,20/, Q1/0,0,0,0/
      call get_app_size (idev_x,idev_y)
      Q(3)=idev_x
      Q(2)=idev_y-20

      TEXT(1) = MENU_STRINGS(2)

c--------------------------- post menu ---------------------------------
      IF (IOP.EQ.1) THEN
         call post_text_widget (Q1,Q, 2, 1, 1,0, 2,' ' )  !- pane

      DO I=1,nbut
        icolt = 0
        CALL POST_TEXT_WIDGET (Q,P(1,I), 2,1, 1,0, 2,TEXT(I))
      ENDDO

c--------------------------- search menus ------------------------------
      ELSEIF (IOP.EQ.2) THEN  
        CALL Q_MENU (IXM,IYM,Q,P,NBUT,ITEM,XM,YM)
        IF (ITEM.LE.0) RETURN   !- no hit 
        CALL GET_CODES_BOX (CODES, OPT(ITEM),OPC(ITEM), Q, P(1,ITEM))
      ENDIF
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE POST_MENU_5 (imnu,iop,IXM,IYM,codes)
C
C     This is Menu # 5 . . . . . . the  RH Tool Bar
C
c     COMMON /PALETTE/PAL
c     INTEGER   PAL (3,0:255)      !- 'current' colour palette

      save
      parameter (nbut=7)
      integer q(4),q1(4),p(4,nbut) , ibc(nbut),   opc(nbut), codes(*)
      character  text(nbut)*12, opt(nbut)*1

      data ((p(j,i),j=1,4),text(i),ibc(i),opt(i),opc(i),i=1,nbut)  
     +  /
     +     1, 20, 18,18,   'P'   ,2   ,'m',  16    !- post-palette

     +   , 1, 50, 18,18,   '+'  ,2   ,'+', 31     !- Zoom-in
     +   , 1, 70, 18,18,   '-'  ,2   ,'-', 30     !- Zoom-out (=reset?)

     +   , 1,100, 18,18,   'P'   ,2   ,'P', 999    !- Print
     +   , 1,120, 18,18,   'A'   ,2   ,'Z', 1    !- Animate !

     +  ,  1,260, 18,18,    'Si' ,2   ,'J'  ,301   !<- hack
     +  ,  1,280, 18,18,    'Li' ,2   ,'J'  ,302

     +  /

       DATA Q/619,40,20,415/, Q1/0,0,0,0/
c      DATA Q/120,40,20,415/, Q1/0,0,0,0/
      call get_app_size (idev_x,idev_y)
      Q(1)=idev_x-20
      Q(4)=idev_y-20-20-20

c--------------------------- post menu ---------------------------------
      IF (IOP.EQ.1) THEN
         call post_text_widget (Q1,Q, 2, 1, 1,0, 2,' ' )  !- pane
c... what the hell is the next line for ?
         call draw_line (q(1), (q(2)),
     +     (q(1)+q(3)),(q(2)),  1)      !- edging in white

c     call set_text_attribute ( 101,  .6, 0.,0.)   !- 'small' text
      CALL STANDARD_MENU_FONT(0)

      DO I=1,NBUT
        icolt = 1
        CALL POST_TEXT_WIDGET (Q,P(1,I), ibc(i),icolt,  1,0, 1,TEXT(I) )
      ENDDO
      RETURN

c--------------------------- search menus ------------------------------
      ELSEIF (IOP.EQ.2) THEN  
        CALL Q_MENU (IXM,IYM,Q,P,NBUT,ITEM,XM,YM)
        IF (ITEM.LE.0) RETURN   !- no hit 
        CALL GET_CODES_BOX (CODES, OPT(ITEM),OPC(ITEM), Q, P(1,ITEM))
      ENDIF
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE POST_MENU_6 (imnu,iop,ixm,iym,codes)
c
c     This is menu #6 ...   the   FILE menu 
c
      parameter (nbut= 10)
      integer q(4),q1(4),p(4,nbut) ,opc(nbut),codes(*)
      character  text(nbut)*13, opt(nbut)*1
      data ((p(j,i),j=1,4),text(i),opt(i),opc(i),i=1,nbut)  
     +  /
     +   10, 10, 80,16,    'New  '          ,'#',  0
     +  ,10, 30, 80,16,    'Open >>'        ,'#',  1
     +  ,10, 50, 80,16,    'Save as >>'     ,'#',  1
     +  ,10, 70, 80,16,    'Import >>'      ,'m', 24
     +  ,10, 90, 80,16,    'Export >>'      ,'m', 25

     +  ,10,115, 80,16,    'Print'          ,'P',999    !- ??
     +  ,10,135, 80,16,    'Setup Printer'  ,'O',-99    ! or 'P' too?
     +  ,10,180,100,16,    'Configure >>'   ,'m', 14
     +  ,10,220, 80,16,    'Status..'       ,'Q',  0
     +  ,10,245, 80,16,    'Exit'           ,'q',  0
     +  /
c     data q/0,40,120,415/,q1/0,0,0,0/          !- menu pane itself
      CALL GET_LH_MENU_POS(Q,Q1)

      IF (IOP.EQ.1) THEN     !--------- post menus ------------
        CALL POST_TEXT_WIDGET (q1,q, 2,1, 1,0, 2,' ' )
        CALL FILL_RECTANGLE
     +    (Q(1)+10,Q(2)+ 92,Q(1)+Q(3)-10,Q(2)+ 92+1, 0)
        CALL FILL_RECTANGLE
     +    (Q(1)+10,Q(2)+138,Q(1)+Q(3)-10,Q(2)+138+1, 0)
        CALL FILL_RECTANGLE
     +    (Q(1)+10,Q(2)+222,Q(1)+Q(3)-10,Q(2)+222+1, 0)
        DO I=1,NBUT   
          CALL POST_TEXT_WIDGET (Q,P(1,i), 2,1,0,1, 0,TEXT(I) )
        ENDDO

c--------------------------- search menus ------------------------------
      ELSEIF (IOP.EQ.2) THEN  
        CALL Q_MENU (IXM,IYM,Q,P,NBUT,ITEM,XM,YM)
        IF (ITEM.LE.0) RETURN   !- no hit 
        CALL GET_CODES_BOX (CODES, OPT(ITEM),OPC(ITEM), Q, P(1,ITEM))
c.... if we have hit the 'print' button, then where do we abstract the 
c     CV() code ?
      ENDIF
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE POST_MENU_7 (imnu,iop,ixm,iym,codes)
C
C     This is menu #7 ...   the   EDIT menu   ??
C
      SAVE
      PARAMETER (NBUT=11)
      INTEGER Q(4),Q1(4),P(4,NBUT) , OPC(NBUT), codes(*)
      CHARACTER  TEXT(NBUT)*14, OPT(NBUT)*1
      DATA ((P(J,I),J=1,4),TEXT(I),OPT(I),OPC(I),I=1,NBUT)  
     +  /

     +   10, 40,100,16,    'Select boxed'    ,'T', 40
     +  ,10, 60,100,16,    'Refine sel.'     ,'T', 41
     +  ,10, 80,100,16,    'Select all'      ,'T', 42
     +  ,10,100,100,16,    'UnSelect all'    ,'T', 43
     +  ,10,120,100,16,    'Invert Select'   ,'T', 44
c    +  ,10,140,100,16,    'Delete Sel.'     ,'T', 45

     +  ,10,180,100,25,    'Mirroring   >'   ,'m', 18
     +  ,10,210,100,25,    'Shift+Scale >'   ,'m', 19
     +  ,10,240,100,25,    'Nodes+Elems >'   ,'m', 20
     +  ,10,270,100,25,    'Loads+Disps >'   ,'m', 21
     +  ,10,300,100,25,    'Materials   >'   ,'m', 22
     +  ,10,330,100,25,    'Misc.       >'   ,'m', 23

     +  /

      CALL GET_LH_MENU_POS(Q,Q1)

c------------------------ post menus -----------------------------------
      IF (IOP.EQ.1) THEN     
        CALL POST_TEXT_WIDGET (Q1,Q, 2,1,1,0, 2,' ' )  !- menu pane
        DO I=1,NBUT   
          CALL POST_TW_2 (Q,P(1,i),text(i),opt(i))
        ENDDO

c--------------------------- search menus ------------------------------
      ELSEIF (IOP.EQ.2) THEN  
        CALL Q_MENU (IXM,IYM,Q,P,NBUT,ITEM,XM,YM)
        IF (ITEM.LE.0) RETURN   !- no hit 
        CALL GET_CODES_BOX (CODES, OPT(ITEM),OPC(ITEM), Q, P(1,ITEM))
      ENDIF
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE POST_MENU_8 (imnu,iop,ixm,iym,codes)
C
C     This is menu #8 ...  the VIEW menu   ??
C
      save
      INTEGER iop,ixm,iym,codes(*)
      parameter (nbut=48)
      integer p(4,0:nbut), opc(0:nbut)             ! *DATA STRUCTURE*
      character  text(0:nbut)*14, opt(0:nbut)*1    ! * for buttons *
      data ((p(j,i),j=1,4),text(i),opt(i),opc(i),i=0,14)  
     &  /
     &    0, 40,120,420,   ' '             ,' '  ,0   !- menu-pane
     &  ,10, 10, 90,16,    '--- view ---'  ,' '  ,0   !- title

     &  ,10, 30, 90,16,    'image size'    ,' '  ,0
     &  ,10, 50, 14,16,    '-'             ,'-'  ,0
     &  ,35, 50, 39,16,    'typed'         ,'*'  ,0
     &  ,65, 50, 14,16,    '+'             ,'+'  ,0

     &  ,10, 70, 90,16,    'disp scale'    ,' '  ,0
     &  ,10, 90, 14,16,    '-'             ,'<'  ,0
     &  ,25, 90, 39,16,    'typed'         ,'s'  ,0
     &  ,65, 90, 14,16,    '+'             ,'>'  ,0

     &  ,10,110, 90,16,    'load step#..'  ,'J'  ,250   !- via color-menu
     &  ,10,130, 20,16,    '<'             ,'z'  ,0     !- ?check
     &  ,35,130, 40,16,    'typed'         ,'l'  ,0
     &  ,80,130, 20,16,    '>'             ,'z'  ,0     !- ?check
     & ,105,130, 12,12,    'L'             ,'F'  ,37001 /   

      data ((p(j,i),j=1,4),text(i),opt(i),opc(i),i=15,28)  
     &  /10,170, 90,16,    'backface-cull' ,' '  ,32000
     &  ,10,190, 16,16,    'B'             ,'F'  ,32001
     &  ,28,190, 16,16,    'n'             ,'F'  ,32000
     &  ,44,190, 16,16,    'F'             ,'F'  ,32002
     &  ,80,190, 28,16,    '1-6'           ,'E'  ,0   !- facet 1-6 to always/never draw

     &  ,10,210, 90,16,    'Depth-sort'    ,' '  ,0
     &  ,10,230, 16,16,    'B'             ,'F'  ,17001
     &  ,28,230, 16,16,    'M'             ,'F'  ,17003
     &  ,44,230, 16,16,    'F'             ,'F'  ,17002
     &  ,55,230, 33,16,    'off'           ,'F'  ,17000

c    &  ,10,260, 75,16,    'x''<->y'''     ,'%'  ,0

     &  ,10,260, 16,16,    'Y'             ,'V'  ,7
     &  ,26,260, 16,16,    'I'             ,'V'  ,9
     &  ,10,276, 16,16,    'Z'             ,'V'  ,6
     &  ,26,276, 16,16,    'X'             ,'V'  ,8/


      data ((p(j,i),j=1,4),text(i),opt(i),opc(i),i=29,nbut)  
     &  /55,270, 14,14,    'r'             ,'V'  ,2    !- rotate eye
     &  ,70,278, 14,14,    'r'             ,'V'  ,5
     &  ,70,262, 14,14,    'r'             ,'V'  ,1
     & , 85,270, 14,14,    'r'             ,'V'  ,4


     &  ,10,305, 14,14,    's'             ,'V'  ,12   !- shift object
     &  ,25,313, 14,14,    's'             ,'V'  ,15
     &  ,25,297, 14,14,    's'             ,'V'  ,11
     & , 40,305, 14,14,    's'             ,'V'  ,14

     &  ,55,305, 14,14,    'l'             ,'V'  ,22   !- rotate light
     &  ,70,313, 14,14,    'l'             ,'V'  ,21
     &  ,70,297, 14,14,    'l'             ,'V'  ,25
     &  ,85,305, 14,14,    'l'             ,'V'  ,24

!    & ,105,278, 14,14,    'i'             ,'V'  ,31   !- zoom in/out  = change perspective
!    & ,105,262, 14,14,    'o'             ,'V'  ,32
     & ,105,278, 14,14,    'i'             ,'+'  ,0   !- zoom in/out
     & ,105,262, 14,14,    'o'             ,'-'  ,0

     &  ,10,345, 90,16,    'colours..'     ,' '  , 0
     &  ,10,365, 70,16,    'background'    ,'I'  ,20
     &  ,83,365, 20,16,    'OS'            ,'I'  ,204 
                                              
     &  ,10,385, 90,14,    'picture #'     ,'J'  ,41
     &  ,10,400, 35,14,    'Nxp'           ,'J'  ,42
     &  ,55,400, 35,14,    'Nyp'           ,'J'  ,43

     &  /

c------------------------ post menus -----------------------------------
      IF (IOP.EQ.1) THEN     
c       CALL SET_CV (32, IVAL1, 5)    !-  current BPC  (5='query')
        CALL CV_GET (32, IVAL1)
C       CALL SET_CV (17, IVAL2, 5)    !-  current depth-sort
        CALL CV_GET (17, IVAL2)
c       CALL SET_CV (17, IVAL3, 5)    !-  current x-y flip
        CALL POST_MENU_PANE  (p(1,0))      !- menu pane
        DO I=1,NBUT   
          IF (OPT(I).EQ.'*') THEN          !- echo the status
            CALL SET_CV (32, IVAL, 5)      !-  current image scale (BPC!)
            WRITE (TEXT(I),'(i4)') IVAL
            CALL POST_TEXT_WIDGET (p(1,0),P(1,i), 0,1, 1,0, 2,TEXT(I))
          ELSE
            CALL POST_TW_2 (p(1,0),P(1,i),text(i),opt(i))    !- zap 'titles'
          ENDIF

          IF (OPT(I).EQ.'%') THEN    !- x-y flip flag
c           CALL POST_MARBLE (ival3.eq.1,Q,P(1,I))
c         ELSEIF (OPT(I).EQ.'\') THEN
          ELSEIF ((OPC(I)+1)/1000.EQ.32) THEN   !(use CV code itself)
ccc         CALL POST_MARBLE (mod(opc(i),1000).eq.ival1,Q,P(1,I))
c         ELSEIF (OPT(I).EQ.'|') THEN
          ELSEIF ((OPC(I)+1)/1000.EQ.17) THEN   !(use CV code itself)
ccc         CALL POST_MARBLE (mod(opc(i),1000).eq.ival2,Q,P(1,I))
          ENDIF
        ENDDO

c--------------------------- search menus ------------------------------
      ELSEIF (IOP.EQ.2) THEN  
        CALL Q_MENU (IXM,IYM, p(1,0),P(1,1),NBUT,ITEM, XM,YM)
        IF (ITEM.LE.0) RETURN   !- no hit 
        CALL GET_CODES_BOX (CODES, OPT(ITEM),OPC(ITEM),p(1,0),P(1,ITEM))
      ENDIF 
      END

C-----------------------------------------------------------------------
      SUBROUTINE POST_MENU_9 (imnu,iop,ixm,iym,codes)
C
C     This is menu #9 ...   the   CONTOURING   menu
C
c  18-12-96 I really need to have all the stress-terms as a list-box
c     or a fill-in panel (sub-menu)
c
      SAVE
      PARAMETER (NBUT=24-7-2)
      INTEGER Q(4),Q1(4),P(4,NBUT) , OPC(NBUT), codes(*)
      CHARACTER  TEXT(NBUT)*16, OPT(NBUT)*1
      DATA ((P(J,I),J=1,4),TEXT(I),OPT(I),OPC(I),I=1,NBUT)  
     +  /
     +   10, 10, 90,16,    '- Contours -'      ,' ',  0

c     +  ,10, 40, 90,16,    'Disps:'            ,' '  ,0 
     +  ,10, 60, 90,16,    'reset'             ,'K'  ,-1
c     +  ,10, 80, 90,16,    'ë-total'           ,'K'  ,30
c     +  ,10,100, 25,16,    'ëx'                ,'K'  ,31
c     +  ,40,100, 25,16,    'ëy'                ,'K'  ,32
c     +  ,65,100, 25,16,    'ëz'                ,'K'  ,33

c    +  ,10,120, 90,16,    'All Others-->'     ,' ' ,  0 
     +  ,10,100, 90,25,    'Value  -->'        ,'m' ,  30  !- to submenu
     +  ,10,130, 90,16,    'Isosurface'        ,'I' ,  83  
     +  ,10,170, 90,16,    'No-contour IMAT'   ,'I' ,  80 
c     +  ,10,140, 90,16,    'Shear Strain'     ,'K' ,  47 
c     +  ,10,160, 90,16,    'Vol.  Strain'     ,'K' ,  46 
c     +  ,10,180, 90,16,    'Vec. Normals'     ,'K' ,  18 
c     +  ,10,200, 90,16,    '  + IMAT'         ,'K' ,  19 
     +  ,10,220, 90,16,    'Shaded'            ,'K' ,  14 
             
     +  ,10,240, 90,16,    'Rescale: '         ,' ' ,  0 
     +  ,10,260, 65,14,    'Auto'              ,'^' ,  1 
     +  ,75,260, 30,14,    'Sym'               ,'^' ,   7 
     +  ,10,280, 90,14,    'Manual'            ,'^' ,  2 


     +  ,10,320, 60,16,    '# Conts'           ,'J' ,18
     + , 80,320, 30,16,    ' '                 ,' ' ,0

     +  ,10,340, 90,16,    'line type..'       ,'J' ,14
     +  ,10,360, 90,16,    'face type..'       ,'J' ,15
     +  ,10,380, 90,16,    'as a menu >'       ,'z' ,  0

     +  /
      CALL GET_LH_MENU_POS(Q,Q1)

c------------------------ post menus -----------------------------------
      IF (IOP.EQ.1) THEN     

C       CALL SET_CV (13, IVAL, 5)    !- get current contour type
        CALL CV_GET (13,IVAL)        !- contour variable
        CALL CV_GET (18,IVAL1)       !- # contours
        CALL POST_TEXT_WIDGET (Q1,Q, 2,1,1,0, 2,' ' )  !- menu pane
        DO I=1,NBUT   
          IF (TEXT(I).EQ.'# Conts') WRITE (TEXT(I+1),'(I3)')  ival1
          CALL POST_TW_2 (Q,P(1,i),text(i),opt(i))
          IF (OPT(i).eq.'K') CALL POST_MARBLE (opc(i).eq.ival,Q,P(1,I))
        ENDDO

c--------------------------- search menus ------------------------------
      ELSEIF (IOP.EQ.2) THEN  
        CALL Q_MENU (IXM,IYM,Q,P,NBUT,ITEM,XM,YM)
        IF (ITEM.LE.0) RETURN   !- no hit 
        CALL GET_CODES_BOX (CODES, OPT(ITEM),OPC(ITEM), Q, P(1,ITEM))
      ENDIF
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE POST_MENU_10 (imnu,iop,ixm,iym,codes)
c
c     This is menu #10 ...   the   'Mesh' items
c
c     if IOP = 1 then the menu is posted
c     if IOP = 2 then the op-codes for a 'hit' on one of this menus 
c     items is returned (maybe 'nul' if no 'hit')
c
      parameter (nbut=19)
      integer q(4),q1(4),p(4,nbut), opc(nbut), codes(*)
      character  text(nbut)*16, opt(nbut)*1
      data ((p(j,i),j=1,4),text(i),opt(i),opc(I),i=1,17)  
     +  /
     +   10, 10,100,16,    'Faces + Edges>'  ,'m', 17    !- as a menu

     +  ,10, 50, 80,16,    ' Sub-edges'     ,'I',  5
     +  ,10, 70, 80,16,    ' Nodes'         ,'I',  9
     +  ,10, 90,100,16,    ' -only 1 elem'  ,'I', 21 
     +  ,10,110, 80,16,    ' Node size'     ,'J', 10
     +  ,10,130, 80,16,    ' Node #'        ,'I', 11
     +  ,10,150, 80,16,    ' Element #'     ,'I',  8    ! was 13
     +  ,10,170, 80,16,    ' Material #'    ,'I',  7
     +  ,10,195, 80,16,    ' Orig mesh E'   ,'I',  16

     +  ,10,220, 80,16,    ' "Extra Lines"' ,'I',  61
     +  ,10,240, 80,16,    ' Undeformed:'   ,' ',    0
     +  ,10,260, 80,16,    ' Ring-edges'    ,'I',  64
     +  ,10,280, 80,16,    ' Ring-fill'     ,'I',  65
     +  ,10,300, 80,16,    ' Deformed:'     ,' ',    0 
     +  ,10,320, 80,16,    ' Ring-Edges'    ,'I',  62
     +  ,10,340, 80,16,    ' Ring-Fill'     ,'I',  63
     +  ,10,360, 80,16,    ' Ring-IMAT'     ,'I',  66/

      data ((p(j,i),j=1,4),text(i),opt(i),opc(I),i=18,nbut)  
     +  /10,380, 80,16,    ' # sub-facets'  ,'J',  19
     +  ,10,400, 80,16,    ' Shrink....'    ,'I',  31

     +  /
      CALL GET_LH_MENU_POS(Q,Q1)

      IF (IOP.EQ.1) THEN     !--------- post menus ------------
        CALL POST_TEXT_WIDGET (Q1,Q, 2,1, 1,0, 2,' ' )
!       CALL FILL_RECTANGLE
!    +    (Q(1)+10,Q(2)+ 33,Q(1)+Q(3)-10,Q(2)+ 33+1, 0)
!       CALL FILL_RECTANGLE
!    +    (Q(1)+10,Q(2)+118,Q(1)+Q(3)-10,Q(2)+118+1, 0)
!       CALL FILL_RECTANGLE
!    +    (Q(1)+10,Q(2)+181,Q(1)+Q(3)-10,Q(2)+181+1, 0)

      DO I=1,NBUT   
        CALL POST_TW_2 (Q,P(1,i),text(i),opt(i))
c        CALL POST_TEXT_WIDGET (Q,P(1,I),
c     +    2,1,0,1, 0,TEXT(I) )

          IF (OPT(i).EQ.'I') THEN
            J = OPC(I)
c           IF (J.EQ.5.or.J.EQ.9.or.J.EQ.8.or.J.EQ.11) THEN
            CALL SET_CV (OPC(I), ICOL, 5)    !- get current contour type
            CALL POST_COLOUR_DOT (ICOL, Q, P(1,I) )
          ENDIF
        ENDDO

c--------------------------- search menus ------------------------------
      ELSEIF (IOP.EQ.2) THEN  
        CALL Q_MENU (IXM,IYM,Q,P,NBUT,ITEM,XM,YM)
        IF (ITEM.LE.0) RETURN   !- no hit 
        CALL GET_CODES_BOX (CODES, OPT(ITEM),OPC(ITEM), Q, P(1,ITEM))
      ENDIF
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE POST_MENU_11 (imnu,iop,ixm,iym,codes)
c
c     This is menu #11 ...   * Animation *
c       - changed 'A' to 'Z' for pgplot   
      parameter (nbut=18)
      integer q(4),q1(4),p(4,nbut), opc(nbut), codes(*)
      character  text(nbut)*16, opt(nbut)*1
cv(54)
      data ((p(j,i),j=1,4),text(i),opt(i),opc(I),i=1,nbut)  
     &  /
     &   10, 20, 80,16,    'Record..'      ,' ', 0
     &  ,10, 40, 60,16,    '# frames'      ,'Z', 2
     +  ,80, 40, 30,16,    ' '             ,' ' ,0   !#frames
     &  ,10, 60, 60,16,    'Destination'   ,'Z', 3
c    &  ,10,140, 80,16,    'file-path'     ,'Z', 4
     &  ,10, 80, 60,16,    'disp code'     ,'Z', 5
     &  ,10,100, 45,18,    'd eye'         ,'Z', 6
     &  ,60,100, 45,18,    'd COA'         ,'Z', 7
     &  ,10,120, 45,18,    'd light'       ,'Z', 7
     &  ,60,120, 45,18,    'd zoom'       ,'Z', 7
     &  ,10,200, 80,18,    '  ** Go **'    ,'Z', 1
     &  ,10,220, 80,18,    'Save to disk'  ,'Z', 8
     &  ,10,240, 50,18,    'reset'         ,'Z', 0

     &  ,10,320, 50,16,    'Replay ..'     ,' ', 0
     &  ,10,340, 50,16,    'forwards'      ,'}', 0
     &  ,62,340, 45,16,    'bounce'        ,'{', 0
c  + control of the pause lengths

c... these 2 also are on the RH-toolbar
c.. note I want save & load to be menu-driven directly now.
     &  ,10,360, 50,16,    'Frame..'      ,' ' , 0
     &  ,10,380, 50,16,    'save'         ,'J'  ,301   !<- hack
     &  ,62,380, 50,16,    'load'         ,'J'  ,302
     &  /
      CALL GET_LH_MENU_POS(Q,Q1)

      DO I=1,NBUT                  !- automaticaly generated button text
        IF (TEXT(I).EQ.'# frames') then
            CALL CV_GET (54,IVAL1)       !- # contours
            WRITE (TEXT(I+1),'(I3)')  ival1
        endif
      enddo
      IF (IOP.EQ.1) THEN     !--------- post menus ------------
        CALL POST_TEXT_WIDGET (Q1,Q, 2,1,1,0, 2,' ' )  !- menu pane

      DO I=1,NBUT   
        CALL POST_TW_2 (Q,P(1,i),text(i),opt(i))  !- zap titles
c      CALL POST_TEXT_WIDGET (Q,P(1,I), 2,1,0,1, 1,TEXT(I) )
      ENDDO

c--------------------------- search menus ------------------------------
      ELSEIF (IOP.EQ.2) THEN  
        CALL Q_MENU (IXM,IYM,Q,P,NBUT,ITEM,XM,YM)
        IF (ITEM.LE.0) RETURN   !- no hit 
        CALL GET_CODES_BOX (CODES, OPT(ITEM),OPC(ITEM), Q, P(1,ITEM))
      ENDIF
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE POST_MENU_12 (imnu,iop,ixm,iym,codes)
c
c     This is menu #13 ...   * Help Menu *
c
      parameter (nbut=6)
      integer q(4),q1(4),p(4,nbut), opc(nbut), codes(*)
      character  text(nbut)*16, opt(nbut)*1
      integer buffer
      data ((p(j,i),j=1,4),text(i),opt(i),opc(I),i=1,nbut)  
     +  /
     +   10, 20, 80,16,    'About..'       ,'m',  13

     +  ,10,100, 80,16,    'Sorry  '       ,' ', 0 
     +  ,10,115, 80,16,    '  No   '       ,' ', 0 
     +  ,10,130, 80,16,    ' Help  '       ,' ', 0 
     +  ,10,145, 80,16,    ' Yet   '       ,' ', 0 
     +  ,10,160, 80,16,    'Availaible'    ,' ', 0 
     +  /
      data buffer/-1/

      CALL GET_LH_MENU_POS(Q,Q1)
      q(1) = q(1) + q(3)

      IF (IOP.EQ.1) THEN     !--------- post menus ------------
        IF (BUFFER.EQ.-1)
     &  CALL GET_SCREEN_BLOCK (Q(1),Q(2),Q(1)+Q(3),Q(2)+Q(4),buffer)
        CALL POST_TEXT_WIDGET (Q1,Q, 2,1, 1,0, 2,' ' )

      DO I=1,NBUT   
        CALL POST_TW_2 (Q,P(1,i),text(i),opt(i))  !- zap titles
c       CALL POST_TEXT_WIDGET (Q,P(1,I), 2,1,0,1, 1,TEXT(I) )
      ENDDO

c-------------------------- un-post menu -------------------------------
      ELSEIF (IOP.eq.3) THEN
        CALL UNPOST_MENU (Q,BUFFER)
        RETURN

c--------------------------- search menus ------------------------------
      ELSEIF (IOP.EQ.2) THEN  
        CALL Q_MENU (IXM,IYM,Q,P,NBUT,ITEM,XM,YM)
        IF (ITEM.LE.0) RETURN   !- no hit 
        CALL GET_CODES_BOX (CODES, OPT(ITEM),OPC(ITEM), Q, P(1,ITEM))
      ENDIF
      RETURN
      END

c-----------------------------------------------------------------------
      SUBROUTINE POST_MENU_13 (imnu,iop,ixm,iym,codes)
c
c     This is Menu # 13 . . . . . . . . . . the  DANPLOT logo
c
c     if IOP = 1 then the menu is posted
c     if IOP = 2 then the op-codes for a 'hit' on one of this menus 
c     items is returned (maybe 'nul' if no 'hit')
c
      save
      parameter (nbut=1,  ncols = 22)
      integer q(4),q1(4),p(4,nbut), opc(nbut),icols(ncols), codes(*)
      character  text(nbut)*12, opt(nbut)*1
      integer ifail_2
      integer buffer
      data ((p(j,i),j=1,4),text(i),opt(i),opc(i),i=1,nbut)  
     + / 
     +    10,  10,  80, 16,    ' ', ' ' , 0   !- 'Logo'
     + /      

      DATA ICOLS / 0, 1,2,3,4,5, 6,7,8,9,10, 11,12,13,14,15, 
     &    16,17,18,19,20, 0/
c     DATA ICOLS / 0,3,0, 4,0,5, 0,6,0, 7,0,8  ,  0, 9, 0, 10, 0,11,0/
c     DATA ICOLS / 0,1,1, 1,1,1, 1,1,0, 1,1,1  ,  1, 1, 1,  1, 0, 0,0/
!     DATA BUFFER/-1/

      call GET_LH_MENU_POS (Q,Q1)
      IF (IOP.EQ.1) THEN    !----- post the menu ----------
        IF (BUFFER.GT.0) THEN
          CALL RESTORE_SCREEN_BLOCK (Q(1),Q(2),BUFFER,0,IFAIL_2)
        ELSE

! - consider a different colour for this pane's background
        CALL POST_TEXT_WIDGET (q1,q, 2,1, 1,0, 2,' ' )     !- background
        DO I=1,NBUT 
          CALL POST_TEXT_WIDGET (q,p(1,i), 2, 1, 0,1, 0,TEXT(I) )
        ENDDO   
c................ the logo bit .....................
!       CALL SET_FONT ( 107,  6., 90.,0.)   !- logo font !

c.............. try the contour palette ! .............
c.. (assumes that contour palette starts at colour #16)
c DRAW_TEXT1  will enlarge the font and rotate by 90 deg.
c   '20' is where the contour colours start
        DO I=1,NCOLS
          if (icols(i).le.0) then
            icol=abs(icols(i))
          else
            ICOL = 20 + ICOLS(I)
          endif
          CALL DRAW_TEXT1 ('Danplot',q(1)+q(3)-60+I
     &                              ,q(2)+q(4)-90+I  , icol)
        ENDDO
c       ... now save the menu image
!       CALL GET_SCREEN_BLOCK (Q(1),Q(2),q(1)+Q(3),q(2)+Q(4),buffer)
      ENDIF

c--------------------------- search menus ------------------------------
      ELSEIF (IOP.EQ.2) THEN  
        CALL Q_MENU (IXM,IYM,Q,P,NBUT,ITEM,XM,YM)
        IF (ITEM.LE.0) RETURN   !- no hit 
        CALL GET_CODES_BOX (CODES, OPT(ITEM),OPC(ITEM), Q, P(1,ITEM))
      ENDIF
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE POST_MENU_14 (imnu,iop,ixm,iym,codes)
c
c     This is menu #14 ...   the DANPLOT Configuration options
c
      parameter (nbut=8)
      integer q(4),q1(4),p(4,nbut), opc(nbut), codes(*)
      character  text(nbut)*16, opt(nbut)*1
      integer buffer
      data ((p(j,i),j=1,4),text(i),opt(i),opc(I),i=1,nbut)  
     +  /
     +   10, 10, 80,18,    'Print. setup..' ,'O',  0
     +  ,10, 30, 80,18,    'SVGA mode >>'   ,'m', 15

     +  ,10, 50, 70,18,    'Menu color'    ,'.',  9    ! (was 19)
     +  ,10, 70, 70,18,    'Menu font..'   ,'|',  1
     +  ,10, 90, 70,18,    'Font size..'   ,'|',  2

     +  ,10,110, 80,18,    'Flash..'       ,'z', 0 
     +  ,10,130, 80,18,    'Beep..'        ,'z', 0 
     +  ,10,150, 80,18,    'Save Status'   ,'Y', 0    !- ie the CV table

     +  /
      DATA BUFFER/-1/
      CALL GET_LH_MENU_POS(Q,Q1)
      Q(1) = Q(1) + Q(3)        !-- move one 'pane' across
      Q(4) = 150+30             !- and make a little shorter

C--------------------------- Post the menus ----------------------------
      IF (IOP.EQ.1) THEN
        IF (BUFFER.EQ.-1)   !- ie if never posted yet
     &  CALL GET_SCREEN_BLOCK (Q(1),Q(2),q(1)+Q(3),q(2)+Q(4),buffer)
        CALL POST_TEXT_WIDGET (Q1,Q, 2,1, 1,0, 2,' ' )    !- pane
        DO I=1,NBUT   
          CALL POST_TEXT_WIDGET (Q,P(1,I), 2,1,0,1, 1,TEXT(I) )
        ENDDO

c-------------------------- un-post menu -------------------------------
      ELSEIF (IOP.eq.3) THEN
        CALL UNPOST_MENU (Q,BUFFER)
        RETURN

c--------------------------- search menus ------------------------------
      ELSEIF (IOP.EQ.2) THEN  
        CALL Q_MENU (IXM,IYM,Q,P,NBUT,ITEM,XM,YM)
        IF (ITEM.LE.0) RETURN   !- no hit 
        CALL GET_CODES_BOX (CODES, OPT(ITEM),OPC(ITEM), Q, P(1,ITEM))
c        IF (CHAR(CODES(1)).EQ.'|') THEN     !- change menu font
c          CALL STANDARD_MENU_FONT(IVAL)  !( do outside ?? )
c          CODES(1) = 0
c        ENDIF
      ENDIF
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE POST_MENU_15 (imnu,iop,ixm,iym,codes)
C
C     This is (transient) menu #15 ...   the   SVGA   modes menu
C
      save
      parameter (nbut=20)
      integer q(4),q1(4),p(4,nbut) ,opc(nbut), codes(*)
      character      text(nbut)*15, opt(nbut)*1

      integer igx(40), igy(40), igm(40)
      
      integer igc(40)
      logical igb(40)
      logical built 
      integer buffer
      data built /.false./
      data ((p(j,i),j=1,4),text(i),opt(i),opc(i),i=1,16)  
     +  /
     +   10, 10,120,16,    '- SVGA modes -'   ,' ',    0

     +  ,10, 50,120, 0,    'n/a'                ,'F',46001
     +  ,10, 70,120, 0,    'n/a'                ,'F',46002
     +  ,10, 90,120, 0,    'n/a'                ,'F',46003
     +  ,10,110,120, 0,    'n/a'                ,'F',46004
     +  ,10,130,120, 0,    'n/a'                ,'F',46005

     +  ,10,150,120, 0,    'n/a'                ,'F',46006
     +  ,10,170,120, 0,    'n/a'                ,'F',46007
     +  ,10,190,120, 0,    'n/a'                ,'F',46008
     +  ,10,210,120, 0,    'n/a'                ,'F',46009
     +  ,10,230,120, 0,    'n/a'                ,'F',46010

     +  ,10,250,120, 0,    'n/a'                ,'F',46011 
     +  ,10,270,120, 0,    'n/a'                ,'F',46012
     +  ,10,290,120, 0,    'n/a'                ,'F',46013
     +  ,10,310,120, 0,    'n/a'                ,'F',46014
     +  ,10,330,120, 0,    'n/a'                ,'F',46015/

      data ((p(j,i),j=1,4),text(i),opt(i),opc(i),i=17,nbut)  
     +  /10,350,120, 0,    'n/a'                ,'F',46016
     +  ,10,370,120, 0,    'n/a'                ,'F',46017
     +  ,10,390,120, 0,    'n/a'                ,'F',46018
     +  ,10,410,120, 0,    'n/a'                ,'F',46018
     +  /
      data buffer /-1/

c------------------------ build menus ----------------------------------
      IF (.NOT.BUILT) THEN
        CALL GET_GRAPHICS_MODES( IGX,IGY,IGC,IGM,IGB) 
        DO I=1, 19     !- 19 = max # of SVGA modes at one time
C.. Hmm surely I can create the utton xyXY here too.
          IBUT = I+1                   !- which button number to set
          IF (IGX(I).EQ.-1) goto 2     !- reached end-of-list
          P (4,IBUT) = 18              !- give it a real depth ?
          nbits = nint (log(real(igc(i)))/ log(2.) )
          WRITE (TEXT(IBUT),'(i4,a,i4,a,i2)')
     &     IGX(I), ' ',IGY(I), ' ', nbits 
        ENDDO
    2   Nmodes = I

C-----------------------------------------------------------------------
c.. nice to auto both width and height of 'pull-downs'
        CALL GET_LH_MENU_POS(Q,Q1)
        Q(1) = Q(1) + 2*Q(3)         !- move two panes across
        Q(3) = Q(3)+20               !- .. a bit wider
        Q(4) = p(2,nmodes+1) + 10    !- shrink depth menu to fit
        BUILT = .TRUE.               !- all done
      ENDIF

c------------------------ post menus -----------------------------------

      IF (IOP.EQ.1) THEN     
        IF (BUFFER.EQ.-1)
     &  CALL GET_SCREEN_BLOCK (Q(1),Q(2),Q(1)+Q(3),Q(2)+Q(4),buffer)

        CALL POST_TEXT_WIDGET (Q1,Q, 2,1,1,0, 2,' ' )      !- menu pane
c       CALL STANDARD_MENU_FONT (0)    
c       CALL SET_FONT ( 101,  .8, 0.,0.)   !(slightly smaller?)
        DO I=1,NBUT   

        IF (OPT(I).EQ.' ') THEN          !- do titles differently
          CALL POST_TEXT_WIDGET (Q,P(1,i), 2,0, 1,0, 0,text(i) )
        ELSE
          CALL POST_TEXT_WIDGET (Q,P(1,i), 2,1, 1,0, 1,text(i) )
        ENDIF
        ENDDO
c............. radio buttons  'marbles' .........
        CALL SET_CV (46, IVAL, 5)    !- get current mode
        DO I=1,Nmodes
          IF (OPC(i)/1000.eq.46) THEN
            CALL POST_MARBLE (mod(opc(i),1000).eq.ival,Q,P(1,I))
          ENDIF
        ENDDO
        RETURN

c-------------------------- un-post menu -------------------------------
      ELSEIF (IOP.EQ.3) THEN
        CALL UNPOST_MENU (Q,BUFFER)
        RETURN

c--------------------------- search menus ------------------------------
c.. I think it is bettet to return 'set',1024,768,256 , here so we have
c.. an explicit mode and not just a code (so later un-encode)
c   (so main prog stores these and just calls 'set_hi_res_mode')
c.. hence this menu could sort the modes and tabulate 
C.. as column headers = 16 col, 256 col, 15bit, 16bit, 24bit
c.. and create (aligned) buttons '640x480' below 
c--- a fill-in form with 'OK' button is best

      ELSEIF (IOP.EQ.2) THEN  
        CALL Q_MENU (IXM,IYM,Q,P,NBUT,ITEM,XM,YM)
        IF (ITEM.LE.0) RETURN   !- no hit 
        CALL GET_CODES_BOX (CODES, OPT(ITEM),OPC(ITEM), Q, P(1,ITEM))

c  ( careful if only 'testing' where the mouse is )
c       IF (CHAR(codes(1)).EQ.'V') THEN  
c         CALL SET_CV (46,ival  ,1)               !- set new value
c         codes(1) = 0
c       ENDIF

      ENDIF    !- post /inquire
      END

C-----------------------------------------------------------------------
      SUBROUTINE POST_MENU_16 (imnu,iop,ixm,iym,codes)
c
c     This is (transient) menu #16 ...   the   PALETTES
c
      SAVE
      PARAMETER (NBUT=23)
      INTEGER Q(4),Q1(4),P(4,NBUT) , OPC(NBUT), codes(*)
      CHARACTER  TEXT(NBUT)*14, OPT(NBUT)*1
      integer buffer

      DATA ((P(J,I),J=1,4),TEXT(I),OPT(I),OPC(I),I=1,16)  
     +  /
     +   10, 10, 90,14,    'Presets..'     ,' ',  0
     +  ,10, 25, 90,14,    'Default'       ,'.', 11       ! 0->16
     +  ,10, 40, 90,14,    'Rainbow'       ,'.', 12   ! contour ..
     +  ,10, 55, 90,14,    'Hot-iron'      ,'.', 13   !
     +  ,10, 70, 90,14,    'Spectrum'      ,'.', 14   !
     +  ,10, 85, 90,14,    '<Patran>'      ,'.', 21   !
     +  ,10,100, 90,14,    'Blue-> yel'    ,'.', 22   !
     +  ,10,115, 90,14,    'Cyan->b->red'  ,'.', 23   !
     +  ,10,130, 90,14,    'Gray'          ,'.', 25   !
     +  ,10,145, 90,14,    'User-defined'  ,'.', 24   !
     +  ,10,160, 90,14,    'Random'        ,'.', 16   !
     +  ,10,175, 90,14,    'Shade..'       ,'I', 201  !- via 'items' ?
     + , 10,190, 90,14,    '..query'       ,'.', 17      !- query palette
     + , 10,205, 90,14,    'Standard VGA'  ,'.', 0    !- 256 cols
     + , 10,220, 90,14,    'Interp 2 cols' ,'.', 26   !- contour? 
     + , 10,235, 90,14,    'shade Mats.'   ,'.', 27/   !-

      DATA ((P(J,I),J=1,4),TEXT(I),OPT(I),OPC(I),I=17,NBUT)  
     +  /10,300, 90,14,    'Adjust..'      ,' ',  0   ! #11 ?
     +  ,10,315, 90,14,    'Reverse'       ,'.',  2      !c
     +  ,10,330, 90,14,    'Invert'        ,'.',  3      !c
     +  ,10,345, 90,14,    'B <-> W'       ,'.',  4    ! just 0&15   
     +  ,10,360, 90,14,    'Swap evens'    ,'.',  5      !contour?
     +  ,10,375, 90,14,    'redefine..'    ,'I', 202   ! (203/204 ??)
     +  ,10,390, 90,14,    'Random..'      ,'I', 203   ! Random-one
     +  /
      data buffer /-1/
      CALL GET_LH_MENU_POS(Q,Q1)
c     Q(1) = 619-120          !-- move  this pane across to the RHS
c---------------------------- post menus -------------------------------
      IF (IOP.EQ.1) THEN
        IF (BUFFER.EQ.-1)
     &  CALL GET_SCREEN_BLOCK (Q(1),Q(2),Q(1)+Q(3),Q(2)+Q(4),buffer)
        CALL POST_TEXT_WIDGET (Q1,Q,  2,1, 1,0, 2,' ' )
        DO I=1,NBUT   
          CALL POST_TW_2 (Q,P(1,i),text(i),opt(i))
        ENDDO

c-------------------------- un-post menu -------------------------------
      ELSEIF (IOP.EQ.3) THEN
        CALL UNPOST_MENU (Q,BUFFER)
        RETURN

c--------------------------- search menus ------------------------------
      ELSEIF (IOP.EQ.2) THEN  
        CALL Q_MENU (IXM,IYM,Q,P,NBUT,ITEM,XM,YM)
        IF (ITEM.LE.0) RETURN   !- no hit 
        CALL GET_CODES_BOX (CODES, OPT(ITEM),OPC(ITEM), Q, P(1,ITEM))
      ENDIF
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE POST_MENU_17 (imnu,iop,ixm,iym,codes)
C
C     This is (transient) menu #17 ...   the   FACES & EDGES menu
C
      save
      parameter (nbut=40)
      integer q(4),q1(4),p(4,nbut) ,opc(nbut), codes(*)
      character      text(nbut)*15, opt(nbut)*1
      integer buffer

      data ((p(j,i),j=1,4),text(i),opt(i),opc(i),i=1,13)  
     &  /
     &   10, 10,100,16,    'face & edges'       ,' ',    0

c    &  ,10, 30, 70,16,    'Invis'              ,' ',   0
c    &  ,10, 50, 70,16,    'Solid'              ,' ',   0
     &  ,10, 90, 70,16,    'Material'           ,' ',   0
     &  ,10,110, 70,16,    'Facet '             ,' ',   0
     &  ,10,130, 70,16,    'Front/back'         ,' ',   0
     &  ,10,150, 70,16,    'Glass '             ,' ',   0
     &  ,10,170, 70,16,    'Node #'             ,' ',   0
     &  ,10,190, 70,16,    'Element #'          ,' ',   0
     &  ,10,210, 70,16,    'nodal b/w'          ,' ',   0
     &  ,10,230, 70,16,    'chess?'             ,' ',   0
     &  ,10,250, 70,16,    'chess'              ,' ',   0 
     &  ,10,270, 70,16,    'shaded'             ,' ',   0
     &  ,10,290, 70,16,    'Matshade'           ,' ',   0
     &  ,10,310, 70,16,    'z-depth'            ,' ',   0/


      data ((p(j,i),j=1,4),text(i),opt(i),opc(i),i=14,25)  
c    &  /87, 30, 16,16,    ' '                  ,'F', -1
c    &  ,87, 50, 16,16,    ' '                  ,'F',  1500
     &  /87, 90, 16,16,    ' '                  ,'F',  1501
     &  ,87,110, 16,16,    ' '                  ,'F',  1502
     &  ,87,130, 16,16,    ' '                  ,'F',  1503
     &  ,87,150, 16,16,    ' '                  ,'F',  1504
     &  ,87,170, 16,16,    ' '                  ,'F',  1505
     &  ,87,190, 16,16,    ' '                  ,'F',  1506
     &  ,87,210, 16,16,    ' '                  ,'F',  1507
     &  ,87,230, 16,16,    ' '                  ,'F',  1508
     &  ,87,250, 16,16,    ' '                  ,'F',  1509
     &  ,87,270, 16,16,    ' '                  ,'F',  1510
     &  ,87,290, 16,16,    ' '                  ,'F',  1523
     &  ,87,310, 16,16,    ' '                  ,'F',  1512/
         
      data ((p(j,i),j=1,4),text(i),opt(i),opc(i),i=26,nbut)  
c    & /100, 30, 16,16,    ' '                  ,'F', -1
c    & ,100, 50, 16,16,    ' '                  ,'F', 3500
     & /100, 90, 16,16,    ' '                  ,'F', 3501
     & ,100,110, 16,16,    ' '                  ,'F', 3502
     & ,100,130, 16,16,    ' '                  ,'F', 3503
     & ,100,150, 16,16,    ' '                  ,'F', 3504
     & ,100,170, 16,16,    ' '                  ,'F', 3505
     & ,100,190, 16,16,    ' '                  ,'F', 3506
     & ,100,210, 16,16,    ' '                  ,'F', 3507
     & ,100,230, 16,16,    ' '                  ,'F', 3508
     & ,100,250, 16,16,    ' '                  ,'F', 3509
     & ,100,270, 16,16,    ' '                  ,'F', 3510
     & ,100,290, 16,16,    ' '                  ,'F', 3523
     & ,100,310, 16,16,    ' '                  ,'F', 3512

     &  ,20, 70, 60,16,    ' + color..'         ,' ',  0  
     & , 87, 70, 16,16,    'F'                  ,'I',  1  ! CV() -faces
     & ,100, 70, 16,16,    'E'                  ,'I',  3  ! CV() -edges
     &  /
      data buffer/-1/
      CALL GET_LH_MENU_POS(Q,Q1)
      q(1) = q(1) + q(3)
      q(4) = 360          !- shrink

c------------------------ post menus -----------------------------------
      IF (IOP.EQ.1) THEN     
        IF (BUFFER.EQ.-1)
     &  CALL GET_SCREEN_BLOCK (Q(1),Q(2),Q(1)+Q(3),Q(2)+Q(4),buffer)
        CALL POST_TEXT_WIDGET (Q1,Q, 2,1,1,0, 2,' ' )      !- menu pane
        DO I=1,NBUT   

        IF (OPT(I).EQ.' '.or. opt(i).eq.'F')  THEN 
          CALL POST_TEXT_WIDGET (Q,P(1,i), 2,0, 1,0, 0,text(i) )   !- plain
        ELSE
          CALL POST_TEXT_WIDGET (Q,P(1,i), 2,1, 1,0, 1,text(i) )   !- +bdr
c          IF (I.EQ.5)      !- *just a test*
c     +    CALL POST_TEXT_WIDGET (Q,P(1,i), 2,1, 0,1, 1,text(i) )   !- bdr
        ENDIF
        ENDDO
c............. radio buttons  'marbles' .........
        CALL SET_CV (1, IVAL1, 5)    !- current face type
        CALL SET_CV (3, IVAL2, 5)    !- current edge type
        DO I=1,nbut
          if (opt(i).ne.'F') cycle
          iop1=OPC(I)/1000 ; iop2=mod(opc(i),1000)
          IF (iop1.eq.1) THEN
            CALL POST_MARBLE (iop2.eq.ival1,Q,P(1,I) ) 
!           print*,'A:',iop1, iop2,ival1
          ELSEIF (iop1.eq.3) THEN
!           print*,'B:',iop1, iop2,ival2
            CALL POST_MARBLE (iop2.eq.ival2,Q,P(1,I) ) 
          ENDIF
        ENDDO

c-------------------------- un-post menu -------------------------------
      ELSEIF (IOP.EQ.3) THEN
        CALL UNPOST_MENU (Q,BUFFER)
        RETURN

c--------------------------- search menus ------------------------------
      ELSEIF (IOP.EQ.2) THEN  
        CALL Q_MENU (IXM,IYM,Q,P,NBUT,ITEM,XM,YM)
        IF (ITEM.LE.0) RETURN   !- no hit 
        CALL GET_CODES_BOX (CODES, OPT(ITEM),OPC(ITEM), Q, P(1,ITEM))

c  ( careful if only 'testing' where the mouse is )
c        IF (CHAR(codes(1)).EQ.'c') THEN     !---------- 'faces' ---------
C         CALL SET_CV (1, IVAL, 1)    !- set new value
C         codes(1) = 0
c        ELSEIF (CHAR(codes(1)).EQ.'|') THEN !---------- 'edges' ---------
c          CALL SET_CV (3, IVAL, 1)    !- set new value
c          codes(1) = 0
c        ENDIF     !- special buttons
      ENDIF    !- post / query / inquire
      END

C-----------------------------------------------------------------------
      SUBROUTINE POST_MENU_18 (imnu,iop,ixm,iym,codes)
c
c     This is  # 18  EDIT : 'Mirroring'
c
      parameter (nbut=12)
      integer q(4),q1(4),p(4,nbut), opc(nbut), codes(*)
      character  text(nbut)*16, opt(nbut)*1
      integer buffer
      data ((p(j,i),j=1,4),text(i),opt(i),opc(I),i=1,nbut)  
     &  /
     &   10, 10, 80,18,    'MIRROR:'       ,' '  ,0
     &  ,10, 30, 20,18,    'x'             ,'R' ,1
     &  ,40, 30, 20,18,    'y'             ,'R' ,2
     &  ,70, 30, 20,18,    'z'             ,'R' ,3
     &  ,10, 50, 20,18,    'X'             ,'R' ,4
     &  ,40, 50, 20,18,    'Y'             ,'R' ,5
     &  ,70, 50, 20,18,    'Z'             ,'R' ,6
                                              
     &  ,10, 90, 80,18,    'UN-MIRROR:'    ,' '  ,0
     &  ,10,110, 30,14,    '1/2'           ,'R' ,7
     &  ,45,110, 30,14,    '1/4'           ,'R' ,8
     &  ,80,110, 30,14,    '1/8'           ,'R' ,9
     &  ,10,130, 30,14,    'typed'         ,'R' ,10

     &  /
      DATA BUFFER/-1/
      CALL GET_LH_MENU_POS(Q,Q1)
      Q(1) = Q(1) + Q(3)                 !-- move one 'pane' across
      Q(4) = P(2,nbut)+P(4,nbut) + 20    !- autosize the menu-length

C--------------------------- Post the menus ----------------------------
      IF (IOP.EQ.1) THEN
        IF (BUFFER.EQ.-1)
     &  CALL GET_SCREEN_BLOCK (Q(1),Q(2),q(1)+Q(3),q(2)+Q(4),buffer)
        CALL POST_TEXT_WIDGET (Q1,Q, 2,1, 1,0, 2,' ' )    !- pane
        DO I=1,NBUT   
          CALL POST_TEXT_WIDGET (Q,P(1,I), 2,1,0,1, 1,TEXT(I) )
        ENDDO

c-------------------------- un-post menu -------------------------------
      ELSEIF (IOP.EQ.3) THEN
        CALL UNPOST_MENU (Q,BUFFER)
        RETURN

c--------------------------- search menus ------------------------------
      ELSEIF (IOP.EQ.2) THEN  
        CALL Q_MENU (IXM,IYM,Q,P,NBUT,ITEM,XM,YM)
        IF (ITEM.LE.0) RETURN   !- no hit 
        CALL GET_CODES_BOX (CODES, OPT(ITEM),OPC(ITEM), Q, P(1,ITEM))
      ENDIF
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE POST_MENU_19 (imnu,iop,ixm,iym,codes)
c
c     This is  # 19  EDIT : 'Shift + Scale Mesh (+rotate)
c
      parameter (nbut=5)
      integer q(4),q1(4),p(4,nbut), opc(nbut), codes(*)        
      character  text(nbut)*16, opt(nbut)*1
      integer buffer
      data ((p(j,i),j=1,4),text(i),opt(i),opc(I),i=1,nbut)  
     +  /

     +   10, 10, 80,25,    'x->y->z'       ,'a' ,0
     +  , 5, 40, 80,25,    'Scale mesh'    ,'R'  ,55
     +  , 5, 70, 80,25,    'Shift mesh'    ,'R'  ,56
     +  , 5,100, 80,25,    'Rotate -y '    ,'R'  ,61    !- v.new option
     +  , 5,130, 80,25,    '*split elems'  ,'R'  ,60    !- new option
C.. do morph to 3d here too ?

     +  /
      DATA BUFFER/-1/
      CALL GET_LH_MENU_POS(Q,Q1)
      Q(1) = Q(1) + Q(3)                 !-- move one 'pane' across
      Q(4) = P(2,nbut)+P(4,nbut) + 20    !- autosize the menu-length

C--------------------------- Post the menus ----------------------------
      IF (IOP.EQ.1) THEN
        IF (BUFFER.EQ.-1)
     &  CALL GET_SCREEN_BLOCK (Q(1),Q(2),q(1)+Q(3),q(2)+Q(4),buffer)
        CALL POST_TEXT_WIDGET (Q1,Q, 2,1, 1,0, 2,' ' )    !- pane
        DO I=1,NBUT   
          CALL POST_TEXT_WIDGET (Q,P(1,I), 2,1,0,1, 1,TEXT(I) )
        ENDDO

c-------------------------- un-post menu -------------------------------
      ELSEIF (IOP.EQ.3) THEN
        CALL UNPOST_MENU (Q,BUFFER)
        RETURN

c--------------------------- search menus ------------------------------
      ELSEIF (IOP.EQ.2) THEN  
        CALL Q_MENU (IXM,IYM,Q,P,NBUT,ITEM,XM,YM)
        IF (ITEM.LE.0) RETURN   !- no hit 
        CALL GET_CODES_BOX (CODES, OPT(ITEM),OPC(ITEM), Q, P(1,ITEM))
      ENDIF
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE POST_MENU_20 (imnu,iop,ixm,iym,codes)
c
c     This is  # 20  EDIT : 'Nodes + Elements' (eg. del orphans)
c
      parameter (nbut=8)
      integer q(4),q1(4),p(4,nbut), opc(nbut), codes(*)
      character  text(nbut)*16, opt(nbut)*1
      integer buffer
      data ((p(j,i),j=1,4),text(i),opt(i),opc(I),i=1,nbut)  
     +  /
     +    5, 10,100,25,    'join coinc n'   ,'R'  ,57
     +  , 5, 40,100,25,    'del orphan n'   ,'R'  ,58
     +  , 5, 70,100,25,    'del zero Area'  ,'R'  ,59

     +  , 5,110,100,25,    'strip facets'   ,'R'  ,100
     +  , 5,140,100,25,    'del int facets' ,'R'  ,101
     +  , 5,170,100,25,    'facets 2 elems' ,'R'  ,102
     +  , 5,200,100,25,    'strip edges'    ,'R'  ,103
     +  , 5,230,100,25,    'strip edges-min','R'  ,104
     +  /
      DATA BUFFER/-1/
      CALL GET_LH_MENU_POS(Q,Q1)
      Q(1) = Q(1) + Q(3)                 !-- move one 'pane' across
      Q(4) = P(2,nbut)+P(4,nbut) + 20    !- autosize the menu-length

C--------------------------- Post the menus ----------------------------
      IF (IOP.EQ.1) THEN
        IF (BUFFER.EQ.-1)
     &  CALL GET_SCREEN_BLOCK (Q(1),Q(2),q(1)+Q(3),q(2)+Q(4),buffer)
        CALL POST_TEXT_WIDGET (Q1,Q, 2,1, 1,0, 2,' ' )    !- pane
        DO I=1,NBUT   
          CALL POST_TEXT_WIDGET (Q,P(1,I), 2,1,0,1, 1,TEXT(I) )
        ENDDO

c-------------------------- un-post menu -------------------------------
      ELSEIF (IOP.EQ.3) THEN
        CALL UNPOST_MENU (Q,BUFFER)
        RETURN

c--------------------------- search menus ------------------------------
      ELSEIF (IOP.EQ.2) THEN  
        CALL Q_MENU (IXM,IYM,Q,P,NBUT,ITEM,XM,YM)
        IF (ITEM.LE.0) RETURN   !- no hit 
        CALL GET_CODES_BOX (CODES, OPT(ITEM),OPC(ITEM), Q, P(1,ITEM))
      ENDIF
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE POST_MENU_21 (imnu,iop,ixm,iym,codes)
c
c     This is  # 21  EDIT : 'Loads + Displacements' (eg. Harden)
c
      parameter (nbut=5)
      integer q(4),q1(4),p(4,nbut), opc(nbut), codes(*)
      character  text(nbut)*16, opt(nbut)*1
      integer buffer
      data ((p(j,i),j=1,4),text(i),opt(i),opc(I),i=1,nbut)  
     +  /
     +    5, 10, 80,25,    'Add disps'     ,'R'  ,51
     +  , 5, 40, 80,25,    'Sum loads'     ,'R'  ,52
     +  , 5, 70, 80,25,    'Diff. loads'   ,'R'  ,53
     +  , 5,100, 80,25,    'Calc Normals'  ,'R'  ,54
     +  , 5,140, 80,25,    '*Calc Volumes' ,'R'  ,201   !- new option
     +  /
      DATA BUFFER/-1/
      CALL GET_LH_MENU_POS(Q,Q1)
      Q(1) = Q(1) + Q(3)                 !-- move one 'pane' across
      Q(4) = P(2,nbut)+P(4,nbut) + 20    !- autosize the menu-length

C--------------------------- Post the menus ----------------------------
      IF (IOP.EQ.1) THEN
        IF (BUFFER.EQ.-1)
     &  CALL GET_SCREEN_BLOCK (Q(1),Q(2),q(1)+Q(3),q(2)+Q(4),buffer)
        CALL POST_TEXT_WIDGET (Q1,Q, 2,1, 1,0, 2,' ' )    !- pane
        DO I=1,NBUT   
          CALL POST_TEXT_WIDGET (Q,P(1,I), 2,1,0,1, 1,TEXT(I) )
        ENDDO

c-------------------------- un-post menu -------------------------------
      ELSEIF (IOP.EQ.3) THEN
        CALL UNPOST_MENU (Q,BUFFER)
        RETURN

c--------------------------- search menus ------------------------------
      ELSEIF (IOP.EQ.2) THEN  
        CALL Q_MENU (IXM,IYM,Q,P,NBUT,ITEM,XM,YM)
        IF (ITEM.LE.0) RETURN   !- no hit 
        CALL GET_CODES_BOX (CODES, OPT(ITEM),OPC(ITEM), Q, P(1,ITEM))
      ENDIF
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE POST_MENU_22 (imnu,iop,ixm,iym,codes)
c
c     This is  # 22  EDIT : 'Materials' ..eg Glassing + connectivity
c
      parameter (nbut=5)
      integer q(4),q1(4),p(4,nbut), opc(nbut), codes(*)
      character  text(nbut)*16, opt(nbut)*1
      integer buffer
      data ((p(j,i),j=1,4),text(i),opt(i),opc(I),i=1,nbut)  
     +  /
     +   10, 10, 80,25,    'Set Marked'    ,'I' ,503    ! zap Materials
     +  ,10, 40, 80,25,    'Hide Mat.'     ,'I' ,501    ! zap Materials
     +  ,10, 70, 80,25,    'UnHide Mat.'   ,'I' ,502    ! and restore
     +  ,10,100, 80,25,    'Del_Invis_Mats','R'  ,64
     +  ,10,130, 80,25,    'Connectivity'  ,'R'  ,50
     +  /
      DATA BUFFER/-1/
      CALL GET_LH_MENU_POS(Q,Q1)
      Q(1) = Q(1) + Q(3)                 !-- move one 'pane' across
      Q(4) = P(2,nbut)+P(4,nbut) + 20    !- autosize the menu-length

C--------------------------- Post the menus ----------------------------
      IF (IOP.EQ.1) THEN
        IF (BUFFER.EQ.-1)
     &  CALL GET_SCREEN_BLOCK (Q(1),Q(2),q(1)+Q(3),q(2)+Q(4),buffer)
        CALL POST_TEXT_WIDGET (Q1,Q, 2,1, 1,0, 2,' ' )    !- pane
        DO I=1,NBUT   
          CALL POST_TEXT_WIDGET (Q,P(1,I), 2,1,0,1, 1,TEXT(I) )
        ENDDO

c-------------------------- un-post menu -------------------------------
      ELSEIF (IOP.EQ.3) THEN
        CALL UNPOST_MENU (Q,BUFFER)
        RETURN

c--------------------------- search menus ------------------------------
      ELSEIF (IOP.EQ.2) THEN  
        CALL Q_MENU (IXM,IYM,Q,P,NBUT,ITEM,XM,YM)
        IF (ITEM.LE.0) RETURN   !- no hit 
        CALL GET_CODES_BOX (CODES, OPT(ITEM),OPC(ITEM), Q, P(1,ITEM))
      ENDIF
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE POST_MENU_23 (imnu,iop,ixm,iym,codes)
c
c     This is  # 23  EDIT : 'Miscilaneous' (eg. ???)
c
      parameter (nbut=1)
      integer q(4),q1(4),p(4,nbut), opc(nbut), codes(*)
      character  text(nbut)*16, opt(nbut)*1
      integer buffer
      data ((p(j,i),j=1,4),text(i),opt(i),opc(I),i=1,nbut)  
     +  /
     +   10, 10, 80,25,    'x->y->z'       ,'a' ,0
     +  /
      DATA BUFFER/-1/
      CALL GET_LH_MENU_POS(Q,Q1)
      Q(1) = Q(1) + Q(3)                 !-- move one 'pane' across
      Q(4) = P(2,nbut)+P(4,nbut) + 20    !- autosize the menu-length

C--------------------------- Post the menus ----------------------------
      IF (IOP.EQ.1) THEN
        IF (BUFFER.EQ.-1)
     &  CALL GET_SCREEN_BLOCK (Q(1),Q(2),q(1)+Q(3),q(2)+Q(4),buffer)
        CALL POST_TEXT_WIDGET (Q1,Q, 2,1, 1,0, 2,' ' )    !- pane
        DO I=1,NBUT   
          CALL POST_TEXT_WIDGET (Q,P(1,I), 2,1,0,1, 1,TEXT(I) )
        ENDDO

c-------------------------- un-post menu -------------------------------
      ELSEIF (IOP.EQ.3) THEN
        CALL UNPOST_MENU (Q,BUFFER)
        RETURN

c--------------------------- search menus ------------------------------
      ELSEIF (IOP.EQ.2) THEN  
        CALL Q_MENU (IXM,IYM,Q,P,NBUT,ITEM,XM,YM)
        IF (ITEM.LE.0) RETURN   !- no hit 
        CALL GET_CODES_BOX (CODES, OPT(ITEM),OPC(ITEM), Q, P(1,ITEM))
      ENDIF
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE POST_MENU_24 (imnu,iop,ixm,iym,codes)
c
c     This is  # 24  'Import Mesh file'  eg. .PL .OFF .DXF
c
      parameter (nbut=10)
      integer q(4),q1(4),p(4,nbut), opc(nbut), codes(*)
      character  text(nbut)*16, opt(nbut)*1
      integer buffer
      data ((p(j,i),j=1,4),text(i),opt(i),opc(I),i=1,nbut)  
     +  /
     +   10, 10, 90,25,    '.PL  Danplot'       ,'#' ,101
     +  ,10, 40, 90,25,    '.OFF Object FF'     ,'#' ,102
     +  ,10,100, 90,25,    '.NFF Neutral FF'    ,'#' ,103
     +  ,10,130, 90,25,    '.SRF (sw)'          ,'#' ,104
     +  ,10,160, 90,25,    '.3D  3D Edir'       ,'#' ,105
     +  ,10,190, 90,25,    '.CAR Carolus'       ,'#' ,106
     +  ,10,220, 90,25,    '.TEC Tecplot'       ,'#' ,107
     +  ,10,250, 90,25,    '.DXF Autocad'       ,'#' ,108
     +  ,10,280, 90,25,    '.RAY Rayshade'      ,'#' ,128
     +  ,10,300, 90,25,    '.FV  Femview'       ,'#' ,129
c.. 18-12-96 MESHSM1 too ?
c.. UCD too ?
     +  /
      DATA BUFFER/-1/
      CALL GET_LH_MENU_POS(Q,Q1)
      Q(1) = Q(1) + Q(3)                 !-- move one 'pane' across
      Q(4) = P(2,nbut)+P(4,nbut) + 20    !- autosize the menu-length

C--------------------------- Post the menus ----------------------------
      IF (IOP.EQ.1) THEN
        IF (BUFFER.EQ.-1)
     &  CALL GET_SCREEN_BLOCK (Q(1),Q(2),q(1)+Q(3),q(2)+Q(4),buffer)
        CALL POST_TEXT_WIDGET (Q1,Q, 2,1, 1,0, 2,' ' )    !- pane
        DO I=1,NBUT   
          CALL POST_TEXT_WIDGET (Q,P(1,I), 2,1,0,1, 1,TEXT(I) )
        ENDDO

c-------------------------- un-post menu -------------------------------
      ELSEIF (IOP.EQ.3) THEN
        CALL UNPOST_MENU (Q,BUFFER)
        RETURN

c--------------------------- search menus ------------------------------
      ELSEIF (IOP.EQ.2) THEN  
        CALL Q_MENU (IXM,IYM,Q,P,NBUT,ITEM,XM,YM)
        IF (ITEM.LE.0) RETURN   !- no hit 
        CALL GET_CODES_BOX (CODES, OPT(ITEM),OPC(ITEM), Q, P(1,ITEM))
      ENDIF
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE POST_MENU_25 (imnu,iop,ixm,iym,codes)
c
c     This is  # 25  'Export Mesh file'  eg. .PL .OFF .DXF
c
      parameter (nbut=11)
      integer q(4),q1(4),p(4,nbut), opc(nbut), codes(*)
      character  text(nbut)*16, opt(nbut)*1
      integer buffer
      data ((p(j,i),j=1,4),text(i),opt(i),opc(I),i=1,nbut)  
     +  /
     +   10, 10, 80,25,    '.PL  Danplot'       ,'#' ,201
     +  ,10, 40, 80,25,    '.OFF Object FF'     ,'#' ,202
     +  ,10, 70, 80,25,    '.NFF Neutral FF'    ,'#' ,203
     +  ,10,100, 80,25,    '.SRF (sw)'          ,'#' ,204
     +  ,10,130, 80,25,    '.3D  3D Edir'       ,'#' ,205
     +  ,10,160, 80,25,    '.CAR Carolus'       ,'#' ,206
     +  ,10,190, 80,25,    '.TEC Tecplot'       ,'#' ,207
     +  ,10,220, 80,25,    '.DXF Autocad'       ,'#' ,208
     +  ,10,250, 80,25,    '.RAY Rayshade'      ,'#' ,209
     +  ,10,280, 80,25,    '.GRD Surfer'        ,'#' ,210
     +  ,10,300, 90,25,    '.FV  Femview'       ,'#' ,211
c.. MESHSM1 too ?
c.. UCD too ?
     +  /
      DATA BUFFER/-1/
      CALL GET_LH_MENU_POS(Q,Q1)
      Q(1) = Q(1) + Q(3)                 !-- move one 'pane' across
      Q(4) = P(2,nbut)+P(4,nbut) + 20    !- autosize the menu-length

C--------------------------- Post the menus ----------------------------
      IF (IOP.EQ.1) THEN
        IF (BUFFER.EQ.-1)
     &  CALL GET_SCREEN_BLOCK (Q(1),Q(2),q(1)+Q(3),q(2)+Q(4),buffer)
        CALL POST_TEXT_WIDGET (Q1,Q, 2,1, 1,0, 2,' ' )    !- pane
        DO I=1,NBUT   
          CALL POST_TEXT_WIDGET (Q,P(1,I), 2,1,0,1, 1,TEXT(I) )
        ENDDO

c-------------------------- un-post menu -------------------------------
      ELSEIF (IOP.eq.3) THEN
        CALL UNPOST_MENU (Q,BUFFER)
        RETURN

c--------------------------- search menus ------------------------------
      ELSEIF (IOP.EQ.2) THEN  
        CALL Q_MENU (IXM,IYM,Q,P,NBUT,ITEM,XM,YM)
        IF (ITEM.LE.0) RETURN   !- no hit 
        CALL GET_CODES_BOX (CODES, OPT(ITEM),OPC(ITEM), Q, P(1,ITEM))
      ENDIF
      END

C-----------------------------------------------------------------------
      SUBROUTINE POST_MENU_26 (imnu,iop,IXM,IYM,codes)
C
C     This   is Menu # 26 . . . . . . the  PALETTE (and NUMBERS)
C
      COMMON /PALETTE/PAL
      INTEGER   PAL (3,0:255)      !- 'current' colour palette

      save
      parameter (nbut=23)
      integer q(4),q1(4),p(4,nbut) , ibc(nbut),   opc(nbut), codes(*)
      character  text(nbut)*12, opt(nbut)*1
c      DATA Q/619,40,20,415/, Q1/0,0,0,0/
       DATA Q/120,40,20,415/, Q1/0,0,0,0/

      data ((p(j,i),j=1,4),text(i),ibc(i),opt(i),opc(i),i=1,17)  
     +  /
c    +     0, 10, 20,16,   'P'   ,2   ,'m',  16    !- post-palette

     +     0, 40, 19,16,   'Off' ,1   ,'C', -1
     +  ,  0, 56, 19,16,   ' 0'  ,0   ,'C',  0
     +  ,  0, 72, 19,16,   ' 1'  ,1   ,'C',  1
     +  ,  0, 88, 19,16,   ' 2'  ,2   ,'C',  2

     +  ,  0,104, 19,16,   ' 3'  ,3   ,'C',  3
     +  ,  0,120, 19,16,   ' 4'  ,4   ,'C',  4
     +  ,  0,136, 19,16,   ' 5'  ,5   ,'C',  5
                                             
     +  ,  0,152, 19,16,   ' 6'  ,6   ,'C',  6
     +  ,  0,168, 20,16,   ' 7'  ,7   ,'C',  7
     +  ,  0,184, 19,16,   ' 8'  ,8   ,'C',  8
     +  ,  0,200, 19,16,   ' 9'  ,9   ,'C',  9
     +  ,  0,216, 19,16,   '10'  ,10  ,'C', 10

     +  ,  0,232, 19,16,   '11'  ,11  ,'C', 11
     +  ,  0,248, 19,16,   '12'  ,12  ,'C', 12
     +  ,  0,264, 19,16,   '13'  ,13  ,'C', 13
     +  ,  0,280, 19,16,   '14'  ,14  ,'C', 14
     +  ,  0,296, 19,16,   '15'  ,15  ,'C', 15/

      data ((p(j,i),j=1,4),text(i),ibc(i),opt(i),opc(i),i=18,nbut)  
     +  /  0,312, 19,16,   '16'  ,16  ,'C', 16
     +  ,  0,328, 19,16,   '17'  ,17  ,'C', 17
     +  ,  0,344, 19,16,   '18'  ,18  ,'C', 18
     +  ,  0,360, 19,16,   '19'  ,19  ,'C', 19
     +  ,  0,376, 19,16,   '20'  ,20  ,'C', 20

     +  ,  0, 10, 20,16,   'us'  ,0   ,'C', -10    !- typed value

c     +  ,  0,400, 20,16,   '<'   ,2   ,'C', -11    !-  val -1
c     +  ,  0,416, 20,16,   '>'   ,2   ,'C', -12    !-  val +1
     +  /

c--------------------------- post menu ---------------------------------
      IF (IOP.EQ.1) THEN
         call post_text_widget (Q1,Q, 2, 1, 1,0, 2,' ' )  !- pane
c... what the hell is the next line for ?
c         call draw_line (ints(q(1)), ints(q(2)),
c     +     ints(q(1)+q(3)),ints(q(2)),  1)      !- edging in white

c     call set_text_attribute ( 101,  .6, 0.,0.)   !- 'small' text
      CALL STANDARD_MENU_FONT(0)

      DO I=1,NBUT
c... Ok try and get a contrasting colour .. but weights for RGB ?
        colb = (0.35 *pal(1,ibc(i))    !- RGB-> mono p.583 Foley/vanDam
     &         +0.59 *pal(2,ibc(i)) 
     &         +0.063*pal(3,ibc(i))) /256.
        if (colb.gt.0.5) icolt = 0    ! = a 'contrasting' colour
        if (colb.le.0.5) icolt = 1
        ij=16
        CALL POST_TEXT_WIDGET (Q,P(1,I), ibc(i)-ij,icolt,  
     &     1,0, 1,TEXT(I) )
      ENDDO
      RETURN

c--------------------------- search menus ------------------------------
      ELSEIF (IOP.EQ.2) THEN  
        CALL Q_MENU (IXM,IYM,Q,P,NBUT,ITEM,XM,YM)
        IF (ITEM.LE.0) RETURN   !- no hit 
        CALL GET_CODES_BOX (CODES, OPT(ITEM),OPC(ITEM), Q, P(1,ITEM))
      ENDIF
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE POST_MENU_27 (imnu,iop,ixm,iym,codes)
c
c     Menu #27  ....                            <<<<  'Vectors'  >>>>.
c
      parameter (nbut=13)
      integer q(4),q1(4),p(4,nbut), opc(nbut), codes(*)
      character  text(nbut)*16, opt(nbut)*1
      data ((p(j,i),j=1,4),text(i),opt(i),opc(I),i=1,nbut)  
     +  /

     +   10, 50, 90,18,    ' Disp vectors'  ,'I',  12
     +  ,10, 90, 90,18,    ' Regrid lines'  ,'I',  33
     +  ,10,110, 90,18,    ' + #lines'      ,'J',  34
     +  ,10,150, 90,18,    ' Flow-nets'     ,'I',  35
     +  ,10,170, 90,18,    ' + #lines'      ,'J',  36

     +  ,10,200, 90,18,    'Stresses..'      ,' ',  0 
     +  ,10,220, 90,18,    ' S1 tensor(C)'   ,'I',  71
     +  ,10,240, 90,18,    ' S2 tensor'      ,'I',  72
     +  ,10,260, 90,18,    ' S3 tensor(T)'   ,'I',  73
     +  ,10,290, 90,18,    ' Tensile  '      ,'I',  75
     +  ,10,310, 90,18,    ' Compr.   '      ,'I',  76

c    +  ,10,300, 90,18,    ' Tmax tensor'    ,'I',  74
     +  ,10,335, 90,18,    'Auto-rescale'    ,'^',    10
     +  ,10,355, 90,18,    'Manual-scale'    ,'^',    11

     +  /
      CALL GET_LH_MENU_POS(Q,Q1)
                         
      IF (IOP.EQ.1) THEN     !--------- post menus ------------
        CALL POST_TEXT_WIDGET (Q1,Q, 2,1, 1,0, 2,' ' )     ! menu pane
        DO I=1,NBUT      !- all the buttons --

          CALL POST_TW_2 (Q,P(1,i),text(i),opt(i))
c         CALL POST_TEXT_WIDGET (Q,P(1,I),  2,1,0,1, 0,TEXT(I) )

          IF (OPT(i).EQ.'I') THEN
            J = OPC(I)
c            IF (J.EQ.5.or.J.EQ.9.or.J.EQ.8.or.J.EQ.11) THEN
              CALL SET_CV (OPC(I), ICOL, 5)    !- get current contour type
              CALL POST_COLOUR_DOT (ICOL, Q, P(1,I) )
c            ENDIF
          ENDIF
        ENDDO

c--------------------------- search menus ------------------------------
      ELSEIF (IOP.EQ.2) THEN  
        CALL Q_MENU (IXM,IYM,Q,P,NBUT,ITEM,XM,YM)
        IF (ITEM.LE.0) RETURN   !- no hit 
        CALL GET_CODES_BOX (CODES, OPT(ITEM),OPC(ITEM), Q, P(1,ITEM))
      ENDIF
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE POST_MENU_28 (imnu,iop,ixm,iym,codes)
c
c     Menu #28 ...   'Axes, Titles, Load step #, etc.'
c                  (ie. all artifacts added to the printed page)
c         
      parameter (nbut=12)
      integer q(4),q1(4),p(4,nbut), opc(nbut), codes(*)
      character  text(nbut)*16, opt(nbut)*1
      data ((p(j,i),j=1,4),text(i),opt(i),opc(I),i=1,nbut)  
     +  /
     +   10, 22, 80,18,    ' Title  '        ,'T',   1
     +  ,10, 40, 80,18,    ' subTitle'       ,'T',   2
     +  ,10, 58, 80,14,    ' title colour'   ,'I',  29
     +  ,10,102, 80,16,    ' Axes   '        ,'I',  24
     +  ,10,120, 80,16,    ' + labels'       ,'I',  27

     +  ,10,140, 80,16,    'BBox-edges'       ,'I',  25
     +  ,10,158, 80,16,    'BBox-fills'       ,'I',  26
     +  ,10,176, 80,16,    ' -%oversize'      ,'I',  23


     +  ,10,260, 80,16,    ' Load #'         ,'I',  39
     +  ,10,280, 80,16,    ' Pic # '         ,'I',  40

     +  ,10,310, 80,16,    ' Window frame'   ,'I',  28
     +  ,10,330, 80,16,    ' Cont. Legend'   ,'I',  22

     +  /
      CALL GET_LH_MENU_POS(Q,Q1)

      IF (IOP.EQ.1) THEN     !--------- post menus ------------
        CALL POST_TEXT_WIDGET (Q1,Q, 2,1, 1,0, 2,' ' )   !- menu pane
        DO I=1,NBUT   
c         CALL POST_TEXT_WIDGET (Q,P(1,I), 2,1,0,1, 0,TEXT(I) )
          CALL POST_TW_2 (Q,P(1,i),text(i),opt(i))

          IF (OPT(i).EQ.'I') THEN    !-- add 'colour'-marbles
            J = OPC(I)
c            IF (J.EQ.5.or.J.EQ.9.or.J.EQ.8.or.J.EQ.11) THEN
             CALL SET_CV (OPC(I), ICOL, 5)    !- get current contour type
             CALL POST_COLOUR_DOT (ICOL, Q, P(1,I) )
          ENDIF
        ENDDO

c--------------------------- search menus ------------------------------
      ELSEIF (IOP.EQ.2) THEN  
        CALL Q_MENU (IXM,IYM,Q,P,NBUT,ITEM,XM,YM)
        IF (ITEM.LE.0) RETURN   !- no hit 
        CALL GET_CODES_BOX (CODES, OPT(ITEM),OPC(ITEM), Q, P(1,ITEM))
      ENDIF
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE POST_MENU_29 (imnu,iop,IXM,IYM,codes)
C
C     This   is Menu # 29 . . . . . . the  PALETTE (and NUMBERS)
C        18-7-95  now 256 cols ..posted in screen centre
C        (so EXIT un-posts this menu ?)
C
      COMMON /PALETTE/PAL
      INTEGER   PAL (3,0:255)      !- 'current' colour palette

      save
      parameter (nbut1 =13, nbut=nbut1 + 256)
      integer q(4),q1(4),p(4,nbut) , ibc(nbut),   opc(nbut), codes(*)
      character  text(nbut)*12, opt(nbut)*1
      integer buffer
      logical built 
c     character label*20
      data built /.false./
c      DATA Q/619,40,20,415/, Q1/0,0,0,0/
       DATA Q/150,70,440,350/, Q1/0,0,0,0/

      data ((p(j,i),j=1,4),text(i),ibc(i),opt(i),opc(i),i=1,nbut1)  
     +  /
     +    30, 34, 32,16,   'Off' ,1   ,'C', -1
     +  , 15, 34, 16,16,   '0'   ,0   ,'C', 0

     +  , 55, 10,120,16,   '  '  ,0   ,' ',  0     !- echoed item#
     +  , 30, 10, 20,16,   '-'   ,2   ,'C', -11    !-  val -1
     +  ,180, 10, 20,16,   '+'   ,2   ,'C', -12    !-  val +1
     +  ,300, 10, 60,16,   '  '  ,0   ,' ',  0     !- echoed output
     +  ,275, 10, 20,16,   '-'   ,2   ,'C', -13    !-  val -1
     +  ,365, 10, 20,16,   '+'   ,2   ,'C', -14    !-  val +1

     +  , 45,320,256, 5,   '    ',262   ,' ',  0     !-  RGB sliders
     +  , 45,325,256, 5,   '    ',259   ,' ',  0     !-  "
     +  , 45,330,256, 5,   '    ',260   ,' ',  0     !-  "   :-)
     +  , 30,320, 10,15,   '    ',265   ,' ',  0     !-  RGB total

     +  ,350,320, 40,18,   ' OK ',2        ,'m',  -2     !-  exit

c    +     0, 10, 20,16,   'P'   ,2   ,'m',  16    !- post-palette
c    +  ,  0,376, 19,16,   '20'  ,20  ,'C', 20
     +  /
      DATA BUFFER/-1/

c------------ build menus --------------
      IF (.NOT.BUILT) THEN
        DO I=0,255
          IBW = 20
          IBH = 15
          nbut_x =20     !- number of button in x
          IBUT = I+1 + nbut1           !  =# OF 'other' buttons
          IX = MOD(I-1,nbut_x)
          IY = (I-1) /nbut_x
          P (1,IBUT) = 15 +IX*IBW
          P (2,IBUT) = 50 +IY*IBH
          P (3,IBUT) =  IBW
          P (4,IBUT) =  IBH
c          IF (MOD(I,5).EQ.0) THEN
c            WRITE(TEXT(IBUT), '(I3)')  I
c          ELSE
c            TEXT(IBUT) = '-'
c          ENDIF
          IBC (IBUT) = I
          OPT (IBUT) = 'C'
          OPC (IBUT) = I
        ENDDO
        BUILT = .TRUE.
      ENDIF

c--------------------------- post menu ---------------------------------

      IF (IOP.EQ.1) THEN
        IF (BUFFER.EQ.-1)
     &  CALL GET_SCREEN_BLOCK (Q(1),Q(2),q(1)+Q(3),q(2)+Q(4),buffer)
        CALL SET_CV (ITEM, IVAL, 6)    !- get current 'item'
c       CALL CV_GET_LABEL (ITEM,LABEL)
        CALL SET_CV (ITEM, IVAL, 5)    !- and get its current color value
        WRITE (TEXT(2),'(I4)')    ITEM
c       WRITE (TEXT(2),'(A)')    LABEL    !- hmm only got 12 characters
        WRITE (TEXT(5),'(I4)')    IVAL
        IF (IVAL.GE.0.AND.IVAL.LE.255) THEN
          P (3, 8) =  PAL(1,ival)
          P (3, 9) =  PAL(2,ival)     !- RGB components - graphicaly
          P (3,10) =  PAL(3,ival)     !- add a scale-bar ?
          IBC(11)  = IVAL             !- do in HLS too?
        ELSE
          P (3, 8) = 0
          P (3, 9) = 0
          P (3,10) = 0
          IBC(11)  = 0
        ENDIF
        DO I=0,255
          IBUT = I+1 + nbut1           !  =# Of 'other' buttons
          TEXT(IBUT) = '.'
          IF (MOD(I,5).EQ.0) WRITE(TEXT(IBUT), '(I3)')  I
        ENDDO
        IBUT = IVAL+1 + nbut1      !- mark the current with an X
        IF (ival.ge.0.and.ival.le.255) TEXT(IBUT) = 'X'

        CALL POST_TEXT_WIDGET (Q1,Q, 2, 1, 1,0, 2,' ' )  !- pane
c       call set_text_attribute ( 101,  .6, 0.,0.)   !- 'small' text
        CALL STANDARD_MENU_FONT(0)

c.. hmm I only want the smaller font for the 'little' numbers
        DO I=1,NBUT
          if (i.eq.nbut1+1)
     &    CALL SET_FONT ( 101,  .6, 0.,0.)   !(slightly smaller?)
c... Ok try and get a contrasting colour .. but weights for RGB ?
          colb = (0.35 *pal(1,ibc(i))    !- RGB-> mono p.583 Foley/vanDam
     &           +0.59 *pal(2,ibc(i)) 
     &           +0.063*pal(3,ibc(i))) /256.
          if (colb.gt.0.5) icolt = 0    ! = a 'contrasting' colour
          if (colb.le.0.5) icolt = 1
          ij=16
          CALL POST_TEXT_WIDGET (Q,P(1,I), ibc(i)-ij,icolt,  
     &       1,0, 1,TEXT(I) )
        ENDDO
        RETURN

c-------------------------- un-post menu -------------------------------
      ELSEIF (IOP.eq.3) THEN
        CALL UNPOST_MENU (Q,BUFFER)
        RETURN

c--------------------------- search menus ------------------------------
      ELSEIF (IOP.EQ.2) THEN  
        CALL Q_MENU (IXM,IYM,Q,P,NBUT,ITEM,XM,YM)
        IF (ITEM.LE.0) RETURN   !- no hit 
        CALL GET_CODES_BOX (CODES, OPT(ITEM),OPC(ITEM), Q, P(1,ITEM))
      ENDIF
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE POST_MENU_30 (imnu,iop,ixm,iym,codes)
c
c     This is menu #30 ...   the contour -able terms.
c
      parameter (nbut=7+7+9+15+2)
      integer q(4),q1(4),p(4,nbut), opc(nbut), codes(*)
      character  text(nbut)*16, opt(nbut)*1
      integer buffer
      data ((p(j,i),j=1,4),text(i),opt(i),opc(I),i=1,16)  
     +  /
c  7 coords
     +   10, 30, 50,18,    'c-rms'          ,'K',  20 
     &  ,65, 30, 40,18,    'area'           ,'K',  29 
     &  ,105,30, 16,18,    'D'              ,'K',  28 
     +  ,10, 50, 25,18,    'cx'             ,'K',  21
     +  ,40, 50, 25,18,    'cy'             ,'K',  22
     +  ,70, 50, 25,18,    'cz'             ,'K',  23
     +  ,10, 70, 25,18,    'pxy'            ,'K',  24
     +  ,40, 70, 25,18,    'pyz'            ,'K',  25
     +  ,70, 70, 25,18,    'pzx'            ,'K',  26

c  7 disps
     +  ,10,100, 50,18,    'd-rms'          ,'K',  30
     +  ,10,120, 25,18,    'dx'             ,'K',  31
     +  ,40,120, 25,18,    'dy'             ,'K',  32
     +  ,70,120, 25,18,    'dz'             ,'K',  33
     +  ,10,140, 25,18,    'qxy'            ,'K',  34
     +  ,40,140, 25,18,    'qyz'            ,'K',  35
     +  ,70,140, 25,18,    'qzx'            ,'K',  36/

      data ((p(j,i),j=1,4),text(i),opt(i),opc(I),i=17,31)  
c.. 6 = 3 + 3 strains
     +  /10,175, 25,18,    'ex'             ,'K',  41
     +  ,40,175, 25,16,    'ey'             ,'K',  42
     +  ,70,175, 25,16,    'ez'             ,'K',  43
     +  ,10,195, 25,16,    'exy'            ,'K',   44 
     +  ,40,195, 25,16,    'eyz'            ,'K',   45 
     +  ,70,195, 25,16,    'ezx'            ,'K',   46
     + , 10,215, 40,16,    'e-mean'         ,'K',  47
     + , 55,215, 40,16,    'e-shear'        ,'K',  48
     + , 10,235, 40,16,    'e-lode'         ,'K',  49

c.. 15 = 6 + 3 + 6 stress 
     +  ,10,260, 25,16,    'Sx'             ,'K',  201
     +  ,40,260, 25,16,    'Sy'             ,'K',  202
     +  ,70,260, 25,16,    'Sz'             ,'K',  203
     +  ,10,280, 25,16,    'Txy'            ,'K',  204
     +  ,40,280, 25,16,    'Tyz'            ,'K',  205
     +  ,70,280, 25,16,    'Tzx'            ,'K',  206/

      data ((p(j,i),j=1,4),text(i),opt(i),opc(I),i=32,nbut)  
     +  /10,300, 25,16,    's1'             ,'K',  211
     + , 40,300, 25,16,    's2'             ,'K',  212
     + , 70,300, 25,16,    's3'             ,'K',  213

     + , 10,320, 40,16,    'S-mean'         ,'K',  214
     + , 10,340, 40,16,    'S-shear'        ,'K',  215
     + , 10,360, 40,16,    'S-Lode'         ,'K',  216
     + , 55,320, 40,16,    'F   '           ,'K',  217
     + , 55,340, 40,16,    'MSS'            ,'K',  218
     + , 55,360, 40,16,    'FOS'            ,'K',  219

     +  /
      DATA BUFFER/-1/
      CALL GET_LH_MENU_POS(Q,Q1)
      Q(1) = Q(1) + Q(3)        !-- move one 'pane' across
      Q(4) = P(2,nbut)+P(4,nbut) + 20    !- autosize the menu-length

C--------------------------- Post the menus ----------------------------
      IF (IOP.EQ.1) THEN
        IF (BUFFER.EQ.-1)   !- ie if never posted yet
     &  CALL GET_SCREEN_BLOCK (Q(1),Q(2),q(1)+Q(3),q(2)+Q(4),buffer)
        CALL POST_TEXT_WIDGET (Q1,Q, 2,1, 1,0, 2,' ' )    !- pane
        DO I=1,NBUT   
          CALL POST_TEXT_WIDGET (Q,P(1,I), 2,1,0,1, 1,TEXT(I) )
        ENDDO

c-------------------------- un-post menu -------------------------------
      ELSEIF (IOP.eq.3) THEN
        CALL UNPOST_MENU (Q,BUFFER)
        RETURN

c--------------------------- search menus ------------------------------
      ELSEIF (IOP.EQ.2) THEN  
        CALL Q_MENU (IXM,IYM,Q,P,NBUT,ITEM,XM,YM)
        IF (ITEM.LE.0) RETURN   !- no hit 
        CALL GET_CODES_BOX (CODES, OPT(ITEM),OPC(ITEM), Q, P(1,ITEM))
      ENDIF
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE POST_MENU_99 (imnu,iop,ixm,iym,codes)
c
c     This is menu #99 ...   the   Image attributes (background etc.)
c      = 'Viewing'
c.... ** There appears to be 2 bversions of this menu !
c
c     if IOP = 1 then the menu is posted
c     if IOP = 2 then the op-codes for a 'hit' on one of this menus 
c     items is returned (maybe 'nul' if no 'hit')
c
      parameter (nbut=8)
      integer q(4),q1(4),p(4,nbut), opc(nbut), codes(*)
      character  text(nbut)*16, opt(nbut)*1
      data ((p(j,i),j=1,4),text(i),opt(i),opc(I),i=1,nbut)  
     +  /
     +   10, 55, 70,16,    'Background'    ,'I',  1
     +  ,10, 80, 70,16,    'Border'        ,'I',  1
     +  ,10,100, 80,16,    '# sub-facets'  ,'I',  19

     +  ,10,120, 80,16,    'Node size'     ,'I', 10
     +  ,10,140, 80,16,    'Element #'     ,'I', 13

     +  ,10,180, 80,16,    'Orig mesh E'   ,'I',  16
     +  ,10,200, 80,16,    'Orig mesh F'   ,'I',  16
     +  ,10,220, 80,16,    'Disp vectors'  ,'I',  12

     +  /
      CALL GET_LH_MENU_POS(Q,Q1)

      IF (IOP.EQ.1) THEN     !--------- post menus ------------
        CALL POST_TEXT_WIDGET (Q1,Q, 2,1, 1,0, 2,' ' )
c--- these next are just bar lines so we just need xlo,xhi,y (+thk)
        CALL FILL_RECTANGLE 
     +    (Q(1)+10,Q(2)+ 33,Q(1)+Q(3)-10,Q(2)+ 33+1, 0)
        CALL FILL_RECTANGLE
     +    (Q(1)+10,Q(2)+ 78,Q(1)+Q(3)-10,Q(2)+ 78+1, 0)
        CALL FILL_RECTANGLE
     +    (Q(1)+10,Q(2)+138,Q(1)+Q(3)-10,Q(2)+138+1, 0)
        CALL FILL_RECTANGLE
     +    (Q(1)+10,Q(2)+161,Q(1)+Q(3)-10,Q(2)+161+1, 0)

      DO I=1,NBUT   
        CALL POST_TEXT_WIDGET (Q,P(1,I),
     +    2,1,0,1, 0,TEXT(I) )
      ENDDO

c--------------------------- search menus ------------------------------
      ELSEIF (IOP.EQ.2) THEN  
        CALL Q_MENU (IXM,IYM,Q,P,NBUT,ITEM,XM,YM)
        IF (ITEM.LE.0) RETURN   !- no hit 
        CALL GET_CODES_BOX (CODES, OPT(ITEM),OPC(ITEM), Q, P(1,ITEM))
      ENDIF
      RETURN
      END


C-----------------------------------------------------------------------
      SUBROUTINE DANPLOT_LOGO (RES,version)
C
C     Ok need more work on this to get a *nice* logo
C
      INTEGER RES(*)
      CHARACTER VERSION*(*)
      REAL X(20),Y(20)  ,X1(2),Y1(2)
      CHARACTER LINE*80

      XS = RES(4)
      YS = RES(5)
      XW = RES(1)
      YW = RES(2)

      CALL MAKE_BOX (XS,YS,XW,YW,X,Y)
      CALL DR_PRIM (LINE,X,Y,4,  9, 2)     !- the 'extra space'
      XS = XS + 30
      YS = YS + 30
      XW = XW - 30 *2
      YW = YW - 30 *2
      CALL MAKE_BOX (XS,YS,XW,YW,X,Y)
      CALL DR_PRIM (LINE,X,Y,4, 15, 2)     !----- 'box edge' ------
      XS = XS + 3                     !>  nice to be able to do  <!
      YS = YS + 3                     !>  rounded corners  :-)   <!
      XW = XW - 3 *2
      YW = YW - 3 *2
      CALL MAKE_BOX (XS,YS,XW,YW,X,Y)
      CALL DR_PRIM (LINE,X,Y,4, 2, 2)     !- 'main box'

      X1(1) = XS  + 75
      Y1(1) = YS  + 50
      WRITE(LINE,'(A)') 'DANPLOT v3.01'
      CALL SET_FONT (107, 4.,0.,0.)
      CALL DR_PRIM (LINE,X1,Y1,1, 1, 20)     !- main title
                                      
                                   
      CALL SET_FONT (107, 2. ,0.,0.)
      X1(1) = XS +145
      Y1(1) = YS +75
      WRITE (LINE,'(A,A)') 'version',VERSION
      CALL DR_PRIM (LINE,X1,Y1,1, 15, 20)     !- version number

      CALL SET_FONT (107, 1.5,0.,0.)

      X1(1) = XS +140
      Y1(1) = YS +290
      WRITE (LINE,'(A)') 'by Dr. Dan Kidger'
      CALL DR_PRIM (LINE,x1,y1,1, 1, 20) 

      X1(1) = XS + 70
      Y1(1) = YS +310
      WRITE (LINE,'(A)') ' Dept. of Civil Engineering'
      CALL DR_PRIM (LINE,x1,y1,1, 15, 20) 

      X1(1) = XS + 70
      Y1(1) = YS +330
      WRITE (LINE,'(A)') 'University of Manchester, UK'
      CALL DR_PRIM (LINE,x1,y1,1, 15, 20) 


      RETURN
      END
C-----------------------------------------------------------------------


C-----------------------------------------------------------------------
      SUBROUTINE POST_POP_UP (string)
C
C     Pops up a message to the screen centre 
C        c. DJK 11-7-95
C     eg. 'Please Wait - Printing'
C     cf use of a %-done scale-line 
c
c     7-2-98 'OK' buttons - like windows I want to be able to supply
C      an icon (eg a '?' and an OK button that must be pressed.
c
      CHARACTER STRING*(*)
      INTEGER Q(4),P(4)
      INTEGER BUFFER
      DATA BUFFER/-1/              !- -1 no saved pane yet
      DATA Q/ 100,200,  400,50/    ! XO,YO,XW,YW
      DATA P/   0,0,    400,50/    !- relative pos. for each line of text

      call get_app_size (idev_x,idev_y)
      q(1) = (idev_x-q(3))/2    !- at the screen centre
      q(2) = (idev_y-q(4))/2

      CALL RESTORE_GRAPHICS_BANK ()    !- just make sure all is aligned
      CALL UNPOST_MENU (Q,BUFFER)

      IF (STRING .EQ.' ') RETURN     !- nothing to do

      IF (BUFFER.EQ.-1) CALL GET_SCREEN_BLOCK    !- save whats under
     &   (Q(1),Q(2),q(1)+Q(3),q(2)+Q(4),buffer)

c     IF (INDEX (STRING,'/').GE.1) THEN   !-- handle multi-line texts

      CALL SET_FONT (101,1.5,  0.,0.)    !-reset font 
      CALL POST_TEXT_WIDGET (q,p, 2,1, 1,0,  5, string)
      END

C-----------------------------------------------------------------------
      SUBROUTINE GET_MESHFILE_EXT (ITYPE, EXT)
c
c       *OBSOLETE* ?
c     Returns the File Name Extension associated with a 
c     particular file type code used by DANPLOT etc.
c      ie. the inverse of GET_FILE_TYPE() qv.
c        DJK 24-2-95
c
      IMPLICIT NONE
      INTEGER ITYPE
      CHARACTER EXT*3
      IF (ITYPE.eq.1) THEN
        EXT = 'PL '                 !  My *Original* Danplot file format
      ELSEIF (ITYPE.eq.2) THEN
        EXT = 'GEO'                ! 'Object File Format'
      ELSEIF (ITYPE.eq.3) THEN 
        EXT = 'NFF'                ! 'Neutral File Format'
      ELSEIF (ITYPE.eq.4) THEN 
        EXT = 'SRF'                ! 'SURF' ? Shareware format
      ELSEIF (ITYPE.eq.5) THEN 
        EXT = '3D'                 ! '3DEDIT' Shareware format
      ELSEIF (ITYPE.eq.6) THEN
        EXT = 'CAR'                ! Carolus's Finite Element Format
      ELSEIF (ITYPE.eq.7) THEN
        EXT = 'TEC'                ! TECPLOT (c) FE Post-Processor
      ELSEIF (ITYPE.eq.8) THEN
        EXT = 'DXF'                ! Autocad (c) Interchange format
      ELSEIF (ITYPE.eq.9) THEN
        EXT = 'RAY'                ! RAYSHADE .. ray-tracer (3nt's)

c     ELSEIF (ITYPE.eq.10) THEN
c       EXT = 'RAY'                ! POVRAY  .. ray-tracer (3nt's?)
c     ELSEIF (ITYPE.eq.99) THEN
c       EXT = 'OBJ'                !  Wavefront Object format
c     ELSEIF (ITYPE.eq.99) THEN
c       EXT = 'RAY'                ! 
c     ELSEIF (ITYPE.eq.9) THEN
c       EXT = 'RAY'                ! 

      ELSE
        PRINT*,'*** Warning : file type code unknown'// 
     &         ' (GET_MESHFILE_EXT)'
        EXT = '!!!'
      ENDIF
      RETURN
      END


C-----------------------------------------------------------------------
!  THESE ROUTINES SHOULD REALLY BE IN THE DEVICE DRIVER (e.g. draw_pgplot)
C-----------------------------------------------------------------------
      subroutine restore_graphics_bank()
c     call restore_graphics_bank@()
      end

C-----------------------------------------------------------------------
      subroutine get_screen_block(ix1,iy1,ix2,iy2,ibuffer)
c  used to grab the area under a popup so we can put it back afterwards
c     call get_screen_block@(ix1,iy1,ix2,iy2,ibuffer)
      end 

C-----------------------------------------------------------------------
      subroutine restore_screen_block (ix,iy,ibuffer,iop,ifail)
c     CALL RESTORE_SCREEN_BLOCK@ (Q(1),Q(2),BUFFER,0,IFAIL_2)
      END

C-----------------------------------------------------------------------
      subroutine fill_rectangle (ix1,iy1,ix2,iy2,icol)
!  note need to make sure pgplot routines use 4-byte floats
      real*4 :: x1,y1, x2,y2
c.. hmm I will need to reverse the y-axis somewhere

c     call clear_screen_area@ (ix1,iy1,ix2,iy2,icol)
c     call fill_rectangle@(ix1,iy1,ix2,iy2,icol)
      call PGSCI (icol)
c     print*,'rectangle, icol=',icol
      x1= 1.*ix1; x2=1.*ix2; y1=1.*iy1; y2=1.*iy2
      call PGRECT (x1,x2,y1,y2)
      end

C-----------------------------------------------------------------------
      subroutine draw_line (ix1,iy1,ix2,iy2,icol)
      real*4 :: x,y
c     call draw_line@ (ix1,iy1,ix2,iy2,icol)
      call PGSCI (icol)
c     print*,'line, icol=',icol
      x=1.*ix1;  y=1.*iy1 ; call PGMOVE(x,y)
      x=1.*ix2;  y=1.*iy2 ; call PGDRAW(x,y)
      end

C-----------------------------------------------------------------------
      subroutine fill_ellipse (ix,iy,irx,iry, icol)
      real*4 :: x,y,r
c     call fill_ellipse@ (ix,iy,irx,iry, icol)
      call PGSCI (icol)
      x=1.*ix; y=1.*iy ; r=1.*irx
      call PGCIRC(x,y,r)
      end

C-----------------------------------------------------------------------
      subroutine draw_text (text, ix,iy,icol)
!  consider here calling PGQTXT which returns teh etxt box size so we can:
!    1. autosize buttons
!    2. centre text on buttons, etc.
      character text*(*)
      real*4 :: x,y
      call PGSCI (icol)
      x= 1.*ix;  y= 1.*iy    
      call PGTEXT(x,y,text)
      end

C-----------------------------------------------------------------------
      subroutine draw_text1 (text, ix,iy,icol)
c  larger rotated text :-)
c  note the fudge to get the anchor point into the right place :-(
c  I think that this is *only*  used to draw the vertical Danplot logo?
      character text*(*)
      real*4 :: x,y, angle,fjust, dx, size

      call PGSCI (icol)
      call PGSCF(3)       !- choose Font #3 
      call PGQCH(size)
      x=5.;call PGSCH(x)  !- force the height to be 5 units
!  arghh dont use 480 explitly here !
      x= 1.*ix;   y= 480.-1.*iy;  angle=90; fjust=0.
      
!- loop with a finer resolution so we dont leave gaps
      do i=0,5
        dx= 3./real(i)
        call PGPTXT(x+dx,y,angle,fjust,text)
      enddo
      x=.8;call PGSCH(x)  !- reset the character height
      call PGSCF(1)       !- reset to the default font
      end


C-----------------------------------------------------------------------
      subroutine get_graphics_modes ( IGX,IGY,IGC,IGM,IGB)
      integer igx(40), igy(40), igm(40)
      integer igc(40)
      logical igb (40)
c     call get_graphics_modes@ ( IGX,IGY,IGC,IGM,IGB)
!  again this is yuk - I want the resolution used to be chosen at runtime
      igx(1) = 640
      igy(1) = 480
c     igx(1) = 800
c     igy(1) = 600
      igc(1) = 256
      igm(1) = 1
      igb(1) = .false.
      igx(2) = 0
      end

C-----------------------------------------------------------------------
      subroutine sound (i1,i2)
c     call sound@ (i1,i2)
      end


C-----------------------------------------------------------------------

C-----------------------------------------------------------------------
