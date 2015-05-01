C-----------------------------------------------------------------------
c        Routines for my own Postscript Driver
c
c         Dan Kidger, School of Engineering, 
c           University of Manchester, UK
c                d.kidger@man.ac.uk
C-----------------------------------------------------------------------

C-----------------------------------------------------------------------
      SUBROUTINE SELECT_PS_PRINTER (LINE,FILE, RES, TITLE,VERSION)
c
c     This initiates 'Postscript' output 
c            Dan Kidger  c.1992 
c
c     A file is opened on unit 55, and the postscript macros written to it.
c       given:
c          TYPE = paper size (eg 'A4 ','A3 ', etc.), 'A4L'=Landscape
c          FILE = filename for the output Posctscript File
c
c          RES  = the returned image size (1=xw,2=yw, 4=xo,5=yo)
c
c     The next 2 shouldn't be done here, but later by the application
C     (however we *can* write the title/version to the PS headers)
c      <    TITLE = title for the plot (usu. the input date file name) >
c          VERSION  = Danplot (tm) version number (do elsewhere ?)
c
C   15-8-94 TYPE added : eg 'A4 ', 'A3 ','A4L' (landscape)
c    7-2-98 move towards generic paper sizes.

      CHARACTER FILE*(*) ,TITLE*(*), VERSION*(*)
     &   ,LINE*(*),DATE_STAMP*20              
      EXTERNAL DATE_STAMP
      INTEGER RES(6)

      CHARACTER ORIENTATION*1, PAPER*8,POS*4
      real bl,br,bt,bb             ! non-printing borders.
      ctp = 72./25.4               !"centimetres to points"

c----------------------- get the paper size -------------------------------
c  1:  call up physical paper size as papx, papy
c  2:  define margins, standard = 10mm all round (15mm on LHS?)
c                     PhD. =15mm L and R, 40 top, 25 (+10 title) bottom
c if 'PHD' force paper to A4 ?
c     eg 'A4 P nologo 10,10,40,25 clip' perhaps?

!-- April 2004 - I would like to use Environment variables here. 
!  call danfe_getarg() and parse for token=value pairs.
!  once done this then parse the one given on as an argument here.
!  so
!    SIZE=A4   {A0,A1,A2,A3,A4,LETTER,PHD}
!    ORIENT=P  {L,P}
!    COLOUR=    so perhaps grayscale here ?
!    BORDER=    control of the border.   {PHD}
!    LOGO=0    Danplot Logo and datestamp {0,1} 

c.. set defaults ?
      PAPER='A4'               !- A4 paper (use LETTER in the USA?)
      ORIENTATION='L'          !- landscape (Portrait/ Seascape,..)
      bl = 10.                 !- left margin (borders) in milimeters
      br = 10.                 !- right
      bt = 10.                 !- top
      bb = 15.                 !- bottom
      POS='    '               !- left/right/ top-third , etc.

c     CALL COUNT_TOKENS (LINE, NTOKS,', ')                   !- better - 
      IEND=0
      CALL GET_NEXT_TOKEN (LINE, IBEG,IEND,', ')
        IF (ibeg.lt.0) goto 123                     !- EXIT
        PAPER= LINE(IBEG:IEND)
        IF (PAPER.eq.'PHD') THEN
          PAPER='A4'
          ORIENTATION='L'
          BL= 15.
          BR= 15.
          BT= 40.            !- the 'binding edge'
          BB= 15  !25.       !- (I think that these are now larger=40+10+10+10)
        ENDIF 
      CALL GET_NEXT_TOKEN (LINE, IBEG,IEND,', ')
        IF (ibeg.lt.0) goto 123
        ORIENTATION= LINE(IBEG:IEND)
        call to_upper(ORIENTATION)

      CALL GET_NEXT_TOKEN (LINE, IBEG,IEND,', ')
        IF (ibeg.lt.0) goto 123
        POS= LINE(IBEG:IEND)              !- sub-picture number

c.. token #4,5,.. = ? clipping, ?nologo


!--   set the size of the paper and allow for printing margins --
  123 continue
      CALL GET_PAPER_SIZE (PAPER, IXPAP,IYPAP)
      res(1) = ixpap         ! image width
      res(2) = iypap         ! image height
      IF (ORIENTATION.eq.'L') then   !----- landscape mode ----------
        res(1) = iypap     
        res(2) = ixpap     
      ELSEIF (ORIENTATION.eq.'S') then   !----- seascape mode ---
        res(1) = iypap                   !- actualy need to invert
        res(2) = ixpap     
      endif


      res(1) = res(1)-int(ctp*(bl+br))         ! image width
      res(2) = res(2)-int(ctp*(bt+bb))         ! image height
      res(4) = int(ctp*bl)                     ! bottom left corner
      res(5) = int(ctp*bb)                     !   of the image

!--------------- sub-picture position --------------------
!  obselete but perhaps interesting as an example
c eg a token pos:
c     1 =top half, 2=bot half, 3=LHS,4=RHS
c     5= top-left quarter, etc. (cf Abaqus/Post)
c     hence  morph  res(), and bl,br,bt,bb
      IF (POS.EQ.'t'.or.pos.eq.'top') then     !- 2 on a page vertically
        res(2)=res(2)/2 
        res(5)=res(5)+res(2)
      ELSEIF (POS.EQ.'b'.or.pos.eq.'bot') then
        res(2)=res(2)/2 

      ELSEIF (POS.EQ.'l') then      ! 2 on a page horizontally
        res(1)=res(1)/2 
        res(4)=res(4)+res(1)
      ELSEIF (POS.EQ.'r') then
        res(1)=res(1)/2 

      ELSEIF (POS.EQ.'3t') then      !- vertical thirds
        res(2)=res(2)/3 
        res(5)=res(5)+2*res(2)
      ELSEIF (POS.EQ.'3c') then
        res(2)=res(2)/3 
        res(5)=res(5)+res(2)
      ELSEIF (POS.EQ.'3b') then
        res(2)=res(2)/3 

      ELSEIF (POS.EQ.'3l') then      !- horiz. thirds
        res(1)=res(1)/3 
        res(4)=res(4)+2*res(1)
      ELSEIF (POS.EQ.'3m') then
        res(1)=res(1)/3 
        res(4)=res(4)+res(1)
      ELSEIF (POS.EQ.'3r') then
        res(1)=res(1)/3 

      ENDIF

c............ set the 'default' paper to A4 ..................
c My original method ..
c      res(1) =  595
c      res(2) =  842
c      res(1) =    ctp* (210 -10 -10)
c      res(2) =    ctp* (297 -10 -10) !- ie. 5mm all round
c      res(4) =    ctp* ( 15 )
c      res(5) =    ctp* ( 10 )

c---------------------- open the PostScript file -----------------------
      IO = 55
      OPEN (IO,FILE=FILE,iostat=ios)
      if (ios.ne.0) stop '** Failed to open postscript file'
C-------------------- write the Postscript headers ---------------------
      WRITE(IO,'(A)')  '%!PS-Adobe-2.0 EPSF-1.2'
      WRITE(IO,'(4A)') '%%Document title: ',TITLE,'  v:',VERSION
c      WRITE(IO,'(A,4I6)') 
c     &  '%%BoundingBox:', res(4),res(5), res(1)+res(4),res(2)+res(5)   
      WRITE(IO,'(A,4I6)') 
     &  '%%BoundingBox:', 0,0, ixpap, iypap
      WRITE(IO,'(A)')
     & '%%Creator: Danplot Finite Element Postprocessor'
     &,'%%Creationdate: '//DATE_STAMP()
     &,'%%DocumentFonts:'
c    &,'%%ColorUsage: None !'
c      WRITE(IO,'(A,4I6)') 
c     &  '%%TileBox:', res(4),res(5), res(1)+res(4),res(2)+res(5)   
      WRITE(IO,'(A)')  '%%EndComments'

c--------------------- define my macros (Prolog) -----------------------

      CALL WRITE_PS_MACROS (IO)

c------------------- set initial sizes and scaling ---------------------
      WRITE(IO,'(A)')  '%%BeginSetup'
      WRITE(IO,'(A)') 
     &    ' gsave'                                   !- old transformation
     &   ,' 0 setgray' 
     &   ,' .31 setlinewidth'                        !- linewidth (was .3)
c is 0.31 a bit too narrow ?
c    &   ,'/Helvetica findfont 5 scalefont setfont'  !- basic font ?
c    &   ,' 72 .4 mul 72 .4 mul translate'           !- shift origin

c.. really the dx is the paper dx, not the (smaller) image box width.

      IF (ORIENTATION.eq.'L') then   !----- rotate into landscape mode ----------
        WRITE(IO,'(2F12.2,A)')
     &    1.*ixpap,0.     ,' translate 90 rotate'
      ELSEIF (ORIENTATION.eq.'S') then   !---- Seascape --
        WRITE(IO,'(A,2F12.2,A)')
     &    '-90 rotate', -1.*iypap,0.     ,' translate'
      ENDIF

      WRITE(IO,'(A)')  '%%EndSetup'

c     - now draw the window frame then clip to it?
c
c.. this next bit is perhaps better as a DR_PRIM call
c   cos maybe I can clip to VGA limits too ?
c     WRITE(IO,'(4f8,2,a)')
C    &   RES(4),res(5), res(4)+res(1), res(5)+res(2),' clippathbox'
c
c-------------------------- end of setup -------------------------------
c      IF (TYPE.eq.'PHD') 
c     &  CALL DR_PHD_LOGO (RES, TITLE)

c-----------------------------------------------------------------------
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE GET_PAPER_SIZE (TYPE, IXPAP,IYPAP)
C
c     This returns the size in points of a variety of paper sizes
c       Dan Kidger 24-1-98
c   Notes:
C   - Based on common paper sizes of...
c      a3 (11.694" x 16.528"),   a4 (8.264" x 11.694")
c      a5 (5.847" x 8.264")     ,a6 (4.125" x 5.847"), b4 (9.847" x 13.917")
C      letter (8.5" x 11"), halfletter (5.5" x 8.5"),  note (7.5" x 10"),
c      legal (8.5" x 14"),  foolscap (8.5" x 13")  , 11x17 (11" x 17")
c   - If not found then return some sort of warning 
C     (and use A4 as default?)
c   - What other sizes might be useful ?  6" square, 4"x3" ?
c
      CHARACTER TYPE*(*)
      INTEGER IXPAP,IYPAP

      PARAMETER (NPAPS=13)
      character paper(NPAPS)*8
      integer width(NPAPS), height(NPAPS)
      data (paper(i),width(i),height(i),i=1,NPAPS) /
     & 'a1      ',2380,1684,'a2      ', 1190,1684,'a3      ',  842,1190, 
     & 'a4      ', 595, 842,'a5      ',  421, 595,'a6      ', 297,421,
     & 'b4      ', 709,1002,
     & 'let     ',612, 792, 'hlet    ', 396, 612,   
     & 'note    ',540, 720, 'legal   ', 612,1008,
     & 'fcap    ',612,936,  '11x7    ', 612, 792
     & /

      IF (TYPE.EQ.'?') THEN
C...   perhaps list the possibilities ?
        WRITE (*,'(4(I4,A,2I6,2X))') 
     &    (I,PAPER(I),WIDTH(I),HEIGHT(I), I=1,NPAPS)
        PRINT*,' Which paper size?'
        READ(*,'(A)') TYPE
      ENDIF
C.. overload and allow an explicit paper size ?  eg '11x8'
c.. avoid lowercase/uppercase problems?
      CALL TO_LOWER(TYPE)
      DO I=1, NPAPS
        IF (TYPE.EQ.PAPER(I)) then
          IXPAP=  width(i)
          IYPAP= height(i)
          RETURN
        ENDIF
      ENDDO
      print*,'** paper size not found!'
      ixpap=72 *3       !- default =3 inch square ?
      iypap=72 *3
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE WRITE_PS_MACROS (IO)
C
C     This simply writes out the Postscript macros as used by DR_PS ()
C      eg. from DANPLOT and DANGINO (+DANSLIP)
C
      WRITE(IO,'(A)') '%%BeginProlog' 
     +,'/sc { setrgbcolor } def' 
     +,'/sf { findfont exch scalefont setfont} def' 
     +,'/Helv { /Helvetica sf} def' 
     +,'/dl { newpath 3 1 roll moveto {rlineto} repeat stroke } def' 
     +,'/dp { newpath 3 1 roll moveto {rlineto} repeat closepath'//
     +    ' stroke } def' 
     +,'/fp { newpath 3 1 roll moveto {rlineto} repeat closepath'//
     +    ' fill } def' 
     +,'/cp {gsave newpath 3 1 roll moveto {rlineto} repeat closepath'//
     +    ' clip } def'             !- clip to a polygon
     +,'/dc { newpath 0 360 arc stroke } def'             !- but diameter ??
     +,'/sp { showpage } def'               !- but disable if EPS file

      WRITE(IO,'(A)')
c    +,'/fc { newpath 0 360 arc fill } def'
ccc- the best?     +,'/fc { newpath pop 4 2 roll 0 360 arc fill } def'
     + '/fc { newpath pop pop 0 360 arc fill } def'

c    +,'/slen {stringwidth pop 0 exch sub 0 } def'        !- -ve text-length
     +,'/dt  { moveto show } def'                    !- text-LH
     +,'/dtc { moveto dup stringwidth pop 2 div'//   !- text-centre
     +    ' 0 exch sub 0 rmoveto show } def'
     +,'/dtr { moveto dup stringwidth pop'//         !- text-right
     +    ' 0 exch sub 0 rmoveto show } def'
     &   ,'%%EndProlog'
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE CLOSE_PS_PRINTER ()
C   
C     This closes the PostScript output on unit 55
C      (what about eps files?)
C       
      WRITE(55,'(A)')  '%%Trailer'
c    +      ,'%   "showpage" will be inhibited for EPS files'
     +      ,'%   finally show the page'  
     +      ,'showpage'
     +      ,'grestore '            !- back out of rescaling
      CLOSE(55)
      RETURN
      END


C-----------------------------------------------------------------------
      SUBROUTINE DR_PS (TEXT,X,Y,N,ICOL, IOP)
C
C     This writes PostScript Output to unit 55 for lines,fill_areas etc.
C     IOP = 1 un-closed line
C           2    closed line = polygon
C           5   filled polygon  
c       (  10   filled circle  )
C          20   text string   (auto font & scale)
c          21/22 = centred/LHjust text
c..... written 29-11-93 
c      13-8-94 only do setrgbcolor if it has changed

      REAL X(*),Y(*)
      INTEGER N, ICOL,IOP, ICOL_OLD
      COMMON /PALETTE/PAL
      INTEGER PAL(3,0:255)
      external istr_end

      CHARACTER FORMAT*50, ACTION*5, TEXT*(*) !*80
      DATA ICOL_OLD/-1/     !- remember the 'last' colour
      DATA TXT_SIZE/-1./    !- remember the 'last' text size
 
      IF (ICOL.NE.ICOL_OLD) WRITE (55,'(3f5.2,a)')
     &           (PAL(J,ICOL)/255.,J=1,3) , ' sc'
      ICOL_OLD = ICOL


      IF (IOP.EQ. 1) THEN
         ACTION = ' dp'        !/* draw-polygon */
      ELSEIF (IOP.EQ. 2) THEN
         ACTION = ' fp'        !/* fill-polygon */
      ELSEIF (IOP.EQ. 3) THEN
         ACTION = ' dl'        !/* draw-line */
      ELSEIF (IOP.EQ.10) THEN
         ACTION = ' fc'        !/* fill-circle */
      ELSEIF (IOP.EQ.20) THEN
         ACTION = ' dt'        !/* draw-text */
      ELSEIF (IOP.EQ.21) THEN
         ACTION = ' dtc'       !/* draw-text-centered */
      ELSEIF (IOP.EQ.22) THEN
         ACTION = ' dtr'       !/* draw-text-RH-just */
      ELSEIF (IOP.EQ.40) THEN
         ACTION = ' cp'        !/* clip-to-polygon */
      ELSEIF (IOP.EQ.41) THEN
         ACTION = ' ucp'       !/* un-clip (back to full image) */
      ELSE
         call myerror (2,'unknown opcode in DR_PS')
      ENDIF

c--------------------- handle text strings -----------------------------
c.. 7-4-95 factor-down by 72. (into points) !
c.. nice to split so we define the font only when it changes (cf pencol)
      IF (IOP.ge.20.and.iop.le.29) THEN
        IF (ABS(X(2)-TXT_SIZE) .GT. 1.E-4) THEN
          TXT_SIZE = X(2)   !/72.
          WRITE (55,'(f10.2,a)') X(2), ' /Helvetica sf '
        ENDIF
        WRITE (55,'(3A,2f10.2,A)')       !-x2=font size !
c    +    ' (',TEXT(1:LENG(TEXT)),') ',X(1),Y(1), X(2)/72., ACTION
c    &    ' (',TEXT(1:LENG(TEXT)),') ',X(1),Y(1)   , ACTION
     &    ' (',TEXT(1:istr_end(TEXT)),') ',X(1),Y(1)   , ACTION

c----------------- polygons (use relative coords :-) -------------------
c.. note that MAX_DAT is the max number of tokens on a line
c  7-2-98 cp= set clip region to polygon
      ELSEIF (ACTION.eq.' dp'.or.ACTION.eq.' fp'
     &    .or.ACTION.eq.' dl'.or.ACTION.eq.' cp') THEN
        N_LINE = 8                   !- max # per line
        N_REP = (2*N-1)/N_LINE       !- number of repeated lines
        N_REM = 2*N-N_REP*N_LINE     !- number on the last line

        IF (N_REP.NE.0) THEN
c next changed from I2 to I4 - DJK 01-02-01
          WRITE (FORMAT,'(A,I4,A,I2,A,I2,A)')
     &     '(', N_REP,'(',N_LINE,'F10.2/),',   N_REM ,'F10.2, I4,A)'
        ELSE
          WRITE (FORMAT,'(A,I2,A)')
     &    '(',     N_REM ,'F10.2, I4,A)'
        ENDIF
        WRITE (55,FORMAT) (X(I)-X(I+1),Y(I)-Y(I+1),I=1,N-1), X(n),Y(n)
     &         , N-1, ACTION             ! note relative coords

c----------------------- filled circles --------------------------
c.. 24-1-97 no-op for now ? cos its doesn't seem to work.
c      ELSEIF (ACTION.eq.' fc') THEN
c        WRITE (55,'(4f10.2,A)')       !- x(2) = dot diamemter x/y
c     &   x(1),y(1), x(2),x(2),  ACTION  
c    &   x(2),x(2), x(1),y(1),  ACTION  

c----------------------- restore clipping --------------------------
      ELSEIF (ACTION.eq.' ucp') THEN
        WRITE (55,'(A)') 'grestore'

c----------------------- all other primitives --------------------------
c.. note that MAX_DAT is the max number of tokens on a line
      ELSE
        N_LINE = 8                   !- max # per line
        N_REP = (2*N-1)/N_LINE       !- number of repeated lines
        N_REM = 2*N-N_REP*N_LINE     !- number on the last line

        IF (N_REP.NE.0) THEN
          WRITE (FORMAT,'(A,I2,A,I2,A,I2,A)')
     &     '(', N_REP,'(',N_LINE,'F10.2/),',   N_REM ,'F9.2, I4,A)'
        ELSE
          WRITE (FORMAT,'(A,I2,A)')
     &     '(',                                N_REM ,'F9.2, I4,A)'
        ENDIF
        WRITE (55,FORMAT) (X(I),Y(I),I=1,N), N-1, ACTION
      ENDIF
      RETURN
      END

c-----------------------------------------------------------------------
      SUBROUTINE WRITE_PS_LOGOS (IO,RES,TITLE,VERSION)
C
C     This writes the frame and Danplot logo to the PS output file
C      (assuming A4 portrait)
C       .. we can use RES() to position more accurately
C
!  should honor a flag to skip any logo, datestamp, etc.
      CHARACTER  TITLE*40, FILE2*40, VERSION*(*)
     &        ,DATE_STAMP*20, BS
      EXTERNAL DATE_STAMP
      INTEGER RES(6)
      REAL X(5),Y(5)
      external istr_end
      BS='\\'             ! only used to sed a dos pathname into unix style

      FILE2 = TITLE
      DO I=1,istr_end(FILE2)     !- reverse backslashes for postscript output
        IF (FILE2(I:I).EQ.BS) FILE2(I:I)='/'
      ENDDO

c.... so a centred title is:
c  x= res(4) + res(1)/2., y=baseline, height
c
c      WRITE(IO,'(A)') ' '  !----------- the data file name ------------
c     + ,' 575 2 div 45 moveto'
c     + ,' ('//FILE2(1:LENG(FILE2))//') dup '
c     +  //'stringwidth pop 2 div 0 exch sub 0 rmoveto show '

c.. maybe nice to 2x width text for the filename
       X(1) = res(4)+res(1)/2.   ! 575 / 2
       Y(1) = res(5)-10.         !  45   !- yuk hard codes the frame ?
       X(2) = 10.
       CALL DR_PRIM (FILE2,x,y,1,0,21)      !-- the title (21=centered)
c                                           !- 0='black' (cf black paper ?

c      WRITE(IO,'(A)') ' '  !--------- the current time and date -------
c     + ,'/Helvetica findfont  6 scalefont setfont 575 50 moveto'
c     + ,' ('//DATE_STAMP()//') dup '
c     +   //'stringwidth pop 0 exch sub 0 rmoveto show'

       X(1) = res(4)+res(1)    ! 575 / 2
       Y(1) = res(5)-6.        ! 45
       X(2) = 5.
       CALL DR_PRIM (DATE_STAMP(),x,y,1,0,22)  

       X(1) = res(4)  
       Y(1) = res(5)-5.  
       X(2) = 4.
       CALL DR_PRIM (version,x,y,1,0,20)  
      END

c-----------------------------------------------------------------------
      SUBROUTINE WRITE_PS_DANPLOT_LOGO (IO,RES,VERSION)
c
C     This writes the Danplot logo to the bottom Left of the Frame
c    moved to here 7-2-98  DJK
c
      CHARACTER VERSION*(*)
      INTEGER RES(6)
c----------------- the Danplot logo -----------------
       WRITE (IO,'(A,/,2i7,A)') 'gsave', res(4)+4, res(5)+4,' translate'
       WRITE(IO,'(A)') 
     &  '0 setgray'           ! always black (or use a contrasting colour?)
     & ,'  /Times-BoldItalic findfont 10 scalefont setfont'
     & ,'  /rays { 0 1.5 179 {gsave rotate 0 0 moveto 108 0 '
     &     //'lineto stroke grestore  } for } def'
     & ,'   0 0 translate .25 setlinewidth  1 1 scale newpath'
     & ,'   0  0 moveto (Danplot '         //') true charpath clip'
c    & ,'  newpath 30 -15 translate rays'    !(was 50 -15)
     & ,'  newpath  0 -10  translate rays'    !(was 50 -15)
     & ,'grestore' 

      END

c-----------------------------------------------------------------------

