C-----------------------------------------------------------------------
c     Routines for my own Autocad DXF Driver
C-----------------------------------------------------------------------
c Feb '96
c
c
c
c

C-----------------------------------------------------------------------
      SUBROUTINE SELECT_DXF_PRINTER (TYPE,FILE, RES, TITLE,VERSION)
c
c     This initiates 'DXF' output (ie. vector-based for CAD packages)
c     (modelled on my Postscript Driver)
c
c       where:
c          TYPE = paper size (eg 'A4 ','A3 ', etc.), 'A4L'=Landscape
c          FILE = filename for the output Posctscript File
c          RES  = the returned image size (1=xw,2=yw, 4=xo,5=yo)
c
c     The next 2 shouldn't be done here, but later by the application
c      <    TITLE = title for the plot (usu. the input date file name) >
c          VERSION  = Danplot (tm) version number (do elsewhere ?)

      CHARACTER FILE*(*) ,TITLE*(*), VERSION*(*)
     &   ,TYPE*3,DATE_STAMP*20              
      EXTERNAL DATE_STAMP
      INTEGER RES(6)
      LOGICAL LANDSCAPE
      ctp = 72./25.4               !"centimetres to points"
c----------------------- get the paper size -------------------------------
      LANDSCAPE = (type(3:3).EQ.'L')
      IF (TYPE.EQ.'   ') LANDSCAPE = .true.

c............ set the 'default' paper to A4 ..................
      res(1) =    nint(ctp* (210 -10 -10))
      res(2) =    nint(ctp* (297 -10 -10)) !- ie. 5mm all round
      res(4) =    nint(ctp* ( 15 ))
      res(5) =    nint(ctp* ( 10 ))

      IF (TYPE(1:2).EQ.'MY') THEN   !- no-op 'cos we have sizes already!
c..... maybe allow units in mm or inches ?

      ELSEIF (TYPE(1:2).EQ.'A4'.or.TYPE(1:2).EQ.'  ') THEN
c        res(1) =    ( 8. -.4) *72      !- available drawing area
c        res(2) =    (11.3-.4) *72        !(was 11")
c        res(4) = .4 *72
c        res(5) = .4 *72

      ELSEIF (TYPE(1:2).EQ.'A3') THEN
        res(1) =    nint(ctp* (297 -5 -5))
        res(2) =    nint(ctp* (420 -5 -5)) !- ie. 5mm all round
        res(4) =    nint(ctp* ( 5 ))
        res(5) =    nint(ctp* ( 5 ))

      ELSEIF (TYPE(1:3).EQ.'PHD') THEN      !- Landscape with 15mm borders
c..... as L,R,T,B = 15.,15.,40., 25. (10mm hanging title)
c..... but beware of the shift of .4" that is done here :-(

        LANDSCAPE = .true.
        res(1) =    nint(ctp* (210  -40 -15))
        res(2) =    nint(ctp* (297  -15 -15))
        res(4) =    nint(ctp* ( 15 ))
        res(5) =    nint(ctp* ( 10 ))
      ELSE
        PRINT*,TYPE,' is an unknown PAPER size.. "A4" assumed'
      ENDIF

c---------------------- open the PostScript file -----------------------
      IO = 55
      OPEN (IO,FILE=FILE)

C-------------------- write the DXF headers ---------------------
c   need to write out *verbose* headers ? inc. line styles and list of 
c      layers ?
c   also can include Danplot(tm) somewhere ?
c      WRITE(IO,'(I3/,4A)') 999,'%%Document title: ',TITLE,'  v:',VERSION
c      WRITE(IO,'(I3/,A,4I6)') 999,
c     +  '%%BoundingBox:', res(4),res(5), res(1)+res(4),res(2)+res(5)   
c      WRITE(IO,'(I3/,A)')
c     + 999,'%%Creator: Danplot Finite Element Postprocessor'
c     +,999,'%%Creationdate: '//DATE_STAMP()

c.. dont forget we also need to define some layers and their default 
c   colours & line-styles via a TABLES module

      WRITE(IO,'(I3/,A)') 0,'ENTITIES'  !- so start of the drawing.

      RETURN
      END


C-----------------------------------------------------------------------
      SUBROUTINE CLOSE_DXF_PRINTER ()
C   
C     This closes the DXF output on unit 55
C       
      IO=55
      WRITE(IO,'(I3/,A)') 0,'ENDSEC'          !- ie 'end_of_entities' ?
      WRITE(IO,'(I3/,A)') 0,'EOF'
      CLOSE(55)
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE DR_DXF (TEXT,X,Y,N,ICOL, IOP)
C
C     This writes DXF Output to unit 55 for lines,fill_areas etc.
C     IOP = 1 un-closed line
C           2    closed line = polygon
C           5   filled polygon  
c       (  10   filled circle  )
C          20   text string   (auto font & scale)
c          21/22 = centred/LHjust text
c..... written 9-2-96 (based on DR_PS)

c.. note how do we specify particular RGB colours in DXF format?
c.. probably in a TABLE entry I guess, one for each pen number.
c  .. maybe I should put each into its own subroutine?
c
      REAL X(*),Y(*)
      INTEGER N, ICOL,IOP, ICOL_OLD
      COMMON /PALETTE/PAL
      INTEGER PAL(3,0:255)
      CHARACTER TEXT*(*) !*80
      INTEGER I,II
      DATA ICOL_OLD/-1/     !- remember the 'last' colour
c     DATA TXT_SIZE/-1./    !- remember the 'last' text size
      DATA IO/55/           !- output file handle

      ICOL_OLD = ICOL

      IF (IOP.EQ. 1) THEN                               !/* draw-polygon */
c.. hmm where is the 'closed polygon' flag ?
            WRITE(IO,'(I3/A, 4(/I3/I5) )') 0,'POLYLINE'
     &        ,66,1, 70,1, 8,1, 62,ICOL
            DO II=N,1,-1
              I=MOD(II,N)+1     !- so {1,6,5,4,3,2} for a 6nq
              WRITE(IO,'(I3/A, /I3/I5/, (I3/G14.5))')     
c    &        0,'VERTEX',  8,1, 10,X(I), 20,0., 30,y(i)   ! y<->z flip
     &        0,'VERTEX',  8,1, 10,X(I), 20,y(i), 30,0.   ! y<->z flip
            ENDDO
            WRITE(IO,'(I3/,A)')  0,'SEQEND'

      ELSEIF (IOP.EQ. 2) THEN                      !/* fill-polygon */   
          IF (N.eq.3.or.N.eq.4) THEN
            WRITE(IO,'(I3/A, 4(/I3/I5) )')
     &      0,'3DFACE', 66,1, 70,1, 8,1, 62,ICOL
            DO I=1,N
              WRITE(IO,'(I3/G14.5)') 10+ I-1,x(I)
     &                 ,20+ I-1,y(i),30+ I-1,0.
            ENDDO
          ELSE
            WRITE(IO,'(I3/A, 4(/I3/I5) )') 0,'POLYLINE'
     &        ,66,1, 70,1, 8,1, 62,ICOL
            DO I=1,N
              WRITE(IO,'(I3/A, /I3/I5/, (I3/G14.5))')
     &        0,'VERTEX',  8,1, 10,X(I), 20,Y(I), 30,0.
            ENDDO
            WRITE(IO,'(I3/,A)')  0,'SEQEND'
          ENDIF

      ELSEIF (IOP.EQ. 3) THEN                      !/* draw-polyline */
            WRITE(IO,'(I3/A, 4(/I3/I5) )') 0,'POLYLINE'
     &        ,66,1, 70,1, 8,1, 62,ICOL      !** check this = 'open-polyline'
            DO I=1,N
              WRITE(IO,'(I3/A, /I3/I5/, (I3/G14.5))')
     &        0,'VERTEX',  8,1, 10,X(I), 20,Y(I), 30,0.
              WRITE(IO,'(I3/,A)')  0,'SEQEND'
            ENDDO
      ELSEIF (IOP.EQ.10) THEN                      !/* fill-circle */
      ELSEIF (IOP.EQ.20) THEN                      !/* draw-text */
      ELSEIF (IOP.EQ.21) THEN                      !/* draw-text-centered */
      ELSEIF (IOP.EQ.22) THEN                      !/* draw-text-RH-just */
      ELSE
         STOP 'unknown opcode in DR_DXF'
      ENDIF
      RETURN
      END

c-----------------------------------------------------------------------
      SUBROUTINE WRITE_DXF_LOGOS (IO,RES,TITLE,VERSION)
C
C     This writes the filename and date (+ frame?) to the DXF output file
C     (modelled on my PS driver)
      CHARACTER  TITLE*40, FILE2*40, VERSION*5
     &        ,DATE_STAMP*20, BS*2, FS
      EXTERNAL DATE_STAMP
      INTEGER RES(6)
      REAL X(5),Y(5)
      FS='/'
      BS='\\'

      FILE2 = TITLE
      DO I=1,LEN(FILE2)     !- reverse backslashes for postscript output
        IF (FILE2(I:I).EQ.BS) then
          FILE2(I:I)=FS
        ENDIF
      ENDDO

                                                                    

c.. maybe nice to 2x width text for the filename
       X(1) = res(4)+res(1)/2.   ! 575 / 2
       Y(1) = res(5)-10.         !  45
       X(2) = 10.
       CALL DR_PRIM (FILE2,x,y,1,0,21)      !-- the title (21=centered)
c                                           !- 0='black' (cf black paper ?
       X(1) = res(4)+res(1)    ! 575 / 2
       Y(1) = res(5)-10.       ! 45
       X(2) = 7.
       CALL DR_PRIM (DATE_STAMP(),x,y,1,0,22)  

      END

c-----------------------------------------------------------------------

