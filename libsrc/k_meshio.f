c
c       A library of routines for reading and writing 
c                  FInite Element Meshes
c
c      Dr. Dan. Kidger     d.kidger@man.ac.uk
c
c Revisions:
C     7-2-96 added WRITE_FEMVIEW (DXF too?)
c    24-8-96 added WRITE_ARENA for the ARENA windows-based 3d-modeller
c    10-9-97 added WRITE_MESH_UCD for AVS Unstructured Cell Data
c   29-11-97 re-ordered routine to move entry points to the top.
c   24- 1-98 added IMPORT_DIANA :-)
c    4- 2-98 added IMPORT_ANSYS :-)
c c.16- 9-98 added Import CRISP for: .GPO, .SCD, etc.
c   28-10-98 Work on exporting VRML (eg. for Nigel John, MVC)
      
C-----------------------------------------------------------------------
c    Entry points:
c
c       KEY_MESH_IO  : *KEYWORD handler
c       R_MESHFILE   : read any format given its EXT
c       WR_MESHFILE  : write any format given its EXT (PS and PCX too)
c
c       GET_MESHFILE_TYPE  : simply abstract the 3-letter file extension
c       R_DISPS        : for PL files with displacemnts (BOV too)
c
c
c  Obsolete routines:
c     GET_FILE_TYPE     :   8 -> .DXF  using an integer for each type.
c     GET_MESHFILE_EXT  :  .DXF -> 8

c-----------------------------------------------------------------------
C    *** IMPORT formats ***
c
c... also some 'parsing' (eg of DXF files)
c   So we can READ :
c
C        */      : any file below given the correct filename extension.
c        1/ PL   : DANPLOT (my original format)
c        2/ OFF  : Object File Format .GEO geometry files
c        3/ NFF  : Neutral File Format Files (Danplot version also reads 
c                     the colours
c        4/ SRF  : Shareware surface program (cf. FACES and CEILING FAN)
c        5/ 3D   : 3DEDIT shareware program (eg. the sailing boat)
c                    cf. the 3DVIEW files of lines (eg house+windpump)
c        6/ CAR  : CAROLUS's format (like .PL) but ELEMs come first
c        7/ TEC  : TECPLOT (tm) files (files must be slightly hacked)
c        8/ DXF  : AUTOCADs neutral file format (mostly lines)

c       10/ LIU  : Tiim Liu's FE file (mesh+analysis data)
c  (    11/ FV   : Femview standard FE visualisation package. )
c  (    12/ SDA  : ARENA solid-modelling package (win3.1)     )
c       13/ UCD  : AVS vis. system, unstructured cell data (Unix)  
c       14/ MESMSM1 : Tiera Amadre's mesh file (no filename extension)
c       15/ PHA  : 'PHASES' FE system as used by Golders.
c       16/ OBJ  : Wavefront's raytracer objects

c-----------------------------------------------------------------------
c
C    *** EXPORT formats ***
c
c... also some 'parsing' (eg of DXF files)
c   So we can WRITE :
c
c        1/ PL   : DANPLOT (my original format)
c        2/ OFF  : Object File Format .GEO geometry files
c   *    3/ NFF  : Neutral File Format Files (Danplot version also reads 
c                     the colours
c
c   *    4/ SRF  : Shareware surafce program (cf. FACES and CEILING FAN)
c   *    5/ 3D   : 3DEDIT shareware program (eg. the sailing boat)
c   *    6/ CAR  : CAROLUS's format (like .PL) but ELEMs come first
c   *    7/ TEC  : TECPLOT (tm) files (files must be slightly hacked)
c        8/ DXF  : AUTOCADs neutral file format (mostly lines)
c        9/ RAY  : RAYSHADE ray-tracing package
c
c       10/ FV   : Femview standard FE visualisation package. 7-2-96
c       11/ SDA  : ARENA 3d-modleer 'Scene Description Ascii'  24-8-96
c       13/ UCD  : AVS vis. system, unstructured cell data (Unix)  
c    *  14/ OBJ  : Wavefront's raytracer objects
c
c   * = not yet implimented
c

C-----------------------------------------------------------------------
c     2: Entry-points
C-----------------------------------------------------------------------
      SUBROUTINE KEY_MESH_IO  (FOUND,IPR,KEYWORD,
     &         IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL,P)
C
C     A Keyword handler for importing and exporting whole meshes
C        DJK  25-8-93
c
C     (do I really need P ? .. only if we 'shuffle' the input data)
C     6-9-95 IPR>=1 for output as we go along .. not yet implimented
c            ie. IPR for *logging*, MYERROR() for failures
C
      REAL GC (IGC,*)
      INTEGER NUMS(INUMS,*) , P(*)      ! P is just workspace (1:NN)
      LOGICAL FOUND, OPENED
      CHARACTER KEYWORD*(*)
      character token*255, FILE*80, FILE2*80, EXT_IN*3

      INTEGER IO2
      DATA IO2/80/     !(was 60)
c              .. nice to have a 'get next free unit after IBASE' cf CBS)

      FOUND = .TRUE. 

c     --- 1: save the old status ---
c- Save NEL so we can count the overall change in the mesh. Hence we can select 
c- these in P() for future operation such as *SCALE and *SHIFT
c.. This is perhaps most relevant when using Danplot(tm) as a mesh-generator

      NN_OLD = NN
      NEL_OLD = NEL

c.. new Keyword 7-12-96 .. uses the suffix to determine the file type.
c. hence can read *ANY* meshfile!
c (But it helps to be able to explicitly give the type too)
c via a second argument possibly
c.. hmm or maybe could supply an optional argument to *IMPORT_MESH
c   to give type explicitly eg. *IMPORT_MESH, FORMAT='DXF'

      IF (KEYWORD.EQ.'*IMPORT_MESH') THEN         
   2    READ(IO,*,iostat=ios) FILE2        !- or use #include "file"' ?
        IF (IOS.NE.0) CALL IN_TEST(io,ios,*2,*199)
  199   CONTINUE     
        OPEN(IO+1,file=FILE2,status='old',iostat=ios)
        IF (IOS.NE.0) CALL MYERROR (1,'Cannot open IMPORT MESH file')
        ext_in=' '

        call get_keyword (-123,idummy, token)   !- skip first
        call get_keyword (-123,idummy, token)   !- get token
        if (token.eq.'FORMAT') then
          call get_keyword (-123,idummy, token)   !- get value
          ext_in=token(1:3)
        endif
        if (ext_in.eq.' ')               !- if not explicit
     &  CALL GET_MESHFILE_TYPE (FILE2,  EXT_IN ) !- cf explicit

        CALL R_MESHFILE (IO+1,IPR,GC,IGC,NDIM,NN,NUMS,INUMS,NEL
     &     ,P,EXT_IN)
        CLOSE (IO+1)


c---- other specific imports (default is in-line data) --------
c.. I think that all the above can be handled by the above.
      ELSEIF (KEYWORD.EQ.'*COORDS_RAW') THEN     
c         WRITE (LINE,'(A)')  'Reading nodal coords'
c         call post_pop_up (line)
         CALL R_DATA_JASPER (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL)
c        CALL GET_MESH_RANGE (GC,IGC ,NN,NDIM, GC_MIN,GC_MAX,DIAG)
      ELSEIF (KEYWORD.EQ.'*READ_DANPLOT') THEN         
         call myerror (2,'Please use *IMPORT_PL instead')
      ELSEIF (KEYWORD.EQ.'*READ_DANPLOT') THEN         
         call myerror (2,'Please use *IMPORT_PL instead')
      ELSEIF (KEYWORD.EQ.'*IMPORT_PL') THEN            
        CALL R_DANPLOT (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL)

      ELSEIF (KEYWORD.EQ.'*IMPORT_OFF') THEN
        CALL R_OFF (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL)

      ELSEIF (KEYWORD.EQ.'*IMPORT_SRF') THEN      !- 'SRF' format
        CALL R_SRF (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL)

c... hmm I need a way of specifying the input DXF filename ?
c.. maybe R_DXF can report too?
      ELSEIF (KEYWORD.EQ.'*CHECK_DXF') THEN      !- AutoCad's DXF
        READ(IO,*) FILE2
        OPEN(IO+1,file=FILE2,status='old')
        CALL R_DXF_STATS (IO+1)
        CLOSE (IO+1)

c.. need to check what sort of input this will give us .. lines/polygons.
c.. maybe DXF checking should be just a stand-alone package?
      ELSEIF (KEYWORD.EQ.'*IMPORT_DXF') THEN      !- AutoCad's DXF
        READ(IO,*) FILE2
        OPEN(IO+1,file=FILE2,status='old')
        CALL R_DXF (IO+1,GC,IGC,NDIM,NN,NUMS,INUMS,NEL)
        CLOSE (IO+1)

      ELSEIF (KEYWORD.EQ.'*IMPORT_3DEDIT') THEN   !- '3D EDIT' format
        CALL R_3DEDIT (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL)

      ELSEIF (KEYWORD.EQ.'*IMPORT_CAROLUS') THEN   !- his own style
        CALL R_CAROLUS (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL)

      ELSEIF (KEYWORD.EQ.'*IMPORT_TIM_LIU') THEN   !- his own style
c--- as a daughter file cos of extra data?
c.. 16-11-95 if the file is called '.' then just use the current input stream?
        READ(IO,*) FILE2
        OPEN(IO+1,file=FILE2,status='old') 
        CALL R_TIMLIU (IO+1,GC,IGC,NDIM,NN,NUMS,INUMS,NEL,P)
        CLOSE (IO+1)

c--------- PHASES .. FE system (as used by Golders)
c.. 16-11-95 if the file is called '.' then just use the current input stream?
      ELSEIF (KEYWORD.EQ.'*IMPORT_PHASES') THEN   !- hmm what was PHASES
        READ (IO,*) FILE2
        OPEN (IO+1,file=FILE2,status='old') 
        CALL R_PHASES (IO+1,GC,IGC,NDIM,NN,NUMS,INUMS,NEL,P)
        CLOSE (IO+1)

C--------- TECPLOT .. FE visualisaation package
      ELSEIF (KEYWORD.EQ.'*IMPORT_TECPLOT') THEN   !- 'TECPLOT' format
        CALL R_TECPLOT (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL)

C--------- AVS . unstructured cell data.
      ELSEIF (KEYWORD.EQ.'*IMPORT_UCD') THEN   !- AVS 
        CALL R_MESH_UCD (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL)

C--------- DIANA: fairly standard FE structure
      ELSEIF (KEYWORD.EQ.'*IMPORT_DIANA') THEN   !-
        READ(IO,*) FILE2
        OPEN(IO+1,file=FILE2,status='old') 
        CALL R_MESH_DIANA (IO+1,GC,IGC,NDIM,NN,NUMS,INUMS,NEL)
        CLOSE (IO+1)

C--------- ANSYS: fairly standard FE structure
      ELSEIF (KEYWORD.EQ.'*IMPORT_ANSYS') THEN   !-
        READ(IO,*) FILE2
        OPEN(IO+1,file=FILE2,status='old') 
        CALL R_MESH_ANSYS (IO+1,GC,IGC,NDIM,NN,NUMS,INUMS,NEL)
        CLOSE (IO+1)

C--------- MARC: fairly standard FE structure
      ELSEIF (KEYWORD.EQ.'*IMPORT_MARC') THEN 
        READ(IO,*) FILE2
        OPEN(IO+1,file=FILE2,status='old') 
        CALL R_MESH_MARC (IO+1,IPR,GC,IGC,NDIM,NN,NUMS,INUMS,NEL, P)
        CLOSE (IO+1)


c-----------------------------------------------------------------------
C                           EXPORT of meshes
c-----------------------------------------------------------------------

C... output file is automaticaly generated from the current input file
C... by changing the file extension 
C-- What if the file has already been opened by the main program ?
C-- so test for the current file being open ? and only open it
C-- if it has NOT already been opened ?
c... be very careful of the output file ie. if 13 exists use it, else
c... best to open our own file here

c.. some applications may also want to leave this file open to write 
c   further information; for example writing load steps in a UCD file.
c   < note that I can always CAT files together in a shell-script. >

c 30-11-97 maybe optional *WRITE_MESH, FILE='foo.dxf' ? (or on line 2)
c   if just EXT is given then use the input name to generate

c -- 30-11-97 nice to allow special file types :
c    eg. 'PRN' for the printer - use a temp file then copy?
c        '.' indicates that the data is inline.


      ELSEIF (KEYWORD.EQ.'*WRITE_DANPLOT') THEN

c.. because .PL is so close to .UCD, mayve .PL should be made obsolete ?
c.. but note that .UCD has less element types and only one loadcase

c** I would like to scan for an optional filename .. if we hit another 
c      keyword.. just BACKSPACE & use the default
        FILE2 = ' '
   11   READ(IO,*,IOSTAT=IOS) FILE2
        IF (IOS.NE.0) CALL IN_TEST(io,ios,*11,*299)
  299   CONTINUE      

c.. as a subroutine : give it the default. It looks for another name
c   if found returns mew name else returns default
c.. it could maybe test to see if this file exists, hence put up a 
c     warning message

        if (file2.eq.' ') then
          INQUIRE (UNIT=IO,NAME=FILE)                !was missing 1-3-98
!TODO call generate_filename()  
!    maybe adds hostname, MPI_RANK, etc.
          file2 = FILE(1:INDEX(FILE,'.'))//'pl'
        endif

        OPEN(IO2,FILE=FILE2)
        CALL WR_DANPLOT (IO2,GC,IGC,NDIM,NN,NUMS,INUMS,NEL)
        CLOSE(IO2)        !- be careful !

      ELSEIF (KEYWORD.EQ.'*WRITE_OFF') THEN
c.. hmm be careful as this will re-open and close DANFE's .OUT file :-(
        INQUIRE (UNIT=IO,NAME=FILE)                !was missing 1-3-98
        file2 = FILE(1:INDEX(FILE,'.'))//'geo'
        OPEN (IO2,FILE=FILE2)
        CALL WR_OFF (IO2,GC,IGC,NDIM,NN,NUMS,INUMS,NEL)
        CLOSE(IO2)

      ELSEIF (KEYWORD.EQ.'*WRITE_DXF') THEN
        INQUIRE (UNIT=IO,NAME=FILE)
        OPEN (IO2,FILE=FILE(1:INDEX(FILE,'.'))//'dxf')
        CALL WR_DXF (IO2,GC,IGC,NDIM,NN,NUMS,INUMS,NEL)
        CLOSE(IO2)

      ELSEIF (KEYWORD.EQ.'*WRITE_RAYSHADE') THEN  
        INQUIRE (UNIT=IO,NAME=FILE)
        OPEN (IO2,FILE=FILE(1:INDEX(FILE,'.'))//'ray')
        CALL WR_RAYSHADE (IO2,GC,IGC,NDIM,NN,NUMS,INUMS,NEL)
        CLOSE(IO2)

      ELSEIF (KEYWORD.EQ.'*WRITE_FEMVIEW') THEN     !-- not yet implemented --
        INQUIRE (UNIT=IO,NAME=FILE)
        OPEN (IO2,FILE=FILE(1:INDEX(FILE,'.'))//'fv')
        CALL WR_FEMVIEW (IO2,GC,IGC,NDIM,NN,NUMS,INUMS,NEL)
        CLOSE(IO2)

      ELSEIF (KEYWORD.EQ.'*WRITE_ARENA') THEN  
        INQUIRE (UNIT=IO,NAME=FILE)
        OPEN (IO2,FILE=FILE(1:INDEX(FILE,'.'))//'sda')
        CALL WR_ARENA (IO2,GC,IGC,NDIM,NN,NUMS,INUMS,NEL,P)
        CLOSE(IO2)

      ELSEIF (KEYWORD.EQ.'*WRITE_MESH_UCD') THEN  
        INQUIRE (UNIT=IO,NAME=FILE)
        OPEN (IO2,FILE=FILE(1:INDEX(FILE,'.'))//'ucd')
        CALL WR_MESH_UCD (IO2,GC,IGC,NDIM,NN,NUMS,INUMS,NEL)
        CLOSE(IO2)

!TODO add .x3d too
      ELSEIF (KEYWORD.EQ.'*WRITE_MESH_VRML') THEN  
        INQUIRE (UNIT=IO,NAME=FILE)
        OPEN (IO2,FILE=FILE(1:INDEX(FILE,'.'))//'wrl')
c       CALL WR_MESH_VRML1 (IO2,GC,IGC,NDIM,NN,NUMS,INUMS,NEL)
        CALL WR_MESH_VRML2 (IO2,GC,IGC,NDIM,NN,NUMS,INUMS,NEL) !v2.0
        CLOSE(IO2)


c-------------------- KEYWORD file writing -----------------------------
c. cf EXPORT of whole meshes

      ELSEIF (KEYWORD.EQ.'*WRITE_DANPLOT_KEY') THEN    !- keyword-based
        INQUIRE (UNIT=IO2,OPENED=OPENED)
        IF (.NOT.OPENED) THEN             !- use the given if present
          INQUIRE (UNIT=IO,NAME=FILE)       ! (must be unit 80 tho)
          OPEN(IO2,FILE=FILE(1:INDEX(FILE,'.'))//'dp')  !- 7-12-96 KEY->DP
        ENDIF
        CALL WR_DANPLOT_KEY (IO2,IPR,GC,IGC,NDIM,NN,NUMS,INUMS,NEL)
        IF (.NOT.OPENED) CLOSE(IO2)      !- close as well ?

c.... these two will write out the nodes/elements (cf *WRITE_NF, etc.) .....
c      ELSEIF (KEYWORD.EQ.'*WRITE_NODES') THEN               !- nodal coords
c        CALL R_NODES (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL)

c      ELSEIF (KEYWORD.EQ.'*WRITE_ELEMENTS') THEN            !- elem steering
c        CALL R_ELEMENTS (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL)

c--------------------------- Mesh Plots --------------------------------
c      ELSEIF (KEYWORD.EQ.'*PLOT') THEN
c         call myerror (1,'Please use *PLOT_MESH instead')
!TODO why are PLOT_MESH and DRAW_MESH different?
      ELSEIF (KEYWORD.EQ.'*PLOT_MESH') THEN
        CALL PLOT_MESH (GC,IGC,NDIM,NN,NUMS,INUMS,NEL)
      ELSEIF (KEYWORD.EQ.'*DRAW_MESH') THEN
        CALL DRAW_MESH (GC,IGC,NDIM,NN,NUMS,INUMS,NEL)
c      ELSEIF (KEYWORD.EQ.'*PLOT_MESH_FILL') THEN
c.. this has options to allow material colours, node #s, etc
c        CALL PLOT_MESH_FILL (GC,IGC,NDIM,NN,NUMS,INUMS,NEL,1)
      ELSEIF (KEYWORD.EQ.'*PLOT_MESH_BOUNDARY') THEN
        CALL MYERROR (1,'"*PLOT_MESH_BOUNDARY" not yet available here')
c       CALL PLOT_MESH_BOUNDARY (GC,IGC,NDIM,NN,RINGS)

!TODO add .svg  for web based vector graphics

C--------------------- End of all known keywords -----------------------
      ELSE
        FOUND = .FALSE.
      ENDIF
      RETURN  
      END

c-----------------------------------------------------------------------
C                    2:   Entry-points
c-----------------------------------------------------------------------
      SUBROUTINE R_MESHFILE (IO,IPR,GC,IGC,NDIM,NN,NUMS,INUMS,NEL,P
     &  ,EXT1)
C
C     Reads a file of Finite Elements / Polygons  
C     Based on its Filename Extension EXT
C        DJK   24-2-95
c  Caveats:
c     .. read MESHSM1 to? . but it doesn't have an extension.
C
      IMPLICIT NONE
      INTEGER    IGC,INUMS, IO, NN,NEL,NDIM, IPR
      REAL       GC (IGC,NN)
      INTEGER    NUMS (INUMS,NEL), P(*)
      CHARACTER  EXT*3,ext1*3

      ext=ext1
      call to_lower(ext)
      if (ipr.ge.2) print*,'File type/extension = ',ext

      IF (EXT.EQ.'pl ') THEN                 !- Danplot (old) format
        CALL R_DANPLOT  (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL)
      ELSEIF (EXT.EQ.'dp ') THEN                 !- Danplot (new) format
c  .. we now need to call a proper parser 
c   - it needs to be able to handle *2D, *NODES,*ELEMENTS only.
        CALL R_DANPLOT_KEY (IO,IPR,GC,IGC,NDIM,NN,NUMS,INUMS,NEL,P)
      ELSEIF (EXT.EQ.'out') THEN                 !- DANFE results file
        CALL R_DANPLOT_KEY (IO,IPR,GC,IGC,NDIM,NN,NUMS,INUMS,NEL,P)

      ELSEIF (EXT.EQ.'off') THEN            !- OFF
        read(io,'()')
        CALL R_OFF      (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL)
      ELSEIF (EXT.EQ.'g  ') THEN            !- OFF
        CALL R_OFF      (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL)
      ELSEIF (EXT.EQ.'geo') THEN            !- OFF
        CALL R_OFF      (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL)
      ELSEIF (EXT.EQ.'srf') THEN            !- SRF (sw)
        CALL R_SRF      (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL)
      ELSEIF (EXT.EQ.'3d') THEN             !- 3DEDIT (sw)
        CALL R_3DEDIT   (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL)
      ELSEIF (EXT.EQ.'car') THEN            !- our CAROLUS
        CALL R_CAROLUS  (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL)
      ELSEIF (EXT.EQ.'tec') THEN             !- TECPLOT
        CALL R_TECPLOT  (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL)
      ELSEIF (EXT.EQ.'dxf') THEN            !- DXF
        CALL R_DXF      (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL)
      ELSEIF (EXT.EQ.'ray') THEN            !- Rayshade
c         CALL R_RAYSHADE (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL)
      ELSEIF (EXT.EQ.'nff') THEN            !- Neutral File Format
c         CALL R_NFF (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL, PAL)
          CALL R_NFF (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL)
      ELSEIF (EXT.EQ.'raw') THEN            !- Raw 3nt's
c         CALL R_RAYSHADE (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL)
c      ELSEIF (EXT.EQ.'PHA') THEN                      !- Rayshade
c.. note that PHASES uses  P() as a workspace to store the type of each 
c     element (beam/rod/3nt/etc.) (or parse twice?)
         CALL R_PHASES (IO+1,GC,IGC,NDIM,NN,NUMS,INUMS,NEL,P)

      ELSEIF (EXT.EQ.'ucd') THEN            !- AVS's UCD   11-9-96 
        CALL R_MESH_UCD (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL)

c.. note BOVIDA-BOPRE has no extension so 'BOV' is pseudo.
      ELSEIF (EXT.EQ.'bov') THEN           !- Tierra Amadra's package. 4-12-96
        CALL R_MESH_BOVIDA (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL)

      ELSEIF (EXT.EQ.'fv') THEN            !- good old FEMVIEW
        CALL R_FEMVIEW (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL)

      ELSEIF (EXT.EQ.'p59') THEN            !- S+G's Book5.9 (Jan 98)
        CALL R_BOOK59 (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL)
      ELSEIF (EXT.EQ.'p68') THEN            !- S+G's Book6.8 (Jan 98)
        CALL R_BOOK59 (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL)  !- similar to P59

      ELSEIF (EXT.EQ.'DIA') THEN           !- The Dutch DIANA package
c                                          ! (Sheffield 3D caisson)
        CALL R_MESH_DIANA (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL)

      ELSEIF (EXT.EQ.'sas') THEN           !- S.A.S's ANSYS FE package 
c                                          ! (Sheffield Floor slabs)
        CALL R_MESH_ANSYS (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL)

      ELSEIF (EXT.EQ.'mrc') THEN           !- MARC format (SPINE data)
        CALL R_MESH_MARC (IO,IPR,GC,IGC,NDIM,NN,NUMS,INUMS,NEL,p)
      ELSEIF (EXT.EQ.'liu') THEN           !- Dr. Tim Liu's plates and shells
        CALL R_TIMLIU (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL,P)

      ELSEIF (EXT.EQ.'dp') THEN           !- ** my KEYWORD format **
        CALL R_MYMESH (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL)

      ELSEIF (EXT.EQ.'obj') THEN           !- ** Wavefront raystracer format
        CALL R_WAVEFRONT (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL)
c----- CRISP Intefaces -----
      ELSEIF (EXT.EQ.'scd') THEN           !- Crisp geom prog output?
        CALL R_CRISP_SCD (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL,P)
      ELSEIF (EXT.EQ.'gpd') THEN           !- Crisp - geom prog data?
        CALL R_CRISP_GPD (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL,P)
      ELSEIF (EXT.EQ.'fam') THEN           !- Crisp - geom prog data?
        CALL MYERROR (1,' Crisp .FAM format not supported')
      ELSEIF (EXT.EQ.'ldb') THEN           !- Crisp - geom prog data?
        CALL MYERROR (1,' Crisp .LDB format not supported')
      ELSEIF (EXT.EQ.'mpo') THEN           !- Crisp - main prog output
c       CALL MYERROR (1,' Crisp .MPD format not yet supported')
        CALL R_CRISP_MPO (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL,P)
      ELSEIF (EXT.EQ.'mpd') THEN           !- Crisp - main prog data?
        CALL MYERROR (1,' Crisp .MPD format not yet supported')
c       CALL R_CRISP_MPD (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL,P)
c  main program data - doesn't include the mesh
c   set of records, eg:
c       C1:  NPLAX  <NMAT>  <NOIB>  INC1  INC2  IPRIM  IUPD  ICOR  ISR
c       D: IMAT, E0  YO M  V  0  0  Kw/Gw  Gbulk  Kx  Ky  0  0   
c       E: elements not present at the start.
c       G1: in situ stresses, given a set of control points
c       I & J: for this increment: elements to add/remove
c       L: edge-based pressure/shear loads

      ELSEIF (EXT.EQ.'gpo') THEN           !- Crisp - geom prog output
        CALL R_CRISP_GPO (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL,P)

      ELSEIF (EXT.EQ.'gpr') THEN           !- Crisp - v.similar to GPD
        CALL R_CRISP_GPD (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL,P)
      ELSE
        PRINT*,'*** Unknown Import File type'
      ENDIF
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE WR_MESHFILE (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL,P, EXT1)
C
C     Writes a file of Finite Elements / Polygons  
C     Based on its Filename Extension EXT
C        DJK   24-2-95
C
      IMPLICIT NONE
      INTEGER    IGC,INUMS, IO, NN,NEL,NDIM, IPR
      REAL       GC (IGC,NN)
      INTEGER    NUMS (INUMS,NEL), P(*)
      CHARACTER  EXT*3, ext1*3
c  (change to upper/lowercase ?)
      ext=ext1
      call to_lower(ext)

      IF (EXT.EQ.'pl ') THEN                      !- Danplot (old) format
        CALL WR_DANPLOT (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL)
      ELSEIF (EXT.EQ.'dan') THEN                      !- Danplot (new) format
        ipr=1    != level of log info
        CALL WR_DANPLOT_KEY (IO,IPR,GC,IGC,NDIM,NN,NUMS,INUMS,NEL)
      ELSEIF (EXT.EQ.'dp') THEN                      !- my new Danplot format
c    .. hack a copy of WR_DANPLOT_KEY to here so we can give K_MESHIO.F away.
        ipr=1    != level of log info
        CALL WR_DANPLOT_KEY2 (IO,IPR,GC,IGC,NDIM,NN,NUMS,INUMS,NEL)


      ELSEIF (EXT.EQ.'geo') THEN                      !- OFF
        CALL WR_OFF     (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL)
      ELSEIF (EXT.EQ.'srf') THEN                      !- SRF (sw)
c       CALL WR_SRF     (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL)
      ELSEIF (EXT.EQ.'3d') THEN                       !- 3DEDIT (sw)
c       CALL WR_3DEDIT  (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL)
      ELSEIF (EXT.EQ.'car') THEN                      !- our CAROLUS
c       CALL WR_CAROLUS (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL)
      ELSEIF (EXT.EQ.'tec') THEN                       !- TECPLOT
c       CALL WR_TECPLOT (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL)
      ELSEIF (EXT.EQ.'dxf') THEN                      !- DXF
        CALL WR_DXF     (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL)
      ELSEIF (EXT.EQ.'ray') THEN                      !- Rayshade
        CALL WR_RAYSHADE(IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL)

c.. can't do ARENA as it needs P() as a workspace (to sort the blocks)
      ELSEIF (EXT.EQ.'ray') THEN                      !- soild modeller
        CALL WR_ARENA (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL,P)
      ELSEIF (EXT.EQ.'ucd') THEN                      !- AVS's UCD
        CALL WR_MESH_UCD (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL)

      ELSEIF (EXT.EQ.'pcx') THEN                      !- a 2d screen-plot
        CALL PLOT_MESH (GC,IGC,NDIM,NN,NUMS,INUMS,NEL)
c       .. now should call 'dump_image_to_file'

      ELSEIF (EXT.EQ.'gif') THEN                      !- a 2d screen-plot
c     .. maybe I should use the VSCREEN ? therefore can do 1024x1024 ?
        CALL PLOT_MESH (GC,IGC,NDIM,NN,NUMS,INUMS,NEL)
c       .. now should call 'dump_image_to_file'
c       CALL SAVE_SCREEN_IMAGE ('PCX')
c      note that at this point we have the output file opened on unit IO
c      so we must INQUIRE it, close it then dump screen to it.
c       .. thence call CISSUE 'PCX2GIF'

      ELSEIF (EXT.EQ.'ps') THEN                      !- a postscript plot
        CALL WR_MESH_PS (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL)
c       .. or should it be PLOT_MESH_PS () ?

      ELSEIF (EXT.EQ.'wmf') THEN                      !- windows metafile
c        CALL PLOT_MESH_WMF (GC,IGC,NDIM,NN,NUMS,INUMS,NEL)
c.. this would be best when students want to put their pictures into MS-WORD
c

c.. note BOVIDA-BOPRE has no extension so 'BOV' is pseudo.
      ELSEIF (EXT.EQ.'bov') THEN           !- Tierra Amadra's package. 4-12-96
        CALL WR_MESH_BOVIDA (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL)

      ELSEIF (EXT.EQ.'fv'                  !- good old FEMVIEW
     &   .or. EXT.EQ.'fvi' ) THEN 
        CALL WR_FEMVIEW (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL)

      ELSEIF (EXT.EQ.'p59') THEN            !- S+G's Book5.9 (Jan 97)
        CALL WR_MESH_SG3 (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL,'P59')
      ELSEIF (EXT.EQ.'p68') THEN            !- S+G's Book6.8 (Jan 97)
        CALL WR_MESH_SG3 (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL,'P68')

c      ELSEIF (EXT.EQ.'DIA') THEN            !- DIANA FE package
c        CALL WR_MESH_DIANA (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL)
c      ELSEIF (EXT.EQ.'SAS') THEN            !- ANSYS FE package
c        CALL WR_MESH_ANSYS (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL)
c      ELSEIF (EXT.EQ.'MRC') THEN            !- MARC FE package
c        CALL WR_MESH_MARC (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL)

      ELSEIF (EXT.EQ.'obj') THEN           !- 
        CALL WR_WAVEFRONT (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL)
      ELSEIF (EXT.EQ.'wrl') THEN           !- 
c       CALL WR_MESH_VRML1 (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL)
        CALL WR_MESH_VRML2 (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL)

      ELSE
        PRINT*,'*** Unknown Export File type'
      ENDIF

      RETURN
      END

C-----------------------------------------------------------------------
C                    4: Mesh reading/writing Modules
c
c  These mostly appear in pairs eg: R_DANPLOT and WR_DANPLOT
c
C-----------------------------------------------------------------------
      SUBROUTINE R_DANPLOT (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL)
C
C     This imports a mesh in 'Danplot .PL format'
C     Caveats:
C        1/ no concept of 2d elements in 3d space, so 4nq's become 4n_tets!
C
      REAL GC(IGC,*)
      INTEGER NUMS(INUMS,*), NUM(32)

      NN_OLD  = NN
      NEL_OLD = NEL

   1  READ (IO,*,IOSTAT=IOS) NDIM
      CALL IN_TEST(IO,IOS,*1,*999)
   2  READ (IO,*,IOSTAT=IOS) NN
      CALL IN_TEST(IO,IOS,*2,*999)
      DO I=1,NN
        READ (IO,*)  ID,(GC(J,ID+NN_OLD),J=1,NDIM)
      ENDDO
      WRITE(*,'(A,I8,A)')  '>>',NN,' nodes read'
      NN = NN_OLD + NN

      READ (IO,*) NEL
      DO IEL=1,NEL
        READ (IO,*) IDUMMY,NOD,(NUM(J),J=1,NOD),IMAT

        DO J=1,NOD
          NUM(J) = NUM(J) + NN_OLD   !-- offsets
        ENDDO
        NDIME = NDIM             !- always the same
        ITYPE = 1                !- all we can guess !
c       IF (NDIM.eq.3.and.NOD.EQ.4) NDIME = 2   !- hack to get 4nq's in 3d space.

c       CALL PUT_ELEMENT 
c    &  (NUMS,INUMS,NEL_OLD+IEL, NUM, NOD,NDIME,ITYPE, IMAT,IEL,1,1)
        CALL PUT_ELEMENT 
     &  (NUMS,INUMS,NEL_OLD+IEL, NUM, NOD,NDIME,ITYPE, IMAT,1,1,1)

      ENDDO
c     CLOSE (IO)
      WRITE(*,'(A,I8,A)')  '>>',NEL,' elements read'
      NEL= NEL_OLD + NEL
  999 CONTINUE
      END

C-----------------------------------------------------------------------
      SUBROUTINE WR_DANPLOT (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL)
C
C     This writes out the mesh in 'Danplot' .PL format
c      - be careful if NDIME/=NDIM esp. 4nq's in 3D
c
C      (where is the straight WR_DANPLOT_KEY ?)
C
      REAL GC(IGC,*)
      INTEGER NUMS(INUMS,*), NUM(32), IO
      WRITE(IO,'(I5)') NDIM,NN
      DO I=1,NN
        WRITE(IO,'(I5,3F14.5)') I,(GC(J,I),J=1,NDIM)
      ENDDO
      WRITE(*,'(A,I8,A)')  '>>',NN,' nodes written'
      WRITE(IO,'(I5)') NEL
      DO IEL=1,NEL
        CALL GET_ELEMENT
     &  (NUMS,INUMS,iEL, NUM, NOD,NDIME,ITYPE, IMAT,I1,i2,i3)
        WRITE(IO,'(99I6)') IEL,NOD,(NUM(J),J=1,NOD),IMAT
      ENDDO
      WRITE(*,'(A,I8,A)')  '>>',NEL,' elements written'
      RETURN
      END  

C-----------------------------------------------------------------------
      SUBROUTINE R_MYMESH (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL)
C
C     reads just the 'mesh' (*NODES and  *ELEMENTS) for a .DP file
c
c   Caveats
c   - I need to be able to use this to read one mesh after another
c    ie. add the offset NN_OLD to all entries in NUMS
C
      REAL GC(IGC,*)
      INTEGER NUMS(INUMS,*), IO
      CHARACTER KEYWORD*70   ! data input keyword 'token'
      integer u0,u1

      U1 = IO                         !-- start from the base file = U0
      DO IKEYWORD = 1, 9999
        CALL GET_KEYWORD (U1,U0,KEYWORD)
c       CALL PRINT_KEYWORD (IKEYWORD, KEYWORD,.false.)
        IF (KEYWORD.EQ.'*EOF') THEN
          close (u1)             !- always close this input file?
          return

        ELSEIF (KEYWORD.EQ.'*NODES') THEN
          CALL R_NODES (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL)

        ELSEIF (KEYWORD.EQ.'*ELEMENTS') THEN            !- elem steering
          CALL R_ELEMENTS (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL)

        ELSEIF (KEYWORD.EQ.'*ONE_DIMENSIONAL') THEN
          CALL CHANGE_NDIM (GC,IGC,NDIM,NN,1)       !- updated 8-2-98

        ELSEIF (KEYWORD.EQ.'*TWO_DIMENSIONAL') THEN
C         NDIM=2
          CALL CHANGE_NDIM (GC,IGC,NDIM,NN,2)

        ELSEIF (KEYWORD.EQ.'*THREE_DIMENSIONAL') THEN
C         NDIM=3
          CALL CHANGE_NDIM (GC,IGC,NDIM,NN,3)

        ELSEIF (KEYWORD.EQ.'*FOUR_DIMENSIONAL') THEN
          CALL CHANGE_NDIM (GC,IGC,NDIM,NN,4)

        ELSE    !----- unknown keyword found -----

        ENDIF
      ENDDO
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE WR_DANPLOT_KEY2 (IO,IPR,GC,IGC,NDIM,NN,NUMS,INUMS,NEL)
C
C     This writes out the mesh in 'Danplot' format .. new *Keyword style
C     * This is a straight copy of WR_DANPLOT_KEY in K_MESH.F
c     * mirrored here so K_MESHIO.F is 'complete'
C     . should the master copy live here ?
C
      REAL GC(IGC,*)
      INTEGER NUMS(INUMS,*), IO, NUM(32)
      WRITE (IO,'(A)') '#',
     & '# A Keyword based output file suitable for plotting by the',
     & '# DANPLOT package,   by Dr. Dan Kidger',
     & '#'
      IF (NDIM.EQ.2) WRITE (IO,'(A)') '*TWO_DIMENSIONAL'
      IF (NDIM.EQ.3) WRITE (IO,'(A)') '*THREE_DIMENSIONAL'

      WRITE (IO,'(A)') '*NODES'
      DO I=1,NN
         WRITE (IO,'(I6,5G14.6)') I,(GC(J,I),J=1,NDIM)
      ENDDO
      IF (IPR.GE.2)
     &WRITE(*,'(A,I7,A)')  '>< ',NN,' nodes written'
      WRITE (IO,'(A)') '*ELEMENTS'
      DO IEL=1,NEL
        CALL GET_ELEMENT
     &  (NUMS,INUMS,iEL, NUM, NOD,NDIME,ITYPE, IMAT,I1,i2,i3)
         WRITE (IO,'(i6,i2,i3,i2, 99i7)')
     &   IEL,NDIME,NOD,ITYPE, (NUM(I),I=1,NOD),IMAT
      ENDDO
      IF (IPR.GE.2)
     &WRITE(*,'(A,I7,A)')  '>< ',NEL,' elements written'
      RETURN
      END  

C-----------------------------------------------------------------------
      SUBROUTINE R_OFF (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL)
C
C     This imports a mesh in 'Object File Format'
C     ( These are 2d polygons (mostly 3 node triangles) in a 3d mesh
C     .. no need to test for comments as they never have any!
C         c. 1992?
C
      REAL GC(IGC,*)
      INTEGER NUMS(INUMS,*), NUM(32)

      NDIM = 3             !- force into 3D
      IMAT = 1             !- 20-9-94 force IMAT to all be type=1 !
      NN_OLD  = NN
      NEL_OLD = NEL
    1 READ (IO,*,IOSTAT=IOS)  NN, NEL, NEDGES
      CALL IN_TEST(IO,IOS,*1,*999)
      READ (IO,*)  ((GC(J,I+NN_OLD),J=1,ndim),I=1,NN)
      WRITE(*,'(A,I8,A)')  '>>',NN,' nodes read'
      NN = NN_OLD + NN


      NDIME = 2   !-- always 2D facets

      DO IEL=1,NEL
        READ (IO,*) NOD,(NUM(J),J=1,NOD)
        ITYPE = 9  
        IF (NOD.EQ.3.OR.NOD.EQ.6.OR.NOD.EQ.4.OR.NOD.EQ.8.OR.NOD.EQ.12)
     &      ITYPE = 1       !- valid as an 'ordinary' 2D element
        DO J=1,NOD
          NUM(J) = NUM(J) + NN_OLD   !-- offsets
        ENDDO
        CALL PUT_ELEMENT 
     &  (NUMS,INUMS,NEL_OLD+IEL, NUM, NOD,NDIME,ITYPE, IMAT,IEL,1,1)

      ENDDO
c     CLOSE (IO)
c     IO = IO - 1     !-- exit from this file  :-)
      WRITE(*,'(A,I8,A)')  '>>',NEL,' elements read'
  999 CONTINUE
      NEL= NEL_OLD+NEL
      END

C----------------------------------------------------------------------
      SUBROUTINE WR_OFF (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL)
C
C     This writes out the mesh in 'Object_File_Format' format
C   .. need to add a zero 'z' component if only 2d
C
      REAL GC(IGC,*)
      INTEGER NUMS(INUMS,*), IO , NUM(32)

      WRITE(IO,'(I5)') NN,NEL,12345
      IF (NDIM.EQ.2) THEN
        DO I=1,NN
          WRITE(IO,'(3F14.5)') (GC(J,I),J=1,NDIM),0.
        ENDDO
      ELSEIF (NDIM.EQ.3) THEN
        DO I=1,NN
          WRITE(IO,'(3F14.5)') (GC(J,I),J=1,NDIM)
        ENDDO
      ENDIF
      WRITE(*,'(A,I8,A)')  '>>',NN,' nodes written'
      DO IEL=1,NEL
          CALL GET_ELEMENT
     &    (NUMS,INUMS,iEL, NUM, NOD,NDIME,ITYPE, IMAT3,I1,i2,i3)
c.. call get_element surely !
c        NOD = NUMS(INUMS,IEL)
c        IMAT= NUMS(INUMS-3,IEL)
        WRITE(IO,'(99I6)') NOD,(NUM(J),J=1,NOD)
      ENDDO
      WRITE(*,'(A,I8,A)')  '>>',NEL,' elements written'
      RETURN
      END  

C-----------------------------------------------------------------------
      SUBROUTINE R_SRF (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL)
C
C     This imports a mesh in 'SRF Format'
C     These are (anticlock?) polygons (mostly 3 node triangles) in a 3d mesh
C     comments lines begin with '#'
c     Format is:
c        .  TITLE
C        .  version #
c        .  nmats, NN, NEL, id, id ??
c        .  nmats * < material properties >
c        .  NN * < x y z >
c        .  NEL * < NOD, IMAT, NUM(:) >

      REAL GC(IGC,*)
      INTEGER NUMS(INUMS,*), NUM(32), IO

      NN_OLD  = NN
      NEL_OLD = NEL

        NDIM  = 3
        NDIME = 2   !-- always 2D facets

      READ (IO,*)    !- dummy for the title line
      READ (IO,*) id !- dummy for the version number
      READ (IO,*)  nmats,NN,NEL, id,id

      DO I=1,NMATS
        READ (IO,*)    !- dummy for the material properties
      ENDDO 

      READ (IO,*) ( GC(3,I),GC(1,I),GC(2,I), I=NN_OLD+1,NN_OLD+NN)
      WRITE(*,'(A,I8,A)')  '>>',NN,' nodes read'
      NN = NN_OLD + NN

      DO IEL=1,NEL
        READ (IO,*) NOD,IMAT,(NUM(J),J=1,NOD) 
        ITYPE = 9  
        IF (NOD.EQ.3.OR.NOD.EQ.6.OR.NOD.EQ.4.OR.NOD.EQ.8.OR.NOD.EQ.12)
     &      ITYPE = 1       !- valid as an 'ordinary' 2D element
        DO J=1,NOD
          NUM(J) = NUM(J) + NN_OLD   !-- offsets
        ENDDO
        CALL PUT_ELEMENT 
     &  (NUMS,INUMS,NEL_OLD+IEL, NUM, NOD,NDIME,ITYPE, IMAT,IEL,1,1)
      ENDDO
c............... need to agree on how we close these files
c      CLOSE (IO)
c      IO = IO - 1     !-- exit from this file  :-)
      WRITE(*,'(A,I8,A)')  '>>',NEL,' elements read'
      NEL= NEL_OLD + NEL
      END

C-----------------------------------------------------------------------
      SUBROUTINE R_3DEDIT (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL)
C
C     This imports a mesh in '3D_EDIT Format'
C     These are (clockwise?) polygons (3nt or 4nq's) in a 3d domain
 
c     Format is:
c        .  NN   NEL_TRI   NEL_QUAD
c        .  NN * < x y z >
c        .  NEL_QUAD * < NUM(1:4), IMAT, 2,0 >
c        .  NEL_TRI  * < NUM(1:3), IMAT, 2,0 >    !- what is the 2,0 ??

      REAL GC(IGC,*)
      INTEGER NUMS(INUMS,*), NUM(32) , IO

      NN_OLD  = NN
      NEL_OLD = NEL

        NDIM  = 3
        NDIME = 2   !-- always 2D facets
        ITYPE = 1   !-- all valid as 4nq or 3nt 

    1 READ (IO,*,IOSTAT=IOS)  NN, NEL_TRI,NEL_QUAD
      CALL IN_TEST(IO,IOS,*1,*999)

c...................... nodal coordinates ...........................
      READ (IO,*)   ((GC(J,I+NN_OLD),J=1,3),I=1,NN)
      WRITE(*,'(A,I8,A)')  '>>',NN,' nodes read'
      NN = NN_OLD + NN

c...................... element steering ...........................
      NEL = NEL_TRI+NEL_QUAD
      NOD = 4
      DO IEL=1,NEL
        IF (IEL.gt.NEL_QUAD) NOD = 3
        READ (IO,*) (NUM(J),J=NOD,1,-1) , IMAT, id,id       

        DO J=1,NOD
          NUM(J) = NUM(J) + NN_OLD   !-- offsets
        ENDDO
        CALL PUT_ELEMENT 
     &  (NUMS,INUMS,NEL_OLD+IEL, NUM, NOD,NDIME,ITYPE, IMAT,IEL,1,1)

      ENDDO
c     CLOSE (IO)
c     IO = IO - 1     !-- exit from this file  :-)
      WRITE(*,'(A,I8,A)')  '>>',NEL,' elements read'
  999 CONTINUE
      NEL= NEL_OLD + NEL
      END

C-----------------------------------------------------------------------
      SUBROUTINE R_WAVEFRONT (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL)
C
C     This imports a mesh in 'WAVEFRONT' Raytracer format
C     These a sets of nodal coords and polygons/lines
c     other records for lighting, material properties need to be skipped
C
 
c     File Format 
c        'code', values      on every line
c       where:
c        #           = comment
c        v <x y z>   = vertex, 
c       (vt, vn, vp  = texture vertex, normal vector, nurbs points)
c        f num()     = polygon (need to count tokens)
c        fo          = as f (obsolete varient)
c        l (num(1:2) = an edge
c        g name      = gives the following objects a group name (cf IMAT)
c        o name      = gives the following objects an object name (cf IMAT)
c

      REAL GC(IGC,*)
      INTEGER NUMS(INUMS,*), NUM(32) , IO
      character line*78, code*6
      integer  istr_beg
      external istr_beg

      NN_OLD  = NN
      NEL_OLD = NEL

        NDIM  = 3
        NDIME = 2   !-- always 2D facets 
        ITYPE = 1   !-- all valid as 4nq or 3nt 
        IMAT=1      !- start with material no. 1

      do iline=1,99999
    1 READ (IO,'(a)',IOSTAT=IOS)  line
      CALL IN_TEST(IO,IOS,*1,*999)
      if (line.eq.' ') goto 1     !- skip blank lines
c     line = line (istr_end(line):)    !- strip leading spaces?
      iend = 0
      CALL GET_NEXT_TOKEN (LINE, IBEG,IEND,' ')
      jj=iend+1
c     if (ibeg.lt.0) goto 99
      code = line(ibeg:iend)
      if (code.eq.'#') then              !-- a comment --
        print*,line                         ! echo to stdout
      elseif (code.eq.'v') then          !-- a nodal vertex --
        nn=nn+1
        read(line(jj:),*) (gc(j,nn),j=1,ndim)
      elseif (code.eq.'vt') then         !-- texture vertex --
      elseif (code.eq.'vp') then         !-- local surface vertex --
      elseif (code.eq.'vn') then         !-- normal vector --

      elseif (code.eq.'f') then          !-- a facet --
        nel=nel+1
        CALL COUNT_TOKENS (LINE, NTOKS,' ')
        nod=ntoks-1
        read(line(jj:),*) (num(j),j=1,nod) 
        ndime=2
        ITYPE = 9  
        IF (NOD.EQ.3.OR.NOD.EQ.6.OR.NOD.EQ.4.OR.NOD.EQ.8.OR.NOD.EQ.12)
     &      ITYPE = 1       !- valid as an 'ordinary' 2D element

       CALL PUT_ELEMENT (NUMS,INUMS,NEL, NUM, 
     &                    NOD,NDIME,ITYPE, IMAT,1,1,1)

      elseif (code.eq.'l') then          !-- an edge --
        nod=2
        read(line(jj:),*) (num(j),j=1,nod )
        ndime=1
        ITYPE = 1  

       CALL PUT_ELEMENT (NUMS,INUMS,NEL, NUM, 
     &                    NOD,NDIME,ITYPE, IMAT,1,1,1)

      elseif (code.eq.'o') then          !-- start of an object --
      elseif (code.eq.'g') then          !-- start of a group --
c       if (imat.gt.1) imat=imat+1
        imat=imat+1
c      elseif (code.eq.'v') then          !--  --
c      elseif (code.eq.'v') then          !--  --
c      elseif (code.eq.'v') then          !--  --
      else
      endif

      enddo
c-------------------------
  999 continue    !- if eof.
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE WR_WAVEFRONT (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL)
C
C     This exports a mesh in 'WAVEFRONT' Raytracer format
C     These a sets of nodal coords and polygons/lines
C
c  CAVEATS
c     - There is no attempt to use IMAT to give the object different colours
c     - Line elements are probably not handled correctly
 
c     File Format 
c        'code', values      on every line
c       where:
c        #           = comment
c        v <x y z>   = vertex, 
c       (vt, vn, vp  = texture vertex, normal vector, nurbs points)
c        f num()     = polygon (need to count tokens)
c        fo          = as f (obsolete varient)
c        l (num(1:2) = an edge
c        g name      = gives the following objects a group name (cf IMAT)
c        o name      = gives the following objects an object name (cf IMAT)
c

      REAL GC(IGC,*)
      INTEGER NUMS(INUMS,*), NUM(32),num2(32), IO
      do i=1,nn
        write (io,'(a,3G13.4)') 'v', (gc(j,i),j=1,ndim)
      enddo
      DO IEL = 1,NEL
        CALL GET_ELEMENT (NUMS,INUMS,IEL, NUM, NOD,NDIME,ITYPE
     &      ,IMAT,IUSER1,IUSER2,IUSER3)
        IF (IMAT.LE.0) GOTO 11        ! patch to not draw invis elemnts
        CALL GET_FACE (NUM,NOD,NDIME,ITYPE
     &                ,NFACES,NUM2,NN_F,NN_FT, 1 )     !- just NFACES
        DO IFACE=1,NFACES         ! loop this element's faces
          CALL GET_FACE (NUM, NOD,NDIME,ITYPE, IFACE,NUM2,NN_F,NN_FT, 2)
          write (io,'(a,99i6)') 'f',(num2(j),j=1,nn_f)
        enddo
   11   continue    !- CYCLE 
      enddo

      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE R_CRISP_SCD (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL,P)
C
C     This imports a mesh in SAGE CRISP's SCD format
C     These are a set of nodal coords and elements, starting with 
c     keywords, 
c     Other records (eg mat_props are skipped)
C
 
c   File Format 
c     'code', then a table of numbers
c     most table start with the number of entries in it :-)
c   where:
c      code = 'Project', <Node>, <Edge>, <Element>, 'insitu', 'NodeSelect'
c    'Edgeselect', 'ElementSelect', 'superNode', 'superEdge', 'SuperElement'
c    'superNodeselect', 'superEdgeselect', 'superElementselect', 'IncBlock'
c    'EdgeGrading', <materialvalue>
c    'layer', 'Text', 'Font', 
c    'EdgeXFixity', 'EdgeYFixity', 'EdgePWPFixity', 'NodeFixity'
c    'EdgeLoad', 'FaceLoad','Elementexcavate'
c    'Colour', 'Linestyles', 'PlotPoint', 'PlotInfo', 'Ip','Ipselect'
c    'Variables', 'CurrentSelection', 'CurrentIncNumber'
c    'MeshGeneration'
c    'Create Onject', 'Snap To', 'Orthogonal', 'Nove', 'Stretch', 'Filter'
c    'Grid', 'BookMark', <Increment>, 'FEType', 'MPOOutputOptions'
c
c  'Node' format:
c       NN
c     << inode, GC(1:3,inode) >>       !- is lst column Z or a flag?
c       (NN+1)   <- on the last line!
c  'Edge' format:
c       NEDGES
c      << iedge, 2, ifrom, ito, 0 >>
c       NEDGES+1
c  'Element' format:
c       NEL
c     << iel, eltype, igrp, num'(1:4), 0, 0/1 >>,
c      (NEL+1)   <- on the last line!
c      where:
c        eltype = 
c  NOTES:
C       num' = corner nodes only, and is fixed at 4 columns (so 3d ??)
c              also some +ve/-ve, perhaps -ves are on the boundary?
c       igrp is used for construction/ excavation
c
C      element types (second index) 
C       1 - 3-noded bar ....................(2-d)                **
c       2 - 6-noded lst triangle............(2-d)
c       3 - 6-noded lst triangle............(2-d consolidation)
c       4 - 8-noded quadrilateral...........(2-d)                **
c       5 - 8-noded quadrilateral...........(2-d consolidation)  **
c       6 - 15-noded cust triangle..........(2-d)
c       7 - 22-noded cust triangle..........(2-d consolidation)
c       8 - 20-noded brick..................(3-d)                **
c       9 - 20-noded brick..................(3-d consolidation)  **
c      10 - 10-noded tetra-hedra............(3-d)                **
c      11 - 10-noded tetra-hedra............(3-d consolidation)  **
cc
      REAL GC(IGC,*)
      INTEGER NUMS(INUMS,*), NUM(32) , IO, P(*)
      character line*78   !, code*6

      NN_OLD  = NN
      NEL_OLD = NEL

      NDIM  = 2

c------------- 1: look for 'Node' --------
    1 READ (IO,'(a)',IOSTAT=IOS)  line
      CALL IN_TEST(IO,IOS,*1,*999)
      if (line.ne.'Node') goto 1    

      READ (IO,*,IOSTAT=IOS)  NN
      do i=1,nn
        READ (IO,*,IOSTAT=IOS)  inode, (gc(j,nn_old+inode),j=1,3)
      enddo
      READ (IO,*,IOSTAT=IOS)  ifoo    !- NN+1 flag
      WRITE (*,'(A,I7,A)')  '>> ',NN-nn_old,' nodes read'
      NN = NN + NN_OLD
c------------- 2: look for 'Edge' --------
    2 READ (IO,'(a)',IOSTAT=IOS)  line
      CALL IN_TEST(IO,IOS,*1,*999)
      if (line.ne.'Edge') goto 2    

      READ (IO,*,IOSTAT=IOS)  Nedges
      do i=1,nedges
        READ (IO,*,IOSTAT=IOS)  iedge, ifoo, ifrom, ito, ibar
        p((iedge-1)*2+1) = ifrom
        p((iedge-1)*2+2) = ito
      enddo
      READ (IO,*,IOSTAT=IOS)  ifoo    !- Nedge+1 flag
      WRITE (*,'(A,I7,A)')  '>> ',nedges,' edges read'

c------------- 3: look for 'Element' --------
    3 READ (IO,'(a)',IOSTAT=IOS)  line
      CALL IN_TEST(IO,IOS,*3,*999)
      if (line.ne.'Element') goto 3    
      READ (IO,*,IOSTAT=IOS)  NEL
      do iel2=1,nel
        READ (IO,*,IOSTAT=IOS)  iel, icode, imat, (num(j),j=1,4), igrp

c----- find the element type ----
c.. really we should refer to the codes as given in the CRISP F77 source code
       NDIME=2
       NOD=4
       IF (NUM(4).EQ.0) NOD=3
c----- extract the node(s) on this edge -----
       DO J=1,NOD                
         iedge=num(j)
         if (iedge.gt.0) NUM(J) = P((iedge-1)*2+1)
         if (iedge.lt.0) NUM(J) = P((abs(iedge)-1)*2+2)
       ENDDO
       ITYPE=1

c---- morph 4nqs to 8nq's ----
c  ie if icode=5 say, then we should 'danblock' just this element.

c--- store the element ----
       CALL PUT_ELEMENT (NUMS,INUMS,NEL_OLD+IEL, NUM, 
     &                    NOD,NDIME,ITYPE, IMAT,1,1,1)

      enddo
      READ (IO,*,IOSTAT=IOS)  ifoo    !- NEL+1 flag
      WRITE (*,'(A,I7,A)')  '>> ',NEL-NEL_OLD,' elements read'
      NEL = NEL + NEL_OLD

c- now we can look for some other section too - eg material properties.

c------------- 5: look for 'insitu' --------
c  a set of depths with their stress values (and PP)
c    i, y, sx,sy,sz, pp ?
c > in DANFE set point above first and below last to the first/last
c > 

    5 READ (IO,'(a)',IOSTAT=IOS)  line
      CALL IN_TEST(IO,IOS,*5,*999)
      if (line.ne.'insitu') goto 5    
c.. what about a daughter routine to look forwards for a line that
c starts with the given word, return ifail if EOF
c cf DANFE's read next token and IF..ELSEIF each
c.. therefore we could even write a 'to_danfe foo.D converter' ?
c .. or an actual analysis engine that uses this 'interface'
c The caveat is that for keywords that we do not understand, we do not 
c      know how far to skip forwards before we find the next (since 
c      keywords do not have [] wrapped around them (or * before)

c------------- 6: look for 'IncBlock' --------
c. can loop and read some info about each of the '10' steps
c. ie. how many elements get added /removed, if 'apply_gravity' etc.
    6 READ (IO,'(a)',IOSTAT=IOS)  line
      CALL IN_TEST(IO,IOS,*6,*999)
      if (line.ne.'insitu') goto 6    
      READ (IO,*,IOSTAT=IOS) NSTEPS    
      write(*,*) '>> ',NSTEPS,' load steps'
c------------- 7: look for 'materialvalue' --------
c. read a line for each, as //IMAT, E, . ,. v, ...
c   then re-write in DANFE format?
c.  note that each has a character string name too.
    7 READ (IO,'(a)',IOSTAT=IOS)  line
      CALL IN_TEST(IO,IOS,*7,*999)
      if (line.ne.'insitu') goto 7    
      READ (IO,*,IOSTAT=IOS) NMATS
      write(*,*) '>> ',NMATS,' materials read'

c------------- 8: look for 'EdgeXfixity' --------
c as // inode, icode,3,1,0,0,0 // 
c  if icode/=0, then extra lines of data follow each.
c  this module, like EdgeYFixity, Nodefixity, EdgeLoad
c  contain this record repeated, one for each loadstep.

c------------- 8: look for 'Elementexcavate' --------
c as // iel, istep //, 1:nsteps 
c  first set are 'instu', so we can -ve their IMATS :-)
c  for DANFE subsequent steps need to get written as *EXCAVATE_LAYER
c  but split so say 10 files - and #include each in turn. :-) :-)
c  for now I would like to write as *CHANGE_MATERIALS perhaps?
c  then it make my life using DANPLOT much easier
c    because I can #include these file(s)

c  This concept also allows for a nicer way to keep a database of 
c  'what happens when' for DANPLOT (hence got forward/back thru loadsteps)
c

c------------- 10: look for 'Increment' ----------
c ? 10 lines of addition info about each load case.
c
c

c-------------------------
  999 continue    !- if eof.
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE R_CRISP_GPD (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL,P)
C
C     This imports a mesh in SAGE CRISP's GPD format (and GPR format)
C     These are a set of nodal coords and elements, 
c     in records eg 'G'= nodal coords, 'J' = elem steering

c  'Node' format:
c       NN
c     << inode, GC(1:3,inode) >>       !- is lst column Z or a flag?
c       (NN+1)   <- on the last line!
c  'Edge' format:
c       NEDGES
c      << iedge, 2, ifrom, ito, 0 >>
c       NEDGES+1
c  'Element' format:
c       NEL
c     << iel, eltype, igrp, num'(1:4), 0, 0/1 >>,
c      (NEL+1)   <- on the last line!
c      where:
c        eltype = 
c  NOTES:
C       num' = corner nodes only, and is fixed at 4 columns (so 3d ??)
c              also some +ve/-ve, perhaps -ves are on the boundary?
c       igrp is used for construction/ excavation
c
C      element types (second index) - see subroutien CRISP_ELEMENTS
C      element types (second index) - 
C       1 - 3-noded bar ....................(2-d)                **
c       2 - 6-noded lst triangle............(2-d)
c       3 - 6-noded lst triangle............(2-d consolidation)
c       4 - 8-noded quadrilateral...........(2-d)                **
c       5 - 8-noded quadrilateral...........(2-d consolidation)  **
c       6 - 15-noded cust triangle..........(2-d)
c       7 - 22-noded cust triangle..........(2-d consolidation)
c       8 - 20-noded brick..................(3-d)                **
c       9 - 20-noded brick..................(3-d consolidation)  **
c      10 - 10-noded tetra-hedra............(3-d)                **
c      11 - 10-noded tetra-hedra............(3-d consolidation)  **
cc
      REAL GC(IGC,*)
      INTEGER NUMS(INUMS,*), NUM(32) , IO, P(*)
      character line*78   !, code*6

      NN_OLD  = NN
      NEL_OLD = NEL

c     NDIM  = 2

c------------- 1: look for 'RECORD C' : Mesh Sizes --------
    1 READ (IO,'(a)',IOSTAT=IOS)  line
      CALL IN_TEST(IO,IOS,*1,*999)
      if (index(line,'RECORD C ').le.0) goto 1    
      read(io,*)   !- skip line
      read(io,*)   NN, NEL,  MXNDV, MXTYP, NDIM, IPLOT

c------------- 2: look for 'RECORD G' : Nodal Coords --------
    2 READ (IO,'(a)',IOSTAT=IOS)  line
      CALL IN_TEST(IO,IOS,*2,*999)
      if (index(line, 'RECORD G ').le.0) goto 2    
      read(io,*)   !- skip line
      do i=1,nn
        READ (IO,*,IOSTAT=IOS)  inode, (gc(j,nn_old+inode),j=1,ndim)
      enddo
      WRITE (*,'(A,I7,A)')  '>> ',NN-nn_old,' nodes read'
      NN = NN + NN_OLD

c------------- 2: look for 'RECORD J' : Element Steering --------
    3 READ (IO,'(a)',IOSTAT=IOS)  line
      CALL IN_TEST(IO,IOS,*3,*999)
      if (index(line, 'RECORD J ').le.0) goto 3    
      read(io,*,iostat=ios)   !- skip line
      do iel2=1,nel
       read(io,*,iostat=ios) IEL, ITYP, IMAT, (NUM(J),J=1,4)

c----- find the element type ----
c.. really we should refer to the codes as given in the CRISP F77 source code
       NDIME=ndim
       NOD=4
       IF (NUM(4).EQ.0) NOD=3

c  really we need a function (like in PHASES) that returns NOD,NDIME,ITYPE 
c  if given ITYP

       itype=1
       CALL PUT_ELEMENT (NUMS,INUMS,NEL_OLD+IEL, NUM, 
     &                    NOD,NDIME,ITYPE, IMAT,1,1,1)
      enddo
      WRITE (*,'(A,I7,A)')  '>> ',NEL-NEL_OLD,' elements read'
      NEL = NEL + NEL_OLD

c-------------------------
  999 continue    !- EOF

      RETURN
      END

c-----------------------------------------------------------------------
      SUBROUTINE R_CRISP_MPO (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL,P)
C
C     This imports a mesh in SAGE CRISP's GPO format
C        Dan Kidger 14-9-98 
C   These are a set of nodal coords and elements, after midside node creation
c   The file is designed for human, not machine reading :-(
c
c  File Format:
c       0: lots of titles

      REAL GC(IGC,*)
      INTEGER NUMS(INUMS,*), IO, P(*)
c     INTEGER NUM(32), NUM2(32)
c     character line*78   !, code*6
!     integer crisp_element
!     external crisp_element

c---- 1: look for headers

c---- 2: loop load cases
C    DO ILD=1,NLDS
c---- 3: look for load case
c  START  OF  LOAD  INCREMENT  BLOCK  NUMBER      1

c--- 4: look for a table..
c may be:
c    1.  -   LIST OF ELEMENT ALTERATIONS
c    2.  -   NODAL DISPLACEMENTS
c    3.  -   STRESSES AT ELEMENT CENTROIDS
c    4.  -   STRAINS AT ELEMENT CENTROIDS
c    5.  -   0CAM CLAY PARAMETERS AT CENTROIDS
c    6.  -   LIST OF REACTIONS
c            (NODAL LOADS EQUIVALENT)
c    7.  -   EQUILIBRIUM CHECK
c    8.  -  **** RESULTS FROM INCREMENT    1    HAS BEEN WRITTEN TO UNIT 2 (DISK) ****




c if
c    1.  skip 1 line (+1blank) then read a raw list of elements, flip 
c        their IMATs and maybe write out as *CHANGE_ELEMENTS ?
c    2.  skip 5 lines (+1blank) until NODE is found, then read as:
c          // inode, dxi,dyi, dx,dy // (where dxi,dyi are the incremental)
c          I guess 3d has dzi and dz too.
c    3.  skip 3 lines until ELEMENT is found then read as:
c         // iel, xc,yc, sx',sy',sz', txy, pp, s1,s2,theta //
c         store as 1 gp sx,sy,txy,sz
c          I assume 3d is similar but we had better check.
c    8. then we have finished this load case so start looking for the next


      RETURN                                 
      END

c-----------------------------------------------------------------------
      SUBROUTINE R_CRISP_GPO (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL,P)
C
C     This imports a mesh in SAGE CRISP's GPO format
C        Dan Kidger 14-9-98 
C   These are a set of nodal coords and elements, after midside node creation
c   The file is designed for human, not machine reading :-(
c
c  File Format:
c       0: lots of titles
c          includes lines that tell me the total NN, NEL, etc.
c       1: look for 'CO-ORDINATES OF VERTEX NODES'
c          skip 3 lines then read //inode,x,y,(z)//
c          (until we ifail on 'IRNFR =   1' line)
c       2: ('skip over OPTIMISED SOLUTION ORDER OF ELEMENTS')
c       3: look for 'ELEMENT TYPE  MAT    1     2     3     ..'
c          (read //iel,code,imat, num(1:4)//)
c          skip this as we want the later complete table
c       4: look for 'COORDINATES OF DISPLACEMENT NODES ALONG ELEMENT SIDES'
c          skip 3 lines then read //inode,x,y,(z)// for midside nodes
c          until we ifail on 'MAXIMUM FRONT WIDTH FOR ..'
c       5: look for: 'ELEMENT TYPE  MAT    1     2     3    ..' again
c          this time read //iel,code,imat, num(1:nod)//
c           <via mung of ICODE to NOD)
c       6: EOF

      REAL GC(IGC,*)
      INTEGER NUMS(INUMS,*), NUM(32), NUM2(32) , IO, P(*)
      character line*78   !, code*6
      integer crisp_element
      external crisp_element

      NN_OLD  = NN
      NEL_OLD = NEL
      NDIM= 2       !- hack to 2d ?

c------- 1: look for 'CO-ORDINATES OF VERTEX NODES' ----------
      do iline=1,999999
        read (io,'(a)',iostat=ios) line
        if (index(line,'CO-ORDINATES OF VERTEX NODES').gt.0) goto 11
      enddo
      call myerror(2,'vertex-nodes tables not found (R_CRISP_GPO)')
   11 continue
      read (io,'(a)',iostat=ios) (line,j=1,3)    !- skip 3 lines
      do i=1,999999
        read (io,*,iostat=ios) inode,(gc(j,nn_old+inode),j=1,ndim)
        if (ios.ne.0) goto 12   !ifail on 'IRNFR =   1' line
        nn= max(nn,nn_old+inode)
      enddo
   12 nn_new= (i-1)
      WRITE (*,'(A,I7,A)')  '>> ',NN_NEW,' corner nodes read'

c------- 2: look for 'COORDINATES OF DISPLACEMENT NODES' -------
      do iline=1,999999
        read (io,'(a)',iostat=ios) line
        if (index(line,'COORDINATES OF DISPLACEMENT').gt.0) goto 13
      enddo
      call myerror(2,'mid-side nodes table not found (R_CRISP_GPO)')
   13 continue
      read (io,'(a)',iostat=ios) (line,j=1,3)    !- skip 3 lines
      do i=1,999999
        read (io,*,iostat=ios) inode,(gc(j,nn_old+inode),j=1,ndim)
        if (i.eq.1) then    !---- fill empty space with blanks ---
          do ii=nn+1,inode-1
            do j=1,ndim
              gc (j,ii) = 0.
            enddo
          enddo
        endif
        if (ios.ne.0) goto 14   !ifail on 'MAXIMUM FRONT ..' line
        nn= max(nn,nn_old+inode)
      enddo
   14 nn_new= (i-1)
      WRITE (*,'(A,I7,A)')  '>> ',NN_NEW,' edge nodes read'


c------- 3: look for 'ELEMENT TYPE  MAT    1     2 ...' -------
      do iline=1,999999
        read (io,'(a)',iostat=ios) line
        if (index(line,'ELEMENT TYPE  MAT').gt.0) goto 15
      enddo
      call myerror(2,'(second) element table not found (R_CRISP_GPO)')
   15 continue
      imat_max=0
      do iel2=1,999999
        read (io,*,iostat=ios) 
     &   iel, icode, imat, (num2(j),j=1,crisp_element(icode,2))
        if (ios.ne.0) goto 16   !ifail on 'MAXIMUM FRONT ..' line
        ndime = crisp_element(icode,1)
        nod   = crisp_element(icode,2)
        itype = crisp_element(icode,3)
        imat_max= max(imat_max,imat)
        do j=1, nod
          num(crisp_element(icode,100+j)) = num2(j)
        enddo
        CALL PUT_ELEMENT (NUMS,INUMS,NEL+IEL2, NUM, 
     &                    NOD,NDIME,ITYPE, IMAT,1,1,1)
      enddo
   16 nel_new= (iel2-1)
      nel= nel + nel_new
      WRITE (*,'(A,I7,A)') '>> ',NEL_NEW,' elements read'
      WRITE (*,'(A,I3,A)') '>> with',imat_max,' materials'

      RETURN
      END

C-----------------------------------------------------------------------
      function CRISP_ELEMENT (icode,iop)
C
C     This 'Onject' delivers information about CRISP elements
C       Dan Kidger  11-9-98
c
c  This information is taken from CRISPS2.FOR (c.1994)
c  For ecah element type, we need to now:
c    1. NDIME (type: 1=1d, 2-7=2d, 8-11=3d)
c    2. NOD   (type: 1=3nl, 2-3=6nt, 4-5=8nq, 6-7=15nt, 8-9=20nb, 10-11=4ntet
c    3. ITYPE = 1 by defautl , perhaps =5 for CONSOL (type=3,5,7,9,11)
c    4. NOD_parent, minimum element polygon, 
c        (type: 1=2nl, 2-3=3nt, 4=5=4nq, 6-7=3nt, 8-9=8nb, 10-11=4ntet
c  100+  NUM2 - to morph NUM into my order :-)

c   also:
c     A1: total NOD, eg type 6=15nt+10nt = 22nt
c     A2: # edges, # faces (3d), #pp nodes, # edge nodes (disp, PP)
c     A3: # middle nodes (disp,pp)
c     A4: NGP (and pointer to those entries), # local coords (=3 for tris)
c     A5: NDOF_tot (can derive this)
c     A6: this centroid GP (hmm do they all have one?)
c     A7: the DOFs for each node (* This has important echoes *)

C      element types (second index) - 
C       1 - 3-noded bar ....................(2-d)                **
c       2 - 6-noded lst triangle............(2-d)
c       3 - 6-noded lst triangle............(2-d consolidation)
c       4 - 8-noded quadrilateral...........(2-d)                **
c       5 - 8-noded quadrilateral...........(2-d consolidation)  **
c       6 - 15-noded cust triangle..........(2-d)
c       7 - 22-noded cust triangle..........(2-d consolidation)
c       8 - 20-noded brick..................(3-d)                **
c       9 - 20-noded brick..................(3-d consolidation)  **
c      10 - 10-noded tetra-hedra............(3-d)                **
c      11 - 10-noded tetra-hedra............(3-d consolidation)  **

      integer crisp_element
      integer table(11,8)
      integer t_3nl(3), t_6nt(6),t_8nq(8)

      data t_3nl /1,3,2/    !- ie col 2= "where crisp puts node #2"
      data t_6nt /1,5,3,6,4,2/
c     data t_6nt /1,3,5,2,4,6/
      data t_8nq /1,7,5,3,8,6,4,2/

      data (table(j,1),j=1,11)                   !- NDIME 
     &     / 1, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3/
      data (table(j,2),j=1,11)                   !- NOD (of parent)
     &     / 3, 6, 6, 8, 8,15,15,20,20,10,10/
      data (table(j,3),j=1,11)                   !- ITYPE 
     &     / 1, 1, 5, 1, 5, 1, 5, 1, 5, 1, 5/
      data (table(j,4),j=1,11)                   !- NOD_corners
     &     / 2, 3, 3, 4, 4, 3, 3, 8, 8, 4, 4/    !- cf via GET_FACE
      data (table(j,5),j=1,11)                   !- ITYPE 
     &     / 1, 1, 5, 1, 5, 1, 5, 1, 5, 1, 5/

c----------- 1: basic data ----------
      if (iop.le.3) then
        crisp_element = table(icode,iop)

c----------- 2: morphs to DANFE node ordering scheme ----------
      elseif (iop.gt.100) then

c    given a record, we want to morph into my prefered order
c    ie. here we store a list of their nodes, but in my order
c    such that NUM(p(I))) = NUM_crisp(I)  ; where P=(/1,3,5,7,2,4,6,8/)
c    so can WR_CRISP_SCD too as WRITE (num(p(i)),i=1,nod)
        inode = iop-100
        if (icode.eq.1) then
          crisp_element = t_3nl (inode)
        elseif (icode.eq.2.or.icode.eq.3) then
          crisp_element = t_6nt (inode)
        elseif (icode.eq.4.or.icode.eq.5) then
          crisp_element = t_8nq (inode)
        else
          crisp_element=inode     !- default is epinomous
        endif
      else
        call myerror (1,'unknown crisp element code')
      endif

      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE R_TECPLOT (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL)
C
C     This imports a mesh in 'TECPLOT Format'
C           Dan Kidger 18-3-94
C     .. as long as the data is hacked slightly for ease of reading
c     .. also only valid for '2d surfaces' embedded in 2d/3d
C     11-9-98  some further work to allow reading of unhacked files
c
c    File Format
c      Keywords, followed by a list of data, first item follows keyword 
c      rest on subsequent lines (one per line)
c
c   -- following derived from D6.TEC : Space Plane --
c    TITLE     = "foo title"         !- may be zero length
c    VARIABLES = "Z"  
c         then X,Y,P,U,V,W           !- so read disps data too?
c    ZONE T    =" foo block "        !- a block (IMAT), 10 chars
c  *    I=26, J=28, K=1,F=POINT      !- NXP,NYP, NZP=1 for sheets
c                                    ! F=POINT for structured grids
c                                    !
c     // z,x,y,p,u,v,w  //           ! one for each as given above
c                                    ! so read then find and store X etc.
c  * if F=FEPOINT then line is: 
c     N=612, E=1181,F=FEPOINT ET=Quadrilateral
c     so table is followed by 1181 NUM()s, if a tri then repeat last index)


      REAL GC(IGC,*)
      INTEGER NUMS(INUMS,*), NUM(32) , IO
      character line*255, title*255, vars(20)*20, zonetitle*255, 
     &   token*20, value*20,
     &   meshtype*20, eltype*20
      real varnode(19)      !- variables at each node
      INTEGER  istr_end, str_to_i
      EXTERNAL istr_end, str_to_i

      NDIM = 2        !- if we find 'Z' then set NDIM to 3

      OPEN (IO+1,FILE='TECPLOT.OUT')    !- contour vars
      write (io+1,'(a)') '#',
     & '#              Tecplot Contour variables',
     & '#    abstracted into DANPLOT format by R_TECPLOT in DANLIB',
     & '#                 Dan Kidger 11-9-98',
     & '#',
     & '*DISPLACEMENTS'

c---------- 1: find TITLE -----------
    1 READ (IO,'(a)',IOSTAT=IOS)  line
      CALL IN_TEST(IO,IOS,*1,*999)
      if (line(1:5).ne.'TITLE') goto 1    

      iend = 0
      CALL GET_NEXT_TOKEN (LINE, IBEG,IEND,'= ')  !- skip 'TITLE'
      CALL GET_NEXT_TOKEN (LINE, IBEG,IEND,'= ')
      title ='.'
      if ( (iend-1)-(ibeg+1). ge.1)          !- title was given
     & title = line (ibeg+1:iend-1)          !- strip 
      print*,title(1:istr_end(title))
      write(io+1,'(a,a)') '! ', title(1:istr_end(title))

c---------- 2: find VARIABLES -----------
    2 READ (IO,'(a)',IOSTAT=IOS)  line
      CALL IN_TEST(IO,IOS,*2,*999)
      if (line(1:9).ne.'VARIABLES') goto 2    
        iend = 0
        CALL GET_NEXT_TOKEN (LINE, IBEG,IEND,'=" ') ! skip 'VARIABLES'
        CALL GET_NEXT_TOKEN (LINE, IBEG,IEND,'=" ')

        ixcol = 0    !- defaults
        iycol = 0
        izcol = 0

        do ivar=1,20       !- loop possible variables
          vars(ivar) = line (ibeg:iend)
          if (vars(ivar).eq.'X') then
            ixcol=1      !- really where 'X' appeared
          elseif (vars(ivar).eq.'Y') then
            iycol=2
          elseif (vars(ivar).eq.'Z') then
            izcol=3
            ndim = 3
          endif
c       --- get the next token from the next line ---
c       I hope that we never have more than one on each line
   22   READ (IO,'(a)',IOSTAT=IOS)  line 
        CALL IN_TEST(IO,IOS,*22,*999)
        iend = 0
        CALL GET_NEXT_TOKEN (LINE, IBEG,IEND,'"')
c       if (ibeg.le.0) then          !- no more "quoted" values found
        if (line(1:1).ne.'"') then   !- no more "quoted" values found
          BACKSPACE(IO)
          goto 23
        ENDIF
      enddo
   23 nvars=ivar-1     !- total # of variables
      write (*,'(12A6)') (vars(j)(1:istr_end(vars(j))), j=1,nvars)
      write (io+1,'(A,12A6)') '! ',
     &    (vars(j)(1:istr_end(vars(j))), j=1,nvars)

c---- decide which column is which ----
c.. should imply from where 'X','Y','Z' appear 
c..for u,v,w we can also write *DISPLACEMENTS to a daughter file too :-)
        ixcol=1      !- really where 'X' appeared
        iycol=2
        izcol=3
c.. if 'Z' appeared then we are in 3D

c---------- 3: find ZONE (and repeat until EOF) -----------
      do izone=1, 99 !nzones
        NN_OLD  = NN
       NEL_OLD = NEL

c.. if we reach EOF, then
    3 READ (IO,'(a)',IOSTAT=IOS)  line
c     CALL IN_TEST(IO,IOS,*3,*999)
      if (IOS.ne.0) then
c        close(io)
c        io = io-1
        goto 999
      endif
      if (line(1:4).ne.'ZONE') goto 3    

c--- 3a: get zone title ---
c.. hmm this will clip at internal spaces :-(
c.. why not just look for the first and second " on the line?
      iend = 0
      CALL GET_NEXT_TOKEN (LINE, IBEG,IEND,'=" ')    !- skip 'ZONE'
      CALL GET_NEXT_TOKEN (LINE, IBEG,IEND,'=" ')
      zonetitle ='.'
      if ((iend-1)-(ibeg+1). ge.1)          !- title was given
     & zonetitle = line (ibeg+1:iend-1)          !- strip 
      write(*,'(i3,a,a)') izone, ': ',zonetitle(1:istr_end(zonetitle))
c      write(io+1,'(a,i3,a,a)') '!' ,izone,
c     &    ': ',zonetitle(1:istr_end(zonetitle))

c--- 3b: parse zone info ---
    4 READ (IO,'(a)',IOSTAT=IOS)  line
      CALL IN_TEST(IO,IOS,*4,*999)

      i = 1  !... defaults 
      j = 1 
      k = 1 
      nn_new  = 0 
      nel_new = 0 
      iend = 0
      DO K=1,99
        CALL GET_NEXT_TOKEN (LINE, IBEG,IEND,'=, ')
        if (ibeg.le.0) goto 41
        token=line(ibeg:iend)
        CALL GET_NEXT_TOKEN (LINE, IBEG,IEND,'=, ')
        value=line(ibeg:iend)
        IF (token.eq.'I') THEN
          nxp=str_to_i(value,ifail)
        ELSEIF (token.eq.'J') THEN
          nyp=str_to_i(value,ifail)
        ELSEIF (token.eq.'K') THEN
          nzp=str_to_i(value,ifail)
        ELSEIF (token.eq.'N') THEN
          nn_new=str_to_i(value,ifail)
        ELSEIF (token.eq.'E') THEN
          nel_new=str_to_i(value,ifail)
        ELSEIF (token.eq.'F') THEN       !- eg. 'POINT', 'FEPOINT'
          meshtype=value
        ELSEIF (token.eq.'ET') THEN      !- eg 'Quadrilateral'
          eltype=value
        ELSE
          call myerror(1,'Unknown ZONE token (R_TECPLOT)')
        ENDIF
      ENDDO
   41 CONTINUE
      if (nn_new.eq.0) NN_NEW  = NXP * NYP * NZP
      if (nel_new.eq.0) 
     & NEL_NEW = max(1,NXP-1) * max(1,NYP-1) * max(1,NZP-1) 

c---------- 3c: Which type of mesh data do we have? ----

      IF (meshtype.eq.'POINT'.or.meshtype.eq.'FEPOINT') THEN

c---- 3d: read each line of nodal data 
      DO I=1,NN_NEW
        READ (IO,*)   (VARNODE(J),j=1,nvars)
c---- 3e: morph to extract the x,y,(z) coords
        if (ixcol.ge.0) gc (1,nn_old+i) = varnode(ixcol)
        if (iycol.ge.0) gc (2,nn_old+i) = varnode(iycol)
        if (izcol.ge.0) gc (3,nn_old+i) = varnode(izcol)

        write (io+1,'(i6,19G14.5)') 
     &   nn_old+i, (varnode(j),j=ndim+1,nvars)
      ENDDO
      WRITE(*,'(A,I8,A)')  '>>',NN_NEW,' nodes created'
      NN = NN_OLD + NN_NEW

      IMAT=IZONE
      IF (meshtype.eq.'POINT') THEN    !---- implicit NUM ----
        if (nzp.eq.1) then !-- 2d grids
          NOD   = 4      !- All 4nq's
          NDIME = 2      !-- always 2D facets ? (cf FEBLOCK)
          ITYPE = 1      !-- all are 4nq's

          IEL = 0
          DO IQ = 1,NYP-1
            DO IP = 1,NXP-1
              IEL = IEL + 1
              NUM (1) = (IQ-1) * NXP + IP
              NUM (2) = NUM(1) + NXP
              NUM (3) = NUM(2) + 1
              NUM (4) = NUM(3) - NXP
              do j=1,nod
                num(j) = num(j) + nn_old
              enddo
              CALL PUT_ELEMENT 
     &       (NUMS,INUMS,NEL_OLD+IEL, NUM, NOD,NDIME,ITYPE,IMAT,1,1,1)
            ENDDO
          ENDDO
        else !-- 3d grids
          NOD   = 8      !- All 4nq's
          NDIME = 3      !-- 8-node bricks
          ITYPE = 1      !-- all are 4nq's

          NXE=NXP-1
          NZE=NYP-1
          NYE=NZP-1
c          NXE=NXP-1
c          NYE=NYP-1
c          NZE=NZP-1
          IEL = 0
          DO IQ = 1,NYE
            DO IP = 1,NXE
              DO IS = 1,NZE
              IEL = IEL + 1
              NUM(1) = (IQ-1)*(NXE+1)*(NZE+1)+IS*(NXE+1)+IP
              NUM(2) = NUM(1)-NXE-1
              NUM(3) = NUM(2)+1
              NUM(4) = NUM(1)+1
              NUM(5) = NUM(1)+(NXE+1)*(NZE+1)
              NUM(6) = NUM(5)-NXE-1
              NUM(7) = NUM(6)+1
              NUM(8) = NUM(5)+1
              do j=1,nod
                num(j) = num(j) + nn_old
              enddo
              CALL PUT_ELEMENT 
     &       (NUMS,INUMS,NEL_OLD+IEL, NUM, NOD,NDIME,ITYPE,IMAT,1,1,1)
            ENDDO
            ENDDO
          ENDDO
        endif

      ELSEIF (meshtype.eq.'FEPOINT') THEN  !---- explicit NUM ----
        DO IEL=1,NEL_NEW
          NOD   = 4      !- All 4nq's (or 3nts)
          NDIME = 2      !-- always 2D facets ?
          ITYPE = 1      !-- all are 4nq's
          READ (IO,*) (NUM(J),J=1,NOD)
          IF (NUM(NOD-1).EQ.NUM(NOD)) NOD=NOD-1     !- 4nq->3nt
          do j=1,nod
            num(j)=num(j) + nn_old  !- add offset
          enddo
          CALL PUT_ELEMENT 
     &   (NUMS,INUMS,NEL_OLD+IEL, NUM, NOD,NDIME,ITYPE,IMAT,IEL,1,1)
        ENDDO
      ELSE
        call myerror (2,'unknown TECPLOT grid' )
      ENDIF
      WRITE(*,'(A,I8,A)')  '>>',NEL_NEW,' elements formed'
      NEL= NEL_OLD + NEL_NEW

c-------------------------------------
      ELSE    !---- mesh type ---
        call myerror (2,'unknown TECPLOT grid type')
      ENDIF

      ENDDO    !-- loop ZONEs until End_of_File

  999 CONTINUE
      close(io+1)   !- output file
      END

C-----------------------------------------------------------------------
      SUBROUTINE R_CAROLUS (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL)
C
C     This imports a mesh in Carolus's format  ( always 8nq's)
c      this also carrys NF which I just junk!
c
C       data format :
C        .   |header info
C        .   < iel, NUM(), IMAT, (load-step) >
C        .   header info
C        .   < inode, nf(), X, Y >
C        .   'trailer' info
C                                                 DJK .. 13-6-94
      REAL GC(IGC,*)
      INTEGER NUMS(INUMS,*), NUM(32) , IO

      NN_OLD  = NN
      NEL_OLD = NEL

        NDIM  = 2
        NDIME = 2   !-- always 2D facets
        NOD   = 8   !- always 8-noded quads
        ITYPE = 1   !-- as 8nq's

c............ pre-scan to find the start of the data ................
    1   READ (IO,*,IOSTAT=IOS)  iel, (num(i), i=1,nod), imat, ijunk 
        if (ios.ne.0) goto 1
        BACKSPACE (io)
c...................... element steering ............................
      DO K = 1, 99999
        READ (IO,*,IOSTAT=IOS)  iel, (num(i), i=1,nod), imat, ijunk 
        if (ios.ne.0) goto 3           !- treat ALL errors as comments !
        DO J=1,NOD
          NUM(J) = NUM(J) + NN_OLD   !-- offsets
        ENDDO
        CALL PUT_ELEMENT 
     &  (NUMS,INUMS,NEL_OLD+IEL, NUM, NOD,NDIME,ITYPE, IMAT,1,1,1)
        nel = max (nel,nel_old+iel)
      ENDDO
  3   continue
      WRITE(*,'(A,I7,A)')  '>> ',NEL-NEL_OLD,' elements read'

c...................... nodal coordinates ...........................
      DO K=1, 99999
        READ (IO,*,iostat=ios)  
     &       inode, idum,idum,idum,  (GC(J,Inode+NN_OLD),J=1,2)
        if (ios.ne.0) goto 4           !- treat ALL errors as comments !
        nn = max (nn,nn_old+inode)
      ENDDO

    4 continue
      WRITE(*,'(A,I7,A)')  '>> ',NN-nn_old,' nodes read'
      END

C-----------------------------------------------------------------------
      SUBROUTINE R_TIMLIU (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL,P)
C
C     This imports a mesh in Tim Liu's format
c        ( 8nq's + some 2-node lines in 3D)
c
C       data format :
C        .   TITLE line
C            NN, NEL, NLOAD, NFIX, NPRO,  MW,M,MV,N,TOL,timlim
C            ==  ===
C        .   < inode_code,  X, Y > (1:nn)
C        .   < iel_code, NUM(1:8), IMAT > (1:nel)
C        .   'trailer' data for loadsteps and analysis
C                                                 DJK .. 4-3-95
      REAL GC(IGC,*)
      INTEGER NUMS(INUMS,*), NUM(32), P(NN), IO

      NN_OLD  = NN
      NEL_OLD = NEL

      NDIM  = 3               !- always embedded in 3D space

c------------------------------- Tim's code ----------------------------
c.. P() is the node's 'label' as used in the Element_Steering Section
      read(IO,*)          !- dummy for the title ?
      read(io,*) NN,NEL      !+ ,NLOAD,NFIX,NPRO,MW,M,MV,N,TOL,timlim
      do i=1,nn
        read(io,*) P(I), (GC(j,NN_OLD+I), j=1,3)   
        P(i) = p(i) + nn_OLD                 !- add offset here too
      enddo
      WRITE(*,'(A,I8,A)')  '>>',NN,' nodes read'

c------------------------------ read elements --------------------------
c.. note Tim uses Abaqus-like 8nqs: first 4 corners then 4 midside nodes
      N_2nl = 0
      N_8nq = 0
      DO IEL=1,NEL
        NDIME = 2          !-- assume 8nq's
        Nod   = 8
        ITYPE = 1   
        read(io,*) IEL_code
     &    ,NUM(1),NUM(3),NUM(5),NUM(7), NUM(2),NUM(4),NUM(6),NUM(8)
     &    ,IMAT
        IMAT = IMAT + 1    !- add 'cos Tim uses IMAT=0 :-)
        IF (NUM(5).EQ.0) THEN    !- 'rods' .. maybe kill altogether ?
          n_2nl = n_2nl + 1
          NDIME = 1              !- 2 node line elemnts
          NOD = 2   
          NUM(2) = NUM(3)        !- copy back the second node
        ELSE
          n_8nq = n_8nq + 1
        ENDIF

c------------------ convert node labels into their node# ---------------
c.. do a complete scan of the label table (no pre-sorting!)
        DO J=1,NOD
          DO I=1,NN
            IF (NUM(J).eq.P(I)) THEN     !- found it!
              NUM(j) = i
              GOTO 10                    !- so can skip out
            ENDIF
          ENDDO
          PRINT*,'>> Nod name not found in R_TIM_LIU : ',NUM(j)
          STOP
   10     CONTINUE
        ENDDO

c---------------- add offsets from the existing mesh -------------------
        DO J=1,NOD
          NUM(J) = NUM(J) + NN_OLD  
        ENDDO

c------------------- add to the element database -----------------------
        CALL PUT_ELEMENT (NUMS,INUMS,NEL_OLD+IEL, NUM, !(note IUSER1)
     &                     NOD,NDIME,ITYPE, IMAT,IEL_code,1,1)
      ENDDO
      WRITE(*,'(A,I5,A/ a,i8,a,i8,a)') 
     &       '>> ',NEL,' elements read'
     &    ,  ' (',  n_8nq,' 8-node quads +', n_2nl,' 2-node rods )'
      NN  = NN  + NN_OLD
      NEL = NEL + NEL_OLD 

      END

C-----------------------------------------------------------------------
      SUBROUTINE R_DANPLOT_KEY (IO,IPR,GC,IGC,NDIM,NN,NUMS,INUMS,NEL,P)
C
C     This imports a mesh in my standard KEYWORD format
C        Dan Kidger  6-2-98
C     This will read *2D, *3D,*NODES,*ELEMENTS plus some other tokens
C
      REAL GC(IGC,*)
      INTEGER NUMS(INUMS,*), P(*), IO

      CHARACTER KEYWORD*70            ! data input keyword 'token'
      LOGICAL FOUND
      INTEGER NN,nel,ikeyword, u0,u1

      U0 = IO
      U1 = U0                        !-- start from the base file = U0
      DO IKEYWORD = 1, 9999
        CALL GET_KEYWORD (U1,U0,KEYWORD)
        CALL PRINT_KEYWORD (IKEYWORD, KEYWORD,.false.)

c------------------------ find the '*keyword' --------------------------
      CALL GET_SECS (TIME1)
      FOUND = .TRUE.

      IF (KEYWORD.EQ.'*EOF') THEN
        close (u1)                        !- always close this input file.
        if (u1.eq.u0) then                !- if back at the root
          GOTO 888                        !- *all done*
        ENDIF
c      ELSEIF (KEYWORD.EQ.'*AUTOPLOT') THEN
c        AUTOPLOT = .TRUE.
c      ELSEIF (KEYWORD.EQ.'*CONTROL') THEN
c      ELSEIF (KEYWORD.EQ.'*PLOT_MESH_BOUNDARY') THEN
c        CALL PLOT_MESH_BOUNDARY (GC,IGC,NDIM,NN,RINGS)
C-----------------------------------------------------------------------      
      ELSE
        FOUND = .FALSE.
      ENDIF

C--------------------- 'standard' keywords -----------------------------

      IF (.NOT.FOUND) CALL KEY_MESH_READ
     &        (FOUND,ipr,KEYWORD,U1,GC,IGC,NDIM,NN,NUMS,INUMS,NEL,P)
      IF (.NOT.FOUND) CALL KEY_MESH_IO
     &        (FOUND,ipr,KEYWORD,U1,GC,IGC,NDIM,NN,NUMS,INUMS,NEL,P)
c      IF (.NOT.FOUND) CALL KEY_FACETS
c     &        (FOUND,ipr,KEYWORD,U1,GC,IGC,NDIM,NN,NUMS,INUMS,NEL,P,
c     &         FACETS,IFCETS,NFCETS, RINGS,IRINGS)
c     NODOF=NDIM    !- enforce here ?


c... only here if *None* of the handlers found the keyword
      IF (.NOT.FOUND) THEN
!        CALL MYERROR(1,'unknown keyword:'//KEYWORD)
         print*, 'skipping',KEYWORD
      ENDIF  

c     IPR.EQ.2
      IF (IPR.GE.3) THEN             ! (now same as in DANFE)   
         CALL GET_SECS (TIME2)
         WRITE(*,'(A, I7,a,I7,a, A, a,f7.3,a, A,  a,f8.2,A )')
     &    char(27)//'[41m'//
     &    '-------'
     &   , NN,' n'  ,NEL,' e' 
     &   ,'----------'
     &   ,' Mem=',FREE_MEMORY(1),'Mb'
     &   ,'----------' 
     &   ,' dt=',time2-time1
     &   ,' ----'
     &    //char(27)//'[40m'
      ENDIF

c      WRITE(*,'(A,2(A,I7),A)') '--------------------------------------'
c     & ,' Now:',NN,' Nodes and',NEL,' Elements'

      ENDDO    !-- loop keywords/command-line data files.

  888 CONTINUE
      RETURN
      END

c-----------------------------------------------------------------------
      SUBROUTINE R_PHASES (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL,P)
C
C     This imports a mesh in 'Phases' format
c      ( all 2D : mostly 3nt, some 2nl's for 'bolts')  
c      as used by Golder Associates for the CERN project
c        Dan KIdger  17-11-95
c
      REAL GC(IGC,*)
      INTEGER NUMS(INUMS,*), NUM(32), P(NEL), IO

      NN_OLD  = NN
      NEL_OLD = NEL

      NDIM  = 2               !- always embedded in 3D space

c.. hmm first we aught to  scan for the  'beginning' of data flag..

c------------------------
    2 READ (IO,*,IOSTAT=IOS) Nstages
      IF (IOS.NE.0) CALL IN_TEST(IO,IOS,*2,*999)

    3 READ (IO,*,IOSTAT=IOS) N_BE_Regions, N_BE_1, N_BE_2, N_BE_N
      IF (IOS.NE.0) CALL IN_TEST(IO,IOS,*3,*999)
      NEL = N_BE_1 + N_BE_2

c------------------------
    4 READ (IO,*,IOSTAT=IOS) (P(I),I=1,NEL)
      IF (IOS.NE.0) CALL IN_TEST(IO,IOS,*4,*999)

c------- get total size of NUMS = sum  (nod) for all elems) --------
c.. I dont need this info!
    5 READ (IO,*,IOSTAT=IOS) NOD_NEL
      IF (IOS.NE.0) CALL IN_TEST(IO,IOS,*5,*999)
      WRITE(*,'(A,I6,A)')  '>> (NOD_NEL =',NOD_NEL,' )'

c------------------- read in the element steering  ---------------------
      DO IEL=1,NEL
        JJJ = P(IEL)

C---------- determine #nodes per elem. from its code) ------
      NDIME = 2
      ITYPE = 1
      IMAT = 1     

      IF(JJJ.EQ.0) THEN        ! -- Truss element, 2D, linear, 2 nodes
        NOD = 2                !    
      ELSEIF(JJJ.EQ.1) THEN    ! --   Beam/Column element, 2D
        NOD = 2                !        , hermitian cubic,2 nodes
      ELSEIF(JJJ.EQ.2) THEN    ! --   Continuum element, 2D, plane strain
        NOD = 3                !      3, linear, noded triangle
      ELSEIF(JJJ.EQ.3) THEN    ! --   Continuum element, 2D, plane strain,
        NOD = 6                !      quadratic, 6 noded triangle
      ELSEIF(JJJ.EQ.4) THEN    ! --   Continuum element, 2D, plane strain
        NOD = 4                !      , linear,4 noded quadrilateral
      ELSEIF(JJJ.EQ.5) THEN    ! --   Continuum element, 2D, plane strain,
        NOD = 8                !      quadratic, 8 noded quadrilateral
      ELSEIF(JJJ.EQ.6) THEN    ! --   Contact joint element, 2D, linear
        NOD = 4                !      , 4 nodes
      ELSEIF(JJJ.EQ.7) THEN    ! --   Contact joint element, 2D, quadratic
        NOD = 6                !      , 6 nodes
      ELSEIF(JJJ.EQ.8) THEN    ! --   Boundary element, 2D, plane strain
        NOD = 2                !      , linear, 2 nodes                     
        NDIME = 1
        IMAT = 99                 !- hack so we can delete them
      ELSEIF(JJJ.EQ.9) THEN    ! --   Boundary element, 2D, plane strain
        NOD = 3                !      , quadratic,3 noded straight segment
        NDIME = 1
      ELSE
        NOD = 0
        PRINT*,'Unknown element type (R_PHASES)'
      ENDIF
c------------------
    6   READ (IO,*,IOSTAT=IOS) (NUM(J),J=1,NOD)
        IF (IOS.NE.0) CALL IN_TEST(IO,IOS,*6,*999)

c--------- fudge the node #s up ------
C  (becuase I believe PHASES starts counting at zero?)
        DO I=1,NOD
          NUM(I) = NUM(I) + 1
        ENDDO

        CALL PUT_ELEMENT (NUMS,INUMS,NEL_OLD+IEL, NUM, 
     &                     NOD,NDIME,ITYPE, IMAT,1,1,1)
      ENDDO
      WRITE(*,'(A,I8,A)')  '>>',NEL,' nodes read'
C      WRITE(*,'(A,I5,A/ a,i8,a,i8,a)') 
C     &       '>> ',NEL,' elements read'
C     &    ,  ' (',  n_8nq,' 8-node quads +', n_2nl,' 2-node rods )'

c---------------- Now read in the nodal coordinates --------------------
    7 READ (IO,*,IOSTAT=IOS) NN
      IF (IOS.NE.0) CALL IN_TEST(IO,IOS,*7,*999)
      DO I=1,NN
    8   READ (IO,*,IOSTAT=IOS)  (GC(j,NN_OLD+I), j=1,2)
        IF (IOS.NE.0) CALL IN_TEST(IO,IOS,*8,*999)
      ENDDO
      WRITE(*,'(A,I6,A)')  '>> ',NN,' nodes read'

c-------------------- other data ---------------------
c.. b=next : element frontal order..
c.. mat types as IEL, IEMAT,IPMAT = elastic mat type, plastic mat type.

C-----------------------------------------------------------------------
      NN  = NN  + NN_OLD
      NEL = NEL + NEL_OLD 
      RETURN

C---------------------------- read error ----------------------------
  999 CALL MYERROR (2,'Error in reading PHASES data file')
      RETURN
      END

C-----------------------------------------------------------------------
c-----------------------------------------------------------------------
c-----------------------------------------------------------------------
c     These Routines are for handling AutoCad's DXF files:
c        (lower-case are not usu. user-callable)
c
c   1. R_DXF             :Reads in a DXF mesh
c  1a. R_DXF_STATS       :Simply parses a DXF for statistical purposes
c   2. WR_DXF            :Saves the mesh in DXF format
c   3. Read_Dxf_Line     :  reads LINE entities
c   4. Read_Dxf_Polyline :  reads POLYLINE entities
c   5. Read_Dxf_3dface   :  reads 3DFACE entities
c   6. Read_Dxf_Solid    :  reads SOLID entities
c   7. Decode_Dxf        :    abstract real/ integer/ character data
c   8. Read_Dxf_Pair     :    as above but does read too.
c   9. 
c-----------------------------------------------------------------------
      SUBROUTINE R_DXF (IO,GC,IGC,NDIM,NN, NUMS,INUMS,NEL)
c
c     This will parse an AutoCad DXF file for valid 'Finite Elements'
c     storing them in GC and NUMS. Most will be 2d line elements
c     SOLID, 3DFACE and POLLYLINE can yield 3nts/4nqs.
c
c  .. maybe print summaries eg. #lines, #solids too ?

      INTEGER    NUMS(INUMS,*)      !- node #s
      REAL       GC(IGC,*)          !- nodal coords
      CHARACTER  C_VAL*20

c     NEL=0   !--- do we do this here or in the main program ?
c     NN=0
c     if (ndim.eq.0) ndim=3       !- eg if using DANMUNG ndim is undefined
      if (nn.eq.0) ndim=3         !- if no nodes yet .. go into 3d

    1 CONTINUE     !-- loop back point  (cf F90 DO..ENDDO)
      CALL READ_DXF_PAIR (IO, IT, I_VAL, R_VAL, C_VAL)
      IF (IT.EQ.0) THEN
        IF (C_VAL.EQ.'EOF') THEN      !- all done
          RETURN            !- close file ? / print summary
        ELSEIF (C_VAL.EQ.'LINE') THEN
          CALL READ_DXF_LINE (IO,GC,IGC,NDIM,NN, NUMS,INUMS,NEL)

        ELSEIF (C_VAL.EQ.'POLYLINE') THEN   !- we will fill these in !
          CALL READ_DXF_POLYLINE (IO,GC,IGC,NDIM,NN, NUMS,INUMS,NEL)

        ELSEIF (C_VAL.EQ.'SOLID') THEN      !- dont know this one :-(
          CALL READ_DXF_SOLID (IO,GC,IGC,NDIM,NN, NUMS,INUMS,NEL)

        ELSEIF (C_VAL.EQ.'3DFACE') THEN
          CALL READ_DXF_3DFACE (IO,GC,IGC,NDIM,NN, NUMS,INUMS,NEL)

        ELSEIF (C_VAL.EQ.'DIMENSION') THEN   !- no-op

        ENDIF
      ENDIF     !- only handle type '0' opcodes
      GOTO 1
      END

c-----------------------------------------------------------------------
      SUBROUTINE R_DXF_STATS (IO)
c
c     This simply parses a DXF files to find the following statistics:
c       1.  2D/3D drawing
c       2.  The coords of the bounding box
c       3.  The number of each sort of primitive
c       4.  Pretty print (unit 89) the DXF file with indentation

      INTEGER IT
      CHARACTER C_VAL*20,TAB*1
      REAL XYZBOX(6)                  !- bounding box
      CHARACTER PRIMITIVES(10)*20     !- primitive names
     &          ,SECTIONS(4)*20       !- section names
      INTEGER N_PRIMS(10)             !- # of each primitive
     &          ,N_SECS(4)            !- # of each section type (0 or 1)
     &         ,N_OCCUR(4)            !- # of lines in each section


      DATA XYZBOX/3*1.E10,3*-1.E10/, 
     &    N_PRIMS/10*0/,N_SECS/4*0/,N_OCCUR/4*0/
      TAB = CHAR(9)

      WRITE (SECTIONS,'(A)') 'HEADER', 'TABLES', 'ENTITIES', 'BLOCKS'
      WRITE (PRIMITIVES,'(A)') 'LINE', 'POLYLINE', '3DFACE', 'SOLID',
     &    'TEXT', 'ARC', 'TRACE', 'DIMENSION', ' dummy1', 'dummy2'
      ISEC_N=0    !- initially section type is unknown

c---------------------- open output logfile (unit #?) ------------------
C... maybe use INQUIRE to get filename on UNIT 88
c... hmm this file is not closed after use here either

      OPEN (89,FILE='decoded.dx2')    

    1 CONTINUE                    !--- loop back point
      CALL READ_DXF_PAIR (IO, IT,  I_VAL, R_VAL, C_VAL)

c----------------- accumulate the bouinding box ------------------------
      IF (IT.LE.9) THEN        
      ELSEIF (IT.LE.19) THEN                  ! X
        XYZBOX(1) = MIN (XYZBOX(1),R_VAL)
        XYZBOX(4) = MAX (XYZBOX(4),R_VAL)
      ELSEIF (IT.LE.29) THEN                  ! Y
        XYZBOX(2) = MIN (XYZBOX(2),R_VAL)
        XYZBOX(5) = MAX (XYZBOX(5),R_VAL)
      ELSEIF (IT.LE.39) THEN                  ! Z
        XYZBOX(3) = MIN (XYZBOX(3),R_VAL)
        XYZBOX(6) = MAX (XYZBOX(6),R_VAL)
      ENDIF

c-------------------- check which SECTION we are in --------------------
      IF (IT.EQ.2) THEN
        DO J=1, 4
          IF(C_VAL.EQ.SECTIONS(J))  THEN
            ISEC_N = J
            N_SECS(J) = N_SECS(J) + 1     !- # of instances
          ENDIF
        ENDDO
      ENDIF
      IF (ISEC_N.NE.0) N_OCCUR(ISEC_N) = N_OCCUR(ISEC_N) + 1

c-------------------- count the # of each primitive --------------------
      IF (IT.EQ.0) THEN
        DO J=1,10
          IF(C_VAL.EQ.PRIMITIVES(J))  N_PRIMS(J)=N_PRIMS(J)+1
        ENDDO
      ENDIF

c---------------------- pretty-print the file --------------------------
c   if (1.eq.0) then
      IF (IT.EQ.0) THEN
        WRITE (89,'(I3,6X,A)') IT, C_VAL
      ELSEIF (IT.EQ.2) THEN
        WRITE (89,'(A,I3,6X,A)') TAB,IT, C_VAL
      ELSEIF (IT.le.9) THEN
        WRITE (89,'(A,I3,6X,A)') TAB//TAB,IT, C_VAL
      ELSEIF (IT.le.59) THEN
        WRITE (89,'(A,I3,G15.6)') TAB//TAB,IT, R_VAL
      ELSEIF (IT.le.79) THEN
        WRITE (89,'(A,I3,6X,I5)') TAB//TAB,IT, I_VAL
      ELSEIF (IT.LE.209) THEN     !-- 'gap'
        
      ELSEIF (IT.LE.239) THEN     !-- more reals
        WRITE (89,'(A,I3,G12.6)') TAB//TAB,IT, R_VAL
      ELSEIF (it.eq.999) THEN     !-- comment-lines
        WRITE (89,'(I3,6X,A)') IT, C_VAL
      ELSE                             !-- 'gap'

      ENDIF

      IF (IT.NE.0.OR.C_VAL.NE.'EOF') GOTO 1

c-------------------- output summary tables ----------------------------
c... also SECTIONS: TABLES/ HEADER/ ENTITIES/ BLOCKS
c... ie. the # lines in each section.
c... and even whether each section appears at all

      WRITE(*,'(A)') '--> Sections:'
      DO J=1,4
        WRITE(*,'(T20,I5, 3A,2X,I5,A)')
     &   N_SECS(J),' ',SECTIONS(J), ' of', N_OCCUR(J),' items in total'
      enddo

      WRITE(*,'(A)') '--> Primitives:'
      DO J=1,10
        IF (N_PRIMS(J).NE.0) WRITE(*,'(T30,A20,A2,I5)')
     &   PRIMITIVES(J), '=', N_PRIMS(J)
      enddo

      WRITE(*,'(A,(T20,3(2X,F15.5)))' ) 
     &      '--> Bounding box (x,y,z) =', (xyzbox(i),i=1,6)

      END

c-----------------------------------------------------------------------
      SUBROUTINE WR_DXF (IO,GC,IGC,NDIM,NN, NUMS,INUMS,NEL)
c
c     This will export the mesh in AutoCad's DXF format
c     3nt/4nqs will be saved as SOLID
c     other 2Ds as POLYLINE
c     3D elements will be split into their FACETS :
C     ?save 1d elements as line/polyline?
c

C.... note that even 2D elements need reducing to their bounding polygon

c      7-2-95 : Note that I also need a a DRAW_DXF driver for DANPLOT
c               so that I can save DANPLOT picture in AUTOCAD format
c               ie. a set of polygons/polyline in 2d (3d?) + some text
c.. note preamble to '  0 ENTITIES', and postfix of '  0 ENDSEC'

      INTEGER    NUMS(INUMS,*)      !- node #s
      REAL       GC(IGC,*)          !- nodal coords
      INTEGER NUM(32), FS(6)
      PARAMETER (ICOORD=27)
      REAL COORD(ICOORD,3)

      WRITE(IO,'(I3/,A)') 0,'ENTITIES'

      DO IEL=1,NEL
        CALL GET_ELEMENT 
     &    (NUMS,INUMS,IEL, NUM, NOD,NDIME,ITYPE, IMAT,IU1,IU2,IU3)

c..... need to get the #facets in 3D
c..... then loop the facets and handle
c (how do we handle 1D elements ?)

        CALL GET_FACE (NUM,NOD,NDIME,ITYPE,IFACE,FS,NN_F,NN_FT, 1 )
        NFACE=IFACE

c------------------ loop the facets and 'draw' them --------------------
        DO IFACE=1,NFACE
c.. next line moved inside the loop 25-8-98 !
          CALL GET_FACE (NUM,NOD,NDIME,ITYPE,IFACE,FS,NN_F,NN_FT, 2 )

          DO I=1,NN_FT     !----- get coord and disps of this facet -----
            DO J=1,NDIM
              COORD (I,J) = GC (J,FS(I))
            ENDDO
          ENDDO

c... if NN_F=3 or 4 then can save as SOLID or 3DFACE ? (more compact)
c.. also maybe AutoShade(tm) only handles these ?
c.. so MAYBE for ray-tracing reduce 20nbs to 4nq sub-facets ?

          IF (NDIM.eq.3.and.(NN_F.eq.3.or.NN_F.eq.4)) THEN
            WRITE(IO,'(I3/A, 4(/I3/I5) )')
     &      0,'3DFACE', 66,1, 70,1, 8,1, 62,IMAT
            DO I=1,NN_F
              WRITE(IO,'(I3/G14.5)') (10*J+ I-1,COORD(I,J),J=1,NDIM)
            ENDDO
          ELSE
            WRITE(IO,'(I3/A, 4(/I3/I5) )')
     &      0,'POLYLINE', 62,IMAT, 66,1, 70,1, 8,1
            DO I=1,NN_F
              WRITE(IO,'(I3/A, /I3/I5/, (I3/G14.5))')
     &        0,'VERTEX',  8,1, (10*J,COORD(I,J),J=1,NDIM)
            ENDDO
            WRITE(IO,'(I3/,A)')  0,'SEQEND'   !- after all verteces?
          ENDIF
        ENDDO    !-- loop facets

      ENDDO    !-- loop elements

      WRITE(IO,'(I3/,A)') 0,'ENDSEC'
      WRITE(IO,'(I3/,A)') 0,'EOF'

      RETURN
      END

c-----------------------------------------------------------------------
      SUBROUTINE READ_DXF_POLYLINE (IO,GC,IGC,NDIM,NN, NUMS,INUMS,NEL)
c
c     This reads in a polygon element as stored in a DXF file
c     .. should I use FIND_OR_ADD_NODE ?
 
c      if flag 70 bit1 /=1 then NOT closed so save as 1d elems only!

c..  hmm if 2d and NOD =4,8,12, 3 or 6 type=1, else type=9
c       ?type=8 is 'double-sided polygon .eg. NFF ?
c..  if NOD>NODMAX( (=27?) then 'cut-short' or ignore completely ?
c.. F90 linked lists would be much better (cf OFF 'gear-wheels' in DANPLOT
 
c.. For DXF plotting (eg.PS) I may wish to handle the line widths too
c.. Maybe store layer # as IUSER1 ? (hence 'filter' some out ?)
c.. (cf. DR_DXF where differnt drawing parts can go in different layers)

c-- 27-8-96 'ARENA' writes a DXF polyline as a set of coords followed by 
c--         a set of 'steering'  to mark some 3nt's/ 4nq's


      INTEGER    NUMS(INUMS,*)      !- node #s
      REAL       GC(IGC,*)          !- nodal coords
      PARAMETER (ICOORD=27)
      REAL COORD(ICOORD,3)
      CHARACTER C_VAL*20

      IMAT = 1    !- default material type
      NOD = 0     !- no nodes yet

    1 CONTINUE    !-  loop back point
      CALL READ_DXF_PAIR (IO, IT, I_VAL, R_VAL, C_VAL)

      IF (IT.EQ.0) THEN
        IF (C_VAL.EQ.'SEQEND') THEN
c          NOD = 2         !- element type= 2nl      <- why?
c          NDIME = 1
c          ITYPE = 9     

          NDIME = 2
          ITYPE = 1       !- default = 'polygon'

          IU1 =0
          IU2 =0          !- user-defined all =0
          IU3 =0
          IF (NOD.EQ.3.OR.NOD.EQ.6.OR.NOD.EQ.4.OR.NOD.EQ.8.OR.NOD.EQ.12)
     &      ITYPE = 1       !- valid as an 'ordinary' 2D element
          CALL PUT_ELEMENT_ALL (NUMS,INUMS,NEL,GC,IGC,NDIM,NN 
     &      , COORD,ICOORD ,NOD,NDIME,ITYPE, IMAT,IU1,IU2,IU3)
          RETURN

        ELSEIF (C_VAL.EQ.'VERTEX') THEN
c        .. fudge as a no-op .. just let X incremnt NOD :-)
c.. NO surely each vertex will add another node.
          NOD = NOD+1
        ENDIF

      ELSEIF (IT.EQ.10) THEN
c       NOD = NOD+1
        IF (NOD.GT.ICOORD) THEN  !-- abort 'cos element is too big'
          PRINT*,'** Too many nodes : this element ignored' 
c         ... maybe we need to parse to the 0 SEQEND anyway?
c         ... if so still count the # nodes? (hence 'adjust'?)
          RETURN   
c       ELSEIF (NOD.eq.0) GOTO 1
        ENDIF
        IF (NOD.ge.1) COORD(NOD,1) = R_VAL
      ELSEIF (IT.EQ.20) THEN
        IF (NOD.ge.1) COORD(NOD,2) = R_VAL
      ELSEIF (IT.EQ.30) THEN
        IF (NOD.ge.1) COORD(NOD,3) = R_VAL
      ELSEIF (IT.EQ.62) THEN
        IMAT = I_VAL
      ENDIF
      GOTO 1     !- loopback for more data
      END

c-----------------------------------------------------------------------
      SUBROUTINE READ_DXF_LINE (IO,GC,IGC,NDIM,NN, NUMS,INUMS,NEL)
c
c     This reads in a rod element as stored in a DXF file
c
      INTEGER    NUMS(INUMS,*)      !- node #s
      REAL       GC(IGC,*)          !- nodal coords
      PARAMETER (ICOORD=27)
      REAL COORD(ICOORD,3)
      CHARACTER C_VAL*20

      IMAT = 1    !- default material type

    1 CONTINUE    !-  loop back point
      CALL READ_DXF_PAIR (IO, IT, I_VAL, R_VAL, C_VAL)

      IF (IT.EQ.10) THEN
        COORD(1,1) = R_VAL
      ELSEIF (IT.EQ.20) THEN
        COORD(1,2) = R_VAL
      ELSEIF (IT.EQ.30) THEN
        COORD(1,3) = R_VAL
      ELSEIF (IT.EQ.11) THEN
        COORD(2,1) = R_VAL
      ELSEIF (IT.EQ.21) THEN
        COORD(2,2) = R_VAL
      ELSEIF (IT.EQ.31) THEN
        COORD(2,3) = R_VAL
      ELSEIF (IT.EQ.62) THEN       !- colour as IMAT ?
        IMAT = I_VAL
      ELSEIF (IT.EQ.0) THEN        !- here at end_of_sequence 
        NOD = 2         !- element type= 2nl
        NDIME = 1
        ITYPE = 1 
        IU1 =0
        IU2 =0          !- user-defined all =0
        IU3 =0
        CALL PUT_ELEMENT_ALL (NUMS,INUMS,NEL,GC,IGC,NDIM,NN 
     +      , COORD,ICOORD ,NOD,NDIME,ITYPE, IMAT,IU1,IU2,IU3)
c... also need to mark this OPCODE_pair to re-read (cf BACKSPACE*2)
         BACKSPACE (IO)
         BACKSPACE (IO)
         RETURN
      ENDIF
      GOTO 1      !-- cf F90's endless DO..ENDDO
      END

c-----------------------------------------------------------------------
      SUBROUTINE READ_DXF_3DFACE (IO,GC,IGC,NDIM,NN, NUMS,INUMS,NEL)
c
c     This reads in a 3nt/4nq from the DXF file (cf polyline)
c       Dan Kidger 9-2-96
c
      INTEGER    NUMS(INUMS,*)      !- node #s
      REAL       GC(IGC,*)          !- nodal coords
      PARAMETER (ICOORD=27)
      REAL COORD(ICOORD,3)
      CHARACTER C_VAL*20

      IMAT = 1    !- default material type
      NOD = 0     !- no nodes yet

    1 CONTINUE    !-  loop back point
      CALL READ_DXF_PAIR (IO, IT, I_VAL, R_VAL, C_VAL)

c.. hmm the next is no good 3DFACEs have no SEQEND marker
      IF (IT.EQ.0) THEN
        BACKSPACE (IO)    !- step back again ?
        BACKSPACE (IO)
c       IF (C_VAL.EQ.'SEQEND') THEN
          NDIME = 2
          ITYPE = 1       !- default = 'polygon'

          IU1 =0
          IU2 =0          !- user-defined all =0
          IU3 =0
          IF (NOD.EQ.3.OR.NOD.EQ.6.OR.NOD.EQ.4.OR.NOD.EQ.8.OR.NOD.EQ.12)
     &      ITYPE = 1       !- valid as an 'ordinary' 2D element
          CALL PUT_ELEMENT_ALL (NUMS,INUMS,NEL,GC,IGC,NDIM,NN 
     &      , COORD,ICOORD ,NOD,NDIME,ITYPE, IMAT,IU1,IU2,IU3)
          RETURN


      ELSEIF (IT.GE.10.and.IT.LE.19) THEN
        NOD = NOD+1
        IF (NOD.GT.ICOORD) THEN  !-- abort 'cos element is too big'
          PRINT*,'** Too many nodes : this element ignored' 
          RETURN   
        ENDIF
        IF (NOD.ge.1) COORD(NOD,1) = R_VAL
      ELSEIF (IT.GE.20.and.IT.LE.29) THEN
        IF (NOD.ge.1) COORD(NOD,2) = R_VAL
      ELSEIF (IT.GE.30.and.IT.LE.39) THEN
        IF (NOD.ge.1) COORD(NOD,3) = R_VAL
      ELSEIF (IT.EQ.62) THEN
        IMAT = I_VAL                ! 'cos 62 = pen-colour
      ENDIF
      GOTO 1     !- loopback for more data
      END

c-----------------------------------------------------------------------
      SUBROUTINE READ_DXF_SOLID (IO,GC,IGC,NDIM,NN, NUMS,INUMS,NEL)
c
c     Called from R_DXF, but not yet implimented
c
      INTEGER    NUMS(INUMS,*)      !- node #s
      REAL       GC(IGC,*)          !- nodal coords
      END

c-----------------------------------------------------------------------
      SUBROUTINE READ_DXF_PAIR (IO, IT, I_VAL, R_VAL, C_VAL)
c
c     This reads a DXF opcode IT from unit IO 
C     followed by its data value so returns *one* of:
C          I_VAL : integer value
C          R_VAL : real value
C          C_VAL : character string
C     the other 2 are set to zero (' ')
c
c         DJK 17-8-94
c
      CHARACTER C_VAL*20

      C_VAL =' '
      I_VAL=0
      R_VAL=0.0
      READ(IO,*,IOSTAT=IOSTAT) IT
      IF (IOSTAT.NE.0) THEN
        PRINT*,  'unexpected end of DXF file'
        IT = 0
        C_VAL='EOF'      !- hack so we exit gracefully
        RETURN
      ENDIF

      IF (IT.LE.9) THEN          !-- character strings  
        READ(IO,'(A)') C_VAL
      ELSEIF (IT.LE.59) THEN      !-- reals
        READ(IO,*) R_VAL
      ELSEIF (IT.LE.79) THEN      !--integers
        READ(IO,*) I_VAL
      ELSEIF (IT.LE.209) THEN     !-- 'gap'
        
      ELSEIF (IT.LE.239) THEN     !-- more reals
        READ(IO,*) R_VAL
      ELSEIF (it.eq.999) THEN     !-- comment-lines
        READ(IO,'(A)') C_VAL
      ELSE                             !-- 'gap'

      ENDIF
      RETURN
      END

c----------------------------------------------------------------------
      SUBROUTINE R_NFF (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL) !, PAL)
c
c     this reads in data from the NFF= 'Neutral FILE Format'
c     currently only polygons are read
c     -- however NFF also contains the viewing params, colours, etc.
c     -- hence just do the mesh here .. rescan the file for colours
c         & rescan the file for camera position too.
c
c   .. maybe I should call up routines to store the colours etc.
c   externally rather than try and store with the object.
c     the NFF file format explained:
c       1/ any number of initial #comment lines
c       2/ data as keyword followed by data .. seems to be all integer.
c       3/ 'v' = a camera data structure
c          with: from x,y,z , at x,y,z  , up 0 0 1, angle 40, hither=1.0
c          and also: resolution 512 512
c       4/ 'l x,y,z' is probally the x,y,z positions of a light(s)
c       5/ 'x r,g,b is the image background colour'
c       6/ 'f r,g,b,spec,a,b,c,d sets the colour of the following polygons
c       7/ 'pp 3' leads a 3nt with x,y,z,nx,ny,nz data.
c       8/ 'p 4'  leads a 4nq with just x,y,z data
c
      parameter (icoord=50)   !- max # of nodes per polygon
      REAL    GC (IGC,*)
      INTEGER NUMS (INUMS,*), IO
c     INTEGER PAL (3,0:255)
      REAL COORD(ICOORD,3)
      CHARACTER code*2,t*1, line*80

      NDIM = 3    !- NFF is always 3D

c      NN   = 0    !- reset the databse ??
c      NEL  = 0

      IOFF = 0 !2    !- offset into the material-colour table (yuk!)
      IMAT = 0    !- no known materials yet

 1111 CONTINUE
      READ (IO,'(a)',ERR=999) CODE
      IF (CODE.eq.'f ') THEN   ! a material colour
        BACKSPACE (IO)
        IMAT = IMAT + 1
       READ (IO,'(A)') LINE
c.. now I ought to use a token looper, then use atoi or atof to get values.
c       LINE(1:2) ='  '                    !-- zap the 'key-letter'
c       READ (LINE,'(3f8.3)') R,G,B        !(note use of a 'format')
c       PAL(1,IOFF+IMAT) = min(R *256,255) 
c       PAL(2,IOFF+IMAT) = min(G *256,255)  ! 12-9-96 PAL removed for now.
c       PAL(3,IOFF+IMAT) = min(B *256,255) 

C-----------  a Polygon / Polygon-with-normals ('p','pp') --------------
      ELSEIF (CODE.eq.'p '.or.CODE.eq.'pp') THEN 
        BACKSPACE (IO)
        IF (CODE.eq.'p ') READ (IO,'( a,i2)')   t,NOD
        IF (CODE.eq.'pp') READ (IO,'(2a,i2)') t,t,NOD
        DO I=1,NOD
          READ (IO,*) X,Y,Z
          COORD (I,1) = X
          COORD (I,2) = Z
          COORD (I,3) = -Y
        ENDDO
        IU1 = 1
        IU2 = 1
        IU3 = 1
        NDIME= 2
        ITYPE = 9
        IF (NOD.EQ.3.OR.NOD.EQ.6.OR.NOD.EQ.4.OR.NOD.EQ.8.OR.NOD.EQ.12)
     &    ITYPE = 1       !- valid as an 'ordinary' 2D element
        CALL PUT_ELEMENT_ALL (NUMS,INUMS,NEL,GC,IGC,NDIM,NN 
     &      ,COORD,ICOORD ,NOD,NDIME,ITYPE, IMAT,IU1,IU2,IU3)

      ENDIF       !- possible opcodes
      GOTO 1111
  999 CONTINUE
c     CALL SET_ALL_DACS ()      !- uncomment this ??

      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE WR_RAYSHADE (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL)
C  
C     This writes out the mesh in the RAYSHADE raytracing package format
C
C    note: this format is essiantialy just a set of polygon descriptions
C    ... only really usefuly for 'curved' objects.. eg OFF.
C    ... therefore need a converter to write out a DANPLOT 3D mesh
C    ... as the OFF file of its Facets
C
C      Dan Kidger   21 July 1993
C
      REAL GC(IGC,*)
      INTEGER NUMS(INUMS,*), IO,  NUM (32)

      DO IEL=1,NEL
        CALL GET_ELEMENT
     &  (NUMS,INUMS,iEL, NUM, NOD,NDIME,ITYPE, IMAT,I1,i2,i3)

        WRITE(IO,'(A, 99f11.3)')   'poly', 
     +  ((GC(jj,NUM(J)),jj=1,ndim),J=NOD,1,-1)  !- reverse order

      ENDDO

      WRITE(*,'(A,I8,A)')  '>>',NEL,' elements written'
      RETURN
      END  

C-----------------------------------------------------------------------
      SUBROUTINE WR_ARENA (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL,P)
C  
C     This writes out the mesh in the ARENA 3d-modeller /ray-tracer format
C        DJK  24-8-96
C     notes on ARENA format:
c      - Databse also includes lights, cameras, material props, etc.
c      - All primitives are triangles (even spheres and cylinders)
c         CSG operations may be performed tho'
c      - Primitives may optionally have normal vectors.
c      - All coords seem to have a 'D' field of 2 varibles - ignore?
c      - quads are formed as a 'bound' pair of triangles.
c
c     This is only a limited implimentation: No normal vectors, no 
c     'binding' of multiple triangles?, no preamble or materials.
c
c     29-8-96 writes consectutive elems with the same IMAT as the same block.
c
      REAL GC(IGC,*)
      INTEGER NUMS(INUMS,*), IO,  NUM (32), P(*)

c-------- write headers ----------
c Algorithm:
c    write file headers
c      set mode = 'start a new block'
c      loop elems:
c         if 'new_block' :
c           loop forwards & count #elems of the same IMAT (IGRP?)
c           flag new_block (ie. store block count and init counter=1
c         else
c           if first_of_block : write headers
c           strip this elem and write
c           if last_of_block : write trailers, set counter=0 to flag a 
c                'look' for a new block
c         endif
c
c.. This assumes that all are 2D primitives, so nust *STRIP_FACETS, and 
c   do *FACETS_TO_ELEMENTS, first. (cf Facets directly from DANPLOT)
c
c    problem: we need to count the nodes that each 'block' uses:
C      then reseq these;   loop & write just these nodes
c         when writing a facet morph 'real' NUM by 'looking up' in P()

c----------- write the field headers -------------
      WRITE (IO,'(A)')
     &   'TAG= "ARENA SCENE"'
     &  ,'VERSION= 1.33'
     &  ,'TB_SELECT= 1'     
c     ... now we skip 'SCENE', 'KeyFramer, 'VIEW' and 'MATERIAL'

c---- ojbects ---
      IOBJECT = 0         !- object count
      IEL2 = 0            !- = 0. flags a new object.
      NEL2 = 0            !- # of elements in this object
      
c-------------------------------------------------------------------
      DO IEL=1,NEL
        CALL GET_ELEMENT
     &  (NUMS,INUMS,iEL, NUM, NOD,NDIME,ITYPE, IMAT,I1,i2,i3)

      IF (NEL2.eq.0) THEN   !----- dont know the list yet ---
c....................................................
c.. all we are after is the count 'NEL2' and the set of labels in P()

        DO I=1,NN
          P(I) = 0         !- assume no nodes are in this block
        ENDDO

        NEL2 = 0
        NFACES = 0
        DO IEL3=IEL, NEL   !-- search and count (note do orig el too)
          CALL GET_ELEMENT
     &    (NUMS,INUMS,iEL3, NUM, NOD,NDIME,ITYPE, IMAT3,I1,i2,i3)
          IF (IMAT3.NE.IMAT) GOTO 22   !-- no more matches
          NEL2 = NEL2 + 1
          IF (NOD.EQ.3) NFACES = NFACES+1    !- tri's
          IF (NOD.EQ.4) NFACES = NFACES+2    !- quad's
          DO J=1,NOD
            P(NUM(J)) = 1      !- flag those nodes present in the block
          ENDDO
        ENDDO      !- loop search
c        .. tidy up ..
   22   CONTINUE
        CALL RESEQ_P (P,NN,NN_OBJECT)  !- hence count them    
        IEL2 = 0     !- flags that on the next pass we write elements.
      ENDIF     !- 'find the size of the next group'
c....................................................

        CALL GET_ELEMENT
     &  (NUMS,INUMS,iEL, NUM, NOD,NDIME,ITYPE, IMAT,I1,i2,i3)
c.. <where do we loop the 6 facets?>

        IEL2 = IEL2 + 1       !- count the elements in this block (to NEL2)
        IF (IEL2.EQ.1) THEN          !- start of a block

          IOBJECT = IOBJECT + 1

          WRITE (IO,'(A,I4.4,A)')
     &     'OBJECT "DP',IOBJECT,'"'
     &    ,'('
c      ...now we skip: 'MATREF=','Culling', 'LFRAME', 'PIVOT', 'KeyList', 
          WRITE (IO,'(A)')
     &     ' ELEMENTLIST'
     &    ,' ('

c--------- write the nodes ---------
c.. if we have a set of NORMAL vectors (an F90 optional argument)
c    then we should write all 6 columns, and FORMAT='PND'

          WRITE (IO,'(A/A/A/ A,i6/ A,i6/ A/A)') 
     &     '  TRISET'     
     &    ,'  ('
     &    ,'   FORMAT= PD'              !- or 'PND' if normals
     &    ,'   VERTICES=' ,NN_OBJECT
     &    ,'   FACES='    ,NFACES
     &    ,'   VERTEXLIST'
     &    ,'   ('
          DO I=1,NN
            IF (P(I).NE.0)THEN 
            IF (NDIM.eq.2)
     &      WRITE(IO,'(t5,3g13.4, A)') (GC(jj,i),jj=1,2),0., ' 0. 0. n'
            IF (NDIM.eq.3)
     &      WRITE(IO,'(t5,3g13.4, A)') (GC(jj,i),jj=1,ndim),  ' 0. 0. n'
            ENDIF
          ENDDO
          WRITE (IO,'(A)')
     &       '  )'             !- end of 'VERTEXLIST' nodal coords section.
          WRITE(*,'(A,I4,A,I5,A)')  '>> Object #', IOBJECT
     &   ,' :',NN_OBJECT,' nodes written' 

          WRITE (IO,'(A)')     !- for the set of elements
     &    '   FACELIST'     
     &   ,'   ('

       ENDIF

c------------------------------------------------
        IF (NOD.EQ.3) THEN     !- only triangles ?
          WRITE (IO,'(A)')  '    FACE'  ,'    (','     1 n' 
c         ... 1 implies that each triangle is independant. (?)
          WRITE(IO,'(T6,A, 6i7,A)') 
     &     'TRI (',P(NUM(1)),1,P(NUM(2)),1,P(NUM(3)),1, ')'  
          WRITE (IO,'(A)')  '    )'            !- end of a 'FACE'

        ELSEIF (NOD.EQ.4) THEN     !- 4nq's as a pair of 3nt's
          WRITE (IO,'(A)')  '    FACE', '    (', '     2 n'   

          WRITE(IO,'(T6,A, 6i7,A)') 
     &       'TRI (',P(NUM(1)),1,P(NUM(2)),1,P(NUM(3)),1, ')'  
     &      ,'TRI (',P(NUM(3)),0,P(NUM(4)),1,P(NUM(1)),0, ')'  
          WRITE (IO,'(A)')   '    )'            !- end of a 'FACE'
        ENDIF  !- only 3nt/4nq's elements

        IF (IEL2.EQ.NEL2) THEN          !**** end of a block ****
          WRITE (IO,'(A)')
     &        '   )'             !- end of 'FACELIST' element steering section
     &      , '  )'             !- end of 'TRISET' - like ELEMENTLIST realy
     &      , ' )'             !- end of 'ELEMENTLIST'  section
     &      , ')'             !- end of 'OBJECT' section
          WRITE(*,'(A,I5,A,A,i5,A,i3)')
     &       '>>            ',NEL2,' elements written'
     &      ,'(=',nfaces,' triangles ), Material=', imat
          NEL2 = 0            !-- so forces then scan for the next block
        ENDIF
C=======================================================================

      ENDDO    !- loop elements and write

      WRITE(*,'(A,I4,A)')  '>> ',IOBJECT,' objects written'
      RETURN             
      END  

C----------------------------------------------------------------------
      SUBROUTINE R_MESH_UCD (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL)
C
C     This reads in a mesh in AVS's 'Unstructure Cell Data' format
c        DJK 10-9-96
c
C     DATA FILE FORMAT DESCRIPTION:
C      line 1: NN, NEL, , n_node_disps, n_cel_disps, n_mats
c      //  inode, x, y, z               //(for inode=1=,nn), always 3D
c      //  iel, imat, 'hex', NUM(1:NOD) //(for iel=1,nel)
c
c     notes:
c       1: the file may begin with any number of #comments lines, but
c           no comments are allowed afterwards
c       2: column 1 must never be left blank, so we must left_shift data
c       3: blank line are not permitted
c       4: valid element types are 'pt, line, tri, quad, tet, pyr, prism, hex'
c       5: all types may optionaly have *one* mid-side node, if a 
c           'mid_edge_flags' in the data structure is set with a bit pattern
c          caveat: only some modules can use the mid-edge data values.

      REAL GC(IGC,*)
      INTEGER NUMS(INUMS,*), IO , NUM(32)
      CHARACTER LINE*100, ELCODE*6
      INTEGER str_to_i, ifail
      EXTERNAL str_to_i            ! does char->int (cf C funtion 'atoi')

      PARAMETER (MAXTOKENS = 50)             ! (I only expect about 10)
      INTEGER  LIST(2,MAXTOKENS)             !- store of STRING pointers

      nn_old = nn
      nel_old = nel
c............ pre-scan to find the start of the data ................
c.. the file may contain any number of #comment lines
c  so echo to stdout?
    1   READ(IO,*,IOSTAT=IOS) NN,NEL, n_node_disps, n_cel_disps, n_mats
        if (ios.ne.0) goto 1
      
c------ read the nodal coords ----------
      IF (NN_OLD.EQ.0) NDIM = 3                !only if our mesh is empty ?
c     .. I would rather have a call to INTO_3D() so adjust all GC

      DO I=1,NN
        READ(IO,*) idummy, (GC(J,I+NN_OLD),J=1,NDIM)
      ENDDO
      WRITE(*,'(A,I4,A)')  '>> ',NN,' nodes read'

      DO IEL=1,NEL
c
c    read a line, mung 'hex' into '8nb' = 3 8 1
c    so call tokenise ?, the count ?
        read (io,'(a)') line     !- the line of data
        CALL COUNT_TOKENS (LINE, NTOKS,', =')                   !- better - 
        CALL GET_TOKENS (LINE,  LIST,MAXTOKENS,NTOKS)     !- old -
c       idummy = 
        imat   = str_to_i ( line(list(1,2):list(2,2)), ifail)
c       if (ifail.ne.0) then .. wasn't an integer here.
        elcode  = line(list(1,3):list(2,3) )
        ndime = -1
        if (elcode.eq.'pt'  )  ndime = 0
        if (elcode.eq.'line')  ndime = 1
        if (elcode.eq.'tri' )  ndime = 2
        if (elcode.eq.'quad')  ndime = 2
        if (elcode.eq.'tet' )  ndime = 3
        if (elcode.eq.'pyr' )  ndime = 3
        if (elcode.eq.'hex' )  ndime = 3
        if (ndime.eq.-1) then
           call myerror (1,' Unknown element type in UCD file')
c          goto 22                 ! CYCLE ?
        endif
        NOD = NTOKS-3      !-- we can imply NOD from the NTOKS-3 
c       IEND=0
        do j=1,nod
c .. or use read(line,*) perhaps?
c         CALL GET_NEXT_TOKEN (LIST, IBEG,IEND, ', ')
          num(j) = str_to_i ( line(list(1,j+3):list(2,j+3)), l_ok )
c         num(j) = str_to_i ( line(ibeg:iend)), l_ok )
        enddo
        itype = 1
        i1 = 0
        i2 = 0
        i3 = 0
        CALL PUT_ELEMENT
     &  (NUMS,INUMS,NEL_OLD+IEL, NUM, NOD,NDIME,ITYPE, IMAT,I1,i2,i3)
      ENDDO

      WRITE(*,'(A,I4,A)')  '>> ',NEL,' elements read'
c.. note that we could skip elements that we dont know how to handle
c--- if n_node_disp /=0 etc. then there is extra data that we could read in
c--- if using DANPLOT then we really must read this stuff in!

      RETURN
      END  

C----------------------------------------------------------------------
      SUBROUTINE WR_MESH_UCD (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL)
C
C     This writes out the mesh in AVS's 'Unstructure Cell Data' format
c        DJK 10-9-96
c
C     DATA FILE FORMAT DESCRIPTION:
C      line 1: NN, NEL, N_load_cases, 0, 0
c      <<  inode, x, y, z  >>              (for inode=1=,nn) so always 3D
c      <<  iel, imat, 'hex', NUM(1:NOD) >> (for iel=1,nel)
c
c     Notes:
c       1: the file may begin with any number of #comments lines, but
c           no comments are allowed afterwards
c       2: column 1 must never be left blank, so we must left_shift data :-(
c       3: blank lines are not permitted
c       4: valid element types are 'pt, line, tri, quad, tet, pyr, prism, hex'
c       5: all types may optionaly have *one* mid-side node, if a 
c           'mid_edge_flags' in the data structure is set with a bit pattern
c          caveat: only some modules can use the mid-edge data values.

      REAL GC(IGC,*)
      INTEGER NUMS(INUMS,*), IO , NUM(32)
      CHARACTER FMT*36, date_stamp*20

      external date_stamp           !- a function to reurn the time and date.

c--------- write the headers ----------
      WRITE (IO,'(''#'',t6,A)')
     &  '---- UCD datafile for AVS viualisation ----'
     & ,'produced by DANPLOT(tm) FE visualiser (d.kidger@man.ac.uk):'
     & ,'file written  :'//date_stamp()
     & ,' '

      n_node_disps = 0              ! we assume all of these are zero
      n_cel_disps  = 0              ! If I later write a set if disps then 
      n_mats       = 0              ! I will need to change the headers too
      WRITE(IO,'(5I8)') NN,NEL, n_node_disps, n_cel_disps, n_mats

c------ write the nodal coords ----------
      IF (NDIM.EQ.2) THEN
        DO I=1,NN            !- i6.6 forced column 1 to be non-blank
          WRITE(IO,'(i6.6, 3F14.5)') i,(GC(J,I),J=1,NDIM),0.
        ENDDO
      ELSEIF (NDIM.EQ.3) THEN
        DO I=1,NN
          WRITE(IO,'(i6, 3F14.5)') i,(GC(J,I),J=1,NDIM)
        ENDDO
      ENDIF
      WRITE(*,'(A,I4,A)')  '>> ',NN,' nodes written'
      iskip=0
      DO IEL=1,NEL
         CALL GET_ELEMENT
     &   (NUMS,INUMS,IEL, NUM, NOD,NDIME,ITYPE, IMAT,I1,i2,i3)

c       FMT='(i6.6, i4,1x,a4, 99I6)'       !- for the element steering
        FMT='(I6, i4,1x,a4, 99I6)'  

        ICODE = NDIME*100+NOD
        IF (ICODE.EQ. 102) THEN
          WRITE(IO,FMT) Iel,imat,'line',(NUM(J),J=1,NOD)
        ELSEIF (ICODE.EQ. 103) THEN
          WRITE(IO,FMT) Iel,imat,'tri',NUM(1),num(3),num(2)

        ELSEIF (ICODE.EQ. 203) THEN
          WRITE(IO,FMT) Iel,imat,'tri ',(NUM(J),J=1,NOD)
        ELSEIF (ICODE.EQ. 206) THEN
          WRITE(IO,FMT) Iel,imat,'tri ',NUM(1),num(3),num(5)
        ELSEIF (ICODE.EQ. 210) THEN
          WRITE(IO,FMT) Iel,imat,'tri ',NUM(1),num(4),num(7)
        ELSEIF (ICODE.EQ. 215) THEN
          WRITE(IO,FMT) Iel,imat,'tri ',NUM(1),num(5),num(9)

        ELSEIF (icode.eq.204) THEN
          WRITE(IO,FMT) Iel,imat,'quad',(NUM(J),J=1,NOD)
        ELSEIF (ICODE.EQ. 208.or.icode.eq.9) THEN
          WRITE(IO,FMT) Iel,imat,'quad',NUM(1),num(3),num(5),num(7)
        ELSEIF (ICODE.EQ. 212) THEN
          WRITE(IO,FMT) Iel,imat,'quad',NUM(1),num(4),num(7),num(10)

        ELSEIF (ICODE.EQ. 304) THEN
          WRITE(IO,FMT) Iel,imat,'tet ',(NUM(J),J=1,NOD)
        ELSEIF (ICODE.EQ. 308) THEN
          WRITE(IO,FMT) Iel,imat,'hex ',(NUM(J),J=1,NOD)
        ELSEIF (ICODE.EQ. 314) THEN
          WRITE(IO,FMT) Iel,imat,'hex ',NUM(1),num(4),num(7),num(10)
     &                                ,NUM(1),num(4),num(7),num(10)
        ELSEIF (ICODE.EQ. 320) THEN
          WRITE(IO,FMT) Iel,imat,'hex ',NUM(1),num(3),num(5),num(7)
     &                                ,NUM(13),num(15),num(17),num(19)

c.. compare the above explicitness, with a call to WTHATN, hence 
c      abstract the 4 (or 8) corner nodes.

        else
          iskip = iskip+1
        endif
      ENDDO
      WRITE(*,'(A,I4,A)')  '>> ',NEL-iskip,' elements written'
      if (iskip.gt.0)
     &WRITE(*,'(A,I4,A)')  'but',iskip,' elements could not be handled'

      RETURN
      END  

C----------------------------------------------------------------------
      SUBROUTINE WR_DISPS_UCD (IO,DISPS,IDISPS,NODOF,NN)
C
C     This writes out the displacements in AVS's 'Unstructure Cell Data' format
c        DJK 13-9-96
c
c      caveats : does not change line 1 to reflect the number of disps 
c                that the file carries
c      File format:
c           line 1:   nodof, 1,1,1
c           line 2:   'mm, mm, mm'
c                    <<inode, disps(1:3,inode) >>

      REAL DISPS(IDISPS,*)
      parameter (m_nodof=4)
      character title(m_nodof)*3, units(m_nodof)*8
      data title/' dx',' dy',' dz',' pp'/
      data units/' mm     ',' mm     ',' mm     ','kN/mm^2 '/

      write (io,'(99i3)') nodof, (1,j=1,nodof)
      
      DO J=1,NODOF
        write (io,'(I3,A,'','',A)')    j, title(j),units(j)
      ENDDO

      DO I=1,NN 
        WRITE(IO,'(i6, 99G13.4)') I, (DISPS(J,I),J=1,NODOF)
      ENDDO
      WRITE(*,'(A,I4,A)')  '>> ',NN,' nodal disps written'

      RETURN
      END  


C-----------------------------------------------------------------------
      SUBROUTINE R_MESH_DIANA (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL)
C
C     This imports a mesh in DIANA's format
c       Dan Kidger  24-1-98
c
c     Data File Format:
c     - a set of sections (in order I guess), the usual nodes and 
c        elements, plus mat_props, loads, etc. 
c
C       Data format :
C        'DIRECT'       - orientation of axes
C        'COORDI'       - usual nodal inode, cx,cy,cz
C        'ELEMEN'       - 2 tables: A: iel_label, 'type', NUM()
C                                   B: / iel_from-iel_to /, imat
C        'GROUPS'       - label a set of elements
C        'SUPPORTS'     - BC fixities (strange?)
C        'MATERI'       - a token per line eg. YOUNG=100.e6
C        'LOADS'        - Applied loads by element and facet#
C        'END'          - end of file flag
C
C      Caveats:
C        1. This implimentation only reads the mesh, not BCs or loads
C        2. Only for 3D
C        3. Assumes 8nb's (HX24L) 
C        4. It assumes that the modules are in the usual order
C           (nodes, before elements)
C
      REAL GC(IGC,*)
      INTEGER NUMS(INUMS,*), NUM(32) , IO
      CHARACTER LINE*255, keyword*6, ETYPE*5
      INTEGER get_nod_DIANA, str_to_i
      EXTERNAL get_nod_DIANA, str_to_i

      NN_OLD  = NN
      NEL_OLD = NEL

c-------------- 0: init ----------------
c        NOD   = 8   !- always 8-noded quads
c        NDIM  = 2
c        NDIME = 2   !-- always 2D facets
c        ITYPE = 1   !-- as 8nq's

c------------- 1: look for 'COORDI' ------------
c cf. read line - test for keyword, & act upon it.
c as in 'pick the first token that is embedded in quotes'
    1   READ (IO,'(a)',IOSTAT=IOS)  LINE
        IEND=0
        CALL GET_NEXT_TOKEN (LINE, IBEG,IEND,' ')
c... is a keyword?
        IF (LINE(IBEG:IBEG).NE.'''') GOTO 1   !- 
        READ(LINE,*) KEYWORD       !- this should chop to 6 chars
c... is it the 'COORDI' keyword?
        PRINT*,'<> Found: ', KEYWORD
        IF (KEYWORD.ne.'COORDI') GOTO 1    !- loop back
          
c-------------- 2: Nodal Coordinates ------------------
      NDIM=3                      !- assume we are in 3D
      DO I=1, 999999 
        READ (IO,*,IOSTAT=IOS) INODE, (GC(J,nn_old+INODE),J=1,NDIM)
        IF (IOS.NE.0) GOTO 21    !'EXIT'
        nn = max (nn,nn_old+inode)
      ENDDO
   21 CONTINUE
c.. hmm what if I left any holes in the list ?
      WRITE(*,'(A,I7,A)')  '>> ',I-1,' nodes read'

c-------------- 2: Element Steering ------------------
c      READ (IO,'(a)',IOSTAT=IOS) LINE         !- skips the CONNEC line?
c      print*,'line=',LINE
      DO iel= 1, 999999                                            
       READ (IO,'(I5,1X,A5,1X,99I8)',iostat=ios)  
     &  iel_label, etype,(num(i), i=1,get_nod_diana(etype))
        IF (IOS.NE.0) GOTO 22

        ndime = 3            !- hack to 3d for now
        nod = get_nod_diana(etype)
        IF (NOD.EQ.0) then 
          call myerror (1,'Element type'//ETYPE//' unknown')
          goto 31  ! CYCLE
        endif
        itype= 1             !- default
        igrp = iel_label     !- use this column to store the label
        imat = 1             !- default to 1 for now

        DO J=1,NOD
          NUM(J) = NUM(J) + NN_OLD   !-- offsets
        ENDDO

        CALL PUT_ELEMENT 
     &  (NUMS,INUMS,NEL_OLD+IEL, NUM, NOD,NDIME,ITYPE, IMAT,IGRP,1,1)
c       CALL PUT_EL_IGRP (NUMS,INUMS,IEL, IGRP)  !- same as this ?

        nel = max (nel,nel_old+iel)
  31    continue
      ENDDO
  22  continue
      WRITE(*,'(A,I7,A)')  '>> ',NEL-NEL_OLD,' elements read'

c------- 2b: Material numbers -----------
      DO II=1,999
        READ (IO,'(a)',IOSTAT=IOS)  LINE
        IEND=0
        CALL GET_NEXT_TOKEN (LINE, IBEG,IEND,'/- ')
          IF (LINE(IBEG:IBEG) .EQ.'''') GOTO 33    !- ALL DONE
        ifrom = STR_TO_I(line(ibeg:iend), ifail)
        CALL GET_NEXT_TOKEN (LINE, IBEG,IEND,'/- ')
        ito = STR_TO_I(line(ibeg:iend), ifail)
        CALL GET_NEXT_TOKEN (LINE, IBEG,IEND,'/- ')
        imat = STR_TO_I(line(ibeg:iend), ifail)

        IC=0
        DO IEL=1,NEL
          CALL GET_EL_IGRP (NUMS,INUMS,IEL, IGRP)
          IF (IGRP.GE.IFROM .AND. IGRP.LE.ITO) THEN
            ic = ic+1
            CALL PUT_EL_IMAT (NUMS,INUMS,IEL, ii)          !- hack for now
c           CALL PUT_EL_IMAT (NUMS,INUMS,IEL, imat)
          ENDIF
        ENDDO
        PRINT*,'>>',ic,' elements set to imat=',imat
      ENDDO
   33 CONTINUE

c-------------- 3: Element Groups "GROUPS"-------------------------

c-------------- 4: Boundary Fixities "SUPPORTS" -------------------

c-------------- 5: Material Properties "MATERI" -------------------

c-------------- 6: Applied Loads "LOADS" --------------------------

c-------------- 7: EOF "EOF" --------------------------------------

      END

C-----------------------------------------------------------------------
      FUNCTION get_nod_diana (ETYPE)
C
C     This simply returns the number of nodes in an element for the given
C     Element type code used by the DIANA F.E. package
C       Dan Kidegr 24-1-98
C
      INTEGER get_nod_diana
      CHARACTER*5 ETYPE
      NOD=0
      IF (ETYPE.EQ.'HX24L') NOD=8     !- 8 node brick
      get_nod_diana = NOD
      END

C-----------------------------------------------------------------------
      SUBROUTINE R_MESH_MARC (IO,IPR,GC,IGC,NDIM,NN,NUMS,INUMS,NEL,P)
C
C     This imports a mesh in MARC's format
c       Dan Kidger  12-2-98
c
c     Data File Format:
c     - a set of sections (in order I guess), the usual nodes and 
c        elements, plus mat_props, loads, etc. 
c
C       Data format :
c         connectivity
C         coordinates
c         transformation
c         define element set
C         + Material properties and groups
c
c   All sections start with a keyword that starts in column 1
c
c
C      Caveats:
C        1. This implimentation only reads the mesh, not BCs or loads
C        2. It assumes that the modules are in the usual order
C           (elements, before nodes
C        3. Material numbers are skipped for now.
C
      REAL GC(IGC,*)
      INTEGER NUMS(INUMS,*), NUM(32), P(*), IO, etype
      CHARACTER LINE*80,  keyword*60, string*8
      INTEGER get_nod_MARC, get_ndime_marc  , str_to_i, istr_end
      EXTERNAL get_nod_MARC, get_ndime_marc , str_to_i, istr_end

      NN_OLD  = NN     !- remember how big the old mesh was.
      NEL_OLD = NEL

c-------------- 0: init ----------------

c------------- 1: look for the next keyword ------------
c note that NEL appears to be defined in the header 'sizing' keyword
      DO ILOOP=1,999999

        READ (IO,'(a)',IOSTAT=IOS)  LINE
        IF (IOS.NE.0) GOTO 99   !- exit
        IEND=0
        IF (LINE(1:1).eq. ' ' ) GOTO 1    !- no keyword yet
        CALL GET_NEXT_TOKEN (LINE, IBEG,IEND,' ')
        KEYWORD = LINE (1:IEND)       !0 force to check from column 1
c       PRINT*,'<> Found: ', KEYWORD
          
        IF (KEYWORD(1:1).eq.'$') THEN           !- a comment
          IF (IPR.GE.3)
     &    PRINT*, LINE (1:ISTR_END(LINE))    !- just echo (colour?)

c---- dummies before the mesh tables -----
        ELSEIF (KEYWORD.eq.'title') THEN   
        ELSEIF (KEYWORD.eq.'sizing') THEN  
        ELSEIF (KEYWORD.eq.'processor') THEN
        ELSEIF (LINE .eq.'large disp') THEN 
        ELSEIF (KEYWORD.eq.'update') THEN   
        ELSEIF (KEYWORD.eq.'finite') THEN   
        ELSEIF (KEYWORD.eq.'finite') THEN   
        ELSEIF (LINE.eq.'all points') THEN  
        ELSEIF (KEYWORD.eq.'dist') THEN   
        ELSEIF (KEYWORD.eq.'shell') THEN
        ELSEIF (KEYWORD.eq.'setname') THEN  
        ELSEIF (KEYWORD.eq.'end') THEN   
        ELSEIF (KEYWORD.eq.'solver') THEN
        ELSEIF (KEYWORD.eq.'optimize') THEN

c---- dummies after the mesh tables -----
        ELSEIF (KEYWORD.eq.'transformation') THEN   
        ELSEIF (KEYWORD.eq.'fixed') THEN   
        ELSEIF (KEYWORD.eq.'contact') THEN   
        ELSEIF (LINE.eq.'no print') THEN   
        ELSEIF (KEYWORD.eq.'post') THEN   
        ELSEIF (KEYWORD.eq.'define') THEN   
        ELSEIF (LINE.eq.'contact table') THEN   
        ELSEIF (LINE.eq.'contact node') THEN   
        ELSEIF (LINE.eq.'auto load') THEN   
        ELSEIF (LINE.eq.'time step') THEN   
        ELSEIF (LINE.eq.'point loads') THEN   
        ELSEIF (KEYWORD.eq.'parameters') THEN   
        ELSEIF (KEYWORD.eq.'control') THEN   
        ELSEIF (KEYWORD.eq.'body') THEN   
        ELSEIF (KEYWORD.eq.'continue') THEN   

        ELSEIF (KEYWORD.eq.'elements') THEN   !- which elements are in use
          CALL GET_NEXT_TOKEN (LINE, IBEG,IEND,' ')
c         print*,' element code=',line(ibeg:iend)
          etype = STR_TO_I(line(ibeg:iend), ifail)
          ndime = get_ndime_marc(etype)
          nod = get_nod_marc(etype)
          itype=1
          call NUM_TO_CODE (NDIME,NOD,ITYPE, STRING)
          IF (IPR.GE.3)
     &    PRINT*, 'Using element :'//STRING

c-------------- 2: Element Steering ------------------
        ELSEIF (KEYWORD.eq.'connectivity') THEN

      DO iel= 1, 999999                                            
       READ (IO,*,iostat=ios)  
     &  iel_label, etype, (num(i), i=1,get_nod_marc(etype))
        IF (IOS.NE.0) GOTO 22       !- end of section

        nod = get_nod_marc(etype)
        ndime = get_ndime_marc(etype)
c       IF (ETYPE.eq.75) NOD=0       !- *hack to skip plates (fools FSTRIP)
        IF (NOD.EQ.0) then 
c         print*,'Skipping unknown Element type:',ETYPE
          goto 31  ! CYCLE
        endif
        itype= 1             !- default
        igrp = iel_label     !- use this column to store the label?
        imat = 0             !- default to 0 (cos all get defined)

C-- reverse orientation of 4nq's (cos they point down)
        IF (NDIME.EQ.2.AND.NOD.EQ.4) 
     &  CALL mirror_element (num, ndime,nod,itype) 

        DO J=1,NOD
          NUM(J) = NUM(J) + NN_OLD   !-- offsets
        ENDDO

        CALL PUT_ELEMENT 
     &  (NUMS,INUMS,NEL_OLD+IEL, NUM, NOD,NDIME,ITYPE, IMAT,IGRP,1,1)

        nel = max (nel,nel_old+iel)
  31    continue
      ENDDO
  22  continue
      backspace (io)   !- cos we read the next keyword
      WRITE(*,'(A,I7,A)')  '>> ',NEL-NEL_OLD,' elements read'

        ELSEIF (KEYWORD.eq.'coordinates') THEN

c-------------- 2: Nodal Coordinates ------------------
c (note that NDIM may accidently be already defined (eg an original 2d mesh??)
c  note that this starts with a 'coordinates' keyword that we have skipped over
      READ (IO,*,IOSTAT=IOS) NDIM, npts
      DO I=1, Npts
        READ (IO,'(i5,3G10.5)',IOSTAT=IOS)
     &   INODE, (GC(J,nn_old+INODE),J=1,NDIM)
c       IF (IOS.NE.0) GOTO 21    !'EXIT'
        nn = max (nn,nn_old+inode)
      ENDDO
c  21 CONTINUE
c.. hmm what if I left any holes in the list ?
      WRITE(*,'(A,I7,A)')  '>> ',I-1,' nodes read'


c--------------- Material properties ---------------------------
      ELSEIF (KEYWORD.eq.'mooney') THEN   
        READ (IO,*) IMAT
        READ (IO,*) E,v      !- save these ?
        CALL READ_MARC_MATS (IO,NUMS,INUMS,NEL, IMAT)
      ELSEIF (KEYWORD.eq.'hypoelastic') THEN   
        READ (IO,*) IMAT
        READ (IO,*) E,v      !- save these ?
        CALL READ_MARC_MATS (IO,NUMS,INUMS,NEL, IMAT)
      ELSEIF (KEYWORD.eq.'isotropic') THEN   
        READ (IO,*) IMAT
        READ (IO,*) E,v      !- save these ?
        CALL READ_MARC_MATS (IO,NUMS,INUMS,NEL, IMAT)


c--------------------------------------------
        ELSE       !- other possible keywords
          IF (IPR.GE.3)
     &    PRINT*,'<> Skipping: ', KEYWORD
        ENDIF

    1 CONTINUE
      ENDDO     !- loop keyword search


c----------- all done ---------
   99 PRINT*,'>>', ILOOP, ' MARC Keywords found'

      END

C-----------------------------------------------------------------------
      SUBROUTINE READ_MARC_MATS (IO,NUMS,INUMS,NEL, IMAT)
c
c     This reads a block of element numbers and sets them
c      all to the given IMAT
c
      INTEGER NUMS(INUMS,*), IMAT, IO
      CHARACTER LINE*80
      INTEGER STR_TO_I
      EXTERNAL STR_TO_I

      IC = 0                       !- count how many elements found
      DO II=1,99999                  !- loop forever
        READ (IO,'(a)',IOSTAT=IOS)  LINE
        CALL COUNT_TOKENS (LINE, NTOKS,' ')
        IF (LINE(80:80).EQ.'c') NTOKS = NTOKS -1
        IC = IC + NTOKS
        iend=0
        DO JJ=1,NTOKS
          CALL GET_NEXT_TOKEN (LINE, IBEG,IEND,' ')
          iel = STR_TO_I(line(ibeg:iend), ifail)
          CALL PUT_EL_IMAT (NUMS,INUMS,IEL, imat) 
        ENDDO
        IF (LINE(80:80).EQ.' ') GOTO 21   !- all done (no continuation marker)
      ENDDO
   21 CONTINUE
C     WRITE(*,*)  IC, ' Elements set to imat=', imat 
      RETURN
      END

C-----------------------------------------------------------------------
      FUNCTION get_nod_marc (ETYPE)
C
C     This simply returns the number of nodes in an element for the given
C     Element type code used by the MARC F.E. package
C       Dan Kidger 12-2-98
C
      implicit none
      INTEGER get_nod_marc
      INTEGER ETYPE, nod
      NOD=0
      IF (ETYPE.EQ.7)  NOD=8     !- 8 node brick
      IF (ETYPE.EQ.84) NOD=8     !- 8 node brick with some odd extra ?
      IF (ETYPE.EQ.9)  NOD=2     !- beams (or rods?)
      IF (ETYPE.EQ.75) NOD=4     !- plates

      get_nod_marc = NOD
      END

C-----------------------------------------------------------------------
      FUNCTION get_ndime_marc (ETYPE)
C
C     This simply returns the number of dimensions in an element for the given
C     Element type code used by the MARC F.E. package
C       Dan Kidegr 12-2-98
C
      implicit none
      INTEGER get_ndime_marc, ndime
      INTEGER ETYPE
      NDIME=0
      IF (ETYPE.EQ.7)  NDIME=3     !- 8 node brick
      IF (ETYPE.EQ.84) NDIME=3     !- 8 node brick as well (+extra node)
      IF (ETYPE.EQ.9)  NDIME=1     !- beams (or rods?)
      IF (ETYPE.EQ.75) NDIME=2     !- plates
      get_ndime_marc = NDIME
      END

C-----------------------------------------------------------------------
      SUBROUTINE R_MESH_ANSYS (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL)
C
C     This imports a mesh in ANSYS's format
c       Dan Kidger  4-2-98
c
c     Data File Format:
c     1. No sections just a set of lines of data
c        The first character indicates whether this is a node (N)
c        or an element (EN)
c     2. Seems to be a fixed format file
c     3. Comma-seperated fields
c     4. if z-coord is zero then it is omitted! 
c     5. Nodes seem to be sequential and complete
c
C      Caveats:
C
      REAL GC(IGC,*)
      INTEGER NUMS(INUMS,*), NUM(32) , IO
      CHARACTER LINE*255
      INTEGER str_to_i
      REAL str_to_real
      EXTERNAL str_to_i, str_to_real

      NN_OLD  = NN
      NEL_OLD = NEL

c-------------- 0: init ----------------
c        NOD   = 8   !- always 8-noded quads
c        NDIM  = 2
c        NDIME = 2   !-- always 2D facets
c        ITYPE = 1   !-- as 8nq's

c------------- 1: look for the first line of data ------------
c    1 READ (IO,'(a)',IOSTAT=IOS)  LINE
c      IEND=0
c      IF (LINE(1:1).EQ.'/') GOTO 1

c------------- 2: loop and handle each ---------------
      DO ILINE=1,999999
        READ (IO,'(a)',IOSTAT=IOS)  LINE
        CALL COUNT_TOKENS (LINE, NTOKS,', ')      

        IEND=0
        CALL GET_NEXT_TOKEN (LINE, IBEG,IEND,', ')

        IF (LINE(IBEG:IEND).EQ.'/GO') THEN   !----- finished ? ---
          goto 99
        ELSEIF (LINE(IBEG:IEND).EQ.'/COM') THEN   !----- first line
        ELSEIF (LINE(IBEG:IEND).EQ.'/NOPR') THEN   !----- second line

        ELSEIF (LINE(IBEG:IEND).EQ.'N') THEN   !----- nodes -------
          CALL GET_NEXT_TOKEN (LINE, IBEG,IEND,', ')       !'R5.1'
c           print*, ibeg,iend, '"',line(ibeg:iend),'"'
          CALL GET_NEXT_TOKEN (LINE, IBEG,IEND,', ')       !'LOC'
          CALL GET_NEXT_TOKEN (LINE, IBEG,IEND,', ')       ! inode
          inode = STR_TO_I(line(ibeg:iend), ifail)
          CALL GET_NEXT_TOKEN (LINE, IBEG,IEND,', ')       ! '0'
          CALL GET_NEXT_TOKEN (LINE, IBEG,IEND,', ')       ! x
          x = STR_TO_REAL(line(ibeg:iend), ifail)
          CALL GET_NEXT_TOKEN (LINE, IBEG,IEND,', ')       ! y
          y = STR_TO_REAL(line(ibeg:iend), ifail)
          if (ntoks.ge.8) then          !- was the z-coord specified?
            CALL GET_NEXT_TOKEN (LINE, IBEG,IEND,', ')       ! z
            z = STR_TO_REAL(line(ibeg:iend), ifail)
          else
            z=0.
          endif
          NN=max (nn,nn_old+inode)
          GC(1,nn_old+INODE)=x
          GC(2,nn_old+INODE)=y
          GC(3,nn_old+INODE)=z

        ELSEIF (LINE(IBEG:IEND).EQ.'EN') THEN   !----- elements -------

          CALL GET_NEXT_TOKEN (LINE, IBEG,IEND,', ')       !'R5.1'
          CALL GET_NEXT_TOKEN (LINE, IBEG,IEND,', ')       !'ATTR' or 'NODE'

          IF (LINE(IBEG:IEND).EQ.'ATTR') THEN
            CALL GET_NEXT_TOKEN (LINE, IBEG,IEND,', ')       ! nod
            nod = STR_TO_I(line(ibeg:iend), ifail)
            CALL GET_NEXT_TOKEN (LINE, IBEG,IEND,', ')       ! 1d or 2d?
            ifoo = STR_TO_I(line(ibeg:iend), ifail)
             ndime=2
             if (ifoo.eq.2) ndime=1
            CALL GET_NEXT_TOKEN (LINE, IBEG,IEND,', ')       ! ??? (='1')
            CALL GET_NEXT_TOKEN (LINE, IBEG,IEND,', ')       ! imat
            imat = STR_TO_I(line(ibeg:iend), ifail)
            CALL GET_NEXT_TOKEN (LINE, IBEG,IEND,', ')       ! iel
            iel2 = STR_TO_I(line(ibeg:iend), ifail)

          ELSEIF (LINE(IBEG:IEND).EQ.'NODE') THEN
            do j=1,nod
              CALL GET_NEXT_TOKEN (LINE, IBEG,IEND,', ')       ! nod
              num(j) = STR_TO_I(line(ibeg:iend), ifail)
            enddo
            itype=1
            NEL = NEL+1
            CALL PUT_ELEMENT (NUMS,INUMS,NEL_OLD+NEL, NUM, 
     &                       NOD,NDIME,ITYPE, IMAT,1,1,1)
c           write(*,'(99i5)') nel,nod,(num(j),j=1,nod),imat
          ELSE
            CALL MYERROR (1,'** Unknown token ('//LINE(IBEG:IEND)//')')
          ENDIF

        ENDIF
      ENDDO
  99  continue
      WRITE (*,'(A,I7,A)')  '>> ',NN-nn_old,' nodes read'
      WRITE (*,'(A,I7,A)')  '>> ',NEL-NEL_OLD,' elements read'
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE R_MESH_BOVIDA (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL)
C
C     This +imports a mesh in Tierra Amadra's 'BOPRE' format
c      (note that this can be used to add meshes together)
C        DJK  4-12-96
C
C     File format:
C        NEL,NN
C        elements as  <IEL,IMAT, NUM(1:8) >
C        nodes as <INODE, X,Y>
C
C     Caveats:
C        be careful if adding a 2D mesh to an existing 3d one. (z-coord)
C

      REAL GC(IGC,*)
      INTEGER NUMS(INUMS,*), NUM(32)

      NN_OLD  = NN
      NEL_OLD = NEL

   1  READ (IO,*,IOSTAT=IOS) NEL,NN
      CALL IN_TEST(IO,IOS,*1,*999)

c---------- Read in the Elements ---------
      NOD   = 8                !- 
      NDIME = 2                !- always 8-noded quadrilaterals.
      ITYPE = 1                !- 
      DO IEL=1,NEL
         READ (IO,*) IDUMMY,IMAT,(NUM(J),J=1,NOD)
         DO J=1,NOD
            NUM(J) = NUM(J) + NN_OLD   !-- offsets
         ENDDO
         CALL PUT_ELEMENT 
     &   (NUMS,INUMS,NEL_OLD+IEL, NUM, NOD,NDIME,ITYPE, IMAT,1,1,1)
      ENDDO
c     CLOSE (IO)
      WRITE(*,'(A,I4,A)')  '>> ',NEL,' elements read'
      NEL= NEL_OLD + NEL

c------- Read in the Nodes -------
      IF (NN_OLD.eq.0) NDIM = 2    !- force 2d if not yet set?
      DO I=1,NN   
        READ (IO,*)  ID,(GC(J,ID+NN_OLD),J=1,NDIM)
      ENDDO
      WRITE(*,'(A,I4,A)')  '>> ',NN,' nodes read'
      NN = NN_OLD + NN

  999 CONTINUE
      END

C-----------------------------------------------------------------------
      SUBROUTINE WR_MESH_BOVIDA (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL)
C
C     This writes out the mesh in Tierra Amadra's 'BOPRE' format
C      DJK  4-12-96
C
C     File format:
C        NEL,NN
C        elements as  <IEL,IMAT, NUM(1:8) >
C        nodes as <INODE, X,Y>
C
C     Caveats:
c      1/  Only valid 8nq's are written .. other elements are ignored, but
c          this will make the NEL count at the top_of_file wrong.
c      2/  3D geometry *is* written .. cos its simple to ignore 'z' later
c
      REAL GC(IGC,*)
      INTEGER NUMS(INUMS,*), IO, NUM(32)

      WRITE(IO,'(I5)') NEL,NN

      IC = 0
      DO IEL=1,NEL
         CALL GET_ELEMENT
     &   (NUMS,INUMS,iEL, NUM, NOD,NDIME,ITYPE, IMAT,I1,i2,i3)
         IF (NOD.EQ.8.AND.NDIM.EQ.2) THEN
           IC = IC+1
           WRITE(IO,'(99I6)') IC,IMAT,(NUMS(J,IEL),J=1,NOD)
         ENDIF
      ENDDO
      WRITE(*,'(A,I4,A)')  '>> ',IC,' elements written'
      IF (IC.NE.NEL)
     &WRITE(*,'(A)') '*** COMMENT: non-8nq element(s) detected.'

      DO I=1,NN
         WRITE(IO,'(I5,3F14.5)') I,(GC(J,I),J=1,NDIM)
      ENDDO
      WRITE(*,'(A,I4,A)')  '>> ',NN,' nodes written'
      RETURN
      END  

C----------------------------------------------------------------------
      SUBROUTINE WR_MESH_VRML1 (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL)
C
C     This writes out the mesh in the Virtual Reality Merkup Language (v1.0)
c     eg. to view the spine dataset.
c        DJK 28-10-98
c
C  DATA FILE FORMAT DESCRIPTION:
c     A set of 'objects' in a 'c'-like format.
c     line one is: #VRML V1.0 ascii
c     in particular we use:
C       geometry IndexedFaceSet {
c          coord Coordinate { point [
c          << x,y,z ; i=1,nn >>
c                                    ] }
c          coordindex [
c          <<  num(:),-1; i=1,nel >>
c                                    ] 
c          solid FALSE   creaseangle 0.5 normalindex [] texCoordindex []
c        }
c
c  Notes:
c    0. The file may start with any number of # comment lines
c    1. There can be any number of IndexedFaceSet's, each with its own 'colour'
c    2. 3D elements must be transfromed in their FACETs, and presumably 
c       we would want to FSTRIP the internal facets
c    3. Nice to write facets out in groups of the same material#
c    4. Nodes start at zero, so num()--
c    5.
c    6.
c
      REAL GC(IGC,*)
      INTEGER NUMS(INUMS,*), IO, NUM(32), NUM2(32)
c      CHARACTER FMT*36, date_stamp*20


c--------- 0: write the headers ----------
      write (io,'(a)') '#VRML V1.0 ascii',' '
c     write (io,'(a)') '#VRML V2.0 utf8',' '

c--------- 1: write an IndexedFaceSet block ----------
c     write (io,'(a)') 'geometry Indexed FaceSet {'
      write (io,'(a)') 'Separator {'
c--------- 2: write the nodes ----------
      write (io,'(t3,a)') 'Coordinate3 { point ['
      do i=1, nn
        write (io,'(t6,3e13.4,a)') (gc(j,i),j=1,3),','
      enddo
      write (io,'(a)') '] }'

c--------- 3: write the facets ----------
c.. assume for now that we have done *STRIP_FACETS, 
c      *DELETE_INTERNAL_FACETS and *FACETS_TO_ELEMNTS
c - ?can add a 'vertexorder CLOCKWISE' to help the browser.
c - also what do we do about 1d entities?
      iskip=0
      write (io,'(t3,a)') 'Separator {'
      write (io,'(t6,a)') 'IndexedFaceSet {'
      write (io,'(t9,a)') 'coordIndex ['
      DO IEL = 1,NEL
        CALL GET_ELEMENT (NUMS,INUMS,IEL, NUM, NOD,NDIME,ITYPE
     &      ,IMAT,IUSER1,IUSER2,IUSER3)
        IF (IMAT.LE.0) GOTO 11        ! patch to not draw invis elemnts?
        CALL GET_FACE (NUM,NOD,NDIME,ITYPE
     &                ,NFACES,NUM2,NN_F,NN_FT, 1 )     !- just NFACES
        DO IFACE=1,NFACES         ! loop this element's faces
          CALL GET_FACE (NUM, NOD,NDIME,ITYPE, IFACE,NUM2,NN_F,NN_FT, 2)
c         .. not C-like index starting at node#=0
c         num2(nn_f+1)=-1   !- create the end-of-record flag
          write (io,'(99(i6,a))') (num2(j)-1,',',j=1,nn_f),-1,','
        enddo
   11   continue
      enddo
      write (io,'(t6,a)') '] }'    !- end of list of facet steering

c--------- 3: write end of IndexedFaceSet ----------
c.. need to check the format of these.
      write (io,'(a,t9,a)') '#','solid FALSE'
      write (io,'(a,t9,a)') '#','creaseAngle 0.5'
      write (io,'(a,t9,a)') '#','normalIndex [ ]'
      write (io,'(a,t9,a)') '#','texCoordIndex [ ]'

      write (io,'(t6,a)') '}'
      write (io,'(t3,a)') '}'
      write (io,'(a)') '}'

c----------------------
      WRITE(*,'(A,I4,A)')  '>> ',NEL-iskip,' elements written'
      if (iskip.gt.0)
     &WRITE(*,'(A,I4,A)')  'but',iskip,' elements could not be handled'

      RETURN
      END  

C----------------------------------------------------------------------
      SUBROUTINE WR_VRML_HEADERS (IO)
C
C     This writes a VRML node to give a nice ground 
c        DJK 28-10-98

      WRITE (IO,'(a)')
     & 'WorldInfo { info [',
     &'"Created by DANFE finite element Package",',
     &'"c. Dan Kidger 1999", "d.kidger@man.ac.uk"',
     &'] }'

      END

C----------------------------------------------------------------------
      SUBROUTINE WR_VRML_SUNSKY (IO)
C
C     This writes a VRML node to give a nice ground 
c        DJK 28-10-98

      WRITE (IO,'(a)')
     & " Background {"
     &,"  groundAngle 1.5708"
     &,"  groundColor [ 0.1 0.2 0.1,   0.3 0.5 0.3 ]"
     &,"  skyAngle    [ 0.05, 0.1, 1, 1.5708 ]"
     &,"  skyColor    [ 1 1 0, 1 1 0.5,   0.125 0.125 0.5, "
     &,"               0.3 0.3 0.55,  0.64 0.734 0.844 ]"
     &,"}"
      END
C----------------------------------------------------------------------
      SUBROUTINE WR_VRML_MATS (IO)
C
C     This writes a VRML color table for my standard 15 materials 
c        DJK 28-10-98
c
      integer pal_default (3,15)
      DATA PAL_DEFAULT/
     &     0, 90, 10,   138, 75, 39,    83,128, 57,    90,  0,190,
     &   123, 45, 76,
     &   194, 49, 56,   252, 78,207,   253,202, 90,     0,255,  0, 
     &    40, 40,255,   169,133, 23,   255,  0,  0,   255,127,  0,
     &    255,255, 0,   255,255,255/


      write (io,'(a)') '          color Color {'
      write (io,'(a)') '            color ['
      do i=1,15 !(really 1:NMATS)
        c_red  = pal_default(1,i)/255.
        c_green= pal_default(2,i)/255.
        c_blue = pal_default(3,i)/255.
        write (io,'(3f6.3,a)') c_red,c_green,c_blue,','
      enddo
      write (io,'(a)') '            ]'  ! end of color 
      write (io,'(a)') '          } #end of color Color'
      END


C----------------------------------------------------------------------
      SUBROUTINE WR_VRML_VIEWPOINTS (IO,GC,IGC, NN,NDIM, dist)
C
C     This writes four VRML viewpoints; down each axis and an isometric
c       where GC is the set of nodal coords and 'dist' is the non-dimensional 
c       viewing distance (try 1.5)
c           DJK 28-10-98
c
      REAL GC(IGC,*), dist
      real gc_min(5), gc_max(5), xc,yc,zc, farx,fary,farz
! 170315: disti is uninitialized! so hack
      disti=dist
      CALL GET_MESH_RANGE (GC,IGC, NN,NDIM, GC_MIN,GC_MAX,DIAG)
      Xc =  (gc_max(1)+gc_min(1))/2.
      Yc =  (gc_max(2)+gc_min(2))/2.
      Zc =  (gc_max(3)+gc_min(3))/2.
      farx = xc + diag*dist
      fary = yc + diag*dist
      farz = zc + diag*dist
      sx=0.4499; sy=-0.8528; sz=-0.2653; s=3.

      write (io,'(a,3g14.4,a)') 
     &  'Viewpoint {description "down Z" position',xc,yc,farz,
     &  ' orientation 0 1 0 0.}',
     &  'Viewpoint {description "down X" position',farx,yc,zc,
     &  ' orientation 0 1 0 1.570796327}',
     &  'Viewpoint {description "down Y" position',xc,fary,zc,
     &  ' orientation -1 0 0 1.570796327}',
c     &  'Viewpoint {description "general" position',farx,fary,farz,
c     &  ' orientation 0 1 0 0.1}'
     &  'Viewpoint {description "general" position',
c     & farx/2.,fary/2.,farz/2.,
     &  158.4294, 143.0079, 138.1471, 
     &  ' orientation 0.4499  -0.8528  -0.2653   0.7599}'

     & , 'Viewpoint {description "gen2" position',
     & xc+sx*diag*dist*s,yc+sy*diag*disti*s,zc+sz*diag*disti*S,
     &  ' orientation 0.4499  -0.8528  -0.2653   0.7599}'
      RETURN
      END

C----------------------------------------------------------------------
      SUBROUTINE WR_MESH_VRML2 (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL)
C
C     This writes out the mesh in VRML v2.0
c     eg. to view the spine dataset.
c        DJK 6-5-99
c
C  DATA FILE FORMAT DESCRIPTION:
c     A set of 'objects' in a 'c'-like format.
c     line one is: #VRML V2.0 utf8
c  (in VRML 2?)
c     in particular we use:
C       geometry IndexedFaceSet {
c          coord Coordinate { point [
c          << x,y,z ; i=1,nn >>
c                                    ] }
c          coordindex [
c          <<  num(:),-1; i=1,nel >>
c                                    ] 
c          solid FALSE   creaseangle 0.5 normalindex [] texCoordindex []
c        }
c
c  Notes:
c    0. The file may start with any number of # comment lines
c    1. There can be any number of IndexedFaceSet's, each with its own 'colour'
c    2. 3D elements must be transfromed in their FACETs, and presumably 
c       we would want to FSTRIP the internal facets
c    3. Nice to write facets out in groups of the same material#
c    4. Nodes start at zero, so num()--
c    5.
c    6.
c
      REAL GC(IGC,*)
      INTEGER NUMS(INUMS,*), IO, NUM(32), NUM2(32)
c      CHARACTER FMT*36, date_stamp*20

      iskip = 0 
c--------- 0: write the headers ----------
      write (io,'(a)') '#VRML V2.0 utf8',' '


c--------- 1a: Navigation Info ------

c--------- 1b: Creator Info ------
      call WR_VRML_HEADERS (IO)
c--------- 1c: Create some useful Viewpoints ------
      call WR_VRML_VIEWPOINTS (IO,GC,IGC, NN,NDIM, 1.4)
c--------- 1d: Create some a background sky and ground ------
      call WR_VRML_SUNSKY (IO)

c--------- 2: start a generic node ------
c assume materials will have 50% shineness -the colours will get changed later
      write (io,'(a)') 'Transform {'
      write (io,'(a)') '  children ['
      write (io,'(a)') '    Shape {'

      write (io,'(a)') '      appearance Appearance {'
      write (io,'(a)') '        material Material {'
      write (io,'(a)') '          ambientIntensity .25'
      write (io,'(a)') '          diffuseColor .5 .5 .5'
      write (io,'(a)') '          specularColor 1. 1. 1.'
      write (io,'(a)') '          emissiveColor .1 .1 .1'
      write (io,'(a)') '          shininess .5 '
      write (io,'(a)') '          transparency 0 '
      write (io,'(a)') '        }'
      write (io,'(a)') '      }'

c------- 3: write a nodal coordinate list ------
      write (io,'(a)') '      geometry IndexedFaceSet {'
      write (io,'(a)') '        coord Coordinate {'
      write (io,'(a)') '          point ['
      do i=1, nn    !-- write the nodes --
        write (io,'(t6,3e13.4,a)') (gc(j,i),j=1,3),','
      enddo
      write (io,'(a)') '          ]'
      write (io,'(a)') '        } #end coord'

c-------- 4: write an element steering list ------
      write (io,'(a)') '          coordIndex ['
      do iel=1, nel   !--- write the elements (as facets) ---
        CALL GET_ELEMENT (NUMS,INUMS,IEL, NUM, NOD,NDIME,ITYPE
     &      ,IMAT,IUSER1,IUSER2,IUSER3)
        IF (IMAT.LE.0) GOTO 11        ! patch to not draw invis elemnts?
        CALL GET_FACE (NUM,NOD,NDIME,ITYPE
     &                ,NFACES,NUM2,NN_F,NN_FT, 1 )     !- just NFACES
        DO IFACE=1,NFACES         ! loop this element's faces
          CALL GET_FACE (NUM, NOD,NDIME,ITYPE, IFACE,NUM2,NN_F,NN_FT, 2)
c         .. note C-like index starting at node#=0
c         num2(nn_f+1)=-1   !- create the end-of-record flag
c         write (io,'(99(i6,a))') (num2(j)-1,',',j=1,nn_f),-1,','
          write (io,*) (num2(j)-1,j=1,nn_f),-1
        enddo
   11   continue
      enddo
      write (io,'(a)') '          ] #end of CoordIndex'


      ncolors = 15
      call WR_VRML_MATS (IO)    !- write out this '15' colour table

      write (io,'(a)') '          colorIndex ['
      do iel=1, nel    !--- write the colors of each facet
        CALL GET_ELEMENT (NUMS,INUMS,IEL, NUM, NOD,NDIME,ITYPE
     &      ,IMAT,IUSER1,IUSER2,IUSER3)
        IF (IMAT.gt.0) then
        CALL GET_FACE (NUM,NOD,NDIME,ITYPE
     &                ,NFACES,NUM2,NN_F,NN_FT, 1 )     !- just NFACES
        imat2 = mod(imat-1,ncolors)+1 -1
        write (io,'(99(i3,a))') (imat2,',',iface=1,nfaces)
        endif
      enddo
      write (io,'(a)') '          ]'
      write (io,'(a)') '          colorPerVertex FALSE' 



      write (io,'(a)') '       ccw FALSE'      !- danfe uses clockwise polygons
      write (io,'(a)') '       solid FALSE'    !- show backfacing faces too
      write (io,'(a)') '      } #end Geometry IndexFaceSet'
      write (io,'(a)') '    } #end Shape'
      write (io,'(a)') '  ] #end Children'
      write (io,'(a)') '} #end Transform'

c--------- 3: write end of IndexedFaceSet ----------
c.. need to check the format of these.
c      write (io,'(a,t9,a)') '#','solid FALSE'
c      write (io,'(a,t9,a)') '#','creaseAngle 0.5'
c      write (io,'(a,t9,a)') '#','normalIndex [ ]'
c      write (io,'(a,t9,a)') '#','texCoordIndex [ ]'


c----------------------
      WRITE(*,'(A,I4,A)')  '>> ',NEL-iskip,' elements written'
      if (iskip.gt.0)
     &WRITE(*,'(A,I4,A)')  'but',iskip,' elements could not be handled'

      RETURN
      END  

C-----------------------------------------------------------------------
      SUBROUTINE R_FEMVIEW (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL)
c
c     This reads a mesh in FEMVIEW format
C       Dan Kidger  10-3-97
C
C     File format:
C        As a set of tables cf. my DANMESH or my DXF reader.
C         - in any order but each is in a rigidly fixed format.
C
C     Caveats:
C        not yet implimented!
c
      REAL GC(IGC,*)
      INTEGER NUMS(INUMS,*)!, NUM(32)

      NN_OLD  = NN
      NEL_OLD = NEL
c     if (nn.eq.0) ndim=3         !- if no nodes yet .. go into 3d ?

c   1 CONTINUE     !-- loop back point  (cf F90 DO..ENDDO)

      READ (IO,*,IOSTAT=IOS) icode
c.. now act on icode - by calling daughter routines. (cf READ_DXF)
c     IF (ICODE.EQ.1) THEN
c     ELSEIF (ICODE.EQ.2) THEN
c     ELSEIF (ICODE.EQ.3) THEN
c     ELSE
c     ENDIF

c 999 CONTINUE

      RETURN
      END
C-----------------------------------------------------------------------
      SUBROUTINE WR_FEMVIEW (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL)
C
C     This writes out the mesh in 'Femview' .FV format
C      Dan Kidger 7-1-97
c
c     File Format:
c        '1C' , header name
c        '2C'    starts the nodal coords
c          << -1, inode,x,y,z >>
c        -3      = end of sequence
c        '3C'    starts the element steering
c          << -1, IEL, ITYPE, IGRP, IMAT, IVARINT, IPHYS 
c             -2,   NUM(1:NOD)                            >>
C        -3      = end of sequence
C        9999    = EOF
C
      REAL GC(IGC,*)
      INTEGER NUMS(INUMS,*), IO, NUM(99)  !, NUM2(99)
      CHARACTER FMT*36
       
      WRITE (IO,'(1X,I4,A1,A6)')   1,'C','Dan2FV'

      WRITE (IO,'(1X,I4,A1,A6)')   2,'C'
      DO I=1,NN
        WRITE(IO,'(1X,I2,I5,3E12.5)') 
     &     -1,I,(GC(J,I),J=1,NDIM),(0.,J=NDIM+1,3)
      ENDDO
      WRITE(IO,'(1X,I2)') -3
      WRITE(*,'(A,I4,A)')  '>> ',NN,' nodes written'

      WRITE (IO,'(1X,I4,A1,A6)')   3,'C'

c     FMT='(1X,''-1'',4i5,5X,2I5, (1X,''-2'',15I5) )'
      FMT='(1X,''-1'',4i5/, (1X,''-2'',15I5) )'
      iskip = 0
      DO IEL=1,NEL
        CALL GET_ELEMENT
     &  (NUMS,INUMS,IEL, NUM, NOD,NDIME,ITYPE, IMAT,I1,i2,i3)
c       CALL MUNG_FEMVIEW (NUM, NOD,NDIME,ITYPE, ITYPE_fv,NUM2)
        IGRP = 1
        IVAR = 1
        IPHYS= 1

      ICODE = NDIME*100+NOD
      IF (ICODE.EQ. 203) THEN          ! TR3
        WRITE(IO,FMT) IEL,3, IGRP,IMAT,NUM(1),num(3),num(2)
      ELSEIF (ICODE.EQ. 204) THEN      ! QU4
        WRITE(IO,FMT) IEL,5, IGRP,IMAT,NUM(1),num(4),num(3),num(2)
      ELSEIF (ICODE.EQ. 206) THEN      ! TR6
        WRITE(IO,FMT) IEL,7, IGRP,IMAT,NUM(1),(num(i),i=nod,2,-1)
      ELSEIF (ICODE.EQ. 208) THEN      ! QU8
c       WRITE(IO,FMT) IEL,9, IGRP,IMAT,NUM(1),(num(i),i=nod,2,-1)
        WRITE(IO,FMT) IEL,10,IGRP,IMAT,NUM(3),num(1),num(7),num(5),
     &                                 num(2),num(8),num(6),num(4)
      ELSEIF (ICODE.EQ. 209) THEN      ! QU9
        WRITE(IO,FMT) IEL,10,IGRP,IMAT,NUM(1),(num(i),i=8,2,-1), num(9)

c     ELSEIF (ICODE.EQ. 306) THEN      ! PE6 - 3d wedge element
c       WRITE(IO,FMT) IEL,11,IGRP,IMAT,NUM(1),num(4),num(3),num(2)
      ELSEIF (ICODE.EQ. 308) THEN      ! HE8
c        WRITE(IO,FMT) IEL,13,IGRP,IMAT,NUM(1),num(5),num(6),num(2)
c     &                                ,NUM(4),num(8),num(7),num(3)
        WRITE(IO,FMT) IEL,1, IGRP,IMAT,NUM(1),num(5),num(6),num(2)
     &                                ,NUM(4),num(8),num(7),num(3)
      ELSEIF (ICODE.EQ. 320) THEN      ! HE20
        WRITE(IO,FMT) IEL,17,IGRP,IMAT
     &     ,NUM(1),num(17),num(5),num(13),num(6),num(18),num(2),num(9) 
     &     ,NUM(12),       num(16),       num(14),       num(10) 
     &     ,NUM(4),num(20),num(8),num(15),num(7),num(19),num(3),num(11) 
      ELSE
          iskip = iskip+1
      ENDIF

      ENDDO
      WRITE(IO,'(1X,I2)') -3
      WRITE(*,'(A,I4,A)')  '>> ',NEL-iskip,' elements written'
      if (iskip.gt.0)
     &WRITE(*,'(A,I4,A)')  'but',iskip,' elements could not be handled'
      WRITE(IO,'(1X,I4)') 9999
      RETURN
      END  

C-----------------------------------------------------------------------
c      SUBROUTINE MUNG_FEMVIEW (NUM, NOD,NDIME,ITYPE, ITYPE_fv,NUM2)
C
C     This turns a DANFE elementinto a Femview one.
C      ie. turns NOD,NDIME into the equivalent ITYPE_fv
C          and MUNGs the order of the nodes into NUM2
C          ( cf MIRROR_ELEMENT)
C      DJK 7-1-97 
C
c      INTEGER ITYPE_fv, NUM(NOD),NDIME,ITYPE, NUM2(*)
c
c      ICODE = NDIME*100+NOD
c
c      FMT='(I6, i4,1x,a4, 99I6)'  
c      IF (ICODE.EQ. 102) THEN
c        WRITE(IO,FMT) Iel,imat,'line',(NUM(J),J=1,NOD)
c      ELSEIF (ICODE.EQ. 103) THEN
c        WRITE(IO,FMT) Iel,imat,'tri',NUM(1),num(3),num(2)
c
c
c      RETURN
c      END
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C     My Quickplot routine  :-)
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

      SUBROUTINE PLOT_MESH (GC,IGC,NDIM,NN,NUMS,INUMS,NEL)
C
C     This will do a 'quick' 2D plot of a mesh
C     for full plotting, see the DANPLOT package
C
C     22-7-93 Added wait for a keypress for 20 secs.. else exit.
C     .. time I had a DISPL_MESH_PLOT routine too ?? :-)
C      ?-2-98 Now loops facets so 15nt, etc are draw correctly
c             (8nb's were previously dran as 'bow-ties'
c     10-3-98 Now skips -ve and 0 material numbers
c
      INTEGER NUMS(INUMS,*)   , IRESX,IRESY
      REAL GC(IGC,*)
      INTEGER NUM (32)           ! nodes of an element
     &       ,NUM2 (32)           ! nodes on a facet- hence daughter elem

c------------------------- statement functions -------------------------
c.. note whether we want to reverse the y-axis here or (better) in the drawing
c   routine DRAW_A_LINE itself.
c If 3d maybe I want to project the 'z' a little ?
c   ie. we add to x and y a bit of z   ie. (z-zmin)/(zmax-xmin)
      fx(i) =           IRESX * (FACT+R*(GC(1,I)-XMIN) /DATAX)
c              +iresx/50.*(gc(3,i)-zmin)/(zmax-zmin)
      fy(i) =   IRESY - IRESX * (FACT+R*(GC(2,I)-YMIN) /DATAX) 
c              -iresx/50.*.7*(gc(3,i)-zmin)/(zmax-zmin)
c     fy(i) =           IRESX * (FACT+R*(GC(2,I)-YMIN) /DATAX) 

c------------------------ set graphics mode size -----------------------
c.. moved prompt to 'INTO_VGA' itself 'cos in Win311/Win95/X11 we
C   just open another window to work in.
c      print*,'press <CR> to plot the mesh (then <CR> again to continue)'
c      read*
       ICOL = 1
      CALL INTO_VGA (ICOL,IRESX,IRESY)
      AR = REAL(IRESX) / REAL(IRESY)

C--------------------- maxima and minima -------------------------------
      XMIN =  1.E37
      XMAX = -1.E37
      YMIN =  1.E37
      YMAX = -1.E37
      ZMIN =  1.E37
      ZMAX = -1.E37
      DO I = 1,NN
        XMAX = MAX (XMAX,GC(1,I))
        XMIN = MIN (XMIN,GC(1,I))
        YMAX = MAX (YMAX,GC(2,I))
        YMIN = MIN (YMIN,GC(2,I))
        IF (NDIM.GE.3) THEN
          ZMAX = MAX (YMAX,GC(3,I))
          ZMIN = MIN (YMIN,GC(3,I))
        ENDIF
      ENDDO
C---------------------- normalize the data -----------------------------
      FACT = 0.05         !- shrink factor
      R = (1.-2.*FACT)    !- resulting size
      DATAX = MAX ((XMAX - XMIN),(YMAX - YMIN)*AR)   !- scale factor (Z?)

C----------------------- plot the mesh ---------------------------------
c.. also maybe 'colour-in' the elements themselves in their material 
c.. colours ?  .. or restore text/grahics screen on exit ?

c------1: old meshod - join all the dots ----
c      DO IEL = 1,NEL
c        NOD = NUMS(INUMS,IEL)      ! ** explicit NUMS() :-( **
c        DO J=1,NOD
c          K = MOD(J,NOD)+1
c          I1 = NUMS(J,IEL)
c          I2 = NUMS(K,IEL)
c         CALL DRAW_A_LINE ( FX(i1),FY(I1),FX(I2),FY(I2),15+3-NOD)
c        ENDDO
c      ENDDO

c------2: new meshod - respect the facets -----
      DO IEL = 1,NEL
        CALL GET_ELEMENT (NUMS,INUMS,IEL, NUM, NOD,NDIME,ITYPE
     &      ,IMAT,IUSER1,IUSER2,IUSER3)
        IF (IMAT.LE.0) GOTO 11        ! patch to not draw invis elemnts
        CALL GET_FACE (NUM,NOD,NDIME,ITYPE
     &                ,NFACES,NUM2,NN_F,NN_FT, 1 )     !- just NFACES
        DO IFACE=1,NFACES         ! loop this element's faces
          CALL GET_FACE (NUM, NOD,NDIME,ITYPE, IFACE,NUM2,NN_F,NN_FT, 2)
          DO J=1,NN_F
             K = MOD(J,NN_F)+1
            I1 = NUM2(J)
            I2 = NUM2(K)
            ICOL = 15+3-min(NOD,10)
            ICOL = 15
c            IF (NDIM.EQ,2)
            CALL DRAW_A_LINE ( FX(I1),FY(I1),FX(I2),FY(I2),ICOL)
c           IF (NDIM.GE,3)
c           CALL DRAW_A_LINE ( FX(I1),FY(I1),FZ(I1),
c                              FX(I2),FY(I2),FZ(i2),15+3-ICOL)
C  cf. filling with colour IMAT
         ENDDO
        ENDDO  !- the (6) facets
  11    CONTINUE
      ENDDO

C------------------- plot the nodes ------------------------------------
      DO I=1,NN
        CALL DRAW_A_LINE (FX(I)-1,FY(I)+1,FX(I)+1,FY(I)-1,12)
      ENDDO

C-----------------------------------------------------------------------
c..... <press any key to continue>
c     read*
      CALL INTO_TEXT()
      RETURN
      END

C-----------------------------------------------------------------------
C   General drawing routine (with arguments)
C-----------------------------------------------------------------------

      SUBROUTINE DRAW_MESH (GC,IGC,NDIM,NN,NUMS,INUMS,NEL)
C
C     General drawing routine (with arguments)
C        c. Dan Kidger  20-9-98
c
      INTEGER NUMS(INUMS,*)   , IRESX,IRESY
      REAL GC(IGC,*)
      INTEGER NUM (32)           ! nodes of an element
     &       ,NUM2 (32)           ! nodes on a facet- hence daughter elem
      character token*80,value*80
      integer str_to_i
      external str_to_i
      real gc_min(5), gc_max(5)

c------------------------- statement functions -------------------------
c.. note whether we want to reverse the y-axis here or (better) in the drawing
c   routine DRAW_A_LINE itself.
c If 3d maybe I want to project the 'z' a little ?
c   ie. we add to x and y a bit of z   ie. (z-zmin)/(zmax-xmin)
      fx(i) =           IRESX * (FACT+R*(GC(1,I)-XMIN) /DATAX)
c              +iresx/50.*(gc(3,i)-zmin)/(zmax-zmin)
      fy(i) =   IRESY - IRESX * (FACT+R*(GC(2,I)-YMIN) /DATAX) 
c              -iresx/50.*.7*(gc(3,i)-zmin)/(zmax-zmin)
c     fy(i) =           IRESX * (FACT+R*(GC(2,I)-YMIN) /DATAX) 

      icolb = -1             !- no special background
      icole = 15             !- default is white lines
      icolf = -1             !- default is no fill
      icoln = 12             !-  node colour
      icolnn= -1             !-  node number colour


c--------- 1: parse tokens ---------
      CALL GET_KEYWORD (-123,IDUMMY, TOKEN)        !- skip *DRAW_MESH
      do itoken=1,25
      CALL GET_KEYWORD (-123,IDUMMY, TOKEN)        !- get option
      if (token.eq.' ') goto 12                       ! - all done
      CALL GET_KEYWORD (-123,IDUMMY, VALUE)        !- get its value
      if (value.eq.' ') goto 12                       ! - all done

      IF (TOKEN.EQ.'BGCOLOR') then
        ibcol = str_to_i (value,ifail)
      ELSEIF (TOKEN.EQ.'LINECOLOR') then
        icole = str_to_i (value,ifail)
      ELSEIF (TOKEN.EQ.'NODECOLOR') then
        icoln = str_to_i (value,ifail)
      ELSEIF (TOKEN.EQ.'NODENCOLOR') then
        icolnn = str_to_i (value,ifail)

      ELSE
      ENDIF
      enddo
   12 continue
c------------------------ set graphics mode size -----------------------
c.. moved prompt to 'INTO_VGA' itself 'cos in Win311/Win95/X11 we
C   just open another window to work in.
c      print*,'press <CR> to plot the mesh (then <CR> again to continue)'
c      read*
       ICOL = 1                          !- background colour
c-- need to be able to select the resolution? eg 800x600
      CALL INTO_VGA (ICOL,IRESX,IRESY)
      AR = REAL(IRESX) / REAL(IRESY)

C--------------------- maxima and minima -------------------------------
      CALL GET_MESH_RANGE (GC,IGC, NN,NDIM, GC_MIN,GC_MAX,DIAG)
      XMIN =  gc_min(1)
      XMAX =  gc_max(1)
      YMIN =  gc_min(2)
      YMAX =  gc_max(2)
      ZMIN =  gc_min(3)
      ZMAX =  gc_max(3)
C---------------------- normalize the data -----------------------------
      FACT = 0.05         !- shrink factor
      R = (1.-2.*FACT)    !- resulting size
      DATAX = MAX ((XMAX - XMIN),(YMAX - YMIN)*AR)   !- scale factor (Z?)

C----------------------- plot the mesh ---------------------------------
c.. also maybe 'colour-in' the elements themselves in their material 
c.. colours ?  .. or restore text/grahics screen on exit ?

c------2: new method - respect the facets ------
      DO IEL = 1,NEL
        CALL GET_ELEMENT (NUMS,INUMS,IEL, NUM, NOD,NDIME,ITYPE
     &      ,IMAT,IUSER1,IUSER2,IUSER3)
        IF (IMAT.LE.0) GOTO 11        ! patch to not draw invis elemnts
        CALL GET_FACE (NUM,NOD,NDIME,ITYPE
     &                ,NFACES,NUM2,NN_F,NN_FT, 1 )     !- just NFACES
        DO IFACE=1,NFACES         ! loop this element's faces
          CALL GET_FACE (NUM, NOD,NDIME,ITYPE, IFACE,NUM2,NN_F,NN_FT, 2)
          DO J=1,NN_F
             K = MOD(J,NN_F)+1
            I1 = NUM2(J)
            I2 = NUM2(K)
c           ICOL = 15+3-min(NOD,10)
            ICOL = icole
c            IF (NDIM.EQ,2)
            if (icole.ge.0)
     &      CALL DRAW_A_LINE ( FX(I1),FY(I1),FX(I2),FY(I2),ICOL)
c           IF (NDIM.GE,3)
c           CALL DRAW_A_LINE ( FX(I1),FY(I1),FZ(I1),
c                              FX(I2),FY(I2),FZ(i2),15+3-ICOL)
C  cf. filling with colour IMAT
         ENDDO
        ENDDO  !- the (6) facets
  11    CONTINUE
      ENDDO

C------------ plot the nodes ------------
      if (icoln.ge.0) then
        DO I=1,NN
          CALL DRAW_A_LINE (FX(I)-1,FY(I)+1,FX(I)+1,FY(I)-1,icoln)
        ENDDO
      endif
C------------ plot the node numbers ------------
      if (icolnn.ge.0) then    !- (need a text function)
        DO I=1,NN
          CALL DRAW_A_LINE (FX(I)-1,FY(I)-1,FX(I)+1,FY(I)+1,icolnn)
        ENDDO
      endif

C-----------------------------------------------------------------------
c..... <press any key to continue>
c     read*
      CALL INTO_TEXT()
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE WR_MESH_PS (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL)
C  
C     This writes out the mesh as a Postscript plot
C     the upper plot is the mesh with node numbers
C     the lower plot is the shaded elements with element numbers.
C        Dan Kidger   12-9-96
C
c  .. for postscript the simplex is:
c     write headers and macros,
c      - find the BB and write (probaly scale the imags to A4 ?
c      loop and draw edged polygons in 5% gray for each element
c          (should I use get_face .. for 15nt's etc. ?
c       should I colour each IMAT in a random# * 10% gray (say). :-)
c       ( so need to save each IMAT ?). or better get_imat_max
c         hence a linear set of grays. (2%->10% gray)
c         really this just like my QP_PS (hence replace it?)



      REAL GC(IGC,*)
      INTEGER NUMS(INUMS,*), IO,  NUM (32)


c-------------------- go into graphics mode ----------------------------
      RESX = 8.*72.     !- A4 paper size ?
      RESY = 11.*72.
      AR = RESX / RESY
C--------------------- maxima and minima -------------------------------
      XMIN =  1.E37            !- or simply MAXVAl and MINVAL in F90
      XMAX = -1.E37
      YMIN =  1.E37
      YMAX = -1.E37
      DO I = 1,NN
        XMAX = MAX (XMAX,GC(1,I))
        XMIN = MIN (XMIN,GC(1,I))
        YMAX = MAX (YMAX,GC(2,I))
        YMIN = MIN (YMIN,GC(2,I))
      ENDDO

C-------------------- write the Postscript headers ---------------------
c     OPEN (IO,FILE='PLOTFILE.PS')
      WRITE(IO,'(A)')  '%!PS-Adobe-2.0 EPSF-1.2'
      WRITE(IO,'(4A)') '%%Document title: '//'DANFE mesh file'
c      WRITE(IO,'(A,4I6)') 
c     &  '%%BoundingBox:', res(4),res(5), res(1)+res(4),res(2)+res(5)   
      WRITE(IO,'(A)') 
C    &    ' gsave',                                  !- old transformation
     &    ' 0 setgray  .31 setlinewidth'             !- linewidth (was .3)

c     CALL WRITE_PS_MACROS (IO)   !- where are these ??
      WRITE(IO,'(A)') '%%BeginProlog' 
     &,'/sc { setrgbcolor } def' 
     &,'/sf { findfont exch scalefont setfont} def' 
     &,'/Helv { /Helvetica sf} def' 
     &,'/dl { newpath 3 1 roll moveto {lineto} repeat stroke } def' 
     &,'/dp { newpath 3 1 roll moveto {lineto} repeat closepath'//
     &    ' stroke } def' 
     &,'/fp { newpath 3 1 roll moveto {lineto} repeat closepath'//
     &    ' fill } def' 
     &,'/dc { newpath 0 360 arc stroke } def'             !- but diameter ??
c    &,'/fc { newpath 0 360 arc fill } def'
     &,'/fc { newpath pop 4 2 roll 0 360 arc fill } def'

c    &,'/slen {stringwidth pop 0 exch sub 0 } def'        !- -ve text-length
     &,'/dt  { moveto show } def'                    !- text-LH
     &,'/dtc { moveto dup stringwidth pop 2 div'//   !- text-centre
     &    ' 0 exch sub 0 rmoveto show } def'
     &,'/dtr { moveto dup stringwidth pop'//         !- text-right
     &    ' 0 exch sub 0 rmoveto show } def'
     &   ,'%%EndProlog'

C---------------------- normalize the data -----------------------------
      FACT = 0.15         !- shrink factor
      R = (1.-2.*FACT)    !- resulting size

      DATAX = MAX ((XMAX - XMIN),(YMAX - YMIN)*AR)   !- scale factor

c.. I would rather write out my own scalings as I go along
      DO I = 1,NN
        GC(1,I) =        RESX * (FACT+R*(GC(1,I)-XMIN) /DATAX)
        GC(2,I) =        RESX * (FACT+R*(GC(2,I)-YMIN) /DATAX) 
      ENDDO 

C----------------------- plot the mesh ---------------------------------
      DO IEL = 1,NEL
        CALL GET_ELEMENT
     &  (NUMS,INUMS,iEL, NUM, NOD,NDIME,ITYPE, IMAT,I1,i2,i3)

        DO J=1,NOD              !- assemble a polygon.
          K = MOD(J,NOD)+1
          WRITE (IO,'(99G13.4)') GC(1,NUM(J)), GC(2,NUM(J))
        ENDDO
        WRITE (IO,'(i5,A)')   nod-1, ' .97 setgray fp'

        DO J=1,NOD              !- assemble a polygon.
          K = MOD(J,NOD)+1
          WRITE (IO,'(99G13.4)') GC(1,NUM(J)), GC(2,NUM(J))
        ENDDO
        WRITE (IO,'(i5,A)')   nod-1, ' .0 setgray dp'

      ENDDO   !- loop elemnts

C------------------- plot the nodes ------------------------------------
c      DO I=1,NN
c        CALL FILL_ELLIPSE@ (INT(GC(1,I)),INT(GC(2,I)),1,1,12)
c      ENDDO
C-----------------------------------------------------------------------

      WRITE (IO,'(a)') 'showpage'
c.. should I close the file here ?

      WRITE(*,'(A,I4,A)')  '>> ',NEL,' elements written'
      END





c-----------------------------------------------------------------------
C-----------------------------------------------------------------------       
c   These subroutines are for reading in various data-file formats
c    .. and also includes GET_MESH_RANGE to return the coords of the 
c    .. bounding-cube
c    READ_NODES and READ_ELEMS are really obsolete as *IMPORT_PL handles these
c    and *IMPORT_OFF can read OFF files

c   READ.FOR is pretty much obsolete now !
c     .. the only bit I now use is READ_DISPS and that only for .PL files

C-----------------------------------------------------------------------       
C-----------------------------------------------------------------------
      SUBROUTINE READ_NODES (IO,IDT,NN,NDIM,GCOORD,IGCRD,INF,NEL)
c
c  this reads in the nodal co-ordinates for various file formats
c     IDT=1 (my .PL), =2 (OFF) , =3 (D.Ho's)

      INTEGER  INF
      REAL GCOORD(IGCRD,INF)
      IF (IDT.eq.1) THEN
C------------------- Dan's .PL format ----------------------------------
        READ (IO,*) NDIM
        READ (IO,*) NN_
        NN = 0
        DO I=1,NN_  ! nicer to have a 'loop-until-error' structure 
          READ(IO,*,ERR=99) II,(GCOORD(J,II),J=1,NDIM)
          NN =  MAX (II,NN)
        ENDDO
   99   CONTINUE
C------------------ 'Object File Format' -------------------------------
      ELSEIF (IDT.eq.2) THEN
        NDIM  = 3       !---  ie. 3D
        READ (IO,*) NN, NEL, NEDGES          ! NEL needs storing !
        READ (IO,*) ((GCOORD(J,I),J=1,NDIM), I=1,NN)
C---------------------- 'David Ho's Format -----------------------------
      ELSEIF (IDT.eq.3) THEN
c       NDIM  = 3
        READ (IO,*)  NDIM
        READ (IO,*)  NN
        READ (IO,*) (II,(GCOORD(J,II),J=1,NDIM),I=1,NN)
      ELSE
        PRINT*,'*** WARNING: data_type=',IDT,' unknown (READ_NODES)'
        NN = 0
      ENDIF
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE READ_ELEMS (IO,IDT,NEL,NUMS,INUMS,MEL,NDIM)
c
c     this reads in the 'elements' into NUMS for different file formats
c     IDT=1 (my .PL), =2 (OFF) , =3 (D.Ho's)
c     ** fixed a IMAT bug 9-11-93 !
c     YUK! better to handle each file format seperately
c
c     INTEGER  MEL
      INTEGER NUMS(INUMS,MEL), NUM(32)
C------------------- Dan's .PL format ----------------------------------
      IF (IDT.eq.1) THEN
        READ (IO,*) NEL_
        NEL = 0
        DO I = 1,NEL_
          READ(IO,*,ERR=99) II,NOD, (NUM(J),J=1,NOD), IMAT
        NDIME = NDIM         ! ie. same as the geometry :-(
        ITYPE = 1                ! default = 1
C                               ... (careful with NDIME)
          CALL PUT_ELEMENT 
     &    (NUMS,INUMS,II, NUM, NOD,NDIME,ITYPE, IMAT,0,0,0)
          NEL = MAX (II,NEL)
        ENDDO
C------------------ 'Object File Format' -------------------------------
      ELSEIF (IDT.eq.2) THEN 
        NDIME = 2       !      with 2D 'polygon' elements
        ITYPE = 9       !      a 'polygonal' element
        DO II=1,NEL
          READ(IO,*) NOD, (NUM(J),J=1,NOD)
          IMAT = NOD       !- set IMAT == # nodes per elem
          ITYPE = 9        !(ie. just a polygon)  (but 3nt & 4nq's are OK)
          CALL PUT_ELEMENT 
     &    (NUMS,INUMS,II, NUM, NOD,NDIME,ITYPE, IMAT,0,0,0)
        ENDDO
C---------------------- 'David Ho's Format -----------------------------
c  *obsolete* surely
c      ELSEIF (IDT.eq.3) THEN
c        ITYPE = 1            ! default = 1
c        READ (IO,*) NEL
c        DO I = 1,NEL
c          NOD = 14
c          READ(IO,*) II, (NUMS(J+1,II),J=1,NOD)
c          NUMS(1,II)     = NOD   ! #nodes per element
c        ENDDO
c        READ(IO,*) (NUMS(2+NOD,II),II=1,NEL)        ! material types
      ELSE
        PRINT*,'*** WARNING: data_type=',IDT,' unknown (READ_ELEMS)'
        NN = 0
      ENDIF
      RETURN
  99  STOP 'Error in reading Elements in DANPLOT format'
      END

C-----------------------------------------------------------------------
      SUBROUTINE READ_LOADS (IO,IDT,NLDS,GDISPS,IGD,IGDISPS,NODOF,NN)
c
c     this reads in the 'displacements' for different file formats
c     IDT=1 (my .PL), =2 (OFF) , =3 (D.Ho's)
c     It reads multiple load-steps and stores them in a data structure.
c
c     .. I think that I can safely assume that this routine is now 
c      obsolete ?
c      *NO* this is called by DANPLOT
c
      INTEGER IGDISPS,IBASE
      REAL GDISPS(IGD,IGDISPS)
      CHARACTER LINE*80

C------------------------- Dan's .PL format ----------------------------
      IF (IDT.eq.1) THEN    ! 
      DO NLDS=1,9999                 !- *NOT* just 1->99 !
        IBASE = NN * (NLDS-1) 
        IF (IBASE+NN.GT.IGDISPS) THEN
          PRINT*,'*** TOO Many Load steps.. only',NLDS-1,' used <CR>'
          read*
          GOTO 123
        ENDIF
        READ(IO,'(A)',end=123) LINE
        WRITE(*,'(a,i3,a,a)') 'load step >',nlds,' <', LINE(1:50)
        DO I=1,NN
          DO J=1,3
            GDISPS(J,IBASE+I) = 123.e-30  ! set the disps to zero
          ENDDO
        ENDDO

        READ (IO,*,ERR=123) NN_
        DO I=1,NN_
c... next line change 14-07-92 !
c         READ (IO,*,ERR=123) II,dummy,(GDISPS(J,IBASE+II),J=1,NODOF)
          READ (IO,*,ERR=123) II      ,(GDISPS(J,IBASE+II),J=1,NODOF)
        ENDDO
      ENDDO
  123 NLDS=NLDS-1
      print*, ' Total No. of Load steps = ',NLDS,'           '
C------------------ 'Object File Format' -------------------------------
c... ie. no-need to ever call this
      ELSEIF (IDT.eq.2.or.IDT.eq.4) THEN
        print*,'** ignoring disps for ''OFF'' format'
        NLDS = 0
C---------------------- 'David Ho's Format -----------------------------
c   *obsolete* surely
c      ELSEIF (IDT.eq.3) THEN
c        NODOF = 3      !--- always in 3D
c        DO NLDS=1,99
c        IBASE = NN * (NLDS-1) 
c        IF (IBASE+NN.GT.IGDISPS)THEN
c          PRINT*,'*** TOO Many Load steps.. only',NLDS-1,' used'
c          GOTO 99
c        ENDIF
c        READ(IO,'(A)',ERR=99) LINE
c        WRITE(*,'(a,i3,a,a)') 'load step >',nlds,' <', LINE(1:50)
c        DO I=1,NN
c          DO J=1,3
c            GDISPS(J,IBASE+I) = 123.e-30  ! set the disps to zero
c          ENDDO
c        ENDDO
c        DO I=1,NN
c          READ (IO,*) II,(GDISPS(J,IBASE+II),J=1,NODOF)
c        ENDDO
c      ENDDO
c  99 CONTINUE
      NLDS = NLDS-1
      print*, ' Total No. of Load steps = ',NLDS,'           '
      LD = NLDS
      ELSE
        PRINT*,'*** WARNING: data_type=',IDT,' unknown (READ_ELEMS)'
        NLDS = 0
      ENDIF
      RETURN
      END

C-----------------------------------------------------------------------
      SUBROUTINE R_DISPS (IO,DISPS_TOT,MDF,IDISPS, NODOF,NN)
c
c     This reads in the 'displacements' as a generic INode,Dx,Dy, (Dz)
C       .. eg my .PL format (and BOVIDA too)
C       DJK  revised: 7-12-96
C     Notes:
c     - If in 3D then the z-terms are naturaly zapped. (ie. always 3D)
C     - To store multiple load-cases multi-call this file with a pseudo
C       DISPS_TOT (1,NN+ISTEP+1) style syntax.
C
c     INTEGER IGDISPS,IBASE
      REAL DISPS_TOT(MDF,IDISPS), DXYZ(5)
c     CHARACTER LINE*80

      DO J=1,3        !- make sure we will will all 3 columns.
        DXYZ(J) = 0.
      ENDDO
                                            
c... loop nodes .. skip lines that look like comments
      DO I=1,NN
   1     READ (IO,*,IOSTAT=IOS) INODE,(DXYZ(j),j=1,nodof)
         CALL IN_TEST(IO,IOS,*1,*999)
         DO J=1,3
            DISPS_TOT(J,I) = DXYZ(J)
         ENDDO
      ENDDO

c.. optional print-out ?
  999 WRITE(*,'(A,I4,A)')  '>> ',NN,' nodal disps read'

      RETURN 
      END

C-----------------------------------------------------------------------
      SUBROUTINE R_BOOK59 (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL)
c    &   ,DISPS_TOT,MDF,IDISPS, NODOF
c    &   ,STRESSES,MSTRESSES,lstress, PSTRESS)
c
c    This reads the results file from Program 5.9 from S+G (3rd ed.)
c
      REAL    GC (IGC,*)  
      INTEGER NUMS (INUMS,*), IO, NUM(32)
c      REAL STRESSES (MSTRESSES), str_el(99)
c      INTEGER PSTRESS (*)

      CHARACTER line*255

c---- 1: Search for the 'Global Coords' table
      do i=1, 999
        READ (IO,'(A)',err=91) line
        IF (LINE.eq.'Global coordinates') goto 1
      enddo
   91 stop 'Nodal Coordinates not found'

    1 continue

c---- 2: read the nodal coords
      DO I=1, 99999
        READ (IO,'(A)') line
        IF (LINE(1:4).ne.'Node') goto 11
        IF (I.EQ.1) THEN
   12     CALL COUNT_TOKENS (LINE, NTOKS,', =')
          IEND=0
          CALL GET_NEXT_TOKEN (LINE, IBEG,IEND, ', ')
          IF (LINE(IBEG:IEND).NE.'1') GOTO 12  !- start node not found
          NDIM = NTOKS-2                   !- skip 'Node', and INODE
          print*,'>> NDIM=',NDIM
        ENDIF
        READ (line(7:),*) inode, (gc(j,inode),j=1,ndim)
      ENDDO
   11 NN = I-1
      print*,'>> NN=',NN


c---- 3: read the element steering
c (note we have lost the information about IMAT and NOD
c     NOD=9   !- hack
      IMAT=1
      IF (LINE.ne.'Global node numbers') stop 'elem steering not found'

      DO IEL=1, 99999
c       print*,'iel=',iel
        READ (IO,'(A)') line
        IF (LINE(1:7).ne.'Element') goto 22
        IF (IEL.EQ.1) THEN
   13     CALL COUNT_TOKENS (LINE, NTOKS,', =')
          IEND=0
          CALL GET_NEXT_TOKEN (LINE, IBEG,IEND, ', ')
          IF (LINE(IBEG:IEND).NE.'1') GOTO 13  !- start element not found
          NOD = NTOKS-2                   !- miss 'Node', and inode
          print*,'>> NOD=',NOD
        ENDIF
        READ (line(10:),*) ii, (num(j),j=1,nod)
        NDIME = NDIM
        ITYPE = 1        
        CALL PUT_ELEMENT 
     &  (NUMS,INUMS,II, NUM, NOD,NDIME,ITYPE, IMAT,0,0,0)

      ENDDO
   22 NEL = IEL-1
      print*,'>> NEL=',NEL
 
c.. now can read the DISPS generaicaly, and also read the stresses too
c---- 1: Search for the 'Nodal Disps' table
      do i=1, 999
        READ (IO,'(A)',err=92) line
        IF (LINE(1:14).eq.'The nodal disp') goto 2
      enddo
   92 call myerror (1,'Nodal Displacements not found')

    2 continue

      return
      end
c-----------------------------------------------------------------------
c     SUBROUTINE R_STRESSES_P59
c---- 5: read the gauss point stresses
C  ** not yet finished **
c      IH =((NODOF+1)*NODOF)/2
c      READ (IO,'(A)') line
c     IF (LINE(1:15).NE.'The Gauss point') stop 'stresses not found'

c      DO IEL=1,NEL
c.. but how do I know NGP ?
c        ibase=0
c        DO I=1,99  !(NGP)
c          READ (IO,'(A)') line
c          IF (LINE.NE.'Point') goto 33
c          READ (IO,*) (STR_EL(ibase+JJ),JJ=1,IH)
c          IBASE = IBASE + IH
c        ENDDO

c   33   NGP = I-1
c        CALL PUT_STRESS (STRESSES,PSTRESS(IEL), STR_EL,NGP,IH )
c      ENDDO   !- loop elements

C      RETURN
C      END

C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
c-----------------------------------------------------------------------
      SUBROUTINE GET_MESHFILE_TYPE (FILE, EXT)
c
c     This returns the 'type' of the given file as a code
c     usualy this is simply the 3-letter suffix (eg .DXF)
c     sometimes it is munged, eg 'MESHSM1' gives EXT='BOV'
c       DJK 6-12-96
c
      IMPLICIT NONE
      CHARACTER FILE*(*), EXT*(*)

c------ 1: default is to use the file suffix ---------
c.. this fails if their is no extension or the file begins with ./
      EXT = FILE (INDEX(FILE,'.')+1:)
c     should I force to uppercase?

c------ 2: some filetypes are based on the file *root* name ------
      IF (FILE .eq.'MESHSM1') ext ='BOV'   !- Tierra Amarda's BOPRE
      IF (FILE .eq.'p59.dat') ext ='P59'  !- S+G's Book5.9 (Jan 97)
      IF (FILE .eq.'p59.res') ext ='P59'  !- S+G's Book5.9 (Jan 97)
      IF (FILE .eq.'p68.dat') ext ='P68'  !- S+G's Book6.8 (Jan 97)
      IF (FILE .eq.'p68.res') ext ='P68'  !- S+G's Book6.8 (Jan 97)

c.. hmm next should also test for the 2-digit code (eg RESBO12)
c   AND only if no file -extension
      IF (FILE(1:5) .eq.'RESBO') ext ='BO2'  

c------ 3: some need to test for 'magic cookie's in the file header ----
c .. Postscript %!PS or GIF89a
      
c------ 4: maybe the directory name is useful (eg cgi-bin?)------

      END

C-----------------------------------------------------------------------
      SUBROUTINE WR_MESH_SG3 (IO,GC,IGC,NDIM,NN,NUMS,INUMS,NEL, EXT)
C
C     This writes out the mesh in 'Smith and Griffiths v3' formats
C     in particular P59 and P68
C      Dan Kidger 17-1-97 
C
      REAL GC(IGC,*)
      INTEGER NUMS(INUMS,*), IO
      CHARACTER STRING*8, TYP*1, ELEMENT*15, EXT*3
      INTEGER NUM(32), BUFFER(20)

c---------- 1: what sort of elements are they ?
      iel = 1
      CALL GET_ELEMENT (NUMS,INUMS,IEL, NUM, NOD,NDIME,ITYPE
     &    ,IMAT,IUSER1,IUSER2,IUSER3)
      CALL NUM_TO_CODE (NDIME,NOD,ITYPE, STRING)
      typ=STRING(4:4)
      ELEMENT = 'element?'
      IF (typ.eq.'l') ELEMENT = 'line'
      IF (typ.eq.'t') ELEMENT = 'triangle'
      iF (typ.eq.'q') ELEMENT = 'quadrilateral'
      IF (typ.eq.'p') ELEMENT = 'tetrahedron'  
      IF (typ.eq.'b') ELEMENT = 'hexahedron'
      IF (typ.eq.'h') ELEMENT = 'hypercube'      !- wow a 4d element! -

c----- 2: N of Gauss points per element
      CALL GET_DEFAULT_NGP (NOD,NDIME,NGP)
      if (nod.eq.20) ngp=27     !- hack to match the book (I use 14)
      NIP = NGP

c----- 3: N of stress terms per GP --

      NST = ((NDIME+1) * NDIME)/2     !- NDIME=1,2,3,4  -> NST=1,3,6,10
      IF (EXT.eq.'P68'.and. NDIME.eq.2) NST=4  ! for plasticity NST=4 in 2d

c------- 4: how many materials ? ----
c also nice to know how many of each number
c.. so scan for IMAT=1, thence 2 etc., EXIT when all done
c.. a better method is to mantain a stack ('bag') of remaining mats
c   and so check-off the imat at the head of teh stack each time.
      NMATS = 0
      NEL_DONE = 0
      imat_max=0
      DO IMAT2=1,999
        IC = 0 
        DO IEL=1,NEL
          CALL GET_EL_IMAT (NUMS,INUMS,IEL, IMAT)
          IF (IMAT.EQ. IMAT2 ) then
            IC = IC + 1
            NEL_DONE = NEL_DONE+1
          ENDIF
        ENDDO
        IF (IC.GT.0) THEN      !- some of this material has been found
c- can record how many of this too - maybe store in PRPTAB ?
          NMATS= NMATS+1   
          imat_max= imat2
          IF (NEL_DONE.EQ.NEL) GOTO 22
        ENDIF
      ENDDO   !- loop materials
  22  CONTINUE    !- all done
      IF (NMATS.GT.1)
     & WRITE(*,'(A,I4,A)')  '>> There are',NMATS,' different materials'


c------------------------ p1: 'top -line' -------------------------
      NODOF=NDIM
      WRITE(IO,'(A,A,A,2I7,5I4)') 
     &  '''',ELEMENT,'''', NEL, NN, NIP, NODOF, NOD, NST, NDIM

c----- p1a: line #2
      IF (EXT.eq.'P59') THEN
         WRITE (IO,'(2i4)') 2, IMAT_MAX
      ELSEIF (EXT.eq.'P68') THEN
         INCS=10                  !- #load steps
         TOL = 0.0001             !- plastic converance tol
         LIMIT= 50                !- max #plastic iteration
         WRITE(IO,'(i6,E13.4,3i6)') INCS,TOL, LIMIT, 6, imat_max
      ENDIF

c----- p2: Material properties
      IF (EXT.eq.'P59') THEN
c       DO I=1,NMATS
        DO I=1,IMAT_MAX
          WRITE (IO,'(a)') '1.E6  0.3'    !- E,v 
        ENDDO
      ELSEIF (EXT.eq.'P68') THEN
        DO I=1,IMAT_MAX
c                           E,  v,  phi, c, psi, (rho?)
          WRITE (IO,'(a)') '100. 0.3  30. 0.  0.   -20.'   
       ENDDO
      ENDIF

c----- p3: IMATS for each element (optional)
      IF (NMATS.GT.1) THEN
        IC = 0
        DO IEL=1,NEL
          CALL GET_EL_IMAT (NUMS,INUMS,IEL, IMAT)
          IC = IC + 1
          BUFFER(IC) = IMAT
          IF (IC.EQ.15 .OR. IEL.EQ.NEL) THEN
            WRITE (IO,'(99i4)') (BUFFER(I),I=1,IC)   !- A row of IMATs
            IC = 0
          ENDIF
        ENDDO
      ENDIF

c----- p4: Nodal coords (G_COORD)
c note I can write (say) 3 coord pairs per line if I want to 
      DO I=1,NN
        WRITE(IO,'(3E13.4)') (GC(J,I),J=1,NDIM)
      ENDDO
      WRITE(*,'(A,I4,A)')  '>> ',NN,' nodes written'

c----- p5: Element Steering (G_NUM)
      DO IEL=1,NEL
        CALL GET_ELEMENT (NUMS,INUMS,IEL, NUM, NOD,NDIME,ITYPE
     &    ,IMAT,IUSER1,IUSER2,IUSER3)
        WRITE(IO,'(99I7)') (NUM(J),J=1,NOD)
      ENDDO
      WRITE(*,'(A,I4,A)')  '>> ',NEL,' elements written'
      RETURN
      END

c-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      SUBROUTINE R_DATA_JASPER (io,GC,IGC,NDIM,NN,NUMS,INUMS,NEL)
c
c     This reads in the coords as a generic Cx,Cy,Cz
C         DJK 21-9-98
c  Notes:
c   -  This is designed to read Jasper's CFD code output
c   -  Displacements can likewise be read in.
c   -  Theoretically 4 or more columns of results could be read (eg pressure)
c   -  Currently this is only called direct from DANPLOT; really it 
C      should be bolted onto K_MESH and K_BC
c   -  you can cheat and use this to read the 'z' displacement as the 
c      'z'-coord say. :-)
c   - ? hmm what about allowing reading column K of M to pick out from a 
c       multiple column table ?


C   file format:
c        nxp,nyp,nzp, nskip         , eg. nskip=2
c        // j, 't', 'datafile' //  for j=1 to NDIM (or 'b' for binary)
c       and each file contains NN raw X (y or Z) values

c   Notes on reducing the data set size.
c    eg. skipping alternate points would reduce the data to 1/8
c   so..
c    1. read a complete column of numbers

      REAL GC(IGC,*)
      INTEGER NUMS(INUMS,*), NUM(32)
      CHARACTER FILE*255, type*1
      logical lx,ly,lz

      READ (IO,*, IOSTAT=IOS) nxp1,nyp1,nzp1,nskip
      nn= nxp1*nyp1*nzp1

      ndim=0                     !- start with no dimensions of dataspace?
      DO J=1,3
        READ (IO,*, IOSTAT=IOS) idir, type,file
        if (ios.ne.0) goto 99
        if (type.eq.'b') then
          OPEN (IO+1,FILE=FILE, FORM='UNFORMATTED',IOSTAT=IOS) 
        else
          if (type.ne.'t') call myerror 
     &    (1,'oops - assuming an ascii text file')
          OPEN (IO+1,FILE=FILE, IOSTAT=IOS) 
        endif
        if (ios.ne.0) call myerror (2,'Could not open given data file')
        READ (IO+1,*,IOSTAT=IOS) (GC(idir,i),i=1,nn)
        if (ios.ne.0) call myerror (2,'Could not read data file')
        CLOSE (IO+1)
        WRITE(*,'(A,I4,A)')  '>> ',NN,' nodal values read'
        NDIM = MAX(NDIM,idir)      !- so get the largest :-)

      ENDDO         !- loop z,y,(z) files
   99 continue

c----- massage the data to a smaller size -----
      ip=0
      ic=0
      nzp=0
      do iz=1,nzp1
        lz= mod(iz,nskip).eq.0.or.iz.eq.nzp1
        if (lz) nzp=nzp+1
      nyp=0
      do iy=1,nyp1
        ly= mod(iy,nskip).eq.0.or.iy.eq.nyp1  
        if (ly) nyp=nyp+1
      nxp=0
      do ix=1,nxp1
        lx= mod(ix,nskip).eq.0.or.ix.eq.nxp1
        if (lx) nxp=nxp+1

        ip=ip+1
        if (lx.and.ly.and.lz) then
          ic=ic+1
          do j=1,ndim
            gc(j,ic)=gc(j,ip)
          enddo
        endif

      enddo
      enddo
      enddo
      nn=ic
      print*,'nxp1,nyp1,nzp1=', nxp1,nyp1,nzp1
      print*,'nxp,nyp,nzp=', nxp,nyp,nzp
c---------- create the element steering -----
      NOD   = 8      !- All 4nq's
      NDIME = 3      !-- 8-node bricks
      ITYPE = 1      !-- all are 4nq's

      NXE=NXP-1
      NZE=NYP-1
      NYE=NZP-1
      IEL = 0
      DO IS = 1,NZE
        DO IQ = 1,NYE
          DO IP = 1,NXE            !- so x as the inner loop
          IEL = IEL + 1
          NUM(1) = (IQ-1)*(NXE+1)*(NZE+1)+IS*(NXE+1)+IP
          NUM(2) = NUM(1)-NXE-1
          NUM(3) = NUM(2)+1
          NUM(4) = NUM(1)+1
          NUM(5) = NUM(1)+(NXE+1)*(NZE+1)
          NUM(6) = NUM(5)-NXE-1
          NUM(7) = NUM(6)+1
          NUM(8) = NUM(5)+1
c          do j=1,nod
c            num(j) = num(j) + nn_old
c          enddo
          CALL PUT_ELEMENT 
     &    (NUMS,INUMS,IEL, NUM, NOD,NDIME,ITYPE,IMAT,1,1,1)
        ENDDO
        ENDDO
      ENDDO
      NEL=IEL

          
      RETURN 
      END
C-----------------------------------------------------------------------

