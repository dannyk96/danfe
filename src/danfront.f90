PROGRAM  auto_mesher
!
!   Just a driver to routines to create a mesh of triangles
!    by means of an 'Advancing front' (Peraire et al 1987)
!          - Adaptivity code originaly by Dr. A. Ma 1993?
!          - Restructuring etc. by Dr. D. Kidger 8-2-96 onwards
!
!   27-1-98 renamed DANFRONT.F90 (was advance.f90)
!    USE Amr                  !- the 2 advancing-front entrypoints
     use plotlib
!    IMPLICIT NONE


!.. here we use allocatable arrays, but of 'fixed' size !
     INTEGER,PARAMETER :: NN_MAX=400000, NEL_MAX=400000 !- max mesh sizes
     REAL, ALLOCATABLE    ::  gc (:,:), &      !  new nodal coords
                              gc_orig(:,:)      !    copy of adv.front mesh for animation
     INTEGER, ALLOCATABLE :: nums (:,:)        !- elem steering of the new mesh
     INTEGER, ALLOCATABLE :: ring (:)          !- nodes on the boundary
                                     
!--------------- small arrays ---------------
      CHARACTER :: filenames*1024=' ',&   !- stack of input filenames
                        FILENAME*60 =' ',&!- input datfile name
                        basename*60       !- base for output filenames
!     integer :: nfilenames = 0           !- and count them
      INTEGER   iel,igc, n_poly,ipoly, nring, nn,nel, imat
      integer  :: iters_barycentric
      REAL     T1,T2,T3,T4             ! (timers)
      CHARACTER :: VERSION*20 = 'ver 23/03/15'
      logical :: debug = .false.
      integer :: iverb=1        ,&  !  verbosity  so can increment or decrement from here.
                 igraph=0       ,&  !- 0=no graphics, 1=lines, 2=fill ..
                 ianim=1            !- animate the 2 meshes at the end. :-)

      character :: base_type*12='backward', &    ! which segement to build on
                   outfile_type*5='dp'           !  what sort of output file to write.
      character :: arg*80, arg1*1, arg2*1        ! command line parsing
      integer :: nargs
!     integer :: danfe_argc

!TODO make this a command line option
! = 'backward', 'shortest', 'random','random2','longest'
!       base_type = 'shortest'
!       base_type = 'random2'
!       base_type = 'backward'


!--------- 0: show the program titles ----------
!    nice to have an option to skip the banner too.
      CALL WRITE_DANADVANCE_BANNER (VERSION)

!--------------- parse the command line ------------------
      nargs= iargc()
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
        call getarg(i,arg)
!       print*,i,arg(1:70)
        arg1 = arg(1:1)
        arg2 = arg(2:2)
        if (arg1 /= '-') then       !- not a prefixed option
!  hmm perhaps instead chack that no options *follow* the first filename found
!          if (i /=nargs) then
!            call error ("filename should be the last option")
!            call usage (); stop
!          else
             if (filenames/=' ') arg=','//arg(1:len_trim(arg))
             filenames=filenames(1:len_trim(filenames))//arg(1:len(filename))
             nfilenames = nfilenames+1
!          endif
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
!   for no graphics version -then good to warn that -g will be ignored
!   also we shoudl make -g the default for danfrontg ?
              elseif (arg2=="A") then     !- animation etc. of results
                 ianim = ianim+1
              elseif (arg2=='w') then       !'where' eg. 'random'
                 if (i==nargs) then
                   call usage(); stop
                 endif
                 call getarg(i+1,arg)
                 iskip=1                    ! already read next argument.
                 base_type=arg(1:len(base_type))
              elseif (arg2=='t') then      !- output file type (default is no file!)
                 if (i==nargs) then
                   call usage(); stop
                 endif
                 call getarg(i+1,arg); iskip=1
                 outfile_type=arg(1:len(outfile_type))    
!                select (arg)                 !-  check it is valid type
!                case ('pl'.'dp',..)          !- 
              else
                call error('unknown option:"'//arg2//'"'); stop  
              endif
            enddo
          endif
        endif
        if (debug) write(*,'(i6," :",a)') i,arg
      enddo
      if (filenames ==" ") then
        call usage (); stop
      endif

!---- files for 'dubug' output (obsolete ?) ----
      OPEN (UNIT=29, FILE='advance.log',ACTION='WRITE', IOSTAT=IOSTAT)
      OPEN (UNIT=91, FILE='quality.log',ACTION='WRITE', IOSTAT=IOSTAT)

!---- 1: loop input filenames --------  
      if (iverb>=2) &
      print*, 'looping over', nfilenames,' files'
      files: do ifiles = 1, huge(ifiles)
!        print*,'filenames=', filenames, len_trim(filenames)
        if (filenames==' ') exit      !- all done
        ipos = index(filenames,',')
!          note be careful if ipos is very large (>len(filename))
        if (ipos>=2) then
          filename  = filenames(:ipos-1)
          filenames = filenames(ipos+1:)
        else
          filename = filenames(1:len(filename))
          filenames=' '
        endif
        if (iverb >=1) &
        print*,"Processing '",filename(1:len_trim(filename)),"'"

!--- generate basename for output files ---
        ipos= index(filename,'.',back=.true.)   !- *last* dot in line
        if (ipos>0) then
          basename= filename(1:ipos-1)
        else
          basename= filename
        endif

!--------- 1: open the main data file ----------
      OPEN (UNIT=25, FILE=FILENAME,STATUS='OLD', IOSTAT=IOSTAT)
      if (IOSTAT.ne.0) then
         call error ('File :'// filename(1:len_trim(filename))//' does not exist !'); stop
      endif

!------------- 2: Initialise : Default parameters ------------ 
!.. contrast malloc'ing the arrays once we know the desired sizes or 
!   just creating 'sufficient' size here.


      iters_barycentric=20             ! default=5
!     iters_barycentric=0             ! for no smoothing
!     statistics=1

!-- plotting etc.
! 13-10-02 I think that I should parse the datafile *twice* 
!  first time we get the xyXY range
!    (alternative is to allow an xyXY window to be defined - hence close-ups possible)
!  - also for each polygon we can estimate the total # of triangles - hence a % complete line :-) ! (consider  parallelism too)

!------------- Maximum array sizes ------------ 
!.. NN_max should also be read in as a control
      igc = NN_max                     ! dummy array sizes
      inums = 10
      ALLOCATE ( gc(2,NN_max) )        !*** the new nodal coords ***
      ALLOCATE ( nums(inums,NEL_max))  !- the new element steering
      ALLOCATE ( ring(NN_MAX) )        !--- list of nodes in the front

      nel = 0   ;  nn = 0               ! start with no nodes or elements

!--------------- loop the keywords ------------------
! KEYWORDS:     DO IKEYWORD=1,99999
!        CALL GET_KEYWORD (U1,U0,KEYWORD)
!        CALL PRINT_KEYWORD (IKEYWORD, KEYWORD,.false.)

!------------------------ find the '*keyword' --------------------------
!      CALL GET_SECS (TIME1)
!      FOUND = .TRUE.

!      IF (KEYWORD.EQ.'*EOF') THEN
!        close (u1)             !- always close this input file.
!        EXIT KEYWORDS
!
!--------------------- 3: read the control params ---------------------
!     ELSEIF (KEYWORD=='*CONTROL')
!.. We need to read:        
!      o Density function code: 0=nul,1=?,2=slip circle, 10=bitmap
!      o The bitamp (usualy .PCX format) and its size?
!      o The nominal edge length (unconcentrated)
!      o The density-factor (=max:min edge length)
!.. Bitmaps are read in PCX format but stored internaly in a character array 
! a UNIX-port would need to read the integer*1 format directly ? 
!.. hmm better to read the desep_lo,dsep_ratio first, then the pattern that 
!   differentiates the low and hi-density parts. 

!     ELSEIF (KEYWORD=='*ATTRACTORS')
      call read_attractors (25,iverb)

!======================== Form the new mesh =========================
!      if (igraph>=1)  call open_graphics_window ()    !(do in adv_front itself)

!-------------- loop each 'material' polygon -------------
!.. create nodes around the edges & so form the initial seg()
!   (either real new nodes then *JOIN_COIN_NODES 
!     or re-use the old nodes (so forces them to be coincident)

     read (25,*) n_poly             !- or just to EOF?
     do ipoly= 1,n_poly

     if (iverb>=2)WRITE (*,'(A)')'Create the nodes around the boundary ...'

!--------------------- 4: read the bounding polygon(s) ----------------
!     ELSEIF (KEYWORD=='*GENERATE_RING')
!      maybe I can use this for a structured mesh generator too?
!      'cos we can form lines of nodes, then perhaps can build quads on then
!       to form a tunnel lining perhaps.

!    Polygon Params: barycentric_iter
!      o  The number of polygons to do (often just =1)
!      o  the number of 'lines' in each polygon (usu=4 maybe just 1 if curved)
!      o  the data for each line eg:
!          x icode: 1=simple line, 2=polyline, 3=Arc, 0=explicit nodes.
!          x #coord-pairs that define the (poly)line
!          x the coord-pairs themselves
!
!.. Nicer to use the command-line for data files or stdin?
!.. Consider using a DXF file for input of the bounding regions. ?

!-------- create the boundary nodes -----
!.. Note that the new mesh will have an undetermined number of nodes
!.. that will usually be (say) 2* the old mesh's

      CALL get_secs(t1)                      !- time the process ?
        call build_front (gc,igc,nn, ring,nring, imat,iverb)
!       call read_ring (gc,igc,nn, ring,nring, imat)
      CALL get_secs(t2)                      !- time the process ?
      if (iverb>=2) &
      print*,'<> ',nring,' boundary nodes created.'

!----------------------- sweep around and fill the mesh -----------------
!     ELSEIF (KEYWORD=='*GENERATE') THEN
      if (iverb>=2) WRITE (*,'(A)') '---> Fill the interior by advancing the front.'
      
      CALL get_secs(t3)                      !- time the process ?
      nn_old = nn                 !- ie. how many boundary nodes
      nel_old = nel 
      call advancing_front (igraph,gc,igc,nn ,nums,inums,nel,nel_max ,ring,nring, &
    & imat,base_type,iverb,igraph )
      CALL get_secs(t4)                      !- time the process ?
!- should I record the number of *NEW* elements or just the current total?
      if (iverb>=1) then
        elements_per_sec=0.
        if (t4-t3/=0.) elements_per_sec=real (nel-nel_old)/ (t4-t3)
        write(*,'(a,i9,a,i9,a,f7.2,a, a,f9.1,a)') &
        '<> Created ',nel-nel_old,' elements  (and ',nn-nn_old,' nodes) in', t4-t3,'secs',&
        ' (=',elements_per_sec,') elements/s'
!        print*,'<>', nel-nel_old,' elements and',nn-nn_old,' nodes created'
      endif
!----- show the mesh quality (before smoothing) -----
!----------- 6: mesh-quality checks ---------------
!  check #1 : the nodal-valency 
!     Target is '6' - but has to be >6 for local refinement (>10 is bad!)

!  check #2 : the distribution of internal angles.
!     Should peak at 60 deg. - values >120. are unacceptable?
!     last param is the width of 'bin', 0->180.
      if (iverb>=2) &
      call valency_stats (nn,nums,inums,nel,nn_old+1,nn)
      if (iverb>=3) &
      call angle_stats (gc,igc,nn  ,nums,inums,nel, nn_old+1, nn, 36,5.,-1)

!--------- 5: Adjust the mesh to improve 'skewed' elements -----------
!.. 2 methods:
!      1: Barycemtric: move nodes to the centroid of the surrounding nodes.
!      2: Delauney : throw away the triangles and re-join the dots.

! use a *BARYCENTRIC keyword here - or maybe just assume
!.. 'relax' the internal nodes (effectively Laplace's equation) 
      if (iverb>=2) WRITE (*,'(A)') &
     & '---> Adjust the new elements by smoothing the internal nodal coords ...'
       if (ianim>=1) then
         allocate (gc_orig(2,nn)) ; gc_orig(:,:) = gc(:,1:nn) !- copy the mesh.   
       endif
      call barycentric (gc,igc,nn  ,nums,inums,nel, nn_old+1,nn, &
     &  iters_barycentric,iverb)

!--------------------------------------------------
!  Once the mesh has been smoothed I should really get the opertunity to plot it
!  .. perhaps in a new window so I can compare the two.  
!  .. or even nicer as an animation that linerly interpolates  :-)
!        (it would be great if we could also dynamicaly zoom in)
!  .. I could colour the mesh wrt:
!       - how far the nodes moved
!       - 'quality' e.g. the 3 angles (as three coloured 'kites') 
!     call plot_mesh((gc,igc,nn  ,nums,inums,nel, nn_old+1,nn)
      
!----- 6: show the mesh quality (after smoothing) -----
!    no need to check the nodal-valency - it cant have changed.
!    get is '6' - but has to be >6 for local refinement (>10 is bad!)
      if (iverb>=3) &
      call angle_stats (gc,igc,nn ,nums,inums,nel, nn_old+1, nn, 36,5.,2)

!---------- 7: write timing information ------------
! cf with standard 'time per keyword' reporting (+total runtime)
!      if (iverb.ge.2) then
!        WRITE (*,'(A/,(A45,F7.2,A))') '<> Run timings:', &
!         ' Phase 1: 1D line-segments (rings)       =', t2-t1,' secs'  &
!     &  ,' Phase 2: 2D triangle-fill (Adv. front)  =', t4-t3,' secs'
!!    &  ,' Phase 3: 3D tetrahedron-fill (Adv. fr.) =', t6-t5,' secs'
!      endif
    enddo                      !- loop polygons
   
!----------- 8: Write out the final mesh ----------------
!     ELSEIF (KEYWORD=='*WRITE_DANPLOT_KEY') THEN
      select case (outfile_type)
      case('null')
        ! nothing to do here - (we just wanted a picture or a speed)    
      case ('dp')
        call write_mesh (basename(1:len_trim(basename))//'.dp' &
          ,gc,igc,nn, nums,inums,nel)     !- in DANPLOT format
        if (iverb>=2) &
        WRITE (*,'(A)') &
         '<> Meshfile written as "advance.dp"' &
        ,'      Use Danplot (tm) to view on screen' &
        ,'       or Danmung (tm) to convert to .PL, .DXF, .UCD .PCX etc.'
      case ('pl')
        call write_mesh_pl (basename(1:len_trim(basename))//'.pl' &
         ,gc,igc,nn, nums,inums,nel)     !- in PL format
      case default
         call error ("Invalid output file format")
      end select

!---------- 9: animate the pre and post-smoothed meshes --------
!        print*,"igraph=",igraph," ianim=",inaim
       if (ianim>=2) then
!-  options:
!     1) Just plot the new smoothed mesh, 
!          either in a new window or perhaps on top of a grayed out original
!     2) Animate interpolate 0->1. the old->new mesh
!     3) Colour code with the amount of movement (and/or direction)
!  All in all I think it is better to pass this to Danplot to do this sort of post-processing

         if (igraph>=1 .and. ianim>=2) then
           do iel=1,nel
             call draw_tri (gc_orig(1,nums(1:3,iel)),&
                            gc_orig(2,nums(1:3,iel)), 6,0)  !- in grey
           enddo
         
           do iel=1,nel
             icol=1+ mod(iel,14)
             icol=1
             call draw_tri (gc(1,nums(1:3,iel)),&
                            gc(2,nums(1:3,iel)), icol,-1)  !- in grey
           enddo

         endif
       endif
!-------------------------------------------------------
!      ENDDO         : loop keywords
!-------------------------------------------------------
         close(25)
         deallocate (gc,nums,ring)
         if (allocated(gc_orig)) deallocate (gc_orig)
      enddo files ! loop input files
!-------------------------------------------------------


      call close_graphics_window ()    !(do in adv_front itself)
      WRITE (*,'(A/)') '<> Program completed OK'

      END PROGRAM auto_mesher

!---------------------------------------------------------------------
!-----------------------------------------------------------------------
      SUBROUTINE WRITE_DANADVANCE_BANNER (version)
!
!     Just writes out the masthead of the Danfront Package
!      ANSI is a logical flag - whether to use fancy colours or not.
!       Dan Kidger 16-7-97 
!   13/10/02  changed colours to be better on a white background
!             updated affiliation and contact addresses
!
      implicit none
      CHARACTER VERSION*(20)
!     LOGICAL ANSI

      character  esc,black*7,red*7,green*7,yellow*7,blue*7,magenta*7,cyan*7,white*7, fg*7, &
      reset*4
      esc = char(27)
      black  = esc//'[0;30m'  ;  red    = esc//'[1;31m' ;  green  = esc//'[1;32m'
      yellow = esc//'[1;33m'  ;  blue   = esc//'[1;34m' ;  magenta= esc//'[1;35m'
      cyan   = esc//'[1;36m'  ;  white  = esc//'[1;37m';  reset=esc//'[0m'
      fg = black

      WRITE (*,'(A)') &
 & '+--------------------------------------------------------------------------+', &
 & '   '//blue//'D A N - F R O N T ' //reset//'   2D Unstructurured Mesh Generator', &
 & '                         '//reset//VERSION, &   
 & '               '//red//'Dr. Dan Kidger,'//blue//'  FE Solutions Ltd.,UK ',&
 & '       '//blue//'daniel.kidger@gmail.com'// &
                                & red//'  http://www.github.com/danfe/danfe  '//reset,&
 & '+--------------------------------------------------------------------------+'
      END

!----------------------------------------------------------------------------------------
      subroutine usage()
!
      write (*,'(a)')&
  "Usage:  danfront [-svhgAV] [ -t {pl|dp}] [-w base_type] input file(s)"      
! TODO if no graphics then we should skip -g

!TODO need both usage (one liner) and multi-line help  --help  see below

      end
!----------------------------------------------------------------------------------------
      subroutine help()
!
      write (*,'(a)')&
 "Usage:  danfront [OPTION..]" , &
 "-v  verbose (may be repeated)" ,&
 "(more options need to be added here)"            

      end

!----------------------------------------------------------------------------------------
      subroutine error(string)
!
      character string*(*)
!TODO: Consider teh user of Color here
!TODO: Consider th euse of a windows pop-up here
!TODO what about a warning too  - hence perhaps a severity code
      write (*,'(a)') "error:", string
      end



