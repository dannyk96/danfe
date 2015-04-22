
!--------------------------------------------------------------------------
!                Unstructured-Triangle-Generation Library
!--------------------------------------------------------------------------

!                             Dr. D. J. Kidger
!   School of Engineering, University of Manchester, Manchester, UK, M13 9PL
!
!   * The source of this program is propriatory and should not be distributed *
!      (Portions of this code are c. A.Ma./M.A.Hicks 1995)
!
!   Revisions:
!     8- 3-96 Tidied and restructured by Dan Kidger
!    21- 2-97 Executable released (to my UG F.E. course students)
!     6- 3-97 Started to move SEG to a derived type.
!    16- 7-97 Break up ADVANCING_FRONT into a set of daughter routines
!    26- 7-97 Now can build on any (random) base segment
!    28- 7-97 renaming: eg SEG->FRONT
!    11-10-97 option to build on SHORTEST edge (or RANDOM, FOREWARD, etc.)
!    12-10-97 work on ring-splitting -> by pushing the shorter on a stack.
!    27- 1-98 graphics now fill the triangles 
!    27- 1-98 calc of remaining area so can abort if hemhoraged.


!   This library is driven by DANFRONT.F90
!   which calls in order the following entrypoints in this library:
!   /   READ_BACK_PCX   to get the background bitmap
!   \   STORE_AFRONT_PARAMS just to store dsep_lo , etc.
!    /  BUILD_FRONT to read and store the polyline. (just as a list of nodes)
!    \  ADVANCING_FRONT to build the mesh.
!    -  BARYCENTRIC to juggle the nodes about a bit
!   /   ANGLE_STATS to show the range of internal angles.
!   \   WRITE_MESH to output the mesh un DANPLOT format

!
!     Subroutines:
!     -----------
!     BUILD_FRONT  - creates the boundary nodes by interpolation
!       BUILD_FRONT_EDGE - interpolates nodes along an edge.
!       INTERP_LINE - returns an XY coord of a point on a given line/arc.
!       POLY_TO_RING - puts given XY coords into the front (and GC)
!       NORMALISE_GC - mungs the x,y coords to 0.->1.

!     ADVANCING_FRONT - The main Advancing-Front algorithm
!       DRAW_TRI - simply plots a 3nt with optional fill /edging
!       PLOT_FRONT - plots the current front as a polyline
!
!       RING_TO_FRONT - creates the datastructure of the original front
!       VALIDATE_FRONT - for debug of pointers
!       GET_SEGMENT - returns its nodes, coords, normal vector, etc.
!       FIND_POINT_C - by calling GET_SPACING gets the 'free choice' new node
!       FIND_CLOSE_C - returns the ordered list of 'close' nodes
!       FIND_IDELTA - returns how far away a node is from a given segment.
!       UPDATE_FRONT - once a new triangle has been formed.

!       ROTATE_FRONT - cycles the front, hence a new base piece.
!       GET_MESH_EXTENT - so we can scale an image to fit the screen.
!
!     / READ_AFRONT_PARAMS - just stores 3 params? (cf whole BG mesh) 
!       READ_ATTRACTORS
!     \ GET_SPACING - returns the spacing at the given x,y 
!    /  READ_BACKG_PCX - reads a BITAMP for nodal density calcs. 
!    \  SAMPLE_PIXEL - returns the pixel value at xi,eta. ! 
!
!       BARYCENTRIC - (optionaly) smoothes the nodal spacing by SOR FD. 
!       ANGLE_STATS - (optionaly) computes the range of internal angles.
!     / WRITE_MESH  - saves the mesh in DANPLOT (tm) format 
!     \ WRITE_MESH_PL  - saves the mesh in PL format 
!
!     INDEX_REALS - ranks a real array in ascending order. (eg nodes)
!     INDEXX - ranks a real array in ascending order. (eg nodes)
!
!  MODULE OLD_MESH:
!    ( READ_OLD_MESH - reads in a background mesh and its nodal-density-val 
!     INTO_GRAPHICS - creates a 1024x768 graphics window

!  Notes on 3D generation of the surfaces of a cube:
!    calc the 'best-fit' eqn. of the plane through the ring.
!    so un-transform all nodes to 0.,0.->xd,yd
!    so record the transformation of the plane.
!    mesh-gen as normal
!    then un-transform back again :-)
!

!======================================================================
module plotlib
!
!  Routines for plotting:  including the elements, and the whole front
!      DJK 26-1-98 
!
! private
  integer  :: ixres=1024,iyres=768    !- screen size
  integer  :: ncols     !- screen colour depth
  real    :: ixo=0,iyo=0            !- the model->screen transfromation
  real    :: xmag=1,ymag=1
  integer :: grstate = 0
contains
!-----------------------------------------------------------------------
!     subroutine open_graphics_window (ixpos,iypos,iwidth,iheight)
      subroutine open_graphics_window ()
!
!     This creates a graphics windows (of the given size)
!
      implicit none
!         best if I can let the user choose
!         A config file, or environmental variable is nicest                   
#ifdef PGPLOT
      real   :: Screen_width=1024.   !- width of image in pixels
      real*4 :: x,y,x1,x2,y1,x1i,x2i,zero=0.  !, one=1.
      real*4 :: pgopen,y2, width,aspect
#endif
!     real*4 :: x,y,x1,y1,y2,x2,x1,xli,
!     real*4 :: VPX1, VPX2, VPY1, VPY2, xsep,ysep
!     real*4 :: D

      if (grstate==0) then
#ifdef PGPLOT
      if (PGOPEN("/XWIN").le.0) then
        print*,'failed to open PGPLOT''s X11 driver, exiting..'
        grstate=0
        return
      endif
#endif
      grstate=1 


#ifdef PGPLOT
!---- create a window of any desired size
!  (note also using the aspect ratio of the mesh here :-)
      call   PGQVSZ (3, X1, X2, Y1, Y2)          !- orig in pixels
      call   PGQVSZ (1,x1i,X2i, Y1, Y2)          !- orig in inches
      width=screen_width/X2 *X2i                 ! hence specify mine in pixels. 
      aspect = 1./sqrt(2.)
      call PGPAP (width,aspect)
!  ? but how can we set the screen position of the pane ?

!     call PGENV (0., 1.*idev_x, 0., 1.*idev_y,1,0)
!     call PGSVP (zero,one,zero,one)     !- set full viewport
!      x1=-0.1; y1=-0.1; x2=1.1; y2=1.1    ! sadly this does remove teh inner black frame :-(
       x1=0.; y1=0.; x2=.99; y2=.99        ! (exactly one causes the topmost line to be clipped
      call PGSVP (x1,x2,y1,y2)     !- set full viewport

!--- perhaps I can use this code fragment to use the whole window and so 
!  avoid the black strip around the edge.

!      CALL PGQVP(1, VPX1, VPX2, VPY1, VPY2)
!      print*,' viewport (xXyY)=', VPX1, VPX2, VPY1, VPY2
!      D = MIN(VPX2-VPX1, VPY2-VPY1)/40.0
!      VPX1 = 0.-.5!VPX1 + 5.0*D   * -0.
!!      VPX2 = 1..!VPX2 - 2.0*D   * -0.
!      VPY1 = 0.-.5!VPY1 + 8.0*D   * -0.
!!      VPY2 = 1.!VPY2 - 2.0*D   * -0.
!      CALL PGVSIZ(VPX1, VPX2, VPY1, VPY2)

!      call PGWNAD(zero,one,zero,one)
!       xsep=.1; ysep=.1
!       CALL PGBOX('ga',xsep,5,'gn',ysep,5)

      x=ixres; y=iyres
      call PGSWIN (zero,x,zero,y)  !- and set internals to 800x600
!-- enable buffering of output - so saty only one screen refresh per sec.
!      call PGBBUF()
#endif
      
      endif
      end subroutine open_graphics_window
!-----------------------------------------------------------------------
      subroutine close_graphics_window ()
!
!     This closes the current graphics window
!     notes:
!       we can have nuliple devices open at once - after pgclos, we ned to call PGSLCT
!      (query with PGQID, use with PGBEG(?)
!
!  also for biitmap reading : see PGQITF, PGIMAG, PGGRAY, and PGWEDG
!  for viewports PGQVP returns xyXY
!       PGQVSZ retyns xyXY of the view surface - hence use PGPAP to change
!
!     real*4 :: dx=2.;dy=2.; t=0.1*1000
      if (grstate /=0) then
#ifdef PGPLOT
        do i=1,2000
!          call pgscrl(dx,dy)
!          call sleepqq(t)   !- in 1000ths of a sec.(ifc routine) 
        enddo
        print*,"press <enter> to exit."
!        read*
        call PGCLOS()
#endif
      endif
      end subroutine close_graphics_window
!-----------------------------------------------------------------------
!      subroutine plot_tri (x,y, icol)
!      /* Just plots the given 3 points in x(), y() as a triangle */
!      1/ Note the need for an explicit interface 
!           cos ixo, etc is in a module (common?)
!      2/ Maybe support filled triangles too? so colour with DSEP
!
!      implicit none
!      real,intent(in) :: x(3),y(3)
!      integer i,j,icol
!       do i=1,3
!         x(i)= (ixo+xmag*x(i)) ; y(i)=iyo+ymag*y(i)
!       enddo
!         call PGSCI (icol)
!         call PGLINE (3,X,Y)!
!
!      do i=1,3
!        j=mod (i,3)+1
!        call draw_line@ (nint, nint(iyo+ymag*y(i)), &
!    &                    nint(ixo+xmag*x(j)), nint(iyo+ymag*y(j)),  icol)
!      enddo
!      end subroutine plot_tri

!-----------------------------------------------------------------------
      subroutine draw_tri (x,y, icol_line,icol_fill)
!      /* Just plots the given 3 points in x(), y() as a filled triangle */
!      1/ Note the need for an explicit interface 
!           cos ixo, etc is in common?
!      2/ Note the technicality that really only 2 lines need be drawn
!         since the base-line must always exist beforehand
!
      implicit none
      real,intent(in) :: x(3),y(3)
!     real :: t,t_lastupdate = -999.
      
      integer i,icol_line
      integer :: icol_fill,icol_line2, icol_fill2
!     integer :: handle, ifail
      real*4 :: xs(4),ys(4)
      if (grstate==0) return

      do i=1,3              !- transfrom the coords to the screen
        xs(i)= ixo+xmag*x(i)
        ys(i)= iyo+ymag*y(i)
      enddo
      xs(4) = xs(1) ; ys(4)=ys(1)

! various colour-schemes:
!    - filled v. edged
!    - triangle-type : eg new-node/across wedge/extra_left/extra_right
!    - depth from the boundary  (hence 'contours')
!    - inhereted code (hence 'fingers')
!    = element area (scaled from dsep and dsep_max)

      icol_fill2=icol_fill
      icol_line2=icol_line

#ifdef PGPLOT
      if (icol_fill2 >=0) then
        call PGSCI (icol_fill2)
        call PGPOLY (3,xs,ys)
      endif
      if (icol_line2 >=0) then
        call PGSCI (icol_line2)
        call PGLINE (4,xs,ys) 
      endif
#endif
!      call get_secs(t)
!        write(*,'(3f12.5)') t,t_lastupdate, t-t_lastupdate
!      if (t-t_lastupdate>.5) then
!!        call PGUPDT()
!        t_lastupdate=t
!      endif
      end subroutine draw_tri

!-----------------------------------------------------------------------
      subroutine plot_front (gc, front, nfront,ifb, icol)
!
!      /* Just plots the complete polyline(s) */
!       DJK
!  13-10-97 Note choice of a/ all fronts, or b/ just the active front.
!  27-1-98 Plotting all fronts is slow (cf MAH) so use sparingly.
!
      implicit none
      real, intent(in)    :: gc(:,:)
      integer, intent(in) :: front(:,:), nfront,ifb, icol
      integer i
      real*4 ::  xs(2),ys(2)
#ifdef PGPLOT
      real*4 :: radius=1.
      integer :: icol_line2, icol_node2
!     integer :: icol_fill2
#endif

      if (grstate==0) return

      do i=1,ifb+nfront                 !-- all fronts 
!     do i=ifb+1,ifb+nfront             !-- just the current
!     do i=1,nfront -1                  !-- all but the last
      
        xs(1) = ixo+xmag*gc(1,front(1,i) )      !- try adding ifb to these 4 ?
        ys(1) = iyo+ymag*gc(2,front(1,i) )
        xs(2) = ixo+xmag*gc(1,front(2,i) )      !- try adding ifb to these 4 ?
        ys(2) = iyo+ymag*gc(2,front(2,i) )

! oe. if we have aproper datastructure..
!      x1 = front(i)%x(1)  ;    y1 = front(i)%y(1)
!      x2 = front(i)%x(2)  ;    y2 = front(i)%y(2)       !- plus 'z' and a 3rd point if 3d.


#ifdef PGPLOT
      icol_line2 = icol 
      if (icol_line2 >=0) then
        call PGSCI (icol_line2)
        call PGLINE (2,xs,ys) 
      endif
      icol_node2 = -1
      if (icol_node2>=0) then
        CALL PGSCI (icol)
        CALL PGCIRC (xs(1),ys(1), radius )
      endif
#endif
      enddo
      end subroutine plot_front

end module plotlib

!======================================================================

!MODULE ADV_FRONT   !- uncomment these to use the library as am F90 MODULE
!CONTAINS

!-----------------------------------------------------------------------
     subroutine BUILD_FRONT (gc,igc,nn, ring,nring,imat,iverb)
!
!    This creates the new nodes on the bounding polygon in RING(1:NRING)
!       Dan Kidger  1996
!
!    The steps:
!      1. Read in the nodal coords and steering (polyline only)
!      2. Loop the (4) edges.
!      3.  sample at '101' points along this edge, and sum the integral =>NN_edge
!      4.  loop NN_edge, interpolate  to get s-pos, hence mung to x,y.
!      5. join_coincident_nodes (really just at the corners?)
!
!    22-2-97 lintyp now a string eg. 'arc'
!
      integer :: igc, nn, nring
      real    ::  gc (2,igc)            !- to store the new nodes in.
      integer :: ring (igc)             !- & the new frlont's nodes (cf NUM)

      integer,  parameter  :: mcp=4000      !- max # of control points per polyline.
      real x(mcp),y(mcp)              ! ?maybe malloc these?
      character lintyp*10             !- 'line','arc','spline','sine'

!--------------- Read in boundary segments -----------------------------
! Read each then create in turn (cf reading all first?)
      read (25,*,iostat=ios) nedges, imat                   
      if (ios/=0) stop ' Reading NEDGES,IMAT failed'

!---------------------- loop the (4) edges -----------------------------
     nring = 0                        !- count total nodes in the polygon

     do iedge = 1,nedges

!.. hmm ilintyp will tell us how many xy-pairs to read .. so maybe
!.. read then BACKSPACE, then IF..ELSEIF the options

      read (25,*,iostat=ios) lintyp, ncp,(x(i),y(i),i=1,ncp)
      if (ios/=0) ncp = i-1   !- reduce ncp if we hit EOF (why?)

!.. OK now a SELECT CASE on ILINTYP 
!.... if explicit nodal coords then just copy into GC() & RING()
!.. else call build_front_edge to do the interpolation.
        if (lintyp.eq.'explicit') then
           call poly_to_ring (gc,igc,nn, ring,nring,x,y,ncp)
           call normalise_gc (gc,igc,nn, ring,nring)   !- a hack surely!
        else
!          ------ determine how many arcs (etc) there are.
           if (lintyp.eq.'line') then      !       SELECT CASE ?
             n_ord=2                       !- 2 points per line
           elseif (lintyp.eq.'3nl' &
     &       .or.lintyp.eq.'arc'.or.lintyp.eq.'cra') then
             n_ord=3                       !- 3 points per line
           elseif (lintyp.eq.'sine' ) then
             n_ord=3                       ! as line but followed by freq and wavelength
           else
             n_ord = ncp                   !- default no. of points per 'piece'
           endif
           n_pieces = (ncp-1) /(n_ord-1)   !(integer divide is OK)
           do j=1,n_pieces
             ibase = 1+ (j-1)*(n_ord-1)
             call build_front_edge (x(ibase:),y(ibase:),n_ord &
      &      , gc,igc,nn, ring,nring,lintyp,iverb)
           enddo
        endif
      enddo                            !- ie. loop the 4 edges.

! -- if we build the ring correctly - there is no need for the next line.
!     call del_coincident_nodes (gc,igc,nn, ring,nring)
      return
      end subroutine build_front

!-------------------------------------------------------------------------
     SUBROUTINE BUILD_FRONT_EDGE (x,y,ncp, gc,igc,nn, ring,nring,lintyp,iverb)
!
!     This builds a set of nodes along the given edge
!     (using a call to GET_SPACING to get each node:node spacing)
!       Dan Kidger  1996
!
!   Notes:
!     1. It loops s=0.->1. along the 'edge'
!     2. We use the Trapezium rule - Simpsons would be a little better.
!
!   Extensions:
!     1. overload to handle curved edges+ arcs via LINTYP
!     2. allow an existing edge to be 're-used' (not new nodes) (?)
!
      real    ::    gc (2,igc)         !- to store the new nodes in.
      integer ::  ring (igc)           !- the new front's nodes
      character :: lintyp*10
      real x(*),y(*)
!     integer,parameter :: m_integ = 100  !- # of integration points
!     integer,parameter :: m_integ = 20
      integer,parameter :: m_integ = 40
      real    :: areas(0:m_integ+1)        !- sampled values.

      real,parameter :: tol = 1.e-20   !- 'closeness of a point' tolerance
      data icount/0/    !- just for debug output.

      icount = icount+1
!----------- sample at the 'm_integ+1' points ----------
      n_integ = m_integ                !(usu. =100 pts)

      tot_area = 0.                    ! total integral
      tot_s = 0.                       ! total edge length
      do j=0,n_integ
        s = real(j)  /real(n_integ)    ! 0.->1.
        call interp_line (s,x,y,ncp, xp,yp, lintyp)   !- do s->(xp,yp)
        call get_spacing (xp,yp,deltap,imat)          !- apply localisations
        deltap = 1./deltap
        if (j.ne.0) then             !- not for the first point
           ds = sqrt( (xp-xold)**2 + (yp-yold)**2 )   !- 'h'
           area = ds* (delta_old+deltap)/2.           !- trapezium rule
           tot_area = tot_area + area
           tot_s = tot_s + ds
           areas(j) = tot_area           !- or accumulate at the end?
        endif
        xold = xp                        !- remember the last point
        yold = yp
        delta_old = deltap
      enddo
      ni = nint(tot_area)              !- target #nodes to create
      if (ni.eq.0) ni = 1              !- must be at least one ?

     if (iverb >=2) &                  !- output for logging
      write (*,'(I3, 3a,f12.4, a,f12.4, a,i6,a)') icount,  &
     &  ' : ',lintyp,' L=',tot_s,                          &
     &  ' Integral = ', tot_area,                          &
     &  ' ->', ni,' edges'

!----------------- patch to 'round' to ni points ----------------------
!.. Distibute the closing 'error' equaly to all the integration points
!.. This is the same algorithm as used in Surveying to adjust a set of levels.
!-- The main reason for doing this is so that if one edge is common to two
!   regions then the nodes created 'left-to-right' will match those done
!   'right-to_left'

      error = ( ni-tot_area ) /real(n_integ)    !- ave. error per point
      do i=1,n_integ
        areas(i) = areas(i) + i*error
      enddo

!-------------- Generate coordinates of these points ---------------------
      if (iverb >=2) &                  !- output for logging
      print*,' looping over ',ni,' points on this edge..'
      areas(0) = 0.
      do k = 0,ni-1                          !- step along the new edge nodes
         darea = areas(n_integ)* real(k)/real(ni)   !- ie. from 0.->tot_integral

!-- find which segment this 'point' is in. --
!.. maybe I could try the 'last time' segment for speed ?
         do l = 1,n_integ
            area = areas(l)
            if (area.gt.darea) goto 10 !exit !(goto 10)
         enddo 
    10    continue 

!---------------------- find the position for this node ---------------------
! cf the original method that DIDNT interpolate within each piece so it was
! only accurate to 1 in 100 (so some nodes will end up on top of each other !)
        s = (l-1.)/n_integ            ! position of this node (0.->1.)

!--------------- now subsample 's' based on delta_area --------------------
         f1 = (darea-areas(l-1) ) / ( areas(l)-areas(l-1) ) !- sub fraction
         s = s + f1/n_integ            !- add the extra bit.

         call interp_line (s,x,y,ncp, xp,yp, lintyp)   !- do s->(xp,yp)
         nn = nn + 1                   ! add this new node
         gc(1,nn) = xp                 ! store global coord 
         gc(2,nn) = yp
         nring = nring+1
         ring(nring) = nn              ! and remember it too.
      enddo
      return
      end SUBROUTINE BUILD_FRONT_EDGE

!-----------------------------------------------------------------------
      subroutine interp_line (s,x,y,ncp, xp,yp,lintyp)
!
!    This returns the (XP,YP) coord of a point along the polyline X(),Y()
!    (of NCP points?) given S in the range 0>s>1
!    Overloaded with ILINTYP to generate sin-waves etc.
!       DJK  28-3-96
!
!    ILINTYP = 1    simple line
!              3    arc 
!              4    arc with a sine wave perturbation (gear wheel)
!              5    line with a sine wave perturbation
!
      REAL :: X(*),Y(*), XP,YP, S, PI
      real :: THETA,R,A,ab,st,ct
      character :: lintyp*10
      save

      pi = 4.*atan(1.)
!------ simple straight line -------
!.. can be multi-called to get a polyline.
!     if (ilintyp.eq.1) then   
      if (lintyp.eq.'line') then 
        xp = (1.-s)*x(1) + s*x(2)        !- hence get the x,y coord
        yp = (1.-s)*y(1) + s*y(2)        !- by linear interpolation

!------ 3-noded (parabolic) line -------
      elseif (lintyp.eq.'3nl') then   
        f1 =  2.*(s-.5)*(s-1.)          !- shape functions
        f2 = -4.*(s   )*(s-1.)
        f3 =  2.*(s   )*(s-.5)
        xp = f1*x(1) + f2*x(2) + f3*x(3)   !- hence get the x,y coord
        yp = f1*y(1) + f2*y(2) + f3*y(3)  


!------ Polyline ------
!.. also poly-spline too
!   need to find which segment we are in. (via sum edge-lengths)
!   hence linear-interpolate each 
!   (this is good for the 'Mersey' dataset so can adjust the edge spacing.
!     elseif (ilintyp.eq.2) then   
      elseif (lintyp.eq.'spline') then   
         print*,'*** WARNING : Splines not yet implimented'
         stop

!------ an Arc ------
!.. clever cos it creates spirals to make sure the 2 ends are on the line
!.. note order: 3 points are A,Centre,B
!.. caveat: sence of the arc is always that it *doesn't cross vertical?!
!     elseif (ilintyp.eq.3) then   
!.. maybe I should define clock/aclock arcs?

      elseif (lintyp.eq.'arc') then      !- clockwise arc?
        dx1 = x(1) - x(2)     ;   dy1 = y(1) - y(2)
        dx2 = x(3) - x(2)     ;   dy2 = y(3) - y(2)
        a1 = atan2(dy1,dx1)   ;   a2 = atan2(dy2,dx2)
        a2 = a2 + .000001                 !- so can get a full 360deg.
        if (a1.lt.a2) a1 = a1 +2.*pi      !- so always clockwise
        r1 = sqrt(dx1*dx1+dy1*dy1)
        r2 = sqrt(dx2*dx2+dy2*dy2)
        theta = (1.-s)*a1 + s*a2
        r     = (1.-s)*r1 + s*r2
        xp = x(2) + r * cos(theta)
        yp = y(2) + r * sin(theta)

      elseif (lintyp.eq.'cra') then      !- anti-clockwise arc?
        dx1 = x(1) - x(2)
        dy1 = y(1) - y(2)
        dx2 = x(3) - x(2)
        dy2 = y(3) - y(2)
        a1 = atan2(dy1,dx1)
        a2 = atan2(dy2,dx2)
        a1 = a1 + .000001                 !- so can get a full 360deg.
        if (a2.lt.a1) a2 = a2 +2.*pi      !- so always anti-clockwise
        r1 = sqrt(dx1*dx1+dy1*dy1)
        r2 = sqrt(dx2*dx2+dy2*dy2)
        theta = (1.-s)*a1 + s*a2
        r     = (1.-s)*r1 + s*r2
        xp = x(2) + r * cos(theta)
        yp = y(2) + r * sin(theta)

!------ an Arc with a Sine wave ------
!.. as type 3 but we have:
!   a normal pertubation of mag. y(3) of wavelength of x(3)
!     - eg. will create a 'gearwheel' like mesh.
!     elseif (ilintyp.eq.4) then   
      elseif (lintyp.eq.'arcsine') then   
        dx1 = x(1) - x(2)
        dy1 = y(1) - y(2)
        dx2 = x(3) - x(2)
        dy2 = y(3) - y(2)
        theta = (1.-s)*atan2(dy1,dx1)        + s*atan2(dy2,dx2)
        r     = (1.-s)*sqrt(dx1*dx1+dy1*dy1) + s*sqrt(dx2*dx2+dy2*dy2)
        ct = cos(theta)                !- cos theta and sin theta
        st = sin(theta) 
        xp = x(2) + r * ct
        yp = y(2) + r * st
        a =  y(4) * SIN (s* pi *x(4))
         xp = xp + a*ct                 !- hence move the point
         yp = yp + a*st

!------ Sine Waves ------
!     elseif (ilintyp.eq.5) then   
      elseif (lintyp.eq.'sine') then   
!.. as type 1 but we have:
!   a normal pertubation of mag. y(3) of wavelength of x(3)
!.. note with a large amplitude then this will not give equaly spaced nodes.
!.. maybe x(3) can be the 'harmonic' 1=natural(up),2=up and down,4=2*lamda
        dx = x(2)-x(1) ; dy=y(2)-y(1)
        ab = sqrt(dx**2 + dy**2)
        xp = (1.-s)*x(1) + s*x(2)        !- hence get the x,y coord
        yp = (1.-s)*y(1) + s*y(2)        !- by linear interpolation
        a =  y(3) * SIN (s* pi *x(3))
!        a=0.
!         print*,dx/ab,dy/ab
        ct = dx*1./ab       !- cos theta and sin theta
        st = dy*1./ab 
!        write(*,'(8F10.5)') dx,dy,ab,a,xp,yp,s
        xp = xp + a*st            ! hence move the point
        yp = yp + a*ct
!         print*,a,st,ct
      else

        stop '** Unknown line type (INTERP_LINE)'
      endif
      write(29,'(a,8F10.5)') lintyp,x(1),y(1),x(2),y(2), x(3),y(3), xp,yp
      return
      end

!-----------------------------------------------------------------------
     subroutine poly_to_ring (gc,igc,nn, ring,nring,x,y,ncp)
!
!    Moves a set of x,y coords to GC: the global coordinates table.
!    whilest adding them to RING() too.
!     (eg the 'Mersey' dataset from Wallingford)
!
   integer :: igc, nn
   integer :: ring (igc)            !- the new front's nodes
   real    :: gc (2,igc)            !- to store the new nodes in.

      real x(*), y(*)
      do i=1,ncp                       !nring
         nn = nn+1
         gc(1,nn) = x(i)        
         gc(2,nn) = y(i)
         ring(i) = nn
      enddo
      nring = ncp
!     if (iverb>=1) &
      write (*,'(a,i6,a)') '<>',nring,' nodes were created'
   end subroutine poly_to_ring

!-----------------------------------------------------------------------
   subroutine normalise_gc (gc,igc,nn, ring,nring)
!
!   This rescales the coords in GC such that they all lie in the
!   unit square (0.->1., 0.->1.)
!      Dan Kidger 9-10-97 
!   Notes:
!      cf. scaling all nodes or just those in the ring ?
!
   integer :: igc, nn, i
   integer :: ring (nring)            !- the new front's nodes
   real    :: gc (2,igc)            !- to store the new nodes in.
   real    :: xmin,xmax, ymin,ymax, xscale, xp,yp

!------- find the bounding box ----------
      xmin = minval (gc(1,ring(1:nn))) ;  ymin = minval (gc(2,ring(1:nn)))
      xmax = maxval (gc(1,ring(1:nn))) ;  ymax = maxval (gc(2,ring(1:nn)))
      xscale = max (xmax-xmin, ymax-ymin)

!------- normalise the data ----------
!-- maintain aspect ratio .. but fit to a unit square
!.. I do not know why we do this .. it ought to be optional surely.
!  (maybe it is so because in AFRONT the convergence tolerances are 
!    absolute not relative ?)
      do i=1,nn !nring
        xp = gc(1,ring(i) )
        yp = gc(2,ring(i) )
        gc (1,i)  =  (xp-xmin) / xscale
        gc (2,i)  =  (yp-ymin) / xscale
      enddo
! or..   gc(1,ring(1:nn) ) =  (gc(1,ring(1:nn))-xmin) / xscale
   end subroutine normalise_gc

!-----------------------------------------------------------------------
!  27-11-97 Need an option to control plotting:
!       -1.=?, 0. = every, 0.5= every 0.5 secs, 999= never

!-----------------------------------------------------------------------
      SUBROUTINE ADVANCING_FRONT (ipl,gc,igc,nnode  ,num3,inum3,nel,nel_max  &
     &   ,ring,nring,imat, base_type,iverb,igraph)

!     This fills in a sub-domain with unstructured triangles by using the
!     now well-known advancing front method.
!         Dan Kidger 14-4-96
!   Notes:
!     1/ ** Based on an implimentation by A.Ma & M.A.Hicks (1994) **
!           (theory from a paper by Peraire et al. (1987))
!
!   Revisions:
!     (heavily worked on in Spring 1996)
!    16-9-97 Moved the sub-tasks out to daughter subroutines

!  Dan's modifications: 
!       1. Declare KEEPN,SORTN,INDX AND HOLDR here (not as arguments)
!       2. Abstract the graphics to daughter routines (hence Unix)
!       3. Revise all THETA calcs -> better to use vector algebra
!       4. Abstract the add_element to a daughter routine (hence plot it)
!       5. reverse subscripts so gc(2,igc) NOT gc(igc,2)
!       6. *NO* BACKGROUND MESH .. use bitmaps instead
!      12. allow counter-clockwise ordering ( but NUM order?)
!  To do:
!       6. Turn FRONT() into a data structure, holding length & D.C's
!       7. Make FRONT() a linked-list so we can add/remove segments
!             .. in F90 or do my own garbage-callection ?
!       8. Hence build on the 'shortest' segment each time.
!          or at least build on a randomly chosen segment
!       9. Put XY coords into FRONT() too, hence 'map' to global xyz
!      10. Hence build the faces of 3D slope-mesh
!      11. Try a curvilinear 3D mapping (eg over a sphere?)
!

!     use seg_struct
      use plotlib          !- calls to plot_front, plot_tri

!     integer num3 (10,inum3)            !-- the new table of elements --
      integer num3 (inum3,*)            !-- the new table of elements --
      real     gc (2,igc)              !-- the new set of nodal coords --
      integer ring(nring)              !- the nodes in the initial front
!----- local workspaces ----
      integer,parameter :: max_front = 10000 
      parameter (ifront=max_front)
      integer ::   front (2,max_front)          !- node pair on the front
      integer ::  sortn(max_front)              !- list of nodes 'near' C
      integer ::  bases(max_front)              !- list of trial bases left
      real    ::  base_lengths(max_front)
      real    fun(3), vc(2),ct(3,3), AB, MN ,randval
      logical :: c_at_head  !-  if point C is at the head of the list.
      logical :: allow_create_element= .true.  ! in corners (idelta=+/-2)
      integer :: ic_dp=0 ,ic_ds=0 ,ic_lb30=0, ic_intri=0, ic_lupfac=0   !- timing counters
      character :: base_type*12
      integer :: ifill=-1    ! element fill colour
      data tol/1.e-20/            

!--------------- 0: initialisation ----------------
!      print*, "top of advfront, igraph=",igraph
      if (igraph>=1)  call open_graphics_window ()    !(do in adv_front itself)
      nfronts_on_stack = 0
      ifb = 0                 !- offset over the 'passive' fronts.
!     ifb=7
!---------- 1: Create the data structure that holds the front ----------
! What about internal holes ?
! ie. an active front is allowed to consist of disjoint pieces
!  (but they must be signed to define the outward normal)
! Oct 2002 - Some notes on internal holes...
!   1. Should I allow building on the 'holes'? Yes I guess so. 
!      Indeed a tuning factor 0.->.1 could be used to favour building from the inside out or
!      the outside in.
!   2. Be careful of parts of the code where we loop around the front until we meet get back to
!      where we started. I guess we should make sure we loop all the pieces.
!   3. So how do we define holes? I guess by specifiing N pieces  
!      or perhaps implicitly by starting a new ring each time we get back to a start point
!      I prefer specifing N holes, then read main loop and then read internal loops
!   4. Area calc - just sum pieces: holes will have -ve area. :-)
!
      call ring_to_front (ring,nring, front, max_front,ifb,nfront)

!------------------ 2: Find the area of the front ----------------------
! Because we can:
!   1: Find if the polygon is clockwise (or aclock), hence which side  of the front we 
!      should build elements on. (although I think clockwise should be mandatory)
!   2: Use the area to calculate DSEP to give a target NEL (=100 say)
!        I like this idea !
!   3: Estimate the number of elements that we will create - hence malloc arrays accordingly
!   4. Show a percentage-complete bar line
!   5. Exit once a certain area has been covered (cf. when the ring splits do teh bigger or 
!      smaller half first.
!

!   If holes then we need to be a little more careful - ie. when refering to teh 'next' segment.
!    hmm also why do we use the next segment at all and not just the far end of ths segment?
      area_F = 0.
      do i=1,nfront
         n1 = front(1,ifb+i)
         n2a= front(2,ifb+i) 
         n2b= front(1,ifb+mod(i,nfront)+1)
         n2=n2b
!        .. We use the general polygon formulae (as in my PhD.)
         area_F = area_F + ((gc(2,n1)+gc(2,n2)) * (gc(1,n2)-gc(1,n1)))/2.
      enddo

      call get_spacing (-9999.,0.,dsep,imat)  !- get DELTA of the background (at infinity)
      area_el = 0.5 * dsep * (0.866*dsep)   !- area of one element
      nel_estimate = nint(area_f/area_el)
! cf. if we provide nel_target then 
!     dsep_estimate = area_f/real(nel_target)
      if (iverb>=2) then
        write (*,*) 'Total area of this polygon (front)=', area_F
        write (*,*) 'Estimated no. of elements =', nel_estimate
      endif
      area_remain = area_f
!-- Really if th user got the sense wrong we should abort
!     hence I could even support building on the *outside* of a shape!
      iflip = int(- sign(1.,area_F))        !- so -ve = clockwise (+ve=anti-c)
!     iflip = 1
!   .. or. warn the user and abort if area_f<0.
!     Ok lets reverse the front if area_f<0.
!  (area_F is not used again.)
!   cf. decrementing it each time we add a triangle hence a 'percentage done' feedback
!   if area_left <0. 

!------------------------ 3: set up the graphics -----------------------
! use a graphics level? (IDBG) 0=no graphics, 1=front,2=nodes, etc.)
! note plot front is internal so we do not need to pass args
      call get_mesh_extent (gc,igc, nnode)  !, xmag,ymag, ixo,iyo)
      if (ipl>=2) call plot_front (gc,front,nfront,ifb, 3)  

!-----------------------------------------------------------------------
!                ****  1: Triangularization loop   ****
!-----------------------------------------------------------------------
   add_elements:   do l=1,huge(l)-1     !- loop forever

!     if (debug_mode=1) &
!     call validate_front (front,ifront,ifb,nfront, ifail)  !- all OK ?

!-----------------------------------------------------------------------
!                ****  2: Loop trial base segments  ****
!-----------------------------------------------------------------------
!  If first time, init - say take the LAST segment 
!  next time, pick another (eg last+1)
!      eg2 if a multi-queue, first time pick the lowest
!      next time continue with the next lowest etc.
!  base_type= 'shortest',  'random', etc.
    try_a_base: do ibase = 1, nfront

!----------------- first time --------------------
!.. if random , we can either shuffle here, or randomly draw one
!. from the 'remaining pack'.
      if (ibase==1) then    
        select case (base_type)
        case ('backward')         !------------------------------------------
          do i=1,nfront
            bases(i) = nfront+1-i   
          enddo

        case ('shortest')         !----------- always shortest edge --------
!          allocate (base_lengths(nbases))       !-hmm!
          do iseg=1,nfront
            call get_segment (front,nfront, ifb+iseg, gc,2,nn,  &
    &       nodea,xca,yca, nodeb,xcb,ycb, cth,sth, ab,ab2    )
            base_lengths (iseg) = ab2
!           base_lengths (iseg) = -ab          !- or this ?
              ifront_base= iseg      !- remember which it was
          enddo
          call index_reals (base_lengths,nfront,bases)  !'Modified Quicksort' 
!          deallocate (base_lengths)
!         -- so now bases(1) = *smallest* (?)

        case default          !----------- general case ------------
          do i=1,nfront
            bases(i) = i      !- default is epinomous.
          enddo
        end select

      endif
      nbases = nfront

!----------------- pick the next 'free' one ---------------

      select case (base_type)
      case ('forward')         !----------------------------------
        ifront_base=bases(ibase)
      case ('backward')        !----------------------------------
        ifront_base=bases(ibase)
      case ('random')          !--------- the 'best'? ------------
        call random_number(randval)      !F90 intrinsic
        ifront_base = 1+int(randval*nbases)
        bases (ifront_base) = bases (nbases)  !- fill-in the gap
        nbases = nbases-1
      case ('random2')         !-- as 'random' but no list shrinking (why?)
        call random_number(randval)
        ifront_base = 1+int(randval*nfront)
      case ('shortest')        !----------------------------------
        ifront_base=bases(ibase)
      case ('longest')         !----------------------------------
        ifront_base=bases(ibase)   !0- this isn;t right surely !
      case default             !----------------------------------
        stop '** ERROR: base type is unknown'
      end select
!       print*,l,ifront_base,randval


! .. if random, then each time we disallow, so the list will get shorter
!  ie. start with a list 1:nseg, each time we take a random one then
!    put the head of the list into the hole
 
! If pick_shortest:
!    first time, compile list of lengths, then sort, then just pick in order
!   (or each time scan remaining list for the 'smallest')

! sometimes we want to loop in order, but start at a paticular base: 
! eg. the first segment; the last; one of the 2 'new' segments, etc.

!-----------------------------------------------------------------------
!     Step 1: Choose a segment to be the base.
!-----------------------------------------------------------------------
!      CALL GET_ANOTHER_BASE ()

! we currently always use the (last) active side 'AB' in the front 
! though any should do;  the shortest is supposed to be best

!  * Notes on heaps for the bases 17-9-97 *
!  *   for each new segment put it in one of (say) 16 queues
!  *   to pick one - parse the queues from left to right for a non-nul
!  *   to reject - try the next above and so on (deadlock if we complete)
!  *   Hence we always build on 'one of the smallest' segments.

!     ifront_base = nfront
!      ifront_base = nfront-1
!      ifront_base = 1
!      print*,'ifront_base=',ifront_base
!      ifront_base = min(max(1,ifront_base),nfront)
       if (ifront_base<1 .or. ifront_base>nfront) stop 'base out of range'

     call get_segment (front,nfront, ifb+ifront_base, gc,2,nn,  &
    &   nodea,xca,yca, nodeb,xcb,ycb, cth,sth, ab,ab2    )
!      ab=sqrt(ab2)
!------- 1a: get the mid-point of the base segment ------                    
      xmid = .5*(xca+xcb)               !- mid-point of AB
      ymid = .5*(yca+ycb)

!-----------------------------------------------------------------------
!    Step 2: Determine the local mesh parameters at M by 
!     interpolating over the background grid - Iterative loop
!-----------------------------------------------------------------------
! < easy as a subroutine >
! < given a lines' xo,yo,dcx,dcy,len: return point C (XC,YC)
!   really forget the base seg and just use xmid,ymid, and the normal vector.

    call find_point_C (xca,yca, xcb,ycb, cth,sth,ab,iflip,  xcc,ycc, ac)

!-----------------------------------------------------------------------
!   Step 3  : Determine all the active nodes which lie within the circle
!             at centre C and radius 5*AB and sort them in order
!-----------------------------------------------------------------------
! < Given XC,YC, Forms a list SORTN of those 'close' nodes 
!    (plus the distances in DISTANCES)
! < for Delauney, this would use *all* the internal nodes as well as the ring 
! < hence sorts them via INDX, so finally return an *ordered* SORTN
!   .. we can filter out those nodes *below* AB here if we wish
!
     CALL FIND_CLOSE_C (front,ifb,nfront,GC,2,NN, &
  &   nodea,nodeb,XCC,YCC, 5.*AB,SORTN, IST, ic_ds)
!    print*,'sortn(1:6)=', sortn(1:6)
!-----------------------------------------------------------------------
!     Step 4 :  Proximity criterion       CLOSE=0.5*delta
!     Place C at the head of the list if CN > CLOSE
!     Hence, generate a proposed new node C.
!-----------------------------------------------------------------------
!. recall that delta is the edge length (AC,BC) of triangle ABC
!.. so get each nodes x,y (again), hence its distance from C (again)

!.. test to see if AB is nearer to our point C than the 
!.. nearest other point in the front.
!    if so create point C as a new node in GC()
!   either way carry forward a point to build a triangle to

!   2-10-97 Or? Always put C at the head - then skip over it if we dont want it
!   ie ll=1 by default, but ll=2 if we dont want it.
!   hence C_AT_HEAD is n/a - use of ll is sufficient
!   IN GENERAL : *only* form the list SORTN of points that *could* be used
!                 to form triangles
!  (but note that most of the time we end up using point C straight off)

      close = .5*AC               ! Circle of 'closeness' has this radius
!     -- so first get the distance to the *nearest* point to C
!     ( isn't this number in DISTANCES ?)
      CN = sqrt ((gc(1,sortn(1))-xcc)**2 + (gc(2,sortn(1))-ycc)**2)
      c_at_head=.false.

      if (cn.gt.close) then            ! If C ->sortn(1) is outside close range.
!        (else just RETURN)
        gc(1,nnode+1) = xcc            ! Allocate coords. of new node C
        gc(2,nnode+1) = ycc            ! Global C(xcc,ycc)-->gc
!        .. maybe store its DISTANCE as -1. as a flag?
        ist = ist+1              ! No. of nodes in range 5AB now increased by 1
        do i=ist,2,-1            ! 
          sortn(i) = sortn(i-1)  ! push the list down. :-(
        enddo                    ! New node C, next nearest node to C, etc...
        sortn(1) = nnode+1       ! Make node C have new node #.
!       sortn(0) = nnode+1       ! or do this?
! or flag as -ve ?
        c_at_head=.true.

! (or leave the list alone and insert as SORTN(0)
!  default =0, so later just step over this zero if we find it.
!- really only a 'potential' node , nnode++ later if all is OK.
!- therefore we could *always* update GC when we create point 'C'
!    - just sometime we will ignore it.
      end if

!- define the node at the head of the list as nodeN (often is nodeC)

!**********************************************************************
!  Test validity of this new element ABN
!    A/ Is it a nice shape? (+ve area, not colinear)
!    B/ Does it fit in? (doesn't cross the far segment boundary)
! (then we play with problems due to ring-splitting and corner-triangles..)
!**********************************************************************
!so
!     call test_abn (xca,yca, xcb, ycb, xcn, ycn, ifail)
!     if (ifail.ne.0) goto 30   !- try another of the 'close' points.
!then
!     call test_front_valid (xca,yca, xcb, ycb, xcn, ycn, front,nfront,ifail)
!     if (ifail.ne.0) goto 30   !- pick another base segment


!--------- try each point in SORTN ------------
!.. as a function we loop SORTN until we find a node that we like.
! back here if:
!     a/ ABN are co-linear
!     b/ ABN contains another node (cf Delauney)
! <    c/ MN crosses any other edges in the ring >

      ll = 1               ! start with the first term in sortn
! this is not quite a do loop cos sometimes we ll jumps up by >1
! a DO LOOP would have to have a dummy loop counter
!------------------------ loop-back point ------------------------------
   30 continue             ! (abort if ll > ring size)
      ic_lb30 = ic_lb30 + 1            !(timings)

      noden = sortn(ll)     ! The proposed connecting node N
!.. if this is nodeC just copy, else get from GC
!     if (c_at_head .and. ll==1) then
!        noden = nodec
!        xcn=xcc
!        ycn=ycc
!     endif
      xcn = gc(1,noden)     ! and pick up its coord
      ycn = gc(2,noden)     ! and the area of triangle ABN

!-------------- is point N on the RHS of line Ab ? (hence a flipped over triangle) -----
      ABN = (xcb*ycn-ycb*xcn) -(xca*ycn-yca*xcn) +(xca*ycb-yca*xcb) !-area
!  hmm so why do I not try another C point and not just abandon this edge ?
!   (I may even get complete stuck if only a few edges left)
!     if (ABN*iflip <=0.) goto 20  !- CYCLE
      if (ABN*iflip <=0.) CYCLE try_a_base

!-----------------------------------------------------------------------
!     Step 6 : Check for a bad triangle
! either test angle ABN is close to zero or test circumcircle.
!-----------------------------------------------------------------------
!.. if almost co-linear I think (eg. when along the boundary)
!.. then either CYCLE if nodes left or just EXIT if all done.

!.. I could of course just test vny/vnx or vny<.0001*vnx

! calc the 'altitude' of N above AB
      vnx =  cth*(xcn-xca) + sth*(ycn-yca)   ! base distance from A
      vny = -sth*(xcn-xca) + cth*(ycn-yca)   ! altitude of N above AB
      if (abs(vnx).lt.1.e-20) then
        rad = 3.14159265/2.                  ! Yuk! 
      else
        rad = atan(abs(vny/vnx))   ! (vnx,vny,rad are not used again.)
      endif
!     rad = atan(abs((vny+1.e-20)/(vnx+1.e-20)))   ! (vnx,vny,rad are not used again.)
      if (rad.lt.1.e-4) then        ! if angle NAB is almost 180 deg.
!-- or always goto 30 but @30 we EXIT if list exhausted.
        if (ll.ge.ist) cycle try_a_base        !- exhausted the list ?
        ll = ll+1                   !- try the next closest point instead
        goto 30                     !- & reiterate
      endif        !- if ABN were colinear

!===================== Front transgressed ? ============================
! check that the new element does not cross out of the front polygon
!   Best algorithm is:
!     1. loop all segments
!     2.  if segment contains nodeN cycle
!     3.   check against AN  - abort if a hit.
!     4.   check against BN - abort if a hit.
!     5.  end-loop 
!     6.  so now it must be OK.

!-----------------------------------------------------------------------
!     Compute shape functions corresponding to point (xp,yp) ?
!-----------------------------------------------------------------------
!     Step 7 : Check if any node lie within ABN
!       (For all nodes in the list following nodeN)
!     This checks the nodes nearest (SORTN) to the base rather than 
!     all active nodes thereby reducing computer run time.
!   hmm - Do we need this test at all - cos if any node *is* inside ABN
!        then at least two segments will cross AN or BN
!-----------------------------------------------------------------------
!.. hmm must be a cut-off point whereby a node P is too far away from C
!   to stand a chance of being within ABC
!   Yes if its DISTANCE is bigger than AN (or BN) it (and all following)
!   nodes are outside ABN
!.. note if we get a hit then make node P be node N and jump-back to retry
!  (note 'ii' is the current node that we are testing.

!-----------------------------------------------------------------------
!     Coefficient matrix    for triangle ABN
!  ie.  |  1    1    1   |
!       |  Xa   Xb   Xn  |
!       |  Ya   Yb   Yn  |
!-----------------------------------------------------------------------
!.. build this proposed element (hence interpolate)
!   DJK 12-2-96 : Although the next is elegant.. is it fast ? 
!  Note that I can use DET to reject anti-clockwise elements (cf the above)
!  ** note six terms here **
!     ct(1,:)  = (/ 1. , 1. ,     1.        /)
      ct(2,:)  = (/ xca, xcb, xcn /)   !- triangle ABN
      ct(3,:)  = (/ yca, ycb, ycn /)
      ic_lupfac = ic_lupfac+1

!---------- explicit matrix inversion -------  DJK 12-3-96
      CTI11 =  CT(2,2)*CT(3,3) - CT(3,2)*CT(2,3)
      CTI21 = -CT(2,1)*CT(3,3) + CT(3,1)*CT(2,3)
      CTI31 =  CT(2,1)*CT(3,2) - CT(3,1)*CT(2,2)
      DET = CTI11 + CTI21 + CTI31
      area_element = det/2.       !- should never be -ve!
!     print*,abn,det
      CTI12 = -        CT(3,3) + CT(3,2)        
      CTI22 =          CT(3,3) - CT(3,1)        
      CTI32 = -        CT(3,2) + CT(3,1)        
      CTI13 =          CT(2,3) - CT(2,2)        
      CTI23 = -        CT(2,3) + CT(2,1)        
      CTI33 =          CT(2,2) - CT(2,1)        
 
      do ii=ll+1,ist                  ! Scan sortn(2:).. - call them node P
        ic_intri  = ic_intri+1        !(timings)
        jnode = sortn(ii)
        xcp = gc(1,jnode)        ! global coord of P
        ycp = gc(2,jnode)        ! 
        fun(1) = (cti11 + cti12*xcp +cti13*ycp) /det
        fun(2) = (cti21 + cti22*xcp +cti23*ycp) /det
        fun(3) = (cti31 + cti32*xcp +cti33*ycp) /det  !(xp,yp: not used again)

!----------- Is the point (xp,yp) within the element ABN? --------------
!..  if so make node N this node
!  27-1-98 * We must dissallow elements that leave nodes very close to the two new edges  
!  - these later give needle like elements. In the worst case the 3 points are co-linear
!.. actualy only 2 of these (the LH and RH size need be considered)
        fmin = min ( fun(1), fun(2), fun(3) )
!       if (abs(fmin).lt.1.e-6) fmin = 0.! If|fmin|< 1.e-6 it is considered 0
!       if (fmin.gt.-tol) then           ! If fmin > -1.e-20 then it is +ve
!       if (fmin.gt.-1.e-2) then
        if (fmin.gt.-1.e-5) then
          ll = ii                 ! Steering for noden updated-->noden=P
          goto 30                 ! jump back
        endif
      enddo   

!-----------------------------------------------------------------------
!     Step 8 : check if MN intersects any existing side in the front
!
!     Method:-
!       Rotate coordinates of segment s.t. MN is parallel to OX
!       Examine ordinates of the ends of the segment in relation
!       to rotated MN
!    ** cf testing all segments against AN *and* BN
!      (in 3d we are testing against the 3 upstanding facets)
!
!-----------------------------------------------------------------------
!  Ok as a subroutine:
! .. maybe should return *which* segment caused the failure?
!    =0 if none
!     call test_front_valid (xca,yca, xcb,ycb, xcn,ycn, front,nfront,ifail)
!
!     if (ifail.ne.0) then
!        ll = ll + 1
!        goto 30   !- try the next node
!      endif

!--------- Compute polar coordinates of MN - origin at M ---------------
!  Yuk! why dont you just use vectors and 0>alpha>1, 0>beta>1
!   (M is the midpoint of AB)
!.. ie the line between point N and the mid-point of AB
!.. don't we have the coords of nodeN explicitly ?
!      tread = gc(1,noden)-xmid       ! Horiz. dist between M and N
!      riser = gc(2,noden)-ymid       ! Vert. dist between M and N
      tread = xcn-xmid       ! Horiz. dist between M and N
      riser = ycn-ymid       ! Vert. dist between M and N
      mn = sqrt(riser**2+tread**2)   ! Length MN
!     ... Local rotation of coords s.t. MN is parallel to OX 
!- this is the unit vector M->N
      cth2 =  tread /mn              !      cos(-th) = cos(th)
      sth2 = -riser /mn              ! but  sin(-th) = -sin(th) 

!-------------- Scan all active segments in the front -----------------
! Notes.
!  2. We could store the direction-cosines of each segment
!     since we access them a lot. (eg see my method for slopes in my PhD.)
!    ( I would rather store the xy of node1 in front(*,jfront) )
!- 3. We do not consider the 2 adjacent segs (or the parent)
!- 4. We also skip the 2 adjacent segs to point N too.
!-     (ie. skip up to 5 of the total ring.)
!- 5. Note we can have the vector of each segment explicitly for speed
!- 6. In 3d, we are looking for a line hitting a triangle

      do jfront=1,nfront                   
        if (jfront==ifront_base) cycle       !- not the base segemnt
        ic_dp = ic_dp + 1              !(timing)
        node1 = front(1,ifb+jfront)        ! the 2 nodes at either end
        node2 = front(2,ifb+jfront)
!    .. cf remembering the (2) segements connected to N
!    .. we will need them later when updating the front
!    (as long as I store one of them, I can follow a pointer to the other)
!-  7. *20-7-97* cf if idelta=+1 or -1 then it cannot possibly cross
!
!
        if (node1.eq.noden.or.node2.eq.noden) cycle   !cycle if adjacent

        vc(1) = gc(1,node1) - xmid     ! Local x,y of active node1 
        vc(2) = gc(2,node1) - ymid
!       vc(1) = front(ifb+jfront)%x(1) -xmid    !- if held here directly
!       vc(2) = front(ifb+jfront)%y(1) -ymid
        vpx   = cth2*vc(1) - sth2*vc(2)
        vpy   = sth2*vc(1) + cth2*vc(2)

        vc(1) = gc(1,node2) - xmid     ! Local x,y of active node2 of front
        vc(2) = gc(2,node2) - ymid
        vqx   = cth2*vc(1) - sth2*vc(2)
        vqy   = sth2*vc(1) + cth2*vc(2)

!-------------------- Does it intersect MN ? --------------------------
!.. must have one of y1,y2 on one side, the other on the other.
!.. Arggh ! we divide by (y2-y1) to get the x-value (cf 'alpha')
!.. hmm maybe I can test for y1*y2<0 earlier?
!  cos if +ve it cannot possibly cross

       if (vpy*vqy.gt.0.) cycle        ! ie. if either y1 or y2 is -ve
          x1 = vpx                     ! store vpx in x1
          y1 = vpy                     ! store vpy in y1
          x2 = vqx                     ! store vqx in x2
          y2 = vqy                     ! store vqy in y2
          xint = x1+((-y1*(x2-x1))/(y2-y1)) ! Local xcoord of intersection
          if (abs(xint-mn) < tol) exit      ! thru node N?
          if (xint-1.e-4.lt.mn.and.xint.gt.0.) then    ! 0<xint<MN
              if (ll.ge.ist) cycle try_a_base        !- exhausted the list ?
              ll = ll+1                 ! update steering for noden such that
              goto 30                   ! Start triangulation process again 
          end if                  ! End of condition: intersection between MN
      enddo   !-- loop all the other edges

!======================== UPDATE EVERYTHING ============================
! This is final substep of the advancing front method.
!  Add the new (node and) element, adjust the front accoringly,
!  then plot new element(s)/front if desired.

!----------- find where the new node lies -----------------
    if (C_at_HEAD.and.ll==1) then
      idelta = 0                   !- nodeN is *new*
    else
      CALL FIND_IDELTA (FRONT,IFRONT,ifb,NFRONT, IFRONT_BASE, &
      NODEN, IDELTA)
    endif

!----------- abort if the ring will split -----------------
!- * we now allow the ring to split. *
!- maybe just loop back with a 'pick another base' flag.
   if (idelta<-2 .or. idelta>2) then
!     call draw_tri (gc(1,num3(1:3,nel)),gc(2,num3(1:3,nel)), 1,ifill) !- blue
!      cycle try_a_base    !- comment this out to allow the front to split
  endif

!---- save the new node ------
   if (idelta==0) then     !- it was a new node...
       nnode = nnode+1     !- so add the new node to the database
       if (nnode>=igc-1) then
         print*,'Maximum number of nodes exceeded (',nnode,') adjust and recompile'
         stop 
       endif
       gc(1,nnode) = xcc   ! (already done this before!)
       gc(2,nnode) = ycc 
   endif

!------------------- save the newly formed element ----------------------
 
      nel = nel + 1          !- cf PUT_ELEMENT
      if (nel>=nel_max) then
        print*,'Maximum number of elements exceeded (',nel,') adjust and recompile'
        stop 
      endif
      num3(1,nel) = nodea     
      num3(2,nel) = nodeb    !- the three nodes. 
      num3(3,nel) = noden 
      num3(4,nel) = imat
      area_remain = area_remain - area_element *(-1)!*(-iflip)
      if (iverb>=3) &
      write(*,'(i7,a,g10.3,g10.3)') nel,':', area_remain, area_element
      if (area_remain/area_f < -0.02) then
        print*, 'Oops: front has hemoraged!'
!       stop
        exit add_elements
      endif

      if (ipl>=3) call plot_front (gc,front,nfront,ifb, 3)     !- green
      if (ipl>=1) then
        size=(-1.*area_element)/area_el         ! fraction of the free field size
        ifill=7                  ! if using an existing node
        if (idelta==0) ifill=9   ! or if a new node was created
  
        ifill=7
        if (size<.4) ifill=10
        if (size<.2) ifill=9
        if (size<.1) ifill=8

        size=(-1.*area_element)/area_f         ! fraction of the total area
!       write(*,'(f12.7)') size

        iedge=0 
        if (size<.00004) iedge=-1

        call draw_tri (gc(1,num3(1:3,nel)),gc(2,num3(1:3,nel)), iedge,ifill)  
      endif

!------------------ update the front ------------------
!  19-10-02  ** NOTE that here we can directly insert as elements those
!      triangles when the point N is that 2 segments away from me.
!   so either :
!     1) Avoid this and just make them turn into simplex 3 edged fronts.
!           this is more rigorous
!     2) As a short cut create them here.on the fly. perhaps faster like this.
!        - probably best to let update_front return the 3 node and we create the exatra
!          triangle here
      allow_create_element= .false. 
      CALL UPDATE_FRONT (front,ifront, ifb,nfront,ifront_base, &
        num3,inum3,nel,imat, &
        nodeA,nodeB,nodeN, idelta,allow_create_element,ifail)
!----  Check the front datastructure ---
!   Here we parse *all* of the segments to check that they all are part of closed rings.
!   
!
      call validate_front (front,ifront,ifb,nfront, ifail)  !- all OK ?

!----- reduce reamaining area by the double triangles too
      if (abs(idelta)==2) then
!        call calc_area (gc(1,num3(1:3,nel)),gc(2,num3(1:3,nel)) &
!         , area_element)
!        area_remain = area_remain - area_element
!        print*, nel,':', area_remain, area_element
      endif

!------- plot 'double' triangles -------
! if happened: abstract the last element & plot it
      if (ipl>=2 .and. abs(idelta)==2) then
        if (idelta==-2) then
!           call draw_tri (gc(1,num3(1:3,nel)),gc(2,num3(1:3,nel)), 1,3) 
        elseif (idelta==2) then
!          call draw_tri (gc(1,num3(1:3,nel)),gc(2,num3(1:3,nel)), 5,6) 
        endif
      endif
      if (nfront==1) then
        print*,'** Oops: front has only one segment left!'
!       stop
        exit add_elements
      endif
!------------------- is front completely shrunk? -----------------------
!        .. hmm how do we ever get a front of only 2 edges ?
      if (nfront<=0 .or. nfront==2) then
         nfront=0               !- zap an y vesitagal '2'
         IF ( ifb== 0 ) exit add_elements     !- no others left?
!        ---- retrive a buffered front ----
         call count_front (front,ifront,ifb, nfront)
         ifb = ifb-nfront         !- reduce the base
!        write(*,*) ' new front length=',nfront
      endif 

!---------- test the size of the remaining  area to be filled ---------
      if (area_remain/area_f < -0.02) then
        print*, 'Oops: front has hemoraged!'
        exit add_elements
      endif

      if (ipl>=4) &
      call plot_front (gc,front,nfront,ifb, mod(nel,12))      !-- optional plot of the current

!--- interuption ---
! nice to be able to hit <esc> say, which causing an exit
! (so can plot partial meshes)

      kk=0
!      call dynt@('get_key1@',ihandle)        !- does this routine exist?
!      if (ihandle.ne.0) call get_key1@(kk)  
      if (kk.eq.27) exit add_elements
!     if (nel.eq.495) exit add_elements   !- ** 27-1-98  hack ***

!-----------------------------------------------------------------
      cycle add_elements       !- was successful
!-----------------------------------------------------------------
     enddo try_a_base

     print*,'** failed to find a base to build on'
!    stop
     exit add_elements

!-----------------------------------------------------------------

    enddo add_elements        !- the OUTER loop

!---------------------- all finished -----------------------------
!.. or at least this front is finished 
! ( now see if there are any other fronts waiting to be done)

!------------ exit graphics ----------------
      if (ipl>=1) then
!        call get_key@(kk)  
!       call text_mode@ ()
      endif

!------------- write summary table -------
      if (iverb>=1) &
      write(*,'(a,g15.4,g15.4,f8.2,a)') &
       'original,final area,%left=', area_F, area_remain, area_remain/area_f*100.,' %'

!  for general release, we do not need to report these to the user.
      if (iverb>=3) then
        write(*,*) '<> Nel=',nel,' NN=',nnode
        write(*,*) '<> # loop_back to line 30''s  :  ic_lb30  =',ic_lb30
        write(*,*) '<> # dist. from point N tests :  ic_ds    =',ic_ds
        write(*,*) '<> # is point in triangle tests: ic_intri =',ic_intri
        write(*,*) '<> # vector-cross tests      :   ic_dp    =',ic_dp
        write(*,*) 
        write(*,*) '<> # LU-factorizations       :   ic_lupfac   =',ic_lupfac
        write(*,*) '<> # LU-solutions            :   ic_lupsol   =',ic_lupsol
      endif
      return

      end subroutine advancing_front            !- of ADVANCING FRONT
!-----------------------------------------------------------------------



!-----------------------------------------------------------------------
!     3/ Tools for handling the FRONT datastructure
!-----------------------------------------------------------------------
      subroutine ring_to_front (ring,nring, front,ifront, ifb,nfront) 
!
!     Simply creates the initial front from the list of nodes in RING()
!      called from ADVANCING_FRONT
!         Dan Kidger 18-3-96
! Notes:
!     1/ nice to use GC to create a set of edge lengths (and D.C's) too.
!     2/ use put_segment() hence store D.C's etc.
!
      implicit none
      integer nring,ifront,ifb,nfront
      integer ring(nring), front(2,ifront)
      integer i,j

      do i = 1,nring
         j = mod(i,nring)+1        !- the 'next' node/segment
         front(1,ifb+i) = ring(i)  ! 13-10-97 ifb is the start-point
         front(2,ifb+i) = ring(j)  ! (eg. if multiple rings
!        -- now calculate derivatives eg. segment lengths
      enddo
      nfront = nring
      return
      end subroutine ring_to_front

!-----------------------------------------------------------------------
      subroutine count_front (front,ifront,ifb, nfront)
!
!     Simply returns the length of the current front
!     (given the last segment: ifb)
!         Dan Kidger 15-10-97
!
      integer :: ifront,nfront,ifb,front(2,ifront)    !- the front segments


!     print*,'(now) ifb,nfront=',ifb,nfront
!     do i=1,ifb+nfront
!       write (*,'(A,3i6)') '-- ',i, front(1,i),front(2,i)
!     enddo

      ifb1=0        !- offset
      do ii=1,9999
        i= ifb+1-ii
        if (i<1) stop 'front has no end (COUNT_FRONT)'
        ileft =  front(1,ifb1+i)
        iright = front(2,ifb1+i)
        if (ii==1) then         !- first time
          iroot = iright
        else
          if (iright/=ileft1) & !--- doesn't follows last ? ---------
            stop 'front is broken (COUNT_FRONT)'

          if (ileft==iroot) then    !---- closed the loop? -----
            ifb1 = ifb1+i            !- update the base pointer
            nfront=ii
            return
          endif
        endif    
        ileft1=ileft     !- remeber this
      enddo

     end subroutine count_front

!-----------------------------------------------------------------------
      subroutine validate_front (front,ifront,ifb,nfront,ifail)
!
!    This validates the current front(s) by checking that:
!      1/ the node# at the LHS matches the node# at the RHS of the next
!      2/ no node appears twice
!      3/ the 'pointers' point both ways correctly
!    It returns:
!        ifail  = 0 if all is ok
!               = -1 if a pair of adjacent segments do not have share the same node number
!               = -2 if I can cycle the whole of the remaining front and not get back to 
!                   my starting node.
!        nfront = the number of closed loops. (aka.fronts)
!        We could also return a pointer to the longest (or shortest) front and its length
!         Dan Kidger 20-9-97   (19-10-02)
!    Notes:
!    1. 13-10-97 This needs reworking if there are multiple fronts.
!
      integer :: nfront,front(2,ifront)    !- the front segments

      ifail = 0
      ifb1 = 0        !- offset
      loop_fronts : do nfronts=1,9999       !- Max possible # of fronts.
         do i=1,ifront                      !- really I mean infinity here 
!                                           !- (but it can't ever exceed the struct size)
           if (ifb1+i > ifb+nfront ) stop ' front not closed!'
           ileft =  front(1,ifb1+i)
           iright = front(2,ifb1+i)        !- wrap at the ends
           if (i==1) then         !- first time
             iroot = ileft
           else
             if (ileft/=iright1) then  !--- doesn't follow last ? ---------
               print*,nfronts,ifb1,'segments not consecutive'
               ifail=-1
               exit loop_fronts
             endif

             if (iright==iroot) then    !---- closed the loop? -----
!               note 'i' = the length of this closed loop
               ifb1 = ifb1+i            !- update the base pointer
               if (ifb1+i > ifb+nfront ) exit loop_fronts
               cycle loop_fronts
             endif

           endif    
           iright1=iright     !- remember this so check next segment matches,
         enddo
         print*,'segments unterminated'
         ifail=-2         !- only here if we couldn't find an end
         exit
      enddo loop_fronts

!      if (nfronts>1) print*,' There are currently',nfronts,' fronts'
      if (ifail/=0) stop 'Front is broken'
     return
     end subroutine validate_front

!-----------------------------------------------------------------------
     subroutine get_segment (front,nfront, ifront, gc,igc,nn,  &
    &   nodea, xca,yca, nodeb,xcb,ycb, cth,sth, ab,ab2 )
!
!   This returns information about the given segment IFRONT
!   where: (xca,yca), (xcb,ycb) are the x,y of the two ends =(nodea,nodeb)
!          (cth,sth) are the direction cosines of the edge
!          (ab,ab2) is the edge length and its square.
!   Called from ADVANCING_FRONT
!      Dan Kidger 16-9-97
!   Notes:
!     1/ in F90 it is nicer to return as a derived type, eg: a.x, a.y
!     2/ in 3D we return 3 points, the area and the *normal* vector
!     3/ if mulitple fronts then we pre-add the offset to ifront.
!
      implicit none
      integer :: ifront, igc,nn, nodea, nodeb
      integer :: nfront,front(2,nfront)         !- the front segments
      real :: gc(igc,nn)                        !- all the nodal coordinates
      real :: xca,yca, xcb,ycb, cth,sth, ab,ab2 !- returned data

      real :: dx,dy

      nodea = front(1,ifront)
      nodeb = front(2,ifront)
      xca = gc(1,nodea)         !- Coordinates of nodes A & B
      yca = gc(2,nodea)
      xcb = gc(1,nodeb)
      ycb = gc(2,nodeb)         !(ia,ib are not used again)
!     print*, 'nodea,nodeb=', nodea,nodeb, ' ifront=',ifront
!     type (point) :: a,b,mid,c,n,m
!     a.x = gc(1,nodea)
!     a.y = gc(2,nodea)
!     a.z = gc(3,nodea)
!     a.n = nodea
! or  a=point(/gc(1:3,nodea),ia/)

!-- abstract the base segments' coords and length
!   hmm maybe I do not need xca etc. yet - just the mid-edge point
!      nodea = base%n(1)
!      nodeb = base%n(2)
!      xca = base%x(1)
!      yca = base%y(1)
!      xcb = base%x(2)
!      ycb = base%y(2)       !- plus 'z' and a 3rd point if 3d.
!      ab  = base%len
!      cth = base%dc(2)      !- 90deg clock to the normal
!      sth = -base%dc(1)

      dX = xcb-xca
      dY = ycb-yca
!---- abstract this segment's length and direction ----
      ab2  = dX*dX + dY*dY          ! its square is useful later
      ab   = sqrt(ab2)
      cth = dX /ab
      sth = dY /ab
!--- optional calc. of the mid-base point
!     xmid = (base%x(1)+base%x(2)) /2.
!     ymid = (base%y(1)+base%y(2)) /2.
!     zmid = (base%z(1)+base%z(2)+base%z(3)) /3.    !- for 3d.

     end subroutine get_segment

!-----------------------------------------------------------------------
     subroutine find_point_C (xca,yca, xcb,ycb, cth,sth,ab,iflip,  &
   &   xcc,ycc,AC)
!
!    This finds point C = the apex of the triangle ABC that has the target
!    nodal spacing. Sampling along the line MC is used where M=midpoint AB
!       Dan Kidger 16-9-97
! Notes:
!   1: cf any other method for creating a 'new' point:
!     -> Delauney has no new points)
!     -> pure equilateral triangles is just point C so AC=BC = AB
!   2: This is only point where we look at the old mesh/ desired density
!   3: hmm interesting to know what the average number of iterations needed is.
!      - converged when point moves by <1% of AB
!      - to avoid a SQRT just use an x-error and a y-error?
! 
    implicit none
    real :: xca,yca, xcb,ycb, cth,sth,ab, xcc,ycc,AC
    integer :: iflip

    integer :: imat, iter
    real :: xpos,ypos, delta, xytol
    real :: xmid,ymid, xc_l,H, xold,yold

      xmid = .5*(xca+xcb)               !- mid-point of AB
      ymid = .5*(yca+ycb)

      xpos = xmid              !- start here.     
      ypos = ymid
!     zpos = zmid

      iterations: do iter=1,10                       !- 10 = arbitary iteration limit.
        call get_spacing (xpos,ypos,delta,imat) 
!       ---- clip this edge to a min of .55AB and a max of 2AB ---
        AC = min(max(.55*ab,delta),2.*ab)  
!       --- Construct point C which is a distance AC from A and B --
!     .. ie AC from both A and B .. an isoceles triangle'
! for equilateral triangles yc_l = 0.866 * ab
        xc_l = .5*ab
        H = sqrt (AC*AC - xc_l*xc_l) * iflip   !-'altitude'

!.. hmm can also construct point C by the normal vector from the mid-edge
! for speed we only need 

!       xcc = xca + cth*xc_l - sth*yc_l      !-- hence global coord
!       ycc = yca + sth*xc_l + cth*yc_l      !-- of this point C
        xcc = xmid + H * (-sth)              !- 'up' the edge *normal* vector
        ycc = ymid + H * cth

        xold = xpos                    !- save this sampling point.
        yold = ypos
        xpos = (xmid+xcc) /2.          !- new sampling position is midway 
        ypos = (ymid+ycc) /2.          !- 'up' the triangle (why not one third?)
!  hmm why do we watch the change in samp-point, NOT the change in point C ??
        if (iter.gt.1) then            !- must do at least 2 iterations.
           xytol = (sqrt((xpos-xold)**2+(ypos-yold)**2))/ab
           if (abs(xytol).lt..01) exit     !goto 91   
        end if
      enddo iterations                           !-- loop iterations
!  91 continue      !- EXIT jump

      return
      end subroutine find_point_C

!-----------------------------------------------------------------------
     SUBROUTINE FIND_CLOSE_C (front,ifb,nfront,GC,igc,NN, &
   & nodea,nodeb,XCC,YCC, five_ab,SORTN, IST, ic_ds)
!
!    This returns the list SORTN of points up to 5*AB away from point C
!    The list is returned in ascending order of distance from C
!       Dan Kidger  16-9-97
!  Notes:
!     1: I only need to find the ones that are < 5AB away.. so can test if DX
!        or DY is too big, before we need a square_root
!     2: Loop all the other segments and calc distance to C (xcc,ycc)
!         (just need to do one end of each segment)
!     3: Really we loop all segments but skip the nodes A and B, cos I 
!        am actualy looping nodes in the front?
!     4: If the front has split in two then only need to do 'this' ring.
!     5: Good to return point C in the list too - maybe -ve if we *dont*
!        want it, else NN+1
!
      real :: gc(igc,NN),xcc,ycc
      integer :: front(2,*)
      integer  SORTN(*)         !- the returned list of nodes
      integer,parameter :: max_front = 20000 
      real    :: distances (max_front)
      integer ::  keepn (max_front), indx(max_front)

!     five_ab  =    5.*ab              !- used to test closeness.
      five_ab2 = five_ab**2            !- faster :-)
!.. raly the next test should occur within the loop (based on ist)
      if (nfront>max_front) stop 'workspace too small (FIND_CLOSE_C)'
      ist = 0                          ! count nodes as we find them (i_store?)
      do i=1,nfront                    ! scan the whole front
        ic_ds = ic_ds + 1              !(count for timings)
        noded = front(2,ifb+i)             ! Recover active node N (why call it D?)
        if (noded==nodea .or. noded==nodeb) cycle !- we dont want these two

        xd = gc(1,noded)-xcc           ! active node coords. relative to C.
!       xd = front(i)%x(2)             !- somehow mark nodes that have been done?
        if (abs(xd).gt.five_ab) cycle  ! reject immediately

        yd = gc(2,noded)-ycc           ! Local N (xn,yn)
!       yd = front(i)%y(2)
        if (abs(yd).gt.five_ab) cycle     ! reject immediately

        cn2 = (xd*xd + yd*yd)   
!     12-3-96 DJK - try testing cn^2 rather than taking lots of square_roots
        if (cn2.lt.five_ab2) then        ! store active node N if CN<5AB

!   2-10-97 calc ABN - reject this point immediately if -ve.
!      (or almost co-linear too?)
!     ABN = (xcb*ycn-ycb*xcn) -(xca*ycn-yca*xcn) +(xca*ycb-yca*xcb) !-area
!     IF (ABN*IFLIP<=0.) CYCLE

           ist = ist+1                   ! Qualifying active node counter
           keepn(ist) = noded            ! Store node N (D)
!  good to store its *minimum* #segs from the base, hence we know where
!  to find it afterwards
           cn = sqrt(cn2)                ! Length CN
           distances(ist) = cn              ! save its distance too (so can sort)
        endif
      enddo                              ! Continue scanning the front

!-----  Sort the above nodes according to their distance from C --------
!     write (29,'(A,i5,a,i5,a)') 'Nel=',nel,' Sorting', ist,' numbers'
      if (ist==0) then
        print*, '** No points found within 5*AB'
        return
!       stop
      endif
!     call indexx (ist,distances,indx)    !s.t. distances(indx(1))<distances(indx(2))
      call index_reals (distances,ist,indx)    !(DJK - 'Modified Quicksort' 
!      .. from 'Numerical Recipes' .. faster but not by much. :-(

! OK make SORTN(1) = nodeC
! so 2:ist++
!  later 
      do i = 1,ist                    ! Scan all qualifying active nodes
         sortn(i) = keepn(indx(i))    ! keepn(indx(1)) is nearest to C  
!         if (sortn(i)==0) stop 'oops2'
      enddo                           ! sortn(1) = nearest node to C
!    print*,'-sortn(1:6)=', sortn(1:6)

!- put node C at the head
!   CN2 = (gc(1,sortn(2))-xcc)**2 + (gc(2,sortn(2))-ycc)**2
!     sortn(1) = 0
!     if (cn2<five_ab) then
!       sortn(1) = nnode+1
      RETURN
      END subroutine find_close_c

!--------------------------------------------------------------------------
    SUBROUTINE FIND_IDELTA (FRONT,IFRONT,ifb,NFRONT, IFRONT_BASE, NODEN,IDELTA)
!
!     This alternately scans the front to the left and right of IFRONT_BASE
!     looking for the node NODEN
!        Dan Kidger 17-9-97
!   Notes
!    1: So idelta is how far away the point is.
!         if |idelta|=1 then a 'dart' will need to be removed
!         if |idelta|=2 then an 'extra triangle' will be formed
!         if |idelta|>=2 then the front will split
!    2: We could have saved this information beforehand when we first found 
!       nodeN
!    3: If NODEN is not found should we return ifail ?
!    4: If the front becomes disjointed (eg if split) we must follow pointers
!       around cos jfront+1 is not necessarily connect to jfront
!
    integer :: ifront,nfront,ifront_base, noden, idelta
    integer front (2,ifront)

!- think in terms of isegleft and isegright so a left step is:
!     isegleft = front(isegleft)%left
!     isegright = front(isegright)%right

    idelta=1
    do ii = 1,nfront                !- still valid if the front is split.
      i=ifront_base+idelta            !- the segment to sample
      if (i>nfront) i=i-nfront        !- wrap
      if (i<1)    i=i+nfront          !- around
      if (front(2,ifb+i)==nodeN.or.front(1,ifb+i)==nodeN) exit    !- found it
      if (idelta>0) then              !-- update the search
        idelta = -idelta
      else
        idelta = abs(idelta)+1
      endif
      if (ii==nfront) stop 'oops: nodeN not found in the front!'
    enddo
!   write (29,'(i5)') idelta
    RETURN
    END SUBROUTINE FIND_IDELTA

!------------------------------------------------------------------------
      SUBROUTINE UPDATE_FRONT (front,ifront, ifb,nfront, ifront_base, &
        num3,inum3,nel,imat, &
        nodeA,nodeB,nodeN, idelta,allow_create_elements,ifail)
!
!     This updates the FRONT datastructure after a new element has been formed
!       Dan Kidger 28-9-97
!  Notes:
!     1/ In the past, we did not let the front split. so idelta must lie
!        within -2 to +2 )
!        One day I will allow the front to split
!     2/ 15-10-97 I am now on the verge of allowing the front to split
!
!   My Method:
!     1/ insert the new node N to the ring
!     2/ test if N is a truely new node -> if so all is OK
!     3/ test if N is on the LHS, so collapse the 2 touching segments
!     4/ test if N is on the RHS, so collapse the 2 touching segments
!        ( if both then we handle 'double triangles' directly. )
!     5/ else N is 'on the far side' so split ring at N
!        (method = cut ring at N on the new edge, then loop L and R
!         and look for N again .. hence join the free ends.
!         we can record the #segments in this ring if desired
!         .. hence can just loop these next time ?
!
!   21-12-96 re. ring-spliting: to think about
!      what about counting how many nodes to the left or right is N
!      so if 1 left (or 1 right act accordingly)
!      else take the smallest piece and dump to disk (as a stack)
!        closing up the big piece.
!        (so when 'finished' need to check for any dumped rings)

!    in Tetrahedral terms, we 
!       1/ add N to the ring giving 3 new facets
!       2/ loop these and test each for touching (is N on its toucher
!       3/ if on the 'far side' - ignore until we have 2 on the far side ?
!           True bisection of the ring is very complex to detect

      integer front (2,ifront), &
!             num3(10,inum3)
              num3(inum3,*)
      logical :: allow_create_elements   !- when idelta=+/-2 can we creat the 
                                         ! corner elemetn here?
      ifail = 0  !- assume all is OK
!------------ handle the different ways to update the front -------------

!------------ D: Double triangle to the right ---------------
!    This leaves a triangle that we can directly use.
!  * The same can be achieved more generaly by allowing the front to split
       if (idelta==-2 .and. allow_create_elements) then
        ifront_left= ifront_base-1        ! so replace ifront-1 and zap iseg_base
        if (ifront_left==0) ifront_left=nfront
         nel = nel+1
         nodeQ = front(1,ifb+ifront_left) 
         num3(1,nel) = nodeN           !- add a new 'extra' element 
         num3(2,nel) = nodeQ
         num3(3,nel) = nodeA    
         num3(4,nel) = imat

         front(:,ifb+ifront_base)   = (/nodeN,nodeB/)            !-

         do i=ifront_left,nfront-1     
           front (:,ifb+i) = front(:,ifb+i+1)              !- kill ifront_left
         enddo
         nfront = nfront-1
!.. yes but ifront_base might get itself get moved by the previous?
    if (ifront_left < ifront_base) ifront_base = ifront_base-1

        ifront_left= ifront_base-1       
        if (ifront_left==0) ifront_left=nfront
         do i=ifront_left,nfront-1     
           front (:,ifb+i) = front(:,ifb+i+1)              !- kill ifront_left
         enddo
         nfront = nfront-1


!-------- A: 'dart' on the left ---------
      elseif (idelta==-1) then
        ifront_left= ifront_base-1        ! so replace ifront-1 and zap ifront_base
        if (ifront_left==0) ifront_left=nfront
        front(:,ifb+ifront_base)   = (/nodeN,nodeB/)            !- 
        do i=ifront_left,nfront-1     
          front (:,ifb+i) = front(:,ifb+i+1)              !- kill ifront_left
        enddo
        nfront = nfront-1
!       call validate_front (front,ifront,ifb,nfront, ifail)  !- all OK ?

!--------- B: a genuinely new node ---------
    elseif (idelta==0) then
!      nnode = nnode+1     !-- add the new node to the database
!      gc(1,nnode) = xcc   !(already done this before)
!      gc(2,nnode) = ycc 
!... store *at_least* the 2 node#s - it may store length,DCs etc. too?
!     call put_segment (ifb+ifront_base,  nodeN,nodeB )
      front(:,ifb+ifront_base)   = (/nodeA,nodeN/)  !- a no-op!
      nfront=nfront+1
!      front(:,ifb+ifront_base+1:ifb+nfront-1)  &
!        = front(:,ifb+ifront_base+2:ifb+nfront)
       do i=nfront,ifront_base+2,-1      !- make space for the (other) new segment
         front (:,ifb+i) = front(:,ifb+i-1)
       enddo

     front(:,ifb+ifront_base+1) = (/nodeN,nodeB/)  !- the new segment
!     call validate_front (front,ifront,ifb,nfront, ifail)  !- all OK ?

!----------- C: 'dart' on the right -------------
!- so update ifront_base to AN, then kill ifront_right
    elseif(idelta==1) then
       ifront_right = mod(ifront_base,nfront)+1 !- the next segment cyclicaly --
       front(:,ifb+ifront_base)   = (/nodeA,nodeN/)            !- a no-op!
       if (front(2,ifb+ifront_right).ne.nodeN) stop 'oops not nodeN #1'
       do i=ifront_right,nfront-1          !- kill ifront_right
         front (:,ifb+i) = front(:,ifb+i+1)
       enddo
       nfront = nfront-1

!------------ D: Double triangle to the right ---------------
!    This leaves a triangle that we can directly use.
!  * The same can be achieved more generaly by allowing the front to split
    elseif (idelta==2 .and. allow_create_elements) then
      ifront_right = mod(ifront_base,nfront)+1 !- the next segment cyclicaly --
       nel = nel+1
       nodeQ = front(2,ifb+ifront_right) 
       num3(1,nel) = nodeB           !- add a new 'extra' element 
       num3(2,nel) = nodeQ
       num3(3,nel) = nodeN    
       num3(4,nel) = imat

       front(:,ifb+ifront_base)   = (/nodeA,nodeN/)  !-

      ifront_right = mod(ifront_base,nfront)+1
       do i=ifront_right,nfront-1           !- kill ifront_right
         front (:,ifb+i) = front(:,ifb+i+1)
       enddo
       nfront = nfront-1
!.. yes but ifront_base might get itself get moved by the previous?
        if (ifront_right < ifront_base) ifront_base = ifront_base-1

        ifront_right = mod(ifront_base,nfront)+1
        do i=ifront_right,nfront-1           !- kill ifront_right
          front (:,ifb+i) = front(:,ifb+i+1)
        enddo
        nfront = nfront-1
!       call validate_front (front,ifront,nfront, ifail)  !- all OK ?


!----------- E: ring splitting -----------
!  Methods
!  1: Push either the smaller or larger fragment onto a stack
!  2: Update *4* pointers - ie. from 1<->2, 3<->4 to 1<->3 and 2<->4
    else

!---------- p1: build the two new edges -----------
      front(:,ifb+ifront_base)   = (/nodeA,nodeN/)
      nfront=nfront+1
      do i=nfront,ifront_base+2,-1      !- make space for the (other) new segment
        front (:,ifb+i) = front(:,ifb+i-1)
      enddo
      front(:,ifb+ifront_base+1) = (/nodeN,nodeB/)  !- the new segment

!---------- p2: some numbers -----------
    if (idelta>0) then
      nright=abs(idelta) +1   !- (cos we have added edge NB)
      nleft=nfront-nright
    else
      nleft=abs(idelta)   +1
      nright=nfront-nleft
    endif
    nsmaller = min (nleft,nright)

!---------- p3: rotate the front  -----------
! such that the smaller piece is below the larger
!    so IFRONT_BASE (AN) is at position NFRONT; (NB) at pos #1 
!    new list is IFRONT_BASE:end + 1:IFRONT_BASE-1 
     do i=1,ifront_base
       CALL ROTATE_FRONT (FRONT,IFRONT, IFB, NFRONT, -1)
     enddo
!----------- p4: now simply raise the base ----------
!  Simply raise the pointer above the stored (inactive) segments
!  (actual no. is implicitly stored!)

!     print*,'Nodes A,B,N =', nodeA,nodeB,nodeN, &
!     ' idelta=',idelta
!     print*,'(old) ifb,nfront=',ifb,nfront

     ifb = ifb + nright
     nfront = nfront - nright
!     print*,' front split into',nright ,'+', nleft
!     print*,'(new) ifb,nfront=',ifb,nfront
!     do i=1,ifb+nfront
!       write (*,'(3i6)') i, front(1,i),front(2,i)
!     enddo

!-------- cases of idelta ---------
    endif
  END SUBROUTINE UPDATE_FRONT

!---------------------------------------------------------------------
      SUBROUTINE ROTATE_FRONT (FRONT,IFRONT, IFB, NFRONT, ISTEP)
!      *Obsolete*?
!     Rotates the list of segments by istep steps 
!      ie istep = -1 puts (2) into (1) etc.
!            DJK 1996
!    Caveat:
!       doesn't work for rotations the other way.
!
!
      integer ip,iq,itran
      integer :: front(2,ifront)

!     print*,' <> Abandon AB and start over :-( .. nfront=',nfront
      if (istep.ne.-1) stop 'Dont know how to rotate FRONT by this step:-('
      ip = front(1,ifb+1)            !- save the first segment
      iq = front(2,ifb+1)
      do itran=2,nfront          ! Shunt active segment list (front) to left
         front(1,ifb+itran-1) = front(1,ifb+itran)
         front(2,ifb+itran-1) = front(2,ifb+itran)
      enddo                    ! continue operation with remaining segements
      front(1,ifb+nfront) = ip
      front(2,ifb+nfront) = iq         ! This becomes new base
      return
      end subroutine rotate_front

!-----------------------------------------------------------------------
      subroutine get_mesh_extent (gc,igc, nn) 
!
!   Simply calculates the Bounding Box of the mesh
!    .. hence the plotting scale (from DANLIB)
!
      use plotlib          !- ixo,iyo (or pass as arguments?)

      REAL GC(2,IGC)
!     .. statement functions ..
!     fx(i) =           IRESX * (FACT+R*(GC(1,I)-XMIN) /DATAX)
!     fy(i) =   IRESY - IRESX * (FACT+R*(GC(2,I)-YMIN) /DATAX) 

!      iresx = 640     !- explicit graphics mode.
!      iresy = 480
      iresx = ixres     ! from the COMMON block
      iresy = iyres
      AR = REAL(IRESX) / REAL(IRESY)       

!      print*,' ar=',ar
!------- find the bounding box ----------
      xmin = minval (gc(1,1:nn))
      ymin = minval (gc(2,1:nn))
      xmax = maxval (gc(1,1:nn))
      ymax = maxval (gc(2,1:nn))
!     xscale = max (xmax-xmin, ymax-ymin)

      dx = xmax-xmin
      dy = ymax-ymin
      write (29,'(A,2(a,2g12.3))') 'Data range:', &
     &  'x-min/max=', xmin,xmax,' y-min/max=', ymin,ymax

!---------------------- normalize the data -----------------------------
      FACT = 0.05         !- shrink factor
      FACT = 0.           !-  shrink factor
!     FACT = -0.05         !- shrink factor
      R = (1.-2.*FACT)    !- resulting size
      DATAX = MAX (DX,DY*AR)   !- scale factor

!.. these next few lines are still wrong !
!     xmag =     r * iresx * (xmax-2*xmin) / datax
      xmag =     r * iresx / datax
!     xmag =       iresx * (fact+r*(xmax-xmin-xmin) / datax)
      ymag =       xmag  !*(-1.)
      ixo =       iresx*(fact/2.-xmin/datax)
!     iyo = iresy -iresx*(fact/2.-ymin/datax)
      iyo =        iresx*(fact/2.-ymin/datax)

      END subroutine get_mesh_extent

!END MODULE Amr

!======================================================================


!-----------------------------------------------------------------------
!    4: Routines for handling the desired element size distribution
!-----------------------------------------------------------------------
!module attractors
     subroutine read_attractors (io,iverb)
!
!   This reads in the 'Densification' functions for Mesh generation. These
!   control the size of each generated edge segment and triangle.
!        c. D.Kidger 8-7-97

!    Notes:
!    1/ Here we simply store the params in a common block for HACKED_DSEP to use.
!    2/ Currently this may be one or more of:
!       a: Point source  (eg at internal angles
!       b: Line source   (eg slip planes
!       b: Arc source    (eg slip circles
!       c: A bitmap      (eg a 'YinYang' symbol)
!    3/ Each source has a 'strength' which controls the spacing at its centre
!    4/ Most also have a 'radius of influence' too.
!    5/ Ths bitmap is stored in seperate character*1 array

!     Data Format:
!        edge_basic = coarse edge length for the main mesh
!        edge_fact_finest = smallest allowable element (as a fraction
!                                     of the largest elements)
!        then for each feature there is:
!          (1) type - eg a point-source, line, arc, bitmap ,..
!          (2) 'strength'- 
!         (3-6)  position of the feature, and its size

!< should realy convert COMMON into MODULE >
      common /hacked_dsep/spacing_lo, ratio_max ,patvals(8,40),npats
      CHARACTER BITMAP_NAME*60         !- filename for an image
      integer ibx,iby                  !- bitmap size.
      character pattyp*10              !- 'point','line','bitmap'
      integer patcode

!---- 1: overall mesh control ----
!- read 'standard' mesh density (eg. 0.1m) and max density (eg. x6)
      read(io,*) spacing_lo, ratio_max

!---- 2: local refinements ----
      read (io,*) npats            !- # of refinement (often =0 or 1)
      do ipat=1,npats

        read(io,*) pattyp, ratio, ncp,(patvals(3+i,ipat),i=1,ncp)
!
!   o=none, 1=hacked vert, 3= another vert, 4=circular slip
        select case (pattyp)
        case('point')
          patcode = 6
        case('line')
          patcode = 7
        case ('arc')
          patcode = 4
        case ('bitmap')
          patcode = 10
!         -- needs ncp=4
!         xlo =  0. ; xhi = 1. ; ylo = 0.; yhi = .3     !- for 'Danmesh'
!         xlo = -1. ; xhi =+1. ; ylo =-1.; yhi = +1.    !- for 'gear-wheel'
        case ('basemesh')
          patcode = 20
        case default
          patcode = 0
          print*,'*** WARNING: unknown pattern type - ignored'
        end select
        patvals(1,ipat) = patcode
        patvals(2,ipat) = ratio      ! 'strength' of this feature
        patvals(3,ipat) = ncp

!----- read extra data for bitmaps ----
        if (pattyp.eq.'bitmap') then
          read (25,*) bitmap_name, ibx,iby    !- .PCX filename + image size
          call read_backg_pcx (bitmap_name, ibx,iby) !- so store in COMMON
        endif
        if (iverb>=2) &
        print*,ipat,' :', pattyp,ratio,ncp

      enddo
      if (iverb>=2) &
      print*,'<> ',npats,' densification function(s) read in'

      end subroutine read_attractors 

!--------------------------------------------------------------------
      subroutine get_spacing (x,y,spacing,imat)
!
!     For a given x,y point: this returns the mesh-spacing to use: SPACING
!         Dan Kidger 15-3-96
!  Notes:
!   1: Some mappers change the IMAT too.
!   2: maybe I can extend this to use a background mesh?
!       < ie. in F90: USE OLD_MESH >
!   3: 28-2-97 new unified approach -
!    Allow a set of polylines/arcs: the same as that used to define the 
!    boundary. (but need to add a 'width' factor)
!    loop each segment and call 'distance_to_edge' routine
!    This returns 'd' the distance of a point from this feature
!    Note that lines have optional circular end caps.
!     circles are simple: general arcs are harder.

!   Algorithm (7-7-97)
!     - define a default background spacing. (eg 0.25)
!     - define an upper maximum spacing ration (15)
!     - each feature has a strength (spacing_ratio)
!     - sum all this in but clip to the peak ratio)
!     - So loop the features: 
!         for each: get the params
!         1: Point Source:
!            compute 'R', hence if R<r0 get fact -> scale
!         2: Line Source:
!            compute dist_to_line: get beta if 0.<beta<1. is not true :skip
!            else like point source
!         3: Polyline:
!            treat as a set of line segments
!         4: Arc:
!            Compute 'R', then distance from Ro
!            hence usual.  But note we can use theta to clip
!         5: *Bitmaps*
!            we need: the bitmap itself & its pixel size
!            and its xo,yo and xl,yl extents (rotation too?)
!            if x,y is not within the box then clip (cf tiling)
!            then turn x,y into xi,eta : hence pixle sample
!         6: Background Meshes
!            Realy need a seperate structure : but note that we may still have
!            an Xo,Yo, xl,yl, so that we can clip??

      common /hacked_dsep/spacing_lo, ratio_max, patvals(8,40),npats

!-----------------------------
!       rat = 0.          !- default density (0.-1.)
!       rat = spacing_lo       !- default density (0.-1.)
        rat = 1.          !- or should it be this ?
       do ipat = 1, npats

        patcode = patvals(1,ipat)      !- 0=none, 6=point, etc.
        ratio   = patvals(2,ipat)      ! 'strength' of this feature
!       ncp     = patvals(3,ipat)

!====================== choose which mapping we want ======================
!------------- 0: null case - all background ----------
       if (patcode.eq.0) then
!        rat = 0.

!-------------------------- 1: a vertical band --------------------------
!       elseif (patcode.eq.1) then
!         rat = 0.
!         if (xi.gt. .4 .and. xi .lt. .6)  d=1.


!--------------------- 4: circlar failure surface -----------------------
       elseif (patcode.eq.4) then 
         xc = patvals(4,ipat)
         yc = patvals(5,ipat)
         rc = patvals(6,ipat) 
         width = patvals(7,ipat)       !(eg 0.05m)
         r = sqrt ((xc-x)**2 + (yc-y)**2)    
         p = 1.-abs(r-rc) / width      !                           __
!        p =    abs(r-rc) / width      !                           __
!        if (p .gt.0 ) d = 1.          !- constant width band   __|  |__
!        ratio = max(width - abs(r-rc),0.)
         d = min(max(0.,p),1.)         !- 'tent' band __/\__

!--------------------- 6: point source ('cone') -----------------------
!.. the same as an arc of zero radius?
       elseif (patcode.eq.6) then 
         xc = patvals(4,ipat)
         yc = patvals(5,ipat)
!        rc = patvals(6,ipat) 
         width = patvals(6,ipat)       !(eg 0.05m)
         r = sqrt ((xc-x)**2 + (yc-y)**2)    
         rc =0.
         p = 1.-abs(r-rc) / width      !                           __
!        p =    abs(r-rc) / width      !                           __
         d = min(max(0.,p),1.)         !- 'tent' band __/\__

!--------------------- 6: line source -----------------------
!.. Algorithm:
!     1: find the point Q on the line segment that is nearest point P
!          q = (c + N.P)/(N.N)    <Graphics Gems p10>
!      ie. q = Lc + (Ln.P); Q = P-q Ln
!     cf get alpha along line, clip to 0>=alpha>=1. hence Q
!     2: hence d= distance PQ
!     3: so linear 'clipped' function =1. on line-> 0. at distance 'thick'

       elseif (patcode.eq.7) then 
         x1 = patvals(4,ipat)       !- the two ends of the line
         y1 = patvals(5,ipat)
         x2 = patvals(6,ipat)
         y2 = patvals(7,ipat)
         width = patvals(8,ipat)       !(eg 0.05m)
!.. get the vector of this line
         dx = x2-x1
         dy = y2-y1
!.. hence its Normal vector
!. now need to find the distance that we are from this line
!. if 0>beta>1  then all is OK - else off the ends so treat as 2 cones?

!        p = 1.-abs(r-rc) / width      !                           __
!        if (p .gt.0 ) d = 1.          !- constant width band   __|  |__
!        ratio = max(width - abs(r-rc),0.)
!        d = min(max(0.,p),1.)         !- 'tent' band __/\__

!----------------------- 10: pixmap sampling -----------------------------
       elseif (patcode.eq.10) then 
         xlo = patvals(4,ipat)
         ylo = patvals(5,ipat)
         xhi = patvals(6,ipat)
         yhi = patvals(7,ipat)
         xi = (x-xlo) / (xhi-xlo)      !-- turn x,y into local coords
         eta= (y-ylo) / (yhi-ylo)      !  (invert y ?)
         call sample_pixel (xi,eta, p)
         d = 1.-p               !- invert 'cos white background=255='lo'
!---- patch to IMAT as a function of mesh density --
!        imat = 1
!        if (p.lt. .4 ) imat = 5    !- hack to get different materials

!------------------- 20: background mesh sampling ------------------------
! ** This is the method used for an "ADAPTIVE MESH REFINEMENT" code.
!  here we find which element point P is in hence linear interpolate SPACING
!  (and maybe other parameters) from the (3) nodes' spacing values.
!
!  This is not relavant for my needs.
!
!  It is interesting to note that if the adaptive code drew a contour-map
!  of the desired SPACING function then we can use the pixmap sampling method 
!  (see above) or (better) a linear grid interpolation of a pixmap.
!     DJK 20-3-96

!-----------------------------------------------------------------------
!.. 8-7-97 hmm this is all a bit wrong.
! so d= always 0.->1.
         endif

         rat = rat + d*ratio             !- acculumate the densities.
!        rat = rat + d*ratio -1.         !- acculumate the densities.
!        rat = rat + (1.+ d *(1./ratio-1.))
!        print*,rat, d, ratio
       enddo

       rat = min(rat,ratio_max)     !- clip to the max range. ??
!      rat = (1.+ (1./rat-1.))      !- mung to desired?

!.. both of these are mathematically equivalent. (0<d<1)
!     spacing = spacing_lo + rat * (spacing_hi-spacing_lo)
      spacing = spacing_lo / rat

    end subroutine get_spacing 

!--------------------------------------------------------------------------

!------------------------------------------------------------------
!    6a: Support for bitmaps
!..as a module ?
!------------------------------------------------------------------

      subroutine read_backg_pcx (FILENAME,ixr,iyr)
!
!     Simply read in the given PCX file for use as a 'texture' for 
!     the new mesh density function
!         Dan Kidger 1996
!  Notes:
!     1: We malloc and store it in a charcater array (0-255)
!     2: Contrast this with reading the pixmap directly in XBM format say.
!
      CHARACTER filename*(*)

      COMMON /C_PIXMAP/ IXRES,IYRES, PIXMAP
      PARAMETER (MPIXMAP=100000)
      CHARACTER PIXMAP(MPIXMAP)
!     CHARACTER, allocatable ::  PIXMAP(:)
      INTEGER IXRES,IYRES

      INTEGER   IFAIL, ix,iy,icol2      !------ for SALFORD calls -----
!     INTEGER   HANDLE
!     CHARACTER PALETTE*17

      print*,' PCX file reading not currently supported -- skipping'
      return
      ifail=-1
!     CALL PCX_TO_SCREEN_BLOCK@ (FILENAME,HANDLE, palette, IFAIL)
      if (ifail.ne.0) stop 'PCX file read failed'
!     CALL OPEN_VSCREEN@ (handle,ifail)

      ixres=ixr       !- *HACK* .. how do we find this out automatically?
      iyres=iyr
      print*,'image size=',ixres,iyres

!---------- get some statistics about the image ----------
!.. and store in a Fortran-friendly CHARACTER array
      imin =  999
      imax = -999
      ave = 0.

!.. hmm if not a 256 colour file (eg bi-level or 16 col) 
!.. we need to 'expand' the contrast.
      ic = 0
!.. should I invert IY so that the origin is the bottom-left ?
!     DO Iy2=iyres,1,-1
      DO IY2=1,iyres
        iy = iy2
!  18-9-97 oh dear IY keeps getting overwritten with zero :-(
!    This code is jinxed!
!       print*,'.... iy=',iy,iy2
        DO IX=1,ixres
!         print*,ic,ix,iy,iy2
!         CALL GET_DEVICE_PIXEL@ (IX,IY2,ICOL2)
          ICOL2=0   !hack
          ICOL =  ICOL2
          imin = min(imin, icol)
          imax = max(imax, icol)
          ave = ave + icol
          IC = IC+1
          IF (IC>MPIXMAP) STOP 'Pixmap too large!'
          PIXMAP(IC) = CHAR(ICOL)
        ENDDO
      ENDDO
      AVE = AVE/REAL(IC)
!     CALL CLOSE_VSCREEN@()

!-------------- print statistics -----------------
      write (29,*) ' Image Size: ixres,iyres=',ixres,iyres
      write (29,'(A,i4,a,i4,a,f10.2)')  &
     & ' Pixel range=', imin,' to', imax,' mean =', ave

!------------- contrast stretching ----------------
!.. essential for 16 bit colour PCX images!
      do i=1,ixres*iyres
          icol = ichar(PIXMAP(I))
          ICOL = ((icol-imin) *255) / (imax-imin) 
!         ICOL = MIN(MAX (1, icol), 254)     ! avoid extrema ? 
          PIXMAP(I) = CHAR(ICOL)
      enddo

!----------- A character file version .. for debugging purposes --------
! probaly better to use WRITEF@ for a pure memory dump
!      open (21,file='pixmap',form='unformatted')
!      DO Iy=1,iyres
!        ibase = (iy-1)*ixres
!        write (21)   (pixmap(ibase+j), j=1,ixres)
!     ENDDO
!     CLOSE (21)

      RETURN
      END

!------------------------------------------------------------------
      subroutine sample_pixel (x,y, value)
!
!     This samples the pixmap at X,Y (each 0.->1.) to return value (0.->1.)
!        Dan Kidger 1996
!  Notes:
!    1:  cf. with texture_mapping techniques
!    2: Should we linearly interpolate the 4 nearest points ?
!    3: can we also support tiling across the entire plane?
!    4: if outside the bitamp, we return the value on the nearest edge
!       (cf. explicitly returning zero)
!
      COMMON /C_PIXMAP/ IXRES,IYRES, PIXMAP     !- the bitmap image
      PARAMETER (MPIXMAP=100000)
      CHARACTER PIXMAP(MPIXMAP)
      INTEGER IXRES,IYRES

      IX = INT(X*IXRES)
      IY = INT(Y*IYRES)
!     IY = (1.-Y)*IYRES                  !- invert Y ?
      IX  = MIN(MAX(0,IX),IXRES-1)       !-- hack to make sure we 
      IY  = MIN(MAX(0,IY),IYRES-1)       !-- stay within the pixmap
      ICOL = ICHAR (PIXMAP ( IY*IXRES +IX +1))
      VALUE = ICOL / 255.
      RETURN
      END

!-----------------------------------------------------------------------
!     7: Mesh Smoothing  and  Statistics of mesh quality
!-----------------------------------------------------------------------
      SUBROUTINE BARYCENTRIC  (GC,IGC,NN  ,NUM3,INUM3,nel, ifrom,ito, &
    &   max_iters,iverb)
!
!     This adjusts the mesh spacing so that each node moves towards 
!     the centroid of its attached nodes. 
!        DJK 20-3-96
!  Notes:
!   1/ This would be much simpler if we knew explicitly which nodes, 
!      a given node is connected to.
!   2/ An alternative method might try an make triangles more 'equilateral'
!      eg. assume each is a deformed equilateral triangle hence 'bodyforces'
!      then just solve a standard FE type analysis. :-)
!   3/ Over/Under-realxation is used to try and avoid 'bouncing'.
!   4/ 7-7-97 This routine is now ported to K_MESH.F so it is not essential
!      to do this here
!   5/ 27-9-97 Now with building on a 'random' base, the starting quality 
!      is better anyway
!
      real    ::gc(2,igc)
      integer ::num3(inum3,*)
      real    ::gc2(2,nn)                      ! automatic array
      integer ::p(nn)  
      integer ::num(3)

!---------------------------- loop iterations -------------------------
!     max_iters = 5    !- or maybe larger eg. 1000 ?
      tol = 1.e-4       !- really should be a f(Diag Boundng Box)
!     tol = 1.e-8
!     fact = 1.0                !- over/under-relaxation factor
      fact = 0.6                !- over/under-relaxation factor
      fact=0.9        
      do iter = 1, max_iters

!-- null the new coords --
      do i=1,nn
        gc2(1,i) = 0.                !- new nodal coords
        gc2(2,i) = 0.
      enddo

!- count of each nodes 'connectivty' (a.k.a 'nodal valency')
!  this doesn't change so maybe simplest to pre-calculate?
      do i=1,nn
        p(i) = 0                       
      enddo

!----- loop elements and do each of its nodes -----
      nod = 3
      do iel = 1, nel
        do i=1,nod                 !- extract num (cf GET_ELEMENT)
          num(i) = num3(i,iel)
        enddo

! NOTE: for the general case of (say) an 8 noded quad, there are 3 choices:
!   (1) use all the other 7 nodes 
!   (2) just use the 2 'edge-connected' nodes
!   (3) like (2) but use the 2 connected *corner* nodes

          do i=1,nod            !-- loop the 3 nodes
            do jj=1,nod-1          !-- loop the other 2 'connected' nodes
              j= mod(i+jj-1,nod)+1
              gc2(1,num(i)) = gc2(1,num(i)) + gc(1,num(j))
              gc2(2,num(i)) = gc2(2,num(i)) + gc(2,num(j))
              p(num(i)) = p(num(i)) + 1
            enddo
          enddo
        enddo

!--------------- update the new coordinates -----------------
!.. realy we 'mask' out some nodes
! for advancing front we want to keep the outer boundary fixed
! for DANMESH then maybe we can allow nodes to 'slide' along the boundaries?
        dx_max = 0.
        dy_max = 0.

!       do i=1,nn                          !just do these nodes or 1,nn?
        do i=ifrom,ito                     !just do these nodes or 1,nn?
          dx = fact* ( gc2(1,i) / real(p(i)) - gc(1,i) )
          dy = fact* ( gc2(2,i) / real(p(i)) - gc(2,i) )
          IF (i.lt.ifrom.or.i.gt.ito) then ! apply 'boundary conditions'
            dx = 0.
            dy = 0.
          endif
!         if (nf(1,i).eq.0) dx = 0.
!         if (nf(2,i).eq.0) dy = 0.
          gc(1,i) = gc(1,i) + dx           !- shift this node.
          gc(2,i) = gc(2,i) + dy
          dx_max = max(dx_max,abs(dx))
          dy_max = max(dy_max,abs(dy))
        enddo
        if (iverb>=4) &
        write(*,'(i5,a,3G12.7)')   iter,' ::',dx_max,dy_max, tol

!----- check convergence -----
        if (max(dx_max,dy_max).lt.tol) exit
      enddo     !-- loop iterations
      if (iverb>=1) write (*,'(A,I3,A)')  &
   &   '<> Mesh smoothed in', iter, ' Iterations'

      return
      end
!-----------------------------------------------------------------------
      SUBROUTINE valency_stats (nn,NUM3,INUM3,nel, ifrom, ito)
!
!     This computes the statistical range of nodal valencies.
!      Dan Kidger 13-10-02   (moved from angle_stats)
!
! contrast valency of nodes:elements and that of nodes:nodes

      integer num3(inum3,*)
      integer,parameter:: nvalency=50
      integer :: stats (nvalency)
      integer :: p(nn)         ! P() holds the number of elements attached at each node

      call get_nodal_valency (nn,NUM3,INUM3,nel, ifrom, ito, p)

!-- now summarise as how many nodes have each valency --
      stats(1:nvalency) = 0           ! zero the counters
      do i=ifrom,ito                     !(should skip external nodes) 
        ivalency = p(i)
        if (ivalency>nvalency) then
           print*, 'too many elements at node #',i
           cycle
        endif
        stats(ivalency) = stats(ivalency) + 1 
      enddo 

!--- just print non-zero values. ---
      do i=1,nvalency
         if (stats(i)==0) cycle
         imin=i; exit
      enddo
      do i=nvalency,imin,-1
        if (stats(i)==0)cycle
        imax=i; exit
      enddo
      write(*,'(A)') 'Valency   No_of_nodes' 
      write(*,'(i5,i11)') (i,stats(i),i=imin,imax)
      end

!-----------------------------------------------------------------------   
     subroutine get_nodal_valency (nn,NUM3,INUM3,nel, ifrom, ito, p)
!
!     returns P() which holds the number of elements attached to this node 
!       Dan Kidger 13/10/02  (extracted from angle_stats)
!
      integer num3(inum3,*)
      integer :: p(*)
      integer num(3)


      p(1:nn) = 0                   !- 'connectivity' (a.k.a 'nodal valency')
!----- loop elements and do each of its nodes -----
      nod = 3
      do iel = 1, nel
        num(1:3) = num3(1:3,iel)
        do j=1,3
          n1=num(j)
          p(n1) = p(n1) + 1                 !- count of nodal valency
        enddo
      enddo
      end

!-----------------------------------------------------------------------
      subroutine angle_stats (GC,IGC,NN  ,NUM3,INUM3,nel, ifrom, ito &
     &  ,nbins, binwidth, ipass)
!
!    This computes statistics about a FE mesh:
!       1: The statistical distribution of internal angles
!       2: The statistical range of nodal valencies.
!         c. Dan Kidger 7-7-97
!  Notes:
!    1: ** move this to K_MESH.F soon! **
!    2: 13/10/02 - if ipass=0,1,2,.. store in a table, if negative don't print
!           
!
!    
      real    gc(2,igc)
!     integer num3(10,inum3)
      integer num3(inum3,*)
      integer :: bins (nbins)
      integer :: table(20,200)

!     integer :: p(nn)
      integer num(3)
      save table 
!
!      call get_nodal_valency (nn,NUM3,INUM3,nel, ifrom, ito, p)

!---------- initialisation ------------
      bins(1:nbins) = 0

!----- loop elements and do each of its nodes -----
      nod = 3
      do iel = 1, nel
        do i=1,nod                 !- extract num (cf GET_ELEMENT)
          num(i) = num3(i,iel)
        enddo

!---- loop nodes and compute the angle.
        do i1=1,nod            !-- loop the 3 nodes
          i0 = i1-1

          if (i0.eq.0) i0=nod
          i2 = i1+1
          if (i2.gt.nod) i2=1
          n0= num(i0)
          n1= num(i1)       !- the three consecutive nodes.
          n2= num(i2)
          dx1 = gc(1,n2) - gc(1,n1)  !- RH vector
          dy1 = gc(2,n2) - gc(2,n1)
          dx2 = gc(1,n0) - gc(1,n1)  !- LH vector
          dy2 = gc(2,n0) - gc(2,n1)
          bl = sqrt(dx1**2+dy1**2) * sqrt(dx2**2+dy2**2)
          dot = (dx1*dx2 + dy1*dy2)/bl
          theta = acos(dot) * 180./3.14159265
          ibin = int(theta/ binwidth + 0.5)

          bins(ibin) = bins(ibin)+1         !- add to this bin
!         p(n1) = p(n1) + 1                 !- count of nodal valency

        enddo   !- loop the (3) nod
      enddo

!--- store the table column for later
      do i=1,nbins
        table(abs(ipass),i) = bins(i)
      enddo
      

!-- Print out the table (one column per pass)
      ntot=0
      if (ipass>0) then 
      write(*,'(/A)') 'Angle  Count'
        do i=1,nbins
          ntot= ntot + bins(i)
          centre = binwidth * (i-0.5)
          if (ipass>=0) &
          write (*,'(f8.2,20i9)') centre, (table(j,i),j=1,abs(ipass))
        enddo
        write(*,'(a,f10.5)') 'Tot. Angles/ Tot. Elems=',real(ntot)/real(nel)
      endif

      end subroutine angle_stats

!-----------------------------------------------------------------------
      subroutine write_mesh (file,gc,igc,nn, num3,inum3,nel)
!
!     This writes out the mesh in DANPLOT's Keyword-based .DP format
!       Dan Kidger 1996
!  Notes:
!    1/ Here NUMS is explicitly 10 wide
!    2/ and  NUMS(4) holds the material number <rather than the NUMS(10)>

      implicit none
      character :: file*(*)
      integer :: igc,inum3, nn,nel
      real    ::   gc (2,igc)
!     integer :: num3 (10,inum3)
      integer :: num3 (inum3,*)
      integer :: i,j, nod,imat

!-- nicer to have the outpur file pre-opened with the root name
      OPEN (35,FILE=file,action='write')
      write (35,'(A)') '#','# Danplot(tm) mesh' &
     &  ,'#   produced by ADVANCING FRONT method','#'
      write (35,'(A)') '*TWO_DIMENSIONAL','*NODES'
      do i=1,nn
        write (35,'(i6,2g13.4)')  i, gc(1,i),gc(2,i)
      enddo
      write (35,'(A)') '*ELEMENTS'

!.. is the order of the 3 nodes correct ?
      nod=3
      do i=1,nel
        imat = num3(4,i)
        write (35,'(99i6)')  i, 2,nod,1, (num3(j,i),j=1,nod), imat
      enddo
      close (35)
   end subroutine write_mesh

!-----------------------------------------------------------------------
      subroutine write_mesh_pl (file,gc,igc,nn, num3,inum3,nel)
!
!     This writes out the mesh in (older) .PL format for DANPLOT
!       Dan Kidger 1996
!  Notes:
!    1/ Here NUMS is explicitly 10 wide
!    2/ and  NUMS(4) holds the material number <rather than the NUMS(10)>
!    3/ The .PL format is no-longer supported but is sometimes 'handy'

      implicit none
      character :: file*(*)
      integer :: igc,inum3,  nn,nel
      real    ::   gc (2,igc)
!     integer :: num3 (10,inum3)
      integer :: num3 (inum3,*)
      integer :: i, nod

      OPEN (35,FILE=file,action='write')
      write (35,*) 2
      write (35,*) nn
      do i=1,nn
        write (35,'(i6,2g13.4)')  i, gc(1,i),gc(2,i)
      enddo

      nod = 3
      write (35,*) nel
      do i=1,nel
        write (35,'(99i6)')  i, nod,num3(1:nod,i) ,num3(4,i)
      enddo
      close (35)
   end subroutine write_mesh_pl


!-----------------------------------------------------------------------
      SUBROUTINE INDEX_REALS (ARR,N, INDX)
!
!     Indexes an array of N real numbers in ARR()
!     so the first element in INDX() points to the *smallest* number in A etc.
!       - from 'indexx' in 'Numerical recipes in Fortran'
!        DJK 1994
!
      IMPLICIT NONE
      INTEGER N,INDX(N)    
      REAL ARR(N)
      INTEGER M,NSTACK               !- local variables ..
      PARAMETER (M=7,NSTACK=50)      !- M = threshhold to just Bubble-sort
      INTEGER ISTACK(NSTACK)         !- workspace (2* 25 entries)
      INTEGER I,INDXT,IR,ITEMP,J,JSTACK,K,L
      REAL A

      DO J=1,N                       !--- assume list is already in order
        INDX(J) = J
      ENDDO
      JSTACK = 0
      L = 1
      IR = N
    1 IF (IR-L.LT.M) THEN    !-- Bubble-sort the small (<7) sub-list --
        DO J=L+1,IR
          INDXT = INDX(J)
          A = ARR(INDXT)
          DO I=J-1,1,-1
            IF (ARR(INDX(I)).LE.A) GOTO 2
            INDX(I+1) = INDX(I)
          ENDDO
          I=0
    2     INDX(I+1) = INDXT
        ENDDO
        IF (JSTACK.EQ.0) RETURN      !--> (this is the *only* exitpoint)
        IR = ISTACK(JSTACK)             
        L  = ISTACK(JSTACK-1)           !- Pop the new IR,L off the stack
        JSTACK = JSTACK-2

      ELSE                   !------ Quicksort a longer list -------

        K = (L+IR)/2                ! midpoint of list
        ITEMP = INDX(K)
        INDX(K) = INDX(L+1)         !> swap the 2 pointers
        INDX(L+1) = ITEMP

        IF (ARR(INDX(L+1)).GT.ARR(INDX(IR))) THEN   !- sort the triplet
          ITEMP = INDX(L+1)
          INDX(L+1) = INDX(IR)
          INDX(IR) = ITEMP
        ENDIF
        IF (ARR(INDX(L)).GT.ARR(INDX(IR))) THEN
          ITEMP = INDX(L)
          INDX(L) = INDX(IR)
          INDX(IR) = ITEMP
        ENDIF
        IF (ARR(INDX(L+1)).GT.ARR(INDX(L))) THEN
          ITEMP = INDX(L+1)
          INDX(L+1) = INDX(L)
          INDX(L) = ITEMP
        ENDIF

        I = L+1
        J = IR
        INDXT = INDX(L)
        A = ARR(INDXT)              !- get a value to compare
    3   CONTINUE                      !- search down list
          I = I+1                     !- to find the first mis-order
        IF (ARR(INDX(I)).LT.A) GOTO 3
    4   CONTINUE
          J = J-1                     !- search up list
        IF (ARR(INDX(J)).GT.A) GOTO 4  !- to find the first mis-order

        IF (J.LT.I) GOTO 5            !-- skip over
        ITEMP = INDX(I)
        INDX(I) = INDX(J)             !> swap the 2 pointers over
        INDX(J) = ITEMP
        goto 3

    5   INDX(L) = INDX(J)
        INDX(J) = INDXT              !(INDXT was = INDX(L)

        JSTACK = JSTACK+2       !---- push the larger half onto the stack ----
        IF (JSTACK.GT.NSTACK) &
        STOP 'Stack size too small (INDEX_REALS)'

        IF (IR-I+1.GE.J-L) THEN       !- which is bigger?
          ISTACK(JSTACK)   = IR
          ISTACK(JSTACK-1) = I
          IR = J-1
        ELSE
          ISTACK(JSTACK)   = J-1
          ISTACK(JSTACK-1) = L
          L = I                         !                           ^
        ENDIF                           !                           |
      ENDIF
      GOTO 1                          !--- jump back to the top  -+
      END SUBROUTINE INDEX_REALS

!----------------------------------------------------------------------------
      subroutine indexx (n,arrin,indx)
!
!     ** obsolete - see INDEX_REALS instead **
!     This ranks data in ARRIN() into ascending order in INDX()
!     method = ? (not bubble sort, some sort of heap sort?)
!      ( Is this from Numerical Recipes ?? )
!
      real arrin(n)
      integer indx(n)

      do j=1,n             !-- assume to be all in order
        indx(j)=j
      enddo

      if (n.eq.1) return   !-- nothing to do

      l=n/2+1              !-- start at the middle
      ir=n                 !-- end_of_list

   10 continue             !------ loop back point ------
       if (l.gt.1) then
          l = l-1
          indxt = indx(l)
          q = arrin(indxt)
       else
          indxt = indx(ir)
          q = arrin(indxt)
          indx(ir) = indx(1)
          ir = ir-1
          if (ir.eq.1) then     !- have we finished ?
             indx(1) = indxt       !- store the last remaining point
             return                !- and exit (the *ONLY* exit point)
          end if
       end if
       i = l
       j = l+l

   20  if (j.le.ir) then        !----- the inner loop -----
          if (j.lt.ir) then
            if (arrin(indx(j)).lt.arrin(indx(j+1))) j = j + 1
          end if
          if (q.lt.arrin(indx(j))) then
             indx(i) = indx(j)
             i = j
             j = j+j                !(wow. 'j' is doubled)
          else
             j = ir+1
          end if
          goto 20          !-- loop back
       end if
       indx(i) = indxt
      goto 10            !-- loop back

      end subroutine indexx

!---------------------------------------------------------------------------
MODULE old_mesh
!
!   This stores the data-structures that hold the old mesh
!   - only ever used when we sub-sample in BKGRND/BKGRND1
!    (potentialy also used by re-mapping of disps/stresses etc.)
!
!   Notes:
!    1/ Perhaps we should suffix to distinguish from the 'new' mesh?
!    2/ Nice to allow 4nqs so we can overlay grids (cf bitmaps)
!
  INTEGER :: NDIM, NN, NEL
  INTEGER, ALLOCATABLE ::            &     !----------------------------------- 
              NUMS (:,:)        !- elem steering (NUMS)

  REAL, ALLOCATABLE ::               &     !-----------------------------------
                       coords (:,:)  &     !  global nodal coords (GC)
                       ,delta (:)    &     ! ** mesh density (by node)
                     ,stretch (:)    &     !  - unused
                       ,alpha (:)          !  - unused


!---------------------------------------------------------------------------
CONTAINS
   subroutine read_old_mesh ()
!
!    Reads the background mesh density in (modified) .PL format
!
!
      READ(10,*) ndim                     ! should = 2 for 2D

!--------- read in nodal coords -----
      READ(10,*) NN                       ! in mesh (=NN)
      ALLOCATE (coords(ndim,NN))             !(invert ??)
      DO I=1,NN                           ! coords of each node in mesh
        READ(10,*) inode,coords(:,inode) 
      END DO  

!--------- read in element steering -----
      READ(10,*) NEL                    
      ALLOCATE (NUMS(NEL,3) )
      DO I=1,NEL                  
        READ(10,*) waste,nod, (NUMS(i,n), n=1,nod), waste
      END DO

!------------------ read the (desired?) nodal density ------------
! cf. reading a 'Displacement' table.
      ALLOCATE (delta(NN) )
      Do i=1,NN
        read (10,*) inode, delta(inode)
      END DO

   end subroutine read_old_mesh
end MODULE old_mesh
!--------------------------------------------------------------------
!---------------------------------------------------------------------
!---------------------------------------------------------------------
! end of Dan Kidger's implimentation of Advancing front mesh generation
!---------------------------------------------------------------------

