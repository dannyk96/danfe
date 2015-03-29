!====================================================================
!  This creates Finite Element meshes in the form of quadtrees
!          Dan Kidger  1-6-98   d.kidger@man.ac.uk
! version 4 : an attempt at tri-trees.
!====================================================================

module mquadtree
!----- data structures ----
! implicit none
  integer,parameter :: maxdaughters=3**3 !- nax # of daughter cells
  type qtreenode                    !- Data-structure for each node of the tree
    integer :: d(maxdaughters)      ! - pointers to the 4 daughters
    integer :: parent               ! - pointer to the parent cell
    integer :: level                ! - the 'depth' in the tree of this cell.
    integer :: data(1)
  end type
  integer :: treesize               !- max allocatab;e tree size
  type (qtreenode),allocatable :: tree(:)   !- * The quadtree *

!----- default controls, change as needed ------
  integer :: maxdepth=5!8!9           !- Limit the branch length of the tree
  logical :: tri=.false.            !- tri-trees ?
  integer :: order=3                  ! try 2(4/8) or 3(9/27)
  integer :: ndim=3                 !- we live in a 2d (or 3d) world 
  integer :: nsplit                 ! 4=tritrees, 4=quadtrees, 8=octrees
  integer :: nn=0,nel=0,nod         !- initialy no nodes or elements 
  real   ,allocatable ::   gc(:,:)  !- Nodal coordinates
  integer,allocatable :: nums(:,:)  !- Element Steering
  integer :: icellroot,ncells=0,nsplitcells=0 ! init. no cells, branch or leafs
  real :: ce(3,8)                   !- 'pattern' for local nodes (cf WTHATN) -

contains
!----------------------------------------------------------------------
  subroutine init ()
!----- 0: initialise -----
!.. leave this until we actauly want to create an FE mesh ?
  treesize=0.01* maxdaughters**maxdepth     !- try 10% of max theoretical?
  treesize=50000
  if (allocated(tree)) deallocate(tree)
  allocate (tree(treesize))
  if (.not.tri) then 
    nod = 2**ndim                  !  =4 in 2d, =8 in 3d
!   allocate (ce(ndim,nod))
    ce (1,1:8) = (/0.,0.,1.,1., 0.,0.,1.,1. /)    
    ce (2,1:8) = (/0.,1.,1.,0., 0.,1.,1.,0. /)    ! or try an 8nq
    ce (3,1:8) = (/1.,1.,1.,1., 0.,0.,0.,0. /)    !   or even a 14nb ?

!   nod = 8                        ! patch for 8nq's
!   ce (1,1:8) = (/0.,0.,0.,.5, 1.,1.,1.,.5 /)    
!   ce (2,1:8) = (/0.,.5,1.,1., 1.,.5,0.,0. /)   
  else
    nod=ndim+1                     !  =3 in 2d, =4 in 3d
    ce (1,1:3) = (/0.,0.,1. /)    
    ce (2,1:3) = (/0.,1.,0. /)      ! simerlarly for 6nt's
  endif

  nsplit=order**ndim                !- number of daughter cells
  call create_cell (0,0, icellroot) !- create the first
  end subroutine init

!----------------------------------------------------------------------
   recursive subroutine insert_point (icell,bb,xyz)
!
!   This recurses down to insert a cell around point xyz
!     it always forces a descent to level maxdepth.
!
     integer :: i,j,icell
     real :: bb(2,3),xyz(3)

! note I could use local coordinates here and 'lamda' or 'beta'
     do j=1,ndim
       if ((xyz(j)-bb(1,j))*sign(1.,bb(2,j))<0.) return   !- not here LHS
       if (.not.tri.and.(xyz(j)>=bb(1,j)+bb(2,j))) return  !- not here RHS
     enddo
!.. tetrahedra use x/xl+y/yl+z/zl > 1
     if (tri.and. (xyz(1)-bb(1,1))/bb(2,1)     &          !- not here
                 +(xyz(2)-bb(1,2))/bb(2,2) > 1.) return   !- (sloping face)

     if (tree(icell)%level==maxdepth) then     !- now deepest?
!      - this is a hit - so flag this cell
         tree(icell)%data(1) = 123
       return
     endif

!    print*, icell, ': creating daughters for',bb(:,:)
     call split_cell(icell)

     do i=1,nsplit
       if (tree(icell)%d(i)>0) &      !- is daughter cell present? (gaskets)
       call insert_point (tree(icell)%d(i),daughter(i,bb),xyz)
     enddo
   end subroutine insert_point 

!-------------------------------------------------------------------------
   function daughter(i,bb) result (bbb)
!
!    return as bbb the i'th cubelet of the given cell bbb
!
     integer :: i,j,ii 
     real :: bb(2,3),bbb(2,3)

     ii=i-1                            ! so 0,1,2,3
     do j=1,ndim
       bbb(2,j)= bb(2,j) /real(order)                     ! size
       bbb(1,j)= bb(1,j) + bbb(2,j)*mod(ii,order)    ! lower left corner
       ii = ii/order
     enddo
     if (tri.and.i>ndim+1) then    !- invert the (last) central triangle
!.. for tetrahedra, we have 4 extra cells to build (=8 total)
       bbb(2,1) = -bbb(2,1)         !- simply use -ve extents 
       bbb(2,2) = -bbb(2,2)
     endif
   end function daughter


!-------------------------------------------------------------------------
   subroutine split_cell (icell)
!
!  This splits the given cell (icell) into (4 or 8) daughter cells
!
     integer :: icell,i, icellnew
     if (tree(icell)%d(1)/=-1) return         ! cell already has daughters
     if (tree(icell)%level>=maxdepth) return  ! call is already at max depth

     do i=1,nsplit                         !--- loop (4) daughter cells ----
       if (gasket(i)) cycle
       call create_cell (icell,tree(icell)%level,icellnew)
       tree(icell)%d(i) = icellnew         !- from (-1) to the new daughter
     enddo
     nsplitcells = nsplitcells+1           !- count this, hence nleaf_cells?
   end subroutine split_cell

!-------------------------------------------------------------------------
   function gasket (i) result (gask)
     logical gask
     gask=.true.                  !- assume it is a 'hole'
!    print*,i
!    if (i==1) return
!    select (gasketype)
       select case (i)
!        case (3,7,9) ;return                 ! 2d
!        case (1) ;return                     ! 2d
!        case (3,5,12,14) ;return             ! 2d
!        case (2,6,12,13) ;return             ! 2d
!        case (2,4,6,8) ;return               ! 2d
!        case (1,3,7,9) ;return               ! 2d
         case (5,11,13,14,15,17,23) ; return     !- rubix' cross 
       end select
     gask=.false.                 ! no it wasnt
   end function gasket

!-------------------------------------------------------------------------
   subroutine create_cell (icell,level, icellnew)
!
!  This creates a new (empty) cell
!   - Really we should malloc the new cell from the storage heap
!
     integer :: icell,level,icellnew
     ncells=ncells+1                      !- increment total size
     icellnew=ncells                      ! return the new cells 'pointer'

     tree(icellnew)%d(:)=-1               != all '4' pointers
     tree(icellnew)%level=level + 1
     tree(icellnew)%parent= icell
     tree(icellnew)%data= 0               ! = no extra data here

   end subroutine create_cell

!-------------------------------------------------------------------------
   recursive subroutine quadtree_to_mesh (icell,bb)
!
!   This creates an element out of the current node and its daughters
!   Note that for p-refinement NOD may be 4,5,6,7 or 8
!
   real :: bb(2,3)
   integer :: i,j,icell

   if (icell.eq.1) then
     nn=0 ;  nel=0
     allocate (nums(nod+1,ncells-nsplitcells))
     allocate (gc(ndim,nod*(ncells-nsplitcells)))
   endif
   if (icell>ncells) stop 'Oops: tree structure is broken'
   if (tree(icell)%d(1) >0) then   !------ recurse daughters ----------
     do i=1,nsplit   !4,8
       if (tree(icell)%d(i)>0) &      !- is daughter cell present? (gaskets)
       call quadtree_to_mesh (tree(icell)%d(i),daughter(i,bb))
     enddo

   else         !--------- cell is a leaf node so create element ----------
     nel=nel+1
     do j=1,nod                  
       nn=nn+1
       gc(1:ndim,nn) = bb(1,1:ndim) +ce(1:ndim,j)*bb(2,1:ndim)  !- nodal coord  
       nums(j,nel)=nn
       if (tri) then
         gc(1,nn) = gc(1,nn) + gc(2,nn)* .5  !- morph from a RH to an 
         gc(2,nn) = gc(2,nn) * .866          !- equilateral triangle
       endif
     enddo
     imat = tree(icell)%level
     imat=2
     if (tree(icell)%data(1)==123) imat=1 !-imat
     nums(nod+1,nel)= imat
   endif
   end subroutine quadtree_to_mesh

!-----------------------------------------------------------------------
      subroutine write_mesh (file)
!
!     This writes out the mesh in DANPLOT's Keyword-based .DP format
!       Dan Kidger 1996
!  Notes:
!    1/ Here NUMS is explicitly 10 wide
!    2/ and  NUMS(4) holds the material number <rather than the NUMS(10)>

      implicit none
      character :: file*(*)
      integer :: i,j,imat,ndime

!-- nicer to have the outpur file pre-opened with the root name
      OPEN (35,FILE=file,action='write')
      write (35,'(A)') '#','# Danplot(tm) mesh' &
     &  ,'#   produced by Quadtree method','#'
      if (ndim==2) write (35,'(A)') '*TWO_DIMENSIONAL'
      if (ndim==3) write (35,'(A)') '*THREE_DIMENSIONAL'
      write (35,'(A)') '*NODES'
      do i=1,nn
        write (35,'(i6,3g13.4)')  i, gc(1:ndim,i)
      enddo

      write (35,'(A)') '*ELEMENTS'

!.. is the order of the 3 nodes correct ?
      do i=1,nel
        imat=nums(nod+1,nel)
        ndime=ndim
        write (35,'(99i6)')  i, ndime,nod,1, (nums(j,i),j=1,nod), imat
      enddo
       write(35,'(a)') '*MERGE_NODES'  !- this is a very useful step!
      close (35)
   end subroutine write_mesh

!-------------------------------------------------------------------------
   subroutine write_mesh_pl ()
!
!   This simply writes the mesh out in the standard '.PL' format
!
     integer :: i,iel
     open (20,file='quadtree.pl',action='write')
     write (20,*) NDIM      
     write (20,*) NN
     do i=1,nn
       write (20,'(i6,3f14.4)') i,gc(1:ndim,i)
     enddo
     write (20,*) nel
     do iel=1,nel
       write (20,'(i6,i2,99i6)') iel,nod,nums(:,iel)
     enddo
     close (20)
     print*,'<>', nel,' elements created'
   end subroutine write_mesh_pl
!-------------------------------------------------------------------------
end module mquadtree

!-------------------------------------------------------------------------
program quadtree
  use mquadtree
  real :: bb(2,3)                   !- bounding box of the whole tree
  integer ::  NRECURS=3

  call init ()                      !--- initialise the tree structure
  bb(1,:) = 0.  ;  bb(2,:) = 1.     !- size of the whole quadtree grid
! bb(1,1)= -.5 ; bb(2,1)=1.618      ! try the Golden ratio ?
!-------- method 0 : regular bisecting of cells -------------
  do ipass=1,NRECURS
    do jcell=1,ncells
      call split_cell(jcell)
    enddo
  enddo
!-------- method 1 : random bisecting of cells -------------
!  do ipass=1,5
!    do jcell=1,ncells
!      jcell =1+int(random()*ncells)
!      call split_cell(jcell)
!    enddo
!  enddo
!-------- method 2 : insert a single point -------------
!   call insert_point (1,bb, (/.25,.25,.3/))
!-------- method 3 : insert a circle of points -------------
!  call method3 ()
!-------- method 4 : a 'ring' structure (cf DANFRONT)
!  call method4 ()
!---------- now turn into an FE mesh -------
  call quadtree_to_mesh (1, bb)
! call write_danplot()                           !- export the mesh (to plot)
  call write_mesh ('sierpinski.dp')   

  stop
contains
!--------------------------------------------------------------------------
subroutine method3 ()
   nseg=5 ; istep= 2             !- n edges per polygon and 'star-maker'

   do iseg = 1,nseg
     fac= 2.*3.14159265/real(nseg)
     x1 = .541+.32*sin(fac*(iseg-.5))
     y1 = .395-.32*cos(fac*(iseg-.5))
     x2 = .541+.32*sin(fac*(iseg-.5+istep))
     y2 = .395-.32*cos(fac*(iseg-.5+istep))
     call insert_line (x1,y1,x2,y2)
   enddo
end subroutine method3

!----------------------------------------------------------------------------
subroutine method4 ()

 character string*9
 real :: x(2000),y(2000)

 open (10,file='rings.dat')
 read (10,*) foo, foo
 read (10,*) ncpts
 do i=1,ncpts
   read(10,*)
 enddo

 read (10,*) nobjects
 do iobject=1,nobjects
   read (10,*) ifoo, ifoo
   read (10,*) string, npts, (x(i),y(i),i=1,npts)
!    bb(1,1) = -100.  ; bb(1,2) = -60.      !- xo,yo
!    bb(2,1) = 200.   ; bb(2,2) = 200.      !- xl,yl
   do i=1,npts
     x(i) = (x(i)+120.)/240.
     y(i) = (y(i)+60.) /240.
   enddo
   do i=2,npts
     x1=x(i-1)
     y1=y(i-1)
     x2=x(i)
     y2=y(i)
     call insert_line (x1,y1,x2,y2)
!    call insert_point (1,bb, (/x1,y1,.4999/))
   enddo
 enddo
 close(10)
end subroutine method4

!-----------------------------------------------------------------------
 subroutine insert_line (x1,y1,x2,y2)
   integer :: npts,j
   real d,xp,yp,zp

   d=sqrt((x2-x1)**2+(y2-y1)**2)
!  npts = 1+ 6* d *real(2**maxdepth) / maxval (bb(2,1:2))
   npts = 1+ 26* d *real(2**maxdepth) / maxval (bb(2,1:2))
   do j=0,npts
     xp = x1 + j/real(npts)*(x2-x1)
     yp = y1 + j/real(npts)*(y2-y1)
     zp = 0.9999
     if (tri) then
       yp = yp * 1./.866   !- morph from equilateral to RH triangle
       xp = xp - .5*yp
     endif
     call insert_point (1,bb, (/xp,yp,zp/))
   enddo
  end subroutine insert_line
!------------------------------------------------------------_
end program quadtree


