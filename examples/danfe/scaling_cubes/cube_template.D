#
#     The common part of all the cubical data files
#          Dan Kidger 12/04/15  (oroginal dated  29-3-96)
#
# note that lines that start with ! are simply copied to stdout

*DEBUG
c 1 2     ! some timings
c 9 2     !- PCG counts

*CONTROL
 2     -1    !- 0=elastic only, -1= no stress calcs either
 2     0    ! so get reaction forces
 3     0    !- SPARIN
 3     1    !-  1=PCG''s !, 6=PCG-tri
c3     3    !- BANRED
c3     4    !- CHOLIN
c3 6
c12     0    !- 0=no-preconditioner, 1=diagonal precon

*THREE_DIMENSIONAL
!
! A mesh of nxe^3 finit elements of 100m on each side
! a uniform pressure of 1mm is applied downwards to one quarter.
!
*SUPERBLOCK
 1       3 _ELTYPE_ 1
 _NXE_      0. 100. 
 _NXE_   -100. 100.
 _NXE_   -100. 100.

*WRITE_DANPLOT_KEY
c*SET_LEGO_ALLSAME

*NULL_ARRAYS       
*FB_RS     !--- 'magic' BC''s = fixed base with roller sides

*MAT_PROPS
  E=1.e5  v=0.3

c*NODAL_LOADS_BY_BOX
*NODAL_DISPS_BY_BOX
   0. 50.    0. 0.    -50. 0.      0. -0.001 0.
*FORM_KM

*ANALYSE

