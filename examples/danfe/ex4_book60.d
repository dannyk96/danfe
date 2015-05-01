#   A test data file for my Analysis program 
*JOB_TITLES

  'Simple Footing 32*8nq''s'
  'example 6.0 for Smith & Griffiths'

c*CONTROL
c 3     1    !- try PCG's !

*CONTROL
 4   999    !- max # iterations
 5     3    !- tolerance (10**-val)
c 7     2    !- use von Mise
10     4    !- NGP=4 .. so R.I.
#--------------------------- the mesh data ----------------------------
*TWO_DIMENSIONAL
*SLOPE_MESH   
  2  8  1
  8 8                           ! (as BOOK60.dat but 20nb's (1 Z-slice)
0. 1. 2. 3. 4. 5.5 7. 9. 12.
0. 1. 2. 3. 4. 5.5 7. 9. 12.
  4 4
0. -1.25 -2.5 -3.75 -5.
*WRITE_DANPLOT_KEY

*NULL_ARRAYS
*MAT_PROPS
  model=2   E=1.e5  v=0.3  C=100.  
  model=2   E=1.e5  v=0.3  C=100.    sprint=1
# model=2   E=1.e5  v=0.3  C=100.    sprint=-31

*BC_BY_COORD
  230964. -5.       0 0   !- fixed base
      12. 230964.  0 0   !- fixed RHS
       0. 230964.  0 1   !- symmetric roller LHS
c*FB_RS       !- cos orig. had a fixed RHS (not roller)
*FORM_KM
*NODAL_LOADS
   0.   0.     0.   -16.6667    !- 
   0.5  0.     0.   -66.6667    !-  
   1.   0.     0.   -33.3333    !- A UDL :-)
   1.5  0.     0.   -66.6667    !- 
   2.   0.     0.   -16.6667    !- 
*LOAD_STEPS
  1 200.            
  1 100.            !- ie. 1  of. 100%
  3  50.            !- then 3 of.  50%
  1  30.            !-   etc.
  1  20.
  1  10.
  2   5.
*ANALYSE


