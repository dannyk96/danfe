
# *ANALYSE
Runs a 'standard' analysis of the given loads and mesh
## DATA
(none)
## DESCRIPTION
This module analyses the effect of the previously given loads on the stucture.
It is the core module of a DANFE run, and will usualy take the bulk of the total
runtime.

*ANALYSE will loop the muplitle load steps as given in *LOAD_STEPS or
*TIME_STEPS, but a seperate *ANALYSE is needed for each Load case - for example
after some element have been removed or a differnt set of loads applied.

For each step a table of displacements is written out to the current output
file. If the SPRINT or EPRINT tags have been given to materials in the
*MAT_PROPS module, then tables of stresses and strains (respectively) will also
be written out. These allow contouring of stresses subsequently in DANPLOT.

For non-linear models, there will be an iterative process where the excess
stresses are restributed until all are in the allowable range as given by the
material model (eg. Mohr-Coulomb). The maximum number of these iterations and
the criteria for convergence may be changed using *CONTROL.

For each *ANALYSE module a summary table is written to stdout. This has one row
per load step and gives a line of 9 values. The first is the load step number.
The second is the number of iteration needed. This will equal 2 for linear
behaviour and rise considerably after the onset of yield.

The third is the total applied load. This is the unsigned sum of the reaction
forces for all nodes that have had an applied load or prescribed displaement.
Fourthly the 'average' displacement of the nodes used for the previous column is
written. This again is unsigned so care must be taken in interpretation if there
are multiple key points moving in differing directions.

The fifth column gives the slope of the graph that would ensue from plotting
column 2 against column 3. This is non-dimensionalised so that the first number
is 100 (%). This number falls below 100 as the material begins to yield. On the
yield plateau, this number should converge to a value of probably less than 5
(%).

The sixth and seventh numbers indicate the extent of yielding. The sixth number
shows the stress stae of the point in the mesh closest or with the most
yielding. For elastic response, this number is always less than 100%, and the
amount of load needed to cause yielding can be seen by simple extrapolation. It
should not rise above 100%, however values of 100%-103% are normaly acceptable.
Higher values indicate that the algorithm is struggling to find convergence, and
perhaps the job should be rerun with a smaller loadstep size. The Seventh number
gives the number of 'Gauss' points in the mesh that are at yield - a simple
indication of the extended of yielded material at this stage.

The final column gives the time of day, useful perhaps to indicate how long the
job has been running and so estimate how long it will take to complete. For
Fortran implemntation that like a real time clock the dummy string "hh:mm:ss" is
printed instead.

## EXAMPLE
*NODAL_LOADS
  5. 10.    0.  -500.     !- 500kN vertical load at point (5.,10.)
*FORM_KM
*ANALYSE
*EOF

## ADVANCED
## CAVEATS
*ANALYSE requires that the 'stiffness matrix' has previously been formed with a
call to *FORM_KM (or *FORM_KP, etc.). In a future release of DANFE, *ANALYSE
will itself call *FORM_KM internaly if the current stiffness matrix is absent or
invalid because the mesh has changed, for example when excavating a tunnel or
performing Adaptive Mesh Refinement by the h-method or the p-method.

## SEEALSO
[*FORM_KM][form_km.md]
*LOAD_STEPS
*TIME_STEPS
*MAT_PROPS
*NODAL_LOADS
*NODAL_DISPS
*BC_BY_COORD
*FB_RS
## FLAGS
'F'    4    4    3
## REVISION
 23:08:98 created
