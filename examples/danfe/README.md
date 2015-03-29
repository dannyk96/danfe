= Examples for teh DANFE Finite Element engine

There are 6 examples here


_ex1_ : a Simple 2d cantilever.
   This shows the basic operation of danfe on a simple structure
   The is a therotical solution to the this case : tip deflection is given by this formula:

    The mesh we used has 20 by 6 finite elements: each an 8 node quadrilateral.
    This gives a tip delfection of xxx mm  which agrees with the formula aove.

ex2: 3d Beam
    This is an extension of the above. Here the mesh is in 3d (which add signifcantly to the run
    time and the storage requirements). 

ex3: Material non-lineariry: 2d Foundation

ex4: example of multiple materias and eleement types?

ex5 : Adaptive Shape optimisation of a beam
   Here a rectangular canilever mesh is created and a point load applied at the tip.
   Danfe then performs multiple passes. In each pass teh stressed are analysed and then the 5% of
   the finite elements with the lowest stress are removed and the process is repeated. 
   (actually these elements are left in but given a very low stiffness).
   Because of the PCG 'Lego' algorithm the process is very quick as only two copies of each 
   element stiffness matrix needs to be stored: the 'nornmal' and the 'removed' .

   This process allows the optimal topoloogy of a structure in terms of stiffness to weight ratio
   to be determined. In teh natual world this is extacly how bones grow and are maintained. 
   Osteocytes form bone matter where stress is highest and remove it where it is low (hence the
   importance of children to get regular exercise and so form strong bones).
  
   If we capture each interation as an image then these can be animated. 


ex10: multiple analyses of beams for different eleemnt types
   Here we use the feature of *DANBLOCKS to convert mesh of one element type to another
   The same simple foundatino footing example is used 
   Note that for some element types the mesh becomes over-stiff and in some over-flexible
   such that the solution does not converge. This is to be expected and shows how Danfe responds
   to a loack of convergence in the solution for a given load.
