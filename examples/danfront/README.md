There are 4 examples here:
These demonstarete severqal aspects of the danfront programs functionality

ex1:  a human femur cros-section
    The demonstates the basic functionality of of the advancing front
    We add -v here to show more details of the meshing process

ex2: An embankment cross-section
    Theory says that such embankments fail on a circular slip plane. Here the location of
    the slip plane had been previously determined with DANSLIP q.v. This mesh is refined
    using a defined circular arc that cross teh mesh and is clipped at the boundarry
    We use danplotg here to demonstrate teh use of on screen graphics

ex3:  3blaocks
    Here we demonstrate that multiple regions can be filed with triangles such that the
    triangles on the boundary have common position of their nodes.
    We use the -s option to invoke 'silent' ie minimal stdout output

ex4: Logo
    Here we show how a predrawn grayscale image can be used to control the mesh density.
    Where the image is black the triangles will be the finest; white the coarest 'background'
    density. mid-gray values have a medium density as can be seen in teh resulting mesh 
    For real Finite Element applications this bitmap would be say the stress density found from
    a previous FE run hence a new mesh with finer element to capture the high stress regions can
    be created and subsequently analysed.

