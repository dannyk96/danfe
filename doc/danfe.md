

                             D A N F E  v 9.0

             Geotechnics-Orientated Finite Element Package

                           Dr. D. J. Kidger   
                   School of Engineering (Civil Division)
                      University of Manchester, UK
                          d.kidger@man.ac.uk

Revsion 0.1   22-1-96

DANFE is a Program for analysing the loadings, displacements, stresses and
strains in structures by the Finite Element Method. The code is distributed as
both a complied PC-executable, and vanilla Fortran source code for compilation 
on any computer system that has a Fortran Complier.


SYSTEM REQUIREMENTS.
     o 1.5 Mb of Disk space for the source code and executables.
     o 2 Mb of Disk space for data files and results files, rising to over
           50Mb for large, non-linear 3D analyses.
     o typically 5 Mb memory for 2d runs, rising to 80Mb for large 3D runs.

     o A Fortran 77 or Fortran 90 complier (only if you need to recompile the
       source). The code uses the standard MIL-STD-???? extensions to Fortran 77
       ,and has been compiled under DOS, Windows, OS/2, HP-UX, Solaris, Cray, 
       Fusjitsu VPX, and on IBM RS/6000s. On the PC, Salford Software's 
       FTN77/486 is recommended.


KEY FEATURES
    The code is built on the standard implimentation of The Finite Element 
Method, as given in "Programming the Finite Element Method", (2nd Edition)
by Smith.I.M. and Griffiths.D.V. (1988).
The code is verified against the computed and analytical solutions given in
that book.

The Key-features of this program that distinguish it from others are:

     o A large library of 18 different element types, including both triangles
       and quadrilaterals and uniquely the 3D 14-node brick, and the 4D 
       32-node tesseract (used to understand element heirarchies).
     o Capacity to run in not only 2 and 3 dimensions but also 1D (eg an 
       expanding cylinder) and 4 dimensions.(!). 2D Axisymetry and 3D Cylindrical
       are possible as is both plane-strain and plane-stress.
     o Six different equation solvers are available, from the full NxN square
       matrix to Upper-triangle Gassian solvers and the efficient skyline-
       storage Choleski decomposition.
     o Indirect solvers are available for large problems. In particular, a
       family of Conjugent Gradient Solvers, optimised for both scalar and vector
       processors. The element stiffness matrices are held in a database, with
       or without a preconditioner. Similar eleemnts can the collapsed out by
       the 'Lego' Algorithm, greatly further reducing storage.
     o All materials may be non-linear, using Von Mise, Tresca or the Mohr-
       Coulomb failure criteria (with or without associated plastic flow).
       For speed, an iterated visco-plastic (or Initial Stress) algorithm
       is used.
     o Input file format is clear text-based Keyowrd-based (cf Windows .INI 
       files) and allows for full commenting and annotation. ( A large set
       of example files is included)
     o Meshes can be imported in 8 different common file formats. In addition
       there is a extensive built-in mesh generator.
     o Output of the Mesh, displacements, stresses and strains is in a standard
       text-based output file(s), suitable for further processing, restart-runs
       or for processing with the in-house DANPLOT (tm) Package to produce 
       contour-pictures, vector-plots, 3D shading and animations.


PROGRAM HISTORY
    The code has its origins in the 30 years history of developing Finite 
Element Software at the University of Manchester, under the Geotechnics research 
group headed by Prof. I.M. Smith. 

 1986     Program 6.2 (2D Slope Stability) from the forthcoming book (above), 
          was taken as a starting point.
 1986-90  The program was developed to analyse both 2D and 3D slopes
          with an auomated 'Factor-of_Safety finding' algorithm. Graphical 
          routines were also developed to extract the shape of the curved 3D 
          slip-surfaces within the Model.
 1990-91  The code was extended to analyse 2D and 3D 'Spud-can' problems for
          the offshore industry, as part of an Industrial Contract. The first 
          Interactive 3D Post-Processor was developed under the working title
          of 'DANPLOT'
 1991-93  The previous 'specific' FE Code was completely rewritten using 
          'modern' Fortran (Fortran-90-esque), and fully modularised.
          The scope of the program was greatly expanded to analyse almost
          all classes of problem, including Eigenvalues and Fluid-flow. 
          The data file format was changed from simply numeric to Keyword based,
          thus aiding readibility and making future versions always backwardly 
          compatable.

 1993-95  A Preconditioned Conjugent-Graident Solverm incorporating a novel
          'Lego' storage stratagy was added. This allowed for the first time
          Large 3D analyses to be performed on a humble 8Mb 486-PC, limited only
          by CPU-speed (which of course is doubling every year).


FURTHER DOCUMENATION:
     o  DANFE   : Introductary User-Guide. (in preparation)
     o  DANMESH : Guide to the mesh-generating Keyword-based modules.
     o  DANFE/DANPLOT : Tutorial on finite element analysis.

THE AUTHOR:
     Daniel Kidger graduated with a B.Sc.(Hons) Civil Engineering in 1986, and
a Ph.D. "Visualisation of Finite Element Eigenmodes and three-dimensional 
plasticity" in 1990. Since then he has worked on a variety of research projects
with industrial clients. He lectures on I.T., Fortran 90 Programming and in
Finite Elements to civil engineering undergraduates and postgraduates.

SOFTWARE AVAILABILITY AND CONTACT DETAILS
     The complied code is freely available. The source code is available by
request only. Queries on the scope of the code and suggestions for further 
features should be mailed to: 
                d.kidger@man.ac.uk
The principal repository for DANFE and other related software from the URL
                ftp://golden.eng.man.ac.uk/pub/fe/dansoft/ 
and its subdirectories. On-line documentation and a set of example plots can
be found from my World Wide Web page:
                http://www.man.ac.uk/~mbgasdk/

                     -------------------------------












