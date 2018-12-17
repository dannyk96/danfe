# Program source files in the DANFE Finite Element Suite

### Main DANFE Suite
#### danfe.f
The main source file for the Finite Element Analysis Engine

#### danmesh.f
This is a cutdown of danfe.f  - ponly doing mesh generation so no huge arrays

#### danplot.f
This is the post processing application with full GU

### Other DANFE software
#### danfront.f90
Triangular mesh generation usin the _Advacning Front_ method.
#### danslip.f
Keyword driven 2d results plotter including resampled random vectos (cf. BBD weather maps)
It is based on the plotter I wrote and used extensively for the plots in my Ph.D thesis.
Note how it draws a title across the bottom of the (landscape) page and supports mutiple (eg. 2x2) plots per page

#### qp_ps.f
plots a meshfile in Postscript
#### qp_pgplot.f
Plots a 2d meshfile using the 3rd party PGPlot library
#### qp_gl.f
Plots a 2d mesg using OpenGL graphics

#### menu.f
All the menus for Danplot. Could maybe be including in the danplot.f source?
Or maybe support multipel versions for different graphics resoltions? 

### Curios
#### Siepinski.f90
