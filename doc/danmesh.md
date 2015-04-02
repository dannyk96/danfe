---------------------------------------------------------------------------



                        DANLIB documentatation 


                 User-Guide to the Mesh-Handling Modules 
                         Keyword descriptions 



                              D.J. Kidger 
            Dept. of Civil Engineering, University of Manchester
                      Oxford Road, Manchester, UK

                       (  Dan.Kidger@man.ac.uk )


---------------------------------------------------------------------------

Revsion 0.0      24-3-94
Revsion 0.1       9-7-94
Revsion 0.2       9-8-94

The latest release of DANMESH and this documentation is available on-
line by anonymous FTP from the following URL:

       ftp://golden.eng.man.ac.uk/pub/fe/danplot/danmesh.zip

Any queries may be addressed to the author at the above address. The 
author welcomes any constructive critisism and is keen to add any 
features requested by users.

                             CONTENTS
                             ========

   1   : OVERVIEW
         1.1   Overall philosophy
         1.2   Introduction

   2   : GUIDE TO THE SECTIONS OF THE LIBRARY
      2.1:  General
      2.2:  Basic mesh-building modules
      2.2A:  Material property numbers
      2.3:  Mesh-enrichemnt
      2.4:  Geometry morphing
      2.5:  Importing and exporting meshes
      2.6:  Tuning : resequencing the nodes, delete orphans, etc.
      2.7:  Miscilaneous modules (due for deletion in the next release)
      2.8   App.A : Boundary Conditions and Loadings

   3.  : THE DATA MODULES .. IN ALPHABETICAL ORDER

   4.  : EXAMPLES
      4.1   Flow under a Dam mesh
      4.2   Plate with a circular hole
      4.3   Three dimensionnal pile groups
       ... do as create pile top , DANBLOCK to 3d , then shift & save *4
       ... create top, save & add-in ,, to 3D later
       ... save, shift, read, (now 2) .. repeat to get 4 piles ..
      4.4   3D Actric Caisson


... Possibly an index at the end .. however the modules should be in
alphabetic order, and contain full X-refs, so maybe we don't need an 
index.

---------------------------------------------------------------------------

** Chapter 1 : The DANMESH mesh generation system

*** 1.1 Overview

DANMESH is a finite element mesh generating system. The package exists 
as a portable Fortran library together with a driving main program. 
Unlike other packages, it is possible for the user to develop his own 
modules to perform specialist tasks. It is recommended that the system 
is used as a stand alone pre-processing package, however the building 
block subroutines may be called from a user written code. 

There are modules for defining the nodes and elements, for moving and 
deleting existing nodes and elements, subdiving and changing elements 
and for saving and loading a mesh in a variety of formats.

Several ÿmethods may be used to create a mesh and so the most ÿefficient 
for ÿa ÿparticular mesh may be chosen. ÿFor example , for ÿa ÿcyclidical 
mesh, wether to create nodes along arcs from the beginning, or form as a 
rectangular block then transform ÿinto a cylinder later. 

*** 1.2 The concept of keyword driven datafiles
... ÿÿPhilosophy ÿof  ÿdata ÿfiles ÿpartitionned ÿinto ÿdata-modules ÿby 
*keywords 


*** 1.3 Special charcters

Although the format of the input data is free-form, several symbols have 
a special meaning if placed in column 1. In particluar, every section of 
data is marked by a Keyword that starts with a '*' character. Other 
symbols mark comment lines, include files and output redirection.

**** 1.3.1 Keywords

All keywords begin have an asterix as their first character to identify 
them. 

 1.3.2 Comment lines

Any line of the data file may be treated as a comment. These lines 
have as their first characters,  a 'c', 'C', '!', '#' or  '/*'. It is 
suggested that 'true' comments have a '#' character, and temporarly
'commented-out lines have a 'c' or 'C' character to diferentiate them.
Lines beginning with a '!' are also echoed to the console. Comments may 
also be placed to the right of any line of data. They should normally be 
preceeded by a '!' character (as in Fortran 90) to avoid cunfusion to 
the parser.
 
1.3.3 File redirection and 'Include' files 

Secondary ÿdata files may be 'Include'-d into the main file by ÿmeans of 
the '<' character followed by a filename. This allows inclusion of whole 
sets ÿof ÿmodules (for example a pre-defined section), ÿor ÿjust ÿa ÿfew 
numbers. ÿThe '<' ÿtoken may appear anywhere even within the middle of a 
module, ÿand also maybe be nested within 'Include' ÿfiles to any ÿlevel. 
Modules such as *IMPORT_NFF are almost always followed by redirection to 
read from the fixed format file. 
   By ÿdefault output from modules such as *WRITE_DANPLOT go to ÿa ÿfile 
with the same name as the primary input file but with the suffix '.OUT'. 
The ÿ'>' ÿcharacter may be used to change the destination of ÿsubsequent 
output. ÿThis is followed by the filename to use. ÿA single '>' ÿwith no 
filename will revert output to the previous output file. 

 (( Should *WRITE_OFF *always*  write to a file with a 'GEO' suffix ? 
    .. so maybe '>' should only give the *root* of the output files ? ))



------------------------------------------------------------------------

Chapter 2 : The format of the DANMESH package

The ÿlibrary of modules is divided into 7 sections, ÿwhich are described 
in this chapter. ÿChapter 3 contains a full description of every module, 
arranged in alphabetical order. ÿThe description includes the syntax, ÿa 
description, examples of use, any caveats, and cross-references to other 
related modules. 

The modules are divided into the following groups:
  1: General
  2: Basic mesh-building modules
  3: Mesh-enrichemnt
  4: Element Property Groups
  5: Geometry morphing Routines
  5: Importing and exporting meshes
  7: Tuning : resequencing the nodes, delete orphans, etc.

  8: Miscilaneous modules (due for deletion in the next release)

App.A : Boundary Conditions and Loadings
App.B : Modules for use in DANFE finite element solving package
App.C : Extra Modules available in DANPLOT post-processing package

------------------------------------------------------------------------
------------------------------------------------------------------------


  SECTION 1. GENERAL 

These are the set of general modules, that control the overall process of
mesh generation.

  
Modules
   *TWO_DIMENSIONAL         - sets all further operations to be in 2D
   *THREE_DIMENSIONAL       - sets 3D mode
   *PLOT_MESH               - plots the current mesh on a VGA display
   *EOF                     - to mark the end of the data file (optional)
   *DUMMY                   - a dummy keyword that has no effect
   *PAUSE                   - pauses the program until a key is pressed.

  SECTION 2: Basic Mesh-building

This sections consists of modules for defining the basic shape of the 
mesh. *NODES allows the direct specification of a set of nodes and 
their coordinates. Joining up nodes to form elements may be done with 
*ELEMENTS. Alternatively a complete mesh may be created by using 
routines such as *SLOPE_MESH or *LAYERED_SLOPE. If necessary these 
meshes may be further refined by modules such as *DELETE_MATERIALS, etc.

Modules

 *NODES              - specify cordinates of the nodes
 *ELEMENTS           - specify the steeing of teh elements
 *BASIC_BLOCK        - a 1D/2D/3D/4D single element 'cube' (obs) ?
 *BASIC_BLOCK_4NQ    - a 2D mesh of regularly spaced 4nq's
 *SLOPE_MESH         - a 2D/3D mesh of a slope with/without a base layer
 *LAYERED_SLOPE      - a 2D mesh of strata of variable thicknesses


  SECTION 3: MESH ENRICHMENT

The most important routine in the section (and indeed in the entire 
package) is *DANBLOCKS. It allows the subdivision an element or a set 
of elements into finer elements. These finer elements may be of a 
different kind, so that for example a basic mesh of 4 node quads as 
produced by *BASIC_BLOCK_4NQ may be replaced with a mesh of 12 node 
quads for example. If the desired elements were 20 node bricks, for 
example, then this would have the effect of 'projecting' a 2D mesh into 
3D.
   Alternatively *DISECT_MESH may be used. This will replace every 3 or 
6 node triangle in the mesh with three 4-noded quad elements. Conversely 
it will also replace every 4nq with 4 3-noded triangles via the creation 
of a new central element node.

in particlar *DANBLOCKS, but also *DISECT_MESH and *TRIANGULARISE_MESH

Modules
   *DANBLOCKS              - changes ANY element into a set of ANY other elements
   *DISECT_MESH            - replaces 4nq's with 3nt's and vice-versa
   *TRIANGULARISE_MESH     - bisects 4nq meshes into a variety of 3nt forms
.. maybe a subset of DANBLOCKS called change_element_typewould be nice?



SECTION 4 :  ELEMENT PROPERTY GROUPS

In ÿa ÿmesh ÿit is typical to have the elements ÿof ÿdifferent ÿproperty 
types, ÿÿeg. ÿConcrete piles with a mesh of sand overlying clay, ÿÿPlate 
bending elements of different thicknesses, or layers of material are the 
same but are added to the mesh in succesive load steps. 
  These may be defined automaticaly (for example by *LAYERED_SLOPE), 
based on their integer x,y,z position within the mesh (*ELEMENT_TYPES), 
or generaly by their centroid coordinates (*MATERIALS_BY_BOX). These may 
be subsequently changed by *CHANGE_MATERIALS, for example to give 
several regions the same material proprty number. Elements of a certain 
material type may then by removed by *DELETE_MATERIALS. It is 
conventional to give materials that exist but are not currently active  
a negative material number, so that an analysis program can simply skip 
over these elements. The module *ADD_MATERIAL merely changes a material 
number from negative to postive (for built-up models); *REMOVE_MATERIAL
performs the inverse operation.

Modules
    *MATERIALS_BY_BOX      - give elements a material # based on its coord
    *ELEMENT_TYPES         - give elements a material # based on its position
    *CHANGE_MATERIALS      - change a #
    *DELETE_MATERIALS      - deletes elements completely
    *ADD_MATERIAL          - makes mats appear present (postive)
    *REMOVE_MATERIAL       - makes mats appear absent (negative)

       

SECTION 5 : GEOMETRY MORPHING ROUTINES

It ÿis ÿoften convenient to form parts of the mesh as a ÿsimple ÿregular 
block ÿand then 'morph' ÿthis mesh into its final form by, ÿfor ÿexample 
wrapping it around into a cylinder. ÿAlso pre-formed standard meshes may 
be ÿscaled (*SCALE_MESH) ÿand translated (*SHIFT_MESH) ÿto ÿthe ÿdesired 
configuration. 
   Some of these modules perform quite specialised tasks, such as 
*WRAP_TWIST_Y. This rotates succesive x-z planes around the y-axis so as 
to give a 'spiral' mesh, eg. for a screw-thread or helix-type mesh.

   After moving nodes about, some nodes may end up in the same location, 
the ÿmodule *JOIN_COINCIDENT_NODES will give these nodes the ÿsame ÿnode 
number, then renumber the remaining nodes after deleting the 'extra' un-
needed node numbers 


Modules
    *MOVE_NODES             - changes the postion of a set of nodes
    *SHIFT_MESH             - translated the whole mesh
    *SCALE_MESH             - rescales the whole mesh               
    *X_TO_Y_TO_Z            - topples the whole mesh eg to make 'Y' upwards
    *X_TO_Y                 - swaps the x and y coordinates

    *WRAP_AROUND_Y          - wrap a mesh into a cylinder about the Y axis
    *WRAP_AROUND_Z          - (same but around the 'z' axis)
    *SQUARE_TO_CIRCLE       - rounds a mesh into arcs
    *CUBE_TO_SPHERE         - makes a cubic mesh spherical
    *WRAP_TWIST_Y           - to make 'staircase' like meshes

    *JOIN_COINCIDENT_NODES  - gives all nodes at a point the same node #


  6. MESH IMPORT AND EXPORT
       These modules allow the reading and writing of meshes in a 
variety of formats. For example; *IMPORT_OFF and *EXPORT_OFF will read and 
write data in the popular "Object File Format", *WRITE_RAYSHADE 
will export the mesh in the format used by the Rayshade (c) Ray tracing 
package. 

Modules

    *IMPORT_OFF                  -  Object FIle Format
    *IMPORT_PL                   -  The 'old' Danplot format
    *IMPORT_SRF                  -  'SURF' 3D modeller format
    *IMPORT_3DEDIT               -  '3DEDIT' modeller format
    *IMPORT_TECPLOT_MESH         -  'TECPLOT' (c) FE post-processor
 !  *IMPORT_RAYSHADE             -  'Rayshade' Ray-tracing format
 
    *EXPORT_OFF
 !  *EXPORT_PL
 !  *EXPORT_SRF
 !  *EXPORT_3DEDIT
 !  *EXPORT_TECPLOT_MESH
    *EXPORT_RAYSHADE

See also:
    *NODES, *ELEMENTS for the prefered standard format for data.


SECTION 7 :  'TUNING' THE MESH DATABASE

These modules are for rearranging the mesh.
Some FE programs are very fussy about the form of the mesh that is 
created. For example some codes will not accept nodes that do not have 
any attached elements. The module *DELETE_ORPHAN_NODES will remove these 
from the mesh and then renumber the remaining nodes in ascending order.
Some FE codes apply a 'freedom number' to each node in order of 
ascending node number, and then solve the resulting equations by storing 
the matrix in 'banded' form .. unlike Conjugate Gradient and Frontal 
Solvers. The module *SORT_NODES_FROM_POINT will renumber all of the 
nodes in order of their distance from a given point in space, thus 
providing a simple way to get a 'good' bandwidth in simple FE programs.
The routine *SORT_ELEMENT_FROM_POINT performs a similar task for the 
element numbering (eg for frontal solvers) by considering the centroid 
of each element.
  Some ÿmodules ÿcan give elements so thin that ÿoppsoite ÿsides ÿbecome 
adjacent, ÿfor example using *LAYERED_SLOPE with a seam that peters out. 
This ÿcan ÿcause ÿproblems ÿin ÿFE ÿsolvers. ÿÿThe ÿmodule *DELETE_ZERO_ 
AREA_ELEMENTS ÿwill ÿremove these and renumber the renmaining ÿelements. 
This ÿwould ÿnormaly ÿbe followed ÿby ÿ*JOIN_COINCIDENT_NODES ÿto ÿ'glue 
together' the elements on either side of the deleted elements. 


SECTION 8: MISCELANEOUS AND OBSOLETE MODULES

This ÿsection contains an Appendix of Modules, ÿwhose tasks are specific 
to ÿparticular cases, ÿor whose action can be better performed be use of 
other modules. Modules in this section will probably be removed from the 
next release of the package. 

    *4NQ_TO_2_3NTS      - see *TRIANGULARISE_MESH
    *4NQ_TO_4_3NTS      - see *DISECT_MESH
    *2_3NTS_TO_4NQ      - n/a
    *4_3NTS_TO_4NQ      - eg. for the teapot .. n/a
    *MIRROR_EVEN_ELEMENTS  - eg. for the 'faces.3d' dataset


App.A : Boundary Conditions and Loadings
    DANMESH also contains a large selection of routines for creating and
maintaining the boundary conditions of the FE model. These are currently
only available form the DANFE Finite Element Solver package and are 
described in the DANFE documentation (q.v.). These modules should be 
available in DANMESH very shortly.
    The Modules fall into two classes: those for specifing the boundary 
fixities, eg.  'rollers' on the LHS, and those for specifying teh 
boundaries tractions, eg. Applied Forces and Displacements.


   .. eg *BC_BY_BOX .. not strictly mesh generation ? .. but nicely done 
by DANMESH.. ; sorted by *RESEQ_NF, written out by *WRITE_NF, read-in by 
*READ_NF
  Simililarly for *LOADS (do DISPS by a code probally ?)
.. ie.

Modules
    *BC_BY_BOX       - best
    *BC_BY_DIR       - sort of OK
    *BC_BY_COORD     - with 230964 as well 
    *FIXED_BASE_ROLLER_SIDES -
 or *BC_FBRS         - fixed base and roller side .. the most common BC's
    *READ_NF         - either a few nodes or ALL of them
    *WRITE_NF        - write by keyword
    *ON_FREEDOMS     - activate the freedoms of all the nodes of those
                       elements with positive material numbers
    *RESEQ_NF        - resequence the freedoms from 1 to N

------------------------------------------------------------------------

  SUMMARY OF THE DATA MODULES

( This section is only really of use as a quick-reference chart  )   
( no need for an index the modules will be in alphabetical order ) 
    *TWO_DIMENSIONAL
    *THREE_DIMENSIONAL
    *BASIC_BLOCK
    *REGULAR_MESH
    *LAYERED_SLOPE
    *PAUSE
    *SLOPE_MESH
    *ELEMENT_TYPES

    *IMPORT_OFF
    *IMPORT_PL
    *IMPORT_SRF
    *IMPORT_3DEDIT
    *IMPORT_TECPLOT_MESH
 !  *IMPORT_RAYSHADE        ..cf _NFF ?
 
    *EXPORT_OFF
 !  *EXPORT_PL
 !  *EXPORT_SRF
 !  *EXPORT_3DEDIT
 !  *EXPORT_TECPLOT_MESH
    *EXPORT_RAYSHADE
 
    *DANBLOCKS

    *CHANGE_MATERIALS
    *MATERIALS_BY_BOX
    *DELETE_MATERIALS

    *SORT_NODES_FROM_POINT
    *SORT_ELEMENTS_FROM_POINT
    *DELETE_ZERO_AREA_ELEMETS
    *DELETE_ORPHAN_NODES

    *JOIN_COINCIDENT_NODES

    *4NQ_TO_2_3NTS
    *4NQ_TO_4_3NTS
    *2_3NTS_TO_4NQ
    *4_3NTS_TO_4NQ

    *MIRROR_EVEN_ELEMENTS
    *MOVE_NODES
    *SHIFT_MESH
    *SCALE_MESH
    *X_TO_Y_TO_Z
    *X_TO_Y

    *WRAP_AROUND_Y 
    *WRAP_AROUND_Z

    *SQUARE_TO_CIRCLE
    *CUBE_TO_SPHERE
    *WRAP_TWIST_Y

-- need to complete all import/export 
-- I think I will use '> file' for output (dafaults too ?)


+++

------------------------------------------------------------------------
  Chapter 4 :  THE DATA MODULES 

The modules are presented in alphabetcal order.  For each a description
of the data format is shown with a full description of the module, with 
notes about any advanced usage. For each module an example of usage is 
given and also a cross reference to other related modules.

4.1  Data format
  For many modules the set of data may be repeated as many times as 
desired until the next keyword is found, for example the lines giving 
the coordinates of nodes in *NODES. The repeatable section is delimited 
by a pair of // to //  symbols.

  Most modules may used for either 2D or 3D meshes. The extra data 
needed for 3D usage is placed between a pair of << to >> symbols. 


------------------------------------------------------------------------

( The modules have not yet been sorted alphabetcially, so we are stuck
with the un-sorted order which is generaly in the order of the sections 
presented above. )



    *TWO_DIMENSIONAL

Data:
    <none>

Description:
    This ÿsets the program to work in two dimensions. ÿIt should be used
before any other mesh-generating keywords. This keyword is not necessary 
if ÿthe ÿmesh is explicitly two dimensional or three ÿdimensional, ÿÿfor 
example *IMPORT_3DEDIT 

See also:
    *THREE_DIMENSIONAL. 

---------------------------------------------------------------------------

    *THREE_DIMENSIONAL

Data:
    <none>
Description:

Example usage:

See also:
    *TWO_DIMENSIONAL.

---------------------------------------------------------------------------

    *BASIC_BLOCK

This creates a single rectangular element of a given size and position.

Data:
    //
    imat
    xlow, xwidth
    ylow, ywidth
    << zlow, zwidth >>
    //
   
    where:
    imat is the material type to assign to the element
    xlow, ylow, zlow is the coordinate of the lower left hand corner of 
this element
    xwidth, ywidth, zwidth are the widths of this element in the x,y and 
z direction respectively.

Description:
    This module creates a single element: a 4-node quadrilateral in 2D, 
an 8-node brick in 3D, given its size and position. This provides a 
method for creating a 'starting' place for mesh-generation. This module 
is typically followed by *DANBLOCKS to fill in the block with more 
elements and/or change the material type, then futher module to shape 
and transform. the mesh

Example usage:
    *TWO_DIMENSIONAL
    *BASIC_BLOCK
    # This creates 2 elements :
    # A long thin base layer with centered block (60m by 30m) above;
    # The starting point for caisson footing model
    3                   # soil is material type 3
      0.  100.            # 100m by 20m deep
      0.   20.            
    2                   # caisson is material type 2
     20.   60.            # 60m wide by 40m high
     20.   30.            # centered on the base-layer

See also:
    *SLOPE_MESH, *REGULAR_MESH

---------------------------------------------------------------------------

    *REGULAR_MESH

This creates a rectangular grid of 4 node quads of unit width and height
(8 node bricks in 3D).

Data:
    nxe, nye, < nze >

    where:
    nxe, nye, nze are the number of elements in the x, y and z 
directions respectively.

Description:
    This ÿmodule ÿcreates a simple regular mesh of simplex ÿelements ÿ(4 
node ÿquadrilaterals ÿin 2D, ÿ8 ÿnode bricks in 3D). ÿÿThe ÿelements ÿare 
numbered in nxe first, then nye (then nze). This module may be used as a 
starting point for mesh-generation. 

Advanced usage:
    This module may used in any dimension not just 2D or 3D, for example 
it can be used to produce a line of 1D elements or a hypothetical mesh 
of 4D elements.

Example usage:
    *TWO_DIMENSIONAL
    # Create a regular mesh of 10 by 10 elements
    # then scale to give an overall mesh of 12m by 6m/
    *REGULAR_MESH 
     10 10
    *SCALE_MESH
     1.2 .6 

See also:
    *LAYERED_SLOPE, *SLOPE_MESH, *BASIC_BLOCK, *DANBLOCKS

---------------------------------------------------------------------------

    *LAYERED_SLOPE

A method for creating meshes of muliple layers of ground of varying 
thicknesses.

Data:
    nlayers
    //
    x, y(1:nlayers+1)
    //

    where:
    nlayers is the number of soil layers to create.
    x,y(:) are the x-coordinate and y-coordinates of each vertical 
'stage-line'

Description:
    Ths ÿmodule allows a direct way of producing meshes where ÿthe ÿsoil 
lies in bands of continously varying thickness. ÿThe mesh produced is in 
the ÿform ÿof 4-noded quadrilaterals with each layer given ÿa ÿdifferent 
material property number. (the lowest layer is material type 1) One line 
of ÿdata is needed for each 'stage-line'. ÿThese are the vertical ÿlines 
between each element, ÿhence for nxe element in the x-direction, ÿÿnxe+1 
lines of data are required. 

Advanced Usage:
    Since ÿthe ÿmesh ÿproduced is in the form ÿof ÿ4-node ÿquadrilateral 
elements, ÿÿwith only one element per layer then we would normally ÿthis 
module ÿwith ÿ*DANBLOCKS ÿto both increase the number ÿof ÿelements ÿand 
change ÿthe ÿelement type to (say) ÿ8-noded quadrilaterals. ÿÿSimerlarly 
*DANBLOCKS may be used transform the mesh into 3D. 
    If ÿone ÿof the 'seams' ÿpeters out, ÿthen the intersection ÿof ÿthe 
layers ÿabove ÿand ÿbelow will coincide in the ÿdata. ÿÿThe ÿ'zero-area' 
elements ÿproduced ÿin ÿthis collapsed layer may then ÿbe ÿdeleted ÿwith 
*DELETE_ZERO_AREA_ELEMETS ÿÿand ÿthen ÿÿthe ÿÿÿsides ÿÿÿrejoined ÿÿÿwith 
*JOIN_COINCIDENT_NODES 


Example usage:
    *LAYERED_SLOPE
    #  This is model of 3 layers of soil with 5 columns of elements
    # the base is horizontal: the surface slopes downhill.
    3
    0.   0.   1.0   2.0   3.9
    2.   0.   1.1   2.2   3.7
    4.   0.   1.2   2.4   3.5
    6.   0.   1.3   2.6   3.4
    8.   0.   1.4   2.4   3.3
   13.   0.   1.5   2.2   3.2

See also:
    *SLOPE_MESH, *DANBLOCKS

---------------------------------------------------------------------------

    *PAUSE

Data:
    <none>
Description:
    This simply pauses the processing of keywords until <CR> is pressed. 
This is of principle for debuging and when developing new meshes and 
modules.

Example usage:

See also:
    *STOP, *PLOT_MESH 

---------------------------------------------------------------------------

    *SLOPE_MESH

A direct method for creating regular meshes with ÿoptional ÿsloping face 
cut-out. 

Data:
    ndim, nod
    nxe,nxs
    top(1:nxe+1)
    bottom(1:nxe+1)
    nye,nys
    depth(1:nye+1)
<< and if ndim=3 :
    nze
    width(1:nze+1)   >>

    where 
      ndim is either 2 or 3 (for 2D or 3D meshes)
      nod is the number of nodes per element (8 in 2D, 8,14 or 20 in 3D)
      nxe,nye,nze are the number of elements in each of the x,y and z 
directions respectively. 
      nxs and nys are the number of elements in the upper 'slope' part
      top(:) are the x-coordinates across the top of the mesh
      bottom(:) are the x-coordinates across the bottom of the mesh
      depth(:) are the y-coordinates of all the rows
      width(:) are the z-coordinates though the thickness of the model

Description:
    This ÿmodule allows the direct creatation of 'block'-style meshes of 
elements. In 2 dimensions, these are 8-node quadrilaterals (8nq's), in 3 
dimensions, ÿthese may be 8,14 or 20 node brick elements for example all 
meshes in Chapter 6 of the Manchester FE book [1]=[Smith and Griffiths]. 
In ÿits simplest form, ÿnxs is set to be equal to nxe, ÿnys is set to be 
equal to nye and bottom(:) ÿis given the same values as top. ÿThis ÿwill 
produce a  regular 'block' ÿ(eg book60.dat). ÿIf top are given different 
values to bottom, then the 'vertical' mesh lines will slope, for example 
the slope stabilty mesh of 'book61.dat'. ÿIf ndim is set to 3 and so nze 
and ÿwidth(:) ÿÿsupplied, ÿthen the 3D equivalents of these ÿmeshes ÿare 
produced with nze elements into the plane of the 2D cross-section. 

Advanced Use:
    If nxs and nys are set to less than nxe and nye respectively, then 
only nxs by nys elements in the top left hand corner of the mesh are 
inclined; all elements below are set to the constant breadth values as 
given in bottom(:). This allows meshes with a cut-out at the top-right 
to be built: for example an exacavated trench, or (if top(:) /= 
bottom(:)), a slope, resting a foundation layer.
    Note, that values in top(:) and bottom(:) should increase, but vlues 
in depth(:) and width(:) should decrease (ie. the most-postive should 
come first).
    These meshes all have parallel mesh-lines in the x and z directions. 
If a mesh with parallel vertical but inlined horizontal lines is 
desired, then this module may be used followed by *X_TO_Y to exchange 
the x- and y- coordinates over.

Example usage:
    *SLOPE_MESH
    #....... book61.dat from Smith and Griffiths.
    #....... a 1m high slope of slope 1 in 2
       2   8
       5 5
       0.   0.4   0.6   0.8   1.0   1.2
       0.   1.0   1.8   2.4   2.8   3.2
       5 5
       0.  -0.2  -0.4  -0.6  -0.8  -1.0

 << nice to have an example #2 for a 3D mesh of 14 node bricks >>

See also:
    *BASIC_BLOCK (with *DANBLOCKS)

---------------------------------------------------------------------------

    *ELEMENT_TYPES

A method for assigning element property groups based on the x,y and z 
indeces of the position of an element within a regular block.

Data:
   //           ( // indicates the repeatable portion )
   ixl,ixh    iyl,iyh    << 3d: izl, izh >>,   imat
   //

    where:
    ixl, ixh are the low and high element postioned numbered in the 
x-direction. iyl,iyh, izl,izh are the y and z equivalents.
    imat is the material property type to assign to this block

Description:
    This routine provides a simple way to assign the material property 
number to a rectangular (a cube in 3D) block of elements. The desired 
elements are specified by the integer 'coordinate' within the mesh. 
By default all materials are of type 1, so only regions of other 
materials types need to be specified.

Advanced Usage:
    A Element's material type may be set and reset, several times For 
Example when producing a mesh of a pile within a soil of 3 layers, we 
first set the horizontal soil layers, then finaly the pile whose 
property number will replace that of the various soil layers that it 
passes through.
    More complex geometries than that possible simply by using 
*SLOPE_MESH can be constructed. For example by setting the material type 
of a patch to (say) zero then following this module with 
*DELETE_MATERIALS to remove those elements, meshes with muliple cut-outs 
can be produced. For the above case of a piled foundation, this can be 
used to remove material either side of the pile above ground level to 
give a free-standing pile.

Example usage:
    *ELEMENT_TYPES
    # This takes a mesh of 9 by 10 elements and gives 2 layers with an
    # embedded pile.
    # material type 1 is the default and so could have been omitted
    1    1 9    3  5     # The upper soft soil-layer
    2    1 9    6 10     # The lower firm soil-layer
    0    1 9    1  2     # The 'Air' above ground surface
    3    5 5    1  7     # The Pile itself


See also: 
    *MATERIALS_BY_BOX, *DELETE_MATERIALS 

Caveats: 
    This routine is only to be used where the mesh was created using the 
*SLOPE_MESH module. 


---------------------------------------------------------------------------

    *IMPORT_OFF

This reads in a mesh in the 'Object File Format' <ref>

Data:
    NN, NEL, NEDGES
    (x(i),y(i),z(i),i=1,nn)
    (nod,(num(1:nod,i),i=1,nel)

    where:
    nn is the number of nodes (vertices) in the mesh
    nel is the number of elements (polygons) in the mesh
    nedges is the number of edges (unused in FE)

Description:
    This module allows the importing of pre-defined meshes produced in
the 'Object File Format'.The 'Object File Format' (OFF) is a set of 
polygonal elements where each node has a geneal x,y,z coordinate. Such 
meshes can be either planar meshes, or more usually curved shells, for 
example the VW 'beetle' dataset. OFF is one of the de-facto standards 
for the interchange of mesh data. 
    The material property type of each element is set to be the same as
the number of nodes in that element. 
    The '< filename' input redirection token is usually used to cause 
the actual mesh data to be read from a secondary file rather than from 
the main Keyword-based input file.
    A side effect of this module is that the number of dimensions is set 
to 3.

Advanced usage:
    OFF elements may in general have any number of nodes. Currently 
elements of 3 and 4 nodes are handled as 3-node triangles and 4-node 
quads. respectively. elements of 5 or more  sides/nodes are handled but 
since pentagons, hexagons do not currently exist as finite elements, 
these must be manually removed or modified if they exist in the mesh, 
before the *ANALYSE phase. Polygons of greater than 27 nodes are 
not supported. Since the material property is set to the same as the 
numberof nodes per element, these can easily be seen and eliminated, 
before converting the remaining 3 and 4 noded elements to the same 
material type (say) before analysis.

Example usage:
    *IMPORT_OFF
    # This imports from a secondary file the classic
    # Volkswagon beetle dataset (1232? shell elements)
    < c:\OFF_FILES\VW.GEO

See also:
    *IMPORT_PL, *IMPORT_NFF, *IMPORT_SRF, *IMPORT_3D, 
*IMPORT_TECPLOT_MESH, *WRITE_OFF, *WRITE_RAYSHADE, *WRITE_DANPLOT 

Caveats:
    This implimentation just handles the '*.geom' file of geometries. The 
table of vertex coloues '*.pcol'? are not relevant. The '*.aoff' file of
textual information is currently not supported, although in the future 
this will used to get the titles, etc.

---------------------------------------------------------------------------

    *IMPORT_PL

This imports a mesh in the DANPLOT style, fixed mesh decription format

Data:

Description:
 .. etc...

Example usage:

See also:
    *NODAL_COORDINATES, *ELEMENT_STEERING

--------------------------------------------------------------------------

    *IMPORT_SRF

This imports a mesh in the SRF datafile format.

Data:

Description:
    This module will import a mesh in the SRF datafile format as used by 
the <.?.> shareware program.


Example usage:

See also:
 *IMPORT_OFF, *IMPORT_PL, *IMPORT_NFF, *IMPORT_SRF, *IMPORT_3D, 
 *WRITE_OFF, *WRITE_RAYSHADE, *WRITE_DANPLOT 

---------------------------------------------------------------------------

    *IMPORT_3DEDIT

This will import a mesh in the .3D format

Data:

Description:
    This module will import a mesh in the .3D datafile format as used by 
the 3D-EDIT (tm) program.

Example usage:

See also:
 *IMPORT_OFF, *IMPORT_PL, *IMPORT_NFF, *IMPORT_3D, 
 *WRITE_OFF, *WRITE_RAYSHADE, *WRITE_DANPLOT 

---------------------------------------------------------------------------

      *IMPORT_TECPLOT_MESH

This imports a mesh in format used by the TECPLOT (tm) finite element 
visualisation program.

Data:
    A file containing a description of the mesh  and the coordinates of the elements
    For full details refer to the TECPLOT user-guide.

Description:
    This module will parse a TECPLOT (tm) format datafile to abstract 
the geometric mesh data. ... 


Example usage:
     *IMPORT_TECPLOT_MESH
     # read the tecplot demonstration space vehicle dataset
     # as a mesh only (ignoring temperatures and pressures)
     < c:\tecplot\data\space.dat

See also:
 *IMPORT_OFF, *IMPORT_PL, *IMPORT_NFF, *IMPORT_SRF, *IMPORT_3D, 
 *WRITE_OFF, *WRITE_RAYSHADE, *WRITE_DANPLOT 

Caveats:
    Currently, only a sub-set of all the possible forms of TECPLOT data are 
supported, for example the 'IJK' form for shells and surfaces. This 
module only handles the geoemtry held in the data files and ignores 
other information such as computed pressures.

---------------------------------------------------------------------------

      *PLOT_MESH

Simply plots the current elements and nodes if a suitable graphics 
device is present.

Data:
    <none>

Description:
    This module pauses until the <CR> ÿkey is pressed, ÿthen clears ÿthe 
screen ÿand ÿdraws ÿthe current mesh. ÿOn a PC with a VGA ÿscreen, ÿÿthe 
backgraound ÿis blue, ÿwith the nodes in red and the elements in ÿwhite. 
The view is held until the next press of <CR>. 
    For ÿmore advanced plotting options including full three-dimensional 
shading, the DANPLOT package should be used. 

Example usage:
    ..
    *DELETE_MATERIAL
     3        # deleet all elements of type 3
    # now plot the mesh to check
    *PLOT
    ..

See also:
    <n/a>

---------------------------------------------------------------------------

    *WRITE_DANPLOT

Data:

Description:

Example usage:

See also:
---------------------------------------------------------------------------

    *EXPORT_OFF

This exports in a mesh in the 'Object File Format' <ref>

Data:
    <none>

Output data:
    NN, NEL, NEDGES
    (x(i),y(i),z(i),i=1,nn)
    (nod,(num(1:nod,i),i=1,nel)

    where:
    nn is the number of nodes (vertices) in the mesh
    nel is the number of elements (polygons) in the mesh
    nedges is the number of edges (unused in FE)

Description:
    This module allows the exporting of polygonal meshes in
the 'Object File Format'.The 'Object File Format' (OFF) is a set of 
polygonal elements where each node has a geneal x,y,z coordinate. Such 
meshes can be either planar meshes, or more usually curved shells, for 
example the VW 'beetle' dataset. OFF is one of the de-facto standards 
for the interchange of mesh data. 
    Most Ray-tracing packages and other scene-drawing programs can 
accept OFF geometry files. This module allows these to be used to 
produce high quality images of the mesh.

Example usage:
    *IMPORT_PL
    # read a standard DANPLOT style mesh
    ..
    *EXPORT_OFF
    # write the mesh in Object File Format for raytracing
    > c:\OFF_FILES\VW.GEO

See also:
    *IMPORT_PL, *IMPORT_NFF, *IMPORT_SRF, *IMPORT_3D, 
*IMPORT_TECPLOT_MESH, *WRITE_RAYSHADE, *WRITE_DANPLOT 

Caveats:
    This implimentation just handles the '*.geom' file of geometries. The 
table of vertex coloues '*.pcol'? are not relevant. The '*.aoff' file of
textual information is currently not supported, although in the future 
this will used to get the titles, etc.

---------------------------------------------------------------------------

     *EXPORT_RAYSHADE

Output a file of polygons suitable be raytracing. 

Data:

Description:
    This ÿexports the mesh as a set of polygons in RAYSHADE ÿformat, ÿÿa 
public-domain raytracing package. .. 

Advanced usage:
    Since in general only the surface of a mesh is visible, it will 
greatly speed up the raytracing package if all internal and/or 
backfacing polygons faces are removed before writing the Rayshade file.
This may be done within the DANPLOT package (q.v.).
 
Example usage:
    *EXPORT_RAYSHADE
   ...

See also:
    *IMPORT_PL, *IMPORT_NFF, *IMPORT_SRF, *IMPORT_3D, 
*IMPORT_TECPLOT_MESH, *WRITE_RAYSHADE, *WRITE_DANPLOT, 
*POLYGONISE_3D_ELEMENTS

Caveats:
    Rayshade files consist of a set of polygons in 3D space. Before 
writing this file, 3D elements such as 20-node bricks should first be 
decomposed into a set its six faces using *POLYGONISE_3D_ELEMENTS.

---------------------------------------------------------------------------


TRANSFORMING THE MESH

The next section describes those modules which transform an existing 
mesh. This includes simple operations such as scaling or translating the 
mesh, through to routines that (say) morph a rectangular mesh around a 
circular arc. The most significant module is *DANBLOCKS. This allows the 
generalised changing of one element type into another and sub-dividing 
each element is smaller sub-elements.

---------------------------------------------------------------------------

    *DANBLOCKS

This major module handles all cases where it is desired to change the 
fineness of a mesh or the type of elements used.

Data:
    //
    imat
    ndime, nod, itype
    nxe, (x_widths(1:nxe)
    nye, (y_widths(1:nye)
    << and in 3D: nze, (z_widths(1:nxe) >>
    //

    where:
    imat : is the material type to change (may be a wildcard)
    ndime: is the number of dimesnions for the new element
    nod  : is the number of nodes per new element
    itype: is the element type (almost always = 1)
    nxe, nye, nze : are the number of sub-elements in each new direction
    x_widths(:). y_widths(:), z_widths(:) are the relative widths of the 
newly created sub-elements.

Description:
    This module ..

Advanced usage:
    In is possible to transform (say) an existing 2D mesh into 3D by 
specifying NDIME as 3, when the current number of dimensions is only 2.
The new mesh contains nze elements in the z-direction (through the 
thickness) of widths directly as given in z_widths.
    It is also possible to divide a quadrilateral mesh into pairs of 
triangles. In this case, if ndime,nod,itype = 2,15,1 then all 
qaudrilateral elements will be replaced by a pair of 15-node triangles; 
the shape functions of the parent element being used to interpolate the 
position of the new nodes. The new elements are split across a 'south 
west' to 'north east' dividing line. More specific disections are 
possible using other modules; for example *4NQ_TO_4_3NTS will take each 
4 node quadrilateral create a central node, then it split each element 
into four 3-node triangles each having a vertex at the central node.

Example usage:
    ..
    # Example No.1
    # this replaces all elements that were (say) 4nq's with 8 node quads.
    *DANBLOCKS
     1              # Handle material type 1
     2 8 1          # into 2D, 8-noded (type 1) elements
     1 1.           #  - with no disection
     1 1.           #  - ie. as 1 by 1 sub-elements
    ..     

    ..
    # Example No.2 (more advanced)
    # this replaces all eleemnt in a 3D mesh with
    # 4 elements in x (unequally spaced)
    # and 2 equally spaced elements in each of the y and z directions
    #
    *DANBLOCKS
     230964         # Handle ALL materials by wildcard
     3 20 1         # into 3D, 20-noded brick elements
     5   5. 2.5 1. 1. .5  # in the ratio 50% : 25% : 10 % : 10% : 5 %
     3   1. 1. 1.   # three equally spaced in 'y'  
     3 3*1.         # three equally spaced in 'z' (note: repeat count)
     ..

    ..

    # example No.3 (advanced)
    # This projects a 2D mesh into 3D replacing each (quadrilateral) 
    # element with 2 15-node 'toblerone' wedge eleemnts.
    #
    ..
    *DANBLOCKS
    1             #
    3 15 1        # 3D wedge element (create 2 per quadrilateral)
    1 1.          # just one element 
    1 1.          # in x-y-z (no disection)
    1 125.        # the new 'z' thickness is 125m.
    ..

See also:
    *4NQ_TO_4_3NTS, etc.?

---------------------------------------------------------------------------

    *CHANGE_MATERIALS

This allows the material property numbers to be swapped with new values.

Data:
   //
   imat_old,  jmat_new
   //

   where
   imat_old is the 'old' material type,
   imat_new is the 'new' type to replace the old.

Description:
    This module allows the number associated with each element as its 
'material property number' to be changed.

Advanced use:
    By means of a temporary number, the numbers of two materials may be 
exchanged, for example to make sure that material 1, is the first to be 
placed in the case of build-up finite element models. Also the finite 
element code DANFE, automatically skips all elements with zero or 
negative material numbers. Thus blocks of elements can selectively be 
included/ excluded from certain load-stesps without altering the overall 
mesh.

Example usage:
      *CHANGE_MATERIALS
      3   0     # mark type 3 as 'zero' for possible deletion
      5  99         # swap over
      6   5         # material types
      99  6         # 5 and 6
 

See also:
    *DELETE_MATERIALS, *MATERIALS_BY_BOX, *ELEMENT_TYPES

---------------------------------------------------------------------------
    *MATERIALS_BY_BOX

This sets the material number of those elements that lie within a given box

Data:
    //
    imat, xlo, xhi, ylo, yhi, <zlo,zhi>
    //

    where:
    imat : the desired material property number.
    xlo  : is the lower value of x, and
    xhi  : is the upper value of x that define the x-range
    ylo, yhi, zlo, zhi simmilarly define the y and z ranges of the box.

Description:
    This module lets a certain material property code be associated with 
all elements whose centres lies within a certain box. The box is given 
by the lower and upper value of the x, y (and z) coordinates. These may 
lie outside the bounds of the mesh.

Example usage:
    *MATERIALS_BY_BOX
    3    0.   20.   -150. 200.       # type 3
    2 -999.  999.     10.  15.       # type 4 (for ALL x-values)

See also:
    *ELEMENT_TYPES, *DELETE_MATERIALS, *CHANGE_MATERIALS

---------------------------------------------------------------------------

    *DELETE_MATERIALS

This deletes all elements of given material type(s)

Data:
    //
    imat
    //

    where
    imat : the material type of the elements to delete

Description:
    This module will remove from the element database all element that 
match the given material type. It does not directly remove the nodes 
however. to delete them, follow *DELETE_MATERIALS with a 
*DELETE_ORPHAN_NODES module.

Advanced usage:
    One reason why the nodes are not automatically deleted is so that we 
can re-mesh the area with other elements; for example Delauney 
triangularisation of the domain using the *TRIANGULARISE module.
    To delete a given area of the mesh we can precede *DELETE_MATERIALS
by either *MATERIALS_BY_BOX, or *ELEMENT_TYPES to give a set of elements 
an arbitary material tpye prior to deleting them.

<< hmm nice to tell *TRIANGLE a sub-set of nodes so that we can do it by
material type it stages >>

Example usage:
    *DELETE_MATERIALS
    # delete all the air elements above the mesh 
    # and remove the berm at the toe of the dam
    0      # 'Air' elements
    5      # the berm material

See also:
    *MATERIALS_BY_BOX, *DELETE_ORPHAN_NODES, *DELETE_ZERO_AREA_ELEMENTS,
 *TRIANGULARISE, *CHANGE_MATERIALS

---------------------------------------------------------------------------
    *DELETE_ZERO_AREA_ELEMENTS

This deletes those elements that have becomes degerate and have no 
volume 

Data:
    <none>

Description:
    This will remove all elements form the mesh that enclose no area.
These can arise deliberately (for example in *LAYERED_SLOPE) where a 
material layer such a coal-seam within the mesh peters out. Such 
elements do not really exist and can cause problems in analysis since 
they have zero area and hence zero stiffness  causing numerical 
problems.
    All ÿthe elements of the mesh are examined. ÿFor the centre of ÿeach 
element, ÿÿthe Jacobian matrix is produced. ÿIf the determinant of ÿthis 
matrix is zero (using a small tolerance), then the element is considered 
degenerate ÿÿand ÿis ÿremoved. ÿÿThe ÿnodes ÿare ÿnot ÿremoved ÿand ÿÿso 
*DELETE_ZERO_AREA_ELEMENTS is normally followed by *DELETE_ORPHAN_NODES. 
    When we are removing an infinitely thin layer from the centre of a 
mesh then we normal which to connect the elements abobe and below this 
layer back to each other, and so *DELETE_ZERO_AREA_ELEMENTS is normally 
preceded by *COLLAPSE_COINCIDENT_NODES.

Example usage:
    # Remove a vestiage coal-seam layer from a mesh
    # first we join the two sides, then delete the row
    # finally we remove any orphaned nodes
    *COLLAPSE_COINCIDENT_NODES  
    *DELETE_ZERO_AREA_ELEMENTS
    *DELETE_ORPHAN_NODES


See also:
    *DELETE_ORPHAN_NODES, *COLLAPSE_COINCIDENT_NODES

---------------------------------------------------------------------------

    *DELETE_ORPHAN_NODES

This will remove all nodes that have become 'orphaned'.

Data:
    <none>

Description:
    Orphan nodes are those which have no element attached to them. These 
may arise when elements have been deleted, when a element is changed 
to one of a different number of nodes, or simply from points used in 
construction. These nodes may be left in the mesh but can break some FE 
programs that assume that all nodes present contribute to the stiffness 
matrix and so are assigned freedoms by default.
    A side effect of this is that the nodes numbers are re-sequenced to 
close up the 'gaps'.

Example usage:
    # delete all elements of material type 7
    *DELETE_MATERIALS                        
     7 
    # clean up the mesh by removing stray nodes
    *DELETE_ORPHAN_NODES

See also:
    *COALESCE_COINCIDENT_NODES

---------------------------------------------------------------------------

    *COALESCE_COINCIDENT_NODES

This gives all nodes that at the same location the same node number.

Data:
    <none>

Description:
    Sometimes several nodes can end up at the same point in space. This 
can occur when joining two seperate meshes together, or when a mesh is 
warped so that seperate edges are brought together. In order for the FE 
program to know that the two are connected, it is convenient to 
give them the same node number. This also reduces the number of nodal 
coordinates that need to be stored.
    A ÿside ÿeffect ÿof this module is that the nodes ÿnumbers ÿare ÿre-
sequenced to close up the 'gaps'. 


Example usage:
    *IMPORT_PL
    < c:\pl_files\ground.dat     # The first mesh 
    *IMPORT_PL                   #    added
    < c:\pl_files\building.dat   # to the second mesh
    # Now remove the 'extra' nodes on the interface.
    *COALESCE_COINCIDENT_NODES

See also:
    *DELETE_ORPHAN_NODES

Caveats:
    This algorithm uses a simple search of order nn^2 where nn is the 
number of nodes and some can become slow when the mesh is very large.
A faster method using octress is currently being developed.
 
---------------------------------------------------------------------------

    *SORT_NODES_FROM_POINT

Renumbers all the nodes in order of distance from a given point. 

Data:
   Xp, Yp, < Zp >

   where
   xp,yp,zp is the coordinate in space of the sorting point.

Description:
    Given ÿa ÿpoint ÿin space, ÿthis sorts the nodes in order ÿof ÿtheir 
distance ÿfrom ÿthis ÿpoint, ÿsuch that the node nearest to ÿthis ÿpoint 
becomes node number '1'. Although, this routine is not really necessary, 
some older FE programs use banded matrix solvers where ÿthe freedoms are 
numbered in the same order as the nodes. without sorting, ÿthis can lead 
to unacceptably large bandwidths. 
          
This sorts all the nodes 

Example usage:
    *IMPORT_PL
    < 'c:\pl_files\footing.pl'
    # Now renumber the nodes from left to right, top to bottom
    *SORT_NODES_FROM_POINT
      -999. 85.

See also:
    *SORT_ELEMENTS_FROM_POINT

Caveats:
  This routine currently relies on a Salford routine to rank the set of 
distances into order. Hence currenly this routine only works on the PC.
A general version of this module is being developed.

---------------------------------------------------------------------------

    *SORT_ELEMENTS_FROM_POINT

Renumbers all the elements in order of distance from a given point. 

Data:
   Xp, Yp, < Zp >

   where
   xp,yp,zp is the coordinate in space of the sorting point.

Description:
    Given ÿa ÿpoint in space, ÿthis sorts the elements in order ÿof ÿthe 
distances ÿof ÿtheir ÿcentres from this point, ÿsuch ÿthat ÿthe ÿeleemnt 
nearest ÿto ÿthis point is becomes element number '1'. ÿAlthough, ÿÿthis 
routine ÿis ÿnot really necessary, ÿÿsome older FE programs ÿuse frontal
matrix solvers. where ÿthe maximum instataneous front-width, is a 
function of the biggest difference in element number at a node.

Example usage:
    *IMPORT_PL
    < 'c:\pl_files\footing.pl'
    # Now renumber the elements from left to right, bottom to top
    *SORT_ELEMENTS_FROM_POINT
      -999. 0.

See also:
    *SORT_NODES_FROM_POINT

Caveats:
  This routine currently relies on a Salford routine to rank the set of 
distances into order. Hence currenly this routine only works on the PC.
A general version of this module is being developed.

---------------------------------------------------------------------------
    *4_3NTS_TO_4NQ

This restores a triangular mesh back to quadrilaterals

Data:
    <none>

Description:
    Many triangular meshes are composed of a grid of parent 
quadrilaterals where each quadrilaterial has been disected into 4 
triangles via the creation of a central common node. This restores each 
set of 4 triangles back into a quad, checking that they do indeed share 
a common node.

Example usage:
    # take a mesh then split into triangles
    *4NQ_to_4_3NTS
    ..
    # now try and restore the original quadrilaterals
    *4_3NTS_TO_4NQ

See also:
    *4NQ_TO_4_3NTS, *4NQ_TO_2_3NTS, *2_3NTS_TO_4NQ

Caveats:
    This module currently only handles 3-node triangles and 4-node 
quadrilaterals.

---------------------------------------------------------------------------

    *2_3NTS_TO_4NQ

This restores a triangular mesh back to quadrilaterals

Data:
    <none>

Description:
    Many triangular meshes are composed of a grid of parent 
quadrilaterals where each quadrilaterial has been disected into 2 
triangles. This restores each 
pair of 2 triangles back into a quad, checking that they do indeed share 
a common edge.

Example usage:
    # take a mesh then split into triangles
    *4NQ_to_2_3NTS
    ..
    # now try and restore the original quadrilaterals
    *2_3NTS_TO_4NQ

See also:
    *4NQ_TO_2_3NTS, *4NQ_TO_4_3NTS, *4_3NTS_TO_4NQ

Caveats:
    This module currently only handles 3-node triangles and 4-node 
quadrilaterals.

---------------------------------------------------------------------------
    *4NQ_TO_4_3NTS

This each quadrilateral in a mesh to 4 triangles

Data:
    <none>

Description:
    This ÿcreates ÿa mesh of triangular elemnts from one composed ÿof ÿa 
grid ÿof ÿparent ÿquadrilaterals.
For each element, a centoidal node is created. For 3-noded triangular 
element are created out of each quadrilateral such the third node of 
each triangle is this new central node.

Example usage:
    # take a mesh then split into triangles
    *IMPORT_PL
    < c:\pl_files\footing.pl
    *4NQ_to_4_3NTS

See also:
    *4_3NTS_TO_4NQ, *4NQ_TO_2_3NTS, *2_3NTS_TO_4NQ

Caveats:
    This module currently only handles 3-node triangles and 4-node 
quadrilaterals.

---------------------------------------------------------------------------

    *4NQ_TO_4_3NTS

This each quadrilateral in a mesh to 2 triangles

Data:
    <none>

Description:
    This ÿcreates ÿa mesh of triangular elemnts from one composed ÿof ÿa 
grid ÿof ÿparent ÿquadrilaterals.
Two 3-noded triangular 
element are created out of each quadrilateral such the first edge of 
each triangle is along the diagonal of the parent.

Example usage:
    # take a mesh then split into triangles
    *IMPORT_PL
    < c:\pl_files\footing.pl
    *4NQ_to_4_3NTS

See also:
    *4_3NTS_TO_4NQ, *4NQ_TO_2_3NTS, *2_3NTS_TO_4NQ

Caveats:
    This module currently only handles 3-node triangles and 4-node 
quadrilaterals.

---------------------------------------------------------------------------

    *MIRROR_EVEN_ELEMENTS

This reverses the node order of every alternate element

Data:
    <none>

Description:
    Some meshes do not respect a consistant clockwise or anti-clockwise
order of nodes around an element. This module treats the special case, 
where the even numbered elements are number the 'wrong' way around, by 
reversing the order of the nodes (keeping the first of the nodes 
intact).
    This is an example of a special purpose module, written to fix a 
particular problem. Its source code is gven here as an example of how to 
structure a user-written module for those with access to the source-code 
and a compiler.

Example usage:
    # take a mesh then reverse node order of alternate elemnents
    *IMPORT_PL
    < c:\pl_files\footing.pl
    *MIRROR_EVEN_ELEMENTS

See also:
    <n/a>

---------------------------------------------------------------------------

    *MOVE_NODES

This moves a node from one location to another

Data:
    //
    x1, y1, <z1>, x2, y2, <z2>
    //
    where:
    x1,y1,z1 is the coordinate of the old node,
    x2,y2,z2 is its new coordinate.

Description:
    This module simply moves nodes from one position to another. For 
example to move the position of the crest of a slope before filling the 
domain with elements. The coordinates of a point in space is read in, 
any point that is within a small tolerance of this point will have its 
coordinate changed to the new value. A warning is given if no nodes are 
found at the specified location.

Advanced use
    Often a whole set of nodes may need to be moved, for example to turn 
a pile square in section into a round one. Here all the nodes with a 
given x and z value but any y value need to be moved. In the input any 
of x1,y1 or z1 may be given the wildcard value of 230964, to indicate 
that it can take any value. In such cases the corresponding x2,y2,z2 
value is a dummy, since it will take the same value as the original.


Example usage:

    # move the 4 corners of a square towards (5,5) by the same amount
    *MOVE_NODES
     0.  0.  1. 1.    # lower left
     0. 10.  1. 9.    # top left
    10. 10.  9. 9.    # top right
    10.  0.  9. 1.    # lower right


See also:
    <n/a>

---------------------------------------------------------------------------

    *SHIFT_MESH

This simply moves the entire mesh by a given offset

Data:
   xd, yd, <zd>

   where
   xd, yd, zd are the amounts to move the mesh by in x, y and z.

Description:
    This module will add a given amount to all nodes in the mesh, for 
example to make the line of zero y-cordinate the ground surface, rather 
than some other datum, or when combining 2 meshes together.

Example usage:
    *IMPORT_PL
    < c:\pl_files\ground,dat
    # now move the mesh so that the building 
    # will be in the righht position
    *SHIFT_MESH
      10. 0. 
    *IMPORT_PL
    < c:\pl_files\building,dat

See also:
    *SCALE_MESH

---------------------------------------------------------------------------

    *SCALE_MESH

This simply scales the entire mesh by a given amount

Data:
   xs, ys, <zs>

   where
   xs, ys, zs are the amounts to scale the mesh by in x, y and z.

Description:
    This module will multiply the coordinates of all nodes in the mesh 
by a given amount, for example when changing the cordinates system from 
feet to metres.

Advanced Usage:
    The xs, ys and zs factors do no have to be postive. Sometimes meshes 
are produced in a 'left-handed' Cartession sytem when we need a right-
handed axes set for analysis or plotting. In which case an odd number of 
xs,ys,zs need to be -1 (usualy as xs,ys,zs = 1,1,-1). This can also 
occur in 2D when we have a y-axis than is number positive downwards 
instead of the usual case of it pointing upwards.
We can also 'squash out' the depth of model by using xs,ys,zs= 1,1,0. or 
exagerate the vertical coordinates for clarity when plotting the mesh.   

Example usage:
    *IMPORT_PL
    < c:\pl_files\ground,dat
    # original data was in feet, so convert to metres.
    *SCALE_MESH
      .3075 .3075

See also:
    *SCALE_MESH, *X_TO_Y_TO_Z

---------------------------------------------------------------------------

    *X_TO_Y_TO_Z

This cycle the x, y and z coordinates.

Data:
    <none>

Description:
    Some programs use different axes notation; for example sometime the 
z-axis is made to be the vertical one instead of the traditional use of the 
y-axis. This module changes the coordinate of every node such that x 
becomes y, y becomes z and z becomes x, (such that three invokations of 
this routine wil restore the original case). 

Example usage:
    *IMPORT_PL
    < c:\pl_files\z_is_up.dat
    # now topple to put the z coordinate properly in the horiz. plane
    *X_TO_Y_TO_Z

See also:
    *SCALE_MESH, *SHIFT_MESH, *X_TO_Y

---------------------------------------------------------------------------

    *X_TO_Y 

This swaps the x and y coordinates over

Data:
    <none>

Description:
    This module has the effect of rotating the mesh by 90 degrees anti-
clockwise. The original x coordiante becomes the new y coordinate and 
the the original y coordinate becomes the new *negative* x coordinate. 
This may be done when performing an axisymmetric analysis of a flywheel 
so the radial direction becomes the local x-axis. This may be swapped 
back for plotting. Also some basic mesh generation modules such as 
*LAYERED_SLOPE and *SLOPE_MESH may be used then translated into a 
differnt orientation. For example, using *SLOPE_MESH to create a spud-
can mesh, or *LAYERED_SLOPE to create the flywheel example. 

Example usage:

    *IMPORT_PL
    < c:\pl_files\flywheel.dat
    # now put the flywheel radius in the x-direction
    *X_TO_Y 


See also:
    *X_TO_Y_TO_Z

---------------------------------------------------------------------------

    *WRAP_AROUND_Y

This wraps the mesh around the y axis into a cylinder. 

Data:
    <none>

Description:
    This module procudes a cylinder from a reactangular 3D mesh. For 
each point; the x-coord is considered as a radius and the z-coord as the 
angle in polar coordinates. Thus to create a quarter circle, the z-coords 
should vary between 0. and 90. The x values should all be positive; the 
smallest value being the inner radius, the largest being the outer 
radius.

Advanced usage.
    The inner radius can be set to zero to give a solid cylinder, 
however this produce a line a singularities along the axis. This usualy 
causes no problems, but can cause degeneracies. If a complete cylinder 
is produced then *COALLESCE_COINCIDENT_NODES will join the two ends, but 
may have the side effect of increasing the bandwidth.

Example usage:
    *SCALE_MESH
     1. 1. 360.     # change a unit z-thickness to 360 degrees
    # now wrap into a cylinder
    *WRAP_AROUND_Y  
    # join the two coincident faces
    *COALESCE_COINCIDENT_NODES

See also:
    *WRAP_TWIST_Y, *WRAP_AROUND_Z

---------------------------------------------------------------------------

    *WRAP_AROUND_Z

This wraps a mesh around in the x-y plane for example to produce a 
quarter circle.

Data:               
    <none>

Description:
    This module will wrap a rectangular mesh into a cirular arc. In 
three dimensions, this procudes a cylinder around the z axis. For 
each point; the x-coord is considered as a radius and the y-coord as the 
angle in polar coordinates. Thus to create a quarter circle, the y-coords 
should vary between 0. and 90. The x values should all be positive; the 
smallest value being the inner radius, the largest being the outer 
radius.

Example usage:
    *SHIFT_MESH
    # move the inner radius to be 10m
     10.  0.      
    # now wrap into a quater circle
    *WRAP_AROUND_Z  

See also:
    *WRAP_AROUND_Y, *CIRCLE_A_SQUARE
                           
---------------------------------------------------------------------------
    *SQUARE_TO_CIRCLE
  !  *CIRCLE_A_SQUARE

This convert a square mesh into a circle or part thereof.

Data:
    <none>

Description:
    This module will reduce to coordinates of all points so that they 
lie on concentric arcs to give a whole or part circular disc. For each 
point then the maximum of its x or y coordinate is taken to be the 
necessary radius for that point. The points position is then scaled 
linearly along a line between this point and the origin to give the 
desired radius.

Example usage:
    #.... << define the mesh ? >>
    *CIRCLE_A_SQUARE
                       
See also:
    *WRAP_SPHERE, *WRAP_AROUND_Z

---------------------------------------------------------------------------
c.. hmm what is this ??
c      ELSEIF (KEYWORD.EQ.'*WRAP_CIRCLE_Y') THEN
---------------------------------------------------------------------------

    *CUBE_TO_SPHERE
 !   *WRAP_SPHERE

This will round a cubic mesh into a sphere or part-sphere

Data:
    <none>
Description:
    This module will reduce to coordinates of all points so that they 
lie on concentric spherical shells to give a whole or part sphere. For each 
point then the maximum of its x,y and z coordinate is taken to be the 
necessary radius for that point. The points position is then scaled 
linearly along a line between this point and the origin to give the 
desired radius.

Example usage:
    *WRAP_SPHERE

See also:
    *CIRCLE_A_SQUARE

---------------------------------------------------------------------------

    *WRAP_TWIST_Y

This rotates successive x-z planes about the y-axis to produce a 
corkscrew effect.

Data:
    <none>

Description:
    This module rotate x-z planes about the y-axis in proportion to the 
value of the y-coordinate at that point. This produces a spiraly twisted 
column from a straight one. If the column is not centered on the y-axis 
then this produces a 'corkscrew' or 'spiral-staircase' type mesh.
    This ÿis an example of a special purpose module. Its ÿsource code is 
gven here as an example of how to structure ÿa user written ÿmodule ÿfor 
transforming the geometry.

Example usage:
    #... 
    *WRAP_TWIST_Y

See also:
    *WRAP_AROUND_Y

---------------------------------------------------------------------------

< end of DANMESH documentation >
