## Numeric menus used by Danplot

no. | Name | Position | called from | Notes
--|------|----------|-------------|-------------
1 | Title .   | top | Perm | should really be in the Window manager itself  
2 | Menu bar | top | Perm | File, Edit, Mesh, .. Help  
3 | Image    | main | perm | only here so we can detect a mouse click here
4 | Statusbar | Bot | Perm | shows #nodes, #elements, and last button press  
5 | Countour  | RHS | Perm | (quick way of zooming,  not really used ) No! This is the Contouring menu
6 | File     | L | Menu | Open, Export, Print, Configure
7 | Edit     | L | Menu | Select boxed, mirroring, scale/shift, nodes/elems, loads, mats, Misc.
8 | View     | L | Menu | Zoom, dispscale, loadstep#, image rotation, light rotation, bg colour, 2x2 pics 
9 | Contour  | L | *Value*, isosurface, shaded, rescale, #conts, line type, face type, *as menu*
10 | Mesh    | L | Menu | Faces+Edges, sub-edges, node#, #sub-facts, shrink 
11 | Animation | L | Menu | #frames, destination, deltas of eye/COA/light/zoom, Go, bounce, save/load frames
12 | Help    | casc. | Menu | *About* (=Danplot rainbow logo), 'sorry no help yet available'
13 | About   | L | Danplot Rainbow Logo, posted at startup, but covered by subsequent menus. 
14 | Config | popup | File | Print setup, SVGA modes, menu color, font size. Some of thee would be better via a config file?
15 | SVGA Modes | popup2 | Config | seems to be full of zeros, so cant change anything :sad:
16 | Colours  | L | menu | HotIron, Rainbow, matshade, interp colours, etc.
17 | Faces+Edges | casc. | Mesh | Two columns for nodes/edges, material, facet#, node#, chess, matshade, z-depth 
18 | Mirror | casc.| Edit | mirror in xyzXYZ, unmirror a 1/4, 
19 | Shft/Scale | casc. | Edit  | x->y->z, scale mesh, shift mesh, rotate-y, split elements
20 | Nodes/Elems | casc. | Edit | Join coincident nodes, del orpahn nodes, strip facets (auto though?), strip edges. 
21 | Loads/Disps | casc. | Edit | Add disps, summation of disps, diff loads, calc normals, calc volumes(?)
22 | Materials  | casc. | View | zap Materials, Hide mats, del invis mats, connectivity
23 | Misc | calc | Edit |  x->y->z only ?  *Doesn't popup - so maybe broken
24 | Import | casc. | File | PL, OFF, NFF, Tecploy, DXF, Rayshade formats
25 | Export | casc | File | PL, OFF, NFF, Tecplot, DXF, GRD, Rayshade formats
26 | Numbers | thin popup | various | A coloured vertical strip of 20 numbers
27 | Vectors | L | Menu | Disp vectors, regrid lines, flownets, Stress tensors. This should really be about menu line 9 above, but was added much later?
28 | Titles/Axes | L | Menu Title, subtutle, show axes, bounding box, window frame, conbtour legend  | | 
29 | numbers | popop | misc | A big popup of 256 boxes. Used for loking at current shading, picking colours, etc. This is teh only menu where the button posiitions are all created on first pass rather than statically.
30 | ContourVals | L | Contour | Table of everyything contourable, coords/disps/strain/stress/vonMise/FOS
99 | Background | L | n/a  | I think that this is siomply a template used when writing new code for a new LH menu.
