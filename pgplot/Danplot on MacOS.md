## Building and running Danplot on MacOS

see http://mingus.as.arizona.edu/~bjw/software/pgplot_fix.html

Some of the issues are:

  1. MacOS uses .dylib as the prefered extension for dynamic link libraries
      * Vanila PGPLOT wont build wiotha simple 'make' because of this.
  2. Still need to remeber to set PGPLOT_DIR so that PGPLOT can p[ick up the fonts in `GRFONT.DAT`
  3. One day I want to move away from PGPLOT since:
  
      * It is yet another library to have to build
      * Some doubts about how long it will be maintained (rememebr originaly for astrophysics university project)
      * It depends on X11 on MacOS (Is this still true?) so need X11 downloading and installing too
      * Danplot uses very few of the features of PGPlot. I only need basic 2d drawing routines, no need for all 
      the xyplots, pie charts et al. Copare perhaps to GKS (whatever happened to that? :-)
  4. The keyboard shortcut pressing does seem to work - eg rotating the FE model. Part of the issue too is that some dialogue is 
  in the GUI window and some in the associated console window DANPLOT was lauched from
  
The other issue I need to solve is a good way to make the winow scalable : I really do need to be able to go full screen on a large monitor,
or have multiple windows on the same screen. 

So

- [ ] Move to native X11
- [x] Stick with PGPlot for a while until I am confident I understand all the features I need to port
- [ ] Rewite in a language that is more modern, and so doesn't need features bolted on (legacy issues).
- [ ] Look for alternative graphics libraries for MacOS (but I still need to support native Linux too!)

<details>
   <summary>Danplot porting issues</summary>
   <p>This is where I descibed the issues found in porting Danplot ot other platforms</p>
</details>

## Some possible enhancement on a MacBook

  1. Use <kbd>⌘↑</kbd> as a 'shift key' eg. when rotating/zooming the object (or the camera, or the light source)
