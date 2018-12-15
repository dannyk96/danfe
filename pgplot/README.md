
### Notes of PGPLOT

Danplot neeeds to be built using a low-level graphics library. Danplot was originally developed using the graphics routines founnd in the Salfrod FTN95 compiler on DOS and then later Windows.
Around 1999 Danplot was ported to Unix / Linux and the graphics were converted to call routines from the PGPLOT library. The only graphics driver used was the /XWIN one. Postscript continued to be writen direct from the Danplot Fortran itself.


One reference I used was http://www.star.le.ac.uk/~cgp/pgplot_Fortran95_WinXP.html
This includes a download of an updated GIF driver

#### Future

It is hoped to port Danplot to a future graphics subsystem. This might be Qt or raw X11 tbc.
Also planned is to make more use of PGPLOT routines - eg to make the graphics window full screen and to allow animation on screen (as was done under Salford FTN95).

The howto to build PGPLOT is taken from:
http://pendientedemigracion.ucm.es/info/Astrof/software/howto/howto-pgplot.html
 
### Installation: method 1 - download prebuilt package
   1  sudo apt-get install pgplot5
   2  query where it was installed to :   dpkg -L pgplot5
   3  try an example
   4  Look at /usr/share/doc/pgplot5/README.Debian  for details about this implimentation. eg use of -fno-backslash and -lpng -lz -lm

~~~
cd /tmp
 gfortran /usr/share/doc/pgplot5/examples/pgdemo6.f -lpgplot
 ./a.out
~~~
Also could to type ? to get a list of built in drivers and try both /GIF and /PNG 

### Installation : method 2 - Compile from original source

   1 download the pgplot tarball from ftp://ftp.astro.caltech.edu/pub/pgplot/
I used the 5.2 version: ftp://ftp.astro.caltech.edu/pub/pgplot/pgplot5.2.tar.gz 
   2 Untar with tar xfz pgplot5.2.tar.gz
   3 rename the source directory: mv pgplot pgplot_src
   4 create a build directory - this eventualy holds the pgplot library and some demos.  mkdir pgplot ; cd pgplot
   5 copy the driver template: cp ../pgplot_dir/driver.list .
   6 edit the driver template - in particular uncomment the /XWINDOW line
   7 optional. Also uncomment the two PNG lines 
   8 so now you should have this:
~~~
$ grep -v ^! drivers.list
  NUDRIV 0 /NULL      Null device (no output)                           Std F77
 XWDRIV 1 /XWINDOW   Workstations running X Window System               C
~~~
   9 build the custom makefile

   ../pgplot_src/makemake /home/dan/pgplot_src linux g77_gcc_aout
   10 edit the makefile and change _g77_ to _gfortran_ on the FCOMP line
   11 build with __make__
   12 explot PGPLOT_DIR=~/pgplot
   13 explot PGPLOT_DEV=/XWIN
   13 test with ./pgdemo1

#### Optional add PNG drivers - main reason is so we can create saved animations from Danplot. It also allows Danfe to create output with embedded displacement plots etc as html or .md

so now we have:
~~~
$ grep -v ^! drivers.list
  NUDRIV 0 /NULL      Null device (no output)                           Std F77
 PNDRIV 1 /PNG       Portable Network Graphics file                    C
 PNDRIV 2 /TPNG      Portable Network Graphics file - transparent background C
 XWDRIV 1 /XWINDOW   Workstations running X Window System               C
~~~
Unfortunately after the makemake we need to edit the makefile to change where pgplot expect to find png.h et al.
~~~
t$ diff makefile ~/pgplot/makefile
872c874
< pndriv.o : /usr/include/png.h /usr/include/pngconf.h /usr/include/zlib.h /usr/include/x86_64-linux-gnu/zconf.h
---
> pndriv.o : ./png.h ./pngconf.h ./zlib.h ./zconf.h
~~~


ref. http://search.cpan.org/~csoe/PDL-2.4.3/Basic/Pod/FAQ.pod#Q:_6.22_PGPLOT_does_not_write_out_PNG_files.
