
#
#  This makefile can generate a variety of different size 3d meshes
#  and so can be used to test the scaling of running danfe on a given platform
#  This is equally useful on the RAspberry Pi as it is on a large parallel supercomputer
#
# Usages:
#    make      : builds just one exampel case (10x10x10 cube mesg of 14noide brick elements
#    make 20nb : make all sizes of the 20node brick mesh
#    make 10   : make te cases for the 10x10x10 sizes mesh: currently 8nb,14nb,20nb
#

# TODO
#  so add option -o to danfe to specify explicitly the outputfile name
#  we use this to generate the '.tab' and other files
#  we also use this as 'sticky' and overrides any .d files we parse.

DANFE=../../../danfe
DANFE=danfe

#
#  Implicit rule to convert any .d file into an output file

%.out %.md: %.d
	echo `date`; $(DANFE) -b -v $<   > $*.md

.PHONY: all default 8nb 14nb 20nb

default: cube2_8nb.out

#default: cube10_14nb.out

all: 8nb 14nb 20nb

14nb: cube10_14nb.out cube20_14nb.out

cube2_8nb.d: cube_template.D
	cat cube_template.D|sed 's/_ELTYPE_/8/g' |sed s/_NXE_/2/g > $@
cube10_8nb.d:  cube_template.D
	cat cube_template.D|sed 's/_ELTYPE_/8/g' |sed s/_NXE_/10/g > $@


cube10_14nb.d: cube_template.D
	cat cube_template.D|sed 's/_ELTYPE_/14/g' |sed s/_NXE_/10/g > $@
cube20_14nb.d: cube_template.D
	cat cube_template.D|sed 's/_ELTYPE_/14/g' |sed s/_NXE_/20/g > $@
cube30_14nb.d: cube_template.D
	cat cube_template.D|sed 's/_ELTYPE_/14/g' |sed s/_NXE_/30/g > $@

cube10_20nb.d: cube_template.D
	cat cube_template.D|sed 's/_ELTYPE_/20/g' |sed s/_NXE_/10/g > $@
cube20_20nb.d: cube_template.D
	cat cube_template.D|sed 's/_ELTYPE_/20/g' |sed s/_NXE_/20/g > $@
cube30_20nb.d: cube_template.D
	cat cube_template.D|sed 's/_ELTYPE_/20/g' |sed s/_NXE_/30/g > $@


#cube4.out:
#	cpp -D_NXE_=4 -D_ELTYPE_=14 cube_gen.D > cube4_14nb.d;$(DANFE) cube4.d_14nb; rm cube04.d
#
#cube5.out:
#	cpp -D_NXE_=5 cube_gen.D > foo; $(DANFE) foo -o $@

datafiles:
	-@for eltype in 8 14 20 ; do \
 	    for size in 10 15 20 ; do \
               FILE="cube$$size.$$eltype.nb.d"; \
	       echo making $$FILE ;\
	       cpp -D_NXE_=$$size -D_ELTYPE_=$$eltype cube_gen.D 2>/dev/null > $$FILE;\
		nice -5 time $(DANFE) -b -s $$FILE ;\
	    done \
	done
	
#
#  Just delete the (large) results files
#
clean:
	rm -f *.out  
#
# Also delete the generated datafiles
#
veryclean: clean
	rm  *.d 
