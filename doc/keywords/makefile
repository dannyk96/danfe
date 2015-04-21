#
#   This extracts the creates the HTML versions of KEYWORD man pages
#   for Danfe et al.
#
#   Dan Kidger   daniel.kidger@quadrics.com


# next is of course platform specific
FC=gfortran 
FFLAGS=-g
.phony: clean veryclean manpages default

default: html/index.html manpages

html/index.html: publish_html keywords.txt
	./publish_html keywords.txt

manpages: keywords.txt
	nroff -man keywords.txt > keywords.man

publish_html: publish_html.f90
	$(FC) $(FFLAGS) -o $@ $^

# Note that we also may want to create unix man pages also
# to do this we simply cut the MASTER_FILE into pieces at each HR
# (and sed teh next line to get generate a filename) 
#  put each in the man/man3 directory and make sure MANPATH is updated
manpages: man

man:
	mkdir -p man/man1 man/man3

clean:
	rm publish_html keywords/*.html 
