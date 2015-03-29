
DANFRONT=danfront
DANFRONTG=../../danfrontg

# there are 4 examples here:
#  ex1:  a human femur cros-section
#  ex2: An embankment cross-section
#  ex3: A  mesh with 3 connected regions of differnt materials
#  ex4: How a logo or other bitmap can be used to provide regions of higher mesh density
default: ex1_femur.dp ex2_slope.dp
all: ex1_femur ex2_slope ex3_3regions ex4_logo

ex1_femur.dp: ex1_femur.d
	$(DANFRONT) $<
ex2_slope.dp: ex2_slope.d
	$(DANFRONTG) $<
ex3_rivermersey.dp: ex3_rivermersey.d
	$(DANFRONTG) $<

ex3_3_regions.dp: ex3_3regions.d
	$(DANFRONT) $<
ex4_logo.dp: ex4_logo.d
	$(DANFRONT) $<



