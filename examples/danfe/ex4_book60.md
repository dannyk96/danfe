 Running is serial on one processor
  the system clock has        1000  ticks/s
    +----------------------------------------------------------------------------+
    |               D A N F E  Finite Element Analysis Program                |
    |                            Version 1.3.00                               |
    |          . . .                                                          |
    |           Author : Dr. Dan Kidger, (ex. University of Manchester)       |
    |         daniel.kidger@gmail.com  http://github.com/dannyk96/danfe       |
    +----------------------------------------------------------------------------+

    - Job started: 10:59:11 22/04/15        2000000  Nodes,       80000  Elements, IKV=    82000000
 #args=           2
 Processing 'ex4_book60.d'
   - Data file name=ex4_book60.d                                                                                                                    

####  1 [JOB_TITLES]


####  2 [CONTROL]

####  3 [TWO_DIMENSIONAL]

####  4 [SLOPE_MESH]
   - NN=    121 NEL=     32 NOD=      8

####  5 [WRITE_DANPLOT_KEY]
   -     121 nodes written
   -      32 elements written

####  6 [NULL_ARRAYS]
   - Storage at Gauss points =  8 *     128 GP's  =     0.004 Mb
   - Storage at Nodal points = 12 *     121 Nodes =     0.011 Mb

####  7 [MAT_PROPS]
     1    model=2   E=1.e5  v=0.3  C=100.
     2    model=2   E=1.e5  v=0.3  C=100.    sprint=1

####  8 [BC_BY_COORD]
<>    16 nodes found of BC type 00
<>     8 nodes found of BC type 00
<>     9 nodes found of BC type 01

####  9 [FORM_KM]
    - Factorising the Stiffness Matrix...

#### 10 [NODAL_LOADS]

#### 11 [LOAD_STEPS]

#### 12 [ANALYSE]
    Table #   1                                           5 loaded freedoms
    +-------+------+-------------+-------------+--------+--------+-----+--------+
    | Step# | #Its | Tot.Force   |  Mean disp. | %Elast |%Mob-SS |  ngp|  Time  |
    +-------+------+-------------+-------------+--------+--------+-----+--------+
    |     1      2 |    400.0       0.5692E-02 | 100.00    84.35      0|10:59:11|
    |     2     11 |    600.0       0.9689E-02 |  71.22   100.20     12|10:59:11|
    |     3     20 |    700.0       0.1364E-01 |  36.01   100.17     18|10:59:11|
    |     4     33 |    800.0       0.1948E-01 |  24.39   100.23     30|10:59:11|
    |     5     45 |    900.0       0.2790E-01 |  16.89   100.28     46|10:59:11|
    |     6     65 |    960.0       0.3552E-01 |  11.21   100.29     55|10:59:11|
    |     7     81 |    1000.       0.4258E-01 |   8.06   100.25     64|10:59:11|
    |     8     99 |    1020.       0.4725E-01 |   6.09   100.17     70|10:59:12|
    |     9    159 |    1030.       0.5056E-01 |   4.31   100.08     74|10:59:12|
    |    10    696 |    1040.       0.6916E-01 |   0.76   100.52     77|10:59:13|
    +-------+------+-------------+-------------+--------+--------+-----+--------+
Total iterations=  1211 plastic and     0 PCG, mean=     0.0  ie. 0.00%N

#### 13 [EOF]
   - Total Run Time = 2.12s =         2.12s
   - DANFE analysis completed
 end of DANFE (spawned)
