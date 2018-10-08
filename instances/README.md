TABLE OF CONTENTS

1. Benchmark instances (how they look like, details on the format, ...)
2. Other files

# BENCHMARK INSTANCES

## GENERATION

All the instances have been generated with the netgen R package
(see https://github.com/jakobbossek/netgen).


## FILE NAMING

The file naming underlies the following pattern:
vrp_{morphed}_n{#customers}[_cl{#clusters}][_mo{morphing.coefficient}][_dyn{percent.of.dynamic.customers}][_r{repetition}].csv

All the stuff wrapped with [ and ] is optional. Examples for filenames are
definitely helpful.


## Examples

* An instance with 100 customers, 5 clusters and 75% dynamic customers and the first
  Poisson process realization is denoted
  vrp_n100_cl5_dyn75_r1.csv

* Another morphed instance with 200 of which 50% are dynamic customers with a
  morphing coefficient of 0.25 is saved in the file
  vrp_morphed_n200_m25_dyn50_r2.csv

* An random instance with 50 nodes and 10% dynamic customers is stored in
  vrp_n50_dyn10_r1.csv

Note: Moreover we append another index right before the file extension to differentiate
between different Poisson-process realizations, e.g., vrp_n50_dyn10_r6.csv


## CSV-FORMAT

We differentiate between the 'minimal' and the 'full' format. The latter is
only interesting for instance reconstruction in the netgen R package.

### Minimal format

The instances are stored as comma-separated files. Each row in the file contains
the customer ID, the x- and y-coordinate in euclidean space and the request time
of the customer in exactly this order. Keep in mind, that an arrival time of 0.00
is assigned to 'static customers' as well as 'depots'.
By convention the first line is stated as the start depot in the context of the
VRP and the second line is the final depot.
The file contains no header information.

### Full format

The full is similar to the minimal format. The main differences are:
* the full format contains header information and
* the full format contains additional columns like (cluster-) membership,
  types, lower and upper bound of the bounding box.

### Publication format

Instances are stored in TSPlib file format.
