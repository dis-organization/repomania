repomania
=========================================
This is a placeholder for some future development. 


TO INSTALL
- git clone this repo
- install package raster and dependencies sp
- in R generate the documentation with library(roxygen2); roxygenize("repomania")
- R CMD build repomania
- R CMD INSTALL repomania_[major.minor-iter].tar.gz
- requires global option "default.datadir" to point to a sensible place

TODO

Generalize the read function, needs to handle
 - different products, product options
 - single time per file
 - multi-time per file
 - mulit-attributes per file / band

Ensure \dots is always passed to brick() so we can use filename for easy out-of-memory handling
- need to make each data source native to raster (or similar) so that stack() can take a list of file names, or no-cost version, currently available for NSIDC and OISST but not much else

Method for xtract#function,data.frame has to work for no-time-dimension



