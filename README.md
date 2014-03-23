repomania
=========================================

*NOTE* This is a placeholder for some future development. 

This package contains a stripped down version of an access system for a file repository of time-series gridded data. There is one basic read function, with a hardcoded data object (`catalog`) of files that are available. 

This is only for use with the relevant files being available locally, this can be set in `R` with 

`options(default.datadir = "/path/to/data")`

TO INSTALL
- git clone this repo
- install package raster and dependencies sp (also rgdal, but this will be changed in future to Suggests)
- in R generate the documentation with `library(roxygen2); roxygenize("repomania")`
- R CMD build repomania
- R CMD INSTALL repomania_[major.minor-patch].tar.gz
- ensure global option "default.datadir" to point to a sensible place

EXAMPLE

`options(default.datadir = "/path/to/gridded/data")`
`library(repomania)`
`x <- readice(catalog$date[c(1, 3, 5)])`

TODO

Ensure overwrite argument is captured and dealt with correctly for gri/grd case. 
Generalize the read function, needs to handle
 - different products, product options
 - single time per file
 - multi-time per file
 - multi-attributes per file / band
 - update raster#character cascade order to avoid rgdal load by `raster("nt_19781026_n07_v01_s.bin")`

Ensure \dots is always passed to `brick()` so we can use filename for easy out-of-memory handling
- need to make each data source native to raster (or similar) so that `stack()` can take a list of file names, or no-cost version, currently available for NSIDC and possibly OISST but not much else

Method for `extract#function,data.frame` has to work for no-time-dimension



