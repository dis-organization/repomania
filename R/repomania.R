##' Repo tools, this is a test package to prototype an idea
##'
##' Tools in R, not yet functional
##' @author Michael D. Sumner \email{michael.sumner@@aad.gov.au}
##'
##' Maintainer: Michael D. Sumner \email{michael.sumner@@aad.gov.au}
##'
##' @name repomania
##' @docType package
##' @keywords package
NULL



##' Stable conversion to POSIXct from character and Date
##'
##' Conversion to POSIXct ensuring no local time zone applied. Currently supported is character, Date and
##' anything understood by \code{\link[base]{as.POSIXct}}.
##'
##' @param x input date-time stamp, character, Date or other supported type.
##' @param \dots ignored
##' @return the vector \code{x} converted (if necessary) to \code{POSIXct}
##' @export
timedateFrom <- function(x, ...) {
  as.POSIXct(x, tz = "GMT", ...)
}



##' Read data from sea ice data products.
##'
##' Sea ice data is read from files managed in \code{\link{catalog}}
##'
##' Currently available products are
##'
##' \describe{
##' \item{'nsidc'}{daily NSIDC concentration data for the Southern Hemisphere, processed by the SMMR/SSMI NASA Team}
##' }
##'
##' Dates are matched to file names by finding the nearest match in
##' time within a short duration. If \code{date} is greater than
##' length 1 then the sorted set of unique matches is returned.
##' @param date date or dates of data to read, see Details
##' @param time.resolution time resoution data to read, daily or monthly
##' @param product choice of sea ice product, see Details
##' @param xylim spatial extents to crop from source data, can be anything accepted by \code{\link[raster]{extent}}, see Details
##' @param setNA mask zero and values greater than 100 as NA
##' @param rescale rescale values from integer range?
##' @param debug ignore data request and simply report on what would be returned after processing arguments
##' @param returnfiles ignore options and just return the file names and dates
##' @param verbose print messages on progress etc.
##' @param ... arguments passed to \code{\link[raster]{brick}}
##' @export
##' @return \code{\link[raster]{raster}} object
##' @importFrom raster brick
##' @examples
##' x <- readice(catalog$date[c(10, 1, 2, 3)])
##' \dontrun{
##' ## try a different kind of file
##'  baseurl <- "ftp://sidads.colorado.edu/pub/DATASETS/"
##' f <- paste(baseurl, "nsidc0081_nrt_nasateam_seaice/north/nt_20130105_f17_nrt_n.bin", sep='')
##' if (!file.exists(basename(f))) download.file(f, basename(f), mode = "wb")
##' ice <- raster(basename(f))
##' library(maptools)
##' library(rgdal)
##' data(wrld_simpl)
##' cm <- spTransform(wrld_simpl, CRS(projection(ice)))
##' plot(ice)
##' plot(cm, add = TRUE)
##' }
readice <-
function (date, time.resolution = c("daily"), product  ="nsidc",  xylim = NULL,
          setNA = TRUE, rescale = TRUE, debug = FALSE,
    verbose = TRUE, returnfiles = FALSE, ...)
{
    ## deal with options
    time.resolution <- match.arg(time.resolution)
    product <- match.arg(product)
    files <- .loadfiles()
    if (returnfiles)
        return(files)
    if (missing(date))
        date <- min(files$date)
    ## normalize date inputs
    date <- timedateFrom(date)
    findex <- .processDates(date, files$date, time.resolution)
    ## this should be .processFiles . . .
    files <- files[findex, ]
    ## setup to crop if needed
     cropit <- FALSE
    if (!is.null(xylim)) {
        cropit <- TRUE
        cropext <- extent(xylim)
    }
    ## work through the file list, stored in generic list()
    ## (we won't get past the date-normalization above with less than 1 file)
    nfiles <- nrow(files)
    r <- vector("list", nfiles)
     for (ifile in seq_len(nfiles)) {
         r0 <- raster(files$fullname[ifile])
        if (cropit)
            r0 <- crop(r0, cropext)
        r[[ifile]] <- r0
    }
    ## build a stack, convert to brick, with arguments from the user for filename etc.
    ## this should probably always be a RasterBrick
    if (nfiles > 1)
        r <- brick(stack(r, quick = TRUE), ...)
    else r <- r[[1L]]
    ## need to explore how raster() elements apply these names
    names(r) <- basename(files$file)
    ## also perhaps stack to capture all the getZ elements . . .
    r <- setZ(r, files$date)
    r
}

.loadfiles <- function() {
    datadir <- getOption("default.datadir")
    if (is.null(datadir)) datadir <- "E:/repo"
    catalog$fullname <- file.path(datadir, catalog$file)
    catalog
}


##' This is the catalog of files, hardcoded for a simple proof of concept
##'
##' @name catalog
##' @docType data
##' @keywords data
NULL
