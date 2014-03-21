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
readice <-
function (date, time.resolution = c("daily"), product  ="nsidc",  xylim = NULL,
          setNA = TRUE, rescale = TRUE, debug = FALSE,
    verbose = TRUE, returnfiles = FALSE, ...)
{

    time.resolution <- match.arg(time.resolution)
    product <- match.arg(product)
    files <- .loadfiles()
    if (returnfiles)
        return(files)
    if (missing(date))
        date <- min(files$date)
    date <- timedateFrom(date)
    findex <- .processDates(date, files$date, time.resolution)
    stersouth <- "+proj=stere +lat_0=-90 +lat_ts=-71 +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
    dims <- switch(product, nsidc = c(316L, 332L), ssmi = c(632L,
        664L))
    res <- switch(product, nsidc = c(25000, 25000), ssmi = c(12500,
        12500))
    rtemplate <- raster(GridTopology(c(-3937500, -3937500), res,
        dims))
    cropit <- FALSE
    if (!is.null(xylim)) {
        cropit <- TRUE
        cropext <- extent(xylim)
    }
    nfiles <- length(findex)
    r <- vector("list", length(findex))
    .readNSIDC <- function(fname) {
        con <- file(fname, open = "rb")
        trash <- readBin(con, "integer", size = 1, n = 300)
        dat <- readBin(con, "integer", size = 1, n = prod(dims),
            endian = "little", signed = FALSE)
        close(con)
        r100 <- dat > 250
        r0 <- dat < 1
        if (rescale) {
            dat <- dat/2.5
        }
        if (setNA) {
            dat[r100] <- NA
            dat[r0] <- NA
        }
        raster(t(matrix(dat, dims[1])), template = rtemplate)
    }
    for (ifile in seq_along(findex)) {
        r0 <- .readNSIDC(files$fullname[findex[ifile]])

        if (cropit)
            r0 <- crop(r0, cropext)
        r[[ifile]] <- r0
    }
    if (length(findex) > 1)
        r <- brick(stack(r), ...)
    else r <- r[[1L]]
    projection(r) <- stersouth
    names(r) <- files$file[findex]
    r <- setZ(r, files$date[findex])
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
