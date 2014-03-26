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


.onAttach <- function(libname, pkgname) {
    options(repocatalog = list())
}


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

.removeTrailingSlashes <- function(x) {
    gsub("\\\\$", "", gsub("/$", "", x))
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
readice <- function (date, time.resolution = c("daily"), product  ="nsidc",  xylim = NULL,
          setNA = TRUE, rescale = TRUE, debug = FALSE,
    verbose = TRUE, returnfiles = FALSE, ...) {
    ## deal with options
    time.resolution <- match.arg(time.resolution)
    product <- match.arg(product)
    ##files <- .loadfiles()
    files <- .icefiles()
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

    ## first test for "filename.grd" and build the header/binary version instead
    arglist <- list(...)
    grdfile <- arglist[["filename"]]
    if (!is.null(grdfile) && grepl("grd$", grdfile)) {
  ##      overwrite <- l[["overwrite"]]
        grifile <- gsub("grd$", "gri", grdfile)
        rawcon <- file(grifile, open = "wb")
        on.exit({
            ## see R.utils::gunzip.default for outComplete test
            if (!is.null(rawcon)) close(rawcon)
        })
        ## loop files and write to raw binary connection
        for (i in seq_len(nfiles)) {
            r0 <- raster(files$fullname[i])
            if (cropit) r0 <- crop(r0, cropext)
            writeBin(as.vector(values(r0)), rawcon)
        }

        r <- build_raster_header(grifile, reference_raster = r0,
                                 out_nlayers = nfiles)
    } else { ## else do the standard brick(stack(thing))
        for (ifile in seq_len(nfiles)) {
            r0 <- raster(files$fullname[ifile])
         if (cropit)
             r0 <- crop(r0, cropext)
            r[[ifile]] <- r0
        }

        ## build a stack, convert to brick, with arguments from the user for filename etc.
        ## this should probably always be a RasterBrick
        ##  if (nfiles > 1)
        r <- brick(stack(r, quick = TRUE), ...)
        ##    else r <- r[[1L]]
    }
     ## need to explore how raster() elements apply these names
    names(r) <- basename(files$file)
    ## also perhaps stack to capture all the getZ elements . . .
    r <- setZ(r, files$date)
    r
}

.icefiles <- function(myname = "nsidc_nasteam_daily", configfile) {

    if (missing(configfile)) configfile <- system.file("extdata", "raad_repo_config.json", package= "repomania")
    rcatalog <- getOption("repocatalog")
    files <- rcatalog[[myname]]

    ## can we use "myname" to define multiple collections in the config?
    mydatasets <- c("SMMR-SSM/I Nasateam daily sea ice concentration",
                    "SMMR SSM/I Nasateam near-real-time sea ice concentration")

    if (is.null(files)) {
        cf <- repo_config(configfile)
        datadir <- .removeTrailingSlashes(cf$global$local_file_root)
	lfiles  <- vector("list", length(mydatasets))
        for (i in seq_along(mydatasets)) {
            ## please don't leave the trailing slash in the config
            localdir <- .removeTrailingSlashes(cf$datasets[match(mydatasets[i], cf$datasets$name), "local_directory"])
            lfiles[[i]] <- list.files(file.path(datadir, localdir), pattern = "s.bin$", recursive = TRUE, full.names = TRUE)
        }
        lfiles <- unlist(lfiles)
        files <- data.frame(file = file.path(localdir, basename(lfiles)),
                            date = timedateFrom(strptime(basename(lfiles), "nt_%Y%m%d")),
                            fullname = lfiles,
                            stringsAsFactors = FALSE)

        files <- files[order(files$date), ]
	rcatalog[[myname]] <- files
	options(repocatalog = rcatalog)
    }
    files
}



## leave this for now
.loadfiles <- function() {


    datadir <- getOption("default.datadir")
    if (is.null(datadir)) datadir <- "E:/repo"
    catalog$fullname <- file.path(datadir, catalog$file)
    catalog
}

## private copy of spatial.tools::build_raster_header
build_raster_header <-
function (x_filename, reference_raster, out_nlayers, dataType = "FLT8S",
    format = "raster", bandorder = "BSQ", setMinMax = FALSE,
    verbose = FALSE)
{
    if (missing(out_nlayers)) {
        out_nlayers = nlayers(reference_raster)
    }
    if (out_nlayers == 1) {
        outraster <- raster(reference_raster)
    }
    else {
        outraster <- brick(raster(reference_raster), nl = out_nlayers)
    }
    outraster@file@name <- x_filename
    outraster@file@datanotation <- dataType
    outraster@file@bandorder <- bandorder
    if (setMinMax)
        outraster@data@haveminmax <- TRUE
    else outraster@data@haveminmax <- FALSE
    try(outhdr <- hdr(outraster, format = format), silent = TRUE)
    if (out_nlayers == 1) {
        outraster <- raster(paste(remove_file_extension(x_filename,
            ".gri"), ".grd", sep = ""))
    }
    else {
        outraster <- brick(paste(remove_file_extension(x_filename,
            ".gri"), ".grd", sep = ""))
    }
    if (setMinMax)
        outraster <- setMinMax(outraster)
    else outraster@data@haveminmax <- FALSE
    return(outraster)
}

remove_file_extension <- function (filename, extension_delimiter = ".")
{
    split_filename = unlist(strsplit(filename, extension_delimiter,
        fixed = TRUE))
    split_filename_length = length(split_filename)
    if (split_filename_length == 1) {
        return(split_filename[1])
    }
    else {
        return(paste(as.character(split_filename)[1:(split_filename_length -
            1)], collapse = extension_delimiter))
    }
}




##' This is the catalog of files, hardcoded for a simple proof of concept
##'
##' @name catalog
##' @docType data
##' @keywords data
NULL
