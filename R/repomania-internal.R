.dedupe <-
function (index, date, removeDupes = TRUE) 
{
    nondupes <- !duplicated(index)
    if (sum(nondupes) < length(index)) {
        if (removeDupes) 
            warning("duplicated dates will be dropped")
        else stop("duplicated dates not allowed")
        index <- index[nondupes]
        date <- date[nondupes]
    }
    list(index = index, date = date)
}
.indexDates <-
function (xdate, filedate) 
{
    windex <- integer(length(xdate))
    for (i in seq_along(xdate)) {
        windex[i] <- which.min(abs(xdate[i] - filedate))
    }
    windex
}
.matchFiles <-
function (querydate, refdate, index, daytest = 7) 
{
    deltatime <- abs(difftime(querydate, refdate, units = "days"))
    deltatest <- deltatime > daytest
    if (all(deltatest)) 
        stop(sprintf("no data file within %.1f days of %s", daytest, 
            format(querydate)))
    if (any(deltatest)) {
        warning(sprintf("%i input dates have no corresponding data file within %f days of available files", 
            sum(deltatest), daytest))
        index <- index[!deltatest]
    }
    index
}
.processDates <-
function (qdate, fdate, timeres) 
{
    qdate <- .valiDates(qdate, allOK = FALSE)
    qdate <- .sortDates(qdate, resortOK = TRUE)
    findex <- .indexDates(qdate, fdate)
    dedupedates <- .dedupe(findex, qdate, removeDupes = TRUE)
    findex <- dedupedates$index
    date <- dedupedates$date
    .matchFiles(date, fdate[findex], findex, daytest = switch(timeres, 
        daily = 1.5, weekly = 4, monthly = 15, weekly3 = 26))
}
.sortDates <-
function (x, resortOK = FALSE) 
{
    ord <- order(x)
    if (any(diff(ord) < 0)) {
        sortOK <- "dates out of order and will be sorted"
        if (resortOK) 
            warning(sortOK)
        else stop(sortOK)
        x <- x[ord]
    }
    x
}
.valiDates <-
function (x, allOK = TRUE) 
{
    xs <- timedateFrom(x)
    bad <- is.na(xs)
    if (all(bad)) 
        stop("no input dates are valid")
    if (any(bad)) {
        notOK <- "not all input dates are valid"
        if (allOK) 
            stop(notOK)
        else warning(notOK)
    }
    xs[!bad]
}
