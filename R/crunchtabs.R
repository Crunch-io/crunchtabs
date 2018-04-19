#' Functions for reports (toplines and banners) generation.
#'
#' crunchtabs provides tools for generating reports (Toplines and Banners)
#'
#' @docType package
#' @name crunchtabs-package
#' @aliases crunchtabs
NULL

#' @importFrom httr config add_headers
.onLoad <- function (lib, pkgname="crunchtabs") {
    setIfNotAlready(
        crunch.api="https://app.crunch.io/api/",
        httpcache.on=TRUE,
        crunch.namekey.dataset="alias",
        crunch.namekey.array="alias"
    )
    
    crunch::notifyIfNewVersion(package="crunchtabs",
        github="Crunch-io/crunchtabs",
        installed.version=as.character(packageVersion(package)))
    invisible()
}

setIfNotAlready <- function (...) {
    newopts <- list(...)
    oldopts <- options()
    oldopts <- oldopts[intersect(names(newopts), names(oldopts))]
    newopts <- modifyList(newopts, oldopts)
    do.call(options, newopts)
    invisible(oldopts)
}