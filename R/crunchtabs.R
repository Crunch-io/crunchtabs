#' Functions for reports (toplines and banners) generation.
#'
#' crunchtabs provides tools for generating reports (Toplines and Banners)
#'
#' @docType package
#' @name crunchtabs-package
#' @aliases crunchtabs
NULL

#' @importFrom httr set_config
.onAttach <- function (lib, pkgname="crunchtabs") {
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
