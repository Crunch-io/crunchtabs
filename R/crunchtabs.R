#' Functions for reports (toplines and banners) generation.
#'
#' crunchtabs provides tools for generating reports (Toplines and Banners)
#'
#' @docType package
#' @name crunchtabs-package
#' @aliases crunchtabs
#' @import crunch
NULL

#' @importFrom crunch notifyIfNewVersion
.onLoad <- function(lib, pkgname = "crunchtabs") {
  crunch::notifyIfNewVersion("crunchtabs", github = "Crunch-io/crunchtabs")
  # invisible() does nothing
}
