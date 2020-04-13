#' Topline Title
#'
#' print.Toplines returns the title...
#'
#' @param x Variable to get name from.
#' @param ... Further arguments. Unused.
#' @export
print.Toplines <- function(x, ...) {
    cat(paste("Data summary for Toplines report.\n",
        "Title:", getName(x), "\n",
        if (is.null(x$metadata$weight)) "Unweighted.\n" else paste0("Weighted based on: the '", x$metadata$weight, "' variable.\n"),
        "Contains data for the following variables:\n",
        collapse_items(names(x$results))))
}


#' Crosstabs Title
#'
#' print.Crosstabs returns the title...
#'
#' @param x Variable to get name from.
#' @param ... Further arguments. Unused.
#' @export
print.Crosstabs <- function(x, ...) {
    cat(paste("Data summary for Crosstabs report.\n",
        "Title:", getName(x), "\n",
        if (is.null(x$metadata$weight)) "Unweighted.\n" else paste0("Weighted based on: the '", x$metadata$weight, "' variable.\n"),
        "Contains data for the following variables:\n",
        collapse_items(names(x$results))))
}

#' S3 Method for getName
#'
#' getName returns....
#'
#' @param x Object to use method on.
#' @export
getName <- function(x) UseMethod("getName", x)

#' @rdname getName
#' @param x An object of class ToplineBase, BannerVar, CrossTabVar, ord CrunchCube
#' @export
getName.default <- function(x) {
    wrong_class_error(x, c("ToplineBase", "BannerVar",
        "CrossTabVar", "CrunchCube"), "getName")
    # TODO: Is this missing CrunchTabs? There's a method for it below
}

#' @rdname getName
#' @param x An object of class ToplineBase
#' @export
getName.ToplineBase <- function(x) {
    x$name
}


#' @rdname getName
#' @param x An object of class BannerVar
#' @export
getName.BannerVar <- function(x) {
    x$name
}

#' @rdname getName
#' @param x An object of class CrossTabVar
#' @export
getName.CrossTabVar <- function(x) {
    x$name
}

#' @rdname getName
#' @param x An object of class CrunchCube
#' @export
getName.CrunchCube <- function(x) {
    names(variables(x))[1]
}

#' @rdname getName
#' @param x An object of class CrunchTabs
#' @export
getName.CrunchTabs <- function(x) {
    x$metadata$title
}

#' Extract names
#'
#' Extract names from crunch cube
#'
#' @param x A CrunchCube object
getSubNames <- function(x) {
    sapply(x@.Data[[1]]$dimensions[[1]]$references$subreferences, function(xi) xi$name)
}

#' Extract aliases
#'
#' Extract aliases from crunch cube
#' @param x A CrunchCube array
getSubAliases <- function(x) {
    sapply(x@.Data[[1]]$dimensions[[1]]$references$subreferences, function(xi) xi$alias)
}
