

#' @export
print.Toplines <- function(x, ...) {
    cat(paste("Data summary for Toplines report.\n",
        "Title:", getName(x), "\n",
        if (is.null(x$metadata$weight)) "Unweighted.\n" else paste0("Weighted based on: the '", x$metadata$weight, "' variable.\n"),
        "Contains data for the following variables:\n",
        collapse_items(names(x$results))))
}


#' @export
print.Crosstabs <- function(x, ...) {
    cat(paste("Data summary for Crosstabs report.\n",
        "Title:", getName(x), "\n",
        if (is.null(x$metadata$weight)) "Unweighted.\n" else paste0("Weighted based on: the '", x$metadata$weight, "' variable.\n"),
        "Contains data for the following variables:\n",
        collapse_items(names(x$results))))
}

getName <- function(x) UseMethod("getName", x)

#' @export
getName.default <- function(x) {
    wrong_class_error(x, c("ToplineBase", "BannerVar",
        "CrossTabVar", "CrunchCube"), "getName")
}

#' @export
getName.ToplineBase <- function(x) {
    x$name
}

#' @export
getName.BannerVar <- function(x) {
    x$name
}

#' @export
getName.CrossTabVar <- function(x) {
    x$name
}

#' @export
getName.CrunchCube <- function(x) {
    names(variables(x))[1]
    # x@.Data[[1]]$dimensions[[1]]$references$name
}

#' @export
getName.CrunchTabs <- function(x) {
    x$metadata$title
}

getSubNames <- function(x) {
    sapply(x@.Data[[1]]$dimensions[[1]]$references$subreferences, function(xi) xi$name)
}

getSubAliases <- function(x) {
    sapply(x@.Data[[1]]$dimensions[[1]]$references$subreferences, function(xi) xi$alias)
}


