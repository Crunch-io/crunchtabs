
getResults <- function(x, proportions = FALSE, details = FALSE) {
    UseMethod("getResults", x)
}

#' @export
getResults.default <- function(x, proportions, details) {
    stop(paste("getResults doesn't support objects of class",
        class(x)))
}

#' @export
getResults.ToplineCategoricalGeneral <- function(x, proportions = FALSE, details = FALSE) {
    if (proportions) {
        if (details) {
            return(x$proportions_details)
        }
        return(x$proportions)
    } else {
        if (details) {
            return(x$counts_details)
        }
        return(x$counts)
    }
}

#' @export
getResults.ToplineMultipleResponse <- function(x, proportions = FALSE, details = FALSE) {
    if (proportions) {
        if (details) {
            return(x$proportions_details)
        }
        return(x$proportions)
    } else {
        if (details) {
            return(x$counts_details)
        }
        return(x$counts)
    }
}

#' @export
getResults.ToplineNumeric <- function(x, proportions = FALSE, details = FALSE) {
    x$summary
}

#' @export
getResults.CrossTabBannerVar <- function(x, proportions = FALSE, details = FALSE) {
    if (proportions) {
        return(x$proportions)
    } else {
        return(x$counts)
    }
}


setResults <- function(x, value, proportions = FALSE, details = FALSE) {
    UseMethod("setResults", x)
}

#' @export
setResults.default <- function(x, value, proportions = FALSE, details = FALSE) {
    stop(paste("setResults doesn't support objects of class",
        class(x)))
}

#' @export
setResults.ToplineCategoricalGeneral <- function(x, value, proportions = FALSE, details = FALSE) {
    if (proportions) {
        if (details) {
            x$proportions_details <- value
            return(x)
        }
        x$proportions <- value
        return(x)
    } else {
        if (details) {
            x$counts_details <- value
            return(x)
        }
        x$counts <- value
        return(x)
    }
}

#' @export
setResults.ToplineMultipleResponse <- function(x, value, proportions = FALSE, details = FALSE) {
    if (proportions) {
        if (details) {
            x$proportions_details <- value
            return(x)
        }
        x$proportions <- value
        return(x)
    } else {
        if (details) {
            x$counts_details <- value
            return(x)
        }
        x$counts <- value
        return(x)
    }
}

#' @export
setResults.ToplineNumeric <- function(x, value, proportions = FALSE, details = FALSE) {
    x$summary <- value
    return(x)
}



getNames <- function(x) {
    UseMethod("getNames", x)
}

#' @export
getNames.default <- function(x) {
    stop(paste("getNames doesn't support objects of class", class(x)))
}

#' @export
getNames.ToplineCategoricalGeneral <- function(x) {
    names(getResults(x))
}

#' @export
getNames.ToplineCategoricalArray <- function(x) {
    dimnames(getResults(x))
}

#' @export
getNames.ToplineNumeric <- function(x) {
    names(getResults(x))
}

#' @export
getNames.BannerVar <- function(x) {
    x$categories[!is.na(x$categories)]
}


# print.ToplineBase <- function(x) { return(paste(getType(x), 'variable with
# alias', getAlias(x), '\nName:', getName(x), '\nDescription:',
# getDescription(x))) }

# print.ToplineCategoricalGeneral <- function(x, proportions) {
# cat(paste(print.ToplineBase(x), '\nCounts:\n')) print(getResults(x)) }

# print.ToplineNumeric <- function(x, proportions) {
# cat(paste(print.ToplineBase(x), '\nSummary:\n')) print(getResults(x)) }


#' @export
print.Toplines <- function(x, ...) {
    cat(paste("Data summary for Toplines report.\n",
        "Title:", getName(x), "\n",
        if (is.null(x$metadata$weight)) "Unweighted.\n" else paste0("Weighted based on: the '", x$metadata$weight, "' variable.\n"),
        "Contains data for the following variables:\n",
        paste(names(x$results), collapse = ", ")))
}


#' @export
print.Crosstabs <- function(x, ...) {
    cat(paste("Data summary for Crosstabs report.\n",
        "Title:", getName(x), "\n",
        if (is.null(x$metadata$weight)) "Unweighted.\n" else paste0("Weighted based on: the '", x$metadata$weight, "' variable.\n"),
        "Contains data for the following variables:\n",
        paste(names(x$results), collapse = ", ")))
}

getType <- function(x) UseMethod("getType", x)

#' @export
getType.ToplineBase <- function(x) {
    x$type
}

#' @export
getType.BannerVar <- function(x) {
    x$type
}

# getType.CrunchCube <- function(out_crtabs) {
# out_crtabs$result$dimensions[[1]]$type$class }

getAlias <- function(x) UseMethod("getAlias", x)

#' @export
getAlias.default <- function(x) {
    stop(paste("getAlias doesn't support objects of class", class(x)))
}

#' @export
getAlias.ToplineBase <- function(x) {
    x$alias
}

#' @export
getAlias.BannerVar <- function(x) {
    x$alias
}

#' @export
getAlias.CrossTabVar <- function(x) {
    x$alias
}

#' @export
getAlias.CrunchCube <- function(x) {
    x@.Data[[1]]$dimensions[[1]]$references$alias
}

getName <- function(x) UseMethod("getName", x)

#' @export
getName.default <- function(x) {
    stop(paste("getName doesn't support objects of class", class(x)))
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
    x@.Data[[1]]$dimensions[[1]]$references$name
}

#' @export
getName.CrunchTabs <- function(x) {
    x$metadata$title
}

getDescription <- function(x) UseMethod("getDescription", x)

#' @export
getDescription.default <- function(x) {
    stop(paste("getDescription doesn't support objects of class", class(x)))
}

#' @export
getDescription.ToplineBase <- function(x) {
    x$description
}

#' @export
getDescription.BannerVar <- function(x) {
    x$description
}

#' @export
getDescription.CrossTabVar <- function(x) {
    x$description
}

#' @export
getDescription.CrunchCube <- function(x) {
    x@.Data[[1]]$dimensions[[1]]$references$description
}

getNotes <- function(x) UseMethod("getNotes", x)

#' @export
getNotes.default <- function(x) {
    stop(paste("getNotes doesn't support objects of class", class(x)))
}

#' @export
getNotes.ToplineBase <- function(x) {
    x$notes
}

#' @export
getNotes.CrossTabVar <- function(x) {
    if (!is.null(x$notes)) {
        x$notes
    } else {
        ""
    }
}

#' @export
getNotes.CrunchCube <- function(x) {
    x@.Data[[1]]$dimensions[[1]]$references$notes
}

getTotal <- function(x) UseMethod("getTotal", x)

#' @export
getTotal.default <- function(x) {
    stop(paste("getTotal doesn't support objects of class", class(x)))
}

#' @export
getTotal.CrunchCube <- function(out_crtabs) {
    out_crtabs@.Data[[3]]$n
}

#' @export
getTotal.ToplineBase <- function(x) {
    x$total
}

getMissing <- function(x) UseMethod("getMissing", x)

#' @export
getMissing.default <- function(x) {
    stop(paste("getMissing doesn't support objects of class", class(x)))
}

#' @export
getMissing.CrunchCube <- function(out_crtabs) {
    out_crtabs@.Data[[3]]$missing
}

#' @export
getMissing.ToplineBase <- function(x) {
    x$missing
}

getSubNames <- function(x) {
    sapply(x@.Data[[1]]$dimensions[[1]]$references$subreferences, function(xi) xi$name)
}

getSubAliases <- function(x) {
    sapply(x@.Data[[1]]$dimensions[[1]]$references$subreferences, function(xi) xi$alias)
}
