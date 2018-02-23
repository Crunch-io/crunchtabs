
getResults <- function(x, proportions = FALSE, details = FALSE) {
    UseMethod("getResults", x)
}

#' @export
getResults.default <- function(x, proportions, details) {
    wrong_class_error(x, c("ToplineCategoricalGeneral", "ToplineMultipleResponse",
        "ToplineNumeric", "CrossTabBannerVar"), "getResults")
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
    wrong_class_error(x, c("ToplineCategoricalGeneral", "ToplineMultipleResponse",
        "ToplineNumeric"), "setResults")
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
    wrong_class_error(x, c("ToplineCategoricalGeneral", "ToplineCategoricalArray",
        "ToplineNumeric", "BannerVar"), "getNames")
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
    wrong_class_error(x, c("ToplineBase", "BannerVar",
        "CrossTabVar", "CrunchCube"), "getAlias")
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
    x@.Data[[1]]$dimensions[[1]]$references$name
}

#' @export
getName.CrunchTabs <- function(x) {
    x$metadata$title
}

getDescription <- function(x) UseMethod("getDescription", x)

#' @export
getDescription.default <- function(x) {
    wrong_class_error(x, c("ToplineBase", "BannerVar",
        "CrossTabVar", "CrunchCube"), "getDescription")
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
    wrong_class_error(x, c("ToplineBase", "CrossTabVar", "CrunchCube"), "getNotes")
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
    wrong_class_error(x, c("ToplineBase", "CrunchCube"), "getTotal")
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
    wrong_class_error(x, c("ToplineBase", "CrunchCube"), "getMissing")
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

collapse_items <- function(x){
    paste(x, collapse = ", ")
}


