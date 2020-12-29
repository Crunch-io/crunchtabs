#' Sort Aliases
#'
#' Given a \link{crosstabs} object, sort the named
#' variables alphabetically.
#'
#' @param ct A \link{crosstabs} object
#' @param vars A character vector of variable aliases that should be sorted. Or a single alias if pin_to_top or pin_to_bottom are specified.
#' @param pin_to_top A character vector of response values to pin to the top of the result presentation
#' @param pin_to_bottom A character vector of response values to pin to the bottom of the result presentation
#' @param fixed A character vector representing a set order of presentation
#' @param alpha Logical. Defaults to FALSE. Should data be sorted alphabetically?
#' @param descending Logical, defaults to NULL. If data are of type TextVariable or DateTimeVariable, default to ascending, if numeric defaults to descending. If FALSE, enforces ascending. If TRUE, enforces descending.
#' @export
sortAliases <- function(ct, vars = NULL, descending = NULL,
                        alpha = FALSE, fixed = NULL, pin_to_top = NULL,
                        pin_to_bottom = NULL) {
  if (!is.null(vars)) {
    if (!all(vars %in% names(ct$results))) {
      stop(
        "One or more specified vars are not in your crosstabs objects: ",
        paste0(setdiff(vars, names(ct$results)), collapse = ", ")
      )
    }

    nms <- intersect(vars, names(ct$results))
  } else {
    nms <- names(ct$results)
  }

  for (nm in nms) {
    ct$results[[nm]] <- sortResults(
      ct$results[[nm]],
      descending = descending,
      alpha = alpha,
      fixed = fixed,
      pin_to_top = pin_to_top,
      pin_to_bottom = pin_to_bottom
    )
  }
  return(ct)
}


#' Sort Results
#'
#' An internal function used to apply custom sorting individual questions
#' in a LateX topline report.
#'
#' @param var An internal \link{crosstabs} results object.
#' @param ... Further arguments passed from \link{sortAliases} to type
#' based \link{sortResults} functions
sortResults <- function(var, ...) {
  if (var$type == "DatetimeVariable") {
    # No sorting applied
    return(var)
  }
  if (var$type == "NumericVariable") {
    # No sorting applied
    return(var)
  }
  if (var$type == "multiple_response") {
    return(sortResults_outer(var, ...))
  }
  if (var$type == "categorical") {
    return(sortResults_outer(var, ...))
  }
  if (var$type == "TextVariable") {
    return(var)
  }
}

#' Sort a categorical or multiple_response result in a crosstabs
#' object.
#'
#' @rdname sortAliases
#' @param var An internal crosstabs elements (Usually ct$results[[x]])
#' @param pin_to_top A character vector of response values to pin to the top of the result presentation
#' @param pin_to_bottom A character vector of response values to pin to the bottom of the result presentation
#' @param fixed A character vector representing a set order of presentation
#' @param alpha Logical. Defaults to FALSE. Should data be sorted alphabetically?
#' @param descending Logical, defaults to NULL. If data are of type TextVariable or DateTimeVariable, default to ascending, if numeric defaults to descending. If FALSE, enforces ascending. If TRUE, enforces descending.
sortResults_outer <- function(var, descending, alpha, fixed, pin_to_top, pin_to_bottom) {

  # If Results, this is a topline report and we only have to take
  # action once. However, if this is a crosstab, we have to run
  # this function on a per banner basis.

  if ("Results" %in% names(var$crosstabs)) {
    nms <- "Results"
  } else {
    nms <- names(var$crosstabs)
  }

  for (nm in nms) {
    srt <- sortResults_inner(
      as.data.frame(var$crosstabs[[nm]]$`___total___`$proportions),
      descending, alpha, fixed, pin_to_top, pin_to_bottom
    )

    r <- srt$r
    ord <- srt$ord

    var$crosstabs[[nm]]$`___total___`$proportions <- r

    # Reorder base presentations to match
    bs <- var$crosstabs[[nm]]$`___total___`$base
    wbs <- var$crosstabs[[nm]]$`___total___`$weighted_base

    var$crosstabs[[nm]]$`___total___`$base <- bs[ord]
    var$crosstabs[[nm]]$`___total___`$weighted_base <- wbs[ord]
    cnts <- var$crosstabs[[nm]]$`___total___`$counts
    cnts <- cnts[ord]

    obj <- structure(
      cnts,
      .Dim = c(as.integer(length(rownames(r))), 1L),
      .Dimnames = list(rownames(r), "Total")
    )
    var$crosstabs[[nm]]$`___total___`$counts <- obj
    attributes(var$crosstabs[[nm]]$`___total___`$counts)$row.names <- rownames(r)
  }
  return(var)
}


#' sortResults Generic
#'
#' The difference between a topline and a crosstabe from the perspective of tabbook
#' is that a topline only has a crosstab called "Results" contained within it.
#' Otherwise, the crosstab object has an embedded summary object per each banner defined
#'
#' @rdname sortAliases
#' @param descending Logical, defaults to NULL. If data are of type TextVariable or DateTimeVariable, default to ascending, if numeric defaults to descending. If FALSE, enforces ascending. If TRUE, enforces descending.
#' @param alpha Logical. Defaults to FALSE. Should data be sorted alphabetically?
#' @param fixed A character vector representing a set order of presentation
#' @param pin_to_top A character vector of response values to pin to the top of the result presentation
#' @param pin_to_bottom A character vector of response values to pin to the bottom of the result presentation
#' @param r The results of a specific banner or Results
sortResults_inner <- function(r, descending, alpha, fixed, pin_to_top, pin_to_bottom) {
  if (alpha) {
    r$X <- NA # Trick for single col
    # Alpha default should be ascending
    ord <- order(rownames(r), decreasing = ifelse(
      !is.null(descending), descending, FALSE
    ))
    r <- r[ord, ]
    r$X <- NULL
  } else {
    r$X <- NA # Trick for single col
    # Numeric default should be descending
    ord <- order(r$Total, decreasing = ifelse(
      !is.null(descending), descending, TRUE
    ))
    r <- r[ord, ]
    r$X <- NULL
  }


  if (length(pin_to_top)) {
    stopifnot(any(pin_to_top %in% rownames(r)))
    pos <- 1:nrow(r)

    locs <- unname(
      sapply(pin_to_top, function(x) which(rownames(r) %in% x))
    )
    ord <- c(locs, setdiff(pos, locs))

    r$X <- NA # Trick for single col
    r <- r[ord, ]
    r$X <- NULL
  }

  if (length(pin_to_bottom)) {
    stopifnot(any(pin_to_bottom %in% rownames(r)))
    pos <- 1:nrow(r)

    locs <- unname(
      sapply(pin_to_bottom, function(x) which(rownames(r) %in% x))
    )
    ord <- c(setdiff(pos, locs), locs)

    r$X <- NA # Trick for single col
    r <- r[ord, ]
    r$X <- NULL
  }

  if (length(pin_to_top) & length(pin_to_bottom)) {
    stop("Ambiguous specification of pin_to_top and pin_to_bottom. Use fixed=c() param instead.")
  }

  if (length(fixed) > 0) {
    stopifnot(all(rownames(r) %in% fixed))
    stopifnot(all(fixed %in% rownames(r)))

    ord <- unname(
      sapply(fixed, function(x) which(rownames(r) %in% x))
    )
    r <- r[ord, ]
  }

  # Return the r and object
  list(
    r = r,
    ord = ord
  )
}
