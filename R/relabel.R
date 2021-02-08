#' Relabel question items and options
#'
#' This function is a passthrough that allows you to relabel question items, options, question
#' descritions and notes before writing to latex or excel. The relabeling must occur in
#' questionnaire order. As it is received by crunch, so should it be set by your relabeling objects.
#'
#' There are two important warnings to consider:
#'
#' * Relabeling should always occur before applying any kind of sorting using the
#' \link{\code{sortAlias}} function to avoid situations where labels could be applied
#' inappropriately.
#' * If your results object is a tracking report or recontact it is strongly recommended that you
#' set your wave labels in the \link{\code{recontact_toplines}} or \link{\code{trackingReport}}.
#' However, if you must set it here options would be the categories and items would be the wave
#' names. We cannot account for aliases with only partial data (n-1 waves), you must set the
#' wave names in order.
#'
#' @examples
#' \dontrun{
#' library(crunchtabs)
#' login()
#'
#' ds <- loadDataset("Example dataset")
#' ct <- crosstabs(ds)
#' ct <- relabel(ct,
#'   list(
#'     alias = "q1",
#'     options = c("Lovely Cat", "Smelly Dog", "Annoying Bird"), # Cat, Dog, Bird
#'     description = "Which pet do you love the most?"), # What is your favorite pet?
#'     notes = "Relabeled by an undeniable cat-lover" # No note before
#'   list(alias = "petloc",
#'     options = c("Lovely Cat", "Smelly Dog", "Annoying Bird"), # Cat, Dog, Bird
#'     items = c("In my home", "At my work")) # Home, Work
#' )
#' writeLatex(ct, pdf = TRUE)
#' }
#'
#' @md
#' @param ct An object from \link{\code{crosstabs}}, \link{\code{recontact_report}},
#' or \link{\code{trackingReport}}
#' @param ... One or more relabel objects. See \link{\code{relabelApply}}
#' @export
relabel <- function(ct, ...) {
  message("Relabeling should always occur before sorting")
  results <- ct$results
  lab_list <- list(...)

  for(i in seq_along(lab_list)) {
    results <- relabelApply(results, labs = lab_list[[i]])
  }

  ct$results <- results
  ct
}


relabelApply <- function(results, labs) {
  obj <- getResultObj(results, labs$alias)
  obj <- relabelSet(obj, labs)
  results <- setResultObj(results, labs$alias, obj)
  results
}



#' relabel description
#'
#' Given a results object, adjust the question description. This will display as the question
#' text in crunchtabs.
#' @param obj A results object for a specific alias
#' @param labs A list of labels, typically passed to \link{\code{relabel}}
relabelDescription <- function(obj, labs) {
  if(length(labs$description) > 0) {
    obj$description <- labs$description
  }
  obj
}

#' relabel notes
#'
#' Given a results object, adjust the note. This will display as the text below
#' the question text in crunchtabs.
#' @param obj A results object for a specific alias
#' @param labs A list of labels, typically passed to \link{\code{relabel}}
relabelNotes <- function(obj, labs) {
  if(length(labs$notes) > 0) {
    obj$notes <- labs$notes
  }
  obj
}


#' Set relabels
#'
#' An S3 method for applying updated text values to a result object in crunchtabs
#' @param obj An inner object under the results list of a \link{\code{crosstabs}} object.
#' @param labs A list including at least one of the following: description, notes, items, or options
relabelSet <- function(obj, labs) {
  UseMethod("relabelSet", obj)
}


#' relabel ToplineVar
#'
#' Relabel a ToplineVar result object.
#' @param obj A results object for a specific alias
#' @param labs A list of labels, typically passed to \link{\code{relabel}}
relabelSet.ToplineVar <- function(obj, labs) {
  current_options <- dimnames(obj$crosstabs$Results$`___total___`$base)[[1]]
  if(length(labs$options) > 0) {
    if(length(labs$options) != length(current_options)) {
      stop("Length of options vector not equal for ", labs$alias)
    }

    obj$rownames <- labs$options
    dimnames(obj$crosstabs$Results$`___total___`$base)[[1]] <- labs$options
    dimnames(obj$crosstabs$Results$`___total___`$proportions)[[1]] <- labs$options
    dimnames(obj$crosstabs$Results$`___total___`$counts)[[1]] <- labs$options
  }


  obj <- relabelNotes(obj, labs)
  obj <- relabelDescription(obj, labs)
  obj
}

#' relabel ToplineCategoricalArray
#'
#' Relabel a ToplineCategoricalArray result object.
#' @param obj A results object for a specific alias
#' @param labs A list of labels, typically passed to \link{\code{relabel}}
relabelSet.ToplineCategoricalArray <- function(obj, labs) { # nolint

  if(length(labs$options) > 0) {
    obj <- relabelSet.ToplineVar(obj, labs)
  }

  if(length(labs$items) > 0) {
    current_items <- dimnames(obj$crosstabs$Results$`___total___`$proportions)[[2]]

    if(length(labs$items) != length(current_items))
      stop("Length of items vector not equal for ", labs$alias)

    if(any(grepl("Wave", current_items))) {
      warning(
        paste("We recommend against relabeling wave names as you may have a variable",
              "that is not available in all waves."
        )
      )
    }

    obj$subnames <- labs$items
    dimnames(obj$crosstabs$Results$`___total___`$base)[[2]] <- labs$items
    dimnames(obj$crosstabs$Results$`___total___`$proportions)[[2]] <- labs$items
    dimnames(obj$crosstabs$Results$`___total___`$counts)[[2]] <- labs$items
  }

  obj <- relabelNotes(obj, labs)
  obj <- relabelDescription(obj, labs)

  obj
}

#' Generic setter for result object
#'
#' Set a result object based on alias
#'
#' @param results A list of results
#' @param alias A character identifying the alias
#' @param obj A result object
setResultObj <- function(results, alias, obj) {
  stopifnot(alias %in% names(results))
  results[[alias]] <- obj
  results
}

#' Generic getter for result object
#'
#' Get a result object based on alias
#'
#' @param results A list of results
#' @param alias A character identifying the alias
#' @param obj A result object
getResultObj <- function(results, alias) {
  stopifnot(alias %in% names(results))
  results[[alias]]
}
