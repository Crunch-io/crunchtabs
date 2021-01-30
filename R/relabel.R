#' Relabel question items and options
#'
#' This function is a passthrough that allows you to relabel question items and options for
#' presentation before writing to latex or excel. The relabeling must occur in questionnaire order.
#' As it is received by crunch, so should it be set by your relabeling objects.
#'
#' There are two important warnings to consider:
#'
#' * Relabeling should always occur before applying any kind of sorting using the
#' \code{\link{sortAlias}} function to avoid situations where labels could be applied
#' inappropriately.
#' * If your results object is a tracking report or recontact it is strongly recommended that you
#' set your wave labels in the \code{\link{recontact_toplines}} or \code{\link{tracking_report}}.
#' However, if you must set it here options would be the categories and items would be the wave
#' names. We cannot account for aliases with only partial data (n-1 waves), you must set the
#' wave names in order.
#'
#' #' @examples
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
#' @param ct An object from \code{\link{crosstabs}}, \code{\link{recontact_report}},
#' or \code{\link{tracking_report}}
#' @param ... One or more relabel objects. See \code{\link{relabelApply}}
relabel <- function(ct, ...) {

  results <- ct$results
  lab_list <- list(...)

  for(labs in seq_along(lab_list)) {
    results <- relabelApply(results, labs)
  }

  ct$results <- results
}


relabelApply <- function(results, labs) {
  obj <- getResultObj(results, labs$alias)
  obj <- relabelSet(obj, labs)
  results <- setResultObj(results, alias, obj)
  results
}

relabelSet <- function(obj, labs) {
  UseMethod("relabelSet", obj)
}


#' relabelSet for ToplineCategoricalArray
relabelSet.ToplineVar <- function(obj, labs) {
  stopifnot(length(labs$options) > 0)
  stopifnot(length(labs$options) == obj$rownames)

  obj$rownames <- labs$options
  dimnames(obj$crosstabs$Results$`___total___`$base)[[1]] <- labs$options
  dimnames(obj$crosstabs$Results$`___total___`$proportions)[[1]] <- labs$options
  dimnames(obj$crosstabs$Results$`___total___`$counts)[[1]] <- labs$options

  obj <- relabelNotes(obj, labs)
  obj <- relabelDescription(obj, labs)
  obj
}

relabelDescription <- function(obj, labs) {
  if(lenght(labs$description) > 0) {
    obj$description <- labs$description
  }
  obj
}

relabelNotes <- function(obj, labs) {
  if(length(labs$notes) > 0) {
    obj$notes <- labs$notes
  }
  obj
}

#' relabelSet for ToplineCategoricalArray
#'
#' @describeIn relabelSet
relabelSet.TopliuneCategoricalArray <- function(obj, labs) {

  if(length(labs$options) > 0) {
    obj <- relabseSet.ToplineVar(obj, labs)
  }

  if(length(labs$items) > 0) {
    if(grepl("Wave", dimnames(obj$crosstabs$Results$`___total___`$proportions)[[2]])) {
      warning(
        paste("We strongly recommend against relabeling wave names as you may have a variable",
              "that is not available in all waves."
        )
      )
    }
    obj$subnames <- labs$items
    dimnames(obj$crosstabs$Results$`___total___`$base)[[2]] <- labs$items
    dimnames(obj$crosstabs$Results$`___total___`$proportions)[[2]] <- labs$itens
    dimnames(obj$crosstabs$Results$`___total___`$counts)[[2]] <- labs$items
  }

  obj <- relabelNotes(obj, labs)
  obj <- relabelDescription(obj, labs)

  obj
}

setResultObj <- function(results, alias, obj) {
  stopifnot(alias %in% names(results$crosstabs))
  results$crosstabs[[alias]] <- obj
  results
}

getResultObj <- function(results, alias) {
  stopifnot(alias %in% names(results$crosstabs))
  results$crosstabs[[alias]]
}
