#' Combine two or more tabbook results as a categorical array
#'
#' Here we manipulate the tabBook results so that they match the layout
#' of a categoricalArray, which has the benefit of already having
#' distinct code to write it to latex.
#' 
#' @param questions A list of two or more results objects. Objects should be
#' provided in time order. From oldest to youngest. (ie, wave 1, wave 2, 
#' wave 3, ..., wave n)
#' @param question_alias The name of the question alias across all results 
#' sets. It must be the same in all datasets.
#' @param labels A character vector of labels that will be displayed in the
#' resulting crunchtabs output. Should match the number of results objects
as.ToplineCategoricalArray <- function(questions, question_alias = NULL, labels = NULL) {
  
  if (is.null(labels))
    labels <- paste0("Wave ", seq_len(length(results)))
  
  if (length(results) != length(labels))
    stop("Number of labels provided does not match number of result sets")

  # Use the first result item as a skeleton
  obj <- questions[[1]]
  obj$subnames <- labels
  obj$notes <- questions[[1]]$notes
  obj$type <- "categorical_array"
  
  matrix_rows <- length(
    attr(obj$crosstabs$Results$`___total___`$counts, "dimnames")[[1]]
  )

  # We pull out counts per result item in wide format  
  m <- sapply(
    questions, 
    function(x) as.numeric(x$crosstabs$Results$`___total___`$counts)
  )
  
  if (questions[[1]]$type == "multiple_response")
    m <- t(m)
  
  dimnames(m) <- list(
    attr(obj$crosstabs$Results$`___total___`$counts, "dimnames")[[1]],
    labels
  )
  
  obj$crosstabs$Results$`___total___`$counts <- m
  
  # We pull out proportions per result item in wide format
  m <- sapply(
    questions, 
    function(x) as.numeric(x$crosstabs$Results$`___total___`$proportions))

  if (questions[[1]]$type == "multiple_response")
    m <- t(m)
  
  dimnames(m) <- list(
    attr(obj$crosstabs$Results$`___total___`$proportions, "dimnames")[[1]],
    labels
  )
  
  obj$crosstabs$Results$`___total___`$proportions <- m
  
  class(obj) <- c("ToplineCategoricalArray", "ToplineVar", "CrossTabVar")
  
  obj
}
