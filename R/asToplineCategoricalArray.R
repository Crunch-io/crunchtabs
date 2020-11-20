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
    labels <- paste0("Wave ", seq_len(length(questions)))
  
  if (length(questions) != length(labels))
    stop("Number of labels provided does not match number of result sets")
  
  # Use the first result item as a skeleton
  obj <- questions[[1]]
  
  is_mr <- questions[[1]]$type == "multiple_response"
  is_catarray <- questions[[1]]$type == "categorical_array"
  
  if(is_catarray) {
    return(
      catArrayToCategorical(questions, question_alias, labels)
    )
  }
  
  props <- obj$crosstabs$Results$`___total___`$proportions
  counts <- obj$crosstabs$Results$`___total___`$counts
  second_label <- attr(counts, "dimnames")[[1]]

  if (is_mr) {
    obj$subnames <- second_label
  } else {
    obj$subnames <- labels
  }

  obj$rownames <- attr(counts, "dimnames")[[1]]
  obj$notes <- questions[[1]]$notes
  obj$type <- "categorical_array"
  obj$labels <- labels
  
  matrix_rows <- length(
    attr(counts, "dimnames")[[1]]
  )
  
  # We pull out counts per result item in wide format  
  m <- sapply(
    questions, 
    function(x) as.numeric(x$crosstabs$Results$`___total___`$counts)
  )
  
  dimnames(m) <- list(
    second_label,
    labels
  )
  
  # if (is_mr) {
  #   m <- t(m)
  # }
  
  obj$crosstabs$Results$`___total___`$counts <- m
  
  # We pull out proportions per result item in wide format
  m <- sapply(
    questions, 
    function(x) as.numeric(x$crosstabs$Results$`___total___`$proportions)
    )
  
  dimnames(m) <- list(
    second_label,
    labels
  )
  
  # if (is_mr) {
  #   m <- t(m)
  # }
  
  obj$crosstabs$Results$`___total___`$proportions <- m
  
  class(obj) <- c("ToplineCategoricalArray", "ToplineVar", "CrossTabVar")
  
  obj
}

#' Decompose a categorical array
#' 
#' Given two or more waves of a categorical array question, convert them into
#' categoricals for presentation in a tracking report.
#' 
#' 
catArrayToCategoricals <- function(questions, question_alias, labels) {
  obj <- questions[[1]]
  statements <- obj$subnames
  
  nms <- paste0(question_alias, seq_along(statements))
  
  if (is.null(question_alias)) {
    question_alias <- obj$alias
  }

  # Create list of objects to fill in, one for each sub statement of the 
  # multiple response group
  l <- lapply(statements, function(x) obj)

  # Pull out counts
  counts <- lapply(
    questions, 
    function(x) matrix(
      as.numeric(
        x$crosstabs$Results$`___total___`$counts), 
      nrow = length(statements))
    )
  
  # Pull out props
  props <- lapply(
    questions, 
    function(x) matrix(
      as.numeric(
        x$crosstabs$Results$`___total___`$proportions), 
      nrow = length(statements))
  )
  
  # Reformat and reorder so that data are grouped by sub question
  # instead of grouped by time period
  reordered_counts <- lapply(seq_along(statements), function(i) {
    matrix(
      unlist(
        lapply(counts, function(x) x[,i])), 
           nrow = length(statements)
    )
  })
  
  reordered_props <- lapply(seq_along(statements), function(i) {
    matrix(
      unlist(
        lapply(props, function(x) x[,i])), 
      nrow = length(statements)
    )
  })
  
  # Reassign it
  l <- lapply(seq_along(statements), function(x) {
    l[[x]]$alias <- nms[x]
    l[[x]]$subnames <- labels
    l[[x]]$rownames <- attr(obj$crosstabs$Results$`___total___`$counts, "dimnames")[[1]]
    l[[x]]$labels <- labels
    l[[x]]$description <- paste0(statements[x], " (", l[[x]]$description, ")")
    # Format and assign counts
    m <- reordered_counts[[x]]
    dimnames(m) <- list(
      attr(obj$crosstabs$Results$`___total___`$counts, "dimnames")[[1]],
      labels
    )
    
    l[[x]]$crosstabs$Results$`___total___`$counts <- m
    
    # Format and assign props
    m <- reordered_props[[x]]
    dimnames(m) <- list(
      attr(obj$crosstabs$Results$`___total___`$proportions, "dimnames")[[1]],
      labels
    )
    
    l[[x]]$crosstabs$Results$`___total___`$proportions <- m
    class(l[[x]]) <- c("ToplineCategoricalArray", "ToplineVar", "CrossTabVar")
    return(l[[x]])
  })
  
  names(l) <- nms
  class(l) <- 
  return(l)
}
