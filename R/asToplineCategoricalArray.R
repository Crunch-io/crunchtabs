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
      catArrayToCategoricals(questions, question_alias, labels)
    )
  }
  
  props <- obj$crosstabs$Results$`___total___`$proportions
  counts <- obj$crosstabs$Results$`___total___`$counts
  second_label <- attr(counts, "dimnames")[[1]]
  
  obj$subnames <- labels
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
  
  obj$crosstabs$Results$`___total___`$proportions <- m
  
  class(obj) <- c("ToplineCategoricalArray", "ToplineVar", "CrossTabVar")
  
  obj
}

#' Decompose a categorical array
#' 
#' Given two or more waves of a categorical array question, convert them into
#' categoricals for presentation in a tracking report.
#' 
#' @param questions A list of results objects from tabBook
#' @param question_alias The name of the alias in question
#' @param labels The labels for the questions. Will default to Wave X, where X is 1:n 
#' @export
catArrayToCategoricals <- function(questions, question_alias, labels) {
  obj <- questions[[1]]
  statements <- obj$subnames
  cats <- attr(obj$crosstabs$Results$`___total___`$proportions, "dimnames")[[1]]
  
  if (is.null(labels))
    labels <- paste0("Wave ", seq_len(length(questions)))
  
  nms <- paste0(question_alias, seq_along(statements))
  
  if (is.null(question_alias)) {
    question_alias <- obj$alias
  }

  # Create list of objects to fill in, one for each sub statement of the 
  # multiple response group
  l <- lapply(statements, function(x) obj)

  # 1. Create a guide for the data we want to pull out
  # 2. Reorder the guide in a format that fits the 
  # manner in which we wish to encapsulate it
  # 3. Create an NA filled object 
  # 4. Push our data into the object
  guide <- expand.grid(
    1:length(statements), 
    1:length(cats), 
    1:length(labels)
  )
  
  names(guide) <- c("statement", "cat", "label")

  # Order is key
  guide <- guide[with(guide, order(statement, cat, label)),]
  rownames(guide) <- NULL
  guide$value <- NA_real_
  
  # Pull out our data
  for(i in 1:nrow(guide)) {
    guide$value[i] <- questions[[
      guide$label[i]
      ]]$crosstabs$Results$`___total___`$proportions[
      guide$cat[i],guide$statement[i]
    ]
  }
  
  # Pre allocate
  rprops <- lapply(1:length(statements), function(x) 
                   matrix(rep(NA_real_, length(cats)*length(labels)), 
                          ncol = length(labels))
  )
  # Add dimnames for clarity  
  rprops <- lapply(rprops, function(x) { dimnames(x) <- list(cats, labels); x})
  
  for(i in 1:nrow(guide)) {
    rprops[[guide$statement[i]]][guide$cat[i], guide$label[i]] <- guide$value[i]
  }
  
  # Reassign 
  l <- lapply(seq_along(statements), function(x) {
    l[[x]]$alias <- nms[x]
    l[[x]]$subnames <- labels
    l[[x]]$rownames <- attr(obj$crosstabs$Results$`___total___`$counts, "dimnames")[[1]]
    l[[x]]$labels <- labels
    l[[x]]$description <- paste0(statements[x], " (", l[[x]]$description, ")")
    l[[x]]$crosstabs$Results$`___total___`$proportions <- rprops[[x]]
    class(l[[x]]) <- c("ToplineCategoricalArray", "ToplineVar", "CrossTabVar")
    return(l[[x]])
  })
  
  names(l) <- nms
  return(l)
}
