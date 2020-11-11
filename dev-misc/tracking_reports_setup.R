# Generate datasets that allow us to test the functionality of multi-ds tracking
# reports. 

library(crunchtabs)
login()

# # Delete datasets if they already exist ----
# with_consent(deleteDataset("Example dataset"))
# with_consent(deleteDataset("Example dataset W1"))
# with_consent(deleteDataset("Example dataset W2"))
# with_consent(deleteDataset("Example dataset W3"))
# 
# # Create datasets ----- 
# ds1 <- newExampleDataset()
# name(ds1) <- "Example dataset W1"
# 
# ds2 <- newExampleDataset()
# name(ds2) <- "Example dataset W2"
# 
# ds3 <- newExampleDataset()
# name(ds3) <- "Example dataset W3"
# 
# # Setup weights
# ds1$weight1 <- makeWeight(ds1$q1 ~ c(0.3,0.3,0.4,0), name = 'weight1')
# ds2$weight1 <- makeWeight(ds2$q1 ~ c(0.4,0.4,0.1,0.1), name = 'weight1')
# ds3$weight1 <- makeWeight(ds3$q1 ~ c(0.2,0.2,0.4,0.2), name = 'weight1')
# 
# weight(ds1) <- ds1$weight1
# weight(ds2) <- ds2$weight1
# weight(ds3) <- ds3$weight1

ds1 <- loadDataset("Example dataset W1")
ds2 <- loadDataset("Example dataset W2")
ds3 <- loadDataset("Example dataset W3")



#' @param datasets A list of two or more crunch datasets. Datasets should be 
#' provided in time order. From oldest to youngest. (i.e, wave 1, wave 2, 
#' ..., wave n)
#' @param weight NULL to accept each dataset's current weight or a single alias
#' that is available in all datasets as a string.
#' @param wave_labels The labels for each wave. Should be of a length that
#' matches the number of datasets.
tracking_report <- function(dataset_list, vars, wave_labels = NULL, weight = NULL) {
  # topline tabbooks
  tabs <- tracking_report_tabs(dataset_list, vars, weight)

  # Use the first result item as a skeleton
  rebuilt_results <- tabs[[1]] 
  
  for (v in vars) {
    result_list <- lapply(tabs, function(x) x$results[[v]])
    rebuilt_results$results[[v]] <- as.ToplineCategoricalArray(
      result_list, 
      question_alias = v, 
      labels = wave_labels
    )
  }
  
  rebuilt_results
}

#' 
tracking_report_tabs <- function(datasets, vars, weight = NULL) {
  lapply(
    datasets, 
    function(x) crosstabs(x, vars, weight)
  )
}


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


ct <- tracking_report(list(ds1, ds2, ds3), vars = c("country", "allpets"))



writeLatex(ct, pdf = TRUE)
