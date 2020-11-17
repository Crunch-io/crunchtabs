#' Create a multi-dataset tracking report
#' 
#' This function provides a method for creating a multi-dataset tracking report
#' 
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
    function(x) { 
      if(is.null(weight)) {
        weight = weight(x)
      }
      crosstabs(x, vars, weight)
    }
  )
}