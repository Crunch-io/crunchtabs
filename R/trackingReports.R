#' Create a multi-dataset tracking report
#'
#' This function provides a method for creating a multi-dataset tracking report
#'
#' @param dataset_list A list of two or more crunch datasets. Datasets should be
#' provided in time order. From oldest to youngest. (i.e, wave 1, wave 2,
#' ..., wave n)
#' @param vars A character vector of question aliases to be included in the report
#' @param weight NULL to accept each dataset's current weight or a single alias
#' that is available in all datasets as a string.
#' @param labels The labels for each wave. Should be of a length that
#' matches the number of datasets.
tracking_report <- function(dataset_list, vars, labels = NULL, weight = NULL) {
  # topline tabbooks
  tabs <- tracking_report_tabs(dataset_list, vars, weight)

  if (is.null(labels)) {
    labels <- paste0("Wave ", seq_len(length(dataset_list)))
  }

  # Use the first result item as a skeleton
  rebuilt_results <- tabs[[1]]

  for (v in vars) {
    message("Preparing: ", v)
    result_list <- lapply(tabs, function(x) x$results[[v]])
    if (rebuilt_results$results[[v]]$type == "categorical_array") {
      rebuilt_results$results <- c(
        catArrayToCategoricals(
          result_list,
          question_alias = v,
          labels = labels
        ),
        rebuilt_results$results
      )
      rebuilt_results$results[[v]] <- NULL

      # Fix the class!
      class(rebuilt_results$results) <- c("ToplineResults", "CrosstabsResults")
    } else {
      rebuilt_results$results[[v]] <- as.ToplineCategoricalArray(
        result_list,
        question_alias = v,
        labels = labels
      )
    }
  }

  rebuilt_results$results <- reflowQuestionNumbers(rebuilt_results$results)

  rebuilt_results
}

#'
tracking_report_tabs <- function(datasets, vars, weight = NULL) {
  lapply(
    datasets,
    function(x) {
      if (is.null(weight)) {
        weight <- weight(x)
      }
      crosstabs(x, vars, weight, include_numeric = TRUE)
    }
  )
}
