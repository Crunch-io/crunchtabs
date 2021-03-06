#' Create a multi-dataset tracking report
#'
#' This function provides a method for creating a multi-dataset tracking report
#'
#' @param dataset_list A list of two or more crunch datasets. Datasets should be
#' provided in time order. From oldest to youngest. (i.e, wave 1, wave 2,
#' ..., wave n)
#' @param vars A character vector of question aliases to be included in the
#' report this may include aliases that are available in at least one of the
#' datasets specified in dataset_list
#' @param weight NULL to accept each dataset's current weight or a single alias
#' that is available in all datasets as a string. Multiple weights is not
#' recommended in a tracking report.
#' @param labels The labels for each wave. Should be of a length that
#' matches the number of datasets.
#' @param show_once A vector of aliases whose values may be available in one or
#' more datasets will only be shown for the latest data set in `dataset_list`.
#' @export
trackingReport <- function(dataset_list, vars, labels = NULL, weight = NULL, show_once = NULL) {
  tabs <- trackingReport_tabs(dataset_list, vars, weight)

  if (is.null(labels)) {
    labels <- paste0("Wave ", seq_along(dataset_list))
  }

  # In previous iterations we used the first item of tabs as a skeleton
  # However, what if there is an alias that is not included in the first
  # variable? Instead we build the rebuilt_results object piece by piece
  # using the first available result for each alias to create a skeleton

  rebuilt_results <- list()
  class(rebuilt_results) <- c("Toplines", "CrunchTabs")
  rebuilt_results$results <- lapply(vars, function(x) NULL)
  rebuilt_results$metadata <- tabs[[1]]$metadata
  names(rebuilt_results$results) <- vars
  rebuilt_results$banner <- NULL

  # Loop through each element of tabs, suck out the first result available
  # per alias and use that result as part of the skeleton. If there is more
  # than one result but less than n results, we need to denote that for future
  # use.
  #
  # For example, if someone has a survey where "q1" was asked in waves 1 and 3
  # but not 2 - we need a good way to identify this.

  for (v in vars) {
    var_results <- lapply(tabs, function(x) {
      return(x$results[[v]])
    })
    results_available <- which(!unlist(lapply(var_results, is.null)))
    first_var_result <- head(which(!unlist(lapply(var_results, is.null))), n = 1)
    last_var_result <- tail(which(!unlist(lapply(var_results, is.null))), n = 1)


    if(v %in% show_once) {
      rebuilt_results$results[[v]] <- var_results[[last_var_result]]
    } else {
      rebuilt_results$results[[v]] <- var_results[[first_var_result]]
    }

    rebuilt_results$results[[v]]$available_at <- results_available


    # For each alias, we set an attribute that identifies it's availability
    # across all the datasets: "all", "partial", and "single"
    # - "all" means it is available in every dataset
    # - "partial" means it is available in only some datasets
    # - "single" means it is available in exactly one dataset

    # Because we use subsetting at the list level, "all" and "partial"
    # would follow a typical path that labeling was adjusted appropriately
    # for presentation in the resulting pdf "single" should act as a simple
    # passthrough where no additional formatting or manipulation takes place
    # on the result.

    # The single case
    if (length(results_available) == 1) {
      rebuilt_results$results[[v]]$availability <- "single"
    } else {
      rebuilt_results$results[[v]]$availability <- "general"
    }
  }

  # Now that we have an attribute that identifies availability we can use it as
  # a trigger for logic that allows us to customize the result of each
  # condition.
  #
  # We wil loop over each variable and either combine those elements that are
  # setup for tracking, or passthrough those that are singles. As singles
  # represent the simplest case, we will deal with them first.

  for (v in vars) {

    c1 <- rebuilt_results$results[[v]]$availability == "single"
    c2 <- v %in% show_once

    if (c1 || c2) {
      next
    }

    available_at <- rebuilt_results$results[[v]]$available_at

    message("Preparing: ", v) # TODO: Delete me after feature dev
    result_list <- lapply(tabs, function(x) x$results[[v]])



    if (rebuilt_results$results[[v]]$type == "categorical_array") {
      rebuilt_results$results <- c(
        catArrayToCategoricals(
          result_list[available_at],
          question_alias = v,
          labels = labels[available_at]
        ),
        rebuilt_results$results
      )
      rebuilt_results$results[[v]] <- NULL

      # We must fake the class of the object
      class(rebuilt_results$results) <- c("ToplineResults", "CrosstabsResults")
    } else {
      rebuilt_results$results[[v]] <- as.ToplineCategoricalArray(
        result_list[available_at],
        question_alias = v,
        labels = labels[available_at]
      )
    }
  }

  rebuilt_results$results <- reflowQuestionNumbers(rebuilt_results$results)

  rebuilt_results
}

#'
trackingReport_tabs <- function(datasets, vars, weight = NULL) {
  lapply(
    datasets,
    function(x) {
      if (is.null(weight)) {
        weight <- weight(x)
      }
      adj_vars <- vars[vars %in% names(x)]
      if(length(adj_vars) == 0) {
        return(NULL)
      } else {
        crosstabs(x, adj_vars, weight, include_numeric = TRUE)
      }

    }
  )
}
