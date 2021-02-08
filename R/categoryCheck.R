#' Verify category stability
#'
#' This function allows one to review a group of questions and identify if they can be reasonably
#' joined together for the purpose of merging, appending, or creating a tracking report.
#'
#' @param var The variable to test
#' @param dataset_list A list of datasets in which the variable resides
#' @param warnings_only Should we present a warning or an error?
categoryCheck <- function(dataset_list, var, warnings_only = FALSE) {

  categoryCheckAvailable(dataset_list, var, warnings_only)
  categoryCheckConsitency(dataset_list, var, warnings_only)

}

#' Check alias availability
#'
#' Given a list of datasets, check which datasets the alias is available inside and present
#' a warning or error for those that it is not available in.
#'
#' @param var The variable to test
#' @param dataset_list A list of datasets in which the variable resides
#' @param warnings_only Should we present a warning or an error?
categoryCheckAvailable <- function(dataset_list, var, warnings_only = FALSE) {
  nms <- lapply(data_list, names)
  checkvar <- unlist(lapply(nms, function(x) var %in% x))
  nms_ds <- lapply(data_list, crunch::name)

  if (warnings_only & !all(checkvar)) {
    stop(
      "`", var, "` not available in every dataset. Missing from dataset_list items: ",
      paste0(nms_ds[which(!checkvar)], collapse = ", ")
    )
  } else {
    warning(
      "`", var, "` not available in every dataset. Missing from dataset_list items: ",
      paste0(nms_ds[which(!checkvar)], collapse = ", ")
    )
  }
}

#' Check category consistency
#'
#' Given a list of results from tracking reports. Check the consistency of aliases and optionally
#' expand (0 filling) the categories so that we can proceed to writing to PDF or Excel
#'
#' @param var The variable to test
#' @param results_list A list of tabBook results from \link{\code{trackingRepotrs_tabs}}
#' @param warnings_only Should we present a warning or an error?
#' @param fix Should we expand categories with zeros so that they do not cause failures
#' when writing to LaTex or Excel?
#' @param output Should we return a list of categories in each of the results s
categoryCheckConsistency <- function(results_list, var, warnings_only = FALSE) {

  cats <- lapply(results_list, function(x) {
    rownames(x$results[[var]]$crosstabs$Results$`___total___`$proportions)
    }
  )

  names(cats) <- unlist(lapply(results_list, function(x) x$metadata$title))

  comparisons <- combn(seq_along(cats), 2)
  comparisons_list <- list()

  for(i in seq_len(ncol(comparisons))) {
    comparisons_list[[i]] <- identical(cats[[comparisons[1, i]]], cats[[comparisons[2, i]]])
  }

  res <- t(rbind(comparisons, unlist(comparisons_list)))

  diffs <- list()

  for(i in seq_len(nrow(res))) {
    diffs[[i]] <- bit::symdiff(cats[[res[i,1]]], cats[[res[i,2]]])
  }

  res <- cbind(res, unlist(lapply(res[,1], paste, collapse = ", ")))
  res <- cbind(res, unlist(lapply(cats[as.numeric(res[,2])], paste, collapse = ", ")))

}


# a1 <- results_list[[1]]$results$presvote2020$crosstabs$Results$`___total___`$proportions
# a2 <- results_list[[2]]$results$presvote2020$crosstabs$Results$`___total___`$proportions
# a3 <- results_list[[3]]$results$presvote2020$crosstabs$Results$`___total___`$proportions
#
#   cats <- list()
#
#   for (id in seq_along(dataset_list)) {
#     tmp <- categories(dataset_list[[id]][[var]])
#
#   }
#
#
#
# }
#
# test_that("Stop if categories are different", {
#
# })
#
# test_that("Stop if variable does not exist in one of the datasets", {
#
# })
#
# test_that("Warn if variable does not exist in one of the datasets", {
#   warnings_only = TRUE
# })
#
# var <- "presvote2020"
# warnings_only = FALSE
# vars <- c('coronavirus_getvaccine', 'consider_biden', 'consider_trump',
#           'blm_agree', 'protests_trump_approval', 'protests_biden_approval', 'concern_suburbs',
#           'end_protests', 'gender', 'ideo5')
