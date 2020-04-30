#' When we manually add summaries of
#' numeric, datetime or text variables
#' we must "reflow" the question numbers
#' so that they match dataset order.
#'
#' @param x A crosstabs object from the \link{crosstabs} function.
reflowQuestionNumbers <- function(x) {
  for(i in 1:length(x$results)) {
    x$results[[i]]$number = i
  }
  x
}

#' Prepare Numeric Content
#'
#' tabBook does not report an appopriate numeric summary without
#' being provided with a multitable. So we "fake" a numeric summary
#' as by overwriting the structure of a categorical object.
#'
prepareNumericSummary <- function(x, digits = 3) {
  y = as.vector(x)
  qt = round(quantile(y, na.rm = TRUE),digits)
  minima = round(min(y, na.rm = TRUE),digits)
  maxima = round(max(y, na.rm = TRUE),digits)
  half = round(median(y, na.rm = TRUE),digits)
  mu = round(mean(y, na.rm = TRUE),digits)
  firstq = qt[2]
  thirdq = qt[4]
  stdev = round(sd(y, na.rm = TRUE),digits)
  n = sum(!is.na(y))

  # Mock the content object

  structure(
    list(
      alias = alias(x),
      name = name(x),
      description = description(x),
      notes = notes(x),
      type = "NumericVariable", # Assigned for diversion
      top = NULL,
      bottom = c(
        unweighted_n = "unweighted_n"),
      data_order = c("body", unweighted_n = "unweighted_n"),
      inserts = c("Category", "Category", "Category", "Category", "Category", "Category"),
      data_list = list(
        body = structure(
          list(
            c(
              minima,
              firstq,
              half,
              mu,
              thirdq,
              maxima,
              stdev
            )),
          .Names = NA_character_,
          class = "data.frame",
          row.names = c("Miniumum",
                        "1st Quartile",
                        "Median","Mean",
                        "3rd Quartile",
                        "Maximum",
                        "Standard Deviation")),
        unweighted_n = structure(
          list(n),
          .Names = NA_character_, class = "data.frame", row.names = "Unweighted N")),
      min_cell_top = NULL,
      min_cell_body = structure(
        c(NA, NA, NA, NA, NA, NA),
        .Dim = c(6L, 1L)),
      min_cell_bottom = structure(
        FALSE,
        .Dim = c(1L, 1L)),
      min_cell = FALSE,
      rownames = c("Miniumum",
                   "1st Quartile",
                   "Median","Mean",
                   "3rd Quartile",
                   "Maximum",
                   "Standard Deviation",
                   "Unweighted N")),
    class = c("ToplineVar", "CrossTabVar")
  )
}
