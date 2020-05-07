#' Reflow Question Numbers
#'
#' When we manually add summaries of
#' numeric, datetime or text variables
#' we must "reflow" the question numbers
#' so that they match dataset order.
#'
#' @param x A crosstabs object from the \link{crosstabs} function.
reflowQuestionNumbers <- function(x) {
  for (i in 1:length(x$results)) {
    x$results[[i]]$number = i
  }
  x
}

#' Prepare Summary Content
#'
#' Prepare summary content for codebooks.
#'
#' @param x A variable of class NumericVariable, DatetimeVariable or TextVariable
#' @param ... Additional arguments passed to methods
#' @export
prepareExtraSummary <- function(x, ...) {
  UseMethod("prepareExtraSummary", x)
}

#' @rdname prepareExtraSummary
#' @export
prepareExtraSummary.default <- function(x) {
  wrong_class_error(x, c("CategoricalVariable", "CategoricalArrayVariable", "MultipleResponseVariable", "TextVariable", "NumericVariable", "DatetimeVariable"), "codebookItem")
}

#' Prepare Numeric Content
#'
#' tabBook does not report an appopriate numeric summary without
#' being provided with a multitable. So we "fake" a numeric summary
#' as by overwriting the structure of a categorical object.
#'
#' If data are weighted we display Weighted N instead of Unweighted
#' N
#'
#' @param x A variable of class \link[crunch]{NumericVariable}
#' @param weighted Logical. Are these data weighted?
#' @export
prepareExtraSummary.NumericVariable <- function(x, weighted = TRUE) {
  y = as.vector(x)
  qt = quantile(y, na.rm = TRUE)
  minima = min(y, na.rm = TRUE)
  maxima = max(y, na.rm = TRUE)
  half = median(y, na.rm = TRUE)
  mu = mean(y, na.rm = TRUE)
  firstq = qt[2]
  thirdq = qt[4]
  stdev = sd(y, na.rm = TRUE)
  n = sum(!is.na(y))

  # Mock the content object

  # If no description, use name:
  descript = ifelse(description(x) == "", name(x), description(x))

  obj = structure(
    list(
      alias = alias(x),
      name = name(x),
      description = descript,
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

  if (weighted) {
    # Overwrite naming
    obj$data_list[[2]] = c("weighted_n" = "weighted_n")
    obj$rownames[length(obj$rownames)] = "Weighted N"
    obj$bottom[[1]] = c("weighted_n" = "weighed_n")
  }

  obj
}

#' Prepare Datetime Content
#'
#' tabBook does not report an appropriate date time summary without
#' being provided with a multitable. So we "fake" a date time summary
#' by overwriting the structure of a categorical object.
#'
#' If data are weighted we display Weighted N instead of Unweighted
#' N
#'
#' @param x A variable of class \link[crunch]{DatetimeVariable}
#' @param weighted Logical. Are these data weighted?
#' @param tz A timezone. Defaults to UTC.
#' @export
prepareExtraSummary.DatetimeVariable <- function(x, weighted = TRUE, tz = "UTC") {
  y = as.POSIXct(as.vector(x), tx = "UTC")
  qt = quantile(y, na.rm = TRUE)
  minima = min(y, na.rm = TRUE)
  maxima = max(y, na.rm = TRUE)
  half = median(y, na.rm = TRUE)
  firstq = qt[2]
  thirdq = qt[4]
  n = sum(!is.na(y))

  # Mock the content object

  # If no description, use name:
  descript = ifelse(description(x) == "", name(x), description(x))

  obj = structure(
    list(
      alias = alias(x),
      name = name(x),
      description = descript,
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
              thirdq,
              maxima
            )),
          .Names = NA_character_,
          class = "data.frame",
          row.names = c("Miniumum",
                        "1st Quartile",
                        "Median",
                        "3rd Quartile",
                        "Maximum")),
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
                   "Median",
                   "3rd Quartile",
                   "Maximum",
                   "Unweighted N")),
    class = c("ToplineVar", "CrossTabVar")
  )

  if (weighted) {
    # There are some object naming
    # differences that need to be accomodated
    # depending on if data are weighted
    obj = fixExtraSummaryWeights(obj)
  }

  obj
}

prepareExtraSummary.SurveyDuration <- function(x, weighted = TRUE) {
  y = as.vector(x)
  qt = quantile(y, na.rm = TRUE)
  minima = min(y, na.rm = TRUE)
  maxima = max(y, na.rm = TRUE)
  half = median(y, na.rm = TRUE)
  mu = mean(y, na.rm = TRUE)
  firstq = qt[2]
  thirdq = qt[4]
  stdev = sd(y, na.rm = TRUE)
  n = sum(!is.na(y))

  # Mock the content object

  # If no description, use name:
  descript = ifelse(description(x) == "", name(x), description(x))

  obj = structure(
    list(
      alias = "duration",
      name = "Survey Length",
      description = "What is the length of this survey, in minutes?",
      notes = "",
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

  if (weighted) {
    # Overwrite naming
    obj$data_list[[2]] = c("weighted_n" = "weighted_n")
    obj$rownames[length(obj$rownames)] = "Weighted N"
    obj$bottom[[1]] = c("weighted_n" = "weighed_n")
  }

  obj
}


#' Prepare Text Content
#'
#' tabBook does not report an appropriate date time summary without
#' being provided with a multitable. So we "fake" a date time summary
#' by overwriting the structure of a categorical object.
#'
#' If data are weighted we display Weighted N instead of Unweighted
#' N
#'
#' @param x A variable of class \link[crunch]{TextVariable}
#' @param weighted Logical. Are these data weighted?
#' @param n The number of verbatim responses to present as a sample. Defaults to 10.
#' @export
prepareExtraSummary.TextVariable <- function(x, weighted = TRUE, n = 10L) {
  y = as.vector(x)
  set.seed(42) # The answer
  y = sort(sample(unique(y[!is.na(y)]), n, replace = FALSE))
  n = sum(!is.na(y)) # This could be wrong for un-enforced responses such as ""

  # Mock the content object

  # If no description, use name:
  descript = ifelse(description(x) == "", name(x), description(x))

  obj = structure(
    list(
      alias = alias(x),
      name = name(x),
      description = descript,
      notes = notes(x),
      type = "NumericVariable", # Assigned for diversion
      top = NULL,
      bottom = c(
        unweighted_n = "unweighted_n"),
      data_order = c("body", unweighted_n = "unweighted_n"),
      inserts = rep("Category", n),
      data_list = list(
        body = structure(
          list(rep("", n)),
          .Names = NA_character_,
          class = "data.frame",
          row.names = y),
        unweighted_n = structure(
          list(n),
          .Names = NA_character_, class = "data.frame", row.names = "Unweighted N")),
      min_cell_top = NULL,
      min_cell_body = structure(
        rep(NA, n + 1),
        .Dim = c((n + 1L), 1L)),
      min_cell_bottom = structure(
        FALSE,
        .Dim = c(1L, 1L)),
      min_cell = FALSE,
      rownames = c(y,
                   "Unweighted N")),
    class = c("ToplineVar", "CrossTabVar")
  )

  if (weighted) {
    # There are some object naming
    # differences that need to be accomodated
    # depending on if data are weighted
    obj = fixExtraSummaryWeights(obj)
  }

  obj
}


#' Weighting Object Titles
#'
#' If the object is weighted
#' there is a slight difference in
#' the naming required to align
#' with existing patterns
#'
#' @param obj The obj created by \link{prepareExtraSummary}
#' @param weighted Is the set weighted?
fixExtraSummaryWeights <- function(obj, weighted = FALSE) {
  if (weighted) {
    names(obj$data_list) = c("body", "weighted_n")
    obj$data_list[[2]] = c("weighted_n" = "weighted_n")
    obj$rownames[length(obj$rownames)] = "Weighted N"
    obj$bottom[[1]] = c("weighted_n" = "weighed_n")
    obj
  }
  obj
}
