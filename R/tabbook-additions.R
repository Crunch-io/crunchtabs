#' Reflow Question Numbers
#'
#' When we manually add summaries of
#' numeric, datetime or text variables
#' we must "reflow" the question numbers
#' so that they match dataset order.
#'
#' @param x A results object from within the \link{crosstabs} function.
reflowQuestionNumbers <- function(x) {
  for (i in 1:length(x)) {
    x[[i]]$number = i
  }
  x
}

#' Prepare Summary Content
#'
#' Prepare summary content for toplines for classes that
#' are not covered by tabBook such as NumericVariable, DatetimeVariables
#' and TextVariable
#'
#' @param x A variable of class NumericVariable, DatetimeVariable or TextVariable
#' @param weighted Logical. Are these data weighted?
#' @param num The number of verbatim responses to present as a sample. Defaults to 10.
#' @param tz A timezone. Defaults to UTC.
#' @param ... Additional arguments passed to methods
#' @export
prepareExtraSummary <- function(x, weighted = TRUE, num = 10, tz = "UTC", ...) {
  UseMethod("prepareExtraSummary", x)
}

#' @rdname prepareExtraSummary
#' @export
prepareExtraSummary.default <- function(x, weighted = TRUE, num = 10, tz = "UTC", ...) {
  wrong_class_error(x, c("CategoricalVariable", "CategoricalArrayVariable", "MultipleResponseVariable", "TextVariable", "NumericVariable", "DatetimeVariable"), "codebookItem")
}

#' Prepare Numeric Content
#'
#' \link[crunch]{tabBook} does not report an appropriate numeric summary
#' without being provided with a multitable. So we "fake" a numeric summary
#' by overwriting the structure of a categorical object.
#'
#' If data are weighted we display Weighted N instead of Unweighted
#' N
#'
#' @param x A variable of class \link[crunch]{NumericVariable}
#' @inheritParams prepareExtraSummary
#' @importFrom stats median quantile
#' @export
prepareExtraSummary.NumericVariable <- function(x, weighted = TRUE, num = 10, tz = "UTC", ...) {
  y = as.vector(x)
  qt = quantile(y, na.rm = TRUE)
  minima = min(y, na.rm = TRUE)
  maxima = max(y, na.rm = TRUE)
  half = median(y, na.rm = TRUE)
  mu = mean(y, na.rm = TRUE)
  firstq = qt[2]
  thirdq = qt[4]
  stdev = sd(y, na.rm = TRUE)

  # Mock the content object

  obj = resultsObject(
    x,
    top = NULL,
    weighted = weighted,
    body_values = c(minima, firstq, half, mu, thirdq, maxima, stdev),
    body_labels = c(
      "Minimum",
      "1st Quartile",
      "Median",
      "Mean",
      "3rd Quartile",
      "Maximum",
      "Standard Deviation"
    )
  )

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
#' @inheritParams prepareExtraSummary
#' @export
prepareExtraSummary.DatetimeVariable <- function(x, weighted = TRUE, num = 10, tz = "UTC", ...) {
  y = as.POSIXct(as.vector(x), tx = "UTC")
  qt = quantile(y, na.rm = TRUE)
  minima = min(y, na.rm = TRUE)
  maxima = max(y, na.rm = TRUE)
  half = median(y, na.rm = TRUE)
  firstq = qt[2]
  thirdq = qt[4]

  # Mock the content object
  obj = resultsObject(
    x,
    top = NULL,
    weighted = weighted,
    body_values = c(minima, firstq, half, thirdq, maxima),
    body_labels = c(
      "Minimum",
      "1st Quartile",
      "Median",
      "3rd Quartile",
      "Maximum"
    )
  )

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
#' @inheritParams prepareExtraSummary
#' @export
prepareExtraSummary.TextVariable <- function(x, weighted = TRUE, num = 10, tz = "UTC", ...) {
  set.seed(42)
  y = as.vector(x)
  y = sort(sample(unique(y[!is.na(y)]), num, replace = FALSE))
  n = sum(!is.na(y)) # This could be wrong for un-enforced responses such as ""

  # Mock the content object
  obj = resultsObject(
    x,
    weighted = weighted,
    body_values = rep("", length(y)),
    body_labels = y
  )

  obj
}


#' Generic Results Object
#'
#' As \link[crunch]{tabBook} does not provide us with a way to  create summaries
#' for some variable types we are forced to create an object that bypasses
#' the reformatVar function. Our goal is to use as much of the
#' code infrastructure for theming purposes as possible while
#' allowing the creation of new topline summary objects
#'
#' @param x A dataset variable
#' @param top The top of the results object. NULL by default
#' @param weighted Logical. Are these data weighted?
#' @param body_values The values to present
#' @param body_labels The labels to present
resultsObject <- function(x, top = NULL, weighted, body_values, body_labels) {

  stopifnot(length(body_values) == length(body_labels))

  top = top
  data_list = list()
  data_list$body = data.frame(
    x = body_values,
    row.names = body_labels
  )
  names(data_list$body) = NA_character_

  # Presentation differences if data are
  # weighted or unweighted

  if (weighted) {
    data_list$weighted_n = data.frame(
      x = sum(!is.na(as.vector(x))),
      row.names = "Weighted N"
    )
    names(data_list$weighted_n) = NA_character_

    bottom = c(weighted_n = "weighted_n")
    data_order = c("body", weighted_n = "weighted_n")

  } else {

    data_list$unweighted_n = data.frame(
      x = sum(!is.na(as.vector(x))),
      row.names = "Unweighted N"
    )
    names(data_list$unweighted_n) = NA_character_

    bottom = c(unweighted_n = "unweighted_n")
    data_order = c("body",unweighted_n = "unweighted_n")
  }

  structure(
    list(
      alias = crunch::alias(x),
      name = crunch::name(x),
      description = ifelse(
        crunch::description(x) == "",
        crunch::name(x),
        crunch::description(x)
      ),
      notes = crunch::notes(x),
      type = class(x)[1],
      top = NULL,
      bottom = bottom,
      data_order = data_order,
      inserts = rep("Category", length(body_values)),
      data_list = data_list,
      min_cell_top = NULL,
      no_totals = TRUE,
      mean_median = FALSE,
      min_cell_body = matrix(rep(NA, length(body_values))),
      min_cell_bottom = matrix(FALSE),
      min_cell = FALSE,
      rownames = c(body_labels, ifelse(weighted, "Weighted N", "Unweighted N"))),
    class = c("ToplineVar", "CrossTabVar"))
}

