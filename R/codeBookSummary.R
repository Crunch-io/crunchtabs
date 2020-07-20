#' Summarize crunch variables for codeBooks
#'
#' This group of functions creates a summarized data.frame that can be cnoverted
#' into either a kable or manually glued into a latex.
#'
#' @param x A single variable from a crunch dataset
#' @param ... Additional arguments, unused.
#' @export
codeBookSummary <- function(x, ...) {
  UseMethod("codeBookSummary", x)
}

#' @describeIn codeBookSummary The default, throws out anything that does not match expected crunch variable classes
#' @export
codeBookSummary.default <- function(x, ...) {
  wrong_class_error(x,
                    c("CategoricalVariable",
                      "CategoricalArrayVariable",
                      "MultipleResponseVariable",
                      "TextVariable",
                      "NumericVariable",
                      "DatetimeVariable"),
                    "codeBookSummary")
}

#' @describeIn codeBookSummary Prepares a codeBookSummary data.frame for a CategoricalVariable
#' @importFrom utils type.convert
#' @param multiple Is this a MultipleResponse or CategoricalArray variable?
#' @export
codeBookSummary.CategoricalVariable <- function(x, multiple = FALSE, ...) {
  cats <- crunch::categories(x)
  responses <- suppressWarnings(do.call(rbind, cats@.Data))
  l <- list()
  for (i in 1:nrow(responses)) {

    if (is.null(responses[i, ]$numeric_value)) {
      # CategoricalVariable within MultipleResponseVariable
      if (responses[i, ]$name %in% c("not selected", "selected")) {
        responses[i,]$numeric_value <- ifelse(
          responses[i, ]$name == "selected",
          1L,
          2L
        )
      } else {
        responses[i,]$numeric_value <- NA_integer_
      }
    }

    l[[i]] <- as.data.frame(
      matrix(
        unlist(responses[i,]),
        ncol = dim(responses)[2]
      )
    )
  }

  l <- do.call(rbind, l)
  # CategoricalVariable
  if (ncol(l) == 4) {
    names(l) <- c("id", "missing", "name", "value")
  }

  # CategoricalVariable within MultipleResponseVariable
  if (ncol(l) == 5) {
    names(l) <- c("id", "missing", "name", "value", "drop")

  }

  for (i in 1:ncol(l)) { l[[i]] <- type.convert(l[[i]], as.is = T) }

  smry <- data.frame(crunch::table(x, useNA = "ifany"))
  names(smry) <- c("name", "n")

  res <- merge(l, smry)

  if (multiple) {
    res[with(res, order(id)),]
  } else {
    res = matrix(c(
      res$id,
      res$name,
      res$n
    ), ncol = 3)
    res
  }

}

#' @describeIn codeBookSummary Prepares a codeBookSummary data.frame for a MultipleResponseVariable
#' @export
codeBookSummary.MultipleResponseVariable <- function(x, ...) {
  responses <- list()
  subvars <- names(subvariables(x))
  for (i in 1:length(names(x))) {
    responses[[i]] <- codeBookSummary(x[[i]], multiple = TRUE)
  }

  names(responses) <- subvars

  rws <- length(responses)
  frame = unique(do.call(rbind, responses)[,c('name', 'id')])
  nms <- c("", "", paste(frame$id, frame$name))
  cols <- nrow(frame)
  m <- matrix(rep(NA, (rws)*(cols + 2)), ncol = cols + 2, nrow = rws)

  for (i in 1:rws) {
    # We merge on a complete frame because responses
    # can be missing from categories
    responses_adj <- merge(
      frame,
      responses[[i]],
      all.x = TRUE,
      sort = FALSE)$n # merge unintuitive sort behaviour!

    m[i,3:(cols + 2)] <- responses_adj
  }

  r <- data.frame(m)
  r$X1 <- names(x)
  r$X2 <- subvars
  names(r) <- nms
  r
}

#' @describeIn codeBookSummary Prepares a codeBookSummary data.frame for a CategoricalArrayVariable
#' @export
codeBookSummary.CategoricalArrayVariable <- function(x, ...) codeBookSummary.MultipleResponseVariable(x, ...)

#' @describeIn codeBookSummary Prepares a codeBookSummary data.frame for a NumericVariable
#' @export
codeBookSummary.NumericVariable <- function(x, ...) {
  minima <- round(min(x, na.rm = T), 2)
  maxima <- round(max(x, na.rm = T), 2)
  missings <- sum(is.na(as.vector(x)))

  type_row <- c("Type", "Numeric")
  range_row <- c("Range", paste0("[", minima,", ", maxima,"]"))

  if (missings > 0) {
    missings_row <- c("Missing", missings)
    r <- rbind(
      type_row, missings_row, range_row
    )
  } else {
    r <- rbind(
      type_row, range_row
    )
  }

  rownames(r) <- NULL
  r

}

#' @describeIn codeBookSummary Prepares a codeBookSummary data.frame for a TextVariable
#' @export
codeBookSummary.TextVariable <- function(x, ...) {

  filled <- sum(as.vector(x) != "" | !is.na(as.vector(x)), na.rm = TRUE)
  type_row <- c("Type", "Text")
  missing <- c("Blank", length(as.vector(x)) - filled)
  filled <- c("Filled", filled)

  r <- rbind(type_row, missing, filled)
  rownames(r) <- NULL
  r
}

#' @describeIn codeBookSummary Prepares a codeBookSummary data.frame for a DatetimeVaraible
#' @export
codeBookSummary.DatetimeVariable <- function(x, ...) {
  minima <- min(x, na.rm = T)
  maxima <- max(x, na.rm = T)
  missings <- sum(is.na(as.vector(x)))

  type_row <- c("Type", "Datetime")
  range_row <- c("Range", paste0("[", minima,", ", maxima,"]"))

  if (missings > 0) {
    missings_row <- c("Missing", missings)
    r <- rbind(
      type_row, missings_row, range_row
    )
  } else {
    r <- rbind(
      type_row, range_row
    )
  }

  rownames(r) <- NULL
  r

}
