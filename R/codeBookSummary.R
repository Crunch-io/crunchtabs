#' Summarize crunch variables for codeBooks
#'
#' This group of functions creates a summarized data.frame that can be cnoverted
#' into either a kable or manually glued into a latex.
#'
#' @param x A single variable from a crunch dataset
#' @param ... Additional arguments, unused.
#' @export
codeBookSummary <- function(x, meta, ...) {
  UseMethod("codeBookSummary")
}

#' @describeIn codeBookSummary The default, throws out anything that does not
#' match expected crunch variable classes
#' @export
codeBookSummary.default <- function(x, meta, ...) {
  wrong_class_error(
    x,
    c(
      "CategoricalVariable",
      "CategoricalArrayVariable",
      "MultipleResponseVariable",
      "TextVariable",
      "NumericVariable",
      "DatetimeVariable",
      "factor",
      "character",
      "integer",
      "numeric"
    ),
    "codeBookSummary"
  )
}

#' @describeIn codeBookSummary Prepares a codeBookSummary data.frame for a factor
#' @param meta A list containning question meta data
#' @export
codeBookSummary.factor <- function(x, meta, ...) {

  if(!is.na(meta$recode)) {
    labs = na.omit(
      eval(
        parse(
          text = as.character(
            meta$recode
          )
        )
      )
    )

    labs <- labs[!is.na(labs)]
    labs[labs == " " & !is.na(labs)] <- NA
  } else {
    labs <- levels(x)
  }

  smry <- table(x, useNA = "ifany")

  if (any(is.na(names(smry)))) {
    labs <- c(labs, NA_character_)
  }

  r <- data.frame(
    id = seq_along(unique(labs)),
    name = names(smry),
    n = as.numeric(smry),
    stringsAsFactors = FALSE
  )

  if (any(is.na(r$name))) {
    r$id[is.na(r$name)] <- NA_integer_
    r$name[is.na(r$name)] <- "Missing"
  }

  r
}

codeBookSummary.numeric <- function(x, meta, ...) {
  mu <- round(mean(x, na.rm = T), 2)
  std <- round(sd(x, na.rm = TRUE))
  minima <- suppressWarnings(round(min(x, na.rm = T), 2))
  maxima <- suppressWarnings(round(max(x, na.rm = T), 2))
  missings <- sum(is.na(x))
  n <- length(is.na(x))

  # type_row <- c("Type", "Numeric")
  # range_row <- c("Range", paste0("[", minima,", ", maxima,"]"))

  r <- as.data.frame(
    matrix(c(
      mu,
      std,
      minima,
      maxima,
      n - missings,
      missings
    ), nrow = 1)
  )

  names(r) <- c("Mean", "SD", "Min", "Max", "n", "Missing")
  rownames(r) <- NULL
  r
}

codeBookSummary.integer <- codeBookSummary.numeric

codeBookSummary.character <- function(x, meta, ...) {
  filled_verbs <- x
  filled_verbs <- filled_verbs[!is.na(filled_verbs)]
  filled_verbs <- filled_verbs[!filled_verbs %in% c("", "__NA__")]

  filled <- length(filled_verbs)
  missing <- length(x) - filled

  r <- as.data.frame(
    matrix(c(
      filled,
      missing,
      ifelse(filled == 0, 0, max(nchar(filled_verbs)))
    ), nrow = 1, ncol = 3),
    stringsAsFactors = FALSE
  )

  names(r) <- c("Filled", "Missing", "Max Length")
  rownames(r) <- NULL
  r
}

#' @describeIn codeBookSummary Prepares a codeBookSummary data.frame for a
#' CategoricalVariable
#' @importFrom utils type.convert
#' @param multiple Is this a MultipleResponse or CategoricalArray variable?
#' @export
codeBookSummary.CategoricalVariable <- function(x, multiple = FALSE, meta = NULL, ...) { # nolint
  cats <- crunch::categories(x)

  cats <- crunch::categories(x)
  responses <- data.frame(
    id = ids(cats),
    missing = missing(cats),
    value = values(cats),
    name = names(cats),
    selected = is.selected(cats)
  )

  smry <- data.frame(crunch::table(x, useNA = "ifany"))
  names(smry) <- c("name", "n")

  res <- merge(responses, smry, sort = FALSE)

  # nocov start
  if (is.null(multiple)) {
    multiple = FALSE
  }

  if (multiple) {
    res <- res[with(res, order(id)), ]
  } else {
    res <- as.data.frame(
      matrix(c(
        res$id,
        res$name,
        res$n
      ), ncol = 3),
      stringsAsFactors = FALSE
    )
    names(res) <- c("id", "name", "n")
  }
  # nocov end
  if (getOption("crunchtabs.codebook.suppress.zeros", default = FALSE)) {
    warning("Zero count categoricals are supressed by options")
    res <- res[res$n != "0", ]
  }

  res
}

#' @describeIn codeBookSummary Prepares a codeBookSummary data.frame for a
#' MultipleResponseVariable
#' @export
codeBookSummary.MultipleResponseVariable <- function(x, meta = NULL, ...) { # nolint
  responses <- list()
  sv <- crunch::subvariables(x)
  subvars <- names(sv)
  for (i in seq_along(names(x))) {
    responses[[i]] <- codeBookSummary(x[[i]], multiple = TRUE)
  }

  names(responses) <- subvars

  rws <- length(responses)
  frame <- unique(do.call(rbind, responses)[, c("name", "id")])
  nms <- c("", "", paste(frame$id, frame$name))
  cols <- nrow(frame)
  m <- matrix(rep(NA, (rws) * (cols + 2)), ncol = cols + 2, nrow = rws)

  for (i in 1:rws) {
    # We merge on a complete frame because responses
    # can be missing from categories
    responses_adj <- merge(
      frame,
      responses[[i]],
      all.x = TRUE,
      sort = FALSE
    )$n # merge unintuitive sort behaviour!

    m[i, 3:(cols + 2)] <- responses_adj
  }

  r <- data.frame(m)
  r$X1 <- names(x)
  r$X2 <- subvars
  names(r) <- nms
  r
}

#' @describeIn codeBookSummary Prepares a codeBookSummary data.frame for a
#' CategoricalArrayVariable
#' @export
codeBookSummary.CategoricalArrayVariable <- function(x, meta = NULL, ...) { # nolint
  codeBookSummary.MultipleResponseVariable(x, ...) # nocov
}

#' @describeIn codeBookSummary Prepares a codeBookSummary data.frame for a NumericVariable
#' @export
codeBookSummary.NumericVariable <- function(x, meta = NULL, ...) { # nolint
  x <- as.vector(x)
  mu <- round(mean(x, na.rm = T), 2)
  std <- round(sd(x, na.rm = TRUE))
  minima <- round(min(x, na.rm = T), 2)
  maxima <- round(max(x, na.rm = T), 2)
  missings <- sum(is.na(as.vector(x)))
  n <- length(is.na(as.vector(x)))

  # type_row <- c("Type", "Numeric")
  # range_row <- c("Range", paste0("[", minima,", ", maxima,"]"))

  r <- as.data.frame(
    matrix(c(
      mu,
      std,
      minima,
      maxima,
      n - missings,
      missings
    ), nrow = 1)
  )

  names(r) <- c("Mean", "SD", "Min", "Max", "n", "Missing")
  rownames(r) <- NULL
  r
}

#' @describeIn codeBookSummary Prepares a codeBookSummary data.frame for a TextVariable
#' @export
codeBookSummary.TextVariable <- function(x, meta = NULL, ...) {
  filled_verbs <- as.vector(x)
  filled_verbs <- filled_verbs[!is.na(filled_verbs)]
  filled_verbs <- filled_verbs[!filled_verbs %in% c("", "__NA__")]
  # verbsamp <- sample(filled_verbs, 5, replace = TRUE)
  # verbsamp <- paste0(substr(verbsamp, start = 1, stop = 27),
  #       "...",
  #       collapse = ", "
  # )
  filled <- length(filled_verbs)
  missing <- length(as.vector(x)) - filled

  r <- as.data.frame(
    matrix(c(
      filled,
      missing,
      ifelse(filled == 0, 0, max(nchar(filled_verbs)))
    ), nrow = 1, ncol = 3),
    stringsAsFactors = FALSE
  )

  names(r) <- c("Filled", "Missing", "Max Length")
  rownames(r) <- NULL
  r
}

#' @describeIn codeBookSummary Prepares a codeBookSummary data.frame for a DatetimeVaraible
#' @export
codeBookSummary.DatetimeVariable <- function(x, meta = NULL, ...) { # nolint
  x <- as.vector(x)
  minima <- min(x, na.rm = T)
  maxima <- max(x, na.rm = T)
  missings <- sum(is.na(as.vector(x)))

  filled <- sum(!is.na(as.vector(x)))
  range_row <- c(paste0("[", minima, ", ", maxima, "]"))

  r <- data.frame(
    Filled = filled,
    Missing = missings,
    Range = range_row,
    stringsAsFactors = FALSE
  )


  rownames(r) <- NULL
  r
}
