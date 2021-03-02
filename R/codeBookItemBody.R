#' codeBookItemBody
#'
#' Create codebook item body.
#'
#' @param x A crunch dataset object
#' @param ... Further arguments, not used.
#' @export
codeBookItemBody <- function(x, meta = NULL, ...) {
  UseMethod("codeBookItemBody")
}

#' @describeIn codeBookItemBody Default codeBookItemBody.
#' @export
codeBookItemBody.default <- function(x, meta, ...) {
  wrong_class_error(
    x, c(
      "CategoricalVariable",
      "CategoricalArrayVariable",
      "MultipleResponseVariable",
      "TextVariable",
      "NumericVariable",
      "DatetimeVariable",
      "factor",
      "numeric",
      "integer",
      "character"
    ),
    "codeBookItemBody"
  )

  if(is.factor(x)) {
    codeBookItemBody.factor(x, meta = NULL, ...)
  }
  if(is.character(x)) {
   codeBookItemBody.character(x, meta = NULL, ...)
  }
  if(is.numeric(x)) {
    codeBookItemBody.numeric(x, meta = NULL, ...)
  }
}

#' @describeIn codeBookItemBody Creates item body for CategoricalVariable
#' @export
codeBookItemBody.CategoricalVariable <- function(x, meta = NULL, ...) { # nolint
  k <- codeBookSummary(x, meta)
  k$name <- texEscape(k$name)

  if (nrow(k) > 1) {
    k <- k[order(as.numeric(k[, 1])), ]
  }

  k <- as.data.frame(k, stringsAsFactors = F)

  rownames(k) <- NULL
  names(k) <- c("Code", "Label", "Count")

  if (nrow(k) > 20) {
    # If we have more than 20 hide counts only show codes. Use multiple tables
    # row-wise

    une_duex_trois <- suppressWarnings(matrix(seq_len(nrow(k)), nrow = 2))
    une_duex_trois[which(duplicated(as.vector(une_duex_trois)))] <- NA
    une_duex_trois <- t(une_duex_trois)
    k <- cbind(
      k[une_duex_trois[, 1], ],
      "",
      k[une_duex_trois[, 2], ],
      stringsAsFactors = FALSE
    )

    rownames(k) <- NULL
    names(k) <- curlyWrap(c("Code", "Label", "Count", "", "Code", "Label", "Count"))

    k[is.na(k)] <- " "

    midrule_strip <- function(x) gsub("\\midrule", "", x, fixed = TRUE)

    alignment <- c("d", "l", "d", "c", "d", "l", "d")
    names(k) <- curlyWrap(names(k))
    kableExtra::kable(
      k, "latex",
      booktabs = TRUE, align = scolumnAlign(k, alignment),
      longtable = TRUE, linesep = "", escape = FALSE
    ) %>%
      kable_styling_defaults(full_width = TRUE, ...) %>%
      kableExtra::column_spec(c(2, 6), width = "1.75in", latex_column_spec = NULL) %>%
      kableExtra::row_spec(
        0,
        extra_latex_after = "\\cmidrule(l){1-3}\\cmidrule(l){5-7}" # nolint
      ) %>%
      midrule_strip()
  } else {
    alignment <- c("d", "l", "d")
    names(k) <- curlyWrap(names(k))
    kab <- kableExtra::kable(
      k, "latex",
      booktabs = TRUE, longtable = TRUE, align = scolumnAlign(k, alignment),
      linesep = "", escape = FALSE
    )

    if (max(nchar(k$`{Label}`)) > 80) {
      kab <- kab %>%
        kableExtra::column_spec(2, width = "5.25in")
    }

    kab <- kab %>%
      kable_styling_defaults(...)

    # Fix for square braces in options
    gsub("\\hspace*{0in}", "", kab, fixed = TRUE)
  }
}

#' @describeIn codeBookItemBody Creates item body for CategoricalArrayVariable
#' @export
codeBookItemBody.CategoricalArrayVariable <- function(x, meta = NULL, ...) { # nolint
  k <- codeBookSummary(x)
  k[, 1] <- texEscape(k[, 1])
  k[, 2] <- texEscape(k[, 2])

  code_labels <- texEscape(gsub("[0-9]+ ", "", names(k))[-c(1, 2)])
  code_numbers <- trimws(sub("\\D*(\\d+).*", "\\1", names(k)))[-c(1, 2)]

  rownames(k) <- NULL


  tab_list <- list()

  # Rows: variable, label

  krows <- data.frame(
    Variable = k[, 1] %>% monospaced(),
    Label = k[, 2],
    stringsAsFactors = F
  )

  names(krows) <- curlyWrap(names(krows))

  if (any(max(nchar(krows$`{Label}`)) > 80)) {
    krows <- kableExtra::kable(
      krows,
      "latex",
      booktabs = TRUE,
      longtable = TRUE,
      align = "ll",
      escape = F, linesep = " "
    ) %>%
      kable_styling_defaults(...) %>%
      kableExtra::add_header_above(c("Rows" = 2)) %>%
      kableExtra::column_spec(2, width = "4.75in", latex_column_spec = NULL)
  } else {
    krows <- kableExtra::kable(
      krows,
      "latex",
      booktabs = TRUE,
      longtable = TRUE,
      align = "ll",
      escape = F, linesep = " "
    ) %>%
      kable_styling_defaults(...) %>%
      kableExtra::add_header_above(c("Rows" = 2)) %>%
      kableExtra::column_spec(1, latex_column_spec = NULL)
  }

  # Columns: Code, Label

  kcols <- data.frame(
    Code = code_numbers,
    Label = code_labels,
    stringsAsFactors = F
  )

  names(kcols) <- curlyWrap(names(kcols))

  kcols <- kableExtra::kable(
    kcols,
    "latex",
    booktabs = TRUE,
    longtable = TRUE,
    align = scolumnAlign(kcols, c("d", "l")),
    escape = FALSE, linesep = " "
  ) %>%
    kable_styling_defaults(...) %>%
    kableExtra::add_header_above(c("Columns" = 2))


  # Counts: Variable, 1,2,3,4,5, etc, Missing

  kcounts <- as.data.frame(
    k[, -2],
    stringAsFactors = FALSE
  )

  kcounts[[1]] <- monospaced(kcounts[[1]])

  names(kcounts) <- curlyWrap(c("Variable", code_numbers))

  alignment <- c("l", rep("d", ncol(kcounts) - 1))
  kcounts <- kableExtra::kable(
    kcounts,
    "latex",
    booktabs = TRUE,
    longtable = TRUE,
    align = scolumnAlign(kcounts, alignment),
    escape = F, linesep = " "
  ) %>%
    kable_styling_defaults(...) %>%
    kableExtra::add_header_above(c(" ", "Counts" = ncol(kcounts) - 1))

  # tab_list

  tab_list$krows <- krows
  tab_list$kcols <- kcols
  tab_list$kcounts <- kcounts

  tab_list
}

#' @describeIn codeBookItemBody Creates item body for MultipleResponseVariable
#' @export
codeBookItemBody.MultipleResponseVariable <- codeBookItemBody.CategoricalArrayVariable # nolint

#' @describeIn codeBookItemBody Creates item body for DatetimeVariable
#' @export
codeBookItemBody.DatetimeVariable <- function(x, meta = NULL, ...) { # nolint
  k <- codeBookSummary(x, meta)
  alignment <- c("c", "c", "c")
  kableExtra::kable(
    k, "latex",
    booktabs = TRUE, longtable = TRUE, align = alignment, linesep = "") %>%
    kable_styling_defaults(...)
}

#' @describeIn codeBookItemBody Creates item body for NumericVariable
#' @export
codeBookItemBody.NumericVariable <- function(x, meta = NULL, ...) { # nolint
  k <- codeBookSummary(x, meta)
  if (k$Mean > 9999) {
    k$Mean <- format(k$Mean, scientific = TRUE, digits = 3)
    k$SD <- format(k$SD, scientific = TRUE, digits = 3)
    k$Min <- format(k$Min, scientific = TRUE, digits = 3)
    k$Max <- format(k$Max, scientific = TRUE, digits = 3)
  }
  names(k) <- curlyWrap(names(k))
  alignment <- c("c") # No S/d columns here because of sci
  kableExtra::kable(k, "latex",
                    booktabs = TRUE, longtable = TRUE,
                    align = alignment, linesep = "", escape = FALSE
  ) %>%
    kable_styling_defaults(...)
}

#' @describeIn codeBookItemBody Creates item body for TextVariable
#' @export
codeBookItemBody.TextVariable <- function(x, meta = NULL, ...) {
  k <- codeBookSummary(x, meta)
  names(k) <- curlyWrap(names(k))
  alignment <- c("c")

  kableExtra::kable(k, "latex",
                    booktabs = TRUE, longtable = TRUE,
                    align = alignment, linesep = "", escape = FALSE
  ) %>%
    kable_styling_defaults(...)
}

#' @export
codeBookItemBody.character <- codeBookItemBody.TextVariable

#' @export
codeBookItemBody.factor <- codeBookItemBody.CategoricalVariable

#' @export
codeBookItemBody.numeric <- codeBookItemBody.NumericVariable

#' @export
codeBookItemBody.integer <- codeBookItemBody.NumericVariable
