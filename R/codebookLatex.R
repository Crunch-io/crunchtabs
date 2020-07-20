# Item Txt Elements -----

#' codeBook Item Text Header
#'
#' Creates a text header for a codebook item
#'
#' @inheritParams codeBookItemTxtDescription
#' @export
codeBookItemTxtHeader <- function(x, ...)  {
  txt <- list()
  txt$name <- crunch::name(x)
  txt$alias <- crunch::alias(x)

  tex = "\\textbf{%s}\\hfill\\textbf{\\ttfamily{%s}}"

  sprintf(
    tex,
    fixUnderscore(txt$name),
    fixUnderscore(txt$alias)
  )
}



#' Extract basic question information
#'
#' Extracts the following:
#'
#' * alias
#' * description or question text
#' * notes or filter text
#'
#' @param x A dataset variable
#' @param ... Additional arguments passed to \link{kable_styling_defaults}
#' @md
#' @export
codeBookItemTxtDescription <- function(x, ...) {
  txt <- list()
  txt$description <- crunch::description(x)
  txt$notes <- crunch::notes(x)

  if (txt$notes != "") {
    tex = "\\vskip 0.10in\n%s\n\\addcontentsline{lot}{table}{%s}\n\\vskip 0.10in\n\\emph{%s}\n\\vskip 0.10in"
    tex = sprintf(
      tex,
      txt$description,
      txt$description,
      txt$notes
    )
  } else {
    tex = "\\vskip 0.10in\n%s\n\\addcontentsline{lot}{table}{%s}\n\\vskip 0.10in"
    tex = sprintf(
      tex,
      txt$description,
      txt$description
    )
  }

  tex
}

# Item Body ----

#' codeBookItemBody
#'
#' Create codebook item body.
#'
#' @param x A crunch dataset object
#' @param ... Further arguments, not used.
#' @export
codeBookItemBody <- function(x, ...) {
  UseMethod("codeBookItemBody")
}

#' @describeIn codeBookItemBody Default codeBookItemBody.
#' @export
codeBookItemBody.default <- function(x, ...) {
  wrong_class_error(x, c(
    "CategoricalVariable",
    "CategoricalArrayVariable",
    "MultipleResponseVariable",
    "TextVariable",
    "NumericVariable",
    "DatetimeVariable"),
    "codeBookItemBody"
  )
}

#' @describeIn codeBookItemBody Creates item body for CategoricalVariable
#' @export
codeBookItemBody.CategoricalVariable <- function(x, ...) {
  k = codeBookSummary(x)
  k = k[order(as.numeric(k[,1])),] %>% as.data.frame(stringsAsFactors = F)


  names(k) = c("Code", "Label", "Count")

  if (nrow(k) > 20) {
    # If we have more than 20 hide counts
    # only show codes. Use multiple tables
    # row-wise

    une_duex_trois <- suppressWarnings(matrix(1:nrow(k), nrow = 2))
    une_duex_trois[which(duplicated(as.vector(une_duex_trois)))] <- NA
    une_duex_trois <- t(une_duex_trois)
    k = cbind(
      k[une_duex_trois[,1],],
      k[une_duex_trois[,2],],
      stringsAsFactors = FALSE
    )

    rownames(k) = NULL
    names(k) = rep(c("Code", "Label", "Count"),2)
    k[is.na(k)] = " "


    alignment = "clcclc"

    knitr::kable(
      k, "latex", booktabs = TRUE, longtable = TRUE, align = alignment, linesep = "") %>%
      kableExtra::kable_styling(full_width = TRUE) %>%
      kableExtra::column_spec(c(2,5), width = "1.75in") %>%
      # {gsub(
      #   "\\midrule",
      #   "\\cmidrule{1-3}\n\\cmidrule{4-6}", .,
      #   fixed = TRUE) } %>%
      {gsub(
        "\\addlinespace",
        "", .,
        fixed = TRUE) }

  } else {
    alignment = c("c","l", "r")
    kableExtra::kable(
      k, "latex", booktabs = TRUE, longtable = TRUE, align = alignment, linesep = "") %>%
      kable_styling_defaults(...) %>%
      kableExtra::column_spec(c(1,3), width = "1in")
  }

}

#' @describeIn codeBookItemBody Creates item body for CategoricalArrayVariable
#' @export
codeBookItemBody.CategoricalArrayVariable <- function(x, ...) {
  k = codeBookSummary(x)
  alignment <- c(rep("l",2),rep("c", ncol(k) - 2))
  col_one <- round(max(nchar(k[,1]))*0.08, 2)
  names(k) = c("Variable", "Label", names(k)[-c(1,2)])
  header_width = round(nchar(names(k)[-c(1,2)])*0.08,2)

  space_remaining = 6.0 - col_one - sum(header_width)*1.5
  message(name(x), space_remaining)
  k$Variable <- kableExtra::cell_spec(k$Variable, "latex", monospace = TRUE)

  ln = ncol(k) - 2

  if ((sum(nchar(k$Label) > 45) < 2) & (space_remaining >= 1.5)) {

    kableExtra::kable(
      k,
      "latex",
      booktabs = TRUE,
      longtable = F,
      align = alignment,
      escape = F,
      linesep = " ") %>%
      # kable_styling_defaults(...) %>%
      kableExtra::column_spec(column = 1, width = paste0(col_one, "in")) %>%
      kableExtra::column_spec(column = 2, width = paste0(col_two, "in")) %>%
      kableExtra::add_header_above(c("", "", "Codes" = ln))


  } else {

    k_adj <- dplyr::select(k, names(k)[!names(k) %in% "Label"])

    label_table <- kableExtra::kable(
      k[1:2],
      "latex",
      booktabs = TRUE,
      longtable = FALSE,
      align = "ll",
      escape = F, linesep = " ") %>%
      kableExtra::column_spec(column = 1, width = paste0(col_one, "in")) %>%
      kableExtra::column_spec(column = 2, width = paste0(6 - col_one, "in"))

    counts_table <- kableExtra::kable(
      k_adj,
      "latex",
      booktabs = TRUE,
      longtable = F,
      align = alignment,
      escape = F, linesep = "") %>%
      # kable_styling_defaults(...) %>%
      kableExtra::column_spec(column = 1, width = paste0(col_one, "in")) %>%
      # kableExtra::column_spec(2, width = paste0(col_two, "in")) %>%
      kableExtra::add_header_above(c(" ", "Codes" = ncol(k_adj) - 1))


    list(label_table, "\\vspace{2em}", counts_table, "\\vspace{2em}")
  }


}

#' @describeIn codeBookItemBody Creates item body for MultipleResponseVariable
#' @export
codeBookItemBody.MultipleResponseVariable <- codeBookItemBody.CategoricalArrayVariable

#' @describeIn codeBookItemBody Creates item body for DatetimeVariable
#' @export
codeBookItemBody.DatetimeVariable <- function(x, ...) {
  k = codeBookSummary(x)
  alignment <- c("c", "l")
  kableExtra::kable(k, "latex", booktabs = TRUE, longtable = TRUE, align = alignment, linesep = "") %>%
    kable_styling_defaults(...) %>%
    kableExtra::column_spec(1, width = "1in")
}

#' @describeIn codeBookItemBody Creates item body for NumericVariable
#' @export
codeBookItemBody.NumericVariable <- function(x, ...) {
  k = codeBookSummary(x)
  alignment <- c("c", "l")
  kableExtra::kable(k, "latex", booktabs = TRUE, longtable = TRUE, align = alignment, linesep = "") %>%
    kable_styling_defaults(...) %>%
    kableExtra::column_spec(1, width = "1in")
}

#' @describeIn codeBookItemBody Creates item body for TextVariable
#' @export
codeBookItemBody.TextVariable <- function(x, ...) {
  k = codeBookSummary(x)
  alignment <- c("c","l")
  kableExtra::kable(k, "latex", booktabs = TRUE, longtable = TRUE, align = alignment, linesep = "") %>%
    kable_styling_defaults(...) %>%
    kableExtra::column_spec(1, width = "1in")
}

# utils ----

#' Defaults for kableExtra
#'
#' Default styling for kable extra
#'
#' @param x A kable object
#' @param full_width Defaults to TRUE.
#' @param ... Additional arguments passed to \link[kableExtra]{kable_styling}
kable_styling_defaults <- function(x, full_width = FALSE, ...) {
  kableExtra::kable_styling(x, position = "left", full_width = full_width, ...)
}

#' Strip rules
#'
#' Strip horizontal lines (also called rules) from
#' codebooks generated for latex
#'
#' @param x A character string
#' @export
kable_strip_rules <- function(x) {
  x <- gsub("\\toprule", "", x, fixed = TRUE)
  x <- gsub("\\bottomrule", "", x, fixed = TRUE)
  x <- gsub("\\midrule", "", x, fixed = TRUE)

  x
}

#' Strip toprule
#'
#' Strip horizontal lines (also called rules) from
#' codebooks generated for latex
#'
#' @param x A character string
#' @export
kable_strip_toprules <- function(x) {
  x <- gsub("\\toprule", "", x, fixed = TRUE)
  x
}

#' No breaks allowed
#'
#' This code wraps arbitrary tex inside of a block that
#' will not break until it is longer than a page
#'
#' @param tex A string including escaped tex
noBreaks <- function(tex) {
  if (length(tex) > 1)
    stop("Sorry, noBreaks only works on a string of length 1")

  paste0(
    "\\begin{absolutelynopagebreak}\n",
    tex, "",
    "\\end{absolutelynopagebreak}",
    collapse = "\n"
  )
}

#' Fix underscore
#'
#' We must escape underscores in aliases because latex treats them
#' like mathematical subtext
#'
#' @param x A string containing an underscore
fixUnderscore <- function(x) gsub("_", "\\_", x, fixed = TRUE)
