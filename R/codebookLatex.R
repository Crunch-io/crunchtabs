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
    tex = "\\vskip 0.10in\n\\addcontentsline{lot}{table}{%s}\n\\vskip 0.10in\n\\emph{%s}"
    tex = sprintf(
      tex,
      txt$description,
      txt$notes
    )
  } else {
    tex = "\\vskip 0.10in\n\\addcontentsline{lot}{table}{%s}"
    tex = sprintf(
      tex,
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
    num_splits = round(nrow(k) / 5, 0)
    splits = split(1:nrow(k), sort(rep_len(1:num_splits, nrow(k))))

    k = lapply(splits, function(x) k[x,c("Code", "Label")])
    k = lapply(k, function(x) { rownames(x) = NULL; return(x) })

    j = list()

    for (i in seq(1, length(k), 2)) {
      j[[as.character(i)]] = tryCatch({
        cbind(k[[i]], k[[i + 1]])
      },
      error = function(e) k[[i]])
    }

    alignment = c("c","l", "c", "l")

    knitr::kable(
      j, "latex", booktabs = TRUE, align = alignment) %>%
      kable_styling(full_width = TRUE)


  } else {
    alignment = c("c","l", "r")
    kableExtra::kable(
      k, "latex", booktabs = TRUE, longtable = TRUE, align = alignment) %>%
      kable_styling_defaults(...) %>%
      column_spec(c(1,3), width = "1in")
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

  space_remaining = 5.5 - col_one - sum(header_width)
  col_two <- 1.5


  k <- k %>% dplyr::mutate(
    Variable = cell_spec(Variable, "latex", monospace = TRUE)
  )

  ln = ncol(k) - 2

  kableExtra::kable(
    k,
    "latex",
    booktabs = TRUE,
    longtable = TRUE,
    align = alignment,
    escape = F) %>%
    # kable_styling_defaults(...) %>%
    column_spec(1, width = paste0(col_one, "in")) %>%
    column_spec(2, width = paste0(col_two, "in")) %>%
    # column_spec(c(3:ncol(k)), width = paste0(header_width[-1], "in")) %>%
    add_header_above(c("", "", "Codes" = ln))
}

#' @describeIn codeBookItemBody Creates item body for MultipleResponseVariable
#' @export
codeBookItemBody.MultipleResponseVariable <- codeBookItemBody.CategoricalArrayVariable

#' @describeIn codeBookItemBody Creates item body for DatetimeVariable
#' @export
codeBookItemBody.DatetimeVariable <- function(x, ...) {
  k = codeBookSummary(x)
  alignment <- c("c", "l")
  kableExtra::kable(k, "latex", booktabs = TRUE, longtable = TRUE, align = alignment) %>%
    kable_styling_defaults(...) %>%
    column_spec(1, width = "1in")
}

#' @describeIn codeBookItemBody Creates item body for NumericVariable
#' @export
codeBookItemBody.NumericVariable <- function(x, ...) {
  k = codeBookSummary(x)
  alignment <- c("c", "l")
  kableExtra::kable(k, "latex", booktabs = TRUE, longtable = TRUE, align = alignment) %>%
    kable_styling_defaults(...) %>%
    column_spec(1, width = "1in")
}

#' @describeIn codeBookItemBody Creates item body for TextVariable
#' @export
codeBookItemBody.TextVariable <- function(x, ...) {
  k = codeBookSummary(x)
  alignment <- c("c","l")
  kableExtra::kable(k, "latex", booktabs = TRUE, longtable = TRUE, align = alignment) %>%
    kable_styling_defaults(...) %>%
    column_spec(1, width = "1in")
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
    "\\begin{absolutelynopagebreak}",
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
