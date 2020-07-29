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

  tex <- "\\textbf{%s}\\hfill\\textbf{\\ttfamily{%s}}\n\n{\\small %s}\n\n"

  type <- class(x)

  lookup = data.frame(
    type = c(
      "CategoricalVariable",
      "CategoricalArrayVariable",
      "TextVariable",
      "NumericVariable",
      "DatetimeVariable",
      "DateVariable",
      "MultipleResponseVariable"
    ),
    softType = c(
      "Categorical",
      "Grid",
      "Verbatim",
      "Numeric",
      "Date",
      "Date",
      "Multiple Response"
    )
  )

  softType <- lookup[lookup$type == class(x),]$softType

  sprintf(
    tex,
    fixUnderscore(txt$name),
    fixUnderscore(txt$alias),
    softType
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
  k$name <- texEscape(k$name)

  if (nrow(k) > 1) {
    k = k[order(as.numeric(k[,1])),] %>% as.data.frame(stringsAsFactors = F)
  } else {
    k = as.data.frame(k, stringsAsFactors = F)

  }

  rownames(k) <- NULL
  names(k) = curlyWrap(c("Code", "Label", "Count"))

  if (nrow(k) > 20) {
    # If we have more than 20 hide counts only show codes. Use multiple tables
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
    names(k) = curlyWrap(rep(c("Code", "Label", "Count"),2))

    k[is.na(k)] = " "


    alignment = c("d", "l", "d")
    names(k) <- curlyWrap(names(k))
    kableExtra::kable(
      k, "latex", booktabs = TRUE, align = alignment, longtable = TRUE, linesep = "", escape = FALSE) %>%
      kableExtra::kable_styling(full_width = TRUE) %>%
      kableExtra::column_spec(c(2,5), width = "1.75in", latex_column_spec = NULL) %>%
      kableExtra::column_spec(3, border_right = TRUE)

  } else {

    names(k) <- curlyWrap(names(k))
    alignment = c("d", "l", "d")
    kableExtra::kable(
      k, "latex", booktabs = TRUE, longtable = TRUE,  align = alignment,
      linesep = "", escape = FALSE) %>%
      kable_styling_defaults(...) # %>%
      # kableExtra::column_spec(3, latex_column_spec = NULL)
  }

}

#' @describeIn codeBookItemBody Creates item body for CategoricalArrayVariable
#' @export
codeBookItemBody.CategoricalArrayVariable <- function(x, ...) {
  k <- codeBookSummary(x)
  k[,1] <- texEscape(k[,1])
  var_labels <- texEscape(k[,2])
  code_labels <- texEscape(gsub("[0-9]+ ", "", names(k))[-c(1,2)])
  code_numbers <- trimws(gsub("[a-zA-Z]+", "", names(k))[-c(1,2)])
  rownames(k) <- NULL


  tab_list <- list()
  # Rows: variable, label

  krows <- data.frame(
    Variable = k[,1],
    Label = k[,2]
  )

  krows <- kableExtra::kable(
    krows,
    "latex",
    booktabs = TRUE,
    longtable = TRUE,
    align = "ll",
    escape = F, linesep = " ") %>%
    kableExtra::add_header_above(c("Rows" = 2))

  # Columns: Code, Label

  kcols <- data.frame(
    Code = code_numbers,
    Label = code_labels
  )

  names(kcols) <- curlyWrap(names(kcols))

  kcols <- kableExtra::kable(
    kcols,
    "latex",
    booktabs = TRUE,
    longtable = TRUE,
    align = "dl",
    escape = FALSE, linesep = " ") %>%
    kableExtra::add_header_above(c("Columns" = 2))


  # Counts: Variable, 1,2,3,4,5, Missing

  kcounts <- as.data.frame(
    k[,-2]
  )


  names(kcounts) <- curlyWrap(c("Variable", code_numbers))

  kcounts <- kableExtra::kable(
    kcounts,
    "latex",
    booktabs = TRUE,
    longtable = TRUE,
    align = c("l", rep("d", ncol(kcounts) - 1)),
    escape = F, linesep = " ") %>%
    kableExtra::add_header_above(c(" ","Counts" = ncol(kcounts) - 1))

  # tab_list

  tab_list$krows <- krows
  tab_list$kcols <- kcols
  tab_list$kcounts <- kcounts

  tab_list

}

#' @describeIn codeBookItemBody Creates item body for MultipleResponseVariable
#' @export
codeBookItemBody.MultipleResponseVariable <- codeBookItemBody.CategoricalArrayVariable

#' @describeIn codeBookItemBody Creates item body for DatetimeVariable
#' @export
codeBookItemBody.DatetimeVariable <- function(x, ...) {
  k = codeBookSummary(x)
  alignment <- c("c", "c", "c")
  kableExtra::kable(k, "latex", booktabs = TRUE, longtable = TRUE, align = alignment, linesep = "") %>%
    kable_styling_defaults(...) #%>%
    # kableExtra::column_spec(1, width = "1in")
}

#' @describeIn codeBookItemBody Creates item body for NumericVariable
#' @export
codeBookItemBody.NumericVariable <- function(x, ...) {
  k <- codeBookSummary(x)
  names(k) <- curlyWrap(names(k))
  alignment <- c("d")
  kableExtra::kable(k, "latex", booktabs = TRUE, longtable = TRUE,
                    align = alignment, linesep = "", escape = FALSE) %>%
    kable_styling_defaults(...) #%>%
    #kableExtra::column_spec(1, width = "1in")
}

#' @describeIn codeBookItemBody Creates item body for TextVariable
#' @export
codeBookItemBody.TextVariable <- function(x, ...) {
  k <- codeBookSummary(x)
  names(k) <- curlyWrap(names(k))
  alignment <- c("d")

  kableExtra::kable(k, "latex", booktabs = TRUE, longtable = TRUE,
                    align = alignment, linesep = "", escape = FALSE) %>%
    kable_styling_defaults(...) # %>%
    # kableExtra::column_spec(1:2, width = "0.5in") %>%
    # kableExtra::column_spec(3, width = "5.5in")
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

#' Header wrap
#'
#' Wrap a character vector in curly braces
#'
#' @param x
curlyWrap <- function(...) paste0("{", ..., "}")
