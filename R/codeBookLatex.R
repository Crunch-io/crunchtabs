# Item Txt Elements -----

#' codeBook Item Text Header
#'
#' Creates a text header for a codebook item
#'
#' @inheritParams codeBookItemTxtDescription
#' @export
codeBookItemTxtHeader <- function(x, ...) {
  txt <- list()
  txt$name <- crunch::name(x)
  txt$alias <- crunch::alias(x)

  tex <- "\\textbf{%s}\\hfill\\textbf{\\ttfamily{%s}}\n\n{\\small %s}\n\n"

  lookup <- data.frame(
    type = c(
      "CategoricalVariable",
      "CategoricalArrayVariable",
      "TextVariable",
      "NumericVariable",
      "DatetimeVariable",
      "DateVariable",
      "MultipleResponseVariable",
      "factor",
      "numeric",
      "integer"
    ),
    softType = c(
      "Categorical",
      "Grid",
      "Text",
      "Numeric",
      "Date",
      "Date",
      "Multiple Response"
    )
  )

  softType <- lookup[lookup$type == class(x), ]$softType

  sprintf(
    tex,
    texEscape(txt$name),
    texEscape(txt$alias),
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
  txt$alias <- crunch::alias(x)
  txt$alias_toc <- ifelse(
    nchar(txt$alias) > 20,
    paste0(substr(txt$alias, 1, 22), "..."),
    txt$alias
  )

  txt$name <- crunch::name(x)

  txt$name_toc <- ifelse(
    nchar(txt$name) > 65,
    paste0(substr(txt$name, 1, 65), "..."),
    txt$name
  )


  if (txt$notes != "") {
    tex <- "\\vskip 0.10in\n%s\n\\addcontentsline{lot}{table}{\\parbox{1.8in}{\\ttfamily{%s}} %s}\n\\vskip 0.10in\n\\emph{%s}\n\\vskip 0.10in" # nolint
    tex <- sprintf(
      tex,
      texEscape(txt$description),
      texEscape(txt$alias_toc),
      texEscape(txt$name_toc),
      txt$notes
    )
  } else {
    tex <- "\\vskip 0.10in\n%s\n\\addcontentsline{lot}{table}{\\parbox{1.8in}{\\ttfamily{%s}} %s}\n\\vskip 0.10in" # nolint
    tex <- sprintf(
      tex,
      texEscape(txt$description),
      texEscape(txt$alias_toc),
      texEscape(txt$name_toc)
    )
  }

  tex
}


# utils ----

#' Defaults for kableExtra
#'
#' Default styling for kable extra
#'
#' @param x A kable object
#' @param full_width Defaults to TRUE.
#' @param position The position of the table. Defaults to "left".
#' @param ... Additional arguments passed to \link[kableExtra]{kable_styling}
kable_styling_defaults <- function(x, full_width = FALSE, position = "left", ...) {
  kableExtra::kable_styling(x, position = position, full_width = full_width, ...)
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
  if (length(tex) > 1) {
    stop("Sorry, noBreaks only works on a string of length 1")
  }

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

#' Fix ttf
#'
#' We must escape underscores in aliases because latex treats them
#' like mathematical subtext
#'
#' @param x A string containing an underscore
monospaced <- function(x) paste0("\\ttfamily{", x, "}")

#' Header wrap
#'
#' Wrap a character vector in curly braces
#'
#' @param x A character vector
curlyWrap <- function(x) paste0("{", x, "}")

#' scolumn_fix
#'
#' Given a data.frame and an alignment vector
#' create dynamic S-Columns based on character
#' width
#'
#' @param k A data.frame to be printed using \link[kableExtra]{kable}
#' @param alignment A string vector of alignments
scolumnAlign <- function(k, alignment) {
  for (i in seq_len(ncol(k))) {
    if (alignment[i] == "d") {
      # If entire column is NA, set to two
      if (all(is.na(k[[i]]))) {
        maxnchar <- 2
      } else {
        maxnchar <- max(nchar(k[[i]]), na.rm = TRUE)
      }

      if (maxnchar > 6) {
        alignment[i] <- sprintf("S[table-format=%s]", maxnchar)
      } else {
        alignment[i] <- c("J", "K", "d", "M", "N", "O")[maxnchar]
      }
    }
  }
  alignment
}
