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
      "Text",
      "Numeric",
      "Date",
      "Date",
      "Multiple Response"
    )
  )

  softType <- lookup[lookup$type == class(x),]$softType

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
  txt$name <- crunch::name(x)

  if (txt$notes != "") {
    tex = "\\vskip 0.10in\n%s\n\\addcontentsline{lot}{table}{\\parbox{1.8in}{\\ttfamily{%s}} %s}\n\\vskip 0.10in\n\\emph{%s}\n\\vskip 0.10in"
    tex = sprintf(
      tex,
      txt$description,
      texEscape(txt$alias),
      texEscape(txt$name),
      txt$notes
    )
  } else {
    tex = "\\vskip 0.10in\n%s\n\\addcontentsline{lot}{table}{\\parbox{1.8in}{\\ttfamily{%s}} %s}\n\\vskip 0.10in"
    tex = sprintf(
      tex,
      txt$description,
      texEscape(txt$alias),
      texEscape(txt$name)
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
  k <- codeBookSummary(x)
  k$name <- texEscape(k$name)

  if (nrow(k) > 1) {
    k = k[order(as.numeric(k[,1])),] %>% as.data.frame(stringsAsFactors = F)
  } else {
    k = as.data.frame(k, stringsAsFactors = F)

  }

  rownames(k) <- NULL
  names(k) <- c("Code", "Label", "Count")

  if (nrow(k) > 20) {
    # If we have more than 20 hide counts only show codes. Use multiple tables
    # row-wise

    une_duex_trois <- suppressWarnings(matrix(1:nrow(k), nrow = 2))
    une_duex_trois[which(duplicated(as.vector(une_duex_trois)))] <- NA
    une_duex_trois <- t(une_duex_trois)
    k = cbind(
      k[une_duex_trois[,1],],
      "",
      k[une_duex_trois[,2],],
      stringsAsFactors = FALSE
    )

    rownames(k) <- NULL
    names(k) <- curlyWrap(c("Code", "Label", "Count", "","Code", "Label", "Count"))

    k[is.na(k)] <- " "

    alignment <- c("d", "l", "d", "c", "d", "l", "d")
    names(k) <- curlyWrap(names(k))
    kableExtra::kable(
      k, "latex", booktabs = TRUE, align = scolumnAlign(k, alignment),
      longtable = TRUE, linesep = "", escape = FALSE) %>%
      kable_styling_defaults(full_width = TRUE, ...) %>%
      kableExtra::column_spec(c(2,6), width = "1.75in", latex_column_spec = NULL) %>%
      kableExtra::row_spec(0, extra_latex_after = "\\cmidrule(l){1-3}\\cmidrule(l){5-7}") %>%
      { gsub("\\midrule", "", ., fixed = TRUE)}
      # kableExtra::column_spec(3, border_right = TRUE) %>%

  } else {

    alignment <- c("d", "l", "d")
    names(k) <- curlyWrap(names(k))
    kab <- kableExtra::kable(
      k, "latex", booktabs = TRUE, longtable = TRUE,  align = scolumnAlign(k, alignment),
      linesep = "", escape = FALSE)


    if (max(nchar(k$`{Label}`)) > 80) {
      kab %>% kableExtra::column_spec(2, width = "5.25in") %>%
        kable_styling_defaults(...)
    } else {
      kab %>%
        kable_styling_defaults(...)
    }

  }

}

#' @describeIn codeBookItemBody Creates item body for CategoricalArrayVariable
#' @export
 codeBookItemBody.CategoricalArrayVariable <- function(x, ...) {
  k <- codeBookSummary(x)
  k[,1] <- texEscape(k[,1])
  k[,2] <- texEscape(k[,2])
  var_labels <- k[,2]
  code_labels <- texEscape(gsub("[0-9]+ ", "", names(k))[-c(1,2)])
  code_numbers <- trimws(sub("\\D*(\\d+).*", "\\1", names(k)))[-c(1,2)]
    # gsub("[a-zA-Z]+", "", names(k))[-c(1,2)])
  rownames(k) <- NULL


  tab_list <- list()
  # Rows: variable, label

  krows <- data.frame(
    Variable = k[,1] %>% monospaced,
    Label = k[,2],
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
      escape = F, linesep = " ") %>%
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
      escape = F, linesep = " ") %>%
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
    align = scolumnAlign(kcols, c("d","l")),
    escape = FALSE, linesep = " ") %>%
    kable_styling_defaults(...) %>%
    kableExtra::add_header_above(c("Columns" = 2))


  # Counts: Variable, 1,2,3,4,5, etc, Missing

  kcounts <- as.data.frame(
    k[,-2],
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
    escape = F, linesep = " ") %>%
    kable_styling_defaults(...) %>%
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
  if (k$Mean > 9999) {
    k$Mean <- format(k$Mean, scientific = TRUE, digits = 3)
    k$SD <- format(k$SD, scientific = TRUE, digits = 3)
    k$Min <- format(k$Min, scientific = TRUE, digits = 3)
    k$Max <- format(k$Max, scientific = TRUE, digits = 3)
  }
  names(k) <- curlyWrap(names(k))
  alignment <- c("c") # No S/d columns here because of sci
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
#' @param position The position of the table. Defaults to "left".
#' @param ... Additional arguments passed to \link[kableExtra]{kable_styling}
kable_styling_defaults <- function(x, full_width = FALSE, position="left", ...) {
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
  nchars <- unlist(lapply(k, function(x) max(nchar(x), na.rm = TRUE)))

  for (i in 1:ncol(k)) {
    if (alignment[i] == "d") {
      maxnchar <- max(nchar(k[[i]]), na.rm = TRUE)
      if (maxnchar > 6) {
        alignment[i] <- sprintf("S[table-format=%s]", maxnchar)
      } else {
        alignment[i] <- c("J", "K", "d", "M", "N", "O")[maxnchar]
        }
    }
  }
  alignment
}
