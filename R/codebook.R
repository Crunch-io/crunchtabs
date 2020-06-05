#' Generate a codebook item
#'
#' A passthrough function that creates a table header appropriate to the class
#' of the data object being passed. Expected classes are:
#'
#' * CategoricalVariable
#' * CategoricalArrayVariable
#' * MultipleResponseVariable
#' * NumericVariable
#' * TextVariable
#' * DatetimeVariable
#'
#' Importantly, this also controls the relative widths of the columns.
#'
#' @md
#' @param x An object of one of the types listed
#' @param ... Additional arguents passed to codebookItem methods
#' @export
codeBookItem <- function(x, ...) {
  UseMethod("codebookItem", x)
}

#' @rdname codebookItem
codeBookItem.default <- function(x) {
  wrong_class_error(x, c("CategoricalVariable", "CategoricalArrayVariable", "MultipleResponseVariable", "TextVariable", "NumericVariable", "DatetimeVariable"), "codebookItem")
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
#' @md
#' @importFrom glue glue
#' @export
codeBookItemTxt <- function(x) {
  txt <- list()
  txt$name <- crunch::name(x)
  txt$alias <- crunch::alias(x)
  txt$description <- crunch::description(x)
  txt$notes <- crunch::notes(x)


  if (txt$description == "") {
    warning(txt$alias, " is missing a description.")
  }

  alias_name <- glue(
    "\\begin{tabularx}{\\textwidth}{lXXr} ",
    "[<<<txt$alias>>>] &  &  & <<<txt$name>>>  \\\\ ",
    "\\end{tabularx}",
    .open = "<<<", .close = ">>>", .sep = "\n"
  )

  question <- glue(
    "",
    "\\begin{tabularx}{\\textwidth}{lXXr} ",
    "& <<<txt$description>>>  &  &  & \\\\ ",
    "\\end{tabularx}) "
    , .open = "<<<", .close = ">>>", .sep = "\n"
  )

  if (txt$notes != "") {
    notestxt <- glue(
      "\\begin{tabularx}{\\textwidth}{lllr} ",
       "& Notes &  <<<txt$notes>>> &  \\\\ ",
       "& & & & \\\\ ",
      "\\end{tabularx} "
      , .open = "<<<", .close = ">>>", .sep = "\n"
    )
  } else {
    notestxt <- ""
  }

  paste(
    alias_name,
    question,
    notestxt,
    sep = "\n\n"
  )
}

#' @describeIn codebookItem Prepares a codebookItem for a CategoricalVariable
#' @export
codeBookItem.CategoricalVariable <- function(x) {
  header <- codebookItemTxt(x)
  res <- codeBookSummary(x)

  top <- "\\begin{tabularx}{\\textwidth}{llcXrr} \n"
  bottom <- "\n\\end{tabularx}"

  l <- 1:nrow(res)
  for (i in 1:nrow(res)) {
    l[i] <- glue(
      '
      & <<<res$value[i]>>> & <<<res$name[i]>>> & \\dotfill & <<<res$n[i]>>> & \\\\
      ',
      .open = "<<<", .close = ">>>"
    )
  }

  paste0(
    header,
    top,
    paste0(
      l,
      collapse = "\n"
    ),
    bottom, collapse = "\n"
  )
}

#' @describeIn codebookItem Prepares a codebookitem for a CategoricalArrayVariable
#' @export
codeBookItem.CategoricalArrayVariable <- function(x) {
  header <- codebookItemTxt(x)
  r <- codeBookSummary(x)



  topalignment <- paste0(c("{",
    rep("l",2),
    "X",
    rep("c", ncol(r) - 2)), collapse = ""
  )
  top <- "\\begin{tabularx}{\\textwidth}{%s}"
  top <- sprintf(top, topalignment)
  bottom <- "\\end{tabularx}"


}

#' @describeIn codebookItem Prepares a codebookitem for a MultipleResponseVariable
#' @export
codeBookItem.MultipleResponseVariable <- function(x) codebookItem.CategoricalArrayVariable(x)

#' @describeIn codebookItem Prepares a codebookitem for a NumericVariable
#' @export
codeBookItem.NumericVariable <- function(x) {
  header <- codebookItemTxt(x)

  top <- "\\begin{tabularx}{\\textwidth}{llXr}"
  bottom <- "\\end{tabularx}"

  minima <- min(x, na.rm = T)
  maxima <- max(x, na.rm = T)

  missings <- sum(is.na(as.vector(x)))
  type_row <- glue("& Type & Numeric & \\\\")
  range_row <- glue("& Range & [<<<minima>>>, <<<maxima>>>] & \\\\",
                    .open = "<<<", .close = ">>>")
  if (missings > 0)
    missings_row <- glue("& Missing & <<<missings>>> & \\\\",
                         .open = "<<<", .close = ">>>")

  paste0(
    header,
    top,
    "\n",
    paste(
      type_row,
      if (missings > 0) missings_row,
      range_row, sep = "\n"),
    "\n",
    bottom,
    sep = "\n"
  )

}

#' @describeIn codebookItem Prepares a codebookitem for a TextVariable
#' @export
codeBookItem.TextVariable <- function(x) {
  header <- codebookItemTxt(x)
}

#' @describeIn codebookItem Prepares a codebookitem for a DatetimeVariable
#' @export
codeBookItem.DatetimeVariable <- codeBookItem.NumericVariable
