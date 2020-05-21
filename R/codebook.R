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
codebookItem <- function(x, ...) {
  UseMethod("codebookItem", x)
}

#' @rdname codebookItem
codebookItem.default <- function(x) {
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
codebookItemTxt <- function(x) {
  txt <- x@tuple@body

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
    notes <- glue(
      "\\begin{tabularx}{\\textwidth}{lllr} ",
       "& Notes &  <<<txt$notes>>> &  \\\\ ",
       "& & & & \\\\ ",
      "\\end{tabularx} "
      , .open = "<<<", .close = ">>>", .sep = "\n"
    )
  } else {
    notes <- ""
  }

  paste(
    alias_name,
    question,
    notes,
    sep = "\n\n"
  )
}

#'
codebookItemSubVars <- function(x) {
  sv <- subvariables(x)
  als <- unname(unlist(lapply(sv@index, getElement, "alias")))
  resp <- unname(unlist(lapply(sv@index, getElement, "name")))
  sv <- data.frame(`Sub Alias` = als, Name = resp)
  sv_responses <- categories(x)

  list(
    key = sv,
    key2 = setNames(sv_responses, c("Response", "Value"))
  )

}



#' @describeIn codebookItem Prepares a codebookItem for a CategoricalVariable
#' @export
codebookItem.CategoricalVariable <- function(x) {
  header <- codebookItemTxt(x)
  res <- as.data.frame(x)

  top = "\\begin{tabularx}{\\textwidth}{llcXrr} \n"
  bottom = "\n\\end{tabularx}"

  l = 1:nrow(res)
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
codebookItem.CategoricalArrayVariable <- function(x) {
  header = codebookItemTxt(x)
  subvars = codebookItemSubVars(x)
}

#' @describeIn codebookItem Prepares a codebookitem for a MultipleResponseVariable
#' @export
codebookItem.MultipleResponseVariable <- function(x) {
  header = codebookItemTxt(x)

}

#' @describeIn codebookItem Prepares a codebookitem for a NumericVariable
#' @export
codebookItem.NumericVariable <- function(x) {
  header = codebookItemTxt(x)

  top = "\\begin{tabularx}{\\textwidth}{llXr}"
  bottom = "\\end{tabularx}"

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
codebookItem.TextVariable <- function(x) {
  header = codebookItemTxt(x)
}

#' @describeIn codebookItem Prepares a codebookitem for a DatetimeVariable
#' @export
codebookItem.DatetimeVariable <- codebookItem.NumericVariable

#' Generate LaTeX CodeBooks
#'
#' \code{writeCodebook} produces publication-quality LaTeX reports
#'
#' @param ds A crunch dataset
#'
#' @param ... Additional arguments passed to writeLatx
#'
#' @importFrom utils installed.packages
#' @export
writeCodebook <- function(...) {
  writeLatex(...)
}

#' Categorical Variable to data.frame
#'
#' Manipulate categorical into a results object without hitting the tabBook
#' endpoint. For the purpose of creating a codebookItem.
#'
#' @param x A CategoricalVariable from a crunch \link[crunch]{loadDataset}
#' @param ... Ignored
as.data.frame.CategoricalVariable <- function(x, ...) {
  cats <- crunch::categories(x)
  responses <- do.call(rbind, cats@.Data)
  l <- list()
  for (i in 1:nrow(responses)) {

    if (is.null(responses[i, ]$numeric_value)) {
      responses[i,]$numeric_value <- NA_integer_
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

  # MultipleResponseVariable
  if (ncol(l) == 5) {
    names(l) <- c("id", "missing", "value", "a", "b")

  }

  for (i in 1:ncol(l)) { l[[i]] <- type.convert(l[[i]], as.is = T) }

  s <- data.frame(crunch::table(x, useNA = "ifany"))
  names(s) <- c("name", "n")

  res <- merge(l, s)
  res[with(res, order(value)),]
}

as.data.frame.MultipleResponseVariable <- function(x, ...) {
  responses = lapply(x, as.data.frame.CategoricalVariable)
}
