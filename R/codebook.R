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
#' * DateTimeVariable
#'
#' Importantly, this also controls the relative widths of the columns.
#'
#' @md
#' @param x An object of one of the types listed
#' @export
codebookItem <- function(x, ...) {
  UseMethod("codebookItem", x)
}

#' @rdname codebookItem
codebookItem.default <- function(x) {
  wrong_class_error(x, c("CategoricalVariable", "CategoricalArrayVariable", "MultipleResponseVariable", "TextVariable", "NumericVariable", "DateTimeVariable"), "codebookItem")
}


#' Extract basic question information
#'
#' Extracts the following:
#'
#' * body
#' * alias
#' * description or question text
#' * notes or filter text
#' * id
#'
#' @md
#' @export
codebookItemTxt <- function(x) {
  txt <- x@tuple@body
  l <- list()
  l$title <- txt$alias
  l$alias <- txt$alias
  l$name <- txt$name
  l$id <- txt$id
  l$filter_text <- txt$notes
  l$question <- txt$description
  l
}

#'
codebookItemSubVars <- function (x) {
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
  txt <- codebookItemTxt(x)
  cats <- categories(x)
  responses <- do.call(rbind, cats@.Data)
  responses <- as.data.frame(responses[!unlist(responses[, "missing"]), ])
  responses <- lapply(responses, unlist)
  if (all(is.null(responses$numeric_value)))
    responses$numeric_value = responses$id
  responses <- as.data.frame(lapply(responses, unlist)[c("name", "numeric_value")])
  names(responses) <- c("Response", "Value")

  latexTop = sprintf("
    \\setlength{\\extrarowheight}{20pt}
    \\begin{tabular*}{7in}{p{2.5in}p{4.5in}}
    Question  & %s \\\\
    Name  & %s \\\\
    Alias  & %s \\\\
    ID  & %s \\\\
    Filtering Notes  & %s \\\\
    \\end{tabular*}
    ",
    txt$meta$question,
    txt$meta$name,
    txt$meta$alias,
    txt$meta$id,
    ifelse(txt$meta$filter_text == "", "None", txt$meta$filter_text)
    ) %>%
  { gsub("    ", "", .) }

}

#' @describeIn codebookItem Prepares a codebookitem for a CategoricalArrayVariable
#' @export
codebookItem.CategoricalArrayVariable <- function(x) {
  txt = codebookItemTxt(x)
  subvars = codebookItemSubVars(x)


}

#' @describeIn codebookItem Prepares a codebookitem for a MultipleResponseVariable
#' @export
codebookItem.MultipleResponseVariable <- function(x) {

}

#' @describeIn codebookItem Prepares a codebookitem for a NumericVariable
#' @export
codebookItem.NumericVariable <- function(x) {
  tmp <- c(summary(c_var), SD = sd(c_var, na.rm = TRUE))
  tmp <- tibble::rownames_to_column(as.data.frame(tmp))
  colnames(tmp) <- c("Summary", "Value")
  tmp$Value <- round(tmp$Value, 2)
}

#' @describeIn codebookItem Prepares a codebookitem for a TextVariable
#' @export
codebookItem.TextVariable <- function(x) {

}

#' @describeIn codebookItem Prepares a codebookitem for a DateTimeVariable
#' @export
codebookItem.DateTimeVariable <- function(x) {

}


