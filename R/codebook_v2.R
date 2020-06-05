# Txt Elements -----

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
bcodeBookItemTxtDescription <- function(x, ...) {
  txt <- list()
  txt$description <- crunch::description(x)
  question_align = c("l", "X", "X", "r")
  question <- c("", txt$description, "", "")

  k = matrix(question, ncol = 4)

  kableExtra::kable(k, "latex", booktabs = TRUE, align = question_align) %>%
    kable_styling_defaults(...)
}

bcodeBookItemTxtNotes <- function(x, ...) {
  txt <- list()
  txt$notes <- crunch::notes(x)
  notes_align <- c("l", "l", "l", "r")
  notetxt <- c("", "Notes", txt$notes, "")

  k = matrix(notetxt, ncol = 4)

  kableExtra::kable(k, "latex", booktabs = TRUE, align = notes_align) %>%
    kable_styling_defaults(...)
}

bcodeBookItemTxtHeader <- function(x, ...)  {
  txt <- list()
  txt$name <- crunch::name(x)
  txt$alias <- crunch::alias(x)
  heading_align <- "lXXr"
  heading_align <- c("l", "X", "X", "r")

  heading <- c(paste0("[", txt$alias, "]", collapse = ""), "", "", txt$name)
  k = matrix(heading, ncol = 4)

  kableExtra::kable(k, "latex", booktabs = TRUE, align = heading_align) %>%
    kable_styling_defaults(...)
}


# Item Header ----

bcodeBookItemHeader <- function(x, ...) {
  header_k <- bcodeBookItemTxtHeader(x, ...)
  description_k <- bcodeBookItemTxtDescription(x, ...)

  has_notes <- crunch::notes(x) != ""

  if (has_notes) {
    notes_k <- bcodeBookItemTxtNotes(x, ...)
  } else {
    notes_k = NULL
  }


  cat(
    header_k,
    description_k,
    notes_k
  )
}

# Item Body ----

#' @export
bcodeBookItemBody <- function(x, ...) {
  UseMethod("bcodeBookItemBody")
}

bcodeBookItemBody.default <- function(x, ...) {
  wrong_class_error(x, c("CategoricalVariable", "CategoricalArrayVariable", "MultipleResponseVariable", "TextVariable", "NumericVariable", "DatetimeVariable"), "codebookItem")
}
bcodeBookItemBody.CategoricalVariable <- function(x, ...) {
  k = codeBookSummary(x)
  alignment = c("l","l","c","X","r","r")
  kableExtra::kable(
    k, "latex", booktabs = TRUE,
    align = alignment
  ) %>%
    kable_styling_defaults(...)
}
bcodeBookItemBody.CategoricalArrayVariable <- function(x, ...) {
  k = codeBookSummary(x)
  alignment <- c(rep("l",2),"X",rep("c", ncol(k) - 2))
  kableExtra::kable(k, "latex", booktabs = TRUE, align = alignment) %>%
    kable_styling_defaults(...)
}
bcodeBookItemBody.MultipleResponseVariable <- function(x, ...) {
  k = codeBookSummary(x)
  alignment <- c(rep("l",2),"X",rep("c", ncol(k) - 2))
  kableExtra::kable(k, "latex", booktabs = TRUE, align = alignment) %>%
    kable_styling_defaults(...)
}
bcodeBookItemBody.DatetimeVariable <- function(x, ...) {
  k = codeBookSummary(x)
}
bcodeBookItemBody.NumericVariable <- function(x, ...) {
  k = codeBookSummary(x)
  kableExtra::kable(k, "latex", booktabs = TRUE, align = heading_align) %>%
    kable_styling_defaults(...)
}

#' Create a codebook
#'
#' @param ds A crunch dataset
#' @param ... Additional arguments. Unused.
#' @export
bwriteCodeBook <- function(x, ...) {

}

# utils ----

#' Defaults for kableExtra
#'
#' Default styling for kable extra
#'
#' @param x A kable object
#' @param ... Additional arguments passed to \link[kableExtra]{kable_styling}
kable_styling_defaults <- function(x, ...) {
  kableExtra::kable_styling(x, full_width = TRUE, ...)
}

#' Strip rules
#'
#' Strip horizontal lines (also called rules) from
#' codebooks generated for latex
#'
#' @param x A character string
#' @export
kable_strip_rules <- function(x) {
  x <- gsub("\\toprule", "", x)
  x <- gsub("\\bottomrule", "", x)
  x
}


bcodeBookTemplate <- function(x) {

  txt = '```{r}
  bcodeBookItemTxtHeader(%s)
  bcodeBookItemTxTDecsription(%s)
  bcodeBookItemTxtNotes(%s)
  bcodeBookItemBody(%s)
  ```'

  sprintf(trimws(txt), rep(x, 4))
}
