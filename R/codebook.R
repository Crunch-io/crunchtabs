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
#' @param ... Additional arguments passed to \link{kable_styling_defaults}
#' @md
#' @export
codeBookItemTxtDescription <- function(x, ...) {
  txt <- list()
  txt$description <- crunch::description(x)
  txt$notes <- crunch::notes(x)

  if (txt$description == "") {
    txt$description <- "No question text"
  }

  question_align = c("l")
  question <- c(txt$description)
  notes_txt <- c(txt$notes)

  k = matrix(c(question, notes_txt), ncol = 1)

  kableExtra::kable(k, "latex", booktabs = TRUE, align = question_align) %>%
    kable_styling_defaults(...)
}

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
  alignment <- c("l", "r")

  heading <- c(paste0("[", txt$alias, "]", collapse = ""), txt$name)
  k = matrix(heading, ncol = 2)

  kableExtra::kable(k, "latex", booktabs = TRUE, align = alignment) %>%
    kable_styling_defaults(...)
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
#' @inheritParams codeBookItemBody
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
#' @inheritParams codeBookItemBody
#' @export
codeBookItemBody.CategoricalVariable <- function(x, ...) {
  k = codeBookSummary(x)
  k = k[order(as.numeric(k[,1])),]
  alignment = c("c","l", "r")
  kableExtra::kable(
    k, "latex", booktabs = TRUE, longtable = TRUE, align = alignment) %>%
    kable_styling_defaults(...) %>%
    column_spec(c(1,3), width = "1in")
}

#' @describeIn codeBookItemBody Creates item body for CategoricalArrayVariable
#' @inheritParams codeBookItemBody
#' @export
codeBookItemBody.CategoricalArrayVariable <- function(x, ...) {
  k = codeBookSummary(x)
  alignment <- c(rep("l",2),rep("c", ncol(k) - 2))
  col_one <- round(max(nchar(k[,1]))*0.07, 2)

  space_remaining = 5.5 - col_one - ((ncol(k) - 2)*0.60)
  col_two <- space_remaining

  kableExtra::kable(k, "latex", booktabs = TRUE, align = alignment) %>%
    kable_styling_defaults(...) %>%
    column_spec(1, width = paste0(col_one, "in")) %>%
    column_spec(2, width = paste0(col_two, "in")) %>%
    column_spec(c(3:ncol(k)), width = "0.60in")
}

#' @describeIn codeBookItemBody Creates item body for MultipleResponseVariable
#' @inheritParams codeBookItemBody
#' @export
codeBookItemBody.MultipleResponseVariable <- codeBookItemBody.CategoricalArrayVariable

#' @describeIn codeBookItemBody Creates item body for DatetimeVariable
#' @inheritParams codeBookItemBody
#' @export
codeBookItemBody.DatetimeVariable <- function(x, ...) {
  k = codeBookSummary(x)
  alignment <- c("l", "l")
  kableExtra::kable(k, "latex", booktabs = TRUE, align = alignment) %>%
    kable_styling_defaults(...) %>%
    column_spec(1, width = "10em")
}

#' @describeIn codeBookItemBody Creates item body for NumericVariable
#' @inheritParams codeBookItemBody
#' @export
codeBookItemBody.NumericVariable <- function(x, ...) {
  k = codeBookSummary(x)
  alignment <- c("l", "l")
  kableExtra::kable(k, "latex", booktabs = TRUE, align = alignment) %>%
    kable_styling_defaults(...) %>%
    column_spec(1, width = "10em")
}

#' @describeIn codeBookItemBody Creates item body for TextVariable
#' @inheritParams codeBookItemBody
#' @export
codeBookItemBody.TextVariable <- function(x, ...) {
  k = codeBookSummary(x)
  alignment <- c("l","l")
  kableExtra::kable(k, "latex", booktabs = TRUE, align = alignment) %>%
    kable_styling_defaults(...) %>%
    column_spec(1, width = "10em")
}

#' Create a codebook
#'
#' @param ds A crunch dataset
#' @param url A crunch dataset url
#' @param rmd Should we create an interim Rmd file? Defaults to TRUE
#' @param pdf Should we write directly to pdf? Defaults to TRUE
#' @param ... Additional arguments. Unused.
#' @export
writeCodeBook <- function(ds, url = NULL, rmd = TRUE, pdf = TRUE, ...) {

  preamble <- readLines(
    system.file(
      "codebook_header.Rmd",
      package = "crunchtabs"
      )
  )

  # Some datasets are not visible using their name
  if (is.null(url)) {
    dataset = sprintf(
      '```{r} \nds = loadDataset("%s") \n```\n\n',
      name(ds)
    )
  } else {
    dataset = sprintf(
      '```{r} \nds = loadDataset("%s") \n```\n\n',
      url
    )
  }


  kables <- list()
  nms <- names(ds)
  for (nm in nms) {
    kables[[nm]] <- sprintf(trimws(
      "```{r}
      codeBookItemTxtHeader(ds[['%s']]) ++ kable_strip_rules
      codeBookItemTxtDescription(ds[['%s']]) ++ kable_strip_rules
      codeBookItemBody(ds[['%s']]) ++ kable_strip_rules
      ```\n\n
      \\begin{center}\\rule{\\linewidth}{0.5pt}\\end{center}"),
      nm, nm, nm
    )

    kables[[nm]] <- gsub("      ", "", kables[[nm]])
    kables[[nm]] <- gsub("++", "%>%", kables[[nm]], fixed = TRUE)
  }

  write(
    c(
      preamble,
      dataset,
      paste0(kables, collapse = "\n\n")
    ), file = paste0(name(ds), ".Rmd")
  )
  if (pdf) {
    rmarkdown::render(paste0(name(ds), ".Rmd"))
    file.open(paste0(name(ds), ".pdf"))
  }

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
  x <- gsub("\\toprule", "", x, fixed = TRUE)
  x <- gsub("\\bottomrule", "", x, fixed = TRUE)
  x <- gsub("\\midrule", "", x, fixed = TRUE)

  x
}
