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
    # May change in the future
    txt$description <- ""
  }

  alignment <- c("l")
  question <- c(txt$description)
  notes_txt <- c(txt$notes)

  if (notes_txt == "") {
    k <- matrix(c(question), ncol = 1) %>%
      as.data.frame(stringsAsFactors = F)
    names(k) <- ""
  } else {
    k <- matrix(c(question, notes_txt), ncol = 1)
    names(k) <- ""
  }


  if (all(unlist(txt) == "")) {
    kableExtra::kable(k, "latex", booktabs = TRUE, align = alignment)
  } else {
    kableExtra::kable(k, "latex", booktabs = TRUE, align = alignment) %>%
      kable_styling_defaults(full_width = T, ...)
  }

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

  heading <- c(txt$name, txt$alias)
  k <- matrix(heading, ncol = 2) %>% as.data.frame(stringsAsFactors = FALSE)
  names(k) <- c("name", "alias")

  k <- k %>% dplyr::mutate(
    alias = cell_spec(alias,"latex", monospace = TRUE)
  )

  names(k) = NULL

  kableExtra::kable(k, "latex", booktabs = TRUE, align = alignment, escape = F) %>%
    kable_styling_defaults(full_width = T, ...) %>%
    row_spec(1, hline_after = F)

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
    align = alignment,
    escape = F) %>%
    kable_styling_defaults(...) %>%
    column_spec(1, width = paste0(col_one, "in")) %>%
    column_spec(2, width = paste0(col_two, "in")) %>%
    column_spec(c(3:ncol(k)), width = paste0(header_width, "in")) %>%
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
  kableExtra::kable(k, "latex", booktabs = TRUE, align = alignment) %>%
    kable_styling_defaults(...) %>%
    column_spec(1, width = "1in")
}

#' @describeIn codeBookItemBody Creates item body for NumericVariable
#' @export
codeBookItemBody.NumericVariable <- function(x, ...) {
  k = codeBookSummary(x)
  alignment <- c("c", "l")
  kableExtra::kable(k, "latex", booktabs = TRUE, align = alignment) %>%
    kable_styling_defaults(...) %>%
    column_spec(1, width = "1in")
}

#' @describeIn codeBookItemBody Creates item body for TextVariable
#' @export
codeBookItemBody.TextVariable <- function(x, ...) {
  k = codeBookSummary(x)
  alignment <- c("c","l")
  kableExtra::kable(k, "latex", booktabs = TRUE, align = alignment) %>%
    kable_styling_defaults(...) %>%
    column_spec(1, width = "1in")
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

    if (codeBookSummary(ds[[nm]]) %>% nrow() > 20) {
      kables[[nm]] <- sprintf(trimws(
      "```{r}
      codeBookItemTxtHeader(ds[['%s']]) ++ kable_strip_rules
      codeBookItemTxtDescription(ds[['%s']]) ++ kable_strip_rules
      ````\n\n
      ```{r, results = 'asis'}
      codeBookItemBody(ds[['%s']])
      ```\n\n
      \\bigbreak
      \\bigbreak
      "),
        nm, nm, nm
      )
    } else {
      kables[[nm]] <- sprintf(trimws(
        "```{r}
      codeBookItemTxtHeader(ds[['%s']]) ++ kable_strip_rules
      codeBookItemTxtDescription(ds[['%s']]) ++ kable_strip_rules
      codeBookItemBody(ds[['%s']])
      ```\n\n
      \\bigbreak
      \\bigbreak
      "),
        nm, nm, nm
      )

    }

    # manual rule example
    # \\begin{center}\\rule{\\linewidth}{0.5pt}\\end{center}

    kables[[nm]] <- gsub("      ", "", kables[[nm]])
    kables[[nm]] <- gsub("++", "%>%", kables[[nm]], fixed = TRUE)
  }

  write(
    c(
      preamble,
      dataset,
      paste0(kables, collapse = "\n\n")
    ), file = gsub(" ", "-", paste0(name(ds), ".Rmd"), fixed = T)
  )
  if (pdf) {
    rmarkdown::render(gsub(" ", "-", paste0(name(ds), ".Rmd"), fixed = T))
    file.open(gsub(" ", "-", paste0(name(ds), ".pdf"), fixed = T))
  }

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
  kableExtra::kable_styling(x, full_width = full_width, ...)
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
