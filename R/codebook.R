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

  latexTop <- sprintf("
    \\thispagestyle{fancy}
    \\lhead{%s}
    \\setlength{\\extrarowheight}{20pt}
    \\begin{tabular*}{7in}{p{2.5in}p{4.5in}}
    Question  & %s \\\\
    Name  & %s \\\\
    Alias  & %s \\\\
    ID  & %s \\\\
    Filtering Notes  & %s \\\\
    \\end{tabular*}
    ",
    txt$meta$name,
    txt$meta$question,
    txt$meta$name,
    txt$meta$alias,
    txt$meta$id,
    ifelse(txt$meta$filter_text == "", "None", txt$meta$filter_text)
    )
  latexTop <- gsub("    ", "", latexTop)

  latexResponse

  latexTop
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

#' Generate LaTeX CodeBooks
#'
#' \code{writeCodebook} produces publication-quality LaTeX reports
#'
#' @param ds An object of class \code{CrunchDataset}.
#' @param theme A theme object (default: `themeDefaultLatex`).
#' @param filename character. The name of the output file (without extension).
#' @param title An optional title. Defaults to the data summary title.
#' @param subtitle An optional character subtitle. Defaults to an empty string.
#' @param pdf logical. Compile LaTeX using pdflatex? Implemented only on MacOS/Linux.
#' @param open logical. If PDF document was produced, open it with
#' the default application? Only implemented for MacOS.
#' @param proportions logical. If \code{TRUE} the output report shows proportions,
#' if \code{FALSE} the report shows counts.
#' @param table_of_contents logical. Should a list of tables be included at the start
#' of the report Defaults to \code{FALSE}.
#' @param sample_desc A character string describing the sample.
#' @param field_period A character string describing the field period.
#' @param moe An optional numeric margin of error.
#' @param append_text An optional character string that, if supplied, will be appended after
#' the final table. Useful for adding in disclosure information. Defaults to an empty string.
#' @param logging add log messages
#'
#' @importFrom utils installed.packages
#' @export
writeCodebook <- function(ds, theme = themeDefaultLatex(),
                       filename = getName(data_summary), title = getName(data_summary),
                       subtitle = NULL, table_of_contents = FALSE, sample_desc = NULL,
                       field_period = NULL, moe = NULL, append_text = NULL, proportions = FALSE,
                       pdf = FALSE, open = FALSE, logging = FALSE) {

  if (pdf && is.null(filename)) {
    stop("Please provide a file name to generate PDF output.", call. = FALSE)
  }
  theme_validator(theme)

  wrong_class_error(ds, "CrunchDataset", "ds")
  if (!any(c("Toplines", "Crosstabs") %in% class(data_summary))) {
    stop("The expected class for `data_summary` is either Toplines, CrunchTabs or Crosstabs CrunchTabs, not ", collapse_items(class(data_summary)))
  }

  # Munge the theme a bit
  theme$topline <- is(data_summary, "Toplines")
  if (is.null(theme$font_size)) {
    theme$font_size <- 12
  }
  theme$proportions <- proportions

  if (table_of_contents) {
    toc <- c("\\listoftables", "\\newpage")
  } else {
    toc <- NULL
  }

  # Now assemble the .tex document
  out <- c(
    latexDocHead(
      theme = theme,
      title = title,
      subtitle = subtitle,
      banner = data_summary$banner
    ),
    document(
      # moved here on 20190907 per delia's email
      "\\setlength{\\LTleft}{0pt}",
      "\\setlength{\\LTright}{\\fill}",
      "\\setlength{\\LTcapwidth}{\\textwidth}",
      vspace(".25in"),
      "",
      "",
      latexSampleDescription(
        sample_desc = sample_desc,
        field_period = field_period,
        moe = moe
      ),
      toc,
      "",
      in_brackets(
        "%% here's where individual input starts %%",
        "",
        "",
        latexReportTables(
          data_summary$results,
          data_summary$banner,
          theme
        ),
        append_text
      )
    )
  )

  if (!is.null(filename)) {
    filename <- paste0(filename, ".tex")
    cat(out, sep = "\n", file = filename)
    if (pdf) {
      if (logging) {
        print("PDF-ing")
      }
      if ("tinytex" %in% rownames(installed.packages())) {
        tinytex::pdflatex(filename, bib_engine = NULL)
        if (open) {
          file.open(filename)
        }
      } else {
        pdflatex(filename, open)
      }
    }
  }
  return(invisible(data_summary))
}
