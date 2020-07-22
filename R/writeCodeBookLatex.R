#' Create a codebook
#'
#' @param ds A crunch dataset
#' @param url A crunch dataset url
#' @param rmd Should we create an interim Rmd file? Defaults to TRUE.
#' @param pdf Should we write directly to pdf? Defaults to TRUE.
#'  title = getName(data_summary),
#' @param title An optional title. Defaults to the data summary title.
#' @param subtitle An optional character subtitle. Defaults to an empty string.
#' @param table_of_contents logical. Should a list of tables be included at the start
#' of the report Defaults to \code{FALSE}.
#' @param sample_desc A character string describing the sample.
#' @param field_period A character string describing the field period.
#' @param preamble A latex string, usually a methodological statement.
#' LaTeX should be escaped.
#' @param supress_zero_counts Should zero count categories be supressed? Defaults to FALSE.
#' @param appendix Should categorical questions with greater than 20 categories be put in an apppendix? Defaults to TRUE.
#' @param ... Additional arguments. Unused.
#' @export
writeCodeBookLatex <- function(ds, url = NULL, rmd = TRUE, pdf = TRUE, title = NULL,
  subtitle = NULL, table_of_contents = FALSE, sample_desc = NULL,
  field_period = NULL, preamble = NULL, suppres_zero_counts = FALSE, appendix = TRUE, ...) {

  options("crunchtabs.codebook.supress.zeros" = suppres_zero_counts)

  # Initialize Codebook Latex ----
  codebook <- readLines(system.file(
    "codebook_latex_wrap.tex",
    package = "crunchtabs"
    )
  )

  # Fancy Headers ----
  fh = NULL
  t1 = !is.null(title)
  t2 = !is.null(subtitle)

  if (!t1) {
    title = crunch::name(ds)
    t1 = TRUE
  }

  if (t1 & t2) { # title and subtitle
    tex = "\\fancyhead{}\n\\fancyhead[L]{{\\fontsize{16}{24}\\textbf{%s}}\\fontsize{12}{18}\\textbf{ \\\\ %s}}"
    fh = sprintf(tex, title, subtitle)
  }

  if (t1 & !t2) { # title no subtitle
    tex = "\\fancyhead{}\n\\fancyhead[L]{{\\fontsize{16}{24}\\textbf{%s}}}"
    fh = sprintf(tex, title)
  }


  # Sample Description -----
  sample_description <- latexSampleDescription(
    sample_desc = sample_desc,
    field_period = field_period,
    moe = NULL

  )

  sample_description = paste0(sample_description, collapse = "\n")


  # Generate codebook items ----
  items <- list()
  nms <- names(ds)

  appendices <- list()

  for (nm in nms) {
    message("Preparing: ", nm)

    items[[nm]] = list()
    items[[nm]]$header <- noBreaks(
      paste0(
        ifelse(nm != nms[1], "\n\\vskip 0.25in\n", ""),
        codeBookItemTxtHeader(ds[[nm]]),
        codeBookItemTxtDescription(ds[[nm]]),
        collapse = "\n"
      )
    )

    body <- codeBookItemBody(ds[[nm]]) # A kable

    if (appendix & !is.list(body)) {
      if (attributes(body)$kable_meta$nrow > 21) {

        appendices[[nm]] <- list()

        description <- codeBookItemTxtDescription(ds[[nm]])
        description <- gsub(
          "addcontentsline{lot}{table}{",
          sprintf("addcontentsline{lot}{table}{Appendix %s: ", length(appendices)),
          description, fixed = TRUE
        )

        appendices[[nm]]$header <- noBreaks(
          paste0(
            ifelse(nm != nms[1], "\n\\vskip 0.25in\n", ""),
            codeBookItemTxtHeader(ds[[nm]]),
            description,
            collapse = "\n"
          )
        )
        appendices[[nm]]$body <- body

        body <- sprintf(
          "\\textit{There are more than 20 categories. Please see Appendix %s}",
          length(appendices)
        )

      }
    }

    items[[nm]] = list()
    items[[nm]]$header <- noBreaks(
      paste0(
        ifelse(nm != nms[1], "\n\\vskip 0.25in\n", ""),
        codeBookItemTxtHeader(ds[[nm]]),
        codeBookItemTxtDescription(ds[[nm]]),
        collapse = "\n"
      )
    )


    if (is.list(body)) {
      items[[nm]]$body <- noBreaks(paste0(unlist(body), collapse = "\n"))
    } else {
      items[[nm]]$body <- body
    }

  }

  # Assign Codebook ----

  codebook[codebook == "<<methods>>"] <- ifelse(!is.null(preamble), preamble, "")
  codebook[codebook == "<<toc>>"] <- ifelse(table_of_contents, "\\listoftables\n\\clearpage", "")
  codebook[codebook == "<<fh>>"] <- fh
  codebook[codebook == "<<sample_description>>"] <- sample_description
  codebook[codebook == "<<drop_zero_notification>>"] <- ifelse(
    getOption("crunchtabs.codebook.supress.zeros", default = FALSE),
    "Important Note: Categories with no responses have been excluded from display.", "")

  # Non breaking blocks
  items = lapply(items, function(x) {
    if (any(grepl("longtabu", x))) {
      return(x)
    } else {
      noBreaks(paste0(unlist(x), collapse = "\n"))
    }
  })

  codebook[codebook == "<<body>>"] <- paste0(
    unname(unlist(items)), collapse = "\n")
  if (length(appendices) > 0) {
    codebook[codebook == "<<appendices>>"] <- paste0(
      unname(unlist(appendices)), collapse = "\n")

    tex = "\\clearpage\n\\fancyhead{}\n\\fancyhead[L]{{\\fontsize{16}{24}\\textbf{Appendix}}}\n"
    codebook[codebook == "<<fh_appendix>>"] <- tex
  } else {
    codebook[codebook == "<<fh_appendix>>"] <- ""
    codebook[codebook == "<<appendices>>"] <- ""
  }

  write(codebook, gsub(" ","-", paste0(name(ds), ".tex")))

  if (pdf) {
    tinytex::pdflatex(gsub(" ","-", paste0(name(ds), ".tex")))
    file.open(paste0(name(ds), ".pdf"))
  }
}
