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
#' @param suppress_zero_counts Should zero count categories be supressed? Defaults to FALSE.
#' @param appendix Should categorical questions with greater than 20 categories be put in an apppendix? Defaults to TRUE.
#' @param logo Default to NULL. A character string one of: yougov or ygblue. Includes the logo automatically. Also accepts a path to a logo file.
#' @param position Defaults to NULL. Identifies the position of the table on the page. Accepts "c", "l", or "r". Default position is left aligned tables.
#' @param ... Additional arguments passed to \link[kableExtra]{kable_styling} Unused.
#' @export
writeCodeBookLatex <- function(
  ds, url = NULL, rmd = TRUE, pdf = TRUE, title = NULL, subtitle = NULL,
  table_of_contents = FALSE, sample_desc = NULL, field_period = NULL,
  preamble = NULL, suppres_zero_counts = FALSE, appendix = TRUE, logo = NULL,
  position = NULL,
  ...) {

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
  headers <- list()
  nms <- names(ds)

  appendices <- list()

  for (nm in nms) {
    message("Preparing: ", nm)

    items[[nm]] = list()
    headers[[nm]] <- noBreaks(
      paste0(
        ifelse(nm != nms[1], "\n\\vskip 0.25in\n", ""),
        codeBookItemTxtHeader(ds[[nm]]),
        codeBookItemTxtDescription(ds[[nm]]),
        collapse = "\n"
      )
    )

    body <- codeBookItemBody(ds[[nm]], ...) # A kable

    if (appendix & !is.list(body)) {
      if (attributes(body)$kable_meta$nrow > 21) {

        appendices[[nm]] <- list()

        description <- codeBookItemTxtDescription(ds[[nm]])
        description <- gsub(
          sprintf(
            "addcontentsline{lot}{table}{\\parbox{1.8in}{\\ttfamily{%s}",
            nm
          ),
          sprintf(
            "addcontentsline{lot}{table}{\\parbox{1.8in}{\\ttfamily{Appendix %s}}{%s -- ",
            length(appendices), nm),
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


    if (is.list(body)) {
      # If grid header should be wrapped with first table so as not to allow
      # pagebreaks (ie, we don't want header and first body to be on separate
      # pages)
      #
      # Our body, is a list of three tables we need to pre-pend the first
      # table with the header for this codebook item
      body[[1]] <- paste(headers[[nm]], body[[1]], collapse = "\n")
      items[[nm]]$body <- paste(lapply(unlist(body), noBreaks), collapse = "\n")

    } else {
      items[[nm]]$body <- noBreaks(paste(headers[[nm]],body, collapse = "\n"))
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

  # Logo ----

  if (is.null(logo)) {
    codebook[codebook == "<<logo>>"] <- ""
  } else {

    if (logo == "yougov") {
      tex <- "\\fancyhead[R]{\\includegraphics[scale=.4]{%s}}"
      path_to_logo <- system.file("YouGov.png", package = "crunchtabs")
      codebook[codebook == "<<logo>>"] <- sprintf(tex, path_to_logo)
    }

    if (logo == "ygblue") {
      tex <- "\\fancyhead[R]{\\includegraphics[scale=.4]{%s}}"
      path_to_logo <- system.file("YouGovBlue_small.png", package = "crunchtabs")
      codebook[codebook == "<<logo>>"] <- sprintf(tex, path_to_logo)
    }
  }

  # Body ----

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

  # Table Positioning

  if (!is.null(position)) {
    stopifnot(position %in% c("c", "l", "r"))
    replacement <- sprintf("[%s]", position)
    codebook <- gsub("\\begin{longtable}[l]", paste0("\\begin{longtable}", replacement), codebook, fixed = TRUE)
    # codebook <- gsub("\\begin{longtabu}", paste0("\\begin{longtabu}", replacement), codebook, fixed = TRUE)
  }

  write(codebook, gsub(" ","-", paste0(name(ds), ".tex")))

  if (pdf) {
    tinytex::pdflatex(gsub(" ","-", paste0(name(ds), ".tex")))
    file.open(paste0(name(ds), ".pdf"))
  }
}
