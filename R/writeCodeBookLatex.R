#' Create a codebook
#'
#' @param ds A crunch dataset
#' @param url A crunch dataset url
#' @param rmd Should we create an interim Rmd file? Defaults to TRUE
#' @param pdf Should we write directly to pdf? Defaults to TRUE
#'  title = getName(data_summary),
#' @param title An optional title. Defaults to the data summary title.
#' @param subtitle An optional character subtitle. Defaults to an empty string.
#' @param table_of_contents logical. Should a list of tables be included at the start
#' of the report Defaults to \code{FALSE}.
#' @param sample_desc A character string describing the sample.
#' @param field_period A character string describing the field period.
#' @param preamble A latex string, usually a methodological statement.
#' @param ... Additional arguments. Unused.
#' @export
writeCodeBookLatex <- function(ds, url = NULL, rmd = TRUE, pdf = TRUE, title = NULL,
  subtitle = NULL, table_of_contents = FALSE, sample_desc = NULL,
  field_period = NULL, preamble = NULL, ...) {

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

  for (nm in nms) {
    items[[nm]] = list()
    items[[nm]]$header <- noBreaks(paste0(
      codeBookItemTxtHeader(ds[[nm]]),
      codeBookItemTxtDescription(ds[[nm]]),
      collapse = "\n"
      )
    )

    body <- codeBookItemBody(ds[[nm]]) # A kable

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

  # Non breaking blocks
  items = lapply(items, function(x) {
    if (any(grepl("longtabu", x))) {
      return(x)
    } else {
      noBreaks(paste0(unlist(x), collapse = "\n"))
    }
  })

  codebook[codebook == "<<body>>"] <- paste0(unname(unlist(items)), collapse = "\n")

  write(codebook, gsub(" ","-", paste0(name(ds), ".tex")))

  if (pdf) {
    tinytex::pdflatex(gsub(" ","-", paste0(name(ds), ".tex")))
    file.open(paste0(name(ds), ".pdf"))
  }
}
