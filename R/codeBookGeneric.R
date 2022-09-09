#' Create a CodeBook from any dataset
#'
#' Given meta data and summary objects, create a codebook using YouGov's typical codebook
#' presentation.
#'
#' @param ds A data.frame
#' @param meta A data.frame containing meta data which requires specific columns as follows:
#' * name: A longer descriptive name should not be more than 80 characters, will show in the top
#' right of the codebook item.
#' * alias: The name of the column reference in df. This is displayed in the top right
#' * description: A longer form description of the data held in this variable
#' * notes: Any kind of additional information that may be relevant to the variable but not
#' excplicitly included in the description. Example include a source description, or perhaps
#' an explanation of if / why missings are importantant, or with survey data it is often useful to
#' identify skip patterns or sub-population specific questions (ie, only asked of people who voted)
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
#' @param appendix Should categorical questions with greater than 20 categories be put in an
#' apppendix? Defaults to TRUE.
#' @param logo Default to NULL. A character string one of: yougov or ygblue. Includes the logo
#' automatically. Also accepts a path to a logo file.
#' @param position Defaults to NULL. Identifies the position of the table on the page. Accepts
#' "c", "l", or "r". Default position is left aligned tables.
#' @param path The path to place .tex and .pdf files.
#' @param logging Leave logs in the working directory, defaults to FALSE
#' @param filename A string. The desired basename of the resulting file with no extension
#' (i.e, "mycodebook")
#' @param open Should the resulting PDF be opened? Defaults to FALSE
#' @param ... Additional arguments passed to \link[kableExtra]{kable_styling} Unused.
#' @md
#'
writeCodeBookLatexGeneric <- function(
  ds, meta, pdf = TRUE, title = NULL, subtitle = NULL,
  table_of_contents = FALSE, sample_desc = NULL, field_period = NULL,
  preamble = NULL, suppress_zero_counts = FALSE, appendix = TRUE,
  logo = NULL, position = NULL, path = NULL, filename = NULL,
  logging = FALSE, open = FALSE, ...) {

  options("crunchtabs.codebook.suppress.zeros" = suppress_zero_counts)


  # Initialize CodeBook Latex ----
  codebook <- readLines(system.file(
    "codebook_latex_wrap.tex",
    package = "crunchtabs"
  ))

  # Fancy Headers ----
  fh <- NULL
  t1 <- !is.null(title)
  t2 <- !is.null(subtitle)

  if (!t1) {
    title <- "General Dataset"
    t1 <- TRUE
  }

  if (t1 & t2) { # title and subtitle
    tex <- "\\fancyhead{}\n\\fancyhead[L]{{\\fontsize{16}{24}\\textbf{%s}}\\fontsize{12}{18}\\textbf{ \\\\ %s}}" # nolint
    fh <- sprintf(tex, title, subtitle)
  }

  if (t1 & !t2) { # title no subtitle
    tex <- "\\fancyhead{}\n\\fancyhead[L]{{\\fontsize{16}{24}\\textbf{%s}}}"
    fh <- sprintf(tex, title)
  }

  # Sample Description -----
  sample_description <- latexSampleDescription(
    sample_desc = sample_desc,
    field_period = field_period,
    moe = NULL
  )

  sample_description <- paste0(sample_description, collapse = "\n")

  # Generate codebook items ----
  items <- list()
  headers <- list()
  nms <- meta$alias

  appendices <- list()

  for (nm in nms) {

    if (any(class(ds) %in% c("ArrowObject", "arrow_dplyr_query"))) {
      cls <- get_class(ds, nm)
      if(cls == "character") {
        x <- ds %>% filter(cd_number == 1) %>% dplyr::select(nm) %>% dplyr::collect() %>% dplyr::pull(nm)
      } else {
        x <- ds %>% dplyr::select(nm) %>% dplyr::collect() %>% dplyr::pull(nm)
      }

    } else {
      x <- ds[[nm]]
    }

    message("Preparing: ", nm)

    items[[nm]] <- list()
    headers[[nm]] <- noBreaks(
      paste0(
        ifelse(nm != nms[1], "\n\\vskip 0.25in\n", ""),
        codeBookItemTxtHeaderGeneral(x, nm, meta),
        codeBookItemTxtDescriptionGeneral(x, nm, meta),
        collapse = "\n"
      )
    )

    body <- codeBookItemBody(x, meta = meta[meta$alias == nm,], ...) # A kable

    if (appendix & !is.list(body)) {
      if (length(capture.output(body)) > 20) {
        appendices[[nm]] <- list()

        description <- codeBookItemTxtDescriptionGeneral(x, nm, meta)
        description <- gsub(
          sprintf(
            "addcontentsline{lot}{table}{\\parbox{1.8in}{\\ttfamily{%s}",
            nm
          ),
          sprintf(
            "addcontentsline{lot}{table}{\\parbox{1.8in}{\\ttfamily{Appendix %s}}{%s -- ",
            length(appendices), nm
          ),
          description,
          fixed = TRUE
        )

        appendices[[nm]]$header <- noBreaks(
          paste0(
            ifelse(nm != nms[1], "\n\\vskip 0.25in\n", ""),
            codeBookItemTxtHeaderGeneral(x, nm, meta),
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
      items[[nm]]$body <- noBreaks(paste(headers[[nm]], body, collapse = "\n"))
    }
  }

  # Assign Codebook ----

  codebook[codebook == "<<methods>>"] <- ifelse(!is.null(preamble), preamble, "")
  codebook[codebook == "<<toc>>"] <- ifelse(table_of_contents, "\\listoftables\n\\clearpage", "")
  codebook[codebook == "<<fh>>"] <- fh
  codebook[codebook == "<<sample_description>>"] <- sample_description
  codebook[codebook == "<<drop_zero_notification>>"] <- ifelse(
    getOption("crunchtabs.codebook.supress.zeros", default = FALSE),
    "Important Note: Categories with no responses have been excluded from display.", ""
  )

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
    unname(unlist(items)),
    collapse = "\n"
  )
  if (length(appendices) > 0) {
    codebook[codebook == "<<appendices>>"] <- paste0(
      unname(unlist(appendices)),
      collapse = "\n"
    )

    tex <- "\\clearpage\n\\fancyhead{}\n\\fancyhead[L]{{\\fontsize{16}{24}\\textbf{Appendix}}}\n"
    codebook[codebook == "<<fh_appendix>>"] <- tex
  } else {
    codebook[codebook == "<<fh_appendix>>"] <- ""
    codebook[codebook == "<<appendices>>"] <- ""
  }

  # Table Positioning

  if (!is.null(position)) {
    stopifnot(position %in% c("c", "l", "r"))
    replacement <- sprintf("[%s]", position)
    codebook <- gsub(
      "\\begin{longtable}[l]", paste0("\\begin{longtable}", replacement), codebook, fixed = TRUE)
  }


  # Issue 204 - Sanitize title
  if (is.null(filename)) {
    filename <- gsub(" ", "-", title)
    filename <- gsub("[[:punct:]]", "-", filename)
    filename <- gsub("[-]$", "", filename)
  }

  # Issue 185 - Specify a path
  if (!is.null(path)) {
    basename <- gsub(" ", "-", filename)
    texname <- paste0(path, "/", basename, ".tex")
    pdfname <- paste0(path, "/", basename, ".pdf")
  } else {
    basename <- gsub(" ", "-", filename)
    texname <- paste0(basename, ".tex")
    pdfname <- paste0(basename, ".pdf")
  }

  write(codebook, texname)

  if (pdf) {
    tinytex::pdflatex(texname, pdf_file = pdfname)

    if (!logging) {
      files <- list.files(path = getwd())
      files <- grep("out$|log$|aux$", files, value = TRUE)
      if (length(files)) {
        file.remove(file.path(getwd(), files))
      }
    }
    if(open) {
      file.open(pdfname) # nocov
    }    
  }
}


#' codeBook Item Text Header
#'
#' Creates a text header for a codebook item
#' @param x An R vector
#' @param nm The alias of the current codeBookItem
#' @param meta A specifically formatted data.frame with meta data.
#' @param ... Further arguments, not used.
#'
#' @export
codeBookItemTxtHeaderGeneral <- function(x, nm, meta, ...) {
  txt <- list()
  txt$name <- meta$name[meta$alias == nm]
  txt$alias <- nm

  tex <- "\\textbf{%s}\\hfill\\textbf{\\ttfamily{%s}}\n\n{\\small %s}\n\n"

  lookup <- data.frame(
    type = c(
      "CategoricalVariable",
      "CategoricalArrayVariable",
      "TextVariable",
      "NumericVariable",
      "DatetimeVariable",
      "DateVariable",
      "MultipleResponseVariable",
      "factor",
      "numeric",
      "integer",
      "character"
    ),
    softType = c(
      "Categorical",
      "Grid",
      "Text",
      "Numeric",
      "Date",
      "Date",
      "Multiple Response",
      "factor",
      "numeric",
      "integer",
      "character"
    )
  )

  softType <- lookup[lookup$type == class(x), ]$softType

  sprintf(
    tex,
    texEscape(txt$name),
    texEscape(txt$alias),
    softType
  )
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
#' @param nm The alias of the current codeBookItem
#' @param meta A specifically formatted data.frame with meta data.
#' @param ... Additional arguments passed to \link{kable_styling_defaults}
#' @md
#' @export
codeBookItemTxtDescriptionGeneral <- function(x, nm, meta, ...) {
  txt <- list()
  txt$description <- meta$description[meta$alias == nm]
  txt$notes <- meta$notes[meta$alias == nm]
  if (class(x) == "character") {
    txt$notes <- paste0(txt$notes, " - ", "Counts displayed are from only the first congressional district of each state.")
  }
  txt$alias <- nm
  txt$alias_toc <- ifelse(
    nchar(txt$alias) > 20,
    paste0(substr(txt$alias, 1, 22), "..."),
    txt$alias
  )

  txt$name <- meta$name[meta$alias == nm]

  txt$name_toc <- ifelse(
    nchar(txt$name) > 65,
    paste0(substr(txt$name, 1, 65), "..."),
    txt$name
  )

  if (txt$notes != "") {
    tex <- "\\vskip 0.10in\n%s\n\\addcontentsline{lot}{table}{\\parbox{1.8in}{\\ttfamily{%s}} %s}\n\\vskip 0.10in\n\\emph{%s}\n\\vskip 0.10in" # nolint
    tex <- sprintf(
      tex,
      texEscape(txt$description),
      texEscape(txt$alias_toc),
      texEscape(txt$name_toc),
      texEscape(txt$notes)
    )
  } else {
    tex <- "\\vskip 0.10in\n%s\n\\addcontentsline{lot}{table}{\\parbox{1.8in}{\\ttfamily{%s}} %s}\n\\vskip 0.10in" # nolint
    tex <- sprintf(
      tex,
      texEscape(txt$description),
      texEscape(txt$alias_toc),
      texEscape(txt$name_toc)
    )
  }

  tex
}

#' Get class name from arrow variable
#'
#' Arrow does not provide a method for class on it's variables. This function prints out
#' the schema and captures the appropriate output.
#'
#' @param ds An arrow dataset
#' @param variable A string identifying the variable
get_class <- function(ds, variable) {
  # nocov start
  r <- utils::capture.output(ds$schema[[variable]])
  r <- r[2]
  r <- gsub(variable, "", r)
  r <- gsub(": ", "", r)

  if (r %in% c("string"))
    return("character")
  if (r %in% c("int32"))
    return("integer")
  if (r %in% c("float", "double"))
    return("numeric")
  if (grepl("dictionary", r))
    return("factor")
  # nocov end
}
