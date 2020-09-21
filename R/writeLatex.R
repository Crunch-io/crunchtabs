#' Generate LaTeX Reports: Toplines and Banners
#'
#' \code{writeLatex} produces publication-quality LaTeX reports:
#' Toplines (one-way frequency tables) or Banners (cross tabulations).
#'
#' @param data_summary An object of class \code{Toplines} or \code{Crosstabs}.
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
#' @param logging Leave logs in the working directory, defaults to FALSE
#'
#' @return If \code{returndata} is set to \code{TRUE}, a processed data that was used to produce
#' the report is returned. Otherwise \code{NULL} is returned.
#' @examples
#' \dontrun{
#' # toplines report
#' toplines_summary <- crosstabs(crunch_dataset, weight = 'weight')
#' writeLatex(toplines_summary, 'filename')
#' # crosstabs report
#' crosstabs_summary <- crosstabs(crunch_dataset, banner = banner_object)
#' writeLatex(crosstabs_summary, 'filename')
#' }
#' @importFrom utils installed.packages
#' @export
writeLatex <- function(data_summary, theme = themeDefaultLatex(),
                       filename = getName(data_summary), title = getName(data_summary),
                       subtitle = NULL, table_of_contents = FALSE, sample_desc = NULL,
                       field_period = NULL, moe = NULL, append_text = NULL, proportions = TRUE,
                       pdf = FALSE, open = FALSE, logging = FALSE) {

  if (pdf && is.null(filename)) {
    stop("Please provide a file name to generate PDF output.", call. = FALSE)
  }
  theme_validator(theme)

  wrong_class_error(data_summary, "CrunchTabs", "data_summary")
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

  if (!is.null(append_text)) {
    append_text <- paste0("\\vspace{0.5in}\n\n", paste0(append_text, collapse = "\n"))
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
      if ("tinytex" %in% rownames(installed.packages())) {
        tinytex::pdflatex(filename, bib_engine = NULL)
        if (!logging) {
          files <- list.files(path = getwd())
          files <- grep("out$|log$|aux$", files, value = TRUE)
          if (length(files)) {
            file.remove(file.path(getwd(), files))
          }
        }

        if (open) {
          file.open(gsub(".tex", ".pdf", filename, fixed = TRUE))
        }
      } else {
        pdflatex(filename, open)
      }
    }
  }
  return(invisible(data_summary))
}

latexReportTables <- function(results, banner, theme) {
  # Each element of `results` contains a list of tables. For toplines and
  # tab books with a single banner, those are length 1, but if there are
  # multiple banners, we'll generate tables for each (slightly differently)
  # and join them together into a single result.
  #
  # This lapply returns a character vector the same length as `results`,
  # each string in it being a "full" table.

  table_bodies <- list()

  for (i in 1:length(results)) { # convert to loop for debug

    x = results[[i]]

    if (!x$type %in% c("NumericVariable", "DatetimeVariable", "TextVariable")) {
      # Do some munging and generate the table bodies to match those header(s)
      x <- removeInserts(x, theme)
      # Lots of dragons in this "reformat" code :shrug:
      content <- reformatLatexResults(x, banner, theme)

      # PT: decide if the final table should be longtable or tabular based on the
      # number of responses and latex_max_lines_for_tabular
      x$longtable <- calculateIfLongtable(content[[1]], theme)

      # PT: because this is a loop, header is singular (i.e. it's only one table at a time).
      header <- tableHeader(x, theme)
      body <- sapply(content, latexTableBody, theme = theme)

      footer <- ifelse(
        x$longtable | !theme$topline,
        "\n\\end{longtable}",
        "\n\\end{tabular}"
      )

      # This paste will collapse the perhaps multiple banner tables into a
      # single string of LaTeX.
      table <- paste(
        header,
        body,
        footer,
        sep = "\n",
        collapse = "\n\n\n"
      )
      if (x$longtable) {
        # centers longtables because otherwise the head/foot text are not centered
        # but doesn't center tabular because it actually ends up being uncentered
        # if centered is used on tabulars.
        table <- center(table)
      }
    } else {
      # Customized path for variables that are manually added
      # to tabBook results. tabBook does not provide summaries for
      # numeric, datetime nor text variables which we need for
      # creating a basic codebook


      x$longtable <- calculateIfLongtable(x, theme)

      header <- tableHeader(x, theme)
      body <- latexTableBody(x, theme)
      footer <- ifelse(
        x$longtable | !theme$topline,
        "\n\\end{longtable}",
        "\n\\end{tabular}"
      )


      table <- paste(
        header,
        body,
        footer,
        sep = "\n",
        collapse = "\n\n\n"
      )

      if (x$longtable) {
        # centers longtables because otherwise the head/foot text are not centered
        # but doesn't center tabular because it actually ends up being uncentered
        # if centered is used on tabulars.
        table <- center(table)
      }

    }

    # beb: Enclose if pagebreak_in_banner = FALSE
    # For some reason that I don't care about
    # bottomrule gets wiped out by nopagebreak environment
    # adding one manually
    if (!theme$pagebreak_in_banner)
      table = gsub(
        "\\end{longtable}",
        "\\bottomrule\\end{longtable}\n\\end{absolutelynopagebreak}",
        table, fixed = TRUE)

    table_bodies[[i]] = table
  }

  if (theme$one_per_sheet) {
    table_bodies <- paste0(table_bodies, "\n\\clearpage")
  } else {
    # two tabulars in a row have no space between them so this is a hack to add space.
    tabulars <- grep("begin\\{tabular\\}", table_bodies)
    add_space <- tabulars[tabulars %in% (tabulars - 1)]
    table_bodies[add_space] <- paste0(table_bodies[add_space], "\n", vspace(".25in"))
  }
  # Put some extra space between each table
  table_bodies <- paste0(table_bodies, "\n\n")
  return(table_bodies)
}

#' Prepare Doc Head
#'
#' Prepare a document head for a latex document
#'
#' @param theme A theme object created by \link{themeNew}
#' @param title A page title
#' @param subtitle A page subtitle
#' @param banner A banner object from \link{banner}
latexDocHead <- function(theme, title, subtitle, banner = NULL) {
  topline <- theme$topline
  title <- texEscape(title)
  subtitle <- texEscape(subtitle)
  if (nchar(subtitle)) {
    # If there is one, precede it by a newline
    subtitle <- paste("", newline, subtitle)
  }

  logo <- theme$logo$file
  if (!is.null(logo)) {
    logo <- paste0("\\fancyhead[R]{\\includegraphics[scale=.4]{", logo, "}}")
  }
  if (topline) {
    bdr <- "1in"
    doc_class <- "\\documentclass{article}"
  } else {
    bdr <- "0.5in"
    doc_class <- "\\documentclass[landscape]{article}"
  }

  nopagebreak = "\\newenvironment{absolutelynopagebreak}
  {\\par\\nobreak\\vfil\\penalty0\\vfilneg
   \\vtop\\bgroup}
  {\\par\\xdef\\tpd{\\the\\prevdepth}\\egroup
   \\prevdepth=\\tpd}"

  unlist(c(
    doc_class,
    usepackage("graphicx", "pdftex"),
    usepackage("inputenc", "utf8"),
    usepackage("fancyhdr"),
    usepackage("sfmath"),
    usepackage("comment"),
    usepackage("fontenc", "T1"),
    usepackage("hyperref",
               "pdftex=true",
               "pdftoolbar=true",
               "pdfmenubar=true",
               "pdfauthor = {}",
               "pdfcreator = {PDFLaTeX}",
               "pdftitle = {}",
               "colorlinks=true",
               "urlcolor=blue",
               "linkcolor=blue",
               "citecolor=blue",
               "implicit=true",
               "hypertexnames=false"
    ),
    usepackage(validLatexFont(theme$font), "scaled"),
    "\\renewcommand*\\familydefault{\\sfdefault}",
    usepackage("booktabs, dcolumn, longtable"),
    usepackage("geometry",
               "top=0.6in",
               "bottom=0.6in",
               paste0("left=", bdr),
               paste0("right=", bdr),
               "includeheadfoot"
    ),
    usepackage("array"),
    usepackage("babel", "english"),
    "\\newcolumntype{B}[2]{>{#1\\hspace{0pt}\\arraybackslash}b{#2}}",
    "\\setlength{\\parindent}{0pt}",
    usepackage("color", "dvipsnames"),
    "\\definecolor{gray}{gray}{0.85}",
    "\\pagestyle{fancy}",
    "\\renewcommand{\\headrulewidth}{0pt}",
    "\\renewcommand{\\footrulewidth}{0pt}",
    "\\fancyhead{}",
    paste0("\\fancyhead[L]{{", applyLatexStyle(title, theme$format_title), "}",
           applyLatexStyle(subtitle, theme$format_subtitle), "}"),
    logo,
    "\\newcolumntype{d}{D{.}{.}{3.2}}", #!topline
    "\\newcolumntype{g}{D{\\%}{\\%}{3.0}}", #!topline #changed 20190907 from 5.0 to 3.0
    usepackage("float"), #topline
    usepackage("marginnote"), #topline
    "\\setlength\\extrarowheight{2pt}", #topline
    "\\newlength\\mywidth", #topline
    "\\setlength\\mywidth{3.5in}", #topline
    usepackage("caption"), #topline
    "\\captionsetup[table]{labelformat=empty}", #topline
    "\\renewcommand*{\\marginfont}{\\scriptsize\\itshape}", #topline
    "\\fancyfoot{}",
    ifelse(theme$latex_page_numbers, "\\fancyfoot[R]{\\thepage}",""),
    newcommand("PreserveBackslash", args = 1, "\\let\\temp=\\\\#1\\let\\\\=\\temp"),
    "\\let\\PBS=\\PreserveBackslash",
    newcommand("longtablesep", paste(
      "\\endfirsthead",
      multicolumn(2, italics(texEscape(theme$latex_headtext))), newline,
      "\\endhead",
      multicolumn(2, italics(texEscape(theme$latex_foottext))), newline,
      "\\endfoot",
      "\\endlastfoot"
    )),
    usepackage("tocloft", "titles"),
    newcommand("cftchapfont", theme$font_size), # TODO: make this actually change the font size
    newcommand("formatvardescription", args = 1,
               applyLatexStyle("#1", theme$format_var_description)),
    newcommand("formatvarname", args = 1,
               applyLatexStyle("#1", theme$format_var_name)),
    newcommand("formatvaralias", args = 1,
               applyLatexStyle("#1", theme$format_var_alias)),
    newcommand("formatvarfiltertext", args = 1,
               applyLatexStyle("#1", theme$format_var_filtertext)),
    newcommand("formatvarsubname", args = 1,
               applyLatexStyle("#1", theme$format_var_subname)),
    usepackage("amsmath"),
    nopagebreak,
    "",
    "",
    # If there are one or more banners, generate the banner definition
    # macros in the header so they can be reused on each page.
    lapply(seq_along(banner), function(j) {
      longtableHeadFootMacros(
        banner[[j]],
        num = j,
        page_width = 9,
        theme = theme
      )
    })
    # (the return is wrapped in unlist() because of ^)
  ))
}


#' Sample Description
#'
#' Adds a sample description, field window and margin of error to
#' your result document.
#'
#' @param sample_desc A string describing the samepl
#' @param field_period A string defining from-to field dates
#' @param moe The margin of error for the survey
latexSampleDescription <- function(sample_desc, field_period, moe) {
  if (is.null(sample_desc) & is.null(moe) & is.null(field_period)) {
    return("")
  } # TODONE: don't include this tabular at all if empty

  # More preamble before the tables
  if (!is.null(sample_desc)) {
    sample_desc <- paste("Sample  & ", sample_desc, newline, "")
  }
  if (!is.null(field_period)) {
    field_period <- paste("Conducted  & ", field_period, newline, "")
  }
  if (!is.null(moe)) {
    moe <- paste("Margin of Error &  $\\pm ", round(100 * moe, digits = 1), "\\%$", newline, "")
  }

  c(
    "\\begin{longtable}[l]{ll}",
    sample_desc,
    field_period,
    moe,
    "\\end{longtable}"
  )
}
