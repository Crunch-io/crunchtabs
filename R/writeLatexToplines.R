
#' @export
writeLatex.Toplines <- function(toplines_summary, filename = NULL, proportions = FALSE, digits = 0, title = getName(toplines_summary), subtitle = NULL, date = Sys.Date(),
                                pdf = FALSE, path.to.pdflatex = Sys.which("pdflatex"), open = TRUE, returndata = FALSE, table_of_contents = FALSE, moe = NULL, headtext = "",
                                foottext = "", sample_desc = "", field_period = "", font = "helvet", font_size = 12, margin = list(top = 0.6, bottom = 0.6, left = 1, right = 1),
                                append_text = "", longtablewrap = FALSE, tableonly = FALSE, landscape = FALSE, pagewidth = ifelse(landscape, 9, 6.5), graphicspath = NULL, logo = NULL, ...) {

  toplines_summary$results <- lapply(toplines_summary$results, function(x) {
    x$data <- reformatResults(x, proportions = proportions, digits = digits)
    x
  })

  headers <- lapply(seq_along(toplines_summary$results), function(i) {
    toplineHeader(toplines_summary$results[[i]], table_width = pagewidth, num = i)
  })

  footers <- lapply(toplines_summary$results, toplineFooter)
  bodies <- lapply(toplines_summary$results, function(x) latexTable.body(x$data, dotfill = TRUE, autorownames = TRUE))

  tables <- sapply(ltranspose(list(headers, bodies, footers)), function(x) paste(x, collapse = "\n"))

  out <- c(tables, append_text)

  if (!tableonly) {
    latexHeadData <- latexHeadT(surveyhead = title, font_size = font_size, margin = margin, font = font, subhead = subtitle, landscape = landscape, graphicspath = graphicspath, logo = logo)
    latexStartData <- latexStartT(table_of_contents = table_of_contents, sample_desc = sample_desc, field_period = field_period, moe = moe)
    latexFootData <- latexFootT()
    out <- c(latexHeadData, latexStartData, out, latexFootData)
  }

  if (!is.null(filename)) {
    filename <- paste0(filename, ".tex")
    cat(out, sep = "\n", file = filename)
    if (pdf) {
      pdflatex(filename, open, path.to.pdflatex = path.to.pdflatex)
    }
  }

  if (returndata) {
    return(out)
  }
}


#' @export
toplineHeader <- function(x, table_width = 6.5, num = NULL, first_col_width = 1.5, padding = 1, use_heuristic = TRUE) {
  UseMethod("toplineHeader", x)
}

#' @export
toplineHeader.default <- function(var_summary, table_width = 6.5, num = NULL, first_col_width = 1.5, padding = 1, use_heuristic = TRUE) {
  tab_definition <- paste0("\\begin{tabular}{p{", table_width - padding, "in}}")
  toplineTableDef(var_summary, table_width, num, tab_definition)
}

#' @export
toplineHeader.ToplineCategoricalArray <- function(var_summary, table_width = 6.5, num = NULL, first_col_width = 1.5, padding = 0.25, use_heuristic = TRUE) {
  header_row <- "\n"
  col_names <- getNames(var_summary)[[2]]
  col_names_len <- length(col_names)
  col_width <- paste(round(1/col_names_len, digits = 2), "\\mywidth", sep = "")
  # use heuristic for scale questions
  if (use_heuristic && col_names_len >= 10) {
    which.split <- grep("^[0-9]+ - ", col_names)
    if (length(which.split) == 2) {
      labs <- escM(sub("^[0-9]+ - (.*)$", "\\1", col_names[which.split]))
      mcwidth <- (max(which.split) - min(which.split) + 1)/2
      midgaps <- 1 + ceiling(mcwidth)  ## in case it's an odd number
      mcwidth <- floor(mcwidth)
      midgaps <- midgaps - mcwidth
      labs[1] <- paste("\\multicolumn{", mcwidth, "}{l}{", labs[1], "}", collapse = "")
      labs[2] <- paste("\\multicolumn{", mcwidth, "}{r}{", labs[2], "}", collapse = "")
      scalestart <- paste(rep(" &", min(which.split)), collapse = "")
      scalemid <- paste(rep(" &", midgaps), collapse = "")
      scaleend <- paste(rep(" &", length(col_names) - max(which.split)), collapse = "")
      thisrow <- paste(scalestart, labs[1], scalemid, labs[2], scaleend, "\\\\")
      header_row <- paste(header_row, thisrow, "\n")
      col_names <- sub("^([0-9]+) - .*$", "\\1", col_names)
      col_width <- paste(round((table_width - padding - 0.75 - first_col_width)/col_names_len - 0.11, 3), "in", sep = "")
    }
  }
  header_row <- paste(header_row, "&", paste(escM(col_names), collapse = " & "), "\\\\\n")
  col.header <- paste("B{\\centering}{", col_width, "}", sep = "")
  col.header <- paste(rep(col.header, col_names_len), collapse = "")
  tab_definition <- paste0("\\begin{tabular*}{", table_width - padding, "in}{@{\\extracolsep{\\fill}}B{\\raggedright}{", first_col_width,
                           "in}", col.header, "}")

  toplineTableDef(var_summary, table_width, num, tab_definition, header_row)
}

toplineTableDef <- function(var_summary, table_width, num, tab_definition, header_row = '\n') {
  filtertext <- getFilterText(var_summary)
  paste("\\begin{table}[H]
        \\addcontentsline{lot}{table}{", escM(getName(var_summary)), "}
        \\colorbox{gray}{
        \\parbox{",
        table_width, "in}{", ifelse(is.null(num), "", paste0(num, ". ")), escM(getDescription(var_summary)), filtertext, "}}
        \\begin{center}", tab_definition,
        "\n", header_row, "\n", sep = "")
        }


#' @export
toplineFooter <- function(x) {
  UseMethod("toplineFooter", x)
}

#' @export
toplineFooter.default <- function(x) {
  toplineFooterDef(is_grid = FALSE)
}

#' @export
toplineFooter.ToplineCategoricalArray <- function(var_summary) {
  toplineFooterDef(is_grid = TRUE)

}

toplineFooterDef <- function(is_grid) {
  paste0("\\end{", paste0("tabular", ifelse(is_grid, "*", "")), "}
         \\end{center}
         \\end{table}")
}


latexFootT <- function() "\\end{hyphenrules} \n \\end{document}\n"


latexHeadT <- function(surveyhead, font_size, margin, font, landscape=FALSE, subhead=NULL, graphicspath = NULL, logo = NULL){
  paste("\\documentclass[", font_size, "pt", ifelse(landscape, ', landscape', ''), "]",
        "{article}\n",
        "\\usepackage[pdftex]{graphicx}\n",
        if (!is.null(graphicspath)) paste0("\\graphicspath{ {", graphicspath,"/} }"),
        "\\usepackage[utf8]{inputenc}\n",
        "\\usepackage{fancyhdr}\n",
        "\\usepackage{sfmath}\n",
        "\\usepackage[T1]{fontenc}\n",
        "\\usepackage{longtable}\n",
        "\\usepackage[scaled]{", font, "}\n",
        '\\renewcommand*\\familydefault{\\sfdefault}
        \\usepackage{booktabs}
        \\usepackage{float}
        \\usepackage[pdftex=true,
        pdftoolbar=true,
        pdfmenubar=true,
        pdfauthor = {},
        pdfcreator = {PDFLaTeX},
        pdftitle = {},
        colorlinks=true,
        urlcolor=blue,
        linkcolor=blue,
        citecolor=blue,
        implicit=true,
        hypertexnames=false]{hyperref}\n',
        "\\usepackage{marginnote}\n",
        "\\usepackage[",
        ifelse(!is.null(margin),
               paste("top=", margin$t, "in, bottom=", margin$b, "in, left=", margin$l, "in, right=", margin$r, "in, includeheadfoot", sep=""),
               "top=.6in,bottom=.6in,left=1in,right=1in,includeheadfoot"),
        "]{geometry}\n",
        "\\usepackage{array}\n",
        "\\setlength\\extrarowheight{2pt}\n",
        "\\newlength\\mywidth\n",
        "\\setlength\\mywidth{", ifelse(landscape, 6, 3.5), "in}\n",
        "\\newcolumntype{B}[2]{>{#1\\hspace{0pt}\\arraybackslash}b{#2}}\n",
        "\\pagestyle{fancy}\n",
        "\\renewcommand{\\headrulewidth}{0pt}\n",
        "\\renewcommand{\\footrulewidth}{0pt}\n",
        "\\fancyhead{}\n",
        "\\fancyhead[L]{{\\Large {\\bf ",
        ifelse(is.null(surveyhead),"",escM(surveyhead)), "}}",
        ifelse(is.null(subhead), "", paste(" \\\\", escM(subhead))), "}\n",
        if (!is.null(logo)) paste0("\\fancyhead[R]{\\includegraphics[scale=.4]{", logo, "}}\n"),
        "\\fancyfoot{}\n",
        "\\fancyfoot[R]{\\thepage}\n",
        "\\setlength{\\parindent}{0pt}",
        "\\usepackage[dvipsnames]{color}\n",
        "\\definecolor{gray}{gray}{0.85}\n\n",
        "\\usepackage[english]{babel}\n",
        "\\newcommand{\\PreserveBackslash}[1]{\\let\\temp=\\",
        "\\#1\\let\\",
        "\\=\\temp}\n",
        "\\let\\PBS=\\PreserveBackslash\n\n", sep="",
        "\\renewcommand*{\\marginfont}{\\scriptsize\\itshape}")
}
