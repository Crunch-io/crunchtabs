#' @export
writeLatex.Toplines <- function(data_summary, filename = NULL, proportions = TRUE, 
    title = getName(data_summary), subtitle = NULL, sample_desc = "", field_period = "", moe = NULL,
    table_of_contents = FALSE, append_text = "",
    pdf = FALSE, open = FALSE,
    row_label_width = 1.5,
    multirowheaderlines = FALSE,
    clearpage = TRUE, grid_num_letters = TRUE, custom_numbering = NULL,
    theme = theme_default()) {
    
    data_summary$results <- lapply(data_summary$results, function(x) {
        x$data <- reformatResults(x, proportions = proportions, theme = theme)
        x
    })

    headers <- lapply(seq_along(data_summary$results), function(i) {
        toplineHeader(data_summary$results[[i]], page_width = 6.5, row_label_width = row_label_width,
            num = if (!is.null(custom_numbering)) custom_numbering[i] else i)
    })

    footers <- lapply(data_summary$results, toplineFooter)
    bodies <- lapply(data_summary$results, function(x) latexTable.body(x$data, dotfill = TRUE, autorownames = TRUE))

    # NPR: this looks like it could instead be:
    # tables <- mapply(paste, headers, bodies, footers, sep="\n")
    # and then ltranspose can be deleted
    tables <- sapply(ltranspose(list(headers, bodies, footers)), function(x) paste(x, collapse = "\n"))

    out <- c(tables, append_text)
    
    latexHeadData <- latexHead(theme = theme, title = title, subtitle = subtitle, crosstabs = FALSE)
    # latexHeadData <- latexHeadT(title = title, font_size = font_size, theme = theme, subtitle = subtitle)
    latexStartData <- latexStartT(table_of_contents = table_of_contents, sample_desc = sample_desc, field_period = field_period, moe = moe)
    latexFootData <- latexFootT()
    out <- c(latexHeadData, latexStartData, out, latexFootData)
    
    if (!is.null(filename)) {
        filename <- paste0(filename, ".tex")
        cat(out, sep = "\n", file = filename)
        if (pdf) {
            print("PDF-ing")
            pdflatex(filename, open, path.to.pdflatex = Sys.which("pdflatex"))
        }
    }
    
    return(invisible(data_summary))
}

toplineHeader <- function(x, page_width = 6.5, num = NULL, row_label_width = 1.5, padding = 1, use_heuristic = TRUE) {
    UseMethod("toplineHeader", x)
}

#' @export
toplineHeader.default <- function(var_summary, page_width = 6.5, num = NULL, row_label_width = 1.5, padding = 1, use_heuristic = TRUE) {
    tab_definition <- paste0("\\begin{tabular}{p{", page_width - padding, "in}}")
    toplineTableDef(var_summary, page_width, num, tab_definition)
}

#' @export
toplineHeader.ToplineCategoricalArray <- function(var_summary, page_width = 6.5, num = NULL, row_label_width = 1.5, padding = 0.25, use_heuristic = TRUE) {
    header_row <- "\n"
    col_names <- getNames(var_summary)[[1]]
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
            col_width <- paste(round((page_width - padding - 0.75 - row_label_width)/col_names_len - 0.11, 3), "in", sep = "")
        }
    }
    header_row <- paste(header_row, "&", paste(escM(col_names), collapse = " & "), "\\\\\n")
    col.header <- paste("B{\\centering}{", col_width, "}", sep = "")
    col.header <- paste(rep(col.header, col_names_len), collapse = "")
    tab_definition <- paste0("\\begin{tabular*}{", page_width - padding, "in}{@{\\extracolsep{\\fill}}B{\\raggedright}{", row_label_width,
        "in}", col.header, "}")

    toplineTableDef(var_summary, page_width, num, tab_definition, header_row)
}

toplineTableDef <- function(var_summary, page_width, num, tab_definition, header_row = '\n') {
    filtertext <- getFilterText(var_summary)
    paste("\\begin{table}[H]
        \\addcontentsline{lot}{table}{", escM(getName(var_summary)), "}
        \\colorbox{gray}{
        \\parbox{",
        page_width, "in}{", ifelse(is.null(num), "", paste0(num, ". ")), escM(getDescription(var_summary)), filtertext, "}}
        \\begin{center}", tab_definition,
        "\n", header_row, "\n", sep = "")
        }

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

latexStartT <- function(table_of_contents, sample_desc, field_period, moe) {
    moe_text <- ""
    if (sample_desc != "")
        sample_desc <- paste("Sample  & ", sample_desc, "\\\\ \n ")
    if (!is.null(moe))
        moe_text <- paste("Margin of Error &  $\\pm ", round(100 * moe, digits = 1),
            "\\%$ \\\\ \n")
    if (field_period != "")
        field_period <- paste("Conducted  & ", field_period, "\\\\ \n")
    paste("\\begin{document}\n", "\\begin{hyphenrules}{nohyphenation}\n", "\\begin{tabular}{ll}\n",
        sample_desc, field_period, moe_text, "\\end{tabular}\n", ifelse(table_of_contents,
            "\\listoftables\n\n\n", "\n\n"), "%% here's where individual input starts %%\n\n\n \\vspace{.25in} \n\n",
        sep = "")
}

ltranspose <- function(l) {
    if (length(unique(unlist(lapply(l, length)))) > 1)
        stop("All nested lists must be of equal length.")
    if (!is.null(names(l[[1]]))) {
        return(sapply(names(l[[1]]), function(x) lapply(l, function(y) y[[x]])))
    } else {
        return(sapply(1:length(l[[1]]), function(x) lapply(l, function(y) y[[x]])))
    }
}




# latexHeadT <- function(title, font_size, theme, subtitle=NULL){
#     margin <- list(top = 0.6, bottom = 0.6, left = 1, right = 1)
#     paste("\\documentclass[", font_size, "pt]",
#         "{article}\n",
#         "\\usepackage[pdftex]{graphicx}\n",
#         "\\usepackage[utf8]{inputenc}\n",
#         "\\usepackage{fancyhdr}\n",
#         "\\usepackage{sfmath}\n",
#         "\\usepackage[T1]{fontenc}\n",
#         "\\usepackage{longtable}\n",
#         "\\usepackage[scaled]{", "helvet", "}\n", #theme$font
#         '\\renewcommand*\\familydefault{\\sfdefault}
#         \\usepackage{booktabs}
#         \\usepackage{float}
#         \\usepackage[pdftex=true, pdftoolbar=true, pdfmenubar=true, pdfauthor = {}, pdfcreator = {PDFLaTeX},
#         pdftitle = {},
#         colorlinks=true,
#         urlcolor=blue,
#         linkcolor=blue,
#         citecolor=blue,
#         implicit=true,
#         hypertexnames=false]{hyperref}\n',
#         "\\usepackage{marginnote}\n",
#         "\\usepackage[",
#         ifelse(!is.null(margin),
#             paste("top=", margin$t, "in, bottom=", margin$b, "in, left=", margin$l, "in, right=", margin$r, "in, includeheadfoot", sep=""),
#             "top=.6in,bottom=.6in,left=1in,right=1in,includeheadfoot"),
#         "]{geometry}\n",
#         "\\usepackage{array}\n",
#         "\\setlength\\extrarowheight{2pt}\n",
#         "\\newlength\\mywidth\n",
#         "\\setlength\\mywidth{", 3.5, "in}\n",
#         "\\newcolumntype{B}[2]{>{#1\\hspace{0pt}\\arraybackslash}b{#2}}\n",
#         "\\pagestyle{fancy}\n",
#         "\\renewcommand{\\headrulewidth}{0pt}\n",
#         "\\renewcommand{\\footrulewidth}{0pt}\n",
#         "\\fancyhead{}\n",
#         "\\fancyhead[L]{{\\Large {\\bf ",
#         ifelse(is.null(title),"",escM(title)), "}}",
#         ifelse(is.null(subtitle), "", paste(" \\\\", escM(subtitle))), "}\n",
#         if (!is.null(theme$logo$file)) paste0("\\fancyhead[R]{\\includegraphics[scale=.4]{", theme$logo$file, "}}\n"),
#         "\\fancyfoot{}\n",
#         "\\fancyfoot[R]{\\thepage}\n",
#         "\\setlength{\\parindent}{0pt}",
#         "\\usepackage[dvipsnames]{color}\n",
#         "\\definecolor{gray}{gray}{0.85}\n\n",
#         "\\usepackage[english]{babel}\n",
#         "\\usepackage{caption}\n",
#         "\\captionsetup[table]{labelformat=empty}\n",
#         "\\newcommand{\\PreserveBackslash}[1]{\\let\\temp=\\",
#         "\\#1\\let\\",
#         "\\=\\temp}\n",
#         "\\let\\PBS=\\PreserveBackslash\n\n", sep="",
#         "\\renewcommand*{\\marginfont}{\\scriptsize\\itshape}")
# }