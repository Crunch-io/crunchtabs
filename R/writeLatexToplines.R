#' @export
writeLatex.Toplines <- function(data_summary, theme = themeDefaultLatex(), 
    filename = getName(data_summary), title = getName(data_summary), 
    subtitle = NULL, table_of_contents = FALSE, sample_desc = NULL, 
    field_period = NULL, moe = NULL, append_text = NULL, proportions = TRUE, 
    pdf = FALSE, open = FALSE, multirowheaderlines = FALSE, 
    grid_num_letters = TRUE, custom_numbering = NULL, logging = FALSE) {
    
    data_summary$results <- lapply(data_summary$results, rm_inserts, theme)
    
    results <- reformatLatexResults(data_summary, proportions = proportions, theme = theme)
    
    headers <- lapply(seq_along(data_summary$results), function(i) {
        toplineHeader(data_summary$results[[i]], page_width = 6.5, row_label_width = theme$format_label_column$col_width,
            num = if (!is.null(custom_numbering)) custom_numbering[i] else i, theme = theme)
    })
    
    footers <- lapply(data_summary$results, toplineFooterDef)
    bodies <- lapply(results, function(x) 
        latexTable.body(x$Results, autorownames = TRUE, crosstabs = FALSE))
    
    tables <- sapply(ltranspose(list(headers, bodies, footers), use_names = TRUE), 
        function(x) paste(c(x, if (theme$one_per_sheet) { "\\clearpage" }), collapse = "\n"))
    out <- c(tables, append_text)
    
    latexHeadData <- latexHead(theme = theme, title = title, subtitle = subtitle, crosstabs = FALSE)
    # latexHeadData <- latexHeadT(title = title, font_size = font_size, theme = theme, subtitle = subtitle)
    latexStartData <- latexStart(table_of_contents = table_of_contents, sample_desc = sample_desc, field_period = field_period, moe = moe,
        font_size = NULL, crosstabs = FALSE)
    latexFootData <- latexFootT()
    out <- c(latexHeadData, latexStartData, out, latexFootData)
    
    if (!is.null(filename)) {
        filename <- paste0(filename, ".tex")
        cat(out, sep = "\n", file = filename)
        if (pdf) {
            if (logging) { print("PDF-ing") }
            pdflatex(filename, open, path.to.pdflatex = Sys.which("pdflatex"))
        }
    }
    
    return(invisible(data_summary))
}

toplineHeader <- function(x, page_width, num = NULL, row_label_width = 1.5, padding = 1, use_heuristic = TRUE, theme) {
    UseMethod("toplineHeader", x)
}

#' @export
toplineHeader.default <- function(var, page_width, num = NULL, row_label_width = 1.5, padding = 1, use_heuristic = TRUE, theme) {
    tab_definition <- paste0("\\begin{longtable}{p{0.3in}p{5.5in}}")
    toplineTableDef(var, page_width, num, tab_definition, header_row = "\n", theme = theme)
}

#' @export
toplineHeader.ToplineCategoricalArray <- function(var, page_width, num = NULL, row_label_width = 1.5, padding = 0.25, use_heuristic = TRUE, theme) {
    header_row <- "\n"
    col_names <- sapply(var$inserts_obj, name)
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
    header_row <- paste("\\\\", header_row, "& &", paste(escM(col_names), collapse = " & "), "\\\n")
    col.header <- paste("B{\\centering}{", col_width, "}", sep = "")
    col.header <- paste(rep(col.header, col_names_len), collapse = "")
    tab_definition <- paste0("\\begin{longtable}{@{\\extracolsep{\\fill}}p{0.1in}B{\\raggedright}{", row_label_width,
        "in}", col.header, "}")
    
    toplineTableDef(var, page_width, num, tab_definition, header_row, theme)
}

toplineTableDef <- function(var, page_width, num, tab_definition, header_row, theme) {
    var_info <- var_header(var, theme)
    if (length(var_info) == 0) var_info <- "\\color{gray}{404}"
    is_array <- is(var, "ToplineCategoricalArray")
    headtext <- if (is.null(theme$latex_headtext)) "" else theme$latex_headtext
    foottext <- if (is.null(theme$latex_foottext)) "" else theme$latex_foottext
    if (headtext %in% "tbc") headtext <- "continued from previous page"
    if (foottext %in% "tbc") foottext <- "continued on the next page \\dots"
    return(paste("\\begin{center}\n",
        tab_definition, "\n",
        "\t\\addcontentsline{lot}{table}{", escM(var_info[[1]]), "}\n",
        "\t\\colorbox{gray}{\n",
        "\t\\parbox{6.5in}{", paste(sapply(names(var_info), function(info_name)
            latexDecoration(escM(var_info[[info_name]]), theme[[info_name]],
                scriptsize = FALSE)), collapse = "\\\\ \n\t"), "}}\\\\\\",
        header_row,
        "\\endfirsthead\n",
        "\\multicolumn{", if (is_array) { nrow(var$crosstabs$Results$`___total___`$base) + 2 
            } else { 2 }, "}{c}{",
        "\\textit{", headtext, "}} \\\\",
        header_row,
        "\\endhead\n",
        "\\multicolumn{", if (is_array) { nrow(var$crosstabs$Results$`___total___`$base) + 2 
            } else { 2 }, "}{c}{",
        "\\textit{", foottext, "}} \\\\ \n",
        "\\endfoot\n",
        "\\endlastfoot\n", sep = ""))
}

toplineFooterDef <- function(var) {
    return(paste0(
        "\\end{longtable}\n",
        "\\end{center}"))
}


latexFootT <- function() return("\\end{hyphenrules} \n \\end{document}\n")



