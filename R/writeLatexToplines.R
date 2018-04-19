#' @export
writeLatex.Toplines <- function(data_summary, filename = getName(data_summary), proportions = TRUE, 
    title = getName(data_summary), subtitle = NULL, sample_desc = NULL, field_period = NULL, moe = NULL,
    table_of_contents = FALSE, append_text = NULL,
    pdf = FALSE, open = FALSE,
    row_label_width = 1.5,
    multirowheaderlines = FALSE,
    clearpage = TRUE, grid_num_letters = TRUE, custom_numbering = NULL,
    theme = theme_default(), logging = FALSE) {
    
    results <- reformatLatexResults(data_summary, proportions = proportions, theme = theme)
    
    headers <- lapply(seq_along(data_summary$results), function(i) {
        toplineHeader(data_summary$results[[i]], page_width = 6.5, row_label_width = row_label_width,
            num = if (!is.null(custom_numbering)) custom_numbering[i] else i, theme = theme)
    })
    
    footers <- lapply(data_summary$results, toplineFooterDef)
    bodies <- lapply(results, function(x) 
        latexTable.body(x$Results, autorownames = TRUE, crosstabs = FALSE))
    
    tables <- sapply(ltranspose(list(headers, bodies, footers), use_names = TRUE), function(x) paste(x, collapse = "\n"))
    
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
    tab_definition <- paste0("\\begin{tabular}{p{", page_width - padding, "in}}")
    toplineTableDef(var, page_width, num, tab_definition, header_row = "\n", theme = theme)
}

#' @export
toplineHeader.ToplineCategoricalArray <- function(var, page_width, num = NULL, row_label_width = 1.5, padding = 0.25, use_heuristic = TRUE, theme) {
    header_row <- "\n"
    col_names <- sapply(var$inserts_obj, name)
    if (is.null(theme$format_headers)) { col_names <- col_names[-c(which(var$inserts %in% "Heading"))] }
    if (is.null(theme$format_subtotals)) { col_names <- col_names[-c(which(var$inserts %in% "Subtotal"))] }
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
    
    toplineTableDef(var, page_width, num, tab_definition, header_row, theme)
}

toplineTableDef <- function(var, page_width, num, tab_definition, header_row, theme) {
    var_info <- var_header(var, theme)
    for (info_name in names(var_info)) {
        var_info[[info_name]] <- latexDecoration(escM(var_info[[info_name]]), theme[[info_name]],
            scriptsize = info_name != names(var_info)[1])
    }
    print(var_info)
    if (length(var_info) == 0) var_info <- "\\color{gray}{404}"
    return(paste("\\begin{table}[H]
        \\addcontentsline{lot}{table}{", escM(getName(var)), "}
        \\colorbox{gray}{
        \\parbox{",
        page_width, "in}{", paste0(unlist(var_info), collapse = ""), "}}
        \\begin{center}", tab_definition,
        "\n", header_row, "\n", sep = ""))
}

latexDecoration <- function(item, item_theme, scriptsize) {
    if (!is.null(item_theme$decoration)) { 
        if ("bold" %in% item_theme$decoration) { item <- paste0("\\textbf{", item, "}") }
        if (any(c("underline", "underline2") %in% item_theme$decoration)) { item <- paste0("\\underline{", item, "}") }
        if ("italic" %in% item_theme$decoration) { item <- paste0("\\textit{", item, "}") }
    }
    if (scriptsize) {
        item <- paste0("\\\\ \n \\scriptsize{", item, "}")
    }
    return(item)
}

#' toplineFooter <- function(x) {
#'     UseMethod("toplineFooter", x)
#' }
#' 
#' #' @export
#' toplineFooter.default <- function(x) {
#'     toplineFooterDef(is_grid = FALSE)
#' }
#' 
#' #' @export
#' toplineFooter.ToplineCategoricalArray <- function(var) {
#'     toplineFooterDef(is_grid = TRUE)
#'     
#' }

toplineFooterDef <- function(var) {
    paste0("\\end{", paste0("tabular", if (is(var, "ToplineCategoricalArray")) { "*" } else { "" }), "}
        \\end{center}
        \\end{table}")
}


latexFootT <- function() "\\end{hyphenrules} \n \\end{document}\n"

# latexStartT <- function(table_of_contents, sample_desc, field_period, moe) {
#     moe_text <- ""
#     if (!is.null(sample_desc))
#         sample_desc <- paste("Sample  & ", sample_desc, "\\\\ \n ")
#     if (!is.null(moe))
#         moe_text <- paste("Margin of Error &  $\\pm ", round(100 * moe, digits = 1),
#             "\\%$ \\\\ \n")
#     if (!is.null(field_period))
#         field_period <- paste("Conducted  & ", field_period, "\\\\ \n")
#     paste("\\begin{document}\n", "\\begin{hyphenrules}{nohyphenation}\n", "\\begin{tabular}{ll}\n",
#         sample_desc, field_period, moe_text, "\\end{tabular}\n", ifelse(table_of_contents,
#             "\\listoftables\n\n\n", "\n\n"), "%% here's where individual input starts %%\n\n\n \\vspace{.25in} \n\n",
#         sep = "")
# }

ltranspose <- function(l, use_names) {
    if (length(unique(sapply(l, length))) > 1)
        stop("All nested lists must be of equal length.")
    if (use_names && !is.null(names(l[[1]]))) {
        return(sapply(names(l[[1]]), function(x) lapply(l, function(y) y[[x]])))
    } else {
        return(lapply(seq_along(l[[1]]), function(x) sapply(l, function(y) y[[x]], simplify = FALSE)))
    }
}

