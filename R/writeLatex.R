#' Generate LaTeX Reports: Toplines and Banners
#'
#' \code{writeLatex} produces publication-quality LaTeX reports:
#' Toplines (one-way frequency tables) or Banners (cross tabulations).
#'
#' @param data_summary An object of class \code{Toplines} or \code{Crosstabs}.
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
#' 
#' @param theme
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
#' @export
writeLatex <- function(data_summary, theme = themeDefaultLatex(), 
    filename = getName(data_summary), title = getName(data_summary), 
    subtitle = NULL, table_of_contents = FALSE, sample_desc = NULL, 
    field_period = NULL, moe = NULL, append_text = NULL, proportions = TRUE, 
    pdf = FALSE, open = FALSE, logging = FALSE) {
    
    if (pdf && is.null(filename)) {
        stop("Please provide a file name to generate PDF output.")
    }
    theme_validator(theme)
    
    wrong_class_error(data_summary, "CrunchTabs", "data_summary")
    if (!any(c("Toplines", "Crosstabs") %in% class(data_summary))) {
        stop("The expected class for `data_summary` is either Toplines, CrunchTabs or Crosstabs CrunchTabs, not ", collapse_items(class(data_summary)))
    }
    
    topline <- is(data_summary, "Toplines")
    if (is.null(theme$font_size)) { theme$font_size <- 12 }
    theme$proportions <- proportions
    
    headers <- lapply(data_summary$results, tableHeader, theme = theme)
    
    data_summary$results <- lapply(data_summary$results, rm_inserts, theme)
    results <- reformatLatexResults(data_summary, proportions = proportions, theme = theme)
    bodies <- lapply(results, function (x) 
        sapply(x, latexTable.body, theme = theme, topline = topline))
    
    out <- c(
        latexDocHead(theme = theme, title = title, subtitle = subtitle, topline = topline),
        if (!topline) sapply(seq_along(data_summary$banner), function (j) {
            longtableHeadFootB(data_summary$banner[[j]], num = j, page_width = 9, 
                theme = theme)
        }),
        latexStart(table_of_contents = table_of_contents, sample_desc = sample_desc, 
            field_period = field_period, moe = moe, font_size = theme$font_size),
        sapply(seq_along(data_summary$results), function(i) {
            c(paste(headers[[i]], bodies[[i]], latexTableFoot(topline = topline),
                sep="\n", collapse="\n"),
                if (theme$one_per_sheet) { "\\clearpage" })
        }),
        append_text,
        latexDocFoot()
    )
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


#' @export
writeLatex.default <- function(data_summary, ...) {
    stop("writeLatex doesn't support objects of class '",
        collapse_items(class(data_summary)), "'.")
}

latexTable.body <- function(df, theme, topline) {
    
    data <- df$data_list
    for (nm in intersect(c("body", "totals_row"), names(data))) {
        data[[nm]][] <- round(data[[nm]], theme$digits)
        data[[nm]][] <- format(data[[nm]], nsmall=theme$digits, big.mark=",")
        data[[nm]][] <- apply(data[[nm]], 2, trimws)
        if (theme$proportions) { data[[nm]][] <- apply(data[[nm]], 2, paste0, "%") }
    }
    for (nm in intersect(c("unweighted_n", "weighted_n"), names(data))) {
        nm2 <- paste0("format_", nm)
        data[[nm]][] <- trimws(format(data[[nm]], big.mark=","))
        if (theme[[nm2]]$latex_add_parenthesis) {
            data[[nm]][] <- apply(data[[nm]], 2, paste_around, "(", ")")
        }
        if (!is.null(theme[[nm2]]$latex_adjust)) {
            data[[nm]][] <- apply(data[[nm]], 2, paste_around, 
                paste0("\\multicolumn{1}{", theme[[nm2]]$latex_adjust, "}{"), "}")
        }
    }
    
    mask_vars <- c("totals_row", "means", "medians")
    if (any(df$min_cell_body)) {
        if (!is.null(theme$format_min_base$mask)) {
            data$body[df$min_cell_body] <- theme$format_min_base$mask
            for (nm in intersect(mask_vars, names(data))) {
                data[[nm]][, df$min_cell] <- theme$format_min_base$mask
            }
        }
        for (i in which(colSums(df$min_cell_body) != 0)) {
            data$body[df$min_cell_body[,i], i] <- latexDecoration(data$body[df$min_cell_body[,i], i], theme$format_min_base)
            for (nm in intersect(mask_vars, names(data))) {
                data[[nm]][, df$min_cell] <- latexDecoration(data[[nm]][, df$min_cell], theme$format_min_base)
            }
        }
    }
    
    data <- lapply(data, function(dt) {
        matrix(apply(data.frame(rownames(dt), dt, stringsAsFactors = FALSE), 2, escM), 
            nrow = nrow(dt))
    })
    
    for (nm in intersect(gsub("format_", "", names(theme)), names(data))) {
        data[[nm]][] <- apply(data[[nm]], 2, latexDecoration, theme[[paste0("format_", nm)]])
    }
    
    for (i in which(df$inserts %in% c("Heading"))) {
        data$body[i, 2:ncol(data$body)] <- ""
        data$body[i, ] <- latexDecoration(data$body[i, ], theme$format_headers)
    }
    for (i in which(df$inserts %in% c("Subtotal"))) {
        data$body[i, ] <- latexDecoration(data$body[i, ], theme$format_subtotals)
    }
    
    # body <- data$body
    # summary <- do.call(rbind, data[intersect(c("means", "totals_row", "unweighted_n", "weighted_n"), df$data_order)])
    # # if (topline || length(intersect(c("totals_row", "unweighted_n", "weighted_n"), df$data_order)) == 0) {
    # #     summary <- NULL
    # # } else {
    # #     summary <- do.call(rbind, lapply(intersect(c("totals_row", "unweighted_n", "weighted_n"), df$data_order), function(x) {
    # #         data[[x]]
    # #     }))
    # # }
    
    collapsestring <- "\\\\\n"
    
    sepstring <- if (topline && ncol(data$body) == 2) { 
        " \\hspace*{0.15em} \\dotfill " 
    } else { " & " }
    
    data <- lapply(data, function(dt) {
        paste(paste(paste0(if (topline) " & ", apply(dt, 1, paste, collapse = sepstring)),
            collapse = collapsestring), collapsestring)
    })
    
    if (is(df, "ToplineCategoricalArray")) df$data_order <- "body"
    a <- paste(paste0(data[intersect(c("body", "medians", "means"), df$data_order)], 
        collapse = ""), if (!topline) "\\midrule",
        paste0(data[intersect(c("totals_row", "weighted_n", "unweighted_n"), df$data_order)], 
            collapse = ""))
    # 
    # if (topline) {
    #     bod <- do.call(rbind, list(body, if (!is(df, "ToplineCategoricalArray")) summary))
    #     bod[,1] <- paste0(" & ", bod[,1])
    #     b <- (paste(paste(apply(bod, 1, paste, collapse = sepstring), collapse = collapsestring),
    #         collapsestring))
    # } else {
    #     body <- paste(paste(apply(body, 1, paste, collapse = sepstring), collapse = collapsestring), collapsestring)
    #     if (!is.null(summary)) summary <- paste(paste(apply(summary, 1, paste, collapse = sepstring), collapse = collapsestring), collapsestring)
    #     b <- (paste(body, "\\midrule", summary))
    # }
    return(a)
}


escM <- function(str) {
    str <- gsub("^ *(\\[)", "\\\\hspace\\*\\{0in\\}\\1", gsub("([#$%&_])", "\\\\\\1", str))
    str <- gsub("[\u00A3\uFFE1]", "\\\\pounds", str)
    str
}

# getFilterText <- function(var_summary) {
#     filtertext <- getNotes(var_summary)
#     if (!is.na(filtertext) && filtertext != "") {
#         return(paste("\\\\ \n \\scriptsize { \\itshape ", escM(filtertext), "}"))
#     }
# }


latexDocHead <- function (theme, title, subtitle, topline) {
    poss_fonts <- c("bookman","charter","courier","fourier","helvet","lmodern",
        "lmr","palatino","tgadventor","tgbonum","tgcursor","tgheros","tgpagella",
        "tgschola","tgtermes","times","utopia")
    if (is.null(theme$font) || !tolower(theme$font) %in% poss_fonts) {
        theme$font <- "helvet"
        warning("theme$font must be in ", paste0(poss_fonts, collapse = ", "), 
            ". It has been set to `helvet`.", .call = FALSE)
    }
    
    title <- if (is.null(theme$format_title) || is.null(title)) { "" 
        } else { escM(title) }
    subtitle <- if (is.null(theme$format_subtitle) || is.null(subtitle)) { "" 
        } else { paste(" \\\\", escM(subtitle)) }
    
    bdr <- if (topline) { 1 } else { 0.5 }
    logo <- if (!is.null(theme$logo$file)) { 
        paste0("\\fancyhead[R]{\\includegraphics[scale=.4]{", theme$logo$file, "}}\n") }

    paste0("\\documentclass", if (!topline) { "[landscape]" }, "{article}\n",
        "\\usepackage[pdftex]{graphicx}\n",
        "\\usepackage[utf8]{inputenc}\n",
        "\\usepackage{fancyhdr}\n",
        "\\usepackage{sfmath}\n",
        "\\usepackage{comment}\n",
        "\\usepackage[T1]{fontenc}\n",
        "\\usepackage[pdftex=true, pdftoolbar=true, pdfmenubar=true, pdfauthor = {},",
            "pdfcreator = {PDFLaTeX}, pdftitle = {}, colorlinks=true, urlcolor=blue,", 
            "linkcolor=blue, citecolor=blue, implicit=true, hypertexnames=false]{hyperref}\n",
        "\\usepackage[scaled]{", theme$font, "}\n",
        "\\renewcommand*\\familydefault{\\sfdefault}\n",
        "\\usepackage{booktabs, dcolumn, longtable}\n",
        "\\usepackage[top=0.6in, bottom=0.6in, left=", bdr, 
            "in, right=", bdr, "in, includeheadfoot]{geometry}\n",
        "\\usepackage{array}\n",
        "\\usepackage[english]{babel}\n",
        "\\newcolumntype{B}[2]{>{#1\\hspace{0pt}\\arraybackslash}b{#2}}\n",
        "\\setlength{\\parindent}{0pt}\n",
        "\\usepackage[dvipsnames]{color}\n",
        "\\definecolor{gray}{gray}{0.85}\n",
        "\\pagestyle{fancy}\n",
        "\\renewcommand{\\headrulewidth}{0pt}\n",
        "\\renewcommand{\\footrulewidth}{0pt}\n",
        "\\fancyhead{}\n",
        "\\fancyhead[L]{{", latexDecoration(title, theme$format_title), "}", 
            latexDecoration(subtitle, theme$format_subtitle), "}\n",
        logo,
        "\\newcolumntype{d}{D{.}{.}{3.2}}\n", #!topline
        "\\newcolumntype{g}{D{\\%}{\\%}{5.0}}\n", #!topline
        "\\usepackage{float}\n", #topline
        "\\usepackage{marginnote}\n", #topline
        "\\setlength\\extrarowheight{2pt}\n", #topline
        "\\newlength\\mywidth\n", #topline
        "\\setlength\\mywidth{3.5in}\n", #topline
        "\\usepackage{caption}\n", #topline
        "\\captionsetup[table]{labelformat=empty}\n", #topline
        "\\renewcommand*{\\marginfont}{\\scriptsize\\itshape}\n", #topline
        "\\fancyfoot{}\n",
        "\\fancyfoot[R]{\\thepage}\n",
        "\\newcommand{\\PreserveBackslash}[1]{\\let\\temp=\\\\#1\\let\\\\=\\temp}\n",
        "\\let\\PBS=\\PreserveBackslash\n",
        "\\newcommand{\\longtablesep}{\\endfirsthead \\multicolumn{2}{c}{\\textit{", 
            escM(theme$latex_headtext), "}} \\\\ \\endhead \\multicolumn{2}{c}{\\textit{", 
            escM(theme$latex_foottext), "}} \\\\ \\endfoot \\endlastfoot}\n", 
        "\\usepackage[titles]{tocloft}\n",
        "\\newcommand{\\cftchapfont}{", fontLine(theme$font_size), "}\n",
        "\\newcommand{\\formatvardescription}[1]{", latexDecoration("#1", theme$format_var_description), "}\n",
        "\\newcommand{\\formatvarname}[1]{", latexDecoration("#1", theme$format_var_name), "}\n",
        "\\newcommand{\\formatvaralias}[1]{", latexDecoration("#1", theme$format_var_alias), "}\n",
        "\\newcommand{\\formatvarfiltertext}[1]{", latexDecoration("#1", theme$format_var_filtertext), "}\n",
        "\\newcommand{\\formatvarsubname}[1]{", latexDecoration("#1", theme$format_var_subname), "}\n",
        "\n\n\n\n\n")
}

fontLine <- function(size) { paste0("\\fontsize{", size, "}{", size * 1.5, "}") }

latexStart <- function(table_of_contents, sample_desc, field_period, moe, font_size) {
    if (!is.null(sample_desc)) { sample_desc <- paste("Sample  & ", sample_desc, "\\\\ \n ") }
    if (!is.null(moe)) { moe <- paste("Margin of Error &  $\\pm ", round(100 * moe, digits = 1), "\\%$ \\\\ \n") }
    if (!is.null(field_period)) { field_period <- paste("Conducted  & ", field_period, "\\\\ \n") }
    return(paste0("\\begin{document}\n", 
        "\\begin{tabular}{ll}\n",
        sample_desc, field_period, moe, 
        "\\end{tabular}\n", 
        if (table_of_contents) { "\\listoftables\n\\newpage\n\n" } else { "\n\n" }, 
        "{", #fontLine(font_size), "\n", #TODO: make this work
        "\\setlength{\\LTleft}{0pt}\n",
        "\\setlength{\\LTright}{\\fill}\n",
        "\\setlength{\\LTcapwidth}{\\textwidth}\n\n\n",
        "%% here's where individual input starts %%\n\n\n \\vspace{.25in} \n\n"))
}

latexDecoration <- function(item, item_theme) {
    if (!is.null(item_theme$decoration)) { 
        if (any(c("underline", "underline2") %in% item_theme$decoration)) { item <- paste0("\\underline{", item, "}") }
        if ("italic" %in% item_theme$decoration) { item <- paste0("\\textit{", item, "}") }
        if ("bold" %in% item_theme$decoration) { item <- paste0("\\textbf{", item, "}") }
    }
    if (!is.null(item_theme$font_size)) {
        item <- paste0(fontLine(item_theme$font_size), item)
    }
    return(item)
}

tableHeader <- function(x, theme) {
    UseMethod("tableHeader", x)
}

latexTableFoot <- function(topline) {
    if (topline) { 
        return("\n\\end{longtable}\n\\end{center}\n\n")
    } else {
        return("\n\\end{longtable}\n\n")
    }
}

latexTableName <- function(var, theme) {
    var_info <- var_header(var, theme)
    col <- if (is.null(theme[[names(var_info)[[1]]]]$background_color)) { "white" 
        } else { theme[[names(var_info)[[1]]]]$background_color }
    if (!is.null(var_info$format_var_subname) && names(var_info)[1] != "format_var_subname") {
        var_info[[1]] <- paste0(var_info[[1]], if (!is.null(var_info$format_var_subname))
            paste0(" — ", var_info$format_var_subname))
        var_info$format_var_subname <- NULL
    }
    if (length(var_info) == 0) var_info <- list(format_var_name = paste0("\\color{", col, "}{404}"))
    paste("\\colorbox{", col, "}{\n",
        "\\addcontentsline{lot}{table}{ ", escM(var_info[[1]]), "}\n",
        "\\parbox{", if (is(var, "ToplineVar")) { "6.5" } else { "9" }, "in}{\n",
        paste0("\\", gsub("_", "", names(var_info)), "{", escM(var_info), "}", collapse = "\\\\ \n"),
        "}} \\\\", sep="")
}

latexDocFoot <- function() { return("\n}\n\\end{document}\n") }
    
    