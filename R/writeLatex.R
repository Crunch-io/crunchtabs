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
    
    UseMethod("writeLatex", data_summary)
}


#' @export
writeLatex.default <- function(data_summary, ...) {
    stop("writeLatex doesn't support objects of class '",
        collapse_items(class(data_summary)), "'.")
}

latexTable.body <- function(df, topline) {
    
    body <- df$data_list$body
    if (topline || length(intersect(c("totals_row", "unweighted_n", "weighted_n"), c(df$top, df$bottom))) == 0) { 
        summary <- NULL 
    } else { 
        summary <- do.call(rbind, lapply(intersect(c("totals_row", "unweighted_n", "weighted_n"), c(df$top, df$bottom)), function(x) {
            df$data_list[[x]]
        }))
    }

    if (!is.null(rownames(body))) body <- data.frame(rownames(body), body, stringsAsFactors = FALSE)
    if (!is.null(rownames(summary))) summary <- data.frame(rownames(summary), summary, stringsAsFactors = FALSE)
    for (j in 1:ncol(body)) body[, j] <- escM(body[, j])
    if (!is.null(summary)) for (j in 1:ncol(summary)) summary[, j] <- escM(summary[, j])

    collapsestring <- "\\\\\n"
    
    sepstring <- if (topline && ncol(body) == 2) { " \\hspace*{0.15em} \\dotfill " } else { " & " }
    if (topline) body[[1]] <- paste0(" & ", body[[1]])
    if (topline) {
        return(paste(paste(apply(rbind(body, summary), 1, paste, collapse = sepstring), collapse = collapsestring),
            collapsestring))
    } else {
        body <- paste(paste(apply(body, 1, paste, collapse = sepstring), collapse = collapsestring), collapsestring)
        summary <- paste(paste(apply(summary, 1, paste, collapse = sepstring), collapse = collapsestring), collapsestring)
        return(paste(body, "\\midrule", summary))
    }
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
        "\\fancyhead[L]{{\\Large {\\bf ", title, "}}", subtitle, "}\n",
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
        "\\usepackage[titles]{tocloft}\n", #TODO: make this work
        "\\newcommand{\\cftchapfont}{", fontLine(theme$font_size), "}\n",
        latexDecoration("format_var_description", theme$format_var_description),
        latexDecoration("format_var_name", theme$format_var_name),
        latexDecoration("format_var_alias", theme$format_var_alias),
        latexDecoration("format_var_filtertext", theme$format_var_filtertext),
        latexDecoration("format_var_subname", theme$format_var_subname),
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

latexDecoration <- function(item_name, item_theme) {
    item <- "#1"
    if (!is.null(item_theme$decoration)) { 
        if (any(c("underline", "underline2") %in% item_theme$decoration)) { item <- paste0("\\underline{", item, "}") }
        if ("italic" %in% item_theme$decoration) { item <- paste0("\\textit{", item, "}") }
        if ("bold" %in% item_theme$decoration) { item <- paste0("\\textbf{", item, "}") }
    }
    if (!is.null(item_theme$font_size)) {
        item <- paste0(fontLine(item_theme$font_size), item)
    }
    return(paste0("\\newcommand{\\", gsub("_", "", item_name), "}[1]{", item, "}\n"))
}

latexTableFoot <- function(topline) {
    if (topline) { 
        return("\n\\end{longtable}\n\\end{center}\n\n")
    } else {
        return("\n\\end{longtable}\n\n")
    }
}

latexDocFoot <- function() { return("\n}\n\\end{document}\n") }
    
    