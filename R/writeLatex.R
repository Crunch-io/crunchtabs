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
#' @param multirowheaderlines logical. Should banners allow multi-row headlines? **
#' @param grid_num_letters logical. Should each layer of a \code{categorical_array} variable (a "grid" question)
#' have the same number with consecutive letters appended? Defaults to \code{TRUE}. **
#' @param custom_numbering A vector of custom values to be used for numbering banner tables.
#' Defaults to \code{NULL} - default numbering scheme is used.
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
    pdf = FALSE, open = FALSE, multirowheaderlines = FALSE, 
    grid_num_letters = TRUE, custom_numbering = NULL, logging = FALSE) {
    
    if (pdf && is.null(filename)) {
        stop("Please provide a file name to generate PDF output.")
    }
    
    if (!is.null(custom_numbering) && length(custom_numbering) != length(data_summary$results)) {
        stop("The length of `custom_numbering` provided (", length(custom_numbering),
            ") is not equal to the length of the results (", length(data_summary$results), ").")
    }

    theme_validator(theme)
    
    UseMethod("writeLatex", data_summary)
}


#' @export
writeLatex.default <- function(data_summary, ...) {
    stop("writeLatex doesn't support objects of class '",
        collapse_items(class(data_summary)), "'.")
}

latexTable.body <- function(df, autorownames = FALSE, crosstabs) {
    
    body <- df$data_list$body
    if (!crosstabs || length(intersect(c("totals_row", "unweighted_n", "weighted_n"), c(df$top, df$bottom))) == 0) { 
        summary <- NULL 
    } else { 
        summary <- do.call(rbind, lapply(intersect(c("totals_row", "unweighted_n", "weighted_n"), c(df$top, df$bottom)), function(x) {
            df$data_list[[x]]
        }))
    }

    if (autorownames) {
        if (!is.null(rownames(body))) body <- data.frame(rownames(body), body, stringsAsFactors = FALSE)
        if (!is.null(rownames(summary))) summary <- data.frame(rownames(summary), summary, stringsAsFactors = FALSE)
    }
    for (j in 1:ncol(body)) body[, j] <- escM(body[, j])
    if (!is.null(summary)) for (j in 1:ncol(summary)) summary[, j] <- escM(summary[, j])

    collapsestring <- "\\\\\n"
    
    sepstring <- if (!crosstabs && ncol(body) == 2) { " \\hspace*{0.15em} \\dotfill " } else { " & " }
    if (!crosstabs && ncol(body) == 2) body[[1]] <- paste0("\\hspace*{0.5in}", body[[1]])
    if (!crosstabs) {
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

getFilterText <- function(var_summary) {
    filtertext <- getNotes(var_summary)
    if (!is.na(filtertext) && filtertext != "") {
        return(paste("\\\\ \n \\scriptsize { \\itshape ", escM(filtertext), "}"))
    }
}


latexHead <- function (theme, title, subtitle, crosstabs) {
    poss_fonts <- c("bookman","charter","courier","fourier","helvet","lmodern","lmr","palatino","tgadventor",
        "tgbonum","tgcursor","tgheros","tgpagella","tgschola","tgtermes","times","utopia")
    if (is.null(theme$font) || !tolower(theme$font) %in% poss_fonts) {
        theme$font <- "helvet"
        warning("theme$font must be in ", paste0(poss_fonts, collapse = ", "), 
            ". It has been set to `helvet`.", .call = FALSE)
    }
    
    paste0("\\documentclass[", if (crosstabs) { "landscape" } else { paste0(theme$font_size, "pt") }, "]{article}\n",
        "\\usepackage[pdftex]{graphicx}\n",
        "\\usepackage[utf8]{inputenc}\n",
        "\\usepackage{fancyhdr}\n",
        "\\usepackage{sfmath}\n",
        "\\usepackage{comment}\n",
        "\\usepackage[T1]{fontenc}\n",
        "\\usepackage[pdftex=true, pdftoolbar=true, pdfmenubar=true, pdfauthor = {}, pdfcreator = {PDFLaTeX}, pdftitle = {}, colorlinks=true, urlcolor=blue, linkcolor=blue, citecolor=blue, implicit=true, hypertexnames=false]{hyperref}\n",
        "\\usepackage[scaled]{", theme$font, "}\n",
        "\\renewcommand*\\familydefault{\\sfdefault}\n",
        "\\usepackage{booktabs, dcolumn, longtable}\n",
        "\\usepackage[top=0.6in, bottom=0.6in, left=", if (crosstabs) { 0.5 } else { 1 }, 
            "in, right=", if (crosstabs) { 0.5 } else { 1 }, "in, includeheadfoot]{geometry}\n",
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
        "\\fancyhead[L]{{\\Large {\\bf ",
        if (is.null(title)) { "" } else { escM(title) }, "}}",
        if (is.null(subtitle)) { "" } else { paste(" \\\\", escM(subtitle)) },
        "}\n",
        if (!is.null(theme$logo$file)) paste0("\\fancyhead[R]{\\includegraphics[scale=.4]{", theme$logo$file, "}}\n"),
        if (crosstabs) "\\newcolumntype{d}{D{.}{.}{3.2}}\n", ##
        if (crosstabs) "\\newcolumntype{g}{D{\\%}{\\%}{5.0}}\n", ##
        if (!crosstabs) "\\usepackage{float}\n", ##
        if (!crosstabs) "\\usepackage{marginnote}\n", ##
        if (!crosstabs) "\\setlength\\extrarowheight{2pt}\n", ##
        if (!crosstabs) "\\newlength\\mywidth\n", ##
        if (!crosstabs) "\\setlength\\mywidth{3.5in}\n", ##
        if (!crosstabs) "\\usepackage{caption}\n", ##
        if (!crosstabs) "\\captionsetup[table]{labelformat=empty}\n", ##
        if (!crosstabs) "\\renewcommand*{\\marginfont}{\\scriptsize\\itshape}", ##
        "\\fancyfoot{}\n",
        "\\fancyfoot[R]{\\thepage}\n",
        "\\newcommand{\\PreserveBackslash}[1]{\\let\\temp=\\",
        "\\#1\\let\\",
        "\\=\\temp}\n",
        "\\let\\PBS=\\PreserveBackslash\n")
}

latexStart <- function(table_of_contents, sample_desc, field_period, moe, font_size, crosstabs) {
    if (!is.null(sample_desc)) { sample_desc <- paste("Sample  & ", sample_desc, "\\\\ \n ") }
    if (!is.null(moe)) { moe <- paste("Margin of Error &  $\\pm ", round(100 * moe, digits = 1), "\\%$ \\\\ \n") }
    if (!is.null(field_period)) { field_period <- paste("Conducted  & ", field_period, "\\\\ \n") }
    if (!is.null(font_size)) { font_size <- paste0("{\\", font_size, "\n") }
    return(paste0("\\begin{document}\n", 
        if (!crosstabs) "\\begin{hyphenrules}{nohyphenation}\n", 
        "\\begin{tabular}{ll}\n",
        sample_desc, field_period, moe, 
        "\\end{tabular}\n", 
        if (table_of_contents) { "\\listoftables\n\\newpage\n\n" } else { "\n\n" }, 
        font_size,
        "\\setlength{\\LTleft}{0pt}\n",
        "\\setlength{\\LTright}{\\fill}\n",
        "\\setlength{\\LTcapwidth}{\\textwidth}\n\n\n",
        "%% here's where individual input starts %%\n\n\n \\vspace{.25in} \n\n"))
}

