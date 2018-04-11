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
#' @param row_label_width Width of the row label comuln. Defaults to 1.5in. ***
#' @param multirowheaderlines logical. Should banners allow multi-row headlines? **
#' @param clearpage logical. Should every banner be on a separete page? **
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
writeLatex <- function(data_summary, filename = NULL, proportions = TRUE, 
    title = getName(data_summary), subtitle = NULL, sample_desc = "", field_period = "", moe = NULL,
    table_of_contents = FALSE, append_text = "",
    pdf = FALSE, open = FALSE,
    row_label_width = 1.5,
    multirowheaderlines = FALSE,
    clearpage = TRUE, grid_num_letters = TRUE, custom_numbering = NULL,
    theme = theme_default()) {
    
    if (pdf && is.null(filename)) {
        stop("Please provide a file name to generate PDF output.")
    }
    
    if (!is.null(custom_numbering) && length(custom_numbering) != length(data_summary$results)) {
        stop("The length of `custom_numbering` provided (", length(custom_numbering),
            ") is not equal to the length of the results (", length(data_summary$results), ").")
    }
    
    UseMethod("writeLatex", data_summary)
}


#' @export
writeLatex.default <- function(data_summary, ...) {
    stop("writeLatex doesn't support objects of class '",
        collapse_items(class(data_summary)), "'.")
}

latexTable.body <- function(df, rownames = FALSE, dotfill = FALSE, autorownames = FALSE,
    autocolnames = FALSE, esc = TRUE, colnames = NULL, summary.midrule = FALSE) {
    
    if (!is.list(df)) {
        body <- as.data.frame(df)
        summary <- NULL
    } else {
        body <- as.data.frame(do.call(cbind, lapply(seq_along(df), function(i) df[[i]]$data)))
        summary <- as.data.frame(do.call(cbind, lapply(seq_along(df), function(i) df[[i]]$bottom)))
    }
    
    if (autocolnames) {
        body <- if (is.null(colnames))
            rbind(c(names(body)), body) else rbind(colnames, body)
            # rbind(gsub(".", " ", names(body), fixed = TRUE), body) else rbind(colnames, body)
        if (!is.null(summary)) summary <- if (is.null(colnames))
            rbind(gsub(".", " ", names(summary), fixed = TRUE), summary) else rbind(colnames, summary)
    }
    if (autorownames) {
        if (!is.null(rownames(body))) body <- data.frame(rownames(body), body, stringsAsFactors = FALSE)
        if (!is.null(rownames(summary))) summary <- data.frame(rownames(summary), summary, stringsAsFactors = FALSE)
    }
    if (esc) {
        for (j in 1:ncol(body)) body[, j] <- escM(body[, j])
        if (!is.null(summary)) for (j in 1:ncol(summary)) summary[, j] <- escM(summary[, j])
    }
    collapsestring <- "\\\\\n"
    
    sepstring <- ifelse(dotfill && ncol(body) == 2, " \\hspace*{0.15em} \\dotfill ",
        " & ")
    if (!summary.midrule) {
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
    if (!theme$font %in% poss_fonts) {
        theme$font <- "helvet"
        warning("theme$font must be in ", paste0(poss_fonts, collapse = ", "), ". It has been set to `helvet`.")
    }
    
    paste0("\\documentclass[", ifelse(crosstabs, "landscape", paste0(theme$font_size, "pt")), "]{article}\n",
        "\\usepackage[pdftex]{graphicx}\n",
        "\\usepackage[utf8]{inputenc}\n",
        "\\usepackage{fancyhdr}\n",
        "\\usepackage{sfmath}\n",
        "\\usepackage[T1]{fontenc}\n",
        "\\usepackage[pdftex=true, pdftoolbar=true, pdfmenubar=true, pdfauthor = {}, pdfcreator = {PDFLaTeX}, pdftitle = {}, colorlinks=true, urlcolor=blue, linkcolor=blue, citecolor=blue, implicit=true, hypertexnames=false]{hyperref}\n",
        "\\usepackage[scaled]{", theme$font, "}\n",
        "\\renewcommand*\\familydefault{\\sfdefault}\n",
        "\\usepackage{booktabs, ", if (crosstabs) "dcolumn, ", "longtable}\n",
        "\\usepackage[top=0.6in, bottom=0.6in, left=1in, right=1in, includeheadfoot]{geometry}\n",
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
        ifelse(is.null(title), "", escM(title)), "}}",
        ifelse(is.null(subtitle), "", paste(" \\\\", escM(subtitle))),
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