
#' Generate LaTeX Reports: Toplines and Banners
#'
#' \code{writeLatex} produces publication-quality LaTeX reports:
#' Toplines (one-way frequency tables) or Banners (cross tabulations).
#'
#' @param data_summary An object of class \code{Toplines} or \code{Crosstabs}.
#' @param filename character. The name of the output file (without extension).
#' @param title An optional title. Defaults to the data summary title.
#' @param subtitle An optional character subtitle. Defaults to an empty string.
#' @param proportions logical. If \code{TRUE} the output report shows proportions,
#' if \code{FALSE} the report shows counts.
#' @param headtext An optional character string indicating what text should be
#' placed at the top of continuation tables. 'tbc' is a shortcut for 'to be
#' continued.'
#' @param foottext An optional character string indicating what text should be
#' placed at the bottom of continuation tables. 'tbc' is a shortcut for
#' 'continued from previous page.'
#' @param landscape logical. If \code{TRUE}, generate pages in the landscape
#' mode. Defaults to \code{TRUE} for Banners and \code{FALSE} for Toplines.
#' @param margin An optional argument to pass to the LaTeX package
#' \code{geometry}. Default is 'top=.6in, bottom=.6in, left=.5in, right=.5in,
#' includeheadfoot' for Banners and 'top=.6in, bottom=.6in, left=1in,
#' right=1in, includeheadfoot' for Toplines.
#' @param table_of_contents logical. Should a list of tables be included at the start
#' of the report Defaults to \code{FALSE}.
#' @param font A character string specifying the font package to use in LaTeX.
#' @param font_size Font size points for toplines and fontsize name for banners.
#' Defaults to '12' pt font for toplines and 'small' for banners
#' @param sample_desc A character string describing the sample.
#' @param field_period A character string describing the field period.
#' @param moe An optional numeric margin of error.
#' @param pdf logical. Compile LaTeX using pdflatex? Implemented only on MacOS/Linux.
#' @param open logical. If PDF document was produced, open it with
#' the default application? Only implemented for MacOS.
#' @param returndata logical. If \code{TRUE}, a processed data that was used to produce
#' the report is returned.
#' @param digits integer. Number of decimal digits to use for rounding.
#' Defaults to 0.
#' @param tableonly logical. If \code{TRUE}, function writes out only the LaTeX
#' content within the \code{table} environments.
#' @param append_text An optional character string that, if supplied, will be appended after
#' the final table. Useful for adding in disclosure information. Defaults to an empty string.
#' @param longtablewrap logical. Should LaTeX longtables be allowed to wrap to
#' a new page? If \code{TRUE}, inserts the 'continued on the next page' breaks.
#' Defaults to \code{TRUE}.
#' @param path.to.pdflatex Character path to pdflatex.
#' @param min_cell_size integer. Minimum number of responses for a cross tabulated
#' category to be displayed in details in a banner report.
#' @param min_cell_label character. If a number of responses for a
#' cross tabulated category is less than \code{min_cell_size} then this string is
#' used to mask out the results.
#' @param show_totals logical. If \code{TRUE} a 'Totals' row with column sums is displayed.
#' Defaults to \code{TRUE}.
#' @param weighted_n logical. Should the total number of responses be weighted?
#' Defaults to \code{FALSE}.
#' @param page_width Page width.
#' Defaults to 9in for banners and 6.5in for toplines.
#' @param row_label_width Width of the row label comuln. Defaults to 1.5in.
#' @param add_parenthesis logical. Should 'Weighted / Unweighted N' values in banners be parenthesised?
#' Defaults to \code{TRUE}.
#' @param graphicspath character. The path to the folder with graphics files, e.g. logo.
#' Defaults to \code{NULL} - LaTeX output directory is used.
#' @param logo character. The name of the logo file.
#' Defaults to \code{NULL} - no logo is used.
#' @param dc Width of new column types for banners.
#' @param multirowheaderlines logical. Should banners allow multi-row headlines?
#' @param latex_adjust A LaTeX column adjustoment setting for banner's 'Weighted / Unweighted N' values.
#' @param clearpage logical. Should every banner be on a separete page?
#' @param grid_num_letters logical. Should each layer of a \code{categorical_array} variable (a "grid" question)
#' have the same number with consecutive letters appended? Defaults to \code{TRUE}.
#' @param custom_numbering A vector of custom values to be used for numbering banner tables.
#' Defaults to \code{NULL} - default numbering scheme is used.
#' @param round_to_100 logical. Should percentages be rounded to sum up to 100?
#' Defaults to \code{FALSE}.
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
writeLatex <- function(data_summary, filename = NULL, proportions = TRUE, digits = 0,
    title = getName(data_summary), subtitle = NULL, sample_desc = "", field_period = "", moe = NULL,
    table_of_contents = FALSE, returndata = FALSE, append_text = "",
    pdf = FALSE, path.to.pdflatex = Sys.which("pdflatex"), open = FALSE,
    headtext = "", foottext = "", graphicspath = NULL, logo = NULL, longtablewrap = TRUE,
    tableonly = FALSE, landscape = FALSE, font = "helvet", font_size = NULL,
    page_width = ifelse(landscape, 9, 6.5), row_label_width = 1.5,
    margin = list(top = 0.6, bottom = 0.6, left = 1, right = 1),
    min_cell_size = NULL, min_cell_label = NULL,
    show_totals = TRUE, weighted_n = FALSE, add_parenthesis = FALSE,
    dc = c(3.2, 4.1), multirowheaderlines = FALSE,
    latex_adjust = 'c', clearpage = TRUE, grid_num_letters = TRUE, custom_numbering = NULL,
    round_to_100 = FALSE) {

    if (pdf && is.null(filename)) {
        stop("Please provide a file name to generate PDF output.")
    }

    if (!is.null(custom_numbering) && length(custom_numbering) != length(data_summary$results)) {
      stop("The length of 'custom_numbering' provided (", length(custom_numbering),
           ") is not equal to the length of the results (", length(data_summary$results), ").")
    }

    UseMethod("writeLatex", data_summary)
}


#' @export
writeLatex.default <- function(data_summary, ...) {
  stop(paste0("writeLatex doesn't support objects of class '",
             paste0(class(data_summary), collapse = " "), "'"))
}


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
        sample_desc, field_period, moe_text, "\\end{tabular}\n", ifelse(table_of_contents ==
            TRUE, "\\listoftables\n\n\n", "\n\n"), "%% here's where individual input starts %%\n\n\n \\vspace{.25in} \n\n",
        sep = "")
}


latexTable.body <- function(df, rownames = FALSE, dotfill = FALSE, autorownames = FALSE,
    autocolnames = FALSE, esc = TRUE, colnames = NULL, longtablewrap = TRUE, summary.midrule = FALSE,
    show_totals = TRUE) {
    if (!is.data.frame(df))
        df <- as.data.frame(df)

    if (autocolnames) {

        df <- if (is.null(colnames))
            rbind(gsub(".", " ", names(df), fixed = TRUE), df) else rbind(colnames, df)
    }
    if (autorownames && !is.null(rownames(df)))
        df <- data.frame(rownames(df), df, stringsAsFactors = FALSE)
    if (esc) {
        for (j in 1:ncol(df)) df[, j] <- escM(df[, j])
    }
    collapsestring <- paste("\\\\", ifelse(longtablewrap, "", "*"), "\n", sep = "")

    sepstring <- ifelse(dotfill && ncol(df) == 2, " \\hspace*{0.15em} \\dotfill ",
        " & ")
    if (!summary.midrule) {
        return(paste(paste(apply(df, 1, paste, collapse = sepstring), collapse = collapsestring),
            collapsestring))
    } else {
        main_part = paste(paste(apply(df[1:(nrow(df) - 2 + (!show_totals)), ], 1,
            paste, collapse = sepstring), collapse = collapsestring), collapsestring)
        summary_part = paste(paste(apply(df[(nrow(df) - 1 + (!show_totals)):nrow(df),
            ], 1, paste, collapse = sepstring), collapse = collapsestring), collapsestring)
        return(paste(main_part, "\\midrule", summary_part))
    }
}


escM <- function(str) {
    str <- gsub("^ *(\\[)", "\\\\hspace\\*\\{0in\\}\\1", gsub("([#$%&_])", "\\\\\\1", str))
    str <- gsub("[\u00A3\uFFE1]", "\\\\pounds", str)
    str
}

ltranspose <- function(l) {
    if (length(unique(unlist(lapply(l, length)))) > 1)
        stop("All nested lists must be of equal length")
    if (!is.null(names(l[[1]]))) {
        return(sapply(names(l[[1]]), function(x) lapply(l, function(y) y[[x]])))
    } else {
        return(sapply(1:length(l[[1]]), function(x) lapply(l, function(y) y[[x]])))
    }
}

getFilterText <- function(var_summary) {
    filtertext <- getNotes(var_summary)
    if (filtertext != "") {
        filtertext <- paste("\\\\ \n \\scriptsize { \\itshape ", escM(filtertext), "}")
    }
    filtertext
}
