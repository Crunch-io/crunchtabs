
#' Generate Excel Reports: Toplines and Banners
#'
#' \code{writeExcel} produces publication-quality Excel reports:
#' Toplines (one-way frequency tables) or Banners (cross tabulations)
#'
#' @param data_summary An object of class \code{Toplines} or \code{Crosstabs}.
#' @param filename The name of the output file (without an extension).
#' @param wb Either a path to a .xlsx file or an openxlsx Workbook object to add
#' to. Useful for custom front pages.
#' @param title An optional title. Defaults to the title provided in the summary.
#' @param subtitle An optional subtitle. Defaults to an empty string.
#' @param n_or_percent Should the data be returned as counts or percents. Valid options are
#' "n", "percents", or "both". Defaults to "percents".
#' @param table_of_contents logical. Should a list of tables be included at the start
#' of the report. Defaults to \code{FALSE}.
#' @param font A text specifying the font to use. Defaults to \code{'Calibri'}. *
#' @param font_size Numeric font size points. Defaults to 12pt font. *
#' @param report_desc An optional named list of report descriptions that should be
#' displayed on the front page / table of contents, e.g.
#' list(`Fieldwork date` = "2017-01-01", `Sample description` = "description").
#' Defaults to \code{NULL}.
#' @param digits integer. The number of decimal places that should be used for
#' rounding numbers. Defaults to \code{0}. *
#' @param append_text A text that, if supplied, will be appended after
#' the final table. Useful for adding in disclosure information.
#' @param min_base_size integer. The minimum number of responses for a cross tabulated *
#' category to be displayed in details in a banner report. 
#' @param min_base_label list. Specifies how cross-tabulated results that are *
#' less than \code{min_base_size} should be formatted. Valid fields are:
#' \itemize{
#'  \item mask - a string that should be used to mask the results or \code{NULL} if the numbers should be printed.
#'  \item decoration - text styling. Valid values are: "bold", "strikeout", "italic", "underline", "underline2". Defaults to \code{NULL} - default text style is used.
#'  \item color - a string specifying the font color. Defaults to \code{NULL} - the workbook default color ("black") is used.
#' }
#' Defaults to \code{list(mask = "*", decoration = NULL, color = NULL)}.
#' @param show_totals logical. If \code{TRUE}, the row containing column sums is displayed. *
#' Defaults to \code{TRUE}.
#' @param banner_format list. Specify format details: *
#' \itemize{
#'  \item font - a font name. Defaults to the value of the \code{font} parameter.
#'  \item decoration - text styling. Valid values are: "bold", "strikeout", "italic", "underline", "underline2". Defaults to \code{NULL} - default text style is used.
#'  \item size - font size. Defaults to the value of the \code{font_size} parameter.
#'  \item color - a string specifying the font color. Defaults to \code{NULL} - the workbook default color ("black") is used.
#' }
#' related to banner's:
#' \itemize{
#'  \item labels,
#'  \item categories,
#'  \item total
#' }
#' Defaults to \code{list(labels = list(decoration="bold"),
#' categories = list(decoration = "bold"),
#' total = list(decoration = "bold"))}.
#' @param insertions_format list. Specify format details: *
#' \itemize{
#'  \item font - a font name. Defaults to the value of the \code{font} parameter.
#'  \item decoration - text styling. Valid values are: "bold", "strikeout", "italic", "underline", "underline2". Defaults to \code{NULL} - default text style is used.
#'  \item size - font size. Defaults to the value of the \code{font_size} parameter.
#'  \item color - a string specifying the font color. Defaults to \code{NULL} - the workbook default color ("black") is used.
#'  \item background color - a string specifying the background color. Defaults to \code{NULL} - the workbook default color ("white") is used.
#' }
#' related to banner's:
#' \itemize{
#'  \item subtotal,
#'  \item heading,
#' }
#' Defaults to list(subtotal=list(decoration = "bold"), 
#' heading=list(decoration='italic'))
#' @param show_information list. Specify format details: *
#' \itemize{
#'  \item decoration - text styling,
#'  \item size - font size. Defaults to the value of the \code{font_size} parameter.
#' }
#' related to cross-tabulated variable's:
#' \itemize{
#'  \item name,
#'  \item description,
#'  \item filtertext,
#'  \item subname.
#' }
#' Valid text decorations are:
#' \itemize{
#'  \item bold - bold cell contents
#'  \item strikeout - strikeout cell contents
#'  \item italic - italicise cell contents
#'  \item underline - underline cell contents
#'  \item underline2 - double underline cell contents
#'  \item NULL - no decoration
#' }
#' Infomration will be printed in the order they're listed.
#' Defaults to list(name=list(decoration="bold"),
#' filtertext=list(decoration="italic")).
#' @param unweighted_n A list of parameters describing the row containing the unweighted bases: *
#' \itemize{
#'  \item name - row label.
#'  \item position - row position. Valid values are any combination of: "top", "bottom", "fixed". 
#'  \item decoration - text styling. Valid values are: "bold", "strikeout", "italic", "underline", "underline2". Defaults to \code{NULL} - default text style is used.
#'  \item size - font size. Defaults to the value of the \code{font_size} parameter.
#'  \item color - a string specifying the font color. Defaults to \code{NULL} - the workbook default color ("black") is used.
#' }
#' Decoration, size and color options are applied only when \code{reduce_format = FALSE}.
#' Defaults to list(name = "Unweighted N", position = "bottom").
#' @param weighted_n A list of parameters describing the row containing the weighted bases: *
#' \itemize{
#'  \item name - row label.
#'  \item position - row position. Valid values are any combination of: "top", "bottom", "fixed". 
#'  \item decoration - text styling. Valid values are: "bold", "strikeout", "italic", "underline", "underline2". Defaults to \code{NULL} - default text style is used.
#'  \item size - font size. Defaults to the value of the \code{font_size} parameter.
#'  \item color - a string specifying the font color. Defaults to \code{NULL} - the workbook default color ("black") is used.
#' }
#' Decoration, size and color options are applied only when \code{reduce_format = FALSE}.
#' Defaults to \code{NULL} - the row containing the weighted bases is not printed.
#' @param total_col A list of parameters describing the Total column: *
#' \itemize{
#'  \item decoration - text styling. Valid values are: "bold", "strikeout", "italic", "underline", "underline2". Defaults to \code{NULL} - default text style is used.
#'  \item size - font size. Defaults to the value of the \code{font_size} parameter.
#'  \item color - a string specifying the font color. Defaults to \code{NULL} - the workbook default color ("black") is used.
#' }
#' Decoration, size and color options are applied only when \code{reduce_format = FALSE}.
#' Defaults to \code{NULL} - default text style used.
#' @param show_grid_lines logical. Should grid lines be shown? *
#' Defaults to \code{FALSE}.
#' @param banner_vars_split the method of splitting banner variables. *
#' Valid values are: {NULL, "empty_col", "line"}.
#' Defaults to \code{NULL} - no split.
#' @param row_label_width width of the first column. Defaults to 30pt. *
#' @param row_label_alignment alignment of the first column. *
#' Valid values are: "left", "right", "center", NULL (a default alignemt).
#' Defaults to NULL.
#' @param labels_wrap list. Specifies which labels should be wrapped: **
#' \itemize{
#'  \item name - variable's name.
#'  \item description - variable's description.
#'  \item row_labels - row labels.
#'  \item banner_labels - banner labels
#'  \item column_categories - column categories.
#' }
#' Defaults to list(name = TRUE, description = TRUE,
#' row_labels = TRUE, banner_labels = TRUE, column_categories = TRUE).
#' @param first_active_col number. First active coulumn. Defaults to \code{2}. *
#' @param logging logical. Should basic information related to the function execution
#' be printed? Defaults to \code{FALSE}.
#' @param one_per_sheet logical. Should every variable be written on a separate sheet?
#' Defaults to \code{TRUE}.
#' @param include_aliases logical. Should the variable's alias be concatenated with *
#'  the variable's name? Defaults to \code{FALSE}.
#' @param logo A list of parameters describing the logo: *
#' \itemize{
#'  \item file - the path to the logo file. Valid file types are: jpeg, png, bmp.
#'  \item startRow - row coordinate of upper left corner of the image.
#'  \item startCol - column coordinate of upper left corner of the image.
#'  \item width - width of figure.
#'  \item height - height of figure.
#'  \item units - units of width and height. Can be "in", "cm" or "px".
#'  \item dpi - image resolution used for conversion between units.
#' }
#' Defaults to \code{NULL} - no logo is used.
#' @param reduce_format logical. Should the number of operations that apply styles *\
#' to tables be minimized? Results in slightly faster execution and slightly different table styles.
#' Defaults to \code{FALSE}.
#' @param banner_border_lines list of 
#' @param title_on_results_page logical. Should title and subtitle be printed on the results page? *\
#' Defaults to \code{FALSE}.
#' @param percent_format_data logical. Should data when \code{proportions = TRUE} *
#' be formatted as "Percentage"? If not, a row with percent signs is added to the banner.
#' Defaults to \code{TRUE}.
#' @param hypothesis_test logical.
#' Defaults to \code{FALSE}.
#' @param return_data logical. If \code{TRUE}, a processed data that was used to produce *\
#' the report is returned.
#' @param header A charcter vector of length 3 specifying what the header should be. To skip 
#' a spot, use NA. Defaults to NULL.
#' Special options:
#' "&[Page]" Page number
#' "&[Pages]" Number of pages
#' "&[Date]" Current date
#' "&[Time]" Current time
#' "&[Path]" File path
#' "&[File]" File name
#' "&[Tab]" Worksheet name
#' @param footer A charcter vector of length 3 specifying what the footer should be. To skip 
#' a spot, use NA. Defaults to NULL.
#' Special options:
#' "&[Page]" Page number
#' "&[Pages]" Number of pages
#' "&[Date]" Current date
#' "&[Time]" Current time
#' "&[Path]" File path
#' "&[File]" File name
#' "&[Tab]" Worksheet name
#' @param orientation One of "portrait" or "landscape" indicating the page orientation in excel.
#' Defaults to "portrait".
#' @return If \code{return_data} is set to \code{TRUE}, a processed data that was used to produce
#' the report is returned. Otherwise \code{NULL} is returned.
#' @examples
#' \dontrun{
#' # toplines report
#' toplines_summary <- crosstabs(crunch_dataset, weight = 'weight')
#' writeExcel(toplines_summary, 'filename')
#' # crosstabs report
#' crosstabs_summary <- crosstabs(crunch_dataset, banner = banner_object)
#' writeExcel(crosstabs_summary, 'filename')
#' }
#' @export
writeExcel <- function(data_summary, filename, wb = NULL, title = getName(data_summary), subtitle = NULL,
    n_or_percent='percent', digits = 0, table_of_contents = FALSE, logo = NULL,
    weighted_n = NULL, unweighted_n = list(name = "Unweighted N", position = "bottom"),
    show_totals = TRUE, report_desc = NULL, font = "Calibri", font_size = 12,
    one_per_sheet = TRUE, append_text = "", row_label_alignment = NULL,
    banner_vars_split = NULL, row_label_width = 30, reduce_format = FALSE,
    min_base_size = NULL, min_base_label = list(mask = "*"),
    show_information = list(name = list(decoration="bold"), filtertext=list(decoration="italic")),
    show_grid_lines = FALSE, return_data = FALSE, logging = FALSE,
    labels_wrap = list(name = TRUE, description = TRUE,
        row_labels = TRUE, banner_labels = TRUE, column_categories = TRUE),
    first_active_col = 2, include_aliases = FALSE, banner_border_lines = FALSE,
    title_on_results_page = FALSE, percent_format_data = TRUE, total_col = NULL,
    banner_format = list(labels = list(decoration="bold"),
        categories = list(decoration = "bold"),
        total = list(decoration = "bold")),
    insertions_format = list(subtotal=list(decoration = "bold"), 
        heading=list(decoration='italic')),
    hypothesis_test = FALSE, header=NULL, footer=NULL, orientation='portrait', save_workbook=TRUE,
    lines_color='black', table_border=FALSE) {
    
    if (missing(filename)) {
        stop("No valid filename provided.")
    }
    UseMethod("writeExcel", data_summary)
}

#' @export
writeExcel.default <- function(data_summary, ...) {
    wrong_class_error(data_summary, c("Toplines", "Crosstabs"), "data_summary")
}

#' @export
writeExcel.Toplines <- function(data_summary, filename = NULL, wb = NULL, title = getName(data_summary), subtitle = NULL,
    n_or_percent='percent', digits = 0, table_of_contents = FALSE, logo = NULL,
    weighted_n = NULL, unweighted_n = list(name = "Unweighted N", position = "bottom"),
    show_totals = TRUE, report_desc = NULL, font = "Calibri", font_size = 12,
    one_per_sheet = TRUE, append_text = "", row_label_alignment = NULL,
    banner_vars_split = NULL, row_label_width = 30, reduce_format = FALSE,
    min_base_size = NULL, min_base_label = list(mask = "*"),
    show_information = list(name = list(decoration="bold"), filtertext=list(decoration="italic")),
    show_grid_lines = FALSE, return_data = FALSE, logging = FALSE,
    labels_wrap = list(name = TRUE, description = TRUE,
        row_labels = TRUE, banner_labels = TRUE, column_categories = TRUE),
    first_active_col = 2, include_aliases = FALSE, banner_border_lines = FALSE,
    title_on_results_page = FALSE, percent_format_data = TRUE, total_col = NULL,
    banner_format = list(labels = list(decoration="bold"),
        categories = list(decoration = "bold"),
        total = list(decoration = "bold")),
    insertions_format = list(subtotal=list(decoration = "bold"),
        heading=list(decoration='italic')),
    hypothesis_test = FALSE, header=NULL, footer=NULL, orientation='portrait', save_workbook=TRUE,
    lines_color='black', table_border=FALSE) {

    if (is.null(crosstabs_summary$metadata$weight) && !is.null(weighted_n)) {
        warning('Data is unweighted. "weighted_n" row will not appear.', call. = FALSE)
        weighted_n <- NULL
    }
    
    proportions <- n_or_percent %in% c('percent', 'both')
    
    data_summary$results <- lapply(data_summary$results, function(var_data) {
        var_data$data <- reformatResults(var_data, proportions = proportions, digits = digits,
            reformat = FALSE)
        if (var_data$type %in% c('categorical', 'categorical_array', 'multiple_response')){
            if (proportions && !percent_format_data) {
                var_data$data[] <- var_data$data * 100
            }
        }
        var_data
    })
    
    styles <- create_styles(banner_border_lines=banner_border_lines, banner_format=banner_format,
        digits=digits, insertions_format=insertions_format, labels_wrap=labels_wrap, min_base_label=min_base_label,
        percent_format_data=percent_format_data, reduce_format=reduce_format,
        row_label_alignment=row_label_alignment, show_information=show_information, total_col=total_col,
        unweighted_n=unweighted_n, weighted_n=weighted_n, lines_color=lines_color)

    writeReportGeneral(x = data_summary, banner = banner, filename = filename, wb=wb, n_or_percent = n_or_percent, 
        digits = digits, title = title, subtitle = subtitle, return_data = return_data, 
        table_of_contents = table_of_contents, report_desc = report_desc, font = font, 
        font_size = font_size, show_grid_lines = show_grid_lines, unweighted_n = unweighted_n, 
        weighted_n = weighted_n, banner_vars_split = banner_vars_split, show_totals = show_totals, 
        append_text = append_text, min_base_size = min_base_size, min_base_label = min_base_label, 
        one_per_sheet = one_per_sheet, row_label_width = row_label_width, styles = styles, logo = logo, 
        show_information = show_information, logging = logging, first_active_col = first_active_col, 
        reduce_format = reduce_format, include_aliases = include_aliases, 
        title_on_results_page = title_on_results_page, percent_format_data = percent_format_data, 
        hypothesis_test = hypothesis_test, header = header, footer = footer, orientation=orientation, 
        save_workbook=save_workbook, table_border=table_border)
}

#' @export
writeExcel.Crosstabs <- function(data_summary, filename = NULL, wb = NULL, title = getName(data_summary), subtitle = NULL,
    n_or_percent='percent', digits = 0, table_of_contents = FALSE, logo = NULL,
    weighted_n = NULL, unweighted_n = list(name = "Unweighted N", position = "bottom"),
    show_totals = TRUE, report_desc = NULL, font = "Calibri", font_size = 12,
    one_per_sheet = TRUE, append_text = "", row_label_alignment = NULL,
    banner_vars_split = NULL, row_label_width = 30, reduce_format = FALSE,
    min_base_size = NULL, min_base_label = list(mask = "*"),
    show_information = list(name = list(decoration="bold"), filtertext=list(decoration="italic")),
    show_grid_lines = FALSE, return_data = FALSE, logging = FALSE,
    labels_wrap = list(name = TRUE, description = TRUE,
        row_labels = TRUE, banner_labels = TRUE, column_categories = TRUE),
    first_active_col = 2, include_aliases = FALSE, banner_border_lines = FALSE,
    title_on_results_page = FALSE, percent_format_data = TRUE, total_col = NULL,
    banner_format = list(labels = list(decoration="bold"),
        categories = list(decoration = "bold"),
        total = list(decoration = "bold")),
    insertions_format = list(subtotal=list(decoration = "bold"), 
        heading=list(decoration='italic')),
    hypothesis_test = FALSE, header=NULL, footer=NULL, orientation='portrait', save_workbook=TRUE,
    lines_color='black', table_border=FALSE) {
    
    if (is.null(crosstabs_summary$metadata$weight) && !is.null(weighted_n)) {
        warning('Data is unweighted. "weighted_n" row will not appear.', call. = FALSE)
        weighted_n <- NULL
    }
    banner <- data_summary$banner
    
    # if (one_per_sheet && length(banner) > 1) {
    #     banner_name <- "Results"
    #     data_summary$results <- mergeBannerResults(data_summary$results, banner_name = banner_name)
    #     banner <- mergeBanner(banner, banner_name = banner_name)
    # }
    
    styles <- create_styles(banner_border_lines=banner_border_lines, banner_format=banner_format, 
        digits=digits, insertions_format=insertions_format, labels_wrap=labels_wrap, min_base_label=min_base_label, 
        percent_format_data=percent_format_data, reduce_format=reduce_format,
        row_label_alignment=row_label_alignment, show_information=show_information, total_col=total_col, 
        unweighted_n=unweighted_n, weighted_n=weighted_n, lines_color=lines_color)
    
    writeReportGeneral(x = data_summary, banner = banner, filename = filename, wb = wb, n_or_percent = n_or_percent, digits = digits,
        title = title, subtitle = subtitle, return_data = return_data, table_of_contents = table_of_contents,
        report_desc = report_desc, font = font, show_grid_lines = show_grid_lines,
        unweighted_n = unweighted_n, weighted_n = weighted_n,
        font_size = font_size, show_totals = show_totals, banner_vars_split = banner_vars_split,
        append_text = append_text, min_base_size = min_base_size, min_base_label = min_base_label,
        one_per_sheet = one_per_sheet, row_label_width = row_label_width, styles = styles, logo = logo,
        show_information = show_information, logging = logging, first_active_col = first_active_col,
        reduce_format = reduce_format, include_aliases = include_aliases, title_on_results_page = title_on_results_page,
        percent_format_data = percent_format_data, hypothesis_test = hypothesis_test, header=header, footer=footer, orientation = orientation, 
        save_workbook=save_workbook, table_border=table_border)
}

write_report_desc <- function(wb, ws, title, subtitle, start_row = 2, start_col = 2, report_desc = NULL, styles = NULL, toc_page = TRUE, logo) {

    if (!is.null(logo)) {
        openxlsx::insertImage(wb, ws, file = logo$file,
            startRow = if (is.null(logo$startRow)) 2 else logo$startRow,
            startCol = if (is.null(logo$startCol)) 6 else logo$startCol,
            width = if (is.null(logo$width)) 4 else logo$width,
            height = if (is.null(logo$height)) 2 else logo$height,
            units = if (is.null(logo$units)) "in" else logo$units,
            dpi = if (is.null(logo$dpi)) 300 else logo$dpi)
    }
    
    openxlsx::writeData(wb, ws, title, startCol = start_col, startRow = start_row)
    openxlsx::addStyle(wb, ws, if (toc_page) styles$toc_title else styles$title, rows = start_row, cols = start_col)
    start_row <- start_row + 1
    if (!is.null(subtitle)) {
        for (subt in subtitle) {
            openxlsx::writeData(wb, ws, subt, startCol = start_col, startRow = start_row)
            openxlsx::addStyle(wb, ws, if (toc_page) styles$toc_subtitle else styles$subtitle, rows = start_row, cols = start_col)
            start_row <- start_row + 1
        }
    }
    start_row <- start_row + 1
    desc_names <- names(report_desc)
    for (desc_num in seq_along(report_desc)) {
        openxlsx::writeData(wb, ws, paste0(desc_names[desc_num], ": ", report_desc[[desc_num]]), startCol = start_col, startRow = start_row)
        if (!toc_page) {
            openxlsx::addStyle(wb, ws, styles$desc, rows = start_row, cols = start_col)
        }
        # openxlsx::writeData(wb, ws, report_desc[[desc_num]], startCol = start_col + 1, startRow = start_row)
        start_row <- start_row + 1
    }
    return(start_row)
}

create_table_of_contents <- function(wb, title, subtitle, toc_row = 2, toc_col = 2,
    report_desc = NULL, styles = NULL, show_grid_lines = FALSE, logo = NULL, header, footer, orientation) {
    
    toc_sheet <- "TOC"
    openxlsx::addWorksheet(wb, toc_sheet, gridLines = show_grid_lines, header=header, footer=footer, orientation=orientation)
    
    toc_row <- write_report_desc(wb, toc_sheet, title = title, subtitle = subtitle, start_row = toc_row,
        start_col = toc_col, report_desc = report_desc, styles = styles, logo=logo)
    
    toc_row <- toc_row + 1

    list(toc_sheet = toc_sheet, toc_row = toc_row, toc_col = 2)
}


create_banner_panel <- function(wb, ws, styles, banner=NULL, banner_vars_split = NULL, start_row = 1, banner_cols_pos = NULL,
    title_on_results_page = FALSE, title = NULL, subtitle = NULL, report_desc = NULL,
    percent_row = FALSE, weighted_n = NULL, unweighted_n = list(name = "Unweighted N", position = "bottom"),
    reduce_format=FALSE, logo) {
    
    if (title_on_results_page) {
        start_row <- write_report_desc(wb, ws, title = title, subtitle = subtitle, start_row = start_row,
            start_col = 1, report_desc = report_desc, styles = styles, toc_page = FALSE, logo=logo)
    }
    
    empty_col <- if (!is.null(banner_vars_split) && banner_vars_split == "empty_col") 1 else 0
    if (!is.null(banner)){
        multicols <- sapply(banner, getNames)
        data <- as.data.frame(lapply(seq_along(banner), function(bv) {
            rbind(
                c(getName(banner[[bv]]), if (length(multicols[[bv]]) > 1) rep("", times = length(multicols[[bv]]) - 1 + empty_col)),
                c(multicols[[bv]], if (empty_col == 1) ""))
        }))
        openxlsx::writeData(wb, ws, data, startCol = 2, startRow = start_row, colNames = FALSE)
        multicols_csum <- cumsum(c(banner_cols_pos[1], sapply(multicols, function(x) {length(x) + empty_col})))
        lapply(seq_along(multicols), function(bv) {
            openxlsx::mergeCells(wb, ws, cols = multicols_csum[bv]:(multicols_csum[bv + 1] -
                    1 - empty_col), rows = start_row)})
        openxlsx::addStyle(wb, ws, styles$labels, rows = start_row,
            cols = multicols_csum[[1]]:(multicols_csum[[length(multicols_csum)]] -
                    1 - empty_col), stack = FALSE)
        openxlsx::addStyle(wb, ws, styles$categories, rows = start_row + 1,
            cols = multicols_csum[[1]]:(multicols_csum[[length(multicols_csum)]] -
                    1 - empty_col), stack = FALSE)
        openxlsx::addStyle(wb, ws, styles$total, rows = start_row + 1,
            cols = multicols_csum[[1]], stack = FALSE)
        if (!is.null(banner_vars_split) && banner_vars_split == "line") {
            openxlsx::addStyle(wb, ws, styles$border_r, rows = start_row:(start_row + 1),
                cols = banner_cols_pos, gridExpand = TRUE, stack = TRUE)
        }
        if (!is.null(weighted_n) && 'fixed' %in% weighted_n$position) {
            openxlsx::writeData(wb, ws, weighted_n$name, startCol = 1, startRow = start_row + 2, colNames = FALSE)
            wn <- matrix(unlist(sapply(seq_along(banner), function(bv) {
                c(banner[[bv]]$weighted_n, if (empty_col == 1) "")
            })), nrow=1)
            start_row <- write_bases_data(wb, ws, wn, col=2, row=start_row + 2,
                last_col_num=length(wn)+2, reduce_format=reduce_format,
                style_data=styles$n_weighted, style_label=styles$row_n_weighted) - 2
        }
        if (!is.null(unweighted_n) && 'fixed' %in% unweighted_n$position) {
            openxlsx::writeData(wb, ws, unweighted_n$name, startCol = 1, startRow = start_row + 2, colNames = FALSE)
            uwn <- matrix(unlist(sapply(seq_along(banner), function(bv) {
                c(banner[[bv]]$unweighted_n, if (empty_col == 1) "")
            })), nrow=1)
            start_row <- write_bases_data(wb, ws, uwn, col=2, row=start_row + 2,
                last_col_num=length(uwn)+2, reduce_format=reduce_format,
                style_data=styles$n_unweighted, style_label=styles$row_n_unweighted) - 2
        }
        if (percent_row) {
            start_row <- start_row + 1
            data <- as.data.frame(lapply(seq_along(banner), function(bv) {
                t(c(rep("%", times = length(multicols[[bv]])), if (empty_col == 1) ""))
            }))
            openxlsx::writeData(wb, ws, data, startCol = banner_cols_pos[1], startRow = start_row + 1, colNames = FALSE)
            openxlsx::addStyle(wb, ws, styles$body_text, rows = start_row + 1,
                cols = multicols_csum[[1]]:(multicols_csum[[length(multicols_csum)]] -
                        1 - empty_col), stack = FALSE)
        }
    }
    return(start_row + 1)
}

write_bases_data <- function(wb, ws, data, col, row, last_col_num, reduce_format, style_data, style_label) {
    openxlsx::writeData(wb, ws, round(data), startCol = col, startRow = row,
        colNames = FALSE)
    if (!reduce_format) {
        for (r in (row + (1:nrow(data))-1)){
            openxlsx::addStyle(wb, ws, style_data, rows = r,
                cols = col:last_col_num, stack = FALSE)
            openxlsx::addStyle(wb, ws, style_label, rows = r,
                cols = col - 1, stack = FALSE)
        }
    }
    row + nrow(data)
}

hypho_test <- function(wb, ws, cross_tab_var, banner_name, margin, empty_col, styles, crow, ccol) {
    pvals <- c(0.1, 0.05, 0.01, 0.001)
    pcol_pos <- c("bg_col_green1", "bg_col_green2", "bg_col_green3", "bg_col_green4")
    pcol_neg <- c("bg_col_red1", "bg_col_red2", "bg_col_red3", "bg_col_red4")
    
    pvals_row <- as.data.frame(lapply(cross_tab_var$crosstabs[[banner_name]], function(x) {
        d <- x$pvals_col
        d <- as.data.frame(d)
        if (empty_col) cbind(d, "" ) else d
    }))
    if (empty_col) {
        pvals_row <- sapply(pvals_row, function(x) as.numeric(as.character(x)))
    }

    if (sum(dim(pvals_row)) == 0) return(NULL)
    for (pvi in seq_along(pvals)) {
        inds <- which(!is.na(pvals_row) & pvals_row < pvals[pvi] & pvi < length(pvals) & pvals_row >= pvals[pvi+1] | (pvi == length(pvals) & pvals_row < pvals[pvi] & pvals_row > 0), arr.ind = TRUE)
        openxlsx::addStyle(wb, ws, styles[[pcol_pos[pvi]]], rows = crow + inds[, 1] - 1,
            cols = ccol + inds[, 2] - 1, gridExpand = FALSE, stack = TRUE)
    }
    pvals <- pvals * (-1)
    for (pvi in seq_along(pvals)) {
        inds <- which(!is.na(pvals_row) & pvals_row > pvals[pvi] & pvi < length(pvals) & pvals_row <= pvals[pvi+1] | (pvi == length(pvals) & pvals_row > pvals[pvi] & pvals_row < 0), arr.ind = TRUE)
        openxlsx::addStyle(wb, ws, styles[[pcol_neg[pvi]]], rows = crow + inds[, 1] - 1,
            cols = ccol + inds[, 2] - 1, gridExpand = FALSE, stack = TRUE)
    }
}

#' @importFrom stats setNames
writeExcelVarBanner <- function(wb, ws, banner_name, cross_tab_var, banner_cols_pos, start_col = 1, start_row = 1,
    digits = 0, proportions = TRUE, show_totals = TRUE, unweighted_n = NULL, weighted_n = NULL,
    row_label_width = 20, toc_sheet = NULL, toc_row = 1, toc_col = 1, styles = NULL,
    banner_vars_split = NULL, show_information = NULL, min_base_size = NULL, min_base_label = NULL,
    reduce_format = FALSE, include_aliases = FALSE, percent_format_data = TRUE, hypothesis_test = FALSE, table_border) {

    show_totals <- !cross_tab_var$settings$no_totals & show_totals
    start_row <- writeVarHeader(wb, ws, cross_tab_var, start_col = start_col, start_row = start_row, toc_sheet = toc_sheet,
        toc_row = toc_row, toc_col = toc_col, styles = styles, show_information = show_information,
        include_aliases = include_aliases)
    weighted_n_top <- !is.null(weighted_n) && 'top' %in% weighted_n$position
    unweighted_n_top <- !is.null(unweighted_n) && 'top' %in% unweighted_n$position
    weighted_n_bottom <- !is.null(weighted_n) && 'bottom' %in% weighted_n$position
    unweighted_n_bottom <- !is.null(unweighted_n) && 'bottom' %in% unweighted_n$position
    empty_col <- !is.null(banner_vars_split) && banner_vars_split == "empty_col"
    crow <- start_row 

    unweighted_n_data <- as.data.frame(lapply(cross_tab_var$crosstabs[[banner_name]], function(x) {
        d <- as.data.frame(x$unweighted_n)
        if (empty_col) cbind(d,  "") else d
    }))

    weighted_n_data <- as.data.frame(lapply(cross_tab_var$crosstabs[[banner_name]], function(x) {
        d <- as.data.frame(if (reduce_format) round(x$totals_counts, digits) else x$totals_counts)
        if (empty_col) cbind(d,  "") else d
    }))

    row_names <- c(
        if (weighted_n_top) paste0(weighted_n$name, if (nrow(weighted_n_data) != 1) paste(':', c('Min', 'Max'))),
        if (unweighted_n_top) paste0(unweighted_n$name, if (nrow(unweighted_n_data) != 1) paste(':', c('Min', 'Max'))),
        rownames(cross_tab_var$crosstabs[[banner_name]][[1]]$proportions),
        if (show_totals) "Totals",
        if (weighted_n_bottom) paste0(weighted_n$name, if (nrow(weighted_n_data) != 1) paste(':', c('Min', 'Max'))),
        if (unweighted_n_bottom) paste0(unweighted_n$name, if (nrow(unweighted_n_data) != 1) paste(':', c('Min', 'Max')))
    )

    openxlsx::writeData(wb, ws, row_names, startCol = start_col, startRow = crow,
        colNames = FALSE)
    openxlsx::addStyle(wb, ws, styles$row_labels, rows = crow:(crow + length(row_names)),
        cols = start_col, stack = FALSE)
    
    start_col <- start_col + 1
    
    last_col_num <- start_col + ncol(unweighted_n_data) - 1 - empty_col
    
    if (weighted_n_top) {
        crow <- write_bases_data(wb, ws, data=weighted_n_data, col=start_col, row=crow,
            last_col_num=last_col_num, reduce_format=reduce_format,
            style_data=styles$n_weighted, style_label=styles$row_n_weighted)
    }
    
    if (unweighted_n_top) {
        crow <- write_bases_data(wb, ws, data=unweighted_n_data, col=start_col, row=crow,
            last_col_num=last_col_num, reduce_format=reduce_format,
            style_data=styles$n_unweighted, style_label=styles$row_n_unweighted)
    }
    
    if ((weighted_n_top || unweighted_n_top) && table_border) {
        openxlsx::addStyle(wb, ws, styles$border_b, rows = crow - 1,
            cols = start_col:last_col_num, stack = !reduce_format)
    }
    
    data <- as.data.frame(lapply(cross_tab_var$crosstabs[[banner_name]], function(x) {
        d <- getResults(x, proportions = proportions)
        d <- as.data.frame(d)
        if (show_totals) {
            d <- rbind(d, setNames(as.data.frame(if (proportions) x$totals_proportions else x$totals_counts), colnames(d)))
        }
        if (proportions && !percent_format_data) {
            d[] <- d * 100
        }
        if (!proportions){
            d[] <- round(d)
        }
        if (empty_col) cbind(d, "" ) else d
    }))

    min_cell_mask <- NULL
    if (!is.null(min_base_size)) {
        unweighted_n_data_num <- sapply(unweighted_n_data, function(x) as.numeric(as.character(x)))
        if (class(unweighted_n_data_num) == 'matrix') unweighted_n_data_num <- c(unweighted_n_data_num[nrow(unweighted_n_data_num),])
        min_cell_mask <- !is.na(unweighted_n_data_num) & unweighted_n_data_num < min_base_size
    }
    if (any(min_cell_mask) && !is.null(min_base_label) && !is.null(min_base_label[["mask"]])) {
        data[, min_cell_mask] <- min_base_label[["mask"]]
    }
    
    openxlsx::writeData(wb, ws, data, startCol = start_col, startRow = crow,
        colNames = FALSE)
    openxlsx::addStyle(wb, ws, if (proportions) styles$body_proportions else styles$body_n, rows = crow:(crow + nrow(data) - 1),
        cols = start_col:last_col_num, gridExpand = TRUE, stack = FALSE)
    if (any(min_cell_mask)) {
        openxlsx::addStyle(wb, ws, if (proportions) styles$body_min_base_proportions else styles$body_min_base_n, rows = crow:(crow + nrow(data) - 1),
            cols = start_col + which(min_cell_mask) - 1, gridExpand = TRUE, stack = FALSE)
    }
    if (!is.null(styles$total_col) && !reduce_format) {
        openxlsx::addStyle(wb, ws, styles$total_col, rows = crow:(crow + nrow(data) - 1),
            cols = start_col, stack = TRUE)
    }
    
    if (hypothesis_test) {
        hypho_test(wb, ws, cross_tab_var, banner_name, margin = 2, empty_col, styles, crow, ccol = start_col)
    }
    
    crow <- crow + nrow(data)
    
    if ((unweighted_n_bottom || weighted_n_bottom || show_totals) && table_border) {
        openxlsx::addStyle(wb, ws, styles$border_b, rows = crow - 1 - show_totals,
            cols = start_col:last_col_num, stack = TRUE)
    }
    
    if (weighted_n_bottom) {
        crow <- write_bases_data(wb, ws, data=weighted_n_data, col=start_col, row=crow,
            last_col_num=last_col_num, reduce_format=reduce_format,
            style_data=styles$n_weighted, style_label=styles$row_n_weighted)
    }
    
    if (unweighted_n_bottom) {
        crow <- write_bases_data(wb, ws, data=unweighted_n_data, col=start_col, row=crow,
            last_col_num=last_col_num, reduce_format=reduce_format,
            style_data=styles$n_unweighted, style_label=styles$row_n_unweighted)
    }
    
    if (!is.null(banner_vars_split) && banner_vars_split == "line") {
        # banner_cols_pos <- cumsum(sapply(banner[[banner_name]], function(x) length(x$categories))) + start_col - 1
        openxlsx::addStyle(wb, ws, styles$border_r, rows = start_row:(crow-1),
            cols = banner_cols_pos[-length(banner_cols_pos)], gridExpand = TRUE, stack = TRUE)
    }
    
    for (si in (start_row + which(cross_tab_var$inserts %in% 'Subtotal') + unweighted_n_top + weighted_n_top - 1)){
        openxlsx::addStyle(wb, ws, styles$subtotal, rows = si,
            cols = (start_col-1):last_col_num, stack = TRUE)
        openxlsx::addStyle(wb, ws, styles$row_labels, rows = si,
            cols = start_col-1, stack = TRUE)
    }
    
    for (si in (start_row + which(cross_tab_var$inserts %in% 'Heading') + unweighted_n_top + weighted_n_top - 1)){
        openxlsx::addStyle(wb, ws, styles$heading, rows = si,
            cols = (start_col-1):last_col_num, stack = TRUE)
        openxlsx::addStyle(wb, ws, styles$row_labels, rows = si,
            cols = start_col-1, stack = TRUE)
    }

    if (table_border){
        openxlsx::addStyle(wb, ws, styles$border_t, rows = start_row,
            cols = start_col:last_col_num, stack = !reduce_format)
        openxlsx::addStyle(wb, ws, styles$border_l, rows = start_row:(crow-1),
            cols = c(start_col, last_col_num + 1), gridExpand = TRUE, stack = TRUE)
        openxlsx::addStyle(wb, ws, styles$border_b, rows = crow-1,
            cols = start_col:last_col_num, gridExpand = TRUE, stack = TRUE)
    }
    
    return(crow + 1)
    
}

writeExcelVarTopline <- function(wb, ws, x, start_col = 1, start_row = 1, digits = 0,
    proportions = TRUE, styles = NULL) {
    UseMethod("writeExcelVarTopline", x)
}

#' @export
writeExcelVarTopline.default <- function(wb, ws, x, start_col = 1, start_row = 1, digits = 0,
    proportions = TRUE, styles = NULL) {
    stop("The 'writeExcelVarTopline' generic function doesn't support objects of type: ",
        collapse_items(class(x)))
}

#' @export
writeExcelVarTopline.ToplineCategoricalArray <- function(wb, ws, x, start_col = 1,
    start_row = 1, digits = 0, proportions = TRUE, styles = NULL) {
    df <- as.matrix(x$data)
    openxlsx::writeData(wb, ws, df, startCol = start_col, startRow = start_row, rowNames = TRUE,
        colNames = TRUE, headerStyle = styles$categories)
    drows <- nrow(df)
    rows <- (start_row + 1):(start_row + drows)
    cols <- 2:(ncol(x$data) + 1)
    openxlsx::addStyle(wb, ws, styles$categorical, rows = rows, cols = cols, gridExpand = TRUE,
        stack = FALSE)
    openxlsx::addStyle(wb, ws, styles$row_labels, rows = rows, cols = start_col, gridExpand = FALSE,
        stack = FALSE)
    return(drows + 1)
}

#' @export
writeExcelVarTopline.ToplineNumeric <- function(wb, ws, x, start_col = 1, start_row = 1,
    digits = 0, proportions = NULL, styles = NULL) {
    df <- as.matrix(x$data)
    openxlsx::writeData(wb, ws, df, startCol = start_col, startRow = start_row, rowNames = TRUE,
        colNames = FALSE)
    drows <- nrow(df)
    rows <- (start_row):(start_row + drows - 1)
    openxlsx::addStyle(wb, ws, styles$numeric, rows = rows, cols = start_col + 1, gridExpand = FALSE,
        stack = TRUE)
    openxlsx::addStyle(wb, ws, styles$row_labels, rows = rows, cols = start_col, gridExpand = FALSE,
        stack = FALSE)
    return(drows)
}

#' @export
writeExcelVarTopline.ToplineCategorical <- function(wb, ws, x, start_col = 1, start_row = 1,
    digits = 0, proportions = TRUE, styles = NULL) {
    drows <- writeExcelVarToplineCategorical(wb, ws, x, start_col = start_col, start_row = start_row,
        digits = digits, proportions = proportions, styles = styles)
    return(drows)
}

#' @export
writeExcelVarTopline.ToplineMultipleResponse <- function(wb, ws, x, start_col = 1,
    start_row = 1, digits = 0, proportions = TRUE, styles = NULL) {
    drows <- writeExcelVarToplineCategorical(wb, ws, x, start_col = start_col, start_row = start_row,
        digits = digits, proportions = proportions, styles = styles)
    return(drows)
}

writeExcelVarToplineCategorical <- function(wb, ws, x, start_col = 1, start_row = 1,
    digits = 0, proportions = TRUE, styles = NULL) {
    df <- as.matrix(x$data)
    openxlsx::writeData(wb, ws, df, startCol = start_col, startRow = start_row, rowNames = TRUE,
        colNames = FALSE)
    drows <- nrow(df)
    rows <- (start_row):(start_row + drows - 1)
    openxlsx::addStyle(wb, ws, styles$categorical, rows = rows, cols = start_col + 1, gridExpand = FALSE,
        stack = FALSE)
    openxlsx::addStyle(wb, ws, styles$row_labels, rows = rows, cols = start_col, gridExpand = FALSE,
        stack = FALSE)
    return(drows)
}

writeExcelVarToplineGeneral <- function(wb, ws, x, start_col = 1, start_row = 1,
    digits = 0, proportions = TRUE, row_label_width = 20, toc_sheet = NULL, toc_row = 1,
    toc_col = 1, styles = NULL, show_information = NULL, include_aliases = FALSE) {
    crow <- writeVarHeader(wb, ws, x, start_col = start_col, start_row = start_row,
        toc_sheet = toc_sheet, toc_row = toc_row, toc_col = toc_col, styles = styles,
        show_information = show_information, include_aliases = include_aliases)
    crow <- crow + 1
    drows <- writeExcelVarTopline(wb, ws, x, start_col = start_col, start_row = crow,
        digits = digits, proportions = proportions, styles = styles)
    openxlsx::setColWidths(wb, ws, 1, row_label_width)
    return(crow + drows + 1)
}

write_var_info <- function(wb, ws, var_info, elem_name, styles, col, row) {
    openxlsx::writeData(wb, ws, var_info, startRow = row, startCol = col)
    openxlsx::addStyle(wb, ws, styles[[elem_name]], rows = row, cols = col)
    return(row + 1)
}

writeVarHeader <- function(wb, ws, x, start_col = 1, start_row = 1, toc_sheet = NULL,
    toc_row = 1, toc_col = 1, styles = NULL, show_information = NULL, include_aliases = FALSE) {
    var_info <- list(alias = getAlias(x),
        name = getName(x),
        description = getDescription(x),
        filtertext = getNotes(x),
        subname = if (!is.na(x$subname)) x$subname)
    if (include_aliases) var_info[[names(show_information)[[1]]]] <- paste0(getAlias(x), ' -- ', var_info[[names(show_information)[[1]]]])
    add_toc_info <- !is.null(toc_sheet)
    if (add_toc_info) {
        openxlsx::writeFormula(wb, toc_sheet, startCol = toc_col, startRow = toc_row, 
            x = openxlsx::makeHyperlinkString(sheet = ws,
                row = start_row - 1, col = start_col, text = var_info$alias))
        openxlsx::writeData(wb, toc_sheet, paste0(c(var_info$subname, var_info$description), collapse = ' - '), startCol = toc_col+1, startRow = toc_row)
    }
    for (info_name in names(show_information)) {
        if (info_name %in% names(var_info) && !is.null(var_info[[info_name]]) && !is.na(var_info[[info_name]]) && var_info[[info_name]] != "") {
            start_row <- write_var_info(wb, ws, var_info = var_info[[info_name]], elem_name = info_name, styles = styles, col = start_col, row = start_row)
        }
    }
    start_row
}

create_styles <- function(banner_border_lines, banner_format, digits, insertions_format, labels_wrap, 
    min_base_label, percent_format_data, reduce_format, row_label_alignment, show_information, 
    total_col, unweighted_n, weighted_n, lines_color){
    
    get_format_info <- function(format_data, info_name, elem) {
        if (!is.null(format_data) && info_name %in% names(format_data)) format_data[[info_name]][[elem]]
    }
    get_decoration_data <- function(data_info, elem) {
        if (!is.null(data_info)) data_info[[elem]]
    }
    
    numFmt <- paste0("0", if (digits > 0) paste0(".", paste0(rep(0, digits), collapse = "")))
    numFmtProp <- paste0(numFmt, if (percent_format_data) "%")
    
    return(list(
        name = openxlsx::createStyle(textDecoration = get_format_info(show_information, "name", "decoration"), fontSize = get_format_info(show_information, "name", "size"), wrapText = labels_wrap$name, halign = row_label_alignment, valign = "center"),
        filtertext = openxlsx::createStyle(textDecoration = get_format_info(show_information, "filtertext", "decoration"), fontSize = get_format_info(show_information, "filtertext", "size"), wrapText = labels_wrap$description, halign = row_label_alignment, valign = "center"),
        description = openxlsx::createStyle(textDecoration = get_format_info(show_information, "description", "decoration"), fontSize = get_format_info(show_information, "description", "size"), wrapText = labels_wrap$description, halign = row_label_alignment, valign = "center"),
        subname = openxlsx::createStyle(textDecoration = get_format_info(show_information, "subname", "decoration"), fontSize = get_format_info(show_information, "subname", "size"), wrapText = labels_wrap$description, halign = row_label_alignment, valign = "center"),
        body_proportions = openxlsx::createStyle(numFmt = numFmtProp, halign = if (!reduce_format) "center"),
        body_min_base_proportions = openxlsx::createStyle(numFmt =  numFmtProp, halign = if (reduce_format) "right" else "center", textDecoration = get_decoration_data(min_base_label, "decoration"), fontColour = get_decoration_data(min_base_label, "color"), valign = "center"),
        body_n = openxlsx::createStyle(numFmt = numFmt, halign = if (!reduce_format) "center"),
        body_min_base_n = openxlsx::createStyle(numFmt =  numFmt, halign = if (reduce_format) "right" else "center", textDecoration = get_decoration_data(min_base_label, "decoration"), fontColour = get_decoration_data(min_base_label, "color"), valign = "center"),
        body_text = openxlsx::createStyle(halign = if (reduce_format) "right" else "center", valign = "center"),
        labels = openxlsx::createStyle(textDecoration = get_format_info(banner_format, "labels", "decoration"), fontSize = get_format_info(banner_format, "labels", "size"), fontName = get_format_info(banner_format, "labels", "font"), fontColour = get_format_info(banner_format, "labels", "color"), halign = "center", wrapText = labels_wrap$banner_labels, border = if (!is.null(banner_border_lines)) "TopBottomLeftRight", borderColour = get_decoration_data(banner_border_lines, "color"), borderStyle = get_decoration_data(banner_border_lines, "style"), valign = "center"),
        subtotal = openxlsx::createStyle(textDecoration = get_format_info(insertions_format, "subtotal", "decoration"), fontSize = get_format_info(insertions_format, "subtotal", "size"), fontName = get_format_info(insertions_format, "subtotal", "font"), fontColour = get_format_info(insertions_format, "subtotal", "color"), halign = if (reduce_format) "right" else "center", valign = "center", bgFill = get_format_info(insertions_format, "subtotal", "backgroundcolor"), fgFill = get_format_info(insertions_format, "subtotal", "backgroundcolor")),
        heading = openxlsx::createStyle(textDecoration = get_format_info(insertions_format, "heading", "decoration"), fontSize = get_format_info(insertions_format, "heading", "size"), fontName = get_format_info(insertions_format, "heading", "font"), fontColour = get_format_info(insertions_format, "heading", "color"), halign = if (reduce_format) "right" else "center", valign = "center", bgFill = get_format_info(insertions_format, "heading", "backgroundcolor"), fgFill = get_format_info(insertions_format, "heading", "backgroundcolor")),
        row_labels = openxlsx::createStyle(halign = if (!is.null(row_label_alignment)) row_label_alignment else "right", wrapText = labels_wrap$row_labels, valign = "center"),
        row_n_weighted = openxlsx::createStyle(halign = if (!is.null(row_label_alignment)) row_label_alignment else "right", wrapText = labels_wrap$row_labels, textDecoration = get_decoration_data(weighted_n, "decoration"), fontSize = get_decoration_data(weighted_n, "size"), fontColour = get_decoration_data(weighted_n, "color"), valign = "center"),
        row_n_unweighted = openxlsx::createStyle(halign = if (!is.null(row_label_alignment)) row_label_alignment else "right", wrapText = labels_wrap$row_labels, textDecoration = get_decoration_data(unweighted_n, "decoration"), fontSize = get_decoration_data(unweighted_n, "size"), fontColour = get_decoration_data(unweighted_n, "color"), valign = "center"),
        categories = openxlsx::createStyle(halign = if (reduce_format) "right" else "center", wrapText = labels_wrap$column_categories, border = if (!is.null(banner_border_lines)) "TopBottomLeftRight", borderColour = get_decoration_data(banner_border_lines, "color"), borderStyle = get_decoration_data(banner_border_lines, "style"), textDecoration = get_format_info(banner_format, "categories", "decoration"), fontName = get_format_info(banner_format, "categories", "font"), fontSize = get_format_info(banner_format, "categories", "size"), fontColour = get_format_info(banner_format, "categories", "color"), valign = "center"),
        total = openxlsx::createStyle(halign = if (reduce_format) "right" else "center", wrapText = labels_wrap$column_categories, border = if (!is.null(banner_border_lines)) "TopBottomLeftRight", borderColour = get_decoration_data(banner_border_lines, "color"), borderStyle = get_decoration_data(banner_border_lines, "style"), textDecoration = get_format_info(banner_format, "total", "decoration"), fontName = get_format_info(banner_format, "total", "font"), fontSize = get_format_info(banner_format, "total", "size"), fontColour = get_format_info(banner_format, "total", "color"), valign = "center"),
        n_weighted = openxlsx::createStyle(numFmt = numFmt, textDecoration = get_decoration_data(weighted_n, "decoration"), fontSize = get_decoration_data(weighted_n, "size"), fontColour = get_decoration_data(weighted_n, "color"), halign = "center", valign = "center"),
        n_unweighted = openxlsx::createStyle(textDecoration = get_decoration_data(unweighted_n, "decoration"), fontSize = get_decoration_data(unweighted_n, "size"), fontColour = get_decoration_data(unweighted_n, "color"), halign = "center", valign = "center"),
        border_b = openxlsx::createStyle(border = "Bottom",  borderColour = get_decoration_data(banner_border_lines, "color"), borderStyle = get_decoration_data(banner_border_lines, "style"), valign = "center"),
        border_t = openxlsx::createStyle(border = "Top", borderColour = get_decoration_data(banner_border_lines, "color"), borderStyle = get_decoration_data(banner_border_lines, "style"), valign = "center"),
        border_t_b = openxlsx::createStyle(border = c('top', 'bottom'),  borderColour = get_decoration_data(banner_border_lines, "color"), borderStyle = get_decoration_data(banner_border_lines, "style"), valign = "center"),
        border_t_b_r = openxlsx::createStyle(border = c('top', 'bottom', 'right'),  borderColour = get_decoration_data(banner_border_lines, "color"), borderStyle = get_decoration_data(banner_border_lines, "style"), valign = "center"),
        border_t_b_l = openxlsx::createStyle(border = c('top', 'bottom', 'left'),  borderColour = get_decoration_data(banner_border_lines, "color"), borderStyle = get_decoration_data(banner_border_lines, "style"), valign = "center"),
        border_t_r = openxlsx::createStyle(border = c('top', 'right'),  borderColour = get_decoration_data(banner_border_lines, "color"), borderStyle = get_decoration_data(banner_border_lines, "style"), valign = "center"),
        border_t_l = openxlsx::createStyle(border = c('top', 'left'),  borderColour = get_decoration_data(banner_border_lines, "color"), borderStyle = get_decoration_data(banner_border_lines, "style"), valign = "center"),
        border_b_r = openxlsx::createStyle(border = c('bottom', 'right'),  borderColour = get_decoration_data(banner_border_lines, "color"), borderStyle = get_decoration_data(banner_border_lines, "style"), valign = "center"),
        border_b_l = openxlsx::createStyle(border = c('bottom', 'left'),  borderColour = get_decoration_data(banner_border_lines, "color"), borderStyle = get_decoration_data(banner_border_lines, "style"), valign = "center"),
        border_r = openxlsx::createStyle(border = "Right", borderColour = get_decoration_data(banner_border_lines, "color"), borderStyle = get_decoration_data(banner_border_lines, "style"), valign = "center"),
        border_l = openxlsx::createStyle(border = "left", borderColour = get_decoration_data(banner_border_lines, "color"), borderStyle = get_decoration_data(banner_border_lines, "style"), valign = "center"),
        toc_title = openxlsx::createStyle(fontSize = 14, textDecoration = "bold", valign = "center"),
        toc_subtitle = openxlsx::createStyle(textDecoration = "bold", valign = "center"),
        title = openxlsx::createStyle(fontSize = 14, textDecoration = "bold", halign = row_label_alignment, valign = "center"),
        subtitle = openxlsx::createStyle(textDecoration = "bold", halign = row_label_alignment, valign = "center"),
        desc = openxlsx::createStyle(halign = row_label_alignment, valign = "center"),
        toc_banner = openxlsx::createStyle(textDecoration = "bold", valign = "center"),
        total_col = if (!is.null(total_col)) openxlsx::createStyle(textDecoration = get_decoration_data(total_col, "decoration"), fontSize = get_decoration_data(total_col, "size"), fontColour = get_decoration_data(total_col, "color"), valign = "center"),
        numeric = openxlsx::createStyle(numFmt = numFmt),
        categorical = openxlsx::createStyle(numFmt = numFmtProp, halign = "center"),
        bg_col_green4 = openxlsx::createStyle(fgFill = "#00CA81"),
        bg_col_green3 = openxlsx::createStyle(fgFill = "#7BC99E"),
        bg_col_green2 = openxlsx::createStyle(fgFill = "#AFDDC3"),
        bg_col_green1 = openxlsx::createStyle(fgFill = "#D7ECD8"),
        bg_col_red4 = openxlsx::createStyle(fgFill = "#DA5130"),
        bg_col_red3 = openxlsx::createStyle(fgFill = "#E67D58"),
        bg_col_red2 = openxlsx::createStyle(fgFill = "#EEA37D"),
        bg_col_red1 = openxlsx::createStyle(fgFill = "#FACBAF")
    ))
}

writeReportGeneral <- function(x, banner, filename, wb, n_or_percent,
    digits, title, subtitle, return_data, table_of_contents,
    report_desc, font, font_size, show_grid_lines,
    unweighted_n, weighted_n, banner_vars_split,
    show_totals, append_text, min_base_size,
    min_base_label, one_per_sheet, row_label_width, styles,
    logo, show_information, logging, first_active_col,
    reduce_format, include_aliases, title_on_results_page,
    percent_format_data, hypothesis_test, header, footer, orientation, save_workbook, table_border) {

    #pageSetup
    if (logging) {
        start.time.wb <- Sys.time()
        print(paste(start.time.wb, "-- workbook generation -- start"))
    }
    
    if (is.character(wb)) wb <- openxlsx::loadWorkbook(file = wb)
    if (is.null(wb)) wb <- openxlsx::createWorkbook()
    if (!class(wb) == 'Workbook') stop('`wb`, if provided, must be a file path to an excel file, or an openxlsx Workbook object.')
    openxlsx::modifyBaseFont(wb, fontSize = font_size, fontColour = "black", fontName = font)
    
    toc_sheet <- NULL
    toc_row <- 2
    toc_col <- 2
    if (table_of_contents) {
        toc_res <- create_table_of_contents(wb, title = title, subtitle = subtitle, toc_row = toc_row, toc_col = toc_col,
            report_desc = report_desc, show_grid_lines = show_grid_lines, styles = styles, logo = logo, header=header, footer=footer, orientation = orientation)
        toc_sheet <- toc_res$toc_sheet
        toc_col <- toc_res$toc_col
        openxlsx::freezePane(wb, toc_sheet, firstActiveRow = toc_res$toc_row + 1)
    }
    
    n_and_percent <- n_or_percent %in% 'both'
    
    banner_names <- if (is.null(banner)) "Results" else names(banner)
    if (n_and_percent) {
        banner_names <- unlist(lapply(banner_names, rep, 2))
    }
    proportions <- !(n_or_percent %in% 'n')
    
    if (one_per_sheet){
        worksheet_names <- names(x$results)
        t5 <- which(nchar(worksheet_names) > 25)
        worksheet_names[t5] <- paste0(strtrim(worksheet_names[t5], 25), t5)
        if (length(banner) > 1) worksheet_names <- sapply(seq_along(banner), function(bix) paste0(worksheet_names, '_', bix))
    } else {
        worksheet_names <- unique(banner_names)
    }
    if (n_and_percent) worksheet_names <- sapply(worksheet_names, function(wn) paste0(wn, '_', c('C', 'P')))
    for (worksheet_name in c(worksheet_names)){
        openxlsx::addWorksheet(wb, worksheet_name, gridLines = show_grid_lines, header=header, footer=footer, orientation=orientation)
        openxlsx::setColWidths(wb, worksheet_name, cols = 1, row_label_width)
    }
    
    for (bix in seq_along(banner_names)) {
        banner_name <- banner_names[bix]
        bna <- if (n_and_percent) ifelse(duplicated(banner_names)[bix], '_P', '_C')
        if (n_and_percent) { proportions <- duplicated(banner_names)[bix] }
        if (logging) {
            start.time <- Sys.time()
            print(paste0(start.time, " -- banner generation: ", banner_name, bna, " -- start"))
        }
        start_col <- 1
        last_row_used <- 1
        banner_cols_pos <- cumsum(sapply(banner[[banner_name]], function(x) length(x$categories))) + start_col
        if (table_of_contents) {
            toc_row <- toc_res$toc_row
            openxlsx::writeData(wb, toc_sheet, paste0(banner_name, bna), startRow = toc_row, startCol = toc_col)
            openxlsx::addStyle(wb, toc_sheet, styles$toc_banner, rows = toc_row, cols = toc_col, stack = FALSE)
        }
        if (!one_per_sheet) {
            worksheet_name <- paste0(banner_name, bna)
            last_row_used <- last_row_used +
                create_banner_panel(wb, worksheet_name, banner = banner[[banner_name]],
                    styles = styles, banner_vars_split = banner_vars_split, start_row = last_row_used,
                    banner_cols_pos = banner_cols_pos, title_on_results_page = title_on_results_page,
                    title = title, subtitle = subtitle, report_desc = report_desc,
                    percent_row = !percent_format_data & proportions,
                    weighted_n=weighted_n, unweighted_n=unweighted_n, reduce_format = reduce_format, logo=logo)
            openxlsx::freezePane(wb, worksheet_name, firstActiveRow = last_row_used, firstActiveCol = first_active_col)
        }
        
        for (vidx in seq_along(x$results)) {
            toc_row <- toc_row + 1
            if (one_per_sheet) {
                last_row_used <- 1
                worksheet_name <- getAlias(x$results[[vidx]])
                # worksheet names must be unique and have less then 32 characters
                if (nchar(worksheet_name) > 25) {
                    worksheet_name <- paste0(strtrim(worksheet_name, 25), vidx)
                }
                if (length(banner) > 1) worksheet_name <- paste0(worksheet_name, '_', which(unique(banner_names) %in% banner_name))
                worksheet_name <- paste0(worksheet_name, bna)
                if (!is.null(banner)) {
                    last_row_used <- create_banner_panel(wb, worksheet_name, banner = banner[[banner_name]],
                        styles = styles, banner_vars_split = banner_vars_split, start_row = 1,
                        banner_cols_pos = banner_cols_pos, title_on_results_page = title_on_results_page,
                        title = title, subtitle = subtitle, report_desc = report_desc,
                        percent_row = !percent_format_data & proportions,
                        weighted_n=weighted_n, unweighted_n=unweighted_n, reduce_format = reduce_format, logo=logo)
                    last_row_used <- last_row_used + 1
                    openxlsx::freezePane(wb, worksheet_name, firstActiveRow = last_row_used, firstActiveCol = first_active_col)
                    last_row_used <- last_row_used + 1
                }
            } else {
                last_row_used <- last_row_used + 1
            }
            last_row_used <- if (is.null(banner)) {
                writeExcelVarToplineGeneral(wb, worksheet_name, x$results[[vidx]], start_col = start_col,
                    start_row = last_row_used, digits = digits, proportions = proportions,
                    row_label_width = row_label_width, toc_sheet = toc_sheet, toc_row = toc_row,
                    toc_col = toc_col, styles = styles, show_information = show_information, include_aliases = include_aliases)
            } else {
                writeExcelVarBanner(wb, worksheet_name, banner_name, x$results[[vidx]], banner_cols_pos,
                    start_col = start_col, start_row = last_row_used, digits = digits, proportions = proportions,
                    unweighted_n = unweighted_n, weighted_n = weighted_n, show_totals = show_totals,
                    row_label_width = row_label_width, toc_sheet = toc_sheet, banner_vars_split = banner_vars_split,
                    toc_row = toc_row, toc_col = toc_col, styles = styles,
                    min_base_size = min_base_size, min_base_label = min_base_label,
                    show_information = show_information, reduce_format = reduce_format, include_aliases = include_aliases,
                    percent_format_data = percent_format_data, hypothesis_test = hypothesis_test, table_border = table_border)
            }
        }
        toc_col <- toc_col + 1
        if (logging) {
            end.time <- Sys.time()
            print(paste0(end.time, " -- banner generation: ", banner_name, bna, " -- end -- elapsed: ", round(difftime(end.time, start.time, units = "mins"), 2), " mins"))
        }
    }
    if (append_text != "") {
        worksheet_name <- "Notes"
        openxlsx::addWorksheet(wb, worksheet_name, gridLines = show_grid_lines, header=header, footer=footer, orientation=orientation)
        openxlsx::writeData(wb, worksheet_name, append_text, startCol = 1, startRow = 1)
    }
    
    if (!save_workbook){
        return(wb)
    }
    
    if (logging) {
        start.time <- Sys.time()
        print(paste(start.time, "-- workbook save -- start"))
    }
    openxlsx::saveWorkbook(wb, paste0(filename, ".xlsx"), overwrite = TRUE)
    if (logging) {
        end.time <- Sys.time()
        print(paste(end.time, "-- workbook save -- end -- elapsed: ", round(difftime(end.time, start.time, units = "mins"), 2), "mins"))
    }
    
    if (logging) {
        end.time.wb <- Sys.time()
        print(paste(end.time.wb, "-- workbook generation -- end -- elapsed: ", round(difftime(end.time.wb, start.time.wb, units = "mins"), 2), "mins"))
    }
    if (return_data) {
        return(invisible(x))
    }
}
