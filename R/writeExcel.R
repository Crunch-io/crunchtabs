
#' Generate Excel Reports: Toplines and Banners
#'
#' \code{writeExcel} produces publication-quality Excel reports:
#' Toplines (one-way frequency tables) or Banners (cross tabulations)
#'
#' @param data_summary An object of class \code{Toplines} or \code{Crosstabs}.
#' @param filename character. The name of the output file (without an extension).
#' @param wb An openxlsx Workbook object to add to. Useful for custom front pages.
#' @param theme An object of class \code{Theme}.
#' @param title character. An optional title. Defaults to the title provided in the summary.
#' @param subtitle character. An optional subtitle. Defaults to an empty string.
#' @param table_of_contents logical. Should a list of tables be included at the start
#' of the report. Defaults to \code{FALSE}.
#' @param n_or_percent character vector. Should the data be returned as counts or percents? 
#' Valid values are 'counts', 'percents' or both. 
#' @param hypothesis_test logical. Should hypothesis testing be shown in the tabs?
#' ** Not yet implemented. Defaults to \code{FALSE}.
#' @param logging logical. Should basic information related to the function execution
#' be printed? Defaults to \code{FALSE}.
#' @param save_workbook logical. Should the Workbook be saved to an excel file or returned for the 
#' user to continue editing? Defaults to \code{TRUE}.
#' @return If \code{save_workbook} is set to \code{FALSE}, the openxlsx Workbook object is returned
#' allowing the user to continue editing it before saving. Otherwise the data is returned as it was provided.
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
writeExcel <- function(data_summary, filename = getName(data_summary), wb = NULL, theme = theme_default(), 
    title = getName(data_summary), subtitle = NULL, table_of_contents = FALSE, n_or_percent = c("percents", "counts"), 
    hypothesis_test = FALSE, logging = FALSE, save_workbook = TRUE) {
    
    if (is.null(filename) && !save_workbook) {
        stop("No filename provided. If save_workbook is true, a filename must be provided.")
    }
    
    if (!is.null(wb) && class(wb) != "Workbook") {
        wrong_class_error(wb, "Workbook", "wb", null = TRUE)
    }
    
    if (!all(n_or_percent %in% c("counts", "percents"))) {
        stop("`n_or_percent` must be a character vector containing 'counts', 'percents' or both, not ", paste0("'", n_or_percent, "'", collapse = ', '))
    }
    
    theme_validator(theme)
    
    wrong_class_error(data_summary, "CrunchTabs", "data_summary")
    if (!any(c("Toplines", "Crosstabs") %in% class(data_summary))) {
        stop("The expected class for `data_summary` is either Toplines, CrunchTabs or Crosstabs CrunchTabs, not ", collapse_items(class(data_summary)))
    }
    
    return(writeReportGeneral(data_summary = data_summary, banner = data_summary$banner, filename = filename, wb = wb, theme = theme, 
        title = title, subtitle = subtitle, table_of_contents = table_of_contents, n_or_percent = n_or_percent, 
        hypothesis_test = hypothesis_test, logging = logging, save_workbook = save_workbook))
}

create_styles <- function(theme){
    
    get_format_info <- function(format_data, info_name, elem) {
        if (!is.null(format_data) && info_name %in% names(format_data)) format_data[[info_name]][[elem]]
    }
    find_null_or_base <- function(theme, info_name, elem){
        x <- if (info_name %in% names(theme)) theme[[info_name]][[elem]]
        if (is.null(x) || (!is.null(theme[[elem]]) && x %in% theme[[elem]])) return(NULL)
        return(x)
    }
    
    numFmt <- paste0("0", if (theme$digits > 0) paste0(".", paste0(rep(0, theme$digits), collapse = "")))
    numFmtProp <- paste0(numFmt, if (theme$percent_format_data) "%")
    
    style_list <- sapply(setdiff(grep("^format_", names(theme), value = TRUE), "format_label_column"), function(v) {
        if (!is.null(theme[[v]])) {
            openxlsx::createStyle(fontName = find_null_or_base(theme, v, "font"), 
                fontSize = find_null_or_base(theme, v, "font_size"),
                fontColour = find_null_or_base(theme, v, "font_color"),
                border = if (!is.null(get_format_info(theme, v, "border_style"))) if(is.null(get_format_info(theme, v, "border_where"))) "TopBottomLeftRight" else get_format_info(theme, v, "border_where"),
                borderColour = get_format_info(theme, v, "border_color"),
                borderStyle = get_format_info(theme, v, "border_style"),
                fgFill = get_format_info(theme, v, "background_color"),
                halign = get_format_info(theme, v, "halign"),
                valign = get_format_info(theme, v, "valign"),
                textDecoration = get_format_info(theme, v, "decoration"), 
                wrapText = get_format_info(theme, v, "wrap_text"))
        }
    })
    
    style_list$format_label_column <- openxlsx::createStyle(
        fontName = find_null_or_base(theme, "format_label_column", "font"), 
        fontSize = find_null_or_base(theme, "format_label_column", "font_size"),
        fontColour = find_null_or_base(theme, "format_label_column", "font_color"),
        border = if (!theme$format_label_column$extend_borders)"TopBottomLeftRight",
        borderStyle = "none",
        fgFill = get_format_info(theme, "format_label_column", "background_color"),
        halign = get_format_info(theme, "format_label_column", "halign"),
        valign = get_format_info(theme, "format_label_column", "valign"),
        textDecoration = get_format_info(theme, "format_label_column", "decoration"), 
        wrapText = get_format_info(theme, "format_label_column", "wrap_text"))
    
    style_list$body_counts <- openxlsx::createStyle(numFmt = numFmt, halign = theme$halign, valign = theme$valign)
    style_list$body_proportions <- openxlsx::createStyle(numFmt = numFmtProp, halign = theme$halign, valign = theme$valign)
    style_list$split_border <- openxlsx::createStyle(border = if (!is.null(theme$banner_vars_split$empty_col) && theme$banner_vars_split$empty_col) "LeftRight" else "left",
        borderStyle = if (!is.null(get_format_info(theme, "banner_vars_split", "border_style"))) get_format_info(theme, "banner_vars_split", "border_style") else "None",
        borderColour = get_format_info(theme, "banner_vars_split", "border_color"))

    return(style_list)
}

get_banner_info <- function(banner, theme){
    if (is.null(banner)) return(list(empty_col = TRUE, multicols = NA, multicols_csum = NA, 
        format_cols = 2, border_columns = NULL))
    
    empty_col <- !is.null(theme$banner_vars_split) && theme$banner_vars_split$empty_col
    banner_cols_pos <- cumsum(sapply(banner, function(x) length(x$categories))) + 1
    multicols <- sapply(banner, getNames)
    multicols_csum <- cumsum(c(banner_cols_pos[1], sapply(multicols, function(x) {length(x) + empty_col})))
    format_cols <- if (empty_col) { unlist(sapply(2:length(multicols_csum), function(i) multicols_csum[i-1]:(multicols_csum[i]-2)))
    } else { multicols_csum[[1]]:(multicols_csum[[length(multicols_csum)]] - 1 - empty_col) }
    
    border_columns <- if (empty_col) { multicols_csum[2:(length(multicols_csum)-1)]-1 
    } else { multicols_csum[2:(length(multicols_csum)-1)] }
    
    list(empty_col = empty_col, multicols = multicols, multicols_csum = multicols_csum, 
        format_cols = format_cols, border_columns = border_columns)
}

get_data <- function(data, item_name, empty_col, round){
    tmp_data <- lapply(seq_along(data), function(bv) {
        dt <- if (round) round(data[[bv]][[item_name]]) else data[[bv]][[item_name]]
        if (is.vector(dt)) return(c(dt, if (empty_col) as.numeric(NA)))
        if (!is.vector(dt)) return(cbind(dt, if (empty_col) as.numeric(NA)))
    })
    if (is.null(unlist(tmp_data))) return(NULL)
    if (!is.null(dim(tmp_data[[1]])) && (nrow(tmp_data[[1]]) != 1 || ncol(tmp_data[[1]]) != 1)) { return(do.call(cbind, tmp_data)) }
    return(unlist(tmp_data))
}

write_and_style <- function(wb, ws, data, style, start_row, cols, write_as_rows){
    if (is.null(style)) return(start_row)
    if (write_as_rows && is.vector(data)) data <- data.frame(data)
    if (!write_as_rows && is.vector(data)) data <- rbind(data)
    data <- data.frame(lapply(data, function(d) {
        tryCatch(as.numeric(as.character(d)), warning=function(w) return(d))
    }))
    openxlsx::writeData(wb, ws, data, startCol = min(cols), startRow = start_row, colNames = FALSE)
    openxlsx::addStyle(wb, ws, style, rows = start_row:(start_row + nrow(data) - 1), cols = cols, stack = TRUE, gridExpand = TRUE)
    return(start_row + nrow(data))
}

write_report_desc <- function(wb, ws, theme, styles, title, subtitle) {
    
    if (!is.null(theme$logo$file)) {
        openxlsx::insertImage(wb, ws, file = theme$logo$file,
            startRow = theme$logo$startRow,
            startCol = theme$logo$startCol,
            width = theme$logo$width,
            height = theme$logo$height,
            units = theme$logo$units,
            dpi = theme$logo$dpi)
    }
    
    start_row <- 1
    if (!is.null(theme$format_title) && !is.null(title)){
        start_row <- write_and_style(wb, ws, title, styles$format_title, start_row = start_row, cols = 1, write_as_rows = TRUE)
    }
    if (!is.null(theme$format_subtitle) && !is.null(subtitle)) {
        start_row <- write_and_style(wb, ws, subtitle, styles$format_subtitle, start_row = start_row, cols = 1, write_as_rows = TRUE)
    }
    return(start_row + 1)
}

write_banner_panel <- function(wb, ws, theme, styles, banner, title, subtitle, 
    start_row, banner_info, percent_row) {
    
    start_row <- sr <- write_report_desc(wb = wb, ws = ws, theme = theme, styles = styles, 
        title = title, subtitle = subtitle)
    
    if (!is.null(banner)){
        if (!is.null(theme$format_banner_labels)){
            data <- unlist(sapply(seq_along(banner), function(bv) {
                c(getName(banner[[bv]]), rep("", times = length(banner_info$multicols[[bv]]) - 1 + banner_info$empty_col))
            }))
            start_row <- write_and_style(wb, ws, data = data, style = styles$format_banner_labels, start_row = start_row, cols = banner_info$format_cols, write_as_rows = FALSE)
            lapply(setdiff(seq_along(banner_info$multicols), 1), function(bv) {
                openxlsx::mergeCells(wb, ws, cols = banner_info$multicols_csum[bv]:(banner_info$multicols_csum[bv + 1] -
                        1 - banner_info$empty_col), rows = start_row - 1)
            })
        }
        start_row <- write_and_style(wb, ws, data = get_data(banner, "categories", banner_info$empty_col, round = FALSE), 
            style = styles$format_banner_categories, start_row = start_row, cols = banner_info$format_cols, write_as_rows = FALSE)
        if (!is.null(theme$format_weighted_n) && theme$format_weighted_n$position_fixed) {
            start_row <- write_and_style(wb, ws, data = c(theme$format_weighted_n$name, get_data(banner, "weighted_n", banner_info$empty_col, round = TRUE)), 
                style = styles$format_weighted_n, start_row = start_row, cols = c(1, banner_info$format_cols), write_as_rows = FALSE)
        }
        if (!is.null(theme$format_unweighted_n) && theme$format_unweighted_n$position_fixed) {
            start_row <- write_and_style(wb, ws, data = c(theme$format_unweighted_n$name, get_data(banner, "unweighted_n", banner_info$empty_col, round = TRUE)), 
                style = styles$format_unweighted_n, start_row = start_row, cols = c(1, banner_info$format_cols), write_as_rows = FALSE)
        }
        if (percent_row) {
            data <- as.data.frame(lapply(seq_along(banner), function(bv) {
                t(c(rep("%", times = length(banner_info$multicols[[bv]])), if (banner_info$empty_col) ""))
            }))
            start_row <- write_and_style(wb, ws, data = data, style = styles$format_banner_categories, start_row = start_row, cols = banner_info$format_cols, write_as_rows = FALSE)
            openxlsx::addStyle(wb, ws, openxlsx::createStyle(textDecoration = NULL), rows = start_row - 1, cols = banner_info$format_cols, stack = TRUE)
        }
        openxlsx::addStyle(wb, ws, styles$format_totals_column, rows = sr:(start_row - 1), cols = 2, stack = TRUE)
        openxlsx::addStyle(wb, ws, styles$split_border, rows = sr:(start_row - 1), 
            cols = if (banner_info$empty_col) banner_info$border_columns, gridExpand = TRUE, stack = TRUE)
    }
    
    openxlsx::addStyle(wb, ws, styles$format_label_column, rows = sr:(start_row - 1), cols = 1, gridExpand = FALSE, stack = TRUE)
    
    openxlsx::pageSetup(wb, ws, printTitleRows = (1 + ifelse(!is.null(theme$format_title), length(title), 0)):start_row)
    openxlsx::freezePane(wb, ws, firstActiveRow = start_row, firstActiveCol = theme$freeze_column + 1)
    
    return(start_row)
}

write_var_header <- function(wb, ws, var, theme, styles, start_row, toc_sheet, toc_row, toc_col) {
    if_there <- function(str) { if (!(is.null(str) || is.na(str) || str == "")) return(str) }
    var_info <- list(format_var_alias = if_there(getAlias(var)),
        format_var_name = if_there(getName(var)),
        format_var_description = if_there(getDescription(var)),
        format_var_filtertext = if_there(getNotes(var)),
        format_var_subname = if_there(var$subname))
    number <- if_there(var$settings$number)
    if (!is.null(toc_sheet)) {
        openxlsx::writeFormula(wb, toc_sheet, startCol = toc_col, startRow = toc_row, 
            x = openxlsx::makeHyperlinkString(sheet = ws,
                row = start_row - 1, col = 1, text = var_info$format_var_alias))
        openxlsx::writeData(wb, toc_sheet, paste0(c(var_info$format_var_subname, var_info$format_var_description), collapse = " - "), startCol = toc_col+1, startRow = toc_row)
    }
    for (info_name in intersect(names(theme), names(var_info))){
        if (!is.null(theme[[info_name]]) && !is.null(var_info[[info_name]])) {
            if (is.null(theme[[info_name]]$repeat_for_subs) || theme[[info_name]]$repeat_for_subs || var$subnumber %in% 1){
                if (!is.null(theme[[info_name]]$include_q_number) && theme[[info_name]]$include_q_number){
                    var_info[[info_name]] <- paste0(number, ". ", var_info[[info_name]])
                }
                if (!is.null(theme[[info_name]]$include_alias) && theme[[info_name]]$include_alias){
                    var_info[[info_name]] <- paste0(var_info$format_var_alias, " - ", var_info[[info_name]])
                }
                start_row <- write_and_style(wb, ws, data = var_info[[info_name]], style = styles[[info_name]], start_row = start_row, cols = 1, write_as_rows = FALSE)
            }
        }
    }
    return(start_row)
}

#' @importFrom stats setNames
writeExcelVar <- function(wb, ws, theme, styles, banner_name, var, banner_info, start_row, start_col,
    toc_sheet, toc_row, toc_col, hypothesis_test, proportions) {
    
    start_row <- sr <- write_var_header(wb = wb, ws = ws, var = var, theme = theme, styles = styles, 
        start_row = start_row, toc_sheet = toc_sheet, toc_row = toc_row, toc_col = toc_col)
    weighted_n_top <- !is.null(theme$format_weighted_n) && theme$format_weighted_n$position_top
    unweighted_n_top <- !is.null(theme$format_unweighted_n) && theme$format_unweighted_n$position_top
    totals_top <- !var$settings$no_totals && !is.null(theme$format_totals_row) && theme$format_totals_row$position_top
    weighted_n_bottom <- !is.null(theme$format_weighted_n) && theme$format_weighted_n$position_bottom
    unweighted_n_bottom <- !is.null(theme$format_unweighted_n) && theme$format_unweighted_n$position_bottom
    totals_bottom <- !var$settings$no_totals && !is.null(theme$format_totals_row) && theme$format_totals_row$position_bottom
    min_base <- !is.null(theme$format_min_base$min_base) && theme$format_min_base$min_base >= 0
    show_totals <- totals_top | totals_bottom
    show_mean <- !is.null(theme$format_mean) && !is.null(var$crosstabs[[banner_name]]$Total$mean)
    show_median <- !is.null(theme$format_medians) && var$mean_median
    
    style_if <- function(ifStatement, start_row, style, data, cols){
        if (ifStatement){
            nrow <- if (!is.null(dim(data))) { nrow(data) } else { 1 }
            openxlsx::addStyle(wb = wb, sheet = ws, style = style, rows = start_row:(start_row + nrow - 1), 
                cols = cols, gridExpand = TRUE, stack = TRUE)
            start_row <- start_row + nrow(data)
        }
        return(start_row)
    }
    
    data <- as.data.frame(lapply(var$crosstabs[[banner_name]], function(x) {
        d <- as.data.frame(reformatResults(x, proportions = proportions, theme = theme, reformat = FALSE))
        if (is.null(x$type) || x$type %in% c("categorical", "categorical_array", "multiple_response")){
            if (proportions && !theme$percent_format_data) {
                d[] <- d * 100
            }
        }
        if (banner_info$empty_col) cbind(d, as.numeric(NA)) else d
    }), check.names = FALSE)
    colnames(data) <- gsub('.*\\.', '', names(data))
    
    unweighted_n_data <- clean_data(get_data(var$crosstabs[[banner_name]], "unweighted_n", banner_info$empty_col, round = FALSE), data)
    weighted_n_data <- clean_data(get_data(var$crosstabs[[banner_name]], "totals_counts", banner_info$empty_col, round = FALSE), data)
    if (show_totals) {
        totals_data <- clean_data(get_data(var$crosstabs[[banner_name]], if (proportions) "totals_proportions" else "totals_counts", banner_info$empty_col, round = FALSE), data)
    }
    if (show_mean) { 
        mean_data <- clean_data(get_data(var$crosstabs[[banner_name]], "mean", banner_info$empty_col, round = FALSE), data) 
        var$inserts <- c(var$inserts, "Mean")
    }
    if (show_median) { 
        median_data <- clean_data(get_data(var$crosstabs[[banner_name]], "median", banner_info$empty_col, round = FALSE), data) 
        var$inserts <- c(var$inserts, "Median")
    }
    if (is.null(theme$format_headers) && any(var$inserts %in% "Heading")){
        data <- data[-c(which(var$inserts %in% "Heading")),]
        var$inserts <- var$inserts[-c(which(var$inserts %in% "Heading"))]
    }
    if (is.null(theme$format_subtotals) && any(var$inserts %in% "Subtotal")){
        data <- data[-c(which(var$inserts %in% "Subtotal")),]
        var$inserts <- var$inserts[-c(which(var$inserts %in% "Subtotal"))]
    }

    row_names <- c(
        if (weighted_n_top) paste0(theme$format_weighted_n$name, if (nrow(weighted_n_data) != 1) paste(":", c("Min", "Max"))),
        if (unweighted_n_top) paste0(theme$format_unweighted_n$name, if (nrow(unweighted_n_data) != 1) paste(":", c("Min", "Max"))),
        if (totals_top) theme$format_totals_row$name,
        rownames(data),
        if (show_mean) theme$format_means$name,
        if (show_median) theme$format_medians$name,
        if (totals_bottom) theme$format_totals_row$name,
        if (weighted_n_bottom) paste0(theme$format_weighted_n$name, if (nrow(weighted_n_data) != 1)  paste(":", c("Min", "Max"))),
        if (unweighted_n_bottom) paste0(theme$format_unweighted_n$name, if (nrow(unweighted_n_data) != 1) paste(":", c("Min", "Max")))
    )
    
    if (!is.null(theme$digits_final) && !is.infinite(theme$digits_final)){
        data <- round(data, theme$digits_final + (proportions && theme$percent_format_data)*2)
        if (show_totals) totals_data <- round(totals_data, theme$digits_final + (proportions && theme$percent_format_data)*2)
        weighted_n_data <- round(weighted_n_data, theme$digits_final)
        if (show_mean) mean_data <- round(mean_data, theme$digits_final + 2)
        if (show_median) median_data <- round(median_data, theme$digits_final + 2)
    }
    
    all_data <- rbind(if (weighted_n_top) weighted_n_data,
        if (unweighted_n_top) unweighted_n_data,
        if (totals_top) totals_data, 
        data, 
        if (show_mean) mean_data,
        if (show_median) median_data,
        if (totals_bottom) totals_data,
        if (weighted_n_bottom) weighted_n_data,
        if (unweighted_n_bottom) unweighted_n_data)

    topline_array <- is(var, "ToplineCategoricalArray")
    if (topline_array) {
        banner_info$format_cols <- seq_along(unweighted_n_data)
        names(all_data) <- gsub('Total\\.', '', names(all_data))
        all_data <- all_data[, 1:(ncol(all_data)-1)]
    }
    
    openxlsx::writeData(wb = wb, sheet = ws, x = all_data, startCol = 2, startRow = start_row, colNames = topline_array,
        headerStyle = styles$format_banner_categories,
        borders = ifelse(!is.null(theme$table_border$border_style), "surrounding", "none"), borderColour = theme$table_border$border_color,
        borderStyle = ifelse(!is.null(theme$table_border$border_style), theme$table_border$border_style, "thin"))
    start_row <- sr <- start_row + topline_array
    
    style_if(!is.null(styles$format_totals_column), start_row, styles$format_totals_column, all_data, 2)
    start_row <- style_if(weighted_n_top, start_row, styles$format_weighted_n, weighted_n_data, cols=c(1, banner_info$format_cols))
    start_row <- style_if(unweighted_n_top, start_row, styles$format_unweighted_n, unweighted_n_data, cols=c(1, banner_info$format_cols))
    start_row <- style_if(totals_top, start_row, styles$format_totals_row, totals_data, cols=c(1, banner_info$format_cols))
    
    openxlsx::addStyle(wb = wb, sheet = ws, style = if (proportions) styles$body_proportions else styles$body_counts, 
        rows = start_row:(start_row + nrow(data) - 1), cols = banner_info$format_cols, gridExpand = TRUE)
    
    ## TODO: make this work better w/ MR vars. 
    if (min_base) {
        unweighted_n_data_num <- sapply(unweighted_n_data[1, ], function(x) as.numeric(as.character(x)))
        if (class(unweighted_n_data_num) == "matrix") unweighted_n_data_num <- c(unweighted_n_data_num[nrow(unweighted_n_data_num),])
        min_cell_mask <- !is.na(unweighted_n_data_num) & unweighted_n_data_num <= theme$format_min_base$min_base
        if (any(min_cell_mask)) {
            if (!is.null(theme$format_min_base$mask)) {
                mask_data <- rep(theme$format_min_base$mask, nrow(data) + show_mean + show_median)
                mask_data[var$inserts %in% "Heading"] <- NA
                for (bki in which(min_cell_mask)) { 
                    write_and_style(wb = wb, ws = ws, data = mask_data, 
                        style = styles$format_min_base, cols = bki + 1, start_row = start_row, write_as_rows = TRUE) 
                }
            } else {
                openxlsx::addStyle(wb, ws, styles$format_min_base, rows = start_row:(start_row + nrow(data) + show_mean + show_median - 1),
                    cols = start_col + which(min_cell_mask) - 1, gridExpand = TRUE, stack = TRUE)
            }
        }
    }
    
    if (any(var$inserts %in% "Heading")) {
        openxlsx::addStyle(wb, ws, styles$format_headers, rows = (start_row + which(var$inserts %in% "Heading") - 1),
            cols =  c(1, banner_info$format_cols), gridExpand = TRUE, stack = TRUE)
    }
    if (any(var$inserts %in% "Subtotal")) {
        openxlsx::addStyle(wb, ws, styles$format_subtotals, rows = (start_row + which(var$inserts %in% "Subtotal") - 1),
            cols =  c(1, banner_info$format_cols), gridExpand = TRUE, stack = TRUE)
    }

    # if (hypothesis_test) {
    #     hypho_test(wb, ws, var, banner_name, margin = 2, banner_info$empty_col, styles, sr, ccol = start_col)
    # }
    
    start_row <- start_row + nrow(data)
    
    wt <- if (!is.null(theme$format_min_base$mask)) theme$format_min_base$mask else "-"
    if (show_mean && !min_base) {
        na_out <- which(is.na(mean_data) & !is.na(weighted_n_data)) + 1
        for (bki in na_out) openxlsx::writeData(wb = wb, sheet = ws, x = wt, startCol = bki + 1, startRow = start_row)
    }
    start_row <- style_if(show_mean, start_row, styles$body_counts, mean_data, cols = banner_info$format_cols)
    start_row <- style_if(show_mean, start_row - show_mean, styles$format_means, mean_data, cols = c(1, banner_info$format_cols))
    if (show_median && !min_base) {
        na_out <- which(is.na(median_data) & !is.na(weighted_n_data)) + 1
        for (bki in na_out) openxlsx::writeData(wb = wb, sheet = ws, x = wt, startCol = bki + 1, startRow = start_row)
    }
    start_row <- style_if(show_median, start_row, styles$body_counts, median_data, cols = banner_info$format_cols)
    start_row <- style_if(show_median, start_row - show_median, styles$format_medians, median_data, cols = c(1, banner_info$format_cols))
    
    start_row <- style_if(totals_bottom, start_row, styles$format_totals_row, totals_data, cols=c(1, banner_info$format_cols))
    start_row <- style_if(weighted_n_bottom, start_row, styles$format_weighted_n, weighted_n_data, cols=c(1, banner_info$format_cols))
    start_row <- style_if(unweighted_n_bottom, start_row, styles$format_unweighted_n, unweighted_n_data, cols=c(1, banner_info$format_cols))

    if (!is.null(banner_info$border_columns)){
        openxlsx::addStyle(wb, ws, styles$split_border, rows = sr:(start_row - 1), 
            cols = banner_info$border_columns, gridExpand = TRUE, stack = TRUE)
    }
    
    openxlsx::writeData(wb = wb, sheet = ws, x = row_names, startCol = 1, startRow = sr)
    openxlsx::addStyle(wb, ws, styles$format_label_column, rows = sr:(start_row - 1),
        cols = 1, stack = TRUE)
    
    return(start_row)
}    

hypho_test <- function(wb, ws, cross_tab_var, banner_name, margin, banner_info, styles, crow, ccol) {
    pvals <- c(0.1, 0.05, 0.01, 0.001)
    pcol_pos <- c("bg_col_green1", "bg_col_green2", "bg_col_green3", "bg_col_green4")
    pcol_neg <- c("bg_col_red1", "bg_col_red2", "bg_col_red3", "bg_col_red4")

    pvals_row <- as.data.frame(lapply(cross_tab_var$crosstabs[[banner_name]], function(x) {
        d <- x$pvals_col
        d <- as.data.frame(d)
        if (banner_info$empty_col) cbind(d, "" ) else d
    }))
    if (banner_info$empty_col) {
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

writeReportGeneral <- function(data_summary, banner, filename, wb, theme, 
    title, subtitle, table_of_contents, n_or_percent, hypothesis_test, 
    logging, save_workbook) {
    
    if (is.null(data_summary$metadata$weight) && !is.null(theme$format_weighted_n$name)) {
        warning("Data is unweighted. `weighted_n` row will not appear.", call. = FALSE)
        theme$format_weighted_n <- NULL
    }
    
    if (is.null(banner)){
        theme$freeze_column <- 1
    }
    
    if (logging) {
        start.time.wb <- Sys.time()
        print(paste(start.time.wb, "-- workbook generation -- start"))
    }
    
    if (is.null(wb)) wb <- openxlsx::createWorkbook()
    openxlsx::modifyBaseFont(wb, fontSize = theme$font_size, fontColour = theme$font_color, fontName = theme$font)
    
    styles <- create_styles(theme)
    
    toc_col <- toc_row <- toc_start_row <- if (table_of_contents) 1
    toc_sheet <- if (table_of_contents) "TOC"
    if (table_of_contents) {
        openxlsx::addWorksheet(wb, toc_sheet, gridLines = theme$show_grid_lines, header = theme$header, footer = theme$footer, orientation = theme$orientation)
        toc_row <- toc_start_row <- write_report_desc(wb = wb, ws = toc_sheet, theme = theme, styles = styles, 
            title = title, subtitle = subtitle)
        openxlsx::freezePane(wb, toc_sheet, firstActiveRow = toc_row + 1)
    }
    
    ws_subtitles <- if (length(n_or_percent) == 2) paste0("_", toupper(substr(n_or_percent, 1, 1)))
    
    banner_names <- if (is.null(banner)) "Results" else names(banner)
    worksheet_names <- sapply(banner_names, function(bn){
        ws_names <- if (theme$one_per_sheet) names(data_summary$results) else bn
        ws_names[nchar(ws_names) > 25] <- paste0(strtrim(ws_names[nchar(ws_names) > 25], 25), which(nchar(ws_names) > 25))
        if (theme$one_per_sheet && length(banner_names) > 1) ws_names <- paste0(ws_names, "_", which(banner_names %in% bn))
        return(ws_names)
    }, simplify = FALSE)
    make_worksheets <- sapply(unlist(worksheet_names), function(x) paste0(x, ws_subtitles))
    for (worksheet_name in make_worksheets){
        openxlsx::addWorksheet(wb, worksheet_name, gridLines = theme$show_grid_lines, header = theme$header, footer = theme$footer, orientation = theme$orientation)
        openxlsx::setColWidths(wb, worksheet_name, cols = 1, theme$format_label_column$col_width)
    }
    
    if (length(n_or_percent) == 2) { banner_names <- rep(banner_names, 2) }
    
    for (bix in seq_along(banner_names)) {
        banner_name <- banner_names[bix]
        bna <- ws_subtitles[duplicated(banner_names)[bix] + 1]
        proportions <- n_or_percent[duplicated(banner_names)[bix] + 1] %in% "percents"
        if (logging) {
            start.time <- Sys.time()
            print(paste0(start.time, " -- banner generation: ", banner_name, bna, " -- start"))
        }
        banner_info <- get_banner_info(banner = banner[[banner_name]], theme = theme)
        if (table_of_contents) {
            openxlsx::writeData(wb, toc_sheet, paste0(banner_name, bna), startRow = toc_start_row, startCol = toc_col)
        }
        if (!theme$one_per_sheet) {
            worksheet_name <- paste0(worksheet_names[[banner_name]], bna)
            last_row_used <- write_banner_panel(wb = wb, ws = worksheet_name, theme = theme, styles = styles, 
                    banner = banner[[banner_name]], title = title, subtitle = subtitle, 
                    banner_info = banner_info, percent_row = !theme$percent_format_data & proportions)
        }
        
        for (vidx in seq_along(data_summary$results)) {
            toc_row <- toc_row + 1 
            if (theme$one_per_sheet) {
                worksheet_name <- paste0(worksheet_names[[banner_name]][[vidx]], bna)
                last_row_used <- write_banner_panel(wb = wb, ws = worksheet_name, theme = theme, styles = styles, 
                    banner = banner[[banner_name]], title = title, subtitle = subtitle, 
                    banner_info = banner_info, percent_row = !theme$percent_format_data & proportions)
            }
            last_row_used <- writeExcelVar(wb = wb, ws = worksheet_name, var = data_summary$results[[vidx]], theme = theme, 
                styles = styles, banner_name = banner_name, banner_info = banner_info,
                start_row = last_row_used + 1, start_col = 2, toc_sheet = toc_sheet, toc_row = toc_row, toc_col = toc_col, 
                hypothesis_test = hypothesis_test, proportions = proportions)
            
        }
        toc_col <- toc_col + 1
        toc_row <- toc_start_row
        if (logging) {
            end.time <- Sys.time()
            print(paste0(end.time, " -- banner generation: ", banner_name, bna, " -- end -- elapsed: ", round(difftime(end.time, start.time, units = "mins"), 2), " mins"))
        }
    }
    if (table_of_contents) {
        openxlsx::addStyle(wb, toc_sheet, styles$format_banner_categories, rows = toc_start_row, cols = 1:(toc_col - 1), stack = FALSE)
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
    return(invisible(data_summary))
}



