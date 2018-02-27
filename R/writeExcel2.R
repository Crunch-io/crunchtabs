#' @export
writeExcel <- function(data_summary, filename, wb = NULL, theme = theme_default(), 
    title = getName(data_summary), subtitle = NULL, header = NULL, footer = NULL, 
    append_text = NULL, table_of_contents = FALSE, 
    n_or_percent = "percent", hypothesis_test = FALSE, 
    logging = FALSE, return_data = FALSE, save_workbook = TRUE) {
    
    if (missing(filename)) {
        stop("No valid filename provided.")
    }
    
    if (!is.null(wb) && class(wb) != 'Workbook') {
        wrong_class_error(wb, 'Workbook', 'wb')
    }
    
    if (is.null(crosstabs_summary$metadata$weight) && !is.null(theme$format_weighted_n$name)) {
        warning('Data is unweighted. "weighted_n" row will not appear.', call. = FALSE)
    }
    
    UseMethod("writeExcel", data_summary)
}

#' @export
writeExcel.default <- function(data_summary, ...) {
    wrong_class_error(data_summary, c("Crosstabs"), "data_summary")
}

#' #' @export
#' writeExcel.Toplines <- function(data_summary, filename, wb = NULL, theme = theme_default(), 
#'     title = getName(data_summary), subtitle = NULL, header = NULL, footer = NULL, 
#'     append_text = NULL, table_of_contents = FALSE, 
#'     n_or_percent = "percent", hypothesis_test = FALSE, 
#'     logging = FALSE, return_data = FALSE, save_workbook = TRUE) {
#' 
#'     writeReportGeneral(x = data_summary, banner = NULL, filename = filename, wb = wb, theme = theme, 
#'         title = title, subtitle = subtitle, header = header, footer = footer, 
#'         append_text = append_text, table_of_contents = table_of_contents, 
#'         n_or_percent = n_or_percent, hypothesis_test = hypothesis_test, 
#'         logging = logging, return_data = return_data, save_workbook = save_workbook)
#' }

#' @export
writeExcel.Crosstabs <- function(data_summary, filename, wb = NULL, theme = theme_default(), 
    title = getName(data_summary), subtitle = NULL, header = NULL, footer = NULL, 
    append_text = NULL, table_of_contents = FALSE, 
    n_or_percent = "percent", hypothesis_test = FALSE, 
    logging = FALSE, return_data = FALSE, save_workbook = TRUE) {
    
    writeReportGeneral(x = data_summary, banner = NULL, filename = filename, wb = wb, theme = theme, 
        title = title, subtitle = subtitle, header = header, footer = footer, 
        append_text = append_text, table_of_contents = table_of_contents, 
        n_or_percent = n_or_percent, hypothesis_test = hypothesis_test, 
        logging = logging, return_data = return_data, save_workbook = save_workbook)
}

create_styles <- function(theme){
    
    get_format_info <- function(format_data, info_name, elem) {
        if (!is.null(format_data) && info_name %in% names(format_data)) format_data[[info_name]][[elem]]
    }
    get_decoration_data <- function(data_info, elem) {
        if (!is.null(data_info)) data_info[[elem]]
    }
    
    numFmt <- paste0("0", if (theme$digits > 0) paste0(".", paste0(rep(0, theme$digits), collapse = "")))
    numFmtProp <- paste0(numFmt, if (theme$percent_format_data) "%")
    
    style_list <- sapply(grep('^format_', names(theme), value = TRUE), function(v) {
        if (!is.null(theme[[v]])) {
            openxlsx::createStyle(fontName = get_format_info(theme, v, "font"), 
                fontSize = get_format_info(theme, v, "font_size"),
                fontColour = get_format_info(theme, v, "font_color"),
                numFmt = if (v %in% c("format_weighted_n", "format_unweighted_n")) "0" else "GENERAL",
                border = if (!is.null(get_format_info(theme, v, "border_style"))) ifelse(is.null(get_format_info(theme, v, "border_where")), "TopBottomLeftRight", get_format_info(theme, v, "border_where")),
                borderColour = get_format_info(theme, v, "border_color"),
                borderStyle = get_format_info(theme, v, "border_style"),
                fgFill = get_format_info(theme, v, "background_color"),
                halign = get_format_info(theme, v, "halign"),
                valign = get_format_info(theme, v, "valign"),
                textDecoration = get_format_info(theme, v, "decoration"), 
                wrapText = get_format_info(theme, v, "wrap_text"))
        }
    })
    
    if (!is.null(get_format_info(theme, "table_border", "border_style"))){
        style_list$body_border_top <- openxlsx::createStyle(border = "top", borderStyle = get_format_info(theme, "table_border", "border_style"), borderColour = get_format_info(theme, "table_border", "border_color"))
        style_list$body_border_bottom <- openxlsx::createStyle(border = "bottom", borderStyle = get_format_info(theme, "table_border", "border_style"), borderColour = get_format_info(theme, "table_border", "border_color"))
        style_list$body_border_left <- openxlsx::createStyle(border = "left", borderStyle = get_format_info(theme, "table_border", "border_style"), borderColour = get_format_info(theme, "table_border", "border_color"))
        style_list$body_border_right <- openxlsx::createStyle(border = "right", borderStyle = get_format_info(theme, "table_border", "border_style"), borderColour = get_format_info(theme, "table_border", "border_color"))
    }
    
    style_list$body_text <- openxlsx::createStyle(halign = theme$halign, valign = theme$valign)
    style_list$body_counts <- openxlsx::createStyle(numFmt = numFmt, halign = theme$halign, valign = theme$valign)
    style_list$body_proportions <- openxlsx::createStyle(numFmt = numFmtProp, halign = theme$halign, valign = theme$valign)
    # style_list$numeric <- openxlsx::createStyle(numFmt = numFmt)
    # style_list$categorical <- openxlsx::createStyle(numFmt = numFmtProp, halign = "center")
    # style_list$bg_col_green4 <- openxlsx::createStyle(fgFill = "#00CA81")
    # style_list$bg_col_green3 <- openxlsx::createStyle(fgFill = "#7BC99E")
    # style_list$bg_col_green2 <- openxlsx::createStyle(fgFill = "#AFDDC3")
    # style_list$bg_col_green1 <- openxlsx::createStyle(fgFill = "#D7ECD8")
    # style_list$bg_col_red4 <- openxlsx::createStyle(fgFill = "#DA5130")
    # style_list$bg_col_red3 <- openxlsx::createStyle(fgFill = "#E67D58")
    # style_list$bg_col_red2 <- openxlsx::createStyle(fgFill = "#EEA37D")
    # style_list$bg_col_red1 <- openxlsx::createStyle(fgFill = "#FACBAF")
    if (!is.null(get_format_info(theme, "banner_vars_split", "border_style"))) {
        style_list$split_border <- openxlsx::createStyle(border = if (get_format_info(theme, "banner_vars_split", "empty_col")) "LeftRight" else "left",
            borderStyle = get_format_info(theme, "banner_vars_split", "border_style"),
            borderColour = get_format_info(theme, "banner_vars_split", "border_color"))
    } 
    
    return(style_list)
}

write_report_desc <- function(wb, ws, theme, styles, title, subtitle, 
    start_row, start_col, toc_page) {
    
    if (!is.null(theme$logo$file)) {
        openxlsx::insertImage(wb, ws, file = theme$logo$file,
            startRow = theme$logo$startRow,
            startCol = theme$logo$startCol,
            width = theme$logo$width,
            height = theme$logo$height,
            units = theme$logo$units,
            dpi = theme$logo$dpi)
    }
    
    if (!is.null(theme$format_title)){
        openxlsx::writeData(wb, ws, title, startCol = start_col, startRow = start_row)
        openxlsx::addStyle(wb, ws, styles$format_title, rows = start_row, cols = start_col)
        start_row <- start_row + 1
    }
    if (!is.null(theme$format_subtitle)) {
        for (subt in subtitle) {
            openxlsx::writeData(wb, ws, subt, startCol = start_col, startRow = start_row)
            openxlsx::addStyle(wb, ws, styles$format_subtitle, rows = start_row, cols = start_col)
            start_row <- start_row + 1
        }
    }
    start_row <- start_row + 1
    return(start_row)
}

create_banner_panel <- function(wb, ws, theme, styles, banner, title, subtitle, 
    start_row, banner_cols_pos, percent_row) {
    
    start_row <- sr <- write_report_desc(wb = wb, ws = ws, theme = theme, styles = styles, title = title, subtitle = subtitle, 
        start_row = start_row, start_col = 1, toc_page = FALSE)

    if (!is.null(banner)){
        empty_col <- !is.null(theme$banner_vars_split) && theme$banner_vars_split$empty_col
        multicols <- sapply(banner, getNames)
        multicols_csum <- cumsum(c(banner_cols_pos[1], sapply(multicols, function(x) {length(x) + empty_col})))
        format_cols <- if (empty_col) { c(sapply(setdiff(2:length(multicols_csum), function(i) multicols_csum[i-1]:(multicols_csum[i]-2)))) 
            } else multicols_csum[[1]]:(multicols_csum[[length(multicols_csum)]] - 1 - empty_col)
        if (!is.null(theme$format_banner_labels)){
            data <- matrix(unlist(sapply(seq_along(banner), function(bv) {
                c(getName(banner[[bv]]), if (length(multicols[[bv]]) > 1) rep("", times = length(multicols[[bv]]) - 1 + empty_col))
            })), nrow = 1)
            openxlsx::writeData(wb, ws, data, startCol = 2, startRow = start_row, colNames = FALSE)
            openxlsx::addStyle(wb, ws, styles$format_banner_labels, rows = start_row, cols = format_cols, stack = FALSE)
            lapply(seq_along(multicols), function(bv) {
                openxlsx::mergeCells(wb, ws, cols = multicols_csum[bv]:(multicols_csum[bv + 1] -
                        1 - empty_col), rows = start_row)
            })
            start_row <- start_row + 1
        }
        
        data <- matrix(unlist(sapply(seq_along(banner), function(bv) {
            c(multicols[[bv]], if (empty_col) "")
        })), nrow = 1)
        openxlsx::writeData(wb, ws, data, startCol = 2, startRow = start_row, colNames = FALSE)
        openxlsx::addStyle(wb, ws, styles$format_banner_categories, rows = start_row, cols = format_cols, stack = FALSE)
        start_row <- start_row + 1
        
        if (theme$format_weighted_n$position_fixed) {
            openxlsx::writeData(wb, ws, theme$format_weighted_n$name, startCol = 1, startRow = start_row, colNames = FALSE)
            wn <- matrix(unlist(sapply(seq_along(banner), function(bv) {
                c(banner[[bv]]$weighted_n, if (empty_col) "")
            })), nrow=1)
            start_row <- write_bases_data(wb, ws, wn, row=start_row, format_cols = c(1, format_cols), style_data=styles$format_weighted_n)
        }
        if (theme$format_unweighted_n$position_fixed) {
            start_row <- start_row + 2
            openxlsx::writeData(wb, ws, theme$format_unweighted_n$name, startCol = 1, startRow = start_row, colNames = FALSE)
            uwn <- matrix(unlist(sapply(seq_along(banner), function(bv) {
                c(banner[[bv]]$unweighted_n, if (empty_col) "")
            })), nrow = 1)
            start_row <- write_bases_data(wb, ws, uwn, row=start_row, format_cols = c(1, format_cols), style_data=styles$format_unweighted_n)
        }
        if (percent_row) {
            data <- as.data.frame(lapply(seq_along(banner), function(bv) {
                t(c(rep("%", times = length(multicols[[bv]])), if (empty_col) ""))
            }))
            openxlsx::writeData(wb, ws, data, startCol = banner_cols_pos[1], startRow = start_row, colNames = FALSE)
            openxlsx::addStyle(wb, ws, styles$body_text, rows = start_row, cols = format_cols, stack = FALSE)
            if (!is.null(theme$format_banner_categories$border_style)){
                openxlsx::addStyle(wb, ws, openxlsx::createStyle(border = "TopBottom", 
                    borderStyle = theme$format_banner_categories$border_style, borderColour = theme$format_banner_categories$border_color), 
                    rows = start_row, cols = format_cols, stack = FALSE)
            }
        }
        
        if (!is.null(styles$format_totals_column)) {
            openxlsx::addStyle(wb, ws, styles$format_totals_column, rows = sr:start_row, cols = start_col, stack = TRUE)
        }
        
        if (!is.null(styles$split_border)) {
            openxlsx::addStyle(wb, ws, styles$split_border, rows = sr:start_row, cols = multicols_csum[2:length(multicols_csum)], gridExpand = TRUE, stack = TRUE)
        }
    }
    
    openxlsx::freezePane(wb, ws, firstActiveRow = start_row + 1, firstActiveCol = theme$freeze_column + 1)
    
    return(start_row + 1)
}

write_bases_data <- function(wb, ws, data, row, format_cols, style_data) {
    openxlsx::writeData(wb, ws, round(data), startCol = format_cols[1], startRow = row, colNames = FALSE)
    openxlsx::addStyle(wb, ws, style_data, rows = (1:nrow(data)) + row, cols = format_cols, gridExpand = TRUE, stack = TRUE)
    row + nrow(data)
}

writeVarHeader <- function(wb, ws, x, theme, styles, start_col, start_row, 
    toc_sheet, toc_row, toc_col) {
    
    var_info <- list("format_var_alias" = getAlias(x),
        "format_var_name" = getName(x),
        "format_var_description" = getDescription(x),
        "format_var_subname" = if (!is.na(x$subname)) x$subname,
        "format_var_filtertext" = getNotes(x))
    add_toc_info <- !is.null(toc_sheet)
    if (add_toc_info) {
        openxlsx::writeFormula(wb, toc_sheet, startCol = toc_col, startRow = toc_row, 
            x = openxlsx::makeHyperlinkString(sheet = ws,
                row = start_row - 1, col = start_col, text = var_info$format_var_alias))
        openxlsx::writeData(wb, toc_sheet, paste0(c(var_info$format_var_subname, var_info$format_var_description), collapse = ' - '), startCol = toc_col+1, startRow = toc_row)
    }
    for (info_name in names(var_info)){
        if (!is.null(theme[[info_name]])){
            if (!is.null(var_info[[info_name]]) && !is.na(var_info[[info_name]]) && var_info[[info_name]] != "") {
                if (!is.null(theme[[info_name]]$include_alias) && theme[[info_name]]$include_alias){
                    var_info[[info_name]] <- paste0(var_info$format_var_alias, ' - ', var_info[[info_name]])
                }
                start_row <- write_var_info(wb, ws, var_info = var_info[[info_name]], elem_name = info_name, styles = styles, col = start_col, row = start_row)
            }
        }
    }
    start_row
}

write_var_info <- function(wb, ws, var_info, elem_name, styles, col, row) {
    openxlsx::writeData(wb, ws, var_info, startRow = row, startCol = col)
    openxlsx::addStyle(wb, ws, styles[[elem_name]], rows = row, cols = col)
    return(row + 1)
}

#' @importFrom stats setNames
writeExcelVarBanner <- function(wb, ws, theme, styles, banner_name, cross_tab_var, banner_cols_pos, start_col, start_row,
    toc_sheet, toc_row, toc_col, hypothesis_test, proportions) {
    
    show_totals <- !cross_tab_var$settings$no_totals && !is.null(theme$format_totals_row)
    start_row <- writeVarHeader(wb = wb, ws = ws, x = cross_tab_var, theme = theme, styles = styles, 
        start_col = start_col, start_row = start_row, toc_sheet = toc_sheet, toc_row = toc_row, toc_col = toc_col)
    weighted_n_top <- !is.null(theme$format_weighted_n) && theme$format_weighted_n$position_top
    unweighted_n_top <- !is.null(theme$format_unweighted_n) && theme$format_unweighted_n$position_top
    weighted_n_bottom <- !is.null(theme$format_weighted_n) && theme$format_weighted_n$position_bottom
    unweighted_n_bottom <- !is.null(theme$format_unweighted_n) && theme$format_unweighted_n$position_bottom
    empty_col <- !is.null(theme$banner_vars_split) && theme$banner_vars_split$empty_col
    multicols <- sapply(banner, getNames)
    multicols_csum <- cumsum(c(banner_cols_pos[1], sapply(multicols, function(x) {length(x) + empty_col})))
    format_cols <- if (empty_col) { c(sapply(setdiff(2:length(multicols_csum), function(i) multicols_csum[i-1]:(multicols_csum[i]-2)))) 
        } else multicols_csum[[1]]:(multicols_csum[[length(multicols_csum)]] - 1 - empty_col)
    sr <- start_row 
    
    unweighted_n_data <- as.data.frame(lapply(cross_tab_var$crosstabs[[banner_name]], function(x) {
        d <- as.data.frame(x$unweighted_n)
        if (empty_col) cbind(d,  "") else d
    }))
    
    weighted_n_data <- as.data.frame(lapply(cross_tab_var$crosstabs[[banner_name]], function(x) {
        d <- as.data.frame(x$totals_counts)
        if (empty_col) cbind(d,  "") else d
    }))
    
    row_names <- c(
        if (weighted_n_top) paste0(theme$format_weighted_n$name, if (nrow(weighted_n_data) != 1) paste(':', c('Min', 'Max'))),
        if (unweighted_n_top) paste0(theme$format_unweighted_n$name, if (nrow(unweighted_n_data) != 1) paste(':', c('Min', 'Max'))),
        rownames(cross_tab_var$crosstabs[[banner_name]][[1]]$proportions),
        if (show_totals) "Totals",
        if (weighted_n_bottom) paste0(theme$format_weighted_n$name, if (nrow(weighted_n_data) != 1) paste(':', c('Min', 'Max'))),
        if (unweighted_n_bottom) paste0(theme$format_unweighted_n$name, if (nrow(unweighted_n_data) != 1) paste(':', c('Min', 'Max')))
    )
    
    openxlsx::writeData(wb, ws, row_names, startCol = start_col, startRow = sr, colNames = FALSE)
    openxlsx::addStyle(wb, ws, styles$format_label_column, rows = sr:(sr + length(row_names)),
        cols = start_col, stack = FALSE)
    
    start_col <- start_col + 1
    
    last_col_num <- start_col + ncol(unweighted_n_data) - 1 - empty_col
    
    if (weighted_n_top) {
        start_row <- write_bases_data(wb, ws, weighted_n_data, row=start_row, format_cols = c(1, format_cols), style_data=styles$format_weighted_n)
    }
    
    if (unweighted_n_top) {
        start_row <- write_bases_data(wb, ws, unweighted_n_data, row=start_row, format_cols = c(1, format_cols), style_data=styles$format_weighted_n)
    }
    
    data <- as.data.frame(lapply(cross_tab_var$crosstabs[[banner_name]], function(x) {
        d <- getResults(x, proportions = proportions)
        d <- as.data.frame(d)
        if (show_totals) {
            d <- rbind(d, setNames(as.data.frame(if (proportions) x$totals_proportions else x$totals_counts), colnames(d)))
        }
        if (proportions && !theme$percent_format_data) {
            d[] <- d * 100
        }
        if (!proportions){
            d[] <- round(d)
        }
        if (empty_col) cbind(d, "" ) else d
    }))
    
    if (is.null(theme$format_headers)){
        data <- data[-c(which(cross_tab_var$inserts %in% 'Heading')),]
    }
    if (is.null(theme$format_subtotals)){
        data <- data[-c(which(cross_tab_var$inserts %in% 'Subtotal')),]
    }

    min_cell_mask <- NULL
    if (!is.null(theme$format_min_base$min_base) && theme$format_min_base$min_base > 0) {
        unweighted_n_data_num <- sapply(unweighted_n_data, function(x) as.numeric(as.character(x)))
        if (class(unweighted_n_data_num) == 'matrix') unweighted_n_data_num <- c(unweighted_n_data_num[nrow(unweighted_n_data_num),])
        min_cell_mask <- !is.na(unweighted_n_data_num) & unweighted_n_data_num < theme$format_min_base$min_base
    }
    if (any(min_cell_mask) && !is.null(theme$format_min_base$mask)) {
        data[, min_cell_mask] <- theme$format_min_base$mask
    }
    
    openxlsx::writeData(wb, ws, data, startCol = start_col, startRow = sr, colNames = FALSE)
    if (any(min_cell_mask)) {
        openxlsx::addStyle(wb, ws, styles$format_min_base, rows = sr:(sr + nrow(data) - 1),
            cols = start_col + which(min_cell_mask) - 1, gridExpand = TRUE, stack = TRUE)
    }

    openxlsx::addStyle(wb, ws, if (proportions) styles$body_proportions else styles$body_counts, rows = sr:(sr + nrow(data) - 1),
        cols = format_cols, gridExpand = TRUE, stack = TRUE)
    
    # if (hypothesis_test) {
    #     hypho_test(wb, ws, cross_tab_var, banner_name, margin = 2, empty_col, styles, sr, ccol = start_col)
    # }
    
    if (is.null(theme$format_headers)){
        for (si in (start_row + which(cross_tab_var$inserts %in% 'Heading'))){
            openxlsx::addStyle(wb, ws, styles$heading, rows = si,
                cols = (start_col-1):last_col_num, stack = TRUE)
            openxlsx::addStyle(wb, ws, styles$row_labels, rows = si,
                cols = start_col-1, stack = TRUE)
        }
    }
    
    start_row <- start_row + nrow(data)
    
    if (is.null(theme$format_subtotals)){
        for (si in (start_row + which(cross_tab_var$inserts %in% 'Subtotal') + unweighted_n_top + weighted_n_top - 1)){
            openxlsx::addStyle(wb, ws, styles$subtotal, rows = si,
                cols = (start_col-1):last_col_num, stack = TRUE)
            openxlsx::addStyle(wb, ws, styles$row_labels, rows = si,
                cols = start_col-1, stack = TRUE)
        }
    }
    
    if (weighted_n_bottom) {
        start_row <- write_bases_data(wb, ws, weighted_n_data, row=start_row, format_cols = c(1, format_cols), style_data=styles$format_weighted_n)
    }
    
    if (unweighted_n_bottom) {
        start_row <- write_bases_data(wb, ws, unweighted_n_data, row=start_row, format_cols = c(1, format_cols), style_data=styles$format_weighted_n)
    }
    
    if (!is.null(styles$format_totals_column)) {
        openxlsx::addStyle(wb, ws, styles$format_totals_column, rows = sr:start_row, cols = start_col, stack = TRUE)
    }
    
    if (!is.null(styles$split_border)) {
        openxlsx::addStyle(wb, ws, styles$split_border, rows = sr:start_row, cols = multicols_csum[2:length(multicols_csum)], gridExpand = TRUE, stack = TRUE)
    }
    
    return(start_row + 1)
    
}

writeReportGeneral <- function(x, banner, filename, wb, theme, 
    title, subtitle, header, footer, 
    append_text, table_of_contents, 
    n_or_percent, hypothesis_test, 
    logging, return_data, save_workbook) {
    
    if (logging) {
        start.time.wb <- Sys.time()
        print(paste(start.time.wb, "-- workbook generation -- start"))
    }
    
    if (is.null(wb)) wb <- openxlsx::createWorkbook()
    openxlsx::modifyBaseFont(wb, fontSize = theme$font_size, fontColour = theme$font_color, fontName = theme$font)
    
    styles <- create_styles(theme)
    
    toc_sheet <- NULL
    toc_row <- 2
    toc_col <- 2
    if (table_of_contents) {
        toc_sheet <- theme$format_toc_banner$sheet_name
        openxlsx::addWorksheet(wb, toc_sheet, gridLines = theme$show_grid_lines, header = header, footer = footer, orientation = theme$orientation)
        toc_row <- write_report_desc(wb = wb, ws = toc_sheet, theme = theme, styles = styles, title = title, subtitle = subtitle, 
            start_row = 2, start_col = 2, toc_page = TRUE)
        toc_start_row <- toc_row
        openxlsx::addStyle(wb, toc_sheet, styles$toc_banner, rows = toc_start_row, cols = toc_col, stack = FALSE)
        openxlsx::freezePane(wb, toc_sheet, firstActiveRow = toc_row + 1)
    }
    
    n_and_percent <- n_or_percent %in% 'both'
    
    banner_names <- if (is.null(banner)) "Results" else names(banner)
    if (n_and_percent) {
        banner_names <- unlist(lapply(banner_names, rep, 2))
    }
    proportions <- !(n_or_percent %in% 'n')
    
    if (one_per_sheet){
        worksheet_names <- names(x$results)
        # worksheet names must be unique and have less then 32 characters
        t5 <- which(nchar(worksheet_names) > 25)
        worksheet_names[t5] <- paste0(strtrim(worksheet_names[t5], 25), t5)
        if (length(banner) > 1) worksheet_names <- sapply(seq_along(banner), function(bix) paste0(worksheet_names, '_', bix))
    } else {
        worksheet_names <- unique(banner_names)
    }
    if (n_and_percent) worksheet_names <- sapply(worksheet_names, function(wn) paste0(wn, '_', c('C', 'P')))
    for (worksheet_name in worksheet_names){
        openxlsx::addWorksheet(wb, worksheet_name, gridLines = theme$show_grid_lines, header=header, footer=footer, orientation = theme$orientation)
        openxlsx::setColWidths(wb, worksheet_name, cols = 1, theme$format_label_column$col_width)
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
            openxlsx::writeData(wb, toc_sheet, paste0(banner_name, bna), startRow = toc_start_row, startCol = toc_col)
        }
        if (!theme$one_per_sheet) {
            worksheet_name <- paste0(banner_name, bna)
            last_row_used <- last_row_used +
                create_banner_panel(wb = wb, ws = worksheet_name, theme = theme, styles = styles, 
                    banner = banner[[banner_name]], title = title, subtitle = subtitle, 
                    start_row = last_row_used, banner_cols_pos = banner_cols_pos, 
                    percent_row = !percent_format_data & proportions)
        }
        
        for (vidx in seq_along(x$results)) {
            toc_row <- toc_row + 1
            if (one_per_sheet) {
                worksheet_name <- getAlias(x$results[[vidx]])
                # worksheet names must be unique and have less then 32 characters
                if (nchar(worksheet_name) > 25) {
                    worksheet_name <- paste0(strtrim(worksheet_name, 25), vidx)
                }
                if (length(banner) > 1) worksheet_name <- paste0(worksheet_name, '_', which(unique(banner_names) %in% banner_name))
                worksheet_name <- paste0(worksheet_name, bna)
                last_row_used <- create_banner_panel(wb = wb, ws = worksheet_name, theme = theme, styles = styles, 
                    banner = banner[[banner_name]], title = title, subtitle = subtitle, 
                    start_row = 1, banner_cols_pos = banner_cols_pos, 
                    percent_row = !percent_format_data & proportions) + 1
                openxlsx::freezePane(wb, worksheet_name, firstActiveRow = last_row_used, firstActiveCol = first_active_col)
            }
            last_row_used <- last_row_used + 1
            
            last_row_used <- #if (is.null(banner)) {
            #     writeExcelVarToplineGeneral(wb, worksheet_name, x$results[[vidx]], start_col = start_col,
            #         start_row = last_row_used, digits = digits, proportions = proportions,
            #         row_label_width = row_label_width, toc_sheet = toc_sheet, toc_row = toc_row,
            #         toc_col = toc_col, styles = styles, show_information = show_information, include_aliases = include_aliases)
            # } else {
                writeExcelVarBanner(wb = wb, ws = worksheet_name, banner_name = banner_name, 
                    cross_tab_var = x$results[[vidx]], banner_cols_pos = banner_cols_pos, 
                    start_col = start_col, start_row = last_row_used, toc_sheet = toc_sheet, 
                    toc_row = toc_row, toc_col = toc_col, styles = styles, hypothesis_test = hypothesis_test, 
                    proportions = proportions, theme = theme)
            # }
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



