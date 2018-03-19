#' @export
writeExcel <- function(data_summary, filename = getName(data_summary), wb = NULL, theme = theme_default(), 
    title = getName(data_summary), subtitle = NULL, header = NULL, footer = NULL, 
    table_of_contents = FALSE, n_or_percent = "percent", hypothesis_test = FALSE, 
    logging = FALSE, save_workbook = TRUE) {
    
    if (is.null(filename) && !save_workbook) {
        stop("No filename provided. If save_workbook is true, a filename must be provided.")
    }
    
    if (!is.null(wb) && class(wb) != "Workbook") {
        wrong_class_error(wb, "Workbook", "wb")
    }
    
    wrong_class_error(data_summary, "CrunchTabs", "data_summary")
    if (!any(c("Toplines", "Crosstabs") %in% class(data_summary))) {
        stop("The expected class for `data_summary` is either Toplines, CrunchTabs or Crosstabs CrunchTabs, not ", collapse_items(class(data_summary)))
    }
    
    return(writeReportGeneral(data_summary = data_summary, banner = data_summary$banner, filename = filename, wb = wb, theme = theme, 
        title = title, subtitle = subtitle, header = header, footer = footer, 
        table_of_contents = table_of_contents, n_or_percent = n_or_percent, hypothesis_test = hypothesis_test, 
        logging = logging, save_workbook = save_workbook))
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
    
    style_list <- sapply(grep("^format_", names(theme), value = TRUE), function(v) {
        if (!is.null(theme[[v]])) {
            openxlsx::createStyle(fontName = get_format_info(theme, v, "font"), 
                fontSize = get_format_info(theme, v, "font_size"),
                fontColour = if (get_format_info(theme, v, "font_color") != "black") get_format_info(theme, v, "font_color"),
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
    
    for (x in c("Top", "Bottom", "Left", "Right")){
        style_list$format_label_column[[paste0("border", x)]] <- "none"
    }
    
    if (!is.null(get_format_info(theme, "table_border", "border_style"))){
        style_list$body_border_top <- openxlsx::createStyle(border = "top", borderStyle = get_format_info(theme, "table_border", "border_style"), borderColour = get_format_info(theme, "table_border", "border_color"))
        style_list$body_border_bottom <- openxlsx::createStyle(border = "bottom", borderStyle = get_format_info(theme, "table_border", "border_style"), borderColour = get_format_info(theme, "table_border", "border_color"))
        style_list$body_border_left <- openxlsx::createStyle(border = "left", borderStyle = get_format_info(theme, "table_border", "border_style"), borderColour = get_format_info(theme, "table_border", "border_color"))
        style_list$body_border_right <- openxlsx::createStyle(border = "right", borderStyle = get_format_info(theme, "table_border", "border_style"), borderColour = get_format_info(theme, "table_border", "border_color"))
    }
    
    style_list$body_counts <- openxlsx::createStyle(numFmt = numFmt, halign = theme$halign, valign = theme$valign)
    style_list$body_proportions <- openxlsx::createStyle(numFmt = numFmtProp, halign = theme$halign, valign = theme$valign)
    style_list$split_border <- openxlsx::createStyle(border = if (get_format_info(theme, "banner_vars_split", "empty_col")) "LeftRight" else "left",
        borderStyle = if (!is.null(get_format_info(theme, "banner_vars_split", "border_style"))) get_format_info(theme, "banner_vars_split", "border_style") else "None",
        borderColour = get_format_info(theme, "banner_vars_split", "border_color"))

    return(style_list)
}

write_report_desc <- function(wb, ws, theme, styles, title, subtitle, toc_page) {
    
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
    if (!is.null(theme$format_title)){
        start_row <- write_data(wb, ws, title, styles$format_title, start_row = start_row, cols = 1, write_as_rows = TRUE)
    }
    if (!is.null(theme$format_subtitle)) {
        start_row <- write_data(wb, ws, subtitle, styles$format_subtitle, start_row = start_row, cols = 1, write_as_rows = TRUE)
    }
    return(start_row + 1)
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

create_banner_panel <- function(wb, ws, theme, styles, banner, title, subtitle, 
    start_row, banner_info, percent_row) {
    
    start_row <- sr <- write_report_desc(wb = wb, ws = ws, theme = theme, styles = styles, 
        title = title, subtitle = subtitle, toc_page = FALSE)
    
    if (!is.null(banner)){
        if (!is.null(theme$format_banner_labels)){
            data <- unlist(sapply(seq_along(banner), function(bv) {
                c(getName(banner[[bv]]), if (length(banner_info$multicols[[bv]]) > 1) rep("", times = length(banner_info$multicols[[bv]]) - 1 + banner_info$empty_col))
            }))
            start_row <- write_data(wb, ws, data = data, style = styles$format_banner_labels, start_row = start_row, cols = banner_info$format_cols, write_as_rows = FALSE)
            lapply(seq_along(banner_info$multicols), function(bv) {
                openxlsx::mergeCells(wb, ws, cols = banner_info$multicols_csum[bv]:(banner_info$multicols_csum[bv + 1] -
                        1 - banner_info$empty_col), rows = start_row - 1)
            })
        }
        if (!is.null(theme$format_banner_categories)){
            start_row <- write_data(wb, ws, data = get_data(banner, "categories", banner_info$empty_col, round = FALSE), 
                style = styles$format_banner_categories, start_row = start_row, cols = banner_info$format_cols, write_as_rows = FALSE)
        }
        if (!is.null(theme$format_weighted_n) && theme$format_weighted_n$position_fixed) {
            start_row <- write_data(wb, ws, data = c(theme$format_weighted_n$name, get_data(banner, "weighted_n", banner_info$empty_col, round = TRUE)), 
                style = styles$format_weighted_n, start_row = start_row, cols = c(1, banner_info$format_cols), write_as_rows = FALSE)
        }
        if (!is.null(theme$format_unweighted_n) && theme$format_unweighted_n$position_fixed) {
            start_row <- write_data(wb, ws, data = c(theme$format_unweighted_n$name, get_data(banner, "unweighted_n", banner_info$empty_col, round = TRUE)), 
                style = styles$format_unweighted_n, start_row = start_row, cols = c(1, banner_info$format_cols), write_as_rows = FALSE)
        }
        if (percent_row) {
            data <- as.data.frame(lapply(seq_along(banner), function(bv) {
                t(c(rep("%", times = length(banner_info$multicols[[bv]])), if (banner_info$empty_col) ""))
            }))
            start_row <- write_data(wb, ws, data = data, style = styles$format_banner_categories, start_row = start_row, cols = banner_info$format_cols, write_as_rows = FALSE)
            openxlsx::addStyle(wb, ws, openxlsx::createStyle(textDecoration = NULL), rows = start_row - 1, cols = banner_info$format_cols, stack = TRUE)
        }
        if (!is.null(styles$format_totals_column)) {
            openxlsx::addStyle(wb, ws, styles$format_totals_column, rows = sr:(start_row - 1), cols = 2, stack = TRUE)
        }
        if (!is.null(styles$split_border)) {
            openxlsx::addStyle(wb, ws, styles$split_border, rows = sr:(start_row - 1), 
                cols = if (banner_info$empty_col) border_info$border_columns, gridExpand = TRUE, stack = TRUE)
        }
    }
    
    openxlsx::addStyle(wb, ws, styles$format_label_column, rows = sr:(start_row - 1), cols = 1, gridExpand = FALSE, stack = TRUE)
    
    openxlsx::pageSetup(wb, ws, printTitleRows = (1 + ifelse(!is.null(theme$format_title), length(title), 0)):start_row)
    openxlsx::freezePane(wb, ws, firstActiveRow = start_row, firstActiveCol = theme$freeze_column + 1)
    
    return(start_row)
}

get_data <- function(data, item_name, empty_col, round){
    return(unlist(lapply(seq_along(data), function(bv) {
        cbind(if (round) round(data[[bv]][[item_name]]) else data[[bv]][[item_name]], if (empty_col) as.numeric(NA))
    })))
}

write_data <- function(wb, ws, data, style, start_row, cols, write_as_rows){
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

writeVarHeader <- function(wb, ws, var, theme, styles, start_row, toc_sheet, toc_row, toc_col) {
    var_info <- list("format_var_alias" = getAlias(var),
        "format_var_name" = getName(var),
        "format_var_description" = getDescription(var),
        "format_var_filtertext" = getNotes(var),
        "format_var_subname" = if (!is.na(var$subname)) var$subname)
    add_toc_info <- !is.null(toc_sheet)
    if (add_toc_info) {
        openxlsx::writeFormula(wb, toc_sheet, startCol = toc_col, startRow = toc_row, 
            x = openxlsx::makeHyperlinkString(sheet = ws,
                row = start_row - 1, col = 1, text = var_info$format_var_alias))
        openxlsx::writeData(wb, toc_sheet, paste0(c(var_info$format_var_subname, var_info$format_var_description), collapse = " - "), startCol = toc_col+1, startRow = toc_row)
    }
    for (info_name in names(var_info)){
        if (!is.null(theme[[info_name]])){
            if (is.null(theme[[info_name]]$repeat_for_subs) || theme[[info_name]]$repeat_for_subs || var$subnumber %in% 1){
                if (!is.null(var_info[[info_name]]) && !is.na(var_info[[info_name]]) && var_info[[info_name]] != "") {
                    if (!is.null(theme[[info_name]]$include_alias) && theme[[info_name]]$include_alias){
                        var_info[[info_name]] <- paste0(var_info$format_var_alias, " - ", var_info[[info_name]])
                    }
                    start_row <- write_data(wb, ws, data = var_info[[info_name]], style = styles[[info_name]], start_row = start_row, cols = 1, write_as_rows = FALSE)
                }
            }
        }
    }
    return(start_row)
}

#' @importFrom stats setNames
writeExcelVar <- function(wb, ws, theme, styles, banner_name, var, banner_info, start_row, start_col,
    toc_sheet, toc_row, toc_col, hypothesis_test, proportions) {
    
    start_row <- sr <- writeVarHeader(wb = wb, ws = ws, var = var, theme = theme, styles = styles, 
        start_row = start_row, toc_sheet = toc_sheet, toc_row = toc_row, toc_col = toc_col)
    weighted_n_top <- !is.null(theme$format_weighted_n) && theme$format_weighted_n$position_top
    unweighted_n_top <- !is.null(theme$format_unweighted_n) && theme$format_unweighted_n$position_top
    totals_top <- !var$settings$no_totals && !is.null(theme$format_totals_row) && theme$format_totals_row$position_top
    weighted_n_bottom <- !is.null(theme$format_weighted_n) && theme$format_weighted_n$position_bottom
    unweighted_n_bottom <- !is.null(theme$format_unweighted_n) && theme$format_unweighted_n$position_bottom
    totals_bottom <- !var$settings$no_totals && !is.null(theme$format_totals_row) && theme$format_totals_row$position_bottom
    min_base <- !is.null(theme$format_min_base$min_base) && theme$format_min_base$min_base >= 0
    show_mean <- !is.null(theme$format_mean) && !is.null(var$crosstabs[[banner_name]]$Total$mean)
    
    style_if <- function(ifStatement, start_row, style, data, cols){
        if (ifStatement){
            openxlsx::addStyle(wb = wb, sheet = ws, style = style, rows = start_row:(start_row + nrow(data) - 1), 
                cols = cols, gridExpand = TRUE, stack = TRUE)
            start_row <- start_row + nrow(data)
        }
        return(start_row)
    }
    
    data <- as.data.frame(lapply(var$crosstabs[[banner_name]], function(x) {
        d <- as.data.frame(reformatResults(x, proportions = proportions, digits = theme$digits, reformat = FALSE))
        if (is.null(x$type) || x$type %in% c("categorical", "categorical_array", "multiple_response")){
            if (proportions && !theme$percent_format_data) {
                d[] <- d * 100
            }
            if (!proportions){
                d[] <- round(d)
            }
        }
        if (banner_info$empty_col) cbind(d, as.numeric(NA)) else d
    }), check.names = FALSE)
    
    unweighted_n <- get_data(var$crosstabs[[banner_name]], "unweighted_n", banner_info$empty_col, round = TRUE)
    if (is.null(dim(unweighted_n))) unweighted_n <- matrix(unweighted_n, nrow = 1, dimnames = list(c(), colnames(data)))
    weighted_n <- get_data(var$crosstabs[[banner_name]], "totals_counts", banner_info$empty_col, round = TRUE)
    if (is.null(dim(weighted_n))) weighted_n <- matrix(weighted_n, nrow = 1, dimnames = list(c(), colnames(data)))
    
    totals <- get_data(var$crosstabs[[banner_name]], if (proportions) "totals_proportions" else "totals_counts", banner_info$empty_col, round = !proportions)
    if ((totals_top || totals_bottom) && is.null(dim(totals))) totals <- matrix(totals, nrow = 1, dimnames = list(c(), colnames(data)))
    mean <- get_data(var$crosstabs[[banner_name]], "mean", banner_info$empty_col, round = FALSE)
    if (show_mean && is.null(dim(mean))) mean <- matrix(mean, nrow = 1, dimnames = list(c(), colnames(data)))

    if (is.null(theme$format_headers)){
        data <- data[-c(which(var$inserts %in% "Heading")),]
        var$inserts <- var$inserts[-c(which(var$inserts %in% "Heading")),]
    }
    if (is.null(theme$format_subtotals)){
        data <- data[-c(which(var$inserts %in% "Subtotal")),]
        var$inserts <- var$inserts[-c(which(var$inserts %in% "Subtotal")),]
    }
    if (is.null(theme$format_means)){
        data <- data[-c(which(var$inserts %in% "Mean")),]
        var$inserts <- var$inserts[-c(which(var$inserts %in% "Mean")),]
    }
    
    row_names <- c(
        if (weighted_n_top) paste0(theme$format_weighted_n$name, if (nrow(weighted_n) != 1) paste(":", c("Min", "Max"))),
        if (unweighted_n_top) paste0(theme$format_unweighted_n$name, if (nrow(unweighted_n) != 1) paste(":", c("Min", "Max"))),
        if (totals_top) "Totals",
        rownames(data),
        if (show_mean) "Mean",
        if (totals_bottom) "Totals",
        if (weighted_n_bottom) paste0(theme$format_weighted_n$name, if (nrow(weighted_n) != 1)  paste(":", c("Min", "Max"))),
        if (unweighted_n_bottom) paste0(theme$format_unweighted_n$name, if (nrow(unweighted_n) != 1) paste(":", c("Min", "Max")))
    )
    
    if (!is.null(theme$final_digits) && !is.infinite(theme$final_digits)){
        data <- round(data, theme$final_digits + (proportions && theme$percent_format_data)*2)
        if (show_mean) mean <- round(mean, theme$final_digits)
    }

    all_data <- rbind(if (weighted_n_top) weighted_n,
        if (unweighted_n_top) unweighted_n,
        if (totals_top) totals, 
        data, 
        if (show_mean) mean,
        if (totals_bottom) totals,
        if (weighted_n_bottom) weighted_n,
        if (unweighted_n_bottom) unweighted_n)

    topline_array <- is(var, "ToplineArrayVar")
    if (topline_array) {
        banner_info$format_cols <- seq_along(unweighted_n)
        names(all_data) <- gsub('Total\\.', '', names(all_data))
        all_data <- all_data[, 1:(ncol(all_data)-1)]
    }
    
    openxlsx::writeData(wb = wb, sheet = ws, x = all_data, startCol = 2, startRow = start_row, colNames = topline_array,
        headerStyle = styles$format_banner_categories,
        borders = ifelse(!is.null(theme$table_border$border_style), "surrounding", "none"), borderColour = theme$table_border$border_color,
        borderStyle = ifelse(!is.null(theme$table_border$border_style), theme$table_border$border_style, "thin"))
    start_row <- sr <- start_row + topline_array
    
    start_row <- style_if(weighted_n_top, start_row, styles$format_weighted_n, weighted_n, cols=c(1, banner_info$format_cols))
    start_row <- style_if(unweighted_n_top, start_row, styles$format_unweighted_n, unweighted_n, cols=c(1, banner_info$format_cols))
    start_row <- style_if(totals_top, start_row, styles$format_totals_row, totals, cols=c(1, banner_info$format_cols))
    
    openxlsx::addStyle(wb = wb, sheet = ws, style = if (proportions) styles$body_proportions else styles$body_counts, 
        rows = start_row:(start_row + nrow(data) - 1), cols = banner_info$format_cols, gridExpand = TRUE)
    
    ## TODO: make this work better w/ MR vars. 
    if (min_base) {
        unweighted_n_data_num <- sapply(unweighted_n, function(x) as.numeric(as.character(x)))
        if (class(unweighted_n_data_num) == "matrix") unweighted_n_data_num <- c(unweighted_n_data_num[nrow(unweighted_n_data_num),])
        min_cell_mask <- !is.na(unweighted_n_data_num) & unweighted_n_data_num <= theme$format_min_base$min_base
        if (any(min_cell_mask)) {
            if (!is.null(theme$format_min_base$mask)) {
                for (bki in which(min_cell_mask)) { 
                    write_data(wb = wb, ws = ws, data = rep(theme$format_min_base$mask, nrow(data) + show_mean), 
                        style = styles$format_min_base, cols = bki + 1, start_row = start_row, write_as_rows = TRUE) 
                }
            } else {
                openxlsx::addStyle(wb, ws, styles$format_min_base, rows = start_row:(start_row + nrow(data) + show_mean - 1),
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
    
    if (show_mean && !min_base) {
        wt <- if (!is.null(theme$format_min_base$mask)) theme$format_min_base$mask else "-"
        for (bki in which(is.na(mean))) openxlsx::writeData(wb = wb, sheet = ws, x = wt, startCol = bki + 1, startRow = start_row)
        openxlsx::addStyle(wb, ws, openxlsx::createStyle(numFmt = "GENERAL"), rows = start_row + 1, cols = which(is.na(mean)) + 1, gridExpand = TRUE, stack = TRUE)
    }
    start_row <- style_if(show_mean, start_row, styles$body_counts, mean, cols=c(1, banner_info$format_cols))
    start_row <- style_if(show_mean, start_row - show_mean, styles$format_means, mean, cols=c(1, banner_info$format_cols))
    start_row <- style_if(totals_bottom, start_row, styles$format_totals_row, totals, cols=c(1, banner_info$format_cols))
    start_row <- style_if(weighted_n_bottom, start_row, styles$format_weighted_n, weighted_n, cols=c(1, banner_info$format_cols))
    start_row <- style_if(unweighted_n_bottom, start_row, styles$format_unweighted_n, unweighted_n, cols=c(1, banner_info$format_cols))

    if (!is.null(banner_info$border_columns)){
        openxlsx::addStyle(wb, ws, styles$split_border, rows = sr:(start_row - 1), 
            cols = banner_info$border_columns, gridExpand = TRUE, stack = TRUE)
    }
    
    openxlsx::writeData(wb = wb, sheet = ws, x = row_names, startCol = 1, startRow = sr)
    openxlsx::addStyle(wb, ws, styles$format_label_column, rows = sr:(sr + length(row_names) - 1),
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
    title, subtitle, header, footer, 
    table_of_contents, n_or_percent, hypothesis_test, 
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
    
    toc_sheet <- NULL
    toc_row <- 2
    toc_col <- 2
    if (table_of_contents) {
        toc_sheet <- theme$format_toc_banner$sheet_name
        openxlsx::addWorksheet(wb, toc_sheet, gridLines = theme$show_grid_lines, header = header, footer = footer, orientation = theme$orientation)
        toc_row <- write_report_desc(wb = wb, ws = toc_sheet, theme = theme, styles = styles, title = title, 
            subtitle = subtitle, toc_page = TRUE)
        toc_start_row <- toc_row
        openxlsx::addStyle(wb, toc_sheet, styles$toc_banner, rows = toc_start_row, cols = toc_col, stack = FALSE)
        openxlsx::freezePane(wb, toc_sheet, firstActiveRow = toc_row + 1)
    }
    
    n_and_percent <- n_or_percent %in% "both"
    
    banner_names <- if (is.null(banner)) "Results" else names(banner)
    if (n_and_percent) {
        banner_names <- unlist(lapply(banner_names, rep, 2))
    }
    proportions <- !(n_or_percent %in% "n")
    
    if (theme$one_per_sheet){
        worksheet_names <- names(data_summary$results)
        # worksheet names must be unique and have less then 32 characters
        t5 <- which(nchar(worksheet_names) > 25)
        worksheet_names[t5] <- paste0(strtrim(worksheet_names[t5], 25), t5)
        if (length(banner) > 1) worksheet_names <- sapply(seq_along(banner), function(bix) paste0(worksheet_names, "_", bix))
    } else {
        worksheet_names <- unique(banner_names)
    }
    if (n_and_percent) worksheet_names <- sapply(worksheet_names, function(wn) paste0(wn, "_", c("C", "P")))
    for (worksheet_name in worksheet_names){
        openxlsx::addWorksheet(wb, worksheet_name, gridLines = theme$show_grid_lines, header=header, footer=footer, orientation = theme$orientation)
        openxlsx::setColWidths(wb, worksheet_name, cols = 1, theme$format_label_column$col_width)
    }
    
    for (bix in seq_along(banner_names)) {
        banner_name <- banner_names[bix]
        bna <- if (n_and_percent) ifelse(duplicated(banner_names)[bix], "_P", "_C")
        if (n_and_percent) { proportions <- duplicated(banner_names)[bix] }
        if (logging) {
            start.time <- Sys.time()
            print(paste0(start.time, " -- banner generation: ", banner_name, bna, " -- start"))
        }
        start_col <- 1
        last_row_used <- 1
        banner_info <- get_banner_info(banner = banner[[banner_name]], theme = theme)
        if (table_of_contents) {
            openxlsx::writeData(wb, toc_sheet, paste0(banner_name, bna), startRow = toc_start_row, startCol = toc_col)
        }
        if (!theme$one_per_sheet) {
            worksheet_name <- paste0(banner_name, bna)
            last_row_used <- last_row_used +
                create_banner_panel(wb = wb, ws = worksheet_name, theme = theme, styles = styles, 
                    banner = banner[[banner_name]], title = title, subtitle = subtitle, 
                    banner_info = banner_info, percent_row = !theme$percent_format_data & proportions)
        }
        
        for (vidx in seq_along(data_summary$results)) {
            toc_row <- toc_row + 1
            if (theme$one_per_sheet) {
                worksheet_name <- getAlias(data_summary$results[[vidx]])
                # worksheet names must be unique and have less then 32 characters
                if (nchar(worksheet_name) > 25) {
                    worksheet_name <- paste0(strtrim(worksheet_name, 25), vidx)
                }
                if (length(banner) > 1) worksheet_name <- paste0(worksheet_name, "_", which(unique(banner_names) %in% banner_name))
                worksheet_name <- paste0(worksheet_name, bna)
                last_row_used <- create_banner_panel(wb = wb, ws = worksheet_name, theme = theme, styles = styles, 
                    banner = banner[[banner_name]], title = title, subtitle = subtitle, 
                    banner_info = banner_info, percent_row = !theme$percent_format_data & proportions) + 1
            }
            last_row_used <- writeExcelVar(wb = wb, ws = worksheet_name, var = data_summary$results[[vidx]], theme = theme, 
                styles = styles, banner_name = banner_name, banner_info = banner_info,
                start_row = last_row_used + 1, start_col = 2, toc_sheet = toc_sheet, toc_row = toc_row, toc_col = toc_col, 
                hypothesis_test = hypothesis_test, proportions = proportions)
            
        }
        toc_col <- toc_col + 1
        if (logging) {
            end.time <- Sys.time()
            print(paste0(end.time, " -- banner generation: ", banner_name, bna, " -- end -- elapsed: ", round(difftime(end.time, start.time, units = "mins"), 2), " mins"))
        }
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



