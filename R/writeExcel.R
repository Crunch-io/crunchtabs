
#' Generate Excel Reports: Toplines and Banners
#'
#' \code{writeExcel} produces publication-quality Excel reports:
#' Toplines (one-way frequency tables) or Banners (cross tabulations)
#'
#' @param data_summary An object of class \code{Toplines} or \code{Crosstabs}.
#' @param filename character. The name of the output file.
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
writeExcel <- function(data_summary, filename = getName(data_summary), wb = NULL, theme = themeDefaultExcel(), 
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
    
    find_null_or_base <- function(theme, info_name, elem){
        x <- if (info_name %in% names(theme)) theme[[info_name]][[elem]]
        if (is.null(x) || (!is.null(theme[[elem]]) && x %in% theme[[elem]])) return(NULL)
        return(x)
    }

    numFmt <- paste0("0", if (theme$digits > 0) paste0(".", paste0(rep(0, theme$digits), collapse = "")))
    numFmtProp <- paste0(numFmt, if (theme$excel_percent_sign) "%")
    
    borders <- c("border_top", "border_bottom", "border_left", "border_right")
    
    if (!theme$format_label_column$extend_borders) {
        theme$format_label_column$border_style <- "none"
        theme$format_label_column[borders] <- TRUE
    }
        
    style_list <- sapply(grep("^format_", names(theme), value = TRUE), function(v) {
        if (!is.null(theme[[v]])) {
            tv <- theme[[v]]
            border <- any(unlist(tv[borders])) && !is.null(tv$border_style)
            border_where <- if (border) gsub("border_", "", borders)[unlist(tv[borders])]
            openxlsx::createStyle(fontName = find_null_or_base(theme, v, "font"), 
                fontSize = find_null_or_base(theme, v, "font_size"),
                fontColour = find_null_or_base(theme, v, "font_color"),
                border = border_where,
                borderColour = tv$border_color,
                borderStyle = tv$border_style,
                fgFill = tv$background_color,
                halign = tv$halign,
                valign = tv$valign,
                textDecoration = tv$decoration,
                wrapText = if (is.null(tv$wrap_text)) TRUE else tv$wrap_text,
                numFmt = if (v %in% c("format_weighted_n", "format_unweighted_n")) { "0" } else { "GENERAL" })
        }
    })
    
    style_list$body_counts <- openxlsx::createStyle(numFmt = numFmt, halign = theme$halign, valign = theme$valign)
    style_list$body_proportions <- openxlsx::createStyle(numFmt = numFmtProp, halign = theme$halign, valign = theme$valign)
    style_list$split_border <- openxlsx::createStyle(border = if (!is.null(theme$format_banner_split$empty_col) && theme$format_banner_split$empty_col) "LeftRight" else "left",
        borderStyle = if (!is.null(theme$format_banner_split$border_style)) theme$format_banner_split$border_style else "None",
        borderColour = theme$format_banner_split$border_color)

    return(style_list)
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
        if (!is.null(theme$format_banner_names)){
            data <- unlist(sapply(seq_along(banner), function(bv) {
                c(getName(banner[[bv]]), rep("", times = length(banner_info$multicols[[bv]]) - 1 + banner_info$empty_col))
            }))
            start_row <- write_and_style(wb, ws, data = data, style = styles$format_banner_names, start_row = start_row, cols = banner_info$format_cols, write_as_rows = FALSE)
            lapply(setdiff(seq_along(banner_info$multicols), 1), function(bv) {
                openxlsx::mergeCells(wb, ws, cols = banner_info$multicols_csum[bv]:(banner_info$multicols_csum[bv + 1] -
                        1 - banner_info$empty_col), rows = start_row - 1)
            })
        }
        start_row <- write_and_style(wb, ws, data = getItemData(banner, "categories", banner_info$empty_col, round = FALSE), 
            style = styles$format_banner_categories, start_row = start_row, cols = banner_info$format_cols, write_as_rows = FALSE)
        if (!is.null(theme$format_weighted_n) && theme$format_weighted_n$position_fixed) {
            start_row <- write_and_style(wb, ws, data = c(theme$format_weighted_n$name, getItemData(banner, "weighted_n", banner_info$empty_col, round = TRUE)), 
                style = styles$format_weighted_n, start_row = start_row, cols = c(1, banner_info$format_cols), write_as_rows = FALSE)
        }
        if (!is.null(theme$format_unweighted_n) && theme$format_unweighted_n$position_fixed) {
            start_row <- write_and_style(wb, ws, data = c(theme$format_unweighted_n$name, getItemData(banner, "unweighted_n", banner_info$empty_col, round = TRUE)), 
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
    
    openxlsx::pageSetup(wb, ws, printTitleRows = (1 + if (!is.null(theme$format_title)) { length(title) } else { 0 }):start_row)
    openxlsx::freezePane(wb, ws, firstActiveRow = start_row, firstActiveCol = theme$excel_freeze_column + 1)
    
    return(start_row)
}

write_var_header <- function(wb, ws, var, theme, styles, start_row, toc_sheet, toc_row, toc_col) {
    var_info <- getVarInfo(var, theme)
    if (!is.null(toc_sheet)) {
        openxlsx::writeFormula(wb, toc_sheet, startCol = toc_col, startRow = toc_row, 
            x = openxlsx::makeHyperlinkString(sheet = ws,
                row = start_row - 1, col = 1, text = var_info$format_var_alias))
        openxlsx::writeData(wb, toc_sheet, paste0(c(var_info$format_var_subname, var_info$format_var_description), collapse = " - "), startCol = toc_col+1, startRow = toc_row)
    }
    for (info_name in names(var_info)){
        start_row <- write_and_style(wb, ws, data = var_info[[info_name]], style = styles[[info_name]], start_row = start_row, cols = 1, write_as_rows = FALSE)
    }
    return(start_row)
}
#' @importFrom stats setNames
writeExcelVar <- function(wb, ws, theme, styles, banner_name, var, banner_info, start_row, start_col,
    toc_sheet, toc_row, toc_col, hypothesis_test, proportions) {
    start_row <- sr <- write_var_header(wb = wb, ws = ws, var = var, theme = theme, styles = styles,
        start_row = start_row, toc_sheet = toc_sheet, toc_row = toc_row, toc_col = toc_col)
    body_style <- if (proportions) styles$body_proportions else styles$body_counts

    style_if <- function(ifStatement, start_row, style, data, cols){
        if (ifStatement){
            nr <- if (!is.null(dim(data))) { nrow(data) } else { 1 }
            if (!is.list(style)) style <- list(style)
            for (s in style) {
                if (!is.null(s)) {
                    openxlsx::addStyle(wb = wb, sheet = ws, style = s, rows = start_row:(start_row + nr - 1),
                        cols = cols, gridExpand = TRUE, stack = TRUE)
                }
            }
            start_row <- start_row + nr
        }
        return(start_row)
    }

    var_info <- reformatVar(var = var, banner_name = banner_name, theme = theme, 
        proportions = proportions, banner_info = banner_info, latex = FALSE) 
    data_list <- var_info$data_list
    
    all_data <- do.call(rbind, data_list[var_info$data_order])
    topline_array <- is(var, "ToplineCategoricalArray")
    if (topline_array) {
        banner_info$format_cols <- seq_along(c(data_list$unweighted_n, "a"))
        names(all_data) <- var$subnames
    }

    openxlsx::writeData(wb = wb, sheet = ws, x = all_data, startCol = 2, startRow = start_row, colNames = topline_array,
        headerStyle = styles$format_banner_categories,
        borders = if (!is.null(theme$excel_table_border$border_style)) { "surrounding" } else { "none" }, 
        borderColour = theme$excel_table_border$border_color,
        borderStyle = if (!is.null(theme$excel_table_border$border_style)) { theme$excel_table_border$border_style } else { "thin" })
    start_row <- sr <- start_row + topline_array

    style_if(!is.null(styles$format_totals_column), start_row, styles$format_totals_column, all_data, cols = 2)

    for (dt in c(var_info$top, "body")) {
        start_row <- style_if(TRUE, start_row = start_row, style = list(styles[[paste0("format_", dt)]],
            if (dt %in% c("means", "medians")) styles$body_counts,
            if (gsub("_row", "", dt) %in% c("body", "totals")) body_style),
            data = data_list[[dt]], cols=c(1, banner_info$format_cols))
    }

    start_row <- start_row - nrow(data_list$body)
    
    min_cell_mask <- rbind(var_info$min_cell_top, var_info$min_cell_body, var_info$min_cell_bottom)
    min_row <- (start_row - if (is.null(var_info$min_cell_top)) { 0 } else { nrow(var_info$min_cell_top) })
    if (!is.null(theme$format_min_base$mask)) {
        for (bki in which(colSums(min_cell_mask, na.rm = TRUE) != 0)){
            vals <- rle(min_cell_mask[, bki])
            vals$start <- cumsum(vals$lengths) - vals$lengths + min_row
            for (bkj in seq_along(vals$lengths)) {
                if (!is.na(vals$values[bkj]) && vals$values[bkj]) {
                    write_and_style(wb = wb, ws = ws, data = rep(theme$format_min_base$mask, vals$lengths[bkj]),
                        style = styles$format_min_base, cols = bki + 1, start_row = vals$start[bkj], write_as_rows = TRUE)
                }
            }
        }
    } else if (!is.null(styles$format_min_base)) {
        cols <- floor((which(min_cell_mask)-1)/nrow(min_cell_mask)) + 1
        rows <- which(min_cell_mask) %% nrow(min_cell_mask)
        rows[rows %in% 0] <- nrow(min_cell_mask)
        rows <- rows + min_row
        openxlsx::addStyle(wb = wb, sheet = ws, style = styles$format_min_base, rows = rows, cols = cols, stack = TRUE)
    }

    if (any(var_info$inserts %in% "Heading")) {
        openxlsx::addStyle(wb, ws, styles$format_headers, rows = (start_row + which(var_info$inserts %in% "Heading") - 1),
            cols = c(1, banner_info$format_cols), gridExpand = TRUE, stack = TRUE)
    }
    if (any(var_info$inserts %in% "Subtotal")) {
        openxlsx::addStyle(wb, ws, styles$format_subtotals, rows = (start_row + which(var_info$inserts %in% "Subtotal") - 1),
            cols = c(1, banner_info$format_cols), gridExpand = TRUE, stack = TRUE)
    }

    start_row <- start_row + nrow(data_list$body)

    for (dt in var_info$bottom) {
        start_row <- style_if(TRUE, start_row = start_row, style = list(styles[[paste0("format_", dt)]],
                if (dt %in% c("means", "medians")) styles$body_counts,
                if (gsub("_row", "", dt) %in% c("body", "totals")) body_style),
            data = data_list[[dt]], cols=c(1, banner_info$format_cols))
    }

    write_and_style(wb, ws, data = unlist(lapply(data_list[var_info$data_order], rownames)),
        style = styles$format_label_column, start_row = sr, cols = 1, write_as_rows = TRUE)

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
        theme$excel_freeze_column <- 1
    }
    
    data_summary$results <- lapply(data_summary$results, removeInserts, theme)
    
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
        openxlsx::addWorksheet(wb, toc_sheet, gridLines = theme$excel_show_grid_lines, header = theme$excel_header, footer = theme$excel_footer, orientation = theme$excel_orientation)
        toc_row <- toc_start_row <- write_report_desc(wb = wb, ws = toc_sheet, theme = theme, styles = styles, 
            title = title, subtitle = subtitle)
        openxlsx::setColWidths(wb, toc_sheet, cols = 1, theme$format_label_column$col_width)
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
        openxlsx::addWorksheet(wb, worksheet_name, gridLines = theme$excel_show_grid_lines, header = theme$excel_header, footer = theme$excel_footer, orientation = theme$excel_orientation)
        openxlsx::setColWidths(wb, worksheet_name, cols = 1, theme$format_label_column$col_width)
    }
    
    if (length(n_or_percent) == 2) { banner_names <- unlist(lapply(banner_names, rep, 2)) }
    
    for (bix in seq_along(banner_names)) {
        banner_name <- banner_names[bix]
        bna <- ws_subtitles[duplicated(banner_names)[bix] + 1]
        proportions <- n_or_percent[duplicated(banner_names)[bix] + 1] %in% "percents"
        if (logging) {
            start.time <- Sys.time()
            print(paste0(start.time, " -- banner generation: ", banner_name, bna, " -- start"))
        }
        banner_info <- getBannerInfo(banner = banner[[banner_name]], theme = theme)
        if (table_of_contents) {
            openxlsx::writeData(wb, toc_sheet, paste0(banner_name, bna), startRow = toc_start_row, startCol = toc_col)
        }
        if (!theme$one_per_sheet) {
            worksheet_name <- paste0(worksheet_names[[banner_name]], bna)
            last_row_used <- write_banner_panel(wb = wb, ws = worksheet_name, theme = theme, styles = styles, 
                    banner = banner[[banner_name]], title = title, subtitle = subtitle, 
                    banner_info = banner_info, percent_row = !theme$excel_percent_sign & proportions)
        }
        
        for (vidx in seq_along(data_summary$results)) {
            toc_row <- toc_row + 1 
            if (theme$one_per_sheet) {
                worksheet_name <- paste0(worksheet_names[[banner_name]][[vidx]], bna)
                last_row_used <- write_banner_panel(wb = wb, ws = worksheet_name, theme = theme, styles = styles, 
                    banner = banner[[banner_name]], title = title, subtitle = subtitle, 
                    banner_info = banner_info, percent_row = !theme$excel_percent_sign & proportions)
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
    
    if (!endsWith(filename, ".xlsx")) { filename <- paste0(filename, ".xlsx") }
    openxlsx::saveWorkbook(wb, filename, overwrite = TRUE)
    
    if (logging) {
        end.time <- Sys.time()
        print(paste(end.time, "-- workbook save -- end -- elapsed: ", round(difftime(end.time, start.time, units = "mins"), 2), "mins"))
        print(paste(end.time, "-- workbook generation -- end -- elapsed: ", round(difftime(end.time, start.time.wb, units = "mins"), 2), "mins"))
    }
    
    return(invisible(data_summary))
}



