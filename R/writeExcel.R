
#' Generate Excel Reports: Toplines and Banners
#'
#' \code{writeExcel} produces publication-quality Excel reports:
#' toplines (one-way frequency tables) or banners (cross tabulations)
#'
#' @param x An object of class \code{Toplines} or \code{Crosstabs}.
#' @param filename The name of the output file (without an extension).
#' @param title An optional title. Defaults to the title provided in the summary.
#' @param subtitle An optional subtitle. Defaults to an empty string.
#' @param proportions logical. \code{TRUE} returns proportions, \code{FALSE} returns counts.
#' @param table_of_contents logical. Should a list of tables be included at the start
#' of the file. Defaults to \code{FALSE}.
#' @param font_size Numeric font size points. Defaults to 12pt font.
#' @param font A string specifying the font to use. Defaults to 'Calibri'.
#' @param sample_desc A string describing the sample.
#' @param field_period A string describing the field period.
#' @param moe An optional numeric margin of error.
#' @param digits integer. Number of decimal digits to use for rounding.
#' @param append_text Character string that, if supplied, will be appended after
#' the final table. Useful for adding in disclosure information.
#' @param min_cell_size integer. Minimum number of responses for a cross tabulated
#' category to be displayed in details in a banner report.
#' @param min_cell_label character. If a number of responses for a
#' cross tabulated category is less than min_cell_size then this string is
#' used to mask the results.
#' @param show_totals logical. If \code{TRUE} a 'Totals' row with column sums is displayed.
#' Defaults to \code{TRUE}.
#' @param weighted_n logical. Should the total number of responses be weighted?
#' Defaults to \code{FALSE}.
#' @param row_label_width width of the first column. Defaults to 20pt.
#' @param add_parenthesis logical. Should 'Weighted / Unweighted N' values in banners be parenthesised?
#' Defaults \code{FALSE}.
#' @param one_per_sheet logical. Should every variable be written on a separate sheet?
#' Defaults to \code{TRUE}.
#' @param returndata logical. If \code{TRUE}, a processed data that was used to produce
#' the report is returned.
#' @param ... other options.
#' @return If \code{returndata} is set to \code{TRUE}, a processed data that was used to produce
#' the report is returned. Otherwise \code{NULL} is returned.
#' @examples
#' \dontrun{
#' ds <- loadDataset('dataset_name')
#' toplines_summary <- tables(ds, weight = 'weight')
#' writeExcel(toplines_summary, 'filename')
#' }
#' @export
writeExcel <- function(x, filename = NULL, proportions = FALSE, digits = 0, title = getName(x),
    subtitle = NULL, returndata = TRUE, table_of_contents = FALSE,
    moe = NULL, sample_desc = "", field_period = "", font = "Calibri", font_size = 12,
    show_totals = TRUE, weighted_n = FALSE, add_parenthesis = FALSE, append_text = "",
    min_cell_size = NULL, min_cell_label = NULL, one_per_sheet = TRUE, row_label_width = 20) {

    UseMethod("writeExcel", x)
}

#' @export
writeExcel.Toplines <- function(x, filename = NULL, proportions = FALSE, digits = 0, title = getName(x),
    subtitle = NULL, returndata = TRUE, table_of_contents = FALSE,
    moe = NULL, sample_desc = "", field_period = "", font = "Calibri", font_size = 12,
    show_totals = TRUE, weighted_n = FALSE, add_parenthesis = FALSE, append_text = "",
    min_cell_size = NULL, min_cell_label = NULL, one_per_sheet = TRUE, row_label_width = 20) {

    x$results <- lapply(x$results, function(var_data) {
        var_data$data <- reformatResults(var_data, proportions = proportions, digits = digits,
            reformat = FALSE)
        var_data
    })

    writeVarGeneral(x, filename = filename, proportions = proportions, digits = digits,
        title = title, subtitle = subtitle, returndata = returndata, table_of_contents = table_of_contents,
        moe = moe, sample_desc = sample_desc, field_period = field_period, font = font,
        font_size = font_size, show_totals = show_totals, add_parenthesis = add_parenthesis,
        append_text = append_text, min_cell_size = min_cell_size, min_cell_label = min_cell_label,
        one_per_sheet = one_per_sheet, row_label_width = row_label_width)

}

#' @export
writeExcel.Crosstabs <- function(x, filename = NULL, proportions = TRUE, digits = 0, title = getName(x),
    subtitle = NULL, returndata = TRUE, table_of_contents = FALSE,
    moe = NULL, sample_desc = "", field_period = "", font = "Calibri", font_size = 12,
    show_totals = TRUE, weighted_n = FALSE, add_parenthesis = FALSE, append_text = "",
    min_cell_size = NULL, min_cell_label = NULL, one_per_sheet = TRUE, row_label_width = 20) {

    banner <- x$banner
    x$results <- reformatCrosstabsResults(x$results, banner, proportions = proportions, digits = digits,
        add_parenthesis = add_parenthesis, show_totals = show_totals, weighted_n = weighted_n,
        min_cell_size = min_cell_size, min_cell_label = min_cell_label, reformat = FALSE)
    x$results <- flattenBannerResults(x$results)
    banner <- flattenBanner(banner)

    writeVarGeneral(x, banner, filename = filename, proportions = proportions, digits = digits,
        title = title, subtitle = subtitle, returndata = returndata, table_of_contents = table_of_contents,
        moe = moe, sample_desc = sample_desc, field_period = field_period, font = font,
        font_size = font_size, show_totals = show_totals, add_parenthesis = add_parenthesis,
        append_text = append_text, min_cell_size = min_cell_size, min_cell_label = min_cell_label,
        one_per_sheet = one_per_sheet, row_label_width = row_label_width)

}

create_table_of_contents <- function(wb, title, subtitle, sample_desc, field_period,
    moe) {
    toc_sheet <- "Table of Contents"
    openxlsx::addWorksheet(wb, toc_sheet)
    toc_row <- 1
    openxlsx::writeData(wb, toc_sheet, title, startCol = 1, startRow = toc_row)
    openxlsx::addStyle(wb, toc_sheet, openxlsx::createStyle(textDecoration = "bold"), rows = toc_row,
        cols = 1)
    toc_row <- toc_row + 1
    if (!is.null(subtitle)) {
        openxlsx::writeData(wb, toc_sheet, subtitle, startCol = 1, startRow = toc_row)
        toc_row <- toc_row + 1
    }
    if (sample_desc != "") {
        openxlsx::writeData(wb, toc_sheet, "Sample", startCol = 1, startRow = toc_row)
        openxlsx::writeData(wb, toc_sheet, sample_desc, startCol = 2, startRow = toc_row)
        toc_row <- toc_row + 1
    }
    if (field_period != "") {
        openxlsx::writeData(wb, toc_sheet, "Conducted", startCol = 1, startRow = toc_row)
        openxlsx::writeData(wb, toc_sheet, field_period, startCol = 2, startRow = toc_row)
        toc_row <- toc_row + 1
    }
    if (!is.null(moe)) {
        openxlsx::writeData(wb, toc_sheet, "Margin of Error", startCol = 1, startRow = toc_row)
        openxlsx::writeData(wb, toc_sheet, moe, startCol = 2, startRow = toc_row)
        toc_row <- toc_row + 1
    }
    toc_row <- toc_row + 1
    openxlsx::writeData(wb, toc_sheet, "List of Tables", startCol = 1, startRow = toc_row)
    toc_row <- toc_row + 1

    list(toc_sheet = toc_sheet, toc_row = toc_row)
}

writeExcelVarBanner <- function(wb, ws, x, banner, start_col = 1, start_row = 1,
    digits = 0, proportions = TRUE, show_totals = TRUE, row_label_width = 20, toc_sheet = NULL,
    toc_row = 1) {

    crow <- writeVarHeader(wb, ws, x, start_col = start_col, start_row = start_row,
        toc_sheet = toc_sheet, toc_row = toc_row)
    ccol <- start_col
    multicols <- sapply(banner, getNames)
    multicols_csum <- cumsum(c(2, sapply(multicols, length)))
    lapply(seq_along(banner), function(bidx) {
        openxlsx::writeData(wb, ws, getName(banner[[bidx]]), startCol = multicols_csum[bidx],
            startRow = crow)
        openxlsx::mergeCells(wb, ws, cols = multicols_csum[bidx]:(multicols_csum[bidx + 1] -
            1), rows = crow)
        openxlsx::addStyle(wb, ws, openxlsx::createStyle(textDecoration = "bold", halign = "center"),
            rows = crow, cols = multicols_csum[[1]]:(multicols_csum[[length(multicols_csum)]] -
                1), stack = TRUE)
    })

    crow <- crow + 1
    df <- as.data.frame(x$crosstabs, optional = TRUE)
    openxlsx::writeData(wb, ws, df, startCol = ccol, startRow = crow, rowNames = TRUE)
    openxlsx::addStyle(wb, ws, openxlsx::createStyle(halign = "center"), rows = crow, cols = multicols_csum[[1]]:(multicols_csum[[length(multicols_csum)]] -
        1), stack = TRUE)
    openxlsx::addStyle(wb, ws, openxlsx::createStyle(textDecoration = "italic", halign = "center"), rows = crow +
        nrow(df), cols = multicols_csum[[1]]:(multicols_csum[[length(multicols_csum)]] -
        1), stack = TRUE)
    openxlsx::addStyle(wb, ws, openxlsx::createStyle(border = "Bottom"), rows = crow, cols = 1:(multicols_csum[[length(multicols_csum)]] -
        1), stack = TRUE)
    openxlsx::addStyle(wb, ws, openxlsx::createStyle(border = "Top"), rows = crow + nrow(df) - ifelse(!x$options$no_totals & show_totals,
        1, 0), cols = 1:(multicols_csum[[length(multicols_csum)]] - 1), stack = TRUE)
    openxlsx::addStyle(wb, ws, openxlsx::createStyle(numFmt = paste0("0", if (digits > 0)
        paste0(".", paste0(rep(0, digits), collapse = "")), if (proportions)
        "%")), rows = (crow + 1):(crow + nrow(df) - 1), cols = 2:(multicols_csum[[length(multicols_csum)]] -
        1), gridExpand = TRUE, stack = TRUE)
    openxlsx::setColWidths(wb, ws, 1, row_label_width)

    return(crow + nrow(df))
}


writeExcelVarTopline <- function(wb, ws, x, start_col = 1, start_row = 1, digits = 0,
    proportions = TRUE, row_label_width = 20, toc_sheet = NULL, toc_row = 1) {
    UseMethod("writeExcelVarTopline", x)
}

#' @export
writeExcelVarTopline.default <- function(wb, ws, x) {
    stop(paste("The 'writeExcelVarTopline' generic function doesn't support objects of type:",
        paste(class(x), collapse = ",")))
}

#' @export
writeExcelVarTopline.ToplineCategoricalArray <- function(wb, ws, x, start_col = 1,
    start_row = 1, digits = 0, proportions = TRUE) {
    df <- as.matrix(x$data)
    openxlsx::writeData(wb, ws, df, startCol = start_col, startRow = start_row, rowNames = TRUE,
        colNames = TRUE)
    numFmt <- paste0("0", if (digits > 0)
        paste0(".", paste0(rep(0, digits), collapse = "")), if (proportions)
        "%")
    drows <- nrow(df)
    rows <- (start_row + 1):(start_row + drows)
    cols <- 2:(ncol(x$data) + 1)
    openxlsx::addStyle(wb, ws, openxlsx::createStyle(numFmt = numFmt), rows = rows, cols = cols, gridExpand = TRUE,
        stack = TRUE)
    return(drows)
}

#' @export
writeExcelVarTopline.ToplineNumeric <- function(wb, ws, x, start_col = 1, start_row = 1,
    digits = 0) {
    df <- as.matrix(x$data)
    openxlsx::writeData(wb, ws, df, startCol = start_col, startRow = start_row, rowNames = TRUE,
        colNames = FALSE)
    numFmt <- paste0("0", if (digits > 0)
        paste0(".", paste0(rep(0, digits), collapse = "")))
    drows <- nrow(df)
    rows <- (start_row):(start_row + drows - 1)
    openxlsx::addStyle(wb, ws, openxlsx::createStyle(numFmt = numFmt), rows = rows, cols = 2, gridExpand = FALSE,
        stack = TRUE)
    return(drows)
}

#' @export
writeExcelVarTopline.ToplineCategorical <- function(wb, ws, x, start_col = 1, start_row = 1,
    digits = 0, proportions = TRUE) {
    drows <- writeExcelVarToplineCategorical(wb, ws, x, start_col = start_col, start_row = start_row,
        digits = digits, proportions = proportions)
    return(drows)
}
#' @export
writeExcelVarTopline.ToplineMultipleResponse <- function(wb, ws, x, start_col = 1,
    start_row = 1, digits = 0, proportions = TRUE) {
    drows <- writeExcelVarToplineCategorical(wb, ws, x, start_col = start_col, start_row = start_row,
        digits = digits, proportions = proportions)
    return(drows)
}

writeExcelVarToplineCategorical <- function(wb, ws, x, start_col = 1, start_row = 1,
    digits = 0, proportions = TRUE) {
    df <- as.matrix(x$data)
    openxlsx::writeData(wb, ws, df, startCol = start_col, startRow = start_row, rowNames = TRUE,
        colNames = FALSE)
    numFmt <- paste0("0", if (digits > 0)
        paste0(".", paste0(rep(0, digits), collapse = "")), if (proportions)
        "%")
    drows <- nrow(df)
    rows <- (start_row):(start_row + drows - 1)
    openxlsx::addStyle(wb, ws, openxlsx::createStyle(numFmt = numFmt), rows = rows, cols = 2, gridExpand = FALSE,
        stack = TRUE)
    return(drows)
}

writeExcelVarToplineGeneral <- function(wb, ws, x, start_col = 1, start_row = 1,
    digits = 0, proportions = TRUE, row_label_width = 20, toc_sheet = NULL, toc_row = 1) {
    crow <- writeVarHeader(wb, ws, x, start_col = start_col, start_row = start_row,
        toc_sheet = toc_sheet, toc_row = toc_row)
    crow <- crow + 1
    drows <- writeExcelVarTopline(wb, ws, x, start_col = start_col, start_row = crow,
        digits = digits, proportions = proportions)
    openxlsx::setColWidths(wb, ws, 1, row_label_width)
    return(crow + drows)
}


writeVarHeader <- function(wb, ws, x, start_col = 1, start_row = 1, toc_sheet = NULL,
    toc_row = 1) {
    crow <- start_row
    ccol <- start_col
    openxlsx::writeData(wb, ws, getName(x), startCol = ccol, startRow = crow)
    openxlsx::addStyle(wb, ws, openxlsx::createStyle(textDecoration = "bold"), rows = crow, cols = ccol)
    crow <- crow + 1
    if (!is.null(toc_sheet)) {
        openxlsx::writeFormula(wb, toc_sheet, startRow = toc_row, x = openxlsx::makeHyperlinkString(sheet = ws,
            row = start_row, col = start_col, text = getName(x)))
        openxlsx::addStyle(wb, toc_sheet, openxlsx::createStyle(fontColour = "black", textDecoration = "underline"),
            rows = toc_row, cols = 1, stack = FALSE)
    }
    openxlsx::writeData(wb, ws, getDescription(x), startCol = ccol, startRow = crow)
    crow <- crow + 1
    filtertext <- getNotes(x)
    if (filtertext != "") {
        openxlsx::writeData(wb, ws, filtertext, startCol = ccol, startRow = crow)
        openxlsx::addStyle(wb, ws, openxlsx::createStyle(textDecoration = "italic"), rows = crow, cols = ccol)
        crow <- crow + 1
    }
    crow + 1
}

writeVarGeneral <- function(x, banner = NULL, filename = NULL, proportions = TRUE,
    digits = 0, title = "", subtitle = NULL, returndata = TRUE, table_of_contents = FALSE,
    moe = NULL, sample_desc = "", field_period = "", font = "Calibri", font_size = 12,
    show_totals = TRUE, add_parenthesis = FALSE, append_text = "", min_cell_size = NULL,
    min_cell_label = NULL, one_per_sheet = TRUE, row_label_width = 20) {

    wb <- openxlsx::createWorkbook()

    openxlsx::modifyBaseFont(wb, fontSize = font_size, fontColour = "black", fontName = font)

    toc_res <- if (table_of_contents)
        create_table_of_contents(wb, title, subtitle, sample_desc, field_period,
            moe) else NULL
    toc_sheet <- if (is.null(toc_res))
        NULL else toc_res$toc_sheet
    toc_row <- if (is.null(toc_res))
        1 else toc_res$toc_row

    worksheet_name <- "Results"
    if (!one_per_sheet) {
        openxlsx::addWorksheet(wb, worksheet_name)
    }
    start_col <- 1
    start_row <- 1
    last_row_used <- 0
    for (vidx in seq_along(x$results)) {
        if (one_per_sheet) {
            worksheet_name <- getAlias(x$results[[vidx]])
            # worksheet names must be unique and have less then 32 characters
            if (nchar(worksheet_name) > 25) {
                worksheet_name <- paste0(strtrim(worksheet_name, 25), vidx)
            }
            openxlsx::addWorksheet(wb, worksheet_name)
        } else {
            start_row <- last_row_used + if (vidx > 1)
                5 else 1
        }
        last_row_used <- if (is.null(banner)) {
            writeExcelVarToplineGeneral(wb, worksheet_name, x$results[[vidx]], start_col = start_col,
                start_row = start_row, digits = digits, proportions = proportions,
                row_label_width = row_label_width, toc_sheet = toc_sheet, toc_row = toc_row)
        } else {
            writeExcelVarBanner(wb, worksheet_name, x$results[[vidx]], banner, start_col = start_col,
                start_row = start_row, digits = digits, proportions = proportions,
                show_totals = show_totals, row_label_width = row_label_width, toc_sheet = toc_sheet,
                toc_row = toc_row)
        }
        toc_row <- toc_row + 1
    }
    if (append_text != "") {
        worksheet_name <- "Notes"
        openxlsx::addWorksheet(wb, worksheet_name)
        openxlsx::writeData(wb, worksheet_name, append_text, startCol = 1, startRow = 1)
    }
    if (!is.null(filename)) {
        openxlsx::saveWorkbook(wb, paste0(filename, ".xlsx"), overwrite = TRUE)
    }
    if (returndata) {
        return(x)
    }
}


# writeExcelVarToplineGeneral <- function(wb, ws, x, start_col = 1, start_row =
# 1, digits = 0, proportions = TRUE, row_label_width = 20, toc_sheet = NULL,
# toc_row = 1) { crow <- writeVarHeader(wb, ws, x, start_col = start_col,
# start_row = start_row, toc_sheet = toc_sheet, toc_row = toc_row,
# row_label_width)  ccol <- start_col crow <- crow + 1 drows <- nrow(x$data)
# dcols <- ncol(x$data) is_grid <- !is.na(dcols) df <- as.matrix(x$data)
# openxlsx::writeData(wb, ws, df, startCol = ccol, startRow = crow, rowNames = TRUE,
# colNames = is_grid) numFmt <- paste0('0', if (digits > 1) paste0('.', rep(0,
# digits)), if (proportions) '%') rows <- (crow + if (is_grid) 1 else 0) : (crow
# + (if (is_grid) 1 else 0) + drows - 1) cols <- 2 : (if (is_grid) dcols + 1 else
# 2) openxlsx::addStyle(wb, ws, openxlsx::createStyle(numFmt = numFmt), rows = rows, cols = cols,
# gridExpand = is_grid, stack = TRUE) openxlsx::setColWidths(wb, ws, 1, row_label_width)
# return(crow + drows) }
