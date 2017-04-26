
#' Generate Excel Reports: Toplines and Banners
#'
#' \code{writeExcel} produces publication-quality Excel reports:
#' Toplines (one-way frequency tables) or Banners (cross tabulations)
#'
#' @param data_summary An object of class \code{Toplines} or \code{Crosstabs}.
#' @param filename The name of the output file (without an extension).
#' @param title An optional title. Defaults to the title provided in the summary.
#' @param subtitle An optional subtitle. Defaults to an empty string.
#' @param proportions logical. If \code{TRUE} the output report shows proportions,
#' if \code{FALSE} the report shows counts.
#' @param table_of_contents logical. Should a list of tables be included at the start
#' of the report. Defaults to \code{FALSE}.
#' @param font A text specifying the font to use. Defaults to 'Calibri'.
#' @param font_size Numeric font size points. Defaults to 12 pt font.
#' @param report_desc An optional named list of report descriptions.
#' @param digits integer. Number of decimal digits to use for rounding.
#' @param append_text A text that, if supplied, will be appended after
#' the final table. Useful for adding in disclosure information.
#' @param min_cell_size integer. Minimum number of responses for a cross tabulated
#' category to be displayed in details in a banner report.
#' @param min_cell_label character. If a number of responses for a
#' cross tabulated category is less than min_cell_size then this text is
#' used to mask the results. Defaults to \code{NULL} - the values are greyed out.
#' @param show_totals logical. If \code{TRUE} a 'Totals' row with column sums is displayed.
#' Defaults to \code{TRUE}.
#' @param weighted_n logical. Should the total number of responses be weighted?
#' Defaults to \code{FALSE}.
#' @param row_label_width width of the first column. Defaults to 20pt.
#' @param one_per_sheet logical. Should every variable be written on a separate sheet?
#' Defaults to \code{TRUE}.
#' @param open logical. Should the report, if successfully generated, be opened with
#' the default application? Defaults to \code{FALSE}.
#' @param logo A list of parameters describing the logo:
#' \itemize{
#'  \item file - The path to the logo file. Valid file types are: jpeg, png, bmp.
#'  \item startRow - Row coordinate of upper left corner of the image.
#'  \item startCol - Column coordinate of upper left corner of the image.
#'  \item width - Width of figure.
#'  \item height - Height of figure.
#'  \item units - Units of width and height. Can be "in", "cm" or "px".
#'  \item dpi - Image resolution used for conversion between units.
#' }
#' Defaults to \code{NULL} - no logo is used.
#' @param wrap_categories logical. Should the categories' names be wrapped?
#' Defaults to \code{FALSE}.
#' @param return_data logical. If \code{TRUE}, a processed data that was used to produce
#' the report is returned.
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
writeExcel <- function(data_summary, filename = NULL, title = getName(data_summary), subtitle = NULL,
                       proportions = FALSE, digits = 0, table_of_contents = FALSE, logo = NULL,
                       report_desc = NULL, font = "Calibri", font_size = 12,
                       show_totals = TRUE, weighted_n = FALSE, append_text = "", wrap_categories = FALSE,
                       min_cell_size = NULL, min_cell_label = NULL, one_per_sheet = TRUE, row_label_width = 20,
                       open = FALSE, return_data = TRUE) {

    UseMethod("writeExcel", data_summary)
}

#' @export
writeExcel.Toplines <- function(data_summary, filename = NULL, title = getName(data_summary), subtitle = NULL,
                                proportions = FALSE, digits = 0, table_of_contents = FALSE, logo = NULL,
                                report_desc = NULL, font = "Calibri", font_size = 12,
                                show_totals = TRUE, weighted_n = FALSE, append_text = "", wrap_categories = FALSE,
                                min_cell_size = NULL, min_cell_label = NULL, one_per_sheet = TRUE, row_label_width = 20,
                                open = FALSE, return_data = TRUE) {

  data_summary$results <- lapply(data_summary$results, function(var_data) {
        var_data$data <- reformatResults(var_data, proportions = proportions, digits = digits,
            reformat = FALSE)
        var_data
    })

    styles = list(
      name = openxlsx::createStyle(textDecoration = "bold"),
      filtertext = openxlsx::createStyle(textDecoration = "italic"),
      numeric = openxlsx::createStyle(numFmt = paste0("0", if (digits > 0)
        paste0(".", paste0(rep(0, digits), collapse = "")))),
      categorical = openxlsx::createStyle(numFmt = paste0("0", if (digits > 0)
        paste0(".", paste0(rep(0, digits), collapse = "")), if (proportions)
          "%")),
      toc_title = openxlsx::createStyle(textDecoration = "bold"),
      toc_slot = openxlsx::createStyle(fontColour = "black", textDecoration = "underline")
    )

    writeReportGeneral(data_summary, filename = filename, proportions = proportions, digits = digits,
        title = title, subtitle = subtitle, return_data = return_data, table_of_contents = table_of_contents,
        report_desc = report_desc, font = font,
        font_size = font_size, show_totals = show_totals,
        append_text = append_text, min_cell_size = min_cell_size, min_cell_label = min_cell_label,
        one_per_sheet = one_per_sheet, row_label_width = row_label_width, styles = styles, logo = logo)

}

#' @export
writeExcel.Crosstabs <- function(data_summary, filename = NULL, title = getName(data_summary), subtitle = NULL,
                                 proportions = FALSE, digits = 0, table_of_contents = FALSE, logo = NULL,
                                 report_desc = NULL, font = "Calibri", font_size = 12,
                                 show_totals = TRUE, weighted_n = FALSE, append_text = "", wrap_categories = FALSE,
                                 min_cell_size = NULL, min_cell_label = NULL, one_per_sheet = TRUE, row_label_width = 20,
                                 open = FALSE, return_data = TRUE) {

    banner <- data_summary$banner
    data_summary$results <- reformatCrosstabsResults(data_summary$results, banner, proportions = proportions, digits = digits,
        add_parenthesis = FALSE, show_totals = show_totals, weighted_n = weighted_n,
        min_cell_size = min_cell_size, min_cell_label = min_cell_label, reformat = FALSE)
    if (one_per_sheet && length(banner) > 1) {
      banner_name <- "Results"
      data_summary$results <- mergeBannerResults(data_summary$results, banner_name = banner_name)
      banner <- mergeBanner(banner, banner_name = banner_name)
    }

    numFmt <- paste0("0", if (digits > 0) paste0(".", paste0(rep(0, digits), collapse = "")))
    styles = list(
      name = openxlsx::createStyle(textDecoration = "bold"),
      filtertext = openxlsx::createStyle(textDecoration = "italic"),
      body = openxlsx::createStyle(numFmt = paste0(numFmt, if (proportions)
          "%")),
      labels = openxlsx::createStyle(textDecoration = "bold", halign = "center"),
      # categories = openxlsx::createStyle(halign = "center"),
      categories_border = openxlsx::createStyle(halign = "center", border = "Bottom", wrapText = TRUE),
      n = openxlsx::createStyle(numFmt = numFmt, textDecoration = "italic", halign = "center"),
      # border_bottom = openxlsx::createStyle(border = "Bottom"),
      border_top = openxlsx::createStyle(border = "Top"),
      toc_title = openxlsx::createStyle(textDecoration = "bold"),
      toc_slot = openxlsx::createStyle(fontColour = "black", textDecoration = "underline"),
      toc_banner = openxlsx::createStyle(textDecoration = "bold")
    )

    writeReportGeneral(data_summary, banner, filename = filename, proportions = proportions, digits = digits,
        title = title, subtitle = subtitle, return_data = return_data, table_of_contents = table_of_contents,
        report_desc = report_desc, font = font,
        font_size = font_size, show_totals = show_totals,
        append_text = append_text, min_cell_size = min_cell_size, min_cell_label = min_cell_label,
        one_per_sheet = one_per_sheet, row_label_width = row_label_width, styles = styles, logo = logo)

}

create_table_of_contents <- function(wb, title, subtitle, report_desc = NULL, styles = NULL, logo = NULL) {
    toc_sheet <- "Table of Contents"
    openxlsx::addWorksheet(wb, toc_sheet)
    toc_row <- 1
    openxlsx::writeData(wb, toc_sheet, title, startCol = 1, startRow = toc_row)
    openxlsx::addStyle(wb, toc_sheet, styles$toc_title, rows = toc_row,
        cols = 1)
    toc_row <- toc_row + 1
    if (!is.null(subtitle)) {
        openxlsx::writeData(wb, toc_sheet, subtitle, startCol = 1, startRow = toc_row)
        toc_row <- toc_row + 1
    }

    desc_names <- names(report_desc)
    for (desc_num in seq_along(report_desc)) {
      openxlsx::writeData(wb, toc_sheet, desc_names[desc_num], startCol = 1, startRow = toc_row)
      openxlsx::writeData(wb, toc_sheet, report_desc[[desc_num]], startCol = 2, startRow = toc_row)
      toc_row <- toc_row + 1
    }

    if (!is.null(logo)) {
      openxlsx::insertImage(wb, toc_sheet, file = logo$file,
                            startRow = if (is.null(logo$startRow)) 1 else logo$startRow,
                            startCol = if (is.null(logo$startCol)) 4 else logo$startCol,
                            width = if (is.null(logo$width)) 4 else logo$width,
                            height = if (is.null(logo$height)) 2 else logo$height,
                            units = if (is.null(logo$units)) "in" else logo$units,
                            dpi = if (is.null(logo$dpi)) 300 else logo$dpi)
    }
    toc_row <- toc_row + 1
    openxlsx::writeData(wb, toc_sheet, "List of Tables", startCol = 1, startRow = toc_row)
    toc_row <- toc_row + 1

    list(toc_sheet = toc_sheet, toc_row = toc_row)
}

writeExcelVarBanner <- function(wb, ws, banner_name, x, banner, start_col = 1, start_row = 1,
    digits = 0, proportions = TRUE, show_totals = TRUE, row_label_width = 20, toc_sheet = NULL,
    toc_row = 1, toc_col = 1, styles = NULL) {

    crow <- writeVarHeader(wb, ws, x, start_col = start_col, start_row = start_row,
        toc_sheet = toc_sheet, toc_row = toc_row, toc_col = toc_col, styles = styles)
    ccol <- start_col
    multicols <- sapply(banner[[banner_name]], getNames)
    multicols_csum <- cumsum(c(2, sapply(multicols, length)))
    lapply(seq_along(banner[[banner_name]]), function(bidx) {
        openxlsx::writeData(wb, ws, getName(banner[[banner_name]][[bidx]]), startCol = multicols_csum[bidx],
            startRow = crow)
        openxlsx::mergeCells(wb, ws, cols = multicols_csum[bidx]:(multicols_csum[bidx + 1] -
            1), rows = crow)
        openxlsx::addStyle(wb, ws, styles$labels,
            rows = crow, cols = multicols_csum[[1]]:(multicols_csum[[length(multicols_csum)]] -
                1), stack = FALSE)
    })

    crow <- crow + 1
    df <- as.data.frame(x$crosstabs[[banner_name]], optional = TRUE)
    openxlsx::writeData(wb, ws, df, startCol = ccol, startRow = crow, rowNames = TRUE, headerStyle = styles$categories_border)
    df1 <- df[nrow(df),]
    df1[] <- as.numeric(df1)
    openxlsx::writeData(wb, ws, df1, startCol = ccol, startRow = crow + nrow(df), rowNames = TRUE, colNames = FALSE)
    # openxlsx::writeData(wb, ws, t(as.numeric(df[nrow(df),])), startCol = ccol + 1, startRow = crow + nrow(df), rowNames = FALSE, colNames = FALSE)
    openxlsx::addStyle(wb, ws, styles$n, rows = crow + nrow(df), cols = multicols_csum[[1]]:(multicols_csum[[length(multicols_csum)]] - 1), stack = FALSE)
    openxlsx::addStyle(wb, ws, styles$body, rows = (crow + 1):(crow + nrow(df) - 1), cols = 2:(multicols_csum[[length(multicols_csum)]] -
                                                                                                 1), gridExpand = TRUE, stack = FALSE)
    # openxlsx::addStyle(wb, ws, styles$categories, rows = crow, cols = multicols_csum[[1]]:(multicols_csum[[length(multicols_csum)]] -
                                                                                             # 1), stack = FALSE)
    openxlsx::addStyle(wb, ws, styles$border_top, rows = crow + nrow(df) - ifelse(!x$options$no_totals & show_totals,
    1, 0), cols = 1:(multicols_csum[[length(multicols_csum)]] - 1), stack = TRUE)
    # openxlsx::addStyle(wb, ws, styles$border_bottom, rows = crow, cols = 1:(multicols_csum[[length(multicols_csum)]] -
                                                                              # 1), stack = TRUE)
    openxlsx::setColWidths(wb, ws, 1, row_label_width)

    return(crow + nrow(df))
}

writeExcelVarTopline <- function(wb, ws, x, start_col = 1, start_row = 1, digits = 0,
    proportions = TRUE, styles = NULL) {
    UseMethod("writeExcelVarTopline", x)
}

#' @export
writeExcelVarTopline.default <- function(wb, ws, x, start_col = 1, start_row = 1, digits = 0,
                                         proportions = TRUE, styles = NULL) {
    stop(paste("The 'writeExcelVarTopline' generic function doesn't support objects of type:",
        paste(class(x), collapse = ",")))
}

#' @export
writeExcelVarTopline.ToplineCategoricalArray <- function(wb, ws, x, start_col = 1,
    start_row = 1, digits = 0, proportions = TRUE, styles = NULL) {
    df <- as.matrix(x$data)
    openxlsx::writeData(wb, ws, df, startCol = start_col, startRow = start_row, rowNames = TRUE,
        colNames = TRUE)
    drows <- nrow(df)
    rows <- (start_row + 1):(start_row + drows)
    cols <- 2:(ncol(x$data) + 1)
    openxlsx::addStyle(wb, ws, styles$categorical, rows = rows, cols = cols, gridExpand = TRUE,
        stack = FALSE)
    return(drows)
}

#' @export
writeExcelVarTopline.ToplineNumeric <- function(wb, ws, x, start_col = 1, start_row = 1,
    digits = 0, proportions = NULL, styles = NULL) {
    df <- as.matrix(x$data)
    openxlsx::writeData(wb, ws, df, startCol = start_col, startRow = start_row, rowNames = TRUE,
        colNames = FALSE)
    drows <- nrow(df)
    rows <- (start_row):(start_row + drows - 1)
    openxlsx::addStyle(wb, ws, styles$numeric, rows = rows, cols = 2, gridExpand = FALSE,
        stack = TRUE)
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
    openxlsx::addStyle(wb, ws, styles$categorical, rows = rows, cols = 2, gridExpand = FALSE,
        stack = FALSE)
    return(drows)
}

writeExcelVarToplineGeneral <- function(wb, ws, x, start_col = 1, start_row = 1,
    digits = 0, proportions = TRUE, row_label_width = 20, toc_sheet = NULL, toc_row = 1
    , styles = NULL) {
    crow <- writeVarHeader(wb, ws, x, start_col = start_col, start_row = start_row,
        toc_sheet = toc_sheet, toc_row = toc_row, styles = styles)
    drows <- writeExcelVarTopline(wb, ws, x, start_col = start_col, start_row = crow,
        digits = digits, proportions = proportions, styles = styles)
    openxlsx::setColWidths(wb, ws, 1, row_label_width)
    return(crow + drows)
}


writeVarHeader <- function(wb, ws, x, start_col = 1, start_row = 1, toc_sheet = NULL,
    toc_row = 1, toc_col = 1, styles = NULL) {
    crow <- start_row
    ccol <- start_col
    openxlsx::writeData(wb, ws, getName(x), startCol = ccol, startRow = crow)
    openxlsx::addStyle(wb, ws, styles$name, rows = crow, cols = ccol)
    crow <- crow + 1
    if (!is.null(toc_sheet)) {
        openxlsx::writeFormula(wb, toc_sheet, startCol = toc_col, startRow = toc_row, x = openxlsx::makeHyperlinkString(sheet = ws,
            row = start_row, col = start_col, text = getName(x)))
        openxlsx::addStyle(wb, toc_sheet, styles$toc_slot,
            rows = toc_row, cols = toc_col, stack = FALSE)
    }
    var_description <- getDescription(x)
    if (!is.null(var_description) && var_description != "") {
      openxlsx::writeData(wb, ws, var_description, startCol = ccol, startRow = crow)
      crow <- crow + 1
    }
    filtertext <- getNotes(x)
    if (!is.null(filtertext) && filtertext != "") {
        openxlsx::writeData(wb, ws, filtertext, startCol = ccol, startRow = crow)
        openxlsx::addStyle(wb, ws, styles$filtertext, rows = crow, cols = ccol)
        crow <- crow + 1
    }
    crow + 1
}

writeReportGeneral <- function(x, banner = NULL, filename = NULL, proportions = TRUE,
    digits = 0, title = "", subtitle = NULL, return_data = TRUE, table_of_contents = FALSE,
    report_desc = NULL, font = "Calibri", font_size = 12,
    show_totals = TRUE, append_text = "", min_cell_size = NULL,
    min_cell_label = NULL, one_per_sheet = TRUE, row_label_width = 20, styles = NULL,
    logo = NULL) {

    wb <- openxlsx::createWorkbook()
    openxlsx::modifyBaseFont(wb, fontSize = font_size, fontColour = "black", fontName = font)

    toc_sheet <- NULL
    toc_row <- 1
    toc_col <- 1
    if (table_of_contents) {
      toc_res <- create_table_of_contents(wb, title, subtitle, report_desc = report_desc, styles = styles, logo = logo)
      toc_sheet <- toc_res$toc_sheet
      toc_row <- toc_res$toc_row
    }

    banner_names <- if (is.null(banner)) "Results" else names(banner)
    if (!one_per_sheet) {
      for (worksheet_name in banner_names) {
        openxlsx::addWorksheet(wb, worksheet_name)
      }
      toc_col <- 2
    }
    for (banner_name in banner_names) {
      start_col <- 1
      start_row <- 1
      last_row_used <- 0
      if (!one_per_sheet) {
        worksheet_name <- banner_name
        if (table_of_contents && !is.null(banner)) {
          openxlsx::writeData(wb, toc_sheet, worksheet_name, startRow = toc_row, startCol = 1)
          openxlsx::addStyle(wb, toc_sheet, styles$toc_banner, rows = toc_row, cols = 1, stack = FALSE)
          toc_row <- toc_row + 1
        }
      }

      for (vidx in seq_along(x$results)) {
          if (one_per_sheet) {
              worksheet_name <- getAlias(x$results[[vidx]])
              # worksheet names must be unique and have less then 32 characters
              if (nchar(worksheet_name) > 25) {
                  worksheet_name <- paste0(strtrim(worksheet_name, 25), vidx)
              }
              openxlsx::addWorksheet(wb, worksheet_name)
          } else {
              start_row <- last_row_used + if (vidx > 1) 5 else 1
          }
          last_row_used <- if (is.null(banner)) {
              writeExcelVarToplineGeneral(wb, worksheet_name, x$results[[vidx]], start_col = start_col,
                  start_row = start_row, digits = digits, proportions = proportions,
                  row_label_width = row_label_width, toc_sheet = toc_sheet, toc_row = toc_row, styles = styles)
          } else {
              writeExcelVarBanner(wb, worksheet_name, banner_name, x$results[[vidx]], banner, start_col = start_col,
                  start_row = start_row, digits = digits, proportions = proportions,
                  show_totals = show_totals, row_label_width = row_label_width, toc_sheet = toc_sheet,
                  toc_row = toc_row, toc_col = toc_col, styles = styles)
          }
          toc_row <- toc_row + 1
      }
    }
    if (append_text != "") {
        worksheet_name <- "Notes"
        openxlsx::addWorksheet(wb, worksheet_name)
        openxlsx::writeData(wb, worksheet_name, append_text, startCol = 1, startRow = 1)
    }
    if (!is.null(filename)) {
        openxlsx::saveWorkbook(wb, paste0(filename, ".xlsx"), overwrite = TRUE)
    }
    if (return_data) {
        return(x)
    }
}
