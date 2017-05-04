
#' Generate Excel Reports: Toplines and Banners
#'
#' \code{writeExcel} produces publication-quality Excel reports:
#' Toplines (one-way frequency tables) or Banners (cross tabulations)
#'
#' @param data_summary An object of class \code{Toplines} or \code{Crosstabs}.
#' @param filename The name of the output file (without an extension).
#' @param title An optional title. Defaults to the title provided in the summary.
#' @param subtitle An optional subtitle. Defaults to an empty string.
#' @param proportions logical. If \code{TRUE}, the output report shows proportions,
#' if \code{FALSE}, the report shows counts.
#' @param table_of_contents logical. Should a list of tables be included at the start
#' of the report. Defaults to \code{FALSE}.
#' @param font A text specifying the font to use. Defaults to \code{'Calibri'}.
#' @param font_size Numeric font size points. Defaults to 12pt font.
#' @param report_desc An optional named list of report descriptions that should be
#' displayed on the front page / table of contents, e.g.
#' \code{list(`Fieldwork date` = "2017-01-01", `Sample description` = "description")}.
#' Defaults to \code{NULL}
#' @param digits integer. The number of decimal places that should be used for
#' rounding numbers. Defaults to \code{0}.
#' @param append_text A text that, if supplied, will be appended after
#' the final table. Useful for adding in disclosure information.
#' @param min_base_size integer. The minimum number of responses for a cross tabulated
#' category to be displayed in details in a banner report.
#' @param min_base_label character. If a number of responses for a
#' cross tabulated category is less than \code{min_base_size} then this text is
#' used to mask the results. Defaults to \code{NULL} - the values are greyed out.
#' @param show_totals logical. If \code{TRUE}, the row containing column sums is displayed.
#' Defaults to \code{TRUE}.
#' @param show_description logical. If \code{TRUE}, variables' descriptions are displayed.
#' Defaults to \code{FALSE}.
#' @param unweighted_n A list of parameters describing the row containing the unweighted bases:
#' \itemize{
#'  \item name - row label.
#'  \item position - row position. Valid values are: "top", "bottom", "both".
#' }
#' Defaults to \code{list(name = "Unweighted N", position = "bottom")}.
#' @param weighted_n A list of parameters describing the row containing the weighted bases:
#' \itemize{
#'  \item name - row label.
#'  \item position - row position. Valid values are: "top", "bottom", "both".
#' }
#' Defaults to \code{NULL} - the row containing the weighted bases is not printed.
#' @param show_grid_lines logical. Should grid lines be shown?
#' Defaults to \code{FALSE}.
#' @param banner_vars_split the method of splitting banner variables.
#' Valid values are: {NULL, "empty_col"}.
#' Defaults to \code{NULL} - no split.
#' @param row_label_width width of the first column. Defaults to 30pt.
#' @param labels_wrap list. Specifies which labels should be wrapped:
#' \itemize{
#'  \item name - variable's name.
#'  \item description - variable's description.
#'  \item row_labels - row labels.
#'  \item banner_labels - banner labels
#'  \item column_categories - column categories.
#' }
#' Defaults to \code{labels_wrap = list(name = TRUE, description = TRUE,
#' row_labels = TRUE, banner_labels = TRUE, column_categories = TRUE)}.
#' @param first_active_col number. First active coulumn. Defaults to \code{2}.
#' @param logging logical. Should basic information related to the function execution
#' be printed? Defaults to \code{FALSE}.
#' @param one_per_sheet logical. Should every variable be written on a separate sheet?
#' Defaults to \code{TRUE}.
#' @param logo A list of parameters describing the logo:
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
#' @param reduce_format logical. Should the number of operations that apply styles
#' to tables be minimized? Results in slightly faster execution and slightly different table styles.
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
                       weighted_n = NULL, unweighted_n = list(name = "Unweighted N", position = "bottom"),
                       show_totals = TRUE, report_desc = NULL, font = "Calibri", font_size = 12,
                       one_per_sheet = TRUE, append_text = "",
                       banner_vars_split = NULL, row_label_width = 30, reduce_format = FALSE,
                       min_base_size = NULL, min_base_label = NULL, show_description = FALSE,
                       show_grid_lines = FALSE, return_data = TRUE, logging = FALSE,
                       labels_wrap = list(name = TRUE, description = TRUE,
                            row_labels = TRUE, banner_labels = TRUE, column_categories = TRUE),
                       first_active_col = 2) {

  if (is.null(filename)) {
    stop("No valid filename provided.")
  }
  UseMethod("writeExcel", data_summary)
}

#' @export
writeExcel.Toplines <- function(data_summary, filename = NULL, title = getName(data_summary), subtitle = NULL,
                                proportions = FALSE, digits = 0, table_of_contents = FALSE, logo = NULL,
                                weighted_n = NULL, unweighted_n = list(name = "Unweighted N", position = "bottom"),
                                show_totals = TRUE, report_desc = NULL, font = "Calibri", font_size = 12,
                                one_per_sheet = TRUE, append_text = "",
                                banner_vars_split = NULL, row_label_width = 30, reduce_format = FALSE,
                                min_base_size = NULL, min_base_label = NULL, show_description = FALSE,
                                show_grid_lines = FALSE, return_data = TRUE, logging = FALSE,
                                labels_wrap = list(name = TRUE, description = TRUE,
                                    row_labels = TRUE, banner_labels = TRUE, column_categories = TRUE),
                                first_active_col = 2) {

  data_summary$results <- lapply(data_summary$results, function(var_data) {
        var_data$data <- reformatResults(var_data, proportions = proportions, digits = digits,
            reformat = FALSE)
        var_data
    })

    styles = list(
      name = openxlsx::createStyle(textDecoration = "bold", wrapText = labels_wrap$name),
      filtertext = openxlsx::createStyle(textDecoration = "italic", wrapText = labels_wrap$description),
      numeric = openxlsx::createStyle(numFmt = paste0("0", if (digits > 0)
        paste0(".", paste0(rep(0, digits), collapse = "")))),
      categorical = openxlsx::createStyle(numFmt = paste0("0", if (digits > 0)
        paste0(".", paste0(rep(0, digits), collapse = "")), if (proportions)
          "%"), halign = "center"),
      categories = openxlsx::createStyle(halign = "center", wrapText = labels_wrap$column_categories),
      row_labels = openxlsx::createStyle(wrapText = labels_wrap$row_labels),
      toc_title = openxlsx::createStyle(fontSize = 14, textDecoration = "bold"),
      toc_slot = openxlsx::createStyle(fontColour = "black", textDecoration = "underline")
    )

    writeReportGeneral(data_summary, filename = filename, proportions = proportions, digits = digits,
        title = title, subtitle = subtitle, return_data = return_data, table_of_contents = table_of_contents,
        report_desc = report_desc, font = font, show_grid_lines = show_grid_lines,
        font_size = font_size, show_totals = show_totals, logging = logging,
        append_text = append_text, min_base_size = min_base_size, min_base_label = min_base_label,
        one_per_sheet = one_per_sheet, row_label_width = row_label_width, styles = styles, logo = logo,
        show_description = show_description)
}

#' @export
writeExcel.Crosstabs <- function(data_summary, filename = NULL, title = getName(data_summary), subtitle = NULL,
                                 proportions = FALSE, digits = 0, table_of_contents = FALSE, logo = NULL,
                                 weighted_n = NULL, unweighted_n = list(name = "Unweighted N", position = "bottom"),
                                 show_totals = TRUE, report_desc = NULL, font = "Calibri", font_size = 12,
                                 one_per_sheet = TRUE, append_text = "",
                                 banner_vars_split = NULL, row_label_width = 30, reduce_format = FALSE,
                                 min_base_size = NULL, min_base_label = NULL, show_description = FALSE,
                                 show_grid_lines = FALSE, return_data = TRUE, logging = FALSE,
                                 labels_wrap = list(name = TRUE, description = TRUE,
                                      row_labels = TRUE, banner_labels = TRUE, column_categories = TRUE),
                                 first_active_col = 2) {

    banner <- data_summary$banner
    # data_summary$results <- reformatCrosstabsResults(data_summary$results, banner, proportions = proportions, digits = digits,
    # add_parenthesis = FALSE, show_totals = show_totals, weighted_n = weighted_n,
    # min_base_size = min_base_size, min_base_label = min_base_label, reformat = FALSE)
    if (one_per_sheet && length(banner) > 1) {
      banner_name <- "Results"
      data_summary$results <- mergeBannerResults(data_summary$results, banner_name = banner_name)
      banner <- mergeBanner(banner, banner_name = banner_name)
    }

    numFmt <- paste0("0", if (digits > 0) paste0(".", paste0(rep(0, digits), collapse = "")))
    numFmtProp <- paste0(numFmt, if (proportions) "%")
    styles = list(
      name = openxlsx::createStyle(textDecoration = "bold", wrapText = labels_wrap$name),
      filtertext = openxlsx::createStyle(textDecoration = "italic", wrapText = labels_wrap$description),
      description = openxlsx::createStyle(wrapText = labels_wrap$description),
      body = openxlsx::createStyle(numFmt = numFmtProp, halign = if (!reduce_format) "center"),
      body_grey = openxlsx::createStyle(numFmt = numFmtProp, halign = if (!reduce_format) "center", fontColour = "lightgrey"),
      body_text = openxlsx::createStyle(halign = "right"),
      labels = openxlsx::createStyle(textDecoration = "bold", halign = "center", wrapText = labels_wrap$banner_labels),
      row_labels = openxlsx::createStyle(halign = "right", wrapText = labels_wrap$row_labels),
      categories = openxlsx::createStyle(halign = if (reduce_format) "right" else "center", wrapText = labels_wrap$column_categories),
      categories_border = openxlsx::createStyle(halign = if (reduce_format) "right" else "center", border = "Bottom", wrapText = labels_wrap$column_categories),
      n_weighted = openxlsx::createStyle(numFmt = numFmt, textDecoration = "italic", halign = "center"),
      n_unweighted = openxlsx::createStyle(textDecoration = "italic", halign = "center"),
      n_border = openxlsx::createStyle(numFmt = numFmt, textDecoration = "italic", halign = "center", border = "Bottom"),
      border_bottom = openxlsx::createStyle(border = "Bottom"),
      border_top = openxlsx::createStyle(border = "Top"),
      toc_title = openxlsx::createStyle(fontSize = 14, textDecoration = "bold"),
      toc_slot = openxlsx::createStyle(fontColour = "black", textDecoration = "underline"),
      # toc_lot = openxlsx::createStyle(fontColour = "black", textDecoration = "underline"),
      toc_banner = openxlsx::createStyle(textDecoration = "bold")
    )

    writeReportGeneral(data_summary, banner, filename = filename, proportions = proportions, digits = digits,
        title = title, subtitle = subtitle, return_data = return_data, table_of_contents = table_of_contents,
        report_desc = report_desc, font = font, show_grid_lines = show_grid_lines,
        unweighted_n = unweighted_n, weighted_n = weighted_n,
        font_size = font_size, show_totals = show_totals, banner_vars_split = banner_vars_split,
        append_text = append_text, min_base_size = min_base_size, min_base_label = min_base_label,
        one_per_sheet = one_per_sheet, row_label_width = row_label_width, styles = styles, logo = logo,
        show_description = show_description, logging = logging, first_active_col = first_active_col,
        reduce_format = reduce_format)

}

create_table_of_contents <- function(wb, title, subtitle, toc_row = 2, toc_col = 2,
                                     report_desc = NULL, styles = NULL, show_grid_lines = FALSE, logo = NULL) {

    toc_sheet <- "Table of Contents"
    openxlsx::addWorksheet(wb, toc_sheet, gridLines = show_grid_lines)

    openxlsx::writeData(wb, toc_sheet, title, startCol = toc_col, startRow = toc_row)
    openxlsx::addStyle(wb, toc_sheet, styles$toc_title, rows = toc_row, cols = toc_col)
    toc_row <- toc_row + 1
    if (!is.null(subtitle)) {
      openxlsx::writeData(wb, toc_sheet, subtitle, startCol = toc_col, startRow = toc_row)
      toc_row <- toc_row + 1
    }

    toc_row <- toc_row + 1
    desc_names <- names(report_desc)
    for (desc_num in seq_along(report_desc)) {
      openxlsx::writeData(wb, toc_sheet, desc_names[desc_num], startCol = toc_col, startRow = toc_row)
      openxlsx::writeData(wb, toc_sheet, report_desc[[desc_num]], startCol = toc_col + 2, startRow = toc_row)
      toc_row <- toc_row + 1
    }

    openxlsx::freezePane(wb, toc_sheet, firstActiveRow = toc_row)

    toc_row <- toc_row + 1
    openxlsx::writeData(wb, toc_sheet, "List of tables:", startCol = toc_col, startRow = toc_row)
    # openxlsx::addStyle(wb, toc_sheet, styles$toc_title, rows = toc_row, cols = toc_col, stack = FALSE)
    toc_row <- toc_row + 1

    if (!is.null(logo)) {
      openxlsx::insertImage(wb, toc_sheet, file = logo$file,
                            startRow = if (is.null(logo$startRow)) 2 else logo$startRow,
                            startCol = if (is.null(logo$startCol)) 6 else logo$startCol,
                            width = if (is.null(logo$width)) 4 else logo$width,
                            height = if (is.null(logo$height)) 2 else logo$height,
                            units = if (is.null(logo$units)) "in" else logo$units,
                            dpi = if (is.null(logo$dpi)) 300 else logo$dpi)
    }

    list(toc_sheet = toc_sheet, toc_row = toc_row, toc_col = 2)
}


create_banner_pane <- function(wb, ws, banner, styles, banner_vars_split = NULL, start_row = 1) {
  empty_col <- if (!is.null(banner_vars_split) && banner_vars_split == "empty_col") 1 else 0
  multicols <- sapply(banner, getNames)
  data <- as.data.frame(lapply(seq_along(banner), function(bv) {
    rbind(
      c(getName(banner[[bv]]), if (length(multicols[[bv]]) > 1) rep("", times = length(multicols[[bv]]) - 1 + empty_col)),
      c(multicols[[bv]], if (empty_col == 1) ""))
  }))
  openxlsx::writeData(wb, ws, data, startCol = 2, startRow = start_row, colNames = FALSE)
  multicols_csum <- cumsum(c(2, sapply(multicols, function(x) {length(x) + empty_col})))
  lapply(seq_along(multicols), function(bv) {
    openxlsx::mergeCells(wb, ws, cols = multicols_csum[bv]:(multicols_csum[bv + 1] -
                                                              1 - empty_col), rows = start_row)})
  openxlsx::addStyle(wb, ws, styles$labels, rows = start_row,
                     cols = multicols_csum[[1]]:(multicols_csum[[length(multicols_csum)]] -
                                                                                     1 - empty_col), stack = FALSE)
  openxlsx::addStyle(wb, ws, styles$categories, rows = start_row + 1,
                     cols = multicols_csum[[1]]:(multicols_csum[[length(multicols_csum)]] -
                                                   1 - empty_col), stack = FALSE)
  return(start_row + 1)
}


#' @importFrom stats setNames
writeExcelVarBanner <- function(wb, ws, banner_name, cross_tab_var, banner, start_col = 1, start_row = 1,
    digits = 0, proportions = TRUE, show_totals = TRUE, unweighted_n = NULL, weighted_n = NULL,
    row_label_width = 20, toc_sheet = NULL, toc_row = 1, toc_col = 1, styles = NULL,
    banner_vars_split = NULL, show_description = FALSE, min_base_size = NULL, min_base_label = NULL,
    reduce_format = FALSE) {

  show_totals <- !cross_tab_var$options$no_totals & show_totals
  start_row <- writeVarHeader(wb, ws, cross_tab_var, start_col = start_col, start_row = start_row, toc_sheet = toc_sheet,
                             toc_row = toc_row, toc_col = toc_col, styles = styles, show_description = show_description)

  weighted_n_top <- !is.null(weighted_n) && (weighted_n$position == "top" || weighted_n$position == "both")
  unweighted_n_top <- !is.null(unweighted_n) && (unweighted_n$position == "top" || unweighted_n$position == "both")
  weighted_n_bottom <- !is.null(weighted_n) && (weighted_n$position == "bottom" || weighted_n$position == "both")
  unweighted_n_bottom <- !is.null(unweighted_n) && (unweighted_n$position == "bottom" || unweighted_n$position == "both")
  empty_col <- !is.null(banner_vars_split) && banner_vars_split == "empty_col"

  row_names <- c(
    if (weighted_n_top) weighted_n$name,
    if (unweighted_n_top) unweighted_n$name,
    rownames(getResults(cross_tab_var$crosstabs[[banner_name]][[1]])),
    if (show_totals) "Totals",
    if (weighted_n_bottom) weighted_n$name,
    if (unweighted_n_bottom) unweighted_n$name
  )

  openxlsx::writeData(wb, ws, row_names, startCol = start_col, startRow = start_row,
                      colNames = FALSE, headerStyle = styles$categories_border)
  openxlsx::addStyle(wb, ws, styles$row_labels, rows = start_row:(start_row + length(row_names)),
                     cols = start_col, stack = FALSE)

  start_col <- start_col + 1

  unweighted_n_data <- as.data.frame(lapply(cross_tab_var$crosstabs[[banner_name]], function(x) {
    d <- as.data.frame(x$unweighted_n)
    if (empty_col) cbind(d,  "") else d
    }))

  last_col_num <- start_col + ncol(unweighted_n_data) - 1 - empty_col

  if (!is.null(weighted_n)) {
    weighted_n_data <- as.data.frame(lapply(cross_tab_var$crosstabs[[banner_name]], function(x) {
      d <- as.data.frame(if (reduce_format) round(x$totals_counts, digits) else x$totals_counts)
      if (empty_col) cbind(d,  "") else d
      }))
  }

  if (weighted_n_top) {
    openxlsx::writeData(wb, ws, weighted_n_data, startCol = start_col, startRow = start_row,
                                        colNames = FALSE)
    if (!reduce_format) {
      openxlsx::addStyle(wb, ws, styles$n_weighted, rows = start_row,
                         cols = start_col:last_col_num, stack = FALSE)
    }
    start_row <-start_row + 1
  }

  if (unweighted_n_top) {
    openxlsx::writeData(wb, ws, unweighted_n_data, startCol = start_col, startRow = start_row,
                        colNames = FALSE)
    if (!reduce_format) {
      openxlsx::addStyle(wb, ws, styles$n_unweighted, rows = start_row,
                         cols = start_col:last_col_num, stack = FALSE)
    }
    start_row <-start_row + 1
  }

  if (weighted_n_top || unweighted_n_top) {
    openxlsx::addStyle(wb, ws, styles$border_bottom, rows = start_row - 1,
                       cols = start_col:last_col_num, stack = !reduce_format)
  }

  data <- as.data.frame(lapply(cross_tab_var$crosstabs[[banner_name]], function(x) {
      d <- getResults(x, proportions = proportions)
      # if (is.vector(d)) d <- t(d)
      d <- as.data.frame(d)
      if (show_totals) {
        d <- rbind(d, setNames(as.data.frame(if (proportions) x$totals_proportions else x$totals_counts), colnames(d)))
      }
      if (empty_col) cbind(d, "" ) else d
    }))

  min_cell_mask <- NULL
  if (!is.null(min_base_size)) {
    min_cell_mask <- unweighted_n_data != "" & unweighted_n_data < min_base_size
  }
  if (any(min_cell_mask) && !is.null(min_base_label)) {
    data[, min_cell_mask] <- min_base_label
  }

  openxlsx::writeData(wb, ws, data, startCol = start_col, startRow = start_row,
                                    colNames = FALSE)
  openxlsx::addStyle(wb, ws, styles$body, rows = start_row:(start_row + nrow(data) - 1),
                     cols = start_col:last_col_num, gridExpand = TRUE, stack = FALSE)
  if (any(min_cell_mask)) {
      if (is.null(min_base_label)) {
        openxlsx::addStyle(wb, ws, styles$body_grey, rows = start_row:(start_row + nrow(data) - 1),
                           cols = start_col + which(min_cell_mask) - 1, gridExpand = TRUE, stack = FALSE)
      }
      if (reduce_format) {
        openxlsx::addStyle(wb, ws, styles$body_text, rows = start_row:(start_row + nrow(data) - 1),
                           cols = start_col + which(min_cell_mask) - 1, gridExpand = TRUE, stack = FALSE)
      }
  }
  start_row <-start_row + nrow(data)

  if (unweighted_n_bottom || weighted_n_bottom || show_totals) {
    openxlsx::addStyle(wb, ws, styles$border_bottom, rows = start_row - 1 - show_totals,
                       cols = start_col:last_col_num, stack = TRUE)
  }

  # if (show_totals) {
  #   data <- as.data.frame(lapply(cross_tab_var$crosstabs[[banner_name]], function(x) {
  #     d <- as.data.frame(t(if (proportions) x$totals_proportions else x$totals_counts))
  #     if (nrow(d)) c(d, if (empty_col) "")}))
  #   if (any(min_cell_mask) && !is.null(min_base_label)) {
  #     data[, min_cell_mask] <- min_base_label
  #   }
  #   openxlsx::writeData(wb, ws, data, startCol = start_col, startRow = start_row, colNames = FALSE)
  #     openxlsx::addStyle(wb, ws, styles$body, rows = start_row,
  #                        cols = start_col:last_col_num, stack = FALSE)
  #   if (any(min_cell_mask)) {
  #     if (is.null(min_base_label)) {
  #       openxlsx::addStyle(wb, ws, styles$body_grey, rows = start_row,
  #                          cols = start_col + which(min_cell_mask) - 1, gridExpand = FALSE, stack = FALSE)
  #     }
  #     if (reduce_format) {
  #       openxlsx::addStyle(wb, ws, styles$body_text, rows = start_row,
  #                          cols = start_col + which(min_cell_mask) - 1, gridExpand = FALSE, stack = FALSE)
  #     }
  #   }
  #   start_row <-start_row + nrow(data)
  # }

  if (weighted_n_bottom) {
    openxlsx::writeData(wb, ws, weighted_n_data, startCol = start_col, startRow = start_row,
                        colNames = FALSE)
    if (!reduce_format) {
      openxlsx::addStyle(wb, ws, styles$n_weighted, rows = start_row,
                         cols = start_col:last_col_num, stack = FALSE)
    }
    start_row <-start_row + 1
  }

  if (unweighted_n_bottom) {
    openxlsx::writeData(wb, ws, unweighted_n_data, startCol = start_col, startRow = start_row,
                        colNames = FALSE)
    if (!reduce_format) {
      openxlsx::addStyle(wb, ws, styles$n_unweighted, rows = start_row,
                         cols = start_col:last_col_num, stack = FALSE)
    }
    start_row <-start_row + 1
  }
  return(start_row)

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
    toc_col = 1, styles = NULL, show_description = TRUE) {
    crow <- writeVarHeader(wb, ws, x, start_col = start_col, start_row = start_row,
        toc_sheet = toc_sheet, toc_row = toc_row, toc_col = toc_col, styles = styles,
        show_description = show_description)
    crow <- crow + 1
    drows <- writeExcelVarTopline(wb, ws, x, start_col = start_col, start_row = crow,
        digits = digits, proportions = proportions, styles = styles)
    openxlsx::setColWidths(wb, ws, 1, row_label_width)
    return(crow + drows + 1)
}


writeVarHeader <- function(wb, ws, x, start_col = 1, start_row = 1, toc_sheet = NULL,
    toc_row = 1, toc_col = 1, styles = NULL, show_description = FALSE) {
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
    if (show_description) {
      var_description <- getDescription(x)
      if (!is.null(var_description) && var_description != "") {
        openxlsx::writeData(wb, ws, var_description, startCol = ccol, startRow = crow)
        openxlsx::addStyle(wb, ws, styles$description, rows = crow, cols = ccol)
        crow <- crow + 1
      }
      filtertext <- getNotes(x)
      if (!is.null(filtertext) && filtertext != "") {
          openxlsx::writeData(wb, ws, filtertext, startCol = ccol, startRow = crow)
          openxlsx::addStyle(wb, ws, styles$filtertext, rows = crow, cols = ccol)
          crow <- crow + 1
      }
    }
    crow
}

writeReportGeneral <- function(x, banner = NULL, filename = NULL, proportions = TRUE,
    digits = 0, title = "", subtitle = NULL, return_data = TRUE, table_of_contents = FALSE,
    report_desc = NULL, font = "Calibri", font_size = 12, show_grid_lines = FALSE,
    unweighted_n = NULL, weighted_n = NULL, banner_vars_split = NULL,
    show_totals = TRUE, append_text = "", min_base_size = NULL,
    min_base_label = NULL, one_per_sheet = TRUE, row_label_width = 20, styles = NULL,
    logo = NULL, show_description = FALSE, logging = FALSE, first_active_col = 2,
    reduce_format = FALSE) {

    if (logging) {
      start.time.wb <- Sys.time()
      print(paste(start.time.wb, "-- workbook generation -- start"))
    }

    wb <- openxlsx::createWorkbook()
    openxlsx::modifyBaseFont(wb, fontSize = font_size, fontColour = "black", fontName = font)

    toc_sheet <- NULL
    toc_row <- 2
    toc_col <- 2
    banner_names <- if (is.null(banner)) "Results" else names(banner)
    if (table_of_contents) {
      toc_res <- create_table_of_contents(wb, title = title, subtitle = subtitle, toc_row = toc_row, toc_col = toc_col,
                                          report_desc = report_desc, show_grid_lines = show_grid_lines, styles = styles, logo = logo)
      toc_sheet <- toc_res$toc_sheet
      toc_row <- toc_res$toc_row
      toc_col <- toc_res$toc_col
    }

    for (banner_name in banner_names) {
      if (logging) {
        start.time <- Sys.time()
        print(paste(start.time, "-- banner generation: ", banner_name, "--", "start"))
      }
      start_col <- 1
      last_row_used <- 1
      if (!one_per_sheet) {
        worksheet_name <- banner_name
        openxlsx::addWorksheet(wb, banner_name, gridLines = show_grid_lines)
        openxlsx::setColWidths(wb, banner_name, start_col, row_label_width)
        if (!is.null(banner)) {
          last_row_used <- last_row_used +
             create_banner_pane(wb, banner_name, banner = banner[[banner_name]],
                             styles = styles, banner_vars_split = banner_vars_split, start_row = last_row_used)
          openxlsx::freezePane(wb, banner_name, firstActiveRow = last_row_used, firstActiveCol = first_active_col)
          if (table_of_contents) {
            toc_col <- toc_res$toc_col
            openxlsx::writeData(wb, toc_sheet, worksheet_name, startRow = toc_row, startCol = toc_col)
            openxlsx::addStyle(wb, toc_sheet, styles$toc_banner, rows = toc_row, cols = toc_col, stack = FALSE)
            toc_row <- toc_row + 1
            toc_col <- toc_col + 1
          }
        }
      }

      for (vidx in seq_along(x$results)) {
          if (one_per_sheet) {
              last_row_used <- 1
              worksheet_name <- getAlias(x$results[[vidx]])
              # worksheet names must be unique and have less then 32 characters
              if (nchar(worksheet_name) > 25) {
                  worksheet_name <- paste0(strtrim(worksheet_name, 25), vidx)
              }
              openxlsx::addWorksheet(wb, worksheet_name, gridLines = show_grid_lines)
              openxlsx::setColWidths(wb, worksheet_name, start_col, row_label_width)
              if (!is.null(banner)) {
                last_row_used <- create_banner_pane(wb, worksheet_name, banner = banner[[banner_name]],
                                   styles = styles, banner_vars_split = banner_vars_split, start_row = 1)
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
                      toc_col = toc_col, styles = styles, show_description = show_description)
              } else {
                  writeExcelVarBanner(wb, worksheet_name, banner_name, x$results[[vidx]], banner,
                      start_col = start_col, start_row = last_row_used, digits = digits, proportions = proportions,
                      unweighted_n = unweighted_n, weighted_n = weighted_n, show_totals = show_totals,
                      row_label_width = row_label_width, toc_sheet = toc_sheet, banner_vars_split = banner_vars_split,
                      toc_row = toc_row, toc_col = toc_col, styles = styles,
                      min_base_size = min_base_size, min_base_label = min_base_label,
                      show_description = show_description, reduce_format = reduce_format)
              }
          toc_row <- toc_row + 1
      }
      if (logging) {
        end.time <- Sys.time()
        print(paste(end.time, "-- banner generation: ", banner_name, "-- end -- elapsed: ", round(difftime(end.time, start.time, units = "mins"), 2), "mins"))
      }
    }
    if (append_text != "") {
        worksheet_name <- "Notes"
        openxlsx::addWorksheet(wb, worksheet_name, gridLines = show_grid_lines)
        openxlsx::writeData(wb, worksheet_name, append_text, startCol = 1, startRow = 1)
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
        return(x)
    }
 }
