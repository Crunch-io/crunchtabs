
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
#' list(`Fieldwork date` = "2017-01-01", `Sample description` = "description").
#' Defaults to \code{NULL}.
#' @param digits integer. The number of decimal places that should be used for
#' rounding numbers. Defaults to \code{0}.
#' @param append_text A text that, if supplied, will be appended after
#' the final table. Useful for adding in disclosure information.
#' @param min_base_size integer. The minimum number of responses for a cross tabulated
#' category to be displayed in details in a banner report.
#' @param min_base_label list. Specifies how cross-tabulated results that are
#' less than \code{min_base_size} should be formatted. Valid fields are:
#' \itemize{
#'  \item mask - a string that should be used to mask the results or \code{NULL} if the numbers should be printed.
#'  \item decoration - text styling. Valid values are: "bold", "strikeout", "italic", "underline", "underline2", NULL (no decoration).
#'  \item color - a string specifying the font color or NULL (a default color is used).
#' }
#' Defaults to \code{list(mask = "*", decoration = NULL, color = NULL)}.
#' @param show_totals logical. If \code{TRUE}, the row containing column sums is displayed.
#' Defaults to \code{TRUE}.
#' @param banner_format list. Specify format details:
#' \itemize{
#'  \item decoration - text styling. Valid values are: "bold", "strikeout", "italic", "underline", "underline2", NULL (no decoration).
#'  \item size - font size.
#'  \item color - a string specifying the font color or NULL (a default color is used).
#' }
#' related to banner's:
#' \itemize{
#'  \item labels,
#'  \item categories,
#' }
#' Defaults to list(labels=list(decoration="bold", size=font_size),
#' categories=list(decoration="bold", size=font_size)).
#' @param show_information list. Specify format details:
#' \itemize{
#'  \item decoration - text styling,
#'  \item size - font size
#' }
#' related to cross-tabulated variable's:
#' \itemize{
#'  \item name,
#'  \item description,
#'  \item filtertext.
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
#' Defaults to list(name=list(decoration="bold", size=font_size),
#' description=list(decoration=NULL, size=font_size),
#' filtertext=list(decoration="italic", size=font_size)).
#' @param unweighted_n A list of parameters describing the row containing the unweighted bases:
#' \itemize{
#'  \item name - row label.
#'  \item position - row position. Valid values are: "top", "bottom", "both".
#'  \item decoration - text styling. Valid values are: "bold", "strikeout", "italic", "underline", "underline2", NULL (no decoration).
#'  \item size - font size.
#'  \item color - a string specifying the font color or NULL (a default color is used).
#' }
#' Decoration, size and color options are applied only when \code{reduce_format = FALSE}.
#' Defaults to list(name = "Unweighted N", position = "bottom",
#' decoration = NULL, size = font_size, color = NULL).
#' @param weighted_n A list of parameters describing the row containing the weighted bases:
#' \itemize{
#'  \item name - row label.
#'  \item position - row position. Valid values are: "top", "bottom", "both".
#'  \item decoration - text styling. Valid values are: "bold", "strikeout", "italic", "underline", "underline2", NULL (no decoratio).
#'  \item size - font size.
#'  \item color - a string specifying the font color or NULL (a default color is used).
#' }
#' Decoration, size and color options are applied only when \code{reduce_format = FALSE}.
#' Defaults to \code{NULL} - the row containing the weighted bases is not printed.
#' @param total_col A list of parameters describing the Total" column:
#' \itemize{
#'  \item decoration - text styling. Valid values are: "bold", "strikeout", "italic", "underline", "underline2", NULL (no decoratio).
#'  \item size - font size.
#'  \item color - font color.
#' }
#' Decoration, size and color options are applied only when \code{reduce_format = FALSE}.
#' Defaults to \code{NULL} - default text style used.
#' @param show_grid_lines logical. Should grid lines be shown?
#' Defaults to \code{FALSE}.
#' @param banner_vars_split the method of splitting banner variables.
#' Valid values are: {NULL, "empty_col", "line"}.
#' Defaults to \code{NULL} - no split.
#' @param row_label_width width of the first column. Defaults to 30pt.
#' @param row_label_alignment alignment of the first column.
#' Valid values are: "left", "right", "center", NULL (a default alignemt).
#' Defaults to NULL.
#' @param labels_wrap list. Specifies which labels should be wrapped:
#' \itemize{
#'  \item name - variable's name.
#'  \item description - variable's description.
#'  \item row_labels - row labels.
#'  \item banner_labels - banner labels
#'  \item column_categories - column categories.
#' }
#' Defaults to list(name = TRUE, description = TRUE,
#' row_labels = TRUE, banner_labels = TRUE, column_categories = TRUE).
#' @param first_active_col number. First active coulumn. Defaults to \code{2}.
#' @param logging logical. Should basic information related to the function execution
#' be printed? Defaults to \code{FALSE}.
#' @param one_per_sheet logical. Should every variable be written on a separate sheet?
#' Defaults to \code{TRUE}.
#' @param include_aliases logical. Should the variable's alias be concatenated with
#'  the variable's name? Defaults to \code{FALSE}.
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
#' @param banner_border_lines logical. Should black border lines be shown around the banner?
#' Defaults to \code{FALSE}.
#' @param title_on_results_page logical. Should title and subtitle be printed on the results page?
#' Defaults to \code{FALSE}.
#' @param percent_format_data logical. Should data when \code{proportions = TRUE}
#' be formatted as "Percentage"? If not, a row with percent signs is added to the banner.
#' Defaults to \code{TRUE}.
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
                       weighted_n = NULL,
                       unweighted_n = list(name = "Unweighted N", position = "bottom", decoration = NULL, size = font_size, color = NULL),
                       show_totals = TRUE, report_desc = NULL, font = "Calibri", font_size = 12,
                       one_per_sheet = TRUE, append_text = "", row_label_alignment = NULL,
                       banner_vars_split = NULL, row_label_width = 30, reduce_format = FALSE,
                       min_base_size = NULL, min_base_label = list(mask = "*", decoration = NULL, color = NULL),
                       show_information = list(name = list(decoration="bold", size=font_size),
                                               description=list(decoration=NULL, size=font_size),
                                               filtertext=list(decoration="italic", size=font_size)),
                       show_grid_lines = FALSE, return_data = TRUE, logging = FALSE,
                       labels_wrap = list(name = TRUE, description = TRUE,
                            row_labels = TRUE, banner_labels = TRUE, column_categories = TRUE),
                       first_active_col = 2, include_aliases = FALSE, banner_border_lines = FALSE,
                       title_on_results_page = FALSE, percent_format_data = TRUE,
                       total_col = NULL, banner_format = list(labels = list(decoration="bold", size=font_size), categories = list(decoration = "bold", size = font_size))) {

  if (is.null(filename)) {
    stop("No valid filename provided.")
  }
  UseMethod("writeExcel", data_summary)
}

#' @export
writeExcel.Toplines <- function(data_summary, filename = NULL, title = getName(data_summary), subtitle = NULL,
                                proportions = FALSE, digits = 0, table_of_contents = FALSE, logo = NULL,
                                weighted_n = NULL,
                                unweighted_n = list(name = "Unweighted N", position = "bottom", decoration = NULL, size = font_size, color = NULL),
                                show_totals = TRUE, report_desc = NULL, font = "Calibri", font_size = 12,
                                one_per_sheet = TRUE, append_text = "", row_label_alignment = NULL,
                                banner_vars_split = NULL, row_label_width = 30, reduce_format = FALSE,
                                min_base_size = NULL, min_base_label = list(mask = "*", decoration = NULL, color = NULL),
                                show_information = list(name = list(decoration="bold", size=font_size),
                                                        description=list(decoration=NULL, size=font_size),
                                                        filtertext=list(decoration="italic", size=font_size)),
                                show_grid_lines = FALSE, return_data = TRUE, logging = FALSE,
                                labels_wrap = list(name = TRUE, description = TRUE,
                                    row_labels = TRUE, banner_labels = TRUE, column_categories = TRUE),
                                first_active_col = 2, include_aliases = FALSE, banner_border_lines = FALSE,
                                title_on_results_page = FALSE, percent_format_data = TRUE,
                                total_col = NULL, banner_format = list(labels = list(decoration="bold", size=font_size), categories = list(decoration = "bold", size = font_size))) {

  data_summary$results <- lapply(data_summary$results, function(var_data) {
        var_data$data <- reformatResults(var_data, proportions = proportions, digits = digits,
            reformat = FALSE)
        var_data
    })

    styles = list(
      name = openxlsx::createStyle(textDecoration = "bold", wrapText = labels_wrap$name),
      description = openxlsx::createStyle(),
      filtertext = openxlsx::createStyle(textDecoration = "italic", wrapText = labels_wrap$description),
      numeric = openxlsx::createStyle(numFmt = paste0("0", if (digits > 0)
        paste0(".", paste0(rep(0, digits), collapse = "")))),
      categorical = openxlsx::createStyle(numFmt = paste0("0", if (digits > 0)
        paste0(".", paste0(rep(0, digits), collapse = "")), if (proportions)
          "%"), halign = "center"),
      categories = openxlsx::createStyle(halign = "center", wrapText = labels_wrap$column_categories),
      row_labels = openxlsx::createStyle(wrapText = labels_wrap$row_labels),
      toc_title = openxlsx::createStyle(fontSize = 14, textDecoration = "bold"),
      toc_subtitle = openxlsx::createStyle(fontSize = 12, textDecoration = "bold"),
      toc_slot = openxlsx::createStyle(fontColour = "black", textDecoration = "underline"),
      title = openxlsx::createStyle(fontSize = 14, textDecoration = "bold"),
      subtitle = openxlsx::createStyle(textDecoration = "bold"),
      desc = openxlsx::createStyle()
    )

    writeReportGeneral(data_summary, filename = filename, proportions = proportions, digits = digits,
        title = title, subtitle = subtitle, return_data = return_data, table_of_contents = table_of_contents,
        report_desc = report_desc, font = font, show_grid_lines = show_grid_lines,
        font_size = font_size, show_totals = show_totals, logging = logging,
        append_text = append_text, min_base_size = min_base_size, min_base_label = min_base_label,
        one_per_sheet = one_per_sheet, row_label_width = row_label_width, styles = styles, logo = logo,
        show_information = show_information, include_aliases = include_aliases)
}

#' @export
writeExcel.Crosstabs <- function(data_summary, filename = NULL, title = getName(data_summary), subtitle = NULL,
                                 proportions = FALSE, digits = 0, table_of_contents = FALSE, logo = NULL,
                                 weighted_n = NULL,
                                 unweighted_n = list(name = "Unweighted N", position = "bottom", decoration = NULL, size = font_size, color = NULL),
                                 show_totals = TRUE, report_desc = NULL, font = "Calibri", font_size = 12,
                                 one_per_sheet = TRUE, append_text = "", row_label_alignment = NULL,
                                 banner_vars_split = NULL, row_label_width = 30, reduce_format = FALSE,
                                 min_base_size = NULL, min_base_label = list(mask = "*", decoration = NULL, color = NULL),
                                 show_information = list(name = list(decoration="bold", size=font_size),
                                                         description=list(decoration=NULL, size=font_size),
                                                         filtertext=list(decoration="italic", size=font_size)),
                                 show_grid_lines = FALSE, return_data = TRUE, logging = FALSE,
                                 labels_wrap = list(name = TRUE, description = TRUE,
                                      row_labels = TRUE, banner_labels = TRUE, column_categories = TRUE),
                                 first_active_col = 2, include_aliases = FALSE, banner_border_lines = FALSE,
                                 title_on_results_page = FALSE, percent_format_data = TRUE,
                                 total_col = NULL, banner_format = list(labels = list(decoration="bold", size=font_size), categories = list(decoration = "bold", size = font_size))) {

    banner <- data_summary$banner
    # data_summary$results <- reformatCrosstabsResults(data_summary$results, banner, proportions = proportions, digits = digits,
    # add_parenthesis = FALSE, show_totals = show_totals, weighted_n = weighted_n,
    # min_base_size = min_base_size, min_base_label = min_base_label, reformat = FALSE)
    if (one_per_sheet && length(banner) > 1) {
      banner_name <- "Results"
      data_summary$results <- mergeBannerResults(data_summary$results, banner_name = banner_name)
      banner <- mergeBanner(banner, banner_name = banner_name)
    }

    # banner_vars_split_line <- banner_vars_split == "line"

    get_format_info <- function(format_data, info_name, elem) {
      if (!is.null(format_data) && info_name %in% names(format_data)) format_data[[info_name]][[elem]]
    }
    get_decoration_data <- function(data_info, elem) {
      if (!is.null(data_info)) data_info[[elem]]
    }
    numFmt <- paste0("0", if (digits > 0) paste0(".", paste0(rep(0, digits), collapse = "")))
    numFmtProp <- paste0(numFmt, if (proportions && percent_format_data) "%")
    styles = list(
      name = openxlsx::createStyle(textDecoration = get_format_info(show_information, "name", "decoration"), fontSize = get_format_info(show_information, "name", "size"), wrapText = labels_wrap$name, halign = row_label_alignment),
      filtertext = openxlsx::createStyle(textDecoration = get_format_info(show_information, "filtertext", "decoration"), fontSize = get_format_info(show_information, "filtertext", "size"), wrapText = labels_wrap$description, halign = row_label_alignment),
      description = openxlsx::createStyle(textDecoration = get_format_info(show_information, "description", "decoration"), fontSize = get_format_info(show_information, "description", "size"), wrapText = labels_wrap$description, halign = row_label_alignment),
      body = openxlsx::createStyle(numFmt = if (proportions) numFmtProp else numFmt, halign = if (!reduce_format) "center"),
      body_min_base = openxlsx::createStyle(numFmt =  if (proportions) numFmtProp else numFmt, halign = if (reduce_format) "right" else "center", textDecoration = get_decoration_data(min_base_label, "decoration"), fontColour = get_decoration_data(min_base_label, "color")),
      body_text = openxlsx::createStyle(halign = if (reduce_format) "right" else "center"),
      labels = openxlsx::createStyle(textDecoration = get_format_info(banner_format, "labels", "decoration"), fontSize = get_format_info(banner_format, "labels", "size"), fontColour = get_format_info(banner_format, "labels", "color"), halign = "center", wrapText = labels_wrap$banner_labels, border = if (banner_border_lines) "TopBottomLeftRight"),
      row_labels = openxlsx::createStyle(halign = if (!is.null(row_label_alignment)) row_label_alignment else "right", wrapText = labels_wrap$row_labels),
      row_n_weighted = openxlsx::createStyle(halign = if (!is.null(row_label_alignment)) row_label_alignment else "right", wrapText = labels_wrap$row_labels, textDecoration = get_decoration_data(weighted_n, "decoration"), fontSize = get_decoration_data(weighted_n, "size"), fontColour = get_decoration_data(weighted_n, "color")),
      row_n_unweighted = openxlsx::createStyle(halign = if (!is.null(row_label_alignment)) row_label_alignment else "right", wrapText = labels_wrap$row_labels, textDecoration = get_decoration_data(unweighted_n, "decoration"), fontSize = get_decoration_data(unweighted_n, "size"), fontColour = get_decoration_data(unweighted_n, "color")),
      categories = openxlsx::createStyle(halign = if (reduce_format) "right" else "center", wrapText = labels_wrap$column_categories, border = if (banner_border_lines) "TopBottomLeftRight", textDecoration = get_format_info(banner_format, "categories", "decoration"), fontSize = get_format_info(banner_format, "categories", "size"), fontColour = get_format_info(banner_format, "categories", "color")),
      n_weighted = openxlsx::createStyle(numFmt = numFmt, textDecoration = get_decoration_data(weighted_n, "decoration"), fontSize = get_decoration_data(weighted_n, "size"), fontColour = get_decoration_data(weighted_n, "color"), halign = "center"),
      n_unweighted = openxlsx::createStyle(textDecoration = get_decoration_data(unweighted_n, "decoration"), fontSize = get_decoration_data(unweighted_n, "size"), fontColour = get_decoration_data(unweighted_n, "color"), halign = "center"),
      border_bottom = openxlsx::createStyle(border = "Bottom"),
      border_top = openxlsx::createStyle(border = "Top"),
      border_right = openxlsx::createStyle(border = "Right"),
      toc_title = openxlsx::createStyle(fontSize = 14, textDecoration = "bold"),
      toc_subtitle = openxlsx::createStyle(textDecoration = "bold"),
      toc_slot = openxlsx::createStyle(fontColour = "black", textDecoration = "underline"),
      title = openxlsx::createStyle(fontSize = 14, textDecoration = "bold", halign = row_label_alignment),
      subtitle = openxlsx::createStyle(textDecoration = "bold", halign = row_label_alignment),
      desc = openxlsx::createStyle(halign = row_label_alignment),
      # toc_lot = openxlsx::createStyle(fontColour = "black", textDecoration = "underline"),
      toc_banner = openxlsx::createStyle(textDecoration = "bold"),
      total_col = if (!is.null(total_col)) openxlsx::createStyle(textDecoration = get_decoration_data(total_col, "decoration"), fontSize = get_decoration_data(total_col, "size"), fontColour = get_decoration_data(total_col, "color"))
    )

    writeReportGeneral(data_summary, banner, filename = filename, proportions = proportions, digits = digits,
        title = title, subtitle = subtitle, return_data = return_data, table_of_contents = table_of_contents,
        report_desc = report_desc, font = font, show_grid_lines = show_grid_lines,
        unweighted_n = unweighted_n, weighted_n = weighted_n,
        font_size = font_size, show_totals = show_totals, banner_vars_split = banner_vars_split,
        append_text = append_text, min_base_size = min_base_size, min_base_label = min_base_label,
        one_per_sheet = one_per_sheet, row_label_width = row_label_width, styles = styles, logo = logo,
        show_information = show_information, logging = logging, first_active_col = first_active_col,
        reduce_format = reduce_format, include_aliases = include_aliases, title_on_results_page = title_on_results_page,
        percent_format_data = percent_format_data)

}

write_report_desc <- function(wb, ws, title, subtitle, start_row = 2, start_col = 2, report_desc = NULL, styles = NULL, toc_page = TRUE) {

  openxlsx::writeData(wb, ws, title, startCol = start_col, startRow = start_row)
  openxlsx::addStyle(wb, ws, if (toc_page) styles$toc_title else styles$title, rows = start_row, cols = start_col)
  start_row <- start_row + 1
  if (!is.null(subtitle)) {
    openxlsx::writeData(wb, ws, subtitle, startCol = start_col, startRow = start_row)
    openxlsx::addStyle(wb, ws, if (toc_page) styles$toc_subtitle else styles$subtitle, rows = start_row, cols = start_col)
    start_row <- start_row + 1
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
                                     report_desc = NULL, styles = NULL, show_grid_lines = FALSE, logo = NULL) {

    toc_sheet <- "Table of Contents"
    openxlsx::addWorksheet(wb, toc_sheet, gridLines = show_grid_lines)

    toc_row <- write_report_desc(wb, toc_sheet, title = title, subtitle = subtitle, start_row = toc_row,
                                 start_col = toc_col, report_desc = report_desc, styles = styles)

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


create_banner_panel <- function(wb, ws, banner, styles, banner_vars_split = NULL, start_row = 1, banner_cols_pos = NULL,
                               title_on_results_page = FALSE, title = NULL, subtitle = NULL, report_desc = NULL,
                               percent_row = FALSE) {

  if (title_on_results_page) {
    start_row <- write_report_desc(wb, ws, title = title, subtitle = subtitle, start_row = start_row,
                                   start_col = 1, report_desc = report_desc, styles = styles, toc_page = FALSE)
  }

  empty_col <- if (!is.null(banner_vars_split) && banner_vars_split == "empty_col") 1 else 0
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
  if (!is.null(banner_vars_split) && banner_vars_split == "line") {
    openxlsx::addStyle(wb, ws, styles$border_right, rows = start_row:(start_row + 1),
                       cols = banner_cols_pos, gridExpand = TRUE, stack = TRUE)
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
  return(start_row + 1)
}

write_bases_data <- function(wb, ws, data, col, row, last_col_num, reduce_format, style_data, style_label) {
  openxlsx::writeData(wb, ws, data, startCol = col, startRow = row,
                      colNames = FALSE)
  if (!reduce_format) {
    openxlsx::addStyle(wb, ws, style_data, rows = row,
                       cols = col:last_col_num, stack = FALSE)
    openxlsx::addStyle(wb, ws, style_label, rows = row,
                       cols = col - 1, stack = FALSE)
  }
  row + 1
}
#' @importFrom stats setNames
writeExcelVarBanner <- function(wb, ws, banner_name, cross_tab_var, banner_cols_pos, start_col = 1, start_row = 1,
    digits = 0, proportions = TRUE, show_totals = TRUE, unweighted_n = NULL, weighted_n = NULL,
    row_label_width = 20, toc_sheet = NULL, toc_row = 1, toc_col = 1, styles = NULL,
    banner_vars_split = NULL, show_information = NULL, min_base_size = NULL, min_base_label = NULL,
    reduce_format = FALSE, include_aliases = FALSE, percent_format_data = TRUE) {

  show_totals <- !cross_tab_var$options$no_totals & show_totals
  start_row <- writeVarHeader(wb, ws, cross_tab_var, start_col = start_col, start_row = start_row, toc_sheet = toc_sheet,
                             toc_row = toc_row, toc_col = toc_col, styles = styles, show_information = show_information,
                             include_aliases = include_aliases)
  crow <- start_row
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

  openxlsx::writeData(wb, ws, row_names, startCol = start_col, startRow = crow,
                      colNames = FALSE)
  openxlsx::addStyle(wb, ws, styles$row_labels, rows = crow:(crow + length(row_names)),
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
    crow <- write_bases_data(wb, ws, data=weighted_n_data, col=start_col, row=crow,
                             last_col_num=last_col_num, reduce_format=reduce_format,
                             style_data=styles$n_weighted, style_label=styles$row_n_weighted)
  }

  if (unweighted_n_top) {
    crow <- write_bases_data(wb, ws, data=unweighted_n_data, col=start_col, row=crow,
                             last_col_num=last_col_num, reduce_format=reduce_format,
                             style_data=styles$n_unweighted, style_label=styles$row_n_unweighted)
  }

  if (weighted_n_top || unweighted_n_top) {
    openxlsx::addStyle(wb, ws, styles$border_bottom, rows = crow - 1,
                       cols = start_col:last_col_num, stack = !reduce_format)
  }

  data <- as.data.frame(lapply(cross_tab_var$crosstabs[[banner_name]], function(x) {
      d <- getResults(x, proportions = proportions)
      # if (is.vector(d)) d <- t(d)
      d <- as.data.frame(d)
      if (show_totals) {
        d <- rbind(d, setNames(as.data.frame(if (proportions) x$totals_proportions else x$totals_counts), colnames(d)))
      }
      if (proportions && !percent_format_data) {
        d[] <- d * 100
      }
      if (empty_col) cbind(d, "" ) else d
    }))

  min_cell_mask <- NULL
  if (!is.null(min_base_size)) {
    unweighted_n_data_num <- sapply(unweighted_n_data, function(x) as.numeric(as.character(x)))
    min_cell_mask <- !is.na(unweighted_n_data_num) & unweighted_n_data_num < min_base_size
  }
  if (any(min_cell_mask) && !is.null(min_base_label) && !is.null(min_base_label[["mask"]])) {
    data[, min_cell_mask] <- min_base_label[["mask"]]
  }

  openxlsx::writeData(wb, ws, data, startCol = start_col, startRow = crow,
                                    colNames = FALSE)
  openxlsx::addStyle(wb, ws, styles$body, rows = crow:(crow + nrow(data) - 1),
                     cols = start_col:last_col_num, gridExpand = TRUE, stack = FALSE)
  if (any(min_cell_mask)) {
      # if (is.null(min_base_label)) {
        openxlsx::addStyle(wb, ws, styles$body_min_base, rows = crow:(crow + nrow(data) - 1),
                           cols = start_col + which(min_cell_mask) - 1, gridExpand = TRUE, stack = FALSE)
      # }
    # else if (reduce_format) {
    #     openxlsx::addStyle(wb, ws, styles$body_text, rows = start_row:(start_row + nrow(data) - 1),
    #                        cols = start_col + which(min_cell_mask) - 1, gridExpand = TRUE, stack = FALSE)
    #   }
  }
  if (!is.null(styles$total_col) && !reduce_format) {
    openxlsx::addStyle(wb, ws, styles$total_col, rows = crow:(crow + nrow(data) - 1),
                       cols = start_col, stack = TRUE)
  }
  crow <- crow + nrow(data)

  if (unweighted_n_bottom || weighted_n_bottom || show_totals) {
    openxlsx::addStyle(wb, ws, styles$border_bottom, rows = crow - 1 - show_totals,
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
  #       openxlsx::addStyle(wb, ws, styles$body_min_base, rows = start_row,
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
    openxlsx::addStyle(wb, ws, styles$border_right, rows = start_row:(crow-1),
                       cols = banner_cols_pos[-length(banner_cols_pos)], gridExpand = TRUE, stack = TRUE)
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
    var_info = list(name = if (include_aliases) paste0(getAlias(x), " - ", getName(x)) else getName(x),
                 description = getDescription(x),
                 filtertext = getNotes(x))
    add_toc_info <- !is.null(toc_sheet)
    for (info_name in names(show_information)) {
      if (info_name %in% names(var_info) && !is.null(var_info[[info_name]]) && var_info[[info_name]] != "") {
        start_row <- write_var_info(wb, ws, var_info = var_info[[info_name]], elem_name = info_name, styles = styles, col = start_col, row = start_row)
        if (add_toc_info) {
          openxlsx::writeFormula(wb, toc_sheet, startCol = toc_col, startRow = toc_row, x = openxlsx::makeHyperlinkString(sheet = ws,
                                                                                                                          row = start_row - 1, col = start_col, text = var_info[[info_name]]))
          openxlsx::addStyle(wb, toc_sheet, styles$toc_slot,
                             rows = toc_row, cols = toc_col, stack = FALSE)
          add_toc_info <- FALSE
        }
      }
    }
    start_row
}

writeReportGeneral <- function(x, banner = NULL, filename = NULL, proportions = TRUE,
    digits = 0, title = "", subtitle = NULL, return_data = TRUE, table_of_contents = FALSE,
    report_desc = NULL, font = "Calibri", font_size = 12, show_grid_lines = FALSE,
    unweighted_n = NULL, weighted_n = NULL, banner_vars_split = NULL,
    show_totals = TRUE, append_text = "", min_base_size = NULL,
    min_base_label = NULL, one_per_sheet = TRUE, row_label_width = 20, styles = NULL,
    logo = NULL, show_information = NULL, logging = FALSE, first_active_col = 2,
    reduce_format = FALSE, include_aliases = FALSE, title_on_results_page = FALSE,
    percent_format_data = TRUE) {

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
      banner_cols_pos <- cumsum(sapply(banner[[banner_name]], function(x) length(x$categories))) + start_col
      if (!one_per_sheet) {
        worksheet_name <- banner_name
        openxlsx::addWorksheet(wb, banner_name, gridLines = show_grid_lines)
        openxlsx::setColWidths(wb, banner_name, start_col, row_label_width)
        if (!is.null(banner)) {
          last_row_used <- last_row_used +
             create_banner_panel(wb, banner_name, banner = banner[[banner_name]],
                             styles = styles, banner_vars_split = banner_vars_split, start_row = last_row_used,
                             banner_cols_pos = banner_cols_pos, title_on_results_page = title_on_results_page,
                             title = title, subtitle = subtitle, report_desc = report_desc,
                             percent_row = !percent_format_data & proportions)
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
                last_row_used <- create_banner_panel(wb, worksheet_name, banner = banner[[banner_name]],
                                   styles = styles, banner_vars_split = banner_vars_split, start_row = 1,
                                   banner_cols_pos = banner_cols_pos, title_on_results_page = title_on_results_page,
                                   title = title, subtitle = subtitle, report_desc = report_desc,
                                   percent_row = !percent_format_data & proportions)
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
                      percent_format_data = percent_format_data)
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
