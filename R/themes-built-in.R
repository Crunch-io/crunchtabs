#' Project-specific themes
#'
#' These are some default themes for specific projects
#'
#' @param logo an image of the logo to use (default: `NULL`)
#'
#' @return a new theme object
#'
#' @name project-themes
NULL

#' @export
#' @rdname project-themes
themeUKPolitical <- function() {
  themeNew(
    default_theme = themeDefaultExcel(font = "Arial", font_size = 8),
    format_title = list(font_size = 14),
    format_banner_names = list(
      font = "Arial Narrow", font_size = 8,
      border_top = TRUE, border_bottom = TRUE, border_left = TRUE, border_right = TRUE),
    format_banner_categories = list(
      font = "Arial Narrow", font_size = 8,
      border_top = TRUE, border_bottom = TRUE, border_left = TRUE, border_right = TRUE),
    format_var_alias = NULL,
    format_var_name = NULL,
    format_var_description = list(
      repeat_for_subs = FALSE, decoration = "bold",
      font_color = "black"),
    format_var_filtertext = list(repeat_for_subs = FALSE),
    format_var_subname = list(decoration = "bold", font_color = "black"),
    format_label_column = list(halign = "right", col_width = 80, extend_borders = FALSE),
    format_subtotals = list(background_color = "#b8cce4"),
    format_headers = list(background_color = "#b8cce4"),
    format_weighted_n = list(
      name = "Weighted Sample",
      border_top = TRUE, border_bottom = TRUE, border_left = TRUE, border_right = TRUE,
      border_style = "thin", border_color = "black", position_bottom = FALSE,
      position_fixed = TRUE, halign = "center"),
    format_unweighted_n = list(
      name = "Unweighted Sample", font_color = "#969696",
      border_top = TRUE, border_bottom = TRUE, border_left = TRUE, border_right = TRUE,
      position_bottom = FALSE, position_fixed = TRUE),
    format_totals_column = list(decoration = "bold"),
    format_means = NULL,
    format_medians = NULL,
    format_totals_row = NULL,
    format_min_base = list(min_base = 50, mask = NULL, decoration = "italic"),
    excel_freeze_column = 2,
    digits_final = 0,
    excel_percent_sign = FALSE,
    latex_headtext = "",
    latex_foottext = "",
    latex_table_align = "r",
    latex_multirowheaderlines = FALSE,
    latex_max_lines_for_tabular = 0)
}

#' @export
#' @rdname project-themes
themeHuffPoToplines <- function(logo = NULL) {
  themeNew(
    default_theme = themeDefaultLatex(),
    logo = logo,
    format_title = list(decoration = "bold"),
    format_var_description = list(
      include_q_number = TRUE, decoration = "bold",
      background_color = "gray"),
    format_var_filtertext = list(decoration = "italic", font_size = 8),
    format_totals_row = NULL,
    format_unweighted_n = NULL,
    latex_headtext = "tbc",
    latex_foottext = "tbc",
    latex_table_align = "r",
    one_per_sheet = FALSE)
}

#' @export
#' @rdname project-themes
themeHuffPoCrosstabs <- function(logo = NULL) {
  themeNew(
    default_theme = themeDefaultLatex(),
    logo = logo,
    format_title = list(decoration = "bold"),
    format_subtitle = list(decoration = "bold"),
    format_min_base = list(min_base = 30, mask = "*"),
    format_var_name = list(include_q_number = TRUE, decoration = "bold"),
    format_var_description = list(include_q_number = FALSE),
    format_var_filtertext = list(decoration = "italic", font_size = 8),
    format_unweighted_n = list(latex_add_parenthesis = TRUE),
    latex_headtext = "tbc",
    latex_foottext = "tbc",
    latex_table_align = "r",
    one_per_sheet = TRUE)
}
