#' Generate Theme for `writeExcel` or `writeLatex`
#'
#' `themeNew` produces themes for `writeExcel` or `writeLatex`.
#'
#' @section Theme Arguments:
#' \describe{
#' \item{digits}{A numeric. How many digits should the data be rounded to? (In Excel, this is excel styling.) Defaults to 0.}
#' \item{digits_numeric}{A numeric. How many digits should continuous variable data be rounded to? (In Latex, , this is Latex styling.) Defaults to 2.}
#' \item{digits_final}{In Excel, an optional numeric. How many digits should the data be rounded to before being added to Excel?}
#' \item{excel_footer}{In Excel, an optional character vector of length 3.  The footer text of the file.}
#' \item{excel_freeze_column}{In Excel, a numeric. What column should be the last frozen column? Defaults to 1.}
#' \item{excel_header}{In Excel, An optional character vector of length 3. The header text of the file.}
#' \item{excel_orientation}{In Excel, a character. The orientation of the page if printed. Valid options are: "landscape", and "portrait". Defaults to "landscape".}
#' \item{excel_percent_sign}{In Excel, a logical. Should "\%" be pasted in each cell that contains a proportion? Defaults to FALSE.}
#' \item{excel_show_grid_lines}{In Excel, a logical. Should the default grid lines of the file show? Defaults to FALSE.}
#' \item{excel_table_border}{In Excel, an optional list. The formatting of the border around each downbreak. Includes: border_color, and border_style.}
#' \item{font}{An optional character. The font to be used.}
#' \item{font_color}{In Excel, an optional color. The color of the font.}
#' \item{font_size}{An optional numeric. The size of the font.}
#' \item{format_banner_categories}{In Excel, a list. How the banner/crossbreak response options should be formatted. Includes: background_color, border_bottom, border_color, border_left, border_right, border_style, border_top, decoration, font, font_color, font_size, halign, valign, and wrap_text.}
#' \item{format_banner_names}{In Excel, an optional list. How the banner/crossbreak variable names should be formatted. Includes: background_color, border_bottom, border_color, border_left, border_right, border_style, border_top, decoration, font, font_color, font_size, halign, valign, and wrap_text.}
#' \item{format_banner_split}{In Excel, an optional list. How should the banner variables be separated? Includes: border_color, border_style}
#' \itemize{
#'      \item{\code{empty_col} In Excel, a logical. Should there be an empty column to separate banner variables? Defaults to FALSE.}
#' }
#' \item{format_headers}{An optional list. How headers should be formatted. If `NULL` headers will not appear. Includes: background_color, border_bottom, border_color, border_left, border_right, border_style, border_top, decoration, font, font_color, font_size, halign, valign, and wrap_text.}
#' \item{format_label_column}{In Excel, a list. How the labels column should be formatted. Includes: background_color, border_bottom, border_color, border_left, border_right, border_style, border_top, decoration, font, font_color, font_size, halign, valign, wrap_text. In LaTeX, allows you to set col_width only, in inches of the crosstab stub globally.}
#' Includes:
#' \itemize{
#'       \item{\code{col_width} A numeric. Width of the label column. Defaults to 40.}
#'       \item{\code{extend_borders} In Excel, a logical. Should the borders created for certain rows extend to the label column? Defaults to FALSE.}
#' }
#' #' \item{format_label_column_exception}{In LaTeX, a character vector of columns widths, specified in inches and named after the question alias whose stub they would effect.}
#' \item{format_means}{An optional list. How means should be formatted. If `NULL` means will not appear. Includes: background_color, border_bottom, border_color, border_left, border_right, border_style, border_top, decoration, font, font_color, font_size, halign, name, position_bottom, position_top, valign, and wrap_text.}
#' \item{format_medians}{An optional list. How medians should be formatted. If `NULL` medians will not appear. Includes: background_color, border_bottom, border_color, border_left, border_right, border_style, border_top, decoration, font, font_color, font_size, halign, name, position_bottom, position_top, valign, and wrap_text.}
#' \item{format_min_base}{An optional list. If a minimum base size is desired, how variables that fall below that base size should be formatted. Includes: background_color, border_bottom, border_color, border_left, border_right, border_style, border_top, decoration, font, font_color, font_size, halign, valign, wrap_text}
#' Includes:
#' \itemize{
#'      \item{\code{mask} An optional character to be used to mark cells with base below the min_base.}
#'      \item{\code{min_base} An optional numeric. The minimum acceptable base size for a question.}
#' }
#' \item{format_subtitle}{An optional list. How the table subtitle should be formatted. If `NULL` the table subtitle will not appear. Includes: background_color, border_bottom, border_color, border_left, border_right, border_style, border_top, decoration, font, font_color, font_size, halign, valign, and wrap_text.}
#' \item{format_subtotals}{An optional list. How subtotals should be formatted. If `NULL` subtotals will not appear. Includes: background_color, border_bottom, border_color, border_left, border_right, border_style, border_top, decoration, font, font_color, font_size, halign, valign, and wrap_text.}
#' \item{format_title}{An optional list. How the table title should be formatted. If `NULL` the table title will not appear. Includes: background_color, border_bottom, border_color, border_left, border_right, border_style, border_top, decoration, font, font_color, font_size, halign, valign, and wrap_text.}
#' \item{format_totals_column}{In Excel, a list. How the totals column should be formatted. Includes: background_color, border_bottom, border_color, border_left, border_right, border_style, border_top, decoration, font, font_color, font_size, halign, valign, and wrap_text.}
#' \item{format_totals_row}{An optional list. How total rows should be formatted. If `NULL` total rows will not appear. Includes: background_color, border_bottom, border_color, border_left, border_right, border_style, border_top, decoration, font, font_color, font_size, halign, name, position_bottom, position_top, valign, and wrap_text.}
#' \item{format_var_alias}{An optional list. How downbreak variable aliases should be formatted. If `NULL` downbreak variable aliases will not appear.  Includes: background_color, border_bottom, border_color, border_left, border_right, border_style, border_top, decoration, font, font_color, font_size, halign, include_q_number, valign, and wrap_text.}
#' \item{format_var_description}{An optional list. How downbreak variable descriptions should be formatted. If `NULL` downbreak variable descriptions will not appear. Includes: background_color, border_bottom, border_color, border_left, border_right, border_style, border_top, decoration, font, font_color, font_size, halign, include_alias, include_q_number, repeat_for_subs, valign, and wrap_text.}
#' \item{format_var_filtertext}{An optional list. How downbreak variable filtertext/notes should be formatted. If `NULL` downbreak variable filtertext/notes will not appear. Includes: background_color, border_bottom, border_color, border_left, border_right, border_style, border_top, decoration, font, font_color, font_size, halign, include_alias, include_q_number, repeat_for_subs, valign, and wrap_text.}
#' \item{format_var_name}{An optional list. How downbreak variable names should be formatted. If `NULL` downbreak variable names will not appear. Includes: background_color, border_bottom, border_color, border_left, border_right, border_style, border_top, decoration, font, font_color, font_size, halign, include_alias, include_q_number, repeat_for_subs, valign, and wrap_text.}
#' \item{format_var_subname}{An optional list. How downbreak subvariable names should be formatted. If `NULL` downbreak subvariable names will not appear. Includes: background_color, border_bottom, border_color, border_left, border_right, border_style, border_top, decoration, font, font_color, font_size, halign, include_alias, include_q_number, valign, and wrap_text.}
#' \item{format_unweighted_n}{An optional list. How unweighted Ns should be formatted. If `NULL` unweighted Ns will not appear. Includes: background_color, border_bottom, border_color, border_left, border_right, border_style, border_top, decoration, font, font_color, font_size, halign, name, position_bottom, position_fixed, position_top, valign, and wrap_text.}
#' \item{format_weighted_n}{An optional list. How weighted Ns should be formatted. If `NULL` weighted Ns will not appear. Includes: background_color, border_bottom, border_color, border_left, border_right, border_style, border_top, decoration, font, font_color, font_size, halign, name, position_bottom, position_fixed, position_top, valign, and wrap_text.}
#' \item{latex_foottext}{In Latex, a character. A character string indicating what text should be placed at the top of continuation tables. 'tbc' is a shortcut for 'to be continued.'}
#' \item{latex_headtext}{In Latex, a character. A character string indicating what text should be placed at the bottom of continuation tables. 'tbc' is a shortcut for 'to be continued.'}
#' \item{latex_max_lines_for_tabular}{In Latex, an integer. What is the maximum number of lines a table can be before it is converted to a longtable? Currently only works on toplines. Defaults to 0.}
#' \item{latex_multirowheaderlines}{In Latex, a logical. Should banners allow multi-row headlines? Defaults to FALSE.}
#' \item{latex_round_percentages}{In Latex, a logical. In Latex, should percentages be recalculated so they do not exceed 100\% where necessary? Defaults to FALSE.}
#' \item{latex_round_percentages_exception}{In Latex, an optional character. A list of variable aliases that should have the opposite behaviour of that specified in latex_round_percentages.}
#' \item{latex_table_align}{In Latex, a character. A character string indicating what the table alignment should be. Defaults to 'r'.}
#' \item{logo}{An optional list. Information about the logo to be included in the tables.}
#' Includes:
#' \itemize{
#'     \item{\code{file} The path to a PNG file that should be used for the logo. Include the extension (.png) for an Excel theme. Exclude the extension for a Latex theme }
#'     \item{\code{dpi} In Excel, a numeric. The image resolution used for conversion between units. Defaults to 300.}
#'     \item{\code{height} In Excel, a numeric. The height of the logo. Defaults to 2.}
#'     \item{\code{width} In Excel, a numeric. The width of the logo. Defaults to 4.}
#'     \item{\code{startCol} In Excel, a numeric. The column coordinate of upper left corner of the logo. Defaults to 1.}
#'     \item{\code{startRow} In Excel, a numeric. The row coordinate of upper left corner of the logo. Defaults to 1.}
#'     \item{\code{units} In Excel, a character. Units of width and height. Valid options are: "cm", "in", and "px." Defaults to "in".}
#' }
#' \item{one_per_sheet}{A logical. Should each question be on its own sheet/page? Defaults to FALSE.}
#'}
#'
#' \subsection{Subarguments}{
#' \describe{
#' \item{background_color}{In Excel, an optional color. Cell background color.}
#' \item{border_bottom}{In Excel, an optional logical. Should there be a border on the bottom? }
#' \item{border_color}{In Excel, an optional color. The border color of the relevant cells.}
#' \item{border_left}{In Excel, an optional logical. Should there be a border on the left of the relevant cells? }
#' \item{border_right}{In Excel, an optional logical. Should there be a border on the right of the relevant cells? }
#' \item{border_style}{In Excel, an optional character. The style of the border of the relevant cells. Valid options are: "dashDot", "dashDotDot", "dashed", "dotted", "double", "hair", "medium", "mediumDashDot", "mediumDashDotDot", "mediumDashed", "none", "slantDashDot", "thick", and "thin".}
#' \item{border_top}{In Excel, an optional logical. Should there be a border on the top of the relevant cells? }
#' \item{decoration}{An optional character vector. Text decorations to be applied to relevant cells. Valid options are: "bold", "italic", "strikeout", "underline", and "underline2".}
#' \item{font}{An optional character. The font to be used.}
#' \item{font_color}{In Excel, an optional color. The color of the font.}
#' \item{font_size}{An optional numeric. The size of the font.}
#' \item{halign}{In Excel, an optional character. The horizontal alignment of the text. Valid options are: "center", "left", and "right".}
#' \item{include_alias}{A logical. Should the alias of the variable be included with the other information? Defaults to FALSE.}
#' \item{include_q_number}{A logical. Should the question number be included with the other information? Defaults to FALSE.}
#' \item{latex_add_parenthesis}{In Latex, a logical. Should parenthesis be added surrounding the values? Defaults to FALSE.}
#' \item{latex_adjust}{In Latex, an optional character. How should the values be adjusted? Can be missing.}
#' \item{name}{A character. The name to be used for the relevant row(s).}
#' \item{position_bottom}{In Excel, a logical. Should the relevant row(s) be at the bottom of each table? Defaults to TRUE.}
#' \item{position_fixed}{In Excel, a logical. Should the relevant row(s) be fixed at the top of the file with the banner? Defaults to FALSE.}
#' \item{position_top}{In Excel, a logical. Should should the relevant row(s) be at the top of each table? Defaults to FALSE.}
#' \item{repeat_for_subs}{A logical. Should the information be repeated for each subvariable? Defaults to TRUE.}
#' \item{pagebreak_in_banner}{A logical. Allow a banner to be broken midway across pages. Defaults to TRUE. If FALSE, Pushes the page breaking banner sub-table to the next page similar to manually using clearpage}
#' \item{valign}{In Excel, an optional character. The vertical alignment of the text. Valid options are: "bottom", "center", and "top".}
#' \item{wrap_text}{In Excel, an optional logical. Should the text wrap if it extends beyond the width of the cell? Defaults to TRUE.}
#' }
#' }
#'
#' @param ... Named arguments that specify the various output types. See the Theme Arguments section below for a complete listing.
#' @param default_theme a theme object to use for defaults if you're modifying an already existing themes
#'
#' @export
#' @importFrom utils modifyList
themeNew <- function(..., default_theme = themeDefaultExcel()){

  wrong_class_error(default_theme, "Theme", "default_theme")

  dots <- list(...)

  if (any(names(dots) == "")) {
    stop("All arguments must have a name.")
  }

  for (nm in setdiff(names(dots), names(default_theme))) {
    if (length(unlist(validators_to_use[[nm]]["include"])) > 1) {
      default_theme[[nm]] <- list()
      for (incl in setdiff(validators_to_use[[nm]]$include, dots[[nm]])) {
        if (as.logical(validators_to_use[[incl]]["missing"])) {
          next
        } else if (incl %in% names(default_theme)) {
          default_theme[[nm]][[incl]] <- default_theme[[incl]]
        } else if (!is.null(validators_to_use[[incl]]["default"]) && !is.na(validators_to_use[[incl]]["default"])) {
          rsp <- unlist(validators_to_use[[incl]]["default"])
          if (validators_to_use[[incl]]["class"] %in% "logical") { default_theme[[nm]][[incl]] <- as.logical(rsp) }
          else if (validators_to_use[[incl]]["class"] %in% "numeric") { default_theme[[nm]][[incl]] <- as.numeric(rsp) }
          else { default_theme[[nm]][[incl]] <- rsp }
        } else if (incl %in% "name") {
          default_theme[[nm]][[incl]] <- gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", gsub("_", " ", gsub("format_", "", nm)), perl=TRUE)
        } else print(paste(nm, incl))
      }
    }
  }

  theme <- modifyList(default_theme, dots, keep.null = TRUE)
  theme <- theme[union(names(dots), names(default_theme))]

  if (theme$latex_headtext %in% "tbc") theme$latex_headtext <- "continued from previous page"
  if (theme$latex_foottext %in% "tbc") theme$latex_foottext <- "continued on the next page \\dots"

  theme_validator(theme)

  class(theme) <- "Theme"
  return(theme)
}

#' Generate default Theme for `writeExcel`
#'
#' \code{themeDefaultExcel} is the default theme. Users can change base options
#'
#' @param font the font to use (default: Calibri)
#' @param font_size the font size to use (default: 12)
#' @param font_color the color of the type (default: black)
#' @param valign the alignment to use in cells (default: center)
#'
#' @return A set of theme defaults for use in `writeExcel`
#'
#' @export
themeDefaultExcel <- function(
  font = getOption("font", default = "Calibri"),
  font_size = getOption("font_size", default = 12),
  font_color = getOption("font_color", default = "black"),
  valign = getOption("valign", default = "center")) {

  fn <- font; fs <- font_size; fc <- font_color; va <- valign;
  norm_maker <- function(font = fn, font_size = fs,
                         font_color = fc, decoration = NULL, valign = va, wrap_text = TRUE,
                         border_style = "thin", border_color = "black", border_top = FALSE,
                         border_bottom = FALSE, border_left = FALSE, border_right = FALSE) {
    list(font = font, font_size = font_size, font_color = font_color,
         decoration = decoration, valign = valign, wrap_text = wrap_text,
         border_style = border_style, border_color = border_color,
         border_top = border_top, border_bottom = border_bottom,
         border_left = border_left, border_right = border_right)
  }
  position_list <- list(position_top = FALSE, position_bottom = TRUE)
  var_incl <- function(include_alias = FALSE, include_q_number = FALSE,
                       repeat_for_subs = TRUE) {
    list(include_alias = include_alias, include_q_number = include_q_number,
         repeat_for_subs = repeat_for_subs)
  }
  defaults <- list(
    font = font, font_size = fs, font_color = font_color, valign = valign, halign = "center",
    format_title = norm_maker(font_size = fs + 4, decoration = "bold"),
    format_subtitle = norm_maker(font_size = fs + 2, decoration = "bold"),
    format_banner_names = c(norm_maker(border_bottom = TRUE, decoration = "bold"), halign = "center"),
    format_banner_categories = c(norm_maker(decoration = "bold"), halign = "center"),
    format_var_name = c(norm_maker(decoration = "bold"), var_incl(include_q_number = TRUE), halign = "left"),
    format_var_subname = c(norm_maker(decoration = "bold"), unlist(var_incl(repeat_for_subs = NULL)), halign = "left"),
    format_var_description = c(norm_maker(font_color = "#444444"), var_incl(), halign = "left"),
    format_var_filtertext = c(norm_maker(decoration = "italic"), var_incl(), halign = "left"),
    format_label_column = c(norm_maker(), col_width = 40, halign = "right", extend_borders = TRUE),
    format_subtotals = norm_maker(decoration = "bold"),
    format_headers = norm_maker(decoration = "bold"),
    format_means = c(name = "Mean", norm_maker(decoration = "bold"), position_list),
    format_medians = c(name = "Median", norm_maker(decoration = "bold"), position_list),
    format_unweighted_n = c(name = "Unweighted N", norm_maker(decoration = "bold"), position_list, position_fixed = FALSE, halign = "center", latex_add_parenthesis = TRUE),
    format_totals_row = c(name = "Totals", norm_maker(border_top = TRUE, decoration = "bold"), position_list, halign = "center"),
    format_totals_column = norm_maker(),
    excel_show_grid_lines = FALSE,
    excel_orientation = "portrait",
    excel_freeze_column = 1,
    excel_percent_sign = TRUE,
    digits = 0,
    digits_numeric = 2,
    one_per_sheet = FALSE,
    latex_round_percentages = TRUE,
    latex_headtext = "",
    latex_foottext = "",
    latex_table_align = "r",
    latex_multirowheaderlines = TRUE,
    latex_max_lines_for_tabular = 0)

  class(defaults) <- "Theme"

  return(defaults)
}

#' Generate default Theme for `writeLatex`
#'
#' \code{themeDefaultLatex} is the default theme. Users can change base options
#'
#' @param font the font to use (default: `"helvet"` for Helvetica)
#' @param font_size the font size to use (default: 12)
#'
#' @return A set of theme defaults for use in `writeLatex`
#'
#' @export
themeDefaultLatex <- function(font = getOption("font", default = "helvet"),
                              font_size = getOption("font_size", default = 12)){

  norm <- list(font = font, font_size = NULL)
  defaults <- list(
    font = font, font_size = font_size,
    format_title = list(font_size = font_size + 4, decoration = "bold"),
    format_subtitle = list(font_size = font_size, decoration = "bold"),
    format_banner_categories = norm,
    format_var_description = c(norm, include_alias = FALSE,
                               include_q_number = TRUE, repeat_for_subs = TRUE),
    format_var_subname = c(norm, include_alias = FALSE, include_q_number = FALSE),
    format_var_filtertext = list(font_size = font_size - 4, decoration = "italic",
                                 include_alias = FALSE, include_q_number = FALSE, repeat_for_subs = TRUE),
    format_subtotals = c(norm, decoration = "bold"),
    format_headers = c(norm, decoration = "bold"),
    format_unweighted_n = c(norm, name = "Unweighted N",
                            position_top = FALSE, position_bottom = TRUE, position_fixed = FALSE,
                            latex_add_parenthesis = FALSE, latex_adjust = "c"),
    format_totals_row = c(norm, name = "Totals",
                          position_top = FALSE, position_bottom = TRUE),
    format_label_column = c(norm, col_width = NA_real_ , extend_borders = FALSE),
    format_totals_column = norm,
    digits = 0,
    digits_numeric = 2,
    one_per_sheet = TRUE,
    excel_percent_sign = TRUE,
    excel_show_grid_lines = FALSE,
    excel_freeze_column = 0,
    excel_orientation = "portrait",
    latex_round_percentages = FALSE,
    latex_headtext = "",
    latex_foottext = "",
    latex_table_align = "r",
    latex_multirowheaderlines = TRUE,
    latex_max_lines_for_tabular = 0,
    pagebreak_in_banner = TRUE
  )

  class(defaults) <- "Theme"

  return(defaults)
}

theme_validators <- list(
  "find_validator" = function(value, name){
    validator <- validators_to_use[[tail(name, 1)]]
    if (!as.logical(validator["missing"]) && is.null(value)) {
      return(theme_validators$null_error(name))
    } else if (is.null(value)) {
      return(NULL)
    } else if (is.list(validator)) {
      if ("include" %in% names(validator)) {
        if (!is.list(value)) {
          return(
            theme_validators$class_error(value, "list", name,
                                         as.logical(validator["missing"])))
        }
        return(sapply(validator$include, function(nm){
          return(theme_validators$find_validator(value[[nm]], c(name, nm)))
        }))
      }
      return(theme_validators$valid_values(value, name, validator))
    }
    if (!is.na(validator["len"]) && length(value) != as.numeric(validator["len"])) {
      return(theme_validators$length_error(value, validator["len"], name,
                                           validator["missing"]))
    }
    if (is.integer(value)) { value <- as.numeric(value) }
    if (validator["class"] %in% "color") {
      return(theme_validators$color(value, name))
    } else if (class(value) != validator["class"]) {
      return(theme_validators$class_error(value, validator["class"], name,
                                          validator["missing"]))
    }
  },
  "class_error" = function(value, expected_class, name, missing) {
    return(paste0("`", paste0(name, collapse = ":"), "`",
                  if (as.logical(missing)) ", if provided,", " must be of class ",
                  expected_class, ", not ", collapse_items(class(value)), "."))
  },
  "null_error" = function(name){
    return(paste0("`", paste0(name, collapse = ":"),
                  "` must have a value. It cannot be `NULL`."))
  },
  "length_error" = function(value, len, name, missing){
    return(paste0("`", paste0(name, collapse = ":"), "`",
                  if (as.logical(missing)) ", if provided,", " must have length ",
                  len, ", not ", length(value), "."))
  },
  "valid_values" = function(value, name, validator){
    if (!validator$mult && length(value) != 1) {
      return(theme_validators$length_error(value, 1, name, validator["missing"]))
    } else if (class(value) != "character") {
      return(theme_validators$class_error(value, "character", name, validator["missing"]))
    } else if (!all(tolower(value) %in% tolower(validator$valid))) {
      vals <- paste0("'", validator$valid, "'", collapse = ", ")
      if (nchar(vals) > 100) vals <- paste0(substr(vals, 1, 100), "...")
      return(paste0("`", paste0(name, collapse = ":"), "`",
                    if (as.logical(validator["missing"])) ", if provided,", " must be in ",
                    vals, ", not ", paste0("'", value, "'", collapse = ', '), "."))
    }
  },
  "color" = function(value, name){
    if (class(value) != "character") {
      return(theme_validators$class_error(value, "character", name, TRUE))
    } else if (!(value %in% colors() | grepl("^#(\\d|[a-f]){6,8}$", value, ignore.case = TRUE))) {
      return(paste0("`", paste0(name, collapse = ":"), "` must be a color."))
    }
  }
)

norm <- list("font", "font_size", "font_color", "background_color", "valign",
             "halign","wrap_text", "decoration", "font_size", "border_style",
             "border_color", "border_top", "border_bottom", "border_left", "border_right")

validators_to_use <- list(
  background_color = c(class = "color", len = 1, missing = TRUE),
  format_banner_split = list(missing = TRUE,
                             include = list("border_style", "border_color", "empty_col")),
  border_color = c(class = "color", len = 1, missing = TRUE),
  border_style = list(mult = FALSE, missing = TRUE,
                      valid = list("none", "thin", "medium", "dashed", "dotted", "thick",
                                   "double", "hair", "mediumDashed", "dashDot", "mediumDashDot", "dashDotDot",
                                   "mediumDashDotDot", "slantDashDot")),
  border_top = c(class = "logical", len = 1, missing = TRUE),
  border_bottom = c(class = "logical", len = 1, missing = TRUE),
  border_left = c(class = "logical", len = 1, missing = TRUE),
  border_right = c(class = "logical", len = 1, missing = TRUE),
  col_width = c(class = "numeric", len = 1, missing = FALSE, default = 40),
  decoration = list(mult = TRUE, missing = TRUE,
                    valid = list("bold","strikeout","italic","underline","underline2")),
  digits = c(class = "numeric", len = 1, missing = FALSE, default = 0),
  digits_numeric = c(class = "numeric", len = 1, missing = FALSE, default = 2),
  digits_final = c(class = "numeric", len = 1, missing = TRUE),
  dpi = c(class = "numeric", len = 1, missing = FALSE, default = 300),
  empty_col = c(class = "logical", len = 1, missing = FALSE, default = FALSE),
  extend_borders = c(class = "logical", len = 1, missing = FALSE, default = FALSE),
  file = c(class = "character", len = 1, missing = TRUE),
  font = c(class = "character", len = 1, missing = TRUE),
  font_color = c(class = "color", len = 1, missing = TRUE),
  font_size = c(class = "numeric", len = 1, missing = TRUE),
  excel_footer = c(class = "character", len = 3, missing = TRUE),
  format_banner_categories = list(missing = FALSE, include = norm),
  format_banner_names = list(missing = TRUE, include = norm),
  format_title = list(missing = TRUE, include = norm),
  format_headers = list(missing = TRUE, include = norm),
  format_label_column = list(missing = TRUE,
                             include = c(norm, "col_width", "extend_borders")),
  format_label_column_exceptions = c(class = "numeric", len = NA, missing = TRUE),
  format_means = list(missing = TRUE,
                      include = c(norm, "name", "position_top", "position_bottom")),
  format_medians = list(missing = TRUE,
                        include = c(norm, "name", "position_top", "position_bottom")),
  format_min_base = list(missing = TRUE, include = c(norm, "min_base", "mask")),
  format_subtitle = list(missing = TRUE, include = norm),
  format_subtotals = list(missing = TRUE, include = norm),
  format_totals_column = list(missing = FALSE, include = norm),
  format_totals_row = list(missing = TRUE,
                           include = c("name", norm, "position_top", "position_bottom")),
  format_unweighted_n = list(missing = TRUE,
                             include = c("name", norm, "position_top", "position_bottom", "position_fixed")),
  format_var_alias = list(missing = TRUE, include = c(norm, "include_q_number")),
  format_var_name = list(missing = TRUE,
                         include = c(norm, "include_alias", "repeat_for_subs", "include_q_number")),
  format_var_description = list(missing = TRUE,
                                include = c(norm, "include_alias", "repeat_for_subs", "include_q_number")),
  format_var_subname = list(missing = TRUE,
                            include = c(norm, "include_alias", "include_q_number")),
  format_var_filtertext = list(missing = TRUE,
                               include = c(norm, "include_alias", "repeat_for_subs", "include_q_number")),
  format_weighted_n = list(missing = TRUE,
                           include = c("name", norm, "position_top", "position_bottom", "position_fixed")),
  excel_freeze_column = c(class = "numeric", len = 1, missing = FALSE, default = 1),
  halign = list(mult = FALSE, missing = TRUE, valid = list("left", "right", "center")),
  excel_header = c(class = "character", len = 3, missing = TRUE),
  height = c(class = "numeric", len = 1, missing = FALSE, default = 2),
  include_alias = c(class = "logical", len = 1, missing = FALSE, default = FALSE),
  include_q_number = c(class = "logical", len = 1, missing = FALSE, default = FALSE),
  latex_add_parenthesis = c(class = "logical", len = 1, missing = FALSE, default = FALSE),
  latex_adjust = c(class = "character", len = 1, missing = TRUE),
  latex_foottext = c(class = "character", len = 1, missing = FALSE, default = ""),
  latex_headtext = c(class = "character", len = 1, missing = FALSE, default = ""),
  latex_max_lines_for_tabular = c(class = "numeric", len = 1, missing = FALSE, default = 0),
  latex_multirowheaderlines = c(class = "logical", len = 1, missing = FALSE, default = FALSE),
  latex_round_percentages = c(class = "logical", len = 1, missing = FALSE, default = FALSE),
  latex_round_percentages_exception = c(class = "character", len = NA, missing = TRUE),
  latex_table_align = c(class = "character", len = 1, missing = FALSE, default = ""),
  logo = list(missing = TRUE, include = list("file", "startRow", "startCol",
                                             "width", "height", "units", "dpi")),
  mask = c(class = "character", len = 1, missing = TRUE),
  min_base = c(class = "numeric", len = 1, missing = TRUE),
  name = c(class = "character", len = 1, missing = FALSE),
  one_per_sheet = c(class = "logical", len = 1, missing = FALSE, default = FALSE),
  excel_orientation = list(mult = FALSE, missing = FALSE,
                           valid = list("portrait", "landscape"), default = "landscape"),
  excel_percent_sign = c(class = "logical", len = 1, missing = FALSE, default = FALSE),
  position_bottom = c(class = "logical", len = 1, missing = FALSE, default = TRUE),
  position_fixed = c(class = "logical", len = 1, missing = FALSE, default = FALSE),
  position_top = c(class = "logical", len = 1, missing = FALSE, default = FALSE),
  repeat_for_subs = c(class = "logical", len = 1, missing = FALSE, default = TRUE),
  pagebreak_in_banner = c(class = "logical", len = 1, missing = TRUE, default = TRUE),
  excel_show_grid_lines = c(class = "logical", len = 1, missing = FALSE, default = FALSE),
  startCol = c(class = "numeric", len = 1, missing = FALSE, default = 1),
  startRow = c(class = "numeric", len = 1, missing = FALSE, default = 1),
  excel_table_border = list(missing = TRUE, include = list("border_style", "border_color")),
  units = list(mult = FALSE, missing = FALSE, valid = list("in", "cm", "px"), default = "in"),
  valign = list(mult = FALSE, missing = TRUE, valid = list("top", "bottom", "center")),
  width = c(class = "numeric", len = 1, missing = FALSE, default = 4),
  wrap_text = c(class = "logical", len = 1, missing = TRUE, default = TRUE)
)

#' Theme Validator
#'
#' Validates a theme.
#'
#' @param theme An object from \link{themeNew}
theme_validator <- function(theme) {
  theme_required <- c(
    "digits","digits_numeric", "digits_final", "excel_footer",
    "excel_freeze_column", "excel_header", "excel_orientation",
    "excel_percent_sign", "excel_show_grid_lines", "excel_table_border",
    "font", "font_color", "font_size", "format_banner_categories",
    "format_banner_names", "format_banner_split", "format_headers",
    "format_label_column", "format_label_column_exceptions", "format_means",
    "format_medians", "format_min_base",
    "format_subtitle", "format_subtotals", "format_title",
    "format_totals_column", "format_totals_row", "format_unweighted_n",
    "format_var_alias", "format_var_description", "format_var_filtertext",
    "format_var_name", "format_var_subname", "format_weighted_n", "halign",
    "latex_foottext", "latex_headtext", "latex_max_lines_for_tabular",
    "latex_multirowheaderlines", "latex_round_percentages",
    "latex_round_percentages_exception", "latex_table_align", "logo",
    "one_per_sheet","valign", "pagebreak_in_banner")

  ignore <- setdiff(names(theme), theme_required)
  if (length(ignore) > 0) {
    warning("Arguments: ", collapse_items(ignore),
            " are not supported in themeNew and will be ignored.")
  }

  errors <- unlist(sapply(theme_required, function(name) {
    theme_validators$find_validator(theme[[name]], name)
  }))

  if (is.numeric(theme$digits) && theme$digits > 20)
    errors <- c(errors,"`digits` must be less than 20.")

  if (length(errors) != 0) {
    if (length(errors) > 5) stop("\n", paste0(errors[1:5], collapse = "\n"),
                                 "\nAnd ", length(errors) - 5, " more errors.", call. = FALSE)
    stop("\n", paste0(errors, collapse = "\n"), call. = FALSE)
  }

  # Issue 88
  if ("logo" %in% names(theme)) {
    # Check if version of logo exists:

    wo_png = file.exists(paste0(theme$logo$file,".png"))
    w_png = file.exists(paste0(theme$logo$file))

    if (!(wo_png | w_png)) {
      stop("Logo file not found, check path to file or current working directory.")
    }
  }

}

