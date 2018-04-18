#' Generate Theme for writeExcel
#'
#' \code{theme_new} produces themes for writeExcel.
#'
#' @param format_title  format_title format_title
#' @param format_subtitle format_subtitle format_subtitle
#' @param format_banner_labels format_banner_labels format_banner_labels
#' @param format_banner_categories format_banner_categories format_banner_categories #XX
#' @param format_var_alias format_var_alias format_var_alias
#' @param format_var_name format_var_name format_var_name
#' @param format_var_description format_var_description format_var_description
#' @param format_var_subname format_var_subname format_var_subname
#' @param format_var_filtertext format_var_filtertext format_var_filtertext
#' @param format_label_column format_label_column format_label_column #XX 
#' @param format_subtotals format_subtotals format_subtotals
#' @param format_headers format_headers format_headers
#' @param format_means format_means format_means
#' @param format_medians format_medians format_medians
#' @param format_weighted_n format_weighted_n format_weighted_n
#' @param format_unweighted_n format_unweighted_n format_unweighted_n
#' @param format_totals_row format_totals_row format_totals_row
#' @param format_totals_column format_totals_column format_totals_column #XX
#' @param format_min_base format_min_base format_min_base
#' @param table_border table_border table_border
#' @param banner_vars_split banner_vars_split banner_vars_split
#' @param show_grid_lines show_grid_lines show_grid_lines
#' @param orientation orientation orientation
#' @param logo logo logo
#' @param freeze_column freeze_column freeze_column
#' @param percent_format_data percent_format_data percent_format_data
#' @param digits digits digits
#' @param digits_final digits_final digits_final
#' @param one_per_sheet one_per_sheet one_per_sheet
#' 
#' @param latex_adjust A LaTeX column adjustoment setting for banner's 'Weighted / Unweighted N' values.**
#' @param latex_add_parenthesis logical. Should 'Weighted / Unweighted N' values in banners be parenthesised?
#' Defaults to \code{TRUE}. **
#' @param latex_headtext An optional character string indicating what text should be
#' placed at the top of continuation tables. 'tbc' is a shortcut for 'to be
#' continued.' **
#' @param latex_foottext An optional character string indicating what text should be
#' placed at the bottom of continuation tables. 'tbc' is a shortcut for
#' 'continued from previous page.' **
#' @param latex_round_percentages logical. Should percentages be rounded to sum up to 100?
#' Defaults to \code{FALSE}. **
#' 
#' @param page_breaks ? page_breaks page_breaks
#' 
#' @examples
#' \dontrun{
#' theme stuff
#' }
#' @export
theme_new <- function(..., default_theme = theme_default()){
    
    wrong_class_error(default_theme, "Theme", "default_theme")
    
    dots <- list(...)
    
    if (any(names(dots) == "")) {
        stop("All arguments must have a name.")
    }
    
    for (nm in setdiff(names(dots), names(default_theme))) {
        if (length(unlist(validators_to_use[[nm]]["include"])) > 1) {
            default_theme[[nm]] <- list()
            for (incl in setdiff(validators_to_use[[nm]]$include, dots[[nm]])) {
                if (incl %in% names(default_theme)) {
                    default_theme[[nm]][[incl]] <- default_theme[[incl]]
                } else if (as.logical(validators_to_use[[incl]]["missing"])) { 
                    next 
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
    
    theme_validator(theme)

    class(theme) <- "Theme"
    return(theme)
}

#' Generate default Theme for writeExcel
#'
#' \code{theme_default} is the default theme. Users can change base options
#'
#' @param font font font
#' @param font_size font_size font_size
#' @param font_color font_color font_color
#' @param border_color border_color border_color
#' @export
theme_default <- function(font = getOption("font", default = "Calibri"),
    font_size = getOption("font_size", default = 12),
    font_color = getOption("font_color", default = "black"),
    border_color = getOption("border_color", default = "black"),
    halign = getOption("halign", default = "center"),
    valign = getOption("valign", default = "center")){

    norm <- list(font = font, font_size = font_size, font_color = font_color, valign = valign, wrap_text = TRUE)
    defaults <- list(font = font, font_size = font_size, font_color = font_color, valign = valign, halign = halign, 
        format_title = c(norm[setdiff(names(norm), "font_size")], decoration = "bold", font_size = font_size + 4), 
        format_subtitle = c(norm[setdiff(names(norm), "font_size")], decoration = "bold", font_size = font_size + 2), 
        format_banner_labels = c(norm, decoration = "bold", border_where = "bottom", border_style = "thin", border_color = border_color, halign = "center"), 
        format_banner_categories = c(norm, decoration = "bold", halign = "center"), 
        format_var_name = c(norm, decoration = "bold", include_alias = FALSE, include_q_number = TRUE, repeat_for_subs = TRUE, halign = "left"), 
        format_var_subname = c(norm, decoration = "bold", include_alias = FALSE, include_q_number = FALSE, halign = "left"), 
        format_var_description = c(norm[setdiff(names(norm), 'font_color')], font_color = "#444444", include_alias = FALSE, include_q_number = FALSE, repeat_for_subs = TRUE, halign = "left"), 
        format_var_filtertext = c(norm, decoration = "italic", include_alias = FALSE, include_q_number = FALSE, repeat_for_subs = TRUE, halign = "left"), 
        format_label_column = c(norm, halign = "right", col_width = 40, halign = "right", extend_borders = TRUE), 
        format_subtotals = c(norm, decoration = "bold"), 
        format_headers = c(norm, decoration = "bold"), 
        format_means = c(name="Mean", norm, decoration = "bold", position_top = FALSE, position_bottom = TRUE), 
        format_medians = c(name="Median", norm, decoration = "bold", position_top = FALSE, position_bottom = TRUE), 
        format_unweighted_n = c(name = "Unweighted N", norm, decoration = "bold", position_top = FALSE, position_bottom = TRUE, position_fixed = FALSE, halign = halign), 
        format_totals_row = c(name = "Totals", norm, decoration = "bold", border_where = "top", border_style = "thin", border_color = border_color, position_top = FALSE, position_bottom = TRUE, halign = halign), 
        format_totals_column = norm, 
        show_grid_lines = FALSE,
        orientation = "portrait",
        freeze_column = 1,
        percent_format_data = TRUE,
        digits = 0, 
        one_per_sheet = FALSE,
        latex_adjust = "c",
        latex_round_percentages = FALSE,
        latex_add_parenthesis = FALSE)

    class(defaults) <- "Theme"
    
    return(defaults)
}

theme_validators <- list(
    "find_validator" = function(value, name){
        validator <- validators_to_use[[tail(name, 1)]]
        if (!as.logical(validator["missing"]) && is.null(value)) { return(theme_validators$null_error(name)) }
        if (is.null(value)) { return(NULL) }
        if (is.list(validator)) {
            if ("include" %in% names(validator)) {
                return(sapply(validator$include, function(nm){
                    return(theme_validators$find_validator(value[[nm]], c(name, nm)))
                }))
            }
            return(theme_validators$valid_values(value, name, validator)) 
        }
        if (length(value) != as.numeric(validator["len"])) { return(theme_validators$length_error(value, validator["len"], name, validator["missing"])) }
        if (is.integer(value)) value <- as.numeric(value)
        if (validator["class"] %in% "color") { return(theme_validators$color(value, name)) }
        if (class(value) != validator["class"]) { return(theme_validators$class_error(value, validator["class"], name, validator["missing"])) }
    },
    "class_error" = function(value, expected_class, name, missing){
        return(paste0("`", paste0(name, collapse = ":"), "`", if (as.logical(missing)) ", if provided,", " must be of class ", expected_class, ", not ", class(value)))
    },
    "null_error" = function(name){
        return(paste0("`", paste0(name, collapse = ":"), "` must have a value. It cannot be `NULL`.")) 
    },
    "length_error" = function(value, len, name, missing){
        return(paste0("`", paste0(name, collapse = ":"), "`", if (as.logical(missing)) ", if provided,", " must have length ", len, ", not ", length(value))) 
    },
    "valid_values" = function(value, name, validator){
        if (!validator$mult && length(value) != 1) { return(theme_validators$length_error(value, 1, name, validator["missing"])) }
        if (class(value) != "character") { return(theme_validators$class_error(value, "character", name, validator["missing"])) }
        if (!all(tolower(value) %in% tolower(validator$valid))){
            vals <- paste0("'", validator$valid, "'", collapse = ", ")
            if (nchar(vals) > 100) vals <- paste0(substr(vals, 1, 100), "...")
            return(paste0("`", paste0(name, collapse = ":"), "`", if (as.logical(validator["missing"])) ", if provided,", " must be in ", vals, ", not ", paste0("'", value, "'", collapse = ', ')))
        }
    },
    "color" = function(value, name){
        if (class(value) != "character") { return(theme_validators$class_error(value, "character", name, TRUE)) }
        if (!(value %in% colors() | grepl("^#(\\d|[a-f]){6,8}$", value, ignore.case=TRUE))) {
            return(paste0("`", paste0(name, collapse = ":"), "` must be a color."))
        }
    }
)

norm <- list("font", "font_size", "font_color", "background_color", "valign", "wrap_text", "decoration", "font_size")
validators_to_use <- list(
    background_color = c(class = "color", len = 1, "missing" = TRUE),
    banner_vars_split = list("missing" = TRUE, include = list("border_style", "border_color", "empty_col")),
    border_color = c(class = "color", len = 1, "missing" = TRUE),
    border_style = list(mult = FALSE, "missing" = TRUE, valid = list("none", "thin", "medium", "dashed", "dotted", "thick", 
        "double", "hair", "mediumDashed", "dashDot", "mediumDashDot", "dashDotDot", "mediumDashDotDot", "slantDashDot")), 
    border_where = list(mult = TRUE, "missing" = TRUE, valid = list("top", "topbottom", "topbottomleft", "topbottomleftright", "topbottomright", 
        "topbottomrightleft", "topleft", "topleftbottom", "topleftbottomright", "topleftright", 
        "topleftrightbottom", "topright", "toprightbottom", "toprightbottomleft", "toprightleft", "toprightleftbottom", 
        "bottom", "bottomtop", "bottomtopleft", "bottomtopleftright", "bottomtopright", "bottomtoprightleft", 
        "bottomleft", "bottomlefttop", "bottomlefttopright", "bottomleftright", "bottomleftrighttop", 
        "bottomright", "bottomrighttop", "bottomrighttopleft", "bottomrightleft", "bottomrightlefttop", 
        "left", "lefttop", "lefttopbottom", "lefttopbottomright", "lefttopright", "lefttoprightbottom", 
        "leftbottom", "leftbottomtop", "leftbottomtopright", "leftbottomright", "leftbottomrighttop", "leftright", 
        "leftrighttop", "leftrighttopbottom", "leftrightbottom", "leftrightbottomtop", 
        "right", "righttop", "righttopbottom", "righttopbottomleft", "righttopleft", "righttopleftbottom", 
        "rightbottom", "rightbottomtop", "rightbottomtopleft", "rightbottomleft", "rightbottomlefttop", 
        "rightleft", "rightlefttop", "rightlefttopbottom", "rightleftbottom", "rightleftbottomtop")),
    col_width = c(class = "numeric", len = 1, "missing" = FALSE, default = 40),
    decoration = list(mult = TRUE, "missing" = TRUE, valid = list("bold","strikeout","italic","underline","underline2")),
    digits = c(class = "numeric", len = 1, "missing" = FALSE, default = 0),
    digits_final = c(class = "numeric", len = 1, "missing" = TRUE),
    dpi = c(class = "numeric", len = 1, "missing" = FALSE, default = 300),
    empty_col = c(class = "logical", len = 1, "missing" = FALSE, default = FALSE),
    extend_borders = c(class = "logical", len = 1, "missing" = FALSE, default = FALSE),
    file = c(class = "character", len = 1, "missing" = TRUE),
    font = c(class = "character", len = 1, "missing" = TRUE),
    font_color = c(class = "color", len = 1, "missing" = TRUE),
    font_size = c(class = "numeric", len = 1, "missing" = TRUE),
    footer = c(class = "character", len = 3, "missing" = TRUE),
    format_banner_categories = list("missing" = FALSE, include = c(norm, "halign", "border_where", "border_style", "border_color")), 
    format_banner_labels = list("missing" = TRUE, include = c(norm, "halign", "border_where", "border_style", "border_color")), 
    format_title = list("missing" = TRUE, include = c(norm, "halign")), 
    format_headers = list("missing" = TRUE, include = norm), 
    format_label_column = list("missing" = FALSE, include = c(norm, "halign", "col_width", "extend_borders")), 
    format_means = list("missing" = TRUE, include = c(norm, "halign", "name", "position_top", "position_bottom")), 
    format_medians = list("missing" = TRUE, include = c(norm, "halign", "name", "position_top", "position_bottom")), 
    format_min_base = list("missing" = TRUE, include = c(norm, "halign", "min_base", "mask")),
    format_subtitle = list("missing" = TRUE, include = c(norm, "halign")), 
    format_subtotals = list("missing" = TRUE, include = norm), 
    format_totals_column = list("missing" = FALSE, include = c(norm, "halign")), 
    format_totals_row = list("missing" = TRUE, include = c("name", norm, "halign", "border_where", "border_style", "border_color", "position_top", "position_bottom")), 
    format_unweighted_n = list("missing" = TRUE, include = c("name", norm, "halign", "border_where", "border_style", "border_color", "position_top", "position_bottom", "position_fixed")), 
    format_var_alias = list("missing" = TRUE, include = c(norm, "halign", "include_q_number")), 
    format_var_name = list("missing" = TRUE, include = c(norm, "halign", "include_alias", "repeat_for_subs", "include_q_number")), 
    format_var_description = list("missing" = TRUE, include = c(norm, "halign", "include_alias", "repeat_for_subs", "include_q_number")), 
    format_var_subname = list("missing" = TRUE, include = c(norm, "halign", "include_alias", "include_q_number")), 
    format_var_filtertext = list("missing" = TRUE, include = c(norm, "halign", "include_alias", "repeat_for_subs", "include_q_number")), 
    format_weighted_n = list("missing" = TRUE, include = c("name", norm, "halign", "border_where", "border_style", "border_color", "position_top", "position_bottom", "position_fixed")), 
    freeze_column = c(class = "numeric", len = 1, "missing" = FALSE, default = 1),
    halign = list(mult = FALSE, "missing" = TRUE, valid = list("left", "right", "center")),
    header = c(class = "character", len = 3, "missing" = TRUE),
    height = c(class = "numeric", len = 1, "missing" = FALSE, default = 2),
    include_alias = c(class = "logical", len = 1, "missing" = FALSE, default = FALSE),
    include_q_number = c(class = "logical", len = 1, "missing" = FALSE, default = FALSE),
    latex_add_parenthesis = c(class = "logical", len = 1, "missing" = FALSE, default = FALSE),
    latex_adjust = c(class = "character", len = 1, "missing" = TRUE),
    latex_foottext = c(class = "character", len = 1, "missing" = TRUE),
    latex_headtext = c(class = "character", len = 1, "missing" = TRUE),
    latex_round_percentages = c(class = "logical", len = 1, "missing" = FALSE, default = FALSE),
    logo = list("missing" = TRUE, include = list("file", "startRow", "startCol", "width", "height", "units", "dpi")),
    mask = c(class = "character", len = 1, "missing" = TRUE),
    min_base = c(class = "numeric", len = 1, "missing" = TRUE),
    name = c(class = "character", len = 1, "missing" = FALSE),
    one_per_sheet = c(class = "logical", len = 1, "missing" = FALSE, default = FALSE),
    orientation = list(mult = FALSE, "missing" = FALSE, valid = list("portrait", "landscape"), default = "landscape"), 
    percent_format_data = c(class = "logical", len = 1, "missing" = FALSE, default = FALSE),
    position_bottom = c(class = "logical", len = 1, "missing" = FALSE, default = TRUE),
    position_fixed = c(class = "logical", len = 1, "missing" = FALSE, default = FALSE),
    position_top = c(class = "logical", len = 1, "missing" = FALSE, default = FALSE),
    repeat_for_subs = c(class = "logical", len = 1, "missing" = FALSE, default = TRUE),
    show_grid_lines = c(class = "logical", len = 1, "missing" = FALSE, default = FALSE),
    startCol = c(class = "numeric", len = 1, "missing" = FALSE, default = 1),
    startRow = c(class = "numeric", len = 1, "missing" = FALSE, default = 1),
    table_border = list("missing" = TRUE, include = list("border_style", "border_color", "border_where")), 
    units = list(mult = FALSE, "missing" = FALSE, valid = list("in", "cm", "px"), default = "in"),
    valign = list(mult = FALSE, "missing" = TRUE, valid = list("top", "bottom", "center")),
    width = c(class = "numeric", len = 1, "missing" = FALSE, default = 4),
    wrap_text = c(class = "logical", len = 1, "missing" = FALSE, default = TRUE))

theme_validator <- function(theme) {
    theme_required <- c("banner_vars_split", "digits", "digits_final", "font", "font_color", "font_size", "footer", 
        "format_banner_categories", "format_banner_labels", "format_headers", "format_label_column", "format_means", 
        "format_medians", "format_min_base", "format_subtitle", "format_subtotals", "format_title", "format_totals_column", 
        "format_totals_row", "format_unweighted_n", "format_var_alias", "format_var_description", 
        "format_var_filtertext", "format_var_name", "format_var_subname", "format_weighted_n", "freeze_column", "halign", 
        "header", "latex_add_parenthesis", "latex_adjust", "latex_foottext", "latex_headtext", "latex_round_percentages", "logo", "one_per_sheet", "orientation", "percent_format_data", "show_grid_lines", "table_border", "valign")
    
    ignore <- setdiff(names(theme), theme_required)
    if (length(ignore) > 0) {
        warning("Arguments: ", collapse_items(ignore), " are not supported in theme_new and will be ignored.")
    }
    
    errors <- unlist(sapply(theme_required, function(name) {
        theme_validators$find_validator(theme[[name]], name)
    }))
    if (length(errors) != 0){
        if (length(errors) > 5) stop("\n", paste0(errors[1:5], collapse = "\n"), "\nAnd ", length(errors) - 5, " more errors.", call. = FALSE)
        stop("\n", paste0(errors, collapse = "\n"), call. = FALSE)
    }
}


#' @export
political_theme <- function() {
    theme_new(default_theme = theme_default(font = "Arial", font_size = 8),
        format_title=list(font_size = 14),
        format_banner_labels=list(font = "Arial Narrow", font_size = 8),
        format_banner_categories=list(font = "Arial Narrow", font_size = 8),
        format_var_alias=NULL,
        format_var_name=NULL,
        format_var_description=list(repeat_for_subs=FALSE),
        format_var_filtertext=list(repeat_for_subs=FALSE),
        format_label_column=list(halign = "right", col_width = 80),
        format_subtotals=list(background_color = "#b8cce4"),
        format_headers=list(background_color = "#b8cce4"),
        format_weighted_n=list(name = "Weighted Sample", border_where = "TopBottomLeftRight", position_bottom = FALSE, position_fixed = TRUE, halign="center"),
        format_unweighted_n=list(name = "Unweighted Sample", font_color = "#969696", border_where = "TopBottomLeftRight", position_bottom = FALSE, position_fixed = TRUE),
        format_totals_row=NULL,
        format_min_base=list(min_base = 50, mask = NULL, decoration = "italic"),
        freeze_column=2,
        digits_final=0,
        percent_format_data=FALSE)
}
