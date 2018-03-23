#' @param format_title 
#' @param format_subtitle
#' @param format_banner_labels
#' @param format_banner_categories #XX
#' @param format_var_alias
#' @param format_var_name
#' @param format_var_description
#' @param format_var_subname
#' @param format_var_filtertext
#' @param format_label_column #XX
#' @param format_subtotals
#' @param format_headers
#' @param format_means
#' @param format_medians
#' @param format_weighted_n
#' @param format_unweighted_n
#' @param format_totals_row
#' @param format_totals_column #XX
#' @param format_min_base

#' @param table_border
#' @param banner_vars_split
#' @param show_grid_lines
#' @param orientation
#' @param logo
#' @param freeze_column
#' @param percent_format_data
#' @param digits
#' @param digits_final
#' @param one_per_sheet

#' @param page_breaks ?
#' @export
theme_new <- function(..., default_theme = theme_default()){
    
    wrong_class_error(default_theme, "Theme", "default_theme")

    defs_theme <- theme_default()
    
    dots <- list(...)
    
    if (any(names(dots) == "")) {
        stop("All arguments must have a name.")
    }
    
    theme <- modifyList(default_theme, dots, keep.null = TRUE)
    
    theme_validator(theme)

    class(theme) <- "Theme"
    return(theme)
}

#' @param font
#' @param font_size
#' @param font_color
#' @param border_color
#' @export
theme_default <- function(font = getOption("font", default = "Calibri"),
    font_size = getOption("font_size", default = 12),
    font_color = getOption("font_color", default = "black"),
    border_color = getOption("border_color", default = "black")){

    norm <- list(font = font, font_size = font_size, font_color = font_color, valign = "center", wrap_text = TRUE)
    defaults <- list(font = font, font_size = font_size, font_color = font_color, valign = "center", halign = "center", 
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
        format_means = c(name="Mean", norm, decoration = "bold"), 
        format_medians = c(name="Median", norm, decoration = "bold"), 
        format_unweighted_n = c(name = "Unweighted N", norm, decoration = "bold", position_top = FALSE, position_bottom = TRUE, position_fixed = FALSE, halign = "center"), 
        format_totals_row = c(name = "Totals", norm, decoration = "bold", border_where = "top", border_style = "thin", border_color = border_color, position_top = FALSE, position_bottom = TRUE, halign = "center"), 
        format_totals_column = norm, 
        show_grid_lines = FALSE,
        orientation = "portrait",
        freeze_column = 1,
        percent_format_data = TRUE,
        digits = 0, 
        one_per_sheet = FALSE)

    class(defaults) <- "Theme"
    
    return(defaults)
}

theme_validators <- list(
    "find_validator" = function(value, name){
        validator <- validators_to_use[[tail(name, 1)]]
        if (!as.logical(validator["NULL"]) && is.null(value)) { return(theme_validators$null_error(name)) }
        if (is.null(value)) { return(NULL) }
        if (is.list(validator)) {
            if ("include" %in% names(validator)) {
                return(sapply(validator$include, function(nm){
                    return(theme_validators$find_validator(value[[nm]], c(name, nm)))
                }))
            }
            return(theme_validators$valid_values(value, name, validator)) 
        }
        if (length(value) != as.numeric(validator["len"])) { return(theme_validators$length_error(value, validator["len"], name, validator["NULL"])) }
        if (is.integer(value)) value <- as.numeric(value)
        if (validator["class"] %in% "color") { return(theme_validators$color(value, name)) }
        if (class(value) != validator["class"]) { return(theme_validators$class_error(value, validator["class"], name, validator["NULL"])) }
    },
    "class_error" = function(value, expected_class, name, null){
        return(paste0("`", paste0(name, collapse = ":"), "`", if (as.logical(null)) ", if provided,", " must be of class ", expected_class, ", not ", class(value)))
    },
    "null_error" = function(name){
        return(paste0("`", paste0(name, collapse = ":"), "` must have a value. It cannot be `NULL`.")) 
    },
    "length_error" = function(value, len, name, null){
        return(paste0("`", paste0(name, collapse = ":"), "`", if (as.logical(null)) ", if provided,", " must have length ", len, ", not ", length(value))) 
    },
    "valid_values" = function(value, name, validator){
        if (!validator$mult && length(value) != 1) { return(theme_validators$length_error(value, 1, name, validator["NULL"])) }
        if (class(value) != "character") { return(theme_validators$class_error(value, "character", name, validator["NULL"])) }
        if (!all(tolower(value) %in% tolower(validator$valid))){
            vals <- paste0("'", validator$valid, "'", collapse = ", ")
            if (nchar(vals) > 100) vals <- paste0(substr(vals, 1, 100), "...")
            return(paste0("`", paste0(name, collapse = ":"), "`", if (as.logical(validator["NULL"])) ", if provided,", " must be in ", vals, ", not ", paste0("'", value, "'", collapse = ', ')))
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
    background_color = c(class = "color", len = 1, "NULL" = TRUE),
    banner_vars_split = list("NULL" = TRUE, include = list("border_style", "border_color", "empty_col")),
    border_color = c(class = "color", len = 1, "NULL" = TRUE),
    border_style = list(mult = FALSE, "NULL" = TRUE, valid = list("none", "thin", "medium", "dashed", "dotted", "thick", 
        "double", "hair", "mediumDashed", "dashDot", "mediumDashDot", "dashDotDot", "mediumDashDotDot", "slantDashDot")), 
    border_where = list(mult = TRUE, "NULL" = TRUE, valid = list("top", "topbottom", "topbottomleft", "topbottomleftright", "topbottomright", 
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
    col_width = c(class = "numeric", len = 1, "NULL" = FALSE),
    decoration = list(mult = TRUE, "NULL" = TRUE, valid = list("bold","strikeout","italic","underline","underline2")),
    digits = c(class = "numeric", len = 1, "NULL" = FALSE),
    dpi = c(class = "numeric", len = 1, "NULL" = FALSE),
    empty_col = c(class = "logical", len = 1, "NULL" = FALSE),
    extend_borders = c(class = "logical", len = 1, "NULL" = FALSE),
    file = c(class = "character", len = 1, "NULL" = TRUE),
    digits_final = c(class = "numeric", len = 1, "NULL" = TRUE),
    font = c(class = "character", len = 1, "NULL" = FALSE),
    font_color = c(class = "color", len = 1, "NULL" = TRUE),
    font_size = c(class = "numeric", len = 1, "NULL" = FALSE),
    footer = c(class = "character", len = 3, "NULL" = TRUE),
    format_banner_categories = list("NULL" = FALSE, include = c(norm, "halign", "border_where", "border_style", "border_color")), 
    format_banner_labels = list("NULL" = TRUE, include = c(norm, "halign", "border_where", "border_style", "border_color")), 
    format_title = list("NULL" = TRUE, include = c(norm, "halign")), 
    format_headers = list("NULL" = TRUE, include = norm), 
    format_label_column = list("NULL" = FALSE, include = c(norm, "halign", "col_width", "extend_borders")), 
    format_means = list("NULL" = TRUE, include = c(norm, "halign", "name")), 
    format_medians = list("NULL" = TRUE, include = c(norm, "halign", "name")), 
    format_min_base = list("NULL" = TRUE, include = c(norm, "halign", "min_base", "mask")),
    format_subtitle = list("NULL" = TRUE, include = c(norm, "halign")), 
    format_subtotals = list("NULL" = TRUE, include = norm), 
    format_totals_column = list("NULL" = FALSE, include = c(norm, "halign")), 
    format_totals_row = list("NULL" = TRUE, include = c("name", norm, "halign", "border_where", "border_style", "border_color", "position_top", "position_bottom")), 
    format_unweighted_n = list("NULL" = TRUE, include = c("name", norm, "halign", "border_where", "border_style", "border_color", "position_top", "position_bottom", "position_fixed")), 
    format_var_alias = list("NULL" = TRUE, include = c(norm, "halign", "include_q_number")), 
    format_var_name = list("NULL" = TRUE, include = c(norm, "halign", "include_alias", "repeat_for_subs", "include_q_number")), 
    format_var_description = list("NULL" = TRUE, include = c(norm, "halign", "include_alias", "repeat_for_subs", "include_q_number")), 
    format_var_subname = list("NULL" = TRUE, include = c(norm, "halign", "include_alias", "include_q_number")), 
    format_var_filtertext = list("NULL" = TRUE, include = c(norm, "halign", "include_alias", "repeat_for_subs", "include_q_number")), 
    format_weighted_n = list("NULL" = TRUE, include = c("name", norm, "halign", "border_where", "border_style", "border_color", "position_top", "position_bottom", "position_fixed")), 
    freeze_column = c(class = "numeric", len = 1, "NULL" = FALSE),
    halign = list(mult = FALSE, "NULL" = TRUE, valid = list("left", "right", "center")),
    header = c(class = "character", len = 3, "NULL" = TRUE),
    height = c(class = "numeric", len = 1, "NULL" = FALSE),
    include_alias = c(class = "logical", len = 1, "NULL" = FALSE),
    include_q_number = c(class = "logical", len = 1, "NULL" = FALSE),
    logo = list("NULL" = TRUE, include = list("file", "startRow", "startCol", "width", "height", "units", "dpi")),
    mask = c(class = "character", len = 1, "NULL" = TRUE),
    min_base = c(class = "numeric", len = 1, "NULL" = TRUE),
    name = c(class = "character", len = 1, "NULL" = FALSE),
    one_per_sheet = c(class = "logical", len = 1, "NULL" = FALSE),
    orientation = list(mult = FALSE, "NULL" = FALSE, valid = list("portrait", "landscape")), 
    percent_format_data = c(class = "logical", len = 1, "NULL" = FALSE),
    position_bottom = c(class = "logical", len = 1, "NULL" = FALSE),
    position_fixed = c(class = "logical", len = 1, "NULL" = FALSE),
    position_top = c(class = "logical", len = 1, "NULL" = FALSE),
    repeat_for_subs = c(class = "logical", len = 1, "NULL" = FALSE),
    show_grid_lines = c(class = "logical", len = 1, "NULL" = FALSE),
    startCol = c(class = "numeric", len = 1, "NULL" = FALSE),
    startRow = c(class = "numeric", len = 1, "NULL" = FALSE),
    table_border = list("NULL" = TRUE, include = list("border_style", "border_color")), 
    units = list(mult = FALSE, "NULL" = FALSE, valid = list("in", "cm", "px")),
    valign = list(mult = FALSE, "NULL" = TRUE, valid = list("top", "bottom", "center")),
    width = c(class = "numeric", len = 1, "NULL" = FALSE),
    wrap_text = c(class = "logical", len = 1, "NULL" = FALSE))

theme_validator <- function(theme) {
    theme_required <- c("banner_vars_split", "digits", "digits_final", "font", "font_color", "font_size", "footer", 
        "format_banner_categories", "format_banner_labels", "format_headers", "format_label_column", "format_means", 
        "format_medians", "format_min_base", "format_subtitle", "format_subtotals", "format_title", "format_totals_column", 
        "format_totals_row", "format_unweighted_n", "format_var_alias", "format_var_description", 
        "format_var_filtertext", "format_var_name", "format_var_subname", "format_weighted_n", "freeze_column", "halign", 
        "header", "logo", "one_per_sheet", "orientation", "percent_format_data", "show_grid_lines", "table_border", "valign")
    
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
        format_means=list(background_color = "#b8cce4"),
        format_medians=list(background_color = "#b8cce4"),
        format_weighted_n=list(name = "Weighted Sample", border_where = "TopBottomLeftRight", position_bottom = FALSE, position_fixed = TRUE),
        format_unweighted_n=list(name = "Unweighted Sample", font_color = "#969696", border_where = "TopBottomLeftRight", position_bottom = FALSE, position_fixed = TRUE),
        format_totals_row=NULL,
        format_min_base=list(min_base = 50, mask = NULL, decoration = "italic"),
        freeze_column=2,
        digits_final=0,
        percent_format_data=FALSE)
}
