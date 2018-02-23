#' @param format_banner_labels
#' @param format_banner_categories
#' @param format_banner_total
#' @param format_subtotals
#' @param format_headers
#' @param format_means
#' @param format_var_name
#' @param format_var_alias
#' @param format_var_description
#' @param format_var_filtertext
#' @param format_var_subname
#' @param format_label_column
#' @param format_weighted_n
#' @param format_unweighted_n
#' @param format_totals_row
#' @param format_totals_col
#' @param format_min_base

#' @param show_grid_lines
#' @param banner_vars_split
#' @param orientation
#' @param logo
#' @param freeze_column
#' @param percent_format_data

#' @param page_breaks ?
theme_new <- function(..., default_theme = theme_default()){
    
    wrong_class_error(default_theme, "Theme", "default_theme")

    defs_theme <- theme_default()
    
    dots <- list(...)
    
    if (any(names(dots) == "")) {
        stop("All arguments must have a name.")
    }
    
    ignore <- c(intersect(names(dots), names(formals(theme_default))), setdiff(names(dots), names(defs_theme)))
    if (length(ignore) > 0) {
        warning("Arguments: ", collapse_items(ignore), " are not supported in theme_new and will be ignored.")
        dots[ignore] <- NULL
    }
    
    theme <- modifyList(default_theme, dots, keep.null = TRUE)
    
    errors <- lapply(names(default_theme), function (name) {
        if (!name %in% names(theme)) return(paste0("`", name, "` cannot be missing."))
        if (name %in% names(theme_validators)) return(theme_validators[[name]](theme[[name]], default_theme[[name]], name))
        if (is.list(default_theme[[name]])) return(theme_validators$list(theme[[name]], default_theme[[name]], name))
        return(theme_validators$other(theme[[name]], default_theme[[name]], name))
    })
    
    error_if_items(paste0(unlist(errors), collapse = "\n"), "\n{items}")
    
    class(theme) <- "Theme"
    return(theme)
}


theme_default <- function(font = getOption("font", default = "Calibri"),
    font_size = getOption("font_size", default = 12),
    font_color = getOption("font_color", default = "black"),
    border_color = getOption("border_color", default = "black")){

    norm <- list(font = font, font_size = font_size, font_color = font_color, backgroundcolor = "white", halign = "right", valign = "center", wrapText = TRUE)
    defaults <- list(font = font, font_size = font_size, font_color = font_color,
        format_banner_labels = c(norm, decoration = "bold", border_style = "thick", border_color = border_color),
        format_banner_categories = c(norm, decoration = "bold", border_style = "thin", border_color = border_color),
        format_banner_total = c(norm, decoration = "bold", border_style = "thin", border_color = border_color),
        format_subtotals = c(norm, decoration = "bold"),
        format_headers = c(norm, decoration = "bold"),
        format_means = c(name="Mean", norm, decoration = "bold"),
        format_var_alias = c(norm, decoration = "bold"),
        format_var_name = c(norm, decoration = "bold", include_alias = FALSE),
        format_var_description = c(norm, decoration = "bold", include_alias = FALSE),
        format_var_subname = c(norm, decoration = "bold", include_alias = FALSE),
        format_var_filtertext = c(norm, decoration = "italic", include_alias = FALSE),
        format_label_column = c(norm, decoration = NULL, col_width = 20),
        format_weighted_n = c(name = "Weighted N", norm, decoration = "bold", border_style = "thin", border_color = border_color, position = "bottom"),
        format_unweighted_n = c(name = "Unweighted N", norm, decoration = "bold", border_style = "thin", border_color = border_color, position = "bottom"),
        format_totals_row = c(name = "Total", norm, decoration = "bold", border_style = "thin", border_color = border_color, position = "bottom"),
        format_totals_col = c(norm, decoration = "bold"),
        min_base = list(min_base = 0, mask = "-", decoration = NULL),
        table_border = list(border_style = NULL, border_color = border_color),
        banner_vars_split = list(border_style = "thin", border_color = border_color, empty_col = FALSE),
        show_grid_lines = FALSE,
        orientation = "landscape",
        logo = NULL,
        freeze_column = 1,
        percent_format_data = TRUE)
    
    class(defaults) <- "Theme"
    
    return(defaults)
}


theme_validators <- list(
    "orientation" = function (value, default_value, name) {
        if (class(value) != class(default_value)){ return(paste0("`orientation` must be of class ", class(default_value), ", not ", class(value), ".")) }
        if (!value %in% c("portrait", "landscape")) { return(paste0("`orientation` must be either 'portrait' or 'landscape', not '", value, "'.")) }
    },
    "logo" = function (value, default_value, name) {
        if (is.null(value)) return(NULL)
        if (!is.list(value)) return("`logo` must be of class list, not ", class(value), ".")
        if (length(intersect(names(value), c("file","startRow","startCol","width","height","units","dpi"))) != 7){
            return("`logo` must either be `NULL` or include 'file', 'startRow', 'startCol', 'width', 'height', 'units', and 'dpi'.")
        }
    },
    "list" = function (value, default_value, name) {
        if (is.null(value)) { return(NULL) }
        if (class(value) != class(default_value)) { return(paste0("`", name, "` must be of class ", class(default_value), ", not ", class(value), ".")) }
        if (length(intersect(names(value), names(default_value))) != length(default_value)){
            return(paste0("`", name, ":", setdiff(names(default_value), names(value)), "` cannot be missing."))
        }
        if (!is.null(value$decoration) && !value$decoration %in% c("bold","strikeout","italic","underline","underline2")){
            return(paste0("`", name, ":decoration` must be either `NULL` or 'bold', 'strikeout', 'italic', 'underline', or 'underline2', not ", value$decoration))
        }
        return(unlist(lapply(setdiff(names(default_value), c("mask", "border_style", "decoration")), function(nm){
            if (is.null(value[[nm]])) { return(paste0("`", name, ":", nm, "` cannot be `NULL`")) }
            if (class(value[[nm]]) != class(default_value[[nm]])) { return(paste0("`", name, ":", nm, "` must be of class ", class(default_value[[nm]]), ", not ", class(value[[nm]]))) }
        })))
    },
    "other" = function(value, default_value, name){
        if (class(value) != class(default_value)) { return(paste0("`", name, "` must be of class ", class(default_value), ", not ", class(value), ".")) }
    }
)
