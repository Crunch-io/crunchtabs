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
#' @param format_weighted_n
#' @param format_unweighted_n
#' @param format_totals_row
#' @param format_totals_column #XX
#' @param format_min_base
#' @param format_toc_banner #XX

#' @param table_border
#' @param banner_vars_split
#' @param show_grid_lines
#' @param orientation
#' @param logo
#' @param freeze_column
#' @param percent_format_data
#' @param digits
#' @param one_per_sheet

#' @param page_breaks ?
#' @param 
#' @export
theme_new <- function(..., default_theme = theme_default()){
    
    wrong_class_error(default_theme, "Theme", "default_theme")

    defs_theme <- theme_default()
    
    dots <<- list(...)
    
    if (any(names(dots) == "")) {
        stop("All arguments must have a name.")
    }
    
    ignore <- c(intersect(names(dots), names(formals(theme_default))), setdiff(names(dots), names(defs_theme)))
    if (length(ignore) > 0) {
        warning("Arguments: ", collapse_items(ignore), " are not supported in theme_new and will be ignored.")
        dots[ignore] <- NULL
    }
    
    theme <- modifyList(default_theme, dots, keep.null = TRUE)
    
    errors <- unlist(sapply(names(default_theme), function (name) {
        if (!name %in% names(theme)) return(paste0("`", name, "` cannot be missing."))
        if (name %in% names(theme_validators)) return(theme_validators[[name]](theme[[name]], default_theme[[name]], name))
        if (name %in% c('format_banner_categories', 'format_label_column', 'format_totals_column', 'format_toc_banner')) theme_validators$non_null(theme[[name]], default_theme[[name]], name)
        if (is.list(default_theme[[name]])) return(theme_validators$list(theme[[name]], default_theme[[name]], name))
        return(theme_validators$other(theme[[name]], default_theme[[name]], name))
    }), use.names = FALSE)

    if (length(errors) != 0){
        if (length(errors) > 5) stop("\n", paste0(errors[1:5], collapse = "\n"), "\nAnd ", length(errors) - 5, " more errors.", call. = FALSE)
        stop("\n", paste0(errors, collapse = "\n"), call. = FALSE)
    }

    class(theme) <- "Theme"
    return(theme)
}

#' @param font
#' @param font_size
#' @param font_color
#' @param valign
#' @param halign
#' @export
theme_default <- function(font = getOption("font", default = "Calibri"),
    font_size = getOption("font_size", default = 12),
    font_color = getOption("font_color", default = "black"),
    border_color = getOption("border_color", default = "black")){

    norm <- list(font = font, font_size = font_size, font_color = font_color, background_color = NULL, valign = "center", wrap_text = TRUE)
    defaults <- list(font = font, font_size = font_size, font_color = font_color, valign = "center", halign = "center", 
        format_title = c(norm[setdiff(names(norm), "font_size")], decoration = "bold", font_size = font_size + 4, halign = NULL), 
        format_subtitle = c(norm[setdiff(names(norm), "font_size")], decoration = "bold", font_size = font_size + 2, halign = NULL), 
        format_banner_labels = c(norm, decoration = "bold", border_style = "thin", border_color = border_color, halign = "center"), 
        format_banner_categories = c(norm, decoration = "bold", border_style = "thin", border_color = border_color, halign = "center"), 
        format_var_alias = c(norm, decoration = "bold", halign = NULL), 
        format_var_name = c(norm, decoration = "bold", include_alias = FALSE, halign = NULL), 
        format_var_description = c(norm, decoration = "bold", include_alias = FALSE, halign = NULL), 
        format_var_subname = c(norm, decoration = "bold", include_alias = FALSE, halign = NULL), 
        format_var_filtertext = c(norm, decoration = "italic", include_alias = FALSE, halign = NULL), 
        format_label_column = c(norm, halign = "right", decoration = NULL, col_width = 80, halign = NULL), 
        format_subtotals = c(norm, decoration = "bold"), 
        format_headers = c(norm, decoration = "bold"), 
        format_means = c(name="Mean", norm, decoration = "bold"), 
        format_weighted_n = c(name = "Weighted N", norm, decoration = "bold", border_where = "TopBottom", border_style = "thin", border_color = border_color, position_top = FALSE, position_bottom = TRUE, position_fixed = FALSE, halign = "center"), 
        format_unweighted_n = c(name = "Unweighted N", norm, decoration = "bold", border_where = "TopBottom", border_style = "thin", border_color = border_color, position_top = FALSE, position_bottom = TRUE, position_fixed = FALSE, halign = "center"), 
        format_totals_row = c(name = "Total", norm, decoration = "bold", border_where = "TopBottom", border_style = "thin", border_color = border_color, position_top = FALSE, position_bottom = TRUE, halign = NULL), 
        format_totals_column = c(norm, decoration = "bold", halign = NULL), 
        format_min_base = c(min_base = 0, mask = "-", norm, decoration = NULL), 
        format_toc_banner = c(sheet_name = "TOC", norm, decoration = "bold", border_where = "TopBottom", border_style = "thin", border_color = border_color, halign = NULL), 
        table_border = list(border_style = NULL, border_color = border_color), 
        banner_vars_split = list(border_style = "thin", border_color = border_color, empty_col = FALSE), 
        show_grid_lines = FALSE,
        orientation = "landscape",
        logo = list(file = NULL, startRow = 2, startCol = 6, width = 4, height = 2, units = "in", dpi = 300),
        freeze_column = 1,
        percent_format_data = TRUE,
        digits = 0, 
        one_per_sheet = FALSE)
    
    class(defaults) <- "Theme"
    
    return(defaults)
}


theme_validators <- list(
    "orientation" = function (value, default_value, name) {
        if (class(value) != class(default_value)){ return(paste0("`orientation` must be of class ", class(default_value), ", not ", class(value), ".")) }
        if (!value %in% c("portrait", "landscape")) { return(paste0("`orientation` must be either 'portrait' or 'landscape', not '", value, "'.")) }
    },
    "non_null" = function (value, default_value, name){
        if (is.null(value)) { return(paste0("`", name, "` must have a value. It cannot be `NULL`.")) }
        return(theme_validators$list(value, default_value, name))
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
        return(unlist(lapply(setdiff(names(default_value), c("mask", "border_style", "halign", "decoration", "background_color", "file")), function(nm){
            if (is.null(value[[nm]])) { return(paste0("`", name, ":", nm, "` cannot be `NULL`")) }
            if (class(value[[nm]]) != class(default_value[[nm]])) { return(paste0("`", name, ":", nm, "` must be of class ", class(default_value[[nm]]), ", not ", class(value[[nm]]))) }
        })))
    },
    "other" = function(value, default_value, name){
        if (class(value) != class(default_value)) { return(paste0("`", name, "` must be of class ", class(default_value), ", not ", class(value), ".")) }
    }
)

#' @export
political_theme <- function() {
    theme_new(default_theme = theme_default(font = "Arial", font_size = 8),
        format_title=list(font_size = 14),
        format_banner_labels=list(font = "Arial Narrow", font_size = 8),
        format_banner_categories=list(font = "Arial Narrow", font_size = 8),
        format_var_alias=NULL,
        format_var_name=NULL,
        format_label_column=list(halign = "right", col_width = 80),
        format_subtotals=list(background_color = "#b8cce4"),
        format_headers=list(background_color = "#b8cce4"),
        format_means=list(background_color = "#b8cce4"),
        format_weighted_n=list(name = "Weighted Sample", border_where = "TopBottomLeftRight", position_bottom = FALSE, position_fixed = TRUE),
        format_unweighted_n=list(name = "Unweighted Sample", font_color = "#969696", border_where = "TopBottomLeftRight", position_bottom = FALSE, position_fixed = TRUE),
        format_totals_row=NULL,
        format_min_base=list(min_base = 50, mask = NULL, decoration = "italic"),
        freeze_column=2,
        percent_format_data=FALSE)
}
