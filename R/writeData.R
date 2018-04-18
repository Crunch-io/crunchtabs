get_banner_info <- function(banner, theme){
    if (is.null(banner)) return(list(empty_col = FALSE, multicols = NA, multicols_csum = NA, 
        format_cols = 2, border_columns = NULL))
    
    empty_col <- !is.null(theme$banner_vars_split) && theme$banner_vars_split$empty_col
    banner_cols_pos <- cumsum(sapply(banner, function(x) length(x$categories))) + 1
    multicols <- sapply(banner, getNames)
    multicols_csum <- cumsum(c(banner_cols_pos[1], sapply(multicols, function(x) {length(x) + empty_col})))
    format_cols <- if (empty_col) { unlist(sapply(2:length(multicols_csum), function(i) multicols_csum[i-1]:(multicols_csum[i]-2)))
    } else { multicols_csum[[1]]:(multicols_csum[[length(multicols_csum)]] - 1 - empty_col) }
    
    border_columns <- if (empty_col) { multicols_csum[2:(length(multicols_csum)-1)]-1 
    } else { multicols_csum[2:(length(multicols_csum)-1)] }
    
    list(empty_col = empty_col, multicols = multicols, multicols_csum = multicols_csum, 
        format_cols = format_cols, border_columns = border_columns)
}

clean_data <- function(y, data) {
    if (!is.null(dim(y)) && nrow(y) == 2) {
        if (all(as.character(y[1, ]) == as.character(y[2, ]), na.rm = TRUE)) { y <- y[1, ] }
    }
    if (is.null(dim(y))) y <- t(y)
    y <- setNames(data.frame(y), colnames(data))
}

get_data <- function(data, item_name, empty_col, round){
    tmp_data <- lapply(seq_along(data), function(bv) {
        dt <- if (round) round(data[[bv]][[item_name]]) else data[[bv]][[item_name]]
        if (is.vector(dt)) return(c(dt, if (empty_col) as.numeric(NA)))
        if (!is.vector(dt) && empty_col) return(cbind(dt, as.numeric(NA)))
        if (!is.vector(dt) && !empty_col) return(dt)
    })
    if (is.null(unlist(tmp_data))) return(NULL)
    if (!is.null(dim(tmp_data[[1]])) && (nrow(tmp_data[[1]]) != 1 || ncol(tmp_data[[1]]) != 1)) { return(do.call(cbind, tmp_data)) }
    return(unlist(tmp_data))
}

munge_var <- function(var, banner_name, theme, proportions, banner_info, latex) {
    possible <- c("weighted_n", "unweighted_n", "totals_row", "means", "medians")
    if (var$settings$no_totals) possible <- setdiff(possible, "totals_row")
    if (!var$mean_median) { possible <- setdiff(possible, c("means", "medians")) }
    top <- unlist(sapply(possible, function(p) if (!is.null(theme[[paste0("format_", p)]]) && theme[[paste0("format_", p)]]$position_top) return(p)))
    bottom <- unlist(sapply(rev(possible), function(p) if (!is.null(theme[[paste0("format_", p)]]) && theme[[paste0("format_", p)]]$position_bottom) return(p)))
    data_order <- c(top, "body", bottom)
    data_list <- sapply(unique(data_order), function(dt) {
        prop_v <- gsub("_row", "", dt) %in% c("body", "totals")
        weight_v <- dt %in% c("unweighted_n", "weighted_n")
        mm_v <- dt %in% c("means", "medians")
        dx <- ifelse(prop_v, paste0("body", ifelse(proportions, "_proportions", "_counts")), dt)
        data <- get_data(data = var$crosstabs[[banner_name]], item_name = dx, 
            empty_col = banner_info$empty_col && !latex, round = FALSE)
        if (all(is.na(data))) { return(NULL) }
        if (is.vector(data)) data <- t(data)
        
        if (prop_v && proportions && (latex || !theme$percent_format_data)) {
            data[] <- data * 100
        }
        
        rdig <- ifelse(!proportions && prop_v || weight_v, 0, 
            ifelse(latex, theme$digits, ifelse(!is.null(theme$final_digits), 
                theme$final_digits + (proportions && theme$percent_format_data && prop_v)*2, Inf)))
        if (latex && prop_v && !is(var, "MultipleResponseCrossTabVar") && proportions && theme$latex_round_percentages) {
            data[] <- apply(data, 2, roundPropCategorical, theme$digits)
        } else if (!is.null(rdig) && !is.infinite(rdig)) {
            data[] <- round(data, rdig)
        }
        
        if (dt %in% "totals_row") { 
            data_tmp <- if (proportions) colSums(data) else get_data(data = var$crosstabs[[banner_name]], item_name = "totals_counts", 
                    empty_col = banner_info$empty_col && !latex, round = FALSE)
            data <- matrix(data_tmp, nrow = 1, ncol = ncol(data),
                dimnames = list(c(theme$format_totals_row$name), colnames(data)))
        }
        
        if (weight_v && nrow(data) == 2 && as.character(data[1, ]) == as.character(data[2, ])) {
            data <- matrix(data[1, ], nrow = 1, dimnames = list(c(), colnames(data)))
        }
        if (!is.null(theme[[paste0("format_", dt)]]$name)) {
            rownames(data) <- paste0(theme[[paste0("format_", dt)]]$name, 
                if (weight_v && !is.null(dim(data)) && nrow(data) == 2) c(": Min", ": Max"))
        }
        # if (var$alias %in% "trump_twitter_20180329" && dt %in% "body") {
        #     TRUE
        # }
        if (latex && prop_v) {
            data[] <- round(data, rdig)
            data[] <- format(data, nsmall=theme$digits, big.mark=",")
            data[] <- apply(data, 2, trimws)
            if (proportions) { data[] <- apply(data, 2, paste0, "%") }
        }
        if (latex && weight_v) {
            data[] <- trimws(format(data, big.mark=","))
            if (theme$latex_add_parenthesis) {
                data[] <- apply(data, 2, paste_around, "(", ")")
            }
            if (!is.null(theme$latex_adjust)) {
                data[] <- apply(data, 2, paste_around, paste0("\\multicolumn{1}{", theme$latex_adjust, "}{"), "}")
            }
        }
        
        data <- setNames(as.data.frame(data), unlist(lapply(banner_info$multicols, function(x) c(x, if (banner_info$empty_col && !latex) "empty"))))
        
        return(data)
    }, simplify = FALSE)
    
    unweighted_n <- get_data(var$crosstabs[[banner_name]], "unweighted_n_all", 
        empty_col = banner_info$empty_col && !latex, round = FALSE)
    if (is.null(theme$format_headers) && any(var$inserts %in% "Heading")){
        data_list$body <- data_list$body[-c(which(var$inserts %in% "Heading")),]
        unweighted_n <- unweighted_n[-c(which(var$inserts %in% "Heading")),]
        var$inserts <- var$inserts[-c(which(var$inserts %in% "Heading"))]
    }
    if (is.null(theme$format_subtotals) && any(var$inserts %in% "Subtotal")){
        data_list$body <- data_list$body[-c(which(var$inserts %in% "Subtotal")),]
        unweighted_n <- unweighted_n[-c(which(var$inserts %in% "Subtotal")),]
        var$inserts <- var$inserts[-c(which(var$inserts %in% "Subtotal"))]
    }
    
    mask_vars <- c("totals_row", "means", "medians")
    min_cell <-  matrix(suppressWarnings(as.numeric(as.character(unweighted_n))) < theme$format_min_base$min_base, nrow = nrow(unweighted_n), ncol = ncol(unweighted_n))
    min_cell_rep <- suppressWarnings(as.numeric(as.character(apply(unweighted_n, 2, min, na.rm=TRUE)))) < theme$format_min_base$min_base
    top_sub <- mask_vars %in% top
    min_cell_top <- if (any(top_sub)) matrix(min_cell_rep, nrow = sum(top_sub), ncol = ncol(unweighted_n))
    bottom_sub <- mask_vars %in% bottom
    min_cell_bottom <- if (any(bottom_sub)) matrix(min_cell_rep, nrow = sum(bottom_sub), ncol = ncol(unweighted_n))

    if (latex && !is.null(theme$format_min_base$mask)) {
        for (x in intersect(data_order, mask_vars)) {
            data_list[[x]][,min_cell_rep] <- theme$format_min_base$mask
        }
        data_list$body[min_cell] <- theme$format_min_base$mask
    }
    
    if (is(var, "ToplineCategoricalArray") && latex) {
        data_list <- lapply(data_list, function(x) {
            names(x) <- var$subnames
            t(x)
        })
    }
    
    return(list(top = top, bottom = bottom, data_order = data_order, inserts = var$inserts, data_list = data_list, 
        min_cell_top = min_cell_top, min_cell_body = min_cell, min_cell_bottom = min_cell_bottom))
}

var_header <- function(var, theme) {
    if_there <- function(str) { if (!(is.null(str) || is.na(str) || str == "")) return(str) }
    var_info <- list(format_var_alias = if_there(getAlias(var)),
        format_var_name = if_there(getName(var)),
        format_var_description = if_there(getDescription(var)),
        format_var_filtertext = if_there(getNotes(var)),
        format_var_subname = if_there(var$subname))
    number <- if_there(var$settings$number)
    var_info2 <- list()
    for (info_name in intersect(names(theme), names(var_info))){
        if (!is.null(theme[[info_name]]) && !is.null(var_info[[info_name]])) {
            if (is.null(theme[[info_name]]$repeat_for_subs) || theme[[info_name]]$repeat_for_subs || var$subnumber %in% 1){
                var_info2[[info_name]] <- var_info[[info_name]]
                if (!is.null(theme[[info_name]]$include_q_number) && theme[[info_name]]$include_q_number){
                    var_info2[[info_name]] <- paste0(number, ". ", var_info2[[info_name]])
                }
                if (!is.null(theme[[info_name]]$include_alias) && theme[[info_name]]$include_alias){
                    var_info2[[info_name]] <- paste0(var_info$format_var_alias, " - ", var_info2[[info_name]])
                }
            }
        }
    }
    return(var_info2)
}


