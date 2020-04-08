
roundPropCategorical <- function(data, digits = 0) {
    rounded <- round(data, digits)
    while (sum(rounded) > 100) {
        sgn <- sign(sum(rounded) - 100)
        index <- which.max((rounded - data) * sgn)
        rounded[index] <- rounded[index] - sgn * 10^(-digits)
    }
    return(rounded)
}

default_banner <- list(
    Results=list(
        empty_col = FALSE,
        multicols = NA,
        multicols_csum = NA,
        format_cols = 2,
        border_columns = NULL
    )
)

getBannerInfo <- function(banner, theme) {
    if (is.null(banner)) {
        return(default_banner)
    }
    empty_col <- !is.null(theme$format_banner_split) && theme$format_banner_split$empty_col
    len <- sapply(banner, function(x) length(x$categories))
    banner_cols_pos <- cumsum(len) + 1
    multicols <- sapply(banner, function(x) x$categories[!is.na(x$categories)])
    multicols_csum <- cumsum(c(banner_cols_pos[1], sapply(multicols, function(x) {
        length(x) + empty_col
    })))
    if (empty_col) {
        format_cols <- unlist(sapply(2:length(multicols_csum), function(i) multicols_csum[i-1]:(multicols_csum[i]-2)))
        border_columns <- multicols_csum[2:(length(multicols_csum)-1)] - 1
    } else {
        format_cols <- multicols_csum[[1]]:(multicols_csum[[length(multicols_csum)]] - 1 - empty_col)
        border_columns <- multicols_csum[2:(length(multicols_csum)-1)]

    }
    names <- sapply(banner, getName)

    list(
        empty_col = empty_col,
        len = len,
        multicols = multicols,
        multicols_csum = multicols_csum,
        format_cols = format_cols,
        border_columns = border_columns,
        names = names
    )
}

getItemData <- function(data, item_name, empty_col, round){
    tmp_data <- lapply(data, function(bv) {
        dt <- bv[[item_name]]
        if (round) {
            dt <- round(dt)
        }
        if (empty_col) {
            if (is.vector(dt)) {
                dt <- c(dt, NA_real_)
            } else {
                dt <- cbind(dt, NA_real_)
            }
        }
        return(dt)
    })
    if (is.null(unlist(tmp_data))) {
        return(NULL)
    } else if (length(dim(tmp_data[[1]])) == 2) {
        return(do.call(cbind, tmp_data))
    } else {
        return(unlist(tmp_data))
    }
}

reformatVar <- function(var, banner_name, theme, proportions, banner_info, latex) {
    possible <- c("weighted_n", "unweighted_n", "totals_row", "means", "medians")
    if (var$no_totals) {
        possible <- setdiff(possible, "totals_row")
    }
    if (!var$mean_median) {
        possible <- setdiff(possible, c("means", "medians"))
    }
    top <- unlist(sapply(possible, function(p) if (!is.null(theme[[paste0("format_", p)]]) && theme[[paste0("format_", p)]]$position_top) return(p)))
    bottom <- unlist(sapply(rev(possible), function(p) if (!is.null(theme[[paste0("format_", p)]]) && theme[[paste0("format_", p)]]$position_bottom) return(p)))
    data_order <- c(top, "body", bottom)
    piece_names <- list(
        "body" = ifelse(proportions, "proportions", "counts"),
        "totals_row" = ifelse(proportions, "proportions", "counts"),
        "weighted_n" = "weighted_base",
        "unweighted_n" = "base",
        "means" = "mean",
        "medians" = "median"
    )

    data_list <- list() 
    
    for(dt in unique(data_order)) {
        prop_v <- gsub("_row", "", dt) %in% c("body", "totals")
        weight_v <- dt %in% c("unweighted_n", "weighted_n")
        mm_v <- dt %in% c("means", "medians")
        dx <- piece_names[[dt]]
        data <- getItemData(data = var$crosstabs[[banner_name]], item_name = dx,
                            empty_col = banner_info$empty_col && !latex, round = FALSE)
        if (is.vector(data)) {
            data <- t(data)
        }
        theme_dt <- theme[[paste0("format_", dt)]]
        
        data[is.nan(data)] <- NA
        
        if (prop_v) {
            data[is.na(data)] <- 0
        }
        
        if (prop_v && proportions && (latex || !theme$excel_percent_sign)) {
            data[] <- data * 100
        }
        
        if (!proportions && prop_v || weight_v) {
            rdig <- 0
        } else if (latex) {
            rdig <- theme$digits
        } else if (!is.null(theme$digits_final)) {
            rdig <- theme$digits_final + (proportions && theme$excel_percent_sign && prop_v)*2
        } else {
            rdig <- Inf
        }
        
        should_round <- (latex && prop_v && !is(var, "MultipleResponseCrossTabVar") && 
                         proportions && theme$latex_round_percentages)
        should_round <- ifelse(var$alias %in% theme$latex_round_percentages_exception, 
                               !should_round, should_round)
        if (should_round & dt !="weighted_n") {
            # if (latex && prop_v && !is(var, "MultipleResponseCrossTabVar") && proportions && theme$latex_round_percentages) {
            #     if (is.null(theme$latex_round_percentage_exception) || !var$alias %in% theme$latex_round_percentage_exception) {
            data[] <- apply(data, 2, roundPropCategorical, theme$digits)
            #     }
        } else if (!is.null(rdig) && !is.infinite(rdig)) {
            data[] <- round(data, rdig)
        } else {
            data[] <- round(data, rdig)
        }
        
        if (dt %in% "totals_row") {
            if (proportions) {
                data_tmp <- colSums(data)
            } else {
                data_tmp <-  getItemData(
                    data = var$crosstabs[[banner_name]],
                    item_name = "weighted_base",
                    empty_col = banner_info$empty_col && !latex,
                    round = FALSE
                )
            }
            data <- matrix(data_tmp, nrow = 1, ncol = ncol(data),
                           dimnames = list(c(theme$format_totals_row$name), colnames(data)))
        }
        if (var$type %in% c("categorical", "categorical_array") && dt %in% "body" &&
            any(var$inserts %in% c("Heading", "Subtotal"))) {
            data <- as.matrix(calcTabInsertions(data, var$inserts_obj, var$categories))
        }
        
        if (weight_v && nrow(data) > 1) {
            data <- rbind(apply(data, 2, min, na.rm = TRUE), apply(data, 2, max, na.rm = TRUE))
            if (all(data[1, ] == data[2, ])) {
                data <- data[1, , drop = FALSE]
            }
        }
        if (!is.null(theme_dt$name)) {
            rownames(data) <- paste0(theme_dt$name,
                                     if (weight_v && !is.null(dim(data)) && nrow(data) == 2) c(": Min", ": Max"))
        }
        
        data <- setNames(as.data.frame(data, stringsAsFactors = FALSE),
                         unlist(lapply(banner_info$multicols, function(x) c(x, if (banner_info$empty_col && !latex) "empty"))))
        
        data_list[[dt]] = data
    }

    unweighted_n <- getItemData(var$crosstabs[[banner_name]], "base",
        empty_col = banner_info$empty_col && !latex, round = FALSE)
    if (any(var$inserts %in% c("Heading", "Subtotal"))){
        unweighted_n <- as.matrix(unweighted_n[rep(1, length(var$inserts)), ], nrow = length(var$inserts),
            ncol = ncol(unweighted_n), byrow = TRUE)
        unweighted_n[var$inserts %in% "Heading", ] <- NA
    }

    # if (is.null(theme$format_min_base$min_base)) theme$format_min_base$min_base <- 0
    mask_vars <- c("totals_row", "means", "medians")
    min_cell <-  matrix(suppressWarnings(as.numeric(as.character(unweighted_n))) <
            theme$format_min_base$min_base, nrow = nrow(unweighted_n), ncol = ncol(unweighted_n))
    min_cell_rep <- colSums(min_cell, na.rm = TRUE) > 0
    top_sub <- mask_vars %in% top
    if (any(top_sub)) {
        min_cell_top <- matrix(min_cell_rep, nrow = sum(top_sub), ncol = ncol(unweighted_n), byrow = TRUE)
    } else {
        min_cell_top <- NULL
    }
    bottom_sub <- mask_vars %in% bottom
    if (any(bottom_sub)) {
        min_cell_bottom <- matrix(min_cell_rep, nrow = sum(bottom_sub), ncol = ncol(unweighted_n), byrow = TRUE)
    } else {
        min_cell_bottom <- NULL
    }
    if (is(var, "ToplineCategoricalArray") && latex) {
        rownames(data_list$body) <- sapply(var$inserts_obj, name) # Even though these are ignored
        names(data_list$body) <- var[["subnames"]]
        data_list <- data_list["body"] # We only keep the body for arrays
        rownames <- var[["subnames"]]
    } else {
        rownames <- unlist(lapply(data_list, rownames), use.names = FALSE)
    }
    
    return(
        structure(
            list(
                top = top,
                bottom = bottom,
                data_order = data_order,
                inserts = var$inserts,
                data_list = data_list,
                min_cell_top = min_cell_top,
                min_cell_body = min_cell,
                min_cell_bottom = min_cell_bottom,
                min_cell = min_cell_rep,
                rownames = rownames
            ),
            class = class(var)
        )
    )
}

getVarInfo <- function(var, theme) {
    if_there <- function(str) {
        if (!is.null(str) && !is.na(str) && str != "") {
            return(str)
        } else {
            return(NULL)
        }
    }
    var_info <- list(
        format_var_alias = if_there(var[["alias"]]),
        format_var_name = if_there(var[["name"]]),
        format_var_description = if_there(var[["description"]]),
        format_var_filtertext = if_there(var[["notes"]]),
        format_var_subname = if_there(var[["subname"]])
    )
    number <- if_there(var[["number"]])
    var_info2 <- list()
    for (info_name in intersect(names(theme), names(var_info))) {
        if (!is.null(theme[[info_name]]) && (var$type != "categorical_array" ||
                (is.null(theme[[info_name]]$repeat_for_subs) ||
                        theme[[info_name]]$repeat_for_subs ||
                        var$subnumber %in% 1))) {
            var_info2[[info_name]] <- var_info[[info_name]]
            if (!is.null(theme[[info_name]]$include_alias) && theme[[info_name]]$include_alias){
                var_info2[[info_name]] <- paste0(c(var_info$format_var_alias, var_info2[[info_name]]), collapse = " -- ")
            }
            if (!is.null(theme[[info_name]]$include_q_number) && theme[[info_name]]$include_q_number) {
                var_info2[[info_name]] <- paste0(number, ". ", var_info2[[info_name]])
            }
        }
    }
    return(var_info2)
}


removeInserts <- function(var, theme) {
    if (!is.null(var$inserts_obj)) {
        if (is.null(theme$format_subtotals)) {
            var$inserts_obj <- var$inserts_obj[sapply(var$inserts_obj, class) != "Subtotal"]
        }
        if (is.null(theme$format_headers)) {
            var$inserts_obj <- var$inserts_obj[sapply(var$inserts_obj, class) != "Headers"]
        }
        var$inserts <- sapply(var$inserts_obj, class)
    }

    return(var)
}


reformatLatexResults <- function(result, banner, theme) {
    if (is.null(banner)) {
        banner_info <- default_banner
    } else {
        banner_info <- lapply(banner, getBannerInfo, theme=theme)
    }
    
    results <- list()
    for(bn in names(banner_info)) {
        results[[bn]] = reformatVar(
            var = result, 
            banner_name = bn, 
            theme = theme, 
            proportions = theme$proportions,
            banner_info = banner_info[[bn]], 
            latex = TRUE)
    }

    return(results)
}
