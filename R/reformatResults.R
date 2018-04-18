
reformatResults <- function(x, proportions = TRUE, theme, reformat = TRUE, details = FALSE) {
    UseMethod("reformatResults", x)
}

#' @export
reformatResults.default <- function(x, proportions = TRUE, theme, reformat = TRUE, details = FALSE) {
    stop("`reformatResults` generic function doesn't support objects of type: ",
        collapse_items(class(x)))
}

#' @export
reformatResults.ToplineBase <- function(x, proportions = TRUE, theme, reformat = TRUE, details = FALSE) {
    reformatResultsGen(x, proportions = proportions, theme = theme,
        reformat = reformat, details = details)
}

#' @export
reformatResults.CrossTabVar <- function(x, proportions = TRUE, theme, reformat = TRUE, details = FALSE) {
    reformatResultsGen(x, proportions = proportions, theme = theme,
        reformat = reformat, details = details)
}

#' @export
reformatResults.ToplineNumeric <- function(x, proportions = TRUE, theme, reformat = TRUE, details = FALSE) {
    reformatResultsGen(x, proportions = proportions, theme = theme,
        reformat = reformat, details = details)
}

#' @export
reformatResults.CrossTabBannerVar <- function(x, proportions = TRUE, theme, reformat = TRUE, details = FALSE) {
    reformatResultsGen(x, proportions = proportions, theme = theme,
        reformat = reformat, details = details)
}

#' @importFrom methods is
reformatResultsGen <- function(x, proportions, theme, reformat, details) {
    data <- as.matrix(getResults(x, proportions = proportions, details = details))
    data[is.nan(data)] <- 0
    if (theme$digits > -1 && reformat) {
        if (!proportions || is(x, "ToplineMultipleResponse") || !theme$latex_round_percentages) {
            data[] <- round(data * if (proportions) 100 else 1, theme$digits)
        } else if (is(x, "ToplineCategoricalArray")) {
            data <- t(apply(t(data), 1, roundPropCategorical, theme$digits))
        } else {
            data[] <- roundPropCategorical(data, theme$digits)
        }
    }
    if (proportions && reformat) {
        data[] <- paste0(data, "%")
    }
    data
}


roundPropCategorical <- function(data, digits = 0) {
    # data <- data * 100
    rounded <- round(data, digits)
    while (sum(rounded) > 100) {
        sgn <- sign(sum(rounded) - 100)
        index <- which.max((rounded - data) * sgn)
        rounded[index] <- rounded[index] - sgn * 10^(-digits)
    }
    return(rounded)
}


roundPropCategoricalArray <- function(data, digits) {
    t(apply(data, 1, roundPropCategorical, digits))
}


roundPropCrosstabs <- function(data, digits) {
    apply(data, 2, roundPropCategorical, digits)
}


reformatResultsCrossTabBannerVar <- function(x, banner_var = NULL, proportions = TRUE,
    show_totals = TRUE, theme) {

    data <- getResults(x, proportions = proportions)
    data[is.nan(data)] <- 0
    bottom <- NULL

    n_data <- NULL
    if (!is.null(theme$format_weighted_n)) {
        weighted_n_data <- clean_data(x$totals_counts, data)
        rownames(weighted_n_data) <- paste0(theme$format_weighted_n$name, if (nrow(weighted_n_data) == 2) c(": Min", ": Max"))
        n_data <- rbind(n_data, weighted_n_data)
    }
    if (!is.null(theme$format_unweighted_n)) {
        unweighted_n_data <- clean_data(x$unweighted_n, data)
        rownames(unweighted_n_data) <- paste0(theme$format_unweighted_n$name, if (nrow(unweighted_n_data) == 2) c(": Min", ": Max"))
        n_data <- rbind(n_data, unweighted_n_data)
    }

    if (theme$digits > -1) {
        if (!proportions || !theme$latex_round_percentages || is(var, "MultipleResponseCrossTabVar")) {
            data[] <- round(data * if (proportions) 100 else 1, theme$digits)
        }
        else {
            data[] <- roundPropCrosstabs(data, theme$digits)
        }
    }
    if (show_totals) {
        data <- rbind(data, if (proportions) colSums(data) else x$totals_counts)
        rownames(data)[nrow(data)] <- "Totals"
    }

    if (theme$digits > -1) {
        data[] <- format(data, nsmall=theme$digits, big.mark=",")
        n_data[] <- round(n_data, 0)
        n_data[] <- format(n_data, nsmall=theme$digits, big.mark=",")
    }
    if (proportions) {
        data[] <- paste0(data, "%")
    }
    min_cell_mask <- x$unweighted_n_all < theme$format_min_base$min_base
    for (xi in which(colSums(min_cell_mask) != 0)) {
        data[min_cell_mask[,xi], xi] <- theme$format_min_base$mask
    }

    if (theme$latex_add_parenthesis) {
        for (xi in seq_along(n_data)) {
            n_data[,xi] <- paste0("(", n_data[,xi], ")")
        }
    }
    if (!is.null(theme$latex_adjust)) {
        for (xi in seq_along(n_data)) {
            n_data[,xi] <- paste0("\\multicolumn{1}{", theme$latex_adjust, "}{", n_data[,xi], "}")
        }
    }

    if (show_totals){
        bottom <- setNames(data.frame(rbind(bottom, "Totals"=c(data[nrow(data),]))), colnames(data))
        data <- data[-nrow(data),]
    }
    bottom <- rbind(bottom, n_data)

    return(list(data=data, bottom=bottom))
}

reformatCrosstabsResults <- function(x, banner = NULL, proportions = TRUE, theme) {
    lapply(x, function(var) {
        var$crosstabs <- sapply(names(var$crosstabs), function(banner_name) {
            lapply(seq_along(var$crosstabs[[banner_name]]), function(banner_var_ind) {
                banner_var <- banner[[banner_name]][[banner_var_ind]]
                cross_tab_banner_var <- var$crosstabs[[banner_name]][[banner_var_ind]]
                reformatResultsCrossTabBannerVar(cross_tab_banner_var, banner_var, proportions = proportions,
                    show_totals = !var$settings$no_totals && !is.null(theme$format_totals_row),
                    theme = theme)
            })
        }, simplify = FALSE)
        var
    })
}


reformatHypothesisTest <- function(x) {
    sapply(x, function(var) {
        var$crosstabs <- sapply(names(var$crosstabs), function(banner_name) {
            sapply(names(var$crosstabs[[banner_name]]), function(banner_var_name) {
                var$crosstabs[[banner_name]][[banner_var_name]]$pvals_col <-
                    compute_pvals(var$crosstabs[[banner_name]][[banner_var_name]]$counts_unweighted,
                        var$crosstabs[[banner_name]][[banner_var_name]]$counts_unweighted)
                var$crosstabs[[banner_name]][[banner_var_name]]
            }, simplify = FALSE)
        }, simplify = FALSE)
        var
    }, simplify = FALSE)
}


flattenBannerResults <- function(x) {
    lapply(x, function(var) {
        var$crosstabs <- flattenBanner(var$crosstabs)
        var
    })
}

flattenBanner <- function(x) {
    unlist(lapply(seq_along(x), function(banner_id) {
        if (banner_id == 1) {
            x[[banner_id]]
        } else {
            x[[banner_id]][2:length(x[[banner_id]])]
        }
    }), recursive = FALSE)
}

mergeBannerResults <- function(x, banner_name = NULL) {
    lapply(x, function(var) {
        var$crosstabs <- mergeBanner(var$crosstabs, banner_name = banner_name)
        var
    })
}

mergeBanner <- function(x, banner_name = NULL) {
    res <- list(unlist(lapply(seq_along(x), function(banner_id) {
        if (banner_id == 1) {
            x[[banner_id]]
        } else {
            x[[banner_id]][2:length(x[[banner_id]])]
        }
    }), recursive = FALSE))
    if (!is.null(banner_name)) {
        names(res) <- banner_name
    }
    res
}


bannerDataRecode <- function(b_table, b_recode) {
    # if (is.null(dim(b_table))) b_table <- matrix(b_table, nrow=1, dimnames = list(c(), names(b_table))) ##  -- added 20180123
    names_mask <- (b_recode$old_categories %in% colnames(b_table)) & !is.na(b_recode$categories_out)
    b_table <- b_table[, colnames(b_table) %in% b_recode$old_categories[names_mask],
        drop = FALSE]
    colnames(b_table) <- b_recode$categories_out[names_mask]
    # b_table <- sapply(b_recode$categories, function(x) {
    #   rowSums(b_table[, colnames(b_table) == x, drop = FALSE])
    # })
    b_table
}



reformatCodebookResults <- function(x, digits = 0, reformat = FALSE, round_percentages = FALSE, details = TRUE) {
    UseMethod("reformatCodebookResults", x)
}

#' @export
reformatCodebookResults.default <- function(x, digits = 0, reformat = FALSE,
    round_percentages = FALSE, details = TRUE) {
    stop("`reformatCodebookResults` generic function doesn't support objects of type:",
        collapse_items(class(x)))
}

#' @export
reformatCodebookResults.ToplineCategoricalGeneral <- function(x, digits = 0, reformat = FALSE,
    round_percentages = FALSE, details = TRUE) {
    x <- setResults(x, reformatResultsGen(x, proportions = FALSE, digits = digits, reformat = reformat,
        round_percentages = round_percentages, details = details),
        proportions = FALSE, details = details)
    
    x <- setResults(x, reformatResultsGen(x, proportions = TRUE, digits = digits, reformat = reformat,
        round_percentages = round_percentages, details = details),
        proportions = TRUE, details = details)
    x
}

#' @export
reformatCodebookResults.ToplineNumeric <- function(x, digits = 0, reformat = FALSE,
    round_percentages = FALSE, details = TRUE) {
    x <- setResults(x, reformatResultsGen(x, digits = digits, details = details),
        details = details)
    x
}

#' @export
reformatCodebookResults.ToplineBase <- function(x, digits = 0, reformat = FALSE,
    round_percentages = FALSE, details = TRUE) {
    x
}

reformatLatexResults <- function(data_summary, proportions, theme) {
    banner <- data_summary$banner
    banner_names <- if (is.null(banner)) "Results" else names(banner)
    banner_info <- sapply(banner_names, function(bn) get_banner_info(banner[[bn]], theme = theme), 
        simplify = FALSE)
    
    return(lapply(data_summary$results, function(x)
        sapply(banner_names, function(bn) 
            munge_var(var = x, banner_name = bn, theme = theme, proportions = proportions, 
                banner_info = banner_info[[bn]], latex = TRUE), simplify = FALSE)))
}

