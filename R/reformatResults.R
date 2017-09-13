
reformatResults <- function(x, proportions = TRUE, digits = 0, reformat = TRUE,
                            round_to_100 = FALSE) {
    UseMethod("reformatResults", x)
}

#' @export
reformatResults.default <- function(x, proportions = TRUE, digits = 0, reformat = TRUE,
                                    round_to_100 = FALSE) {
    stop(paste("The 'reformatResults' generic function doesn't support objects of type:",
        paste(class(x), collapse = ",")))
}

#' @export
reformatResults.ToplineBase <- function(x, proportions = TRUE, digits = 0,
    reformat = TRUE, round_to_100 = FALSE) {
  reformatResultsGen(x, proportions = proportions, digits = digits,
                     reformat = reformat, round_to_100 = round_to_100)
}

#' @export
reformatResults.ToplineNumeric <- function(x, proportions = TRUE, digits = 0, reformat = TRUE,
                                           round_to_100 = FALSE) {
    reformatResultsGen(x, proportions = FALSE, digits = 0, reformat = reformat)
}

#' @importFrom methods is
reformatResultsGen <- function(x, proportions = FALSE, digits = 0, reformat = TRUE,
                               round_to_100 = FALSE) {
    data <- getResults(x, proportions = proportions)
    data[is.nan(data)] <- 0
    if (digits > -1 && reformat) {
      if (!proportions || is(x, "ToplineMultipleResponse") || !round_to_100) {
        data[] <- round(data * if (proportions) 100 else 1, digits)
      }
      else if (is(x, "ToplineCategorical")) {
        data[] <- roundPropCategorical(data, digits)
      }
      else if (is(x, "ToplineCategoricalArray")) {
        data[] <- roundPropCategoricalArray(data, digits)
      }
    }
    if (proportions && reformat) {
        data[] <- paste0(data, "%")
    }
    data
}


roundPropCategorical <- function(data, digits = 0) {
  data <- data * 100
  rounded <- round(data, digits)
  while (sum(rounded) != 100) {
    sgn <- sign(sum(rounded) - 100)
    index <- which.max((rounded - data) * sgn)
    rounded[index] <- rounded[index] - sgn * 10^(-digits)
  }
  return(rounded)
}


roundPropCategoricalArray <- function(data, digits) {
  t(apply(data, 1, roundPropCategorical, digits))
}


reformatResultsCrossTabBannerVar <- function(x, banner_var = NULL, proportions = TRUE,
    digits = 0, add_parenthesis = TRUE, show_totals = TRUE, weighted_n = FALSE, latex_adjust = NULL,
    min_cell_size = NULL, min_cell_label = "*", reformat = TRUE, ...) {

    data <- getResults(x, proportions = proportions)
    if (show_totals) {
        data <- rbind(data, if (proportions) x$totals_proportions else x$totals_counts)
        rownames(data)[length(rownames(data))] <- "Totals"
    }
    data[is.nan(data)] <- 0
    if (!reformat) {
      data <- as.data.frame(data)
    }
    n_data <- if (weighted_n) x$totals_counts else x$unweighted_n
    min_cell_mask <- NULL
    if (!is.null(min_cell_size)) {
      min_cell_mask <- n_data < min_cell_size
    }
    if (digits > -1 && reformat) {
        data[] <- format(round(data * if (proportions) 100 else 1, digits), nsmall=digits, big.mark=",")
        n_data[] <- round(n_data, digits)
        n_data[] <- format(n_data, nsmall=digits, big.mark=",")
    }
    if (proportions && reformat) {
        data[] <- paste0(data, "%")
    }
    if (any(min_cell_mask)) {
        data[, min_cell_mask] <- min_cell_label
    }
    if (add_parenthesis && reformat) {
        n_data[] <- paste0("(", n_data, ")")
    }
    if (!is.null(latex_adjust)) {
        n_data[] <- paste0("\\multicolumn{1}{", latex_adjust, "}{", n_data, "}")
    }

    data <- rbind(data, n_data)
    rownames(data)[length(rownames(data))] <- if (weighted_n)
        "Weighted N" else "Unweighted N"
    return(data)
}

reformatCrosstabsResults <- function(x, banner = NULL, proportions = TRUE,
    digits = 0, add_parenthesis = FALSE, show_totals = TRUE, weighted_n = FALSE, latex_adjust = NULL,
    min_cell_size = NULL, min_cell_label = "*", reformat = TRUE, ...) {
    lapply(x, function(var) {
        var$crosstabs <- sapply(names(var$crosstabs), function(banner_name) {
            lapply(seq_along(var$crosstabs[[banner_name]]), function(banner_var_ind) {
                banner_var <- banner[[banner_name]][[banner_var_ind]]
                cross_tab_banner_var <- var$crosstabs[[banner_name]][[banner_var_ind]]
                reformatResultsCrossTabBannerVar(cross_tab_banner_var, banner_var, proportions = proportions,
                  digits = digits, add_parenthesis = add_parenthesis, show_totals = !var$settings$no_totals & show_totals,
                  weighted_n = weighted_n, latex_adjust = latex_adjust, min_cell_size = min_cell_size,
                  min_cell_label = min_cell_label, reformat = reformat)
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
    names_mask <- (b_recode$old_categories %in% colnames(b_table)) & !is.na(b_recode$categories_out)
    b_table <- b_table[, colnames(b_table) %in% b_recode$old_categories[names_mask],
        drop = FALSE]
    colnames(b_table) <- b_recode$categories_out[names_mask]
    # b_table <- sapply(b_recode$categories, function(x) {
    #   rowSums(b_table[, colnames(b_table) == x, drop = FALSE])
    # })
    b_table
}
