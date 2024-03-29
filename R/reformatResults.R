#' Rounds a proportions
#'
#' Round a vector by number of digits. If the rounding
#' result is greater than 100, rescale to 100
#'
#' @param data A numeric vector or CrunchCube object
#' @param digits The number of trailing digits
roundPropCategorical <- function(data, digits = 0) {
  rounded <- round(data, digits)
  while (sum(rounded, na.rm = TRUE) > 100) {
    sgn <- sign(sum(rounded, na.rm = TRUE) - 100)
    index <- which.max((rounded - data) * sgn)
    rounded[index] <- rounded[index] - sgn * 10^(-digits)
  }
  return(rounded)
}

#' Create default banner
#'
#' Creates a banner for use with \link{reformatLatexResults}
default_banner <- function() {
  list(
    Results = list(
      empty_col = FALSE,
      multicols = NA,
      multicols_csum = NA,
      format_cols = 2,
      border_columns = NULL
    )
  )
}

#' Banner Meta Data
#'
#' Create a list containing meta data regarding the banner allowing for
#' latex placement decisions to be made.
#'
#' @details Meta data includes the following as a list:
#' * empty_col A logical identifying if the column is empty
#' * len A vector identifying the length of the category responses
#' * mulitcols A vector of categories
#' * multicols_csum A cumulative sum of columns
#' * format_cols Numeric reference to which columns are to be formatted
#' * border_cols Numeric reference to which columns should have a border
#' * names The names of the columns to be formatted
#'
#' @md
#' @param banner A banner from \link{banner}
#' @param theme A crunchtabs theme object
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
    format_cols <- unlist(
      sapply(2:length(multicols_csum), function(i) multicols_csum[i - 1]:(multicols_csum[i] - 2)))
    border_columns <- multicols_csum[2:(length(multicols_csum) - 1)] - 1
  } else {
    format_cols <- multicols_csum[[1]]:(multicols_csum[[length(multicols_csum)]] - 1 - empty_col)
    border_columns <- multicols_csum[2:(length(multicols_csum) - 1)]
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

#' get Item Data for print
#'
#' Retrieves item data and prepares it for printing
#'
#' @param data A list or data frame
#' @param item_name A string identifying the item
#' @param empty_col A logical identifying if the column is empty
#' @param round A logical identifying if the item should be rounded
getItemData <- function(data, item_name, empty_col, round) {
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

#' Reformat variables for printing
#'
#' This function prepares data for placement in tex. By taking the following actions:
#'
#' * Adding totals_row
#' * Adding weighted/un-weighted base rows
#' * Placing table content appropriately
#'
#' @md
#' @param var A crunch variable
#' @param banner_name A string identfying the name of the banner
#' @param theme A theme object created by \link{themeNew}
#' @param proportions Should proportions be displayed or counts? One of "proportions" or "counts"
#' @param banner_info A meta data object from \link{getBannerInfo}
#' @param latex A logical identifying if this code is for LaTex or Excel. If TRUE, LaTeX
reformatVar <- function(var, banner_name, theme, proportions, banner_info, latex) {

  # nolint start

  # if (!identical(names(var$crosstabs[[1]]), names(banner_info$names))) {
  #   return(NULL) #
  # } # could be used in a future where we chain different banners

  possible <- c("weighted_n", "unweighted_n", "totals_row", "means", "medians")
  if (var$no_totals) {
    possible <- setdiff(possible, "totals_row")
  }
  if (!var$mean_median) {
    possible <- setdiff(possible, c("means", "medians"))
  }
  top <- unlist(
    sapply(possible, function(p) {
      if (!is.null(theme[[paste0("format_", p)]]) && theme[[paste0("format_", p)]]$position_top) {
        return(p)
      }
    })
  )
  bottom <- unlist(sapply(rev(possible), function(p) {
    if (!is.null(theme[[paste0("format_", p)]]) && theme[[paste0("format_", p)]]$position_bottom) {
      return(p)
    }
  }))
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

  for (dt in unique(data_order)) {
    prop_v <- gsub("_row", "", dt) %in% c("body", "totals")
    weight_v <- dt %in% c("unweighted_n", "weighted_n")

    dx <- piece_names[[dt]]
    data <- getItemData(
      data = var$crosstabs[[banner_name]], item_name = dx,
      empty_col = banner_info$empty_col && !latex, round = FALSE
    )
    if (is.vector(data)) {
      data <- t(data)
    }
    theme_dt <- theme[[paste0("format_", dt)]]

    data[is.nan(data)] <- NA

    # if (prop_v) {
    #   data[is.na(data)] <-
    # }

    if (prop_v && proportions && (latex || !theme$excel_percent_sign)) {
      data[] <- data * 100
    }

    if (!proportions && prop_v || weight_v) {
      rdig <- 0
    } else if (latex) {
      rdig <- theme$digits
    } else if (!is.null(theme$digits_final)) {
      rdig <- theme$digits_final + (proportions && theme$excel_percent_sign && prop_v) * 2
    } else {
      rdig <- Inf
    }

    should_round <- (latex && prop_v && !is(var, "MultipleResponseCrossTabVar") &&
      proportions && theme$latex_round_percentages)
    should_round <- ifelse(var$alias %in% theme$latex_round_percentages_exception,
      !should_round, should_round
    )

    # Calculate tabInsertions before rounding!
    if (var$type %in% c("categorical", "categorical_array") && dt %in% "body" &&
        any(var$inserts %in% c("Heading", "Subtotal"))) {
      data <- as.matrix(calcTabInsertions(data, var$inserts_obj, var$categories))
    }

    if (should_round & dt != "weighted_n") {
      data[] <- apply(data, 2, roundPropCategorical, theme$digits)
    } else if (!is.null(rdig) && !is.infinite(rdig)) {
      data[] <- round(data, rdig)
    } else {
      data[] <- round(data, rdig)
    }

    if (dt %in% "totals_row") {
      if (proportions) {
        data_tmp <- colSums(data)
        if (theme$enforce_onehundred) {
          data_tmp[data_tmp < 100 | data_tmp > 100] <- 100
        }
      } else {
        data_tmp <- getItemData(
          data = var$crosstabs[[banner_name]],
          item_name = "weighted_base",
          empty_col = banner_info$empty_col && !latex,
          round = FALSE
        )
      }
      data <- matrix(data_tmp,
        nrow = 1, ncol = ncol(data),
        dimnames = list(c(theme$format_totals_row$name), colnames(data))
      )
    }

    if (weight_v && nrow(data) > 1) {
      data <- rbind(apply(data, 2, min, na.rm = TRUE), apply(data, 2, max, na.rm = TRUE))
      if (all(data[1, ] == data[2, ])) {
        data <- data[1, , drop = FALSE]
      }
    }
    if (!is.null(theme_dt$name)) {
      rownames(data) <- paste0(
        theme_dt$name,
        if (weight_v && !is.null(dim(data)) && nrow(data) == 2) c(": Min", ": Max")
      )
    }

    data <- setNames(
      as.data.frame(data, stringsAsFactors = FALSE),
      unlist(lapply(
        banner_info$multicols,
        function(x) c(x, if (banner_info$empty_col && !latex) "empty")
      ))
    )

    data_list[[dt]] <- data
  }

  unweighted_n <- getItemData(var$crosstabs[[banner_name]], "base",
    empty_col = banner_info$empty_col && !latex, round = FALSE
  )
  if (any(var$inserts %in% c("Heading", "Subtotal"))) {
    unweighted_n <- as.matrix(unweighted_n[rep(1, length(var$inserts)), ],
      nrow = length(var$inserts),
      ncol = ncol(unweighted_n), byrow = TRUE
    )
    unweighted_n[var$inserts %in% "Heading", ] <- NA
  }

  # if (is.null(theme$format_min_base$min_base)) theme$format_min_base$min_base <- 0
  mask_vars <- c("totals_row", "means", "medians")
  min_cell <- matrix(suppressWarnings(as.numeric(as.character(unweighted_n))) <
    theme$format_min_base$min_base, nrow = nrow(unweighted_n), ncol = ncol(unweighted_n))
  min_cell_rep <- colSums(min_cell, na.rm = TRUE) > 0
  top_sub <- mask_vars %in% top
  if (any(top_sub)) {
    min_cell_top <- matrix(
      min_cell_rep,
      nrow = sum(top_sub), ncol = ncol(unweighted_n), byrow = TRUE
    )
  } else {
    min_cell_top <- NULL
  }
  bottom_sub <- mask_vars %in% bottom
  if (any(bottom_sub)) {
    min_cell_bottom <- matrix(
      min_cell_rep,
      nrow = sum(bottom_sub), ncol = ncol(unweighted_n), byrow = TRUE
    )
  } else {
    min_cell_bottom <- NULL
  }
  if (is(var, "ToplineCategoricalArray") && latex) {
    if (all(rownames(data_list$body) == as.character(seq_len(nrow(data_list$body))))) {
      # Even though these are ignored
      rownames(data_list$body) <- sapply(var$inserts_obj, name)
    }

    if (suppressWarnings(all(rownames(data_list$body) == as.character(var$subnames)))) {
      names(data_list$body) <- var[["labels"]]
    } else {
      names(data_list$body) <- var[["subnames"]]
    }

    # We only keep the body for arrays
    data_list <- data_list["body"]
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
  # nolint end
}

#' Get Variable Info
#'
#' Given a crunch variable and a theme object
#' prepare a list of meta data to package or
#' subset from the variable for use in the
#' application of the theme.
#'
#' * alias
#' * name
#' * description
#' * filtertext
#' * subname
#' @md
#' @param var The crunch variable
#' @param theme The theme object from \link{themeNew}
getVarInfo <- function(var, theme) {
  # nolint start
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

  if (is.null(var_info$format_var_description)) {
    var_info$format_var_description <- var_info$format_var_name
  }

  number <- if_there(var[["number"]])
  var_info2 <- list()

  for (info_name in intersect(names(theme), names(var_info))) {
    if (!is.null(theme[[info_name]]) && (var$type != "categorical_array" ||
      (is.null(theme[[info_name]]$repeat_for_subs) ||
        theme[[info_name]]$repeat_for_subs ||
        var$subnumber %in% 1))) {
      var_info2[[info_name]] <- var_info[[info_name]]
      if (!is.null(theme[[info_name]]$include_alias) && theme[[info_name]]$include_alias) {
        var_info2[[info_name]] <- paste0(
          c(var_info$format_var_alias, var_info2[[info_name]]), collapse = " -- ")
      }
      if (!is.null(theme[[info_name]]$include_q_number) && theme[[info_name]]$include_q_number) {
        var_info2[[info_name]] <- paste0(number, ". ", var_info2[[info_name]])
      }
    }
  }
  return(var_info2)
  # nolint end
}

#' Remove Inserts
#'
#' Manually drop out Subtotal and Headers from
#' inserts
#'
#'
#' @param var A summarized crunch variable
#' @param theme A theme object from \link{themeNew}
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

#' Apply reformats
#'
#' A wrapper for reformatVar that
#' loops over each of the banners.
#'
#' @param result A summarized crunchtab variable
#' @param banner A crunchtabs \link{banner}
#' @param theme A crunchtabs theme object from \link{themeNew}
reformatLatexResults <- function(result, banner, theme) {
  if (is.null(banner)) {
    banner_info <- default_banner()
  } else {
    banner_info <- lapply(banner, getBannerInfo, theme = theme)
  }

  results <- list()
  for (bn in names(banner_info)) {
    results[[bn]] <- reformatVar(
      var = result,
      banner_name = bn,
      theme = theme,
      proportions = theme$proportions,
      banner_info = banner_info[[bn]],
      latex = TRUE
    )
  }

  return(results)
}
