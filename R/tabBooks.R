#' Create a tabBook
#'
#' Prepares the actual data for tabulation
#'
#' @importFrom crunch multitables newMultitable tabBook allVariables aliases types type crtabs prop.table margin.table bases
#' @importFrom digest digest
#' @param dataset A crunch dataset name
#' @param vars A character vector of var names that exist within the crunch dataset
#' @param banner A banner object from \link{banner}
#' @param weight A weighting variable passed to \link[crunch]{tabBook}
#' @param topline Logical identifying if this is a topline only
#' @param include_original_weighted Logical, if you have specified complex weights
#' should the original weighted variable be included or only the custom weighted version?
tabBooks <- function(dataset, vars, banner, weight = NULL, topline = FALSE, include_original_weighted = TRUE) {

  banner_flatten <- unique(unlist(banner, recursive = FALSE))
  names(banner_flatten) <- sapply(banner_flatten, function(v) v$alias)
  banner_use <- banner
  if (topline) { banner_use$Results[[2]] <- NULL }

  multitable <- getMultitable(banner_flatten, dataset)

  if (is.null(weight) | is.null(weight(dataset))) {
    default_weight <- NULL
  } else {
    default_weight <- alias(weight(dataset))
  }


  if (is.list(weight)) {
    tab_frame <- crunch::tabBookWeightSpec(dataset, weight, append_default_wt = include_original_weighted)
    tab_frame <- tab_frame[tab_frame$alias %in% vars,]

    book <- suppressWarnings(
      crunch::tabBook(
        multitable,
        dataset = dataset[unique(c(vars, unique(tab_frame$weight)))],
        weight = weight,
        output_format = "json"
      )
    )

  } else {
    if (is.null(default_weight)) {
      tab_frame <- data.frame(alias = vars, weight = NA_character_)
    } else {
      tab_frame <- data.frame(alias = vars, weight = default_weight)
    }

    book <- suppressWarnings(
      crunch::tabBook(
        multitable,
        dataset = dataset[vars],
        weight = weight,
        output_format = "json"
      )
    )

  }

  # Put tab_frame in vars order
  tab_frame <- tab_frame[
    rev(
      order(tab_frame$alias, factor(vars, levels = vars)
            )
      ),
  ]

  banner_var_names <- sapply(seq_along(book[[1]]), function(ix) {
    crunch::aliases(crunch::variables(book[[1]][[ix]]))[2] })
  banner_var_names[1] <- "___total___"
  # var_nums <- seq_len(nrow(tab_frame))
  var_nums <- setdiff(match(vars, crunch::aliases(book)), NA)

  structure(unlist(lapply(seq_along(var_nums), function(tab_frame_pos) {
    vi <- var_nums[tab_frame_pos]
    crunch_cube <- book[[vi]][[1]]

    ## Metadata
    cube_variable <- crunch::variables(crunch_cube)[1]

    if (all(is.na(tab_frame$weight))) {
      default_weighted <- TRUE
    } else {
      default_weighted <- tab_frame$weight[tab_frame_pos] == default_weight
    }

    if (default_weighted) {
      alias <- aliases(cube_variable)
    } else {
      alias <- paste0(aliases(cube_variable), "_", tab_frame$weight[tab_frame_pos])
    }

    if (alias == "total") {
      alias <- tab_frame$alias[tab_frame_pos]
      var_type <- type(dataset[[alias]])
    } else {
      var_type <- type(dataset[[aliases(cube_variable)]])
    }

    if (getOption("testing_crunchtabs", default = FALSE)) print(alias)

    is_mr_type <- var_type == "multiple_response"
    is_cat_type <- var_type %in% c("categorical", "categorical_array")
    is_array_type <- var_type == "categorical_array"
    is_toplines_array <- is_array_type && topline
    is_crosstabs_array <- is_array_type && !topline


    if (is_crosstabs_array) {
      valiases <- getSubAliases(crunch_cube)
    } else {
      valiases <- crunch::aliases(cube_variable)
      if (valiases == "total") {
        valiases <- alias
      }
    }

    if (!default_weighted) valiases <- paste0(valiases, "_", tab_frame$weight[tab_frame_pos])

    subnames <- if (is_array_type) getSubNames(crunch_cube)
    var_cats <- categories(cube_variable[[1]])
    inserts <- if (is_cat_type) {
      collateCats <- get("collateCats", envir = asNamespace("crunch"), inherits = FALSE)
      collateCats(crunch::transforms(cube_variable)[[1]]$insertions, var_cats)
    }
    show_mean_median <- is_cat_type && any(!is.na(values(na.omit(var_cats))))

    metadata <- list(
      name = names(cube_variable),
      description = crunch::descriptions(cube_variable),
      notes = crunch::notes(cube_variable),
      type = var_type,
      no_totals = is_mr_type,
      mean_median = show_mean_median,
      subnames = subnames,
      categories = var_cats,
      inserts_obj = inserts[sapply(inserts, function(x) is.null(x$missing) || !x$missing)]
    )

    pbook <- lapply(seq_along(book[[vi]]), function(vix) {
      crunch::prop.table(crunch::noTransforms(book[[vi]][[vix]]), margin = c(2, if (is_array_type) 3))
    })
    bbook <- lapply(seq_along(book[[vi]]), function(vix) {
      crunch::bases(crunch::noTransforms(book[[vi]][[vix]]), margin = c(2, if (is_array_type) 3))
    })
    cbook <- lapply(seq_along(book[[vi]]), function(vix) {
      as.array(crunch::noTransforms(book[[vi]][[vix]]))
    })
    wbbook <- lapply(seq_along(book[[vi]]), function(vix) {
      crunch::margin.table(crunch::noTransforms(book[[vi]][[vix]]), margin = c(2, if (is_array_type) 3))
    })

    names(pbook) <- names(bbook) <- names(cbook) <- names(wbbook) <- banner_var_names

    for (bi in banner_var_names) {
      if (!identical(banner_flatten[[bi]]$categories_out, banner_flatten[[bi]]$categories)) {
        pbook[[bi]] <- bannerDataRecode(pbook[[bi]], banner_flatten[[bi]])
        bbook[[bi]] <- bannerDataRecode(bbook[[bi]], banner_flatten[[bi]])
        cbook[[bi]] <- bannerDataRecode(cbook[[bi]], banner_flatten[[bi]])
        wbbook[[bi]] <- bannerDataRecode(wbbook[[bi]], banner_flatten[[bi]])
      }
    }

    sapply(valiases, function(valias) {
      ri <- which(valiases %in% valias)

      pdata <- row_data(pbook, ri, is_crosstabs_array, is_toplines_array, FALSE)
      cdata <- row_data(cbook, ri, is_crosstabs_array, is_toplines_array, FALSE)
      bdata <- row_data(bbook, ri, is_crosstabs_array, is_toplines_array, TRUE)
      wbdata <- row_data(wbbook, ri, is_crosstabs_array, is_toplines_array, TRUE)
      mndata <- lapply(cdata, function(mbook) {
        if (show_mean_median) { applyInsert(mbook, var_cats, calcTabMeanInsert) }
      })
      mddata <- lapply(cdata, function(mbook) {
        if (show_mean_median) { applyInsert(mbook, var_cats, calcTabMedianInsert) }
      })

      if (!is_mr_type) {
        bdata <- lapply(bdata, function(xi) {
          matrix(xi, nrow = nrow(pdata[[2]]), ncol = length(xi), byrow = TRUE,
                 dimnames = list(rownames(pdata[[2]]), names(xi)))
        })
      }

      structure(c(alias = valias,
                  metadata,
                  subnumber = ri,
                  subname = if (!is_toplines_array) subnames[ri],
                  number = paste0(which(var_nums %in% vi), if (is_crosstabs_array)
                    get_grid_number(ri), collapse = ""),
                  crosstabs = list(sapply(banner_use, function(bu) {
                    sapply(bu, function(bux) {
                      structure(list(
                        counts = cdata[[bux$alias]],
                        proportions = pdata[[bux$alias]],
                        base = bdata[[bux$alias]],
                        weighted_base = wbdata[[bux$alias]],
                        mean = mndata[[bux$alias]],
                        median = mddata[[bux$alias]],
                        pvals_col = NULL
                      ), class = c("CrossTabBannerVar", "list"))
                    }, simplify = FALSE, USE.NAMES = TRUE)
                  }, simplify = FALSE, USE.NAMES = TRUE))),
                class = c(if (is_mr_type) "MultipleResponseCrossTabVar",
                          if (is_toplines_array) "ToplineCategoricalArray",
                          if (topline) "ToplineVar", "CrossTabVar"))
    }, simplify = FALSE)
  }), recursive = FALSE), class = c(if (topline) "ToplineResults", "CrosstabsResults", "list"))
}

#' Get Multitable
#'
#' Given a Banner object and a dataset, find or create the corresponding Crunch multitable
#'
#' @param banner_flatten A banner object from \link{banner}
#' @param dataset A CrunchDataset from \link[crunch]{loadDataset}
getMultitable <- function(banner_flatten, dataset) {
  mtvars <- paste0("`", setdiff(names(banner_flatten), "___total___"), "`")
  mt_name <- substr(digest::digest(sort(mtvars), "md5"), 1, 15)
  multitable <- multitables(dataset)[[mt_name]]
  if (is.null(multitable)) {
    multitable <- newMultitable(paste("~", paste(mtvars, collapse = " + ")),
                                data = dataset, name = mt_name)
  }
  return(multitable)
}

#' Banner Data Recode
#'
#' Recodes the names of categories based
#'
#' @param b_table A categorical matrix
#' @param b_recode A categorical matrix identifying the new category names
bannerDataRecode <- function(b_table, b_recode) {
  names_mask <- (b_recode$old_categories %in% dimnames(b_table)[[b_recode$alias]]) &
    !is.na(b_recode$categories_out)
  n_dim <- length(dim(b_table))
  dim_num <- which(names(dimnames(b_table)) == b_recode$alias)
  if (length(dim_num) > 1) dim_num <- dim_num[2]
  t_table <- b_table
  if (n_dim < 3) {
    dim(t_table) <- c(dim(t_table), rep(1, 3 - n_dim))
    dimnames(t_table) <- dimnames(b_table)
  }
  t_table <- t_table[if (dim_num == 1) names_mask else TRUE,
                     if (dim_num == 2) names_mask else TRUE,
                     if (dim_num == 3) names_mask else TRUE, drop = FALSE]
  dimnames(t_table)[[dim_num]] <- b_recode$categories_out[names_mask]
  if (n_dim < 3) {
    d_names <- dimnames(t_table)
    dim(t_table) <- dim(t_table)[1:n_dim]
    dimnames(t_table) <- d_names[1:n_dim]
  }
  return(t_table)
}

#' Return Excel-style column name.
#'
#' Returns an excel style column name. Useful
#' for identifying a specific column. Column 1, would be A
#' in Excel, column 100 would be CV in excel
#'
#' @param n An integer identfying the column number
get_grid_number <- function(n) {
  out <- c()
  while (n > 0) {
    modulo <- (n - 1) %% 26
    out <- c(LETTERS[modulo + 1], out)
    n <- (n - modulo) %/% 26
  }
  paste0(out, collapse = "")
}

#' Create row data
#'
#' Adjust table data to match the desired output
#' depending on the type of data being presented
#'
#' @param data An object containing data. Either a crosstab array or topline array.
#' @param row An integer identifying the row number
#' @param is_crosstabs_array Logical, is this a crosstab array?
#' @param is_toplines_array Logical, is this a toplines array?
#' @param is_base Logical, is this a row of bases?
row_data <- function(data, row, is_crosstabs_array, is_toplines_array, is_base) {
  dimnames(data$`___total___`)$total <- "Total"
  data <- lapply(data, function(dt){
    names(dimnames(dt)) <- NULL
    return(dt)
  })

  if (is_crosstabs_array) {
    data <- lapply(data, function(xi) {
      if (length(dim(xi)) == 3) {
        dt <- xi[ , , row, drop = FALSE]
      } else {
        dt <- xi[, row, drop = FALSE]
      }
      if (is_base) {
        dt <- t(dt)
        dim(dt) <- dim(dt)[2]
        dimnames(dt)[1] <- dimnames(xi)[1]
      } else {
        dim(dt) <- dim(dt)[1:2]
        dimnames(dt) <- dimnames(xi)[1:2]
      }
      return(dt)
    })
  } else if (is_toplines_array) {
    dt <- data$`___total___`
    if (is_base) {
      dim(dt) <- dim(dt)[2]
      dimnames(dt)[1] <- dimnames(data$`___total___`)[2]
    } else {
      dim(dt) <- dim(dt)[c(1,3)]
      dimnames(dt) <- dimnames(data$`___total___`)[c(1,3)]
    }
    data$`___total___` <- dt
  }
  return(data)
}


#' Column based hypothesis testing
#'
#' Calculates chi-square and returns p-values
#'
#' @param counts A data.frame or matrix of counts
#' @param counts_unweighted A data.frame or matrix of counts
#' @importFrom stats pnorm
compute_pvals <- function(counts, counts_unweighted) {
  shape <- dim(counts)
  n <- margin.table(counts)
  bases_adj <- counts_unweighted + 1
  n_adj <- margin.table(bases_adj)

  nrows <- nrow(counts)
  ncols <- ncol(counts)

  R <- margin.table(counts, 1) / n
  C_adj <- margin.table(bases_adj, 2) / n_adj
  Ctbl <- prop.table(counts, margin = 2)
  Ctbl_adj <- prop.table(bases_adj, margin = 2)

  observed <- (Ctbl_adj * (1 - Ctbl_adj))
  expected <- observed %*% C_adj
  d.c <- (1 - 2 * C_adj) / C_adj
  se.c <- matrix(nrow = nrows, ncol = ncols)
  for (i in 1:nrows) {
    for (j in 1:ncols) {
      se.c[i,j] <- d.c[j] * observed[i,j] + expected[i]
    }
  }
  se.c <- sqrt(se.c / n_adj)
  Z.c <- (Ctbl - matrix(rep(R, ncols), nrow = nrows)) / se.c
  psign <- sign(Z.c)
  pvals <- psign * 2 * pnorm(abs(Z.c), lower.tail = FALSE)
  pvals[is.nan(pvals) | psign == 0] <- 1
  return(pvals)
}
