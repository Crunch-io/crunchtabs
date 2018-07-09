#' @importFrom crunch multitables newMultitable tabBook allVariables aliases types type crtabs prop.table margin.table bases
#' @importFrom digest digest
tabBooks <- function(dataset, vars, banner, weight = NULL, topline = FALSE) {

    banner_flatten <- flattenBanner(banner)
    banner_use <- banner
    if (topline) { banner_use$Results[[2]] <- NULL }
    
    multitable <- getMultitable(banner_flatten, dataset)
    book <- tabBook(multitable, dataset = dataset[vars], weight = weight, format="json")
    
    banner_var_names <- sapply(seq_along(book[[1]]), function(ix) {
        aliases(variables(book[[1]][[ix]]))[2] })
    banner_var_names[1] <- "___total___"
    var_nums <- match(vars, aliases(book))
    
    structure(unlist(lapply(var_nums, function(vi) {
        crunch_cube <- book[[vi]][[1]]
        
        ## Metadata
        cube_variable <- variables(crunch_cube)[1]
        alias <- aliases(cube_variable)
        var_type <- type(dataset[[aliases(cube_variable)]])
        
        is_mr_type <- var_type == "multiple_response"
        is_cat_type <- var_type %in% c("categorical", "categorical_array")
        is_array_type <- var_type == "categorical_array"
        is_toplines_array <- is_array_type && topline
        is_crosstabs_array <- is_array_type && !topline
        
        valiases <- if (is_crosstabs_array) { getSubAliases(crunch_cube) 
        } else { aliases(cube_variable) }
        subnames <- if (is_array_type) getSubNames(crunch_cube)
        
        var_cats <- categories(cube_variable[[1]])
        inserts <- if (is_cat_type) crunch:::collateCats(transforms(cube_variable)[[1]]$insertions, na.omit(var_cats))
        show_mean_median <- is_cat_type && any(!is.na(values(na.omit(var_cats))))
        
        metadata <- list(
            name = names(cube_variable), 
            description = descriptions(cube_variable), 
            notes = notes(cube_variable), 
            type = var_type,
            no_totals = is_mr_type, 
            mean_median = show_mean_median,
            subnames = subnames,
            categories = var_cats,
            inserts_obj = inserts
        )
        
        pbook <- lapply(seq_along(book[[vi]]), function(vix) {
            crunch::prop.table(noTransforms(book[[vi]][[vix]]), margin = c(2, if (is_array_type) 3))
        })
        bbook <- crunch::bases(book[[vi]], margin = c(2, if (is_array_type) 3))
        cbook <- as.array(book[[vi]])
        wbbook <- lapply(seq_along(book[[vi]]), function(bi) 
            crunch::margin.table(book[[vi]][[bi]], margin = c(2, if (is_array_type) 3)))
        
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
                        dimnames=list(rownames(pdata[[2]]), names(xi)))
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


getMultitable <- function (banner_flatten, dataset) {
    ## Given a Banner object and a dataset, find/create the Crunch multitable that corresponds
    mtvars <- paste0("`", setdiff(names(banner_flatten), "___total___"), "`")
    mt_name <- digest(sort(mtvars), "md5")
    multitable <- multitables(dataset)[[mt_name]]
    if (is.null(multitable)) {
        multitable <- newMultitable(paste("~", paste(mtvars, collapse = " + ")), data = dataset, name = mt_name)
    }
    return(multitable)
}

# This function computes p-values for column hypothesis testing only.
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
    for (i in 1: nrows) {
        for (j in 1: ncols) {
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


# Return Excel-style column name.
get_grid_number <- function(n) {
    out <- c()
    while (n > 0) {
        modulo <- (n - 1) %% 26
        out <- c(LETTERS[modulo + 1], out)
        n <- (n - modulo) %/% 26
    }
    paste0(out, collapse = "")
}

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

