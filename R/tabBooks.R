#' @importFrom crunch multitables newMultitable tabBook allVariables aliases types type crtabs prop.table margin.table bases
#' @importFrom digest digest
tabBooks <- function(dataset, vars, banner, weight = NULL, topline = FALSE) {
    tabs_data <- list()
    
    multitable <- getMultitable(banner, dataset)
    book <- tabBook(multitable, dataset = dataset[vars], weight = weight, format="json")
    
    banner_map <- lapply(seq_along(banner), function(bx) sapply(banner[[bx]], function(bv) bv$alias))
    banner_flatten <- flattenBanner(banner)
    
    get_topline_array_data <- function(tabs_data, valiases, name){
        tmp <- data.frame(lapply(valiases, function(vxi) c(tabs_data[[vxi]]$crosstabs$Results$Total[[name]])))
        if (any(dim(tmp) %in% 0)) return(NULL)
        tmp <- setNames(tmp, subnames)
        if (!is.null(rownames(tabs_data[[valiases[1]]]$crosstabs$Results$Total[[name]]))) {
            rownames(tmp) <- rownames(tabs_data[[valiases[1]]]$crosstabs$Results$Total[[name]])
        }
        return(tmp)
    }
    # for every variable in the book
    for (vi in seq_along(book)) {
        crunch_cube <- book[[vi]][[1]]
        
        var_type <- type(dataset[[getAlias(crunch_cube)]])
        is_array_type <- var_type == "categorical_array"
        is_mr_type <- var_type == "multiple_response"
        cat_type <- var_type %in% c("categorical", "categorical_array")
        topline_array <- is_array_type && topline
        
        # generate new names and aliases for categorical_array variables by combining variable's names/aliases
        # with subvariables' names/aliases
        # vnames <- if (is_array_type) paste(getName(crunch_cube), getSubNames(crunch_cube), sep = " - ") else getName(crunch_cube)
        valiases <- if (is_array_type) getSubAliases(crunch_cube) else getAlias(crunch_cube)
        subnames <- if (is_array_type) getSubNames(crunch_cube) else NA
        dvaliases <- c(if (topline_array) getAlias(crunch_cube), valiases)
        
        var_cats <- categories(dataset[[getAlias(crunch_cube)]])
        inserts <- if (!is.null(var_cats) && !is.null(transforms(crunch_cube)[[1]]$insertions)) crunch:::collateCats(transforms(crunch_cube)[[1]]$insertions, na.omit(var_cats))
        mean_median <- cat_type && any(!is.na(values(na.omit(var_cats))))
        # prepare a data structure for every variable (categorical_array variables are sliced)
        tabs_data[dvaliases] <- lapply(seq_along(dvaliases), function(vai)
            structure(list(alias = dvaliases[vai], 
            type = var_type,
            name = getName(crunch_cube), #vnames[vai], 
            subnames = subnames[vai],
            subnumber = if (is_array_type) vai else NA,
            description = getDescription(crunch_cube), 
            notes = getNotes(crunch_cube), 
            settings = list(no_totals = is_mr_type, number = paste0(vi, if (length(dvaliases) > 1) get_grid_number(vai), collapse = "")), 
            categories = var_cats,
            mean_median = mean_median,
            inserts = sapply(inserts, class),
            crosstabs = sapply(names(banner), function(x) list(), simplify = FALSE, USE.NAMES = TRUE)),
            class = c(if (is_mr_type) "MultipleResponseCrossTabVar", if (topline_array) "ToplineArrayVar", "CrossTabVar")))

        seq_num <- if (topline) 1 else seq_along(book[[vi]])
        # for every "column" variable
        for (vbi in seq_num) {
            crunch_cube <- book[[vi]][[vbi]]
            crunch_cube@useNA <- "no"
            margin <- if (is_array_type) c(2, 3) else 2
            
            banner_counts <- as.array(crunch_cube)
            banner_proportions <- crunch::prop.table(noTransforms(crunch_cube), margin = margin)
            banner_totals_counts <- crunch::margin.table(crunch_cube, margin = margin)
            banner_totals_proportions <- crunch::margin.table(banner_proportions, margin = margin)
            banner_unweighted_n <- if (is.null(weight)) banner_totals_counts else crunch::bases(crunch_cube, margin = margin)
            banner_counts_unweighted <- if (is.null(weight)) banner_counts else crunch::bases(crunch_cube, margin = 0)

            banner_totals_counts[banner_totals_counts %in% c(NULL, NaN)] <- 0
            banner_totals_proportions[banner_totals_proportions %in% c(NULL, NaN)] <- 0
            banner_unweighted_n[banner_unweighted_n %in% c(NULL, NaN)] <- 0
            banner_proportions[banner_proportions %in% c(NULL, NaN)] <- 0
            banner_counts[banner_counts %in% c(NULL, NaN)] <- 0
            banner_counts_unweighted[banner_counts_unweighted %in% c(NULL, NaN)] <- 0
            
            banner_var_alias <- if (vbi == 1) "___total___" else aliases(crunch_cube)[2]
            
            ## TODO: Cat array tabbook
            for (ri in seq_along(valiases)) {
                counts_out <- as.matrix(if (is_array_type) banner_counts[,,ri] else banner_counts)
                proportions_out <- as.matrix(if (is_array_type) banner_proportions[,,ri] else banner_proportions)
                totals_counts_out <- as.matrix(if (is_array_type) banner_totals_counts[,ri] else banner_totals_counts)
                totals_proportions_out <- as.matrix(if (is_array_type) banner_totals_proportions[,ri] else banner_totals_proportions)
                unweighted_n_out <- as.matrix(if (is_array_type) banner_unweighted_n[,ri] else banner_unweighted_n)
                counts_unweighted_out <- as.matrix(if (is_array_type) banner_counts_unweighted[,,ri] else banner_counts_unweighted)

                if (is_array_type && ncol(counts_out) == 1 && vbi > 1) {
                    col_names <- dimnames(banner_counts)[[2]]
                    colnames(counts_unweighted_out) <- colnames(counts_out) <- colnames(proportions_out) <- col_names
                    colnames(totals_counts_out) <- colnames(totals_proportions_out) <- colnames(unweighted_n_out) <- col_names
                }
                
                if (vbi == 1) {
                    colnames(counts_out) <- "Total"
                    colnames(proportions_out) <- "Total"
                    colnames(totals_counts_out) <- "Total"
                    colnames(totals_proportions_out) <- "Total"
                    colnames(unweighted_n_out) <- "Total"
                    colnames(counts_unweighted_out) <- "Total"
                }

                ## conditional transpose to flip totals and unweighted Ns -- added 20171207
                ## min and max on MR -- added 20180212
                if (is_mr_type) {
                    totals_counts_out <- rbind(Min=apply(totals_counts_out, 2, min), Max=apply(totals_counts_out, 2, max))
                    unweighted_n_out <- rbind(Min=apply(unweighted_n_out, 2, min), Max=apply(unweighted_n_out, 2, max))
                } else {
                    totals_counts_out <- t(totals_counts_out)
                    unweighted_n_out <- t(unweighted_n_out)
                    totals_proportions_out <- t(totals_proportions_out)
                }

                banner_var <- banner_flatten[[banner_var_alias]]
                if (banner_var_alias != "___total___" &&
                        !identical(banner_var$old_categories, banner_var$categories)) {
                    counts_out <- bannerDataRecode(counts_out, banner_var)
                    proportions_out <- bannerDataRecode(proportions_out, banner_var)
                    totals_counts_out <- bannerDataRecode(totals_counts_out, banner_var)
                    totals_proportions_out <- bannerDataRecode(totals_proportions_out, banner_var)
                    unweighted_n_out <- bannerDataRecode(unweighted_n_out, banner_var)
                    counts_unweighted_out <- bannerDataRecode(counts_unweighted_out, banner_var)
                }
                
                means_out <- if (mean_median) {
                    unlist(sapply(colnames(counts_out), function(k) calcTabMeanInsert(counts_out[,k], na.omit(var_cats))))
                }
                medians_out <- if (mean_median) {
                    unlist(sapply(colnames(counts_out), function(k) calcTabMedianInsert(counts_out[,k], na.omit(var_cats))))
                }
                ### THIS IS JUST FOR NOW. THIS NEEDS TO BE CHANGED WHEN NETS ARE UPDATED!!!
                if (!is.null(inserts)){
                    counts_out <- as.matrix(calcTabInsertions(counts_out, inserts, var_cats))
                    proportions_out <- as.matrix(calcTabInsertions(proportions_out, inserts, var_cats))
                    counts_unweighted_out <- as.matrix(calcTabInsertions(counts_unweighted_out, inserts, var_cats))
                }

                banner_var_cross <- structure(list(
                    counts = counts_out,
                    proportions = proportions_out,
                    totals_counts = totals_counts_out,
                    totals_proportions = c(totals_proportions_out),
                    unweighted_n = unweighted_n_out,
                    counts_unweighted = counts_unweighted_out,
                    total = c(unweighted_n_out),
                    mean = c(means_out),
                    median = c(medians_out),
                    pvals_col = NULL#crunch::rstandard(crunch_cube)
                ), class = c("CrossTabBannerVar", "list"))

                for (bi in seq_along(banner_map)) {
                    for (bij in seq_along(banner_map[[bi]])) {
                        if (banner_var_alias == banner_map[[bi]][bij]) {
                            tabs_data[[valiases[ri]]][['crosstabs']][[bi]][[bij]] <- banner_var_cross
                            names(tabs_data[[valiases[ri]]][['crosstabs']][[bi]])[bij] <- if (banner_var_alias == "___total___") "Total" else banner_var_alias
                        }
                    }
                }
            }
            
            if (topline_array) {
                tabs_data[[dvaliases[1]]]$subname <- NA
                for (name in setdiff(names(tabs_data[[dvaliases[2]]]$crosstabs$Results$Total), names(tabs_data[[dvaliases[1]]]$crosstabs$Results$Total))) {
                    tabs_data[[dvaliases[1]]]$crosstabs$Results$Total[[name]] <- get_topline_array_data(tabs_data, valiases, name)
                }
                class(tabs_data[[dvaliases[1]]]$crosstabs$Results$Total) <- c("CrossTabBannerVar", "list")
                tabs_data[valiases] <- NULL
            }
            
        }
    }
    class(tabs_data) <- c("CrosstabsResults", class(tabs_data))
    return(tabs_data)
}

getMultitable <- function (banner, dataset) {
    ## Given a Banner object and a dataset, find/create the Crunch multitable that corresponds
    mtvars <- setdiff(sapply(flattenBanner(banner), function(x) paste0("`", getAlias(x), "`")), "`___total___`")
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
