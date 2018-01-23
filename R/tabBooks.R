#' @importFrom crunch multitables newMultitable tabBook allVariables aliases types type crtabs prop.table margin.table bases
#' @importFrom digest digest
tabBooks <- function(dataset, vars, banner, weight = NULL) {
    tabs_data <- list()
    
    multitable <- getMultitable(banner, dataset)
    book <- tabBook(multitable, dataset = dataset[vars], weight = weight, format="json")
    
    banner_map <- lapply(seq_along(banner), function(bx) sapply(banner[[bx]], function(bv) bv$alias))
    banner_flatten <- flattenBanner(banner)
    
    # for every variable in the book
    for (vi in seq_along(book)) {
        crunch_cube <- book[[vi]][[1]]
        
        var_type <- type(dataset[[getAlias(crunch_cube)]])
        is_array_type <- var_type == 'categorical_array'
        is_mr_type <- var_type == 'multiple_response'
        
        # generate new names and aliases for categorical_array variables by combining variable's names/aliases
        # with subvariables' names/aliases
        vnames <- if (is_array_type) paste(getName(crunch_cube), getSubNames(crunch_cube), sep = " - ") else getName(crunch_cube)
        valiases <- if (is_array_type) getSubAliases(crunch_cube) else getAlias(crunch_cube)
        subnames <- if (is_array_type) getSubNames(crunch_cube) else NA
        
        # prepare a data structure for every variable (categorical_array variables are sliced)
        for (vai in seq_along(valiases)) {
            tabs_data[[valiases[vai]]] <- structure(list(alias = valiases[vai], 
                name = vnames[vai], 
                subnames = subnames[vai],
                description = getDescription(crunch_cube), 
                notes = getNotes(crunch_cube), 
                settings = list(no_totals = is_mr_type, number = paste0(vi, if (length(valiases) > 1) get_grid_number(vai), collapse = "")), 
                crosstabs = sapply(names(banner), function(x) list(), simplify = FALSE, USE.NAMES = TRUE)),
                class = c(if (is_mr_type) "MultipleResponseCrossTabVar", "CrossTabVar"))
        }
        
        # for every "column" variable
        for (vbi in seq_along(book[[vi]])) {
            crunch_cube <- book[[vi]][[vbi]]
            crunch_cube@useNA <- "no"
            margin <- if (is_array_type) c(2, 3) else 2
            
            banner_counts <- as.array(crunch_cube)
            banner_proportions <- crunch::prop.table(crunch_cube, margin = margin)
            banner_totals_counts <- crunch::margin.table(crunch_cube, margin = margin)
            banner_totals_proportions <- crunch::margin.table(banner_proportions, margin = margin)
            banner_unweighted_n <- if (is.null(weight)) banner_totals_counts else bases(crunch_cube, margin = margin)
            banner_counts_unweighted <- if (is.null(weight)) banner_counts else bases(crunch_cube, margin = 0)
            
            banner_totals_counts[banner_totals_counts %in% c(NULL, NaN)] <- 0
            banner_totals_proportions[banner_totals_proportions %in% c(NULL, NaN)] <- 0
            banner_unweighted_n[banner_unweighted_n %in% c(NULL, NaN)] <- 0
            banner_proportions[banner_proportions %in% c(NULL, NaN)] <- 0
            banner_counts[banner_counts %in% c(NULL, NaN)] <- 0
            banner_counts_unweighted[banner_counts_unweighted %in% c(NULL, NaN)] <- 0
            
            banner_var_alias <- if (vbi == 1) "___total___" else aliases(crunch_cube)[2]
            
            for (ri in seq_along(valiases)) {
                counts_out <- as.matrix(if (is_array_type) banner_counts[,,ri] else banner_counts)
                proportions_out <- as.matrix(if (is_array_type) banner_proportions[,,ri] else banner_proportions)
                totals_counts_out <- t(if (is_array_type) banner_totals_counts[,ri] else banner_totals_counts)
                totals_proportions_out <- t(if (is_array_type) banner_totals_proportions[,ri] else banner_totals_proportions)
                unweighted_n_out <- t(if (is_array_type) banner_unweighted_n[,ri] else banner_unweighted_n)
                counts_unweighted_out <- as.matrix(if (is_array_type) banner_counts_unweighted[,,ri] else banner_counts_unweighted)
                
                if (is_array_type && ncol(counts_out) == 1 && vbi > 1) {
                    col_names <- dimnames(banner_counts)[[2]]
                    colnames(counts_unweighted_out) <- colnames(counts_out) <- colnames(proportions_out) <- col_names
                    colnames(totals_counts_out) <- colnames(totals_proportions_out) <- colnames(unweighted_n_out) <- col_names
                }
                
                if (vbi == 1) {
                    # if (is_mr_type){
                    #     totals_counts_out <- t(totals_counts_out)
                    #     totals_proportions_out <- t(totals_proportions_out)
                    #     unweighted_n_out <- t(unweighted_n_out)
                    # }
                    colnames(counts_out) <- "Total"
                    colnames(proportions_out) <- "Total"
                    colnames(totals_counts_out) <- "Total"
                    colnames(totals_proportions_out) <- "Total"
                    colnames(unweighted_n_out) <- "Total"
                    colnames(counts_unweighted_out) <- "Total"
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
                
                ### THIS IS JUST FOR NOW. THIS NEEDS TO BE CHANGED WHEN NETS ARE UPDATED!!!
                cats <- na.omit(categories(dataset[[getAlias(crunch_cube)]]))
                banner_var_cross <- structure(list(
                    counts = counts_out,
                    proportions = proportions_out,
                    totals_counts = totals_counts_out,
                    totals_proportions = totals_proportions_out,
                    unweighted_n = unweighted_n_out,
                    counts_unweighted = counts_unweighted_out,
                    ### THIS IS JUST FOR NOW. THIS NEEDS TO BE CHANGED WHEN NETS ARE UPDATED!!!
                    inserts = if (!is.null(cats)) collateCats(transforms(crunch_cube)[[getAlias(crunch_cube)]]$insertions, cats),
                    pvals_col = NULL
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
        }
    }
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
