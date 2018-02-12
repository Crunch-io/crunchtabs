
#' Toplines and Crosstabs Summaries
#'
#' \code{crosstabs} produces Toplines (one-way frequency tables) or Crosstabs (cross tabulations)
#' summaries of a Cruch dataset.
#'
#' @param dataset A Crunch dataset.
#' @param vars An optional vector of aliases of the non-hidden variables that shoulds be used.
#' Defaults to all non-hidden variables.
#' @param weight The alias of a numeric variable that should be used for data weighting.
#' Defaults to current weight variable. For unweighted, set to \code{NULL}
#' @param banner An optional object of class \code{Banner} that should be used to generate
#' a Crosstabs summary. Defaults to \code{NULL} - a Toplines summary is produced and returned.
#' @param codebook If \code{TRUE}, codebook data summaries are prepared. Defaults to \code{FALSE}.
#' @param title An optional title. Defaults to the name of the dataset.
#' @param date An optional date. Defaults to the current date.
#' @param metadata An optional list with additional metadata that should be added to the summary.
#' @return A Toplines (when no banner is provided) or Crosstabs (when a banner is provided)
#' summary of the input dataset.
#' @examples
#' \dontrun{
#' toplines_summary <- crosstabs(crunch_dataset, weight = 'weight')
#' crosstabs_summary <- crosstabs(crunch_dataset, vars = c('alias1', 'alias2'),
#'                                weight = 'weight', banner = banner_object)
#' }
#' @importFrom crunch name aliases allVariables is.Numeric is.dataset
#' @importFrom methods is
#' @export
crosstabs <- function(dataset, vars = names(dataset), weight = weight(dataset), banner = NULL, codebook = FALSE,
    title = name(dataset), date = Sys.Date(), metadata = NULL) {
    
    if (!is.dataset(dataset)) {
        stop("'dataset' is not an object of class 'CrunchDataset'.")
    }
    
    not_found_vars <- setdiff(vars, aliases(allVariables(dataset)))
    if (length(not_found_vars) > 0) {
        stop(paste("Variables:", paste(not_found_vars, collapse = ", "), "not found."))
    }
    if (!is.null(weight)) {
        if (!weight %in% aliases(allVariables(dataset))) {
            stop(paste("No variable with alias", weight, "found in 'dataset'."))
        }
        if (!weight %in% weightVariables(dataset)) {
            stop(paste("No 'weight' variable with alias", weight, "found in 'dataset'."))
        }
    }
    if (!is.null(banner) && !is(banner, "Banner")) {
        stop("'banner', if provided, must be an object of class 'Banner'.")
    }
    
    weight_var <- if (!is.null(weight)) dataset[[weight]]
    
    vars_out <- if (codebook) { vars } else {
        intersect(vars, aliases(allVariables(dataset))[types(allVariables(dataset)) %in% c("categorical", "multiple_response", "categorical_array", "numeric")]) }
    
    filtered_vars <- setdiff(vars, vars_out)
    if (length(filtered_vars) > 0) {
        warning(paste("Variables of types:", paste(unique(types(allVariables(dataset[filtered_vars]))),
            collapse = ", "), "are not supported and have been skipped"))
    }
    
    if (length(vars_out) == 0){
        stop("No variables provided.")
    }
    
    if (is.null(banner)) {
        results <- lapply(vars_out, function(var) topline(var = dataset[[var]], dataset = dataset, weight = weight_var,
            codebook = codebook))
        names(results) <- vars_out
        res_class <- c(if (codebook) "Codebook" else "Toplines", "CrunchTabs")
    } else {
        results <- tabBooks(dataset = dataset, vars = vars_out, banner = banner, weight = weight_var)
        class(results) <- c("CrosstabsResults", class(results))
        res_class <- c("Crosstabs", "CrunchTabs")
        banner <- lapply(banner, function(b)
            lapply(b, function(b1) {
                if (b1$alias %in% '___total___') {
                    b1$unweighted_n <- nrow(dataset)
                    b1$weighted_n <- sum(as.vector(weight_var), na.rm = TRUE)
                } else {
                    b1$unweighted_n <- setNames(as.array(crtabs(paste0('~', b1$alias), data=dataset, weight=NULL)), b1$categories_out)
                    b1$unweighted_n <- b1$unweighted_n[!is.na(names(b1$unweighted_n))]
                    b1$weighted_n <- setNames(as.array(crtabs(paste0('~', b1$alias), data=dataset, weight=weight_var)), b1$categories_out)
                    b1$weighted_n <- b1$weighted_n[!is.na(names(b1$weighted_n))]
                }
                return(b1)
            }))
    }
    
    summary_data <- list(metadata = c(list(title = title, date = date, weight = weight), metadata),
        results = results, banner = banner)
    class(summary_data) <- res_class
    
    return(summary_data)
}

#' @importFrom crunch is.dataset
checkCrunchDatasetClass <- function(dataset) {
    if (!is.dataset(dataset)) {
        stop("'dataset' is not an object of class 'CrunchDataset'.")
    }
}

#' @importFrom crunch types variables
filter_unsupported_toplines <- function(results, dataset, vars) {
    vars_filtered <- sapply(results, is.null)
    if (any(vars_filtered)) {
        warning(paste("Variables of types:", paste(unique(types(allVariables(dataset[vars]))[vars_filtered]),
            collapse = ", "), "are not supported and have been skipped"))
    }
    results[!vars_filtered]  # filter out NULLs generated by unsupported variable types
}

#' @importFrom stats sd qt
computeMoe <- function(n = length(weight), weight = NULL) {
    if (is.null(weight)) {
        warning("\n No weight provided. All cases will be assigned weight equal to 1")
        weight <- rep(1, n)
    }
    moe <- qt(0.975, n - 1) * sqrt((1 + sd(weight)^2)) * sqrt(1/(4 * n))
    moe
}
