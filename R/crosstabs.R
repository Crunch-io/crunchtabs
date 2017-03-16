
#' Toplines and Crosstabs Summaries
#'
#' \code{crosstabs} produces toplines (one-way frequency tables) or crosstabs (cross tabulations)
#' summaries of a Cruch dataset.
#'
#' @param dataset A Crunch dataset.
#' @param vars An optional vector of names of the (non-hidden) variables that shoulds be used.
#' Defaults to all non-hidden variables.
#' @param weight The name of a numeric variable that should be used for data weighting.
#' Defaults to NULL - data is not weighted.
#' @param banner An optional object of class \code{Banner} that should be used to generate
#' a cross tabulations summary. Defaults to NULL - a toplines summary is produced and returned.
#' @param title An optional title. Defaults to the name of the dataset.
#' @param date An optional date. Defaults to the current date.
#' @return A toplines (when no banner is provided) or crosstabs (when a banner is provided)
#' summary of the input dataset.
#' @examples
#' \dontrun{
#' crunch_dataset <- loadDataset('dataset_name')
#' toplines_summary <- crosstabs(crunch_dataset, weight = 'weight')
#' crosstabs_summary <- crosstabs(crunch_dataset, weight = 'weight', banner = banner_object)
#' }
#' @importFrom crunch name aliases allVariables is.Numeric
#' @export
crosstabs <- function(dataset, vars = names(dataset), weight = NULL, banner = NULL,
    title = name(dataset), date = Sys.Date()) {

    checkCrunchDatasetClass(dataset)

    not_found_vars <- setdiff(vars, names(dataset))
    if (length(not_found_vars) > 0) {
        stop(paste("Variables:", paste(not_found_vars, collapse = ", "), "not found"))
    }
    if (!is.null(weight)) {
        if (!weight %in% aliases(allVariables(dataset))) {
            stop(paste("No variable with alias", weight, "found in", name(dataset)))
        }
        if (!is.Numeric(dataset[[weight]])) {
            stop("The weight variable has to be numeric")
        }
    }
    if (!is.null(banner) && !is(banner, "Banner")) {
        stop("The banner parameter, if provided, must be an object of class 'Banner'")
    }

    weight_var <- if (!is.null(weight)) dataset[[weight]]

    if (is.null(banner)) {
        results <- lapply(dataset[vars], topline, data = dataset, weight = weight_var)
        names(results) <- vars
        results <- filter_unsupported_toplines(results, dataset, vars)
        res_class <- c("Toplines", "CrunchTabs")
    } else {
        mtvars <- setdiff(sapply(flattenBanner(banner), getAlias), "___total___")
        results <- tabBooks(data = dataset[aliases(allVariables(dataset))[aliases(allVariables(dataset)) %in% c(vars, mtvars)]], vars = vars, banner = banner, weight = weight_var)
        class(results) <- c("CrosstabsResults", class(results))
        res_class <- c("Crosstabs", "CrunchTabs")
    }

    summary_data <- list(title = title, date = date, results = results, banner = banner)
    class(summary_data) <- res_class

    return(summary_data)
}

#' @importFrom crunch is.dataset
checkCrunchDatasetClass <- function(dataset) {
    if (!is.dataset(dataset)) {
        stop("The dataset parameter must be an object of class 'CrunchDataset'")
    }
}

#' @importFrom crunch types variables
filter_unsupported_toplines <- function(results, dataset, vars) {
    vars_filtered <- sapply(results, is.null)
    if (any(vars_filtered)) {
        warning(paste("Variables of types:", paste(unique(types(variables(dataset[vars]))[vars_filtered]),
            collapse = ", "), "are not supported and have been skipped"))
    }
    results[!vars_filtered]  # filter out NULLs caused by unsupported variable types
}

computeMoe <- function(n = length(weight), weight = NULL) {
    if (is.null(weight)) {
        warning("\n No weight provided. All cases will be assigned weight equal to 1")
        weight <- rep(1, n)
    }
    moe <- qt(0.975, n - 1) * sqrt((1 + sd(weight)^2)) * sqrt(1/(4 * N))
    moe
}
