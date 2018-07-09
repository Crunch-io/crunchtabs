
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
#' @return A Toplines (when no banner is provided) or Crosstabs (when a banner is provided)
#' summary of the input dataset.
#' @examples
#' \dontrun{
#' toplines_summary <- crosstabs(crunch_dataset, weight = 'weight')
#' crosstabs_summary <- crosstabs(crunch_dataset, vars = c('alias1', 'alias2'),
#'                                weight = 'weight', banner = banner_object)
#' }
#' @importFrom crunch name aliases allVariables is.Numeric is.dataset weight alias weightVariables
#' @importFrom methods is
#' @export
crosstabs <- function(dataset, vars = names(dataset), weight = crunch::weight(dataset), 
    banner = NULL, codebook = FALSE) {
    
    # TODO: open ends
    wrong_class_error(dataset, "CrunchDataset", "dataset")
    
    error_if_items(setdiff(vars, aliases(allVariables(dataset))), 
        "Variables in `vars` must be valid aliases in aliases(allVariables(dataset)). This is not true for {items}.",
        and = TRUE, quotes = TRUE)
    
    if (!is.null(weight)) {
        if (is.variable(weight)) { weight <- alias(weight) }
        if (!weight %in% aliases(allVariables(dataset))) {
            stop("`weight`, if provided, must be a valid variable in `dataset`. '", weight, "' is not found.")
        }
        if (!weight %in% weightVariables(dataset)) {
            stop("`weight`, if provided, must be a valid weight variable in `dataset`. '", weight, "' is not a weight variable.")
        }
    }
    if (!is.null(banner) && !is(banner, "Banner")) {
        stop("`banner`, if provided, must be an object of class 'Banner'.")
    }
    
    weight_var <- if (!is.null(weight)) dataset[[weight]]
    
    vars_out <- if (codebook) { vars } else {
        intersect(vars, aliases(allVariables(dataset))[types(allVariables(dataset)) %in% c("categorical", "multiple_response", "categorical_array", "numeric")]) }
    
    error_if_items(unique(types(allVariables(dataset[setdiff(vars, vars_out)]))), 
        "`vars` of type(s) {items} are not supported and have been skipped.", 
        and = TRUE, error = FALSE)
    
    if (length(vars_out) == 0){
        stop("No variables provided.")
    }
    
    banner_use <- if (is.null(banner)) {
        banner(dataset, vars=list('Results'=aliases(allVariables(dataset))[types(allVariables(dataset)) %in% c("categorical", "multiple_response")][1])) } else { banner }
    
    results <- tabBooks(dataset = dataset, vars = vars_out, banner = banner_use, weight = weight_var, topline = is.null(banner))

    if (codebook) { res_class <- "Codebook" }
    else if (is.null(banner)) { res_class <- c("Toplines", "CrunchTabs") }
    else res_class <- c("Crosstabs", "CrunchTabs")
    
    if (!is.null(banner)) {
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
        class(banner) <- 'Banner'
    }
    
    summary_data <- list(metadata = c(list(title = name(dataset), weight = weight, 
        start_date = startDate(ds), end_date = endDate(ds), description = description(ds))),
        results = results, banner = banner)
    class(summary_data) <- res_class
    
    return(summary_data)
}

