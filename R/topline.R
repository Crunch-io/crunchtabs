#' Crunch variable summary for toplines reports
#'
#' \code{topline} returns a toplines summary of a Crunch variable.
#'
#' @param var A Cruch variable.
#' @param dataset A Crunch dataset.
#' @param weight A (numeric) Cruch variable that should be used for data weighting.
#' Defaults to NULL - data is not weighted.
#' @return A toplines summary of a Crunch variable.
#' @examples
#' \dontrun{
#' topline(dataset[[variable_name]], dataset, weight)
#' }
#' @export
topline <- function(var, dataset, weight) {
    UseMethod("topline", var)
}

#' @export
topline.default <- function(var, dataset, weight = NULL) {
    warning(paste("topline doesn't support objects of class", class(var)))
    NULL
}

#' @importFrom stats sd
#' @export
topline.NumericVariable <- function(var, dataset, weight = NULL) {
    topline_base <- toplineBase(var)
    data <- as.vector(var)
    valid <- length(data[!is.null(data)])
    missing <- length(data) - valid
    summary_data <- c(`Minimum value` = min(data, na.rm = TRUE), `Maximum value` = max(data,
        na.rm = TRUE), `Mean value` = mean(data, na.rm = TRUE), `Standard deviation` = sd(data,
        na.rm = TRUE))
    ret <- c(topline_base, list(summary = array(summary_data, dimnames = list(names(summary_data))),
        valid = valid, missing = missing))
    class(ret) <- c(generateClassList(topline_base), class(ret))
    ret
}

#' @export
topline.CategoricalVariable <- function(var, dataset, weight = NULL) {
    ret <- toplineGen(var, dataset, weight = weight)
    ret
}

#' @export
topline.MultipleResponseVariable <- function(var, dataset, weight = NULL) {
    #### Persephone: it's not always 'not selected'. if it didn't come in from the
    #### exporter, it won't be.  so maybe make 'not selected' a default? and let people
    #### change it if necessary
    ret <- toplineGen(var, dataset = dataset, weight = weight)
    ret$counts <- cbind(selected = ret$counts, `not selected` = ret$total - ret$counts -
        ret$missing, missing = ret$missing)
    ret
}

#' @export
topline.CategoricalArrayVariable <- function(var, dataset, weight = NULL) {
    ret <- toplineGen(var, dataset, weight = weight, sumFun = rowSums, margin = 1)
    ret$valid = rowSums(ret$counts)
    ret$missing = ret$total - ret$valid
    dimnames(ret$counts) <- list(subvariables = dimnames(ret$counts)[[1]], categories = dimnames(ret$counts)[[2]])
    dimnames(ret$proportions) <- dimnames(ret$counts)
    ret
}
toplineGen <- function(var, dataset, weight = NULL, sumFun = sum, margin = NULL) {
    topline_base <- toplineBase(var)
    out_crtabs <- crtabs(formula = paste("~", alias(var)), data = dataset, weight = weight)
    total <- getTotal.CrunchCube(out_crtabs)
    missing <- getMissing.CrunchCube(out_crtabs)
    ret <- c(topline_base, list(counts = as.array(out_crtabs), proportions = crunch::prop.table(out_crtabs,
        margin = margin), total = total, missing = missing, valid = total - missing))
    class(ret) <- c(generateClassList(topline_base), class(ret))
    ret
}

generateClassList <- function(topline_base) {
    categorical_general <- NULL
    if (getType(topline_base) %in% c("categorical", "multiple_response", "categorical_array")) {
        categorical_general <- "ToplineCategoricalGeneral"
    }
    c(paste0("Topline", nameToClass(getType(topline_base))), categorical_general,
        class(topline_base))
}

nameToClass <- function(name) {
    gsub("(^|[_])([[:alpha:]])", "\\U\\2", name, perl = TRUE)
}
