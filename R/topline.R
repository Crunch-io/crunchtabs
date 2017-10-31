#' Topline summary of a Crunch variable
#'
#' \code{topline} returns a Topline summary of a Crunch variable.
#'
#' @param var A Cruch variable.
#' @param dataset A Crunch dataset.
#' @param weight A numeric Cruch variable that should be used for data weighting.
#' Defaults to NULL - data is not weighted.
#' @param codebook If \code{TRUE}, codebook data summaries are prepared.
#' Defaults to \code{FALSE}.
#' @return A Toplines summary of a Crunch variable.
#' @examples
#' \dontrun{
#' topline_summary <- topline(crunch_variable, crunch_dataset, crunch_weight_variable)
#' }
#' @export
topline <- function(var, dataset, weight, codebook = FALSE) {
    UseMethod("topline", var)
}

#' @export
topline.default <- function(var, dataset, weight = NULL, codebook = FALSE) {
    warning(paste("topline doesn't support objects of class", class(var)))
    NULL
}

#' @export
topline.TextVariable <- function(var, dataset, weight = NULL, codebook = FALSE) {
  toplineBase(var)
}

#' @export
topline.DatetimeVariable <- function(var, dataset, weight = NULL, codebook = FALSE) {
  toplineBase(var)
}

#' @importFrom stats sd
#' @importFrom crunch digits
#' @export
topline.NumericVariable <- function(var, dataset, weight = NULL, codebook = FALSE) {
    topline_base <- toplineBase(var)
    var_alias <- getAlias(topline_base)
    out_crtabs <- crtabs(paste0("list(min(", var_alias, "), max(", var_alias, "), mean(", var_alias, "), sd(", var_alias, ")) ~ 1"), data = dataset, weight = weight)
    total <- getTotal(out_crtabs)
    missing <- getMissing(out_crtabs)
    summary_data <- c(Minimum = out_crtabs@arrays$min, Maximum = out_crtabs@arrays$max, Mean = out_crtabs@arrays$mean, `Standard deviation` = out_crtabs@arrays$stddev)
    ret <- c(topline_base, list(summary = array(summary_data, dimnames = list(names(summary_data)))
                                , total = total, missing = missing, valid = total - missing, settings = list(digits = digits(var))))
    class(ret) <- c(generateClassList(topline_base), class(ret))
    ret
}

#' @export
topline.CategoricalVariable <- function(var, dataset, weight = NULL, codebook = FALSE) {
    toplineGen(var, dataset, weight = weight, codebook = codebook)
}

#' @export
topline.MultipleResponseVariable <- function(var, dataset, weight = NULL, codebook = FALSE) {
    ret <- toplineGen(var, dataset = dataset, weight = weight, margin = NULL, mr = TRUE, codebook = codebook)
    ret$subvariables <- getSubvarData(var)
    ret
}

#' @export
topline.CategoricalArrayVariable <- function(var, dataset, weight = NULL, codebook = FALSE) {
    ret <- toplineGen(var, dataset, weight = weight, margin = 1, codebook = codebook)
    ret$subvariables <- getSubvarData(var)
    ret$valid = rowSums(ret$counts)
    ret$missing = ret$total - ret$valid
    dimnames(ret$counts) <- list(subvariables = dimnames(ret$counts)[[1]], categories = dimnames(ret$counts)[[2]])
    dimnames(ret$proportions) <- dimnames(ret$counts)
    ret
}

toplineGen <- function(var, dataset, weight = NULL, margin = NULL, mr = FALSE, codebook = FALSE) {
    topline_base <- toplineBase(var)
    ret <- c(topline_base, if (codebook) {
      out_crtabs_details <- crtabs(formula = paste0("~", if (mr) "as_selected(", "`", alias(var), "`", if (mr) ")"),
                                   data = dataset, weight = weight, useNA = "always")
      list(counts_details = as.array(out_crtabs_details),
           proportions_details = crunch::prop.table(out_crtabs_details),
           categories = as.data.frame(lapply(as.data.frame(do.call(rbind, lapply(categories(var),
                                      function(x) {
                                        sapply(x[c("id", "missing", "name", "numeric_value")]
                                               , function(xx) if (is.null(xx)) NA else xx)
                                      }))), unlist), stringsAsFactors = FALSE))
    } else {
      out_crtabs <- crtabs(formula = paste0("~", if (mr) "as_selected(", "`", alias(var), "`", if (mr) ")"), data = dataset, weight = weight)
      total <- getTotal(out_crtabs)
      missing <- getMissing(out_crtabs)
      list(counts = as.array(out_crtabs),
           proportions = crunch::prop.table(out_crtabs, margin = margin),
           counts_unweighted = bases(out_crtabs, 0),
           total = total,
           missing = missing,
           valid = total - missing)
    })
    class(ret) <- c(generateClassList(topline_base), class(ret))
    ret
}


getSubvarData <- function(var) {
  list(aliases = aliases(subvariables(var)), names = names(subvariables(var)))
}


generateClassList <- function(topline_base) {
    categorical_general <- if (getType(topline_base) %in% c("categorical", "multiple_response", "categorical_array")) {
        categorical_general <- "ToplineCategoricalGeneral"
    }
    c(paste0("Topline", nameToClass(getType(topline_base))), categorical_general,
        class(topline_base))
}

nameToClass <- function(name) {
    gsub("(^|[_])([[:alpha:]])", "\\U\\2", name, perl = TRUE)
}
