#' Create an object of class \code{Banner}
#'
#' @param dataset A Crunch dataset.
#' @param vars A named list of vectors of aliases of the variables that shoulds constitute the banner object.
#' These should be categorical (and typically hidden) variables present in the \code{dataset}.
#' @param labels An optional named list of labels, where names are variables' aliases
#' (present in the \code{vars} parameter) and values are the labels that should be used in the report.
#' Defaults to NULL - variables names are used.
#' @param recodes An optional named list of categories recodes (the syntax is similar
#' to the one used in the \code{recode} function in the R \code{car} library). Use \code{NA} to
#' exclude categories. Not listed categories will be left unchanged.
#' Use \code{else} to replace all not listed categories. See examples for details.
#' Defaults to NULL - categories are not modified.
#' @return An object of class \code{Banner}.
#' @examples
#' \dontrun{
#' banner_data <- banner(crunch_dataset,
#'     vars = list(subBanner1 = c('alias1', 'alias2'), subBanner2 = c('alias3')),
#'     labels = c(alias1 = 'var1 label', alias2 = 'var2 label'),
#'     recodes = list(
#'         alias1 = "'cat1a' = 'new cat1a'; 'cat1b' = NA",
#'         alias2 = "'cat2a' = 'new cat2a'; 'cat2b' = 'new cat2b'; else = NA"))
#' }
#' @importFrom crunch alias allVariables types categories subvariables
#' @export
banner <- function(dataset, vars, labels = NULL, recodes = NULL) {
    checkCrunchDatasetClass(dataset)

    if (!(is.vector(vars) && is.recursive(vars))) {
        stop("'vars' should be a list of vectors.")
    }
    vars_vec <- unlist(vars)
    if (length(vars_vec) == 0) {
        stop("'vars' doesn't contain valid values.")
    }

    not_found_vars <- setdiff(vars_vec, aliases(allVariables(dataset)))
    if (length(not_found_vars) != 0) {
        stop(paste("Variables:", paste(not_found_vars, collapse = ", "), "not found."))
    }

    ds_vars <- allVariables(dataset[vars_vec])

    var_types <- types(ds_vars)
    if (!all(var_types %in% c("categorical", "multiple_response"))) {
        not_categorical <- aliases(ds_vars)[!(var_types %in% c("categorical", "multiple_response"))]
        stop(paste("All banner variables have to be categorical or multiple_response. This is not true for:",
                   paste(not_categorical, collapse = ", ")))
    }

    if (!is.null(labels)) {
        not_found <- setdiff(names(labels), vars_vec)
        if (length(not_found) > 0) {
            stop("Aliases used in 'labels' not in 'vars': ", paste(not_found, collapse = ", "))
        }
    }

    if (!is.null(recodes)) {
        not_found <- setdiff(names(recodes), vars_vec)
        if (length(not_found) > 0) {
            stop("Aliases used in 'recodes' not in 'vars': ", paste(not_found, collapse = ", "))
        }
    }

    ret_data <- list(alias = aliases(ds_vars), name = replace(names(ds_vars), match(names(labels),
        (aliases(ds_vars))), labels), old_categories = lapply(vars_vec, function(x) {
          cat_fun <- if (type(dataset[[x]]) == "multiple_response") {
            names(subvariables(dataset[[x]]))
          } else {
            names(categories(dataset[[x]])[!is.na(categories(dataset[[x]]))])
          }}))
    ret_data$categories_out <- ret_data$old_categories

    ret_data <- lstranspose(ret_data)
    names(ret_data) <- vars_vec
    categories_ordered <- list()

    for (var_name in names(recodes)) {
        var_recodes <- recodes[[var_name]]
        var_recodes <- gsub("\n|\t", " ", var_recodes)
        recode_list <- rev(strsplit(var_recodes, ";")[[1]])
        used_list <- c()
        else_target <- NULL
        for (term in recode_list) {
            if (grepl("^else=", squeezeBlanks(term))) {
                target <- try(eval(parse(text = strsplit(term, "=")[[1]][2])), silent = TRUE)
                if (class(target) == "try-error") {
                  stop("\n  in recode term: ", term, "\n  message: ", target)
                }
                else_target <- target
            } else {
                set <- try(eval(parse(text = strsplit(term, "=")[[1]][1])), silent = TRUE)
                if (class(set) == "try-error") {
                  stop("\n  in recode term: ", term, "\n  message: ", set)
                }
                for (ccat in set)
                  if (!ccat %in% ret_data[[var_name]][["old_categories"]]) {
                    stop(paste0("No category with name '", ccat, "' in '", var_name, "'"))
                  }
                target <- try(eval(parse(text = strsplit(term, "=")[[1]][2])), silent = TRUE)
                if (class(target) == "try-error") {
                  stop("\n  in recode term: ", term, "\n  message: ", target)
                }
                ret_data[[var_name]][["categories_out"]][ret_data[[var_name]][["old_categories"]] %in%
                  set] <- target
                if (!is.na(target))
                  categories_ordered[[var_name]] <- c(target, categories_ordered[[var_name]])
                used_list <- c(set, used_list)
            }
        }
        if (!is.null(else_target)) {
            ret_data[[var_name]][["categories_out"]][!(ret_data[[var_name]][["old_categories"]] %in%
                used_list)] <- else_target
            if (!is.na(target))
                categories_ordered[[var_name]] <- c(else_target, categories_ordered[[var_name]])
        }
    }

    ret <- lapply(vars, function(ban) sapply(ban, function(v) {
        ret_val <- ret_data[[v]]
        categories <- unique(ret_val$categories_out[!is.na(ret_val$categories_out)])
        if (length(categories) != length(ret_val$categories_out[!is.na(ret_val$categories_out)])) {
          stop("Combining categories is not supported")
        }
        ret_val$categories <- if (length(categories) == length(categories_ordered[[v]]))
            categories_ordered[[v]] else categories
        class(ret_val) <- c("BannerVar", class(ret_val))
        ret_val
    }, simplify = FALSE))

    total <- list(alias = "___total___", name = "", old_categories = "Total", categories_out = "Total",
        categories = "Total")
    class(total) <- "BannerVar"

    # Add the 'Total' column at the beginning of each subbanner
    ret <- lapply(ret, function(banner) {
        sapply(c("___total___", names(banner)), function(bi) {
            if (bi == "___total___")
                total else banner[[bi]]
        }, simplify = FALSE)
    })

    names(ret) <- if (is.null(names(vars))) paste0("Banner", seq_along(ret)) else names(vars)
    class(ret) <- "Banner"
    ret
}


squeezeBlanks <- function(text) {
    gsub(" *", "", text)
}

lstranspose <- function(l) {
    if (length(unique(sapply(l, length))) > 1)
        stop("All nested lists must be of equal length")
    return(lapply(seq_along(l[[1]]), function(x) sapply(l, function(y) y[[x]], simplify = FALSE)))
}
