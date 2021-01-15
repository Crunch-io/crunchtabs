#' Create an object of class \code{Banner}
#'
#' @param dataset A Crunch dataset.
#' @param vars A named list of vectors of aliases of the variables that should constitute the banner object.
#' These should be categorical (and typically hidden) variables present in the \code{dataset}.
#' @param labels An optional named list of labels, where names are variables' aliases
#' (present in the \code{vars} parameter) and values are the labels that should be used in the report.
#' Defaults to \code{NULL} - variables names are used.
#' @param recodes An optional named list of categories recodes (the syntax is similar
#' to the one used in the \code{recode} function in the R \code{dplyr} package). Use \code{NA} to
#' exclude categories. Not listed categories will be left unchanged.
#' Use \code{.default} to replace all not listed categories. See examples for details.
#' Defaults to \code{NULL} - categories are not modified.
#' @return An object of class \code{Banner}.
#' @examples
#' \dontrun{
#' banner_data <- banner(crunch_dataset,
#'     vars = list(subBanner1 = c('alias1', 'alias2'), subBanner2 = c('alias3')),
#'     labels = c(alias1 = 'var1 label', alias2 = 'var2 label'),
#'     recodes = list(
#'         alias1 = list(cat1a = 'new cat1a', cat1b = NA),
#'         alias2 = list(cat2a = 'new cat2a', cat2b = 'new cat2b', .default = NA))
#'    )
#' }
#' @importFrom crunch alias allVariables types categories subvariables is.dataset
#' @importFrom stats ave
#' @export
banner <- function(dataset, vars, labels = NULL, recodes = NULL) {

  wrong_class_error(dataset, "CrunchDataset", "dataset")

  if (!(is.vector(vars) && is.recursive(vars))) {
    stop("`vars` must be a list of vectors.", call. = FALSE)
  }
  if (is.null(names(vars))) {
    names(vars) <- paste0("Banner", seq_along(vars))
  }
  names(vars)[names(vars) == ""] <- paste0("Banner", which(names(vars) == ""))
  dups <- names(vars)[duplicated(names(vars))]
  names(vars)[duplicated(names(vars))] <- paste0(
    dups,
    as.numeric(ave(dups, dups, FUN = seq_along)) + 1)

  vars_vec <- unique(unlist(vars))
  if (length(vars_vec) == 0) {
    stop("`vars` must have a length greater than 0.", call. = FALSE)
  }
  error_if_items(
    names(vars)[sapply(vars, length) == 0],
    "No variables found in {items} in `vars`. {items} will be ignored.",
    error = FALSE, and = TRUE, quotes = TRUE)
  vars <- vars[sapply(vars, length) != 0]
  error_if_items(
    setdiff(vars_vec, aliases(allVariables(dataset))),
    "Variables in `vars` must be valid aliases in aliases(allVariables(dataset)). This is not true for {items}.",
    and = TRUE, quotes = TRUE)
  if (!is.null(labels)) labels <- lapply(labels, function(l) return(l))
  if (!is.null(labels) && is.null(names(labels))) {
    stop("`labels` must be a named list or vector.", call. = FALSE)
  }
  error_if_items(setdiff(names(labels), vars_vec),
                 "Variables in `labels` must be included in `vars`. This is not true for {items}.",
                 and = TRUE, quotes = TRUE)
  if (!is.null(recodes) && is.null(names(recodes))) {
    stop("`recodes` must be a named list of lists.", call. = FALSE)
  }
  error_if_items(
    setdiff(names(recodes), vars_vec),
    "Variables in `recodes` must be included in `vars`. This is not true for {items}.",
    and = TRUE, quotes = TRUE)
  error_if_items(
    if (!is.null(recodes)) names(recodes)[!sapply(recodes, is, "list")],
    "`recodes` must be a list of lists. This is not true for {items}.")

  ds_vars <- allVariables(dataset[vars_vec])

  error_if_items(aliases(ds_vars)[!(types(ds_vars) %in% c("categorical", "multiple_response"))],
                 "Variables in `vars` must be of type categorical or multiple_response. This is not true for {items}.",
                 and = TRUE, quotes = TRUE)

  variables <- list()
  for (i in 1:length(ds_vars)) {
    var <- ds_vars[[i]]
    alias <- alias(var)
    name <- if (is.null(labels[[alias]])) {
      name(var)
    } else {
      labels[[alias]]
    }

    type <- type(var)
    responses <- if (type %in% "multiple_response") {
      subvariables(dataset[[alias]])
    } else {
      na.omit(categories(dataset[[alias]]))
    }

    variables[[i]] <- structure(
      c(alias = alias, name = name, type = type,
        recode_categories(alias, responses, recodes[[alias]])),
      class = "BannerVar")
  }

  names(variables) <- aliases(ds_vars)

  total <- structure(
    list(alias = "___total___", name = "", type = "Total",
         old_categories = "Total", categories_out = "Total", categories = "Total"),
    class = "BannerVar")

  structure(sapply(vars, function(var) {
    c("___total___" = list(total), variables[var])
  }, simplify = FALSE), class = "Banner")
}

#' Recode Categories
#'
#' Given a question alias, recode the responses in
#' that question based on the recodes provided.
#'
#' @param alias A question alias
#' @param responses The responses for the specified question alias
#' @param recodes A character vector of recodes for those responses
recode_categories <- function(alias, responses, recodes) {
  error_if_items(
    setdiff(names(recodes), c(names(responses), ".default")),
    paste0(
      "Responses in `recodes` must be included in variable responses. This is not true for {items} in '",
      alias, "'."),
    quotes = TRUE,
    and = TRUE
  )

  if (!is.null(recodes))
    recodes <- lapply(recodes, function(r) return(r))

  if (!is.null(recodes) && ((!is.null(recodes[[".default"]]) && !is.na(recodes[[".default"]])) ||
                            (any(duplicated(unlist(recodes)[!is.na(unlist(recodes))]))))) {
    stop("Combining categories is not currently supported. Please check '", alias, "' recodes.", call. = FALSE)
  }
  if (!is.null(recodes[[".default"]])) {
    recodes[setdiff(names(responses), names(recodes))] <- recodes[[".default"]]
    recodes[[".default"]] <- NULL
  }
  recodes[setdiff(names(responses), names(recodes))] <- setdiff(names(responses), names(recodes))
  recodes <- recodes[names(responses)]
  return(list(old_categories = names(recodes),
              categories_out = setNames(unlist(recodes, use.names = FALSE), NULL),
              categories = setdiff(unlist(recodes, use.names = FALSE), NA)))
}
