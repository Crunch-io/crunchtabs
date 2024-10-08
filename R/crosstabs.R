#' Toplines and Crosstabs Summaries
#'
#' \code{crosstabs} produces Toplines (one-way frequency tables) or Crosstabs (cross tabulations)
#' summaries of a Cruch dataset.
#'
#' @param dataset A Crunch dataset.
#' @param vars An optional vector of aliases of the non-hidden variables that shoulds be used.
#' Defaults to all non-hidden variables.
#' @param weight The alias of a numeric variable that should be used for data
#' weighting. Alternatively a named list where the name is the alias of the weight
#' and the contents of the list component are a character vector of aliases to
#' which that weight should apply. Defaults to current weight variable. For
#' unweighted, set to \code{NULL}
#' @param banner An optional object of class \code{Banner} that should be used
#' to generate a Crosstabs summary. Defaults to \code{NULL} - a Toplines summary
#' is produced and returned.
#' @param codebook If \code{TRUE}, codebook data summaries are prepared. Defaults
#' to \code{FALSE}.
#' @param include_numeric Logical. Should we include numeric questions? Defaults
#' to FALSE. Implemented for Toplines only.
#' @param include_datetime Logical. Should we include date time questions? Defaults
#' to FALSE. Implemented for Toplines only.
#' @param include_verbatims Logical. Should we include a sample text varaibles?
#' Defaults to FALSE. Implemented for Toplines only.
#' @param include_original_weighted Logical. When providing list of weights to apply,
#' should we include the default weighted vars? Defaults to TRUE.
#' @param num_verbatims An integer identifying the number of examples to extract
#' from a text variable. Defaults to 10. Implemented for Toplines only.
#' @param filter A list of `CrunchExpression`s, `CrunchFilter`s, or string names
#' of filters to be combined with the filter applied to dataset passed into the
#' `dataset` argument.
#' @return A Toplines (when no banner is provided) or Crosstabs (when a banner
#' is provided) summary of the input dataset.
#' @examples
#' \dontrun{
#' toplines_summary <- crosstabs(crunch_dataset, weight = "weight")
#' crosstabs_summary <- crosstabs(crunch_dataset,
#'   vars = c("alias1", "alias2"),
#'   weight = "weight", banner = banner_object
#' )
#' }
#' @importFrom crunch name aliases allVariables is.Numeric is.dataset weight alias
#' weightVariables is.variable
#' @importFrom methods is
#' @export
crosstabs <- function(
                      dataset, vars = names(dataset), weight = crunch::weight(dataset),
                      banner = NULL, codebook = FALSE, include_numeric = FALSE,
                      include_datetime = FALSE, include_verbatims = FALSE,
                      num_verbatims = 10, include_original_weighted = TRUE,
                      filter = NULL) {
  wrong_class_error(dataset, "CrunchDataset", "dataset")
  # nolint start
  all_types <- crunch::types(crunch::allVariables(dataset))
  all_aliases <- crunch::aliases(crunch::allVariables(dataset))

  error_if_items(
    setdiff(vars, all_aliases),
    paste(
      "Variables in `vars` must be valid aliases in aliases(allVariables(dataset)).",
      "This is not true for {items}."
    ),
    and = TRUE,
    quotes = TRUE
  )

  if (!is.null(weight) & !is.list(weight)) {
    if (crunch::is.variable(weight)) {
      weight <- crunch::alias(weight)
    }

    if (!weight %in% all_aliases) {
      stop(
        "`weight`, if provided, must be a valid variable in `dataset`. '",
        weight, "' is not found."
      )
    }

    if (!weight %in% weightVariables(dataset)) {
      stop(
        "`weight`, if provided, must be a valid weight variable in `dataset`. '",
        weight,
        "' is not a weight variable."
      )
    }
  }
  if (!is.null(banner) && !is(banner, "Banner")) {
    stop("`banner`, if provided, must be an object of class 'Banner'.")
  }

  if (!is.null(weight) & !is.list(weight)) {
    weight_var <- dataset[[weight]]
  } else {
    weight_var <- weight
  }

  # TODO: Add check here to verify variables in weight are included
  # in vars and the ds

  vars_out <- if (codebook) {
    vars # nocov
  } else {
    intersect(
      vars,
      all_aliases[all_types %in% c("categorical", "multiple_response", "categorical_array")])
  }

  if (length(vars_out) == 0) {
    stop("No variables provided.")
  }

  banner_use <- if (is.null(banner)) {
    banner(
      dataset,
      vars = list(
        "Results" = all_aliases[all_types %in% c("categorical", "multiple_response")][1]
      )
    )
  } else {
    banner
  }

  results <- tabBooks(
    dataset = dataset,
    vars = vars_out,
    banner = banner_use,
    weight = weight_var,
    filter = filter,
    topline = is.null(banner),
    include_original_weighted = include_original_weighted
  )

  if (codebook) {
    res_class <- "Codebook"
  } else if (is.null(banner)) {
    res_class <- c("Toplines", "CrunchTabs")
  } else {
    res_class <- c("Crosstabs", "CrunchTabs")
  }

  banner <- banner_manipulation(banner, dataset, weight_var)

  # Here we create logic for including summaries
  # for variable types that did not previously have
  # summaries (Numeric, Datetime, Text)

  var_types <- unlist(lapply(dataset[vars], class))

  names(var_types) <- vars
  numerics <- vars[var_types == "NumericVariable"]
  datetimes <- vars[var_types == "DatetimeVariable"]
  verbatims <- vars[var_types == "TextVariable"]

  if (include_numeric & length(numerics) > 0) {
    # drop weighting vars
    weightVars <- unlist(
      lapply(numerics, function(x) is.weightVariable(dataset[[x]]))
    )

    numerics <- numerics[!weightVars]

    numRes <- lapply(numerics, function(x) {
      nonTabBookSummary(
        dataset[[x]],
        weighted = !is.null(weight)
      )
    })
    names(numRes) <- numerics

    results <- c(
      results, numRes
    )
  }

  if (include_datetime & length(datetimes) > 0) {
    datetimeRes <- lapply(datetimes, function(x) {
      nonTabBookSummary(
        dataset[[x]],
        weighted = !is.null(weight)
      )
    })

    names(datetimeRes) <- datetimes
    results <- c(
      results,
      datetimeRes
    )
  }

  if (include_verbatims & length(verbatims) > 0) {
    verbatimRes <- lapply(verbatims, function(x) {
      nonTabBookSummary(
        dataset[[x]],
        weighted = !is.null(weight)
      )
    })

    names(verbatimRes) <- verbatims

    results <- c(
      results,
      verbatimRes
    )
  }

  if (include_verbatims | include_datetime | include_numeric) {
    # If we include new question types we must reflow question
    # numbers because otherwise they will be missing from the
    # faked objects

    # First re-flow in dataset order
    tmpResults <- list()
    for (i in vars) {
      tmpResults[[i]] <- results[[i]]
    }
    # Then re-flow question numbers
    results <- reflowQuestionNumbers(tmpResults)
  }

  summary_data <- list(
    metadata = c(
      list(
        title = name(dataset), weight = weight,
        start_date = crunch::startDate(dataset), end_date = crunch::endDate(dataset),
        description = crunch::description(dataset)
      )
    ),
    results = results,
    banner = banner
  )

  class(summary_data) <- res_class

  return(summary_data)
  # nolint end
}


banner_manipulation <- function(banner, dataset, weight_var) {
  if (!is.null(banner)) { # nocov start
    banner <- lapply(banner, function(b) {
      lapply(b, function(b1) {
        if (b1$alias %in% "___total___") {
          b1$unweighted_n <- nrow(dataset)
          b1$weighted_n <- sum(as.vector(weight_var), na.rm = TRUE)
        } else {
          b1$unweighted_n <- setNames(
            as.array(
              crtabs(paste0("~", b1$alias), data = dataset, weight = NULL)), b1$categories_out)
          b1$unweighted_n <- b1$unweighted_n[!is.na(names(b1$unweighted_n))]
          b1$weighted_n <- setNames(
            as.array(crtabs(
              paste0("~", b1$alias), data = dataset, weight = weight_var)), b1$categories_out)
          b1$weighted_n <- b1$weighted_n[!is.na(names(b1$weighted_n))]
        }
        return(b1)
      })
    })
    class(banner) <- "Banner"
  }
  banner
  # nocov end
}

#' @describeIn crosstabs An alias for \code{crosstabs}
#' @export
toplines <- crosstabs
