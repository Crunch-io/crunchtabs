#' Reflow Question Numbers
#'
#' When we manually add summaries of
#' numeric, datetime or text variables
#' we must "reflow" the question numbers
#' so that they match dataset order.
#'
#' @param x A results object from within the \link{crosstabs} function.
reflowQuestionNumbers <- function(x) {
  for (i in 1:length(x)) {
    x[[i]]$number <- i
  }
  x
}

#' Prepare Summary Content
#'
#' Prepare summary content for toplines for classes that
#' are not covered by tabBook such as NumericVariable, DatetimeVariables
#' and TextVariable
#'
#' @param x A variable of class NumericVariable, DatetimeVariable or TextVariable
#' @param weighted Logical. Are these data weighted?
#' @param num The number of verbatim responses to present as a sample. Defaults to 10.
#' @param tz A timezone. Defaults to UTC.
#' @param ... Additional arguments passed to methods
#' @export
nonTabBookSummary <- function(x, ...) {
  UseMethod("nonTabBookSummary", x)
}

#' @rdname nonTabBookSummary
#' @export
nonTabBookSummary.default <- function(x, weighted = TRUE, num = 10, tz = "UTC", ...) {
  wrong_class_error(
    x,
    c(
      "CategoricalVariable",
      "CategoricalArrayVariable",
      "MultipleResponseVariable",
      "TextVariable",
      "NumericVariable",
      "DatetimeVariable"
    ), "nonTabBookSummary"
  )
}

#' Prepare Numeric Content
#'
#' \link[crunch]{tabBook} does not report an appropriate numeric summary
#' without being provided with a multitable. So we "fake" a numeric summary
#' by overwriting the structure of a categorical object.
#'
#' If data are weighted we display Weighted N instead of Unweighted
#' N
#'
#' @param x A variable of class \link[crunch]{NumericVariable}
#' @inheritParams nonTabBookSummary
#' @importFrom stats median quantile
#' @export
nonTabBookSummary.NumericVariable <- function(x, weighted = TRUE, num = 10, tz = "UTC", ...) {
  y <- as.vector(x)
  qt <- quantile(y, na.rm = TRUE)
  minima <- min(y, na.rm = TRUE)
  maxima <- max(y, na.rm = TRUE)
  half <- median(y, na.rm = TRUE)
  mu <- mean(y, na.rm = TRUE)
  firstq <- qt[2]
  thirdq <- qt[4]
  stdev <- sd(y, na.rm = TRUE)

  # Mock the content object

  obj <- resultsObject(
    x,
    top = NULL,
    weighted = weighted,
    body_values = c(minima, firstq, half, mu, thirdq, maxima, stdev),
    body_labels = c(
      "Minimum",
      "1st Quartile",
      "Median",
      "Mean",
      "3rd Quartile",
      "Maximum",
      "Standard Deviation"
    ),
    vector = y
  )

  obj
}

#' Prepare Datetime Content
#'
#' tabBook does not report an appropriate date time summary without
#' being provided with a multitable. So we "fake" a date time summary
#' by overwriting the structure of a categorical object.
#'
#' If data are weighted we display Weighted N instead of Unweighted
#' N
#'
#' @param x A variable of class \link[crunch]{DatetimeVariable}
#' @inheritParams nonTabBookSummary
#' @export
nonTabBookSummary.DatetimeVariable <- function(x, weighted = TRUE, num = 10, tz = "UTC", ...) {
  y <- as.POSIXct(as.vector(x))
  qt <- quantile(y, na.rm = TRUE)
  minima <- min(y, na.rm = TRUE)
  maxima <- max(y, na.rm = TRUE)
  half <- median(y, na.rm = TRUE)
  firstq <- qt[2]
  thirdq <- qt[4]

  # Mock the content object
  obj <- resultsObject(
    x,
    top = NULL,
    weighted = weighted,
    body_values = c(minima, firstq, half, thirdq, maxima),
    body_labels = c(
      "Minimum",
      "1st Quartile",
      "Median",
      "3rd Quartile",
      "Maximum"
    ),
    vector = y
  )

  obj
}

#' Prepare Text Content
#'
#' tabBook does not report an appropriate date time summary without
#' being provided with a multitable. So we "fake" a date time summary
#' by overwriting the structure of a categorical object.
#'
#' If data are weighted we display Weighted N instead of Unweighted
#' N
#'
#' @param x A variable of class \link[crunch]{TextVariable}
#' @inheritParams nonTabBookSummary
#' @export
nonTabBookSummary.TextVariable <- function(x, weighted = TRUE, num = 10, tz = "UTC", ...) {
  set.seed(42)
  y <- as.vector(x)
  z <- sort(sample(unique(y[!is.na(y)]), num, replace = FALSE))
  n <- sum(!is.na(z)) # This could be wrong for un-enforced responses such as ""

  # Mock the content object
  obj <- resultsObject(
    x,
    weighted = weighted,
    body_values = rep("", length(z)),
    body_labels = z,
    vector = y
  )

  obj
}


#' Generic Results Object
#'
#' As \link[crunch]{tabBook} does not provide us with a way to  create summaries
#' for some variable types we are forced to create an object that bypasses
#' the reformatVar function. Our goal is to use as much of the
#' code infrastructure for theming purposes as possible while
#' allowing the creation of new topline summary objects
#'
#' @param x A dataset variable
#' @param top The top of the results object. NULL by default
#' @param weighted Logical. Are these data weighted?
#' @param body_values The values to present
#' @param body_labels The labels to present
#' @param vector The data vector
resultsObject <- function(x, top = NULL, weighted, body_values, body_labels, vector) {
  stopifnot(length(body_values) == length(body_labels))

  top <- top
  data_list <- list()
  data_list$body <- data.frame(
    x = body_values,
    row.names = body_labels
  )
  names(data_list$body) <- NA_character_

  # Presentation differences if data are
  # weighted or unweighted

  if (weighted) {
    data_list$weighted_n <- data.frame(
      x = sum(!is.na(vector)),
      row.names = "Weighted N"
    )
    names(data_list$weighted_n) <- NA_character_

    bottom <- c(weighted_n = "weighted_n")
    data_order <- c("body", weighted_n = "weighted_n")
  } else {
    data_list$unweighted_n <- data.frame(
      x = sum(!is.na(vector)),
      row.names = "Unweighted N"
    )
    names(data_list$unweighted_n) <- NA_character_

    bottom <- c(unweighted_n = "unweighted_n")
    data_order <- c("body", unweighted_n = "unweighted_n")
  }

  structure(
    list(
      alias = crunch::alias(x),
      name = crunch::name(x),
      description = ifelse(
        crunch::description(x) == "",
        crunch::name(x),
        crunch::description(x)
      ),
      notes = crunch::notes(x),
      type = class(x)[1],
      top = NULL,
      bottom = bottom,
      data_order = data_order,
      inserts = rep("Category", length(body_values)),
      data_list = data_list,
      min_cell_top = NULL,
      no_totals = TRUE,
      mean_median = FALSE,
      min_cell_body = matrix(rep(NA, length(body_values))),
      min_cell_bottom = matrix(FALSE),
      min_cell = FALSE,
      rownames = c(body_labels, ifelse(weighted, "Weighted N", "Unweighted N"))
    ),
    class = c("ToplineVar", "CrossTabVar")
  )
}

#' Compute a Tab Book
#'
#' This function allows you to generate a tab book from a multitable and data.
#' As with other functions, you can select the rows and columns you want to
#' work with by subsetting the `dataset` you pass into the function.
#'
#' By specifying a "json" `format`, instead of generating an Excel
#' workbook, you'll get a TabBookResult object, containing nested CrunchCube
#' results. You can then further format these and construct custom tab reports.
#'
#' Tabbook pages are organized in the order the variables are stored by the
#' server, unless complex weights are specified, in which case, the variables
#' are sorted in the order of the weight specification dataset (like the
#' one created by `tabBookWeightSpec()`).
#'
#' @param multitable a `Multitable` object
#' @param dataset CrunchDataset, which may be subset with a filter expression
#' on the rows, and a selection of variables to use on the columns.
#' @param weight a CrunchVariable that has been designated as a potential
#' weight variable for `dataset`, or `NULL` for unweighted results.
#' Default is the currently applied [`weight`]. Additionally, weights can be
#' set on a per variable basis for json export only. To do so, specify the weight
#' as either a list (which will be passed to `tabBookWeightSpec()`, or a data.frame
#' that mimics the structure. See [`tabBookWeightSpec()`] for more details.
#' @param output_format character export format: currently supported values are "json"
#' (default) and "xlsx".
#' @param file character local filename to write to. A default filename will be
#' generated from the `multitable`'s name if one is not supplied and the
#' "xlsx" format is requested. Not required for "json" format export.
#' of \code{\link{filters}} defined in the dataset.
#' @param use_legacy_endpoint Logical, indicating whether to use a 'legacy'
#' endpoint for compatibility (this endpoint will be removed in the future).
#' Defaults to `FALSE`, but can be set in the function, or with the environment
#' variable `R_USE_LEGACY_TABBOOK_ENDPOINT` or R option
#' `use.legacy.tabbook.endpoint`.
#' @param append_default_wt passed to [`tabBookWeightSpec()`] if `weight` is a list
#' @param ... Additional "options" passed to the tab book POST request.
#' More details can be found
#' [in the crunch API documentation](
#' https://docs.crunch.io/endpoint-reference/endpoint-multitable.html#options)
#' or [for the legacy endpoint](
#' https://docs.crunch.io/endpoint-reference/endpoint-tabbook.html#options)
#' @return If "json" format is requested, the function returns an object of
#' class `TabBookResult`, containing a list of `MultitableResult`
#' objects, which themselves contain `CrunchCube`s. For single weight tabbook,
#' the variables are always sorted in the order the server stores them in,
#' but complex weights are sorted in the order of the data.frame given. If
#' "xlsx" is requested, the function invisibly returns the filename (`file`,
#' if specified, or the the autogenerated file name). If you request "json" and
#' wish to access the JSON data underlying the `TabBookResult`, pass in a path
#' for `file` and you will get a JSON file written there as well.
#' @examples
#' \dontrun{
#' m <- newMultitable(~ gender + age4 + marstat, data = ds)
#' tabBook(m, ds, format = "xlsx", file = "wealthy-tab-book.xlsx", filter = "wealthy")
#' book <- tabBook(m, ds) # Returns a TabBookResult
#' tables <- prop.table(book, 2)
#' }
#' @importFrom jsonlite fromJSON
#' @export
tabBook_crunchtabs <- function(multitable, dataset, weight = crunch::weight(dataset),
                               append_default_wt = TRUE) {
  if (is.null(weight) | is.variable(weight)) {
    return(tabBookSingle_crunchtabs(multitable, dataset, weight))
  } else if (is.list(weight) || is.data.frame(weight)) {
    return(tabBookMulti_crunchtabs(
      multitable,
      dataset,
      weight,
      append_default_wt
    ))
  } else {
    stop("weight must be NULL, a CrunchVariable or a list indicating a multi-weight spec")
  }
}

tabBookSingle_crunchtabs <- function(multitable, dataset, weight) {
  if (!is.null(weight)) {
    weight <- self(weight)
  }
  # filter <- standardize_tabbook_filter(dataset, filter)
  body <- list(
    filter = NULL,
    weight = weight,
    options = list(format = NULL)
  )

  body$where <- varFilter(dataset)

  tabbook_url <- crunch::shojiURL(multitable, "views", "export")

  ## POST the query, which (after progress polling) returns a URL to download
  result <- crunch::crPOST(tabbook_url,
    config = httr::add_headers(`Accept` = "application/json"),
    body = crunch::toJSON(body)
  )

  out <- download_result(result)
  return(crunch:::TabBookResult(out))
}


varFilter <- function(dataset) {
  crunch:::variablesFilter(dataset)
}

download_result <- function(result) {
  crunch:::retry(crunch::crGET(result), wait = 0.5) # For mocks
}

tabBookResult <- function(...) {
  crunch:::TabBookResult(...) # For mocks
}

#' @importFrom stats ave
#' @importFrom utils stack
tabBookMulti_crunchtabs <- function(
                                    multitable,
                                    dataset,
                                    weight_spec,
                                    append_default_wt) {
  if (length(weight_spec) == 0) {
    stop("Empty list not allowed as a weight spec, use NULL to indicate no weights")
  }

  if (is.data.frame(weight_spec) && !setequal(names(weight_spec), c("weight", "alias"))) {
    stop("if weight_spec is a data.frame it must have exactly two columns: 'weight' & 'alias'")
  }

  if (any(duplicated(weight_spec))) {
    stop("Found duplicate weight and alias combinations in weight_spec")
  }

  # We can't trust that weight variables are included in the dataset subset
  # that we are using for the tabbook, so we need to load the full variable list
  # NB: The `relative=on` is to get a cache hit, and might need to change
  # (comes from `variablesFilter()`)
  all_dsvars <- getCatalog(dataset)

  if (is.list(weight_spec)) weight_spec <- tabBookWeightSpec(dataset, weights = weight_spec, append_default_wt)

  wt_vars <- unique(weight_spec$weight)
  # Add a column that indicates what page the variable will be on
  # in the weight-specific tabbook
  weight_spec$page_num <- as.numeric(ave(weight_spec$weight, weight_spec$weight, FUN = seq_along))

  books <- lapply(wt_vars, function(wt) {
    page_vars <- weight_spec$alias[weight_spec$weight == wt]
    tabBookSingle_crunchtabs(
      multitable,
      dataset[page_vars],
      weight = dataset[[wt]]
    )
  })

  names(books) <- wt_vars

  # stitch together
  # Most of the objects should be the same because they come from the same multitable
  # But the analyses object contain an item per variable with the weight included
  # and then the pages section contains the cube results
  analyses <- mapply(
    weight = weight_spec$weight,
    page_num = weight_spec$page_num,
    FUN = function(weight, page_num) {
      books[[which(names(books) == weight)]]@.Data[[1]]$analyses[[page_num]]
    },
    SIMPLIFY = FALSE,
    USE.NAMES = FALSE
  )
  pages <- mapply(
    weight = weight_spec$weight,
    page_num = weight_spec$page_num,
    FUN = function(weight, page_num) {
      books[[which(names(books) == weight)]]@.Data[[2]][[page_num]]
    },
    SIMPLIFY = FALSE,
    USE.NAMES = FALSE
  )

  combined <- books[[1]] # start with first one for skeleton
  combined@.Data[[1]]$analyses <- analyses
  combined@.Data[[2]] <- pages
  combined
  return(combined)
}


#' Helper function for setting complex weights on a `tabbook`
#'
#' For json [`tabBook()`], you can specify a weight per variable in the
#' dataset, where each row in the data.frame indicates a weight and
#' alias to use for each page.
#'
#' @param dataset A `CrunchDataset`
#' @param weights A list where each item has a name that indicates the
#' weight's alias that should be use (no name indicates unweighted) and
#' each item is a vector of variable aliases to include as pages in the
#' `tabbook`.
#' @param append_default_wt Whether to append the dataset's default weight
#' (or unweighted pages if no weight is set) for all variables.
#'
#' @return A data.frame with two columns, `weight`, the alias of the weight to use,
#' and alias, the alias of the variable to use the weight on. If `append_default_wt`
#' is `TRUE`, the returned object is sorted in the order of aliases in the dataset,
#' and with the default weight first, followed by the weights specified in the `weights`
#' argument.
#' @export
#'
#' @examples
#' \dontrun{
#' ds <- newExampleDataset("pets")
#' mt <- newMultitable(~q1, ds)
#'
#' weight_spec <- tabBookWeightSpec(
#'   ds,
#'   list(wt1 = "gender", wt2 = "starttime", "gender")
#' )
#'
#' # Now can use the weight spec in `tabBook()`
#' tabbook <- tabBook(mt, ds, weight = weight_spec)
#' }
tabBookWeightSpec <- function(dataset, weights, append_default_wt = TRUE) {
  weight_df <- stack(weights)
  names(weight_df) <- c("alias", "weight")
  # stack does mostly what we want, but we don't want factor
  weight_df$weight <- as.character(weight_df$weight)

  # If we don't need to append the default weights, we're done
  if (!append_default_wt) {
    return(weight_df)
  }

  default_weight <- if (is.null(weight(dataset))) "" else alias(weight(dataset))
  default_weight_df <- data.frame(
    alias = names(dataset),
    weight = default_weight,
    stringsAsFactors = FALSE
  )

  # Combine, but reorder so that the variables are in the same order as they are in the
  # original dataset, with the default weight first and then the weights
  # from the list are ordered after in the order they came in
  default_weight_df$wt_pos <- 0
  weight_df$wt_pos <- seq_len(nrow(weight_df))

  out <- rbind(weight_df, default_weight_df)
  out <- out[order(match(out$alias, names(dataset)), out$wt_pos), ]
  out$wt_pos <- NULL
  row.names(out) <- NULL # Really just for testing purposes

  duplicated <- duplicated(out)
  if (any(duplicated)) {
    warning("Dropping duplicated alias & weight combinations")
    out <- out[!duplicated, ]
  }
  out
}


getCatalog <- function(dataset) {
  crunch:::ShojiCatalog(crGET(self(allVariables(dataset)), query = list(relative = "on")))
}


extToContentType <- function(ext) {
  mapping <- list(
    json = "application/json",
    xlsx = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
    pptx = "application/vnd.openxmlformats-officedocument.presentationml.presentation"
  )
  return(mapping[[ext]])
}
