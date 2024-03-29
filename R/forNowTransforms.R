#' Given a vector of values and elements, calculate the insertions
#'
#' @param vec values to transform (a single dimension of an array)
#' @param elements AbstractCategories of both `Category`s and `Insertion`s to
#' calculate. Generally derived from `mapInsertions()`
#' @param var_cats the `Categories` object tat corresponds to the vector in
#' `vec` of the transform
#'
#' @importFrom crunch is.Heading is.Subtotal ids
#' @return the values given in `vec`, with any insertions specified in
#' `trans` calculated and inserted
#' @keywords internal
calcTabInsertions <- function(vec, elements, var_cats) {

  # make the actual calculations and insertions
  vec_out <- do.call(rbind, lapply(elements, function(element) {
    # if element is a category, simply return the value
    if (inherits(element, "Category")) {
      return(vec[name(element), ])
    }

    # if element is a heading return NA (since there is no value to be
    # calculated but we need a placeholder non-number)
    if (crunch::is.Heading(element)) {
      return(NA)
    }

    # if element is a subtotal, sum the things it corresponds to which are
    # found with arguments()
    if (crunch::is.Subtotal(element)) {
      # Check if subtotal is strictly a sum of variables or a sum/difference
      if (is.null(element$negative) || length(element$negative) == 0) {
        # grab category combinations, and then sum those categories.
        combos <- element$categories
        which.cats <- names(var_cats)[crunch::ids(var_cats) %in% combos]
        if (any(is.na(var_cats)[crunch::ids(var_cats) %in% combos])) {
          return(NA) # nocov
        }
        if (dim(vec)[2] == 1) {
          return(sum(vec[which.cats, ]))
        }
        return(colSums(vec[which.cats, , drop = FALSE]))
      } else {
        # if element has a "negative" item, these need to be subtracted from
        # the other categories
        # grab category combinations, and then sum those categories.
        combos <- element$categories
        combos_negative <- as.integer(unlist(element$negative))
        which.cats <- names(var_cats)[crunch::ids(var_cats) %in% combos]
        which.cats_negative <- names(var_cats)[crunch::ids(var_cats) %in% combos_negative]
        if (any(is.na(var_cats)[crunch::ids(var_cats) %in% c(combos, combos_negative)])) {
          return(NA) # nocov
        }
        if (dim(vec)[2] == 1) {
          return(sum(vec[which.cats, ]) - sum(vec[which.cats_negative, ]))
        }
        return(colSums(vec[which.cats, , drop = FALSE]) - colSums(vec[which.cats_negative, , drop = FALSE]))
      }
    }
  }))

  colnames(vec_out) <- colnames(vec)

  # make sure that the vector is named appropriately
  rownames(vec_out) <- names(elements)

  return(vec_out)
}

#' Given an array of values and elements, calculates some summary statistics
#' of some columns
#'
#' @param vec An array of values and elements
#' @param var_cats The `Categories` object that corresponds to the object `vec`
#' @param a_func A function usually to calculates a summary statistic
#'
#' @return If `vec` has three dimentions, a row that results from applying `a_func`
#'  to each column of each object of the third dimention of `vec`; otherwise,
#'  a row that results from applying `a_func` to each column of `vec`
applyInsert <- function(vec, var_cats, a_func) {
  if (length(dim(vec)) == 3) {
    dt <- do.call(cbind, sapply(dimnames(vec)[[3]], function(xi) {
      apply(
        vec[, , xi, drop = FALSE], 2, a_func,
        na.omit(var_cats)
      )
    }, simplify = FALSE))
    dimnames(dt) <- dimnames(vec)[2:length(dim(vec))]
  } else {
    dt <- matrix(
      apply(vec, 2, a_func, na.omit(var_cats)),
      nrow = 1, dimnames =
        setNames(list(
          "",
          dimnames(vec)[[2]]
        ), names(dimnames(vec)))
    )
  }
  return(dt)
}

#' Given a vector of values, calculates the weighted mean
#'
#' @param vec A vector to use for calculation (a single dimension of an array)
#' @param var_cats The `Categories` object that corresponds to the vector in `vec`
#'
#' @return The weighted mean of the numeric values of `var_cats` given the weights
#'  in `vec`
#' @importFrom stats weighted.mean
#' @importFrom crunch values
calcTabMeanInsert <- function(vec, var_cats) {
  ok <- !is.na(vec) & !is.na(crunch::values(var_cats))
  return(stats::weighted.mean(crunch::values(var_cats)[ok], vec[ok]))
}

#' Given a vector of values, calculates the weighted median
#'
#' @param vec A vector to use for calculation (a single dimension of an array)
#' @param var_cats The `Categories` object that corresponds to the vector in `vec`
#'
#' @return The weighted median of the numeric values of `var_cats` given the
#'  weights in `vec`
#'
#' @importFrom crunch values
calcTabMedianInsert <- function(vec, var_cats) {
  ok <- !is.na(vec) & vec != 0 & !is.na(crunch::values(var_cats))
  if (all(!ok)) {
    return(NA) # nocov
  }
  num_values <- crunch::values(var_cats[ok])
  counts <- vec[ok]

  # weighted median function
  o <- order(num_values)
  num_values <- num_values[o]
  counts <- counts[o]
  perc <- cumsum(counts) / sum(counts)

  # if any of the bins are 0.5, return the mean of that and the one above it.
  if (any(as.character(perc) %in% as.character(0.5))) {
    n <- which(perc == 0.5)
    return((num_values[n] + num_values[n + 1]) / 2)
  }

  # otherwise return the first bin that is more than 50%
  over0.5 <- which(perc > 0.5)
  return(num_values[min(over0.5)])
}
