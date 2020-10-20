#' Recontact Toplines
#'
#' Allows the user to create a simple report that shows recontact question.
#' @param dataset A crunch dataset
#' @param questions A character vector of aliases that should be included in the
#' report. If your recontact has been named using a suffix such as _pre, _post
#' leave that out.
#' @param suffixes The suffixes of recontact questions, for example _pre, _post
#' @param labels Formal labels for your recontact period. For example, "Before
#' the election", "After the election".
#' @param weights A character vector of equal to the length of suffixes. You may
#' specify a unique weight per recontact period. The default, NULL, would return
#' all variables with the default survey weighting `weight(ds)`. Your weights
#' should be in the same order as your suffixies.
recontact_toplines <- function(dataset, questions, suffixes, labels,
                               weights = NULL) {

  stopifnot(is.dataset(dataset))
  stopifnot(is.character(questions))
  stopifnot(is.character(suffixes))
  stopifnot(is.character(labels))

  groupings <- lapply(questions, function(x) paste0(x, suffixes))
  names(groupings) <- questions
  vars <- unlist(groupings)

  if (is.null(weights)) {
    weight_spec <- weight(ds)
  } else {
    weight_spec <- lapply(suffixes, function(x) vars[grepl(x, vars)])
    names(weight_spec) <- weights
  }

  ct <- crosstabs(
    dataset,
    vars = c(unlist(groupings), names(weight_spec)),
    weight = weight_spec,
    include_original_weighted = FALSE
  )

  default_weight <- alias(weight(dataset))

  for (question in questions) {
    # TODO: Here we have to rename because the non-default weighted
    # variable is appended with it's weight. This could cause problems
    # if the pre_ variable has a weight that is not the default.
    #
    # Surely there's a more elegant way to do this.

    if (!is.null(weights)) {

      if (weights[1] == default_weight) {
        p1 <- groupings[[question]][1]
      } else {
        p1 <- paste0(groupings[[question]][1],"_", weights[1])
      }

      if (weights[2] == default_weight) {
        p2 <- groupings[[question]][2]
      } else {
        p2 <- paste0(groupings[[question]][2],"_", weights[2])
      }

    } else {

      p1 <- groupings[[question]][1]
      p2 <- groupings[[question]][2]
    }

    ct$results[[question]] <- as.ToplineCategoricalArray(
      ct$results[[p1]],
      ct$results[[p2]],
      labels,
      weights
    )

    ct$results[[p1]] <- NULL
    ct$results[[p2]] <- NULL

  }

  ct
}

#' Combine two questions as a categorical array
#'
#' Here we manipulate the tabBook results so that they match the layout
#' of a categoricalArray, which has the benefit of already having
#' distinct code to write it to latex.
#'
#' @param q1 The results object for the first question
#' @param q2 The results onject second question
#' @param labels Two character strings used to describe the pre and post waves
#' @param weights A single alias, list, or NULL
as.ToplineCategoricalArray <- function(q1, q2, labels = c("Pre", "Post"), weights) {

  q1$subnames <- labels
  q1$notes <- paste0(labels, " is weighted by ", weights, collapse = " : ")
  q1$type <- "categorical_array"

  matrix_rows <- length(
    attr(q1$crosstabs$Results$`___total___`$counts, "dimnames")[[1]]
  )

  # Build counts
  m <- matrix(
    c(
      as.numeric(q1$crosstabs$Results$`___total___`$counts),
      as.numeric(q2$crosstabs$Results$`___total___`$counts)),
      nrow = matrix_rows
  )

  dimnames(m) <- list(
    attr(q1$crosstabs$Results$`___total___`$counts, "dimnames")[[1]],
    labels
  )

  q1$crosstabs$Results$`___total___`$counts <- m

  # Build proportions
  m <- matrix(
    c(
      as.numeric(q1$crosstabs$Results$`___total___`$proportions),
      as.numeric(q2$crosstabs$Results$`___total___`$proportions)
      ),
      nrow = matrix_rows
  )

  dimnames(m) <- list(
    attr(q1$crosstabs$Results$`___total___`$counts, "dimnames")[[1]],
    labels
  )

  q1$crosstabs$Results$`___total___`$proportions <- m

  class(q1) <- c("ToplineCategoricalArray", "ToplineVar", "CrossTabVar")

  q1

}
