#' Combine two or more tabbook results as a categorical array
#'
#' Here we manipulate the tabBook results so that they match the layout
#' of a categoricalArray, which has the benefit of already having
#' distinct code to write it to latex.
#'
#' @param questions A list of two or more results objects. Objects should be
#' provided in time or
#' wave 3, ..., wave n)
#' @param question_alias The name of the question alias across all results
#' sets. It must be the same in all datasets.
#' @param labels A character vector of labels that will be displayed in the
#' resulting crunchtabs output. Should match the number of results objects
as.ToplineCategoricalArray <- function(
                                       questions, question_alias = NULL, labels) {
  if (length(questions) != length(labels)) {
    stop("Number of labels provided does not match number of result sets")
  }

  # Use the first result item as a skeleton
  obj <- questions[[1]]

  is_catarray <- questions[[1]]$type == "categorical_array"

  if (is_catarray) {
    return(
      catArrayToCategoricals(questions, question_alias, labels)
    )
  }

  counts <- obj$crosstabs$Results$`___total___`$counts
  second_label <- attr(counts, "dimnames")[[1]]

  obj$subnames <- labels

  obj$notes <- questions[[1]]$notes
  obj$type <- "categorical_array"
  obj$labels <- labels

  matrix_rows <- length(
    attr(counts, "dimnames")[[1]]
  )

  # We pull out counts per result item in wide format
  count_list <- lapply(questions, function(x) x$crosstabs$Results$`___total___`$counts)
  prop_list <- lapply(questions, function(x) x$crosstabs$Results$`___total___`$proportions)
  m <- categoryFill(count_list)

  dimnames(m)[[2]] <- as.character(labels)

  obj$crosstabs$Results$`___total___`$counts <- m

  # We pull out proportions per result item in wide format
  m <- categoryFill(prop_list)

  dimnames(m)[[2]] <- as.character(labels)
  obj$crosstabs$Results$`___total___`$proportions <- m

  obj$rownames <- rownames(m)

  class(obj) <- c("ToplineCategoricalArray", "ToplineVar", "CrossTabVar")

  obj
}

#' Decompose a categorical array
#'
#' Given two or more waves of a categorical array question, convert them into
#' categoricals for presentation in a tracking report.
#'
#' @param questions A list of results objects from tabBook
#' @param question_alias The name of the alias in question
#' @param labels The labels for the questions. Will default to Wave X, where X is 1:n
#' @export
catArrayToCategoricals <- function(questions, question_alias, labels) {
  obj <- questions[[1]]
  statements <- obj$subnames
  cats <- attr(obj$crosstabs$Results$`___total___`$proportions, "dimnames")[[1]]

  if (is.null(labels)) {
    labels <- paste0("Wave ", seq_along(questions))
  }

  # Collisions can occur so we randomize names because people version aliases sometimes
  # votefactors with sub vars + votefactors2 :/
  # randNames <- function(n) {
  #   a <- do.call(paste0, replicate(5, sample(LETTERS, n, TRUE), FALSE))
  #   paste0(a, sprintf("%04d", sample(9999, n, TRUE)), sample(LETTERS, n, TRUE))
  # }

  nms <- paste0(question_alias, "_", seq_along(statements))

  warning(
    "New variables derived from a `categorical_array`. If you need flipped grids",
    " add the following to latex_flip_specific_grids: ",
    paste0(nms, collapse = ", ")
  )

  # Create list of objects to fill in, one for each sub statement of the
  # multiple response group
  l <- lapply(statements, function(x) obj)

  # 1. Create a guide for the data we want to pull out
  # 2. Reorder the guide in a format that fits the
  # manner in which we wish to encapsulate it
  # 3. Create an NA filled object
  # 4. Push our data into the object
  guide <- expand.grid(
    seq_along(statements),
    seq_along(cats),
    seq_along(labels)
  )

  names(guide) <- c("statement", "cat", "label")

  # Order is key
  guide <- guide[with(guide, order(statement, cat, label)), ] # nolint
  rownames(guide) <- NULL
  guide$value <- NA_real_

  # Pull out our data
  for (i in seq_len(nrow(guide))) {
    guide$value[i] <- tryCatch({
       questions[[
        guide$label[i]
      ]]$crosstabs$Results$`___total___`$proportions[
        guide$cat[i], guide$statement[i]
      ]
    }, error = function(e) NA_real_)
  }

  # Pre allocate
  rprops <- lapply(seq_along(statements), function(x) {
    matrix(rep(NA_real_, length(cats) * length(labels)),
      ncol = length(labels)
    )
  })
  # Add dimnames for clarity
  rprops <- lapply(rprops, function(x) {
    dimnames(x) <- list(cats, labels)
    x
  })

  for (i in seq_len(nrow(guide))) {
    rprops[[guide$statement[i]]][guide$cat[i], guide$label[i]] <- guide$value[i]
  }

  # Reassign
  l <- lapply(seq_along(statements), function(x) {
    l[[x]]$alias <- nms[x]
    l[[x]]$subnames <- labels
    l[[x]]$rownames <- attr(obj$crosstabs$Results$`___total___`$counts, "dimnames")[[1]]
    l[[x]]$labels <- labels
    l[[x]]$description <- paste0(statements[x], " (", l[[x]]$description, ")")
    l[[x]]$crosstabs$Results$`___total___`$proportions <- rprops[[x]]
    class(l[[x]]) <- c("ToplineCategoricalArray", "ToplineVar", "CrossTabVar")
    return(l[[x]])
  })

  names(l) <- nms
  return(l)
}

#' Merge two data.frames by rownames
#'
#' This function is designed to cbind via rownames where
#' the rownames may not match and then fix the result so that
#' it can be further merged to another data.frame.
#' @param x A data.frame
#' @param y A data.frame
cbindFill <- function(x, y) {
  r <- merge(x, y, by = "row.names", all = TRUE, sort = FALSE)
  rownames(r) <- r$Row.names
  r$Row.names <- NULL
  r
}

#' Add position
#'
#' A small utility function to add position to a matrix
#' @param x A matrix
addPos <- function(x) {
  x[,1] <- 1:nrow(x)
  x
}

#' Column Bind Unequal Matrices
#'
#' This function takes a list of matrices and binds them together into
#' a single frame. Accounts for missing or unequal rows, by rowname.
#'
#' @param clist A list of matrices with rownames
categoryFill <- function(clist) {
  r <- lapply(clist, addPos)
  r <- do.call(rbind, r)
  r <- data.frame(nm = names(r[,1]), pos = r[,1])
  r <- r[!duplicated(r$nm),]
  rownames(r) <- r$nm
  r$nm <- NULL

  m <- Reduce(function(x,y) suppressWarnings(cbindFill(x,y)), clist)
  m <- suppressWarnings(merge(m, r, by = "row.names", all = TRUE, sort = FALSE))
  m <- m[with(m, order(pos)),]

  rownames(m) <- m$Row.names
  m$Row.names <- NULL
  m$pos <- NULL
  as.matrix(m)
}
