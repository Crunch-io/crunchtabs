#' categoricalArray tex adjustments
#'
#' If a user wishes to show category/statement data that is too large to be shown comfortably on
#' the width of the PDF document, we provide functionality for renaming the columns headers of a
#' categoricalArray's presentation and putting them above or below the table in a itemized list.
#'
#' The default itemization scheme is uppercase letters. If you do not provide shortnames or aliases
#' (not to be confused with crunch aliases), you will see A, B, C, and so on for each column of
#' your categorical array
#'
#' @param bullets A logical. Defaults to TRUE. Should the output have bullets?
#' @param category_aliases Defaults to NULL. Otherwise, the categories will be itemized using
#' item_alias.
#' @param category_text Defaults to NULL. Otherwise, overwrites the category text
#' @param width A number representing the width of the LaTeX parbox that contains the category
#' list
#' @param result A crosstab result sub-object that is typically passed to \code{\link{writeLatex}}
catArrayTexAdjustments <- function(result, category_aliases = NULL, category_text = NULL,
                                   width = NULL, bullets = TRUE) {
  nms <- getCatArrayColNames(result)

  if(is.null(category_aliases)) {
    category_aliases <- LETTERS[seq_along(nms)]
  }

  if(!is.null(category_text)) {
    if (length(category_text) != length(nms))
      stop("Category text labels not the same length as result objects categories")

    nms <- category_text
  }

  if (length(category_aliases) != length(nms))
    stop("Category aliases not the same length as result objects categories")



  if(is.null(width))
    width <- 6.5

  l <- list()

  l[[1]] <- sprintf(
    "\\\\\n  \\hangindent=0em \\parbox{%sin}{\\formatvardescription{",
    width
  )

  l[[2]] <- "    \\begin{itemize}"

  if (bullets) {
    template <- "    \\item[] %s. %s"
  } else {
    template <- "    \\item %s. %s"
  }

  l[[3]] <- lapply(seq_along(nms), function(x) {
    sprintf(template, category_aliases[x], nms[x])
  }
  )

  l[[4]] <- "    \\end{itemize}"
  l[[5]] <- "  }}\n\\\\"

  paste0(unlist(l), collapse = "\n")
}

#' getter for categoricalArray column names
#'
#' @param result A crosstab result sub-object that is typically passed to \code{\link{writeLatex}}
getCatArrayColNames <- function(result) {
  dimnames(result$crosstabs$Results$`___total___`$counts)[[2]]
}

#' setter for categoricalArray column names
#'
#' @param result A crosstab result sub-object that is typically passed to \code{\link{writeLatex}}
#' @param category_aliases Alternative presentations of the category wording as a character vector.
#' Must be the same length as the number of categories.
setCatArrayColNames <- function(result, category_aliases) {
  nms <- dimnames(result$crosstabs$Results$`___total___`$counts)[[2]]

  if(length(nms) != length(category_aliases))
    stop("Category aliases do not match number of categories")

  dimnames(result$crosstabs$Results$`___total___`$proportions)[[2]] <- category_aliases

  return(result)
}
