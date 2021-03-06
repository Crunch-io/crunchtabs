% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tabbook-additions.R
\name{tabBookWeightSpec}
\alias{tabBookWeightSpec}
\title{Helper function for setting complex weights on a `tabbook`}
\usage{
tabBookWeightSpec(dataset, weights, append_default_wt = TRUE)
}
\arguments{
\item{dataset}{A `CrunchDataset`}

\item{weights}{A list where each item has a name that indicates the
weight's alias that should be use (no name indicates unweighted) and
each item is a vector of variable aliases to include as pages in the
`tabbook`.}

\item{append_default_wt}{Whether to append the dataset's default weight
(or unweighted pages if no weight is set) for all variables.}
}
\value{
A data.frame with two columns, `weight`, the alias of the weight to use,
and alias, the alias of the variable to use the weight on. If `append_default_wt`
is `TRUE`, the returned object is sorted in the order of aliases in the dataset,
and with the default weight first, followed by the weights specified in the `weights`
argument.
}
\description{
For json [`tabBook()`], you can specify a weight per variable in the
dataset, where each row in the data.frame indicates a weight and
alias to use for each page.
}
\examples{
\dontrun{
ds <- newExampleDataset("pets")
mt <- newMultitable(~q1, ds)

weight_spec <- tabBookWeightSpec(
  ds,
  list(wt1 = "gender", wt2 = "starttime", "gender")
)

# Now can use the weight spec in `tabBook()`
tabbook <- tabBook(mt, ds, weight = weight_spec)
}
}
