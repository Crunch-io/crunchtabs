% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/banner.R
\name{banner}
\alias{banner}
\title{Create an object of class \code{Banner}}
\usage{
banner(dataset, vars, labels = NULL, recodes = NULL)
}
\arguments{
\item{dataset}{A Crunch dataset.}

\item{vars}{A named list of vectors of aliases of the variables that should
constitute the banner object. These should be categorical (and typically hidden)
variables present in the \code{dataset}.}

\item{labels}{An optional named list of labels, where names are variables' aliases
(present in the \code{vars} parameter) and values are the labels that should be
used in the report. Defaults to \code{NULL} - variables names are used.}

\item{recodes}{An optional named list of categories recodes (the syntax is similar
to the one used in the \code{recode} function in the R \code{dplyr} package).
Use \code{NA} to exclude categories. Not listed categories will be left unchanged.
Use \code{.default} to replace all not listed categories. See examples for details.
Defaults to \code{NULL} - categories are not modified.}
}
\value{
An object of class \code{Banner}.
}
\description{
Create an object of class \code{Banner}
}
\examples{
\dontrun{
banner_data <- banner(crunch_dataset,
  vars = list(subBanner1 = c("alias1", "alias2"), subBanner2 = c("alias3")),
  labels = c(alias1 = "var1 label", alias2 = "var2 label"),
  recodes = list(
    alias1 = list(cat1a = "new cat1a", cat1b = NA),
    alias2 = list(cat2a = "new cat2a", cat2b = "new cat2b", .default = NA)
  )
)
}
}
