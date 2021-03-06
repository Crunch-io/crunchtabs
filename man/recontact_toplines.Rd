% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/recontactReports.R
\name{recontact_toplines}
\alias{recontact_toplines}
\title{Recontact Toplines}
\usage{
recontact_toplines(
  dataset,
  questions,
  suffixes,
  labels,
  weights = crunch::weight(dataset),
  default_weight = crunch::alias(crunch::weight(dataset))
)
}
\arguments{
\item{dataset}{A crunch dataset}

\item{questions}{A character vector of aliases that should be included in the
report. If your recontact has been named using a suffix such as _pre, _post
leave that out.}

\item{suffixes}{The suffixes of recontact questions, for example _pre, _post}

\item{labels}{Formal labels for
the election", "After the election".}

\item{weights}{A character vector of equal to the length of suffixes. You may
specify a unique weight per recontact period. The default would return
all variables with the default survey weighting `weight(ds)`. Your weights
should be in the same order as your suffixies.}

\item{default_weight}{The default weight of the dataset, if any.}
}
\description{
Allows the user to create a simple report that shows recontact question.
}
