% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/relabel.R
\name{relabel}
\alias{relabel}
\title{Relabel question items and options}
\usage{
relabel(ct, ...)
}
\arguments{
\item{ct}{An object from \link{crosstabs}, \link{recontact_toplines},
or \link{trackingReport}}

\item{...}{One or more relabel objects. See \link{relabelSet}}
}
\description{
This function is a passthrough that allows you to relabel question items, options, question
descritions and notes before writing to latex or excel. The relabeling must occur in
questionnaire order. As it is received by crunch, so should it be set by your relabeling objects.
}
\details{
There are two important warnings to consider:
\itemize{
\item Relabeling should always occur before applying any kind of sorting using the
\link{sortAliases} function to avoid situations where labels could be applied
inappropriately.
\item If your results object is a tracking report or recontact it is strongly recommended that you
set your wave labels in the \link{recontact_toplines} or \link{trackingReport}.
However, if you must set it here options would be the categories and items would be the wave
names. We cannot account for aliases with only partial data (n-1 waves), you must set the
wave names in order.
}
}
\examples{
\dontrun{
library(crunchtabs)
login()

ds <- loadDataset("Example dataset")
ct <- crosstabs(ds)
ct <- relabel(ct,
  list(
    alias = "q1",
    options = c("Lovely Cat", "Smelly Dog", "Annoying Bird"), # Cat, Dog, Bird
    description = "Which pet do you love the most?"), # What is your favorite pet?
    notes = "Relabeled by an undeniable cat-lover" # No note before
  list(alias = "petloc",
    options = c("Lovely Cat", "Smelly Dog", "Annoying Bird"), # Cat, Dog, Bird
    items = c("In my home", "At my work")) # Home, Work
)
writeLatex(ct, pdf = TRUE)
}

}
