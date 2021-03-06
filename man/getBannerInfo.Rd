% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reformatResults.R
\name{getBannerInfo}
\alias{getBannerInfo}
\title{Banner Meta Data}
\usage{
getBannerInfo(banner, theme)
}
\arguments{
\item{banner}{A banner from \link{banner}}

\item{theme}{A crunchtabs theme object}
}
\description{
Create a list containing meta data regarding the banner allowing for
latex placement decisions to be made.
}
\details{
Meta data includes the following as a list:
\itemize{
\item empty_col A logical identifying if the column is empty
\item len A vector identifying the length of the category responses
\item mulitcols A vector of categories
\item multicols_csum A cumulative sum of columns
\item format_cols Numeric reference to which columns are to be formatted
\item border_cols Numeric reference to which columns should have a border
\item names The names of the columns to be formatted
}
}
