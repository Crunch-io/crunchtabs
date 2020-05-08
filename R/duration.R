#' Survey Duration
#'
#' Calculate survey duration
#'
#' @param ds A crunch dataset
#' @param starttime A crunch variable containing the starttime
#' @param endtime A crunch variable containing the endtime
#' @param outliers At what factor of overall mean should outliers be dropped? Defaults to 2.5.
#' @export
surveyDuration <- function(ds, starttime=NULL, endtime=NULL, outliers = 2.5) {
  if (is.null(starttime)) starttime = ds$starttime
  if (is.null(endtime)) endtime = ds$endtime

  res = as.numeric(
    as.POSIXct(as.vector(ds$endtime)) - as.POSIXct(as.vector(ds$starttime))
  )

  # Incompletes are negative
  res[res < 0] = NA
  # 2.5x mean as outlier
  if (!is.na(outliers)) {
    res[res > mean(res, na.rm = TRUE) * outliers] = mean(res, na.rm = TRUE)
  }
  res
}

