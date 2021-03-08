context("trackingReport_tabs")

test_that("tracking report returns a list of results", {
  mockery::stub(trackingReport_tabs, "crosstabs", "resultsObject")
  mockery::stub(trackingReport_tabs, "weight", NULL)
  res <- trackingReport_tabs(list("a", "b", "c"), vars = c(), weight = NULL)
  expect_equal(unlist(res), c(NULL, NULL, NULL))
})

context("trackingReport")

test_that("tracking report returns without cat arrays", {
  dataset_list <- readRDS(test_path("fixtures/trackingReport-dataset_list.rds"))
  ct <- readRDS(test_path("fixtures/trackingReport_tabs-crosstabs.rds"))
  mockery::stub(trackingReport, "trackingReport_tabs", list(ct, ct, ct))
  res <- trackingReport(dataset_list, vars = "allpets")

  expect_equal(
    res$results$allpets$crosstabs$Results$`___total___`$proportions,
    structure(c(
      0.735294117644663, 0.755102040814789, 0.672727272725442,
      0.735294117644663, 0.755102040814789, 0.672727272725442, 0.735294117644663,
      0.755102040814789, 0.672727272725442
    ), .Dim = c(3L, 3L), .Dimnames = list(
      c("Cat", "Dog", "Bird"), c("Wave 1", "Wave 2", "Wave 3")
    ))
  )
})

test_that("tracking report with cat arrays", {
  dataset_list <- readRDS(test_path("fixtures/trackingReport-dataset_list.rds"))
  tabs <- readRDS(test_path("fixtures/trackingReport_tabs-petloc.rds"))
  mockery::stub(trackingReport, "trackingReport_tabs", tabs)
  # mockery::stub(trackingReport, "trackingReport_tabs", questions)
  res <- suppressWarnings(trackingReport(dataset_list, vars = "petloc"))

  expect_named(res$results, c("petloc1", "petloc2"))
  expect_equal(
    res$results$petloc1$crosstabs$Results$`___total___`$proportions,
    structure(c(
      0.421875000002028, 0.484374999997521, 0.0937500000004507,
      0.5, 0.333333333333333, 0.166666666666667, 0.321428571427136,
      0.464285714288106, 0.214285714284758
    ), .Dim = c(3L, 3L), .Dimnames = list(
      c("Cat", "Dog", "Bird"), c("Wave 1", "Wave 2", "Wave 3")
    ))
  )
})
