context("trackingDeclareAvailability")

test_that("Returns single as expected", {
  rebuilt_results <- list()
  rebuilt_results$results <- list(
    somevar = list(
      notes = ""
    )
  )
  results_available <- 1
  labels = "Mar 2021"

  rebuilt_results <- trackingDeclareAvailability(
    rebuilt_results, results_available, var = "somevar", labels
  )

  expect_equal(rebuilt_results$results$somevar$notes, "Asked in Mar 2021")
  expect_equal(rebuilt_results$results$somevar$availability, "single")
})

test_that("Returns single as expected existing note", {
  rebuilt_results <- list()
  rebuilt_results$results <- list(
    somevar = list(
      notes = "Existing note"
    )
  )
  results_available <- 1
  labels = "Mar 2021"

  rebuilt_results <- trackingDeclareAvailability(
    rebuilt_results, results_available, var = "somevar", labels
  )

  expect_equal(rebuilt_results$results$somevar$notes, "Existing note (Asked in Mar 2021)")
  expect_equal(rebuilt_results$results$somevar$availability, "single")
})

test_that("Returns general as expected", {
  rebuilt_results <- list()
  rebuilt_results$results <- list(
    somevar = list(
      notes = "Existing note"
    )
  )
  results_available <- c(1,3,5)
  labels = paste0("Wave ", 1:5)

  rebuilt_results <- trackingDeclareAvailability(
    rebuilt_results, results_available, var = "somevar", labels
  )

  expect_equal(rebuilt_results$results$somevar$notes, "Existing note")
  expect_equal(rebuilt_results$results$somevar$availability, "general")
})
