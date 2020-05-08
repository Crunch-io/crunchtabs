
with_api_fixture <- function(fixture_path, expr) {
  with(
    crunch::temp.options(
      crunch.api = "https://app.crunch.io/api/",
      httptest.mock.paths = fixture_path,
      crunch.show.progress = FALSE
    ),
    httptest::with_mock_api(
      # Also need to redact UUIDs as is done in POSTs
      with_mock(
        `crunch::crPOST` = function(...) {
          args <- list(...)
          args$body <- gsub("([0-9a-f]{6})[0-9a-f]{26}", "\\1", args$body)
          do.call(
            function(...) crunch:::crunchAPI("POST", ...),
            args
          )
        },
        expr
      )
    )
  )
}

context("prepareExtraSummary NumericVariable")

with_api_fixture("fixtures-1-2-5", {

  ds <- loadDataset("Example dataset")

  test_that("dataset is not weighted", {
    expect_null(weight(ds))
  })

  test_that("dataset is accessible", {
    expect_true(exists("ds"))
  })

  ct <- crosstabs(ds)

})

context("prepareExtraSummary DatetimeVariable")
context("prepareExtraSummary TextVariable")
