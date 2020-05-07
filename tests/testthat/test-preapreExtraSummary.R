
with_api_fixture <- function(fixture_path, expr) {
  with(
    crunch::temp.options(
      crunch.api = "https://app.crunch.io/api/",
      httptest.mock.paths = fixture_path
    ),
    httptest::with_mock_api(expr)
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
