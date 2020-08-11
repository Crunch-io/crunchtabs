context("prepareExtraSummary NumericVariable")

# YANBF: Yet another broken fixture.
with_api_fixture("fixtures-1-2-5", {

  ds <- loadDataset("Example dataset")

  test_that("dataset is not weighted", {
    expect_null(weight(ds))
  })

  test_that("dataset is accessible", {
    expect_true(exists("ds"))
  })

  ct <- crosstabs(ds, include_numeric = TRUE,
                  include_datetime = TRUE,
                  include_verbatims = TRUE)

  expect_equal(
    names(ct$results),
    c("allpets", "q1", "petloc", "ndogs", "ndogs_a", "ndogs_b", "q3",
      "country", "wave")
    )

  expect_equal(
    rownames(ct$results$ndogs$data_list$body),
    c("Minimum", "1st Quartile", "Median", "Mean", "3rd Quartile",
      "Maximum", "Standard Deviation")
  )

  expect_equal(
    ct$results$ndogs$data_list$body[1:7,],
    c(0, 1, 2, 2, 2.25, 6, 1.4142135623731)
  )
})

context("prepareExtraSummary DatetimeVariable")

# NBF: Yet another broken fixture.
with_api_fixture("fixtures-1-2-5", {

  ds <- loadDataset("Example dataset")

  test_that("dataset is not weighted", {
    expect_null(weight(ds))
  })

  test_that("dataset is accessible", {
    expect_true(exists("ds"))
  })


  ct <- crosstabs(ds, include_numeric = TRUE,
                  include_datetime = TRUE,
                  include_verbatims = TRUE)

  expect_equal(
    names(ct$results),
    c("allpets", "q1", "petloc", "ndogs", "ndogs_a", "ndogs_b", "q3",
      "country", "wave")
  )

  expect_equal(
    rownames(ct$results$wave$data_list$body),
    c("Minimum", "1st Quartile", "Median", "3rd Quartile", "Maximum")
  )

  expect_equal(
    ct$results$wave$data_list$body[,],
    structure(c(1417392000, 1417392000, 1418731200, 1420070400, 1420070400
    ), class = c("POSIXct", "POSIXt"))
  )
})

context("prepareExtraSummary TextVariable")

context("Fails as expected")

test_that("Garbage in, garbage out", {
  expect_error(prepareExtraSummary(letters), "The expected class")
})

# TODO: Add test for weighted data
