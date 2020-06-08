context("codeBookSummary")

with_api_fixture("fixtures-1-2-5", {

  ds = loadDataset("Example dataset")

  test_that("codeBookSummary correct for CategoricalVariable", {
    r = codeBookSummary(ds$q1)
    expect_equal(
      r,
      structure(c("3", "1", "2", "9", "8", "Bird", "Cat", "Dog", "Not Asked",
                  "Skipped", "3", "6", "4", "4", "3"), .Dim = c(5L, 3L))
    )
  })

  test_that("codeBookSummary correct for MultipleResponseVariable", {
    r = codeBookSummary(ds$allpets)

    expect_equal(
      r,
      structure(list(c("allpets_1", "allpets_2", "allpets_3"),
                     c("Cat", "Dog", "Bird"),
                     `1 selected` = c(8, 8, 3),
                     `2 not selected` = c(4, 3, 6),
                     `8 skipped` = c(4, 5, 5),
                     `9 not asked` = c(4, 4, 6)), row.names = c(NA, -3L),
                class = "data.frame")
    )
  })

  test_that("codeBookSummary correct for NumericVariable", {
    r = codeBookSummary(ds$ndogs)

    expect_equal(
      r,
      structure(c("Type", "Missing", "Range", "Numeric", "4", "[0, 6]"
      ), .Dim = 3:2, .Dimnames = list(NULL, NULL))
    )
  })


  test_that("codeBookSummary correct for DatetimeVariable", {
    r = codeBookSummary(ds$wave)

    expect_equal(
      r,
      structure(c("Type", "Range", "Datetime", "[2014-12-01, 2015-01-01]"
      ), .Dim = c(2L, 2L), .Dimnames = list(NULL, NULL))
    )

  })


  test_that("codeBookSummary correct for TextVariable", {


  r = codeBookSummary(ds$q3)

  expect_equal(
    r,
    structure(c("Type", "Filled", "Text", "16"),
              .Dim = c(2L, 2L),
              .Dimnames = list(NULL, NULL))
  )
  })

})
