context("codeBookSummary")

with_api_fixture("fixtures-1-2-5", {

  ds = loadDataset("Example dataset")

  test_that("codeBookSummary correct for CategoricalVariable", {
    r = codeBookSummary(ds$q1)
    expect_equal(
      r,
      structure(
        list(
          id = c("1", "2", "3", "8", "9"),
          name = c("Cat","Dog", "Bird", "Skipped", "Not Asked"),
          n = c("6", "4", "3", "3", "4")),
        class = "data.frame", row.names = c(NA, -5L)
      )
    )
  })

  test_that("codeBookSummary correct for MultipleResponseVariable", {
    r = codeBookSummary(ds$allpets)

    expect_equal(
      r,
      structure(
        list(c("allpets_1", "allpets_2", "allpets_3"), c("Cat",
        "Dog", "Bird"), `1 selected` = c(4, 5, 5),
        `2 not selected` = c(4, 3, 6), `8 skipped` = c(4, 4, 6),
        `9 not asked` = c(8, 8, 3)), row.names = c(NA, -3L),
        class = "data.frame")
    )
  })

  test_that("codeBookSummary correct for NumericVariable", {
    r = codeBookSummary(ds$ndogs)

    expect_equal(
      r,
      structure(list(
        Mean = 2, SD = 1, Min = 0, Max = 6,
        n = 16, Missing = 4), class = "data.frame",
        row.names = c(NA, -1L))
    )
  })


  test_that("codeBookSummary correct for DatetimeVariable", {
    r = codeBookSummary(ds$wave)

    expect_equal(
      r,
      structure(
        list(
          Filled = 20L, Missing = 0L,
          Range = "[2014-12-01, 2015-01-01]"),
        class = "data.frame", row.names = c(NA, -1L))
    )

  })


  test_that("codeBookSummary correct for TextVariable", {


  r = codeBookSummary(ds$q3)

  expect_equal(
    r,
    structure(list(Filled = 16L, Missing = 4L, `Max Length` = 8L),
              class = "data.frame", row.names = c(NA, -1L))
  )
  })

})
