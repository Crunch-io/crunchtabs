context("codeBookSummary")

with_api_fixture("fixtures-1-2-5", {

  ds = loadDataset("Example dataset")

  r = codeBookSummary(ds$q1)

  expect_equal(
    r,
    structure(c("", "", "", "", "", "3", "1", "2", "9", "8", "Bird",
    "Cat", "Dog", "Not Asked", "Skipped", "", "", "", "", "", "3",
    "6", "4", "4", "3", "", "", "", "", ""), .Dim = 5:6)
  )

  r = codeBookSummary(ds$allpets)

  expect_equal(
    r,
    structure(list(c("allpets_1", "allpets_2", "allpets_3"),
                   c("Cat","Dog", "Bird"),
                   `1 selected` = c(4, 5, 5),
                   `2 not selected` = c(4, 3, 6),
                   `8 skipped` = c(4, 4, 6),
                   `9 not asked` = c(8, 8, 3)),
              row.names = c(NA, -3L), class = "data.frame"
    )
  )

  r = codeBookSummary(ds$ndogs)

  expect_equal(
    r,
    structure(c("", "", "", "Type", "Missing", "Range", "Numeric",
    "4", "[0, 6]", "", "", ""), .Dim = 3:4, .Dimnames = list(NULL,
    NULL))
  )

  r = codeBookSummary(ds$wave)

  expect_equal(
    r,
    structure(c("", "", "Type", "Range", "Numeric", "[2014-12-01, 2015-01-01]",
                "", ""), .Dim = c(2L, 4L), .Dimnames = list(NULL, NULL))
  )

  r = codeBookSummary(ds$q3)

  expect_equal(
    r,
    structure(c("", "", "Type", "Filled", "Text", NA, "", ""),
              .Dim = c(2L, 4L), .Dimnames = list(NULL, NULL))
  )

})
