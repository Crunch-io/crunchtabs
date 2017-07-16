context("Getter methods dispatch")

test_that("Getter methods dispatch error handling", {
    expect_error(getResults(NULL),
                 "getResults doesn't support objects of class NULL")
    expect_error(getResults(NA),
                 "getResults doesn't support objects of class logical")
    expect_error(getResults(structure(list(), class = "FakeClass")),
               "getResults doesn't support objects of class FakeClass")
    expect_error(getResults(1),
                 "getResults doesn't support objects of class numeric")
    expect_error(getResults(NA),
                 "getResults doesn't support objects of class logical")
    expect_error(getNames(NULL),
                 "getNames doesn't support objects of class NULL")
    expect_error(getAlias(NULL),
                 "getAlias doesn't support objects of class NULL")
    expect_error(getName(NULL),
                 "getName doesn't support objects of class NULL")
    expect_error(getDescription(NULL),
                 "getDescription doesn't support objects of class NULL")
    expect_error(getNotes(NULL),
                 "getNotes doesn't support objects of class NULL")
    expect_error(getTotal(NULL),
                 "getTotal doesn't support objects of class NULL")
    expect_error(getMissing(NULL),
                 "getMissing doesn't support objects of class NULL")
})

test_that("Getter methods return correct data", {
  x <- structure(list(proportions = c(0.2, 0.8), counts = c(1, 4)), class = "ToplineCategoricalGeneral")
  expect_identical(getResults(x), c(1, 4))
  expect_identical(getResults(x, proportions = TRUE), c(0.2, 0.8))
  x <- structure(list(summary = 1), class = "ToplineNumeric")
  expect_identical(getResults(x), 1)
  expect_identical(getResults(x, proportions = TRUE), 1)
})
