context("collapse_items")

test_that("Collapses results appropriately", {
  res = collapse_items(c(1,2,3))
  expect_equal(res, "1, 2, 3")

  res = collapse_items(c(1,2,3), quotes = TRUE)
  expect_equal(res, "'1', '2', '3'")
})

context("operator overload")

test_that("Returns as expected", {
  a = NULL
  b = 1

  expect_equal(a %||% b, 1)

  a = 10
  b = 1

  expect_equal(a %||% b, 10)
})

context("error_if_items")

test_that("error_if_that", {
  expect_error(
    error_if_items(c(1,2), text = "Vars in {items}", and = T),
    "Vars in 1 and 2"
  )
})

test_that("warning if not error", {
  expect_equal(
    error_if_items(c("item1", "item2"), error = FALSE, text = "Text"),
    "Text"
  )
})

context("collapse_items")

test_that("collapse items works", {
  res <- collapse_items(as.character(1:3))
  expect_equal(res, "1, 2, 3")
})

test_that("Paths appropriately on different OS", {
  # Too much effort, ignoring tests.
})

context("custom theme tests")

test_that("huffpost theme as expected", {
  r <- themeHuffPoCrosstabs(logo = list(file = default_yg_logo()))
  expect_is(r, "Theme")
})

test_that("huffpost theme as expected", {
  r <- themeHuffPoToplines(logo = list(file = default_yg_logo()))
  expect_is(r, "Theme")
})

context("file.open")

test_that("file open works as expected", {
  mockery::stub(file.open, "system", print(TRUE))
  mockery::stub(file.open, "Sys.info", c("sysname" = "Linux"))
  expect_true(all(file.open(1)))
  mockery::stub(file.open, "Sys.info", c("sysname" = "Windows"))
  expect_true(all(file.open(1)))
  mockery::stub(file.open, "Sys.info", c("sysname" = "MacOS"))
  expect_true(all(file.open(1)))
})


