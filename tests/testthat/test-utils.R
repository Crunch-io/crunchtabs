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
