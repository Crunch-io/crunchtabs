context("codeBookItemTxtHeaderGeneral")

test_that("Fails on bad type", {
  set.seed(42)
  x <- sample(letters[1:5], 100, replace = T)
  class(x) <- "BadClass"
  meta <- data.frame(
    alias = "hello",
    name = "This is Hello",
    notes = "This is a note",
    description = "This is a description",
    stringsAsFactors = F
  )
  res <- codeBookItemTxtHeaderGeneral(x, "hello", meta)
  expect_equal(res, character(0))
})

test_that("End to end", {
  set.seed(42)
  x <- sample(letters[1:5], 100, replace = T)
  meta <- data.frame(
    alias = "hello",
    name = "This is Hello",
    notes = "This is a note",
    description = "This is a description",
    stringsAsFactors = F
  )
  res <- codeBookItemTxtHeaderGeneral(x, "hello", meta)
  expect_equal(
    res,
    "\\textbf{This is Hello}\\hfill\\textbf{\\ttfamily{hello}}\n\n{\\small character}\n\n"
  )
})
