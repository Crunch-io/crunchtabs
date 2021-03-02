context("codeBookItemTxtDescriptionGeneral")

test_that("Character with notes", {
  set.seed(42)
  x <- sample(letters[1:5], 100, replace = T)
  meta <- data.frame(
    alias = "hello",
    name = "This is Hello",
    notes = "This is a note",
    description = "This is a description",
    stringsAsFactors = F
  )

  res <- codeBookItemTxtDescriptionGeneral(x, "hello", meta)
  expect_true(grepl("This is a description", res))
  expect_true(grepl("This is Hello", res))
  expect_true(grepl("This is a note", res))
  expect_true(grepl("ttfamily{hello}", res, fixed = TRUE))
})

test_that("Non character with notes", {
  set.seed(42)
  x <- factor(sample(letters[1:5], 100, replace = T))
  meta <- data.frame(
    alias = "hello",
    name = "This is Hello",
    notes = "This is a note",
    description = "This is a description",
    stringsAsFactors = F
  )

  res <- codeBookItemTxtDescriptionGeneral(x, "hello", meta)
  expect_true(grepl("This is a description", res))
  expect_true(grepl("This is Hello", res))
  expect_true(grepl("This is a note", res))
  expect_true(grepl("ttfamily{hello}", res, fixed = TRUE))
})

test_that("Non character no notes", {
  set.seed(42)
  x <- rnorm(100)
  meta <- data.frame(
    alias = "hello",
    name = "This is Hello",
    notes = "",
    description = "This is a description",
    stringsAsFactors = F
  )

  res <- codeBookItemTxtDescriptionGeneral(x, "hello", meta)
  expect_true(grepl("This is a description", res))
  expect_true(grepl("This is Hello", res))
  expect_true(!grepl("This is a note", res))
  expect_true(grepl("ttfamily{hello}", res, fixed = TRUE))
})
