context("writeCodeBookLatexGeneric")

test_that("End to end! Defaults + ygblue logo", {
  set.seed(42)

  ds <- data.frame(
    a_factor = factor(sample(letters[1:5], 1000, replace = TRUE)),
    a_character = sample(letters, 1000, replace = TRUE),
    a_numeric = rnorm(1000),
    an_integer = round(runif(1000, 0, 1000)),
    a_factor_missings = sample(c(letters[1:5], NA), 1000, replace = TRUE),
    a_numeric_missings = sample(c(NA, rnorm(20)), 1000, replace = TRUE),
    an_integer_missings = sample(c(NA, round(runif(20, 0, 100))), 1000, replace = TRUE),
    a_character_missings = sample(c(NA, letters), 1000, replace = TRUE),
    stringsAsFactors = FALSE
  )

  meta <- data.frame(
    alias = names(ds),
    name = paste0("Lorem ipsum dolor sit amet ", names(ds)),
    description = substr(stringi::stri_rand_lipsum(8), 0, 160),
    notes = sample(c("", "Asked only of a specific group"), 8, replace = TRUE),
    recode = c(
      'list(a = "a", b = "b", c = "c", d = "d", e = "e")',
      NA,
      NA,
      NA,
      'list(a = "a", b = "b", c = "c", d = "d", e = "e")',
      NA,
      NA,
      NA
    )
  )

  writeCodeBookLatexGeneric(ds, meta, pdf = FALSE, logo = "ygblue", position = "c")
  res <- readLines("General-Dataset.tex")
  expect_true(any(grepl("\\begin{longtable}[c]", res, fixed = TRUE)))
  expect_equal(res[1], "\\documentclass{article}")
  expect_true(any(grepl("YouGovBlue_small", res)))
  expect_equal(res[length(res)], "\\end{document}")
  expect_true(any(grepl("Lorem ipsum dolor sit amet a\\_numeric}", res, fixed = TRUE)))
  expect_true(any(grepl("Lorem ipsum dolor sit amet a\\_factor}", res, fixed = TRUE)))
  expect_true(any(grepl("Lorem ipsum dolor sit amet an\\_integer}", res, fixed = TRUE)))
  expect_true(any(grepl("Lorem ipsum dolor sit amet a\\_factor\\_missings}", res, fixed = TRUE)))
  expect_true(any(grepl("Lorem ipsum dolor sit amet a\\_numeric\\_missings}", res, fixed = TRUE)))
  expect_true(any(grepl("Lorem ipsum dolor sit amet an\\_integer\\_missings}", res, fixed = TRUE)))
  expect_true(any(grepl("Lorem ipsum dolor sit amet a\\_character\\_missings}", res, fixed = TRUE)))
  expect_true(file.remove("General-Dataset.tex"))
})

test_that("End to end for subtitle and n > 21 category with appendix, factor, yougov logo", {
  ds <- data.frame(
    a_factor = factor(sample(letters, 1000, replace = TRUE)),
    a_character = sample(letters, 1000, replace = TRUE),
    a_numeric = rnorm(1000),
    an_integer = round(runif(1000, 0, 1000)),
    a_factor_missings = sample(c(letters[1:5], NA), 1000, replace = TRUE),
    a_numeric_missings = sample(c(NA, rnorm(20)), 1000, replace = TRUE),
    an_integer_missings = sample(c(NA, round(runif(20, 0, 100))), 1000, replace = TRUE),
    a_character_missings = sample(c(NA, letters), 1000, replace = TRUE),
    stringsAsFactors = FALSE
  )

  meta <- data.frame(
    alias = names(ds),
    name = paste0("Lorem ipsum dolor sit amet ", names(ds)),
    description = substr(stringi::stri_rand_lipsum(8), 0, 160),
    notes = sample(c("", "Asked only of a specific group"), 8, replace = TRUE),
    recode = c(
      'list(a = "a", b = "b", c = "c", d = "d", e = "e", f = "f", g = "g", h = "h", i = "i", j = "j", k = "k", l = "l", m = "m", n = "n", o = "o", p = "p", q = "q", r = "r", s = "s", t = "t", u = "u", v = "v", w = "w", x = "x", y = "y", z = "z")',
      NA,
      NA,
      NA,
      'list(a = "a", b = "b", c = "c", d = "d", e = "e")',
      NA,
      NA,
      NA
    )
  )

  writeCodeBookLatexGeneric(ds, meta, pdf = FALSE, logo = "yougov",
                            subtitle = "This is an amazing subtitle", appendix = TRUE, path = ".")
  res <- readLines("General-Dataset.tex")
  expect_equal(res[1], "\\documentclass{article}")
  expect_true(any(grepl("This is an amazing subtitle", res)))
  expect_true(any(grepl("Appendix", res)))
  expect_true(any(grepl("YouGov", res)))
  expect_equal(res[length(res)], "\\end{document}")
})

test_that("End to end write to pdf", {
  set.seed(42)

  ds <- data.frame(
    a_factor = factor(sample(letters[1:5], 1000, replace = TRUE)),
    a_character = sample(letters, 1000, replace = TRUE),
    a_numeric = rnorm(1000),
    an_integer = round(runif(1000, 0, 1000)),
    a_factor_missings = sample(c(letters[1:5], NA), 1000, replace = TRUE),
    a_numeric_missings = sample(c(NA, rnorm(20)), 1000, replace = TRUE),
    an_integer_missings = sample(c(NA, round(runif(20, 0, 100))), 1000, replace = TRUE),
    a_character_missings = sample(c(NA, letters), 1000, replace = TRUE),
    stringsAsFactors = FALSE
  )

  meta <- data.frame(
    alias = names(ds),
    name = paste0("Lorem ipsum dolor sit amet ", names(ds)),
    description = substr(stringi::stri_rand_lipsum(8), 0, 160),
    notes = sample(c("", "Asked only of a specific group"), 8, replace = TRUE),
    recode = c(
      'list(a = "a", b = "b", c = "c", d = "d", e = "e")',
      NA,
      NA,
      NA,
      'list(a = "a", b = "b", c = "c", d = "d", e = "e")',
      NA,
      NA,
      NA
    )
  )
  mockery::stub(writeCodeBookLatexGeneric, "file.open", TRUE)
  expect_true(suppressWarnings(
    writeCodeBookLatexGeneric(ds, meta, pdf = TRUE, logo = "ygblue", position = "c")))
  expect_true(file.remove(c("General-Dataset.tex")))
  expect_true(file.remove(c("General-Dataset.pdf")))
})
