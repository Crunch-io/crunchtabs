context("writeCodeBookLatex")

with_api_fixture("fixtures-1-2-5", {
  ds = crunch::loadDataset("Example dataset")

  test_that("Tex result as expected with features", {

    suppressWarnings(
      writeCodeBookLatex(
      ds,
      title = "Hello",
      subtitle = "Goodbye",
      sample_desc = "US Voting Adults",
      logo = "yougov",
      pdf = FALSE)
    )

    tex <- readLines("Example-dataset.tex")
    original <- readRDS("fixtures/writeCodeBookLatexOne.rds")

    expect_true(length(tex) == length(original))
    expect_true(sum(tex %in% original)/length(tex) > 0.98)

  })
})

with_api_fixture("fixtures-1-2-5", {
  ds = crunch::loadDataset("Example dataset")

  test_that("Default tex as expected", {
    suppressWarnings(writeCodeBookLatex(ds, pdf = FALSE))
    tex <- readLines("Example-dataset.tex")
    original <- readRDS("fixtures/writeCodeBookLatexFull.rds")
    expect_true(length(tex) == length(original))
    expect_true(sum(tex %in% original)/length(tex) > 0.98)
  })
})

context("codeBookItemBody")

test_that("Errors appropriately when passed bad object", {
  expect_error(codeBookItemBody(c(1,2,3,4)))
})

context("kable_strip_rules")

test_that("strips rules as expected", {
  x <- "\\toprule\\bottomrule\\midrule"
  r <- kable_strip_rules(x)
  expect_equal(r, "")

  r <- kable_strip_toprules(x)
  expect_equal(r, "\\bottomrule\\midrule")
})

test_that("fixes underscore", {
  x <- "___"
  r <- fixUnderscore(x)
  expect_equal(r, "\\_\\_\\_")
})

test_that("default yg logo returns normal path", {
  p <- default_yg_logo()
  expect_equal(p, system.file("YouGov.png", package = "crunchtabs"))
})
