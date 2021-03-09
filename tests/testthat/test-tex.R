context("underline")

test_that("underline works as expected", {
  r <- underline("Hello!")
  expect_equal(r, "\\underline{Hello!}")
})

context("applyLatexStyle")

test_that("underline or underline2", {
  item <- "Hello!"
  item_theme <- list()
  item_theme$decoration <- "underline"
  r <- applyLatexStyle(item, item_theme)
  expect_equal(r, "\\underline{Hello!}")
})

test_that("Warning on hex color", {
  item <- "Hello!"
  item_theme <- list()
  item_theme$font_color <- "#000000"
  expect_warning(applyLatexStyle(item, item_theme),
                 "In Latex, colors must be color names not hex codes")
})

test_that("Color application", {
  item <- "Hello!"
  item_theme <- list()
  item_theme$font_color <- "blue"
  r <- applyLatexStyle(item, item_theme)
  expect_equal(r, "\\color{blue}Hello!")
})

context("validLatexFont")

test_that("Warning on missing font set to helvet", {
  expect_warning(validLatexFont("notafont"), "It has been set to")
})
