context("Getter methods dispatch")

test_that("print.Toplines", {
  obj <- list()
  class(obj) <- "Toplines"
  mockery::stub(print.Toplines, "getName", "A title!")
  expect_equal(
    capture.output(print.Toplines(obj)),
    c(
      "Data summary for Toplines report.", " Title: A title! ", " Unweighted.",
      " Contains data for the following variables:", " "
    )
  )
})

test_that("print.Crosstabs", {
  obj <- list()
  class(obj) <- "Crosstabs"
  mockery::stub(print.Crosstabs, "getName", "A title!")
  expect_equal(
    capture.output(print.Crosstabs(obj)),
    c(
      "Data summary for Crosstabs report.", " Title: A title! ",
      " Unweighted.", " Contains data for the following variables:",
      " "
    )
  )
})

test_that("getName.default", {
  expect_error(getName.default(list()), "The expected class for `getName` is ToplineBase", fixed = TRUE)
})

test_that("getName.ToplineBase", {
  obj <- list()
  obj$name <- "Oops, my name is"
  class(obj) <- "ToplineBase"
  expect_equal(getName(obj), "Oops, my name is")
})

test_that("getName.CrunchCube", {
  obj <- list()
  x <- c("v1" = "v1", "v2" = "v1")
  mockery::stub(getName.CrunchCube, "variables", x)
  class(obj) <- "CrunchCube"
  expect_equal(getName(obj), "v1")
})

test_that("getName.BannerVar", {
  obj <- list()
  obj$name <- "Oops, my name is"
  class(obj) <- "BannerVar"
  expect_equal(getName(obj), "Oops, my name is")
})

test_that("getName.CrossTabVar", {
  obj <- list()
  obj$name <- "Oops, my name is"
  class(obj) <- "CrossTabVar"
  expect_equal(getName(obj), "Oops, my name is")
})
