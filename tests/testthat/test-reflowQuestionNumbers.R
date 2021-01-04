context("reflowQuestionNumbers")
test_that("reflowQuestion Numbers", {
  set.seed(42)
  s <- sample(1:100)
  x <- lapply(1:50, function(x) {
    y <- list()
    y$number <- s[x]
    y
  })
  
  expect_false(
    all(unlist(lapply(x, function(x) x[["number"]])) == 1:50)
  )
  expect_true(
    all(unlist(lapply(reflowQuestionNumbers(x), function(x) x[["number"]])) == 1:50)
  )
})
