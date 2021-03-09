context("as.ToplineCategoricalArray")
test_that("Stop condition", {
  expect_error(
    as.ToplineCategoricalArray(
      questions = list("a", "b", "c"),
      labels = c(letters[1:4])
    ),
    "Number of labels provided does not"
  )
})
