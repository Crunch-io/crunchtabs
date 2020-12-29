test_that("rounds appropriately", {
  set.seed(42)
  res <- roundPropCategorical(runif(100) * 100)
  expect_equal(mean(res), 1)
  expect_equal(sd(res), 30.197, tolerance = 0.001)
})
