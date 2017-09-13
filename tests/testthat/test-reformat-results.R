context("Rounding percenteges")

test_that("Rounded percentes add up to 100 for categorical variables", {
  data <- c(a1=0.0049, a2=0.3902, a3=0.6049)
  expect_identical(roundPropCategorical(data, 0), c(a1=0, a2=39, a3=61))
  data <- c(a1=0.0090, a2=0.3855, a3=0.6055)
  expect_identical(roundPropCategorical(data, 0), c(a1=1, a2=38, a3=61))
  data <- c(a1=0.091, a2=0.387, a3=0.522)
  expect_identical(roundPropCategorical(data, 0), c(a1=9, a2=39, a3=52))
  K <- 10
  M <- 100
  data <- matrix(abs(rnorm(K * M)), M, K)
  data <- data / rowSums(data)
  rounded.100.data <- t(apply(data, 1, roundPropCategorical, digits = 0))
  expect_identical(table(rowSums(rounded.100.data)), table(rep(100, 100)))
})

test_that("Rounded percentes add up to 100 for every column for categorical_array variables", {
  data <- matrix(c(0.0049, 0.3902, 0.6049, 0.0090, 0.3855, 0.6055, 0.091, 0.387, 0.522),
                 nrow = 3, byrow = TRUE,
                 dimnames = list(rows=c("r1", "r2", "r3"), cols=c("c1", "c2", "c3")))
  result <- roundPropCategoricalArray(data, 0)
  expected <- matrix(c(0, 39, 61, 1, 38, 61, 9, 39, 52), nrow = 3, byrow = TRUE,
                     dimnames = list(rows=c("r1", "r2", "r3"), cols=c("c1", "c2", "c3")))
  expect_identical(result, expected)
  expect_identical(rowSums(expected), c(r1=100, r2=100, r3=100))
})
