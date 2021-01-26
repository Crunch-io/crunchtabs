context("forNowTransforms")

# test_that("Returns the value of a category", {
#   set.seed(42)
#   vec = sample(1:5, 100, rep = T)
#   elements = crunch::Category(name = "First", id = 1,
#                               numeric_value = 1, missing = FALSE)
#   var_cats = factor(sample(letters[1:5], 100, rep = TRUE))
#
#   expect_equal(calcTabInsertions(vec, elements, var_cats))
# })

test_that("Returns weighted mean", {
  set.seed(42)
  vec <- sample(1:5, 5, rep = T)
  var_cats <- Categories(
    Category(name = "A", id = 1, numeric_value = 1, missing = FALSE),
    Category(name = "B", id = 2, numeric_value = 2, missing = FALSE),
    Category(name = "C", id = 3, numeric_value = 3, missing = FALSE),
    Category(name = "D", id = 4, numeric_value = 4, missing = FALSE),
    Category(name = "E", id = 5, numeric_value = 5, missing = FALSE)
  )
  expect_equal(
    crunchtabs:::calcTabMeanInsert(vec, var_cats),
    stats::weighted.mean(values(var_cats), vec)
  )
})

test_that("Returns weighted mean with NA in var_cats", {
  set.seed(42)
  vec <- sample(1:5, 5, rep = T)
  var_cats <- Categories(
    Category(name = "A", id = 1, numeric_value = 1, missing = FALSE),
    Category(name = "B", id = 2, numeric_value = 2, missing = FALSE),
    Category(name = "C", id = 3, numeric_value = 3, missing = FALSE),
    Category(name = "D", id = 4, numeric_value = 4, missing = FALSE),
    Category(name = "E", id = 5, numeric_value = 5, missing = FALSE)
  )
  ok <- !is.na(values(var_cats)) & !is.na(vec)
  expect_equal(
    crunchtabs:::calcTabMeanInsert(vec, var_cats),
    stats::weighted.mean(
      values(var_cats)[ok],
      vec[ok]
    )
  )
})

test_that("Returns weighted mean with NA in vec", {
  set.seed(42)
  vec <- sample(c(1:4, NA), 5, rep = T)
  var_cats <- Categories(
    Category(name = "A", id = 1, numeric_value = 1, missing = FALSE),
    Category(name = "B", id = 2, numeric_value = 2, missing = FALSE),
    Category(name = "C", id = 3, numeric_value = 3, missing = FALSE),
    Category(name = "D", id = 4, numeric_value = 4, missing = FALSE),
    Category(name = "E", id = 5, numeric_value = 5, missing = FALSE)
  )
  ok <- !is.na(values(var_cats)) & !is.na(vec)
  expect_equal(
    crunchtabs:::calcTabMeanInsert(vec, var_cats),
    stats::weighted.mean(values(var_cats[ok]),
      vec[ok],
      na.rm = TRUE
    )
  )
})


test_that("Returns weighted median", {
  set.seed(42)
  vec <- sample(1:5, 5, rep = T)
  var_cats <- Categories(
    Category(name = "A", id = 1, numeric_value = 1, missing = FALSE),
    Category(name = "B", id = 2, numeric_value = 2, missing = FALSE),
    Category(name = "C", id = 3, numeric_value = 3, missing = FALSE),
    Category(name = "D", id = 4, numeric_value = 4, missing = FALSE),
    Category(name = "E", id = 5, numeric_value = 5, missing = FALSE)
  )

  perc <- cumsum(vec) / sum(vec)
  num_val <- values(var_cats)

  if (any(as.character(perc) %in% as.character(0.5))) {
    n <- which(perc == 0.5)
    median <- (num_val[n] + num_val[n + 1]) / 2
  }

  over0.5 <- which(perc > 0.5)
  med <- num_val[min(over0.5)]

  expect_equal(crunchtabs:::calcTabMedianInsert(vec, var_cats), med)
})
