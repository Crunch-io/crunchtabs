# sorting multiple_response ----
context("multiple_response sorting")

test_that("Sorts multiple_response alphabetically, ascending", {
  ct = readRDS("fixtures/toplines_summary.RDS")
  ct = sortAliases(ct, vars = "allpets", alpha = TRUE)

  # Counts
  expect_equal(
    attributes(ct$results$allpets$crosstabs$Results$`___total___`$counts)$row.names,
    c("Bird", "Cat", "Dog")
  )

  # Proportions
  expect_equal(
    rownames(ct$results$allpets$crosstabs$Results$`___total___`$proportions),
    c("Bird", "Cat", "Dog")
  )
})

test_that("Sorts multiple_response alphabetically, descending", {
  ct = readRDS("fixtures/toplines_summary.RDS")
  ct = sortAliases(ct, vars = "allpets", alpha = TRUE, descending = TRUE)

  # Counts
  expect_equal(
    attributes(ct$results$allpets$crosstabs$Results$`___total___`$counts)$row.names,
    rev(c("Bird", "Cat", "Dog"))
  )

  # Proportions
  expect_equal(
    rownames(ct$results$allpets$crosstabs$Results$`___total___`$proportions),
    rev(c("Bird", "Cat", "Dog"))
  )
})

test_that("Sort multiple_response numerically, descending", {
  ct = readRDS("fixtures/toplines_summary.RDS")
  ct = sortAliases(ct, vars = "allpets", descending = TRUE)

  expect_equal(
    ct$results$allpets$crosstabs$Results$`___total___`$proportions,
    structure(list(
      Total = c(
        0.472527472525689,
        0.384615384616165,
        0.252747252748925)),
      row.names = c("Dog", "Bird", "Cat"),
      class = "data.frame")
  )
})

test_that("Sort multiple_response numerically, ascending", {
  ct = readRDS("fixtures/toplines_summary.RDS")
  ct = sortAliases(ct, vars = "allpets", descending = FALSE)
  expect_equal(
    ct$results$allpets$crosstabs$Results$`___total___`$proportions,
    structure(list(
      Total = c(
        0.252747252748925,
        0.384615384616165,
        0.472527472525689)),
      row.names = c("Cat", "Bird", "Dog"),
      class = "data.frame")
  )
})

# sorting categorical ----
context("sorting categorical")

test_that("Sorts categorical alphabetically, ascending", {
  ct = readRDS("fixtures/toplines_summary.RDS")
  ct = sortAliases(ct, vars = "gender", alpha = TRUE)

  # Counts
  expect_equal(
    attributes(ct$results$gender$crosstabs$Results$`___total___`$counts)$row.names,
    c("Female","Male")
  )

  # Proportions
  expect_equal(
    rownames(ct$results$gender$crosstabs$Results$`___total___`$proportions),
    c("Female", "Male")
  )
})

test_that("Sorts categorical alphabetically, descending", {
  ct = readRDS("fixtures/toplines_summary.RDS")
  ct = sortAliases(ct, vars = "gender", alpha = TRUE, descending = FALSE)

  # Counts
  expect_equal(
    attributes(ct$results$gender$crosstabs$Results$`___total___`$counts)$row.names,
    c("Female", "Male")
  )

  # Proportions
  expect_equal(
    rownames(ct$results$gender$crosstabs$Results$`___total___`$proportions),
    c("Female", "Male")
  )
})

test_that("Sorts categorical alphabetically, descending", {
  ct = readRDS("fixtures/toplines_summary.RDS")
  ct = sortAliases(ct, vars = "gender", alpha = TRUE, descending = TRUE)

  # Counts
  expect_equal(
    attributes(ct$results$gender$crosstabs$Results$`___total___`$counts)$row.names,
    c("Male", "Female")
  )

  # Proportions
  expect_equal(
    rownames(ct$results$gender$crosstabs$Results$`___total___`$proportions),
    c("Male", "Female")
  )
})

test_that("Sort categorical numerically, descending", {
  ct = readRDS("fixtures/toplines_summary.RDS")
  ct = sortAliases(ct, vars = "gender", descending = TRUE)

  expect_equal(
    ct$results$gender$crosstabs$Results$`___total___`$proportions,
    structure(list(
      Total = c(
        0.541666666669231,
        0.458333333330769
    )),
    row.names = c("Male", "Female"),
    class = "data.frame")
  )
})

test_that("Sort categorical numerically, ascending", {
  ct = readRDS("fixtures/toplines_summary.RDS")
  ct = sortAliases(ct, vars = "gender", descending = FALSE)
  expect_equal(
    ct$results$gender$crosstabs$Results$`___total___`$proportions,
    structure(list(
      Total = c(
        0.458333333330769,
        0.541666666669231
    )),
    row.names = c("Female", "Male"),
    class = "data.frame"
    )
  )
})

