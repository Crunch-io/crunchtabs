# sorting multiple_response ----
context("multiple_response sorting")

test_that("Sorts multiple_response alphabetically, ascending", {
  ct <- readRDS("fixtures/toplines_summary.RDS")
  ct <- sortAliases(ct, vars = "allpets", alpha = TRUE)

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
  ct <- readRDS("fixtures/toplines_summary.RDS")
  ct <- sortAliases(ct, vars = "allpets", alpha = TRUE, descending = TRUE)

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
  ct <- readRDS("fixtures/toplines_summary.RDS")
  ct <- sortAliases(ct, vars = "allpets", descending = TRUE)

  expect_equal(
    ct$results$allpets$crosstabs$Results$`___total___`$proportions,
    structure(list(
      Total = c(
        0.472527472525689,
        0.384615384616165,
        0.252747252748925
      )
    ),
    row.names = c("Dog", "Bird", "Cat"),
    class = "data.frame"
    )
  )
})

test_that("Sort multiple_response numerically, ascending", {
  ct <- readRDS("fixtures/toplines_summary.RDS")
  ct <- sortAliases(ct, vars = "allpets", descending = FALSE)
  expect_equal(
    ct$results$allpets$crosstabs$Results$`___total___`$proportions,
    structure(list(
      Total = c(
        0.252747252748925,
        0.384615384616165,
        0.472527472525689
      )
    ),
    row.names = c("Cat", "Bird", "Dog"),
    class = "data.frame"
    )
  )
})

# sorting categorical ----
context("categorical sorting")

test_that("Sorts categorical alphabetically, ascending", {
  ct <- readRDS("fixtures/toplines_summary.RDS")
  ct <- sortAliases(ct, vars = "gender", alpha = TRUE)

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
  ct <- readRDS("fixtures/toplines_summary.RDS")
  ct <- sortAliases(ct, vars = "gender", alpha = TRUE, descending = FALSE)

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
  ct <- readRDS("fixtures/toplines_summary.RDS")
  ct <- sortAliases(ct, vars = "gender", alpha = TRUE, descending = TRUE)

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
  ct <- readRDS("fixtures/toplines_summary.RDS")
  ct <- sortAliases(ct, vars = "gender", descending = TRUE)

  expect_equal(
    ct$results$gender$crosstabs$Results$`___total___`$proportions,
    structure(list(
      Total = c(
        0.541666666669231,
        0.458333333330769
      )
    ),
    row.names = c("Male", "Female"),
    class = "data.frame"
    )
  )
})

test_that("Sort categorical numerically, ascending", {
  ct <- readRDS("fixtures/toplines_summary.RDS")
  ct <- sortAliases(ct, vars = "gender", descending = FALSE)
  expect_equal(
    ct$results$gender$crosstabs$Results$`___total___`$proportions,
    structure(list(
      Total = c(
        0.458333333330769,
        0.541666666669231
      )
    ),
    row.names = c("Female", "Male"),
    class = "data.frame"
    )
  )
})

test_that("Fails if vars not in crosstabs", {
  ct <- readRDS("fixtures/toplines_summary.RDS")
  expect_error(
    sortAliases(ct, vars = "doesntexist", descending = FALSE),
    "One or more specified vars are not in your"
  )
})

test_that("Fails if fixed specified but not all responses included", {
  ct <- readRDS("fixtures/toplines_summary.RDS")
  expect_error(
    sortAliases(ct, vars = "allpets", fixed = c("Cat", "Dog")), "not TRUE"
  )
})

test_that("Defaults should sort by numeric", {
  ct <- readRDS("fixtures/toplines_summary.RDS")
  ct <- sortAliases(ct)

  expect_equal(
    ct$results$gender$crosstabs$Results$`___total___`$proportions,
    structure(list(
      Total = c(
        0.541666666669231,
        0.458333333330769
      )
    ),
    row.names = c("Male", "Female"),
    class = "data.frame"
    )
  )

  expect_equal(
    ct$results$allpets$crosstabs$Results$`___total___`$proportions,
    structure(list(
      Total = c(
        0.472527472525689,
        0.384615384616165,
        0.252747252748925
      )
    ),
    row.names = c("Dog", "Bird", "Cat"),
    class = "data.frame"
    )
  )
})

context("sorting pin_to_top")

test_that("Pin to top works as expected", {
  ct <- readRDS("fixtures/toplines_summary.RDS")
  ct <- sortAliases(ct, var = "allpets", alpha = TRUE, descending = TRUE, pin_to_top = "Bird")
  # alpha + descending = Dog, Bird, Cat
  # pin to top should be = Bird, Dog, Cat

  expect_equal(
    ct$results$allpets$crosstabs$Results$`___total___`$proportions,
    structure(list(
      Total = c(
        0.384615384616165,
        0.472527472525689,
        0.252747252748925
      )
    ),
    row.names = c("Bird", "Dog", "Cat"),
    class = "data.frame"
    )
  )
})


context("sort pin_to_bottom")

test_that("Pin to bottom works as expected", {
  ct <- readRDS("fixtures/toplines_summary.RDS")
  ct <- sortAliases(
    ct,
    var = "allpets",
    alpha = TRUE,
    descending = FALSE,
    pin_to_bottom = "Bird"
  )

  # alpha + !descending = Bird, Cat, Dog
  # pin to bottom should be = Cat, Dog, Bird

  expect_equal(
    ct$results$allpets$crosstabs$Results$`___total___`$proportions,
    structure(list(
      Total = c(
        0.252747252748925,
        0.472527472525689,
        0.384615384616165
      )
    ),
    row.names = c("Cat", "Dog", "Bird"),
    class = "data.frame"
    )
  )
})

test_that("Ambiguous error if pin to top/bottom both specified", {
  ct <- readRDS("fixtures/toplines_summary.RDS")
  expect_error(sortAliases(
    ct,
    var = "allpets",
    alpha = TRUE,
    descending = FALSE,
    pin_to_bottom = "Bird",
    pin_to_top = "Dog"
  ), "Ambiguous")
})

context("Skips datetime and numeric vars")
test_that("DatetimeVariable does nothing", {
  ct <- readRDS("fixtures/toplines_summary.RDS")
  before <- ct
  before$results$allpets$type <- "DatetimeVariable"
  after <- sortAliases(before, var = "allpets")

  expect_equal(before$results$allpets, after$results$allpets)
})

test_that("NumericVariable does nothing", {
  ct <- readRDS("fixtures/toplines_summary.RDS")
  before <- ct
  before$results$allpets$type <- "NumericVariable"
  after <- sortAliases(before, var = "allpets")

  expect_equal(before$results$allpets, after$results$allpets)
})


context("crosstabs also apply sorting as expected")

test_that("Alpha sorts with banner", {
  ct <- readRDS("fixtures/ct_allpets.rds")
  ct <- sortAliases(ct, vars = "allpets", alpha = TRUE)

  expect_equal(
    rownames(ct$results$allpets$crosstabs$`banner 1`$`___total___`$counts),
    c("Bird", "Cat", "Dog")
  )

  expect_equal(
    as.vector(ct$results$allpets$crosstabs$`banner 1`$`___total___`$counts),
    c(5, 4, 5)
  )
})

test_that("Alpha sorts with banner, descending", {
  ct <- readRDS("fixtures/ct_allpets.rds")
  ct <- sortAliases(ct, vars = "allpets", alpha = TRUE, descending = TRUE)

  expect_equal(
    rownames(ct$results$allpets$crosstabs$`banner 1`$`___total___`$counts),
    rev(c("Bird", "Cat", "Dog"))
  )

  expect_equal(
    as.vector(ct$results$allpets$crosstabs$`banner 1`$`___total___`$counts),
    c(5, 4, 5)
  )
})


test_that("Numeric sorts with banner", {
  ct <- readRDS("fixtures/ct_allpets.rds")
  ct <- sortAliases(ct, vars = "allpets")

  expect_equal(
    rownames(ct$results$allpets$crosstabs$`banner 1`$`___total___`$counts),
    c("Dog", "Cat", "Bird")
  )

  expect_equal(
    ct$results$allpets$crosstabs$`banner 1`$`___total___`$proportions$Total,
    c(0.625, 0.5, 0.454545454545455)
  )
})


test_that("Numeric sorts with banner", {
  ct <- readRDS("fixtures/ct_allpets.rds")
  ct <- sortAliases(ct, vars = "allpets", descending = FALSE)

  expect_equal(
    rownames(ct$results$allpets$crosstabs$`banner 1`$`___total___`$counts),
    rev(c("Dog", "Cat", "Bird"))
  )

  expect_equal(
    ct$results$allpets$crosstabs$`banner 1`$`___total___`$proportions$Total,
    rev(c(0.625, 0.5, 0.454545454545455))
  )
})
