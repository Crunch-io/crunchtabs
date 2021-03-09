context("relabel")

# tests/testthat/
ct <- readRDS(testthat::test_path("fixtures/toplines_summary.RDS"))

test_that("Relabels ToplineVar", {
  expect_message(ct <- relabel(
    ct,
    list(
      alias = "allpets",
      options = c("Amazing Cat", "Smelly Dog", "Annoying Bird"),
      notes = c("This is a new note"),
      description = c("This is a new description")
    )
    ), "Relabeling should always occur before sorting"
  )

  expect_equal(
    ct$results$allpets$description, "This is a new description"
  )
  expect_equal(
    ct$results$allpets$notes, "This is a new note"
  )
  expect_equal(
    dimnames(ct$results$allpets$crosstabs$Results$`___total___`$counts)[[1]],
    c("Amazing Cat", "Smelly Dog", "Annoying Bird")
  )
  expect_equal(
    dimnames(ct$results$allpets$crosstabs$Results$`___total___`$proportions)[[1]],
    c("Amazing Cat", "Smelly Dog", "Annoying Bird")
  )
  expect_equal(
    dimnames(ct$results$allpets$crosstabs$Results$`___total___`$base)[[1]],
    c("Amazing Cat", "Smelly Dog", "Annoying Bird")
  )

  expect_named(ct, c("metadata", "results", "banner"))
  expect_is(ct$results, c("ToplineResults", "CrosstabsResults", "list"))
})

test_that("Relabels ToplineCategoricalArray", {
  ct <- relabel(
    ct,
    list(
      alias = "petloc",
      options = c("Amazing Cat", "Smelly Dog", "Annoying Bird"),
      items = c("At Home", "At Work"),
      notes = c("This is a new note"),
      description = c("This is a new description")
    )
  )

  expect_equal(
    ct$results$petloc$description, "This is a new description"
  )
  expect_equal(
    ct$results$petloc$notes, "This is a new note"
  )
  expect_equal(
    dimnames(ct$results$petloc$crosstabs$Results$`___total___`$counts)[[1]],
    c("Amazing Cat", "Smelly Dog", "Annoying Bird")
  )
  expect_equal(
    dimnames(ct$results$petloc$crosstabs$Results$`___total___`$proportions)[[1]],
    c("Amazing Cat", "Smelly Dog", "Annoying Bird")
  )
  expect_equal(
    dimnames(ct$results$petloc$crosstabs$Results$`___total___`$base)[[1]],
    c("Amazing Cat", "Smelly Dog", "Annoying Bird")
  )
  expect_equal(
    dimnames(ct$results$petloc$crosstabs$Results$`___total___`$counts)[[2]],
    c("At Home", "At Work")
  )
  expect_equal(
    dimnames(ct$results$petloc$crosstabs$Results$`___total___`$proportions)[[2]],
    c("At Home", "At Work")
  )
  expect_equal(
    dimnames(ct$results$petloc$crosstabs$Results$`___total___`$base)[[2]],
    c("At Home", "At Work")
  )

  expect_named(ct, c("metadata", "results", "banner"))
  expect_is(ct$results, c("ToplineResults", "CrosstabsResults", "list"))
})

test_that("Fails if unequal length of category items", {

  expect_error(ct <- relabel(
    ct,
    list(
      alias = "petloc",
      options = c("Amazing Cat", "Smelly Dog", "Annoying Bird"),
      items = c("At Home"), # Should fail!
      notes = c("This is a new note"),
      description = c("This is a new description")
    )
    ), "Length of items vector not equal for petloc"
  )
})

test_that("Fails if unequal length of category options", {

  expect_error(ct <- relabel(
    ct,
    list(
      alias = "petloc",
      options = c("Amazing Cat", "Smelly Dog"), # Should fail!
      items = c("At Home", "At work"),
      notes = c("This is a new note"),
      description = c("This is a new description")
    )
  ), "Length of options vector not equal for petloc"
  )
})

test_that("Shows wave warning", {
  ct <- relabel(
    ct,
    list(
      alias = "petloc",
      options = c("Amazing Cat", "Smelly Dog", "Annoying Bird"),
      items = c("Wave 1", "Wave 2"),
      notes = c("This is a new note"),
      description = c("This is a new description")
    )
  )

  expect_warning(
    ct <- relabel(
      ct,
      list(
        alias = "petloc",
        options = c("Amazing Cat", "Smelly Dog", "Annoying Bird"),
        items = c("At Home", "At work"),
        notes = c("This is a new note"),
        description = c("This is a new description")
      )
    ),
    "We recommend against relabeling wave names"
  )
})

test_that("ToplineCategoricalArray notes only", {
  ct <- relabel(
    ct,
    list(
      alias = "petloc",
      notes = c("This is a new note")
    )
  )

  expect_equal(ct$results$petloc$notes, "This is a new note")
})

test_that("ToplineCategoricalArray description only", {
  ct <- relabel(
    ct,
    list(
      alias = "petloc",
      description = c("This is a new description")
    )
  )

  expect_equal(ct$results$petloc$description, "This is a new description")
})

test_that("ToplineCategoricalArray options only", {
  ct <- relabel(
    ct,
    list(
      alias = "petloc",
      options = c("Amazing Cat", "Smelly Dog", "Annoying Bird"),
      notes = c("This is a new note"),
      description = c("This is a new description")
    )
  )

  expect_equal(
    dimnames(ct$results$petloc$crosstabs$Results$`___total___`$counts)[[1]],
    c("Amazing Cat", "Smelly Dog", "Annoying Bird")
  )
  expect_equal(
    dimnames(ct$results$petloc$crosstabs$Results$`___total___`$proportions)[[1]],
    c("Amazing Cat", "Smelly Dog", "Annoying Bird")
  )
  expect_equal(
    dimnames(ct$results$petloc$crosstabs$Results$`___total___`$base)[[1]],
    c("Amazing Cat", "Smelly Dog", "Annoying Bird")
  )

})
test_that("ToplineCategoricalArray items only", {
  ct <- relabel(
    ct,
    list(
      alias = "petloc",
      items = c("At Home", "At Work"),
      notes = c("This is a new note"),
      description = c("This is a new description")
    )
  )

  expect_equal(
    dimnames(ct$results$petloc$crosstabs$Results$`___total___`$counts)[[2]],
    c("At Home", "At Work")
  )
  expect_equal(
    dimnames(ct$results$petloc$crosstabs$Results$`___total___`$proportions)[[2]],
    c("At Home", "At Work")
  )
  expect_equal(
    dimnames(ct$results$petloc$crosstabs$Results$`___total___`$base)[[2]],
    c("At Home", "At Work")
  )
})
