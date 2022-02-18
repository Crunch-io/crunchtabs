context("catArrayToCategoricals")

test_that("Binds categorical arrays appropriately", {
  questions <- readRDS(test_path("fixtures/catArrayToCategorical_questions.rds"))
  res <- expect_warning(
    catArrayToCategoricals(questions, "petloc", labels = NULL),
    "New variables derived from a"
  )

  expect_equal(
    res$petloc_1$crosstabs$Results$`___total___`$proportions,
    structure(c(
      0.421875000002028, 0.484374999997521, 0.0937500000004507,
      0.5, 0.333333333333333, 0.166666666666667, 0.321428571427136,
      0.464285714288106, 0.214285714284758
    ), .Dim = c(3L, 3L), .Dimnames = list(
      c("Cat", "Dog", "Bird"), c("Wave 1", "Wave 2", "Wave 3")
    ))
  )
})
