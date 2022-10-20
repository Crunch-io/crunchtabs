context("codeBookItemTxtDescription")

ds <- readRDS(test_path("fixtures/example_dataset.rds"))

test_that("codeBookItemTxtDescription CategoricalVariable", {
  res <- codeBookItemTxtDescription(ds$q1)

  expect_equal(
    res,
    "\\vskip 0.10in\nWhat is your favorite pet?\n\\addcontentsline{lot}{table}{\\parbox{1.8in}{\\ttfamily{q1}} Pet}\n\\vskip 0.10in" # nolint
  )
})

test_that("codeBookItemTxtDescription CategoricalArrayVariable", {
  res <- codeBookItemTxtDescription(ds$petloc)

  expect_equal(
    res,
    "\\vskip 0.10in\nName the kinds of pets you have at these locations.\n\\addcontentsline{lot}{table}{\\parbox{1.8in}{\\ttfamily{petloc}} Pets by location}\n\\vskip 0.10in" # nolint
  )
})

test_that("codeBookItemTxtDescription MultipleResponseVariable", {
  res <- codeBookItemTxtDescription(ds$allpets)

  expect_equal(
    res,
    "\\vskip 0.10in\nDo you have any of these animals as pets? Please select all that apply.\n\\addcontentsline{lot}{table}{\\parbox{1.8in}{\\ttfamily{allpets}} All pets owned}\n\\vskip 0.10in" # nolint
  )
})

test_that("codeBookItemTxtDescription NumericVariable", {
  res <- codeBookItemTxtDescription(ds$ndogs)

  expect_equal(
    res,
    "\\vskip 0.10in\n(ndogs)\n\\addcontentsline{lot}{table}{\\parbox{1.8in}{\\ttfamily{ndogs}} Number of dogs}\n\\vskip 0.10in" # nolint
  )
})

test_that("codeBookItemTxtDescription DatetimeVariable", {
  res <- codeBookItemTxtDescription(ds$wave)

  expect_equal(
    res,
    "\\vskip 0.10in\n(wave)\n\\addcontentsline{lot}{table}{\\parbox{1.8in}{\\ttfamily{wave}} Wave}\n\\vskip 0.10in" # nolint
  )
})

test_that("codebookItemTxtDescription notes", {
  res <- with_mock(
    codeBookItemTxtDescription(ds$q1), "crunch::notes" = function(x) "This is a note!")
  expect_equal(
    res,
    "\\vskip 0.10in\nWhat is your favorite pet?\n\\addcontentsline{lot}{table}{\\parbox{1.8in}{\\ttfamily{q1}} Pet}\n\\vskip 0.10in\n\\emph{This is a note!}\n\\vskip 0.10in" # nolint
  )
})
