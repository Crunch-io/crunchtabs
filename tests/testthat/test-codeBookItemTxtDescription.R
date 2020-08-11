context("codeBookItemTxtDescription")

with_api_fixture("fixtures-1-2-5", {
  ds = crunch::loadDataset("Example dataset")

  test_that("codeBookItemTxtDescription CategoricalVariable", {
    res = codeBookItemTxtDescription(ds$q1)

    expect_equal(
      res,
      "\\vskip 0.10in\nWhat is your favorite pet?\n\\addcontentsline{lot}{table}{\\parbox{1.8in}{\\ttfamily{q1}} Pet}\n\\vskip 0.10in"
    )
  })

  test_that("codeBookItemTxtDescription CategoricalArrayVariable", {
    res = codeBookItemTxtDescription(ds$petloc)

    expect_equal(
     res,
     "\\vskip 0.10in\nName the kinds of pets you have at these locations.\n\\addcontentsline{lot}{table}{\\parbox{1.8in}{\\ttfamily{petloc}} Pets by location}\n\\vskip 0.10in"
    )

  })

  test_that("codeBookItemTxtDescription MultipleResponseVariable", {
    res = codeBookItemTxtDescription(ds$allpets)

    expect_equal(
      res,
      "\\vskip 0.10in\nDo you have any of these animals as pets? Please select all that apply.\n\\addcontentsline{lot}{table}{\\parbox{1.8in}{\\ttfamily{allpets}} All pets owned}\n\\vskip 0.10in"
    )

  })

  test_that("codeBookItemTxtDescription NumericVariable", {
    res = codeBookItemTxtDescription(ds$ndogs)

    expect_equal(
      res,
      "\\vskip 0.10in\n\n\\addcontentsline{lot}{table}{\\parbox{1.8in}{\\ttfamily{ndogs}} Number of dogs}\n\\vskip 0.10in"
    )

  })

  test_that("codeBookItemTxtDescription DatetimeVariable", {
    res = codeBookItemTxtDescription(ds$wave)

    expect_equal(
      res,
      "\\vskip 0.10in\n\n\\addcontentsline{lot}{table}{\\parbox{1.8in}{\\ttfamily{wave}} Wave}\n\\vskip 0.10in"
    )
  })

})
