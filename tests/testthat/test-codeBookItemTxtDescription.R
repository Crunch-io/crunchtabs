context("codeBookItemTxtDescription")

with_api_fixture("fixtures-1-2-5", {
  ds = crunch::loadDataset("Example dataset")

  test_that("codeBookItemTxtDescription CategoricalVariable", {
    res = codeBookItemTxtDescription(ds$q1)

    expect_equal(
      res,
      "\\vskip 0.10in\n\\addcontentsline{lot}{table}{What is your favorite pet?}"
    )
  })

  test_that("codeBookItemTxtDescription CategoricalArrayVariable", {
    res = codeBookItemTxtDescription(ds$petloc)

    expect_equal(
     res,
     "\\vskip 0.10in\n\\addcontentsline{lot}{table}{Name the kinds of pets you have at these locations.}"
    )

  })

  test_that("codeBookItemTxtDescription MultipleResponseVariable", {
    res = codeBookItemTxtDescription(ds$allpets)

    expect_equal(
      res,
      "\\vskip 0.10in\n\\addcontentsline{lot}{table}{Do you have any of these animals as pets? Please select all that apply.}"
    )

  })

  test_that("codeBookItemTxtDescription NumericVariable", {
    res = codeBookItemTxtDescription(ds$ndogs)

    expect_equal(
      res,
      "\\vskip 0.10in\n\\addcontentsline{lot}{table}{}"
    )

  })

  test_that("codeBookItemTxtDescription DatetimeVariable", {
    res = codeBookItemTxtDescription(ds$wave)

    expect_equal(
      res,
      "\\vskip 0.10in\n\\addcontentsline{lot}{table}{}"
    )
  })

})
