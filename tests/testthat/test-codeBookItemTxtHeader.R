context("codebookItemTxtHeader")

with_api_fixture("fixtures-1-2-5", {
  ds = crunch::loadDataset("Example dataset")

  test_that("codeBookItemTxtHeader CategoricalVariable", {
    res = codeBookItemTxtHeader(ds$q1)

    expect_equal(
      res,
      "\\textbf{Pet}\\hfill\\textbf{\\ttfamily{q1}}"
    )

  })

  test_that("codeBookItemTxtHeader CategoricalArrayVariable", {
    res = codeBookItemTxtHeader(ds$petloc)

    expect_equal(
      res,
      "\\textbf{Pets by location}\\hfill\\textbf{\\ttfamily{petloc}}"
    )
  })

  test_that("codeBookItemTxtHeader MultipleResponseVariable", {
    res = codeBookItemTxtHeader(ds$allpets)

    expect_equal(
      res,
      "\\textbf{All pets owned}\\hfill\\textbf{\\ttfamily{allpets}}"
    )

  })

  test_that("codeBookItemTxtHeader NumericVariable", {
    res = codeBookItemTxtHeader(ds$ndogs)

    expect_equal(
      res,
      "\\textbf{Number of dogs}\\hfill\\textbf{\\ttfamily{ndogs}}"
    )

  })

  test_that("codeBookItemTxtHeader DatetimeVariable", {
    res = codeBookItemTxtHeader(ds$wave)

    expect_equal(
      res,
      "\\textbf{Wave}\\hfill\\textbf{\\ttfamily{wave}}"
    )

  })
})
