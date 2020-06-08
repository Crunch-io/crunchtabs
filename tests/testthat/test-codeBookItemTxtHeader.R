context("codebookItemTxtHeader")

with_api_fixture("fixtures-1-2-5", {
  ds = crunch::loadDataset("Example dataset")

  test_that("codeBookItemTxtHeader CategoricalVariable", {
    res = codeBookItemTxtHeader(ds$q1)

    expect_equal(
      attributes(res)$kable_meta$contents,
      "\\[q1\\] & Pet"
    )

    expect_equal(
      attributes(res)$kable_meta$align_vector_origin,
      c("l", "r")
    )
  })

  test_that("codeBookItemTxtHeader CategoricalArrayVariable", {
    res = codeBookItemTxtHeader(ds$petloc)

    expect_equal(
      attributes(res)$kable_meta$contents,
      "\\[petloc\\] & Pets by location"
    )

    expect_equal(
      attributes(res)$kable_meta$align_vector_origin,
      c("l", "r")
    )
  })

  test_that("codeBookItemTxtHeader MultipleResponseVariable", {
    res = codeBookItemTxtHeader(ds$allpets)

    expect_equal(
      attributes(res)$kable_meta$contents,
      "\\[allpets\\] & All pets owned"
    )

    expect_equal(
      attributes(res)$kable_meta$align_vector_origin,
      c("l", "r")
    )
  })

  test_that("codeBookItemTxtHeader NumericVariable", {
    res = codeBookItemTxtHeader(ds$ndogs)

    expect_equal(
      attributes(res)$kable_meta$contents,
      "\\[ndogs\\] & Number of dogs"
    )

    expect_equal(
      attributes(res)$kable_meta$align_vector_origin,
      c("l", "r")
    )
  })

  test_that("codeBookItemTxtHeader DatetimeVariable", {
    res = codeBookItemTxtHeader(ds$wave)

    expect_equal(
      attributes(res)$kable_meta$contents,
      "\\[wave\\] & Wave"
    )

    expect_equal(
      attributes(res)$kable_meta$align_vector_origin,
      c("l", "r")
    )
  })
})
