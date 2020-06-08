context("codeBookItemBody")

with_api_fixture("fixtures-1-2-5", {
  ds = crunch::loadDataset("Example dataset")

  test_that("codeBookItemBody CategoricalVariable", {
    res = codeBookItemBody(ds$q1)

    expect_equal(
      attributes(res)$kable_meta$contents,
      c(
        "1 & Cat & 6",
        "2 & Dog & 4",
        "3 & Bird & 3",
        "8 & Skipped & 3",
        "9 & Not Asked & 4"
      )
    )

    expect_equal(
      attributes(res)$kable_meta$align_vector_origin,
      c("c", "l", "r")
    )
  })

  test_that("codeBookItemBody CategoricalArrayVariable", {
    res = codeBookItemBody(ds$petloc)

    expect_equal(
      attributes(res)$kable_meta$contents,
      c(" &  & 1 Cat & 2 Dog & 3 Bird & 8 Skipped & 9 Not Asked",
        "petloc\\\\_home & Home & 3 & 5 & 3 & 5 & 4",
        "petloc\\\\_work & Work & 6 & 6 & 4 & 1 & 3"
      )

    )

    expect_equal(
      attributes(res)$kable_meta$align_vector_origin,
      c("l", "l", "c", "c", "c", "c", "c")
    )
  })

  test_that("codeBookItemBody MultipleResponseVariable", {
    res = codeBookItemBody(ds$allpets)

    expect_equal(
      attributes(res)$kable_meta$contents,
        c(" &  & 1 selected & 2 not selected & 8 skipped & 9 not asked",
          "allpets\\\\_1 & Cat & 8 & 4 & 4 & 4",
          "allpets\\\\_2 & Dog & 8 & 3 & 5 & 4",
          "allpets\\\\_3 & Bird & 3 & 6 & 5 & 6"
        )
    )

    expect_equal(
      attributes(res)$kable_meta$align_vector_origin,
      c("l", "l", "X", "c", "c", "c")
    )
  })

  test_that("codeBookItemBody NumericVariable", {
    res = codeBookItemBody(ds$ndogs)

    expect_equal(
      attributes(res)$kable_meta$contents,
      c(
        "Type & Numeric",
        "Missing & 4",
        "Range & \\[0, 6\\]"
      )
    )

    expect_equal(
      attributes(res)$kable_meta$align_vector_origin,
      c("l", "l")
    )
  })

  test_that("codeBookItemBody DatetimeVariable", {
    res = codeBookItemBody(ds$wave)

    expect_equal(
      attributes(res)$kable_meta$contents,
      c(
        "Type & Datetime",
        "Range & \\[2014-12-01, 2015-01-01\\]"
      )
    )

    expect_equal(
      attributes(res)$kable_meta$align_vector_origin,
      c("l", "l")
    )
  })

  test_that("codeBookItemBody TextVariable", {
    res = codeBookItemBody(ds$q3)

    expect_equal(
      attributes(res)$kable_meta$contents,
      c("Type & Text", "Filled & 16")
    )

    expect_equal(
      attributes(res)$kable_meta$align_vector_origin,
      c("l", "l")
    )
  })

})
