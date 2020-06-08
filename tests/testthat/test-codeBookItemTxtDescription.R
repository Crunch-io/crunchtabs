context("codeBookItemTxtDescription")

with_api_fixture("fixtures-1-2-5", {
  ds = crunch::loadDataset("Example dataset")

  test_that("codeBookItemTxtDescription CategoricalVariable", {
    res = codeBookItemTxtDescription(ds$q1)

    expect_equal(
      attributes(res)$kable_meta$contents,
      c("What is your favorite pet\\?", "")
    )

    expect_equal(
      attributes(res)$kable_meta$align_vector_origin,
      c("l")
    )
  })

  test_that("codeBookItemTxtDescription CategoricalArrayVariable", {
    res = codeBookItemTxtDescription(ds$petloc)

    expect_equal(
      attributes(res)$kable_meta$contents,
      c("Name the kinds of pets you have at these locations.", "")
    )

    expect_equal(
      attributes(res)$kable_meta$align_vector_origin,
      c("l")
    )
  })

  test_that("codeBookItemTxtDescription MultipleResponseVariable", {
    res = codeBookItemTxtDescription(ds$allpets)

    expect_equal(
      attributes(res)$kable_meta$contents,
      c(
        "Do you have any of these animals as pets\\? Please select all that apply.",
        ""
        )
    )

    expect_equal(
      attributes(res)$kable_meta$align_vector_origin,
      c("l")
    )
  })

  test_that("codeBookItemTxtDescription NumericVariable", {
    res = codeBookItemTxtDescription(ds$ndogs)

    expect_equal(
      attributes(res)$kable_meta$contents,
      c("No question text", "")
    )

    expect_equal(
      attributes(res)$kable_meta$align_vector_origin,
      c("l")
    )
  })

  test_that("codeBookItemTxtDescription DatetimeVariable", {
    res = codeBookItemTxtDescription(ds$wave)

    expect_equal(
      attributes(res)$kable_meta$contents,
      c("No question text", "")
    )

    expect_equal(
      attributes(res)$kable_meta$align_vector_origin,
      c("l")
    )
  })

})
