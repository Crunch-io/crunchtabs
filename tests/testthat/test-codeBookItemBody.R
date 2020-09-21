context("codeBookItemBody")

with_api_fixture("fixtures-1-2-5", {
  ds = crunch::loadDataset("Example dataset")

  test_that("codeBookItemBody CategoricalVariable", {
    res = codeBookItemBody(ds$q1)

    expect_equal(
      attributes(res)$kable_meta$contents,
      c("\\{Code\\} & \\{Label\\} & \\{Count\\}",
        "1 & Cat & 6", "2 & Dog & 4", "3 & Bird & 3",
        "8 & Skipped & 3", "9 & Not Asked & 4")
    )

    expect_equal(
      attributes(res)$kable_meta$align_vector_origin,
      c("J", "l", "J")
    )
  })

  test_that("codeBookItemBody CategoricalArrayVariable", {
    res = codeBookItemBody(ds$petloc)

    expect_equal(
      attributes(res$kcounts)$kable_meta$contents,
      c("\\{Variable\\} & \\{1\\} & \\{2\\} & \\{3\\} & \\{8\\} & \\{9\\}",
        "\\\\ttfamily\\{petloc\\\\_home\\} & 5 & 3 & 3 & 4 & 5", "\\\\ttfamily\\{petloc\\\\_work\\} & 6 & 4 & 6 & 3 & 1"
      )
    )

    expect_equal(
      attributes(res$kcounts)$kable_meta$align_vector_origin,
      c("l", "J", "J", "J", "J", "J")
    )
  })

  test_that("codeBookItemBody MultipleResponseVariable", {
    res = codeBookItemBody(ds$allpets)

    expect_equal(
      attributes(res$kcounts)$kable_meta$contents,
      c("\\{Variable\\} & \\{1\\} & \\{2\\} & \\{8\\} & \\{9\\}",
        "\\\\ttfamily\\{allpets\\\\_1\\} & 4 & 4 & 4 & 8",
        "\\\\ttfamily\\{allpets\\\\_2\\} & 5 & 3 & 4 & 8",
        "\\\\ttfamily\\{allpets\\\\_3\\} & 5 & 6 & 6 & 3"
      )
    )
    expect_equal(
      attributes(res$kcounts)$kable_meta$align_vector_origin,
      c("l", "J", "J", "J", "J")
    )
  })

  test_that("codeBookItemBody NumericVariable", {
    res = codeBookItemBody(ds$ndogs)

    expect_equal(
      attributes(res)$kable_meta$contents,
      c(
        "\\{Mean\\} & \\{SD\\} & \\{Min\\} & \\{Max\\} & \\{n\\} & \\{Missing\\}",
        "2 & 1 & 0 & 6 & 16 & 4")
    )

    expect_equal(
      attributes(res)$kable_meta$align_vector_origin,
      rep("c", 6)
    )
  })

  test_that("codeBookItemBody DatetimeVariable", {
    res = codeBookItemBody(ds$wave)

    expect_equal(
      attributes(res)$kable_meta$contents,
      c(
        "Filled & Missing & Range",
        "20 & 0 & \\[2014-12-01, 2015-01-01\\]"
      )
    )

    expect_equal(
      attributes(res)$kable_meta$align_vector_origin,
      c("c", "c", "c")
    )
  })

  test_that("codeBookItemBody TextVariable", {
    res = codeBookItemBody(ds$q3)

    expect_equal(
      attributes(res)$kable_meta$contents,
      c("\\{Filled\\} & \\{Missing\\} & \\{Max Length\\}",
        "16 & 4 & 8"
      )
    )

    expect_equal(
      attributes(res)$kable_meta$align_vector_origin,
      c("d", "d", "d")
    )
  })

})
