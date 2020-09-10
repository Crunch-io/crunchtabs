context("writeCodeBookLatex")

with_api_fixture("fixtures-1-2-5", {
  ds = crunch::loadDataset("Example dataset")

  test_that("Tex result as expected with features", {

    suppressWarnings(
      writeCodeBookLatex(
      ds,
      title = "Hello",
      subtitle = "Goodbye",
      sample_desc = "US Voting Adults",
      logo = "yougov",
      pdf = FALSE)
    )

    tex <- readLines("Example-dataset.tex")
    original <- readRDS("fixtures/writeCodeBookLatexOne.rds")

    expect_true(length(tex) == length(original))
    expect_true(sum(tex %in% original)/length(tex) > 0.98)

  })

  test_that("Tex result as expected with features", {

    suppressWarnings(
      writeCodeBookLatex(
        ds,
        title = "Hello",
        subtitle = "Goodbye",
        sample_desc = "US Voting Adults",
        logo = "ygblue",
        pdf = FALSE)
    )

    tex <- readLines("Example-dataset.tex")
    original <- readRDS("fixtures/writeCodeBookLatexOne.rds")

    expect_true(length(tex) == length(original))
    expect_true(sum(tex %in% original)/length(tex) > 0.98)

  })

  test_that("Creates pdf output", {


    with_mock(suppressWarnings(
      writeCodeBookLatex(
        ds,
        title = "Hello",
        subtitle = "Goodbye",
        sample_desc = "US Voting Adults",
        logo = "ygblue",
        pdf = TRUE)
    ), "crunchtabs::file.open" = function(x) return(TRUE))

    expect_true(file.exists("Example-dataset.pdf"))
    expect_true(file.remove("Example-dataset.pdf"))

  })

  test_that("Tex result set position as expected", {
    suppressWarnings(
      writeCodeBookLatex(
        ds,
        title = "Hello",
        subtitle = "Goodbye",
        sample_desc = "US Voting Adults",
        logo = "ygblue",
        position = "c",
        pdf = FALSE)
    )

    tex <- readLines("Example-dataset.tex")
    expect_true(any(grepl("\\begin{longtable}[c]", tex, fixed = TRUE)))
  })
})

with_api_fixture("fixtures-1-2-5", {
  ds = crunch::loadDataset("Example dataset")

  test_that("Default tex as expected", {
    suppressWarnings(writeCodeBookLatex(ds, pdf = FALSE))
    tex <- readLines("Example-dataset.tex")
    original <- readRDS("fixtures/writeCodeBookLatexFull.rds")
    expect_true(length(tex) == length(original))
    expect_true(sum(tex %in% original)/length(tex) > 0.98)
  })
})

with_api_fixture("fixtures-1-2-5", {
  ds = loadDataset(
    "https://app.crunch.io/dataset/10c3c3/"
  )

  test_that("Default tex as expected", {
    suppressWarnings(writeCodeBookLatex(
      ds[1],
      url = "https://app.crunch.io/dataset/10c3c3/",
      appendix = TRUE, suppress_zero_counts = FALSE, pdf = FALSE)
    )
    tex <- readLines("Data-for-Progress-National-Issues-Survey----Foreign-Policy.tex")
    original <- readRDS("fixtures/writeCodeBookLatexLongCat.rds")
    expect_true(length(tex) == length(original))
    expect_true(sum(tex %in% original)/length(tex) > 0.98)
  })

  test_that("Default tex as expected", {
    dir.create("tmp")
    suppressWarnings(writeCodeBookLatex(
      ds[1],
      url = "https://app.crunch.io/dataset/10c3c3/",
      appendix = TRUE, suppress_zero_counts = FALSE, pdf = FALSE, path = "tmp")
    )
    tex <- readLines("tmp/Data-for-Progress-National-Issues-Survey----Foreign-Policy.tex")
    original <- readRDS("fixtures/writeCodeBookLatexLongCat.rds")
    expect_true(length(tex) == length(original))
    expect_true(sum(tex %in% original)/length(tex) > 0.98)
    file.remove("tmp/Data-for-Progress-National-Issues-Survey----Foreign-Policy.tex")
    file.remove("tmp")
  })
})

# with_api_fixture("fixtures-1-2-5", {
#   ds = loadDataset(
#     "https://app.crunch.io/dataset/10c3c3cbd28b420aaa4976b70caba851/"
#   )
#
#   test_that("Default tex as expected", {
#     suppressWarnings(writeCodeBookLatex(
#       ds[1],
#       url = "https://app.crunch.io/dataset/10c3c3cbd28b420aaa4976b70caba851/",
#       appendix = TRUE, suppress_zero_counts = FALSE, pdf = FALSE)
#     )
#     tex <- readLines("Data-for-Progress-National-Issues-Survey----Foreign-Policy.tex")
#     original <- readRDS("fixtures/writeCodeBookLatexLongCat.rds")
#     expect_true(length(tex) == length(original))
#     expect_true(sum(tex %in% original)/length(tex) > 0.98)
#   })
# })

context("codeBookItemBody")

test_that("Errors appropriately when passed bad object", {
  expect_error(codeBookItemBody(c(1,2,3,4)))
})

context("kable_strip_rules")

test_that("strips rules as expected", {
  x <- "\\toprule\\bottomrule\\midrule"
  r <- kable_strip_rules(x)
  expect_equal(r, "")

  r <- kable_strip_toprules(x)
  expect_equal(r, "\\bottomrule\\midrule")
})

test_that("fixes underscore", {
  x <- "___"
  r <- fixUnderscore(x)
  expect_equal(r, "\\_\\_\\_")
})

test_that("default yg logo returns normal path", {
  p <- default_yg_logo()
  expect_equal(p, system.file("YouGov.png", package = "crunchtabs"))
})
