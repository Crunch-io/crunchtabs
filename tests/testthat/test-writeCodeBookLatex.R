context("codebookItemTxtHeader")

with_api_fixture("fixtures-1-2-5", {
  ds = crunch::loadDataset("Example dataset")

  test_that("Tex result as expected with features", {

    suppressWarnings(
      writeCodeBookLatex(
      ds,
      title = "Hello",
      subtitle = "Goodbye",
      sample_desc = "US Voting Adults",
      logo = "yougov")
    ))

    tex <- readLines("Example-dataset.tex")
    original <- readRDS("fixtures/writeCodeBookLatexOne.rds")
    expect_equal(tex, original)
  })
})

with_api_fixture("fixtures-1-2-5", {
  ds = crunch::loadDataset("Example dataset")

  test_that("Default tex as expected", {
    suppressWarnings(writeCodeBookLatex(ds))
    tex <- readLines("Example-dataset.tex")
    original <- readRDS("fixtures/writeCodeBookLatexFull.rds")
    expect_equal(tex, original)
  })
})
