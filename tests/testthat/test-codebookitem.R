# Expects wd to be tests/testthat

context("codebookItemTxt")

with_api_fixture("fixtures-1-2-5", {
  ds = crunch::loadDataset("Example dataset")

  test_that("codebookItemTxT returns as expected", {
    res = codebookItemTxt(ds$q1)
    dput(res)
  })
})

with_api_fixture("fixtures-1-2-5", {

  ds <- crunch::loadDataset("Example dataset")
 }
)
