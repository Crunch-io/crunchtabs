context('Preparing banner data summary')
test_that("We can read a Banner object from a file", {
  txt <- readLines(con = file.path(fixtures_dir, "ds1_banner1.json"))
  banner_data <- unserializeJSON(txt)
  expect_is(banner_data, "Banner")
})

context('Loading a dataset from the crunch package')
with_mock_crunch({
  test_that("We can load a dataset from the crunch package", {
    ds <- loadDataset("test ds")
    expect_is(ds, "CrunchDataset")
  })
})

context('Loading a dataset from the crunchtabas package')
with_mock_crunchtabs({
  test_that("We can load a dataset from the crunchtabas package", {
    ds <- loadDataset("Example dataset")
    expect_is(ds, "CrunchDataset")
  })
})

context('Loading a dataset from the crunchtabas package with mock API')
with_mock_API({
  test_that("We can load a dataset from the crunchtabas package with mock API", {
    ds <- loadDataset("Example dataset")
    expect_is(ds, "CrunchDataset")
  })
})

context('Loading a dataset from the crunchtabas package with mock API using URL')
with_mock_API({
  test_that("We can load a dataset from the crunchtabas package with mock API using URL", {
    ds <- loadDataset("https://app.crunch.io/api/datasets/9955eddef2674cb895a4f91857965e9f/")
    expect_is(ds, "CrunchDataset")
  })
})

context('Loading a dataset from the crunchtabas package using URL')
with_mock_crunchtabs({
  test_that("We can load a dataset from the crunchtabas package using URL", {
    ds <- loadDataset("https://app.crunch.io/api/datasets/9955eddef2674cb895a4f91857965e9f/")
    expect_is(ds, "CrunchDataset")
  })
})
