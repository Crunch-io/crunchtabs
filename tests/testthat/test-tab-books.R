context('Preparing a banner data summary')

with_mock_crunch({
  ds <- loadDataset("https://app.crunch.io/api/datasets/9955eddef2674cb895a4f91857965e9f/")
  test_that("We can load a dataset from the crunchtabs package", {
    expect_is(ds, "CrunchDataset")
    expect_identical(name(ds), "Example dataset")
  })

  banner_data <- unserializeJSON(readLines(con = file.path(fixtures_dir, "ds1_banner1.json")))
  test_that("We can read in a Banner object from a file", {
    expect_is(banner_data, "Banner")
  })

  tabBook_vars <- c("allpets", "q1", "petloc", "ndogs", "ndogs_a", "ndogs_b", "country", "Weight", "age", "age2", "age3", "age5", "gender")
  tabBooks_data <- tabBooks(dataset = ds, vars = tabBook_vars, banner = banner_data, weight = NULL)
  test_that("We can generate a tabBook data summary", {
    expect_is(tabBooks_data, "list")
    expect_identical(names(tabBooks_data), c("allpets", "q1", "petloc_home", "petloc_work", "ndogs", "ndogs_a", "ndogs_b", "country", "Weight", "age", "age2", "age3", "age5", "gender"))
    expect_identical(names(tabBooks_data[[1]]), c("alias", "name", "description", "notes", "options", "crosstabs"))
  })

  crosstabs_summary <- crosstabs(ds, vars = tabBook_vars, banner = banner_data, weight = NULL)
  test_that("We can generate a banner data summary", {
    expect_is(crosstabs_summary, c("Crosstabs"))
  })
})
