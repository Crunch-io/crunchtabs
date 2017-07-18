context('Preparing a banner data summary')

with_mock_crunch({
  ds <- loadDataset("https://app.crunch.io/api/datasets/9955eddef2674cb895a4f91857965e9f/")
  test_that("We can load a dataset from the crunchtabs package", {
    expect_s4_class(ds, "CrunchDataset")
    expect_identical(name(ds), "Example dataset")
  })

  banner_data <- unserializeJSON(readLines(con = file.path(fixtures_dir, "ds1_banner1.json")))
  test_that("We can read in a Banner object from a file", {
    expect_s3_class(banner_data, "Banner")
  })

  tabBook_vars <- c("allpets", "q1", "petloc", "ndogs", "ndogs_a", "ndogs_b", "country", "Weight", "age", "age2", "age3", "age5", "gender")
  tabBooks_data <- tabBooks(dataset = ds, vars = tabBook_vars, banner = banner_data, weight = NULL)
  test_that("We can generate a tabBook data summary", {
    expect_is(tabBooks_data, "list")
    expect_named(tabBooks_data, c("allpets", "q1", "petloc_home", "petloc_work", "ndogs", "ndogs_a", "ndogs_b", "country", "Weight", "age", "age2", "age3", "age5", "gender"))
    expect_named(tabBooks_data$allpets, c("alias", "name", "description", "notes", "options", "crosstabs"))
    expect_named(tabBooks_data$allpets$crosstabs, c("banner 1"))
    expect_named(tabBooks_data$allpets$crosstabs$`banner 1`, c("Total", "gender", "age5"))
    expect_named(tabBooks_data$allpets$crosstabs$`banner 1`$Total, c("counts", "proportions", "totals_counts", "totals_proportions", "unweighted_n", "counts_unweighted", "pvals_col"))
  })

  crosstabs_summary <- crosstabs(ds, vars = tabBook_vars, banner = banner_data, weight = NULL)
  test_that("We can generate a banner data summary", {
    expect_s3_class(crosstabs_summary, "Crosstabs")
  })
})
