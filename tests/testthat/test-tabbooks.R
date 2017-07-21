context('Preparing a banner data summary')

with_mock_crunch({
  ds <- loadDataset("https://app.crunch.io/api/datasets/995/")
  test_that("We can load a dataset from the crunchtabs package", {
    expect_s4_class(ds, "CrunchDataset")
    expect_identical(name(ds), "Example dataset")
  })

  banner_data <- unserializeJSON(readLines(con = file.path(fixtures_dir, "ds1_banner1.json")))
  test_that("We can read in a Banner object from a file", {
    expect_s3_class(banner_data, "Banner")
  })

  tabBook_vars <- c("allpets", "q1", "petloc", "ndogs", "ndogs_a", "ndogs_b", "country", "Weight", "age", "age2", "age3", "age5", "gender")
  # with_POST("https://app.crunch.io/api/datasets/995/multitables/e263e0b9ffb8405ba12ca10825425dad/tabbook-22aab0", {
  with_mock_tabs(book_file = "e263e0b9ffb8405ba12ca10825425dad/tabbook-22aab0.json",
                 mt_file = "e263e0b9ffb8405ba12ca10825425dad.json",
                 path = "app.crunch.io/api/datasets/995/multitables/", expr = {
    tabBooks_data <- tabBooks(dataset = ds, vars = tabBook_vars, banner = banner_data, weight = NULL)
    test_that("We can generate a tabBook data summary", {
      expect_is(tabBooks_data, "list")
      expect_named(tabBooks_data, c("allpets", "q1", "petloc_home", "petloc_work", "ndogs", "ndogs_a", "ndogs_b", "country", "Weight", "age", "age2", "age3", "age5", "gender"))
      expect_named(tabBooks_data$allpets, c("alias", "name", "description", "notes", "options", "crosstabs"))
      expect_named(tabBooks_data$allpets$crosstabs, c("banner 1"))
      expect_named(tabBooks_data$allpets$crosstabs$`banner 1`, c("Total", "gender", "age5"))
      expect_named(tabBooks_data$allpets$crosstabs$`banner 1`$Total, c("counts", "proportions", "totals_counts", "totals_proportions", "unweighted_n", "counts_unweighted", "pvals_col"))
      expect_identical(tabBooks_data$allpets$crosstabs$`banner 1`$Total$counts,
                       structure(c(4,5,5), .Dim = c(3,1), .Dimnames = list(allpets = c("Cat", "Dog", "Bird"), total = c("Total"))))
      expect_equal(tabBooks_data$allpets$crosstabs$`banner 1`$Total$proportions,
                   structure(c(0.2352941,0.2941176,0.2941176), .Dim = c(3,1), .Dimnames = list(allpets = c("Cat", "Dog", "Bird"), total = c("Total"))),
                   tolerance = 1e-7)
      expect_identical(tabBooks_data$allpets$crosstabs$`banner 1`$Total$totals_counts,
                       structure(c(17), .Dim = c(1,1), .Dimnames = list(NULL, total = c("Total"))))
      expect_equal(tabBooks_data$allpets$crosstabs$`banner 1`$Total$totals_proportions,
                   structure(c(0.8235294), .Dim = c(1,1), .Dimnames = list(NULL, total = c("Total"))),
                   tolerance = 1e-7)
      expect_identical(tabBooks_data$allpets$crosstabs$`banner 1`$Total$unweighted_n,
                       structure(c(17), .Dim = c(1,1), .Dimnames = list(NULL, total = c("Total"))))
      expect_identical(tabBooks_data$allpets$crosstabs$`banner 1`$Total$counts_unweighted,
                       structure(c(4,5,5), .Dim = c(3,1), .Dimnames = list(allpets = c("Cat", "Dog", "Bird"), total = c("Total"))))

      expect_identical(tabBooks_data$allpets$crosstabs$`banner 1`$gender$counts,
                       structure(c(3,2,3,1,3,2), .Dim = c(3,2), .Dimnames = list(allpets = c("Cat", "Dog", "Bird"), gender = c("Male", "Female"))))
      expect_equal(tabBooks_data$allpets$crosstabs$`banner 1`$gender$proportions,
                   structure(c(0.3333333,0.2222222,0.3333333,0.125,0.375,0.250), .Dim = c(3,2), .Dimnames = list(allpets = c("Cat", "Dog", "Bird"), gender = c("Male", "Female"))),
                   tolerance = 1e-7)
      expect_identical(tabBooks_data$allpets$crosstabs$`banner 1`$gender$totals_counts,
                       structure(c(9,8), .Dim = c(1,2), .Dimnames = list(NULL, gender = c("Male", "Female"))))
      expect_equal(tabBooks_data$allpets$crosstabs$`banner 1`$gender$totals_proportions,
                   structure(c(0.8888889, 0.75), .Dim = c(1,2), .Dimnames = list(NULL, gender = c("Male", "Female"))),
                   tolerance = 1e-7)
      expect_identical(tabBooks_data$allpets$crosstabs$`banner 1`$gender$unweighted_n,
                       structure(c(9,8), .Dim = c(1,2), .Dimnames = list(NULL, gender = c("Male", "Female"))))
      expect_identical(tabBooks_data$allpets$crosstabs$`banner 1`$gender$counts_unweighted,
                       structure(c(3,2,3,1,3,2), .Dim = c(3,2), .Dimnames = list(allpets = c("Cat", "Dog", "Bird"), gender = c("Male", "Female"))))
    })

    crosstabs_summary <- crosstabs(ds, vars = tabBook_vars, banner = banner_data, weight = NULL)
    test_that("We can generate a banner data summary", {
      expect_s3_class(crosstabs_summary, "Crosstabs")
    })
  })
})
