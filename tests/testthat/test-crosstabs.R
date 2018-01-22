context("Generating Toplines and Crosstabs data summaries")

test_that("Error handling - not a dataset", {
  expect_error(crosstabs(NULL),
               "'dataset' is not an object of class 'CrunchDataset'.")
})

with_mock_crunch({
    ds <- loadDataset("https://app.crunch.io/api/datasets/ad5fa16abb5a46819139f7a421bf6d93")
    
  test_that("Error handling", {
    expect_error(crosstabs(ds, vars = c("a", "b")),
                 "Variables: a, b not found.")
    expect_error(crosstabs(ds, weight = "a_weight"),
            "No variable with alias a_weight found in 'dataset'.")
    expect_error(crosstabs(ds, weight = "allpets"),
            "No 'weight' variable with alias allpets found in 'dataset'.")
    expect_error(crosstabs(ds, banner = list()),
                 "'banner', if provided, must be an object of class 'Banner'.")
  })

  test_that("Warnings", {
    expect_warning(crosstabs(ds, vars = c("allpets", "petname")),
                 "Variables of types: text are not supported and have been skipped")
    expect_warning(crosstabs(ds, vars = c("allpets", "wave")),
                   "Variables of types: datetime are not supported and have been skipped")
    expect_warning(crosstabs(ds, vars = c("allpets", "petname", "wave")),
                   "Variables of types: text, datetime are not supported and have been skipped")

  })

  test_that("Toplines", {
    crosstabs_data <- crosstabs(ds, vars = c("allpets", "favpet", "petloc"))
    expect_s3_class(crosstabs_data, "Toplines")
    expect_named(crosstabs_data, c("metadata", "results", "banner"))
    expect_named(crosstabs_data$metadata, c("title", "date", "weight"))
    expect_null(crosstabs_data$banner)
    expect_identical(getName(crosstabs_data), "Example Dataset")
    expect_null(crosstabs_data$metadata$weight)
    expect_named(crosstabs_data$results, c("allpets", "favpet", "petloc"))
    expect_named(crosstabs_data$results$allpets, c("alias", "name", "type", "description", "notes", "counts", "proportions", "counts_unweighted", "total", "missing", "valid", 'subvariables'))
    expect_identical(crosstabs_data$results$allpets$alias, "allpets")
    expect_identical(crosstabs_data$results$allpets$name, "All pets owned")
    expect_identical(crosstabs_data$results$allpets$type, "multiple_response")
    expect_identical(crosstabs_data$results$allpets$description, "Do you have any of these animals as pets? Please select all that apply.")
    expect_identical(crosstabs_data$results$allpets$notes, "")
    x <- c(Cat=4, Dog=7, Bird=5)
    expect_identical(c(crosstabs_data$results$allpets$counts), x)
    x <- c(Cat=0.5000000, Dog=0.6363636, Bird=0.4545455)
    expect_equal(c(crosstabs_data$results$allpets$proportions), x, tolerance = 1e-7)
    expect_equal(crosstabs_data$results$allpets$total, 20)
    expect_equal(crosstabs_data$results$allpets$missing, 3)
    expect_equal(crosstabs_data$results$allpets$valid, 17)
  })
  
  banner_data <- unserializeJSON(readLines(con = file.path(fixtures_dir, "ds1_banner1.json")))
  tabBook_vars <- c("allpets", "favpet", "petloc", "ndogs", "ndogs_a", "ndogs_b", "country", "age", "age2", "age3", "age5", "gender", "weight", "noweight")
  with_mock_tabs(book_file = "4996c3212e25441bae5bd16605bbc659/tabbook-023abd.json",
                 mt_file = "4996c3212e25441bae5bd16605bbc659.json",
                 path = "app.crunch.io/api/datasets/ad5fa16abb5a46819139f7a421bf6d93/multitables/", expr = {
    crosstabs_data <- crosstabs(ds, vars = tabBook_vars, banner = banner_data)
    test_that("Crosstabs", {
    expect_s3_class(crosstabs_data, "Crosstabs")
    expect_named(crosstabs_data, c("metadata", "results", "banner"))
    expect_named(crosstabs_data$metadata, c("title", "date", "weight"))
    expect_s3_class(crosstabs_data$banner, "Banner")
    expect_identical(getName(crosstabs_data), "Example Dataset")
    expect_null(crosstabs_data$metadata$weight)
    expect_named(crosstabs_data$results, c("allpets", "favpet", "petloc_home", "petloc_work", "ndogs", "ndogs_a", "ndogs_b", "country", "age", "age2", "age3", "age5", "gender", "weight", "noweight"))
    expect_named(crosstabs_data$results$favpet, c("alias", "name", "description", "notes", "settings", "crosstabs"))
    expect_identical(crosstabs_data$results$favpet$alias, "favpet")
    expect_identical(crosstabs_data$results$favpet$name, "Favorite pet")
    expect_identical(crosstabs_data$results$favpet$description, "What is your favorite pet?")
    expect_identical(crosstabs_data$results$favpet$notes, NULL)
    expect_named(crosstabs_data$results$favpet$crosstabs, c("banner 1"))
  })
 })
})

