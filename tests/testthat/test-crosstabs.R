context("Generating Toplines and Crosstabs data summaries")

test_that("Error handling - not a dataset", {
  expect_error(crosstabs(NULL),
               "'dataset' is not an object of class 'CrunchDataset'.")
})

with_mock_crunch({
  ds <- loadDataset("https://app.crunch.io/api/datasets/995/")
  test_that("Error handling", {
    expect_error(crosstabs(ds, vars = c('a', 'b')),
                 "Variables: a, b not found.")
    expect_error(crosstabs(ds, weight = 'a_weight'),
                 "No 'weight' variable with alias a_weight found in 'dataset'.")
    expect_error(crosstabs(ds, weight = 'allpets'),
                 "The weight variable has to be numeric.")
    expect_error(crosstabs(ds, banner = list()),
                 "'banner', if provided, must be an object of class 'Banner'.")
  })

  test_that("Warnings", {
    expect_warning(crosstabs(ds, vars = c('allpets', "q3")),
                 "Variables of types: text are not supported and have been skipped")
    expect_warning(crosstabs(ds, vars = c('allpets', "wave")),
                   "Variables of types: datetime are not supported and have been skipped")
    expect_warning(crosstabs(ds, vars = c('allpets', "q3", "wave")),
                   "Variables of types: text, datetime are not supported and have been skipped")

  })

  test_that("Toplines", {
    crosstabs_data <- crosstabs(ds, vars = c('allpets', 'q1', 'petloc'))
    expect_s3_class(crosstabs_data, "Toplines")
    expect_named(crosstabs_data, c("title", "date", "results", "banner", "weight"))
    expect_null(crosstabs_data$banner)
    expect_identical(crosstabs_data$title, "Example dataset")
    expect_null(crosstabs_data$weight)
    expect_named(crosstabs_data$results, c("allpets", "q1", "petloc"))
    expect_named(crosstabs_data$results$allpets, c("alias", "name", "type", "description", "notes", "counts", "proportions", "total", "missing", "valid"))
    expect_identical(crosstabs_data$results$allpets$alias, "allpets")
    expect_identical(crosstabs_data$results$allpets$name, "All pets owned")
    expect_identical(crosstabs_data$results$allpets$type, "multiple_response")
    expect_identical(crosstabs_data$results$allpets$description, "Do you have any of these animals as pets? Please select all that apply.")
    expect_identical(crosstabs_data$results$allpets$notes, "")
    x <- matrix(c(4,5,5,13,12,12,3,3,3), nrow = 3, ncol = 3, dimnames = list(c("Cat", "Dog", "Bird"), c("selected", "not selected", "missing")))
    expect_identical(crosstabs_data$results$allpets$counts, x)
    x <- array(c(0.2352941,0.2941176,0.2941176), dim = c(3), dimnames = list(allpets=c("Cat", "Dog", "Bird")))
    expect_equal(crosstabs_data$results$allpets$proportions, x, tolerance = 1e-7)
    expect_equal(crosstabs_data$results$allpets$total, 20)
    expect_equal(crosstabs_data$results$allpets$missing, 3)
    expect_equal(crosstabs_data$results$allpets$valid, 17)
  })

  banner_data <- unserializeJSON(readLines(con = file.path(fixtures_dir, "ds1_banner1.json")))
  tabBook_vars <- c("allpets", "q1", "petloc", "ndogs", "ndogs_a", "ndogs_b", "country", "Weight", "age", "age2", "age3", "age5", "gender")
  with_mock_tabs(book_file = "e26/tabbook-22aab0.json",
                 mt_file = "e26.json",
                 path = "app.crunch.io/api/datasets/995/multitables/", expr = {
    crosstabs_data <- crosstabs(ds, vars = tabBook_vars, banner = banner_data)
    test_that("Crosstabs", {
    expect_s3_class(crosstabs_data, "Crosstabs")
    expect_named(crosstabs_data, c("title", "date", "results", "banner", "weight"))
    expect_s3_class(crosstabs_data$banner, "Banner")
    expect_identical(crosstabs_data$title, "Example dataset")
    expect_null(crosstabs_data$weight)
    expect_named(crosstabs_data$results, c('allpets', 'q1', 'petloc_home', 'petloc_work', 'ndogs', 'ndogs_a', 'ndogs_b', 'country', 'Weight', 'age', 'age2', 'age3', 'age5', 'gender'))
    expect_named(crosstabs_data$results$q1, c('alias', 'name', 'description', 'notes', 'settings', 'crosstabs'))
    expect_identical(crosstabs_data$results$q1$alias, "q1")
    expect_identical(crosstabs_data$results$q1$name, "Pet")
    expect_identical(crosstabs_data$results$q1$description, "What is your favorite pet?")
    expect_identical(crosstabs_data$results$q1$notes, NULL)
    expect_named(crosstabs_data$results$q1$crosstabs, c("banner 1"))
  })
 })
})

