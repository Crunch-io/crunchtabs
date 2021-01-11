context("Banner object creation")

# Cheating for no login hit ----
cats = new(
  "Categories", .Data = list(
    new("Category", .Data = list(1L, FALSE, "16 to 24", NULL), names = c("id", "missing", "name", "numeric_value")),
    new("Category", .Data = list(2L, FALSE, "25 to 34", NULL), names = c("id", "missing", "name", "numeric_value")),
    new("Category", .Data = list(3L, FALSE, "35 to 44", NULL), names = c("id", "missing", "name", "numeric_value")),
    new("Category", .Data = list(4L, FALSE, "45 to 54", NULL), names = c("id", "missing", "name", "numeric_value")),
    new("Category", .Data = list(
      5L, FALSE, "55+", NULL), names = c("id", "missing", "name",
"numeric_value")), new("Category", .Data = list(-1L, TRUE, "No Data",
 NULL), names = c("id", "missing", "name", "numeric_value"
 ))))

subvars = readRDS(test_path("fixtures/subvar-responses.rds"))

ds = readRDS(test_path("fixtures/banner_ds.rds"))
categories_over = function(x) cats
subvariables_over = function(x) subvars


test_that("Error handling - not a dataset", {
    expect_error(banner(list()),"The expected class for `dataset` is CrunchDataset, not list.")
})

test_that("Error handling - banner", {
  # Tests that don't require auth
  ds = readRDS("fixtures/banner_ds.rds")
  expect_error(banner(ds, "a"),"`vars` must be a list of vectors.")
  expect_error(banner(ds, c("a", "b")),"`vars` must be a list of vectors.")
  expect_error(banner(ds, list(c())),"`vars` must have a length greater than 0.")
  expect_error(
    banner(ds, list(Results = c("a", "b"))),
    "Variables in `vars` must be valid aliases in aliases(allVariables(dataset)). This is not true for 'a' and 'b'.", fixed = TRUE
  )

  expect_error(banner(ds, list(Results = c("art3", "a", "tv4", "b", "movies5"))),
               "Variables in `vars` must be valid aliases in aliases(allVariables(dataset)). This is not true for 'a' and 'b'.", fixed = TRUE
  )
  expect_error(banner(ds, list(Results = c("art2")), labels = list("Doggy")),
               "`labels` must be a named list or vector.")
  expect_error(banner(ds, list(Results = c("art3")), labels = c(allpets1 = "All Pets")),
               "Variables in `labels` must be included in `vars`. This is not true for 'allpets1'.")
  expect_error(banner(ds, list(Results = c("art3", "age4")), labels = c(allpets1 = "All Pets", age4 = "Age (2 categories")),
               "Variables in `labels` must be included in `vars`. This is not true for 'allpets1'.")
  expect_error(banner(ds, list(Results = c("art2")), recodes = list(list("Dog" = "Doggy"))),
               "`recodes` must be a named list.")
  expect_error(banner(ds, list(Results = c("art2")), recodes = list(favpet1 = list("Dog" = "Doggy"))),
               "Variables in `recodes` must be included in `vars`. This is not true for 'favpet1'.")
  expect_error(banner(ds, list(Results = c("art2", "age4")), recodes = list(age4 = list("16 to 34" = "<= 34"), favpet1 = list("Dog" = "Doggy"))),
               "Variables in `recodes` must be included in `vars`. This is not true for 'favpet1'.")
  expect_error(banner(ds, list(Results = c("art2")), recodes = list("art2"=c("Dog" = "Doggy"))),
               "`recodes` must be a list of lists. This is not true for art2.")
  expect_error(banner(ds, list(Results = c("movies5"))),
               "Variables in `vars` must be of type categorical or multiple_response. This is not true for 'movies5'.")
  expect_error(banner(ds, list(Results = c("movies5", "tv4", "movies3", "age"))),
               "Variables in `vars` must be of type categorical or multiple_response. This is not true for 'movies5', 'tv4', 'movies3', and 'age'.")
  expect_error(banner(ds, list(Results = c("art3", "movies5", "movies3", "age4"))),
               "Variables in `vars` must be of type categorical or multiple_response. This is not true for 'movies5' and 'movies3'.")
})



test_that("Single banner with single variable", {
  ds = readRDS("fixtures/banner_ds.rds")
  banner_data <- with_mock(categories = categories_over, banner(ds, vars = list(Results = c("age5"))))
  expect_s3_class(banner_data, "Banner")
  expect_length(banner_data, 1)
  expect_named(banner_data, "Results")
  expect_is(banner_data, "Banner")
  expect_is(banner_data[["Results"]], "list")
  expect_length(banner_data[["Results"]], 2)
  expect_named(banner_data[["Results"]], c("___total___", "age5"))
  expect_s3_class(banner_data[["Results"]][["___total___"]], "BannerVar")
  expect_s3_class(banner_data[["Results"]][["age5"]], "BannerVar")
  expect_named(banner_data[["Results"]][["___total___"]], c("alias", "name", "type", "old_categories", "categories_out", "categories"))
  expect_named(banner_data[["Results"]][["age5"]], c("alias", "name", "type", "old_categories", "categories_out", "categories"))
  expect_identical(banner_data[["Results"]][["age5"]][["alias"]], "age5")
  expect_identical(banner_data[["Results"]][["age5"]][["name"]], "Age 5")
  expect_identical(banner_data[["Results"]][["age5"]][["old_categories"]], c("16 to 24", "25 to 34", "35 to 44", "45 to 54", "55+"))
  expect_identical(banner_data[["Results"]][["age5"]][["categories_out"]], c("16 to 24", "25 to 34", "35 to 44", "45 to 54", "55+"))
  expect_identical(banner_data[["Results"]][["age5"]][["categories"]], c("16 to 24", "25 to 34", "35 to 44", "45 to 54", "55+"))
})


test_that("Single banner with two variables", {
  banner_data <- with_mock(subvariables = subvariables_over, categories = categories_over, banner(ds, vars = list(Results = c("age5", "art3"))))
  expect_s3_class(banner_data, "Banner")
  expect_length(banner_data, 1)
  expect_named(banner_data, "Results")
  expect_is(banner_data[["Results"]], "list")
  expect_length(banner_data[["Results"]], 3)
  expect_named(banner_data[["Results"]], c("___total___", "age5", "art3"))
  expect_s3_class(banner_data[["Results"]][["___total___"]], "BannerVar")
  expect_s3_class(banner_data[["Results"]][["age5"]], "BannerVar")
  expect_s3_class(banner_data[["Results"]][["art3"]], "BannerVar")
  expect_named(banner_data[["Results"]][["___total___"]], c("alias", "name", "type", "old_categories", "categories_out", "categories"))
  expect_named(banner_data[["Results"]][["age5"]], c("alias", "name", "type", "old_categories", "categories_out", "categories"))
  expect_named(banner_data[["Results"]][["art3"]], c("alias", "name", "type", "old_categories", "categories_out", "categories"))
  expect_identical(banner_data[["Results"]][["age5"]][["alias"]], "age5")
  expect_identical(banner_data[["Results"]][["age5"]][["name"]], "Age 5")
  expect_identical(banner_data[["Results"]][["art3"]][["name"]], "Art Forms Enjoyed")
  expect_identical(banner_data[["Results"]][["age5"]][["old_categories"]], c("16 to 24", "25 to 34", "35 to 44", "45 to 54", "55+"))
  expect_identical(banner_data[["Results"]][["age5"]][["categories_out"]], c("16 to 24", "25 to 34", "35 to 44", "45 to 54", "55+"))
  expect_identical(banner_data[["Results"]][["age5"]][["categories"]], c("16 to 24", "25 to 34", "35 to 44", "45 to 54", "55+"))
  expect_identical(banner_data[["Results"]][["art3"]][["old_categories"]], c("Painting", "Sculpture", "Dance", "Music", "Poetry"))
  expect_identical(banner_data[["Results"]][["art3"]][["categories_out"]], c("Painting", "Sculpture", "Dance", "Music", "Poetry"))
  expect_identical(banner_data[["Results"]][["art3"]][["categories"]], c("Painting", "Sculpture", "Dance", "Music", "Poetry"))
})

test_that("Double banner, one variable in each subbanner", {
  banner_data <- with_mock(subvariables = subvariables_over, categories = categories_over, banner_data <- banner(ds, vars = list(Results1 = c("age5"), Results2 = c("art3"))))
  expect_s3_class(banner_data, "Banner")
  expect_length(banner_data, 2)
  expect_named(banner_data, c("Results1", "Results2"))
  expect_is(banner_data[["Results1"]], "list")
  expect_length(banner_data[["Results1"]], 2)
  expect_named(banner_data[["Results1"]], c("___total___", "age5"))
  expect_named(banner_data[["Results2"]], c("___total___", "art3"))
})


test_that("Single banner with one variable, recodes - categories rename", {
  banner_data <- with_mock(subvariables = subvariables_over, categories = categories_over, banner(ds, vars = list(Results = c("age5")),
                        recodes = list(age5 = list("16 to 24" = "Under 25", "55+" = "Over 54"))))
  expect_identical(banner_data[["Results"]][["age5"]][["old_categories"]], c("16 to 24", "25 to 34", "35 to 44", "45 to 54", "55+"))
  expect_identical(banner_data[["Results"]][["age5"]][["categories_out"]], c("Under 25", "25 to 34", "35 to 44", "45 to 54", "Over 54"))
  expect_identical(banner_data[["Results"]][["age5"]][["categories"]], c("Under 25", "25 to 34", "35 to 44", "45 to 54", "Over 54"))
})


test_that("Single banner with one variable, recodes - categories rename, hiding", {
  banner_data <- with_mock(subvariables = subvariables_over, categories = categories_over, banner(ds, vars = list(Results = c("age5")),
                        recodes = list(age5 = list("16 to 24" = "Under 25", "45 to 54" = NA, "55+" = NA))))
  expect_identical(banner_data[["Results"]][["age5"]][["old_categories"]], c("16 to 24", "25 to 34", "35 to 44", "45 to 54", "55+"))
  expect_identical(banner_data[["Results"]][["age5"]][["categories_out"]], c("Under 25", "25 to 34", "35 to 44", NA, NA))
  expect_identical(banner_data[["Results"]][["age5"]][["categories"]], c("Under 25", "25 to 34", "35 to 44"))
})

test_that("Single banner with one variable, label change", {
  banner_data <-  with_mock(subvariables = subvariables_over, categories = categories_over, banner(ds, vars = list(Results = c("age5")), labels = c(age5 = "Age (5 categories)")))
  expect_identical(banner_data[["Results"]][["age5"]][["name"]], "Age (5 categories)")
})

test_that("Single banner with one variable, recodes - categories rename, else", {
  banner_data <- with_mock(subvariables = subvariables_over, categories = categories_over, banner(ds, vars = list(Results = c("age5")),
                        recodes = list(age5 = list("16 to 24" = "Under 25", "55+" = "Over 54", .default = NA))))
  expect_identical(banner_data[["Results"]][["age5"]][["old_categories"]], c("16 to 24", "25 to 34", "35 to 44", "45 to 54", "55+"))
  expect_identical(banner_data[["Results"]][["age5"]][["categories_out"]], c("Under 25", NA, NA, NA, "Over 54"))
  expect_identical(banner_data[["Results"]][["age5"]][["categories"]], c("Under 25", "Over 54"))
})

context("getBannerInfo")

test_that("Returns default banner", {
  expect_equal(getBannerInfo(NULL), default_banner)
})

context("removeInserts")

test_that("Adjustments for subtotals", {
  var <- list()
  theme <- list()
  theme$format_subtotals <- NULL
  theme$format_headers <- NULL
  var$inserts_obj <- list()
  var$inserts_obj$test <- "Fake Object of class Subtotal"
  class(var$inserts_obj$test) <- "Subtotal"
  var$inserts_obj$other <- "Fake Object of class Headers"
  class(var$inserts_obj$other) <- "Headers"
  
  expect_equal(
    removeInserts(var, theme),
    list(inserts_obj = structure(list(), .Names = character(0)), 
       inserts = structure(list(), .Names = character(0)))
  )
})
