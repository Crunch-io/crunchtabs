context("Banner object creation")

test_that("Error handling - not a dataset", {
  expect_error(banner(list()),
               "'dataset' is not an object of class 'CrunchDataset'.")
})

with_mock_crunch({
  ds <- loadDataset("https://app.crunch.io/api/datasets/85f/")
  test_that("Error handling - vars", {
    expect_error(banner(ds, 'a'),
                 "'vars' should be a list of vectors.")
    expect_error(banner(ds, c('a', 'b')),
                 "'vars' should be a list of vectors.")
    expect_error(banner(ds, list(Results = c('a', 'b'))),
                 "Variables: a, b not found.")
    expect_error(banner(ds, list(Results = c('a', 'petloc', 'b'))),
                 "Variables: a, b not found.")
    expect_error(banner(ds, list(Results = c('allpets', 'a', 'petloc', 'b', 'ndogs'))),
                 "Variables: a, b not found.")
    expect_error(banner(ds, list(Results = c('ndogs'))),
                 "All banner variables have to be categorical or multiple_response. This is not true for: ndogs")
    expect_error(banner(ds, list(Results = c('ndogs', 'petloc', 'petname', 'age'))),
                 "All banner variables have to be categorical or multiple_response. This is not true for: ndogs, petloc, petname, age")
    expect_error(banner(ds, list(Results = c('allpets', 'ndogs', 'petname', 'age2'))),
                 "All banner variables have to be categorical or multiple_response. This is not true for: ndogs, petname")
    expect_error(banner(ds, list(Results = c('allpets')), labels = c(allpets1 = 'All Pets')),
                 "Aliases used in 'labels' not in 'vars': allpets1")
    expect_error(banner(ds, list(Results = c('allpets', 'age2')), labels = c(allpets1 = 'All Pets', age2 = "Age (2 categories")),
                 "Aliases used in 'labels' not in 'vars': allpets1")
    expect_error(banner(ds, list(Results = c('favpet')), recodes = list(favpet1 = "'Dog' = 'Doggy'")),
                 "Aliases used in 'recodes' not in 'vars': favpet1")
    expect_error(banner(ds, list(Results = c('favpet', 'age2', 'age3')), recodes = list(age2 = "'16 to 34' = '<= 34", favpet1 = "'Dog' = 'Doggy'", age3 = "'16 to 34' = '<= 34")),
                 "Aliases used in 'recodes' not in 'vars': favpet1")
    expect_error(banner(ds, list(Results = c('gender')), recodes = list(gender = "'Male2' = 'Male1'")),
                 "No category with name 'Male2' in 'gender'")
    expect_error(banner(ds, list(Results = c('gender')), recodes = list(gender = "'Male = 'Male1'")),
                 "\n  in recode term: 'Male = 'Male1'\n  message: Error in parse(text = strsplit(term, "=")[[1]][1]) : \n  <text>:1:1: unexpected INCOMPLETE_STRING\n1: 'Male \n    ^\n")
  })

  banner_data <- banner(ds, vars = list(Results = c('age5')))
  test_that("Single banner with single variable", {
    expect_s3_class(banner_data, "Banner")
    expect_length(banner_data, 1)
    expect_named(banner_data, "Results")
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

  banner_data <- banner(ds, vars = list(Results = c('age5', 'allpets')))
  test_that("Single banner with two variables", {
    expect_s3_class(banner_data, "Banner")
    expect_length(banner_data, 1)
    expect_named(banner_data, "Results")
    expect_is(banner_data[["Results"]], "list")
    expect_length(banner_data[["Results"]], 3)
    expect_named(banner_data[["Results"]], c("___total___", "age5", "allpets"))
    expect_s3_class(banner_data[["Results"]][["___total___"]], "BannerVar")
    expect_s3_class(banner_data[["Results"]][["age5"]], "BannerVar")
    expect_s3_class(banner_data[["Results"]][["allpets"]], "BannerVar")
    expect_named(banner_data[["Results"]][["___total___"]], c("alias", "name", "type", "old_categories", "categories_out", "categories"))
    expect_named(banner_data[["Results"]][["age5"]], c("alias", "name", "type", "old_categories", "categories_out", "categories"))
    expect_named(banner_data[["Results"]][["allpets"]], c("alias", "name", "type", "old_categories", "categories_out", "categories"))
    expect_identical(banner_data[["Results"]][["age5"]][["alias"]], "age5")
    expect_identical(banner_data[["Results"]][["age5"]][["name"]], "Age 5")
    expect_identical(banner_data[["Results"]][["allpets"]][["name"]], "All pets owned")
    expect_identical(banner_data[["Results"]][["age5"]][["old_categories"]], c("16 to 24", "25 to 34", "35 to 44", "45 to 54", "55+"))
    expect_identical(banner_data[["Results"]][["age5"]][["categories_out"]], c("16 to 24", "25 to 34", "35 to 44", "45 to 54", "55+"))
    expect_identical(banner_data[["Results"]][["age5"]][["categories"]], c("16 to 24", "25 to 34", "35 to 44", "45 to 54", "55+"))
    expect_identical(banner_data[["Results"]][["allpets"]][["old_categories"]], c("Cat", "Dog", "Bird"))
    expect_identical(banner_data[["Results"]][["allpets"]][["categories_out"]], c("Cat", "Dog", "Bird"))
    expect_identical(banner_data[["Results"]][["allpets"]][["categories"]], c("Cat", "Dog", "Bird"))
  })

  banner_data <- banner(ds, vars = list(Results1 = c('age5'), Results2 = c('allpets')))
  test_that("Double banner, one variable in each subbanner", {
    expect_s3_class(banner_data, "Banner")
    expect_length(banner_data, 2)
    expect_named(banner_data, c("Results1", "Results2"))
    expect_is(banner_data[["Results1"]], "list")
    expect_length(banner_data[["Results1"]], 2)
    expect_named(banner_data[["Results1"]], c("___total___", "age5"))
    expect_named(banner_data[["Results2"]], c("___total___", "allpets"))
  })

  banner_data <- banner(ds, vars = list(Results = c('age5')), labels = c(age5 = 'Age (5 categories)'))
  test_that("Single banner with one variable, label change", {
    expect_identical(banner_data[["Results"]][["age5"]][["name"]], "Age (5 categories)")
  })

  banner_data <- banner(ds, vars = list(Results = c('age5')),
                        recodes = list(age5 = "'16 to 24' = 'Under 25'; '55+' = 'Over 54'"))
  test_that("Single banner with one variable, recodes - categories rename", {
    expect_identical(banner_data[["Results"]][["age5"]][["old_categories"]], c("16 to 24", "25 to 34", "35 to 44", "45 to 54", "55+"))
    expect_identical(banner_data[["Results"]][["age5"]][["categories_out"]], c("Under 25", "25 to 34", "35 to 44", "45 to 54", "Over 54"))
    expect_identical(banner_data[["Results"]][["age5"]][["categories"]], c("Under 25", "25 to 34", "35 to 44", "45 to 54", "Over 54"))
  })

  banner_data <- banner(ds, vars = list(Results = c('age5')),
                        recodes = list(age5 = "'16 to 24' = 'Under 25'; '45 to 54' = NA; '55+' = NA"))
  test_that("Single banner with one variable, recodes - categories rename, hiding", {
    expect_identical(banner_data[["Results"]][["age5"]][["old_categories"]], c("16 to 24", "25 to 34", "35 to 44", "45 to 54", "55+"))
    expect_identical(banner_data[["Results"]][["age5"]][["categories_out"]], c("Under 25", "25 to 34", "35 to 44", NA, NA))
    expect_identical(banner_data[["Results"]][["age5"]][["categories"]], c("Under 25", "25 to 34", "35 to 44"))
  })

  banner_data <- banner(ds, vars = list(Results = c('age5')),
                        recodes = list(age5 = "'16 to 24' = 'Under 25'; '55+' = 'Over 54'; else = NA"))
  test_that("Single banner with one variable, recodes - categories rename, else", {
    expect_identical(banner_data[["Results"]][["age5"]][["old_categories"]], c("16 to 24", "25 to 34", "35 to 44", "45 to 54", "55+"))
    expect_identical(banner_data[["Results"]][["age5"]][["categories_out"]], c("Under 25", NA, NA, NA, "Over 54"))
    expect_identical(banner_data[["Results"]][["age5"]][["categories"]], c("Under 25", "Over 54"))
  })
})
