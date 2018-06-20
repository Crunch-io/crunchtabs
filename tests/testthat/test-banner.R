context("Banner object creation")

test_that("Error handling - not a dataset", {
    expect_error(banner(list()),
        "The expected class for `dataset` is CrunchDataset, not list.")
})

with_test_authentication({
    ds <- loadDataset("https://app.crunch.io/api/datasets/868e8b3e01834c45b73e56e80160d3c3/")
    # "allpets"="art3", "petloc"="tv4", "ndogs"="movies5", "age2"="age4", "favpet"="art2",
    # "petname"="movies3"
    test_that("Error handling - banner", {
        expect_error(banner(ds, "a"),
            "`vars` must be a list of vectors.")
        expect_error(banner(ds, c("a", "b")),
            "`vars` must be a list of vectors.")
        expect_error(banner(ds, list(c())),
            "`vars` must have a length greater than 0.")
        expect_warning(banner(ds, list(c(), "A"="art3")),
            "No variables found in 'Banner1' in `vars`. 'Banner1' will be ignored.")
        expect_error(banner(ds, list(Results = c("a", "b"))),
            "Variables in `vars` must be valid aliases in aliases(allVariables(dataset)). This is not true for 'a' and 'b'.")
        expect_error(banner(ds, list(Results = c("art3", "a", "tv4", "b", "movies5"))),
            "Variables in `vars` must be valid aliases in aliases(allVariables(dataset)). This is not true for 'a' and 'b'.")
        expect_error(banner(ds, list(Results = c("art3")), labels = c(allpets1 = "All Pets")),
            "Variables in `labels` must be included in `vars`. This is not true for 'allpets1'.")
        expect_error(banner(ds, list(Results = c("art3", "age4")), labels = c(allpets1 = "All Pets", age4 = "Age (2 categories")),
            "Variables in `labels` must be included in `vars`. This is not true for 'allpets1'.")
        expect_error(banner(ds, list(Results = c("art2")), recodes = list(favpet1 = list("Dog" = "Doggy"))),
            "Variables in `recodes` must be included in `vars`. This is not true for 'favpet1'.")
        expect_error(banner(ds, list(Results = c("art2", "age4")), recodes = list(age4 = list("16 to 34" = "<= 34"), favpet1 = list("Dog" = "Doggy"))),
            "Variables in `recodes` must be included in `vars`. This is not true for 'favpet1'.")
        expect_error(banner(ds, list(Results = c("movies5"))),
            "Variables in `vars` must be of type categorical or multiple_response. This is not true for 'movies5'.")
        expect_error(banner(ds, list(Results = c("movies5", "tv4", "movies3", "age"))),
            "Variables in `vars` must be of type categorical or multiple_response. This is not true for 'movies5', 'tv4', 'movies3', and 'age'.")
        expect_error(banner(ds, list(Results = c("art3", "movies5", "movies3", "age4"))),
            "Variables in `vars` must be of type categorical or multiple_response. This is not true for 'movies5' and 'movies3'.")
        expect_error(banner(ds, list(Results = c("profile_gender")), recodes = list(profile_gender = list("Male2"="Man"))),
            "Responses in `recodes` must be included in variable responses. This is not true for 'Male2' in 'profile_gender'.")
        expect_error(banner(ds, list(Results = c("profile_gender")), recodes = list(profile_gender = list("Male"="Man", "Female"="Man"))),
            "Combining categories is not currently supported. Please check 'profile_gender' recodes.")
    })
    
    banner_data <- banner(ds, vars = list(Results = c("age4")))
    test_that("Single banner with single variable", {
        expect_s3_class(banner_data, "Banner")
        expect_length(banner_data, 1)
        expect_named(banner_data, "Results")
        expect_is(banner_data, "Banner")
        expect_is(banner_data[["Results"]], "list")
        expect_length(banner_data[["Results"]], 2)
        expect_named(banner_data[["Results"]], c("___total___", "age4"))
        expect_s3_class(banner_data[["Results"]][["___total___"]], "BannerVar")
        expect_s3_class(banner_data[["Results"]][["age4"]], "BannerVar")
        expect_named(banner_data[["Results"]][["___total___"]], c("alias", "name", "type", "old_categories", "categories_out", "categories"))
        expect_named(banner_data[["Results"]][["age4"]], c("alias", "name", "type", "old_categories", "categories_out", "categories"))
        expect_identical(banner_data[["Results"]][["age4"]][["alias"]], "age4")
        expect_identical(banner_data[["Results"]][["age4"]][["name"]], "Age (4 categories)")
        expect_identical(banner_data[["Results"]][["age4"]][["old_categories"]], c("16-24", "25-39", "40-54", "55+"))
        expect_identical(banner_data[["Results"]][["age4"]][["categories_out"]], c("16-24", "25-39", "40-54", "55+"))
        expect_identical(banner_data[["Results"]][["age4"]][["categories"]], c("16-24", "25-39", "40-54", "55+"))
    })
    
    banner_data <- banner(ds, vars = list(Results = c("age5", "art3")))
    test_that("Single banner with two variables", {
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
        expect_identical(banner_data[["Results"]][["art3"]][["old_categories"]], c('Painting', 'Sculpture', 'Dance', 'Music', 'Poetry'))
        expect_identical(banner_data[["Results"]][["art3"]][["categories_out"]], c('Painting', 'Sculpture', 'Dance', 'Music', 'Poetry'))
        expect_identical(banner_data[["Results"]][["art3"]][["categories"]], c('Painting', 'Sculpture', 'Dance', 'Music', 'Poetry'))
    })
    
    banner_data <- banner(ds, vars = list(Results1 = c("age5"), Results2 = c("art3")))
    test_that("Double banner, one variable in each subbanner", {
        expect_s3_class(banner_data, "Banner")
        expect_length(banner_data, 2)
        expect_named(banner_data, c("Results1", "Results2"))
        expect_is(banner_data[["Results1"]], "list")
        expect_length(banner_data[["Results1"]], 2)
        expect_named(banner_data[["Results1"]], c("___total___", "age5"))
        expect_named(banner_data[["Results2"]], c("___total___", "art3"))
    })
    
    banner_data <- banner(ds, vars = list(Results = c("age5")), labels = c(age5 = "Age (5 categories)"))
    test_that("Single banner with one variable, label change", {
        expect_identical(banner_data[["Results"]][["age5"]][["name"]], "Age (5 categories)")
    })
    
    banner_data <- banner(ds, vars = list(Results = c("age5")),
        recodes = list(age5 = list('16 to 24' = 'Under 25', '55+' = 'Over 54')))
    test_that("Single banner with one variable, recodes - categories rename", {
        expect_identical(banner_data[["Results"]][["age5"]][["old_categories"]], c("16 to 24", "25 to 34", "35 to 44", "45 to 54", "55+"))
        expect_identical(banner_data[["Results"]][["age5"]][["categories_out"]], c("Under 25", "25 to 34", "35 to 44", "45 to 54", "Over 54"))
        expect_identical(banner_data[["Results"]][["age5"]][["categories"]], c("Under 25", "25 to 34", "35 to 44", "45 to 54", "Over 54"))
    })
    
    banner_data <- banner(ds, vars = list(Results = c('age5')),
        recodes = list(age5 = list('16 to 24' = 'Under 25', '45 to 54' = NA, '55+' = NA)))
    test_that("Single banner with one variable, recodes - categories rename, hiding", {
        expect_identical(banner_data[["Results"]][["age5"]][["old_categories"]], c("16 to 24", "25 to 34", "35 to 44", "45 to 54", "55+"))
        expect_identical(banner_data[["Results"]][["age5"]][["categories_out"]], c("Under 25", "25 to 34", "35 to 44", NA, NA))
        expect_identical(banner_data[["Results"]][["age5"]][["categories"]], c("Under 25", "25 to 34", "35 to 44"))
    })
    
    banner_data <- banner(ds, vars = list(Results = c('age5')),
        recodes = list(age5 = list('16 to 24' = 'Under 25', '55+' = 'Over 54', .default = NA)))
    test_that("Single banner with one variable, recodes - categories rename, else", {
        expect_identical(banner_data[["Results"]][["age5"]][["old_categories"]], c("16 to 24", "25 to 34", "35 to 44", "45 to 54", "55+"))
        expect_identical(banner_data[["Results"]][["age5"]][["categories_out"]], c("Under 25", NA, NA, NA, "Over 54"))
        expect_identical(banner_data[["Results"]][["age5"]][["categories"]], c("Under 25", "Over 54"))
    })
})
