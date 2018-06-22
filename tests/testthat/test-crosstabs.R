context("Generating Toplines and Crosstabs data summaries")

test_that("Error handling - not a dataset", {
  expect_error(crosstabs(NULL),
               "The expected class for `dataset` is CrunchDataset, not NULL.")
})

with_test_authentication({
    ds <- loadDataset("https://app.crunch.io/api/datasets/868e8b3e01834c45b73e56e80160d3c3/")
    
    test_that("Error handling", {
        expect_error(crosstabs(ds, vars = c("a", "b")),
                     "Variables in `vars` must be valid aliases in aliases(allVariables(dataset)). This is not true for 'a' and 'b'.",
            fixed = TRUE)
        expect_error(crosstabs(ds, weight = "a_weight"),
                     "`weight`, if provided, must be a valid variable in `dataset`. 'a_weight' is not found.")
        expect_error(crosstabs(ds, weight = "age"),
                     "`weight`, if provided, must be a valid weight variable in `dataset`. 'age' is not a weight variable.")
        expect_error(crosstabs(ds, banner = list()),
                     "`banner`, if provided, must be an object of class 'Banner'.")
    })

    test_that("Warnings", {
        expect_warning(crosstabs(ds, vars = c("movies2_a_1", "movies2_a_2", "books1")),
                       "`vars` of type(s) text are not supported and have been skipped.", fixed = TRUE)
        expect_warning(crosstabs(ds, vars = c("books1", "starttime")),
                       "`vars` of type(s) datetime are not supported and have been skipped.", fixed = TRUE)
        expect_warning(crosstabs(ds, vars = c("books1", "starttime", "movies2_a_1")),
            "`vars` of type(s) datetime and text are not supported and have been skipped.", fixed = TRUE)
        expect_warning(crosstabs(ds, vars = c("books1", "starttime", "endtime", "movies2_a_1")),
            "`vars` of type(s) datetime and text are not supported and have been skipped.",
            fixed = TRUE)
    })

    test_that("Toplines", {
        crosstabs_data <- crosstabs(ds, vars = c("art3", "books1", "movies1"), weight = NULL)
        expect_s3_class(crosstabs_data, "Toplines")
        expect_named(crosstabs_data, c("metadata", "results", "banner"))
        expect_named(crosstabs_data$metadata, c("title", "weight"))
        expect_null(crosstabs_data$banner)
        expect_identical(getName(crosstabs_data), "YouGov crunchtabs_example monkeys (starttime: from 2018-06-19)")
        expect_null(crosstabs_data$metadata$weight)
        expect_named(crosstabs_data$results, c("art3", "books1", "movies1"))
        expect_named(crosstabs_data$results$art3, c('type', 'name', 'description', 'notes', 'no_totals', 'mean_median', 'subnames', 'categories', 'inserts_obj', 'alias', 'subnumber', 'number', 'crosstabs'))
        expect_identical(crosstabs_data$results$art3$alias, "art3")
        expect_identical(crosstabs_data$results$art3$name, "Art Forms Enjoyed")
        expect_identical(crosstabs_data$results$art3$type, "multiple_response")
        expect_identical(crosstabs_data$results$art3$description, "Which of these forms of art do you like? Choose all that apply.")
        expect_identical(crosstabs_data$results$art3$notes, "")
        x <- matrix(c(317, 307, 313, 317, 311), ncol = 1, 
            dimnames = list(c('Painting', 'Sculpture', 'Dance', 'Music', 'Poetry'), "Total"))
        expect_identical(crosstabs_data$results$art3$crosstabs$Results$`___total___`$counts, x)
        x <- matrix(c(0.5283333, 0.5116667, 0.5216667, 0.5283333, 0.5183333), ncol = 1, 
            dimnames = list(c('Painting', 'Sculpture', 'Dance', 'Music', 'Poetry'), "Total"))
        expect_equal(crosstabs_data$results$art3$crosstabs$Results$`___total___`$proportions, x, tolerance = 1e-7)
        expect_equal(crosstabs_data$results$art3$crosstabs$Results$`___total___`$base[1], 600)
    })
  
    banner_data <- banner(ds, vars=list(c('profile_gender', 'age5')))
    tabBook_vars <- c("allpets", "favpet", "petloc", "ndogs", "ndogs_a", "ndogs_b", "country", "age", "age2", "age3", "age5", "gender", "weight", "noweight")
  
    crosstabs_data <- crosstabs(ds, vars = tabBook_vars, banner = banner_data)
    test_that("Crosstabs", {
        expect_s3_class(crosstabs_data, "Crosstabs")
        expect_named(crosstabs_data, c("metadata", "results", "banner"))
        expect_named(crosstabs_data$metadata, c("title", "weight"))
        expect_s3_class(crosstabs_data$banner, "Banner") # NOTE: check expectation
        expect_identical(getName(crosstabs_data), "Testing Dataset")
        expect_null(crosstabs_data$metadata$weight)
        expect_named(crosstabs_data$results, c("allpets", "favpet", "petloc_home", "petloc_work", "ndogs", "ndogs_a", "ndogs_b", "country", "age", "age2", "age3", "age5", "gender", "weight", "noweight"))
        # expect_named(crosstabs_data$results$favpet, c("alias", "name", "subnames", "description", "notes", "settings", "inserts", "crosstabs"))
        expect_identical(crosstabs_data$results$favpet$alias, "favpet")
        expect_identical(crosstabs_data$results$favpet$name, "Favorite pet")
        expect_identical(crosstabs_data$results$favpet$description, "What is your favorite pet?")
        expect_identical(crosstabs_data$results$favpet$notes, NULL)
        expect_named(crosstabs_data$results$favpet$crosstabs, c("Banner1"))
    })
})

