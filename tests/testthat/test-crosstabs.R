context("Generating Toplines and Crosstabs data summaries")

ds = readRDS("fixtures/banner_ds.rds")

test_that("Error handling - not a dataset", {
    expect_error(crosstabs(NULL),
        "The expected class for `dataset` is CrunchDataset, not NULL.")
})

test_that("Error handling", {
  expect_error(crosstabs(ds, vars = c("a", "b")),
               "Variables in `vars` must be valid aliases in aliases(allVariables(dataset)). This is not true for 'a' and 'b'.",
               fixed = TRUE)
  # expect_error(crosstabs(ds, weight = "a_weight"),
  #              "`weight`, if provided, must be a valid variable in `dataset`. 'a_weight' is not found.")
  expect_error(with_mock(weightVariables = function(x) "weight", crosstabs(ds, weight = "age")),
               "`weight`, if provided, must be a valid weight variable in `dataset`. 'age' is not a weight variable.")

})

with_test_authentication({
    ds <- loadDataset("https://app.crunch.io/api/datasets/868e8b3e01834c45b73e56e80160d3c3/")

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


    test_that("Errors requiring auth", {

      expect_error(crosstabs(ds, banner = list()),
                   "`banner`, if provided, must be an object of class 'Banner'.")
    })

    test_that("Toplines", {
        crosstabs_data <- crosstabs(ds, vars = c("art3", "books1", "movies1"), weight = NULL)
        expect_s3_class(crosstabs_data, "Toplines")
        expect_named(crosstabs_data, c("metadata", "results", "banner"))
        expect_named(crosstabs_data$metadata, c("title", "weight", "start_date", "end_date", "description"))
        expect_null(crosstabs_data$banner)
        expect_identical(getName(crosstabs_data), "YouGov crunchtabs_example monkeys (starttime: from 2018-06-19)")
        expect_null(crosstabs_data$metadata$weight)
        expect_named(crosstabs_data$results, c("art3", "books1", "movies1"))
        expect_named(crosstabs_data$results$art3, c("type", "name", "description", "notes", "no_totals", "mean_median", "subnames", "categories", "inserts_obj", "alias", "subnumber", "number", "crosstabs"))
        expect_identical(crosstabs_data$results$art3$alias, "art3")
        expect_identical(crosstabs_data$results$art3$name, "Art Forms Enjoyed")
        expect_identical(crosstabs_data$results$art3$type, "multiple_response")
        expect_identical(crosstabs_data$results$art3$description, "Which of these forms of art do you like? Choose all that apply.")
        expect_identical(crosstabs_data$results$art3$notes, "")
        x <- matrix(c(317, 307, 313, 317, 311), ncol = 1,
            dimnames = list(c("Painting", "Sculpture", "Dance", "Music", "Poetry"), "Total"))
        expect_identical(crosstabs_data$results$art3$crosstabs$Results$`___total___`$counts, x)
        x <- matrix(c(0.5283333, 0.5116667, 0.5216667, 0.5283333, 0.5183333), ncol = 1,
            dimnames = list(c("Painting", "Sculpture", "Dance", "Music", "Poetry"), "Total"))
        expect_equal(crosstabs_data$results$art3$crosstabs$Results$`___total___`$proportions, x, tolerance = 1e-7)
        expect_equal(crosstabs_data$results$art3$crosstabs$Results$`___total___`$base[1], 600)
    })

    banner_data <- banner(ds, vars = list(c("profile_gender", "age5")))
    tabBook_vars <- c("age", "age4", "profile_socialgrade_cie", "profile_ethnicity",
        "sports1", "books1", "books2", "books3_book", "books4", "books5", "movies1",
        "movies4", "movies5", "tv1", "tv2", "tv3", "tv4", "tv5", "art1_a", "art2",
        "art3", "art4", "art5", "art5_nonUniform", "media1_a", "media1_b", "misc1_a",
        "misc2", "misc2_dk", "misc3_a", "misc3_b", "misc3_c", "misc3_d", "misc3_e")

    crosstabs_data <- crosstabs(ds, weight = NULL, vars = tabBook_vars, banner = banner_data)
    test_that("Crosstabs", {
        expect_true("c71d234541716d7ee9be13f1c97deade" %in% names(multitables(ds)))
        expect_s3_class(crosstabs_data, "Crosstabs")
        expect_named(crosstabs_data, c("metadata", "results", "banner"))
        expect_named(crosstabs_data$metadata, c("title", "weight", "start_date", "end_date", "description"))
        expect_s3_class(crosstabs_data$banner, "Banner") # NOTE: check expectation
        expect_identical(getName(crosstabs_data), "YouGov crunchtabs_example monkeys (starttime: from 2018-06-19)")
        expect_null(crosstabs_data$metadata$weight)
        expect_named(crosstabs_data$results, c("age", "age4", "profile_socialgrade_cie",
            "profile_ethnicity", "sports1", "books1", "books2", "books3_book", "books4",
            "books5_a", "books5_b", "books5_c", "books5_d", "books5_e", "movies1_a",
            "movies1_b", "movies1_c", "movies1_d", "movies4", "movies5", "tv1", "tv2",
            "tv3", "tv4_a", "tv5_a", "art1_a", "art2", "art3", "art4_1", "art4_2",
            "art4_3", "art4_4", "art4_5", "art5", "art5_nonUniform", "media1_a",
            "media1_b", "misc1_a", "misc2_1", "misc2_2", "misc2_3", "misc2_4",
            "misc2_5", "misc2_1#", "misc2_2#", "misc2_3#", "misc2_4#", "misc2_5#",
            "misc3_a", "misc3_b", "misc3_c", "misc3_d", "misc3_e"))
        expect_named(crosstabs_data$results$art2, c("alias", "name", "description",
            "notes", "type", "no_totals", "mean_median", "subnames", "categories",
            "inserts_obj", "subnumber", "number", "crosstabs"))
        expect_identical(crosstabs_data$results$art2$alias, "art2")
        expect_identical(crosstabs_data$results$art2$name, "Favorite Art Form")
        expect_identical(crosstabs_data$results$art2$description, "Which of these forms of art do you like the most?")
        expect_identical(crosstabs_data$results$art2$notes, "")
        expect_named(crosstabs_data$results$art2$crosstabs, c("Banner1"))
    })
})

