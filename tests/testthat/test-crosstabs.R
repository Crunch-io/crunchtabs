context("Generating Toplines and Crosstabs data summaries")

ds = readRDS("fixtures/banner_ds_new.rds")
# This fixture is from:
# https://app.crunch.io/datasets/868e8b3e01834c45b73e56e80160d3c3/

test_that("Error handling - not a dataset", {
    expect_error(crosstabs(NULL),
        "The expected class for `dataset` is CrunchDataset, not NULL.")
})

test_that("Error handling", {
  expect_error(crosstabs(ds, vars = c("a", "b")),
               "Variables in `vars` must be valid aliases in aliases(allVariables(dataset)). This is not true for 'a' and 'b'.",
               fixed = TRUE)
  expect_error(crosstabs(ds, weight = "a_weight"),
               "`weight`, if provided, must be a valid variable in `dataset`. 'a_weight' is not found.")
  expect_error(with_mock(weightVariables = function(x) "weight", crosstabs(ds, weight = "age")),
               "`weight`, if provided, must be a valid weight variable in `dataset`. 'age' is not a weight variable.")

})

with_api_fixture("fixtures-1-2-5", {
  ds <- loadDataset("https://app.crunch.io/api/datasets/868e8b/")
    # ds <- loadDataset("https://app.crunch.io/api/datasets/868e8b3e01834c45b73e56e80160d3c3/")

    # test_that("Warnings", {
    #   expect_warning(crosstabs(ds, vars = c("movies2_a_1", "movies2_a_2", "books1")),
    #                  "`vars` of type(s) text are not supported and have been skipped.", fixed = TRUE)
    #   expect_warning(crosstabs(ds, vars = c("books1", "starttime")),
    #                  "`vars` of type(s) datetime are not supported and have been skipped.", fixed = TRUE)
    #   expect_warning(crosstabs(ds, vars = c("books1", "starttime", "movies2_a_1")),
    #                  "`vars` of type(s) datetime and text are not supported and have been skipped.", fixed = TRUE)
    #   expect_warning(crosstabs(ds, vars = c("books1", "starttime", "endtime", "movies2_a_1")),
    #                  "`vars` of type(s) datetime and text are not supported and have been skipped.",
    #                  fixed = TRUE)
    # })


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
        expect_named(crosstabs_data$results$art3, c("alias", "name", "description", "notes","type", "no_totals", "mean_median", "subnames", "categories", "inserts_obj", "subnumber", "number", "crosstabs"))
        expect_identical(crosstabs_data$results$art3$alias, "art3")
        expect_identical(crosstabs_data$results$art3$name, "Art Forms Enjoyed")
        expect_identical(crosstabs_data$results$art3$type, "multiple_response")
        expect_identical(crosstabs_data$results$art3$description, "Which of these forms of art do you like? Choose all that apply.")
        expect_identical(crosstabs_data$results$art3$notes, "")
        x <- matrix(c(317, 307, 313, 317, 311), ncol = 1,
            dimnames = list(c("Painting", "Sculpture", "Dance", "Music", "Poetry"), "Total"))
        expect_identical(crosstabs_data$results$art3$crosstabs$Results$`___total___`$counts, x)
        x <- structure(c(0.528333333333333, 0.511666666666667, 0.521666666666667, 0.528333333333333, 0.518333333333333), class = c("CrunchCubeCalculation", "array"), dims = new("CubeDims", .Data = list(list(name = c("Painting", "Sculpture", "Dance", "Music", "Poetry"), missing = c(FALSE, FALSE, FALSE, FALSE, FALSE), references = list(alias = "art3",description = "Which of these forms of art do you like? Choose all that apply.", name = "Art Forms Enjoyed", subreferences = list(art3_1 = list(alias = "art3_1", name = "Painting"), art3_2 = list(alias = "art3_2", name = "Sculpture"), art3_3 = list(alias = "art3_3", name = "Dance"), art3_4 = list(alias = "art3_4", name = "Music"), art3_5 = list(alias = "art3_5", name = "Poetry")), type = "subvariable_items", subvariables = c("art3_1/", "art3_2/", "art3_3/", "art3_4/", "art3_5/"))), list(name = "", missing = FALSE, references = list(alias = "total", name = "Total", type = "enum"))), names = c("art3",
       "total")), type = "proportion", .Dim = c(5L, 1L), .Dimnames = list(
       c("Painting", "Sculpture", "Dance", "Music", "Poetry"), "Total"))
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
        expect_true("c71d234541716d7" %in% names(multitables(ds)))
        expect_s3_class(crosstabs_data, "Crosstabs")
        expect_named(crosstabs_data, c("metadata", "results", "banner"))
        expect_named(crosstabs_data$metadata, c("title", "weight", "start_date", "end_date", "description"))
        expect_s3_class(crosstabs_data$banner, "Banner") # NOTE: check expectation
        expect_identical(getName(crosstabs_data), "YouGov crunchtabs_example monkeys (starttime: from 2018-06-19)")
        expect_null(crosstabs_data$metadata$weight)
        expect_named(crosstabs_data$results,
                     c("age4", "profile_socialgrade_cie", "profile_ethnicity", "books1",
                       "books2", "books3_book", "books4", "books5_a", "books5_b", "books5_c",
                       "books5_d", "books5_e", "movies1_a", "movies1_b", "movies1_c",
                       "movies1_d", "movies4", "tv1", "tv2", "tv3", "tv4_a", "tv5_a",
                       "art1_a", "art2", "art3", "art4_1", "art4_2", "art4_3", "art4_4",
                       "art4_5", "art5", "art5_nonUniform", "media1_a", "media1_b",
                       "misc1_a", "misc2_1", "misc2_2", "misc2_3", "misc2_4", "misc2_5",
                       "misc2_1#", "misc2_2#", "misc2_3#", "misc2_4#", "misc2_5#"))
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

