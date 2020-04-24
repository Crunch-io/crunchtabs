context('Preparing a banner data summary')

# ds = readRDS("fixtures/testing-dataset.rds")
#
# test_that("We can load a dataset from the crunchtabs package", {
#   expect_s4_class(ds, "CrunchDataset")
#   expect_identical(name(ds), "Testing Dataset")
# })


with_test_authentication({
    ds <- loadDataset("Testing Dataset", project="crunchtabs") # Doesn't work anymore

    # NOTE: is this an expected behaviour?
    # banner_data <- unserializeJSON(readLines(con = file.path(fixtures_dir, "ds1_banner1.json")))
    # test_that("We can read in a Banner object from a file", {
    #   expect_s3_class(banner_data, "Banner")
    # })

    banner_data <- banner(ds, vars=list(c('gender', 'age5')))

    tabBook_vars <- c("allpets", "allpets2", "favpet", "petloc", "ndogs", "ndogs_a", "ndogs_b", "country", "age", "age2", "age3", "age5", "gender", "weight", "noweight")

    tabBooks_data <- tabBooks(dataset = ds, vars = tabBook_vars, banner = banner_data, weight = NULL)
    test_that("We can generate a tabBook data summary", {
        expect_is(tabBooks_data, "list")
        expect_named(tabBooks_data, c("allpets", "favpet", "petloc_home", "petloc_work", "ndogs", "ndogs_a", "ndogs_b", "country", "age", "age2", "age3", "age5", "gender", "weight", "noweight", "allpets2"))

        # expect_named(tabBooks_data$allpets, c('alias', 'name', 'subnames', 'description', 'notes', 'settings', 'inserts', 'crosstabs'))
        expect_named(tabBooks_data$allpets$crosstabs, c("Banner1"))
        expect_named(tabBooks_data$allpets$crosstabs$Banner1, c("Total", "gender", "age5"))
        expect_named(tabBooks_data$allpets$crosstabs$Banner1$Total, c("counts", "proportions", "totals_counts", "totals_proportions", "unweighted_n", "counts_unweighted", "pvals_col"))
        expect_identical(tabBooks_data$allpets$crosstabs$Banner1$Total$counts,
            structure(c(4,7,5), .Dim = c(3,1), .Dimnames = list(allpets = c("Cat", "Dog", "Bird"), total = c("Total"))))
        expect_equal(tabBooks_data$allpets$crosstabs$Banner1$Total$proportions,
            structure(c(0.5000000,0.6363636,0.4545455), .Dim = c(3,1), .Dimnames = list(allpets = c("Cat", "Dog", "Bird"), total = c("Total"))),
            tolerance = 1e-7)
        expect_identical(tabBooks_data$allpets$crosstabs$Banner1$Total$totals_counts,
            structure(c(8), .Dim = c(1,1), .Dimnames = list(NULL, c("Total"))))
        expect_equal(tabBooks_data$allpets$crosstabs$Banner1$Total$totals_proportions,
            structure(c(1.590909), .Dim = c(1,1), .Dimnames = list('', c("Total"))),
            tolerance = 1e-7)
        expect_identical(tabBooks_data$allpets$crosstabs$Banner1$Total$unweighted_n,
            structure(c(8), .Dim = c(1,1), .Dimnames = list(NULL, c("Total"))))
        expect_identical(tabBooks_data$allpets$crosstabs$Banner1$Total$counts_unweighted,
            structure(c(4,7,5), .Dim = c(3,1), .Dimnames = list(allpets = c("Cat", "Dog", "Bird"), total = c("Total"))))
        expect_identical(tabBooks_data$allpets$crosstabs$Banner1$gender$counts,
            structure(c(3,3,3,1,4,2), .Dim = c(3,2), .Dimnames = list(allpets = c("Cat", "Dog", "Bird"), gender = c("Male", "Female"))))
        expect_equal(tabBooks_data$allpets$crosstabs$Banner1$gender$proportions,
            structure(c(0.5,0.75,0.6,0.5,0.5714286,0.3333333), .Dim = c(3,2), .Dimnames = list(allpets = c("Cat", "Dog", "Bird"), gender = c("Male", "Female"))),
            tolerance = 1e-7)
        expect_identical(tabBooks_data$allpets$crosstabs$Banner1$gender$totals_counts, # NOTE: actual error?
            structure(c(9,8), .Dim = c(1,2), .Dimnames = list(NULL, gender = c("Male", "Female"))))
        expect_equal(tabBooks_data$allpets$crosstabs$Banner1$gender$totals_proportions, # NOTE: Remove? not relevant in non-uniform basis
            structure(c(1.85, 1.404762), .Dim = c(2,1), .Dimnames = list(c("Male", "Female"), NULL)),
            tolerance = 1e-7)
        expect_identical(tabBooks_data$allpets$crosstabs$Banner1$gender$unweighted_n,
            structure(c(9,8), .Dim = c(1,2), .Dimnames = list(NULL, gender = c("Male", "Female"))))
        expect_identical(tabBooks_data$allpets$crosstabs$Banner1$gender$counts_unweighted,
            structure(c(3,3,3,1,4,2), .Dim = c(3,2), .Dimnames = list(allpets = c("Cat", "Dog", "Bird"), gender = c("Male", "Female"))))


        # expect_named(tabBooks_data$allpets2, c('alias', 'name', 'subnames', 'description', 'notes', 'settings', 'inserts', 'crosstabs'))
        expect_named(tabBooks_data$allpets2$crosstabs, c("Banner1"))
        expect_named(tabBooks_data$allpets2$crosstabs$Banner1, c("Total", "gender", "age5"))
        expect_named(tabBooks_data$allpets2$crosstabs$Banner1$Total, c("counts", "proportions", "totals_counts", "totals_proportions", "unweighted_n", "counts_unweighted", "pvals_col"))
        expect_identical(tabBooks_data$allpets2$crosstabs$Banner1$Total$counts,
            structure(c(4,7,5), .Dim = c(3,1), .Dimnames = list(allpets2 = c("Cat", "Dog", "Bird"), total = c("Total"))))
        expect_equal(tabBooks_data$allpets2$crosstabs$Banner1$Total$proportions,
            structure(c(0.2352941,0.4117647,0.2941176), .Dim = c(3,1), .Dimnames = list(allpets2 = c("Cat", "Dog", "Bird"), total = c("Total"))),
            tolerance = 1e-7)
        expect_identical(tabBooks_data$allpets2$crosstabs$Banner1$Total$totals_counts,
            structure(c(17), .Dim = c(1,1), .Dimnames = list(NULL, c("Total"))))
        expect_equal(tabBooks_data$allpets2$crosstabs$Banner1$Total$totals_proportions,
            structure(c(0.9411765), .Dim = c(1,1), .Dimnames = list('', c("Total"))),
            tolerance = 1e-7)
        expect_identical(tabBooks_data$allpets2$crosstabs$Banner1$Total$unweighted_n,
            structure(c(17), .Dim = c(1,1), .Dimnames = list(NULL, c("Total"))))
        expect_identical(tabBooks_data$allpets2$crosstabs$Banner1$Total$counts_unweighted,
            structure(c(4,7,5), .Dim = c(3,1), .Dimnames = list(allpets2 = c("Cat", "Dog", "Bird"), total = c("Total"))))
        expect_identical(tabBooks_data$allpets2$crosstabs$Banner1$gender$counts,
            structure(c(3,3,3,1,4,2), .Dim = c(3,2), .Dimnames = list(allpets2 = c("Cat", "Dog", "Bird"), gender = c("Male", "Female"))))
        expect_equal(tabBooks_data$allpets2$crosstabs$Banner1$gender$proportions,
            structure(c(0.3333333,0.3333333,0.3333333,0.125,0.500,0.250), .Dim = c(3,2), .Dimnames = list(allpets2 = c("Cat", "Dog", "Bird"), gender = c("Male", "Female"))),
            tolerance = 1e-7)
        expect_identical(tabBooks_data$allpets2$crosstabs$Banner1$gender$totals_counts, # NOTE: actual error?
            structure(c(9,8), .Dim = c(1,2), .Dimnames = list(NULL, gender = c("Male", "Female"))))
        expect_equal(tabBooks_data$allpets2$crosstabs$Banner1$gender$totals_proportions,
            structure(c(1, 0.875), .Dim = c(2,1), .Dimnames = list(c("Male", "Female"), NULL)),
            tolerance = 1e-7)
        expect_identical(tabBooks_data$allpets2$crosstabs$Banner1$gender$unweighted_n,
            structure(c(9,8), .Dim = c(1,2), .Dimnames = list(NULL, gender = c("Male", "Female"))))
        expect_identical(tabBooks_data$allpets2$crosstabs$Banner1$gender$counts_unweighted,
            structure(c(3,3,3,1,4,2), .Dim = c(3,2), .Dimnames = list(allpets2 = c("Cat", "Dog", "Bird"), gender = c("Male", "Female"))))



        crosstabs_summary <- crosstabs(ds, vars = tabBook_vars, banner = banner_data, weight = NULL)
        test_that("We can generate a banner data summary", {
            expect_s3_class(crosstabs_summary, "Crosstabs")
        })
    })
})
