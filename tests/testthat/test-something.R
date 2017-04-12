context("crunchtabs tests")

# test_that("Create dataset from fixture", {
#     ds <- newDatasetFromFixture("ds1")
#     expect_true(is.dataset(ds))
#     expect_identical(dim(ds), c(20L, 15L))
#     expect_identical(names(ds),
#                      c("allpets", "q1", "petloc", "ndogs", "ndogs_a", "ndogs_b", "q3",
#                        "country", "wave", "Weight", "age", "age2", "age3", "age5", "gender"))
#     expect_identical(name(ds), "Example dataset")
#     expect_identical(names(categories(ds$q1)),
#                      c("Cat", "Dog", "Bird", "Skipped", "Not Asked"))
#     expect_is(ds, "CrunchDataset")
#     browser()
# })

test_that("Basic method dispatch error handling", {
    expect_error(getResults(NULL),
        "getResults doesn't support objects of class NULL")
    expect_error(getNames(NULL),
        "getNames doesn't support objects of class NULL")
})

with_mock_tabs("ds1_book1.json", "ds1_mt1.json", {
    test_that("We can get a tab book", {
        b <- tabBook()
        expect_is(b, "TabBookResult")
    })
    test_that("We can get tabs_data", {
        banner <- jsonlite::fromJSON(file.path(fixtures_dir, "ds1_banner1.json"), simplifyVector=FALSE)
        dataset <- jsonlite::fromJSON(file.path(fixtures_dir, "ds1.json"), simplifyVector=FALSE)
        browser()
        b <- tabBooks(banner = banner, dataset = dataset)

        expect_is(b, "list")
    })
})
