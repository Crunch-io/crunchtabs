context("crunchtabs tests")

test_that("Basic method dispatch error handling", {
    expect_error(getResults(NULL),
        "getResults doesn't support objects of class NULL")
    expect_error(getNames(NULL),
        "getNames doesn't support objects of class NULL")
})

# with_mock_tabs("ds1_book1.json", "ds1_mt1.json", {
#     test_that("We can get a tab book", {
#         b <- tabBook()
#         expect_is(b, "TabBookResult")
#     })
#     test_that("We can get tabs_data", {
#         banner <- jsonlite::fromJSON(file.path(fixtures_dir, "ds1_banner1.json"), simplifyVector=FALSE)
#         dataset <- jsonlite::fromJSON(file.path(fixtures_dir, "ds1.json"), simplifyVector=FALSE)
#         # browser()
#         b <- tabBooks(banner = banner, dataset = dataset)
#         expect_is(b, "list")
#     })
# })
