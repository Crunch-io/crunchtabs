context("Your first test")

with_mock_API({
    test_that("Tests are running--delete this and put in real tests!", {
        ds <- loadDataset("test")
        expect_is(ds, "CrunchDataset")
    })
})

test_that("Basic method dispatch error handling", {
    expect_error(getResults(NULL),
        "The getResults generic function doesn't support objects of class NULL")
    expect_error(getNames(NULL),
        "getNames doesn't support objects of class NULL")
    # NPR: why are these different messages?
})
