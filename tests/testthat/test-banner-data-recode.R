context("Banner data recode")

test_that("Martix columns are renamed and excluded according to the parametrization", {
    b_table <- matrix(1:12, ncol = 4, dimnames = list(NULL, "cats_stuff"=c("cat1", "cat2", "cat3", "cat4")))
    b_recode <- list(alias = "cats_stuff",
        old_categories = c("cat1", "cat2", "cat3", "cat4"),
        categories_out = c("cat1a", "cat2", NA, "cat4a"),
        categories = c("cat1a", "cat2", "cat4a"))
    expected_result <- structure(b_table[, c("cat1", "cat2", "cat4")], 
        dimnames = list(NULL, "cats_stuff"=c("cat1a", "cat2", "cat4a")))
    result <- crunchtabs:::bannerDataRecode(b_table, b_recode)
    expect_equal(result, expected_result)
})

test_that("The input matrix is left unchanged if default parametrization is used", {
    b_table <- matrix(1:12, ncol = 4, dimnames = list(NULL, "cats_stuff"=c("cat1", "cat2", "cat3", "cat4")))
    b_recode <- list(alias = "cats_stuff",
        old_categories = c("cat1", "cat2", "cat3", "cat4"),
        categories_out = c("cat1", "cat2", "cat3", "cat4"),
        categories = c("cat1", "cat2", "cat3", "cat4"))
    result <- crunchtabs:::bannerDataRecode(b_table, b_recode)
    expect_equal(result, b_table)
})
