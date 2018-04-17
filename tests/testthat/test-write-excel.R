context("writeExcel")

test_that("writeExcel input validation", {
    expect_error(writeExcel(), "No valid filename provided.")
    expect_error(writeExcel(NA, file=""),
        "writeExcel doesn't support objects of class 'logical'")
})
