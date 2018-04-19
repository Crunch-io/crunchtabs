context("Write Excel")

cs <- readRDS(test_path("fixtures/crosstab_summary.RDS"))
ts <- readRDS(test_path("fixtures/toplines_summary.RDS"))

with_temp_dir({
    test_that("Write Excel file", {
        wd <- getwd()
        d <- tempfile()
        dir.create(d)
        setwd(d)
        on.exit(setwd(wd))
        writeExcel(cs, logging = FALSE)
        
        expect_true(file.exists("Example Dataset with Nets.xlsx"))
        
        expect_silent(wb <- loadWorkbook("Example Dataset with Nets.xlsx"))
        
        expect_equal(getSheetNames("Example Dataset with Nets.xlsx"), 
            c("RESULTS_P", "RESULTS_C", "R2_P", "R2_C"))
        expect_equal(getBaseFont(wb)$size$val, "12")
    })
})


