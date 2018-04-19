context("Write Excel")

cs <- readRDS(test_path("fixtures/crosstab_summary.RDS"))
ts <- readRDS(test_path("fixtures/toplines_summary.RDS"))

with_temp_dir({
    test_that("Write Excel file", {
        writeExcel(cs)
        expect_true(file.exists("Example Dataset with Nets.xlsx"))
        expect_silent(wb <- loadWorkbook("Example Dataset with Nets.xlsx"))
        expect_equal(getSheetNames("Example Dataset with Nets.xlsx"), 
            c("RESULTS_P", "RESULTS_C", "R2_P", "R2_C"))
        expect_equal(getBaseFont(wb)$size$val, "12")
        
        writeExcel(cs, table_of_contents = TRUE)
        expect_equal(getSheetNames("Example Dataset with Nets.xlsx"), 
            c("TOC", "RESULTS_P", "RESULTS_C", "R2_P", "R2_C"))

        theme <- theme_new(one_per_sheet = TRUE)
        writeExcel(cs, theme = theme, table_of_contents = TRUE, n_or_percent = "percents")
        expect_equal(getSheetNames("Example Dataset with Nets.xlsx"), 
            c("TOC", "allpets_1", "favpet_1","petloc_home_1", "petloc_work_1","ndogs_1", "ndogs_a_1", "ndogs_b_1","country_1", 
                "age_1","age2_1", "age3_1", "age5_1","gender_1", "weight_1","noweight_1", "allpets_non_ub_1", "allpets_2",
                "favpet_2", "petloc_home_2","petloc_work_2", "ndogs_2", "ndogs_a_2","ndogs_b_2", "country_2","age_2", "age2_2", 
                "age3_2","age5_2", "gender_2","weight_2", "noweight_2", "allpets_non_ub_2"))
        
        expect_is(writeExcel(cs, theme = theme, save_workbook = FALSE), "Workbook")

    })
})


