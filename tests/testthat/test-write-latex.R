context("Write Latex")

cs <- readRDS(test_path("fixtures/crosstab_summary.RDS"))
ts <- readRDS(test_path("fixtures/toplines_summary.RDS"))


with_temp_dir({
    test_that("Write Latex error handling", {
        expect_error(writeLatex(cs, filename = NULL, pdf = TRUE))
        expect_error(writeLatex(cs, custom_numbering = c(letters[1:5])))
    })
    

    test_that("Write Latex crosstab", {
        expect_warning(writeLatex(cs))
        theme <- theme_default(font = "helvet")
        
        writeLatex(cs, theme = theme)
        expect_true(file.exists("Example Dataset with Nets.tex"))
        expect_silent(tex <- readLines("Example Dataset with Nets.tex"))
        expect_equal(tex[1], "\\documentclass[landscape]{article}")
    
        writeLatex(cs, theme = theme, pdf = TRUE)
        expect_true(file.exists("Example Dataset with Nets.pdf"))
        
    })
    
    test_that("Write Latex toplines", {
        expect_warning(writeLatex(ts))
        theme <- theme_default(font = "helvet")
        
        writeLatex(ts, theme = theme)
        expect_true(file.exists("Example Dataset with Nets.tex"))
        expect_silent(tex <- readLines("Example Dataset with Nets.tex"))
        expect_equal(tex[1], "\\documentclass[12pt]{article}")
    
        writeLatex(ts, theme = theme, pdf = TRUE)
        expect_true(file.exists("Example Dataset with Nets.pdf"))
        
    })
})
