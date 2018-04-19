context("Write Latex")

cs <- readRDS(test_path("fixtures/crosstab_summary.RDS"))
ts <- readRDS(test_path("fixtures/toplines_summary.RDS"))


with_temp_dir({
    test_that("Write Latex error handling", {
        expect_error(writeLatex(cs, custom_numbering = c(letters[1:5])))
        expect_error(writeLatex("stuff"))
        skip_on_appveyor()
        expect_error(writeLatex(cs, filename = NULL, pdf = TRUE))
    })
    

    test_that("Write Latex crosstab", {
        theme <- theme_latex_default(font = "helvet")

        writeLatex(cs, theme = theme)
        expect_true(file.exists("Example Dataset with Nets.tex"))
        expect_silent(tex <- readLines("Example Dataset with Nets.tex"))
        expect_equal(tex[1], "\\documentclass[landscape]{article}")

        writeLatex(cs, theme = theme, sample_desc = "Adults")
        expect_silent(tex <- readLines("Example Dataset with Nets.tex"))
        expect_equal(tex[72], "Sample  &  Adults \\\\ ")
        writeLatex(cs, theme = theme, moe = 0.2, field_period = "2018-01-01 to 2018-01-02")

        skip_on_appveyor()
        writeLatex(cs, theme = theme, pdf = TRUE)
        expect_true(file.exists("Example Dataset with Nets.pdf"))
        expect_output(writeLatex(cs, theme = theme, pdf = TRUE, logging = TRUE), "PDF-ing")
        writeLatex(cs, theme = theme, subtitle = "something", pdf = TRUE)
        writeLatex(cs, theme = theme, sample_desc = "Adults", pdf = TRUE)
        writeLatex(cs, theme = theme, moe = 0.2, field_period = "2018-01-01 to 2018-01-02", pdf = TRUE)

        theme <- theme_new(default_theme = theme, digits = 1)
        writeLatex(cs, theme = theme, pdf = TRUE)
        theme <- theme_new(default_theme = theme, font_size = 20)
        writeLatex(cs, theme = theme, pdf = TRUE)
        writeLatex(cs, theme = theme, multirowheaderlines = TRUE, pdf = TRUE)
        theme <- theme_new(default_theme = theme, latex_round_percentages = TRUE)
        writeLatex(cs, theme = theme, pdf = TRUE)
        theme <- theme_new(default_theme = theme, latex_add_parenthesis = TRUE)
        writeLatex(cs, theme = theme, pdf = TRUE)
        
    })
    
    test_that("Write Latex toplines", {
        writeLatex(ts)
        expect_true(file.exists("Example Dataset with Nets.tex"))
        expect_silent(tex <- readLines("Example Dataset with Nets.tex"))
        expect_equal(tex[1], "\\documentclass[12pt]{article}")
    
        skip_on_appveyor()
        writeLatex(ts, pdf = TRUE)
        expect_true(file.exists("Example Dataset with Nets.pdf"))
        
    })
})
