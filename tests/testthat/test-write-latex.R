context("Write Latex")

cs <- readRDS(test_path("fixtures/crosstab_summary.RDS"))
ts <- readRDS(test_path("fixtures/toplines_summary.RDS"))

tabbook_reference <- normalizePath(test_path("ref/tabbook1.tex"))
topline_reference <- normalizePath(test_path("ref/topline1.tex"))

bad_description <- "Q.
and"

test_that("LaTeX escaping", {
    expect_identical(texEscape("$"), "\\$")
    expect_identical(texEscape(NULL), "")
    expect_identical(texEscape(bad_description), "Q. \\newline and")
})

with_temp_dir({
    test_that("Write Latex error handling", {
        expect_error(writeLatex("stuff"))
        expect_error(writeLatex(cs, filename = NULL, pdf = TRUE),
            "Please provide a file name to generate PDF output.")
    })

    test_that("Write Latex crosstab", {
        writeLatex(cs)
        expect_true(file.exists("Example Dataset with Nets.tex"))
        tex <- readLines("Example Dataset with Nets.tex")
        expect_equal(tex[1], "\\documentclass[landscape]{article}")
        ref <- readLines(tabbook_reference)
        expect_identical(tex, ref)
        if (!identical(tex, ref)) {
            system(paste("diff", tabbook_reference, shQuote("Example Dataset with Nets.tex")))
        }
        # To update reference table on NPR's computer:
        # file.copy("Example Dataset with Nets.tex", "~/c/crunchtabs/tests/testthat/ref/tabbook1.tex", overwrite=TRUE)

        writeLatex(cs, sample_desc = "Adults")
        expect_silent(tex <- readLines("Example Dataset with Nets.tex"))
        expect_equal(tex[90], "Sample  &  Adults \\\\ ")
        writeLatex(cs, moe = 0.2, field_period = "2018-01-01 to 2018-01-02")

        skip_on_appveyor()
        writeLatex(cs, pdf = TRUE)
        expect_true(file.exists("Example Dataset with Nets.pdf"))
        expect_output(writeLatex(cs, pdf = TRUE, logging = TRUE), "PDF-ing")
        writeLatex(cs, subtitle = "something", pdf = TRUE)
        writeLatex(cs, sample_desc = "Adults", pdf = TRUE)
        writeLatex(cs, moe = 0.2, field_period = "2018-01-01 to 2018-01-02", pdf = TRUE)

        theme <- themeNew(default_theme = themeDefaultLatex(), digits = 1)
        writeLatex(cs, theme = theme, pdf = TRUE)
        theme <- themeNew(default_theme = theme, font_size = 20)
        writeLatex(cs, theme = theme, pdf = TRUE)
        theme <- themeNew(default_theme = theme, format_unweighted_n=list(latex_round_percentages = TRUE))
        writeLatex(cs, theme = theme, pdf = TRUE)
        theme <- themeNew(default_theme = theme, format_weighted_n=list(latex_add_parenthesis = TRUE))
        writeLatex(cs, theme = theme, pdf = TRUE)
    })

    test_that("Write Latex toplines", {
        writeLatex(ts, file="topline1")
        expect_true(file.exists("topline1.tex"))
        tex <- readLines("topline1.tex")
        expect_equal(tex[1], "\\documentclass{article}")
        ref <- readLines(topline_reference)
        expect_identical(tex, ref)
        if (!identical(tex, ref)) {
            system(paste("diff", topline_reference, "topline1.tex"))
        }
        # To update reference table on NPR's computer:
        # file.copy("topline1.tex", "~/c/crunchtabs/tests/testthat/ref/topline1.tex", overwrite=TRUE)

        skip_on_appveyor()
        writeLatex(ts, pdf = TRUE)
        expect_true(file.exists("Example Dataset with Nets.pdf"))

        bad <- ts
        ts$results[[1]]$description <- bad_description
        writeLatex(ts, pdf = TRUE, file="topline2")
        # expect_true(file.exists("topline2.tex"))
        # file.copy("topline2.tex", "~/c/crunchtabs/tests/testthat/ref/topline2.tex", overwrite=TRUE)
        expect_true(file.exists("topline2.pdf"))
    })
})
