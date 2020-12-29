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

test_that("Does not delete log/aux/out files if logging = TRUE", {
  system("echo 'This is an out file' >> text.out")
  system("echo 'This is a log file' >> text.log")
  system("echo 'This is an aux file' >> text.aux")
  writeLatex(cs, logging = TRUE, pdf = TRUE)
  expect_true(file.exists("text.out"))
  expect_true(file.exists("text.log"))
  expect_true(file.exists("text.aux"))
})

test_that("Fails when given inappropriate data", {
  expect_error(writeLatex(list()), "The expected class for `data_summary` is")
})


test_that("Deletes log/aux/out files if logging = FALSE (the default)", {
  system("echo 'This is an out file' >> text.out")
  system("echo 'This is a log file' >> text.log")
  system("echo 'This is an aux file' >> text.aux")

  expect_true(file.exists("text.out"))
  expect_true(file.exists("text.log"))
  expect_true(file.exists("text.aux"))
  writeLatex(cs, pdf = TRUE, logging = FALSE)
  expect_true(!file.exists("text.out"))
  expect_true(!file.exists("text.log"))
  expect_true(!file.exists("text.aux"))
})

with_temp_dir({
  test_that("Write Latex error handling", {
    expect_error(writeLatex("stuff"))
    expect_error(
      writeLatex(cs, filename = NULL, pdf = TRUE),
      "Please provide a file name to generate PDF output."
    )
  })

  test_that("Write Latex crosstab", {
    writeLatex(cs)
    expect_true(file.exists("Example Dataset with Nets.tex"))
    tex <- readLines("Example Dataset with Nets.tex")
    expect_equal(tex[1], "\\documentclass[landscape]{article}")
    ref <- readLines(tabbook_reference)
    expect_identical(trimws(tex), trimws(ref))
    if (!identical(trimws(tex), trimws(ref))) {
      system(paste("diff", tabbook_reference, shQuote("Example Dataset with Nets.tex")))
    }
    # To update reference table on NPR's computer:
    # file.copy("Example Dataset with Nets.tex", "~/c/crunchtabs/tests/testthat/ref/tabbook1.tex", overwrite=TRUE)

    writeLatex(cs, sample_desc = "Adults")
    expect_silent(tex <- readLines("Example Dataset with Nets.tex"))
    expect_true(any(tex == "Sample  &  Adults \\\\ "))
    writeLatex(cs, moe = 0.2, field_period = "2018-01-01 to 2018-01-02")

    skip_on_appveyor()
    writeLatex(cs, pdf = TRUE)
    expect_true(file.exists("Example Dataset with Nets.pdf"))
    # expect_output(writeLatex(cs, pdf = TRUE, logging = TRUE), "PDF-ing") # Kind of useless just for one tiny message
    writeLatex(cs, subtitle = "something", pdf = TRUE)
    writeLatex(cs, sample_desc = "Adults", pdf = TRUE)
    writeLatex(cs, moe = 0.2, field_period = "2018-01-01 to 2018-01-02", pdf = TRUE)

    theme <- themeNew(default_theme = themeDefaultLatex(), digits = 1)
    writeLatex(cs, theme = theme, pdf = TRUE)
    theme <- themeNew(default_theme = theme, font_size = 20)
    writeLatex(cs, theme = theme, pdf = TRUE)
    theme <- themeNew(default_theme = theme, format_unweighted_n = list(latex_round_percentages = TRUE))
    writeLatex(cs, theme = theme, pdf = TRUE)
    theme <- themeNew(default_theme = theme, format_weighted_n = list(latex_add_parenthesis = TRUE))
    writeLatex(cs, theme = theme, pdf = TRUE)
  })

  test_that("Write Latex toplines", {
    writeLatex(ts, file = "topline1")
    expect_true(file.exists("topline1.tex"))
    tex <- readLines("topline1.tex")
    expect_equal(tex[1], "\\documentclass{article}")
    ref <- readLines(topline_reference)
    expect_identical(trimws(tex), trimws(ref))
    if (!identical(trimws(tex), trimws(ref))) {
      system(paste("diff", topline_reference, "topline1.tex"))
    }
    # To update reference table on NPR's computer:
    # file.copy("topline1.tex", "~/c/crunchtabs/tests/testthat/ref/topline1.tex", overwrite=TRUE)

    skip_on_appveyor()
    writeLatex(ts, pdf = TRUE)
    expect_true(file.exists("Example Dataset with Nets.pdf"))

    # Test that fix for #36: previously failed to generate PDF
    bad <- ts
    ts$results[[1]]$description <- bad_description
    writeLatex(ts, pdf = TRUE, file = "topline2")
    expect_true(file.exists("topline2.pdf"))
  })
})

context("latexDocHead")
test_that("latexDocHead accepts a logo appropriately", {
  tema <- themeNew(
    logo = list(file = system.file("YouGov.png", package = "crunchtabs")),
    default_theme = themeDefaultLatex()
  )

  tema$topline <- TRUE
  res <- latexDocHead(tema, "Hello", "Goodbye")
  expect_true(
    any(
      grepl("includegraphics*.*YouGov", res)
    )
  )
})

context("latexReportTables")
test_that("Adds nonTabBookSummary as expected", {
  results <- readRDS(test_path("fixtures/tabbook_results_nonTabBookSummary.rds"))

  # Test some theme options as well
  tema <- structure(list(
    logo = list(
      startRow = 1, startCol = 1, width = 4,
      height = 2, units = c(default = "in"), dpi = 300,
      file = "/home/beb/Projects/crunchtabs/inst/YouGov.png"
    ),
    # Test pagebreak_in_banner oddity
    pagebreak_in_banner = FALSE,
    # Test one_per_sheet FALSE
    one_per_sheet = FALSE,
    font = "helvet",
    font_size = 12, format_title = list(font_size = 16, decoration = "bold"),
    format_subtitle = list(font_size = 12, decoration = "bold"),
    format_banner_categories = list(font = "helvet", font_size = NULL),
    format_var_description = list(
      font = "helvet", font_size = NULL,
      include_alias = FALSE, include_q_number = TRUE, repeat_for_subs = TRUE
    ),
    format_var_subname = list(
      font = "helvet", font_size = NULL,
      include_alias = FALSE, include_q_number = FALSE
    ), format_var_filtertext = list(
      font_size = 8, decoration = "italic", include_alias = FALSE,
      include_q_number = FALSE, repeat_for_subs = TRUE
    ), format_subtotals = list(
      font = "helvet", font_size = NULL, decoration = "bold"
    ),
    format_headers = list(
      font = "helvet", font_size = NULL,
      decoration = "bold"
    ), format_unweighted_n = list(
      font = "helvet",
      font_size = NULL, name = "Unweighted N", position_top = FALSE,
      position_bottom = TRUE, position_fixed = FALSE, latex_add_parenthesis = FALSE,
      latex_adjust = "c"
    ), format_totals_row = list(
      font = "helvet",
      font_size = NULL, name = "Totals", position_top = FALSE,
      position_bottom = TRUE
    ), format_label_column = list(
      font = "helvet",
      font_size = NULL, col_width = NA_real_, extend_borders = FALSE
    ),
    format_totals_column = list(font = "helvet", font_size = NULL),
    digits = 0, digits_numeric = 2, excel_percent_sign = TRUE,
    excel_show_grid_lines = FALSE, excel_freeze_column = 0, excel_orientation = "portrait",
    latex_round_percentages = FALSE, enforce_onehundred = FALSE,
    latex_headtext = "", latex_foottext = "", latex_table_align = "r",
    latex_multirowheaderlines = TRUE, latex_max_lines_for_tabular = 0,
    latex_page_numbers = TRUE, latex_flip_grids = FALSE, topline = TRUE,
    proportions = TRUE
  ), class = "Theme")

  res <- latexReportTables(results, NULL, tema)

  # Expect absolutelynopagebreak wraps on all
  expect_true(
    all(grepl("absolutelynopagebreak", res))
  )

  # Clear page not appended to results
  expect_false(
    any(grepl("clearpage$", res))
  )
})
