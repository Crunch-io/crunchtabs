context("theme")

test_that("Setting latex_round_percentages_exception works",{
  toplines_theme <- themeNew(
    default_theme = themeDefaultLatex(),
    format_title = list(decoration = "bold"),
    format_subtitle = list(decoration = "bold", font_size = 14),
    format_var_description = list(include_q_number = TRUE, background_color = "gray"), 
    format_var_filtertext = list(decoration = "italic", font_size = 8),
    latex_headtext = "tbc", 
    latex_foottext = "tbc",
    latex_round_percentages = TRUE,
    latex_round_percentages_exception = c('demfirstchoice', 'demsecondchoice'), 
    latex_max_lines_for_tabular = 15, 
    latex_table_align = 'g', 
    format_totals_row = NULL,
    format_unweighted_n = NULL,
    format_weighted_n = NULL,
    one_per_sheet = FALSE)
  expect_true(toplines_theme$latex_round_percentages)
  expect_equal(toplines_theme$latex_round_percentages_exception, c('demfirstchoice', 'demsecondchoice'))
})