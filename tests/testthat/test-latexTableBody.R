context("latexTableBody")
test_that("latexTableBody applies mininmum n masking", {
  df <- structure(list(
    top = NULL, bottom = c(unweighted_n = "unweighted_n"),
    data_order = c("body", unweighted_n = "unweighted_n"), inserts = NULL,
    data_list = list(body = structure(list(c(50, 62, 45)), .Names = NA_character_, class = "data.frame", row.names = c(
      "Cat",
      "Dog", "Bird"
    )), unweighted_n = structure(list(c(8, 11)), .Names = NA_character_, class = "data.frame", row.names = c(
      "Unweighted N: Min",
      "Unweighted N: Max"
    ))), min_cell_top = NULL, min_cell_body = structure(c(
      TRUE,
      TRUE, TRUE
    ), .Dim = c(3L, 1L)), min_cell_bottom = NULL, min_cell = TRUE,
    rownames = c("Cat", "Dog", "Bird", "Unweighted N: Min", "Unweighted N: Max")
  ), class = c(
    "MultipleResponseCrossTabVar", "ToplineVar",
    "CrossTabVar"
  ))

  theme <- themeNew(
    default_theme = themeDefaultLatex(),
    format_min_base = list(mask = "*", min_base = 20)
  )
  theme$proportions <- TRUE
  theme$topline <- TRUE

  res <- latexTableBody(df, theme, question_alias = NULL)
  # should replace numbers with * after dotfill and mask
  # the numbers where unweight_n is lower than 20
  # this is guaranteed to be the case for allpets question
  # wh
  expect_true(
    grepl("\\dotfill * \\", res, fixed = TRUE)
  )
})
