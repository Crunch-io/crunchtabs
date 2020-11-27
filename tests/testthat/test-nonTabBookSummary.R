context("nonTabBookSummary end-end")

# test_that("E-E for NumericVariable, DatetimeVariable", {
#   ds <- readRDS(test_path("fixtures/example_dataset.rds"))
#   mockery::stub(crosstabs, "crunch::weight", NULL)
#   typs <- c("multiple_response", "categorical", "categorical_array", "numeric", 
#             "numeric", "numeric", "text", "categorical", "datetime", "numeric")
#   mockery::stub(crosstabs, "crunch::types", typs)
#   als <- c("allpets", "q1", "petloc", "ndogs", "ndogs_a", "ndogs_b", "q3", 
#            "country", "wave", "caseid")
#   mockery::stub(crosstabs, "crunch::aliases", als)
#   
#   bnr <- structure(list(Results = list(`___total___` = structure(list(
#     alias = "___total___", name = "", type = "Total", old_categories = "Total", 
#     categories_out = "Total", categories = "Total"), class = "BannerVar"), 
#   allpets = structure(list(alias = "allpets", name = "All pets owned", 
#     type = "multiple_response", old_categories = c("Cat", 
#     "Dog", "Bird"), categories_out = c("Cat", "Dog", "Bird"
#     ), categories = c("Cat", "Dog", "Bird")), class = "BannerVar"))), class = "Banner")
#   
#   mockery::stub(crosstabs, "banner", bnr)
#   mockery::stub(crosstabs, "tabBooks", readRDS(test_path("fixtures/tabbook_results_nonTabBookSummary.rds")))
#   mockery::stub(crosstabs, "is.weightVariable", FALSE)
#   stub(nonTabBookSummary.NumericVariable, "as.vector", 
#        c(1, NA, 2, 3, 1, 2, 2, 3, 2, 2, 2, NA, 3, 0, 6, 1, NA, 0, NA, 2),
#        depth = 5)
#   
#   # mockery::stub(nonTabBookSummary.TextVariable, "as.vector", 
#   #               c("Jasmine", "Clyde", "Geoffrey", "Spike", "Zeus", "Teddy", "Ali", 
#   #                 "Hugo", "Snoopy", "Lady", "Biscuit", NA, "Daisy", "Doug", NA, 
#   #                 "Fluffy", NA, NA, "Felix", "Rocky"))
#   # 
#   # 
#   # 
# 
#   ct <- crosstabs(ds, include_numeric = TRUE,
#                   include_datetime = TRUE)
#   expect_equal(
#     names(ct$results),
#     c("allpets", "q1", "petloc", "ndogs", "ndogs_a", "ndogs_b",
#       "country", "wave")
#   )
#   
#   expect_equal(
#     rownames(ct$results$ndogs$data_list$body),
#     c("Minimum", "1st Quartile", "Median", "Mean", "3rd Quartile",
#       "Maximum", "Standard Deviation")
#   )
#   
#   expect_equal(
#     ct$results$ndogs$data_list$body[1:7,],
#     c(0, 1, 2, 2, 2.25, 6, 1.4142135623731)
#   )
#   
#   expect_equal(
#     names(ct$results),
#     c("allpets", "q1", "petloc", "ndogs", "ndogs_a", "ndogs_b",
#       "country", "wave")
#   )
#   
#   expect_equal(
#     rownames(ct$results$wave$data_list$body),
#     c("Minimum", "1st Quartile", "Median", "3rd Quartile", "Maximum")
#   )
#   
#   expect_equal(
#     ct$results$wave$data_list$body[,],
#     structure(c(1417392000, 1417392000, 1418731200, 1420070400, 1420070400
#     ), class = c("POSIXct", "POSIXt")))
#   
# })
  
test_that("Garbage in, garbage out", {
  expect_error(nonTabBookSummary(letters), "The expected class")
})

context("nonTabBookSummary DatetimeVariable")

test_that("Creates result object appropriately for a TextVariable", {
  ds <- readRDS(test_path("fixtures/example_dataset.rds"))
  stub(nonTabBookSummary.DatetimeVariable, "as.vector",
                structure(c(16405, 16405, 16405, 16405, 16405, 16405, 16405,
                            16405, 16405, 16405, 16436, 16436, 16436, 16436, 16436, 16436,
                            16436, 16436, 16436, 16436), class = "Date"))

  res <- nonTabBookSummary(ds$wave)
  
  expect_equal(
    res, 
    structure(list(alias = "wave", name = "Wave", description = "Wave", 
      notes = "", type = "DatetimeVariable", top = NULL, bottom = c(weighted_n = "weighted_n"), 
      data_order = c("body", weighted_n = "weighted_n"), inserts = c("Category", 
      "Category", "Category", "Category", "Category"), data_list = list(
      body = structure(list(structure(c(1417392000, 1417392000, 
      1418731200, 1420070400, 1420070400), class = c("POSIXct", 
      "POSIXt"))), .Names = NA_character_, class = "data.frame", row.names = c("Minimum", 
      "1st Quartile", "Median", "3rd Quartile", "Maximum")), 
      weighted_n = structure(list(20L), .Names = NA_character_, class = "data.frame", row.names = "Weighted N")), 
      min_cell_top = NULL, no_totals = TRUE, mean_median = FALSE, 
      min_cell_body = structure(c(NA, NA, NA, NA, NA), .Dim = c(5L, 
      1L)), min_cell_bottom = structure(FALSE, .Dim = c(1L, 1L)), 
      min_cell = FALSE, rownames = c("Minimum", "1st Quartile", 
      "Median", "3rd Quartile", "Maximum", "Weighted N")), class = c("ToplineVar", 
      "CrossTabVar"))
  )

})

context("nonTabBookSummary NumericVariable")

test_that("Creates result object appropriately for a TextVariable", {
  ds <- readRDS(test_path("fixtures/example_dataset.rds"))
  stub(nonTabBookSummary.NumericVariable, "as.vector", 
       c(1, NA, 2, 3, 1, 2, 2, 3, 2, 2, 2, NA, 3, 0, 6, 1, NA, 0, NA, 2))
  res <- nonTabBookSummary(ds$ndogs)
  expect_equal(
    res, 
    structure(list(alias = "ndogs", name = "Number of dogs", description = "Number of dogs", 
    notes = "", type = "NumericVariable", top = NULL, bottom = c(weighted_n = "weighted_n"), 
    data_order = c("body", weighted_n = "weighted_n"), inserts = c("Category", 
    "Category", "Category", "Category", "Category", "Category", 
    "Category"), data_list = list(body = structure(list(c(0, 
    1, 2, 2, 2.25, 6, 1.4142135623731)), .Names = NA_character_, class = "data.frame", row.names = c("Minimum", 
    "1st Quartile", "Median", "Mean", "3rd Quartile", "Maximum", 
    "Standard Deviation")), weighted_n = structure(list(16L), 
    .Names = NA_character_, class = "data.frame", row.names = "Weighted N")), 
    min_cell_top = NULL, no_totals = TRUE, mean_median = FALSE, 
    min_cell_body = structure(c(NA, NA, NA, NA, NA, NA, NA), .Dim = c(7L, 
    1L)), min_cell_bottom = structure(FALSE, .Dim = c(1L, 1L)), 
    min_cell = FALSE, rownames = c("Minimum", "1st Quartile", 
    "Median", "Mean", "3rd Quartile", "Maximum", "Standard Deviation", 
    "Weighted N")), class = c("ToplineVar", "CrossTabVar"))
  )
})


context("nonTabBookSummary TextVariable")

test_that("Creates result object appropriately for a TextVariable", {
  ds <- readRDS(test_path("fixtures/example_dataset.rds"))
  smpl <- c("Jasmine", "Clyde", "Geoffrey", "Spike", "Zeus", "Teddy", "Ali", 
            "Hugo", "Snoopy", "Lady", "Biscuit", NA, "Daisy", "Doug", NA, 
            "Fluffy", NA, NA, "Felix", "Rocky")
  mockery::stub(nonTabBookSummary.TextVariable, "as.vector", smpl)
  res <- nonTabBookSummary(ds$q3)
  
  expect_equal(
    res$rownames,
    c("Ali", "Clyde", "Fluffy", "Hugo", "Jasmine", "Lady", "Rocky", 
      "Snoopy", "Spike", "Zeus", "Weighted N"))
  
  expect_equal(res,
    structure(list(alias = "q3", name = "Pet name", description = "What is your favorite pet's maiden name?", 
    notes = "", type = "TextVariable", top = NULL, bottom = c(weighted_n = "weighted_n"), 
    data_order = c("body", weighted_n = "weighted_n"), inserts = c("Category", 
    "Category", "Category", "Category", "Category", "Category", 
    "Category", "Category", "Category", "Category"), data_list = list(
    body = structure(list(c("", "", "", "", "", "", "", "", 
    "", "")), .Names = NA_character_, class = "data.frame", row.names = c("Ali", 
    "Clyde", "Fluffy", "Hugo", "Jasmine", "Lady", "Rocky", 
    "Snoopy", "Spike", "Zeus")), weighted_n = structure(list(
    16L), .Names = NA_character_, class = "data.frame", row.names = "Weighted N")), 
    min_cell_top = NULL, no_totals = TRUE, mean_median = FALSE, 
    min_cell_body = structure(c(NA, NA, NA, NA, NA, NA, NA, NA, 
    NA, NA), .Dim = c(10L, 1L)), min_cell_bottom = structure(FALSE, .Dim = c(1L, 
    1L)), min_cell = FALSE, rownames = c("Ali", "Clyde", "Fluffy", 
    "Hugo", "Jasmine", "Lady", "Rocky", "Snoopy", "Spike", "Zeus", 
    "Weighted N")), class = c("ToplineVar", "CrossTabVar"))
    )
})
