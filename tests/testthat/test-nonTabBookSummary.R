context("nonTabBookSummary end-end")
  
test_that("Garbage in, garbage out", {
  expect_error(nonTabBookSummary(letters), "The expected class")
})

context("nonTabBookSummary DatetimeVariable")

test_that("Creates result object appropriately for a TextVariable", {
  ds <- readRDS(test_path("fixtures/example_dataset.rds"))
  mockery::stub(nonTabBookSummary.DatetimeVariable, "as.vector",
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
  mockery::stub(nonTabBookSummary.NumericVariable, "as.vector", 
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

context("resultsObject")

test_that("resultsObject on unweighted data", {
  x <- new("TextVariable", filter = new("CrunchLogicalExpr", dataset_url = "", 
  expression = list(), filter = list()), tuple = new("VariableTuple", 
  index_url = "a_long_url", 
  entity_url = "a_long_url", 
  body = list(discarded = FALSE, alias = "q3", secure = FALSE, 
  name = "Pet name", id = "an_id", 
  type = "text", notes = "", derived = FALSE, hidden = FALSE, 
  description = "What is your favorite pet's maiden name?")))
  
  body_values <- rep("", 10)
  body_labels <- c("Ali", "Clyde", "Fluffy", "Hugo", "Jasmine", "Lady", "Rocky", 
                   "Snoopy", "Spike", "Zeus")
  
  vector <- c("Jasmine", "Clyde", "Geoffrey", "Spike", "Zeus", "Teddy", "Ali", 
    "Hugo", "Snoopy", "Lady", "Biscuit", NA, "Daisy", "Doug", NA, 
    "Fluffy", NA, NA, "Felix", "Rocky")
  
  res <- resultsObject(x, top = NULL, weighted = FALSE, 
                       body_values = body_values,
                       body_labels = body_labels, 
                       vector = vector)
  expect_equal(
    res$data_list$unweighted_n,
    structure(list(16L), .Names = NA_character_, class = "data.frame", row.names = "Unweighted N")
  )
})
