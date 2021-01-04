context("tabBookWeightSpec")

test_that("Returns spec as expected", {
  ds <- readRDS(test_path("fixtures/recontact_dataset.rds"))
  mockery::stub(tabBookWeightSpec, "alias", "weight1")
  mockery::stub(tabBookWeightSpec, "weight", "weight1")
  res <- expect_warning(tabBookWeightSpec(ds, weights = list(
    "weight1" = c("q1_pre", "country_pre"),
    "weight2" = c("q1_post", "country_post"))),"Dropping duplicated alias")
  
  expect_equal(
    res, 
    structure(list(alias = c("allpets", "q1", "petloc", "ndogs", 
    "ndogs_a", "ndogs_b", "q3", "country", "wave", "weight1", "weight2", 
    "q1_pre", "q1_post", "q1_post", "country_pre", "country_post", 
    "country_post"), weight = c("weight1", "weight1", "weight1", 
    "weight1", "weight1", "weight1", "weight1", "weight1", "weight1", 
    "weight1", "weight1", "weight1", "weight1", "weight2", "weight1", 
    "weight1", "weight2")), row.names = c(1L, 2L, 3L, 4L, 5L, 6L, 
    7L, 8L, 9L, 10L, 11L, 12L, 14L, 15L, 16L, 18L, 19L), class = "data.frame")           
  )
})

test_that("Don't append default weights", {
  ds <- readRDS(test_path("fixtures/recontact_dataset.rds"))
  # mockery::stub(tabBookWeightSpec, "alias", "weight1")
  mockery::stub(tabBookWeightSpec, "weight", "weight1")
  res <- tabBookWeightSpec(ds, weights = list(
    "weight1" = c("q1_pre", "country_pre"),
    "weight2" = c("q1_post", "country_post")), FALSE)
  
  expect_equal(
    res, 
    structure(list(alias = c("q1_pre", "country_pre", "q1_post", 
    "country_post"), weight = c("weight1", "weight1", "weight2", 
    "weight2")), row.names = c(NA, -4L), class = "data.frame")
  )
})

