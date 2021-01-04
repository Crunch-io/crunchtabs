context("banner_manipulation")

test_that("returns null if banner is null", {
  expect_equal(banner_manipulation(NULL, NULL, NULL), NULL)
})

test_that("adjusts banner", {
  ds <- readRDS(test_path("fixtures/example_dataset.rds"))
  banner_use <- structure(list(Results = list(`___total___` = structure(list(
    alias = "___total___", name = "", type = "Total", old_categories = "Total", 
    categories_out = "Total", categories = "Total"), class = "BannerVar"), 
    allpets = structure(list(alias = "allpets", name = "All pets owned", 
    type = "multiple_response", old_categories = c("Cat", 
    "Dog", "Bird"), categories_out = c("Cat", "Dog", "Bird"
    ), categories = c("Cat", "Dog", "Bird")), class = "BannerVar"))), class = "Banner")
  
  
  mockery::stub(banner_manipulation, "nrow", 20)
  mockery::stub(banner_manipulation, "setNames", structure(
    c(Cat = 4, Dog = 5, Bird = 5), .Dim = 3L, 
    .Dimnames = list(c("Cat", "Dog", "Bird"))))
  res <- banner_manipulation(banner_use, ds, NULL)
  
  expect_named(res, "Results")
  expect_is(res, "Banner")
  expect_equal(res$Results$allpets$alias, "allpets")
  expect_equal(as.vector(res$Results$allpets$unweighted_n),c(4,5,5))

})

