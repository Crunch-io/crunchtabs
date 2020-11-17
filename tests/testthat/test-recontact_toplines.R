context('recontact_toplines')

test_that("Stops if not a dataset", {
  expect_error(
    recontact_toplines(
      "String is not a dataset",
      questions = "q1",
      suffixes = c("_pre", "_post"),
      labels = c("Before", "After"),
      weights = NULL
    ),
    "is.dataset(dataset) is not TRUE",
    fixed = TRUE
  )
})

test_that("Stops if questions not character", {
  expect_error(
    with_mock(
      recontact_toplines(
    "mocked",
    questions = 1L, # not character
    suffixes = c("_pre", "_post"),
    labels = c("Before", "After"),
    weights = NULL
      ),
    `crunch::is.dataset` = function(...) TRUE,
  ),
  "is.character(questions) is not TRUE", fixed = TRUE)
})
test_that("Stops if suffixes not character", {
  expect_error(
    with_mock(
      recontact_toplines(
        "mocked",
        questions = "character",
        suffixes = 1L, # not character
        labels = c("Before", "After"),
        weights = NULL
      ),
      `crunch::is.dataset` = function(...) TRUE
    ),
    "is.character(suffixes) is not TRUE", fixed = TRUE)
})

test_that("Stops if labels not character", {
  expect_error(
    with_mock(
      recontact_toplines(
        "String is not a dataset",
        questions = "text to pass check",
        suffixes = "text to pass check",
        labels = 1L,
        weights = NULL
      ),
      `crunch::is.dataset` = function(...) TRUE
    ),
    "is.character(labels) is not TRUE", fixed = TRUE)
})


# with_api_fixture("fixtures-1-3-0", {
#   # httpcache::clearCache()
#   test_that("End to end", {
#     ds <- loadDataset("Recontact dataset")
#     # aliases(allVariables(ds))
# 
#     r <- recontact_toplines(
#         ds,
#         questions = c("q1", "country"),
#         suffixes = c("_pre", "_post"),
#         labels = c("Pre", "Post"),
#         weights = c("weight1", "weight2")
#       )
# 
#     expect_named(r$results, c("q1", "country"))
#     expect_is(
#       r$results$q1,
#       c("ToplineCategoricalArray", "ToplineVar", "CrossTabVar")
#     )
#     expect_is(
#       r$results$country,
#       c("ToplineCategoricalArray", "ToplineVar", "CrossTabVar")
#     )
#   })
# 
# })
