context("Preparing a banner data summary")

context("tab_frame_generate")

test_that("If default_weight null", {
  r <- tab_frame_generate(NULL, "hello")
  expect_equal(nrow(r), 1)
  expect_true(is.na(r$weight))
})

test_that("If default_weight not null", {
  r <- tab_frame_generate("weight1", "hello")
  expect_equal(nrow(r), 1)
  expect_equal(as.character(r$weight), "weight1")
  expect_equal(as.character(r$alias), "hello")
})

context("valiases_tabbook_extract")

test_that("if is_crosstabs_array", {
  r <- with_mock(
    valiases_tabbook_extract(TRUE, "cube", "cube_var", "question_name"),
    `crunchtabs::getSubAliases` = function(...) "from_getSubAliases",
    `crunch::aliases` = function(...) "from_crunchaliases"
  )
  expect_equal(r, "from_getSubAliases")
})

test_that("if not is_crosstabs_array", {
  r <- with_mock(
    valiases_tabbook_extract(FALSE, "cube", "cube_var", "question_name"),
    `crunchtabs::getSubAliases` = function(...) "from_getSubAliases",
    `crunch::aliases` = function(...) "from_crunchaliases"
  )
  expect_equal(r, "from_crunchaliases")
})

test_that("if not is_crosstabs_array but total", {
  r <- with_mock(
    valiases_tabbook_extract(FALSE, "cube", "cube_var", "question_name"),
    `crunchtabs::getSubAliases` = function(...) "from_getSubAliases",
    `crunch::aliases` = function(...) "total"
  )
  expect_equal(r, "question_name")
})

context("extToContentType")

test_that("Returns appropriate mapping", {
  expect_equal(extToContentType("json"), "application/json")
  expect_equal(
    extToContentType("xlsx"),
    "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
  )
  expect_equal(
    extToContentType("pptx"),
    "application/vnd.openxmlformats-officedocument.presentationml.presentation"
  )
})

context("tabBooks")

test_that("End to End tabBooks", {
  ds <- readRDS(test_path("fixtures/recontact_dataset.rds"))
  vars <- c("allpets", "q1")
  banner_use <- structure(list(Results = list(
    `___total___` = structure(list(
      alias = "___total___", name = "", type = "Total", old_categories = "Total",
      categories_out = "Total", categories = "Total"
    ), class = "BannerVar"),
    allpets = structure(list(
      alias = "allpets", name = "All pets owned",
      type = "multiple_response", old_categories = c(
        "Cat",
        "Dog", "Bird"
      ), categories_out = c("Cat", "Dog", "Bird"), categories = c("Cat", "Dog", "Bird")
    ), class = "BannerVar")
  )), class = "Banner")

  multtable <- new("Multitable",
    element = "shoji:entity", self = "https",
    body = list(
      name = "c4cae4937918ee0", user = "https",
      template = list(list(query = list(
        list(each = "https"),
        list(`function` = "as_selected", args = list(list(
          variable = "https:"
        )))
      ))),
      team = NULL, is_public = FALSE, display_settings = structure(list(), .Names = character(0)),
      id = "b97265d117d74aeab4ebffff9a55318f"
    ), urls = NULL,
    catalogs = NULL, views = list(
      applied = "https",
      tabbook = "https",
      export = "https"
    ),
    fragments = NULL
  )

  mockery::stub(tabBooks, "getMultitable", multtable)

  tbwtspec <- structure(list(alias = c(
    "allpets", "q1", "petloc", "ndogs",
    "ndogs_a", "ndogs_b", "q3", "country", "wave", "weight1", "weight2",
    "q1_pre", "q1_post", "q1_post", "country_pre", "country_post",
    "country_post"
  ), weight = c(
    "weight1", "weight1", "weight1",
    "weight1", "weight1", "weight1", "weight1", "weight1", "weight1",
    "weight1", "weight1", "weight1", "weight1", "weight2", "weight1",
    "weight1", "weight2"
  )), row.names = c(
    1L, 2L, 3L, 4L, 5L, 6L,
    7L, 8L, 9L, 10L, 11L, 12L, 14L, 15L, 16L, 18L, 19L
  ), class = "data.frame")

  mockery::stub(tabBooks, "weight", NULL)

  book <- readRDS(test_path("fixtures/tabBooks-tabBook_crunchtabs.rds"))
  mockery::stub(tabBooks, "tabBook_crunchtabs", book)

  res <- tabBooks(ds, vars, banner_use, topline = TRUE)

  expect_named(res, c("allpets", "q1"))
  expect_equal(
    res$allpets$crosstabs$Results$`___total___`$counts,
    structure(
      c(4, 5, 5),
      .Dim = c(3L, 1L),
      .Dimnames = list(c("Cat", "Dog", "Bird"), "Total")
    )
  )

  mockery::stub(tabBooks, "tabBookWeightSpec", tbwtspec)

  res <- tabBooks(ds, vars, banner_use, topline = TRUE, weight = list(
    "weight1" = c("q1_pre", "country_pre"),
    "weight2" = c("q1_post", "country_post")
  ))

  expect_named(res, c("allpets_weight1", "q1_weight1"))
  expect_equal(
    res$allpets$crosstabs$Results$`___total___`$counts,
    structure(
      c(4, 5, 5),
      .Dim = c(3L, 1L),
      .Dimnames = list(c("Cat", "Dog", "Bird"), "Total")
    )
  )
})


context("getMultitable")

test_that("getMultitable", {
  ds <- readRDS(test_path("fixtures/example_dataset.rds"))
  banner_flatten <- list(`___total___` = structure(list(
    alias = "___total___", name = "",
    type = "Total", old_categories = "Total", categories_out = "Total",
    categories = "Total"
  ), class = "BannerVar"), allpets = structure(list(
    alias = "allpets", name = "All pets owned", type = "multiple_response",
    old_categories = c("Cat", "Dog", "Bird"), categories_out = c(
      "Cat",
      "Dog", "Bird"
    ), categories = c("Cat", "Dog", "Bird")
  ), class = "BannerVar"))

  mockery::stub(getMultitable, "multitables", list("c4cae4937918ee0" = "this"))
  res <- getMultitable(banner_flatten, ds)
  expect_equal(res, "this")
})
#

context("tabBook_crunchtabs")

test_that("tabBook_crunchtabs", {
  mockery::stub(tabBook_crunchtabs, "tabBookSingle_crunchtabs", "Successful Single Run")
  mockery::stub(tabBook_crunchtabs, "tabBookMulti_crunchtabs", "Successful Multi Run")
  mockery::stub(tabBook_crunchtabs, "crunch::weight", NULL)
  expect_error(tabBook_crunchtabs("", "dataset", weight = 1), "weight must be NULL")
  expect_equal(tabBook_crunchtabs("", "dataset", weight = NULL), "Successful Single Run")
  expect_equal(tabBook_crunchtabs("", "dataset", weight = data.frame()), "Successful Multi Run")
})


context("tabBookSingle_crunchtabs")

test_that("tabBookSingle_crunchtabs", {
  mockery::stub(tabBookSingle_crunchtabs, "name", "hello")
  mockery::stub(tabBookSingle_crunchtabs, "self", "some_url")
  mockery::stub(tabBookSingle_crunchtabs, "crunch::shojiURL", "shoji_url")
  mockery::stub(tabBookSingle_crunchtabs, "download_result", "downloaded_result")
  mockery::stub(tabBookSingle_crunchtabs, "varFilter", "Doesn't matter!")
  mockery::stub(tabBookSingle_crunchtabs, "tabBookResult", function(x) x)
  mockery::stub(tabBookSingle_crunchtabs, "crunch::crPOST", function(x, ...) x)
  res <- tabBookSingle_crunchtabs("mt", "dataset", weight = NULL)
  expect_equal(res, new("TabBookResult",
    .Data = list("downloaded_result", list()),
    names = c(NA, "sheets")
  ))
})

context("tabBookMulti_crunchtabs")

test_that("tabBookMulti_crunchtabs errors", {
  expect_error(
    tabBookMulti_crunchtabs(
      multitable = "mult",
      dataset = "",
      weight_spec = character(0),
      append_default_wt = TRUE
    ),
    "Empty list not allowed as a weight spec"
  )

  expect_error(
    tabBookMulti_crunchtabs(
      multitable = "mult",
      dataset = "",
      weight_spec = data.frame("notalias" = 1, "notweight" = 2),
      append_default_wt = TRUE
    ),
    "if weight_spec is a data.frame"
  )
  expect_error(
    tabBookMulti_crunchtabs(
      multitable = "mult",
      dataset = "",
      weight_spec = data.frame(alias = c("a", "a"), weight = c("b", "b")),
      append_default_wt = TRUE
    ),
    "Found duplicate weight and alias combinations in weight_spec"
  )
})

test_that("tabBookMulti_crunchtabs E-E", {
  ds <- readRDS(test_path("fixtures/recontact_dataset.rds"))
  weight_spec <- list(
    weight1 = c(q11 = "q1_pre", country1 = "country_pre"),
    weight2 = c(q12 = "q1_post", country2 = "country_post")
  )

  mockery::stub(
    tabBookMulti_crunchtabs, "getCatalog",
    readRDS(test_path("fixtures/tabBookMulti_crunchtabs-getCatalog.rds"))
  )

  book <- readRDS(test_path("fixtures/tabBookMulti_crunchtabs-tabBookSingle_crunchtabs.rds"))

  mockery::stub(tabBookMulti_crunchtabs, "tabBookSingle_crunchtabs", book)
  res <- tabBookMulti_crunchtabs(
    "mult", ds[c("q1_pre", "q1_post", "country_pre", "country_post", "weight1", "weight2")],
    weight_spec = weight_spec,
    append_default_wt = FALSE
  )

  expect_equal(length(res), 4)
  # TODO: Add more direct tests
})

context("row_data")

test_that("row_data crosstabs coverage", {
  d <- readRDS(test_path("fixtures/row_data-data.rds"))
  res <- row_data(d, 1, TRUE, FALSE, FALSE)
  expect_equal(
    res$`___total___`,
    structure(c(0.454545454545455, 0.272727272727273, 0.272727272727273), .Dim = c(3L, 1L), .Dimnames = list(
      c("Cat", "Dog", "Bird"),
      "Total"
    ))
  )
})
