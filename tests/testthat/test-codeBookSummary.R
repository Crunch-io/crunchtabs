context("codeBookSummary")

test_that("codeBookSummary correct for CategoricalVariable", {
  options("crunchtabs.codebook.suppress.zeros" = TRUE)
  ds <- readRDS(test_path("fixtures/example_dataset.rds"))
  cats <- new("Categories", .Data = list(
    new("Category", .Data = list(
      1L,
      FALSE, "Cat", 1L
    ), names = c("id", "missing", "name", "numeric_value")),
    new("Category", .Data = list(2L, FALSE, "Dog", 2L), names = c(
      "id",
      "missing", "name", "numeric_value"
    )), new("Category", .Data = list(
      3L, FALSE, "Bird", 3L
    ), names = c(
      "id", "missing", "name",
      "numeric_value"
    )), new("Category", .Data = list(
      8L, TRUE, "Skipped",
      8L
    ), names = c("id", "missing", "name", "numeric_value")),
    new("Category",
      .Data = list(9L, TRUE, "Not Asked", 9L),
      names = c("id", "missing", "name", "numeric_value")
    ),
    new("Category",
      .Data = list(-1L, TRUE, "No Data", NULL),
      names = c("id", "missing", "name", "numeric_value")
    )
  ))

  tab <- structure(c(Cat = 6, Dog = 4, Bird = 3), .Dim = 3L, .Dimnames = list(
    q1 = c("Cat", "Dog", "Bird")
  ), class = "table")

  mockery::stub(where = codeBookSummary.CategoricalVariable, what = "crunch::categories",
                how = function(...) cats)
  mockery::stub(codeBookSummary.CategoricalVariable, "crunch::table", function(...) tab)

  r <- suppressWarnings(codeBookSummary(ds$q1))
  expect_equal(
    r,
    structure(
      list(
        id = c("1", "2", "3"),
        name = c("Cat", "Dog", "Bird"),
        n = c("6", "4", "3")
      ),
      class = "data.frame", row.names = c(NA, -3L)
    )
  )
})


test_that("codeBookSummary correct for CategoricalVariable", {
  ds <- readRDS(test_path("fixtures/example_dataset.rds"))
  cats <- new("Categories", .Data = list(
    new("Category", .Data = list(
      1L,
      FALSE, "selected", NULL
    ), names = c("id", "missing", "name", "numeric_value")),
     new("Category", .Data = list(
      3L, FALSE, "not selected", NULL
    ), names = c(
      "id", "missing", "name",
      "numeric_value"
    )), new("Category", .Data = list(
      8L, TRUE, "Skipped",
      8L
    ), names = c("id", "missing", "name", "numeric_value")),
    new("Category",
        .Data = list(9L, TRUE, "Not Asked", 9L),
        names = c("id", "missing", "name", "numeric_value")
    ),
    new("Category",
        .Data = list(-1L, TRUE, "No Data", NULL),
        names = c("id", "missing", "name", "numeric_value")
    )
  ))

  tab <- structure(c("selected" = 6, "not selected" = 4), .Dim = 2L, .Dimnames = list(
    q1 = c("selected", "not selected")
  ), class = "table")

  mockery::stub(where = codeBookSummary.CategoricalVariable, what = "crunch::categories",
                how = function(...) cats)
  mockery::stub(codeBookSummary.CategoricalVariable, "crunch::table", function(...) tab)

  r <- suppressWarnings(codeBookSummary(ds$q1))
  expect_equal(
    r,
    structure(list(id = c("1", "3"), name = c("selected", "not selected"
    ), n = c("6", "4")), row.names = 1:2, class = "data.frame")
  )
})


test_that("codeBookSummary correct for MultipleResponseVariable", {

  ds <- readRDS(test_path("fixtures/example_dataset.rds"))

  cats <- new("Categories", .Data = list(
    new("Category", .Data = list(
      1L,
      FALSE, "Cat", 1L
    ), names = c("id", "missing", "name", "numeric_value")),
    new("Category", .Data = list(2L, FALSE, "Dog", 2L), names = c(
      "id",
      "missing", "name", "numeric_value"
    )), new("Category", .Data = list(
      3L, FALSE, "Bird", 3L
    ), names = c(
      "id", "missing", "name",
      "numeric_value"
    )), new("Category", .Data = list(
      8L, TRUE, "Skipped",
      8L
    ), names = c("id", "missing", "name", "numeric_value")),
    new("Category",
      .Data = list(9L, TRUE, "Not Asked", 9L),
      names = c("id", "missing", "name", "numeric_value")
    ),
    new("Category",
      .Data = list(-1L, TRUE, "No Data", NULL),
      names = c("id", "missing", "name", "numeric_value")
    )
  ))

  fakedf <- data.frame("Cat" = 1, "Dog" = 2, "Bird" = 3)
  tab <- structure(list(name = c(
    "selected", "not selected", "skipped",
    "not asked"
  ), id = c(1L, 2L, 8L, 9L), missing = c(
    FALSE, FALSE,
    TRUE, TRUE
  ), value = c(1L, 2L, 8L, 9L), drop = c(
    "TRUE", "FALSE",
    "8", "9"
  ), n = c(1, 2, 8, 9)), row.names = c(2L, 1L, 4L, 3L), class = "data.frame")

  mockery::stub(
    codeBookSummary.MultipleResponseVariable, "crunch::categories", function(...) cats)
  mockery::stub(
    codeBookSummary.MultipleResponseVariable, "crunch::subvariables", function(...) fakedf)
  mockery::stub(
    codeBookSummary.MultipleResponseVariable, "codeBookSummary", function(...) tab, depth = 5)
  mockery::stub(
    codeBookSummary.MultipleResponseVariable, "names", c("Cat", "Dog", "Bird"))

  r <- codeBookSummary.MultipleResponseVariable(ds$allpets)

  # Trade off on simplicity for getting subvar names
  # Because names will always return Cat Dog and Bird
  expect_equal(
    r,
    structure(
      list(c("Cat", "Dog", "Bird"), c(
        "Cat",
        "Dog", "Bird"
      ),
      `1 selected` = rep(1, 3),
      `2 not selected` = rep(2, 3), `8 skipped` = rep(8, 3),
      `9 not asked` = rep(9, 3)
      ),
      row.names = c(NA, -3L),
      class = "data.frame"
    )
  )
})

test_that("codeBookSummary correct for NumericVariable", {
  ds <- readRDS(test_path("fixtures/example_dataset.rds"))

  mockery::stub(
    codeBookSummary.NumericVariable, "as.vector",
    c(1, NA, 2, 3, 1, 2, 2, 3, 2, 2, 2, NA, 3, 0, 6, 1, NA, 0, NA, 2)
  )

  r <- codeBookSummary(ds$ndogs)
  expect_equal(
    r,
    structure(list(
      Mean = 2, SD = 1, Min = 0, Max = 6,
      n = 16, Missing = 4
    ),
    class = "data.frame",
    row.names = c(NA, -1L)
    )
  )
})

test_that("codeBookSummary correct for DatetimeVariable", {
  ds <- readRDS(test_path("fixtures/example_dataset.rds"))

  dates <- structure(c(
    16405, 16405, 16405, 16405, 16405, 16405, 16405,
    16405, 16405, 16405, 16436, 16436, 16436, 16436, 16436, 16436,
    16436, 16436, 16436, 16436
  ), class = "Date")

  mockery::stub(codeBookSummary.DatetimeVariable, "as.vector", dates)
  r <- codeBookSummary(ds$wave)

  expect_equal(
    r,
    structure(
      list(
        Filled = 20L, Missing = 0L,
        Range = "[2014-12-01, 2015-01-01]"
      ),
      class = "data.frame", row.names = c(NA, -1L)
    )
  )
})

test_that("codeBookSummary correct for TextVariable", {
  ds <- readRDS(test_path("fixtures/example_dataset.rds"))
  mockery::stub(
    codeBookSummary.TextVariable, "as.vector",
    c(
      "Jasmine", "Clyde", "Geoffrey", "Spike", "Zeus", "Teddy", "Ali",
      "Hugo", "Snoopy", "Lady", "Biscuit", NA, "Daisy", "Doug", NA,
      "Fluffy", NA, NA, "Felix", "Rocky"
    )
  )
  r <- codeBookSummary(ds$q3)

  expect_equal(
    r,
    structure(list(Filled = 16L, Missing = 4L, `Max Length` = 8L),
      class = "data.frame", row.names = c(NA, -1L)
    )
  )
})

test_that("Fails when passed a garbage object", {
  expect_error(codeBookSummary("Hello"), "The expected class for")
})

context("scolumnAlign")

test_that("scolumnAlign works as expected", {
  k <- data.frame(a = c(1, 2, 3, 4), b = rep(NA, 4))
  align <- scolumnAlign(k, c("d", "d"))
  expect_equal(align, c("J", "K"))
  align <- scolumnAlign(k, c("d", "l"))
  expect_equal(align, c("J", "l"))
})

test_that("scolumnAlign uses S when maxnchar > 6", {
  k <- data.frame(a = c("aasdfasdfadsf", "asfhashfjkldsafjdk",
                        "sahsdfadjkfladsfhklf", "asfhjasfjklhsaf"),
                  b = rep(NA, 4), stringsAsFactors = F)
  align <- scolumnAlign(k, c("d", "l"))
  expect_equal(align, c("S[table-format=20]", "l"))
})

test_that("codeBookSummary categoricalVariable, multpleResponse type", {
  mockery::stub(
    codeBookSummary.CategoricalVariable,
    "crunch::categories(x)",
    NULL
  )

  responses <- structure(list(2L, 1L, 9L, 8L, -1L, FALSE, FALSE, TRUE, TRUE,
    TRUE, "not selected", "selected", "not asked", "skipped",
    "No Data", NULL, NULL, 9L, 8L, NULL, FALSE, TRUE, 9L, 8L,
    -1L), .Dim = c(5L, 5L), .Dimnames = list(NULL, c("id", "missing",
    "name", "numeric_value", "selected")))

  mockery::stub(
    codeBookSummary.CategoricalVariable,
    "do.call",
    responses
  )


})
