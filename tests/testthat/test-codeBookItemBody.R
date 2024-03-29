context("codeBookItemBody")

test_that("codeBookItemBody CategoricalVariable", {
  ds <- readRDS(test_path("fixtures/example_dataset.rds"))
  smry <- structure(list(id = c("1", "2", "3", "8", "9"), name = c(
    "Cat",
    "Dog", "Bird", "Skipped", "Not Asked"
  ), n = c(
    "6", "4", "3",
    "3", "4"
  )), class = "data.frame", row.names = c(NA, -5L))
  mockery::stub(
    codeBookItemBody.CategoricalVariable,
    "codeBookSummary.CategoricalVariable", smry
  )
  res <- codeBookItemBody(ds$q1)

  expect_equal(
    attributes(res)$kable_meta$contents,
    c(
      "\\{Code\\} & \\{Label\\} & \\{Count\\}",
      "1 & Cat & 6", "2 & Dog & 4", "3 & Bird & 3",
      "8 & Skipped & 3", "9 & Not Asked & 4"
    )
  )

  expect_equal(
    attributes(res)$kable_meta$align_vector_origin,
    c("J", "l", "J")
  )
})


test_that("codeBookItemBody long label CategoricalVariable", {

  ds <- readRDS(test_path("fixtures/example_dataset.rds"))
  smry <- structure(list(id = c("1", "2", "3", "8", "9"), name = c(
    paste0(rep("OMG Cats!", 9), collapse = " "),
    "Dog", "Bird", "Skipped", "Not Asked"
  ), n = c(
    "6", "4", "3",
    "3", "4"
  )), class = "data.frame", row.names = c(NA, -5L))
  mockery::stub(
    codeBookItemBody.CategoricalVariable,
    "codeBookSummary.CategoricalVariable", smry
  )
  res <- codeBookItemBody(ds$q1)

  expect_equal(
    attributes(res)$kable_meta$contents,
    c(
      "\\{Code\\} & \\{Label\\} & \\{Count\\}",
      "1 & OMG Cats! OMG Cats! OMG Cats! OMG Cats! OMG Cats! OMG Cats! OMG Cats! OMG Cats! OMG Cats! & 6", "2 & Dog & 4", "3 & Bird & 3",
      "8 & Skipped & 3", "9 & Not Asked & 4"
    )
  )

  expect_equal(
    attributes(res)$kable_meta$align_vector_origin,
    c("J", "l", "J")
  )
  expect_true(grepl("5.25in", res))
})

test_that("codeBookItemBody CategoricalVariable", {
  set.seed(42)
  ds <- readRDS(test_path("fixtures/example_dataset.rds"))
  smry <- structure(list(
    id = as.character(1:22),
    name = letters[1:22],
    n = sample(1:6, 22, replace = TRUE)
  ), class = "data.frame", row.names = c(NA, -22L))
  mockery::stub(
    codeBookItemBody.CategoricalVariable,
    "codeBookSummary.CategoricalVariable", smry
  )
  res <- codeBookItemBody(ds$q1)

  expect_equal(
    attributes(res)$kable_meta$contents[2:12],
    c(
      "1 & a & 1 &  & 2 & b & 5", "3 & c & 1 &  & 4 & d & 1", "5 & e & 2 &  & 6 & f & 4",
      "7 & g & 2 &  & 8 & h & 2", "9 & i & 1 &  & 10 & j & 4", "11 & k & 1 &  & 12 & l & 5",
      "13 & m & 6 &  & 14 & n & 4", "15 & o & 2 &  & 16 & p & 2", "17 & q & 3 &  & 18 & r & 1",
      "19 & s & 1 &  & 20 & t & 3", "21 & u & 4 &  & 22 & v & 5"
    )
  )

  expect_equal(
    attributes(res)$kable_meta$align_vector_origin,
    c("K", "l", "J", "c", "K", "l", "J")
  )
})



test_that("codeBookItemBody CategoricalArrayVariable", {
  ds <- readRDS(test_path("fixtures/example_dataset.rds"))
  smry <- structure(list(c("petloc_home", "petloc_work"), c("Home", "Work"),
    `1 Cat` = c(5, 6), `2 Dog` = c(3, 4), `3 Bird` = c(3, 6),
    `8 Skipped` = c(4, 3), `9 Not Asked` = c(5, 1)
  ),
  row.names = c(NA, -2L), class = "data.frame"
  )

  mockery::stub(
    codeBookItemBody.CategoricalArrayVariable,
    "codeBookSummary.CategoricalArrayVariable", smry
  )
  res <- codeBookItemBody(ds$petloc)
  expect_equal(
    attributes(res$kcounts)$kable_meta$contents,
    c(
      "\\{Variable\\} & \\{1\\} & \\{2\\} & \\{3\\} & \\{8\\} & \\{9\\}",
      "\\\\ttfamily\\{petloc\\\\_home\\} & 5 & 3 & 3 & 4 & 5", "\\\\ttfamily\\{petloc\\\\_work\\} & 6 & 4 & 6 & 3 & 1" # nolint
    )
  )

  expect_equal(
    attributes(res$kcounts)$kable_meta$align_vector_origin,
    c("l", "J", "J", "J", "J", "J")
  )
})

test_that("codeBookItemBody CategoricalArrayVariable long label adjustments", {
  ds <- readRDS(test_path("fixtures/example_dataset.rds"))
  smry <- structure(list(c("petloc_home", "petloc_work"), c(
    "This is an extremely, absurdley long label. Because we often like to present categories that should really be Yes or No questions.", # nolint
    "This is another absurdely, extremely long label. Because we often like to present categories that should really be Yes or No questions." # nolint
  ),
  `1 Cat` = c(5, 6), `2 Dog` = c(3, 4), `3 Bird` = c(3, 6),
  `8 Skipped` = c(4, 3), `9 Not Asked` = c(5, 1)
  ),
  row.names = c(NA, -2L), class = "data.frame"
  )

  mockery::stub(
    codeBookItemBody.CategoricalArrayVariable,
    "codeBookSummary.CategoricalArrayVariable", smry
  )
  res <- codeBookItemBody(ds$petloc)
  expect_equal(
    attributes(res$kcounts)$kable_meta$contents,
    c(
      "\\{Variable\\} & \\{1\\} & \\{2\\} & \\{3\\} & \\{8\\} & \\{9\\}",
      "\\\\ttfamily\\{petloc\\\\_home\\} & 5 & 3 & 3 & 4 & 5",
      "\\\\ttfamily\\{petloc\\\\_work\\} & 6 & 4 & 6 & 3 & 1"
    )
  )
  expect_true(
    grepl("\\begin{longtable}[l]{l>{\\raggedright\\arraybackslash}p{4.75in}}",
          res$krows, fixed = TRUE)
  )
})


test_that("codeBookItemBody MultipleResponseVariable", {
  ds <- readRDS(test_path("fixtures/example_dataset.rds"))
  smry <- structure(list(c("allpets_1", "allpets_2", "allpets_3"),
    c("Cat", "Dog", "Bird"),
    `1 selected` = c(4, 5, 5), `2 not selected` = c(
      4,
      3, 6
    ), `8 skipped` = c(4, 4, 6), `9 not asked` = c(8, 8, 3)
  ), row.names = c(
    NA,
    -3L
  ), class = "data.frame")

  mockery::stub(
    codeBookItemBody.MultipleResponseVariable,
    "codeBookSummary.MultipleResponseVariable",
    smry
  )
  res <- codeBookItemBody(ds$allpets)
  # print(attributes(res$kcounts)$kable_meta$contents)
  # print("\n\n")
  # print(attributes(res$kcounts)$kable_meta$align_vector_origin)
  expect_equal(
    attributes(res$kcounts)$kable_meta$contents,
    c(
      "\\{Variable\\} & \\{1\\} & \\{2\\} & \\{8\\} & \\{9\\}",
      "\\\\ttfamily\\{allpets\\\\_1\\} & 4 & 4 & 4 & 8",
      "\\\\ttfamily\\{allpets\\\\_2\\} & 5 & 3 & 4 & 8",
      "\\\\ttfamily\\{allpets\\\\_3\\} & 5 & 6 & 6 & 3"
    )
  )
  expect_equal(
    attributes(res$kcounts)$kable_meta$align_vector_origin,
    c("l", "J", "J", "J", "J")
  )
})

test_that("codeBookItemBody NumericVariable", {
  ds <- readRDS(test_path("fixtures/example_dataset.rds"))
  smry <- structure(
    list(
      Mean = 2, SD = 1, Min = 0, Max = 6, n = 16, Missing = 4
    ),
    class = "data.frame", row.names = c(NA, -1L)
  )
  mockery::stub(codeBookItemBody.NumericVariable, "codeBookSummary.NumericVariable", smry)

  res <- codeBookItemBody(ds$ndogs)

  expect_equal(
    attributes(res)$kable_meta$contents,
    c(
      "\\{Mean\\} & \\{SD\\} & \\{Min\\} & \\{Max\\} & \\{n\\} & \\{Missing\\}",
      "2 & 1 & 0 & 6 & 16 & 4"
    )
  )

  expect_equal(
    attributes(res)$kable_meta$align_vector_origin,
    rep("c", 6)
  )
})


test_that("codeBookItemBody DatetimeVariable", {
  ds <- readRDS(test_path("fixtures/example_dataset.rds"))
  smry <- structure(list(
    Filled = 20L, Missing = 0L,
    Range = "[2014-12-01, 2015-01-01]"
  ),
  class = "data.frame",
  row.names = c(NA, -1L)
  )

  mockery::stub(codeBookItemBody.DatetimeVariable, "codeBookSummary.DatetimeVariable", smry)
  res <- codeBookItemBody(ds$wave)
  expect_true(grepl("\\[2014-12-01, 2015-01-01\\]", attributes(res)$kable_meta$contents[2], fixed = T))
  expect_equal(
    attributes(res)$kable_meta$contents[1],
      "Filled & Missing & Range"
  )

  expect_equal(
    attributes(res)$kable_meta$align_vector_origin,
    c("c", "c", "c")
  )
})

test_that("codeBookItemBody TextVariable", {
  ds <- readRDS(test_path("fixtures/example_dataset.rds"))
  smry <- structure(
    list(
      Filled = 16L, Missing = 4L, `Max Length` = 8L
    ),
    class = "data.frame",
    row.names = c(NA, -1L)
  )

  mockery::stub(codeBookItemBody.TextVariable, "codeBookSummary.TextVariable", smry)
  res <- codeBookItemBody(ds$q3)

  expect_equal(
    attributes(res)$kable_meta$contents,
    c(
      "\\{Filled\\} & \\{Missing\\} & \\{Max Length\\}",
      "16 & 4 & 8"
    )
  )

  expect_equal(
    attributes(res)$kable_meta$align_vector_origin,
    c("c", "c", "c")
  )
})

context("codeBookItemBody")

test_that("codeBookItemBody defaults fails as expected", {
  x <- 1:5
  class(x) <- "NotAnAcceptedClass"
  expect_error(codeBookItemBody.default(x))
})

test_that("codeBookItemBody works for factors", {
  x <- factor(sample(letters[1:5], 100, r = T))
  mockery::stub(codeBookItemBody.default, "codeBookItemBody.factor", TRUE)
  expect_true(codeBookItemBody.default(x, meta = data.frame()))
})

test_that("codeBookItemBody works for characters", {

  x <- sample(letters[1:5], 100, replace = T)
  mockery::stub(codeBookItemBody.default, "codeBookItemBody.character", TRUE)
  expect_true(codeBookItemBody.default(x, meta = data.frame()))
})

test_that("codeBookItemBody works for numeric", {
  x <- rnorm(100)
  mockery::stub(codeBookItemBody.default, "codeBookItemBody.numeric", TRUE)
  expect_true(codeBookItemBody.default(x, meta = data.frame()))
})

context("codeBookSummary Mean > 9999")

test_that("codeBookItemBody.NumericVariabl handles big numbers with scientific formatting", {
  smry <- data.frame(Mean = 99999, SD = 1000, Min = 0, Max = 150000)
  mockery::stub(codeBookItemBody.NumericVariable, "codeBookSummary", smry)
  res <- codeBookItemBody.NumericVariable("fake")
  expect_equal(
    capture.output(res),
    c("", "\\begin{longtable}[l]{cccc}", "\\toprule", "{Mean} & {SD} & {Min} & {Max}\\\\",
    "\\midrule", "1e+05 & 1e+03 & 0e+00 & 1.5e+05\\\\", "\\bottomrule",
    "\\end{longtable}")
  )
})
