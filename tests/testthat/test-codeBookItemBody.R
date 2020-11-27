context("codeBookItemBody")

test_that("codeBookItemBody CategoricalVariable", {
  ds <- readRDS(test_path("fixtures/example_dataset.rds"))
  smry <- structure(list(id = c("1", "2", "3", "8", "9"), name = c("Cat", 
    "Dog", "Bird", "Skipped", "Not Asked"), n = c("6", "4", "3", 
    "3", "4")), class = "data.frame", row.names = c(NA, -5L))
  mockery::stub(
    codeBookItemBody.CategoricalVariable, 
    "codeBookSummary.CategoricalVariable", smry
  )
  res = codeBookItemBody(ds$q1)
  
  expect_equal(
    attributes(res)$kable_meta$contents,
    c("\\{Code\\} & \\{Label\\} & \\{Count\\}",
      "1 & Cat & 6", "2 & Dog & 4", "3 & Bird & 3",
      "8 & Skipped & 3", "9 & Not Asked & 4")
  )
  
  expect_equal(
    attributes(res)$kable_meta$align_vector_origin,
    c("J", "l", "J")
  )
})

test_that("codeBookItemBody CategoricalArrayVariable", {
  ds <- readRDS(test_path("fixtures/example_dataset.rds"))
  smry <- structure(list(c("petloc_home", "petloc_work"), c("Home", "Work"
  ), `1 Cat` = c(5, 6), `2 Dog` = c(3, 4), `3 Bird` = c(3, 6), 
  `8 Skipped` = c(4, 3), `9 Not Asked` = c(5, 1)), 
  row.names = c(NA, -2L), class = "data.frame")
  
  mockery::stub(codeBookItemBody.CategoricalArrayVariable, 
       "codeBookSummary.CategoricalArrayVariable", smry)
  res <- codeBookItemBody(ds$petloc)
  expect_equal(
    attributes(res$kcounts)$kable_meta$contents,
    c("\\{Variable\\} & \\{1\\} & \\{2\\} & \\{3\\} & \\{8\\} & \\{9\\}",
      "\\\\ttfamily\\{petloc\\\\_home\\} & 5 & 3 & 3 & 4 & 5", "\\\\ttfamily\\{petloc\\\\_work\\} & 6 & 4 & 6 & 3 & 1"
    )
  )
  
  expect_equal(
    attributes(res$kcounts)$kable_meta$align_vector_origin,
    c("l", "J", "J", "J", "J", "J")
  )
})


test_that("codeBookItemBody MultipleResponseVariable", {
  ds <- readRDS(test_path("fixtures/example_dataset.rds"))
  smry <- structure(list(c("allpets_1", "allpets_2", "allpets_3"), 
     c("Cat", "Dog", "Bird"), `1 selected` = c(4, 5, 5), `2 not selected` = c(4, 
        3, 6), `8 skipped` = c(4, 4, 6), `9 not asked` = c(8, 8, 3)), row.names = c(NA, 
      -3L), class = "data.frame")
  
  mockery::stub(
    codeBookItemBody.MultipleResponseVariable, 
    "codeBookSummary.MultipleResponseVariable", 
    smry, depth = 2)
  res = codeBookItemBody(ds$allpets)
  expect_equal(
    attributes(res$kcounts)$kable_meta$contents,
    c("\\{Variable\\} & \\{1\\} & \\{2\\} & \\{8\\} & \\{9\\}",
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
      Mean = 2, SD = 1, Min = 0, Max = 6, n = 16, Missing = 4), 
    class = "data.frame", row.names = c(NA, -1L)
  )
  mockery::stub(codeBookItemBody.NumericVariable, "codeBookSummary.NumericVariable", smry)
  
  res = codeBookItemBody(ds$ndogs)
  
  expect_equal(
    attributes(res)$kable_meta$contents,
    c(
      "\\{Mean\\} & \\{SD\\} & \\{Min\\} & \\{Max\\} & \\{n\\} & \\{Missing\\}",
      "2 & 1 & 0 & 6 & 16 & 4")
  )
  
  expect_equal(
    attributes(res)$kable_meta$align_vector_origin,
    rep("c", 6)
  )
})


test_that("codeBookItemBody DatetimeVariable", {
  ds <- readRDS(test_path("fixtures/example_dataset.rds"))
  smry = structure(list(
    Filled = 20L, Missing = 0L, 
    Range = "[2014-12-01, 2015-01-01]"), class = "data.frame", 
    row.names = c(NA, -1L))
  
  mockery::stub(codeBookItemBody.DatetimeVariable, "codeBookSummary.DatetimeVariable", smry)
  res = codeBookItemBody(ds$wave)
  
  expect_equal(
    attributes(res)$kable_meta$contents,
    c(
      "Filled & Missing & Range",
      "20 & 0 & \\[2014-12-01, 2015-01-01\\]"
    )
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
      Filled = 16L, Missing = 4L, `Max Length` = 8L), class = "data.frame", 
    row.names = c(NA, -1L))
  
  mockery::stub(codeBookItemBody.TextVariable, "codeBookSummary.TextVariable", smry)
  res <- codeBookItemBody(ds$q3)
  
  expect_equal(
    attributes(res)$kable_meta$contents,
    c("\\{Filled\\} & \\{Missing\\} & \\{Max Length\\}",
      "16 & 4 & 8"
    )
  )
  
  expect_equal(
    attributes(res)$kable_meta$align_vector_origin,
    c("d", "d", "d")
  )
})
