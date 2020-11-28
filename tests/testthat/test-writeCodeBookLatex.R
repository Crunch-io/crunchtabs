context("writeCodeBookLatex")

test_that("End to end writeCodeBookLatex", {
  ds <- readRDS(test_path("fixtures/example_dataset.rds"))
  
  mockery::stub(
    writeCodeBookLatex, 
    "codeBookItemBody", 
    readRDS(test_path("fixtures/codeBookItem_allpets.rds"))
  )
  
  mockery::stub(writeCodeBookLatex, "file.open", NULL)

  res <- suppressWarnings(
    writeCodeBookLatex(
      ds[c("allpets")],
      title = "Hello",
      subtitle = "Goodbye",
      sample_desc = "US Voting Adults",
      logo = "yougov",
      pdf = TRUE)
  )
  tex <- readLines("Example-dataset.tex")
  expect_equal(res, NULL)
  expect_equal(length(tex), 149)
  expect_true(file.size("Example-dataset.pdf") > 61200)
  # Test title
  expect_true(
    any(grepl("fancyhead*.*Hello", tex))
  )
  # Test subtitle
  expect_true(
    any(grepl("fancyhead*.*Goodbye", tex))
  )
  # Test sample description 
  expect_true(
    any(grepl("US Voting Adults", tex))
  )
  # Test logo
  expect_true(
    any(grepl("includegraphics*.*YouGov", tex))
  ) 
  # Test allpets header
  allpets <- "\\addcontentsline{lot}{table}{\\parbox{1.8in}{\\ttfamily{allpets}} All pets owned}"
  expect_true(
    any(grepl(allpets, tex, fixed = TRUE))
  )
  expect_true(file.exists("Example-dataset.tex"))
  expect_true(file.exists("Example-dataset.pdf"))
  expect_true(file.remove("Example-dataset.tex"))
  expect_true(file.remove("Example-dataset.pdf"))
})

test_that("Dataset name as title if title not specified", {
  ds <- readRDS(test_path("fixtures/example_dataset.rds"))
  
  mockery::stub(
    writeCodeBookLatex, 
    "codeBookItemBody", 
    readRDS(test_path("fixtures/codeBookItem_allpets.rds"))
  )
  
  mockery::stub(
    writeCodeBookLatex,
    "crunch::name",
    "AnAmazingTitle"
  )
  
  res <- suppressWarnings(
    writeCodeBookLatex(
      ds[c("allpets")],
      title = NULL,
      subtitle = NULL,
      sample_desc = "US Voting Adults",
      logo = "yougov",
      pdf = FALSE)
  )
  tex <- readLines("Example-dataset.tex")
  expect_true(
    any(grepl("AnAmazingTitle", tex))
  )

  expect_true(file.exists("Example-dataset.tex"))
  expect_true(file.remove("Example-dataset.tex"))
})

test_that("Dataset name as title if title not specified", {
  ds <- readRDS(test_path("fixtures/example_dataset.rds"))
  
  mockery::stub(
    writeCodeBookLatex, 
    "codeBookItemBody", 
    readRDS(test_path("fixtures/codeBookItem_allpets.rds"))
  )
  
  mockery::stub(
    writeCodeBookLatex,
    "crunch::name",
    "AnAmazingTitle"
  )
  
  res <- suppressWarnings(
    writeCodeBookLatex(
      ds[c("allpets")],
      title = NULL,
      subtitle = NULL,
      sample_desc = "US Voting Adults",
      logo = "ygblue",
      pdf = FALSE)
  )
  tex <- readLines("Example-dataset.tex")
  expect_true(
    any(grepl("AnAmazingTitle", tex))
  )
  # Test ygblue logo while we're here
  expect_true(
    any(grepl("includegraphics*.*YouGovBlue", tex))
  )  
  
  expect_true(file.exists("Example-dataset.tex"))
  expect_true(file.remove("Example-dataset.tex"))
})


test_that("Appendices are positioned as expected", {
  ds <- readRDS(test_path("fixtures/example_dataset.rds"))
  
  # inputregstate comes from DFP
  # ds <- loadDataset("Data for Progress National Issues Survey Wave 4")
  # codeBookItemBody(ds$inputregstate) %>% 
  #   dput %>% 
  #   saveRDS("tests/testthat/fixtures/codeBookItem_inputregstate")
  
  mockery::stub(
    writeCodeBookLatex, 
    "codeBookItemBody", 
    readRDS(test_path("fixtures/codeBookItem_inputregstate.rds"))
  )
  
  mockery::stub(
    writeCodeBookLatex,
    "crunch::name",
    "AnAmazingTitle"
  )
  
  res <- suppressWarnings(
    writeCodeBookLatex(
      ds[c("allpets")],
      sample_desc = "US Voting Adults",
      appendix = TRUE, 
      pdf = TRUE)
  )
  
  tex <- readLines("Example-dataset.tex")
    # Test ygblue logo while we're here
  
  # Test no logo
  expect_true(
    !any(grepl("includegraphics", tex))
  )  
  
  # We expect Appendix title to be found on 100th line or greater
  expect_true({
    which(grepl("\\fancyhead[L]{{\\fontsize{16}{24}\\textbf{Appendix}}}", tex, fixed = TRUE)) > 100
  })
  
  expect_true(file.exists("Example-dataset.tex"))
  expect_true(file.remove("Example-dataset.tex"))
})

test_that("Position functions as expected", {
  ds <- readRDS(test_path("fixtures/example_dataset.rds"))
  
  # ds <- loadDataset("Example dataset")
  # codeBookItemBody(ds$allpets) %>% dput() %>% 
  #   saveRDS("tests/testthat/fixtures/codeBookItem_allpets.rds")
  
  mockery::stub(
    writeCodeBookLatex, 
    "codeBookItemBody", 
    readRDS(test_path("fixtures/codeBookItem_allpets.rds"))
  )
  
  mockery::stub(
    writeCodeBookLatex,
    "crunch::name",
    "AnAmazingTitle"
  )
  
  suppressWarnings(
    writeCodeBookLatex(
      ds[c("allpets")],
      sample_desc = "US Voting Adults",
      position = "c", 
      pdf = TRUE)
  )
  
  tex <- readLines("Example-dataset.tex")
  # Test ygblue logo while we're here
  
  # Test no logo
  expect_true(
    !any(grepl("includegraphics", tex))
  )  
  
  # We expect Appendix title to be found on 100th line or greater
  expect_true({
    which(grepl("\\fancyhead[L]{{\\fontsize{16}{24}\\textbf{Appendix}}}", tex, fixed = TRUE)) > 100
  })
  
  expect_true(file.exists("Example-dataset.tex"))
  expect_true(file.remove("Example-dataset.tex"))
})

# with_api_fixture("fixtures-1-2-5", {
#   ds = crunch::loadDataset("Example dataset")
# 
#   test_that("Default tex as expected", {
#     suppressWarnings(writeCodeBookLatex(ds, pdf = FALSE))
#     tex <- readLines("Example-dataset.tex")
#     original <- readRDS("fixtures/writeCodeBookLatexFull.rds")
#     # expect_true(length(tex) == length(original))
#     expect_true(sum(tex %in% original)/length(tex) > 0.98)
#   })
# })
# 
# with_api_fixture("fixtures-1-2-5", {
#   ds = loadDataset(
#     "https://app.crunch.io/dataset/10c3c3/"
#   )
# 
#   test_that("Default tex as expected", {
#     suppressWarnings(writeCodeBookLatex(
#       ds[1],
#       url = "https://app.crunch.io/dataset/10c3c3/",
#       appendix = TRUE, suppress_zero_counts = FALSE, pdf = FALSE)
#     )
#     tex <- readLines(test_path("fixtures/Data-for-Progress-National-Issues-Survey----Foreign-Policy.tex"))
#     original <- readRDS("fixtures/writeCodeBookLatexLongCat.rds")
#     expect_true(length(tex) == length(original))
#     expect_true(sum(tex %in% original)/length(tex) > 0.98)
#   })
# 
#   test_that("Default tex as expected", {
#     dir.create("tmp")
#     suppressWarnings(writeCodeBookLatex(
#       ds[1],
#       url = "https://app.crunch.io/dataset/10c3c3/",
#       appendix = TRUE, suppress_zero_counts = FALSE, pdf = FALSE, path = "tmp")
#     )
#     tex <- readLines("tmp/Data-for-Progress-National-Issues-Survey----Foreign-Policy.tex")
#     original <- readRDS("fixtures/writeCodeBookLatexLongCat.rds")
#     # expect_true(length(tex) == length(original))
#     expect_true(sum(tex %in% original)/length(tex) > 0.98)
#     file.remove("tmp/Data-for-Progress-National-Issues-Survey----Foreign-Policy.tex")
#     file.remove("tmp")
#   })
# })


context("codeBookItemBody")

test_that("Errors appropriately when passed bad object", {
  expect_error(codeBookItemBody(c(1,2,3,4)))
})

context("kable_strip_rules")

test_that("strips rules as expected", {
  x <- "\\toprule\\bottomrule\\midrule"
  r <- kable_strip_rules(x)
  expect_equal(r, "")

  r <- kable_strip_toprules(x)
  expect_equal(r, "\\bottomrule\\midrule")
})

test_that("fixes underscore", {
  x <- "___"
  r <- fixUnderscore(x)
  expect_equal(r, "\\_\\_\\_")
})

test_that("default yg logo returns normal path", {
  p <- default_yg_logo()
  expect_equal(p, system.file("YouGov.png", package = "crunchtabs"))
})
