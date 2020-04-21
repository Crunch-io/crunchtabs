test_that("Direct run of tabbook", {
  file.copy("ref/tabbook1.tex", "tabbook1.tex")
  crunchtabs:::pdflatex("tabbook1.tex",path.to.pdflatex = paste0(tinytex::tinytex_root(), "/bin/x86_64-linux/pdflatex"))
  expect_true(file.exists("tabbook1.pdf"))
  expect_true(file.remove("tabbook1.pdf"))
})

test_that("Direct run of topline", {
  file.copy("ref/topline1.tex", "topline1.tex")
  crunchtabs:::pdflatex("topline1.tex",path.to.pdflatex = paste0(tinytex::tinytex_root(), "/bin/x86_64-linux/pdflatex"))
  expect_true(file.exists("topline1.pdf"))
  expect_true(file.remove("topline1.pdf"))
})
