context("codebookItemTxt")

crunchtabs:::with_api_fixture("fixtures-1-2-5", {
  ds = loadDataset("Example dataset")

  test_that("categorical", {
    r = codebookItem(ds$country)
    expect_equal(r, "\\begin{tabularx}{\\textwidth}{lXXr} \n[q1] &  &  & Pet  \\\\ \n\\end{tabularx}\n\n\\begin{tabularx}{\\textwidth}{lXXr} \n& What is your favorite pet?  &  &  & \\\\ \n\\end{tabularx}) \n\n\\begin{tabularx}{\\textwidth}{llcXrr} \n& 1 & Cat & \\dotfill & 6 & \\      \n& 2 & Dog & \\dotfill & 4 & \\      \n& 3 & Bird & \\dotfill & 3 & \\      \n& 8 & Skipped & \\dotfill & 3 & \\      \n& 9 & Not Asked & \\dotfill & 4 & \\      \n\\end{tabularx}")

  })
  test_that("numeric", {
    r = suppressWarnings(codebookItemTxt(ds$ndogs))
    expect_equal(r, "\\begin{tabularx}{\\textwidth}{lXXr} \n[ndogs] &  &  & Number of dogs  \\\\ \n\\end{tabularx}\n\n\\begin{tabularx}{\\textwidth}{lXXr} \n&   &  &  & \\\\ \n\\end{tabularx}) \n\n")
  })

  test_that("Missing description causes warning", {
    expect_warning(codebookItemTxt(ds$ndogs), "ndogs is missing a description")
  })

  test_that("multiple_response", {
    r = codebookItemTxt(ds$allpets)
    expect_equal(
      r,
      "\\begin{tabularx}{\\textwidth}{lXXr} \n[allpets] &  &  & All pets owned  \\\\ \n\\end{tabularx}\n\n\\begin{tabularx}{\\textwidth}{lXXr} \n& Do you have any of these animals as pets? Please select all that apply.  &  &  & \\\\ \n\\end{tabularx}) \n\n")
  })
})
