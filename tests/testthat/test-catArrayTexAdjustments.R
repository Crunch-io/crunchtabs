context("repositionTex")

result <- structure(list(alias = "allpets", name = "All pets owned",
description = "Do you have any of these animals as pets? Please select all that apply.",
notes = "", type = "categorical_array", no_totals = TRUE,
mean_median = FALSE, subnames = c("Wave 1", "Wave 2", "Wave 3"
), categories = NULL, inserts_obj = NULL, subnumber = 1L,
number = 1L, crosstabs = list(Results = list(`___total___` = structure(list(
counts = structure(c(2.7083333333, 4.0083333333, 4.0083333333,
2.6666666667, 4.2666666667, 4.2666666667, 4, 4, 4), .Dim = c(3L,
3L), .Dimnames = list(c("A", "B", "C"), c("This is an entry", "This is another entry",
"This is the last entry"))), proportions = structure(c(0.735294117644663,
0.755102040814789, 0.672727272725442, 0.5555555555625,
0.666666666671875, 0.5, 0.681818181814308, 0.789473684205332,
0.517241379312574), .Dim = c(3L, 3L), .Dimnames = list(
c("Cat", "Dog", "Bird"), c("This is an entry", "This is another entry",
                           "This is the last entry"))),
base = structure(c(8, 8, 11), .Dim = c(3L, 1L
), .Dimnames = list(c("Cat", "Dog", "Bird"), "Total")),
weighted_base = structure(c(3.6833333333, 5.3083333333,
5.9583333333), .Dim = c(3L, 1L), .Dimnames = list(c("Cat",
"Dog", "Bird"), "Total"), class = c("CrunchCubeCalculation",
"array"), dims = new("CubeDims", .Data = list(list(name = c("Cat",
"Dog", "Bird"), missing = c(FALSE, FALSE, FALSE), references = list(
uniform_basis = FALSE,
description = "Do you have any of these animals as pets? Please select all that apply.",
format = list(summary = list(digits = 0L)), subreferences = list(
allpets_1 = list(alias = "allpets_1", name = "Cat",
view = list(show_counts = FALSE, include_noneoftheabove = FALSE,
include_missing = FALSE, column_width = NULL)),
allpets_2 = list(alias = "allpets_2", name = "Dog",
view = list(show_counts = FALSE, include_noneoftheabove = FALSE,
include_missing = FALSE, column_width = NULL)),
allpets_3 = list(alias = "allpets_3", name = "Bird",
view = list(show_counts = FALSE, include_noneoftheabove = FALSE,
include_missing = FALSE, column_width = NULL))),
notes = "", alias = "allpets", view = list(show_counts = FALSE,
include_noneoftheabove = FALSE, include_missing = FALSE,
column_width = NULL), name = "All pets owned",
type = "subvariable_items", subvariables = c("allpets_1/",
"allpets_2/", "allpets_3/"))), list(name = "", missing = FALSE,
references = list(alias = "total", name = "Total",
type = "enum"))), names = c("allpets", "total"
)), type = "margin"), mean = NULL, median = NULL, pvals_col = NULL), class = c("CrossTabBannerVar",
"list")))), rownames = c("Cat", "Dog", "Bird"),
labels = c("This is an entry", "This is another entry","This is the last entry")),
class = c("ToplineCategoricalArray",
"ToplineVar", "CrossTabVar"))

test_that("no bulllets", {

  res <- catArrayTexAdjustments(result, bullets = FALSE)
expected <- "\\\\
  \\hangindent=0em \\parbox{6.5in}{\\formatvardescription{
    \\begin{itemize}
    \\item A. This is an entry
    \\item B. This is another entry
    \\item C. This is the last entry
    \\end{itemize}
  }}
\\\\"
expect_equal(res, expected)

})

test_that("bullets", {
  res <- catArrayTexAdjustments(result, bullets = TRUE)
expected <- "\\\\
  \\hangindent=0em \\parbox{6.5in}{\\formatvardescription{
    \\begin{itemize}
    \\item[] A. This is an entry
    \\item[] B. This is another entry
    \\item[] C. This is the last entry
    \\end{itemize}
  }}
\\\\"
  expect_equal(res, expected)
})

test_that("custom category aliases, no bullets", {
  res <- catArrayTexAdjustments(result, bullets = FALSE, category_aliases = c("AA", "BB", "CC"))
  expected <- "\\\\
  \\hangindent=0em \\parbox{6.5in}{\\formatvardescription{
    \\begin{itemize}
    \\item AA. This is an entry
    \\item BB. This is another entry
    \\item CC. This is the last entry
    \\end{itemize}
  }}
\\\\"
  expect_equal(res, expected)
})


test_that("custom category alias and custom category", {
  res <- catArrayTexAdjustments(
    result,
    bullets = FALSE,
    category_text = c("Entry number one", "Entry number two", "Entry number three"),
    category_aliases = c("AA", "BB", "CC")
  )

expected <- "\\\\
  \\hangindent=0em \\parbox{6.5in}{\\formatvardescription{
    \\begin{itemize}
    \\item AA. Entry number one
    \\item BB. Entry number two
    \\item CC. Entry number three
    \\end{itemize}
  }}
\\\\"
  expect_equal(res, expected)
})


test_that("Error if category_items not same length", {
  expect_error(catArrayTexAdjustments(result, category_aliases = 1:4), "Category aliases not")
  expect_error(catArrayTexAdjustments(result, category_text = 1:4), "Category text labels not")
})


context("getCatArrayColNames")

test_that("Returns colnames as expected", {
  r <- getCatArrayColNames(result)
  expect_equal(
    r,
    c("This is an entry",
      "This is another entry",
      "This is the last entry")
  )
})


context("setCatArrayColNames")

test_that("Returns updated object", {
  r <- setCatArrayColNames(
    result,
    category_aliases = c("Entry number one", "Entry number two", "Entry number three")
  )

  expect_equal(
    dimnames(r$crosstabs$Results$`___total___`$proportions)[[2]],
    c("Entry number one", "Entry number two", "Entry number three")
  )
})

test_that("Fails when category aliases not same length", {
  expect_error(
    setCatArrayColNames(
      result,
      category_aliases = c(
        "Entry number one",
        "Entry number two",
        "Entry number three",
        "I don't exist!"
      )),
    "Category aliases do not match"
  )
})

rm(result)
