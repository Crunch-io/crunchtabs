context("tableHeader.default")

test_that("Fails in wrong class", {
  expect_error(tableHeader("a string"), "The expected class")
})

test_that("format_label_column_exceptions", {
  theme = themeNew(format_label_column_exceptions = c('test' = 2.5), default_theme = themeDefaultLatex())
  var = list()
  var$alias = "test"
  var$name = "test"
  class(var) = "CrossTabVar"
  res = tableHeader(var, theme)
  expect_equal(res, "\n\\tbltopa[2.5in]\n\\addcontentsline{lot}{table}{ . test}\n\\hangindent=0em \\parbox{9in}{\n\\formatvardescription{. test}} \\\\\n\\addlinespace\n\\bannera{}\n\n")
})

test_that("format_label_column_exceptions overwrites global", {
  theme = themeNew(
    format_label_column = list(col_width = 2.5),
    format_label_column_exceptions = c('test' = 3.5),
    default_theme = themeDefaultLatex())
  var = list()
  var$alias = "test"
  var$name = "test"
  var$longtable = FALSE
  class(var) = "ToplineCategoricalArray"
  res = tableHeader(var, theme)
  expect_equal(res, "\\begin{tabular}{@{\\extracolsep{\\fill}}p{0.1in}B{\\raggedright}{3.5in}B{\\centering}{Inf\\mywidth}}\n\\addcontentsline{lot}{table}{ . test}\n\\hangindent=0em \\parbox{9in}{\n\\formatvardescription{. test}} \\\\\n\\\\ &  & \\\\\n\n")
})

test_that("format_label_column_exceptions acts appropriately", {
  theme = themeNew(
    format_label_column_exceptions = c('test' = 3.5),
    default_theme = themeDefaultLatex())
  var = list()
  var$alias = "test"
  var$name = "test"
  var$longtable = FALSE
  var$inserts_obj = new(
    "AbstractCategories", .Data = list(
      new("Category", .Data = list(
        1L, FALSE, "Big reason", 1L),
        names = c("id", "missing",
                  "name", "numeric_value")),
      new("Category", .Data = list(
        2L, FALSE,
        "Small reason", 2L),
        names = c("id", "missing", "name", "numeric_value"
        )),
      new("Category", .Data = list(
        3L, FALSE, "Not a reason", 3L),
        names = c("id", "missing", "name", "numeric_value"))))
  class(var) = "ToplineCategoricalArray"
  res = tableHeader(var, theme)
  expect_equal(res, "\\begin{tabular}{@{\\extracolsep{\\fill}}p{0.1in}B{\\raggedright}{3.68333333333333in}B{\\centering}{0.2\\mywidth}B{\\centering}{0.2\\mywidth}B{\\centering}{0.2\\mywidth}B{\\centering}{0.2\\mywidth}}\n\\addcontentsline{lot}{table}{ . test}\n\\hangindent=0em \\parbox{9in}{\n\\formatvardescription{. test}} \\\\\n\\\\ &  & Big reason & Small reason & Not a reason & \\\\\n\n")
})

test_that("Spacing on toplines", {
  theme = themeNew(
    format_var_description = list(include_q_number = TRUE, background_color = "gray"),
    default_theme = themeDefaultLatex()
  )
  var = list()
  var$alias = "test"
  var$description = "This is a description"
  var$notes = "This is filtertext"
  var$longtable = FALSE
  var$inserts_obj = new(
    "AbstractCategories", .Data = list(
      new("Category", .Data = list(
        1L, FALSE, "Big reason", 1L),
        names = c("id", "missing",
                  "name", "numeric_value")),
      new("Category", .Data = list(
        2L, FALSE,
        "Small reason", 2L),
        names = c("id", "missing", "name", "numeric_value"
        )),
      new("Category", .Data = list(
        3L, FALSE, "Not a reason", 3L),
        names = c("id", "missing", "name", "numeric_value"))))
  class(var) = "ToplineVar"
  res = latexTableName(var, theme)
  expect_equal(res, "\\colorbox{gray}{\n\\addcontentsline{lot}{table}{ . This is a description}\n\\hangindent=0em \\parbox{6.5in}{\n\\formatvardescription{. This is a description}\\\\ \n\\formatvarfiltertext{This is filtertext}}\\hspace*{1ex}} \\\\")
})

test_that("Global for crosstab works format_label_column_exceptions", {
  theme = themeNew(format_label_column = list(col_width = 3), format_label_column_exceptions = c('test' = 2.5), default_theme = themeDefaultLatex())
  var = list()
  var$alias = "nottest"
  var$name = "nottest"
  class(var) = "CrossTabVar"
  res = tableHeader(var, theme)
  expect_equal(res, "\n\\tbltopa[3in]\n\\addcontentsline{lot}{table}{ . nottest}\n\\hangindent=0em \\parbox{9in}{\n\\formatvardescription{. nottest}} \\\\\n\\addlinespace\n\\bannera{}\n\n")
})
