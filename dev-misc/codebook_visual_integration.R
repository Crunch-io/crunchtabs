library(crunchtabs)

# Example dataset
login()
ds = loadDataset("Example dataset")
writeCodeBookLatex(
  ds,
  title = "This is a title",
  subtitle = "This is a subtitle",
  sample_desc = "US Voters",
  field_period = "Start Date - End Date")

# Example dataset toptline

localTheme = themeNew(
  default_theme = themeDefaultLatex(),
  logo = list(file = default_yg_logo())
)

localTheme = themeNew(
  default_theme = themeDefaultLatex(),
  logo = list(file = "YouGov.png")
)

localTheme = themeNew(
  default_theme = themeDefaultLatex(),
  logo = list(file = "YouGov")
)

# Should fail
ct = crosstabs(ds)

localTheme = themeNew(
  default_theme = themeDefaultExcel(),
  logo = list(file = "YouGov")
)

writeExcel(ct, theme = localTheme)

# Should not fail
localTheme = themeNew(
  default_theme = themeDefaultExcel(),
  logo = list(file = "YouGov.png")
)
writeExcel(ct, theme = localTheme)


ct = crosstabs(ds)

writeLatex(ct, theme = localTheme, pdf = TRUE, title = "A", subtitle = "B")
writeExcel(ct, theme = localTheme)

# Example

ds = loadDataset("Example dataset")
writeCodeBookLatex(ds)

# DFN

ds = loadDataset("https://app.crunch.io/dataset/10c3c3cbd28b420aaa4976b70caba851/")
writeCodeBook(ds, url = "https://app.crunch.io/dataset/10c3c3cbd28b420aaa4976b70caba851/")

# DFN Long question

ds = loadDataset("https://app.crunch.io/dataset/10c3c3cbd28b420aaa4976b70caba851/")
writeCodeBookLatex(ds, url = "https://app.crunch.io/dataset/10c3c3cbd28b420aaa4976b70caba851/", appendix = FALSE, suppres_zero_counts = TRUE)


# CBS

ds = loadDataset("CBS News Poll - April 10, 2020 - FOR TESTING")
writeCodeBookLatex(ds)

# Codebook Latex



ds = loadDataset("CBS News Poll - April 10, 2020 - FOR TESTING")
writeCodeBookLatex(ds, table_of_contents = TRUE, sample_desc = "US Voting Adults")

ds = loadDataset("https://app.crunch.io/dataset/10c3c3cbd28b420aaa4976b70caba851/")
writeCodeBookLatex(ds, url = "https://app.crunch.io/dataset/10c3c3cbd28b420aaa4976b70caba851/", table_of_contents = TRUE)
