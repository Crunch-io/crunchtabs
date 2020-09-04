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
writeCodeBookLatex(ds, url = "https://app.crunch.io/dataset/10c3c3cbd28b420aaa4976b70caba851/")

# DFN Long question

ds = loadDataset("https://app.crunch.io/dataset/10c3c3cbd28b420aaa4976b70caba851/")
writeCodeBookLatex(ds, url = "https://app.crunch.io/dataset/10c3c3cbd28b420aaa4976b70caba851/", appendix = FALSE, suppres_zero_counts = TRUE)


# CBS

ds = loadDataset("CBS News Poll - April 10, 2020 - FOR TESTING")
writeCodeBookLatex(ds)

# Codebook Latex

ds = loadDataset("CBS News Poll - April 10, 2020 - FOR TESTING")
writeCodeBookLatex(ds, table_of_contents = TRUE, sample_desc = "US Voting Adults", title = "CBS News Poll - April 10, 2020", logo = "yougov")

ds = loadDataset("https://app.crunch.io/dataset/10c3c3cbd28b420aaa4976b70caba851/")
writeCodeBookLatex(ds, url = "https://app.crunch.io/dataset/10c3c3cbd28b420aaa4976b70caba851/", table_of_contents = TRUE, logo = "ygblue", title="Data for Progress - Foreign Policy")

ds = loadDataset("Example dataset")
writeCodeBookLatex(ds)

ds <- loadDataset("Forked Huffpost")
writeCodeBookLatex(ds, logo = "yougov", table_of_contents = T)

ds = loadDataset("https://app.crunch.io/dataset/2375608c53694a899213fe7daf7e2d1e/")

writeCodeBookLatex(
  ds,
  url = "https://app.crunch.io/dataset/2375608c53694a899213fe7daf7e2d1e/",
  table_of_contents = TRUE, logo = "yougov",
  pdf = TRUE
)


ds <- loadDataset("BEB Fork STAN0138")
writeCodeBookLatex(ds, table_of_contents = TRUE, suppress_zero_counts = TRUE,
                   title = 'Presidential Election Study - August 2020',
                   field_period = 'August 24-31, 2020',
                   sample_desc = paste0(nrow(ds), ' Adults'),
                   rmd=FALSE)
