library(crunchtabs)

# Example dataset
login()
ds = loadDataset("Example dataset")
writeCodeBook(ds)

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

writeLatex(ct, theme = localTheme, pdf = TRUE)
writeExcel(ct, theme = localTheme)
# DFN

ds = loadDataset("https://app.crunch.io/dataset/10c3c3cbd28b420aaa4976b70caba851/")
writeCodeBook(ds, url = "https://app.crunch.io/dataset/10c3c3cbd28b420aaa4976b70caba851/")

# DFN Long question

ds = loadDataset("https://app.crunch.io/dataset/10c3c3cbd28b420aaa4976b70caba851/")
writeCodeBook(ds[1], url = "https://app.crunch.io/dataset/10c3c3cbd28b420aaa4976b70caba851/")


# CBS

ds = loadDataset("CBS News Poll - April 10, 2020 - FOR TESTING")
writeCodeBook(ds)

# Huff

ds = loadDataset("HuffPost Daily Survey #20200331")
writeCodeBook(ds)
