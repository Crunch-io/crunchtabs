# Test -----

library(crunchtabs)
login()
ds <- loadDataset("Example dataset")

ct <- recontact_toplines(
  ds,
  questions = c("q1", "country"),
  suffixes = c("_pre", "_post"),
  labels = c("Pre", "Post"),
  weights = c("weight1", "weight2")
)



writeLatex(ct, pdf = TRUE, open = TRUE)
theme <- themeNew(default_theme = themeDefaultLatex(), latex_flip_grids = TRUE)

writeLatex(ct, pdf = TRUE, open = TRUE, theme = theme)
# Data setup -----

# library(crunch)
# login()
# with_consent(deleteDataset("Example dataset"))
# ds <- newExampleDataset()
# ds <- loadDataset("Example dataset")
#
# ds$weight1 <- makeWeight(ds$q1 ~ c(0.3,0.3,0.4,0), name = 'weight1')
# ds$weight2 <- makeWeight(ds$q1 ~ c(0.4,0.4,0.1,0.1), name = 'weight2')
# weight(ds) <- ds$weight1
#
# ds$q1_pre <- copyVariable(ds$q1, deep = TRUE, name = "Pet name pre")
# ds$q1_post <- copyVariable(ds$q1, deep = TRUE, name = "Pet name post")
# ds$country_pre <- copyVariable(ds$country, deep = TRUE, name = "Country pre")
# ds$country_post <- copyVariable(ds$country, deep = TRUE, name = "Country post")
#
# questions = c("q1", "country")
# suffixes = c("_pre", "_post")
