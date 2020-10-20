# Fixtures for recontact_toplines and tracking reports
#
# - tests/testthat/recontactQuestion.R

library(crunchtabs)
library(httptest)

system("rm -rf tests/testthat/fixtures-1-3-0")


with_consent(deleteDataset("Example dataset"))
ds <- newExampleDataset()
name(ds) <- "Recontact dataset"

ds$weight1 <- makeWeight(ds$q1 ~ c(0.3,0.3,0.4,0), name = 'weight1')
ds$weight2 <- makeWeight(ds$q1 ~ c(0.4,0.4,0.1,0.1), name = 'weight2')
weight(ds) <- ds$weight1
weight(ds) <- ds$weight2
weight(ds) <- ds$weight1

ds$q1_pre <- copyVariable(ds$q1, deep = TRUE, name = "Pet name pre")
ds$q1_post <- copyVariable(ds$q1, deep = TRUE, name = "Pet name post")
ds$country_pre <- copyVariable(ds$country, deep = TRUE, name = "Country pre")
ds$country_post <- copyVariable(ds$country, deep = TRUE, name = "Country post")

rm(list = ls())
httpcache::clearCache()
httptest::start_capturing("tests/testthat/fixtures-1-3-0")

login()
ds <- loadDataset("Recontact dataset")

aliases(allVariables(ds))
# debugonce(recontact_toplines)
ct <- recontact_toplines(
  ds,
  questions = c("q1", "country"),
  suffixes = c("_pre", "_post"),
  labels = c("Pre", "Post"),
  weights = c("weight1", "weight2")
)
httptest::stop_capturing()

deleteDataset("Recontact dataset")
