library(crunchtabs)
library(httptest)

system("rm -rf tests/testthat/fixtures-1-2-5")

rm(list = ls())
httpcache::clearCache()
start_capturing("tests/testthat/fixtures-1-2-5")
login()

# ds = newExampleDataset()
ds = loadDataset("Example dataset")
# Unweighted ----

ct_banner <- banner(
  ds,
  vars = list(`banner 1` = 'allpets')
)

# For codeBookItemTxt

codeBookItemBody(ds$q1)
codeBookItemBody(ds$allpets)
codeBookItemBody(ds$petloc)
codeBookItemBody(ds$ndogs)
codeBookItemBody(ds$wave)
codeBookItemBody(ds$q3)

codeBookItemTxtDescription(ds$q1)
codeBookItemTxtDescription(ds$allpets)
codeBookItemTxtDescription(ds$petloc)
codeBookItemTxtDescription(ds$ndogs)
codeBookItemTxtDescription(ds$wave)
codeBookItemTxtDescription(ds$q3)

codeBookSummary(ds$q1)
codeBookSummary(ds$allpets)
codeBookSummary(ds$petloc)
codeBookSummary(ds$ndogs)
codeBookSummary(ds$wave)
codeBookSummary(ds$q3)

topline_unweighted <- crosstabs(
  ds,
  include_numeric = TRUE,
  include_datetime = TRUE,
  include_verbatims = TRUE
)

ct_unweighted <- crosstabs(
  dataset = ds,
  banner = ct_banner
)


ds = loadDataset("https://app.crunch.io/datasets/868e8b3e01834c45b73e56e80160d3c3/")
crosstabs_data <- crosstabs(ds, vars = c("movies2_a_1", "movies2_a_2", "books1"))
crosstabs_data <- crosstabs(ds, vars = c("art3", "books1", "movies1"), weight = NULL)
crosstabs_data <- tryCatch(crosstabs(ds, vars = c("books1", "starttime")))
crosstabs_data <- crosstabs(ds, vars = c("books1", "starttime", "movies2_a_1"))
crosstabs_data <- crosstabs(ds, vars = c("books1", "starttime", "endtime", "movies2_a_1"))

stop_capturing()

# Weighted ----

# start_capturing("tests/testthat/fixtures-1-2-5-weighted")
#
# ds <- loadDataset("Example dataset")
#
# ct_banner <- banner(
#   ds,
#   vars = list(`banner 1` = 'allpets')
# )
#
# ds$weight = makeWeight(ds$q1 ~ c(0.75, 0.15, 0.10), name = "weight")
# modifyWeightVariables(ds, "weight")
#
# topline_weighted <- crosstabs(ds, weight = "weight")
#
# missings = setdiff(names(ds), names(topline_weighted$results))
#
# topline_withExtraSummary <- topline_weighted
#
# topline_withExtraSummary$results <- c(
#   topline_withExtraSummary$results,
#   setNames(lapply(missings, function(x) prepareExtraSummary(ds[[x]])), missings)
# )
#
# results_list <- list()
# for (x in names(ds)) {
#   results_list[[x]] <- topline_withExtraSummary$results[[x]]
# }
#
# topline_withExtraSummary$results = results_list
#
# topline_withExtraSummary <- crunchtabs:::reflowQuestionNumbers(ct)
#
# ct_banner <- banner(
#   ds,
#   vars = list(`banner 1` = 'allpets')
# )
#
# ct_weighted <- crosstabs(
#   dataset = ds,
#   weight = 'weight',
#   banner = ct_banner
# )
#
# stop_capturing()
#
# with_consent(deleteDataset("Example dataset"))

rm(list = ls())
