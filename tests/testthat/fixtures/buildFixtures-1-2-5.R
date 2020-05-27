library(crunchtabs)
library(httptest)

# We build two sets of exmaples. Unweighted and weighted

login()

# Assumes your wd is project dir
rm(list = ls())
ds <- newExampleDataset()

httpcache::clearCache()
start_capturing("tests/testthat/fixtures-1-2-5")

ds <- loadDataset("Example dataset")

# Unweighted ----

ct_banner <- banner(
  ds,
  vars = list(`banner 1` = 'allpets')
)

# For codebookItemTxt
codebookItemTxt(ds$q1)
codebookItemTxt(ds$allpets)
codebookItemTxt(ds$ndogs)
codebookItemTxt(ds$wave)
codebookItemTxt(ds$q3)

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
with_consent(deleteDataset("Example dataset"))
