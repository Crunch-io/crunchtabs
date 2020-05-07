library(crunchtabs)
library(httptest)

login()

# Assumes your wd is project dir
start_capturing("tests/testthat/extra_summary_fixtures")

ds <- newExampleDataset()
ds$weight = makeWeight(ds$q1 ~ c(0.75, 0.15, 0.10), name = "weight")
modifyWeightVariables(ds, "weight")

topline_unweighted <- crosstabs(ds)

topline_weighted <- crosstabs(ds, weight = "weight")

missings = setdiff(names(ds), names(topline_weighted$results))

topline_withExtraSummary <- topline_weighted

topline_withExtraSummary$results <- c(
  topline_withExtraSummary$results,
  setNames(lapply(missings, function(x) prepareExtraSummary(ds[[x]])), missings)
)

results_list <- list()
for (x in names(ds)) {
  results_list[[x]] <- topline_withExtraSummary$results[[x]]
}

topline_withExtraSummary$results = results_list

ct_banner <- banner(
  ds,
  vars = list(`banner 1` = 'allpets')
)

ct_weighted <- crosstabs(
  dataset = ds,
  weight = 'weight',
  banner = ct_banner
)

ct_unweighted <- crosstabs(
  dataset = ds,
  banner = ct_banner
)
