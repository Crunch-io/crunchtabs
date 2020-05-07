
# Same tests, but using function
with_api_fixture("fixture 1", {
  ds <- crunch::loadDataset(
    "fixture 1",
    project = crunch::projects()[["crunchtestdemo fixtures"]]
  )

  actual <- crunch_mean_by(ds, "age", "region")

  expect_equivalent(as.numeric(actual@arrays$mean[1:2]), c(30.75, 70))
  expect_equivalent(names(actual@arrays$mean[1:2]), c("North", "South"))
})

context("prepareExtraSummary Text")
context("prepareExtraSummary Numeric")

test_that("Created response matches expectations", {

})


context("prepareExtraSummary Datetime")
