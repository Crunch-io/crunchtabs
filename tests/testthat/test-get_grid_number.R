test_that("integration test", {
  expect_equal(get_grid_number(1), "A")
  expect_equal(get_grid_number(26), "Z")
  expect_equal(get_grid_number(27), "AA")
  expect_equal(get_grid_number(100), "CV")
})
