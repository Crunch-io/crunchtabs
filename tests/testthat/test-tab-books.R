context('preparing banner data summary')

with_mock_API({
  test_that("We can get a Banner object", {
    txt <- readLines(con = file.path(fixtures_dir, "ds1_banner1.json"))
    banner_data <- unserializeJSON(txt)
    expect_is(banner_data, "Banner")
  })
})
