test_that("Stop if logo missing", {
  # Issue 88
  theme <- themeNew()
  expect_equal(theme$logo, NULL)

  expect_error(
    themeNew(
      logo = list(file = "some-file-that-doesnt-exist")
    ),
    "Logo file not found, check path to file or current working directory."
  )

  write("0", file = "test-file.png")
  theme <- themeNew(logo = list(file = "test-file"))
  expect_equal(theme$logo$file, "test-file")
  expect_true(file.remove("test-file.png"))
})
