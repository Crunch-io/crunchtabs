test_that("returns full string or truncated string depending on length", {
  expect_equal(adjustCrunchName("abc"), "abc")
  
  expect_equal(
    adjustCrunchName(paste0(LETTERS, letters, collapse = "")),
    "AaBbCcDdEeFfGgHhIiJjKkLlMmNnOoPpQqRrS..."
  )
  
  expect_equal(adjustCrunchAlias("abc"), "abc")
  
  expect_equal(
    adjustCrunchAlias(paste0(LETTERS, letters, collapse = "")),
    "AaBbCcDdEeFfGgHhIiJjKkLlMmNnOoPp..."
  )
})

test_that("returns full name and/or alias in description if elsewhere too long", {
  expect_equal(adjustCrunchDescription("nm", "alias", "description"), "description")
  
  expect_equal(
    adjustCrunchDescription(
      paste0(LETTERS, letters, collapse = ""),
      "alias",
      "description"
    ),
    "AaBbCcDdEeFfGgHhIiJjKkLlMmNnOoPpQqRrSsTtUuVvWwXxYyZzdescription"
  )
  
  expect_equal(
    adjustCrunchDescription(
      "nm",
      paste0(LETTERS, letters, collapse = ""),
      "description"
    ),
    "description \n(AaBbCcDdEeFfGgHhIiJjKkLlMmNnOoPpQqRrSsTtUuVvWwXxYyZz)"
  )
  
  expect_equal(
    adjustCrunchDescription(
      paste0(LETTERS, letters, collapse = ""),
      paste0(LETTERS, letters, collapse = ""),
      "description"
    ),
    "AaBbCcDdEeFfGgHhIiJjKkLlMmNnOoPpQqRrSsTtUuVvWwXxYyZzdescription \n(AaBbCcDdEeFfGgHhIiJjKkLlMmNnOoPpQqRrSsTtUuVvWwXxYyZz)"
  )
})
