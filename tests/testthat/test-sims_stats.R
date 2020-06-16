test_that("extract_trts", {
  # It must have two elements
  expect_length(extract_trts(filename = "apsimx/ModifiedSKL_0.07AshleyDeneSD1.db"), 2)
})
