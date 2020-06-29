test_that("subsetByTreatment works", {
  SW_mean <- readRDS("C:/Data/autoapsimx/data/SW_mean.rds")

  expect_output(str(subsetByTreatment(SW_mean)), "25 variables")
  expect_is(subsetByTreatment(SW_mean), "data.table")
})
