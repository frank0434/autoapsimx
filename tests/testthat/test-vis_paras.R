test_that("subsetByTreatment works", {
  SW_mean <- readRDS("C:/Data/autoapsimx/data/meanSWC.rda")

  expect_output(str(subsetByTreatment(SW_mean)), "25 variables")
  expect_is(subsetByTreatment(SW_mean), "data.table")
})
