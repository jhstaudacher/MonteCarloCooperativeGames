test_that("buildAirportCostVector", {
  costs <- buildAirportCostVector(list(c(1, 2), c(2, 2), c(3, 1), c(4, 5)))

  expect_equal(costs, c(1, 1, 2, 2, 3, 4, 4, 4, 4, 4))
})
