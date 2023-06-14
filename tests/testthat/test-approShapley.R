test_that("m %% n == 0", {
  result <- approShapley(5, 25, weightedVotingGameForSampling(c(1, 2, 1, 3, 1), 0.5))
  expect_equal(sum(result), 1)
})

test_that("m %% n != 0", {
  expect_warning(result <- approShapley(5, 27, weightedVotingGameForSampling(c(1, 2, 1, 3, 1), 0.5)))
  expect_equal(sum(result), 1)
})

test_that("n == 1", {
  # This test can only use 1 sample because, there is only one player -> one permutation/order
  result <- approShapley(1, 1, weightedVotingGameForSampling(c(1), 0.5))
  expect_equal(sum(result), 1)
})
