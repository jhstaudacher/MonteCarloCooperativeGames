test_that("WeightedVotingGame Complete", {
  v <- weightedVotingGame(c(1, 1, 2, 2), 1 / 2)
  v_result <- v(c(1:4))

  expect_equal(v_result, 1)
})

test_that("WeightedVotingGame Standard", {
  v <- weightedVotingGame(c(1, 1, 2, 2), 1 / 2)
  v_result <- v(c(3, 4))

  expect_equal(v_result, 1)
})

test_that("WeightedVotingGame Tie", {
  v <- weightedVotingGame(c(1, 1, 2, 2), 1 / 2)
  v_result <- v(c(2, 4))

  expect_equal(v_result, 1)
})


test_that("WeightedVotingGame Loss", {
  v <- weightedVotingGame(c(1, 1, 2, 2), 2 / 3)
  v_result <- v(c(2, 4))

  expect_equal(v_result, 0)
})
