test_that("weightedVotingGameForSampling Complete", {
  v <- weightedVotingGameForSampling(c(1, 1, 2, 2), 1 / 2)
  v_result <- v(c(1:4))

  expect_equal(v_result, 1)
})

test_that("weightedVotingGameForSampling Standard", {
  v <- weightedVotingGameForSampling(c(1, 1, 2, 2), 1 / 2)
  v_result <- v(c(3, 4))

  expect_equal(v_result, 1)
})

test_that("weightedVotingGameForSampling Tie", {
  v <- weightedVotingGameForSampling(c(1, 1, 2, 2), 1 / 2)
  v_result <- v(c(2, 4))

  expect_equal(v_result, 1)
})


test_that("weightedVotingGameForSampling Loss", {
  v <- weightedVotingGameForSampling(c(1, 1, 2, 2), 2 / 3)
  v_result <- v(c(2, 4))

  expect_equal(v_result, 0)
})
