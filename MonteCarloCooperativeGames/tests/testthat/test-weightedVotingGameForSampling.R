test_that("q == 0 and n == 4", {
  v <- weightedVotingGameForSampling(c(1, 1, 2, 2), 0)

  expect_equal(v(c(1:4)), 1)
  expect_equal(v(c(3, 4)), 1)
  expect_equal(v(c(1, 4)), 1)
  expect_equal(v(c(1, 2)), 1)
  expect_equal(v(c()), 1)
})

test_that("q == 1/2 and n == 4", {
  v <- weightedVotingGameForSampling(c(1, 1, 2, 2), 1 / 2)

  expect_equal(v(c(3, 4)), 1)
  expect_equal(v(c(1, 4)), 1)
  expect_equal(v(c(1, 2)), 0)
  expect_equal(v(c(1:4)), 1)
  expect_equal(v(c()), 0)
})

test_that("q == 1 and n == 4", {
  v <- weightedVotingGameForSampling(c(1, 1, 2, 2), 1)

  expect_equal(v(c(1:4)), 1)
  expect_equal(v(c(3, 4)), 0)
  expect_equal(v(c(1, 4)), 0)
  expect_equal(v(c(1, 2)), 0)
  expect_equal(v(c()), 0)
})

test_that("q == 0 and n == 1", {
  v <- weightedVotingGameForSampling(c(1), 0)

  expect_equal(v(c(1)), 1)
  expect_equal(v(c()), 1)
})

test_that("q == 1/2 and n == 1", {
  v <- weightedVotingGameForSampling(c(1), 1 / 2)

  expect_equal(v(c(1)), 1)
  expect_equal(v(c()), 0)
})

test_that("q == 1 and n == 0", {
  v <- weightedVotingGameForSampling(c(1), 1)

  expect_equal(v(c(1)), 1)
  expect_equal(v(c()), 0)
})

