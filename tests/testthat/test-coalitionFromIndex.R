test_that("CoalitionFromIndex empty coalition", {
  empty_set <- coalitionFromIndex(1:5, 1)
  expect_equal(empty_set, c())
})

test_that("CoalitionFromIndex all elements", {
  result <- coalitionFromIndex(1:5, 2**5)
  expect_equal(result, 1:5)
})

test_that("CoalitionFromIndex subset", {
  result <- coalitionFromIndex(c(1, 3, 5), 4)
  expect_equal(result, c(1, 3))
})

test_that("CoalitionFromIndex empty subset with partition", {
  result <- coalitionFromIndex(list(c(1, 2), c(3), c(4, 5, 6)), 1)
  expect_equal(result, c())
})

test_that("CoalitionFromIndex all elements with partition", {
  p <- list(c(1, 2), c(3), c(4, 5, 6))
  result <- coalitionFromIndex(p, 2**3)
  expect_equal(result, p)
})

test_that("CoalitionFromIndex subset with partition", {
  result <- coalitionFromIndex(list(c(1, 2), c(3), c(4, 5, 6)), 6)
  expect_equal(result, list(c(1, 2), c(4, 5, 6)))
})
