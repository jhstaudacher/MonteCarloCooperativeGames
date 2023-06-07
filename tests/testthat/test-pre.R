test_that("length(O) == 6", {
  expect_equal(pre(c(1, 2, 5, 8, 7, 6), 1), c())
  expect_equal(pre(c(1, 2, 5, 8, 7, 6), 2), c(1))
  expect_equal(pre(c(1, 2, 5, 8, 7, 6), 5), c(1, 2))
  expect_equal(pre(c(1, 2, 5, 8, 7, 6), 8), c(1, 2, 5))
  expect_equal(pre(c(1, 2, 5, 8, 7, 6), 7), c(1, 2, 5, 8))
  expect_equal(pre(c(1, 2, 5, 8, 7, 6), 6), c(1, 2, 5, 8, 7))
})


test_that("length(O) == 1", {
  expect_equal(pre(c(2), 2), c())
})
