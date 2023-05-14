test_that("gloveGameForSampling Complete", {
  v <- gloveGameForSampling(c(1, 2, 3), c(4, 5, 6))

  v_result <- v(c(1, 2, 3, 4, 5, 6))

  expect_equal(v_result, 3)
})

test_that("gloveGameForSampling Standard", {
  v <- gloveGameForSampling(c(1, 2, 3), c(4, 5, 6))

  v_result <- v(c(1, 5, 6))

  expect_equal(v_result, 1)
})

test_that("gloveGameForSampling No Match", {
  v <- gloveGameForSampling(c(1, 2, 3), c(4, 5, 6))

  v_result <- v(c(4, 5, 6))

  expect_equal(v_result, 0)
})


test_that("gloveGameForSampling No Left", {
  v <- gloveGameForSampling(c(), c(4, 5, 6))

  v_result <- v(c(1, 2, 3, 4, 5, 6))

  expect_equal(v_result, 0)
})

test_that("gloveGameForSampling No Player", {
  v <- gloveGameForSampling(c(1, 2, 3), c(4, 5, 6))

  v_result <- v(c())

  expect_equal(v_result, 0)
})
