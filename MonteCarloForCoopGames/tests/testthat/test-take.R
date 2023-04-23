test_that("Take Standard", {
  result <- take(c(1, 2, 3), 2)

  expect_equal(result, c(1, 2))
})


test_that("Take Empty", {
  result <- take(c(1, 2, 3), 0)

  expect_equal(result, c())
})

test_that("Take Complete", {
  result <- take(c(1, 2, 3), 3)

  expect_equal(result, c(1, 2, 3))
})

# FIXME: This is an unhandled case, which should be handled in the future
# test_that("Take Out of bounds", {
#  result <- take (c (1,2,3),4)
#  expect_equal(result, c(1,2,3))
# })
