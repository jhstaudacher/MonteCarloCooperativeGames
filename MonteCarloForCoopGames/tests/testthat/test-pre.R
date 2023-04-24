test_that("Pre Standard", {
  result <- pre(c(1, 2, 5, 8, 7, 6), 8)

  expect_equal(result, c(1, 2, 5))
})


test_that("Pre First", {
  result <- pre(c(1, 2, 5, 8, 7, 6), 1)

  expect_equal(result, c())
})

test_that("Pre Last", {
  result <- pre(c(1, 2, 5, 8, 7, 6), 6)

  expect_equal(result, c(1, 2, 5, 8, 7))
})
