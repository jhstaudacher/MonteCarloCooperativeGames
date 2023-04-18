test_that("GloveGame Complete", {
  v <- gloveGame (c (1,2,3),c (4,5,6))

  v_result <- v (c (1,2,3,4,5,6))

  expect_equal(v_result, 3)
})

test_that("GloveGame Standard", {
  v <- gloveGame (c (1,2,3),c (4,5,6))

  v_result <- v (c (1,5,6))

  expect_equal(v_result, 1)
})

test_that("GloveGame No Match", {
  v <- gloveGame (c (1,2,3),c (4,5,6))

  v_result <- v (c (4,5,6))

  expect_equal(v_result, 0)
})


test_that("GloveGame No Left", {
  v <- gloveGame (c(),c (4,5,6))

  v_result <- v (c (1,2,3,4,5,6))

  expect_equal(v_result, 0)
})

test_that("GloveGame No Player", {
  v <- gloveGame (c (1,2,3),c (4,5,6))

  v_result <- v (c ())

  expect_equal(v_result, 0)
})
