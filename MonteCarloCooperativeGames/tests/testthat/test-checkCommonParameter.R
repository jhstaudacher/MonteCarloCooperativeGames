test_that("check_v", {
  expect_error(check_v(NULL)) # NULL
  expect_error(check_v(1)) # not a function

  expect_equal(check_v(function(x) x), NULL)
})


test_that("check_positive_number", {
  expect_error(check_positive_number(NULL))
  expect_error(check_positive_number("asdf")) # not numeric
  expect_error(check_positive_number(0)) # smaller or equal 0
  expect_error(check_positive_number(-1)) # smaller or equal 0

  expect_equal(check_positive_number(3), NULL)
})


test_that("check_n_i", {
  expect_error(check_n_i(3, 5)) # i not in n

  expect_equal(check_n_i(3, 2), NULL)
})


test_that("check_P", {
  expect_error(check_P(NULL)) # not NULL
  expect_error(check_P("asdf")) # not a list
  expect_error(check_P(list(c(1, 2, 3), "asdf"))) # not list or vector
  expect_error(check_P(list(c(1, 2, 3), c(4, 5, "asdf")))) # not numeric
  expect_error(check_P(list(c(1, 2, 3), c(4, 5, -6)))) # not positive
  expect_error(check_P(list(c(1, 2, 3), c(3, 5, 6)))) # not disjunct

  expect_equal(check_P(list(c(1, 2, 3), c(4, 5, 6))), NULL)
})


test_that("check_P_i", {
  expect_error(check_P_i(list(c(1, 2, 3), c(4, 5, 6)), 7)) # i not in p

  expect_equal(check_P_i(list(c(1, 2, 3), c(4, 5, 6)), 4), NULL)
})
