# Because the algorithms involve randomness it is expected that some of the tests fail.
# Use test_active_file() to just all run the tests in the active file.

# float has a approximate precision of 23*log10(2) ≈ 7 digits in base10
# double has a approximate precision of 53*log10(2) ≈ 16 digits in base10

# If there are too many test failures, so the overview is not displayed use testthat::set_max_fails(1)  to limit
# the amount of test failures that are displayed.

skip_tests <- TRUE
with_expects <- TRUE
with_prints <- FALSE

setup_game <- function() {
  total <- read.csv(file = test_path("fixtures", "imf_2015.txt"), sep = ";")$Total
  v <- weightedVotingGameForSampling(total, 0.5)
  v
}
setup_exact_shap <- function() {
  exact_shap <- read.csv(file = test_path("fixtures", "IMF2015Res.csv"), sep = " ")$simp2015Shap
  exact_shap <- as.numeric(sub("%", "", exact_shap))
  exact_shap <- exact_shap / 100
  exact_shap
}
setup_exact_banz <- function() {
  exact_banz <- read.csv(file = test_path("fixtures", "IMF2015Res.csv"), sep = " ")$simp2015Banz
  exact_banz <- as.numeric(sub("%", "", exact_banz))
  exact_banz <- exact_banz / 100
  exact_banz
}

test_that("approShapley", {

  skip_if(skip_tests)
  skip_on_cran()

  v <- setup_game()
  exact_shap <- setup_exact_shap()

  print("----------------------------------")
  print("---------- approShapley ----------")
  print("----------------------------------")
  calculated_shapley <- approShapley(188, 188 * 10000, v)
  for (i in 1:188) {

    if (with_prints) {
      print(paste("i: ", format(i, digits = 3), " Calculated: ", format(calculated_shapley[i], nsmall = 17, scientific = FALSE), " Expected: ", format(exact_shap[i], nsmall = 17, scientific = FALSE), " Abs Difference: ", format(abs(calculated_shapley[i] - exact_shap[i]), nsmall = 17, scientific = FALSE)))
    }

    if (with_expects) {
      expect_equal(calculated_shapley[i], exact_shap[i], tolerance = 0.01)
    }
  }
})

test_that("confidenceShapleyShubik", {

  skip_if(skip_tests)
  skip_on_cran()

  v <- setup_game()
  exact_banz <- setup_exact_banz()

  print("---------------------------------------------")
  print("---------- confidenceShapleyShubik ----------")
  print("---------------------------------------------")
  print("10% (18) should fail on average.")
  tolerance <- 0.01
  for (i in 1:188) {
    c <- confidenceShapleyShubik(i, 188, v, 0.90, tolerance)

    middle <- (c[1] + c[2]) / 2

    if (with_prints) {
      print(paste("i: ", format(i, digits = 3), "Calculated Interval: min: ", format(c[1], nsmall = 17, scientific = FALSE), " max: ", format(c[2], nsmall = 17, scientific = FALSE), "Avg: ", format(middle, nsmall = 17, scientific = FALSE), " Expected: ", format(exact_banz[i], nsmall = 17, scientific = FALSE), "Abs Difference: ", format(abs(middle - exact_banz[i]), nsmall = 17, scientific = FALSE)))
    }
    if (with_expects) {
      expect_equal(middle, exact_banz[i], tolerance = tolerance)
    }
  }
})

test_that("approOwen", {

  skip_if(skip_tests)
  skip_on_cran()

  v <- setup_game()
  exact_shap <- setup_exact_shap()

  print("----------------------------------")
  print("---------- approOwen  ------------")
  print("----------------------------------")
  calculated_owen <- approOwen(188, 188 * 1000, v, list(c(1:188)))
  for (i in 1:188) {
    if (with_prints) {
      print(paste("i: ", format(i, digits = 3), " Calculated: ", format(calculated_owen[i], nsmall = 17, scientific = FALSE), " Expected: ", format(exact_shap[i], nsmall = 17, scientific = FALSE), " Abs Difference: ", format(abs(calculated_owen[i] - exact_shap[i]), nsmall = 17, scientific = FALSE)))
    }
    if (with_expects) {
      expect_equal(calculated_owen[i], exact_shap[i], tolerance = 0.01)
    }
  }
})

test_that("confidenceBanzhaf", {

  skip_if(skip_tests)
  skip_on_cran()

  v <- setup_game()
  exact_banz <- setup_exact_banz()

  print("---------------------------------------------")
  print("---------- confidenceBanzhaf ----------------")
  print("---------------------------------------------")
  print("10% (18) should fail on average.")
  tolerance <- 0.01
  for (i in 1:188) {
    c <- confidenceBanzhaf(180, 188, v, 0.90, tolerance)

    middle <- (c[1] + c[2]) / 2

    if (with_prints) {
      print(paste("i: ", format(i, digits = 3), "Calculated Interval: min: ", format(c[1], nsmall = 17, scientific = FALSE), " max: ", format(c[2], nsmall = 17, scientific = FALSE), "Avg: ", format(middle, nsmall = 17, scientific = FALSE), " Expected: ", format(exact_banz[i], nsmall = 17, scientific = FALSE), "Abs Difference: ", format(abs(middle - exact_banz[i]), nsmall = 17, scientific = FALSE)))
    }
    if (with_expects) {
      expect_equal(middle, exact_banz[i], tolerance = tolerance)
    }
  }
})

test_that("stApproOwenAndBanzhafOwen", {

  skip_if(skip_tests)
  skip_on_cran()

  exact_banz <- setup_exact_banz()
  exact_shap <- setup_exact_shap()
  v <- setup_game()

  print("---------------------------------------------")
  print("---------- stApproOwenAndBanzhafOwen --------")
  print("---------------------------------------------")
  for (i in 1:188) {
    r <- stApproOwenAndBanzhafOwen(i, 10000, v, list(c(1:188)))

    owen <- r[["Owen"]]
    banzhaf_owen <- r[["Banzhaf-Owen"]]

    if (with_prints) {
      print(paste("i: ", format(i, digits = 3), "Calculated Owen:    ", format(owen, nsmall = 17, scientific = FALSE), " Expected: ", format(exact_shap[i], nsmall = 17, scientific = FALSE), " Abs Difference: ", format(abs(owen - exact_shap[i]), nsmall = 17, scientific = FALSE)))
      print(paste("i: ", format(i, digits = 3), "Calculated Banzhaf: ", format(banzhaf_owen, nsmall = 17, scientific = FALSE), " Expected: ", format(exact_banz[i], nsmall = 17, scientific = FALSE), " Abs Difference: ", format(abs(banzhaf_owen - exact_banz[i]), nsmall = 17, scientific = FALSE)))
    }
    if (with_expects) {
      expect_equal(owen, exact_shap[i], tolerance = 0.01)
      expect_equal(banzhaf_owen, exact_banz[i], tolerance = 0.01)
    }
  }
})

test_that("twoStageStApproShapleyOptCor", {

  skip_if(skip_tests)
  skip_on_cran()

  exact_banz <- setup_exact_banz()
  exact_shap <- setup_exact_shap()
  v <- setup_game()

  print("---------------------------------------------")
  print("---------- twoStageStApproShapleyOptCor -----")
  print("---------------------------------------------")
  calculated_shapley <- twoStageStApproShapleyOptCor(188, v, 100000)
  for (i in 1:188) {
    if (with_prints) {
      print(paste("i: ", format(i, digits = 3), " Calculated: ", format(calculated_shapley[i], nsmall = 17, scientific = FALSE), " Expected: ", format(exact_shap[i], nsmall = 17, scientific = FALSE), " Abs Difference: ", format(abs(calculated_shapley[i] - exact_shap[i]), nsmall = 17, scientific = FALSE)))
    }
    if (with_expects) {
      expect_equal(calculated_shapley[i], exact_shap[i], tolerance = 0.01)
    }
  }
})


test_that("twoStageApproBanzhafOwen", {

  skip_if(skip_tests)
  skip_on_cran()

  exact_banz <- setup_exact_banz()
  exact_shap <- setup_exact_shap()
  v <- setup_game()

  print("---------------------------------------------")
  print("---------- twoStageApproBanzhafOwen  --------")
  print("---------------------------------------------")
  for (i in 1:188) {
    banzhaf_owen <- twoStageApproBanzhafOwen(i, 1, 100, v, list(c(1:188)))

    if (with_prints) {
      print(paste("i: ", format(i, digits = 3), "Calculated: ", format(banzhaf_owen, nsmall = 17, scientific = FALSE), " Expected: ", format(exact_banz[i], nsmall = 17, scientific = FALSE), " Abs Difference: ", format(abs(banzhaf_owen - exact_banz[i]), nsmall = 17, scientific = FALSE)))
    }
    if (with_expects) {
      expect_equal(banzhaf_owen, exact_banz[i], tolerance = 0.01)
    }
  }
})
