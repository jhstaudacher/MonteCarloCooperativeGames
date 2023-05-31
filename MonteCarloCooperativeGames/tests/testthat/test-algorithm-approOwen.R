test_that("approOwen", {
  skip_if(skip_tests)
  skip_on_cran()

  v <- setup_game()
  exact_shap <- setup_exact_shap()

  print("----------------------------------")
  print("---------- approOwen  ------------")
  print("----------------------------------")
  calculated_owen <- approOwen(188, 188 * 10000, v, list(c(1:188)))
  for (i in 1:188) {
    if (with_prints) {
      print(paste("i: ", format(i, width = 3), " Calculated: ", format(calculated_owen[i], nsmall = 17, scientific = FALSE), " Expected: ", format(exact_shap[i], nsmall = 17, scientific = FALSE), " Abs Difference: ", format(abs(calculated_owen[i] - exact_shap[i]), nsmall = 17, scientific = FALSE)))
    }

    if (with_expects) {
      expect_equal(calculated_owen[i], exact_shap[i], tolerance = 0.01)
    }

    if (with_prints && with_expects) {
      cat("\n")
    }
  }
})
