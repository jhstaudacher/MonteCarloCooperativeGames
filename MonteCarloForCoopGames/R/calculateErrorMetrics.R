#' Calculates the following error metrics given the estimated shapley values
#' and the actual shapley values:
#'   - total_error (sum of absolute errors)
#'   - mean_error (the mean of absolute errors)
#'   - mean_squared_error (the mean of squared errors)
#'   - root_mean_squared_error (the root of mean_squared_error)
#'   - max_error (the absolute value of the maximum error)
#'   - min_error (the absolute value of the minimal error)
#'   - std_of_errors (the standard deviation of absolute errors)
#'
#' @param estimated_shapley_values The estimated shapley values for each player
#' in form of a vector
#' @param actual_shapley_values The actual shapley values for each player in
#' form of a vector
#'
#' @return The error metrics in a list
#' @export
#'
#' @examples
#' calculateErrorMetrics(c(0.4, 0.6, 0.55), c(0.5, 0.5, 0.5))
calculateErrorMetrics <- function(estimated_shapley_values, actual_shapley_values) {
  errors <- actual_shapley_values - estimated_shapley_values
  abs_errors <- abs(errors)
  total_error <- sum(abs_errors)
  mean_error <- total_error / length(abs_errors)

  squared_errors <- errors**2
  mean_squared_error <- sum(squared_errors) / length(squared_errors)
  root_mean_squared_error <- sqrt(mean_squared_error)

  std <- sd(abs_errors)

  max_error <- max(abs_errors)
  min_error <- min(abs_errors)

  list("total_error" = total_error,
       "mean_error" = mean_error,
       "mean_squared_error" = mean_squared_error,
       "root_mean_squared_error" = root_mean_squared_error,
       "max_error" = max_error,
       "min_error" = min_error,
       "std_of_errors" = std)
}
