#' @name twoStageStApproShapleyOptEstimateSamples
#' @title Two Stage St Appro Shapley Opt Estimate Samples
#' @description
#' Estimates the sample count m that should be used with
#' 'twoStageStApproShapleyOpt', based on the given error and probability.
#' The returned m leads to an estimation of shapley values where the difference
#' between estimated Shapley value and actual Shapley value is smaller than or
#' equal to 'error' with a probability of 'probability'.
#' @details Proposition 4.2
#' @template author/DU
#' @template param/n
#' @template param/v
#' @param error The error (difference between actual shapley value and estimated
#' shapley value) we do not want to exceed.
#' @param probability How likely it is that we stay below 'error'.
#' @return An estimate for m/the sample size (upper bound).
#' @template cites/CASTRO_ET_AL_2017
#' @templateVar CASTRO_ET_AL_2017_P pp. 184
#' @export
#' @examples
#' v <- gloveGameForSampling(L = 1:5, R = 6:10)
#' m <- twoStageStApproShapleyOptEstimateSamples(10, v, 0.2, 0.8)
twoStageStApproShapleyOptEstimateSamples <- function(n, v, error, probability) {
  alpha <- 1 - probability
  x_min <- v(c())
  x_max <- v(1:n)

  variance_upper_bound <- (x_max - x_min)^2 / 4
  Z_alpha_half <- qnorm(1 - alpha/2)

  m <- Z_alpha_half^2 * n * variance_upper_bound^2 / error

  m
}
