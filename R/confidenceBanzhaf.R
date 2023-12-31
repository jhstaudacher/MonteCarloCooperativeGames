#' @name confidenceBanzhaf
#' @title Confidence Banzhaf
#' @description
#' Monte Carlo Simulation for approximating the Banzhaf value in a simple game.
#' @details
#' This function calculates an approximation of the Banzhaf value. You do not
#' have to provide a sample size. The needed sample size is calculated inside
#' the function based on the given confidence and interval width. The interval
#' width is the width of the interval of the approximated Banzhaf value
#' (accuracy) while the confidence is the probability of the true (exact)
#' Banzhaf value being in this interval. Decreasing the interval width and/ or
#' increasing the confidence will lead to an increase of the required samples
#' and a longer computation time.
#' The method is based on the paper "Approximating power indices: theoretical
#' and empirical analysis" by Bachrach et al. from 2010.
#' @template author/TP
#' @template param/i
#' @template param/n
#' @template param/v
#' @template param/conf
#' @template param/w
#' @template return/ConfInterval
#' @export
#' @template cites/BACHRACH_ET_AL_2010
#' @templateVar BACHRACH_ET_AL_2010_P pp. 105-122
#' @examples
#' confidenceBanzhaf(3, 4, weightedVotingGameForSampling(c(1, 1, 2, 3), 1 / 2), 0.95, 0.01)
#' \donttest{
#' weights <- c(1, 1, 2, 3, 2, 3, 4, 2, 1, 1, 1, 2, 2, 3, 2, 2, 1, 2, 3)
#' v <- weightedVotingGameForSampling(weights, 1 / 2)
#' Bz <- rep(0, length(weights))
#' for (i in 1:length(weights)) {
#'   Bz[i] <- mean(confidenceBanzhaf(i, length(weights), v, 0.95, 0.03))
#' }
#' }
confidenceBanzhaf <- function(i, n, v, conf, w) {
  check_n_i(n, i)
  check_v(v)
  check_conf(conf)
  check_positive_number(w)

  # X is the critical count
  X <- 0
  # k is the sample count
  k <- 0
  # e is the max error
  e <- w / 2
  # delta is the probability that we miss the confidence interval
  delta <- 1 - conf
  # min samples needed
  k_required <- log(2 / delta) / (2 * e^2)
  N <- 1:n
  N_without_i <- N[!N %in% i]
  # probabilities of different sample sizes
  C_size_probs <- c()
  for (j in 0:(n - 1)) {
    C_size_probs <- append(C_size_probs, choose(n - 1, j))
  }

  while (k < k_required) {
    k <- k + 1

    # sample a random coalition
    C_without_i <- sample(N_without_i, sample(0:(n - 1), 1, prob = C_size_probs))
    C <- if (length(C_without_i) == 0) c(i) else append(C_without_i, i)

    # check if player i is critical
    if (v(C) - v(C_without_i) == 1) {
      X <- X + 1
    }
  }

  # calculate Banzhaf value of player i
  b_i <- X / k
  # update e based on used samples
  # => e should be a little smaller since k > k_required
  e <- sqrt(1 / (2 * k) * log(2 / delta))
  # calculate confidence interval
  c(b_i - e, b_i + e)
}
