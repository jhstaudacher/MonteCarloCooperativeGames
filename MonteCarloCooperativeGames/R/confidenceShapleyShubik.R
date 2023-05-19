#' @name confidenceShapleyShubik
#' @title Confidence Shapley Shubik
#' @description
#' Monte Carlo Simulation for calculating the Shapley-Shubik power index in a simple game based on the paper "Approximating power indices: theoretical and empirical analysis" by Bachrach et al. from 2010
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
#' confidenceShapleyShubik(3, 4, weightedVotingGameForSampling(c(1, 1, 2, 3), 1 / 2), 0.95, 0.01)
confidenceShapleyShubik <- function(i, n, v, conf, w) {
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

  while (k < k_required) {
    k <- k + 1

    # sample random order
    O <- sample(N)
    pre_i <- pre(O, i)

    # check if player i is critical
    if (v(append(pre_i, i)) - v(pre_i) == 1) {
      X <- X + 1
    }
  }

  # calculate Shapley-Shubik power index of player i
  sh_i <- X / k
  # update e based on used samples
  # => e should be a little smaller since k > k_required
  e <- sqrt(1 / (2 * k) * log(2 / delta))
  # calculate confidence interval
  c(sh_i - e, sh_i + e)
}
