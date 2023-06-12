#' @name stApproShapley
#' @title Stratified Appro Shapley
#' @description
#' Monte Carlo Simulation for calculating the Shapley value with stratification.
#' @details
#' This function uses stratification to approximate the Shapley value. A stratum is
#' created for the given player at each possible position.
#' Note that it is possible that the provided sample size is not divisible by
#' the number of positions the player can occur in. In that case the remaining samples
#' m %% n will be randomly distributed over the strata, so some strata will have the
#' sample size m / n and others the size m / n + 1.
#' Based on the paper "Improving polynomial estimation of the Shapley value by
#' stratified random sampling with optimum allocation" by Castro et al. from 2017.
#' @template author/TP
#' @template param/i
#' @template param/n
#' @template param/v
#' @template param/m
#' @template return/Sh_i
#' @template cites/CASTRO_ET_AL_2017
#' @templateVar CASTRO_ET_AL_2017_P pp. 5
#' @export
#' @examples
#' stApproShapley(1, 10, gloveGameForSampling(1:5, 6:10), 1000)
#' \donttest{
#' # sample an airport game as described in section 6.3 of the underlying paper
#' costs <- buildAirportCostVector(list(
#'   c(1, 8), c(2, 12), c(3, 6), c(4, 14), c(5, 8), c(6, 9),
#'   c(7, 13), c(8, 10), c(9, 10), c(10, 10)
#' ))
#' v <- airportGameForSampling(costs)
#' Sh <- rep(0, length(costs))
#' for (i in 1:length(costs)) {
#'   Sh[i] <- stApproShapley(i, length(costs), v, 5000)
#' }
#' }
stApproShapley <- function(i, n, v, m) {
  check_n_i(n, i)
  check_v(v)
  check_positive_number(m)

  # accumulated marginal contribution of player i over all positions l
  sh_sum <- 0
  # player  set
  N <- 1:n
  # sample size of player i in position l
  m_il <- m / n
  m_il_rest <- sample(c(rep(1, m %% n), rep(0, n - m %% n)))

  # for every position l in which player i occurs
  for (l in N) {
    # accumulated marginal contribution of player i in position l
    x_il <- 0
    m_il_corr <- m_il + m_il_rest[l]

    # sample m_il_corr times and calculate each marginal contribution
    for (idx in 1:m_il_corr) {
      S <- sample(N[N != i], size = l - 1)
      x_il <- x_il + (v(append(S, i)) - v(S))
    }

    # add average marginal contribution of player i in position l
    sh_sum <- sh_sum + x_il / m_il_corr
  }

  # calculate average marginal contribution of player i
  sh_sum / n
}