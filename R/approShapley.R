#' @name approShapley
#' @title Appro Shapley
#' @description
#' Approximates the Shapley value by using a simple Monte Carlo simulation.
#' @details
#' Appro Shapley is a sampling method, which samples orders of the players.
#' In each of these orders the marginal contribution of each player in the
#' current order will be calculated to estimate the real Shapley value.
#' Note that it is possible that the provided sample size is not divisible by
#' the number of players. In that case the remaining samples ```m %% n``` will
#' not be used. This preserves the efficiency (i.e. the sum of the result vector
#' is 1) of the algorithm.
#' Based on: "Polynomial calculation of the Shapley value based on sampling"
#' (Javier Castro Et al., 2008)
#' @template details/BigQSupport
#' @template author/JM
#' @template author/DU
#' @template author/TP
#' @template author/EW
#' @template author/MS
#' @template param/n
#' @template param/m
#' @template param/v
#' @template return/Sh
#' @export
#' @template cites/CASTRO_ET_AL_2008
#' @templateVar CASTRO_ET_AL_2008_P pp. 1727
#' @examples
#' approShapley(10, 10000, gloveGameForSampling(1:5, 6:10))
#' \donttest{
#' # sample an airport game as described in section 4.3 of the underlying paper
#' costs <- buildAirportCostVector(list(
#'   c(1, 8), c(2, 12), c(3, 6), c(4, 14), c(5, 8), c(6, 9),
#'   c(7, 13), c(8, 10), c(9, 10), c(10, 10)
#' ))
#' v <- airportGameForSampling(costs)
#' Sh <- approShapley(length(costs), 1000000, v)
#' }
approShapley <- function(n, m, v) {
  using_bigz <- is.bigz(m)

  check_m_n(m, n, factorial(n), using_bigz)
  check_v(v)

  N <- 1:n

  # First it is checked if a bigz is used. Than the follow variables are initialized with the right type.
  # Sh: create list that will be used to store the Shapley values for each player
  # m_O: with one sample a value is calculated for all players, so the sample size can be divided by n
  # idx_m: counter for the samples
  if (using_bigz) {
    Sh <- rep(as.bigz(0), n)
    m_O <- as.bigz(m / n)
    idx_m <- as.bigz(0)
  } else {
    Sh <- rep(0, n)
    m_O <- as.integer(m / n)
    idx_m <- 0
  }

  # Calculate the Shapley value
  while (idx_m < m_O) {
    O <- sample(N)

    idx_n <- 1
    while (idx_n <= n) {
      sh_i <- v(take(O, idx_n)) - v(take(O, idx_n - 1))
      i <- O[idx_n]
      Sh[i] <- Sh[i] + sh_i
      idx_n <- idx_n + 1
    }

    idx_m <- idx_m + 1
  }

  Sh <- Sh / m_O

  Sh
}
