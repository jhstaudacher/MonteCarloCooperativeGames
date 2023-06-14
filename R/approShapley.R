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

# calcSampleSize <- function(v, n, err, alpha) {
# x_i_max <- v(c(1:n))
# x_i_min <- v(c())
# x_i_mean <- (x_i_max + x_i_min) / 2
# var <- ((x_i_max - x_i_min)^2) / 4

#  z <- qnorm(1 - alpha / 2)

# m <- ceiling(z^2 * var / err^2)
# m
# }

calcErr <- function(v, n, m, alpha) {
  x_i_max <- v(c(1:n))
  x_i_min <- v(c())
  x_i_mean <- (x_i_max + x_i_min) / 2
  var <- ((x_i_max - x_i_min)^2) / 4
  print(var)

  z <- -qnorm(alpha / 2)

  e <- sqrt(z^2 * var / m)
  e
}

testErrAppro <- function(airport = TRUE) {
  if (airport) {
    # 4.3
    costs <- buildAirportCostVector(list(
      c(1, 8), c(2, 12), c(3, 6), c(4, 14), c(5, 8), c(6, 9),
      c(7, 13), c(8, 10), c(9, 10), c(10, 10)
    ))
    v <- airportGameForSampling(costs)
    n <- length(costs)
  } else {
    # 4.2
    weights <- c(45, 41, 27, 26, 26, 25, 21, 17, 17, 14, 13, 13, 12, 12, 12, 11, 10, 10, 10, 10, 9, 9, 9, 9, 8, 8, 7, 7, 7, 7, 6, 6, 6, 6, 5, 4, 4, 4, 4, 4, 4, 4, 4, 4)
    v <- weightedVotingGameForSampling(weights, 0.5)
    n <- length(weights)
  }

  alpha <- 0.01

  if (airport) {
    # 4.3
    e_ths <- c(0.31, 0.098, 0.031, 0.0098)
  } else {
    # 4.2
    e_ths <- c(0.0475, 0.015, 0.00475, 0.0015)
  }

  for (i in 1:4) {
    e_th_calc <- calcErr(v, n, 10^(i + 2), alpha)
    factor <- e_th_calc / e_ths[i]
    print(paste("samples=", 10^(i + 2), ", e_th_calc=", e_th_calc, ", e_th=", e_ths[i], ", factor=", factor, sep = ""))
  }

  # for (i in 1:4) {
  #  m_real <- calcSampleSize(v, n, e_ths[i], alpha)
  # factor <- m_real / (10^(i+2))
  # print(factor)
  # }
}
