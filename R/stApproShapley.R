#' @name stApproShapley
#' @title Stratified Appro Shapley
#' @description
#' Monte Carlo Simulation for calculating the Shapley value with stratification.
#' @details
#' This function uses stratification to approximate the Shapley value. A stratum
#' is created for each player at each possible position. The estimated Shapley value
#' is the mean over the estimated Shapley values of all strata.
#' @template author/TP
#' @template param/n
#' @template param/v
#' @template param/m
#' @template return/Sh
#' @template cites/CASTRO_ET_AL_2017
#' @templateVar CASTRO_ET_AL_2017_P pp. 183
#' @export
#' @examples
#' stApproShapley(10, gloveGameForSampling(1:5, 6:10), 1000)
stApproShapley <- function(n, v, m) {
  check_natural_number(n)
  check_v(v)
  check_m(m)

  N <- 1:n
  m_ih <- ceiling(m / (n * n))

  Sh <- rep(0, n)
  for (i in N) {
    N_without_i <- N[N != i]
    for (h in N) {
      Sh_tmp <- 0
      for (j in 1:m_ih) {
        S <- sample(N_without_i, size = h - 1)
        x <- v(append(S, i)) - v(S)
        Sh_tmp <- Sh_tmp + x / m_ih
      }
      Sh[i] <- Sh[i] + Sh_tmp / n
    }
  }
  Sh
}
