#' @name twoStageStApproShapleyOptCor
#' @title Two Stage Stratified ApproShapley with Optimum Allocation (Corrected)
#' @description
#' Calculates the Shapley value for each player by using stratification. The
#' sample size per stratum is based on each stratums estimated variance.
#' @details
#' ```twoStageStApproShapleyOptCor``` is a sampling procedure to estimate the Shapley value for cooperative games.
#' In the first stage, the optimal allocation is calculated and executed in the second stage. Using
#' the original algorithm ```twoStageStApproShapleyOpt``` from Castro et al. (2017), it may occur that
#' more samples than specified are used. If this is the case, this algorithm here adjusts the sample sizes proportionally.
#' Modified according to: "Improving polynomial estimation of the Shapley value by stratified
#' random sampling with optimum allocation" (J. Castro Et al., 2017)
#' @template author/MS
#' @template author/TP
#' @template param/n
#' @template param/v
#' @template param/m
#' @template return/Sh
#' @template cites/CASTRO_ET_AL_2017
#' @templateVar CASTRO_ET_AL_2017_P pp. 182
#' @export
#' @examples
#' # sample a airport game with 100 players
#' \donttest{
#' costs <- buildAirportCostVector(list(
#'   c(1, 8), c(2, 12), c(3, 6), c(4, 14), c(5, 8), c(6, 9),
#'   c(7, 13), c(8, 10), c(9, 10), c(10, 10)
#' ))
#' v <- airportGameForSampling(costs)
#' Sh <- twoStageStApproShapleyOptCor(length(costs), v, 100000)
#' print(Sh)
#' }
#' print(twoStageStApproShapleyOptCor(3, gloveGameForSampling(1:2, 3:3), 1000))
twoStageStApproShapleyOptCor <- function(n, v, m) {
  check_v(v)
  check_m(m)
  check_natural_number(n)
  if (m < 4 * n^2) {
    stop("The provided sample size results is too small. Please increase the sample size to at least m = 4 * n^2")
  }

  N <- 1:n
  S_Squared <- matrix(rep(0, n^2), nrow = n, ncol = n)
  Sh <- matrix(rep(0, n^2), nrow = n, ncol = n)
  M_exp <- matrix(rep(ceiling(m / (2 * n^2)), n^2), nrow = n, ncol = n)

  for (i in 1:n) {
    N_without_i <- N[N != i]
    for (l in 1:n) {
      sum_quad_l <- 0
      j <- 0
      while (j < M_exp[i, l]) {
        j <- j + 1

        S <- sample(N_without_i, size = l - 1)
        x <- v(append(S, i)) - v(S)
        Sh[i, l] <- Sh[i, l] + x
        sum_quad_l <- sum_quad_l + x^2
      }
      S_Squared[i, l] <- (sum_quad_l - (Sh[i, l]^2 / M_exp[i, l])) / (M_exp[i, l] - 1)
    }
  }

  M <- ceiling(m * S_Squared / sum(S_Squared))
  M_st <- M - M_exp
  while (sum(M_st < 0)) {
    M_st[M_st < 0] <- 0
    m_diff <- sum(M_st) + sum(M_exp) - m
    M_st[M_st > 0] <- M_st[M_st > 0] - ceiling(m_diff * S_Squared[M_st > 0] / sum(S_Squared))
  }

  for (i in 1:n) {
    N_without_i <- N[N != i]
    for (l in 1:n) {
      j <- 0
      while (j < M_st[i, l]) {
        j <- j + 1
        S <- sample(N_without_i, size = l - 1)
        xOi <- v(append(S, i)) - v(S)
        Sh[i, l] <- Sh[i, l] + xOi
      }
    }
  }

  Sh <- Sh / (M_exp + M_st)
  Sh <- rowMeans(Sh)
  Sh
}
