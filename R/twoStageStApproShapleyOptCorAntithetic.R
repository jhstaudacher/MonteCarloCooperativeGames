#' @name twoStageStApproShapleyOptCorAntithetic
#' @title Two Stage Stratified ApproShapley with Optimum Allocation (Corrected) with Antithetic Sampling
#' @description
#' Calculates the Shapley value for each player by using stratification and antithetic sampling. The
#' sample size per stratum is based on each stratums estimated variance.
#' @details
#' ```twoStageStApproShapleyOptCorAntithetic``` is a sampling procedure to estimate the Shapley value for cooperative games.
#' In the first stage, the optimal allocation is calculated and executed in the second stage. Using
#' the original algorithm ```twoStageStApproShapleyOpt``` from Castro et al. (2017), it may occur that
#' more samples than specified are used. If this is the case, this algorithm here adjusts the sample sizes proportionally.
#' Furthermore, in comparison to the original ```twoStageStApproShapleyOpt```, this algorithm
#' was extended to make use of antithetic sampling by Staudacher and Pollmann (2023).
#' @template author/TP
#' @template param/n
#' @template param/v
#' @template param/m
#' @template return/Sh
#' @template cites/STAUDACHER_POLLMANN_2023
#' @export
#' @examples
#' # sample a airport game with 100 players
#' \donttest{
#' costs <- buildAirportCostVector(list(
#'   c(1, 8), c(2, 12), c(3, 6), c(4, 14), c(5, 8), c(6, 9),
#'   c(7, 13), c(8, 10), c(9, 10), c(10, 10)
#' ))
#' v <- airportGameForSampling(costs)
#' Sh <- twoStageStApproShapleyOptCorAntithetic(length(costs), v, 100000)
#' print(Sh)
#' }
#' print(twoStageStApproShapleyOptCorAntithetic(3, gloveGameForSampling(1:2, 3:3), 1000))
twoStageStApproShapleyOptCorAntithetic <- function(n, v, m) {
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

      for (j in 1:ceiling(M_exp[i, l] / 2)) {
        S <- sample(N_without_i, size = l - 1)
        x <- v(append(S, i)) - v(S)
        Sh[i, l] <- Sh[i, l] + x
        sum_quad_l <- sum_quad_l + x^2

        S <- get_antithetic_S_for_l(N_without_i, S, l)
        x <- v(append(S, i)) - v(S)
        Sh[i, l] <- Sh[i, l] + x
        sum_quad_l <- sum_quad_l + x^2
      }
      S_Squared[i, l] <- (sum_quad_l - (Sh[i, l]^2 / (M_exp[i, l] + M_exp[i, l] %% 2))) / (M_exp[i, l] + M_exp[i, l] %% 2 - 1)
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
      if (M_st[i, l] > 0) {
        for (j in 1:ceiling(M_st[i, l] / 2)) {
          S <- sample(N_without_i, size = l - 1)
          xOi <- v(append(S, i)) - v(S)
          Sh[i, l] <- Sh[i, l] + xOi

          S <- get_antithetic_S_for_l(N_without_i, S, l)
          x <- v(append(S, i)) - v(S)
          Sh[i, l] <- Sh[i, l] + x
        }
      }
    }
  }

  Sh <- Sh / (M_exp + M_exp %% 2 + M_st + M_st %% 2)
  Sh <- rowMeans(Sh)
  Sh
}

# The method returns an antithetic sample for a given subset S and position l.
get_antithetic_S_for_l <- function(N_without_i, S, l) {
  new_S <- N_without_i[!N_without_i %in% S]

  if (length(new_S) < (l - 1)) {
    diff <- (l - 1) - length(new_S)
    additional_S <- sample(N_without_i[!N_without_i %in% new_S], size = diff)
    new_S <- append(new_S, additional_S)
  }

  if (length(new_S) > (l - 1)) {
    new_S <- sample(new_S, size = (l - 1))
  }

  new_S
}
