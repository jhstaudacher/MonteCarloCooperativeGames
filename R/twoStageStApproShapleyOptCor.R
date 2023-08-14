#' @name twoStageStApproShapleyOptCor
#' @title Two Stage St Appro Shapley Opt Cor
#' @description
#' Calculates the shapley value for each player with stratification and
#' sample size per strata based on each stratums variance samples size adjusted.
#' @details
#' twoStageStApproShapleyOptCor is a sampling procedure to estimate the Shapley value for cooperative games.
#' In the first stage, the optimal allocation is calculated and executed in the second stage. It may occur that
#' more samples would be used than specified. If this is the case, the algorithm adjusts the samples proportionally
#' to the given sample size. The algorithm twoStageStApproShapleyOpt does not correct the sample size.
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
  if (m < 2 * n^2) {
    stop("The provided min_sample_size results in a sample size per stratum which is smaller than one. Please increase the min_sample_size.")
  }

  N <- 1:n
  s_squared <- matrix(rep(0, n^2), nrow = n, ncol = n)
  Sh <- matrix(rep(0, n^2), nrow = n, ncol = n)
  m_exp <- matrix(rep(m %/% (2 * n^2), n^2), nrow = n, ncol = n)

  for (i in 1:n) {
    N_without_i <- N[N != i]
    for (l in 1:n) {
      sum_quad_l <- 0
      j <- 0
      while (j < m_exp[i, l]) {
        j <- j + 1
        S <- sample(N_without_i, size = l - 1)
        x <- v(append(S, i)) - v(S)
        Sh[i, l] <- Sh[i, l] + x
        sum_quad_l <- sum_quad_l + x^2
      }
      s_squared[i, l] <- (sum_quad_l - (Sh[i, l]^2 / m_exp[i, l])) / (m_exp[i, l] - 1)
    }
  }

  # calculate samples for stage 2 and correct them, so they do not exceed n samples
  m_il <- m * s_squared / sum(s_squared)
  m_st <- m_il - m_exp
  sum_negatives <- sum(m_st[m_st < 0])
  m_st[m_st < 0] <- 0
  m_st <- floor(m_st + m_st / sum(m_st) * sum_negatives)
  m_remaining <- m - sum(m_exp) - sum(m_st)
  corrVec <- sample(c(rep(1, m_remaining), rep(0, n^2 - m_remaining)))
  m_st <- m_st + corrVec

  for (i in 1:n) {
    N_without_i <- N[N != i]
    for (l in 1:n) {
      j <- 0
      while (j < m_st[i, l]) {
        j <- j + 1
        S <- sample(N_without_i, size = l - 1)
        xOi <- v(append(S, i)) - v(S)
        Sh[i, l] <- Sh[i, l] + xOi
      }
    }
  }

  Sh <- Sh / (m_exp + m_st)
  Sh <- rowMeans(Sh)
  Sh
}
