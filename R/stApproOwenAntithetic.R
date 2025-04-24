#' @name stApproOwenAntithetic
#' @title Owen value approximation by stratified antithetic sampling
#' @description ```stApproOwenAntithetic``` is a sampling algorithm to estimate the
#' Owen value for a given player ```i``` for a specified TU game with a system of
#' a priori unions by using stratified antithetic sampling.
#' @details
#' This algorithm approximates the Owen value for one player by stratified
#' antithetic sampling. It is based on "On stratified sampling for estimating coalitional
#' values" by A. Saavedra-Nieves (2022) and was extended to make use of antithetic
#' sampling by Staudacher and Pollmann (2023). The base algorithm can also be found in this package
#' under the name ```stApproOwenAndBanzhafOwen```.
#' Note that the sample allocation over the strata is proportional to the weights of the strata.
#' We refer to Staudacher and Pollmann (2023) for more details, especially regarding an edge
#' case that occurs when the number precoalitions and the number of players in player
#' i's union are both odd.
#' @template author/TP
#' @template param/i
#' @template param/m
#' @template param/v
#' @template param/P
#' @template return/Owen_i
#' @template cites/STAUDACHER_POLLMANN_2023
#' @export
#' @examples
#' stApproOwenAntithetic(1, 1000, gloveGameForSampling(1:2, 3:3), list(c(1, 2), c(3)))
stApproOwenAntithetic <- function(i, m, v, P) {
  N <- unlist(P)
  n <- length(N)

  check_n_i(n, i)
  check_P(P)
  check_m(m)
  check_v(v)

  p <- length(P)
  P_i_idx <- 0
  for (j in 1:p) {
    if (i %in% P[[j]]) {
      P_i_idx <- j
      break
    }
  }
  P_i <- P[[P_i_idx]]
  P_i_without_i <- P_i[!P_i %in% i]
  p_i <- length(P_i)
  P_without_P_i <- P[-P_i_idx]
  N_without_i <- N[!N %in% i]

  W <- matrix(
    rep(1 / (p * p_i), p * ceiling(p_i / 2)),
    nrow = p,
    ncol = ceiling(p_i / 2)
  )
  M <- ceiling(m * W)

  if (p_i %% 2 == 1 && p %% 2 == 1) {
    M[(p - 1) / 2 + 1, (p_i - 1) / 2 + 1] <- ceiling(M[(p - 1) / 2 + 1, (p_i - 1) / 2 + 1] / 2)
  }

  O_i <- 0
  for (k in 0:(p - 1)) {
    for (h_tilde in 0:(ceiling(p_i / 2) - 1)) {
      if (p_i %% 2 == 1 && h_tilde == (p_i - 1) / 2 && k >= ceiling(p / 2)) {
        next
      }
      O_tmp <- 0
      O_tmp_as <- 0
      for (j in 1:M[k + 1, h_tilde + 1]) {
        R <- sample(P_without_P_i, k)
        Q <- sample(P_i_without_i, h_tilde)
        S <- unlist(append(R, Q))
        S_as <- N_without_i[!N_without_i %in% S]
        O_tmp <- O_tmp + (v(append(S, i)) - v(S)) / M[k + 1, h_tilde + 1]
        O_tmp_as <- O_tmp_as + (v(append(S_as, i)) - v(S_as)) / M[k + 1, h_tilde + 1]
      }
      if (p %% 2 == 1 && p_i %% 2 == 1 && k == (p - 1) / 2 && h_tilde == (p_i - 1) / 2) {
        O_i <- O_i + (W[k + 1, h_tilde + 1] / 2) * (O_tmp + O_tmp_as)
      } else {
        O_i <- O_i + W[k + 1, h_tilde + 1] * (O_tmp + O_tmp_as)
      }
    }
  }
  O_i
}
