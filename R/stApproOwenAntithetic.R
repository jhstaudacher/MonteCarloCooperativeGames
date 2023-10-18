#' @name stApproOwenAntithetic
#' @title A stratified antithetic sampling procedure to estimate coalitional values.
#' @description stApproOwenAntithetic is a sampling methodology to estimate coalitional values, in particular the
#' Owen value for a specified TU game with a system of a priori unions, based on stratified and antithetic sampling.
#' @details
#' The algorithm combines stratified with antithetic sampling. The algorithm used
#' for the extension to antithetic sampling is based on the algorithm proposed by
#' A. Saavedra-Nieves (2022). This algorithm can also be found in this package
#' under the name ```stApproOwenAndBanzhafOwen```.
#' @template author/TP
#' @template param/i
#' @template param/m
#' @template param/v
#' @template param/P
#' @template cites/STAUDACHER_POLLMANN_2023
#' @templateVar STAUDACHER_POLLMANN_2023_P PLACEHOLDER PAGES
#' @return Approximation of the Owen value based on stratified antithetic sampling.
#' @export
#' @examples
#' stApproOwenAntithetic(1, 1000, gloveGameForSampling(1:2, 3:3), list(c(1, 2), c(3)))
stApproOwenAntithetic <- function(i, m, v, P) {
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
  N <- unlist(P)
  n <- length(N)
  N_without_i <- N[!N %in% i]

  W <- matrix(
    rep(1 / (p * p_i), p * ceiling(p_i / 2)),
    nrow = p,
    ncol = ceiling(p_i / 2)
  )
  M <- M <- ceiling(m * W)

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
