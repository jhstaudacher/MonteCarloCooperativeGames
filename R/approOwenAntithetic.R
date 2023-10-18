#' @name approOwenAntithetic
#' @title An antithetic sampling procedure to estimate the Owen value.
#' @description approOwenAntithetic is a sampling methodology to estimate the
#' Owen value for a given player ```i``` and a specified TU game with a system of
#' a priori unions by antithetic sampling.
#' @details
#' This algorithm approximates the Owen value for one player by random antithetic
#' sampling. It is based on "Estimation of the Owen Value Based on Sampling" by Alejandro
#' Saavedra-Nieves et al. (2018) and was extended to make use of antithetic sampling by Staudacher and
#' Pollmann (2023).
#' @template author/TP
#' @template param/i
#' @template param/m
#' @template param/v
#' @template param/P
#' @template return/Owen_i
#' @template cites/STAUDACHER_POLLMANN_2023
#' @templateVar STAUDACHER_POLLMANN_2023_P PLACEHOLDER PAGES
#' @export
#' @examples
#' approOwenAntithetic(1, 1000, gloveGameForSampling(1:2, 3:3), list(c(1, 2), c(3)))
approOwenAntithetic <- function(i, m, v, P) {
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
  N_without_i <- N[!N %in% i]

  O_i <- 0
  for (j in 1:ceiling(m / 2)) {
    R <- sample(P_without_P_i, sample(0:(p - 1), 1))
    Q <- sample(P_i_without_i, sample(0:(p_i - 1), 1))
    S <- unlist(append(R, Q))
    S_as <- N_without_i[!N_without_i %in% S]
    O_i <- O_i + (v(append(S, i)) - v(S) + v(append(S_as, i)) - v(S_as)) / (m + m %% 2)
  }
  O_i
}
