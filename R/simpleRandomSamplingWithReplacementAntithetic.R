#' @name simpleRandomSamplingWithReplacementAntithetic
#' @title A antithetic sampling procedure to estimate the Banzhaf value.
#' @description simpleRandomSamplingWithReplacementAntithetic is a sampling methodology to estimate the
#' Banzhaf value for a specified player ```i``` and a specified TU game by antithetic sampling.
#' @details
#' This algorithm approximates the Banzhaf value for a given player ```i``` by random
#' antithetic sampling. It is based on "Statistics and game theory: Estimating coalitional
#' values in R software" by A. Saavedra-Nieves (2020) and was extended to make use of antithetic
#' sampling by Staudacher and Pollmann (2023). The base algorithm can also be found in this package
#' under the name ```simpleRandomSamplingWithReplacement```.
#' @template author/TP
#' @template param/i
#' @template param/n
#' @template param/m
#' @template param/v
#' @template return/Bz_i
#' @template cites/STAUDACHER_POLLMANN_2023
#' @templateVar STAUDACHER_POLLMANN_2023_P PLACEHOLDER PAGES
#' @export
#' @examples
#' simpleRandomSamplingWithReplacementAntithetic(1, 3, 200, gloveGameForSampling(1:2, 3:3))
simpleRandomSamplingWithReplacementAntithetic <- function(i, n, m, v) {
  N <- 1:n
  Bz_i <- 0

  s_probs <- c()
  for (j in 0:(n - 1)) {
    s_probs <- append(s_probs, choose(n - 1, j))
  }

  N_without_i <- N[!N %in% i]
  for (j in 1:ceiling(m / 2)) {
    s <- sample(0:(n - 1), 1, prob = s_probs)
    S <- sample(N_without_i, s)
    S_as <- N_without_i[!N_without_i %in% S]
    Bz_i <- Bz_i + (v(append(S, i)) - v(S) + v(append(S_as, i)) - v(S_as)) / (m + m %% 2)
  }

  Bz_i
}
