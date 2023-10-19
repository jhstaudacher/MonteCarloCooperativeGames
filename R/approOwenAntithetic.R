#' @name approOwenAntithetic
#' @title Owen value approximation by antithetic sampling
#' @description ```approOwenAntithetic``` is a sampling algorithm to estimate the
#' Owen value for all players for a specified TU game with a system of
#' a priori unions by using antithetic sampling.
#' @details
#' This algorithm approximates the Owen value for all players by random antithetic
#' sampling. It is based on "Estimation of the Owen Value Based on Sampling" by Alejandro
#' Saavedra-Nieves et al. (2018) and was extended to make use of antithetic sampling by Staudacher and
#' Pollmann (2023). The base algorithm can also be found in this package
#' under the name ```approOwen```.
#' @template author/TP
#' @template param/n
#' @template param/m
#' @template param/v
#' @template param/P
#' @template return/Owen
#' @template cites/STAUDACHER_POLLMANN_2023
#' @export
#' @examples
#' approOwenAntithetic(3, 1000, gloveGameForSampling(1:2, 3:3), list(c(1, 2), c(3)))
approOwenAntithetic <- function(n, m, v, P) {
  N <- 1:n
  m_per_i <- ceiling(m / n)
  Ow <- rep(0, n)

  for (j in 1:ceiling(m_per_i / 2)) {
    O <- sampleOrderP(P)
    for (i in N) {
      N_without_i <- N[!N %in% i]
      S <- pre(O, i)
      S_as <- N_without_i[!N_without_i %in% S]
      Ow[i] <- Ow[i] + (v(append(S, i)) - v(S) + v(append(S_as, i)) - v(S_as)) / (m_per_i + m_per_i %% 2)
    }
  }
  Ow
}
