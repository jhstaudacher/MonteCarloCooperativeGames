#' @name approShapleyAntithetic
#' @title Shapley value approximation by antithetic sampling
#' @description ```approShapleyAntithetic``` is a sampling methodology to estimate the
#' Shapley value for all players for a given TU game by using antithetic sampling.
#' @details
#' This algorithm approximates the Shapley value for all players by random
#' antithetic sampling. It is based on "Polynomial calculation of the Shapley value based on sampling"
#' by Javier Castro Et al. (2008) and was extended to make use of antithetic
#' sampling by Staudacher and Pollmann (2023). The base algorithm can also be found in this package
#' under the name ```approShapley```.
#' @template author/TP
#' @template param/n
#' @template param/m
#' @template param/v
#' @template return/Sh
#' @template cites/STAUDACHER_POLLMANN_2023
#' @export
#' @examples
#' approShapleyAntithetic(3, 200, gloveGameForSampling(1:2, 3:3))
approShapleyAntithetic <- function(n, m, v) {
  check_m_n(m, n)
  check_v(v)

  N <- 1:n
  m_per_O <- ceiling(m / n)

  Sh <- rep(0, n)
  for (j in 1:ceiling(m_per_O / 2)) {
    O <- sample(N)
    for (i in N) {
      N_without_i <- N[!N %in% i]
      S <- pre(O, i)
      S_as <- N_without_i[!N_without_i %in% S]
      Sh[i] <- Sh[i] + (v(append(S, i)) - v(S) + v(append(S_as, i)) - v(S_as)) / (m_per_O + m_per_O %% 2)
    }
  }

  Sh
}
