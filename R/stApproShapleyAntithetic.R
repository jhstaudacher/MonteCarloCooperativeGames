#' @name stApproShapleyAntithetic
#' @title Shapley value approximation by stratified antithetic sampling
#' @description ```stApproShapleyAntithetic``` is a sampling algorithm to estimate the
#' Shapley value for all players for a given TU game by using stratified antithetic sampling.
#' @details
#' This algorithm approximates the Shapley value for all players by stratified
#' antithetic sampling. It is based on "Improving polynomial estimation of the Shapley value by
#' stratified random sampling with optimum allocation" by Castro et al. (2017)
#' and was extended to make use of antithetic sampling by Staudacher and Pollmann (2023).
#' The base algorithm can also be found in this package under the name ```stApproShapley```.
#' Note that the sample allocation over the strata is proportional to the weights of the strata.
#' We refer to Staudacher and Pollmann (2023) for more details, especially regarding an edge
#' case that occurs when the number of players ```n``` is odd.
#' @template author/TP
#' @template param/i
#' @template param/n
#' @template param/m
#' @template param/v
#' @template return/Sh_i
#' @template cites/STAUDACHER_POLLMANN_2023
#' @export
#' @examples
#' stApproShapleyAntithetic(1, 3, 200, gloveGameForSampling(1:2, 3:3))
stApproShapleyAntithetic <- function(i, n, m, v) {
  check_n_i(n, i)
  check_m(m)
  check_v(v)

  N <- 1:n
  N_without_i <- N[N != i]
  n_strata <- ceiling(n / 2)
  M <- rep(ceiling(m / n), n_strata)

  if (n %% 2 == 1) {
    M[n_strata] <- ceiling(M[n_strata] / 2)
  }

  Sh_i <- 0
  for (h_tilde in 0:(n_strata - 1)) {
    Sh_tmp <- 0
    Sh_tmp_as <- 0
    for (j in 1:M[h_tilde + 1]) {
      S <- sample(N_without_i, size = h_tilde)
      S_as <- N_without_i[!N_without_i %in% S]
      Sh_tmp <- Sh_tmp + (v(append(S, i)) - v(S)) / M[h_tilde + 1]
      Sh_tmp_as <- Sh_tmp_as + (v(append(S_as, i)) - v(S_as)) / M[h_tilde + 1]
    }
    if (n %% 2 == 1 && h_tilde == (n - 1) / 2) {
      Sh_i <- Sh_i + (Sh_tmp + Sh_tmp_as) / (2 * n)
    } else {
      Sh_i <- Sh_i + (Sh_tmp + Sh_tmp_as) / n
    }
  }

  Sh_i
}
