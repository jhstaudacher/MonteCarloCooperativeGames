#' @name stApproShapley
#' @title Stratified Appro Shapley
#' @description
#' Monte Carlo Simulation for calculating the Shapley value based on the paper "Improving polynomial estimation of the Shapley value by stratified random sampling with optimum allocation" by Castro et al. from 2017
#' @template author/TP
#' @template param/i
#' @template param/n
#' @template param/v
#' @template param/m
#' @template return/Sh_i
#' @template cites/CASTRO_ET_AL_2017
#' @templateVar CASTRO_ET_AL_2017_P pp. 5
#' @export
#' @importFrom stats rmultinom
#' @examples
#' stApproShapley(1, 10, gloveGameForSampling(1:5, 6:10), 1000)
stApproShapley <- function(i, n, v, m) {
  check_n_i(n, i)
  check_v(v)
  check_positive_number(m)

  # accumulated marginal contribution of player i over all positions l
  sh_sum <- 0
  # player  set
  N <- 1:n
  # sample size of player i in position l
  m_il <- m / n
  m_il_rest <- as.vector(rmultinom(1, size=(m%%n), prob=rep(1, n)))

  # for every position l in which player i occurs
  for (l in N) {
    # accumulated marginal contribution of player i in position l
    x_il <- 0
    m_il_corr <- m_il + m_il_rest[l]

    # sample m_il_corr times and calculate each marginal contribution
    for (idx in 1:m_il_corr) {
      S <- sample(N[N != i], size = l - 1)
      x_il <- x_il + (v(append(S, i)) - v(S))
    }

    # add average marginal contribution of player i in position l
    sh_sum <- sh_sum + x_il / m_il_corr
  }

  # calculate average marginal contribution of player i
  sh_sum / n
}
