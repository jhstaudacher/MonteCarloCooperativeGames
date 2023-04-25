#' Monte Carlo Simulation for calculating the Shapley value based on the paper "Improving polynomial estimation of the Shapley value by stratified random sampling with optimum allocation" by Castro et al. from 2017
#'
#' @param i Player
#' @param n Number of players
#' @param v Characteristic function
#' @param m Sample size
#'
#' @return Estimated Shapley value of player i
#' @export
#'
#' @examples
#' stApproShapley(1, 10, gloveGame(1:5, 6:10), 1000)
stApproShapley <- function(i, n, v, m) {
  # accumulated marginal contribution of player i over all positions l
  sh_sum <- 0
  # player  set
  N <- 1:n
  # sample size of player i in position l
  m_il <- m / n

  # for every position l in which player i occurs
  for (l in N) {
    # accumulated marginal contribution of player i in position l
    x_il <- 0

    # sample m_il times and calculate each marginal contribution
    for (idx in 1:m_il) {
      S <- sample(N[N != i], size = l - 1)
      x_il <- x_il + (v(append(S, i)) - v(S))
    }

    # add average marginal contribution of player i in position l
    sh_sum <- sh_sum + x_il / m_il
  }

  # calculate average marginal contribution of player i
  sh_sum / n
}
