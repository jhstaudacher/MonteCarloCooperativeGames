#' Monte Carlo Simulation for calculating the Banzhaff value in a simple game based on the paper "Approximating power indices: theoretical and empirical analysis" by Bachrach et al. from 2010
#'
#' @param i Player
#' @param n Number of players
#' @param v Characteristic function
#' @param conf Confidence of the true Banzhaff value being in the confidence interval
#' @param w Width of the confidence interval
#'
#' @return Confidence interval as a vector of size 2
#' @export
#'
#' @examples
#' confidenceBanzhaff(3, 4, weightedVotingGame(c(1, 1, 2, 3), 1/2), 0.95, 0.01)

confidenceBanzhaff <- function(i, n, v, conf, w) {
  # X is the critical count
  X <- 0
  # k is the sample count
  k <- 0
  # e is the max error
  e <- w / 2
  # delta is the probability that we miss the confidence interval
  delta <- 1 - conf
  # min samples needed
  k_required <- log(2/delta) / (2 * e^2)
  N <- 1:n
  N_without_i <- N[!N %in% i]
  # probabilities of different sample sizes
  C_size_probs <- c()
  for (j in 0:(n-1)) {
    C_size_probs <- append(C_size_probs, choose(n-1, j))
  }

  while (k < k_required) {
    k <- k + 1

    # sample a random coalition
    C_without_i <- sample(N_without_i, sample(0:(n-1), 1, prob=C_size_probs))
    C <- if(length(C_without_i) == 0) c(i) else append(C_without_i, i)

    # check if player i is critical
    if (v(C) - v(C_without_i) == 1) {
      X <- X + 1
    }
  }

  # calculate Banzhaff value of player i
  b_i <- X / k
  # update e based on used samples
  # => e should be a little smaller since k > k_required
  e = sqrt(1 / (2*k) * log(2/delta))
  # calculate confidence interval
  c(b_i - e, b_i + e)
}
