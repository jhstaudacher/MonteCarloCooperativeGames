#' @name approShapley
#' @title Appro Shapley
#' @description
#' Approximates the Shapley value by using a simple Monte Carlo simulation.
#' @details
#' Appro Shapley is a sampling method, which samples permutations of the players.
#' Note that it is possible that the provided sample size is not divisible by
#' the number of players. In that case the remaining samples m %% n will not be
#' used. This preserves the efficiency (i.e. the sum of the result vector is 1)
#' of the algorithm.
#' Based on: "Polynomial calculation of the Shapley value based on sampling" (Javier Castro Et al., 2008)
#' @template author/JM
#' @template author/DU
#' @template author/TP
#' @template author/EW
#' @template author/MS
#' @template param/n
#' @template param/m
#' @template param/v
#' @template return/Sh
#' @export
#' @template cites/CASTRO_ET_AL_2008
#' @templateVar CASTRO_ET_AL_2008_P pp. 1727
#' @examples
#' approShapley(10, 10000, gloveGameForSampling(1:5, 6:10))
#' \donttest{
#' # sample an airport game as described in section 4.3 of the underlying paper
#' costs <- buildAirportCostVector(list(
#'   c(1, 8), c(2, 12), c(3, 6), c(4, 14), c(5, 8), c(6, 9),
#'   c(7, 13), c(8, 10), c(9, 10), c(10, 10)
#' ))
#' v <- airportGameForSampling(costs)
#' Sh <- approShapley(length(costs), 1000000, v)
#' }
approShapley <- function(n, m, v) {
  check_m_n(m, n)
  check_v(v)

  # N is a list of players with the specified length according to the number of players (n)
  N <- 1:n

  # Initialize a list that will be used to store the Shapley values for each player
  # It is initialized with 0
  Sh <- rep(0, n)
  m_O <- as.integer(m / n)

  # Loop for all samples
  # The number m (number of samples) is divided by the player count because in the inner loop each player is sampled
  for (x in 1:m_O) {
    # Get a random permutation of the player list for sampling
    O <- sample(N)

    # Loop though every player in the sample
    # i is not the player (like in the paper) but the idx of the player in the current order O
    for (i in 1:n) {
      # Calculate the marginal contribution with the current player and without the current player
      sh_i <- v(take(O, i)) - v(take(O, i - 1))
      # Get the index of the current player
      player_i <- O[i]
      # Add the marginal contribution to the Shapley value of the current player
      Sh[player_i] <- Sh[player_i] + sh_i
    }
  }

  # Divide the Shapley list by the number of samples.
  Sh <- Sh / m_O

  Sh
}
