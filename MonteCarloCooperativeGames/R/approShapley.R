#' @name approShapley
#' @title Appro Shapley
#' @description
#' Approximates the Shapley value by using a simple Monte Carlo simulation.
#' @details
#' Monte Carlo Simulation based on the 2008 paper by Javier Castro
#' @template param/n
#' @template param/m
#' @template param/v
#' @template return/Sh
#' @export
#' @template cites/CASTRO_ET_AL_2008
#' @templateVar CASTRO_ET_AL_2008_P pp. 1727
#' @examples
#' print(approShapley(10, 10000, gloveGame(1:5, 6:10)))
approShapley <- function(n, m, v) {
  # N is a list of players with the specified length according to the number of players (n)
  N <- 1:n

  # Initialize a list that will be used to store the Shapley values for each player
  # It is initialized with 0
  Sh <- rep(0, n)

  # Loop for all samples
  # The number m (number of samples) is divided by the player count because in the inner loop each player is sampled
  for (x in 1:(m / n)) {
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
  Sh <- Sh / (m / n)

  Sh
}
