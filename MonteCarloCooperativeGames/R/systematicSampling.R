#' @name systematicSampling
#' @title Systematic Sampling
#' @description Systematic sampling based on paper: "Statistics and game theory:
#' Estimating coalitional values in R software" (A. Saavedra-Nieves, 2020)
#' Algorithm 3
#' @template author/AR
#' @template param/i
#' @template param/n
#' @template param/m
#' @template param/v
#' @template return/Banzhaf
#' @template cites/SAAVEDRA_NIEVES_ET_AL_2020
#' @templateVar SAAVEDRA_NIEVES_ET_AL_2020_P pp. 132
#' @export
#' @examples
#' print(systematicSampling(1, 10, 200, gloveGame(1:5, 6:10)))
systematicSampling <- function(i, n, m, v) {
  stopifnot(0 < i) # Player(-index) cannot be negative.
  stopifnot(i <= n) # Player has to be in the list of all players.
  
  stopifnot(0 < n) # At least one player is needed.
  
  stopifnot(0 < m) # At least one sample must be taken.
  stopifnot(m <= 2^(n - 1)) # Number of samples too large.
  
  player_i <- i
  all_players <- 1:n
  sampling_size <- m
  game <- v

  banzhaf_value <- 0
  player_i_value <- all_players[player_i]
  players_without_i <- all_players[-player_i]
  increment <- floor((2^(length(all_players) - 1)) / sampling_size)
  starting_point <- sample(1:increment, 1)
  indices <- c()

  for (j in seq(starting_point, 2^(length(all_players) - 1), by = increment)) {
    indices <- append(indices, j)
  }

  for (j in 1:sampling_size) {
    sample <- coalitionFromIndex(players_without_i, indices[j])
    banzhaf_value <- banzhaf_value + game(append(sample, player_i_value)) - game(sample)
  }

  banzhaf_value <- banzhaf_value / sampling_size
  return(banzhaf_value)
}
