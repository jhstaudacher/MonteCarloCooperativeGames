#' @name simpleRandomSamplingWithReplacement
#' @title Simple Random Sampling With Replacement
#' @description Simple random sampling with replacement based on paper:
#' "Statistics and game theory: Estimating coalitional values in R software" (A.
#' Saavedra-Nieves, 2020) Algorithm 1
#' @template author/AR
#' @template param/i
#' @template param/n
#' @template param/m
#' @template param/v
#' @template return/Banzhaf
#' @template cites/SAAVEDRA_NIEVES_ET_AL_2020
#' @templateVar SAAVEDRA_NIEVES_ET_AL_2020_P pp. 131
#' @export
#' @examples
#' print(simpleRandomSamplingWithReplacement(1, 10, 200, gloveGame(1:5, 6:10)))
simpleRandomSamplingWithReplacement <- function(i, n, m, v) {
  stopifnot(0 < i) # Player(-index) cannot be negative.
  stopifnot(i <= n) # Player has to be in the list of all players.
  
  stopifnot(0 < n) # At least one player is needed.
  
  stopifnot(0 < m) # At least one sample must be taken.
  stopifnot(m <= 2^(n - 1)) # Number of samples too large.
  
  player_i <- i
  all_players <- 1:n
  sampling_size <- m
  game <- v
  
  stopifnot(player_i < 0)

  banzhaf_value <- 0
  player_i_value <- all_players[player_i]
  players_without_i <- all_players[-player_i]

  for (j in 1:sampling_size) {
    sample <- coalitionFromIndex(players_without_i, sample(1:2^length(players_without_i), 1))
    banzhaf_value <- banzhaf_value + game(append(sample, player_i_value)) - game(sample)
  }

  banzhaf_value <- banzhaf_value / sampling_size
  return(banzhaf_value)
}
