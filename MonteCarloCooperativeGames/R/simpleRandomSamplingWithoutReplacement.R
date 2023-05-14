#' @name simpleRandomSamplingWithoutReplacement
#' @title Simple random sampling without replacement
#' @description Simple random sampling without replacement based on paper:
#' "Statistics and game theory: Estimating coalitional values in R software" (A.
#' Saavedra-Nieves, 2020) Algorithm 2
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
#' print(simpleRandomSamplingWithoutReplacement(1, 10, 200, gloveGame(1:5, 6:10)))
simpleRandomSamplingWithoutReplacement <- function(i, n, m, v) {
  player_i <- i
  all_players <- 1:n
  sampling_size <- m
  game <- v

  stopifnot(1 < sampling_size)
  stopifnot(sampling_size <= 2^(length(all_players) - 1))

  banzhaf_value <- 0
  player_i_value <- all_players[player_i]
  players_without_i <- all_players[-player_i]
  random_numbers <- sample(1:2^(length(all_players) - 1), sampling_size, replace = FALSE)

  for (j in 1:sampling_size) {
    sample <- coalitionFromIndex(players_without_i, random_numbers[j])
    banzhaf_value <- banzhaf_value + game(append(sample, player_i_value)) - game(sample)
  }

  banzhaf_value <- banzhaf_value / sampling_size
  return(banzhaf_value)
}
