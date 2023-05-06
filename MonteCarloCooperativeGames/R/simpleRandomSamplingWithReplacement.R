#' @name simpleRandomSamplingWithReplacement
#' @title Simple Random Sampling With Replacement
#' @description Simple random sampling with replacement based on paper:
#' "Statistics and game theory: Estimating coalitional values in R software" (A.
#' Saavedra-Nieves, 2020) Algorithm 1
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
  player_i <- i
  all_players_N <- 1:n
  sampling_size_l <- m
  game_v <- v
  
  stopifnot(1 < sampling_size_l)
  stopifnot(sampling_size_l <= 2^(length(all_players_N) - 1))

  banzhaf_value <- 0
  player_i_value <- all_players_N[player_i]
  players_N_without_i <- all_players_N[-player_i]

  for (j in 1:sampling_size_l) {
    sample_T_j <- coalitionFromIndex(players_N_without_i, sample(1:2^length(players_N_without_i), 1))
    banzhaf_value <- banzhaf_value + game_v(append(sample_T_j, player_i_value)) - game_v(sample_T_j)
  }

  banzhaf_value <- banzhaf_value / sampling_size_l
  return(banzhaf_value)
}