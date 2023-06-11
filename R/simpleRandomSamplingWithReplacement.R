#' @name simpleRandomSamplingWithReplacement
#' @title Simple Random Sampling With Replacement
#' @description Simple random sampling with replacement is a method to
#'   approximate the Banzhaf value for a specified TU game.
#' @details First, the algorithm takes a sample with replacement of all players
#' without the player i. Then the algorithm checks the value the player adds to
#' the sample. These two steps are repeated for the number of samples.
#'
#' Based on the paper: "Statistics and game theory: Estimating coalitional
#' values in R software" (A. Saavedra-Nieves, 2020) Algorithm 1
#' @template author/AR
#' @template author/RL
#' @template author/MM
#' @template param/i
#' @template param/n
#' @template param/m
#' @template param/v
#' @template return/Banzhaf
#' @template cites/SAAVEDRA_NIEVES_ET_AL_2020
#' @templateVar SAAVEDRA_NIEVES_ET_AL_2020_P pp. 131
#' @import gmp
#' @export
#' @examples
#' print(simpleRandomSamplingWithReplacement(1, 10, 200, gloveGameForSampling(1:5, 6:10)))
simpleRandomSamplingWithReplacement <- function(i, n, m, v) {
  using_bigz <- is.bigz(m)
  
  check_n_i(n, i)
  check_m(m, max_value = as.bigz(2)^(n - 1), bigz_allowed = using_bigz)
  check_v(v)
  
  player_i <- i
  all_players <- 1:n
  sampling_size <- m
  game <- v
  
  if(using_bigz) banzhaf_value <- as.bigq(0) else banzhaf_value <- 0
  player_i_value <- all_players[player_i]
  players_without_i <- all_players[-player_i]
  
  if(using_bigz) j <- as.bigz(0) else j <- 0
  while (j < sampling_size) {
    j <- j + 1
    if(using_bigz) random_nr <- urand.bigz(1, length(players_without_i)) + 1 else random_nr <- sample.int(2^length(players_without_i), 1)
    sample <- coalitionFromIndex(players_without_i, random_nr)
    banzhaf_value <- banzhaf_value + (game(append(sample, player_i_value)) - game(sample))
  }

  banzhaf_value <- banzhaf_value / sampling_size
  return(banzhaf_value)
}
