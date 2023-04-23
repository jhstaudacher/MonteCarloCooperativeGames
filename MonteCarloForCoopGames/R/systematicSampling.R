getCoalition <- function(n, i_coalition){
  code <- fromICoalitionToCoalition(length(n), i_coalition)
  coaltion <- c()
  for (i in 1:length(n)){
    if (1 == code[i]){
      coaltion <- append(coaltion, n[i])
    }
  }
  return(coaltion)
}


#' Systematic sampling based on paper: "Statistics and game theory: Estimating coalitional values in R software" (A. Saavedra-Nieves, 2020) Algorithm 3
#'
#' @param all_players_N List of all players
#' @param player_i Index of the player
#' @param sampling_size_l Amount of samples with replacement
#' @param game_v Function of the TU-game
#'
#' @return Banzhaf value
#' @export
#'
#' @examples 
#' print(systematicSampling(1:10, 1, 200, gloveGame(1:5,6:10)))
systematicSampling <- function(all_players_N, player_i, sampling_size_l, game_v){
  stopifnot(1 < sampling_size_l)
  stopifnot(sampling_size_l <= 2^(length(all_players_N)-1))
  
  banzhaf_value <- 0
  player_i_value <- all_players_N[player_i]
  players_N_without_i <- all_players_N[-player_i]
  increment_K <- floor( (2^(length(all_players_N)-1)) / sampling_size_l )
  starting_point_k <- sample(1:increment_K, 1)
  indices_T <- c()
  
  for (j in seq(starting_point_k, 2^(length(all_players_N)-1), by=increment_K)){
    indices_T <- append(indices_T, j)
  }
  
  for (j in 1:sampling_size_l){
    sample_T_j <- getCoalition(players_N_without_i, indices_T[j])
    banzhaf_value <- banzhaf_value + game_v(append(sample_T_j, player_i_value)) - game_v(sample_T_j)
  }
  
  banzhaf_value <- banzhaf_value / sampling_size_l
  return(banzhaf_value)
}