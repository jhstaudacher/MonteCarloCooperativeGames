library(gmp)

#' @name systematicSampling
#' @title Systematic Sampling
#' @description Systematic sampling is a method to estimate the Banzhaf-Owen
#'   value for a specified TU game.
#' @details First, the algorithm systematically creates samples of players
#' without the player i by incrementally taking samples out of a sequence of all
#' possible combinations of players. Then the algorithm checks the value the
#' player adds to all the samples.
#'
#' Based on the paper: "Statistics and game theory: Estimating coalitional
#' values in R software" (A. Saavedra-Nieves, 2020) Algorithm 3
#' @template author/AR
#' @template author/RL
#' @template author/MM
#' @template param/i
#' @template param/n
#' @template param/m
#' @template param/v
#' @template return/Banzhaf
#' @template cites/SAAVEDRA_NIEVES_ET_AL_2020
#' @templateVar SAAVEDRA_NIEVES_ET_AL_2020_P pp. 132
#' @export
#' @examples
#' print(systematicSampling(1, 10, 200, gloveGameForSampling(1:5, 6:10)))
systematicSampling <- function(i, n, m, v) {
  check_n_i(n, i)
  check_m(m, max_value = as.bigz(2)^(n - 1), bigz_allowed = TRUE)
  check_v(v)
  
  player_i <- i
  all_players <- 1:n
  sampling_size <- as.bigz(m)
  game <- v

  banzhaf_value <- as.bigq(0)
  player_i_value <- all_players[player_i]
  players_without_i <- all_players[-player_i]
  max_samples <- as.bigz(2)^(length(all_players) - 1)
  increment <- floor(max_samples / sampling_size)
  
  if(increment < .Machine$integer.max)
    starting_point <- sample(1:as.integer(increment), 1)
  else
    starting_point <- urand.bigz(1, as.integer(log2(increment)))
  
  index<-starting_point
  while(index < max_samples){
    index<-index+increment
    sample <- coalitionFromIndex(players_without_i, index)
    banzhaf_value <- banzhaf_value + (game(append(sample, player_i_value)) - game(sample))
  }

  banzhaf_value <- banzhaf_value / sampling_size
  return(as.double(banzhaf_value))
}
