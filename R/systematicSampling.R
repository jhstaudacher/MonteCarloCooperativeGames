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
#' @template details/BigQSupport
#' @template author/AR
#' @template author/RL
#' @template author/MM
#' @template param/i
#' @template param/n
#' @template param/mBigz
#' @template param/v
#' @template return/Bz_i
#' @template cites/SAAVEDRA_NIEVES_ET_AL_2020
#' @templateVar SAAVEDRA_NIEVES_ET_AL_2020_P pp. 132
#' @import gmp
#' @export
#' @examples
#' print(systematicSampling(1, 10, 200, gloveGameForSampling(1:5, 6:10)))
#' # --------------
#' \dontrun{
#' # very long runtime
#' m <- as.bigz(.Machine$integer.max) + 1
#' print(simpleRandomSamplingWithoutReplacement(1, 100, m, gloveGameForSampling(1:50, 51:100)))
#' }
systematicSampling <- function(i, n, m, v) {
  using_bigz <- is.bigz(m)

  check_n_i(n, i)
  check_m(m, max_value = as.bigz(2)^(n - 1), bigz_allowed = using_bigz)
  check_v(v)

  if (30 < n & !using_bigz) {
    stop("m must be bigz if n is more than 30")
  }

  player_i <- i
  all_players <- 1:n
  sampling_size <- m
  game <- v

  if (using_bigz) banzhaf_value <- as.bigq(0) else banzhaf_value <- 0
  player_i_value <- all_players[player_i]
  players_without_i <- all_players[-player_i]
  if (using_bigz) max_samples <- as.bigz(2)^(length(players_without_i)) else max_samples <- 2^(length(players_without_i))
  if (using_bigz) increment <- as.bigz(max_samples / sampling_size) else increment <- as.integer(max_samples / sampling_size)
  if (using_bigz) starting_point <- urand.bigz(1, as.integer(log2(increment))) else starting_point <- sample.int(as.integer(increment), 1)

  index <- starting_point
  while (index < max_samples) {
    index <- index + increment
    sample <- coalitionFromIndex(players_without_i, index)
    banzhaf_value <- banzhaf_value + (game(append(sample, player_i_value)) - game(sample))
  }

  banzhaf_value <- banzhaf_value / sampling_size
  return(banzhaf_value)
}
