#' @name simpleRandomSamplingWithoutReplacement
#' @title Simple random sampling without replacement
#' @description Simple random sampling without replacement is a method to
#' approximate the Banzhaf value for a specified TU game.
#' @details First, the algorithm takes a sample without replacement of all
#' players without the player i. Then the algorithm checks the value the player
#' adds to the sample. These two steps are repeated for the number of samples.
#'
#' Based on the paper: "Statistics and game theory: Estimating coalitional
#' values in R software" (A. Saavedra-Nieves, 2020) Algorithm 2
#' @template author/AR
#' @template author/RL
#' @template author/MM
#' @template param/i
#' @template param/n
#' @template param/mBigz
#' @template param/v
#' @template return/Banzhaf
#' @template cites/SAAVEDRA_NIEVES_ET_AL_2020
#' @templateVar SAAVEDRA_NIEVES_ET_AL_2020_P pp. 131
#' @export
#' @examples
#' print(simpleRandomSamplingWithoutReplacement(1, 10, 200, gloveGameForSampling(1:5, 6:10)))
#' # --------------
#' \dontrun{
#'   # very long runtime
#'   m <- as.bigz(.Machine$integer.max) + 1
#'   print(simpleRandomSamplingWithoutReplacement(1, 100, m, gloveGameForSampling(1:50, 51:100)))
#' }
simpleRandomSamplingWithoutReplacement <- function(i, n, m, v) {
  check_n_i(n, i)
  check_m(m, max_value = 2^(n - 1))
  check_v(v)

  player_i <- i
  all_players <- 1:n
  sampling_size <- m
  game <- v

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
