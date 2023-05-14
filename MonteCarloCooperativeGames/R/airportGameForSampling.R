#' @name airportGameForSampling
#' @title Construct Airport Game
#' @description
#' Returns the characteristic function of an airport game with the specified cost vector
#' @template author/DU
#' @template author/AR
#' @template author/RL
#' @template author/MM
#' @param costs The vector where costs[i] gives the cost for player i. N is 1..length(costs).
#' cost[i] has to be smaller than or equal to cost[i+1].
#' @return The characteristic function configured with the provided cost function that returns the cost the provided set of players (S) has to pay.
#' @export
#' @examples
#' v <- airportGameForSampling(c(1, 1, 1, 3, 3, 5, 5, 5, 6, 6, 10, 10))
#' v(c(1, 3, 6)) # returns 5
airportGameForSampling <- function(costs) {
  v <- function(S) {
    if (length(S) == 0) 0 else max(costs[c(S)])
  }

  v
}
