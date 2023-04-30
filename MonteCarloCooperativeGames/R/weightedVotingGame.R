#' @name weightedVotingGame
#' @title Weighted Voting Game
#' @description
#' Returns the characteristic function of a weighted voting game with the specified weights and quota
#' @param weights The weights of the players
#' @param quota The required relative quota
#' @return 1 if sum of weighted votes >= quota; 0 otherwise
#' @export
#' @examples
#' v <- weightedVotingGame(c(1, 2, 3, 4, 5, 4, 3, 2, 1, 10), 1 / 2)
#' v(c(1:10))
weightedVotingGame <- function(weights, quota) {
  relative_quota <- quota * sum(weights)

  v <- function(S) {
    as.integer(sum(weights[S]) >= relative_quota)
  }

  v
}
