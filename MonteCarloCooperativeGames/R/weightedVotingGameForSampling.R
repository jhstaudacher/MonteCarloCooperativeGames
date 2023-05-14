#' @name weightedVotingGameForSampling
#' @title Weighted Voting Game
#' @description
#' Returns the characteristic function of a weighted voting game with the specified weights and quota
#' @template author/JM
#' @template author/MS
#' @template author/TP
#' @template author/DU
#' @template author/AR
#' @template author/RL
#' @template author/MM
#' @param weights The weights of the players
#' @param quota The required relative quota
#' @return 1 if sum of weighted votes >= quota; 0 otherwise
#' @export
#' @examples
#' v <- weightedVotingGameForSampling(c(1, 2, 3, 4, 5, 4, 3, 2, 1, 10), 1 / 2)
#' v(c(1:10))
weightedVotingGameForSampling <- function(weights, quota) {
  relative_quota <- quota * sum(weights)

  v <- function(S) {
    as.integer(sum(weights[S]) >= relative_quota)
  }

  v
}
