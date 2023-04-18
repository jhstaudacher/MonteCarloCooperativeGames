#' Returns the characteristic function of a weighted voting game with the specified weights and quota
#'
#' @param weights The weights of the players
#' @param quota The required quota
#'
#' @return A sub-list
#' @export
#'
#' @examples
#' v <- weightedVotingGame(c(1, 2, 3, 4, 5, 4, 3, 2, 1, 10), 1/2)
#' v(c(1:10))
weightedVotingGame <- function(weights, quota){

  v <- function (S) {
    quota_sum <- quota * sum(weights)
    S_value <- sum(weights[S])
    result <- as.integer(S_value > quota_sum)
    result
  }

  v
}
