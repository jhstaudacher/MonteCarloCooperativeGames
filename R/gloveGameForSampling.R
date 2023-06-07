#' @name gloveGameForSampling
#' @title Glove Game
#' @description
#' Returns the characteristic function of a glove game with the specified sets of left and right gloves.
#' @template author/JM
#' @template author/DU
#' @template author/MS
#' @template author/TP
#' @template author/EW
#' @template author/AR
#' @template author/RL
#' @template author/MM
#' @param L Set of players with a left glove
#' @param R Set of players with a right glove
#' @return The characteristic function configured with the provided left and right glove lists that returns the number of matching  glove pairs according to the provided set of players (S)
#' @export
#' @examples
#' v <- gloveGameForSampling(L = 1:50, R = 51:100)
#' v(c(10, 90, 99, 70, 20))
gloveGameForSampling <- function(L, R) {
  # TODO: check if L and R are disjoint
  v <- function(S) {
    left_glove_count <- length(intersect(S, L))
    right_glove_count <- length(intersect(S, R))

    min(left_glove_count, right_glove_count)
  }

  v
}
