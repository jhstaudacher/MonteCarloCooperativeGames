#' @title Get a coalition from a index
#' @description
#' Gets the coalition of players in list n described by the index.
#' @template author/AR
#' @param n List of players
#' @param index Index describing the coalition
#' @return A coalition from n described by index
coalitionFromIndex <- function(n, index) {
  code <- fromICoalitionToCoalition(length(n), index)
  coaltion <- c()
  for (i in 1:length(n)) {
    if (1 == code[i]) {
      coaltion <- append(coaltion, n[i])
    }
  }
  return(coaltion)
}

#' TODO add these to test
#' S <- coalitionFromIndex(1:5, 1) # empty set
#' S <- coalitionFromIndex(1:5, 32) # all players
