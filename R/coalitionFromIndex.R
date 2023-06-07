#' @title Get a coalition from an index
#' @description
#' Gets the coalition/subset of superset described by index.
#' @template author/AR
#' @template author/MS
#' @template author/DU
#' @param superset The superset from which to get a specific coalition described
#' by the parameter index
#' @param index Index describing the coalition/subset. Valid values are in the
#' range from 1 to including 2^len(superset).
#' @return A coalition/subset from superset described by index
coalitionFromIndex <- function(superset, index) {
  if (index == 1) {
    return(c())
  }
  bits <- fromICoalitionToCoalition(length(superset), index)
  indices <- which(bits == 1)
  superset[indices]
}
