#' @name subsetFromIndex
#' @title Get Subset From Index
#' @description
#' Gets the subset of players N (1:n) described by an index.
#' @param n Indicates the cardinality of the set of players N
#' @param index A one based index describing the subset
#' @return A subset from N described by index.
#' @export
#' @examples
#' S <- subsetFromIndex(5, 1) # empty set
#' S <- subsetFromIndex(5, 32) # all players
subsetFromIndex <- function(n, index) {
  if (index == 1) {
    return(c())
  }

  binary <- as.numeric(intToBits(index - 1))[1:n]
  subset <- which(binary == 1)

  subset
}
