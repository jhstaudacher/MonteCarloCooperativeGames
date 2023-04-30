#' @name fromICoalitionToCoalition
#' @title from ICoalition To Coalition
#' @description
#' Access possible coalitions of players through index
#' @details
#'  Based on: "Statistics and game theory: Estimating coalitional values in R software" (Alejandro Saavedra-Nieves, 2020)
#' @param cardinality_n indicates the cardinality of the set of players N
#' @param iCoalition indicates the index of the coalition. This value should be greater than or equal to 1 and less than or equal to 2^n
#' @return numeric binary vector, of dimension n, which each component i indicates the belonging of player i to the coalition
#' @examples
#' # example code
#' # TODO: create example
fromICoalitionToCoalition <- function(cardinality_n, iCoalition) {
  Coalition <- array(0, dim = cardinality_n)
  a1 <- iCoalition - 1
  ij <- 0

  while (ij < cardinality_n) {
    ij <- ij + 1
    Coalition[ij] <- a1 - as.integer(a1 / 2) * 2
    a1 <- as.integer(a1 / 2)
  }

  return(Coalition)
}
