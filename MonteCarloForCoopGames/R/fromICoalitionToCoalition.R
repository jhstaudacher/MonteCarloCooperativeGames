#' Access possible coalitions of players through index; based on paper: "Statistics and game theory: Estimating coalitional values in R software" (Alejandro Saavedra-Nieves, 2020)
#'
#' @param n indicates the cardinality of the set of players N
#' @param iCoalition indicates the index of the coalition. This value should be greater than or equal to 1 and less than or equal to 2^n
#'
#' @return numeric binary vector, of dimension n, which each component i indicates the belonging of player i to the coalition
#' @export
#'
#' @examples
#' print(fromICoalitionToCoalition(5, 27))
fromICoalitionToCoalition <- function(n, iCoalition) {
  Coalition <- array(0, dim = n)
  a1 <- iCoalition - 1
  ij <- 0

  while (ij < n) {
    ij <- ij + 1
    Coalition[ij] <- a1 - as.integer(a1 / 2) * 2
    a1 <- as.integer(a1 / 2)
  }

  return(Coalition)
}
