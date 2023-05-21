#' @name twoStageApproBanzhafOwen
#' @title A two-stage procedure to estimate the Banzhaf–Owen value
#' @description
#' twoStageApproBanzhafOwen is a sampling method to approximate the Banzhaf-Owen value for
#' a specified TU game with a system of a priori unions, based on two-stage sampling.
#' @details
#' First, the algorithm takes a sample of coalitions of unions
#' other than the one to which player i belongs to, without replacement. Then, for each element, the algorithm takes a sample of
#' coalitions without replacement of the union to which player i belongs.
#' Based on: "Sampling methods to estimate the Banzhaf–Owen value" (A. Saavedra-Nieves & M. G. Fiestras-Janeiro, 2020)
#' @template author/MS
#' @template param/i
#' @template param/v
#' @param lr number of samples of coalitions of unions
#' @param ls number of samples of coalitions in Pi for each element of sample
#' @template param/P
#' @return approximation of the Banzaf-Owen value based on two-stage sampling
#' @template cites/SAAVEDRA_NIEVES_ET_AL_2020
#' @templateVar SAAVEDRA_NIEVES_ET_AL_2020_P pp. 208-221
#' @export
#' @examples
#' print(twoStageApproBanzhafOwen(1, 2, 2, gloveGameForSampling(1:2, 3:3), list(c(1, 2), (3))))
twoStageApproBanzhafOwen <- function(i, lr, ls, v, P) {
  check_P_i(P, i)
  check_v(v)
  check_positive_number(lr)
  check_positive_number(ls)


  withoutPi <- list() # contains all coalitions without Pi
  idx <- 1
  for (x in P) {
    # if x contains i it is P(i)
    if (i %in% x) {
      Pi <- x[x != i]
    } else {
      withoutPi[[idx]] <- x
      idx <- idx + 1
    }
  }
  # prepare the list R that contains lists of different permutations of P\Pi

  getPermutationIdxR <- sample(2^(length(withoutPi)), lr, replace = FALSE)
  BzO <- 0
  for (j in 1:lr) {
    getPermutationIdxS <- sample(2^(length(Pi)), ls, replace = FALSE)
    for (k in 1:ls) {
      R <- unlist(coalitionFromIndex(withoutPi, getPermutationIdxR[j]))
      S <- coalitionFromIndex(Pi, getPermutationIdxS[k])
      xRS <- v(unlist(c(R, S, i))) - v(unlist(c(R, S)))
      BzO <- BzO + xRS
    }
  }
  BzO <- BzO / (lr * ls)
  return(BzO)
}
