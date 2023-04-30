#' @name twoStageApproBanzhafOwen
#' @title A two-stage procedure to estimate the Banzhaf–Owen value
#' @description
#' twoStageApproBanzhafOwen is a sampling method to approximate the Banzhaf-Owen value for
#' a specified TU game with a system of a priori unions, based on two-stage sampling.
#' Based on: "Sampling methods to estimate the Banzhaf–Owen value" (A. Saavedra-Nieves & M. G. Fiestras-Janeiro, 2020)
#' @param i indicates player
#' @param lr number of samples of coalitions of unions
#' @param ls number of samples of coalitions in Pi for each element of sample
#' @param P A priori unions
#' @param v TU-game with a system of a priori unions
#' @return approximation of the Banzaf-Owen value based on two-stage sampling
#' @template cites/SAAVEDRA_NIEVES_ET_AL_2020
#' @templateVar SAAVEDRA_NIEVES_ET_AL_2020_P TODO
#' @export
#' @examples
#' print(twoStageApproBanzhafOwen(1, 2, 2, list(c(1, 2), (3)), gloveGame(1:2, 3:3)))
#'
twoStageApproBanzhafOwen <- function(i, lr, ls, P, v) {
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
  # Take R = {R1 to Rr } where each R j ⊆ P\P(i) is obtained without replacement for all j = 1 to r .
  # prepare the list R that contains lists of different permutations of P\Pi
  R <- createRandomSamples(withoutPi, lr)
  BzO <- 0
  for (j in 1:lr) {
    # Take SR j = {S j1 to S js } where S jk ⊆ P(i)\{i} is obtained without replacement for all k = 1 to s .
    S <- createRandomSamples(Pi, ls)
    for (k in 1:ls) {
      xRS <- v(unlist(c(R[j], S[k], i))) - v(unlist(c(R[j], S[k])))
      # print(unlist(c(R[j], S[k], i)))
      # print(xRS)
      BzO <- BzO + xRS
    }
  }
  BzO <- BzO / (lr * ls)
  return(BzO)
}

createRandomSamples <- function(input, samplesize) {
  iCoalition <- sample(2^(length(input)), samplesize, replace = FALSE)
  R <- list()
  for (idx in 1:length(iCoalition)) {
    if (length(input) == 0) {
      bincoalations <- 0
    } else {
      bincoalations <- fromICoalitionToCoalition(length(input), iCoalition[idx])
    }
    # print(bincoalations)
    R[[idx]] <- list()
    for (ele in 1:length(bincoalations)) {
      if (bincoalations[ele] == 1) {
        R[[idx]] <- unlist(append(R[[idx]], input[ele]))
      }
    }
  }
  return(R)
}
