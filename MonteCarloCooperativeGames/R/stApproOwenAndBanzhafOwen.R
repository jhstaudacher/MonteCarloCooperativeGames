#' @name stApproOwenAndBanzhafOwen
#' @title A stratified sampling procedure to estimate coalitional values
#' @description stApproOwenAndBanzhafOwen is a sampling methodology to estimate coalitional values, in particular the
#' Owen value and the Banzhaf–Owen value for a specified TU game with a system of a priori unions, based on stratified sampling.
#' Offering two allocation procedures for the samples.
#' @details
#' The algorithm loops through a strata of k unions and h players.
#' Firstly, the algorithm takes with replacement a sample of coalitions of size k composed by unions other than the
#' one to which player i belongs. Secondly, for each element of the first sample, the algorithm takes with replacement a coalition
#' of players in the union of player i other than player i itself. This sample is used to calculate the Owen and Banzhaf-Owen values.
#' If propotional is set, the samples are allocated proportionally to the weight of the strata. Otherwise they are evenly distributed.
#' Based on: "On stratified sampling for estimating coalitional values" (A. Saavedra-Nieves, 2022)
#' @template author/MS
#' @template param/i
#' @template param/m
#' @template param/v
#' @template param/P
#' @param proportional boolean with true = propotional allocation and false = simple allocation procedure of samples
#' @template cites/SAAVEDRA_NIEVES_ET_AL_2022
#' @templateVar SAAVEDRA_NIEVES_ET_AL_2022_P pp. 5
#' @return approximation of the Banzaf-Owen value and Owen value based on stratified sampling
#' @export
#' @examples
#' print(stApproOwenAndBanzhafOwen(1, 1000, gloveGameForSampling(1:2, 3:3), list(c(1, 2), c(3))))
#' print(stApproOwenAndBanzhafOwen(1, 1000, gloveGameForSampling(1:2, 3:3), list(c(1, 2), c(3)), FALSE)$Owen)
#'
stApproOwenAndBanzhafOwen <- function(i, m, v, P, proportional = TRUE) {

  check_P_i(P, i)
  check_v(v)
  check_positive_number(m)

  # extract the P(i) Partition
  # extract R Partitions
  # player i
  R <- list() # contains all coalitions without Pi
  idx <- 1
  for (x in P) {
    # if x contains i it is P(i)
    if (i %in% x) {
      Pi <- x[x != i]
    } else {
      R[[idx]] <- x
      idx <- idx + 1
    }
  }
  # cnt <- 0
  # initialize Owen Value
  O <- 0
  # initialize Banzhaf Owen Value
  BzO <- 0
  # for every k to m-1
  for (k in 0:(length(P) - 1)) {
    # for every h to pi - 1 #not -1 because Pi doesn't contain i in this implementation
    for (h in 0:(length(Pi))) {
      # print('----------')
      # print(cat('K: ', k, ', H: ', h))
      # strata weight calculation, needed for proportional distribution of samples lkh and Banzhaf–Owen
      W <- (choose(length(P) - 1, k) * choose(length(Pi), h)) / ((2^(length(P) - 1)) * (2^(length(Pi))))
      # print(W)
      ekh <- 0
      # calculates lkh evaluating variable proportional
      lkh <- ceiling(m / (length(P) * (length(Pi) + 1)))
      if (proportional) lkh <- ceiling(m * W)
      for (sampleidx in 1:(lkh)) {
        if (length(R) <= 1) { # prevent sample behavior for x
          first <- rep(R, k)
        } else {
          first <- sample(x = R, size = k, replace = FALSE)
        }
        if (length(Pi) <= 1) { # prevent sample behavior for pi
          sec <- rep(Pi, h)
        } else {
          sec <- sample(x = Pi, size = h, replace = FALSE)
        }
        sample <- unlist(append(first, sec))
        # print(append(sample, i))
        xi <- v(append(sample, i)) - v(sample)
        # print(xi)
        ekh <- ekh + (1 / lkh) * xi
        # cnt <- cnt +1
      }
      # Owen calculation
      O <- O + ekh
      # print(O)
      # print(W)
      # Banzhaf–Owen calculation
      BzO <- BzO + W * ekh
    }
  }
  O <- O / (length(P) * (length(Pi) + 1))
  return(list("Owen" = O, "Banzhaf-Owen" = BzO))
}
