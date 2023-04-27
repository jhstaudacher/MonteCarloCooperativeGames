#' @name twoStageStApproShapleyOptCor
#' @title Calculates the shapley value for each player with stratification and
#' sample size per strata based on each stratums variance samples size adjusted
#' @description twoStageStApproShapleyOptCor is a sampling procedure to estimate the Shapley value for cooperative games.
#' Based on: "Improving polynomial estimation of the Shapley value by stratified
#' random sampling with optimum allocation" (J. Castro Et al., 2017)
#' @param n The number of players
#' @param v The characteristic function
#' @param min_sample_size The amount of samples that should be taken.
#' Based on the variances of each stratum it is likely to happen, that more
#' samples are used than specified in min_sample_size. These samples are propotionally
#' adjusted-
#'
#' @return The shapley value of each player
#' @export
#'
#' @examples
#' # sample a airport game with 100 players
#' costs <- buildAirportCostVector(list(
#'   c(1, 8), c(2, 12), c(3, 6), c(4, 14), c(5, 8), c(6, 9),
#'   c(7, 13), c(8, 10), c(9, 10), c(10, 10)
#' ))
#' v <- airportGame(costs)
#' Sh <- twoStageStApproShapleyOptCor(length(costs), v, 100000)
#' print(Sh)
twoStageStApproShapleyOptCor <- function(n, v, min_sample_size) {
  N <- 1:n
  m <- min_sample_size
  totalSampleCount <- 0
  L <- 1:n
  Shes <- matrix(0, nrow = n, ncol = n)
  mst <- matrix(0, nrow = n, ncol = n)
  totalvar <- 0
  mExpIl <- m/(2*(n^2))
  for(l in L){
    for(i in N){
      sum_cuad_l <- 0
      for(cont_l in 1:mExpIl){
        order <- c(sampleP(i, l, N))
        xOi <- v(toI(order, l)) - v(toI(order, l-1))
        Shes[l,i] <-Shes[l,i] + xOi
        sum_cuad_l <- sum_cuad_l + xOi^2
        totalSampleCount <- totalSampleCount + 1
      }
      #variances
      sil <- (1/(mExpIl - 1))*(sum_cuad_l - (Shes[l,i]^2)/mExpIl)
      totalvar <- totalvar + sil
      mst[l,i] <- m * sil
    }
  }
  mstst <- mst/totalvar - mExpIl
  mststSumPostitves <- sum(mstst[mstst>=0])
  mststSumNegatives <- sum(mstst[mstst<0])
  correctionRatio <- (m/2)/mststSumPostitves
  mstst <- mstst * correctionRatio
  mstst[mstst<0] = 0
  mstst <- floor(mstst)
  for(l in L){
    for(i in N){
      if(mstst[l,i] <= 1) next
      for(cont_l in 1:mstst[l,i]){
        order <- c(sampleP(i, l, N))
        xOi <- v(toI(order, l)) - v(toI(order, l-1))
        Shes[l,i] <-Shes[l,i] + xOi
        totalSampleCount <- totalSampleCount + 1
      }
    }
  }

  Shes <- Shes/(mstst + mExpIl)
  Sh <- colSums(Shes)/n
}

toI <- function (o, idx){
  if (idx==0){
    return(c())
  }
  return(o[1:idx])
}

sampleP <- function(i, l, N){
  sawithout <- sample(N[-i], l-1)
  return(append(sawithout, i, after = l-1))
}



