#' A two-stage procedure to estimate the Banzhaf–Owen value based on paper: "On stratified sampling for estimating coalitional values" (A. Saavedra-Nieves, 2022)
#'
#' @param i indicates player
#' @param l set total sample size
#' @param P A priori unions
#' @param v TU-game with a system of a priori unions
#'
#' @return approximation of the Banzaf-Owen value and Owen value based on stratified sampling
#' @export
#'
#' @examples
#' print (stApproOwenAndBanzhafOwen (1, 10000, list(c(1,2),(3)), gloveGame(1:2,3:3)))
#' print (stApproOwenAndBanzhafOwen (3, 10000, list(c(1), c(2, 3)), gloveGame(1:2,3:3))$Owen)
stApproOwenAndBanzhafOwen <- function(i, l, P, v){
  #extract the P(i) Partition
  #extract R Partitions
  # player i
  R <- list()   #contains all coalitions without Pi
  idx <- 1
  for(x in P){
    # if x contains i it is P(i)
    if(i %in% x){
      Pi <- x[x != i]
    } else {
      R[[idx]] <- x
      idx <- idx + 1
    }
  }
  #cnt <- 0
  # initialize Owen Value
  O <- 0
  # initialize Banzhaf Owen Value
  BzO <- 0
  # for every k to m-1
  for(k in 0:(length(P)-1)){
    # for every h to pi - 1 #not -1 because Pi doesn't contain i in this implementation
    for(h in  0:(length(Pi))){
      #print('----------')
      #print(cat('K: ', k, ', H: ', h))
      # strata weight calculation, needed for proportional distribution of samples lkh and Banzhaf–Owen
      W <- (choose(length(P)-1, k)*choose(length(Pi), h))/((2^(length(P)-1))*(2^(length(Pi))))
      #print(W)
      ekh <- 0
      # proportional allocation procedure for sample site lkh
      for(sampleidx in 1:(l*W)){
        if(length(R) <= 1){             #prevent sample behavior for x
          first <- rep(R, k)
        } else {
          first <- sample(x=R, size=k, replace=FALSE)
        }
        if(length(Pi) <= 1){             #prevent sample behavior for pi
          sec <- rep(Pi, h)
        } else {
          sec <- sample(x=Pi, size=h, replace=FALSE)
        }
        sample <- unlist(append(first, sec))
        #print(append(sample, i))
        xi <- v(append(sample, i)) - v(sample)
        #print(xi)
        ekh <- ekh + (1/(l*W))*xi
        #cnt <- cnt +1
      }
      # Owen calculation
      O <- O + ekh
      #print(O)
      #print(W)
      # Banzhaf–Owen calculation
      BzO <- BzO + W*ekh
    }
  }
  O <- O/(length(P)*(length(Pi)+1))
  return(list("Owen"=O, "Banzhaf-Owen" = BzO))
}

