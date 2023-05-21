#' @name approOwen
#' @title Appro Owen
#' @description
#' Approximates the Owen value by using a simple Monte Carlo simulation.
#' @details
#' It is an adaptation of an analogous procedure
#' for the estimation of the Shapley value and it is particularly useful when dealing with games having large sets of players.
#' Based on: "Estimation of the Owen Value Based on Sampling" (Alejandro Saavedra-Nieve et al., 2018)
#' @template author/MS
#' @template param/n
#' @template param/m
#' @template param/v
#' @template param/P
#' @template return/Owen
#' @export
#' @references Alejandro Saavedra-Nieves, M. G.-J., Ignacio García-Jurado. (2018). Estimation of the Owen Value Basedon Sampling. In E. Gil, E. Gil, J. Gil, & M. Á. Gil (Eds.), The Mathematics of the Uncertain. doi:10.1007/978-3-319-73848-2
#' @examples
#' print(approOwen(3, 100, gloveGameForSampling(1:2, 3:3), list(c(1, 2), (3))))
approOwen <- function(n, m, v, P) {
  # paramCheckResult=getEmptyParamCheckResult()
  # initialParamChecksApproOwen(paramCheckResult, n, m, v, P)

  check_v(v)
  check_positive_number(m)
  check_P(P)
  if (length(unlist(P)) != n) {
    stop("Partitions do not fit to n")
  }


  Owen <- rep(0, n)
  # calculate shapley value only with permutations compatible with coalition structure P
  for (x in 1:(m / n)) {
    order <- sampleOrderP(P)
    # i is not the player (like in the paper) but the idx of the player in the current order O
    for (i in 1:n) {
      owen_i <- v(take(order, i)) - v(take(order, i - 1))
      player_i <- order[i]
      Owen[player_i] <- Owen[player_i] + owen_i
    }
  }
  Owen
  Owen <- Owen / (m / n)
  return(Owen)
}

# calculate random order which is compatible with the coalition structure P
sampleOrderP <- function(P) {
  # random permutation of union elements
  i <- 0
  while (i < length(P)) {
    i <- i + 1
    # prevent sample characteristic (e.g sample(c(3)) returns sample with 1,2,3)
    if (length(P[[i]]) <= 1) {
      next
    }
    P[[i]] <- sample(P[[i]])
  }
  # random permutation of unions
  li <- sample(P)
  un <- unlist(li, use.names = FALSE)
  # print(un)
  return(un)
}

# initialParamChecksApproOwen <- function(paramCheckResult, n, m, v, P) {
#   # ToDo common parameter checks
#   stopOnInvalidGameVector=function(paramCheckResult, v, n)
#
#   # ToDo custom parameter checks
#   checkResult=getEmptyParamCheckResult()
#   # Fitting P to n?
#   if(length(unlist(P)) != n){
#     checkResult$errMessage="Partitions don't fit to n"
#     checkResult$errCode=2010
#   }
#   eval.parent(substitute(paramCheckResult<-checkResult))
#   stopOnParamCheckError(paramCheckResult)
# }
