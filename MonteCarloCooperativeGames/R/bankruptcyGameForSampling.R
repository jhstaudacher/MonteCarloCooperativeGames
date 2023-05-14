#' @name bankruptcyGameForSampling
#' @title Bankruptcy Game 
#' @description Returns the characteristic function of a bankruptcy game with the
#'   specified players and estate.
#' @template author/AR
#' @template author/RL
#' @template author/MM
#' @param d The numeric vector which contains the claims of each player in a
#'   bankruptcy game
#' @param E The value of the estate in a bankruptcy game
#' @return The characteristic function configured with the provided list of
#'   players and estate value that returns remaining value for players (S)
#' @export
#' @examples
#' v <- bankruptcyGameForSampling(c(1,2,3),4)
#' v(c(2,3)) # returns 3

bankruptcyGameForSampling<-function( d, E) {
  v<-function(S){
    involvedPlayers <- S
    sumOtherClaims <- 0
    players <- 1:length(d)
    uninvolvedPlayers=players[-involvedPlayers]
    
    for(p in uninvolvedPlayers){ 
      sumOtherClaims = sumOtherClaims + d[p] 
    }
    
    if (sumOtherClaims >= E) {
      return(0)
    } else {
      return(E - sumOtherClaims)
    }
  }
  
  return(v)
}
