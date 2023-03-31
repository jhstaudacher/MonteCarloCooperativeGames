#A symmetric voting game
#For N={1,...,1000}
n <- 1000
N <- c(1:n)

symmetricVotingGame <- function(S){
  if(length(S) > n/2){
    return(1)
  }
  return(0)
}

shapleySymmetricVotingGame <- function(player){
  return(0.001)
}
  

symmetricVotingGame(c(1:500))
shapleySymmetricVotingGame(5)


#A non-symmetric voting game
#For N={1,...,51}
n <- 51
N <- c(1:n)
nonsymmetricVotingGame <- function(S){
  w <- c(45, 41, 27, 26, 26, 25, 21, 17, 17, 14, 13, 13, 12, 12, 12, 11, 10, 10, 10, 10, 9,9,9,9,8,8,7,7,7,7,6,6,6,6,5,4,4,4,4,4,4,4,4,4,3,3,3,3,3,3,3)
  firstsum <- 0
  for(i in S){
    firstsum <- firstsum + w[i]
  }
  secondsum <- 0
  for(j in N){
    secondsum <- secondsum + (w[j]/2)
  }
  if(firstsum > secondsum){
    return(1)
  }
  return(0)
}

nonshapleySymmetricVoting <- function(){
  return(read.csv(file = 'NonSymmetricVotingGame.csv')$Shapley)
}

nonshapleySymmetricVotingGamePerPlayer <- function(player){
  return(nonshapleySymmetricVoting()[player])
}

nonshapleySymmetricVotingGamePerPlayer(2)

