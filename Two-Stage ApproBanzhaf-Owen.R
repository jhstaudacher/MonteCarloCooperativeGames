# rm(list = ls())

from_iCoalition_to_Coalition<-function(n,iCoalition){
  Coalition <- array(0, dim = n)
  a1 <- iCoalition-1
  ij <- 0
  while (ij < n) {
    ij <- ij + 1
    Coalition[ij] <- a1 - as.integer(a1/2) * 2
    a1 <- as.integer(a1/2)
  }
  return(Coalition)
}

createRandomSamples <- function(input, samplesize){
  iCoalition <- sample(2^(length(input)), samplesize, replace = FALSE)
  R <- list()
  for(idx in 1:length(iCoalition)){
    if(length(input) == 0){
      bincoalations <- 0
    } else{
      bincoalations <- from_iCoalition_to_Coalition(length(input), iCoalition[idx])
    }
    #print(bincoalations)
    R[[idx]] <- list()
    for(ele in 1:length(bincoalations)){
      if(bincoalations[ele] == 1){
        R[[idx]] <- unlist(append(R[[idx]], input[ele]))
      }
    }
  }
  return(R)
}

# glove game: left 1, 2; right 3
v <- function (S) {
  left_in_S <- sum(S <= 2)
  right_in_S <- length(S) - left_in_S
  
  return(min(c(left_in_S, right_in_S)))
}

#samples
P <- list(c(1, 2), c(3,4,5), c(6)) 

#P <- list(c(1, 2, 3))       #expected result O = (1/6, 1/6, 2/3)    BzO = (0.25, 0.25, 0.75)
#P <- list(c(1), c(2), c(3)) #expected result O = (1/6, 1/6, 2/3)   BzO = (0.25, 0.25, 0.75)
#P <- list(c(1, 2), c(3))    #expected result O = (1/4, 1/4, 1/2)  
#P <- list(c(1, 3), c(2))    #expected result O = (1/4, 0, 3,4)
#P <- list(c(1), c(2, 3))    #expected result O = (0  , 1/4, 3/4)

# set player
i <- 1
# sample has to be less than population
lr <- 4
ls <- 2

withoutPi <- list()   #contains all coalitions without Pi
idx <- 1
for(x in P){
  # if x contains i it is P(i)
  if(i %in% x){
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
for(j in 1:lr){
  # Take SR j = {S j1 to S js } where S jk ⊆ P(i)\{i} is obtained without replacement for all k = 1 to s .
  S <- createRandomSamples(Pi, ls)
  for(k in 1:ls){
    xRS <- v(unlist(c(R[j], S[k], i)))-v(unlist(c(R[j], S[k])))
    #print(unlist(c(R[j], S[k], i)))
    #print(xRS)
    BzO <- BzO + xRS
  }
}
BzO <- BzO/(lr*ls)
print('Banzhaf-Owen: ')
print(BzO)


