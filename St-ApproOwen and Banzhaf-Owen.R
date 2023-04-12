#rm(list = ls())  # clear environment

n <- 3

N <- 1:n

# glove game: left 1, 2; right 3
v <- function (S) {
  left_in_S <- sum(S <= 2)
  right_in_S <- length(S) - left_in_S
  
  return(min(c(left_in_S, right_in_S)))
}

# P <- list(c(1, 2), c(3,4,5), c(6)) 

#P <- list(c(1, 2, 3))       #expected result O = (1/6, 1/6, 2/3)    BzO = (0.25, 0.25, 0.75)
P <- list(c(1), c(2), c(3)) #expected result O = (1/6, 1/6, 2/3)   BzO = (0.25, 0.25, 0.75)
#P <- list(c(1, 2), c(3))    #expected result O = (1/4, 1/4, 1/2)  
#P <- list(c(1, 3), c(2))    #expected result O = (1/4, 0, 3,4)
#P <- list(c(1), c(2, 3))    #expected result O = (0  , 1/4, 3/4)


#extract the P(i) Partition
#extract R Partitions
i <- 1
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
# set total sample size
l <- 100000
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
      ekh <- ekh + (1/(l*W))*xi
      #cnt <- cnt +1 
    }
    # Owen calculation
    O <- O + ekh
    #print(W)
    # Banzhaf–Owen calculation
    BzO <- BzO + W*ekh
  }
}
O <- O/(length(P)*(length(Pi)+1))

#print("cnt:")
#print(cnt)
print('Owen:')
print(O)
print('Banzhaf–Owen:')
print(BzO)

