n <- 3

N <- 1:n

# glove game: left 1, 2; right 3
v <- function (S) {
  left_in_S <- sum(S <= 2)
  right_in_S <- length(S) - left_in_S
  
  return(min(c(left_in_S, right_in_S)))
}

# P <- list(c(1), c(2), c(3)) #expected result y = (1/6, 1/6, 2/3)
# P <- list(c(1, 2, 3))       #expected result y = (1/6, 1/6, 2/3)
# P <- list(c(1, 2), c(3))    #expected result y = (1/4, 1/4, 1/2)
P <- list(c(1, 3), c(2))    #expected result y = (1/4, 0, 3,4)
# P <- list(c(1), c(2, 3))    #expected result y = (0  , 1/4, 3/4)


# calculate random order which is compatible with the coalition structure P
sampleOrderP <- function(P) {
  # random permutation of union elements
  i <- 0
  while (i < length(P)) {
    i <- i+1;
    #prevent sample characteristic (e.g sample(c(3)) returns sample with 1,2,3)
    if(length(P[[i]]) <= 1) {
      next
    }    
    P[[i]] <- sample(P[[i]])
  }
  # random permutation of unions
  li <- sample(P)
  un <- unlist(li, use.names=FALSE)
  #print(un)
  return (un)
}

# return set for order o from index 1 to i
pre <- function(O, i) {
  if (i == 0) {
    return(c())
  }
  
  return(O[1:i])
}

# sample size m
m <- 100000
Owen <- rep(0, n)

# calculate shapley value only with permutations compatible with coalition structure P
for (x in 1:m) {
  order <- sampleOrderP(P)
  # i is not the player (like in the paper) but the idx of the player in the current order O
  for (i in 1:n) {
    owen_i <- v(pre(order, i)) - v(pre(order, i-1))
    player_i <- order[i]
    Owen[player_i] <- Owen[player_i] + owen_i
  }
}

Owen
Owen <- Owen/m

print("Owen-values (estimates):")

print(Owen)

# the estimate Owen is efficient in allocation
print("----------------------")
print("Sum")
print(sum(Owen))
