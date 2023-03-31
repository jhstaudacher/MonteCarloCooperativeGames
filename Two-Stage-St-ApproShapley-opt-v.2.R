
# n <- 10
# N <- 1:n
# 
# v <- function (S) {
#   left_in_S <- sum(S <= n/2)
#   right_in_S <- length(S) - left_in_S
#   
#   return(min(c(left_in_S, right_in_S)))
# }

start_time <- Sys.time()

n <- 100
N <- 1:n

# cost_info describes how often each number is going to appear in the final cost array
# ((number, times), ...)
cost_info <- list(c(1, 8), c(2, 12), c(3, 6), c(4, 14), c(5, 8), c(6, 9), c(7, 13), c(8, 10), c(9, 10), c(10, 10))
costs <- c()
for (x in cost_info) {
  cost = x[1]
  times = x[2]
  
  costs <- append(costs, rep(cost, times))
}

# airport game
v <- function (S) {
  return(if(length(S) == 0) 0 else max(costs[c(S)]))
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

sampleP(5, 3, N)
sampleP(1, 1, N)

v(toI(c(6,4,3,6,2), 2))-v(toI(c(6, 4, 12,4), 1))

m <- 100000
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
      #totalSampleCount <- totalSampleCount + 1
    }
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
      #totalSampleCount <- totalSampleCount + 1
    }
  }
}

Shes <- Shes/(mstst + mExpIl)
#print(Shes)
Sh <- colSums(Shes)/n
#print(Sh)

end_time <- Sys.time()

print("Shapley-values (estimates):")
print(Sh)
print("------------------")

real_shapley <- read.csv(file = 'Airport_Game_Shapley_Values.csv')$Shapley

errors <- abs(real_shapley-Sh)
error_total <- sum(errors)
print("Errors:")
print(errors)
print("------------------")
print("Total error:")
print(error_total)
print("------------------")
print("Average error:")
print(error_total/n)
print("------------------")
print("Samples")
print(sum(mstst[mstst>0] + mExpIl))
#print(totalSampleCount)
print("------------------")
print("Total Time")
print(end_time - start_time)
