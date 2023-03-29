#set.seed(0)
#set.seed(NULL)

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

pre <- function(O, i) {
  if (i == 0) {
    return(c())
  }
  
  return(O[1:i])
}

m <- 150000
Sh <- rep(0, n)
cnt <- m/n

for (x in 1:cnt) {
  O <- sample(N)
  
  # i is not the player (like in the paper) but the idx of the player in the current order O
  for (i in 1:n) {
    sh_i <- v(pre(O, i)) - v(pre(O, i-1))
    player_i <- O[i]
    Sh[player_i] <- Sh[player_i] + sh_i
  }
}

Sh <- Sh/cnt

# the estimate Sh is efficient in allocation
print(sum(Sh))

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
