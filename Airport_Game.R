set.seed(0)

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

m <- 1000
Sh <- rep(0, n)

for (x in 1:m) {
  O <- sample(N)
  
  # i is not the player (like in the paper) but the idx of the player in the current order O
  for (i in 1:n) {
    sh_i <- v(pre(O, i)) - v(pre(O, i-1))
    player_i <- O[i]
    Sh[player_i] <- Sh[player_i] + sh_i
  }
}

print(Sh)
print(Sh/m)
# the estimate Sh is efficient in allocation
print(sum(Sh/m))
