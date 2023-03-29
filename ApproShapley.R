set.seed(0)


n <- 10
N <- 1:n

v <- function (S) {
  # 1:50 left; 51:100 right
  left_in_S <- sum(S <= n/2)
  right_in_S <- length(S) - left_in_S
  
  return(min(c(left_in_S, right_in_S)))
}

pre <- function(O, i) {
  if (i == 0) {
    return(c())
  }
  
  return(O[1:i])
}

m <- 10000
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

Sh
Sh <- Sh/m

# the estimate Sh is efficient in allocation
sum(Sh)

print("Shapley-values (estimates):")
print(Sh)
print("------------------")

# we know that the shapley value for the glove game is 0.5 for every player
errors <- abs(rep(0.5, n)-Sh)
error_total <- sum(errors)
print("Errors:")
print(errors)
print("------------------")
print("Total error:")
print(error_total)
print("------------------")

