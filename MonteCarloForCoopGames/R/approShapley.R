source("./R/take.R")

# n: number of player
# m: number of samples
# v: characteristic function
ApproShapley <- function (n, m, v) {

  N <- 1:n

  Sh <- rep(0, n)

  for (x in 1:(m/n)) {
    O <- sample(N)

    # i is not the player (like in the paper) but the idx of the player in the current order O
    for (i in 1:n) {
      sh_i <- v(take(O, i)) - v(take(O, i-1))
      player_i <- O[i]
      Sh[player_i] <- Sh[player_i] + sh_i
    }
  }

  Sh <- Sh/(m/n)

  Sh
}

# Example
print (ApproShapley (10,10000, gloveGame(1:5,6:10)))
