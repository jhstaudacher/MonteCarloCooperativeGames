#set.seed(0)

n <- 10
N <- 1:n

v <- function (S) {
  # 1:50 left; 51:100 right
  left_in_S <- sum(S <= n/2)
  right_in_S <- length(S) - left_in_S
  
  return(min(c(left_in_S, right_in_S)))
}

get_sample_from_population <- function(player, position) {
  order = sample(N[!N == player])
  return(append(order, player, after=position-1))
}

up_to <- function(S, index) {
  if (index == 0) return(c())
  else return(S[1:index])
}

m <- 10000
m_exp <- m/(2*n*n)

# Sh[player, position]
Sh <- matrix(rep(0, n*n), nrow=n, ncol=n)

# variances[player, position]
variances <- matrix(rep(0, n*n), nrow=n, ncol=n)

for (pos in 1:n) {
  for (player in 1:n) {
    sum_squared <- 0
    for (x in 1:m_exp) {
      order <- get_sample_from_population(player, pos)
      marg_contrib <- v(up_to(order, pos)) - v(up_to(order, pos-1))
      Sh[player, pos] <- Sh[player, pos] + marg_contrib
      sum_squared <- sum_squared + marg_contrib*marg_contrib
    }
    variances[player, pos] <- (sum_squared - (Sh[player, pos]/m_exp))/(m_exp-1)
  }
}

variances_sum <- sum(variances)

for (pos in 1:n) {
  for (player in 1:n) {
    target_sample_count = m*variances[player, pos]/variances_sum
    remaining_sample_count = target_sample_count - m_exp
    
    if (remaining_sample_count > 0) {
      for (x in 1:remaining_sample_count) {
        order <- get_sample_from_population(player, pos)
        marg_contrib <- v(up_to(order, pos)) - v(up_to(order, pos-1))
        Sh[player, pos] <- Sh[player, pos] + marg_contrib
      }
    }
    
    actual_sample_count <- m_exp + max(remaining_sample_count, 0)
    
    # this does not yet cover the case where remaining_sample_count is negative
    # the paper says to allocate the "not yet used sample size" among the remaining strata
    # but I do not understand that completely yet...
    # this should work nonetheless
    Sh[player, pos] <- Sh[player, pos]/actual_sample_count
  }
}

# sum over all positions for each player
# and divide by number of positions
Sh <- rowSums(Sh)/n
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
