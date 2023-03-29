#set.seed(0)
set.seed(NULL)

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

get_sample_from_population <- function(player, position) {
  order = sample(N[!N == player])
  return(append(order, player, after=position-1))
}

up_to <- function(S, index) {
  if (index == 0) return(c())
  else return(S[1:index])
}

m <- 100000
actual_total_sample_count <- 0
m_exp <- m/(2*n*n)

if (m_exp < 1) {
  warning("Please choose m sufficiently large so m/(2*n*n) is at least 1")
}

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
      actual_total_sample_count <- actual_total_sample_count + 1
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
        actual_total_sample_count <- actual_total_sample_count + 1
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
print(paste("v(N) = ", sum(Sh), " [estimate]"))
print("------------------")
print(paste("m = ", m, " (actual m = ", actual_total_sample_count, ")"))
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