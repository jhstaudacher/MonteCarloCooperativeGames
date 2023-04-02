# set.seed(0)

n <- 10
N <- 1:n

v <- function (S) {
  left_in_S <- sum(S <= n/2)
  right_in_S <- length(S) - left_in_S
  
  return(min(c(left_in_S, right_in_S)))
}

# calculates sample of player i in position l with sample size m_il
get_sample <- function(i, l, m_il) {
  M_il = list()
  for (idx in 1:m_il) {
    S <- append(sample(N[N != i], size = l - 1), i)
    M_il[[idx]] <- S
  }
  return(M_il)
}

Sh <- rep(0, n)

# for every player i
for (i in 1:n) {
  # accumulated marginal contribution of player i over all positions l
  Sh_i = 0
  
  # for every position l in which player i occurs
  for (l in 1:n) {
    m_il = 100
    M_il = get_sample(i, l, m_il)
    
    # accumulated marginal contribution of player i in position l
    x_il = 0
    for (S in M_il) {
      x_il = x_il + (v(S) - v(S[1:length(S)-1]))
    }
    
    # add average marginal contribution of player i in position l
    Sh_i = Sh_i + x_il / m_il
  }
  
  # calculate average marginal contribution of player i
  Sh[i] = Sh_i / n
}

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

