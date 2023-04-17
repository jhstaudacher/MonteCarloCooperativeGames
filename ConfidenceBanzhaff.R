# set.seed(0)

n <- 10
N <- 1:n

v <- function (S) {
  weights = c(1, 2, 3, 4, 5, 4, 3, 2, 1, 10)
  quota = 2/3 * sum(weights)
  S_value = sum(weights[S])
  result = as.integer(S_value > quota)
  return(result)
}

get_random_coalition <- function(i) {
  N_without_i = N[!N %in% i]
  C_size = sample(0:(n-1), 1)
  C_without_i <- sample(N_without_i, C_size)
  C = NULL
  if (length(C_without_i) == 0) {
    C = i
  } else {
    C <- append(C_without_i, i)
  }
  return(C)
}

is_critical <- function(C, i) {
  C_without_i = C[!C %in% i]
  critical = (v(C) - v(C_without_i) == 1)
  return (critical)
}

calc_conf_intervall <- function(b_i, k, delta) {
  e = sqrt(1 / (2*k) * log(2/delta))
  intervall = c(b_i - e, b_i + e)
  return(intervall)
}

ConfidenceBanzhaff <- function(i, conf, w) {
  X <- 0
  k <- 0
  e <- w / 2
  delta <- 1 - conf
  k_required <- log(2/delta) / (2 * e^2)
  
  while (k < k_required) {
    k <- k + 1
    C <- get_random_coalition(i)
    if (is_critical(C, i)) {
      X <- X + 1
    }
  }
  
  b_i <- X / k
  conf_intervall = calc_conf_intervall(b_i, k, delta)
  return(conf_intervall)
}

b_sum <- 0.0
for (i in N) {
  conf_intervall <- ConfidenceBanzhaff(i, 0.99, 0.01)
  b_i <- (conf_intervall[1] + conf_intervall[2]) / 2
  print(conf_intervall)
  b_sum <- b_sum + b_i
}
print(b_sum)
