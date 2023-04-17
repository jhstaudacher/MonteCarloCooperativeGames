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

pre <- function(O, i) {
  i_idx <- match(i, O)
  
  if (i_idx == 1) {
    return (c())
  }
  
  pre_i = O[1:(i_idx-1)]
  return (pre_i)
}

is_critical <- function(O, i) {
  pre_i <- pre(O, i)
  including_i <- append(pre_i, i)
  critical = (v(including_i) - v(pre_i) == 1)
  return (critical)
}

calc_conf_intervall <- function(sh_i, k, delta) {
  e = sqrt(1 / (2*k) * log(2/delta))
  intervall = c(sh_i - e, sh_i + e)
  return(intervall)
}

ConfidenceShapleyShubik <- function(i, conf, w) {
  X <- 0
  k <- 0
  e <- w / 2
  delta <- 1 - conf
  k_required <- log(2/delta) / (2 * e^2)
  
  while (k < k_required) {
    k <- k + 1
    O <- sample(N)
    if (is_critical(O, i)) {
      X <- X + 1
    }
  }
  
  sh_i <- X / k
  sum <- sum + b_i
  conf_intervall = calc_conf_intervall(sh_i, k, delta)
  return(conf_intervall)
}

sh_sum <- 0.0
for (i in N) {
  conf_intervall <- ConfidenceShapleyShubik(i, 0.99, 0.01)
  sh_i <- (conf_intervall[1] + conf_intervall[2]) / 2
  print(conf_intervall)
  sh_sum <- sh_sum + sh_i
}
print(sh_sum)
