costs <- buildAirportCostVector(list(
   c(1, 8), c(2, 12), c(3, 6), c(4, 14), c(5, 8), c(6, 9),
   c(7, 13), c(8, 10), c(9, 10), c(10, 10)
))
v <- airportGame(costs)

stApproshapleyAll <- function(n, v, m) {
  m_per_player = m/n
  Sh <- c()

  for (i in 1:n) {
    Sh <- append(Sh, stApproShapley(i, n, v, m_per_player))
  }

  Sh
}

Sh_real <- read.csv('../Airport_Game_Shapley_Values.csv')$Shapley

sample_sizes <- c(10000, 100000, 1000000, 10000000)
errors_two_stage <- list()
errors_strat <- list()
errors_simple <- list()

for (m in sample_sizes) {
  print(paste("Sample size m = ", m))

  print("Sampling with approShapley")
  Sh_simple <- approShapley(length(costs), m, v)
  mse_simple <- calculateErrorMetrics(Sh_simple, Sh_real)$mean_squared_error
  errors_simple <- append(errors_simple, mse_simple)
  print(paste("Error is ", mse_simple))

  print("Sampling with stApproShapleyAll")
  Sh_strat <- stApproshapleyAll(length(costs), v, m)
  mse_strat <- calculateErrorMetrics(Sh_strat, Sh_real)$mean_squared_error
  errors_strat <- append(errors_strat, mse_strat)
  print(paste("Error is ", mse_strat))

  print("Sampling with twoStageStApproShapleyOptCor")
  Sh_two_stage <- twoStageStApproShapleyOptCor(length(costs), v, m)
  mse_two_stage <- calculateErrorMetrics(Sh_two_stage, Sh_real)$mean_squared_error
  errors_two_stage <- append(errors_two_stage, mse_two_stage)
  print(paste("Error is ", mse_two_stage))
}

print(errors_simple)
print(errors_strat)
print(errors_two_stage)

errors_simple <- c(0.006585417, 0.0004753569, 6.609877e-05, 5.910757e-06)
errors_strat <- c(0.000647483, 9.845161e-05, 1.103517e-05, 7.248092e-07)
errors_two_stage <- c(0.0005018368, 4.240639e-05, 1.206376e-06, 1.065394e-07)

plot(log10(sample_sizes), errors_simple, col='red', type='l', ylab='errors')
lines(log10(sample_sizes), errors_strat, col='green')
lines(log10(sample_sizes), errors_two_stage, col='blue')

ylim_low <- min(log(errors_simple), log(errors_strat), log(errors_two_stage))
ylim_high <- max(log(errors_simple), log(errors_strat), log(errors_two_stage))

plot(log10(sample_sizes), log(errors_simple), col='red', type='l', ylab='log(errors)', ylim=c(ylim_low, ylim_high))
lines(log10(sample_sizes), log(errors_strat), col='green')
lines(log10(sample_sizes), log(errors_two_stage), col='blue')

