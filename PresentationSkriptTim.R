library(CoopGame)
library(MonteCarloCooperativeGames)

n_max <- 16
weights <- c(sample(1:10, 1))
classic_t <- c()
monte_carlo_t <- list(c(), c(), c())
monte_carlo_e <- list(c(), c(), c())
confs <- c(0.98, 0.99, 0.995)
widths <- c(0.05, 0.01, 0.005)
colors <- c("blue", "green", "orange")

stopifnot(length(monte_carlo_t) == length(monte_carlo_e))
stopifnot(length(monte_carlo_e) == length(confs))
stopifnot(length(confs) == length(widths))
stopifnot(length(widths) == length(colors))

for (n in 2:n_max) {
  print(paste("n=", n, sep=""))

  # Configure Game
  weights <- append(weights, sample(1:10, 1))
  rel_quota <- runif(1, 1/3, 2/3)
  abs_quota <- rel_quota * sum(weights)
  n <- length(weights)

  # Classic
  v_vec <- weightedVotingGameVector(n, weights, abs_quota)
  start.time <- Sys.time()
  classic_result <- shapleyShubikIndex(v_vec)
  classic_t <- append(classic_t, Sys.time() - start.time)

  # Monte Carlo
  v <- MonteCarloCooperativeGames::weightedVotingGame(weights, rel_quota)
  for (a in 1:length(widths)) {
    result <- c()
    start.time <- Sys.time()
    for (i in 1:n) {
      conf_intervall <- confidenceShapleyShubik(i, n, v, confs[a], widths[a])
      result <- append(result, (conf_intervall[1] + conf_intervall[2]) / 2)
    }
    monte_carlo_t[[a]] <- append(monte_carlo_t[[a]], Sys.time() - start.time)
    monte_carlo_e[[a]] <- append(monte_carlo_e[[a]], calculateErrorMetrics(result, classic_result)$mean_error)
  }
}

# Plot
confs = sprintf(confs, fmt = '%#.3f')
widths = sprintf(widths, fmt = '%#.3f')
plot(2:n_max, classic_t, type="l", col="red", xlab="n", ylab="sec", main="Comparison of Time Complexity", ylim=c(0, 180))
labels <- c("Classic")
for (a in 1:length(monte_carlo_t)) {
  lines(2:n_max, monte_carlo_t[[a]], col=colors[a], lty=(a+1))
  error <- sprintf(mean(monte_carlo_e[[a]]), fmt = '%#.5f')
  labels <- append(labels, paste("Monte Carlo, c=", confs[a], ", w=", widths[a], ", me=", error, sep=""))
}
legend(2, 170, legend=labels, col=append(c("red"), colors), lty=1:(length(colors)+1), cex=0.8)
