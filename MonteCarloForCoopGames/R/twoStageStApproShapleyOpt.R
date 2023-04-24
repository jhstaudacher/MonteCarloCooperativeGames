#' Calculates the shapley value for each player with stratification and
#' sample size per strata based on each stratums variance
#'
#' @param n The number of players
#' @param v The characteristic function
#' @param min_sample_size The amount of samples that should be taken.
#' Based on the variances of each stratum it is likely to happen, that more
#' samples are used than specified in min_sample_size. The actual sample size
#' depends on the problem.
#' @param first_stage_size The amount of samples to use for the first stage as
#' a ratio. Usually 0.5 (half of all samples).
#'
#' @return The shapley value of each player
#' @export
#'
#' @examples
#' # sample a airport game with 100 players
#' costs <- buildAirportCostVector(list(
#'   c(1, 8), c(2, 12), c(3, 6), c(4, 14), c(5, 8), c(6, 9),
#'   c(7, 13), c(8, 10), c(9, 10), c(10, 10)
#' ))
#' v <- airportGame(costs)
#' Sh <- twoStageStApproShapleOpt(length(costs), v, 100000)
twoStageStApproShapleOpt <- function(n, v, min_sample_size, first_stage_size = 0.5) {
  N <- 1:n

  # function to get a random permutation from N with the
  # provided player fixed at the specified position
  get_sample_from_population <- function(player, position) {
    order <- sample(N[!N == player])
    append(order, player, after = position - 1)
  }

  # for the first stage we take a portion
  # (usually half) of the available samples
  sample_size_first_half <- min_sample_size * first_stage_size

  # there are n*n (n player and n positions) strata
  sample_size_per_strata_first_half <- sample_size_first_half / (n * n)

  # Sh[player, position]
  Sh <- matrix(rep(0, n * n), nrow = n, ncol = n)

  # variances[player, position]
  variances <- matrix(rep(0, n * n), nrow = n, ncol = n)

  # 1. Stage:
  # iterate through each position and player (each stratum) and sample
  # the shapley value and its variance
  # -> save these values into Sh and variances
  for (pos in 1:n) {
    for (player in 1:n) {
      sum_squared <- 0
      for (x in 1:sample_size_per_strata_first_half) {
        order <- get_sample_from_population(player, pos)
        marg_contrib <- v(take(order, pos)) - v(take(order, pos - 1))
        Sh[player, pos] <- Sh[player, pos] + marg_contrib
        sum_squared <- sum_squared + marg_contrib * marg_contrib
      }

      # estimate the variance of each stratum
      variances[player, pos] <- (sum_squared - (Sh[player, pos] / sample_size_per_strata_first_half)) / (sample_size_per_strata_first_half - 1)
    }
  }

  variances_sum <- sum(variances)

  # 2. Stage
  # iterate through each position and player (each stratum) again and
  # calculate how many more samples to take depending on the variance of this
  # stratum
  for (pos in 1:n) {
    for (player in 1:n) {
      target_sample_count <- min_sample_size * variances[player, pos] / variances_sum
      remaining_sample_count <- target_sample_count - sample_size_per_strata_first_half

      if (remaining_sample_count > 0) {
        for (x in 1:remaining_sample_count) {
          order <- get_sample_from_population(player, pos)
          marg_contrib <- v(take(order, pos)) - v(take(order, pos - 1))
          Sh[player, pos] <- Sh[player, pos] + marg_contrib
        }
      }

      actual_sample_count <- sample_size_per_strata_first_half + max(remaining_sample_count, 0)

      Sh[player, pos] <- Sh[player, pos] / actual_sample_count
    }
  }

  # todo: maybe also return the variances and the actual sample count
  Sh <- rowSums(Sh) / n
  Sh
}
