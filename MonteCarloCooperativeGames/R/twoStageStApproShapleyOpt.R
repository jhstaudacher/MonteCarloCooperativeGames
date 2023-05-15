#' @name twoStageStApproShapleyOpt
#' @title Two Stage St Appro Shapley Opt
#' @description
#' Calculates the shapley value for each player with stratification and
#' sample size per strata based on each stratums variance.
#' @details
#' Stratified:
#' This function uses strata to calculate the shapley value. A stratum is
#' created for each player at each possible position. E.g. there is a stratum
#' for player 1 at position 1, for player 1 at position 2..., for player 2
#' at position 1, for player 2 at position 2...
#'
#' Two Stages:
#' The computation is split into two stages. The first stage gets a fixed amount
#' of samples (min_sample_size * first_stage_size) which are distributed to each
#' stratum equally. While sampling for the first stage, the variance of each
#' stratum is recorded next to the shapley value.
#' In the second stage, the variances are used to calculate the amount of
#' samples each stratum should get in total. If a stratum already got enough
#' samples from stage one it is not sampled again in stage two, otherwise the
#' remaining samples are taken for each stratum. This probably leads to more
#' samples taken in total than specified by min_sample_size (hence the name).
#' @template author/DU
#' @template param/n
#' @template param/v
#' @param min_sample_size The amount of samples that should be taken.
#' Based on the variances of each stratum it is likely to happen, that more
#' samples are used than specified in min_sample_size. The actual sample size
#' depends on the problem.
#' @param first_stage_size The amount of samples to use for the first stage as
#' a ratio. Usually 0.5 (half of all samples).
#' @template return/Sh
#' @template cites/CASTRO_ET_AL_2017
#' @templateVar CASTRO_ET_AL_2017_P pp. 182
#' @export
#' @examples
#' \donttest{
#' # sample a airport game with 100 players
#' costs <- buildAirportCostVector(list(
#'   c(1, 8), c(2, 12), c(3, 6), c(4, 14), c(5, 8), c(6, 9),
#'   c(7, 13), c(8, 10), c(9, 10), c(10, 10)
#' ))
#' v <- airportGameForSampling(costs)
#' Sh <- twoStageStApproShapleyOpt(length(costs), v, 100000)
#' }
twoStageStApproShapleyOpt <- function(n, v, min_sample_size, first_stage_size = 0.5) {
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
