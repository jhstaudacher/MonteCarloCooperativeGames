#' @name twoStageStApproShapleyOpt
#' @title Two Stage St Appro Shapley Opt
#' @description
#' Calculates the shapley value for each player with stratification and
#' sample size per strata based on each stratum's variance.
#' @details
#' Stratified:
#' This function uses strata to calculate the shapley value. A stratum is
#' created for each player at each possible position. E.g. there is a stratum
#' for player 1 at position 1, for player 1 at position 2..., for player 2
#' at position 1, for player 2 at position 2...
#'
#' Two Stages:
#' The computation is split into two stages. The first stage gets a fixed amount
#' of samples (```min_sample_size * first_stage_size```) which are distributed to each
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
#' samples are used than specified in ```min_sample_size```. The actual sample size
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
  check_m(min_sample_size, bigz_allowed = TRUE)
  check_natural_number(n)
  check_v(v)
  check_first_stage_size(first_stage_size)

  use_bigz <- FALSE
  if (is.bigz(min_sample_size)) {
    min_sample_size <- as.bigq(min_sample_size)
    use_bigz <- TRUE
  }

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
  check_sample_size_first_half(sample_size_first_half)

  # there are n*n (n player and n positions) strata
  sample_size_per_strata_first_half <- sample_size_first_half / (n * n)
  check_sample_size_per_strata_first_half(sample_size_per_strata_first_half)

  initial_zero <- 0
  if (use_bigz) {
    initial_zero <- as.bigq(0)
  }

  # Sh[player, position]
  Sh <- matrix(rep(initial_zero, n * n), nrow = n, ncol = n)

  # variances[player, position]
  variances <- matrix(rep(initial_zero, n * n), nrow = n, ncol = n)

  # 1. Stage:
  # iterate through each position and player (each stratum) and sample
  # the shapley value and its variance
  # -> save these values into Sh and variances
  for (pos in 1:n) {
    for (player in 1:n) {
      sum_squared <- 0
      count <- 1
      while (count <= sample_size_per_strata_first_half) {
        order <- get_sample_from_population(player, pos)
        marg_contrib <- v(take(order, pos)) - v(take(order, pos - 1))
        Sh[player, pos] <- Sh[player, pos] + marg_contrib
        sum_squared <- sum_squared + marg_contrib * marg_contrib
        count <- count + 1
      }

      # estimate the variance of each stratum
      variances[player, pos] <- (sum_squared - (Sh[player, pos] / sample_size_per_strata_first_half)) / max(sample_size_per_strata_first_half - 1, 1)
    }
  }

  variances_sum <- sum(variances)

  # 2. Stage
  # iterate through each position and player (each stratum) again and
  # calculate how many more samples to take depending on the variance of this
  # stratum
  for (pos in 1:n) {
    for (player in 1:n) {
      target_sample_count <- min_sample_size * variances[player, pos] / max(variances_sum, 1)
      remaining_sample_count <- target_sample_count - sample_size_per_strata_first_half

      if (remaining_sample_count > 0) {
        count <- 1
        while (count <= remaining_sample_count) {
          order <- get_sample_from_population(player, pos)
          marg_contrib <- v(take(order, pos)) - v(take(order, pos - 1))
          Sh[player, pos] <- Sh[player, pos] + marg_contrib
          count <- count + 1
        }
      }

      actual_sample_count <- sample_size_per_strata_first_half + max(remaining_sample_count, 0)

      Sh[player, pos] <- Sh[player, pos] / actual_sample_count
    }
  }

  # todo: maybe also return the variances and the actual sample count

  if (use_bigz) {
    # for some reason rowSums does not work with bigz
    row_sum_result <- c()
    for (row in 1:nrow(Sh)) {
      row_sum_result <- append(row_sum_result, sum(Sh[row, ]))
    }
    Sh <- row_sum_result / n
  } else {
    Sh <- rowSums(Sh) / n
  }

  Sh
}

check_first_stage_size <- function(first_stage_size) {
  if (first_stage_size <= 0.0 || first_stage_size >= 1.0) {
    stop("first_stage_size has to be between 0.0 (excluding) and 1.0 (excluding)")
  }
}

check_sample_size_first_half <- function(sample_size_first_half) {
  if (sample_size_first_half < 1.0) {
    stop("The combination of the provided min_sample_size and first_stage_size does not leave any samples for the first stage")
  }
}

check_sample_size_per_strata_first_half <- function(sample_size_per_strata_first_half) {
  if (sample_size_per_strata_first_half < 1.0) {
    stop("The combination of the provided min_sample_size and first_stage_size results in a sample size per stratum that is smaller than one. Please increase one or both of the parameters.")
  }
}
