#' @name ergodicSamplingGreedyK2
#' @title Ergodic Sampling with GreedyK2
#' @description
#' Approximates the Shapley value by ergodic sampling with the GreedyK2 algorithm.
#' @details
#' Approximates the Shapley value by ergodic sampling. The GreedyK2 algorithm proposed by
#' Illes and Kerenyi (2022) is used to find a transformation, that generates samples
#' with negative covariance.
#' @template author/TP
#' @template param/i
#' @template param/n
#' @template param/v
#' @template param/m
#' @param m1 Number of sampled permutations used to find a suitable transformation
#' @template return/Sh_i
#' @export
#' @importFrom stats cov
#' @template cites/ILLES_KERENYI_2022
#' @examples
#' ergodicSamplingGreedyK2(1, 10, gloveGameForSampling(1:5, 6:10), 20000, 10)
ergodicSamplingGreedyK2 <- function(i, n, v, m, m1) {
  check_n_i(n, i)
  check_m(m)
  check_v(v)
  check_natural_number(m1)

  K <- 2
  greedy <- greedy_K2(m1, n, i, v)
  pairsPlayers <- greedy$pairsPlayers
  true_sample_count <- greedy$true_sample_count
  m2 <- m - true_sample_count
  L <- ceiling(m2 / 2)
  Y <- matrix(NA, nrow = L, ncol = K)
  estShapleyValue <- 0

  for (j in 1:L) {
    randPerm <- sample(1:n)
    randPermAnti <- randPerm[pairsPlayers]
    estShapleyValue <- estShapleyValue + (get_marg_contrib(randPerm, i, v) + get_marg_contrib(randPermAnti, i, v)) / (2 * L)
  }

  return(estShapleyValue)
}


get_marg_contrib <- function(O, i, v) {
  i_idx <- match(i, O)

  if (i_idx == 1) {
    return(v(c(i)))
  }

  return(v(O[1:(i_idx)]) - v(O[1:(i_idx - 1)]))
}

greedy_K2 <- function(m1, n, i, game) {
  true_sample_count <- 0
  pairsPlayers <- 1:n
  randomPermutations <- vector("list", m1)
  tranPermutations <- vector("list", m1)
  X <- rep(NA, m1)
  Y <- rep(NA, m1)
  BetaOrg <- matrix(NA, n, n)

  for (j in 1:m1) {
    randomPermutations[[j]] <- sample(1:n)
  }

  for (r in 1:n) {
    for (s in r:n) {
      for (j in 1:m1) {
        valuer <- randomPermutations[[j]][r]
        values <- randomPermutations[[j]][s]
        tranPermutations[[j]] <- randomPermutations[[j]]
        tranPermutations[[j]][r] <- values
        tranPermutations[[j]][s] <- valuer
        X[j] <- get_marg_contrib(randomPermutations[[j]], i, game)
        Y[j] <- get_marg_contrib(tranPermutations[[j]], i, game)

        true_sample_count <- true_sample_count + 2
      }
      BetaOrg[r, s] <- cov(X, Y)
    }
  }

  Beta <- BetaOrg

  for (j in 1:(ceiling(n / 2))) {
    I <- which.min(Beta)
    I_row <- (I - 1) %/% n + 1
    I_col <- I %% n
    if (I_col == 0) I_col <- n

    pairsPlayers[I_row] <- I_col
    pairsPlayers[I_col] <- I_row

    Beta[I_row, ] <- NA
    Beta[I_col, ] <- NA
    Beta[, I_row] <- NA
    Beta[, I_col] <- NA
  }

  # divide sample count by 3 as stated by the authors on page 12
  # (theoretical optimation)
  return(list(pairsPlayers = pairsPlayers, BetaOrg = BetaOrg, Beta = Beta, true_sample_count = round(true_sample_count / 3)))
}
