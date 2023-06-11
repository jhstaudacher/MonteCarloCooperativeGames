#' @name twoStageApproShapleyEff
#' @title Two Stage Appro Shapley Eff
#' @description
#' Described in "Improving polynomial estimation of the Shapley value by stratified random sampling with optimum allocation" on page 183.
#' Calculates the shapley value based on the twoStageStApproShapleyOpt algorithm and the common approShapley
#' with the extension, that the algorithm calculates the value efficient.
#' @details
#' For an efficient value the sum of all shapley values are exact the same as the value of the huge coalition.
#' First this algorithm calculates the common shapley value, that is an efficient value.
#' Second the value is calculated with stratified sampling. This algorithm returns values that don't fulfill the efficient property.
#' With the first value and the shapley value of the huge coalition a ratio is calculated for each player.
#' This ratio is the factor for the gap between all second values and the huge coalition, so the gap that is missing for being efficient.
#' This gap multiply with the factor is than added to the second value for each player.
#' @template author/EW
#' @template param/n
#' @template param/v
#' @param sample_size_eff The amount of samples for the common approShapley algorithm.
#' @param sample_size_opt The amount of samples for the twoStageStApproShapleyOpt algorithm.
#' @return The shapley value of each player with the addition, that ```sum(sh_i) = v(N)``` (efficient).
#' @template cites/CASTRO_ET_AL_2017
#' @templateVar CASTRO_ET_AL_2017_P pp:183
#' @export
#' @examples
#' # sample a glove Game with 100 players (|L|=50 => |R|=50)
#' L <- 1:50
#' R <- 51:100
#' Sh <- twoStageApproShapleyEff(100, gloveGameForSampling(L, R), 10000, 100000)
twoStageApproShapleyEff <- function(n, v, sample_size_eff, sample_size_opt) {
  check_positive_number(n)
  check_v(v)
  check_positive_number(sample_size_eff)
  check_positive_number(sample_size_opt)

  # calculate the common shapley value
  sh_eff <- approShapley(n, sample_size_eff, v)

  # calculate the shapley value with stratified sampling
  sh_opt <- twoStageStApproShapleyOpt(n, v, sample_size_opt)

  # Because the common shapley value is efficient, the sum is equal to v_N
  v_N <- sum(sh_eff)

  # Calculate the correction value for the sh_opt value
  sum_sh_opt <- sum(sh_opt)
  eff_gap <- v_N - sum_sh_opt
  sh_eff_ratio <- sh_eff / v_N
  sh_opt_corrections <- sh_eff_ratio * eff_gap

  # Correct the value with the calculated correction
  sh_opt_eff <- sh_opt + sh_opt_corrections

  # return the vector
  sh_opt_eff
}
