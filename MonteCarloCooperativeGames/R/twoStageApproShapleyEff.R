#' @name twoStageApproShapleyEff
#' @title Two Stage Appro Shapley Eff
#' @description
#' Described in "Improving polynomial estimation of the Shapley value by stratified random sampling with optimum allocation" on page 183
#' Calculates the shapley value based on the twoStageStApproShapleyOpt algorithm and the common approShapley
#' with the extension, that the algorithm calculates the value Efficient
#' @template author/EW
#' @template param/n
#' @template param/v
#' @param sample_size_eff The amount of samples for the common approShapley algorithm.
#' @param sample_size_opt The amount of samples for the twoStageStApproShapleyOpt algorithm
#' @return The shapley value of each player with the addition, that sum(sh_i) = v(N) (efficient)
#' @template cites/CASTRO_ET_AL_2017
#' @templateVar CASTRO_ET_AL_2017_P TODOe
#' @export
#' @examples
#' # sample a glove Game with 100 players (|L|=50 => |R|=50)
#' L <- 1:50
#' R <- 51:100
#' Sh <- twoStageApproShapleyEff(100, gloveGameForSampling(L, R), 10000, 100000)
twoStageApproShapleyEff <- function(n, v, sample_size_eff, sample_size_opt) {
  # to check if it is really efficient execute sum(Sh). The result should be 50

  # calculate the common shapley value
  sh_eff <- approShapley(n, sample_size_eff, v)

  # calculate the shapley value with stratified sampling
  sh_opt <- twoStageStApproShapleyOpt(n, v, sample_size_opt)

  # Because the common shaplay value is efficient, the sum is equal to v_N
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
