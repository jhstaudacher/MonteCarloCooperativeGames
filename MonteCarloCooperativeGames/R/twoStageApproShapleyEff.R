twoStageApproShapleyEff <- function(n, v, sample_size_eff, sample_size_opt) {
  sh_eff <- approShapley(n, sample_size_eff, v)
  sh_opt <- twoStageStApproShapleOpt(n, v, sample_size_opt)

  v_N <- sum(sh_eff)

  sum_sh_opt <- sum(sh_opt)
  eff_gap <- v_N - sum_sh_opt

  sh_eff_ratio <- sum_eff/ v_N
  sh_opt_corrections <- sh_eff_ratio * eff_gap

  sh_opt_eff = sh_opt + sh_opt_corrections

  sh_opt_eff
}
