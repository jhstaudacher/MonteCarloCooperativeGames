skip_tests <- TRUE
with_expects <- TRUE
with_prints <- TRUE

setup_game <- function() {
  total <- read.csv(file = test_path("fixtures", "imf_2015.txt"), sep = ";")$Total
  v <- weightedVotingGameForSampling(total, 0.5)
  v
}
setup_exact_shap <- function() {
  exact_shap <- read.csv(file = test_path("fixtures", "IMF2015Res.csv"), sep = " ")$simp2015Shap
  exact_shap <- as.numeric(sub("%", "", exact_shap))
  exact_shap <- exact_shap / 100
  exact_shap
}
setup_exact_banz <- function() {
  exact_banz <- read.csv(file = test_path("fixtures", "IMF2015Res.csv"), sep = " ")$simp2015Banz
  exact_banz <- as.numeric(sub("%", "", exact_banz))
  exact_banz <- exact_banz / 100
  exact_banz
}
