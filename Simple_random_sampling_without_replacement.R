# From A. Saavedra-Nieves 2020 Statistics and game theory: Estimating coalitional values in R software


# From 2. Estimating the Banzhaf value and the Banzhaf–Owen value
from_iCoalition_to_Coalition<-function(n,iCoalition){
  Coalition <- array(0, dim = n)
  a1 <- iCoalition-1
  for (ij in 1:n){
    Coalition[ij] <- a1 - as.integer(a1/2) * 2
    a1 <- as.integer(a1/2)
  }
  return(Coalition)
}


get_random_coalition_without_replacement <- function(n, random_number){
  code <- from_iCoalition_to_Coalition(length(n), random_number)
  coaltion <- c()
  for (i in 1:length(n)){
    if (1 == code[i]){
      coaltion <- append(coaltion, n[i])
    }
  }
  return(coaltion)
}


# From 2.2 Simple random sampling without replacement
# Algorithm 2
# params: all_players_N = list, player_i = int, game_v = function
simple_random_sampling_without_replacement <- function(all_players_N, player_i, sampling_size_l, game_v){
  stopifnot(1 < sampling_size_l)
  stopifnot(sampling_size_l <= 2^(length(all_players_N)-1))
  
  banzhaf_value <- 0
  player_i_value <- all_players_N[player_i]
  players_N_without_i <- all_players_N[-player_i]
  random_numbers <- sample(1:2^(length(all_players_N)-1), sampling_size_l, replace=F)
  
  for (j in 1:sampling_size_l){
    sample_T_j <- get_random_coalition_without_replacement(players_N_without_i, random_numbers[j])
    banzhaf_value <- banzhaf_value + game_v(append(sample_T_j, player_i_value)) - game_v(sample_T_j)
  }
  
  banzhaf_value <- banzhaf_value / sampling_size_l
  return(banzhaf_value)
}

# -----

glove_game_ten_players <- function (S) {
  left_in_S <- sum(S <= 10/2)
  right_in_S <- length(S) - left_in_S
  
  return(min(c(left_in_S, right_in_S)))
}


n <- 4
glove_game_n_players <- function (S) {
  left_in_S <- sum(S <= n/2)
  right_in_S <- length(S) - left_in_S
  
  return(min(c(left_in_S, right_in_S)))
}


# max sample size = 512
print(Simple_random_sampling_without_replacement(1:10, 1, 200, glove_game_ten_players))

