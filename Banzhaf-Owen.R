P <- list(c(1, 2), c(3,4,5), c(6)) 
i <- 1
# glove game: left 1, 2; right 3


v <- function (S) {
  left_in_S <- sum(S <= 2)
  right_in_S <- length(S) - left_in_S
  
  return(min(c(left_in_S, right_in_S)))
}
create_t <- function(i) {
  for(dx in 1:length(P)){
    
  }
  
}
get_size_of_player_partition <- function(player, ListOfPriorUnion){
  for(x in ListOfPriorUnion){
    for(j in x){
      if(j==player){
        return (length(x))
      }
    }
  }
  return(0)
}

l <- 2^(length(P)-1)*2^(get_size_of_player_partition(i, P)-1)
print(l)