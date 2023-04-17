gloveGame <- function(L, R) {
  # todo check if L and R are disjoint
  v <- function(S) {
    left_glove_count <- length(intersect(S, L))
    right_glove_count <- length(intersect(S, R))

    min(left_glove_count, right_glove_count)
  }

  v
}


v <- gloveGame(L=1:50,R=51:100)
v(c())
