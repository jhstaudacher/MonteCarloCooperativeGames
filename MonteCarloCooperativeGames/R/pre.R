#' @name pre
#' @title Pre
#' @description Get predecessors of i in O
#' Returns a vector of all predecessors of player i in order O
#' @template author/TP
#' @param O Order
#' @template param/i
#' @return Vector of all predecessors of player i in order O
pre <- function(O, i) {
  i_idx <- match(i, O)

  if (i_idx == 1) {
    return(c())
  }

  O[1:(i_idx - 1)]
}
