#' Returns a vector of all predecessors of player i in order O
#'
#' @param O Order
#' @param i Player
#'
#' @return Vector of all predecessors of player i in order O
#' @export
#'
#' @examples
#' pre(c(1, 4, 2, 3), 2)
pre <- function(O, i) {
  i_idx <- match(i, O)

  if (i_idx == 1) {
    return(c())
  }

  O[1:(i_idx - 1)]
}
