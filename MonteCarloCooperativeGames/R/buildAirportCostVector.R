#' @name buildAirportCostVector
#' @title Build Airport Cost Vector
#' @description
#' Returns the cost vector for an airport game
#' @template author/DU
#' @param cost_info A list of tuples that describes how often each number is going to appear ((number, times), ...).
#' The tuples in cost_info should be sorted by the cost in ascending order.
#' @return The cost vector described by cost_info.
#' @export
#' @examples
#' # TODO: Should this be a exported function. ?!
#' # Currently it's used by the example in twoStageStApproShapleyOpt but maybe
#' # it should be entirely removed.
#' costs <- buildAirportCostVector(list(
#'   c(1, 8), c(2, 12), c(3, 6), c(4, 14), c(5, 8), c(6, 9),
#'   c(7, 13), c(8, 10), c(9, 10), c(10, 10)
#' ))
buildAirportCostVector <- function(cost_info) {
  costs <- c()

  for (x in cost_info) {
    cost <- x[1]
    times <- x[2]

    costs <- append(costs, rep(cost, times))
  }

  costs
}
