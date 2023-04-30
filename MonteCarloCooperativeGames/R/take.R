#' @name take
#' @title Take
#' @description
#' Returns a sub-list of the provided source with the elements from the start up-to the specified index
#' @param source The source list
#' @param index Up-to where to include the elements
#' @return A sub-list
#' @export
#' @examples
#' source <- "alfa,bravo,charlie,delta"
#' take(source, 2)
take <- function(source, index) {
  if (index == 0) {
    return(c())
  }

  return(source[1:index])
}
