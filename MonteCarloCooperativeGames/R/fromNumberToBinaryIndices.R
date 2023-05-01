#' @name fromNumberToBinaryIndices
#' @title calculates the indices of the binary representation of the input number
#' @description
#' supportfunction to calculate the indices of binary representations. Used for the calculation of permutations.
#' @param num number
#' @return vector of indices
#' @examples
#' # example code
#' # TODO: create example
fromNumberToBinaryIndices <- function(num) {
  # convert the number to binary and split it into individual digits
  binary <- strsplit(as.character(intToBits(num)), " ")

  # find the indices where the binary digit is 1
  ones_indices <- which(binary == "01")
}
