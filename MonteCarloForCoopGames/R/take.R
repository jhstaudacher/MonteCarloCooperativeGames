take <- function(source, index) {
  if (index == 0) {
    return(c())
  }

  return(source[1:index])
}
