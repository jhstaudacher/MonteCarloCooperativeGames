#
# Here are all Functions, that check the common parameters used by the algorithms.
#


#' @name check_v
#' @title Check parameter ```v```
#' @description
#' Check parameter ```v``` for validity. ```v``` must not be ```NULL``` and it has to be a function.
#' @template author/EW
#' @template param/v
#' @return None
#
# example
# check_v(function(x)x)
check_v <- function(v) {
  if (is.null(v)) {
    stop("Function is NULL")
  }

  if (!is.function(v)) {
    stop("Parameter v isn't a function")
  }
}





#' @name check_positive_number
#' @title Check if the parameter is a number and positive.
#' @description
#' Check if the parameter is a number and positive. It can be used for the
#' parameters ```n```, ```i```, ```m``` and ```w```.
#' @template author/EW
#' @param parameter value to check
#' @return None
check_positive_number <- function(parameter) {
  if (is.null(parameter)) {
    stop("parameter is NULL")
  }

  if (!is.numeric(parameter)) {
    stop("parameter must be a number")
  }

  if (parameter <= 0) {
    stop("parameter must be greater than 0")
  }
}


#' @name check_natural_number
#' @title Check if the parameter is a natural number.
#' @description
#' Checks if the value of the parameter is a natural number. Note: The
#' datatype will not be checked. It is will only checked if the value of the
#' parameter is greater than 0 and not a decimal number.
#' @template author/TP
#' @param parameter value to check
#' @return None
check_natural_number <- function(parameter) {
  check_positive_number(parameter)

  if (parameter %% 1 != 0) {
    stop("parameter must be a natural number")
  }
}


#' @name check_n_i
#' @title Check parameter ```n``` and ```i```
#' @description
#' Check the parameters ```n``` and ```i``` for validity with the intended functions for that purpose.
#' Additionally, check if ```i``` is smaller or equal ```n```, because player ```i``` must be
#' in the set generated by ```n```.
#' @template author/EW
#' @template author/AR
#' @template param/n
#' @template param/i
#' @return None
check_n_i <- function(n, i) {
  check_natural_number(n)
  check_natural_number(i)

  if (n <= 0) {
    stop("n must be at least 1")
  }
  if (i <= 0) {
    stop("i must be at least 1")
  }
  if (i > n) {
    stop("i isn't in n")
  }
}





#' @name check_P
#' @title Check parameter ```P```
#' @description
#' Check parameter ```P``` for validity. Parameter ```P``` must not be ```NULL```, must be a
#' list of vectors or a list of lists for the priori unions. The priori unions
#' must be numeric and positive. Additionally, the priori unions have to be disjunct.
#' @template author/EW
#' @template param/P
#' @return None
check_P <- function(P) {
  # if P is set
  if (is.null(P)) {
    stop("P is NULL")
  }

  # Check if it is a list
  if (!is.list(P)) {
    stop("P must be a list")
  }

  # Check if the values of the priori unions are numeric and positive
  if (any(sapply(unlist(P), function(x) is.numeric(x) && x > 0) == FALSE)) {
    stop("There is a priori union that contains a element that isn't numeric or smaller than 0")
  }

  # Check if the prior unions are disjunct
  last <- -1
  liste <- sort(unlist(P))
  for (p in liste) {
    if (p != last) {
      last <- p
    } else {
      stop("Priori unions aren't disjunct")
    }
  }
}





#' @name check_P_i
#' @title Check parameter ```P``` and ```i```
#' @description
#' Check parameter ```P``` and ```i``` for validity with the intended functions for that purpose.
#' Additionally, check if player ```i``` is element of a priori union in ```P```.
#' @template author/EW
#' @template param/P
#' @template param/i
#' @return None
check_P_i <- function(P, i) {
  check_P(P)
  check_natural_number(i)

  if (i %in% unlist(P) == FALSE) {
    stop("i isn't in a priori unions")
  }
}





#' @name check_conf
#' @title Check parameter ```conf```
#' @description
#' Check parameter ```conf``` for validity.
#' @template author/TP
#' @template param/conf
#' @return None
check_conf <- function(conf) {
  check_positive_number(conf)

  if (conf >= 1.0) {
    stop("conf cannot be greater or equal to 1.0")
  }
}





#' @name check_m
#' @title Check parameter ```m```
#' @description
#' Check parameter ```m``` for validity.
#' @template author/AR
#' @template param/m
#' @param max_value Maximal value ```m``` can take.
#' @param bigz_allowed Determine if big ints from ```gmp``` are allowed.
#' @return None
check_m <- function(m, max_value = NULL, bigz_allowed = FALSE) {
  if (is.null(m)) {
    stop("m cannot be NULL")
  }

  if (!is.numeric(m)) {
    if (bigz_allowed) {
      if (!is.bigz(m)) {
        stop("m must be a number or an R object of class \"bigz\"")
      }
    } else {
      stop("m must be a number")
    }
  }

  if (m <= 0) {
    stop("m must be at least 1")
  }

  if (m %% 1 != 0) {
    stop("m must be a natural number")
  }

  if (!is.null(max_value)) {
    if (max_value < m) {
      stop("m cannot be greater than ", max_value)
    }
  }
}

#' @name check_m_n
#' @title Check parameters ```m``` and ```n```
#' @description
#' Check parameters ```m``` and ```n``` for validity.
#' @template author/TP
#' @template param/m
#' @template param/n
#' @param m_max Maximal value ```m``` can take.
#' @param bigz_allowed Determines if bigz are allowed.
#' @return None
check_m_n <- function(m, n, m_max = NULL, bigz_allowed = FALSE) {
  check_m(m, m_max, bigz_allowed)
  check_natural_number(n)

  if (m < n) {
    msg <- paste("You provided a sample size of m=", m, ", but m is less than n=", n, ". To guarantee that every player gets at least one sample you need to provide a higher sample size m.", sep = "")
    stop(msg)
  }

  if ((m %% n) > 0) {
    msg <- paste("You provided a sample size of m=", m, ", but m is not divisible by n=", n, ". To preserve the efficiency (i.e. the sum of the result vector is v(N)) the remaining ", m %% n, " samples will not be used.", sep = "")
    warning(msg)
  }
}
