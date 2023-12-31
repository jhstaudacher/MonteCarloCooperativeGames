% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/checkCommonParameter.R
\name{check_n_i}
\alias{check_n_i}
\title{Check parameter \code{n} and \code{i}}
\usage{
check_n_i(n, i)
}
\arguments{
\item{n}{integer: Indicates the cardinality of the set of players \code{N} (The number of players).}

\item{i}{integer: The player \code{i}.}
}
\value{
None
}
\description{
Check the parameters \code{n} and \code{i} for validity with the intended functions for that purpose.
Additionally, check if \code{i} is smaller or equal \code{n}, because player \code{i} must be
in the set generated by \code{n}.
}
\author{
Elias Wucher

Alexander Riessbeck
}
