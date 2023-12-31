% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simpleRandomSamplingWithReplacement.R
\name{simpleRandomSamplingWithReplacement}
\alias{simpleRandomSamplingWithReplacement}
\title{Simple Random Sampling With Replacement}
\usage{
simpleRandomSamplingWithReplacement(i, n, m, v)
}
\arguments{
\item{i}{integer: The player \code{i}.}

\item{n}{integer: Indicates the cardinality of the set of players \code{N} (The number of players).}

\item{m}{integer: The number of samples.}

\item{v}{function: The characteristic function.}
}
\value{
Banzhaf value of player \code{i}.
}
\description{
Simple random sampling with replacement is a method to
approximate the Banzhaf value for a specified TU game.
}
\details{
First, the algorithm takes a sample with replacement of all players
without the player i. Then the algorithm checks the value the player adds to
the sample. These two steps are repeated for the number of samples.

Based on the paper: "Statistics and game theory: Estimating coalitional
values in R software" (A. Saavedra-Nieves, 2020) Algorithm 1

This algorithm supports BigQ from GMP.
}
\examples{
print(simpleRandomSamplingWithReplacement(1, 10, 200, gloveGameForSampling(1:5, 6:10)))
# --------------
\dontrun{
# very long runtime
m <- as.bigz(.Machine$integer.max) + 1
print(simpleRandomSamplingWithReplacement(1, 100, m, gloveGameForSampling(1:50, 51:100)))
}
}
\references{
Saavedra-Nieves, A. et al. (2020) "Statistics and game theory: Estimating coalitional values in R software ", Operations Research Letters 49 pp. 131
}
\author{
Alexander Riessbeck

Robin Liebermann

Mithunan Mehanathan
}
