% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/selectExtreme.r
\name{selectExtreme}
\alias{selectExtreme}
\title{' @title selectExtreme
#@description returns the extremes of a distribution}
\usage{
selectExtreme(N = 200, n = 40, lower = TRUE, p, probaS, K)
}
\arguments{
\item{N}{number of individuals in the experiment}

\item{n}{number of individuals required in the extremal group}

\item{lower}{TRUE for the group with the lower value of T (threhsold), FALSE for the group with the highest threshold}

\item{p}{probability to fail an AFC test}

\item{probaS}{distribution of probability}

\item{K}{number of successive AFC tests}
}
\value{
nG
}
\description{
This function an extreme group from a given experimentation : given p, probaS, K (number of samples) and N (number of tests) it returns a group containing n subjects (with the lowest -when lower = TRUE or the highest values).
}
