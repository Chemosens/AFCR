\name{triangular}
\alias{triangular}
\docType{data}
\title{triangular data}
\description{
This dataset is a toy dataset containing the results of triangular test for 9 concentrations and 3 subjects.
}
\usage{data(triangular)}
\format{
  A data frame with 84 observations on the following 8 variables directly exported from TimeSens.
  \describe{
    \item{\code{Seance}}{Numer of the session}
    \item{\code{Repetition}}{Number of the replicate}
    \item{\code{Prise}}{Number of the intake (not relevant here)}
    \item{\code{Paneliste}}{Subject Code}
    \item{\code{Produit}}{Product Code}
    \item{\code{Descripteur}}{Descriptor Code: contains the name of the concentration, then "_" then OK if the corresponding triangular test is succeeded, KO if else}
    \item{\code{Score}}{Score (not relevant)}
     \item{\code{Temps}}{Time of the click}
     }
}
