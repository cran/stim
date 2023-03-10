% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lavaanEq.R
\name{lavaanEq}
\alias{lavaanEq}
\title{Create lavaan syntax based on the blueprint matrix}
\usage{
lavaanEq(blueprint, S)
}
\arguments{
\item{blueprint}{A character matrix which specifies which effects to estimate and
which effects to constrain to a non-zero value}

\item{S}{Sample covariance matrix}
}
\value{
A character vector which contains the lavaan syntax to specify the latent variables, variances, and covariances
}
\description{
Create lavaan syntax based on the blueprint matrix
}
\keyword{internal}
