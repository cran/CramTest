\name{Calc.Sigma}
\alias{Calc.Sigma}
\title{Generalised Pareto Distribution theoretical and empirical variance difference}
\description{Calculates the difference between the theoretical variance for the Generalised Pareto Distribution (GPD) and the empirical variance of some data.}
\usage{Calc.Sigma(sigma, xi, variance)}
\arguments{
\item{sigma}{a number indicating the scale parameter for the GPD.}
\item{xi}{a number indicating the shape parameter for the GPD.}
\item{variance}{a number indicating the sample variance.}
}
\details{
The inputs must be numeric and a single value. 

For this function to be well defined, xi<1/2.

All parameters must be provided.

This function can be optimised to find the fitted value of the scale parameter Sigma for the GPD if the shape parameter is known.
}
\author{Alison Telford <mm11ajt@leeds.ac.uk>}
\examples{
	data = rnorm(100)
	s = sd(data)
	Calc.Sigma(0.25,0.25,s)
}
