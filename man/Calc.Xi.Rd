\name{Calc.Xi}
\alias{Calc.Xi}
\title{Generalised Pareto Distribution theoretical and empirical skewness difference}
\description{Calculates the difference between the theoretical skewness for the Generalised Pareto Distribution (GPD) and the empirical skewness of some data.}
\usage{Calc.Xi(xi, skewness)}
\arguments{
\item{xi}{a number indicating the shape parameter for the GPD.}
\item{skewness}{a number indicating the sample skewness.}
}
\details{
The inputs must be numeric and a single value. 

For this function to be well defined, xi<1/3.

All parameters must be provided.

This function can be optimised to find the fitted value of the shape parameter Xi for the GPD.
}
\author{Alison Telford <mm11ajt@leeds.ac.uk>}
\examples{
	data = rnorm(100)
	g = skewness(data)
	Calc.Xi(0.25,g)
}
