\name{Calc.Mu}
\alias{Calc.Mu}
\title{Generalised Pareto Distribution theoretical and empirical mean difference}
\description{Calculates the difference between the theoretical mean for the Generalised Pareto Distribution (GPD) and the empirical mean of some data.}
\usage{Calc.Mu(mu, xi, sigma, mean)}
\arguments{
\item{mu}{a number indicating the location parameter for the GPD.}
\item{xi}{a number indicating the shape parameter for the GPD.}
\item{sigma}{a number indicating the scale parameter for the GPD.}
\item{mean}{a number indicating the sample mean.}
}
\details{
The inputs must be numeric and a single value. 

For this function to be well defined, xi<1.

All parameters must be provided.

This function can be optimised to find the fitted value of the location parameter mu for the GPD if the scale and shape parameters are known.
}
\author{Alison Telford <mm11ajt@leeds.ac.uk>}
\examples{
	data = rnorm(100)
	m = mean(data)
	Calc.Mu(0.5,0.5,0.5,m)
}
