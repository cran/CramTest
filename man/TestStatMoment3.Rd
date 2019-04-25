\name{TestStatMoment3}
\alias{TestStatMoment3}
\title{The third moment of the Cramer test statistics}
\description{Calculates the third moment of the test statistics calculated using the Cramer test.}
\usage{TestStatMoment3(n1, n2, sample, Regions)}
\arguments{
\item{n1}{an integer referring to the number of observations in the first sample.}
\item{n2}{an integer reffering to the number of observations in the second sample.}
\item{sample}{a (non-empty) numeric vector of data values.}
\item{Regions}{a (non-empty) numeric vector of grid points to be used in the integration.}
}
\details{
\code{sample} should be the two samples of data that is to be tested using the Cramer test, concatinated.

\code{Regions} must be sorted from smallest to largest number. \code{Regions} must not contain any numbers smaller than the minimum value or larger than the maximum value of \code{sample}. 

The result is more accurate when more grid points are used, however the use of more grid points could make the function run slower. It is recommended to use equally spaced grid points.
}
\author{Alison Telford <mm11ajt@leeds.ac.uk>}
\examples{
	data1 = rnorm(100)
	data2 = rnorm(100)
	data = c(data1, data2)
	regions = seq(min(data), max(data), length = 50)
	TestStatMoment3(100, 100, data, regions)
}
