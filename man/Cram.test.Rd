\name{Cram.test}
\alias{Cram.test}
\title{Two sample Univariate Cramer Test}
\description{Performs the two sample Cramer Test on two vectors of data.}
\usage{Cram.test(Data1, Data2, P.Value = T, GridPoints=50)}
\arguments{
\item{Data1}{a (non-empty) numeric vector of data values.}
\item{Data2}{a (non-empty) numeric vector of data values.}
\item{P.Value}{logical, if true will return the p-value of the test as well as the statistic.}
\item{GridPoints}{an integer indicating the number of grid points to be used for the integration when calculating the moments of the test statistics.}
}
\details{
Consider data \eqn{latex}{x_1, x_2, ..., x_n} and \eqn{latex}{y_1, y_2, ..., y_m} as two samples, assumed to have come from probability density functions \eqn{latex}{f} and \eqn{latex}{g}, respectively. Let \eqn{latex}{F} and \eqn{latex}{G} be the cumulative density functions, respectively. We are interested to test the null hypothesis \eqn{latex}{H_0: F=G}. The main function is \code{Cram.test()} which will calculate the test statistic

\deqn{latex}{T_{n,m}=\int_{-\infty}^{\infty}(F(t)-G(t))^2 dt}

and its corresponding p-value based on the approximation of the generalized Pareto distribution to the test statistic.
}
\value{
\item{Statistic}{the value of Cramer test statistic.}
\item{p.value}{the p-value for the test.}
}
\references{
For further information, refer to "Properties, Advantages and a Faster p-value Calculation of the Cramer test" (submitted for review).
}
\author{Alison Telford <mm11ajt@leeds.ac.uk>}
\examples{
	sample1 = rnorm(100)
	sample2 = rnorm(100)
	Cram.test(sample1, sample2, P.Value = TRUE, 50)
}
