\name{CramTest-package}
\alias{CramTest-package}
\alias{CramTest}
\docType{package}
\title{
  Performs the two sample univariate Cramer Test on two equal length vectors of data.
}
\description{
  Performs a nonparametric two sample test to identify differences between two groups. The test compares the cumulative distribution estimates by integrating the squared difference. 
}
\details{
	To perform the Cramer Test on two equal length vectors Data1 and Data2 use the function Cram.test(Data1, Data2, P.Value = T, GridPoints=50). For a more accurate result, increase the value of GridPoints. However a larger GridPoints may result in a slower computation.
}
\author{
Alison Telford

Maintainer: Alison Telford <atelford7@gmail.com>
}
\references{
	For further information, refer to "Properties, Advantages and Modifications of the Cramer test" (submitted for review).
}
\examples{
	sample1 = rnorm(100)
	sample2 = rnorm(100)
	Cram.test(sample1, sample2, P.Value = TRUE, 50)
}
