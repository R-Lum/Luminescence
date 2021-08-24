\name{Model_AgeS}
\alias{Model_AgeS}
\docType{data}
\title{JAGS models use in \code{AgeS_Computation}}
\description{
A list of JAGS models use to a Bayesian analysis of OSL age of various samples. There are models for various growth curves and various distrubution to describe equivalent dose distribution around the palaeodose.
}
\usage{data("Model_AgeS")}
\format{This list contains:
  \describe{
   \item{\code{AgesMultiCS2_EXPLIN}}{a list of 4 models that all consider a saturating exponential plus linear growth. These 4 models have different distribution to describe equivalent dose values around the palaeodose.}
   \item{\code{AgesMultiCS2_EXP}}{a list of 4 models that all consider a saturating exponential growth. These 4 models have different distribution to describe equivalent dose values around the palaeodose.}
   \item{\code{AgesMultiCS2_EXPZO}}{a list of 4 models that all consider a saturating exponential plus linear growth and fitting through the origin. These 4 models have different distribution to describe equivalent dose values around the palaeodose.}
   \item{\code{AgesMultiCS2_EXPLINZO}}{a list of 4 models that all consider a saturating exponential growth and fitting through the origin. These 4 models have different distribution to describe equivalent dose values around the palaeodose.}
   }
}
\details{
The different distibutions to describe equivalent dose values around the palaeodose are:
  \describe{
   \item{\code{cauchy}}{a Cauchy distribution with postition parameter equal to the palaeodose of the sample}
   \item{\code{gaussian}}{a Gaussian distribution with mean equal to the palaeodose of the sample}
   \item{\code{lognormal_A}}{a log-normal distribution with mean or \bold{A}verage equal to the palaeodose of the sample}
   \item{\code{lognormal_M}}{a log-normal distribution with \bold{M}edian equal to the palaeodose of the sample}
   }
For more information we refer to the function \code{\link{AgeS_Computation}}, section Details.
}
\references{
Plummer, M. (2003). JAGS: A program for analysis of Bayesian graphical models using Gibbs sampling.
In Proceedings of the 3rd international workshop on distributed statistical computing, volume 124, page 125.
Technische Universit at Wien, Austria.

Plummer, M. (2015). JAGS Version 4.0. 0 user manual.
}
\seealso{
\code{\link{rjags}}
}
\examples{
data(Model_AgeS)
## The JAGS model for a saturating exponential plus linear growth
## (a function of the type \code{f(x)=a(1-exp(-x/b))+cx+d})
## and a gaussian distribution of equivalent doses around the palaeodose:
writeLines(Model_AgeS$AgesMultiCS2_EXP$gaussian)
}
\keyword{datasets}
