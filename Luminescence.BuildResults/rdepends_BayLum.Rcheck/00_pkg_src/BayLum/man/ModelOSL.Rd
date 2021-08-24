\name{ModelOSL}
\alias{ModelOSL}
\docType{data}
\title{
Likelihood of OSL samples for JAGS models use in \code{Age_OSLC14}
}
\description{
A list of models for OSL data to define likelyhood in JAGS models.
}
\usage{data("ModelOSL")}
\format{
 This list contains:
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
%\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
%}
\references{
Plummer, M. (2003). JAGS: A program for analysis of Bayesian graphical models using Gibbs sampling.
In Proceedings of the 3rd international workshop on distributed statistical computing, volume 124, page 125.
Technische Universit at Wien, Austria.

Plummer, M. (2015). JAGS Version 4.0. 0 user manual.
}
\examples{
data(ModelOSL)
## The JAGS model of the likelyhood for a saturating exponential plus linear growth
## (a function of the type \code{f(x)=a(1-exp(-x/b))+cx+d})
## and a gaussian distribution of equivalent doses around the palaeodose:
writeLines(ModelOSL$AgesMultiOSL_EXPLIN$gaussian)
}
\keyword{datasets}
