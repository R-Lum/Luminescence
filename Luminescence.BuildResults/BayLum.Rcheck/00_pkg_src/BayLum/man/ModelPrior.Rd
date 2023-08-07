\name{ModelPrior}
\alias{ModelPrior}
\docType{data}
\title{
Prior for JAGS models use in \code{Age_OSLC14}
}
\description{
A list to define prior in JAGS models, taking acount OSL data and C14 data in stratigraphic constraint.
The difficulty is in the fact that each cases is different. The youngest sample can be a C14 as well as a OSL sample.
To resolve this problem we consider diferent cases thanks to this list.
}
\usage{data("ModelPrior")}
\format{
 This list contains:
  \describe{
    \item{\code{Sample1_C14}}{model considering that the youngest sample is a C14 sample}
    \item{\code{Sample1_OSL}}{model considering that the youngest sample is a OSL sample}
    \item{\code{C14_OSL}}{model considering that the second sample is a C14 sample}
    \item{\code{OSL_C14}}{model considering that the second sample is a OSL sample}
    \item{\code{C14}}{model considering that the last sample is a C14 sample}
    \item{\code{OSL}}{model considering that the last sample is a OSL sample}
  }
}
%\details{
%%  ~~ If necessary, more details than the __description__ above ~~
%}
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
data(ModelPrior)
## ModelPrior[[OSL]]
writeLines(ModelPrior$OSL)
}
\keyword{datasets}
