\name{Model_AgeC14}
\alias{Model_AgeC14}
\docType{data}
\title{JAGS models use in \code{AgeC14_Computation}
}
\description{
A list of JAGS models use to a Bayesian analysis of C14 calibration age of various sample.
Stratigraphic relations can be taken in count to calibrate C14 ages.
This ages take into account that some data can be an outlier.
}
\usage{data("Model_AgeC14")}
\format{
 This list contains:
  \describe{
    \item{\code{full}}{a model considering error on calibration curve.}
    \item{\code{naive}}{a model not considering error on calibration curve.}
  }
}
%\details{
%%  ~~ If necessary, more details than the __description__ above ~~
%}
%\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
%}
\references{
Reimer PJ, Bard E, Bayliss A, Beck JW, Blackwell PC, Bronl Ramsey C, Buck CE, Cheng H, Edwards RL, Friedrich M, Grootes PM, Guilderson TP, Haflidason H, Hajdas I, Hatte C, Heaton TJ, Hoffmann DL, Hogg AG, Hughen KA, Kaiser KF, Kromer B,Manning SW, Niu M, Reimer RW, Richards DA, Scott EM, Southon JR, Staff RA, Turney CSM, van der Plicht J. 2013. IntCal13 ans Marine13 radiocarbon age calibration curves 0-50000 years cal BP. Radiocarbon 55(4)=1869-1887.

Hogg AG, Hua Q, Blackwell PG, Niu M, Buck CE, Guilderson TP, Heaton TJ, Palmer JG, Reimer PJ, Reimer RW, Turney CSM, Zimmerman SRH. 2013. SHCal13 Southern Hemisphere calibration, 0-50000 years cal BP. Radiocarbon 55(4):1889-1903
}
\seealso{
\code{\link{rjags}}
}
\examples{
data(Model_AgeC14)
writeLines(Model_AgeC14$full)
}
\keyword{datasets}
