\name{ModelC14}
\alias{ModelC14}
\docType{data}
\title{
Likelihood of C14 samples for JAGS models use in \code{Age_OSLC14}
}
\description{
A list of models for C14 data to define likelyhood in JAGS models.
}
\usage{data("ModelC14")}
\format{
This list contains:  \describe{
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

Hogg AG, Hua Q, Blackwell PG, Niu M, Buck CE, Guilderson TP, Heaton TJ, Palmer JG, Reimer PJ, Reimer RW, Turney CSM, Zimmerman SRH. 2013. SHCal13 Southern Hemisphere calibration, 0-50000 years cal BP. Radiocarbon 55(4):1889-1903}
\examples{
data(Model_AgeC14)
writeLines(Model_AgeC14$full)
}
\keyword{datasets}
