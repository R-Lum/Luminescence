\name{AgeS}
\alias{AgeS}
\docType{data}
\title{Output of \code{\link{AgeS_Computation}} function for the samples: "GDB5" and "GDB3"
}
\description{
Output of \code{\link{AgeS_Computation}} function for the samples: "GDB5" and "GDB3", there is no stratigraphic relation neither systematic errors.
}
\usage{data("AgeS")}
\format{
  A list containing
  \describe{
    \item{\code{Sampling}}{MCMC.list that corresponds to a sample of the posterior distributions of the ages (in ka), palaeodoses (in Gy) and equivalent dose dispersions (in Gy) parameters of samples "GDB5" and "GDB3"; }
    \item{\code{Model_GrowthCurve}}{stating which dose response fitting option was chosen to run the function}
    \item{\code{Distribution}}{stating which distribution was chosen to model the dispersion of individual equivalent dose values around the palaeodose of the sample;}
    \item{\code{PriorAge}}{stating the priors used for the age parameter (in ka);}
    \item{\code{StratiConstraints}}{stating the matrix of stratigraphic relations between samples considered in the model;}
    \item{\code{CovarianceMatrix}}{stating the covariance matrix of error used in the model, highlighting  not common errors between samples in our cases (diagonal matrix).}
  }
}
%\details{
%%  ~~ If necessary, more details than the __description__ above ~~
%}
%\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
%}
\references{
Tribolo, C., Asrat, A., Bahain, J. J., Chapon, C., Douville, E., Fragnol, C., Hernandez, M., Hovers, E., Leplongeon, A.,  Martin, L,  Pleurdeau, D,  Pearson, O ,  Puaud, S,  Assefa, Z. (2017). Across the Gap: Geochronological and Sedimentological Analyses from the Late Pleistocene-Holocene Sequence of Goda Buticha, Southeastern Ethiopia. PloS one, 12(1), e0169418.
}
\examples{
data(AgeS)
str(AgeS)
}
\keyword{datasets}
