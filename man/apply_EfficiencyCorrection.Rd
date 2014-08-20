\name{apply_EfficiencyCorrection}
\alias{apply_EfficiencyCorrection}
\title{Function to apply spectral efficiency correction on RLum.Data.Spectrum S4 class objects}
\description{The function allows spectral efficiency corrections for  
RLum.Data.Spectrum S4 class objects }
\usage{apply_EfficiencyCorrection(object, spectral.efficiency)}
\arguments{
  \item{object}{\code{\linkS4class{RLum.Data.Spectrum}} (\bold{required}): 
S4 object of class \code{RLum.Data.Spectrum}}
  \item{spectral.efficiency}{\code{\link{data.frame}} (\bold{required}): Data set containg wavelengths
(x-column) and relative spectral response values (y-column) in percentage}
}
\details{The efficiency correction is based on a spetral response dataset provided
by the user. Usually the data set for the quantum efficiency is of lower
resolution and values are interpolated for the required spectral 
resolution. }
\value{Returns same object as input (\code{\linkS4class{RLum.Data.Spectrum}})}
\references{-}
\author{Sebastian Kreutzer, JLU Giessen (Germany),
Johannes Friedrich, University of Bayreuth (Germany)
R Luminescence Package Team}
\note{Please note that the spectral efficiency data from the camera 
may not sufficiently correct for spectral efficiency of the entire 
optical system (e.g., spectrometer, camera ...). 

This function has BETA status}


\seealso{\code{\linkS4class{RLum.Data.Spectrum}}}
\examples{
 
##(1) - use with your own data (uncomment for usage)
## spectral.efficiency <- read.csv("your data")
##
## your.spectrum <- apply_EfficiencyCorrection(your.spectrum, )
 
}

\keyword{manip}
\section{Function version}{0.1 (2014-04-13 14:27:05)}
