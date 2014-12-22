\name{Second2Gray}
\alias{Second2Gray}
\title{Converting values from seconds (s) to gray (Gy)}
\description{Conversion of absorbed radiation dose in seconds (s) to the SI unit gray (Gy) 
including error propagation. Normally used for equivalent dose data.}
\usage{Second2Gray(data, dose.rate, method = "gaussian")}
\arguments{
  \item{data}{\code{\link{data.frame}} (\bold{required}): input values, structure: 
data (\code{values[,1]}) and data error (\code{values [,2]}) are required}
  \item{dose.rate}{\code{\linkS4class{RLum.Results}} or \code{\link{vector}} (\bold{required}): 
\code{RLum.Results} needs to be orginated from the function \code{\link{calc_SourceDoseRate}},
for \code{vector}dose rate in Gy/s and dose rate error in Gy/s}
  \item{method}{\link{character} (with default): method used for error calculation 
(\code{gaussian} or \code{absolute}), see details for further information}
}
\details{Calculation of De values from seconds (s) to gray (Gy)
\deqn{De [Gy] = De [s] * Dose Rate [Gy/s])} \cr
Provided calculation methods for error calculation:  
\bold{gaussian} error propagation \cr
\deqn{De.error.gray = \sqrt(dose.rate * De.error.seconds)^2 + (De.seconds * dose.rate.error)^2 ))}
\bold{absolute} error propagation \cr
\deqn{De.error.gray = abs(dose.rate * De.error.seconds) + abs(De.seconds * dose.rate.error)}}
\value{Returns a \link{data.frame} with converted values.}
\references{#}
\author{Sebastian Kreutzer, IRAMAT-CRP2A, Universite Bordeaux Montaigne (France),\cr
Michael Dietze, GFZ Potsdam (Germany),\cr
Margret C. Fuchs, AWI Potsdam (Germany), \cr
R Luminescence Package Team}
\note{If no or a wrong method is given, the execution of the function is stopped. }


\seealso{# }
\examples{

##(A) for known source dose rate at date of measurement
## - load De data from the example data help file
data(ExampleData.DeValues, envir = environment())
## - convert De(s) to De(Gy)
Second2Gray(ExampleData.DeValues, c(0.0438,0.0019))





##(B) for source dose rate calibration data
## - calculate source dose rate first
dose.rate <-  calc_SourceDoseRate(measurement.date = "2012-01-27",
                                  calib.date <- "2014-12-19",
                                  calib.dose.rate <- 0.0438,
                                  calib.error <- 0.0019)  
# read example data
data(ExampleData.DeValues, envir = environment())

# apply dose.rate to convert De(s) to De(Gy)
Second2Gray(ExampleData.DeValues, dose.rate)

}

\keyword{manip}
\section{Function version}{0.4 (2014-12-18 15:38:33)}
