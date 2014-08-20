\name{Second2Gray}
\alias{Second2Gray}
\title{Converting values from seconds (s) to gray (Gy)}
\description{Conversion of absorbed radiation dose in seconds (s) to the SI unit gray (Gy) 
including error propagation. Normally used for equivalent dose data.}
\usage{Second2Gray(values, dose_rate, method = "gaussian")}
\arguments{
  \item{values}{\link{data.frame} (\bold{required}): measured data (\code{values[,1]}) and data error 
(\code{values [,2]})}
  \item{dose_rate}{\link{vector} (\bold{required}): dose rate in Gy/s and dose rate error in Gy/s}
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
\author{Sebastian Kreutzer, JLU Giessen (Germany), 
Michael Dietze, GFZ Potsdam (Germany),
Margret C. Fuchs, AWI Potsdam (Germany), \cr
R Luminescence Package Team}
\note{If no or a wrong method is given, the execution of the function is stopped. }


\seealso{# }
\examples{

##(1) for dose taken from the example data help file
data(ExampleData.DeValues, envir = environment())
Second2Gray(ExampleData.DeValues, c(0.0438,0.0019))

}

\keyword{manip}
\section{Function version}{0.3 (2014-04-13 14:32:24)}
