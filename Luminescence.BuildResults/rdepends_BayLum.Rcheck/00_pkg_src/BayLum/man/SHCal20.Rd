\name{SHCal20}
\alias{SHCal20}
\docType{data}
\title{
Atmospheric South data for calibration of 14C age }
\description{
As 14C years is not equal to calendar years because atmospheric 14C concentration varies through time. Hence, data in AtmosphericSouth_CalC14 allows a calibration for mid-latitude Southern Hemisphere atmospher reservoir.
}
\usage{data("SHCal20")}
\format{
  A data frame with 3 variables.
  \describe{
    \item{\code{CAL.BP}}{a numeric vector correpondig to calendar years (in ky)  befor present}
    \item{\code{X14C.age}}{a numeric vector correponding to 14C age}
    \item{\code{Error}}{a numeric vector correponding to error arround 14C age measurement}
  }
}
%\details{
%%  ~~ If necessary, more details than the __description__ above ~~
%}
%\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
%}
\references{
Hogg, A., Heaton, T., Hua, Q., Palmer, J., Turney, C., Southon, J., . . . Wacker, L. (2020). SHCal20 Southern Hemisphere Calibration, 0–55,000 Years cal BP. Radiocarbon, 62(4), 759-778. doi:10.1017/RDC.2020.59
}
\examples{
data(SHCal20)
## maybe str(SHCal20) ; head(SHCal20) ...
}
\keyword{datasets}
